//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.Cci;

namespace Microsoft.Research.Vcc
{
  class TestRunner
  {
    /// <summary>
    /// Suffix used for generating names of temporary files (.c and .i) 
    /// used during run of test suites.
    /// </summary>
    const string vccSplitSuffix = "__vcc_split__";

    private static double GetTime() {
      return System.Environment.TickCount / 1000.0;
    }

    private static double methodVerificationTime;

    public static int RunTestSuite(VccOptions commandLineOptions) {
      int errs;
      if (commandLineOptions.TimeStats)
        Console.WriteLine("<![CDATA[");
      try {
        errs = commandLineOptions.FileNames.Count(fileName => !RunTestSuite(fileName, commandLineOptions));
      } finally {
        if (commandLineOptions.TimeStats)
          Console.WriteLine("]]>");
      }
      if (commandLineOptions.TimeStats) {
        Console.WriteLine("<testrun>");
        Console.WriteLine(testrunTimeStats.ToString());
        Console.WriteLine("</testrun>");
        testrunTimeStats.Length = 0;
      }

      return errs;
    }

    static bool RunTestSuite(string fileName, VccOptions commandLineOptions) {
      if (Directory.Exists(fileName)) {
        int errorCount = 0;

        methodVerificationTime = 0;
        double startTime = GetTime();
        int threads = 1;
        if (commandLineOptions.RunTestSuiteMultiThreaded == 0) threads = System.Environment.ProcessorCount;
        else if (commandLineOptions.RunTestSuiteMultiThreaded > 0) threads = commandLineOptions.RunTestSuiteMultiThreaded;

        TestRunnerMT trmt = threads > 1 ? new TestRunnerMT(threads, commandLineOptions) : null;
        

        foreach (FileInfo fi in new DirectoryInfo(fileName).GetFiles("*", SearchOption.TopDirectoryOnly)) {
          if (fi.Name.StartsWith(".")) continue;
          if (fi.Name.Contains(vccSplitSuffix)) continue;
          if (fi.Extension == ".i" || fi.Extension == ".bpl" || fi.Extension == ".h") continue;

          if (trmt != null) trmt.Queue(fi);
          else {
            if (!TestRunner.RunTestSuite(fi, commandLineOptions))
              errorCount++;
          }
        }

        if (trmt != null) {
          errorCount += trmt.Run();
        }

        foreach (DirectoryInfo di in new DirectoryInfo(fileName).GetDirectories("*", SearchOption.TopDirectoryOnly)) {
          if (di.Name.StartsWith(".")) continue;
          RunTestSuite(di.FullName, commandLineOptions);
        }

        double now = GetTime();
        testrunTimeStats.AppendFormat("<file name=\"{0}\">\r\n", Path.GetFileName(fileName));
        testrunTimeStats.AppendFormat("<time><total>{0:0.00}</total><compiler>{1:0.00}</compiler><boogie>{2:0.00}</boogie><verification>{3:0.00}</verification></time>\r\n",
                    now - startTime, now - startTime - methodVerificationTime,
                    0,
                    methodVerificationTime);
        testrunTimeStats.AppendFormat("<errors>{0}</errors>\r\n", errorCount);
        testrunTimeStats.AppendFormat("</file>\r\n");


        if (errorCount != 0) {
          Console.WriteLine();
          Console.WriteLine("*** {0} error(s) ***", errorCount);
          Console.WriteLine();
          Console.WriteLine();
          return false;
        }
        return true;
      } else {
        return TestRunner.RunTestSuite(new FileInfo(fileName), commandLineOptions);
      }
    }

    public static string currentTestcaseName;

    static bool RunTestSuite(FileInfo testFile, VccOptions commandLineOptions) {
      using (var reader = new StreamReader(testFile.Open(FileMode.Open, FileAccess.Read)))
        return RunTestSuite(testFile.DirectoryName, testFile.Name, reader, commandLineOptions);
    }

    static bool RunTestSuite(string directoryName, string suiteName, StreamReader instream, VccOptions commandLineOptions) {
      System.Diagnostics.Debug.Listeners.Remove("Default");
      var errorHandler = new CciErrorHandler(commandLineOptions);
      StringBuilder source = null;
      StringBuilder expectedOutput = null;
      StringBuilder actualOutput = null;
      List<string> compilerParameters;
      int errors = 0;
      int testCaseCount = 0;
      var WhiteSpaceChars = " \r\n\t".ToCharArray();

      try {
        int line = 1;

        while (!instream.EndOfStream) {
          var l = instream.ReadLine(); // strips Unix or Dos line ending
          line++;

          source = new StringBuilder();
          if (l.StartsWith("`") || l.StartsWith("//`")) {
            string optionString = l.Substring(l.IndexOf('`') + 1);
            compilerParameters = optionString.Split(WhiteSpaceChars, StringSplitOptions.RemoveEmptyEntries).ToList<string>();
          } else {
            compilerParameters = new List<string>();
            source.Append(l);
            source.Append("\r\n");
          }

          while (!instream.EndOfStream) {
            l = instream.ReadLine();
            line++;
            if (l == "`" || l == "/*`")
              break;
            source.Append(l);
            source.Append("\r\n");
          }

          if (instream.EndOfStream) {
            Console.WriteLine("The last test case in the suite has not been provided with expected output");
            errors++;
            break;
          }
          
          int errLine = line;
          expectedOutput = new StringBuilder();
          while (!instream.EndOfStream) {
            l = instream.ReadLine();
            line++;
            if (l == "`" || l == "`*/")
              break;
            expectedOutput.Append(l);
            expectedOutput.Append("\r\n");
          }

          if (l != "`" && l != "`*/") {
            Console.WriteLine("The last test case in the suite has been provided with incomplete expected output");
            errors++;
            break;
          }

          actualOutput = new StringBuilder();
          TextWriter savedOut = Console.Out;
          Console.SetOut(new StringWriter(actualOutput));
          System.Diagnostics.TextWriterTraceListener myWriter = new System.Diagnostics.TextWriterTraceListener(System.Console.Out);
          System.Diagnostics.Debug.Listeners.Add(myWriter);

          ++testCaseCount;
          string suiteNameWithoutExt = Path.GetFileNameWithoutExtension(suiteName);
          string fileNameWithoutExt = directoryName + Path.DirectorySeparatorChar + suiteNameWithoutExt + vccSplitSuffix + testCaseCount + "_" + System.Diagnostics.Process.GetCurrentProcess().Id;
          currentTestcaseName = Path.GetFileName(string.Format("{0}.{1:00}", suiteNameWithoutExt, testCaseCount));

          StringBuilder backupTimeStats = testrunTimeStats;
          if (suiteName.EndsWith(".c"))
            testrunTimeStats.AppendFormat("<file name=\"{0}/{1}.{2}\">\r\n", Path.GetFileName(directoryName), suiteName, testCaseCount);
          else
            testrunTimeStats = null;
          try {
            int returnCode = RunTest(errorHandler, suiteNameWithoutExt, fileNameWithoutExt, source.ToString(), commandLineOptions, compilerParameters);
            if (returnCode != 0)
              actualOutput.Append("Non zero return code: " + returnCode);
          } catch (System.Reflection.TargetInvocationException e) {
            actualOutput.Append(e.InnerException);
          } catch (Exception e) {
            actualOutput.Append(e);
          } finally {
            if (testrunTimeStats == null)
              testrunTimeStats = backupTimeStats;
            else
              testrunTimeStats.Append("</file>\r\n");
          }

          errorHandler.Reset();
          VccCommandLineHost.ErrorHandler.ResetReportedErrors();
          Console.SetOut(savedOut);
          System.Diagnostics.Debug.Listeners.Remove(myWriter);
          Regex rx = new Regex(@"[a-zA-Z]:\\.*?\\(.*)" + vccSplitSuffix + @"[0-9_]*.c\(");
          string actualOutputRepl = rx.Replace(actualOutput.ToString(), "testcase(");
          
          if (!expectedOutput.ToString().Equals(actualOutputRepl)) {
            ReportError(suiteName, source, expectedOutput, actualOutputRepl, errLine, errors++ == 0);
          }

        }
        instream.Close();
        if (errors == 0)
          Console.WriteLine(suiteName + " passed");
        else {
          Console.WriteLine();
          Console.WriteLine(suiteName + " had " + errors + (errors > 1 ? " failures" : " failure"));
        }
      } catch {
        ReportError(suiteName, source, expectedOutput, actualOutput.ToString(), -1, true);
      }
      return errors == 0;
    }

    private static void ReportError(string suiteName, StringBuilder source, StringBuilder expectedOutput, string actualOutput, int errLine, bool printBanner) {
      if (printBanner) {
        Console.WriteLine();
        Console.WriteLine();
        Console.WriteLine("----------------------------- " + suiteName + " failed -----------------------------");
      }
      Console.WriteLine();
      if (errLine > 0)
        Console.WriteLine("*** source({0}):", errLine);
      else
        Console.WriteLine("*** source:");
      if (source != null) {
        Console.WriteLine(source);
        Console.WriteLine();
      }
      Console.WriteLine("*** actual output:");
      Console.WriteLine(actualOutput);
      Console.WriteLine("***");
      Console.WriteLine();
      Console.WriteLine("*** expected output:");
      if (expectedOutput != null) {
        Console.WriteLine(expectedOutput);
        Console.WriteLine("***");
        Console.WriteLine();
      }
    }

    private static int RunTest(CciErrorHandler errorHandler, string suiteName, string fileNameWithoutExt,
                               string test, VccOptions commandLineOptions, List<string> compilerParameters) {

      VccCommandLineHost.ErrorCount = 0;
      string fileNameC = fileNameWithoutExt + ".c";
      string fileNameI = fileNameWithoutExt + ".i";
      StreamWriter tempStreamWriter = new StreamWriter(fileNameC);
      tempStreamWriter.Write(test);
      tempStreamWriter.Close();

      VccOptions options = new VccOptions();
      options.CopyFrom(commandLineOptions);

      if (compilerParameters != null)
        options = OptionParser.ParseCommandLineArguments(VccCommandLineHost.dummyHostEnvironment, compilerParameters, options);

      options.NoPreprocessor = false;
      options.CheckedArithmetic = true;
      options.RunTestSuite = true;
      options.FileNames = new List<string> { fileNameC };

      HostEnvironment hostEnvironment = new HostEnvironment(options.PointerSize);
      hostEnvironment.Errors += errorHandler.HandleErrors;

      bool errorsInPreprocessor;
      CCompilerHelper.Preprocess(options, out errorsInPreprocessor);
      StreamReader tempStreamReader = new StreamReader(fileNameI);
      test = tempStreamReader.ReadToEnd();
      tempStreamReader.Close();
      File.Delete(fileNameC);
      if (!options.KeepPreprocessorFiles) File.Delete(fileNameI);

      IName name = hostEnvironment.NameTable.GetNameFor(suiteName);
      List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
      List<IModuleReference> moduleReferences = new List<IModuleReference>();
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity));
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.VccRuntimeAssemblyIdentity));
      VccAssembly/*?*/ assem = null;    
      if (hostEnvironment.previousDocument == null || compilerParameters == null ||
          !compilerParameters.Contains("/incremental"))
      {
        List<VccSourceDocument> programSources = new List<VccSourceDocument>(1);
        assem = new VccAssembly(name, "", hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
        var helper = new VccCompilationHelper(assem.Compilation);
        programSources.Add(hostEnvironment.previousDocument = new VccSourceDocument(helper, name, "", test));
      }
      VccCommandLineHost.ResetStartTime();
      return VccCommandLineHost.Felt2Cast2Plugin("testcase", options, hostEnvironment, assem);
    }

    public static StringBuilder testrunTimeStats = new StringBuilder();

    class TestRunnerMT
    {
      readonly VccOptions commandLineOptions;
      readonly int threadCount;
      readonly Queue<FileInfo> queue = new Queue<FileInfo>();
      readonly object lkQueue = new object();
      readonly object lkOutput = new object();
      long maxBytesPerProcess;
      int errorCount;
      int runningThreadCount;
      int totalTestCount;
      int failedTestCount;
      int passedTestCount;
      StreamWriter globalLogFile;

      public TestRunnerMT(int threadCount, VccOptions commandLineOptions) {
        this.threadCount = threadCount;
        this.commandLineOptions = commandLineOptions;
      }

      public void Queue(FileInfo job) {
        this.queue.Enqueue(job);
      }

      private System.Diagnostics.ProcessStartInfo VccStartInfo(IEnumerable<FileInfo> jobs) {
        System.Diagnostics.ProcessStartInfo result = new System.Diagnostics.ProcessStartInfo ();
        foreach (var arg in this.commandLineOptions.HandledOptions) {
          var opt = arg.Substring(1);
          if (opt.StartsWith("smt") || opt.StartsWith("suitemt")) continue;
          result.Arguments += " " + arg;
        }
        foreach (var job in jobs) { result.Arguments += " \"" + job.FullName + "\""; }        
        result.CreateNoWindow = true;
        result.FileName = typeof(TestRunnerMT).Assembly.Location;
        result.UseShellExecute = false;
        result.RedirectStandardError = true;
        result.RedirectStandardOutput = true;
        return result;
      }

      private static void CopyFileToStream(string path, TextWriter writer) {
        string[] lines = File.ReadAllLines(path);
        foreach (var line in lines) { writer.WriteLine(line); }
      }

      class Runner
      {
        private static readonly Regex TestPassed = new Regex("^\\S+ passed$");
        private static readonly Regex TestFailed = new Regex("--- \\S+ failed ---");
        private List<string> stdout = new List<string>();
        private List<string> stderr = new List<string>();
        private TestRunnerMT parent;
        private int needFlush;
        private object dataLock = new object();

        public Runner(TestRunnerMT p)
        {
          parent = p;
        }

        private void Flush()
        {
          if (needFlush == 0) return;
          lock (parent.globalLogFile) {
            foreach (var v in stderr)
              parent.globalLogFile.WriteLine(v);
            foreach (var v in stdout)
              parent.globalLogFile.WriteLine(v);
            stderr.Clear();
            stdout.Clear();
            parent.globalLogFile.Flush();
            needFlush = 0;
          }
        }

        private void GotStdout(object sender, System.Diagnostics.DataReceivedEventArgs data)
        {
          var line = data.Data;

          if (String.IsNullOrEmpty(data.Data)) return;

          lock (dataLock) {
            if (TestPassed.IsMatch(line)) {
              Flush();
              Interlocked.Increment(ref parent.passedTestCount);
            } else if (TestFailed.IsMatch(line)) {
              Flush();
              Interlocked.Increment(ref parent.failedTestCount);
              needFlush++;
              lock (parent.lkOutput)
                Console.WriteLine(line);
            }
            stdout.Add(line);
          }
        }

        private void GotStderr(object sender, System.Diagnostics.DataReceivedEventArgs data)
        {
          var line = data.Data;
          if (String.IsNullOrEmpty(line)) return;
          lock (dataLock) {
            stderr.Add(line);
            needFlush++;
          }
        }

        public void Run(IEnumerable<FileInfo> jobs)
        {
          using (var vccProc = System.Diagnostics.Process.Start(parent.VccStartInfo(jobs))) {
            vccProc.PriorityClass = System.Diagnostics.ProcessPriorityClass.AboveNormal;
            vccProc.OutputDataReceived += this.GotStdout;
            vccProc.ErrorDataReceived += this.GotStderr;

            vccProc.BeginOutputReadLine();
            vccProc.BeginErrorReadLine();
            vccProc.WaitForExit();

            needFlush++;
            Flush();

            if (vccProc.ExitCode != 0) System.Threading.Interlocked.Increment(ref parent.errorCount);
          }
        }
      }

      private void RunJobSequence(IEnumerable<FileInfo> jobs) {
        new Runner(this).Run(jobs);
      }

      private void RunJobs() {
        System.Threading.Interlocked.Increment(ref this.runningThreadCount);
        while (true) {
          var jobSequence = new List<FileInfo>();
          long totalSize = 0;
          lock (lkQueue) {
            while (queue.Count > 0) {
              var job = queue.Peek();
              if (jobSequence.Count > 0 && totalSize + job.Length > maxBytesPerProcess)
                break;
              if (jobSequence.Count > 50)
                break;
              queue.Dequeue();
              totalSize += job.Length;
              jobSequence.Add(job);
            }
          }
          if (jobSequence.Count == 0) break;
          RunJobSequence(jobSequence);
        }
      }

      private void Report(string app)
      {
        lock(lkOutput)
          System.Console.Write("Passed {0}, failed {1}, total {2}{3}", this.passedTestCount, this.failedTestCount, this.totalTestCount, app);
      }

      private void Reporter()
      {
        while (true) {
          Report("\r");
          Thread.Sleep(500);
        }
      }

      private void SpawnRunAndJoin() {
        List<System.Threading.Thread> threads = new List<System.Threading.Thread>(this.threadCount);
        for (int i = 0; i < this.threadCount; i++) {
          System.Threading.Thread thread = new System.Threading.Thread(RunJobs);
          thread.Start();
          threads.Add(thread);
        }

        // wait for all threads to come up becaue attempting to join them before will raise an exception
        while (this.runningThreadCount < this.threadCount) System.Threading.Thread.Sleep(50);

        var reporter = new Thread(Reporter);
        reporter.Start();

        for (int i = 0; i < this.threadCount; i++)
          threads[i].Join();

        reporter.Abort();
        reporter.Join();

        Report("\r\n");
      }

      public int Run() {
        errorCount = 0;
        runningThreadCount = 0;
        long totalBytes = 0;
        foreach (var job in queue) {
          totalBytes += job.Length;
        }
        totalTestCount = queue.Count;
        maxBytesPerProcess = totalBytes / this.threadCount;
        if (maxBytesPerProcess > 10000)
          maxBytesPerProcess /= 4;
        using (globalLogFile = new StreamWriter("testsuite.log")) {
          SpawnRunAndJoin();
        }
        return errorCount;
      }
    }
  }
}
