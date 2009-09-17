//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Cci;
using System.IO;
using System.Text.RegularExpressions;

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
      int errs = 0;
      if (commandLineOptions.TimeStats)
        Console.WriteLine("<![CDATA[");
      try {
        foreach (string fileName in commandLineOptions.FileNames)
          if (!RunTestSuite(fileName, commandLineOptions))
            errs++;
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

        foreach (FileInfo fi in new DirectoryInfo(fileName).GetFiles("*", SearchOption.TopDirectoryOnly)) {
          if (fi.Name.StartsWith(".")) continue;
          if (fi.Name.Contains(vccSplitSuffix)) continue;
          if (fi.Extension == ".i" || fi.Extension == ".bpl" || fi.Extension == ".h") continue;
          if (!TestRunner.RunTestSuite(fi.DirectoryName, fi.Name, new StreamReader(fi.Open(FileMode.Open, FileAccess.Read)), commandLineOptions, !fi.DirectoryName.Contains("WithErrors")))
            errorCount++;
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
        return TestRunner.RunTestSuite(Path.GetDirectoryName(fileName), fileName, File.OpenText(fileName), commandLineOptions, true);
      }
    }

    public static string currentTestcaseName;

    static bool RunTestSuite(string directoryName, string suiteName, TextReader instream, VccOptions commandLineOptions, bool verify) {
      System.Diagnostics.Debug.Listeners.Remove("Default");
      HostEnvironment hostEnvironment = new HostEnvironment();
      hostEnvironment.Errors += VccCommandLineHost.ErrorHandler.HandleErrors;
      StringBuilder source = null;
      StringBuilder expectedOutput = null;
      StringBuilder actualOutput = null;
      List<string> suiteParameters = new List<string>();
      List<string> compilerParameters = null;
      List<string> testCaseParameters = null;
      int errors = 0;
      int testCaseCount = 0;
      try {
        int ch = instream.Read();
        int line = 1;
        while (ch >= 0) {
          compilerParameters = new List<string>(suiteParameters);
          bool skipTest = false;
          if (ch == '`') {
            ch = instream.Read();
            bool parametersAreForEntireSuite = false;
            if (ch == '`') {
              parametersAreForEntireSuite = true;
              ch = instream.Read();
            }
            while (ch == '/') {
              //compiler parameters
              StringBuilder cParam = new StringBuilder();
              do {
                cParam.Append((char)ch);
                ch = instream.Read();
              } while (ch != '/' && ch != 0 && ch != 10 && ch != 13);
              for (int i = cParam.Length - 1; i >= 0; i--) {
                if (!Char.IsWhiteSpace(cParam[i])) break;
                cParam.Length = i;
              }
              string cp = cParam.ToString();
              compilerParameters.Add(cp);
            }
            if (parametersAreForEntireSuite)
              suiteParameters.AddRange(compilerParameters);
            if (ch == 13) ch = instream.Read();
            if (ch == 10) {
              line++;
              ch = instream.Read();
              if (parametersAreForEntireSuite && ch == '`') continue;
            }
          }
          if (ch == ':') {
            ch = instream.Read();
            while (ch == '=') {
              //test case parameters
              StringBuilder tcParam = new StringBuilder();
              ch = instream.Read(); //discard =
              while (ch != '=' && ch != 0 && ch != 10 && ch != 13) {
                tcParam.Append((char)ch);
                ch = instream.Read();
              }
              for (int i = tcParam.Length - 1; i >= 0; i--) {
                if (!Char.IsWhiteSpace(tcParam[i])) break;
                tcParam.Length = i;
              }
              if (testCaseParameters == null) testCaseParameters = new List<string>();
              testCaseParameters.Add(tcParam.ToString());
            }
            if (ch == 13) ch = instream.Read();
            if (ch == 10) {
              ch = instream.Read();
              line++;
            }
          }
          source = new StringBuilder();
          while (ch >= 0 && ch != '`') {
            source.Append((char)ch);
            ch = instream.Read();
            if (ch == 10) line++;
          }
          if (ch < 0) {
            Console.WriteLine("The last test case in the suite has not been provided with expected output");
            errors++;
            break;
          }
          ch = instream.Read();
          if (ch == 13) ch = instream.Read();
          if (ch == 10) {
            line++;
            ch = instream.Read();
          }
          int errLine = line;
          expectedOutput = new StringBuilder();
          while (ch >= 0 && ch != '`') {
            expectedOutput.Append((char)ch);
            ch = instream.Read();
            if (ch == 10) line++;
          }
          if (expectedOutput.Length > 0 && expectedOutput[expectedOutput.Length - 1] == 10)
            expectedOutput.Length -= 1;
          if (expectedOutput.Length > 0 && expectedOutput[expectedOutput.Length - 1] == 13)
            expectedOutput.Length -= 1;
          ch = instream.Read();

          // Allow for:
          // void foo() {}
          // /*`
          // Verification of foo succeeded.
          // `*/
          if (ch == '*') {
            ch = instream.Read();
            if (ch != '/') {
              Console.WriteLine("Expecting / after `*");
              errors++;
              break;
            }
            while (source.Length > 0 && source[source.Length - 1] == 10 || source[source.Length - 1] == 13)
              source.Length -= 1;
            if (source.Length > 2 && source[source.Length - 1] == '*' && source[source.Length - 2] == '/')
              source.Length -= 2;
            ch = instream.Read();
          }

          if (ch == 13) ch = instream.Read();
          if (ch == 10) {
            ch = instream.Read();
            line++;
          }
          if (skipTest) continue;
          actualOutput = new StringBuilder();
          TextWriter savedOut = Console.Out;
          Console.SetOut(new StringWriter(actualOutput));
          System.Diagnostics.TextWriterTraceListener myWriter = new System.Diagnostics.TextWriterTraceListener(System.Console.Out);
          System.Diagnostics.Debug.Listeners.Add(myWriter);

          ++testCaseCount;
          string suiteNameWithoutExt = Path.GetFileNameWithoutExtension(suiteName);
          string fileNameWithoutExt = directoryName + Path.DirectorySeparatorChar + suiteNameWithoutExt + vccSplitSuffix + testCaseCount;
          currentTestcaseName = Path.GetFileName(string.Format("{0}.{1:00}", suiteNameWithoutExt, testCaseCount));

          StringBuilder backupTimeStats = testrunTimeStats;
          if (suiteName.EndsWith(".c"))
            testrunTimeStats.AppendFormat("<file name=\"{0}/{1}.{2}\">\r\n", Path.GetFileName(directoryName), suiteName, testCaseCount);
          else
            testrunTimeStats = null;
          try {
            int returnCode = RunTest(hostEnvironment, suiteNameWithoutExt, fileNameWithoutExt, source.ToString(), actualOutput, commandLineOptions, compilerParameters, testCaseParameters, verify);
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
          compilerParameters = null;
          testCaseParameters = null;
          Console.SetOut(savedOut);
          System.Diagnostics.Debug.Listeners.Remove(myWriter);
          if (actualOutput.Length > 0 && actualOutput[actualOutput.Length - 1] == 10)
            actualOutput.Length -= 1;
          if (actualOutput.Length > 0 && actualOutput[actualOutput.Length - 1] == 13)
            actualOutput.Length -= 1;
          Regex rx = new Regex(@"[a-zA-Z]:\\.*?\\(.*)" + vccSplitSuffix + @"[0-9]*.c\(");
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

    private static int RunTest(HostEnvironment hostEnvironment, string suiteName, string fileNameWithoutExt,
                               string test, StringBuilder actualOutput, VccOptions commandLineOptions, List<string> compilerParameters, List<string> testCaseParameters,
                               bool verify) {

      VccCommandLineHost.ErrorCount = 0;
      bool keepPreprocessorOutput = false;
      string fileNameC = fileNameWithoutExt + ".c";
      string fileNameI = fileNameWithoutExt + ".i";
      StreamWriter tempStreamWriter = new StreamWriter(fileNameC);
      tempStreamWriter.Write(test);
      tempStreamWriter.Close();
      VccOptions options = new VccOptions();
      options.DumpBoogie = commandLineOptions.DumpBoogie;
      options.NoPreprocessor = false;
      options.CheckedArithmetic = true;
      options.RunTestSuite = true;
      options.FileNames = new List<string> { fileNameC };

      if (compilerParameters != null) {
        foreach (string cp in compilerParameters) {
          if (cp.StartsWith("/functions:"))
            options.Functions.AddRange(cp.Substring(11, cp.Length - 11).Split(','));

          if (cp == "/a" || cp == "/aggressivepruning") options.AggressivePruning = true;
          if (cp == "/keepppoutput") keepPreprocessorOutput = true;
        }
      }

      // TODO maybe copy more stuff
      options.Z3Options.AddRange(commandLineOptions.Z3Options);
      options.BoogieOptions.AddRange(commandLineOptions.BoogieOptions);
      options.TimeStats = commandLineOptions.TimeStats;
      options.VcOpt.AddRange(commandLineOptions.VcOpt);

      VccCommandLineHost.ErrorHandler.ResetReportedErrors();
      bool errorsInPreprocessor;
      CCompilerHelper.PreprocessAndCompile(options, true, out errorsInPreprocessor);
      StreamReader tempStreamReader = new StreamReader(fileNameI);
      test = tempStreamReader.ReadToEnd();
      tempStreamReader.Close();
      File.Delete(fileNameC);
      if (!keepPreprocessorOutput) File.Delete(fileNameI);

      IName name = hostEnvironment.NameTable.GetNameFor(suiteName);
      List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
      List<IModuleReference> moduleReferences = new List<IModuleReference>();
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity));
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.VccRuntimeAssemblyIdentity));
      IUnit unit;
      VccAssembly/*?*/ assem = null;
      VccCompilationHelper helper;
      if (hostEnvironment.previousDocument != null && compilerParameters.Contains("/incremental")) {
        unit = hostEnvironment.GetIncrementalUnit(test);
        helper = (VccCompilationHelper)hostEnvironment.previousDocument.VccCompilationPart.Helper;
      } else {
        List<VccSourceDocument> programSources = new List<VccSourceDocument>(1);
        assem = new VccAssembly(name, "", hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
        helper = new VccCompilationHelper(assem.Compilation);
        programSources.Add(hostEnvironment.previousDocument = new VccSourceDocument(helper, name, "", test));
        unit = assem;
      }
      VccCommandLineHost.ResetStartTime();
      return VccCommandLineHost.Felt2Cast2Plugin("testcase", options, hostEnvironment, assem);
    }

    public static StringBuilder testrunTimeStats = new StringBuilder();
  }
}
