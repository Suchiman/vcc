//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using Microsoft.Boogie;
using Microsoft.Boogie.AbstractInterpretation;
using Microsoft.Cci.Ast;
using Microsoft.Research.Vcc;
using Microsoft.Cci;
using System.Text.RegularExpressions;

namespace Microsoft.Research.Vcc
{

  public class VccCommandLineHost
  {

    /// <summary>
    /// Enumeration of error codes for verification errors
    /// </summary>
    enum ErrorCode : long {
      AssertionFailed=(long)Cci.Ast.Error.ToBeDefined + 1,
      PreconditionFailed,
      PostconditionFailed,
      RelatedInformation
    };

    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static int Main(string[] args)
    {
      swTotal.Start();

      // reference symbol from Z3 so it gets copied
      Microsoft.Boogie.Z3.Factory x = new Microsoft.Boogie.Z3.Factory();

      startTime = GetTime();
      hostEnvironment.Errors += HandleErrors;
      commandLineOptions = OptionParser.ParseCommandLineArguments(hostEnvironment, args);
      commandLineOptions.RunningFromCommandLine = true;
      InitializePlugin();

      if (commandLineOptions.DisplayCommandLineHelp) {
        DisplayCommandLineHelp();
        return 0;
      }
      if (commandLineOptions.DisplayVersion) {
        DisplayVersion();
        return 0;
      }
      if (errorCount > 0) {
        System.Console.WriteLine("Exiting with 1 ({0} errors)", errorCount);
        return 1;
      }

      if (commandLineOptions.RunTestSuite)
        errorCount = TestRunner.RunTestSuite(commandLineOptions);
      else
        RunPlugin(commandLineOptions);

      swTotal.Stop();

      DumpTimes(commandLineOptions);

      if (errorCount > 0) {
        System.Console.WriteLine("Exiting with 2 ({0} errors)", errorCount);
      }

      if (commandLineOptions.PauseBeforeExit) {
        Console.WriteLine("Done. Press any key to continue . . .");
        Console.ReadKey();
      }

      return errorCount > 0 ? 2 : 0;
    }

    static PluginManager pluginManager;

    private static void PrintPluginMessage(object sender, string message)
    {
      Console.Write(message);
    }

    static Stopwatch swVisitor = new Stopwatch("FELT Visitor");
    static Stopwatch swPlugin = new Stopwatch("Total Plugin");
    static Stopwatch swPrelude = new Stopwatch("Prelude");
    static Stopwatch swTotal = new Stopwatch("Total");

    private static void InitializePlugin()
    {
      try {
        string pluginName = null;

        if (commandLineOptions.PluginOptions.Count != 0 || commandLineOptions.DisplayCommandLineHelp) {
          pluginManager = new PluginManager(commandLineOptions);
          string pluginDir = PathHelper.ProbeForPluginDir();
          if (pluginDir != null)  pluginManager.AddPluginDirectory(pluginDir);
          pluginManager.Discover();
          foreach (var opt in commandLineOptions.PluginOptions.Keys) {
            if (opt == "dir") continue;
            if (pluginName == null) {
              pluginName = opt;
            } else {
              System.Console.WriteLine("More than one plugin requested ('{0}' and '{1}')", pluginName, opt);
              errorCount++;
            }
          }

          if (pluginName != null) {
            foreach (var plugin in pluginManager.Plugins) {
              if (string.Compare(pluginName, plugin.Name(), true) == 0) {
                if (currentPlugin != null) {
                  System.Console.WriteLine("More than one plugin matches '{0}'", pluginName);
                  errorCount++;
                }
                currentPlugin = plugin;
              }
            }
            if (currentPlugin == null) {
              System.Console.WriteLine("Plugin '{0}' not found", pluginName);
              errorCount++;
            }
          }
        }

        if (currentPlugin == null) {
          currentPlugin = new VccPlugin();
        }

        currentPlugin.RegisterStopwatch(swTotal);
        currentPlugin.RegisterStopwatch(swVisitor);
        currentPlugin.RegisterStopwatch(swPlugin);
        currentPlugin.RegisterStopwatch(swPrelude);

        currentPlugin.MessageHandler.AddHandler(new Microsoft.FSharp.Control.Handler<string>(PrintPluginMessage));

        try {
          swPlugin.Start();
          currentPlugin.UseVccOptions(commandLineOptions);
          if (pluginName != null)
            currentPlugin.UseCommandLineOptions(commandLineOptions.PluginOptions[pluginName]);
        } finally {
          swPlugin.Stop();
        }

      } catch (System.Reflection.ReflectionTypeLoadException e) {
        foreach (Exception ex in e.LoaderExceptions) {
          Console.WriteLine(ex.Message);
          Console.WriteLine(ex.StackTrace);
        }
      }
    }

    private static VccOptions commandLineOptions;
    private static double startTime;
    internal static HostEnvironment hostEnvironment = new HostEnvironment();

    // 9000-9099 - warnings mapped in this file
    // 9100-9199 - warning generated in the translator
    // 9500-9599 - errors mapped in this file
    // 9600-9699 - errors generated in translator
    private static string ErrorCodeToString(ErrorCode errCode) {
      long id;
      switch (errCode) {
        case ErrorCode.AssertionFailed:
          id = 9500; break;
        case ErrorCode.PostconditionFailed:
          id = 9501; break;
        case ErrorCode.PreconditionFailed:
          id = 9502; break;
        case ErrorCode.RelatedInformation:
          id = 9599; break;
        default:
          id = (long)errCode; break;
      }
      return "VC" + id.ToString("0000");
    }

    private static void DisplayCommandLineHelp() {
      System.Resources.ResourceManager rm = new System.Resources.ResourceManager("Microsoft.Research.Vcc.Host.ErrorMessages", typeof(VccCommandLineHost).Assembly);
      Console.Out.WriteLine(rm.GetString("Usage"));
      if (pluginManager != null && pluginManager.Plugins != null) {
        foreach (var plugin in pluginManager.Plugins) {
          Console.WriteLine();
          Console.WriteLine("--------------------------- Plugin: {0} ---------------------------", plugin.Name());
          Console.WriteLine();
          Console.WriteLine(plugin.Help());
          Console.WriteLine();
        }
      }
    }

    private static void DisplayVersion() {
      string fileVersionString = System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion;
      if (commandLineOptions.XmlFormatOutput) {
        Console.WriteLine("<version>" + fileVersionString + "</version>");
      } else {
        System.Resources.ResourceManager rm = new System.Resources.ResourceManager("Microsoft.Research.Vcc.Host.ErrorMessages", typeof(VccCommandLineHost).Assembly);
        Console.WriteLine(rm.GetString("Version"), fileVersionString);
      }
    }

    static int errorCount;

    public static void ResetErrorCount() {
      errorCount = 0;
    }

    static void RunPlugin(VccOptions commandLineOptions) {
      var processedFiles = Preprocess(commandLineOptions);
      if (errorCount > 0) return;
      using (var fnEnum = commandLineOptions.FileNames.GetEnumerator())
      using (var ppEnum = processedFiles.GetEnumerator())
      while (fnEnum.MoveNext() && ppEnum.MoveNext())
        RunPlugin(fnEnum.Current, ppEnum.Current, commandLineOptions);
    }

    private static Plugin currentPlugin;
    private static void RunPlugin(string fileName, string ppFileName, VccOptions commandLineOptions)
    {
      HostEnvironment hostEnvironment = new HostEnvironment();
      hostEnvironment.Errors += HandleErrors;
      IName assemName = hostEnvironment.NameTable.GetNameFor(Path.GetFileNameWithoutExtension(fileName));
      IName docName = hostEnvironment.NameTable.GetNameFor(Path.GetFileName(fileName));
      List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
      List<IModuleReference> moduleReferences = new List<IModuleReference>();
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity));
      assemblyReferences.Add(hostEnvironment.LoadAssembly(hostEnvironment.VccRuntimeAssemblyIdentity));
      StreamReader instream = File.OpenText(ppFileName);
      List<VccSourceDocument> programSources = new List<VccSourceDocument>(1);
      VccAssembly assem = new VccAssembly(assemName, Path.GetFullPath(fileName), hostEnvironment, commandLineOptions, assemblyReferences, moduleReferences, programSources);
      VccCompilationHelper helper = new VccCompilationHelper(assem.Compilation);
      programSources.Add(new VccSourceDocument(helper, docName, Path.GetFullPath(fileName), instream));
      Felt2Cast2Plugin(fileName, commandLineOptions, hostEnvironment, assem);
    }

    private static string FunctionOrTypeRoot(string name) {
      name = name.Replace(':', '#');
      int pos = name.IndexOf('#');
      if (pos <= 0) return name;
      return name.Substring(0, pos);
    }

    public static void ResetStartTime() {
      startTime = GetTime();
    }

    internal static int Felt2Cast2Plugin(string fileName, VccOptions commandLineOptions, HostEnvironment hostEnvironment, VccAssembly assem)
    {
      try {
        
        Helper.Env helperenv;
        FSharp.Collections.FSharpList<CAST.Top> res;

        try {
          swVisitor.Start();
          helperenv = new Microsoft.Research.Vcc.Helper.Env(hostEnvironment, commandLineOptions);
          var visitor = new Microsoft.Research.Vcc.Visitor(assem.Compilation.ContractProvider, helperenv);

          try {
            if (commandLineOptions.AggressivePruning && (commandLineOptions.Functions.Count > 0 || commandLineOptions.FunctionsWithExactName.Count > 0)) {
              var pruningRoots = new List<string>();
              pruningRoots.AddRange(commandLineOptions.Functions.ConvertAll<string>(FunctionOrTypeRoot));
              pruningRoots.AddRange(commandLineOptions.FunctionsWithExactName.ConvertAll<string>(FunctionOrTypeRoot));
              visitor.Visit(assem, pruningRoots);
            } else
              ((ICodeVisitor)visitor).Visit(assem);
          } catch (Exception) {
            if (helperenv.ShouldDumpStack) throw;
            return 0;
          }

          res = visitor.GetResult();
        } finally {
          swVisitor.Stop();
        }

        if (errorCount > 0) return 0;

        try {
          swPlugin.Start();
          if (currentPlugin.IsModular()) {
            var fv = currentPlugin.GetFunctionVerifier(fileName, helperenv, res);
            if (helperenv.ShouldContinue && errorCount == 0)
              VerifyFunctions(commandLineOptions, assem.Name.ToString(), fv);
          } else {
            currentPlugin.Verify(fileName, helperenv, res);
          }
        } finally {
          swPlugin.Stop();
        }

        return 0;

      } catch (ProverDiedException e) {
        // we might want to do something else for this one
        System.Console.WriteLine();
        System.Console.WriteLine();
        System.Console.WriteLine(e.Message);
      } catch (UnexpectedProverOutputException e) {
        System.Console.WriteLine();
        System.Console.WriteLine();
        System.Console.WriteLine(e.Message);
      } catch (Exception e) {
        System.Console.WriteLine();
        System.Console.WriteLine();
        System.Console.WriteLine(e);
      }

      return -2;
    }

    private static StringBuilder GenerateClArgs(string fileName, VccOptions commandLineOptions) {
      StringBuilder args = new StringBuilder();
      args.Append("/nologo /TC");
      foreach (string ppOption in commandLineOptions.PreprocessorOptions)
      {
        args.Append(' ');
        args.Append(ppOption);
      }
      string/*?*/ vccHeaders = PathHelper.ProbeForVccHeaders(true);
      if (vccHeaders != null)
      {
        args.Append(" /I");
        args.Append(vccHeaders);
      }
      args.Append(" \"").Append(fileName).Append('\"');
      return args;
    }

    private static void StartClProcess(string fileName, string arguments, bool reportError, bool redirect, string outFileName) {
      ProcessStartInfo info = ConfigureStartInfoForClVersion9Or8(commandLineOptions);
      info.Arguments = arguments;
      info.CreateNoWindow = true;
      info.RedirectStandardOutput = true;
      info.RedirectStandardError = true;
      info.UseShellExecute = false;

      StringBuilder output = new StringBuilder();
      StringBuilder errors = new StringBuilder();
      StreamWriter outFile = null;
      if (redirect) {
        outFile = new StreamWriter(outFileName);
      }

      try {
        using (Process process = Process.Start(info)) {
          process.OutputDataReceived += delegate(object sender, DataReceivedEventArgs args)
          {
            if (args.Data != null) {
              if (outFile != null) {
                if (currentPlugin != null)
                  currentPlugin.WritePreProcessed(outFile, args.Data);
                else
                  outFile.WriteLine(args.Data);
              }  else
                output.Append(args.Data).Append("\r\n");
            }
          };

          process.ErrorDataReceived += delegate(object sender, DataReceivedEventArgs args)
          {
            if (args.Data != null) {
              errors.Append(args.Data).Append("\r\n");
            }
          };

          process.BeginErrorReadLine();
          process.BeginOutputReadLine();
          process.WaitForExit();

          if (currentPlugin != null) {
            currentPlugin.FinishPreProcessing(outFile);
          }

          ProcessOutput(fileName, reportError, output);
          ProcessOutput(fileName, reportError, errors);
        }
      } finally {
        if (outFile != null) outFile.Dispose();
      }
    }

    private static void ProcessOutput(string fileName, bool reportError, StringBuilder outputSB)
    {
      string output = outputSB.ToString();
      if (output.Length > 0) {
        output = output.Replace(Path.GetFileName(fileName) + "\r\n", "");
        if (output.Length > 0) Console.Write(output);
        if (reportError && (output.Contains(": error C") || output.Contains(": fatal error C")))
          errorCount++;
      }
    }

    private static void RunCompiler(string fileName, VccOptions commandLineOptions) {
      string args = GenerateClArgs(fileName, commandLineOptions).ToString();
      StartClProcess(fileName, "/c /Zs " + args, true, false, null);
    }

    private static string RunPreprocessor(string fileName, VccOptions commandLineOptions) {
      string args = GenerateClArgs(fileName, commandLineOptions).ToString();
      string outExtension = ".i";
      if (commandLineOptions.ModifiedPreprocessorFiles) outExtension += "." + System.Diagnostics.Process.GetCurrentProcess().Id.ToString();
      string outFileName = Path.ChangeExtension(fileName, outExtension);
      StartClProcess(fileName, args + " /E /D_PREFAST_ /DVERIFY /DVERIFY2 /D_USE_DECLSPECS_FOR_SAL", false, true, outFileName);
      return outFileName;
    }

    public static IEnumerable<String> Preprocess(VccOptions commandLineOptions) {
      return Preprocess(commandLineOptions, !commandLineOptions.NoCompilerRun);
    }

    public static IEnumerable<String> Preprocess(VccOptions commandLineOptions, bool runCompiler) {
      if (commandLineOptions.NoPreprocessor) return commandLineOptions.FileNames;
      string savedCurentDir = Directory.GetCurrentDirectory();
      List<String> preprocessedFiles = new List<string>();
      try {
        foreach (string fileName in commandLineOptions.FileNames) {
          Directory.SetCurrentDirectory(Path.GetDirectoryName(fileName));
          //System.Console.WriteLine("{0} run compiler", System.DateTime.Now.Second);
          if (runCompiler) {
            RunCompiler(fileName, commandLineOptions);
            //System.Console.WriteLine("{0} run compiler done", System.DateTime.Now.Second);
            if (errorCount > 0) break;
          }
          preprocessedFiles.Add(RunPreprocessor(fileName, commandLineOptions));
          //System.Console.WriteLine("{0} cpp done", System.DateTime.Now.Second);
        }
      } catch {
        if (commandLineOptions.ClPath != null)
          Console.WriteLine("Error while running preprocessor " + commandLineOptions.ClPath);
        else
          Console.WriteLine("Please install Microsoft VC++ before using VCC. If already installed, please ensure that the VS80COMNTOOLS or VS90COMNTOOLS environment variable is set.");
        errorCount++;
      } finally {
        Directory.SetCurrentDirectory(savedCurentDir);
      }

      return preprocessedFiles;
    }

    /// <summary>
    /// Determine the install location of cl.exe via the environment variables VS90COMNTOOLS and
    /// VS80COMNTOOLS and setup the start info to invoke the found instance of cl, unless an explicit 
    /// location has been given as command line option.
    /// </summary>
    private static ProcessStartInfo ConfigureStartInfoForClVersion9Or8(VccOptions commandLineOptions) {
      if (!String.IsNullOrEmpty(commandLineOptions.ClPath)) {
        ProcessStartInfo result = new ProcessStartInfo("\"" + commandLineOptions.ClPath + "\"");
        try {
          FileInfo clPath = new FileInfo(commandLineOptions.ClPath);
          string path = result.EnvironmentVariables["path"];
          result.EnvironmentVariables["path"] = path + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.FullName, @"Common7\IDE") + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.Parent.FullName, @"Common7\IDE");
          result.EnvironmentVariables["include"] = 
              Path.Combine(clPath.Directory.Parent.Parent.FullName, @"VC\INCLUDE") + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.Parent.FullName, @"VC\INCLUDE");

        } catch (Exception) { } // we only do a best effort to set the path
        return result;
      }
      else {
        string VSCOMNTOOLS = Environment.GetEnvironmentVariable("VS90COMNTOOLS");
        if (VSCOMNTOOLS == null) {
          VSCOMNTOOLS = Environment.GetEnvironmentVariable("VS80COMNTOOLS");
          if (VSCOMNTOOLS == null) {
            throw new FileNotFoundException();
          }
        }

        string vsDir = new DirectoryInfo(VSCOMNTOOLS).Parent.Parent.FullName;
        ProcessStartInfo info = new ProcessStartInfo(Path.Combine(vsDir, @"vc\bin\cl.exe"));
        string path = info.EnvironmentVariables["path"];
        info.EnvironmentVariables["path"] = path + ";" + Path.Combine(vsDir, @"Common7\IDE");
        info.EnvironmentVariables["include"] = Path.Combine(vsDir, @"VC\INCLUDE");
        return info;
      }
    }


    private static double methodVerificationTime;

    private static void VerifyFunctions(VccOptions commandLineOptions, string fileName, FunctionVerifier fver)
    {
      double beforeBoogie = GetTime();
      int numErrors = 0;
      double beforeMethods = GetTime();
      bool checkSpecificFunctions = commandLineOptions.Functions.Count > 0 || commandLineOptions.FunctionsWithExactName.Count > 0;
      List<string> foundFunctionSpecifiers = new List<string>();

      if (commandLineOptions.DumpBoogie) {
        fver.DumpInternalsToFile(fileName, true);
      }

      foreach (string funcName in fver.FunctionsToVerify()) {
        if (checkSpecificFunctions) {
          // check if this function has been requested either specifically or by prefix
          bool checkThisFunction = false;
          if (commandLineOptions.FunctionsWithExactName.Contains(funcName)) {
            checkThisFunction = true;
            foundFunctionSpecifiers.Add(funcName);
          }
          if (!checkThisFunction) {
            foreach (var fn in commandLineOptions.Functions) {
              var normalized = fn.Replace(':', '#');
              if (funcName.StartsWith(normalized) &&
                   (normalized.Length == funcName.Length || funcName[normalized.Length] == '#')) {
                checkThisFunction = true;
                foundFunctionSpecifiers.Add(fn);
                break;
              }
            }
          }
          if (!checkThisFunction) continue;
        }

        var outcome = fver.Verify(funcName);
        FlushErrors();

        if (outcome == VerificationResult.Succeeded || outcome == VerificationResult.Skipped) { } else { numErrors++; }
      }

      if (commandLineOptions.DumpBoogie) {
        fver.DumpInternalsToFile(fileName, false);
      }

      if (checkSpecificFunctions) {
        List<string> functionSpecifiers = new List<string>();
        functionSpecifiers.AddRange(commandLineOptions.Functions);
        functionSpecifiers.AddRange(commandLineOptions.FunctionsWithExactName);
        // some functions have not been encountered; warn about those
        foreach (var fn in functionSpecifiers) {
          if (!foundFunctionSpecifiers.Contains(fn)) {
            Console.WriteLine("vcc : error : '{0}' did not match any function.", fn);
            errorCount++;
          }
        }
      }

      double now = GetTime();
      methodVerificationTime += now - beforeMethods;

      fver.Close();

      if (commandLineOptions.TimeStats) {
        string xmlReport = string.Format("<time><total>{0:0.00}</total><compiler>{1:0.00}</compiler><boogie>{2:0.00}</boogie><verification>{3:0.00}</verification></time>",
                            now - startTime, beforeBoogie - startTime,
                            beforeMethods - beforeBoogie,
                            now - beforeMethods);
        xmlReport += "\r\n" + "<errors>" + numErrors.ToString() + "</errors>\r\n";
        if (commandLineOptions.RunTestSuite) {
          if (TestRunner.testrunTimeStats != null)
            TestRunner.testrunTimeStats.Append(xmlReport);
        } else if (commandLineOptions.XmlFormatOutput) {
          Console.Write(xmlReport);
        } else {
          Console.WriteLine("Time: {0:0.00}s total, {1:0.00}s compiler, {2:0.00}s Boogie, {3:0.00}s method verification.",
                            now - startTime, beforeBoogie - startTime,
                            beforeMethods - beforeBoogie,
                            now - beforeMethods);
          if (numErrors != 0)
            Console.WriteLine("Detected verification errors in {0} methods.", numErrors);
        }
      }
    }

    private static void DumpTimes(VccOptions commandLineOptions)
    {
      if (commandLineOptions.DetailedTimes) {
        foreach (var s in currentPlugin.Stopwatches) {
          Console.WriteLine(s.Display());
        }
      }
    }

    internal static Program CEVPrelude
    {
        get
        {
            // For now Boogie does not support reusing the prelude.

            //if (standardPrelude == null)
            //  standardPrelude = GetStandardPrelude();
            //return standardPrelude;
            return GetCEVPrelude();
        }
    }

    internal static Program StandardPrelude {
      get {
        // For now Boogie does not support reusing the prelude.

        //if (standardPrelude == null)
        //  standardPrelude = GetStandardPrelude();
        //return standardPrelude;

        try {
          swPrelude.Start();
          return GetStandardPrelude();
        } finally {
          swPrelude.Stop();
        }
      }
    }

    static string[] cevPreludeLines;
    static string cevPreludeString;
    public static string preludePath = "";
    private static Program GetCEVPrelude()
    {

        string headersDir = PathHelper.ProbeForVccHeaders(false);

        System.IO.Stream stream = null;
        if (cevPreludeLines == null)
        {

            stream = headersDir == null ?
                //typeof(ConvertFelt2Boogie).Assembly.GetManifestResourceStream("Microsoft.Cci.VccPrelude.bpl") :
            null : // TODO
            System.IO.File.OpenRead(preludePath);
            BoogiePL.Buffer.Fill(new System.IO.StreamReader(stream));
        }
        else
        {
            BoogiePL.Buffer.Fill(cevPreludeString);
        }

        BoogiePL.Scanner.Init(preludePath);
        Program prelude;
        int errorCount = BoogiePL.Parser.Parse(out prelude);
        if (stream != null) stream.Close();
        if (prelude == null || errorCount > 0)
        {
            System.Console.WriteLine("There were errors parsing VccPrelude.bpl.");
            return new Program();
        }
        else
        {
            if (cevPreludeString == null)
            {
                cevPreludeLines = File.ReadAllLines(preludePath, Encoding.UTF8);
                cevPreludeString = String.Join(Environment.NewLine, cevPreludeLines);
            }
            return prelude;
        }
    }

    // [Once]
    //static Program/*?*/ standardPrelude;
    static string[] standardPreludeLines;
    static string standardPreludeString;

    private static Program GetStandardPrelude() {

      string preludePath = PathHelper.ProbeForPrelude();
      if (standardPreludeLines == null) {
        standardPreludeLines = File.ReadAllLines(preludePath, Encoding.UTF8);
        standardPreludeString = String.Join(Environment.NewLine, standardPreludeLines);
      }

      BoogiePL.Buffer.Fill(standardPreludeString);
      BoogiePL.Scanner.Init(preludePath);
      Program prelude;
      int errorCount = BoogiePL.Parser.Parse(out prelude);
      if (prelude == null || errorCount > 0) {
        System.Console.WriteLine("There were errors parsing VccPrelude.bpl.");
        return new Program();
      } else {
        return prelude;
      }
    }

    internal static void ReportCounterexample(Counterexample ce, string message) {
      if (commandLineOptions != null && commandLineOptions.XmlFormatOutput) return;
      if (message != null) message = " (" + message + ")";
      else message = "";

      ReturnCounterexample/*?*/ rce = ce as ReturnCounterexample;
      if (rce != null) {
        IToken tok = rce.FailingReturn.tok;
        for (int i = rce.Trace.Length - 1; i >= 0; i--) {
          foreach (Cmd c in rce.Trace[i].Cmds) {
            AssertCmd assrt = c as AssertCmd;
            if (assrt != null) {
              NAryExpr nary = assrt.Expr as NAryExpr;
              if (nary != null) {
                FunctionCall fcall = nary.Fun as FunctionCall;
                if (fcall != null && fcall.FunctionName == "$position_marker") {
                  tok = assrt.tok;
                }
              }
            }
          }
        }
        ReportOutcomePostconditionFailed(rce.FailingEnsures.tok, tok, message);
      }
      AssertCounterexample/*?*/ ace = ce as AssertCounterexample;
      if (ace != null) {
        ReportOutcomeAssertFailed(ace.FailingAssert.tok,
          (ace.FailingAssert is LoopInvMaintainedAssertCmd ? "Loop body invariant" :
         ace.FailingAssert is LoopInitAssertCmd ? "Loop entry invariant" : "Assertion"),
         message
          );
      }
      CallCounterexample/*?*/ cce = ce as CallCounterexample;
      if (cce != null)
        ReportOutcomePreconditionFailed(cce.FailingCall.tok, cce.FailingRequires, message);
    }

    internal static string TokenLocation(IToken tok) {
      if (commandLineOptions == null || commandLineOptions.NoPreprocessor)
        return string.Format("{0}({1},{2})", tok.filename, tok.line, tok.col);
      else
        return string.Format("{0}({1})", tok.filename, tok.line);
    }

    static List<string> errors = new List<string>();

    static Dictionary<IToken, List<ErrorCode>> reportedVerificationErrors = new Dictionary<IToken, List<ErrorCode>>();

    private static bool ErrorHasBeenReported(IToken tok, ErrorCode code)
    {
      List<ErrorCode> errorsForTok;
      if (!reportedVerificationErrors.TryGetValue(tok, out errorsForTok))
      {
        errorsForTok = new List<ErrorCode>();
        reportedVerificationErrors[tok] = errorsForTok;
      }

      if (errorsForTok.Contains(code)) return true;
      errorsForTok.Add(code);
      return false;
    }

    private static bool ReportError(IToken tok, ErrorCode code, string fmt, params string[] args)
    {
      if (ErrorHasBeenReported(tok, code)) return false;
      string msg = string.Format("{0}({1},{2}) : error {3}: {4}.", 
                                 commandLineOptions.RunTestSuite ? "testcase" : tok.filename, tok.line, tok.col, 
                                 ErrorCodeToString(code), string.Format(fmt, args));
      if (commandLineOptions.RunTestSuite)
        errors.Add(msg);
      else
        Console.WriteLine(msg);

      return true;
    }

    private static void ReportRelated(IToken tok, string fmt, params string[] args)
    {
      string msg = string.Format("{0}({1},{2}) : error {3}: (related information) {4}.", 
                                 commandLineOptions.RunTestSuite ? "testcase" : tok.filename, tok.line, tok.col, 
                                 ErrorCodeToString(ErrorCode.RelatedInformation),
                                 string.Format(fmt, args));
      if (commandLineOptions.RunTestSuite)
        errors[errors.Count - 1] = errors[errors.Count - 1] + "\r\n" + msg;
      else
        Console.WriteLine(msg);
    }

    private static ErrorCode GetErrorNumber(ref string msg, ErrorCode def)
    {
      msg = RemoveWhiteSpace(msg);
      if (msg.StartsWith("#VCCERR:")) {
        int i = 8;
        while (i < msg.Length && char.IsDigit(msg[i])) i++;
        int num = int.Parse(msg.Substring(8, i - 8));
        msg = msg.Substring(i + 1);
        return (ErrorCode)num;
      } else
        return def;
    }

    private static bool IsStandaloneError(ErrorCode num)
    {
      return (int)num >= 8000 && (int)num < 8500;
    }

    private static void ReportOutcomePreconditionFailed(IToken callTok, Requires req, string addComment)
    {
      string/*?*/ comment = req.Comment;
      IToken reqTok = req.tok;
      if (comment != null) comment = ": " + comment; else comment = "";
      comment += addComment;

      // in case of testsuite, don't print the full paths to prelude
      // also skip line numbers as they change
      bool isPrelude = reqTok.filename.EndsWith("VccPrelude.bpl") || reqTok.filename.EndsWith("VccPrelude.bpl>");
      if (isPrelude && reqTok.line > 1) {
        string line = standardPreludeLines[reqTok.line - 2];
        int idx = line.IndexOf("TOKEN:");
        if (idx > 0) {
          reqTok.val = line.Substring(idx + 7);
        } else {
          line = standardPreludeLines[reqTok.line - 1];
          idx = line.IndexOf("requires");
          if (idx >= 0)
            reqTok.val = line.Substring(idx + 8);
          else
            reqTok.val = line;
        }
      }
      if (commandLineOptions != null && commandLineOptions.RunTestSuite && isPrelude) {
        reqTok.filename = "VccPrelude.bpl";
        reqTok.line = 0;
        reqTok.col = 0;
      }

      string reqMsg = reqTok.val;
      ErrorCode errNo = GetErrorNumber(ref reqMsg, ErrorCode.PreconditionFailed);

      if (IsStandaloneError(errNo)) {
        ReportError(callTok, errNo, "{0} (in call '{1}'){2}", reqMsg, RemoveWhiteSpace(callTok.val), comment);
      } else {
        if (ReportError(callTok, errNo, "Call '{0}' did not verify{1}", RemoveWhiteSpace(callTok.val), comment))
          ReportRelated(reqTok, "Precondition: '{0}'", reqMsg);
      }
    }

    private static void ReportOutcomeAssertFailed(IToken assertTok, string kind, string comment)
    {
      string msg = assertTok.val;
      ErrorCode errNo = GetErrorNumber(ref msg, ErrorCode.AssertionFailed);
      if (IsStandaloneError(errNo))
        ReportError(assertTok, errNo, "{0}{1}", msg, comment);
      else
        ReportError(assertTok, errNo, "{0}{2} '{1}' did not verify", kind, msg, comment);

      BoogieToken btok = assertTok as BoogieToken;
      if (btok != null && btok.Related != null)
        ReportRelated(btok.Related, btok.Related.val);
    }

    private static void ReportOutcomePostconditionFailed(IToken ensTok, IToken retTok, string comment) {
      string msg = ensTok.val;
      ErrorCode errNo = GetErrorNumber(ref msg, ErrorCode.PostconditionFailed);
      if (retTok.line == 0) retTok = ensTok;
      if (ensTok.line == 0) ensTok = retTok;

      if (IsStandaloneError(errNo))
        ReportError(retTok, errNo, "{0}{1}", msg, comment);
      else {
        if (ReportError(retTok, errNo, "Post condition{0} '{1}' did not verify", comment, msg) && retTok.line != 0)
          ReportRelated(ensTok, "Location of post condition");
      }
    }

    static internal void ReportOutcomeMethodSummary(VC.VCGen.Outcome outcome, string addInfo, string methodName, double startTime, IEnumerable<string> proverWarnings) {
      if (outcome != VC.VCGen.Outcome.Correct) errorCount++;
      if (!commandLineOptions.XmlFormatOutput) {
        string result = OutcomeToDescription(outcome);
        if (addInfo != null)
          result = addInfo;
        if (commandLineOptions != null && commandLineOptions.VCLikeErrorMessages) {
          if (outcome == VC.VCGen.Outcome.Correct)
            Console.Write("vcc : Verification of {0} {1}.", methodName, result);
          else
            Console.Write("vcc : error : Verification of {0} {1}.", methodName, result);
          if (commandLineOptions.TimeStats)
            Console.Write(" [{0:0.00}]", GetTime() - startTime);
          Console.WriteLine();
        } else {
          if (commandLineOptions != null && commandLineOptions.TimeStats) {
            double t = GetTime() - startTime;
            if (commandLineOptions.RunTestSuite) {
              if (TestRunner.testrunTimeStats != null)
                TestRunner.testrunTimeStats.AppendFormat("<method name=\"{0}\" time=\"{1:0.00}\"/>\r\n", methodName, t);
            } else
              Console.Write("[{0:0.00}s] ", t);
          }
          Console.WriteLine("{0}.", result);
        }
      }

      if (!commandLineOptions.RunTestSuite) {
        foreach (var proverWarning in proverWarnings) {
          Console.WriteLine("Prover warning: {0}", proverWarning);
        }
      }
    }

    private static void FlushErrors()
    {
      if (errors.Count > 0) {
        errors.Sort(string.CompareOrdinal);
        foreach (string e in errors) Console.WriteLine(e);
        errors.Clear();
      }
    }

    private static string OutcomeToDescription(VC.VCGen.Outcome outcome) {
      switch (outcome) {
        case VC.VCGen.Outcome.Correct:
          return "succeeded";
        case VC.VCGen.Outcome.Inconclusive:
          return "was inconslusive";
        case VC.VCGen.Outcome.TimedOut:
          return "timed out";
        case VC.VCGen.Outcome.Errors:
          return "failed";
        case VC.VCGen.Outcome.OutOfMemory:
          return "ran out of memory";
        default:
          return "returned an unknown result";
      }
    }

    static long ErrorToId(long code)
    {
      switch ((Cci.Ast.Error)code) {
        case Cci.Ast.Error.ExpressionStatementHasNoSideEffect:
          return 9001;
      }

      switch ((Vcc.Error)code) {
        case Vcc.Error.DiscardedContractAtDefinition:
          return 9002;
        case Vcc.Error.SizeOfUnknown:
          return 9003; 
      }

      return code;
    }

    private static Dictionary<string, bool> reportedErrors = new Dictionary<string, bool>();

    public  static void ResetReportedErrors()
    {
      reportedErrors.Clear();
      reportedVerificationErrors.Clear();
    }

    internal static void HandleErrors(object sender, Microsoft.Cci.ErrorEventArgs args) {
      foreach (IErrorMessage error in args.Errors) {
        ISourceLocation/*?*/ sourceLocation = error.Location as ISourceLocation;
        if (sourceLocation == null) continue;
        bool isError = !error.IsWarning || commandLineOptions.WarningsAsErrors;
        if (isError) errorCount++;
        CompositeSourceDocument/*?*/ compositeDocument = sourceLocation.SourceDocument as CompositeSourceDocument;
        if (compositeDocument != null) {
          foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sourceLocation)) {
            sourceLocation = sl;
            break;
          }
        }
        IPrimarySourceLocation/*?*/ primarySourceLocation = sourceLocation as IPrimarySourceLocation;
        if (primarySourceLocation == null) {
          Console.WriteLine(error.Message);
          continue;
        }
        string docName = primarySourceLocation.SourceDocument.Location;
        if (docName == null) docName = primarySourceLocation.SourceDocument.Name.Value;
        int startLine = primarySourceLocation.StartLine;
        int startColumn = primarySourceLocation.StartColumn;
        int endLine = primarySourceLocation.EndLine;
        int endColumn = primarySourceLocation.EndColumn;
        IncludedSourceLocation/*?*/ includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
        if (includedSourceLocation != null) {
          docName = includedSourceLocation.OriginalSourceDocumentName;
          if (docName != null) docName = docName.Replace("\\\\", "\\");
          startLine = includedSourceLocation.OriginalStartLine;
          endLine = includedSourceLocation.OriginalEndLine;
        }
        long id = error.IsWarning ? ErrorToId(error.Code) : error.Code;
        if (commandLineOptions.DisabledWarnings.ContainsKey(id)) return;

        StringBuilder msgBldr = new StringBuilder();
        msgBldr.AppendFormat("{0}({1},{2})", commandLineOptions.RunTestSuite ? "testcase" : docName, startLine, startColumn);
        //if (commandLineOptions == null || (commandLineOptions.NoPreprocessor && !commandLineOptions.VCLikeErrorMessages))
        //  msgBldr.AppendFormat("-({0},{1})", endLine, endColumn);
        msgBldr.AppendFormat(" : {0} VC{1:0000}: {2}", isError ? "error" : "warning", commandLineOptions.RunTestSuite && id < 9000 ? 0 : id, error.Message);

        string msg = msgBldr.ToString();
        if (reportedErrors.ContainsKey(msg)) return;
        Console.WriteLine(msg);
        reportedErrors[msg] = true;

        string firstErrFile = docName;
        int firstErrLine = startLine;

        foreach (ILocation relatedLocation in error.RelatedLocations) {
          ISourceLocation/*?*/ sloc = relatedLocation as ISourceLocation;
          if (sloc != null) {
            compositeDocument = sloc.SourceDocument as CompositeSourceDocument;
            if (compositeDocument != null) {
              foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sloc)) {
                sloc = sl;
                break;
              }
            }
            primarySourceLocation = sloc as IPrimarySourceLocation;
            if (primarySourceLocation == null) continue;
            docName = primarySourceLocation.SourceDocument.Location;
            if (docName == null) docName = primarySourceLocation.SourceDocument.Name.Value;
            startLine = primarySourceLocation.StartLine;
            startColumn = primarySourceLocation.StartColumn;
            endLine = primarySourceLocation.EndLine;
            endColumn = primarySourceLocation.EndColumn;
            includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
            if (includedSourceLocation != null) {
              docName = includedSourceLocation.OriginalSourceDocumentName;
              if (docName != null) docName = docName.Replace("\\\\", "\\");
              startLine = includedSourceLocation.OriginalStartLine;
              endLine = includedSourceLocation.OriginalEndLine;
            }
            if (docName != firstErrFile || firstErrLine != startLine) {
              Console.Write("{0}({1},{2})", commandLineOptions.RunTestSuite ? "testcase" : docName, startLine, startColumn);
              if (commandLineOptions == null || (commandLineOptions.NoPreprocessor && !commandLineOptions.VCLikeErrorMessages))
                Console.Write("-({0},{1})", endLine, endColumn);
              Console.WriteLine(" : (Location of symbol related to previous {0}.)", isError ? "error" : "warning");
            }
          }
          //TODO: deal with non source locations
        }
      }
    }

    internal static double GetTime() {
      return System.Environment.TickCount / 1000.0;
    }

    private static string RemoveWhiteSpace(string str)
    {
      return String.Join(" ", Array.FindAll(str.Split(new char[] { '\n', '\r', '\t', ' ' }), s => !String.IsNullOrEmpty(s)));
    }
  }

  class BoogieErrorSink : IErrorSink
  {

    public BoogieErrorSink(bool noPreprocessor)
    {
      this.noPreprocessor = noPreprocessor;
    }

    readonly bool noPreprocessor;

    public void Error(IToken tok, string msg)
    {
      if (this.noPreprocessor)
        Console.Out.WriteLine("({0},{1}): verification: {2}", tok.line, tok.col, msg);
      else
        Console.Out.WriteLine("({0}): verification: {1}", tok.line, msg);
    }
  }


  internal class HostEnvironment : SourceEditHostEnvironment {

    internal HostEnvironment()
      : base(new NameTable(), 8) {
      this.peReader = new PeReader(this);
      string/*?*/ loc = typeof(object).Assembly.Location;
      if (loc == null) loc = "";
      System.Reflection.AssemblyName mscorlibName = new System.Reflection.AssemblyName(typeof(object).Assembly.FullName);
      var tempMscorlibIdentity = new AssemblyIdentity(this.NameTable.GetNameFor(mscorlibName.Name), "", mscorlibName.Version, mscorlibName.GetPublicKeyToken(), loc);
      this.RegisterAsLatest(this.peReader.OpenAssembly(BinaryDocument.GetBinaryDocumentForFile(tempMscorlibIdentity.Location, this), out this.mscorlibIdentity));
      loc = typeof(Microsoft.Research.Vcc.Runtime).Assembly.Location;
      if (loc == null) loc = "";
      System.Reflection.AssemblyName runtimeName = new System.Reflection.AssemblyName(typeof(Microsoft.Research.Vcc.Runtime).Assembly.FullName);
      var tempVccRuntimeAssemblyIdentity = new AssemblyIdentity(this.NameTable.GetNameFor(runtimeName.Name), "", runtimeName.Version, runtimeName.GetPublicKeyToken(), loc);
      this.RegisterAsLatest(this.peReader.OpenAssembly(BinaryDocument.GetBinaryDocumentForFile(tempVccRuntimeAssemblyIdentity.Location, this), out this.vccRuntimeAssemblyIdentity));
    }

    internal IUnit GetIncrementalUnit(string newText) {
      string[] lines = newText.Split('$');
      if (lines.Length != 4) return Dummy.Unit;
      string prefix = lines[0];
      string textToReplace = lines[1];
      string replacement = lines[2];
      IVccSourceDocument updatedDocument = this.previousDocument.GetUpdatedDocument(prefix.Length, textToReplace.Length, replacement);
      return updatedDocument.VccCompilationPart.Compilation.Result;
    }
    
    internal VccSourceDocument/*?*/ previousDocument;

    protected override AssemblyIdentity GetCoreAssemblySymbolicIdentity() {
      return this.mscorlibIdentity;
    }    
    readonly AssemblyIdentity mscorlibIdentity;

    public AssemblyIdentity VccRuntimeAssemblyIdentity {
      get { return this.vccRuntimeAssemblyIdentity; }
    }
    readonly AssemblyIdentity vccRuntimeAssemblyIdentity;

    public override IUnit LoadUnitFrom(string location) {
      IUnit result = this.peReader.OpenModule(BinaryDocument.GetBinaryDocumentForFile(location, this));
      this.RegisterAsLatest(result);
      return result;
    }

    Microsoft.Cci.PeReader peReader;

    public override void ReportErrors(Microsoft.Cci.ErrorEventArgs errorEventArguments) {
      this.SynchronousReportErrors(errorEventArguments);
    }
  }
}
