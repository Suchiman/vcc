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
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static int Main(string[] args)
    {
      swTotal.Start();

      // reference symbol from Z3 so it gets copied
      Microsoft.Boogie.Z3.Factory x = new Microsoft.Boogie.Z3.Factory();

      startTime = GetTime();
      commandLineOptions = OptionParser.ParseCommandLineArguments(hostEnvironment, args);
      commandLineOptions.RunningFromCommandLine = true;
      errorHandler = new ErrorHandler(commandLineOptions);
      hostEnvironment.Errors += errorHandler.HandleErrors;
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

    internal static ErrorHandler ErrorHandler {
      get { return errorHandler; }
    }

    static ErrorHandler errorHandler;

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

    public static int ErrorCount {
      get { return errorCount; }
      set { errorCount = value; }
    }

    static void RunPlugin(VccOptions commandLineOptions) {
      var processedFiles = CCompilerHelper.Preprocess(commandLineOptions);
      if (errorCount > 0) return;
      using (var fnEnum = commandLineOptions.FileNames.GetEnumerator())
      using (var ppEnum = processedFiles.GetEnumerator())
      while (fnEnum.MoveNext() && ppEnum.MoveNext())
        RunPlugin(fnEnum.Current, ppEnum.Current, commandLineOptions);
    }

    public static Plugin currentPlugin;
    private static void RunPlugin(string fileName, string ppFileName, VccOptions commandLineOptions)
    {
      HostEnvironment hostEnvironment = new HostEnvironment();
      hostEnvironment.Errors += errorHandler.HandleErrors;
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
        errorHandler.FlushErrors();

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

    public static IList<String> StandardPreludeLines {
      get { return standardPreludeLines.AsReadOnly(); }
    }

    static List<String> standardPreludeLines;
    static string standardPreludeString;

    private static Program GetStandardPrelude() {

      string preludePath = PathHelper.ProbeForPrelude();
      if (standardPreludeLines == null) {
        var lines = File.ReadAllLines(preludePath, Encoding.UTF8);
        standardPreludeLines = new List<string>(lines);
        standardPreludeString = String.Join(Environment.NewLine, lines);
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

    internal static string TokenLocation(IToken tok) {
      if (commandLineOptions == null || commandLineOptions.NoPreprocessor)
        return string.Format("{0}({1},{2})", tok.filename, tok.line, tok.col);
      else
        return string.Format("{0}({1})", tok.filename, tok.line);
    }

    internal static double GetTime() {
      return System.Environment.TickCount / 1000.0;
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
