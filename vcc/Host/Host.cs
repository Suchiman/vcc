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
    public static int Main(string[] args)
    {
      swTotal.Start();

      // reference symbol from Z3 so it gets copied
      Microsoft.Boogie.Z3.Factory x = new Microsoft.Boogie.Z3.Factory();

      startTime = GetTime();
      cciErrorHandler = new CciErrorHandler();
      dummyHostEnvironment.Errors += cciErrorHandler.HandleErrors;
      var commandLineOptions = OptionParser.ParseCommandLineArguments(dummyHostEnvironment, args);
      commandLineOptions.RunningFromCommandLine = true;
      standardPreludePath = commandLineOptions.PreludPath;
      cciErrorHandler.CommandLineOptions = commandLineOptions;
      verificationErrorHandler = new VerificationErrorHandler(commandLineOptions);
      
      if (commandLineOptions.DisplayCommandLineHelp) {
        DisplayCommandLineHelp();
        return 0;
      }
      if (commandLineOptions.DisplayVersion) {
        DisplayVersion(commandLineOptions.XmlFormatOutput);
        return 0;
      }

      if (errorCount > 0) {
        Console.WriteLine("Exiting with 1 - error parsing arguments.");
        return 1;
      }

      if ((currentPlugin = InitializePlugin(commandLineOptions)) == null) {
        Console.WriteLine("Exiting with 2 - error initializing plugin.");
        return 2;
      }

      if (commandLineOptions.RunTestSuite)
        errorCount = TestRunner.RunTestSuite(commandLineOptions);
      else
        RunPlugin(commandLineOptions);

      swTotal.Stop();

      DumpTimes(commandLineOptions);

      int retVal = 0;

      if (errorCount > 0) {
        Console.WriteLine("Exiting with 3 ({0} error(s).)", errorCount);
        retVal = 3;
      }

      if (commandLineOptions.PauseBeforeExit) {
        Console.WriteLine("Done. Press any key to continue . . .");
        Console.ReadKey();
      }

      return retVal;
    }

    internal static VerificationErrorHandler ErrorHandler {
      get { return verificationErrorHandler; }
    }

    static VerificationErrorHandler verificationErrorHandler;
    static CciErrorHandler cciErrorHandler;
    static PluginManager pluginManager;

    private static void PrintPluginMessage(object sender, string message)
    {
      Console.Write(message);
    }

    static Stopwatch swVisitor = new Stopwatch("FELT Visitor");
    static Stopwatch swPlugin = new Stopwatch("Total Plugin");
    static Stopwatch swPrelude = new Stopwatch("Prelude");
    static Stopwatch swTotal = new Stopwatch("Total");

    private static Plugin InitializePlugin(VccOptions commandLineOptions)
    {
      try {
        string pluginName = null;
        Plugin selectedPlugin = null;

        if (commandLineOptions.PluginOptions.Count != 0 || commandLineOptions.DisplayCommandLineHelp || commandLineOptions.Vcc3) {
          pluginManager = new PluginManager(commandLineOptions);
          string pluginDir = PathHelper.PluginDir;
          if (pluginDir != null)  pluginManager.AddPluginDirectory(pluginDir);
          pluginManager.Discover();
          foreach (var opt in commandLineOptions.PluginOptions.Keys) {
            if (opt == "dir") continue;
            if (pluginName == null) {
              pluginName = opt;
            } else {
              System.Console.WriteLine("More than one plugin requested ('{0}' and '{1}').", pluginName, opt);
              return null;
            }
          }

          if (pluginName != null) {
            foreach (var plugin in pluginManager.Plugins) {
              if (string.Compare(pluginName, plugin.Name(), true) == 0) {
                if (selectedPlugin != null) {
                  System.Console.WriteLine("More than one plugin matches '{0}'.", pluginName);
                  return null;
                }
                selectedPlugin = plugin;
              }
            }
            if (selectedPlugin == null) {
              System.Console.WriteLine("Plugin '{0}' not found.", pluginName);
              return null;
            }
          }
        }

        if (selectedPlugin == null) selectedPlugin = new VccPlugin(commandLineOptions.Vcc3 ? pluginManager.VCGenPlugins : null); // the default

        selectedPlugin.RegisterStopwatch(swTotal);
        selectedPlugin.RegisterStopwatch(swVisitor);
        selectedPlugin.RegisterStopwatch(swPlugin);
        selectedPlugin.RegisterStopwatch(swPrelude);
        selectedPlugin.MessageHandler.AddHandler(new Microsoft.FSharp.Control.FSharpHandler<string>(PrintPluginMessage));

        try {
          swPlugin.Start();
          selectedPlugin.UseVccOptions(commandLineOptions);
          if (pluginName != null)
            selectedPlugin.UseCommandLineOptions(commandLineOptions.PluginOptions[pluginName]);
        } finally {
          swPlugin.Stop();
        }

        return selectedPlugin;

      } catch (System.Reflection.ReflectionTypeLoadException e) {
        foreach (Exception ex in e.LoaderExceptions) {
          Console.WriteLine(ex.Message);
          Console.WriteLine(ex.StackTrace);
        }
      }
      return null;
    }

    private static double startTime;
    internal static HostEnvironment dummyHostEnvironment = new HostEnvironment(64);

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

    private static string GetZ3Version() {
      ProcessStartInfo z3Psi = new ProcessStartInfo("z3.exe", "/version");
      z3Psi.CreateNoWindow = true;
      z3Psi.UseShellExecute = false;
      z3Psi.RedirectStandardOutput = true;
      try {
        using (Process z3Proc = Process.Start(z3Psi)) {
          z3Proc.WaitForExit();
          var z3VersionString = z3Proc.StandardOutput.ReadToEnd();
          Regex regex = new Regex("(?<major>\\d+)\\s+(?<minor>\\d+)\\s+(?<build>\\d+)\\s+(?<revision>\\d+)");
          var match = regex.Match(z3VersionString);
          if (match.Success) {
            return String.Format("{0}.{1}.{2}.{3}", match.Groups["major"].Value, match.Groups["minor"].Value, match.Groups["build"].Value, match.Groups["revision"].Value);
          }
          return z3VersionString;
        }
      } catch (Exception) {
        return "unknow";
      }
    }

    private static void DisplayVersion(bool formatAsXml) {
      string vccVersionString = System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion;
      string boogieVersionString = System.Diagnostics.FileVersionInfo.GetVersionInfo(typeof(BoogiePL.Parser).Assembly.Location).FileVersion;
      string z3VersionString = GetZ3Version();
      if (formatAsXml) {
        Console.WriteLine("<version>Vcc " + vccVersionString + ", Boogie " + boogieVersionString + ", Z3 " + z3VersionString + "</version>");
      } else {
        System.Resources.ResourceManager rm = new System.Resources.ResourceManager("Microsoft.Research.Vcc.Host.ErrorMessages", typeof(VccCommandLineHost).Assembly);
        Console.WriteLine(rm.GetString("Version"), vccVersionString, boogieVersionString, z3VersionString);
      }
    }

    static int errorCount;

    public static int ErrorCount {
      get { return errorCount; }
      set { errorCount = value; }
    }

    static void RunPlugin(VccOptions commandLineOptions) {
      bool errorsInPreprocessor;
      var processedFiles = CCompilerHelper.Preprocess(commandLineOptions, out errorsInPreprocessor);
      if (errorsInPreprocessor) return;
      using (var fnEnum = commandLineOptions.FileNames.GetEnumerator())
      using (var ppEnum = processedFiles.GetEnumerator())
      while (fnEnum.MoveNext() && ppEnum.MoveNext())
        RunPlugin(fnEnum.Current, ppEnum.Current, commandLineOptions);
    }

    private static Plugin currentPlugin;
    private static void RunPlugin(string fileName, string ppFileName, VccOptions commandLineOptions)
    {
      HostEnvironment hostEnvironment = new HostEnvironment(commandLineOptions.PointerSize);
      hostEnvironment.Errors += new CciErrorHandler(commandLineOptions).HandleErrors;
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
        verificationErrorHandler.FlushErrors();

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

    public static string preludePath = "";
    // [Once]

    public static IList<String> StandardPreludeLines {
      get { return standardPreludeLines.AsReadOnly(); }
    }

    static List<String> standardPreludeLines;
    static string standardPreludeString;
    static string standardPreludePath;

    private static Program GetStandardPrelude() {
      string preludePath = PathHelper.PreludePath(standardPreludePath);
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

    internal HostEnvironment(int pointerSizeInBits)
      : base(new NameTable(), (byte)(pointerSizeInBits / 8)) {
      Debug.Assert(pointerSizeInBits == 32 || pointerSizeInBits == 64);
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
