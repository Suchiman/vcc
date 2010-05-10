//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.Cci;
using Microsoft.Cci.Ast;

namespace Microsoft.Research.Vcc
{

  public sealed class VccOptions : FrameworkOptions
  {
    public List<string> BoogieOptions = new List<string>();
    public bool NoPreprocessor;
    public List<string> PreprocessorOptions = new List<string>();
    public bool RunTestSuite;
    public int RunTestSuiteMultiThreaded = -1;
    public bool TranslateToBPL;
    public List<string> Z3Options = new List<string>();
    public bool VCLikeErrorMessages;
    public bool TimeStats;
    public bool TimeStatsForVs;
    public bool XmlFormatOutput;
    public string/*?*/ ClPath;
    public List<string> Functions = new List<string>();
    public List<string> FunctionsWithExactName = new List<string>();
    public bool RunningFromCommandLine;
    public uint? VerifyUpToLine;
    public bool PauseBeforeExit;
    public bool EagerTranslation;
    public bool OmitReadWriteChecking;
    public bool RunInBatchMode;
    public bool ModifiedPreprocessorFiles;
    public Dictionary<long, bool> DisabledWarnings = new Dictionary<long, bool>();
    public bool AggressivePruning;
    public List<string> PipeOperations = new List<string>();
    public List<string> VcOpt = new List<string>();
    public Dictionary<string, List<string>> PluginOptions = new Dictionary<string, List<string>>();
    public bool DumpBoogie;
    public bool GenerateFieldOffsetAxioms = true;
    public bool WarningsAsErrors;
    public int WarningLevel = 1;
    public bool DebugOnWarningOrError;
    public bool SaveModel;
    public bool RunModelViewer;
    public bool RunInspector;
    public bool DetailedTimes;
    public bool PrintCEVModel;
    public int PointerSize = 64;
    public bool Vcc2; // new syntax
    public bool Vcc3;
    // we might want an option for setting it
    public string PreludPath = "VccPrelude.bpl";
  }

  public class OptionParser : OptionParser<VccOptions>
  {

    private OptionParser(MetadataHostEnvironment hostEnvironment)
      : base(hostEnvironment) {
      //^ assume this.options != null;
      this.options.CheckedArithmetic = true;
    }

    public static VccOptions ParseCommandLineArguments(MetadataHostEnvironment hostEnvironment, IEnumerable<string> arguments) {
      return OptionParser.ParseCommandLineArguments(hostEnvironment, arguments, true);
    }

    public static VccOptions ParseCommandLineArguments(MetadataHostEnvironment hostEnvironment, IEnumerable<string> arguments, bool oneOrMoreSourceFilesExpected) {
      OptionParser parser = new OptionParser(hostEnvironment);
      parser.ParseCommandLineArguments(arguments, oneOrMoreSourceFilesExpected);
      return parser.options;
    }

    protected override bool ParseCompilerOption(string arg) {
      int n = arg.Length;
      if (n <= 1) return false;
      char ch = arg[0];
      if (ch != '/' && ch != '-') return false;
      ch = arg[1];
      switch (ch) {
        case '2':
          bool? vcc2 = this.ParseNamedBoolean(arg, "2", "2");
          if (vcc2 != null) {
            this.options.Vcc2 = vcc2.Value;
            return true;
          }
          return false;
        case '3':
          bool? vcc3 = this.ParseNamedBoolean(arg, "3", "3");
          if (vcc3 != null) {
            this.options.Vcc3 = vcc3.Value;
            this.options.PreludPath = "Vcc3Prelude.bpl";
            return true;
          }
          return false;
        case 'a':
          bool? aggressivePruning = this.ParseNamedBoolean(arg, "aggressivepruning", "a");
          if (aggressivePruning != null) {
            this.options.AggressivePruning = aggressivePruning.Value;
            return true;
          }
          return false;
        case 'b':
          if (this.ParseName(arg, "batch", "batch")) {
            this.options.RunInBatchMode = true;
            return true;
          }
          List<string>/*?*/ boogieOptions = this.ParseNamedArgumentList(arg, "boogie", "b");
          if (boogieOptions == null || boogieOptions.Count == 0) return false;
          this.options.BoogieOptions.AddRange(boogieOptions);
          return true;
        case 'c':
          bool? checkedArithmetic = this.ParseNamedBoolean(arg, "checked", "c");
          if (checkedArithmetic != null) {
            this.options.CheckedArithmetic = checkedArithmetic.Value;
            return true;
          }
          bool? clErrors = this.ParseNamedBoolean(arg, "clerrors", "clerrors");
          if (clErrors != null) {
            this.options.VCLikeErrorMessages = clErrors.Value;
            return true;
          }
          string/*?*/ clPath = this.ParseNamedArgument(arg, "clpath", "clpath");
          if (clPath != null) {
            this.options.ClPath = clPath;
            return true;
          }
          string filename = this.ParseNamedArgument(arg, "cevprint", "cev");
          if (filename != null) {
              this.options.PrintCEVModel = true;
              this.options.BoogieOptions.Add("/cev:" + filename);
              return true;
          }
          return false;
        case 'd':
          if (this.ParseName(arg, "debug", "dbg")) {
            this.options.DebugOnWarningOrError = true;
            return true;
          }
          bool? dump = this.ParseNamedBoolean(arg, "dumpsource", "d");
          if (dump != null) {
            this.options.PipeOperations.Add("dump after end");
            return true;
          }
          dump = this.ParseNamedBoolean(arg, "dumpsource0", "d0");
          if (dump != null) {
            this.options.PipeOperations.Add("dump before begin");
            return true;
          }
          dump = this.ParseNamedBoolean(arg, "dumpboogie", "db");
          if (dump != null) {
            this.options.DumpBoogie = dump.Value;
            return true;
          }
          return false;
        case 'e':
          bool? eager = this.ParseNamedBoolean(arg, "eager", "e");
          if (eager != null) {
            this.options.EagerTranslation = eager.Value;
            return true;
          }

          bool? exe = this.ParseNamedBoolean(arg, "exe", "exe");
          if (exe != null) {
            this.options.EagerTranslation = eager.Value;
            return true;
          }

          return false;
        case 'f':
          var functions = this.ParseNamedArgumentList(arg, "functions", "f");
          if (functions == null || functions.Count == 0) return false;
          this.options.Functions.AddRange(functions);
          return true;

        case 'F':
          var functionExactMatch = this.ParseNamedArgumentList(arg, "functions", "f");
          if (functionExactMatch == null || functionExactMatch.Count == 0) return false;
          this.options.FunctionsWithExactName.AddRange(functionExactMatch);
          return true;

        case 'h':
          if (this.ParseName(arg, "help", "help")) {
            this.options.DisplayCommandLineHelp = true;
            return true;
          }
          List<string>/*?*/ hiddenWarnings = this.ParseNamedArgumentList(arg, "hide", "h");
          if (hiddenWarnings == null || hiddenWarnings.Count == 0) return false;
          foreach (string w in hiddenWarnings) {
            long tmp;
            if (!long.TryParse(w, out tmp)) return false;
            if (!this.options.DisabledWarnings.ContainsKey(tmp)) {
              this.options.DisabledWarnings.Add(tmp, true);
            }
          }
          return true;

        case 'i':
          if (this.ParseName(arg, "inspector", "i")) {
            this.options.RunInspector = true;
            return true;
          }
          return false;

        case 'm':
          if (this.ParseName(arg, "modifiedpreprocessorfile", "modifiedpreprocessorfile")) {
            this.options.ModifiedPreprocessorFiles = true;
            return true;
          }
          if (this.ParseName(arg, "modelviewer", "mv")) {
            this.options.SaveModel = true;
            this.options.RunModelViewer = true;
            return true;
          }
          if (this.ParseName(arg, "model", "m")) {
            this.options.SaveModel = true;
            return true;
          }
          return false;
        case 'n':
          if (this.ParseName(arg, "nopreprocessor", "n")) {
            this.options.NoPreprocessor = true;
            return true;
          }
          return false;
        case 'o':
          string/*?*/ path = this.ParseNamedArgument(arg, "out", "o");
          if (path != null) {
            this.options.OutputFileName = path;
            return true;
          }
          if (this.ParseName(arg, "omitrw", "omitrw")) {
            this.options.OmitReadWriteChecking = true;
            return true;
          }
          bool? fieldOffsetAxioms = this.ParseNamedBoolean(arg, "offsetaxioms", "offsetaxioms");
          if (fieldOffsetAxioms.HasValue) {
            this.options.GenerateFieldOffsetAxioms = fieldOffsetAxioms.Value;
            return true;
          }
          return false;
        case 'p':
          if (this.ParseName(arg, "pause", "pause")) {
            this.options.PauseBeforeExit = true;
            return true;
          }

          string pointerSizeStr = this.ParseNamedArgument(arg, "pointersize", "ps");
          if (pointerSizeStr != null) {
            int pointerSize;
            if (!Int32.TryParse(pointerSizeStr, out pointerSize) || (pointerSize != 32 && pointerSize != 64)) return false;
            this.options.PointerSize = pointerSize;
            return true;
          }

          List<string>/*?*/ pipeOptions = this.ParseNamedArgumentList(arg, "pipe", "pipe");
          if (pipeOptions != null && pipeOptions.Count > 0) {
            this.options.PipeOperations.AddRange(pipeOptions);
            return true;
          }

          List<string>/*?*/ preprocessorOptions = this.ParseNamedArgumentList(arg, "preprocessor", "p");
          if (preprocessorOptions != null && preprocessorOptions.Count > 0) {
            //i-sebaf: If IncludeDir Contains Spaces like "Program Files" than a quote is required.
            for (int i = 0; i < preprocessorOptions.Count; i++) {
              if (preprocessorOptions[i].Contains(" ")) {
                preprocessorOptions[i] = preprocessorOptions[i].Insert(2, "\"") + "\"";
              }
            }
            this.options.PreprocessorOptions.AddRange(preprocessorOptions);
            return true;
          }

        {
            int end = arg.IndexOf(':');
            int semi = end;
            if (end < 0) end = arg.Length;
            string option = arg.Substring(1, end - 1).ToLower(System.Globalization.CultureInfo.InvariantCulture);
            string plugin = option.Substring(1);
            List<string> args;
            if (!this.options.PluginOptions.TryGetValue(plugin, out args)) {
              args = new List<string>();
              this.options.PluginOptions[plugin] = args;
            }
            if (semi > 0) {
              List<string>/*?*/ pluginOptions = this.ParseNamedArgumentList(arg, option, option);
              if (pluginOptions == null)
                args.Add("");
              else
                args.AddRange(pluginOptions);
            } else {
              args.Add("");
            }
            return true;
          }

        case 's':
        if (this.ParseName(arg, "suite", "s")) {
          this.options.RunTestSuite = true;
          this.options.NoPreprocessor = true;
          return true;
        } else if (this.ParseName(arg, "stats", "st")) {
          this.options.TimeStats = true;
          return true;
        } else if (this.ParseName(arg, "statsvs", "stvs")) {
          this.options.TimeStats = true;
          this.options.TimeStatsForVs = true;
          return true;
        } else if (this.ParseName(arg, "smoke", "sm")) {
          this.options.BoogieOptions.Add("/smoke");
          return true;
        } else {
          string mt = this.ParseNamedArgument(arg, "suitemt", "smt");
          int threads = 0;
          if (!String.IsNullOrEmpty(mt)) {
            if (Int32.TryParse(mt, out threads)) {
              this.options.RunTestSuiteMultiThreaded = threads;
              return true;
            } else {
              return false;
            }
          }
          if (this.ParseName(arg, "suitemt", "smt")) {
            this.options.RunTestSuiteMultiThreaded = 0;
            return true;
          }
        }
        return false;
        case 't':
        bool? detailedTimes = this.ParseNamedBoolean(arg, "time", "time");
        if (detailedTimes != null) {
          this.options.DetailedTimes = detailedTimes.Value;
          return true;
        }
        this.options.TranslateToBPL = this.ParseName(arg, "translate", "t");
        return true;
        case 'u':
        string lineStr = this.ParseNamedArgument(arg, "uptoline", "uptoline");
        uint lineNo;
        if (lineStr != null && UInt32.TryParse(lineStr, out lineNo)) {
          this.options.VerifyUpToLine = lineNo;
          return true;
        }
        return false;
        case 'v':

        if (this.ParseName(arg, "verify", "v")) {
          DummyExpression dummyExpression = new DummyExpression(SourceDummy.SourceLocation);
          this.hostEnvironment.ReportError(new AstErrorMessage(dummyExpression, Microsoft.Cci.Ast.Error.InvalidCompilerOption, "/verify is the default option and does not need to be specified explicitly"));
          return true;
        }

        if (this.ParseName(arg, "version", "version")) {
          this.options.DisplayVersion = true;
          return true;
        }

        bool? vcOpt = this.ParseNamedBoolean(arg, "vo", "vcopt");
        if (vcOpt != null) {
          if (vcOpt.Value) this.options.VcOpt.Add("yes");
          else this.options.VcOpt.Clear();
          return true;
        } else {
          List<string> vcopts = this.ParseNamedArgumentList(arg, "vo", "vcopt");
          if (vcopts != null) {
            this.options.VcOpt.AddRange(vcopts);
            return true;
          }
        }

        return false;

        case 'w':
        string warnLevelStr = this.ParseNamedArgument(arg, "warn", "w");
        if (warnLevelStr != null) {
          int warnLevel;
          if (!Int32.TryParse(warnLevelStr, out warnLevel) || (warnLevel < 0 || warnLevel > 2)) return false;
          this.options.WarningLevel = warnLevel;
          return true;
        }
        bool? wx = this.ParseNamedBoolean(arg, "warningsaserrors", "wx");
        if (wx != null) {
          this.options.WarningsAsErrors = wx.Value;
          return true;
        }
        return false;
        case 'x':
        if (this.ParseName(arg, "xml", "xml")) {
          this.options.XmlFormatOutput = true;
          return true;
        }
        return false;
        case 'z':
        List<string>/*?*/ z3Options = this.ParseNamedArgumentList(arg, "z3", "z");
        if (z3Options == null || z3Options.Count == 0) return false;
        this.options.Z3Options.AddRange(z3Options);
        return true;
        case '?':
        this.options.DisplayCommandLineHelp = true;
        return true;
        default:
        break;
      }
      return false;
    }

    protected override bool DirectoryIsOk(string path, string pattern, string extension) {
      if (!this.options.RunTestSuite) return false;
      bool foundADirectory = false;
      if (path != null && Directory.Exists(path)) {
        if ((path == ".\\" || path == "..\\") && pattern == ".") {
          this.options.FileNames.Add(Path.GetFullPath(path));
          foundADirectory = true;
        } else {
          foreach (string file in Directory.GetDirectories(path, pattern)) {
            string ext = Path.HasExtension(file) ? Path.GetExtension(file) : "";
            if (string.Compare(extension, ext, true, System.Globalization.CultureInfo.InvariantCulture) != 0) continue;
            this.options.FileNames.Add(Path.GetFullPath(file));
            foundADirectory = true;
          }
        }
      }
      return foundADirectory;
    }
  }
}