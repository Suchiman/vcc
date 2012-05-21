namespace Microsoft.Research.Vcc.Cpp
{
    using Microsoft.Boogie;
    using System.Collections.Generic;
    using System;

    class VccppOptions : CommandLineOptions, Helper.IOptions
    {
        public List<string> boogieOptions = new List<string>();
        public List<string> functions = new List<string>();
        public List<string> functionsWithExactName = new List<string>();
        public List<string> pipeOperations = new List<string>();
        public List<string> preprocessorOptions = new List<string>();
        public List<string> weightOptions = new List<string>();

        public bool AggressivePruning { get; private set; }
        public bool DebugOnWarningOrError { get; private set; }
        public bool DetailedTimes { get; private set; }
        public bool DeterminizeOutput { get; private set; }
        public bool DisplayCommandLineHelp { get; private set; }
        public bool DisplayVersion { get; private set; }
        public bool DumpBoogie { get; private set; }
        public bool KeepPreprocessorFiles { get; private set; }
        public bool NoPreprocessor { get; private set; }
        public bool NoVerification { get; private set; }
        public bool OpsAsFunctions { get; private set; }
        public bool PrintCEVModel { get; private set; }
        public bool RunInBatchMode { get; private set; }
        public bool RunInspector { get; private set; }
        public bool RunTestSuite { get; private set; }
        public bool SaveModelForBvd { get; private set; }
        public bool TimeStats { get; private set; }
        public bool TranslateToBPL { get; private set; }
        public bool WarningsAsErrors { get; private set; }
        public bool YarraMode { get; private set; }
        public Dictionary<long, bool> DisabledWarnings = new Dictionary<long, bool>();
        public double RunTestSuiteMultiThreaded { get; private set; }
        public IEnumerable<string> BoogieOptions { get { return boogieOptions; } }
        public IEnumerable<string> Functions { get { return functions; } }
        public IEnumerable<string> FunctionsWithExactName { get { return functionsWithExactName; } }
        public IEnumerable<string> PipeOperations { get { return pipeOperations; } }
        public IEnumerable<string> PreprocessorOptions { get { return preprocessorOptions; } }
        public IEnumerable<string> WeightOptions { get { return weightOptions; } }
        public int DefExpansionLevel { get; private set; }
        public int DumpTriggers { get; private set; }
        public int PointerSize { get; private set; }
        public int TerminationLevel { get; private set; }
        public bool TerminationForPure { get { return TerminationLevel >= 1; } }
        public bool TerminationForGhost { get { return TerminationLevel >= 2; } }
        public bool TerminationForAll { get { return TerminationLevel >= 3; } }
        public int WarningLevel { get; private set; }
        public string IgnoreIncludes { get; private set; }
        public string OutputDir { get; private set; }
        public string PreludePath { get; private set; }
        public string VerificationLocation { get; private set; }
        public string XmlLogFile { get; private set; }
        public UInt32 VerifyUpToLine { get; private set; }

        public bool ExplicitTargetsGiven
        {
            // TODO
            get { return false; }
        }

        public VccppOptions(string[] pipeOperations)
            : base("Vccpp", "VCC")
        {
            this.pipeOperations.AddRange(pipeOperations);
            RunTestSuiteMultiThreaded = -1;
            WarningLevel = 1;
            PointerSize = 32;
            TerminationLevel = 1;
        }

        protected override bool ParseOption(string name, CommandLineOptionEngine.CommandLineParseState ps)
        {
            switch (name)
            {
                // TODO: better error handling during parsing

                case "a":
                case "aggressivepruning":
                    this.AggressivePruning = true;
                    return true;
                case "b":
                case "boogie":
                    this.boogieOptions.AddRange(ps.args);
                    return true;
                case "bvd":
                    this.PrintCEVModel = true;
                    this.SaveModelForBvd = true;
                    return true;
                case "batch":
                    this.RunInBatchMode = true;
                    return true;
                case "cev":
                case "cevprint":
                    this.PrintCEVModel = true;
                    this.boogieOptions.Add("/mv:" + ps.args[ps.i]);
                    return true;
                case "dbg":
                case "debug":
                    this.DebugOnWarningOrError = true;
                    return true;
                case "det":
                case "determinize":
                    this.DeterminizeOutput = true;
                    return true;
                case "dexp":
                case "defexpansion":
                    int expansionLev;
                    if (int.TryParse(ps.args[ps.i], out expansionLev))
                    {
                        this.DefExpansionLevel = expansionLev;
                    }
                    return true;
                case "db":
                case "dumpboogie":
                    this.DumpBoogie = ParseBool(ps.args[ps.i]);
                    return true;
                case "dt":
                case "dumptriggers":
                    this.DumpTriggers = int.Parse(ps.args[ps.i]);
                    return true;
                case "d0":
                case "dumpsource0":
                    this.pipeOperations.Add("dump before begin");
                    return true;
                case "d":
                case "dumpsource":
                    this.pipeOperations.Add("dump after end");
                    return true;
                case "f":
                case "functions":
                    this.functions.AddRange(ps.args);
                    return true;
                case "F":
                case "Functions":
                    this.functionsWithExactName.AddRange(ps.args);
                    return true;
                case "?":
                case "help":
                    this.DisplayCommandLineHelp = true;
                    return true;
                case "h":
                case "hide":
                    foreach (string w in ps.args)
                    {
                        long tmp;
                        if (!long.TryParse(w, out tmp)) return false;
                        if (!this.DisabledWarnings.ContainsKey(tmp))
                        {
                            this.DisabledWarnings.Add(tmp, true);
                        }
                    }
                    return true;
                case "i":
                case "inspector":
                    this.RunInspector = true;
                    return true;
                case "ii":
                case "ignoreincludes":
                    this.IgnoreIncludes = ps.i < ps.args.Length ? ps.args[ps.i] : "";
                    return true;
                case "keepppoutput":
                    this.KeepPreprocessorFiles = ParseBool(ps.args[ps.i]);
                    return true;
                case "launch":
                    System.Diagnostics.Debugger.Launch();
                    return true;
                case "loc":
                case "location":
                    this.VerificationLocation = ps.args[ps.i];
                    return true;
                case "n":
                case "nopreprocessor":
                    this.NoPreprocessor = true;
                    return true;
                case "nv":
                case "noverification":
                    this.NoVerification = true;
                    return true;
                case "oaf":
                case "opsasfuncs":
                    this.OpsAsFunctions = true;
                    return true;
                case "o":
                case "out":
                    this.OutputDir = ps.args[ps.i];
                    return true;
                case "p":
                case "preprocessor":
                    // If IncludeDir Contains Spaces like "Program Files" then a quote is required.
                    for (int i = 0; i < ps.args.Length; i++)
                    {
                        if (ps.args[i].Contains(" "))
                        {
                            ps.args[i] = ps.args[i].Insert(2, "\"") + "\""; //TODO: test, why index 2
                        }
                        this.preprocessorOptions.Add(ps.args[i]);
                    }
                    return true;
                case "prelude":
                    this.PreludePath = ps.args[ps.i];
                    return true;
                case "ps":
                case "pointersize":
                    int pointerSize = int.Parse(ps.args[ps.i]);
                    if (pointerSize == 32 || pointerSize == 64)
                    {
                        this.PointerSize = pointerSize;
                    }
                    return true;
                case "pipe":
                    this.pipeOperations.AddRange(ps.args);
                    return true;
                case "suitemt":
                case "smt":
                    this.RunTestSuiteMultiThreaded = double.Parse(ps.args[ps.i]);
                    return true;
                case "sm":
                case "smoke":
                    this.boogieOptions.Add("/smoke");
                    return true;
                case "st":
                case "stats":
                    this.TimeStats = true;
                    return true;
                case "s":
                case "suite":
                    this.RunTestSuite = true;
                    this.NoPreprocessor = true;
                    return true;
                case "time":
                    this.DetailedTimes = ParseBool(ps.args[ps.i]);
                    return true;
                case "t":
                case "translate":
                    this.TranslateToBPL = ParseBool(ps.args[ps.i]);
                    return true;
                case "term":
                case "termination":
                    this.TerminationLevel = ushort.Parse(ps.args[ps.i]);
                    return true;
                case "uptoline":
                    this.VerifyUpToLine = UInt32.Parse(ps.args[ps.i]);
                    return true;
                case "version":
                    this.DisplayVersion = true;
                    return true;
                case "weight":
                    this.weightOptions.AddRange(ps.args);
                    return true;
                case "wx":
                case "warningsaserrors":
                    this.WarningsAsErrors = ParseBool(ps.args[ps.i]);
                    return true;
                case "w":
                case "warn":
                    this.WarningLevel = int.Parse(ps.args[ps.i]);
                    return true;
                case "xml":
                    this.XmlLogFile = ps.args[ps.i];
                    return true;
                case "yarra":
                    this.YarraMode = true;
                    return true;
                case "z":
                case "z3":
                    this.Z3Options.AddRange(ps.args);
                    return true;
                default:
                    break;
            }

            bool result = base.ParseOption(name, ps);
            if (!result)
            {
                Utils.Log("Unsupported switch: " + name);
            }

            return result;
        }

        private bool ParseBool(string input)
        {
            return input == "1" || input == "true";
        }

        protected override void ApplyDefaultOptions()
        {
            base.ApplyDefaultOptions();
        }

        public override void AttributeUsage()
        {
            // Provide attribute help here
        }

        public override void Usage()
        {
            Utils.Log(@"  ---- Vccpp options ---------------------------------------------------------
/aggressivepruning             Remove unreferenced items when verifying specific functions (Short form: /a)
/boogie:<boogie option list>   Options to pass on to Boogie (Short form: /b)
/bvd                           Generate counter examples for the Boogie Verification Debugger
/determinize                   Sort items to generate more deterministic Boogie files (Short form: /det)
/dumpsource0                   Print source before syntax transformations (Short form: /d0)
/dumpsource                    Print source after syntax transformations (Short form: /d)
/functions:<function list>     List of function roots to verify (Short form: /f)
/Functions:<function list>     List of exact function names to verify (Short form: /F)
/help                          Display this usage message (Short form: /?)
/hide:<warning list>           Hide specified warnings (by number; short form: /h)
/inspector                     Run Z3 Inspector to show progress of verification (Short form: /i)
/ignoreincludes[:<filename>]   Only verify entities from the files on the command line, not included files (Short form: /ii)
                 With filename: verify only entities from that file
/nopreprocessor                Do not invoke the C preprocessor on the input files (Short form: /n)
/noverification                Do not run the verification; stop after semantic analysis (Short form: /nv)
/opsasfuncs                    Translate arithmetic operations into Boogie functions (Short form: /oaf)
/out:<directory>               Place generated files in <directory> (Short form: /o)
/preprocessor:<pp option list> Options to pass on to the C preprocessor (Short form: /p)
/prelude:<BPL prelude path>    Specify the path to an alternative VCC BPL prelude
/pointersize:<32|64>           Size of the target platform's pointer type in bits (32,64), default is 64 (Short form: /ps)
/pipe:<opt1>,...               Alter compiler pipeline. Can use . or + instead of the space in option names.
                                     'active' lists stages
                                     'dump after <stage>' dumps source after the stage
                                     'dump after ...', ditto
                                     'dump-all', dump after every stage (lots of output!)
                                     'show-types', show expression types when dumping
                                     'time', dump time takes by each stage
                                     'disable <stage>' turns off a stage
                                     'move <stage> before <stage>' moves stage in the pipeline
                                     'move ... after ...', ditto
                                     'isabelle <theory-name> <filename-prefix>', dump Isabelle theory
/weight:<name=N>,...           Set weight for a particular kind of quantifier. Quantifiers with lower weights are instantiated first. 
                                     To learn abouts kinds of quantifiers use /weight:dump-names /t and inspect the resulting .bpl file.
                                     Specyfing /weight:foo=5 will set weight to 5 to 'foo' as well as 'foobar'.
                                     The default weight for user supplied quantifier (user-exists and user-forall) is 10.
                                     Most of the VCC generated ones have default of 1.
                                     Definition axioms for lambda expressions (c-lambda-def) and _(def ...) function (c-def-function) have a default of 10.
                                     You should avoid setting weight to 0.
/smoke                         Run smoke tests, which is equivalent to /b:/smoke (Short form: /sm)
/stats                         Display time statistics (Short form: /st)
/suite <file or directory>     Treat the given file or directory as a collection of test cases with expected output and report any differences (Short form: /s)
/time                          Display more detailed time statistics.
/translate                     Translate the source files into corresponding BPL files (Short form: /t)
/termination:<n>               Check termination behavior of: 0-nothing, 1-pure functions (default), 2-also ghost functions, 3-also regular functions (Short form: /term)
/defexpansion:<n>              Specify number of expansion levels for recursive _(def ...) functions (default: 3; 0 is no limit; Short form: /dexp)
/version                       Display the compiler version.
/warningsaserrors              Treat warnings as errors (Short form: /wx)
/warn:<n>                      Set warning level (0-2), default is 1 (Short form: /w)
/xml:<fil>                     Write XML log to <file>.
/z3:<z3 option list>           Options to pass on to Z3 (Short form: /z)
/dumptriggers:<n>              1-report inferred triggers, 2-report results for different {:level}s, 3-report also when explicit triggers are present, 4-include Boogie form, 5-debug (Short form: /dt)
");

            //base.Usage();  // Prints the Boogie options
        }
    }
}
