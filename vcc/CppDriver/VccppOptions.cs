namespace Microsoft.Research.Vcc.Cpp
{
    using Microsoft.Boogie;
    using System.Collections.Generic;

    class VccppOptions : CommandLineOptions
    {
        public VccppOptions()
            : base("Vccpp", "VCC")
        {
        }

        protected override bool ParseOption(string name, CommandLineOptionEngine.CommandLineParseState ps)
        {
            switch (name)
            {
                // Example
                //case "rprint":
                //    if (ps.ConfirmArgumentCount(1))
                //    {
                // = ps.args[ps.i];
                //}
                //break;

                //TODO: set vccOptions
                case "a":
                case "aggressivepruning":
                    return true;
                case "b":
                case "boogie":
                    return true;
                case "bvd":
                    return true;
                case "det":
                case "determinize":
                    return true;
                case "d0":
                case "dumpsource0":
                    return true;
                case "d":
                case "dumpsource":
                    return true;
                case "f":
                case "functions":
                    return true;
                case "F":
                case "Functions":
                    return true;
                case "?":
                case "help":
                    return true;
                case "h":
                case "hide":
                    return true;
                case "i":
                case "inspector":
                    return true;
                case "ii":
                case "ignoreincludes":
                    return true;
                case "n":
                case "nopreprocessor":
                    return true;
                case "nv":
                case "noverification":
                    return true;
                case "oaf":
                case "opsasfuncs":
                    return true;
                case "o":
                case "out":
                    return true;
                case "p":
                case "preprocessor":
                    return true;
                case "prelude":
                    return true;
                case "ps":
                case "pointersize":
                    return true;
                case "pipe":
                    return true;
                case "weight":
                    return true;
                case "sm":
                case "smoke":
                    return true;
                case "st":
                case "stats":
                    return true;
                case "s":
                case "suite":
                    return true;
                case "time":
                    return true;
                case "t":
                case "translate":
                    return true;
                case "term":
                case "termination":
                    return true;
                case "dexp":
                case "defexpansion":
                    return true;
                case "version":
                    return true;
                case "wx":
                case "warningsaserrors":
                    return true;
                case "w":
                case "warn":
                    return true;
                case "xml":
                    return true;
                case "z":
                case "z3":
                    return true;
                case "dt":
                case "dumptriggers":
                    return true;
                default:
                    break;
            }

            return base.ParseOption(name, ps);
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

            base.Usage();  // Prints the Boogie options
        }
    }
}
