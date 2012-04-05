//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using Microsoft.Boogie;
using Microsoft.Boogie.AbstractInterpretation;
using Microsoft.FSharp.Collections;
using VC;

namespace Microsoft.Research.Vcc.Cpp
{
    public class CppDriver
    {
        private readonly TransEnv env;

        static CppDriver()
        {
            // reference these so they get copied to the output directory
#pragma warning disable 168
            var y = new Boogie.SMTLib.Factory();
#pragma warning restore 168
        }

        public CppDriver(string vccArgs, string[] pipeOperations)
        {
            // Register VccppOptions as CommandLineOption
            VccppOptions vccOptions = new VccppOptions(pipeOperations);
            VccppOptions.Install(vccOptions);

            // Parse arguments
            if (!String.IsNullOrEmpty(vccArgs))
            {
                var vccArgsArr = vccArgs.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                if (!CommandLineOptions.Clo.Parse(vccArgsArr))
                {
                    throw new ArgumentException("Unsupported switch.");
                }
            }

            if (vccOptions.DisplayCommandLineHelp)
            {
                vccOptions.Usage();
                throw new Exception("vcchelp");
            }

            env = new TransEnv(vccOptions);
            CAST.PointerSizeInBytes.Value = env.PointerSizeInBytes;
            Transformers.init(env);
            Transformers.processPipeOptions(env);
        }

        private Program PreparePrelude()
        {
            string vcc3pfile = "Vcc3Prelude.bpl";
            string preludePath = PathHelper.PreludePath(vcc3pfile);

            if (preludePath == null)
            {
                env.Oops(Token.NoToken, "Could not locate " + vcc3pfile);
                return new Program();
            }

            Program prelude;
            int errorCount = Parser.Parse(preludePath, new List<string>(), out prelude);
            if (prelude == null || errorCount > 0)
            {
                env.Oops(Token.NoToken, "There were errors parsing " + vcc3pfile);
                return new Program();
            }

            return prelude;
        }

        private Program TranslateToBoogie(FSharpList<CAST.Top> decls)
        {
            // process declarations 
            var tdecls = env.ApplyTransformers(decls);
            if (!env.ShouldContinue) return null;

            // Translate to Boogie AST
            var boogieDecls = Translator.translate(null, env, PreparePrelude, tdecls);
            if (!env.ShouldContinue) return null;

            // Translate to BoogiePL
            return BoogieAST.trProgram(boogieDecls);
        }

        private static void InstallBoogieOptions()
        {
            var options = new[]
                      {
                        "/errorLimit:10",
                        "/typeEncoding:m",
                        "/proverMemoryLimit:0",
                        "/proverWarnings:1",
                        "/liveVariableAnalysis:0",
                        "/prover:SMTLib",
                        "/z3opt:/memory:300",
                        "/z3opt:QI_EAGER_THRESHOLD=1000",
                        "/z3opt:CASE_SPLIT=5"
                      };

            var clo = new CommandLineOptions();
            bool parseRes = clo.Parse(options);
            System.Diagnostics.Debug.Assert(parseRes, "Wrong Boogie parameters.");

            clo.RunningBoogieFromCommandLine = false;
            CommandLineOptions.Install(clo);
        }

        public bool WriteToBpl(FSharpList<CAST.Top> decls, string outputFileName)
        {
            var program = TranslateToBoogie(decls);
            if (program == null) return false;

            // write Boogie

            CommandLineOptions.Install(new CommandLineOptions());

            using (var writer = new TokenTextWriter(outputFileName))
            {
                program.Emit(writer);
            }

            return true;
        }

        private static bool CompleteOutputCheck(ExpectedOutputChecker checker)
        {
            checker.CompleteChecking();

            if (checker.Mismatches > 0)
            {
                Utils.Log("\n\n*** Found output mismatch. ***\n");
                Utils.Log("*** Expected (line " + checker.FirstMismatchFoundAt + "): ***");
                Utils.Log(checker.FirstMismatchExpected);
                Utils.Log("*** Received: ***");
                Utils.Log(checker.FirstMismatchReceived);
                Utils.Log("*** End of mismatch. ***\n\n");

                return false;
            }

            return true;
        }

        public bool Verify(FSharpList<CAST.Top> decls, string reference, bool dumpAstBeforeTransformations = false)
        {
            if (dumpAstBeforeTransformations)
            {
                TransUtil.dumpDecls("AST after Conversion", false, decls);
            }

            var errorReporter = new VerificationErrorReporter();
            var checker = string.IsNullOrEmpty(reference) ? null : new ExpectedOutputChecker(reference);

            if (checker != null)
            {
                this.env.ErrorReportedEvent += checker.ErrorReported;
                errorReporter.ErrorReported += checker.ErrorReported;
                errorReporter.VerificationFinished += checker.VerificationFinished;
            }

            try
            {
                var program = TranslateToBoogie(decls);
                if (program == null)
                {
                    if (checker != null) return CompleteOutputCheck(checker);
                    return false;
                }

                var prelude = PreparePrelude();
                if (prelude.TopLevelDeclarations.Count == 0) return false;

                var verifierInput = new Program();
                verifierInput.TopLevelDeclarations.AddRange(prelude.TopLevelDeclarations);
                verifierInput.TopLevelDeclarations.AddRange(program.TopLevelDeclarations);

                InstallBoogieOptions();

                // prepare for verification
                if (verifierInput.Resolve(this.env) > 0) return false;
                if (verifierInput.Typecheck(this.env) > 0) return false;
                AbstractInterpretation.RunAbstractInterpretation(verifierInput);
                LambdaHelper.ExpandLambdas(verifierInput);

                // verify all implementation functions
                foreach (var decl in verifierInput.TopLevelDeclarations)
                {
                    var impl = decl as Implementation;
                    if (impl != null)
                    {
                        var vcGen = new VCGen(verifierInput, null, false);
                        errorReporter.StartFunction(impl.Name);
                        vcGen.VerifyImplementation(impl, program, errorReporter);
                        errorReporter.EndFunction();
                        vcGen.Close();
                    }
                }

                if (checker != null) return CompleteOutputCheck(checker);

                return !errorReporter.AnyErrorReported;

            }
            catch (Exception e)
            {
                Utils.Log("*** Exception occurred during verification ***");
                Utils.Log(e.ToString());
                throw;
            }
            finally
            {
                if (checker != null)
                {
                    errorReporter.VerificationFinished -= checker.VerificationFinished;
                    errorReporter.ErrorReported -= checker.ErrorReported;
                    this.env.ErrorReportedEvent -= checker.ErrorReported;
                }
            }
        }
    }
}
