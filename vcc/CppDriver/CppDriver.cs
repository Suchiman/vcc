using System.IO;
using Microsoft.Boogie;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;
using VC;
using Microsoft.Boogie.AbstractInterpretation;

namespace Microsoft.Research.Vcc.Cpp
{
  public class CppDriver
  {
    private readonly TransEnv env = new TransEnv();

    static CppDriver()
    {
      // reference these so they get copied to the output directory
      #pragma warning disable 168
      var x = new Microsoft.Boogie.Z3.Factory();
      var y = new Microsoft.Boogie.SMTLib.Factory();
      #pragma warning restore 168
    }

    public CppDriver()
    {
      CAST.PointerSizeInBytes.Value = env.PointerSizeInBytes;
      Transformers.init(env);
      Transformers.processPipeOptions(env);   
    }

    private Program PreparePrelude()
    {
      string _preludePath = PathHelper.PreludePath("Vcc3Prelude.bpl");

      Program prelude;
      int _errorCount = Boogie.Parser.Parse(_preludePath, new List<string>(), out prelude);
      if (prelude == null || _errorCount > 0)
      {
        env.Oops(Token.NoToken, "There were errors parsing Vcc3Prelude.bpl.");
        return new Program();
      }
      else
      {
        return prelude;
      }
    }

    private Program TranslateToBoogie(FSharpList<CAST.Top> decls)
    {
      // process declarations 
      var tdecls = env.ApplyTransformers(decls);
      if (!env.ShouldContinue) return null;

      // Translate to Boogie AST
      var boogieDecls = Translator.translate(null, env, PreparePrelude, decls);
      if (!env.ShouldContinue) return null;

      // Translate to BoogiePL
      return BoogieAST.trProgram(boogieDecls);
    }

    private static void InstallBoogieOptions()
    {
      var options = new string[]
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
      clo.Parse(options);
      CommandLineOptions.Install(clo);
    }

    public bool WriteToBpl(FSharpList<CAST.Top> decls, string outputFileName)
    {
      var program = TranslateToBoogie(decls);
      if (program == null) return false;

      // write Boogie
      CommandLineOptions.Install(new CommandLineOptions());
      using (var writer = new TokenTextWriter(outputFileName)) {
        program.Emit(writer);
      }

      return true;
    }

    public bool Verify(FSharpList<CAST.Top> decls)
    {
      var program = TranslateToBoogie(decls);
      if (program == null) return false;

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

      // verify all implementation functions
      var errorReporter = new VerificationErrorReporter();

      foreach (var decl in verifierInput.TopLevelDeclarations)
      {
        var impl = decl as Implementation;
        if (impl != null)
        {
          var vcGen = new VCGen(program, Path.GetTempFileName(), false);
          errorReporter.StartFunction(impl.Name);
          vcGen.VerifyImplementation(impl, program, errorReporter);
          errorReporter.EndFunction();
        }
      }

      return errorReporter.AnyErrorReported;
    }
  }
}
