using System.IO;
using Microsoft.Boogie;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;

namespace Microsoft.Research.Vcc.Cpp
{
  public class CppDriver
  {
    private readonly TransEnv env = new TransEnv();

    public CppDriver()
    {
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
        return null;
      }
      else
      {
        return prelude;
      }
    }

    public bool Process(FSharpList<CAST.Top> decls, string outputFileName)
    {
      CAST.PointerSizeInBytes.Value = env.PointerSizeInBytes;

      // process declarations 
      var tdecls = env.ApplyTransformers(decls);
      if (!env.ShouldContinue) return false;

      // read prelude
      var prelude = PreparePrelude();
      if (prelude == null) return false;

      // Translate to Boogie AST
      var boogieDecls = Translator.translate(null, env, () => prelude, decls);
      if (!env.ShouldContinue) return false;

      // Translate to BoogiePL
      var bpl =  BoogieAST.trProgram(boogieDecls);
      if (!env.ShouldContinue) return false;

      // write Boogie
      CommandLineOptions.Install(new CommandLineOptions());
      using (var writer = new TokenTextWriter(outputFileName)) {
        bpl.Emit(writer);
      }


      return true;
    }
  }
}
