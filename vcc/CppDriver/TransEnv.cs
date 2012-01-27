using System;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc.Cpp
{
  class TransEnv : TransHelper.TransEnv, IErrorSink
  {

    public TransEnv(string [] pipeOperations) : base(new TransOptions(pipeOperations)) {}

    private bool errorReported;

    public override bool ErrorReported
    {
      get { return this.errorReported; }
    }

    public override bool ShouldDumpStack
    {
      get { return true; }
    }

    public override void Error(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
    {
      errorReported = true;
      Console.WriteLine("{0}({1},{2}): error {3}: {4}", tok.Filename, tok.Line, tok.Column, code, msg);
    }

    public override void Oops(Token tok, string msg)
    {
      if (!errorReported)
      {
        Console.WriteLine("{0}({1},{2}): oops: {3}", tok.Filename, tok.Line, tok.Column, msg);
      }
    }

    public override int PointerSizeInBytes
    {
      get { return 4; }
    }

    public override void Warning(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
    {
      Console.WriteLine("{0}({1},{2}): warning {3}: {4}", tok.Filename, tok.Line, tok.Column, code, msg);
    }

    public void Error(IToken tok, string msg)
    {
      errorReported = true;
      Console.WriteLine("{0}({1},{2}): Boogie error: {3}", tok.filename, tok.line, tok.col, msg);
    }
  }
}
