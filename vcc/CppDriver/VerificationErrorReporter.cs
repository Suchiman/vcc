using System;

namespace Microsoft.Research.Vcc.Cpp
{
  class VerificationErrorReporter : Boogie.VerifierCallback
  {
    private string currentFunction = "<unknown>";
    private bool errorReported;

    public bool AnyErrorReported { get; private set; }

    public override void OnCounterexample(Boogie.Counterexample ce, string reason)
    {
      Console.WriteLine("Verification of {0} failed: {1}", this.currentFunction, reason);
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnOutOfMemory(string reason)
    {
      Console.WriteLine("Verification of {0} ran out of memory: {1}", this.currentFunction, reason);
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnTimeout(string reason)
    {
      Console.WriteLine("Verification of {0} timed out : {1}", this.currentFunction, reason);
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnUnreachableCode(Boogie.Implementation impl)
    {
      Console.WriteLine("Verification of {0} found unreachable code", this.currentFunction);
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnWarning(string msg)
    {
      Console.WriteLine("Verification of {0} gave warning: {1}", this.currentFunction, msg);
    }

    public void StartFunction(string functionName)
    {
      this.errorReported = false;
      this.currentFunction = functionName;
    }

    public bool EndFunction()
    {
      if (!this.errorReported) {
        Console.WriteLine("Verification of {0} succeeded.", this.currentFunction);
        return true;
      }

      this.errorReported = false;
      return false;
    }
  }
}
