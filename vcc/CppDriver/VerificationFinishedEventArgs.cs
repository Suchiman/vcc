using System;

namespace Microsoft.Research.Vcc
{
  class VerificationFinishedEventArgs : EventArgs
  {
    private readonly string name;
    private readonly string outcome;

    public VerificationFinishedEventArgs(string name, string outcome)
    {
      this.name = name;
      this.outcome = outcome;
    }

    public string Name
    {
      get { return this.name; }
    }

    public string Outcome
    {
      get { return this.outcome; }
    }
  }
}
