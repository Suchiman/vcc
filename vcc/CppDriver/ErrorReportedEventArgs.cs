using System;

namespace Microsoft.Research.Vcc
{
  public class ErrorReportedEventArgs : EventArgs
  {
    private readonly ErrorDetails details;

    public ErrorReportedEventArgs(ErrorDetails details)
    {
      this.details = details;
    }

    public ErrorDetails Details
    {
      get { return this.details; }
    }

  }
}
