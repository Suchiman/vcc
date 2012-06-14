using System;

namespace Microsoft.Research.Vcc
{
  class ExpectedOutputChecker
  {
    private readonly string[] expected;
    private int idx;
    private int mismatchesFound;
    private int firstMismatchFoundAt = -1;
    private string firstMismatchExpected = "";
    private string firstMismatchReceived = "";

    public ExpectedOutputChecker(string expected)
    {
      this.expected = Array.FindAll(expected.Split(new[] {'\n', '\r'}), s => !String.IsNullOrEmpty(s));
    }

    public void ErrorReported(object sender, ErrorReportedEventArgs error)
    {
      if (idx >= expected.Length)
      {
        mismatchesFound++;
        if (firstMismatchFoundAt < 0)
        {
          firstMismatchFoundAt = idx+1;
          firstMismatchExpected = "<no more output expected>";
          firstMismatchReceived = error.Details.ToString();
        }

        return;
      }

      var expectedDetails = ErrorDetails.Parse(expected[idx]);
      if (expectedDetails == null || !expectedDetails.IsMatch(error.Details, false, false, false))
      {
        mismatchesFound++;
        if (firstMismatchFoundAt < 0)
        {
          firstMismatchFoundAt = idx;
          firstMismatchExpected = expected[idx];
          firstMismatchReceived = error.Details.ToString();
        }
      }

      idx++;
    }

    public void VerificationFinished(object sender, VerificationFinishedEventArgs data)
    {
      var expectedMsg1 = String.Format("Verification of {0} {1}", data.Name, data.Outcome);
      var expectedMsg2 = String.Format("Verification of '{0}' {1}", data.Name, data.Outcome);

      if (idx >= expected.Length) {
        mismatchesFound++;
        if (firstMismatchFoundAt < 0) {
          firstMismatchFoundAt = idx+1;
          firstMismatchExpected = "<no more output expected>";
          firstMismatchReceived = expectedMsg2;
        }

        return;
      }

      if ((!this.expected[idx].StartsWith(expectedMsg1)) && (!this.expected[idx].StartsWith(expectedMsg2)))
      {
        mismatchesFound++;
        if (firstMismatchFoundAt < 0) {
          firstMismatchFoundAt = idx;
          firstMismatchExpected = expected[idx];
          firstMismatchReceived = expectedMsg2;
        }
      }

      idx++;
    }

    public void CompleteChecking()
    {
      if (idx != expected.Length)
      {
        this.mismatchesFound++;
        if (this.firstMismatchFoundAt < 0)
        {
          this.firstMismatchFoundAt = idx;
          this.firstMismatchExpected = this.expected[idx];
          this.firstMismatchReceived = "<end of output>";
        }
      }
    }

    public int Mismatches
    {
      get { return this.mismatchesFound; }
    }

    public int FirstMismatchFoundAt
    {
      get { return this.firstMismatchFoundAt; }
    }

    public string FirstMismatchReceived
    {
      get { return firstMismatchReceived; }
    }

    public string FirstMismatchExpected
    {
      get { return firstMismatchExpected; }
    }
  }
}
