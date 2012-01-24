using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;

namespace Microsoft.Research.Vcc.Cpp
{
  class TransOptions : TransHelper.TransOptions
  {
    public override bool AggressivePruning
    {
      get { return false; }
    }

    public override int DefExpansionLevel
    {
      get { return 0; }
    }

    public override bool DeterminizeOutput
    {
      get { return false; }
    }

    public override int DumpTriggers
    {
      get { return 0; }
    }

    public override bool ExplicitTargetsGiven
    {
      get { return false; }
    }

    public override IEnumerable<string> Functions
    {
      get { return new string[] {}; }
    }

    public override bool OpsAsFunctions
    {
      get { return false; }
    }

    public override IEnumerable<string> PipeOperations
    {
      get { return new string[] {}; }
    }

    public override bool PrintCEVModel
    {
      get { return false; }
    }

    public override bool TerminationForAll
    {
      get { return false; }
    }

    public override bool TerminationForGhost
    {
      get { return false; }
    }

    public override bool TerminationForPure
    {
      get { return false; }
    }

    public override bool YarraMode
    {
      get { return false; }
    }
  }
}
