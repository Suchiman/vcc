﻿using System.Collections.Generic;

namespace Microsoft.Research.Vcc.Cpp
{
  class TransOptions : Helper.Options
  {
    private readonly string[] pipeOperations;

    public TransOptions(string[] pipeOperations)
    {
      this.pipeOperations = pipeOperations; 
    }

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
      get { return pipeOperations; }
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
