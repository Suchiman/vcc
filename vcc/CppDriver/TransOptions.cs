namespace Microsoft.Research.Vcc.Cpp
{
    using System.Collections.Generic;

    class TransOptions : Helper.IOptions
    {
        private readonly string[] pipeOperations;
        private readonly VccppOptions vccppOptions;

        public bool AggressivePruning
        {
            get { return false; }
        }

        public int DefExpansionLevel
        {
            get { return 0; }
        }

        public bool DeterminizeOutput
        {
            get { return false; }
        }

        public int DumpTriggers
        {
            get { return 0; }
        }

        public bool ExplicitTargetsGiven
        {
            get { return false; }
        }

        public IEnumerable<string> Functions
        {
            get { return new string[] { }; }
        }

        public bool OpsAsFunctions
        {
            get { return false; }
        }

        public IEnumerable<string> PipeOperations
        {
            get { return pipeOperations; }
        }

        public bool PrintCEVModel
        {
            get { return false; }
        }

        public bool TerminationForAll
        {
            get { return false; }
        }

        public bool TerminationForGhost
        {
            get { return false; }
        }

        public bool TerminationForPure
        {
            get { return false; }
        }

        public IEnumerable<string> WeightOptions
        {
            get { return new string[] { }; }
        }

        public bool YarraMode
        {
            get { return false; }
        }

        public TransOptions(string[] pipeOperations, VccppOptions vccppOptions)
        {
            this.vccppOptions = vccppOptions;
            this.pipeOperations = pipeOperations;
        }
    }
}
