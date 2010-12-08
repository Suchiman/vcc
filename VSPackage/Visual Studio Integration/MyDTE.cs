using System;
using EnvDTE;
using Microsoft.VisualStudio.Shell;

namespace MicrosoftResearch.VSPackage.Visual_Studio_Integration
{
    class MyDTE
    {
        private static Lazy<DTE> _dte = new Lazy<DTE>(() => { return (DTE)Package.GetGlobalService(typeof(DTE)); });

        public static DTE Instance
        {
            get { return _dte.Value; }
        }
    }
}
