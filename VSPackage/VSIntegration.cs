using System;
using EnvDTE;
using Microsoft.VisualStudio.Shell;

namespace MicrosoftResearch.VSPackage
{
    static class VSIntegration
    {
        private static Lazy<DTE> _dte = new Lazy<DTE>(() => { return (DTE)Package.GetGlobalService(typeof(DTE)); });
        
        /// <summary>
        ///     Returns an instance of the toplevel object for interaction with Visual Studio
        /// </summary>
        public static DTE dte
        {
            get { return _dte.Value; }
        }

        /// <summary>
        ///     Returns the name of the active document with path
        /// </summary>
        /// <returns>the name of the active document with path</returns>
        public static string GetActiveFileFullName()
        {
            if (dte.ActiveDocument != null)
            {
                return dte.ActiveDocument.FullName;
            }
            else
            {
                return String.Empty;
            }
        }

        /// <summary>
        ///     Returns the name of the active document without path
        /// </summary>
        /// <returns>the name of the active document without path</returns>
        public static string GetActiveFileName()
        {
            if (dte.ActiveDocument != null)
            {
                return dte.ActiveDocument.Name;
            }
            else
            {
                return String.Empty;
            }
        }

        /// <summary>
        ///     Returns wether the active document is in C or C++
        /// </summary>
        /// <returns>true, if active document is existent and in C or C++</returns>
        public static bool IsCodeFile()
        {
            if (dte.ActiveDocument != null)
            {
                if (dte.ActiveDocument.Language == "C/C++")
                {
                    return true;
                }
            }

            return false;
        }
        
        /// <summary>
        ///     Returns the name of the function surrounding the cursor
        /// </summary>
        /// <returns>the name of the function surrounding the cursor, null if the cursor is not in a function</returns>
        public static string GetCurrentFunctionName()
        {
            //// Get CodeElement which represents the current function
            TextDocument textDocument = (TextDocument)dte.ActiveDocument.Object(null);
            VirtualPoint currentFunctionActivePoint = textDocument.Selection.ActivePoint;
            CodeElement currentFunctionCodeElement = currentFunctionActivePoint.CodeElement[vsCMElement.vsCMElementFunction];
            if (currentFunctionCodeElement != null)
            {
                return currentFunctionCodeElement.Name;
            }
            else
            {
                return null;
            }
        }
    }
}
