using System;
using EnvDTE;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     This class handles all Interaction with Visual Studio
    /// </summary>
    internal static class VSIntegration
    {
        private static Lazy<DTE> _dte = new Lazy<DTE>(() => { return (DTE)Package.GetGlobalService(typeof(DTE)); });
        
        /// <summary>
        ///     Returns an instance of the toplevel object for interaction with Visual Studio
        /// </summary>
        private static DTE dte
        {
            get { return _dte.Value; }
        }

        private static Lazy<IVsOutputWindowPane> _pane = new Lazy<IVsOutputWindowPane>(() =>
        {
            IVsOutputWindow outputwindow = (IVsOutputWindow)Package.GetGlobalService(typeof(SVsOutputWindow));
            Guid guidVerificationPane = new Guid("{1EE5916F-A3C7-403C-89D8-58C61285688F}");
            IVsOutputWindowPane result;

            outputwindow.CreatePane(ref guidVerificationPane, "Verification", 1, 1);
            outputwindow.GetPane(ref guidVerificationPane, out result);
            return result;
        }
        );

        /// <summary>
        ///     This object represents the Verification Outputpane.
        /// </summary>
        private static IVsOutputWindowPane verificationOutputpane
        {
            get { return _pane.Value; }
        }

        /// <summary>
        ///     Returns the name of the active document with path
        /// </summary>
        /// <returns>the name of the active document with path</returns>
        internal static string ActiveFileFullName 
        {
            get
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
        }

        /// <summary>
        ///     Returns the name of the active document without path
        /// </summary>
        /// <returns>the name of the active document without path</returns>
        internal static string ActiveFileName
        {
            get
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
        }

        /// <summary>
        ///     Returns wether the active document is in C or C++
        /// </summary>
        /// <returns>true, if active document is existent and in C or C++</returns>
        internal static bool IsCodeFile
        {
            get
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
        }
        
        /// <summary>
        ///     Returns the selected Text or (if nothing's selected) the name of the function surrounding the cursor or an empty string
        /// </summary>
        /// <returns>the selected Text or (if nothing's selected) the name of the function surrounding the cursor or an empty string</returns>
        internal static string CurrentFunctionName
        {
            get
            {
                //// Get CodeElement which represents the current function
                TextDocument textDocument = (TextDocument)dte.ActiveDocument.Object(null);
                CodeElement currentFunctionCodeElement = textDocument.Selection.ActivePoint.CodeElement[vsCMElement.vsCMElementFunction];
                if (textDocument.Selection.Text != string.Empty)
                {
                    return textDocument.Selection.Text;
                }
                else
                {
                    //// This second check is necessary because ActivePoint's CodeElement-Property unexpectedly
                    //// returns CodeElements that are not functions
                    if (currentFunctionCodeElement != null && currentFunctionCodeElement.Kind == vsCMElement.vsCMElementFunction)
                    {
                        return currentFunctionCodeElement.Name;
                    }
                    else
                    {
                        return string.Empty;
                    }

                }
            }
        }

        internal static void ClearPane()
        {
            verificationOutputpane.Clear();
        }

        internal static void WriteToPane(string message)
        {
            verificationOutputpane.OutputString(message);
        }
    }
}