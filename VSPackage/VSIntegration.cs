using System;
using EnvDTE;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using Microsoft.VisualStudio.Package;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     This class handles interaction with Visual Studio
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

        private static ErrorListProvider errorListProvider = new ErrorListProvider(VSPackagePackage.Instance);
        private static Regex VccErrorMessageRegEx = new Regex("(.*?)'(?<identifier>(.*?))'.*");
        //// this just helps with underlining just the code, no preceding whitespaces or comments
        private static Regex CodeLine = new Regex(@"(?<whitespaces>(\s*))(?<code>.*?)(?<comment>\s*(//|/\*).*)?$");

        internal static void updateStatus(string text, bool animationOn)
        {
            dte.StatusBar.Text = text;
            dte.StatusBar.Animate(animationOn, vsStatusAnimation.vsStatusAnimationGeneral);
        }

        #region Outputpane

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
        
        internal static void ClearPane()
        {
            verificationOutputpane.Clear();
        }

        internal static void WriteToPane(string message)
        {
            verificationOutputpane.OutputString(message);
        }

        #endregion

        #region document info

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

        internal static string CurrentStructName
        {
            get
            {
                //// Get CodeElement which represents the current struct
                TextDocument textDocument = (TextDocument)dte.ActiveDocument.Object(null);
                CodeElement currentStructCodeElement = textDocument.Selection.ActivePoint.CodeElement[vsCMElement.vsCMElementStruct];
                if (textDocument.Selection.Text != string.Empty)
                {
                    return textDocument.Selection.Text;
                }
                else
                {
                    //// This second check is necessary because ActivePoint's CodeElement-Property unexpectedly
                    //// returns CodeElements that are not structs
                    if (currentStructCodeElement != null && currentStructCodeElement.Kind == vsCMElement.vsCMElementStruct)
                    {
                        return currentStructCodeElement.Name;
                    }
                    else
                    {
                        return string.Empty;
                    }

                }
            }
        }

        #endregion

        #region document saving

        internal static void DocumentsSavedCheck()
        {
            if (!DocumentsSaved())
            {
                DialogResult dialogResult =
                    MessageBox.Show(    "There are unsaved documents. Would you like to save all documents before proceding?",
                                        "Unsaved Items",
                                        MessageBoxButtons.YesNoCancel,
                                        MessageBoxIcon.Question,
                                        MessageBoxDefaultButton.Button3);

                switch (dialogResult)
                {
                    case DialogResult.Cancel:
                        return;
                    case DialogResult.Yes:
                        SaveAll();
                        break;
                    case DialogResult.No:
                    default:
                        break;
                }

            }
        }
        /// <summary>
        ///     Returns wether all open documents were saved after the last change
        /// </summary>
        /// <returns>true, when no changes were made since the last save, false, if there are open documents that are dirty<returns>
        private static bool DocumentsSaved()
        {
            foreach (Document document in dte.Documents)
            {
                if (!document.Saved)
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        ///     Saves all open documents that belong to the project containing the active document.
        /// </summary>
        private static void SaveAll()
        {
            dte.Documents.SaveAll();
        }

        #endregion

        #region error list and markers

        private static List<IVsTextLineMarker> markers = new List<IVsTextLineMarker>();

        internal static void initializeErrorList()
        {
            addErrorToErrorList(dte.ActiveDocument.Name, "initializing...", 1);
            clearErrorList();
        }

        internal static void clearErrorList()
        {
            errorListProvider.Tasks.Clear();
            clearMarkers();
        }

        /// <summary>
        ///     Adds an entry to the error list
        /// </summary>
        /// <param name="serviceProvider">In this case the service provider is the VSPackage</param>
        /// <param name="document">Complete path of the document in which the error was found</param>
        /// <param name="text">Errormessage</param>
        /// <param name="line">the line, counting from one</param>
        /// <param name="column"></param>
        internal static void addErrorToErrorList(string document, string text, int line)
        {
            ErrorTask errorTask = new ErrorTask();
            errorTask.ErrorCategory = TaskErrorCategory.Error;
            errorTask.Document = document;
            errorTask.Text = text;
            errorTask.Line = line - 1;
            errorTask.Column = 0;
            errorTask.Navigate += new EventHandler(errorTask_Navigate);
            errorListProvider.Tasks.Add(errorTask);
            addMarker(document, line, text);
        }

        /// <summary>
        ///     Adds squigglies to the specified line. The line is underlined starting with the first nonwhitespace character.
        /// </summary>
        /// <param name="document"></param>
        /// <param name="line">the line, counting from one</param>
        internal static void addMarker(string document, int line, string text)
        {
            IVsUIShellOpenDocument uiShellOpenDocument = Package.GetGlobalService(typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;
            if (uiShellOpenDocument == null) { return; }
            
            //// get hierCaller i.e. the project containing the file containing the error
            string projectUniqueName = null;
            foreach(Document currentDocument in dte.Documents)
            {
                if (currentDocument.FullName == document)
                {
                    projectUniqueName = currentDocument.ProjectItem.ContainingProject.UniqueName;
                }
            }
            IVsSolution solution = Package.GetGlobalService(typeof(IVsSolution)) as IVsSolution;
            IVsHierarchy hierarchy;
            solution.GetProjectOfUniqueName(projectUniqueName, out hierarchy);
            IVsUIHierarchy hierCaller = hierarchy as IVsUIHierarchy;

            //// Set the other arguments for IsDocumentOpen
            Guid logicalView = VSConstants.LOGVIEWID_Code;

            IVsUIHierarchy hierOpen = null;
            uint[] itemIdOpen = new uint[1];
            IVsWindowFrame windowFrame = null;
            int open = 0;

            //// this is called to get windowFrame which we need to get the IVsTextLines Object
            if (uiShellOpenDocument.IsDocumentOpen(     hierCaller,
                                                        (uint)__VSIDOFLAGS.IDO_ActivateIfOpen,
                                                        document,
                                                        ref logicalView,
                                                        (uint)__VSIDOFLAGS.IDO_ActivateIfOpen,
                                                        out hierOpen,
                                                        itemIdOpen,
                                                        out windowFrame,
                                                        out open
                                                        ) == VSConstants.S_OK)
            {
                //// Document is open, get lines as IVsTextLines
                if (windowFrame == null) { return; }

                object docData;
                windowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);
                IVsTextLines lines = docData as IVsTextLines;

                MarkerClient textMarkerClient = new MarkerClient(text);
                IVsTextLineMarker[] textLineMarker = new IVsTextLineMarker[1];

                int lineLength;
                lines.GetLengthOfLine(line - 1, out lineLength);

                string lineText;
                lines.GetLineText(line - 1, 0, line - 1, lineLength, out lineText);

                //// This is used to get the position of the first non-whitespace character
                Match match = CodeLine.Match(lineText);

                CreateLineMarker createLineMarker = new CreateLineMarker( lines,
                                                                (int)MARKERTYPE.MARKER_OTHER_ERROR,
                                                                line - 1,
                                                                match.Groups["whitespaces"].Length,
                                                                line - 1,
                                                                match.Groups["whitespaces"].Length + match.Groups["code"].Length,
                                                                textMarkerClient,
                                                                textLineMarker);

                ThreadHelper.Generic.Invoke( createLineMarker.doIt );

                if (textLineMarker.Length > 0 && textLineMarker[0] != null)
                {
                    markers.Add(textLineMarker[0]);
                }
            }
        }

        private static void clearMarkers()
        {
            foreach (IVsTextLineMarker marker in markers)
            {
                marker.Invalidate();
            }
            markers.Clear();
        }
        
        internal static void errorTask_Navigate(object sender, EventArgs e)
        {
            if (sender != null)
            {
                //// Open the document if necessary
                ErrorTask errorTask = sender as ErrorTask;
                IVsUIShellOpenDocument uiShellOpenDocument = Package.GetGlobalService(typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;
                if (uiShellOpenDocument == null) { return; }
                IVsWindowFrame windowFrame;
                Microsoft.VisualStudio.OLE.Interop.IServiceProvider serviceProvider;
                IVsUIHierarchy hierachy;
                uint itemid;
                Guid logicalView = VSConstants.LOGVIEWID_Code;
                uiShellOpenDocument.OpenDocumentViaProject( errorTask.Document,
                                                            ref logicalView,
                                                            out serviceProvider,
                                                            out hierachy,
                                                            out itemid,
                                                            out windowFrame);
                if (windowFrame == null) { return; }
                object docData;
                windowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);

                //// Get the TextBuffer
                VsTextBuffer buffer = docData as VsTextBuffer;
                if (buffer == null)
                {
                    IVsTextBufferProvider bufferProvider = docData as IVsTextBufferProvider;
                    if (bufferProvider != null)
                    {
                        IVsTextLines lines;
                        bufferProvider.GetTextBuffer(out lines);
                        buffer = lines as VsTextBuffer;
                        if (buffer == null) { return; }
                    }
                }


                /* could be useful if not underlining whole lines
                
                //// Get length of identifier if there is one (else set it to 1)
                int identifierLength;
                Match match = VccErrorMessageRegEx.Match(errorTask.Text);
                if (match.Groups["identifier"] != null && match.Groups["identifier"].Value != string.Empty)
                {
                    identifierLength = match.Groups["identifier"].Length;
                }
                else
                {
                    identifierLength = 1;
                }
                */

                //// Navigate to line
                IVsTextManager textManager = Package.GetGlobalService(typeof(VsTextManagerClass)) as IVsTextManager;
                if (textManager == null) { return; }
                textManager.NavigateToLineAndColumn(    buffer, 
                                                        ref logicalView, 
                                                        errorTask.Line, 
                                                        errorTask.Column, 
                                                        errorTask.Line, 
                                                        errorTask.Column);
            }
        }

        #endregion

    }
}