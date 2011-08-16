using System;
using System.Linq;
using EnvDTE;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace Microsoft.Research.Vcc.VSPackage
{
  /// <summary>
  ///     This class handles interaction with Visual Studio
  /// </summary>
  internal static class VSIntegration
  {
    private static readonly Lazy<DTE> _dte = new Lazy<DTE>(() => (DTE)Package.GetGlobalService(typeof(DTE)));

    /// <summary>
    ///     Returns an instance of the toplevel object for interaction with Visual Studio
    /// </summary>
    internal static DTE DTE
    {
      get { return _dte.Value; }
    }

    internal static void UpdateStatus(string text, bool animationOn)
    {
      DTE.StatusBar.Text = text;
      DTE.StatusBar.Animate(animationOn, vsStatusAnimation.vsStatusAnimationGeneral);
    }

    #region Outputpane

    private static readonly Lazy<IVsOutputWindowPane> _pane = new Lazy<IVsOutputWindowPane>(() =>
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
    private static IVsOutputWindowPane VerificationOutputpane
    {
      get { return _pane.Value; }
    }

    internal static void ClearPane()
    {
      VerificationOutputpane.Clear();
    }

    internal static void WriteToPane(string message)
    {
      VerificationOutputpane.OutputString(message);
    }

    #endregion

    #region document info

    internal static int CurrentLine
    {
      get
      {
        TextDocument textDocument = (TextDocument)DTE.ActiveDocument.Object(null);
        return textDocument.Selection.ActivePoint.Line;
      }
    }

    internal static int CurrentErrorModel
    {
      get
      {
        return markers.IndexOfKey(new Tuple<int, string>(CurrentLine, ActiveFileFullName));
      }
    }

    internal static bool CurrentLineHasError
    {
      get { return markers.ContainsKey(new Tuple<int, string>(CurrentLine, ActiveFileFullName)); }
    }

    /// <summary>
    ///     Returns the name of the active document with path
    /// </summary>
    /// <returns>the name of the active document with path</returns>
    internal static string ActiveFileFullName
    {
      get { return DTE.ActiveDocument != null ? DTE.ActiveDocument.FullName : String.Empty; }
    }

    /// <summary>
    ///     Returns the name of the active document without path
    /// </summary>
    /// <returns>the name of the active document without path</returns>
    internal static string ActiveFileName
    {
      get { return DTE.ActiveDocument != null ? DTE.ActiveDocument.Name : String.Empty; }
    }

    /// <summary>
    ///     Returns wether the active document is in C or C++
    /// </summary>
    /// <returns>true, if active document is existent and in C or C++</returns>
    internal static bool IsCodeFile
    {
      get
      {
        if (DTE.ActiveDocument != null)
        {
          if (DTE.ActiveDocument.Language == "C/C++")
          {
            return true;
          }
        }

        return false;
      }
    }

    /// <summary>
    ///     Returns the selected Text
    /// </summary>
    /// <returns>the selected Text</returns>
    internal static string CurrentSelection
    {
      get
      {
        TextDocument textDocument = (TextDocument)DTE.ActiveDocument.Object(null);
        return textDocument.Selection.Text;
      }
    }

    #endregion

    #region project settings

    internal static string CurrentPlatform
    {
      get
      {
        SelectedItem sitem = DTE.SelectedItems.Item(1);
        return sitem.ProjectItem.ConfigurationManager.ActiveConfiguration.PlatformName;
      }
    }

    internal static CompilerSettings CurrentCompilerSettings
    {
      get
      {
        SelectedItem sitem = DTE.SelectedItems.Item(1);
        return new CompilerSettings(sitem.ProjectItem);
      }
    }

    #endregion

    #region document saving

    internal static bool DocumentsSavedCheck(VccOptionPage options) {
      if (DTE.Documents.Cast<Document>().All(document => document.Saved)) return true;

      if (options.SaveMode == SaveMode.Automatically) {
        DTE.Documents.SaveAll();
        return true;
      }
      else {
        if (MessageBox.Show(
          "There are unsaved changes. Press OK to save all documents and proceed with the verification.",
          "Unsaved Changes",
          MessageBoxButtons.OKCancel,
          MessageBoxIcon.Question,
          MessageBoxDefaultButton.Button1) == DialogResult.OK) {
            DTE.Documents.SaveAll();
          return true;
        }
        else {
          return false;
        }
      }
    }

    #endregion

    #region error list and markers

    private static readonly ErrorListProvider errorListProvider = new ErrorListProvider(VSPackagePackage.Instance);
    //// this just helps with underlining just the code, no preceding whitespaces or comments
    private static readonly Regex CodeLine = new Regex(@"(?<whitespaces>(\s*))(?<code>.*?)(?<comment>\s*(//|/\*).*)?$");

    private static readonly SortedList<Tuple<int, string>, IVsTextLineMarker> markers = new SortedList<Tuple<int, string>, IVsTextLineMarker>();

    internal static void ClearErrorsAndMarkers()
    {
      foreach (var entry in markers)
      {
        entry.Value.Invalidate();
      }
      markers.Clear();
      errorListProvider.Tasks.Clear();
    }

    /// <summary>
    ///     Adds an entry to the error list
    /// </summary>
    /// <param name="document">Complete path of the document in which the error was found</param>
    /// <param name="text">Errormessage</param>
    /// <param name="line">the line, counting from one</param>
    /// <param name="category">is this an error or a warning?</param>
    internal static void AddErrorToErrorList(string document, string text, int line, TaskErrorCategory category)
    {
      ErrorTask errorTask = new ErrorTask
                              {
                                ErrorCategory = category,
                                Document = document,
                                Text = text,
                                Line = line - 1,
                                Column = 0
                              };
      errorTask.Navigate += errorTask_Navigate;
      errorListProvider.Tasks.Add(errorTask);
      AddMarker(document, line, text);
    }

    /// <summary>
    ///     Adds squigglies to the specified line. The line is underlined starting with the first nonwhitespace character.
    /// </summary>
    /// <param name="document"></param>
    /// <param name="line">the line, counting from one</param>
    /// <param name="text"></param>
    internal static void AddMarker(string document, int line, string text)
    {
      var markersKey = new Tuple<int, string>(line, document);
      if (markers.ContainsKey(markersKey)) return;

      IVsUIShellOpenDocument uiShellOpenDocument =
        Package.GetGlobalService(typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;
      if (uiShellOpenDocument == null)
      {
        return;
      }

      //// get hierCaller i.e. the project containing the file containing the error
      string projectUniqueName = null;
      foreach (Document currentDocument in DTE.Documents)
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

      IVsUIHierarchy hierOpen;
      uint[] itemIdOpen = new uint[1];
      IVsWindowFrame windowFrame;
      int open;

      //// this is called to get windowFrame which we need to get the IVsTextLines Object
      if (uiShellOpenDocument.IsDocumentOpen(hierCaller,
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
        if (windowFrame == null)
        {
          return;
        }

        object docData;
        windowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);
        IVsTextLines lines = docData as IVsTextLines;

        MarkerClient textMarkerClient = new MarkerClient(text);

        int lineLength;
        lines.GetLengthOfLine(line - 1, out lineLength);

        string lineText;
        lines.GetLineText(line - 1, 0, line - 1, lineLength, out lineText);

        if (lineText != null)
        {

          //// This is used to get the position of the first non-whitespace character
          Match match = CodeLine.Match(lineText);
          var wsLength = match.Groups["whitespaces"].Length;
          LineMarkerClosure createLineMarker = new LineMarkerClosure(lines,
                                                                   (int)MARKERTYPE.MARKER_OTHER_ERROR,
                                                                   line - 1,
                                                                   wsLength,
                                                                   line - 1,
                                                                   wsLength + match.Groups["code"].Length,
                                                                   textMarkerClient);

          var marker = ThreadHelper.Generic.Invoke<IVsTextLineMarker>(createLineMarker.CreateMarker);

          if (marker != null)
          {
            markers[markersKey] = marker;
          }
        }
      }
    }

    internal static void ModelViewer_LineColumnChanged(object sender, VccModelViewer.LineColumnChangedEventArgs e)
    {
      try
      {
        Document Doc = DTE.Documents.Cast<object>().Where(doc => e.fileName.Equals(((Document)doc).FullName, StringComparison.OrdinalIgnoreCase)).Cast<Document>().FirstOrDefault();

        if (Doc != null)
        {
          TextDocument textDocument = Doc.Object(null) as TextDocument;
          int tabsize = Doc.TabSize;
          textDocument.Selection.MoveTo(e.lineNumber, e.columnNumber, false);
          textDocument.Selection.SelectLine();
          string Text = textDocument.Selection.Text;

          int newcolumn = 0;
          for (int idx = 0; idx < e.columnNumber; idx++)
          {
            if (Text[idx] == '\t')
            {
              newcolumn += tabsize - (newcolumn % tabsize);
            }
            else
            {
              newcolumn++;
            }
          }

          textDocument.Selection.MoveTo(e.lineNumber, newcolumn);
        }
      }
      catch { }
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
        uiShellOpenDocument.OpenDocumentViaProject(errorTask.Document,
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
        textManager.NavigateToLineAndColumn(buffer,
                                                ref logicalView,
                                                errorTask.Line,
                                                errorTask.Column,
                                                errorTask.Line,
                                                errorTask.Column);
      }
    }

    #endregion

    public static void InsertIntoCurrentDocument(string str)
    {
        TextDocument textDocument = DTE.ActiveDocument.Object(null) as TextDocument;
        if (textDocument != null)
        {
            textDocument.Selection.Insert(str);
        }
    }
  }
}