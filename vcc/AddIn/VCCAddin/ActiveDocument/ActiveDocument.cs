//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin {
  /// <summary>
  /// This class handles all properties for the current opened and active document within the IDE.
  /// It also proviedes some features function to retrive current setting, and code elements. 
  /// And many more...
  /// It's based internaly of DTE2.ActiveDocument
  /// </summary>
  public class ActiveDocument {
    /// <summary>
    /// Singleton instance variable
    /// </summary>
    static private ActiveDocument instance;
    /// <summary>
    /// Lock object for instanciating the singleton
    /// </summary>
    static readonly object padlock = new object();
    /// <summary>
    /// Reference to the DTE object.
    /// </summary>
    static private DTE2 addinDTE;

    static private readonly Regex StructMatch = new Regex(@"\s*(struct|union)\s*(vcc\(\w*\)\s*)*(?<Name>\w+)\s*{");
    
    #region private properties
    /// <summary>
    /// Returns the active document from the addinDTE object.
    /// </summary>
    private static Document _Document {
      get { return addinDTE.ActiveDocument; }
    }
    /// <summary>
    /// Returns a TextDocument cast from _Document
    /// </summary>
    private static TextDocument _TextDocument {
      get { if (_Document == null)  return null;
      return (TextDocument)_Document.Object(null);
      }
    }
    /// <summary>
    /// Returns the current projectItem object.
    /// </summary>
    private static ProjectItem _ProjectItem {
      get { return _Document.ProjectItem; }
    }
    #endregion

    #region public properties
    /// <summary>
    /// Returns if active document is a C/C++ file.
    /// </summary>
    public bool IsCodeFile {
      get { return isCodeFile(); }
    }
    /// <summary>
    /// Retruns if active document is a header file
    /// </summary>
    public bool IsHeaderFile {
      get { return isHeaderFile(); }
    }
    /// <summary>
    /// Returns if caret position is within a function
    /// </summary>
    public bool IsInFunction {
      get {
        return (getCurrentFunctionName() != null);
      }
    }
    /// <summary>
    /// Returns if caret position is within a type declaration / definition
    /// </summary>
    public bool IsInType {
      get {
        return (getCurrentTypeName() != null);
      }
    }
    /// <summary>
    /// Retruns if caret position is within a group keyword
    /// </summary>
    public bool IsInGroup{
      get { return (getCurrentGroupName() != null); }
    }
    /// <summary>
    /// retruns group name at current caret position
    /// </summary>
    public string CurrentGroupName {
      get { return getCurrentGroupName(); }
    }
    /// <summary>
    /// Retrun type name at current caret position
    /// </summary>
    public string CurrentTypeName {
      get { return getCurrentTypeName(); }
    }
    /// <summary>
    /// Retruns function name at current caret postion
    /// </summary>
    public string CurrentFunctionName {
      get { return getCurrentFunctionName(); }
    }
    /// <summary>
    /// Returns current line of caret
    /// </summary>
    public int CurrentLine {
      get { return getCurrentLine(); }
    }
    /// <summary>
    /// Returns current column of caret
    /// </summary>
    public int CurrentColumn {
      get { return getCurrentColumn(); }
    }
    /// <summary>
    /// Returns current Document
    /// </summary>
    public Document Document {
      get { return _Document; }
    }
    /// <summary>
    /// Returns the current project item 
    /// </summary>
    public ProjectItem ProjectItem {
      get { return _ProjectItem; }
    }
    /// <summary>
    /// Returns the full filename of current document, with full path.
    /// </summary>
    public string FullFileName {
      get { return getFullFileName(); }
    }
    /// <summary>
    /// Returns the filename of current document without path.
    /// </summary>
    public string FileName {
      get { return getFileName(); }
    }
    /// <summary>
    /// Returns the VCCSettings of current document.
    /// </summary>
    public VCCSettings VCCSettings { 
      get { return getVCCSettings(); }
    }
    /// <summary>
    /// Returns the containing project of current document
    /// </summary>
    public Project ContainingProject {
      get { return getContainingProject(); }
    }
    #endregion

    /// <summary>
    /// private constructor, for singleton instance is is forbidden to use the default constructor.
    /// </summary>
    private ActiveDocument() {
    }
    /// <summary>
    /// Singleton constructor for ActiveDocument
    /// </summary>
    /// <param name="AddinDTE">Add-in DTE object</param>
    /// <returns>Singleton instance of ActiveDocument</returns>
    public static ActiveDocument getInstance(DTE2 AddinDTE) {
      lock (padlock) {
        if (instance == null) {
          instance = new ActiveDocument();
          addinDTE = AddinDTE;
        }
        return instance;
      }
    }

    /// <summary>
    /// Returns the current GroupName (caret position)
    /// </summary>
    /// <returns></returns>
    private static string getCurrentGroupName() {
      string Text = getCodeForCurrentLine(1);

      //Where is the right click in the document?
      int click_pos = getCurrentColumn();
      
      if (Text.Contains("def_group") || Text.Contains("in_group") || Text.Contains("inv_group")) {
        int last_in_group_def = Utilities.GetLastIndexBefore(Text, "in_group", click_pos);
        int last_def_group_def = Utilities.GetLastIndexBefore(Text, "def_group", click_pos);
        int last_inv_group_def = Utilities.GetLastIndexBefore(Text, "inv_group", click_pos);

        int start = Math.Max(last_in_group_def, Math.Max(last_def_group_def, last_inv_group_def));

        if (start == -1)
          return null;
        string Substring = Text.Substring(start);
        List<string> Args = Utilities.GetArgumentsInLine(Substring);

        if (Args.Count > 0)
          return Args[0];
      }
      return null;
    }

    /// <summary>
    /// Returns only the filename, without path
    /// </summary>
    /// <returns></returns>
    private static string getFileName()
    {
      return Path.GetFileName(_Document.FullName);
    }

    /// <summary>
   /// Returns the full path within the filename
   /// </summary>
   /// <returns></returns>
    private static string getFullFileName() {
      string FullFileName = String.Empty;
      if (_Document != null) {
        FullFileName = _Document.FullName;
      }
      return FullFileName;
    }

    /// <summary>
    /// Returns active line (caret position)
    /// </summary>
    /// <returns></returns>
    private static int getCurrentLine() {
      if (_TextDocument != null) {
        return _TextDocument.Selection.ActivePoint.Line;
      } else {
        return 0;
      }
    }

    /// <summary>
    /// Returns active Column (caret position)
    /// </summary>
    /// <returns></returns>
    private static int getCurrentColumn() {
      if (_TextDocument != null) {
        return _TextDocument.Selection.ActivePoint.DisplayColumn;
      } else {
        return 0;
      }
    }

    /// <summary>
    /// Retruns the current function name (caret position)
    /// </summary>
    /// <returns></returns>
    private static string getCurrentFunctionName() {
      CodeElement Element = getActivePointCodeElement(vsCMElement.vsCMElementFunction);
      if (Element != null) {
        return Element.Name;
      } else {
        if (AddInGlobals.ActiveDocument.getSelectedText() != string.Empty) {
          return AddInGlobals.ActiveDocument.getSelectedText();
        }
        return null;
      }
    }

    /// <summary>
    /// Retruns the current type name (caret position)
    /// </summary>
    /// <returns></returns>
    private string getCurrentTypeName() {
      CodeElement Element = getActivePointCodeElement(vsCMElement.vsCMElementStruct, vsCMElement.vsCMElementUnion);
      if (Element != null) {
        return Element.Name;
      } else {
        //2nd try to find a struct or union over RegEx
        if (Document == null )
          return null;
        return FindStructOrUnionViaRegEx(_TextDocument.Selection.ActivePoint);
        //return null;
      }
    }

    /// <summary>
    /// Checks is document a C/C++ file
    /// </summary>
    /// <returns></returns>
    private static bool isCodeFile() {
      if (_Document == null)
        return false;

      // Check if we had a project, if not don't show vcc menue items.
      if (_Document.ProjectItem.Object == null)
        return false;

      return (_Document.Language.Equals("C/C++"));
    }

    /// <summary>
    /// Returns true is document filename ends with .h
    /// </summary>
    /// <returns></returns>
    private bool isHeaderFile() {
      return FileName.ToLower().EndsWith(".h");
    }

    /// <summary>
    /// Returns the code in current line
    /// </summary>
    /// <param name="Lines">Lines to get</param>
    /// <returns></returns>
    private static string getCodeForCurrentLine(int Lines) {
      if (_TextDocument != null) {
        EditPoint p = _TextDocument.Selection.ActivePoint.CreateEditPoint();
        string CodeBlock = p.GetLines(p.Line, p.Line + Lines);
        return CodeBlock;
      } else {
        return String.Empty;
      }
    }


    /// <summary>
    /// Calculates the length of a CodeBlock with start as begining position.
    /// Remember start must be behind the first {, and before the first }
    /// </summary>
    /// <param name="Code">SourceCode as string</param>
    /// <param name="start">Startposition</param>
    /// <returns>The Length of the Codeblock</returns>
    private static int FindEndOfBlock(string Code, int start) {
      int auff = 1;
      int zu = 0;
      int chars = 0;
      int startpos = Code.IndexOf("{", start);
      string CodeBlock = Code.Substring(startpos+1);

      foreach (char c in CodeBlock) {
        chars++;

        if (c == '{')
          auff++;
        if (c == '}')
          zu++;
        if (c == '\n')
          chars++;
        if (auff == zu)
          break;
      }
      return chars;
    }

    /// <summary>
    /// Searches for the lastest Match before a specified position.
    /// </summary>
    /// <param name="Collection">RegEx Match collection</param>
    /// <param name="Position">Current Source Position</param>
    /// <returns></returns>
    private static Match FindMatch(MatchCollection Collection, int Position) {
      Match candidate = null;
      foreach (Match m in Collection) {
        if (m.Index > Position)
          break;
        candidate = m;        
      }
      return candidate;
    }

    /// <summary>
    /// Tries to find union or struct via RegEx, in case that VS-CodeModel don't work.
    /// This can be if, you use spec(...).
    /// </summary>
    /// <param name="ActivePoint">Active Position of Carret</param>
    /// <returns>Returns the name of the union or struct</returns>
    private static string FindStructOrUnionViaRegEx(VirtualPoint ActivePoint) {

      int CurrentMousePosition = ActivePoint.AbsoluteCharOffset + ActivePoint.Line;

      EditPoint p = _TextDocument.StartPoint.CreateEditPoint();
      string SourceCode = p.GetLines(p.Line, p.Line + _TextDocument.EndPoint.Line);
    
      MatchCollection Coll = StructMatch.Matches(SourceCode);
      Match Candidate = FindMatch(Coll, CurrentMousePosition);
      if (Candidate == null) {
        Debug.WriteLine("No Candidate found...");
        return null;
      }
      string MatchedName = Candidate.Groups["Name"].Value;
      
      Debug.WriteLine("Possible Candidate: " + MatchedName);
      int Length = FindEndOfBlock(SourceCode, Candidate.Index);
            
      if (CurrentMousePosition < (Candidate.Index + Length)) {
        Debug.WriteLine("Is inside " + MatchedName);
        return MatchedName;
      } else {
        Debug.WriteLine("Is outside " + MatchedName);
        return null;
      }

    }


    /// <summary>
    /// Returns the CodeElement from ActivePoint of document, with scope
    /// First element wins.
    /// </summary>
    /// <param name="scopeElements"></param>
    /// <returns>Current codeElement or null</returns>
    private static CodeElement getActivePointCodeElement(params vsCMElement[] scopeElements) {      
      if (_TextDocument != null) {
        foreach (vsCMElement scope in scopeElements) {
          CodeElement Element = _TextDocument.Selection.ActivePoint.CodeElement[scope];
          return Element;
        }
      }
      return null;
    }

    /// <summary>
    /// Replaces within current selection the pattern-string with replace-string 
    /// </summary>
    /// <param name="Pattern">String with search pattern</param>
    /// <param name="Replace">String to replace string-pattern</param>
    internal void replaceSelectedText(string Pattern, string Replace) {
      _TextDocument.Selection.ReplaceText(Pattern, Replace);
    }

    /// <summary>
    /// Returns the selectes text in active document
    /// </summary>
    /// <returns></returns>
    internal string getSelectedText() {
      return _TextDocument.Selection.Text;
    }

    /// <summary>
    /// Builds the VCCSettings of this Document
    /// </summary>
    /// <returns></returns>
    private VCCSettings getVCCSettings() {
      VCCSettings sets = new VCCSettings(ProjectItem,
                                         getActiveConfigNameOfProject());
      return sets;
    }

    /// <summary>
    /// Retruns the full Configname like Debug|x64
    /// </summary>
    /// <returns></returns>
    private string getActiveConfigNameOfProject() {
      string ActiveConfig = Utilities.GetActiveConfigOfProject(ContainingProject) + "|" + Utilities.GetActivePlatformOfProject(ContainingProject);
      return ActiveConfig;
    }
    
    /// <summary>
    /// Returns the Project of current Item
    /// </summary>
    /// <returns></returns>
    private Project getContainingProject() {
      return ProjectItem.ContainingProject;
    }

    /// <summary>
    /// Generates the xxxx::yyy string.
    /// If withGroup, then ::yyy is added, with yyy equ group name
    /// </summary>
    /// <param name="withGroup"></param>
    /// <returns></returns>
    internal string getAdmissibilityCheckString(bool withGroup) {
      
      StringBuilder sb = new StringBuilder();
      sb.AppendFormat("{0}", CurrentTypeName);
      if (withGroup && IsInGroup)
        sb.AppendFormat("::{0}", CurrentGroupName);

      return sb.ToString();
    }

    /// <summary>
    /// Returns the filename of the document, but if allowMasterFile it returns the masterfile.
    /// </summary>
    /// <param name="allowMasterFile"></param>
    /// <returns></returns>
    internal string getFileName(bool allowMasterFile) {
      string fileName = FullFileName;

      if (!isHeaderFile())
        return fileName;

      if (AddinSettingsManager.UseMasterFile && allowMasterFile) {
        fileName = Utilities.getMasterFileName();
      }
      return fileName;
    }
  }
}
