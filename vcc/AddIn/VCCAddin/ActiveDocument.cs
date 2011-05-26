//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using EnvDTE;
using System.IO;

namespace VerifiedCCompilerAddin {
  /// <summary>
  /// Wraps the EnvDTE.ActiveDocument for use with add-in.
  /// </summary>
  public static class ActiveDocumentWrapper {

    private static Document Doc {
      get { return getActiveDocument(); }
    }

    public static string FileName { 
      get { return getFileName(); } 
    }

    public static string CurrentFunctionName {
      get { return getCurrentFunctionName(); } 
    }

    public static int CurrentLineNumber {
      get { return getCurrentLineNumber(); } 
    }

    public static ProjectItem ProjectItem {
      get { return Doc.ProjectItem; }
    }

    private static Document getActiveDocument() {
      return Utilities.GlobalDTE.ActiveDocument;
    }

    private static TextDocument getTextDocument()
    {
      TextDocument textDocument = Doc.Object(null) as TextDocument;
      return textDocument;
    }
    private static TextPoint getActiveTextPoint(TextDocument textDocument) {
      TextPoint activeTextPoint = textDocument.Selection.ActivePoint;
      return activeTextPoint;
    }

    private static CodeElement getFunctionCodeElement(TextPoint textPoint) {
      CodeElement codeElement = Doc.ProjectItem.FileCodeModel.CodeElementFromPoint(textPoint, vsCMElement.vsCMElementFunction);
      return codeElement;
    }

    private static string getCurrentFunctionName() {
      try {
        
        TextDocument textDocument = getTextDocument();
        TextPoint activeTextPoint = getActiveTextPoint(textDocument);
        CodeElement codeElement = getFunctionCodeElement(activeTextPoint);
        if (codeElement != null)
          return codeElement.Name;
        else
          return null;
      } catch (NullReferenceException) {
        return null;
      }
    }
    private static int getCurrentLineNumber() {
      try {
        TextDocument textDocument = getTextDocument();
        TextPoint activeTextPoint = getActiveTextPoint(textDocument);
        
        return activeTextPoint.Line;
      } catch (NullReferenceException) {
        return -1;
      }
    }
    private static string getFileName()
    {
      try {
        return Doc.FullName;
      } catch (NullReferenceException) {
        return null;
      }
    }

    public static bool IsCFile(out string FileName) {
      FileName = string.Empty;

      try {
        if (IsInVCProj()) {
          FileName = Doc.FullName.ToLower();
          FileName = Path.GetFileName(FileName);
          if (FileName.EndsWith(".c") || FileName.EndsWith(".h"))
            return true;
        }
      } 
      catch (NullReferenceException) 
      { }

      return false;
    }
    public static bool IsInFunction(out string FunctionName) {

      FunctionName = CurrentFunctionName;
      if (FunctionName == null || !IsInVCProj())
        return false;
      else
        return true;
    }
    
    internal static bool IsInVCProj() {
      try {
        if (Doc == null)
          return false;
        ProjectItem prjItem = Doc.ProjectItem;
        Project prj = prjItem.ContainingProject;
        return Utilities.IsVCPrj(prj);
      } catch {
        return false;
      }
    }

  }
}
