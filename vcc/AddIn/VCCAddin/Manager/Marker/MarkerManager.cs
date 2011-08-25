//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

namespace VerifiedCCompilerAddin.Manager.Marker {

  public class MarkerManager {

    /// <summary>
    /// List of all markers that are set during a session.
    /// </summary>
    private Dictionary<IVsTextLineMarker, TextMarkerHandler> Markers = new Dictionary<IVsTextLineMarker, TextMarkerHandler>();
    /// <summary>
    /// Stores VCCErrors
    /// </summary>
    private List<VCCErrorItem> GeneralErrors = new List<VCCErrorItem>();
    private List<VCCErrorItem> VCCModelErrors = new List<VCCErrorItem>();

    public List<VCCErrorItem> Errors { get { return VCCModelErrors; } }

    public MarkerManager() {
      Markers.Clear();
      GeneralErrors.Clear();
      VCCModelErrors.Clear();
    }

    /// <summary>
    /// If a line is Changed, than remove Marker in this line.
    /// Maybe the error has fixed, so we don't highlight it anymore.
    /// </summary>
    /// <param name="FullPath"></param>
    /// <param name="Line"></param>
    public void LineChanged(string FullPath, int Line) {
      IVsTextLineMarker marker = null;

      Microsoft.VisualStudio.OLE.Interop.IServiceProvider serviceProvider = (Microsoft.VisualStudio.OLE.Interop.IServiceProvider)AddInGlobals.DTE;
      Guid SID = typeof(SVsRunningDocumentTable).GUID;
      Guid IID = typeof(IVsRunningDocumentTable).GUID;
      IntPtr output = (IntPtr)0;
      serviceProvider.QueryService(ref SID, ref IID, out output);
      IVsRunningDocumentTable documentTable = (IVsRunningDocumentTable)Marshal.GetObjectForIUnknown(output);
      IVsHierarchy ppHier;
      uint itemid;
      IntPtr ppunkDocData;
      uint pdwCookie;
 
      if (documentTable.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_NoLock, FullPath, out ppHier, out itemid, out ppunkDocData, out pdwCookie) == VSConstants.S_OK) {
        IVsTextLines lines = Marshal.GetObjectForIUnknown(ppunkDocData) as IVsTextLines;
        
        lines.FindMarkerByLineIndex((int)MARKERTYPE.MARKER_OTHER_ERROR,
                                         Line - 1,
                                         0,
                                         (int)FINDMARKERFLAGS.FM_FORWARD,
                                         out marker);

        if (marker != null) {
          TextSpan[] tsp = new TextSpan[1];
          marker.GetCurrentSpan(tsp);
          if (tsp[0].iStartLine == Line-1) {
            marker.Invalidate();
            RemoveMarker(marker, FullPath, Line);
          }
        }
        
      }
    }

    private void RemoveMarker(IVsTextLineMarker marker, string FullPath, int Line) {
      //Remove marker from list.
      Markers.Remove(marker);

      //Remove error from list.
      for (int i = 0; i < GeneralErrors.Count; i++) {
        if ((GeneralErrors[i].FileName.ToLower() == FullPath.ToLower()) && (GeneralErrors[i].Line == Line - 1))
          GeneralErrors.RemoveAt(i);
      }
    }

    /// <summary>
    /// Removes all known markers. 
    /// Called e.g. befor a new VCC run ist started.
    /// </summary>
    public void InvalidateAllMarkers() {
      foreach (IVsTextMarker Marker in Markers.Keys)
        Marker.Invalidate();

      Markers.Clear();
      GeneralErrors.Clear();
      VCCModelErrors.Clear();
    }

    /// <summary>
    /// Marks a line with a wavyline...
    /// </summary>
    /// <param name="FullPath">FullName of Doc, absolute Filename</param>
    /// <param name="Message">Message thats popup in ToolTip</param>
    /// <param name="startLine">Line in that the error wavy ouccurs</param>
    public void CreateMarker(string FullPath, string Message, string Type, int startLine, bool Update) {
      //Remeber Errors in seperate list, we mark only open documents with errors.
      //UpdateDocument tries to mark closed documents after they opened.
      if (!Update) {
        VCCErrorItem Item = new VCCErrorItem(startLine, 0, Type, FullPath, Message);
        GeneralErrors.Add(Item);
        //Only for assertion, pre and post condition Erorr exists a Model
        Regex ErrNumRegex = new Regex("VC(?<ErrNum>[8-9][0-9][0-9][0-9])");
        Match errMatch = ErrNumRegex.Match(Type);
        if (errMatch.Success)
        {
          int no;
          if (int.TryParse(errMatch.Groups["ErrNum"].Value, out no) &&
              ((8000 <= no && no <= 8999) || (9500 <= no && no <= 9502)))
          {
            VCCModelErrors.Add(Item);
          }
        }
      }

      IVsTextLineMarker[] marker_ret = new IVsTextLineMarker[1];
      Microsoft.VisualStudio.OLE.Interop.IServiceProvider serviceProvider = (Microsoft.VisualStudio.OLE.Interop.IServiceProvider)AddInGlobals.DTE;
      Guid SID = typeof(SVsRunningDocumentTable).GUID;
      Guid IID = typeof(IVsRunningDocumentTable).GUID;
      IntPtr output = (IntPtr)0;
      serviceProvider.QueryService(ref SID, ref IID, out output);
      IVsRunningDocumentTable documentTable = (IVsRunningDocumentTable)Marshal.GetObjectForIUnknown(output);
      IVsHierarchy ppHier;
      uint itemid;
      IntPtr ppunkDocData;
      uint pdwCookie;
      //Check if document opened, then mark it!
      if (documentTable.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_NoLock, FullPath, out ppHier, out itemid, out ppunkDocData, out pdwCookie) == VSConstants.S_OK) {
        IVsTextLines lines = Marshal.GetObjectForIUnknown(ppunkDocData) as IVsTextLines;

        //Calculate start position in Line...
        int linelen;
        string LineString;
        lines.GetLengthOfLine(Convert.ToInt32(startLine), out linelen);
        lines.GetLineText(startLine, 0, startLine, linelen, out LineString);
        int start = LineString.Length - LineString.TrimStart(' ', '\t').Length;

        //Set Marker only if there is no one, 
        //is there allready one first one wins!
        IVsTextLineMarker mark;
        lines.FindMarkerByLineIndex((int)MARKERTYPE.MARKER_OTHER_ERROR, startLine, 1, (int)FINDMARKERFLAGS.FM_FORWARD, out mark);
        TextSpan[] tsp = new TextSpan[1];
        
        if (mark != null) {
          mark.GetCurrentSpan(tsp);
        }
     
        if (startLine != tsp[0].iStartLine || mark == null)
        {
          //Set Marker
          TextMarkerHandler handler;
          handler = new TextMarkerHandler(Message);
          lines.CreateLineMarker((int)MARKERTYPE.MARKER_OTHER_ERROR,
                                    Convert.ToInt32(startLine),
                                    start,
                                    Convert.ToInt32(startLine),
                                    linelen,
                                    handler,
                                    marker_ret);

          if (marker_ret.Length > 0)
            if (marker_ret[0] != null)
              Markers.Add(marker_ret[0], handler);
        }
        documentTable.UnlockDocument((uint)_VSRDTFLAGS.RDT_NoLock, pdwCookie);
      }
    }

    /// <summary>
    /// Called if a document is opened after a VCC run, so we need to mark the errors.
    /// </summary>
    /// <param name="FullPath">absolute filename</param>
    public void UpdateDocument(string FullPath) {
      foreach (VCCErrorItem item in GeneralErrors) {
        if (item.FileName.ToLower() == FullPath.ToLower()) {
          CreateMarker(FullPath, item.Message, item.Type, Convert.ToInt32(item.Line), true);
        }
      }
    }

  }

}
