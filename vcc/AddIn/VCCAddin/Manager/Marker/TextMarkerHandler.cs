//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;

namespace VerifiedCCompilerAddin.Manager.Marker {
  #region TextMarkerHandler
  /// <summary>
  /// Responsable for displaying ToolTips over Markers
  /// </summary>
  public class TextMarkerHandler : IVsTextMarkerClient {
    private string ErrorHint;
    /// <summary>
    /// Constructor
    /// </summary>
    /// <param name="Message">Message that apears while hovering a marker</param>
    public TextMarkerHandler(string Message) {
      ErrorHint = Message;
    }

    public int ExecMarkerCommand(IVsTextMarker pMarker, int iItem) {
      return VSConstants.S_OK;
    }

    public int GetMarkerCommandInfo(IVsTextMarker pMarker, int iItem, string[] pbstrText, uint[] pcmdf) {
      return -1;
    }

    public int GetTipText(IVsTextMarker pMarker, string[] pbstrText) {
      pbstrText[0] = ErrorHint;
      return VSConstants.S_OK;
    }

    public void MarkerInvalidated() {
      return;
    }

    public int OnAfterMarkerChange(IVsTextMarker pMarker) {
      return VSConstants.S_OK;
    }

    public void OnAfterSpanReload() {
      return;
    }

    public void OnBeforeBufferClose() {
      return;
    }

    public void OnBufferSave(string pszFileName) {
      return;
    }
  }
  #endregion
}
