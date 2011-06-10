using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio;

namespace Microsoft.Research.Vcc.VSPackage
{
    public class MarkerClient : IVsTextMarkerClient
    {
        private readonly string errorHint;

        public MarkerClient(string message)
        {
            errorHint = message;
        }

        public int ExecMarkerCommand(IVsTextMarker pMarker, int iItem)
        {
            return -1;
        }

        public int GetMarkerCommandInfo(IVsTextMarker pMarker, int iItem, string[] pbstrText, uint[] pcmdf)
        {
            return -1;
        }

        public int GetTipText(IVsTextMarker pMarker, string[] pbstrText = null)
        {
            pbstrText[0] = errorHint;
            return VSConstants.S_OK;
        }

        public void MarkerInvalidated() { }

        public int OnAfterMarkerChange(IVsTextMarker pMarker)
        {
            return VSConstants.S_OK;
        }

        public void OnAfterSpanReload() { }

        public void OnBeforeBufferClose() { }

        public void OnBufferSave(string pszFileName) { }
    }
}
