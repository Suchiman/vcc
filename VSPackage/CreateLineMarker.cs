using Microsoft.VisualStudio.TextManager.Interop;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     An Instance of this helper class generates a textmarker by calling IVsTextLines.createLineMarker.
    ///     This class is necessary, because a delegate with parameters is needed which is to be called by
    ///     the UI-thread.
    /// </summary>
    class CreateLineMarker
    {
        private readonly IVsTextLines lines;
        private readonly int markerType;
        private readonly int startLine;
        private readonly int startIndex;
        private readonly int endLine;
        private readonly int endIndex;
        private readonly IVsTextMarkerClient client;
        private readonly IVsTextLineMarker[] marker;

        public CreateLineMarker(   IVsTextLines lines,
                            int markerType,
                            int startLine,
                            int startIndex,
                            int endLine,
                            int endIndex,
                            IVsTextMarkerClient client,
                            IVsTextLineMarker[] marker)
        {
            this.lines = lines;
            this.markerType = markerType;
            this.startLine = startLine;
            this.startIndex = startIndex;
            this.endLine = endLine;
            this.endIndex = endIndex;
            this.client = client;
            this.marker = marker;
        }

        public void doIt()
        {
            lines.CreateLineMarker( markerType,
                                    startLine,
                                    startIndex,
                                    endLine,
                                    endIndex,
                                    client,
                                    marker);
        }

    }
}
