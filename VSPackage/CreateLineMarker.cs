using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     An Instance of this helper class generates a textmarker by calling IVsTextLines.createLineMarker. This class is necessary, because a delegate with
    ///     parameters is needed which is to be called by the UI-thread.
    /// </summary>
    class CreateLineMarker
    {
        private IVsTextLines lines;
        private int markerType;
        private int startLine;
        private int startIndex;
        private int endLine;
        private int endIndex;
        private IVsTextMarkerClient client;
        private IVsTextLineMarker[] marker;

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
