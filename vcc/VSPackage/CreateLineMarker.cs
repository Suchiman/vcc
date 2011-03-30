using Microsoft.VisualStudio.TextManager.Interop;

namespace Microsoft.Research.Vcc.VSPackage
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

    public CreateLineMarker(IVsTextLines lines,
                        int markerType,
                        int startLine,
                        int startIndex,
                        int endLine,
                        int endIndex,
                        IVsTextMarkerClient client)
    {
      this.lines = lines;
      this.markerType = markerType;
      this.startLine = startLine;
      this.startIndex = startIndex;
      this.endLine = endLine;
      this.endIndex = endIndex;
      this.client = client;
    }

    public IVsTextLineMarker CreateMarker()
    {
      IVsTextLineMarker[] markers = new IVsTextLineMarker[1];
      lines.CreateLineMarker(markerType,
                              startLine,
                              startIndex,
                              endLine,
                              endIndex,
                              client,
                              markers);
      return markers[0];
    }

  }
}
