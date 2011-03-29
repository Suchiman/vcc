namespace MicrosoftResearch.VSPackage
{
  using System;
  using System.Runtime.InteropServices;
  using Microsoft.VisualStudio.Shell;
  using VccModelViewer;

  [Guid(GuidList.guidErrorToolWindowPersistanceString)]
  internal sealed class ErrorModelToolWindow : ToolWindowPane
  {
    private static readonly Lazy<ModelViewer> modelViewerInstance = new Lazy<ModelViewer>(() => new ModelViewer(true));

    public ErrorModelToolWindow() : base(null)
    {
      this.Caption = "Vcc Error Model";
      this.BitmapResourceID = 9892;
      this.BitmapIndex = 1;
      //this.Content = ErrorModelToolWindow.ModelViewer;
    }

    public override System.Windows.Forms.IWin32Window Window
    {
      get { return ErrorModelToolWindow.ModelViewer; }
    }

    public static ModelViewer ModelViewer
    {
      get { return modelViewerInstance.Value; }
    }
  }
}
