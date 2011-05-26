namespace Microsoft.Research.Vcc.VSPackage
{
  using System;
  using System.Runtime.InteropServices;
  using Microsoft.VisualStudio.Shell;
  using VccModelViewer;

  [Guid(GuidList.GuidErrorToolWindowPersistanceString)]
  internal sealed class ErrorModelToolWindow : ToolWindowPane
  {
    private static readonly Lazy<ModelViewer> modelViewerInstance = new Lazy<ModelViewer>(() => new ModelViewer(true));

    public ErrorModelToolWindow() : base(null)
    {
      this.Caption = "Vcc Error Model";
      this.BitmapResourceID = 301;
      this.BitmapIndex = 5;
    }

    public override System.Windows.Forms.IWin32Window Window
    {
      get { return modelViewerInstance.Value; }
    }

    public static ModelViewer ModelViewer
    {
      get { return modelViewerInstance.Value; }
    }
  }
}
