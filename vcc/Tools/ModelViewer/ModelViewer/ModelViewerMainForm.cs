using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Vcc2ModelViewer
{
  public partial class ModelViewerMainForm : Form
  {
    public ModelViewer modelViewer;

    public ModelViewerMainForm()
    {
      InitializeComponent();

      modelViewer = new ModelViewer(false);
      this.Controls.Add(modelViewer);
      modelViewer.Parent = this;
      modelViewer.Dock = DockStyle.Fill;
      modelViewer.Visible = true;
      modelViewer.AutoSizeMode = AutoSizeMode.GrowAndShrink;
    }

    public void loadModel(string fileName, int lineNumber, int modelNumber)
    {
      modelViewer.LoadModel(fileName, lineNumber, modelNumber);
    }
  }
}
