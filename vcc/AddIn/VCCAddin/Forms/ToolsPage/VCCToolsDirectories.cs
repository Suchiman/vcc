using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using System.Drawing.Imaging;

namespace VerifiedCCompilerAddin.Forms.ToolsPage {
  public partial class VCCToolsDirectories : UserControl, IDTToolsOptionsPage {
    public VCCToolsDirectories() {
      InitializeComponent();
    }
    private Test sets = new Test();
    public Test Settings { get { return sets; } set { sets = value; } }

    #region IDTToolsOptionsPage Members

    public void GetProperties(ref object PropertiesObject) {
      throw new NotImplementedException();
    }

    public void OnAfterCreated(DTE DTEObject) {
      propertyGrid1.SelectedObject = Settings;
    }

    public void OnCancel() {
      throw new NotImplementedException();
    }

    public void OnHelp() {
      throw new NotImplementedException();
    }

    public void OnOK() {
      throw new NotImplementedException();
    }

    #endregion
  }

  public class Test {

    [TypeConverter()]
    [Editor(typeof(System.Windows.Forms.Design.FolderNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
    public string VCCHeadersDirectory { get; set; }


    [TypeConverter()]
    [Editor(typeof(System.Windows.Forms.Design.FileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
    [Category("VCC Main-Executables")]
    [Description("Full path to VCC.EXE")]
    public string VCC1Executable { get; set; }

    [TypeConverter()]
    [Editor(typeof(System.Windows.Forms.Design.FileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
    [Category("VCC Main-Executables")]
    [Description("Full path to VCC2.EXE")]
    public string VCC2Executable { get; set; }

    [TypeConverter()]
    [Editor(typeof(System.Windows.Forms.Design.FileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
    [Category("VCC Tools")]
    [Description("Full path to CL.EXE")]
    public string CLExecutable { get; set; }
    
  }
}
