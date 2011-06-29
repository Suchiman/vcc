namespace VerifiedCCompilerAddin.Forms.ToolsPage {
  partial class VCCToolsPage {
    /// <summary> 
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary> 
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing) {
      if (disposing && (components != null)) {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Component Designer generated code

    /// <summary> 
    /// Required method for Designer support - do not modify 
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent() {
      this.vccSettingsControl1 = new VerifiedCCompilerAddin.Forms.VCCSettingsControl.VCCSettingsControl();
      this.SuspendLayout();
      // 
      // vccSettingsControl1
      // 
      this.vccSettingsControl1.Location = new System.Drawing.Point(3, 3);
      this.vccSettingsControl1.Name = "vccSettingsControl1";
      this.vccSettingsControl1.Size = new System.Drawing.Size(490, 227);
      this.vccSettingsControl1.TabIndex = 0;
      this.vccSettingsControl1.Version = "";
      // 
      // VCCToolsPage
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.vccSettingsControl1);
      this.Name = "VCCToolsPage";
      this.Size = new System.Drawing.Size(496, 315);
      this.ResumeLayout(false);
    }

    #endregion

    private VerifiedCCompilerAddin.Forms.VCCSettingsControl.VCCSettingsControl vccSettingsControl1;



  }
}
