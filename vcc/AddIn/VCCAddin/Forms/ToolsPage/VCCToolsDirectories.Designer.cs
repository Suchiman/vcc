namespace VerifiedCCompilerAddin.Forms.ToolsPage {
  partial class VCCToolsDirectories {
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
      this.propertyGrid1 = new System.Windows.Forms.PropertyGrid();
      this.SuspendLayout();
      // 
      // propertyGrid1
      // 
      this.propertyGrid1.Font = new System.Drawing.Font("Segoe UI", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.propertyGrid1.Location = new System.Drawing.Point(3, 3);
      this.propertyGrid1.Name = "propertyGrid1";
      this.propertyGrid1.Size = new System.Drawing.Size(454, 324);
      this.propertyGrid1.TabIndex = 0;
      this.propertyGrid1.ToolbarVisible = false;
      // 
      // VCCToolsDirectories
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.propertyGrid1);
      this.Name = "VCCToolsDirectories";
      this.Size = new System.Drawing.Size(460, 330);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.PropertyGrid propertyGrid1;
  }
}
