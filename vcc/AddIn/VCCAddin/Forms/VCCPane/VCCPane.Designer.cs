//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using VerifiedCCompilerAddin.Forms.UserControls;
namespace VerifiedCCompilerAddin.Forms {
  partial class VCCPane {
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
      this.components = new System.ComponentModel.Container();
      VerifiedCCompilerAddin.Forms.UserControls.ListBoxColors listBoxColors1 = new VerifiedCCompilerAddin.Forms.UserControls.ListBoxColors();
      this.btnVerify = new System.Windows.Forms.Button();
      this.lbActiveFile = new System.Windows.Forms.Label();
      this.lbStaticText = new System.Windows.Forms.Label();
      this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
      this.verifyFunctionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.cancelVCCToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.verifyListBox1 = new VerifiedCCompilerAddin.Forms.UserControls.VerifyListBox();
      this.contextMenuStrip1.SuspendLayout();
      this.SuspendLayout();
      // 
      // btnVerify
      // 
      this.btnVerify.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.btnVerify.Location = new System.Drawing.Point(353, 1);
      this.btnVerify.Name = "btnVerify";
      this.btnVerify.Size = new System.Drawing.Size(104, 23);
      this.btnVerify.TabIndex = 0;
      this.btnVerify.Text = "Verify Selected";
      this.btnVerify.UseVisualStyleBackColor = true;
      this.btnVerify.Click += new System.EventHandler(this.VerifyButton_Click);
      // 
      // lbActiveFile
      // 
      this.lbActiveFile.AutoSize = true;
      this.lbActiveFile.Location = new System.Drawing.Point(69, 6);
      this.lbActiveFile.Name = "lbActiveFile";
      this.lbActiveFile.Size = new System.Drawing.Size(60, 13);
      this.lbActiveFile.TabIndex = 1;
      this.lbActiveFile.Text = "(not active)";
      // 
      // lbStaticText
      // 
      this.lbStaticText.AutoSize = true;
      this.lbStaticText.Location = new System.Drawing.Point(0, 6);
      this.lbStaticText.Name = "lbStaticText";
      this.lbStaticText.Size = new System.Drawing.Size(63, 13);
      this.lbStaticText.TabIndex = 4;
      this.lbStaticText.Text = "Current File:";
      // 
      // contextMenuStrip1
      // 
      this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.verifyFunctionToolStripMenuItem,
            this.cancelVCCToolStripMenuItem});
      this.contextMenuStrip1.Name = "contextMenuStrip1";
      this.contextMenuStrip1.Size = new System.Drawing.Size(155, 48);
      // 
      // verifyFunctionToolStripMenuItem
      // 
      this.verifyFunctionToolStripMenuItem.Name = "verifyFunctionToolStripMenuItem";
      this.verifyFunctionToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
      this.verifyFunctionToolStripMenuItem.Text = "Verify Function";
      this.verifyFunctionToolStripMenuItem.Paint += new System.Windows.Forms.PaintEventHandler(this.verifyFunctionToolStripMenuItem_Paint);
      this.verifyFunctionToolStripMenuItem.Click += new System.EventHandler(this.verifyFunctionToolStripMenuItem_Click);
      // 
      // cancelVCCToolStripMenuItem
      // 
      this.cancelVCCToolStripMenuItem.Name = "cancelVCCToolStripMenuItem";
      this.cancelVCCToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
      this.cancelVCCToolStripMenuItem.Text = "Cancel VCC";
      this.cancelVCCToolStripMenuItem.Paint += new System.Windows.Forms.PaintEventHandler(this.cancelVCCToolStripMenuItem_Paint);
      this.cancelVCCToolStripMenuItem.Click += new System.EventHandler(this.cancelVCCToolStripMenuItem_Click);
      // 
      // verifyListBox1
      // 
      this.verifyListBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.verifyListBox1.Colors = listBoxColors1;
      this.verifyListBox1.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
      this.verifyListBox1.FormattingEnabled = true;
      this.verifyListBox1.ImeMode = System.Windows.Forms.ImeMode.Close;
      this.verifyListBox1.IntegralHeight = false;
      this.verifyListBox1.ItemHeight = 15;
      this.verifyListBox1.Location = new System.Drawing.Point(0, 25);
      this.verifyListBox1.Name = "verifyListBox1";
      this.verifyListBox1.Size = new System.Drawing.Size(457, 154);
      this.verifyListBox1.TabIndex = 5;
      this.verifyListBox1.MouseUp += new System.Windows.Forms.MouseEventHandler(this.verifyListBox1_MouseUp);
      // 
      // VCCPane
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
      this.Controls.Add(this.verifyListBox1);
      this.Controls.Add(this.lbStaticText);
      this.Controls.Add(this.lbActiveFile);
      this.Controls.Add(this.btnVerify);
      this.MinimumSize = new System.Drawing.Size(250, 50);
      this.Name = "VCCPane";
      this.Size = new System.Drawing.Size(457, 179);
      this.contextMenuStrip1.ResumeLayout(false);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Button btnVerify;
    private System.Windows.Forms.Label lbActiveFile;
    private System.Windows.Forms.Label lbStaticText;
    private VerifyListBox verifyListBox1;
    private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
    private System.Windows.Forms.ToolStripMenuItem verifyFunctionToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem cancelVCCToolStripMenuItem;
  }
}
