//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
namespace VerifiedCCompilerAddin.Forms {
  partial class CustomVerifyForm {
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

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent() {
      this.label1 = new System.Windows.Forms.Label();
      this.txtAddidtional = new System.Windows.Forms.TextBox();
      this.button1 = new System.Windows.Forms.Button();
      this.cbFunction = new System.Windows.Forms.CheckBox();
      this.pictureBox1 = new System.Windows.Forms.PictureBox();
      this.label2 = new System.Windows.Forms.Label();
      this.label3 = new System.Windows.Forms.Label();
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
      this.SuspendLayout();
      // 
      // label1
      // 
      this.label1.AutoSize = true;
      this.label1.Location = new System.Drawing.Point(17, 16);
      this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(195, 17);
      this.label1.TabIndex = 0;
      this.label1.Text = "Additional parameter for VCC:";
      // 
      // txtAddidtional
      // 
      this.txtAddidtional.Location = new System.Drawing.Point(21, 36);
      this.txtAddidtional.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
      this.txtAddidtional.Name = "txtAddidtional";
      this.txtAddidtional.Size = new System.Drawing.Size(488, 22);
      this.txtAddidtional.TabIndex = 1;
      // 
      // button1
      // 
      this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
      this.button1.Location = new System.Drawing.Point(411, 68);
      this.button1.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
      this.button1.Name = "button1";
      this.button1.Size = new System.Drawing.Size(100, 28);
      this.button1.TabIndex = 2;
      this.button1.Text = "&Verify";
      this.button1.UseVisualStyleBackColor = true;
      this.button1.Click += new System.EventHandler(this.button1_Click);
      // 
      // cbFunction
      // 
      this.cbFunction.AutoSize = true;
      this.cbFunction.Location = new System.Drawing.Point(21, 68);
      this.cbFunction.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
      this.cbFunction.Name = "cbFunction";
      this.cbFunction.Size = new System.Drawing.Size(150, 21);
      this.cbFunction.TabIndex = 3;
      this.cbFunction.Text = "Use active function";
      this.cbFunction.UseVisualStyleBackColor = true;
      // 
      // pictureBox1
      // 
      this.pictureBox1.BackColor = System.Drawing.SystemColors.ButtonHighlight;
      this.pictureBox1.Image = global::VerifiedCCompilerAddin.Properties.Resources.info;
      this.pictureBox1.Location = new System.Drawing.Point(21, 107);
      this.pictureBox1.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
      this.pictureBox1.Name = "pictureBox1";
      this.pictureBox1.Size = new System.Drawing.Size(21, 20);
      this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      this.pictureBox1.TabIndex = 0;
      this.pictureBox1.TabStop = false;
      // 
      // label2
      // 
      this.label2.AutoSize = true;
      this.label2.Location = new System.Drawing.Point(51, 107);
      this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(264, 17);
      this.label2.TabIndex = 1;
      this.label2.Text = "The following switches are also included:";
      // 
      // label3
      // 
      this.label3.Location = new System.Drawing.Point(51, 123);
      this.label3.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
      this.label3.Name = "label3";
      this.label3.Size = new System.Drawing.Size(460, 39);
      this.label3.TabIndex = 2;
      this.label3.Text = "label3 wrap me";
      // 
      // CustomVerifyForm
      // 
      this.AcceptButton = this.button1;
      this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(523, 106);
      this.Controls.Add(this.label3);
      this.Controls.Add(this.label2);
      this.Controls.Add(this.cbFunction);
      this.Controls.Add(this.pictureBox1);
      this.Controls.Add(this.button1);
      this.Controls.Add(this.txtAddidtional);
      this.Controls.Add(this.label1);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
      this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "CustomVerifyForm";
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
      this.Text = "Custom Verify";
      this.TopMost = true;
      this.Shown += new System.EventHandler(this.CustomVerifyForm_Shown);
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.Button button1;
    internal System.Windows.Forms.TextBox txtAddidtional;
    internal System.Windows.Forms.CheckBox cbFunction;
    private System.Windows.Forms.PictureBox pictureBox1;
    private System.Windows.Forms.Label label3;
    private System.Windows.Forms.Label label2;
  }
}