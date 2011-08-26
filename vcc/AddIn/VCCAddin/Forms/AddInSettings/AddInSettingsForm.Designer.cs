namespace VerifiedCCompilerAddin.Forms.AddInSettings {
  partial class AddInSettingsForm {
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AddInSettingsForm));
      this.button1 = new System.Windows.Forms.Button();
      this.button2 = new System.Windows.Forms.Button();
      this.lbVersion = new System.Windows.Forms.Label();
      this.label3 = new System.Windows.Forms.Label();
      this.pictureBox1 = new System.Windows.Forms.PictureBox();
      this.cbWarnForHeader = new System.Windows.Forms.CheckBox();
      this.groupBox2 = new System.Windows.Forms.GroupBox();
      this.label1 = new System.Windows.Forms.Label();
      this.cb_Inspector = new System.Windows.Forms.CheckBox();
      this.cb_DistZ3 = new System.Windows.Forms.CheckBox();
      this.VccCommandLineSwitchesActive = new System.Windows.Forms.CheckBox();
      this.VCCCommandLineSwitches = new System.Windows.Forms.TextBox();
      this.cbShowBallonTip = new System.Windows.Forms.CheckBox();
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
      this.groupBox2.SuspendLayout();
      this.SuspendLayout();
      // 
      // button1
      // 
      this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
      this.button1.Location = new System.Drawing.Point(337, 282);
      this.button1.Name = "button1";
      this.button1.Size = new System.Drawing.Size(75, 23);
      this.button1.TabIndex = 4;
      this.button1.Text = "OK";
      this.button1.UseVisualStyleBackColor = true;
      this.button1.Click += new System.EventHandler(this.button1_Click);
      // 
      // button2
      // 
      this.button2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.button2.DialogResult = System.Windows.Forms.DialogResult.Cancel;
      this.button2.Location = new System.Drawing.Point(418, 282);
      this.button2.Name = "button2";
      this.button2.Size = new System.Drawing.Size(75, 23);
      this.button2.TabIndex = 5;
      this.button2.Text = "Cancel";
      this.button2.UseVisualStyleBackColor = true;
      // 
      // lbVersion
      // 
      this.lbVersion.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.lbVersion.AutoSize = true;
      this.lbVersion.Location = new System.Drawing.Point(91, 287);
      this.lbVersion.Name = "lbVersion";
      this.lbVersion.Size = new System.Drawing.Size(50, 13);
      this.lbVersion.TabIndex = 1;
      this.lbVersion.Text = "lbVersion";
      // 
      // label3
      // 
      this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.label3.AutoSize = true;
      this.label3.Location = new System.Drawing.Point(50, 287);
      this.label3.Name = "label3";
      this.label3.Size = new System.Drawing.Size(45, 13);
      this.label3.TabIndex = 7;
      this.label3.Text = "Version ";
      // 
      // pictureBox1
      // 
      this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
      this.pictureBox1.Location = new System.Drawing.Point(12, 278);
      this.pictureBox1.Name = "pictureBox1";
      this.pictureBox1.Size = new System.Drawing.Size(32, 32);
      this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
      this.pictureBox1.TabIndex = 0;
      this.pictureBox1.TabStop = false;
      // 
      // cbWarnForHeader
      // 
      this.cbWarnForHeader.AutoSize = true;
      this.cbWarnForHeader.Checked = true;
      this.cbWarnForHeader.CheckState = System.Windows.Forms.CheckState.Checked;
      this.cbWarnForHeader.Location = new System.Drawing.Point(22, 194);
      this.cbWarnForHeader.Name = "cbWarnForHeader";
      this.cbWarnForHeader.Size = new System.Drawing.Size(220, 17);
      this.cbWarnForHeader.TabIndex = 9;
      this.cbWarnForHeader.Text = "Warn, if vcc header file cannot be found.";
      this.cbWarnForHeader.UseVisualStyleBackColor = true;
      // 
      // groupBox2
      // 
      this.groupBox2.Controls.Add(this.label1);
      this.groupBox2.Controls.Add(this.cb_Inspector);
      this.groupBox2.Controls.Add(this.cb_DistZ3);
      this.groupBox2.Controls.Add(this.VccCommandLineSwitchesActive);
      this.groupBox2.Controls.Add(this.VCCCommandLineSwitches);
      this.groupBox2.Location = new System.Drawing.Point(12, 12);
      this.groupBox2.Name = "groupBox2";
      this.groupBox2.Size = new System.Drawing.Size(481, 174);
      this.groupBox2.TabIndex = 8;
      this.groupBox2.TabStop = false;
      this.groupBox2.Text = "Additional VCC command line arguments:";
      // 
      // label1
      // 
      this.label1.AutoSize = true;
      this.label1.Location = new System.Drawing.Point(7, 136);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(62, 13);
      this.label1.TabIndex = 6;
      this.label1.Text = "Z3 Options:";
      // 
      // cb_Inspector
      // 
      this.cb_Inspector.AutoSize = true;
      this.cb_Inspector.Location = new System.Drawing.Point(131, 152);
      this.cb_Inspector.Name = "cb_Inspector";
      this.cb_Inspector.Size = new System.Drawing.Size(108, 17);
      this.cb_Inspector.TabIndex = 5;
      this.cb_Inspector.Text = "Use Z3 Inspector";
      this.cb_Inspector.UseVisualStyleBackColor = true;
      // 
      // cb_DistZ3
      // 
      this.cb_DistZ3.AutoSize = true;
      this.cb_DistZ3.Location = new System.Drawing.Point(10, 152);
      this.cb_DistZ3.Name = "cb_DistZ3";
      this.cb_DistZ3.Size = new System.Drawing.Size(114, 17);
      this.cb_DistZ3.TabIndex = 4;
      this.cb_DistZ3.Text = "Use Distributed Z3";
      this.cb_DistZ3.UseVisualStyleBackColor = true;
      // 
      // VccCommandLineSwitchesActive
      // 
      this.VccCommandLineSwitchesActive.AutoSize = true;
      this.VccCommandLineSwitchesActive.Location = new System.Drawing.Point(10, 19);
      this.VccCommandLineSwitchesActive.Name = "VccCommandLineSwitchesActive";
      this.VccCommandLineSwitchesActive.Size = new System.Drawing.Size(56, 17);
      this.VccCommandLineSwitchesActive.TabIndex = 2;
      this.VccCommandLineSwitchesActive.Text = "Active";
      this.VccCommandLineSwitchesActive.UseVisualStyleBackColor = true;
      // 
      // VCCCommandLineSwitches
      // 
      this.VCCCommandLineSwitches.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.VCCCommandLineSwitches.Location = new System.Drawing.Point(10, 42);
      this.VCCCommandLineSwitches.Multiline = true;
      this.VCCCommandLineSwitches.Name = "VCCCommandLineSwitches";
      this.VCCCommandLineSwitches.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.VCCCommandLineSwitches.Size = new System.Drawing.Size(465, 86);
      this.VCCCommandLineSwitches.TabIndex = 3;
      // 
      // cbShowBallonTip
      // 
      this.cbShowBallonTip.AutoSize = true;
      this.cbShowBallonTip.Location = new System.Drawing.Point(22, 217);
      this.cbShowBallonTip.Name = "cbShowBallonTip";
      this.cbShowBallonTip.Size = new System.Drawing.Size(152, 17);
      this.cbShowBallonTip.TabIndex = 10;
      this.cbShowBallonTip.Text = "Show balloon notifications.";
      this.cbShowBallonTip.UseVisualStyleBackColor = true;
      // 
      // AddInSettingsForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
      this.AutoScroll = true;
      this.AutoSize = true;
      this.ClientSize = new System.Drawing.Size(505, 312);
      this.ControlBox = false;
      this.Controls.Add(this.cbShowBallonTip);
      this.Controls.Add(this.cbWarnForHeader);
      this.Controls.Add(this.groupBox2);
      this.Controls.Add(this.pictureBox1);
      this.Controls.Add(this.lbVersion);
      this.Controls.Add(this.label3);
      this.Controls.Add(this.button2);
      this.Controls.Add(this.button1);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
      this.Name = "AddInSettingsForm";
      this.ShowInTaskbar = false;
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
      this.Text = "VCC Add-in Settings";
      this.TopMost = true;
      this.Load += new System.EventHandler(this.AddInSettingsForm_Load);
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
      this.groupBox2.ResumeLayout(false);
      this.groupBox2.PerformLayout();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Button button1;
    private System.Windows.Forms.Button button2;
    private System.Windows.Forms.Label lbVersion;
    private System.Windows.Forms.Label label3;
    private System.Windows.Forms.PictureBox pictureBox1;
    private System.Windows.Forms.CheckBox cbWarnForHeader;
    private System.Windows.Forms.GroupBox groupBox2;
    private System.Windows.Forms.CheckBox VccCommandLineSwitchesActive;
    private System.Windows.Forms.TextBox VCCCommandLineSwitches;
    private System.Windows.Forms.CheckBox cb_DistZ3;
    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.CheckBox cb_Inspector;
    private System.Windows.Forms.CheckBox cbShowBallonTip;
  }
}