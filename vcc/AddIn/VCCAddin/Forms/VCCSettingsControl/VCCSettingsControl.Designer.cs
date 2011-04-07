namespace VerifiedCCompilerAddin.Forms.VCCSettingsControl {
  partial class VCCSettingsControl {
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(VCCSettingsControl));
      this.pictureBox1 = new System.Windows.Forms.PictureBox();
      this.lbVersion = new System.Windows.Forms.Label();
      this.label3 = new System.Windows.Forms.Label();
      this.tabControl1 = new System.Windows.Forms.TabControl();
      this.tabPage1 = new System.Windows.Forms.TabPage();
      this.groupBox3 = new System.Windows.Forms.GroupBox();
      this.rbVersion2 = new System.Windows.Forms.RadioButton();
      this.rbVersion1 = new System.Windows.Forms.RadioButton();
      this.groupBox2 = new System.Windows.Forms.GroupBox();
      this.VccCommandLineSwitchesActive = new System.Windows.Forms.CheckBox();
      this.VccCommandLineSwitches = new System.Windows.Forms.TextBox();
      this.tabPage2 = new System.Windows.Forms.TabPage();
      this.gbAssumtions = new System.Windows.Forms.GroupBox();
      this.cbHint001 = new System.Windows.Forms.CheckBox();
      this.cbHint003 = new System.Windows.Forms.CheckBox();
      this.cbHint002 = new System.Windows.Forms.CheckBox();
      this.cbHintsActive = new System.Windows.Forms.CheckBox();
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
      this.tabControl1.SuspendLayout();
      this.tabPage1.SuspendLayout();
      this.groupBox3.SuspendLayout();
      this.groupBox2.SuspendLayout();
      this.tabPage2.SuspendLayout();
      this.gbAssumtions.SuspendLayout();
      this.SuspendLayout();
      // 
      // pictureBox1
      // 
      this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
      this.pictureBox1.Location = new System.Drawing.Point(5, 269);
      this.pictureBox1.Name = "pictureBox1";
      this.pictureBox1.Size = new System.Drawing.Size(32, 32);
      this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
      this.pictureBox1.TabIndex = 8;
      this.pictureBox1.TabStop = false;
      // 
      // lbVersion
      // 
      this.lbVersion.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.lbVersion.AutoSize = true;
      this.lbVersion.Location = new System.Drawing.Point(43, 288);
      this.lbVersion.Name = "lbVersion";
      this.lbVersion.Size = new System.Drawing.Size(50, 13);
      this.lbVersion.TabIndex = 9;
      this.lbVersion.Text = "lbVersion";
      // 
      // label3
      // 
      this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.label3.AutoSize = true;
      this.label3.Location = new System.Drawing.Point(43, 269);
      this.label3.Name = "label3";
      this.label3.Size = new System.Drawing.Size(45, 13);
      this.label3.TabIndex = 11;
      this.label3.Text = "Version ";
      // 
      // tabControl1
      // 
      this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.tabControl1.CausesValidation = false;
      this.tabControl1.Controls.Add(this.tabPage1);
      this.tabControl1.Controls.Add(this.tabPage2);
      this.tabControl1.Location = new System.Drawing.Point(0, 0);
      this.tabControl1.Margin = new System.Windows.Forms.Padding(0);
      this.tabControl1.Name = "tabControl1";
      this.tabControl1.Padding = new System.Drawing.Point(0, 0);
      this.tabControl1.SelectedIndex = 0;
      this.tabControl1.Size = new System.Drawing.Size(453, 260);
      this.tabControl1.TabIndex = 0;
      // 
      // tabPage1
      // 
      this.tabPage1.Controls.Add(this.groupBox3);
      this.tabPage1.Controls.Add(this.groupBox2);
      this.tabPage1.Location = new System.Drawing.Point(4, 22);
      this.tabPage1.Name = "tabPage1";
      this.tabPage1.Padding = new System.Windows.Forms.Padding(3, 3, 10, 3);
      this.tabPage1.Size = new System.Drawing.Size(445, 234);
      this.tabPage1.TabIndex = 0;
      this.tabPage1.Text = "Main";
      this.tabPage1.UseVisualStyleBackColor = true;
      // 
      // groupBox3
      // 
      this.groupBox3.Controls.Add(this.rbVersion2);
      this.groupBox3.Controls.Add(this.rbVersion1);
      this.groupBox3.Location = new System.Drawing.Point(7, 7);
      this.groupBox3.Name = "groupBox3";
      this.groupBox3.Size = new System.Drawing.Size(429, 53);
      this.groupBox3.TabIndex = 5;
      this.groupBox3.TabStop = false;
      this.groupBox3.Text = "VCC Version";
      // 
      // rbVersion2
      // 
      this.rbVersion2.AutoSize = true;
      this.rbVersion2.Location = new System.Drawing.Point(114, 22);
      this.rbVersion2.Name = "rbVersion2";
      this.rbVersion2.Size = new System.Drawing.Size(69, 17);
      this.rbVersion2.TabIndex = 1;
      this.rbVersion2.TabStop = true;
      this.rbVersion2.Text = "Version 2";
      this.rbVersion2.UseVisualStyleBackColor = true;
      this.rbVersion2.CheckedChanged += new System.EventHandler(this.rbVersion2_CheckedChanged);
      // 
      // rbVersion1
      // 
      this.rbVersion1.AutoSize = true;
      this.rbVersion1.Location = new System.Drawing.Point(9, 22);
      this.rbVersion1.Name = "rbVersion1";
      this.rbVersion1.Size = new System.Drawing.Size(69, 17);
      this.rbVersion1.TabIndex = 0;
      this.rbVersion1.TabStop = true;
      this.rbVersion1.Text = "Version 1";
      this.rbVersion1.UseVisualStyleBackColor = true;
      this.rbVersion1.CheckedChanged += new System.EventHandler(this.rbVersion1_CheckedChanged);
      // 
      // groupBox2
      // 
      this.groupBox2.Controls.Add(this.VccCommandLineSwitchesActive);
      this.groupBox2.Controls.Add(this.VccCommandLineSwitches);
      this.groupBox2.Location = new System.Drawing.Point(6, 66);
      this.groupBox2.Name = "groupBox2";
      this.groupBox2.Size = new System.Drawing.Size(430, 162);
      this.groupBox2.TabIndex = 4;
      this.groupBox2.TabStop = false;
      this.groupBox2.Text = "Additional vcc commandline arguments:";
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
      this.VccCommandLineSwitchesActive.CheckedChanged += new System.EventHandler(this.VccCommandLineSwitchesActive_CheckedChanged);
      // 
      // VccCommandLineSwitches
      // 
      this.VccCommandLineSwitches.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.VccCommandLineSwitches.Location = new System.Drawing.Point(10, 42);
      this.VccCommandLineSwitches.Multiline = true;
      this.VccCommandLineSwitches.Name = "VccCommandLineSwitches";
      this.VccCommandLineSwitches.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.VccCommandLineSwitches.Size = new System.Drawing.Size(414, 114);
      this.VccCommandLineSwitches.TabIndex = 3;
      this.VccCommandLineSwitches.TextChanged += new System.EventHandler(this.VccCommandLineSwitches_TextChanged);
      // 
      // tabPage2
      // 
      this.tabPage2.Controls.Add(this.gbAssumtions);
      this.tabPage2.Controls.Add(this.cbHintsActive);
      this.tabPage2.Location = new System.Drawing.Point(4, 22);
      this.tabPage2.Name = "tabPage2";
      this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage2.Size = new System.Drawing.Size(445, 234);
      this.tabPage2.TabIndex = 1;
      this.tabPage2.Text = "Hints";
      this.tabPage2.UseVisualStyleBackColor = true;
      // 
      // gbAssumtions
      // 
      this.gbAssumtions.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.gbAssumtions.Controls.Add(this.cbHint001);
      this.gbAssumtions.Controls.Add(this.cbHint003);
      this.gbAssumtions.Controls.Add(this.cbHint002);
      this.gbAssumtions.Location = new System.Drawing.Point(6, 29);
      this.gbAssumtions.Name = "gbAssumtions";
      this.gbAssumtions.Size = new System.Drawing.Size(433, 203);
      this.gbAssumtions.TabIndex = 4;
      this.gbAssumtions.TabStop = false;
      this.gbAssumtions.Text = "Assumtions";
      // 
      // cbHint001
      // 
      this.cbHint001.AutoSize = true;
      this.cbHint001.Location = new System.Drawing.Point(6, 19);
      this.cbHint001.Name = "cbHint001";
      this.cbHint001.Size = new System.Drawing.Size(96, 17);
      this.cbHint001.TabIndex = 0;
      this.cbHint001.Text = "Memory Safety";
      this.cbHint001.UseVisualStyleBackColor = true;
      this.cbHint001.CheckedChanged += new System.EventHandler(this.cbHint001_CheckedChanged);
      // 
      // cbHint003
      // 
      this.cbHint003.AutoSize = true;
      this.cbHint003.Location = new System.Drawing.Point(6, 66);
      this.cbHint003.Name = "cbHint003";
      this.cbHint003.Size = new System.Drawing.Size(134, 17);
      this.cbHint003.TabIndex = 3;
      this.cbHint003.Text = "Functional Correctness";
      this.cbHint003.UseVisualStyleBackColor = true;
      this.cbHint003.CheckedChanged += new System.EventHandler(this.cbHint003_CheckedChanged);
      // 
      // cbHint002
      // 
      this.cbHint002.AutoSize = true;
      this.cbHint002.Location = new System.Drawing.Point(6, 42);
      this.cbHint002.Name = "cbHint002";
      this.cbHint002.Size = new System.Drawing.Size(111, 17);
      this.cbHint002.TabIndex = 1;
      this.cbHint002.Text = "Structural Integrity";
      this.cbHint002.UseVisualStyleBackColor = true;
      this.cbHint002.CheckedChanged += new System.EventHandler(this.cbHint002_CheckedChanged);
      // 
      // cbHintsActive
      // 
      this.cbHintsActive.AutoSize = true;
      this.cbHintsActive.Location = new System.Drawing.Point(6, 6);
      this.cbHintsActive.Name = "cbHintsActive";
      this.cbHintsActive.Size = new System.Drawing.Size(72, 17);
      this.cbHintsActive.TabIndex = 2;
      this.cbHintsActive.Text = "Use Hints";
      this.cbHintsActive.UseVisualStyleBackColor = true;
      this.cbHintsActive.CheckedChanged += new System.EventHandler(this.cbHintsActive_CheckedChanged);
      // 
      // VCCSettingsControl
      // 
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.None;
      this.Controls.Add(this.pictureBox1);
      this.Controls.Add(this.lbVersion);
      this.Controls.Add(this.label3);
      this.Controls.Add(this.tabControl1);
      this.MaximumSize = new System.Drawing.Size(460, 340);
      this.MinimumSize = new System.Drawing.Size(460, 340);
      this.Name = "VCCSettingsControl";
      this.Padding = new System.Windows.Forms.Padding(2);
      this.RightToLeft = System.Windows.Forms.RightToLeft.No;
      this.Size = new System.Drawing.Size(460, 340);
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
      this.tabControl1.ResumeLayout(false);
      this.tabPage1.ResumeLayout(false);
      this.groupBox3.ResumeLayout(false);
      this.groupBox3.PerformLayout();
      this.groupBox2.ResumeLayout(false);
      this.groupBox2.PerformLayout();
      this.tabPage2.ResumeLayout(false);
      this.tabPage2.PerformLayout();
      this.gbAssumtions.ResumeLayout(false);
      this.gbAssumtions.PerformLayout();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.PictureBox pictureBox1;
    private System.Windows.Forms.Label lbVersion;
    private System.Windows.Forms.Label label3;
    private System.Windows.Forms.TabControl tabControl1;
    private System.Windows.Forms.TabPage tabPage1;
    private System.Windows.Forms.GroupBox groupBox3;
    private System.Windows.Forms.RadioButton rbVersion2;
    private System.Windows.Forms.RadioButton rbVersion1;
    private System.Windows.Forms.GroupBox groupBox2;
    private System.Windows.Forms.CheckBox VccCommandLineSwitchesActive;
    private System.Windows.Forms.TextBox VccCommandLineSwitches;
    private System.Windows.Forms.TabPage tabPage2;
    private System.Windows.Forms.GroupBox gbAssumtions;
    private System.Windows.Forms.CheckBox cbHint001;
    private System.Windows.Forms.CheckBox cbHint003;
    private System.Windows.Forms.CheckBox cbHint002;
    private System.Windows.Forms.CheckBox cbHintsActive;

  }
}
