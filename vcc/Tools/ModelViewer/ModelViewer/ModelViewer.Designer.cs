namespace VccModelViewer
{
    partial class ModelViewer
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
          this.components = new System.ComponentModel.Container();
          System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ModelViewer));
          this.splitContainerLeftRight = new System.Windows.Forms.SplitContainer();
          this.leftPanel = new System.Windows.Forms.Panel();
          this.modelView = new System.Windows.Forms.TreeView();
          this.imageList1 = new System.Windows.Forms.ImageList(this.components);
          this.executionStatesComboBox = new System.Windows.Forms.ComboBox();
          this.stateValueTextBox = new System.Windows.Forms.TextBox();
          this.stateValueView = new System.Windows.Forms.ListView();
          this.lineno = new System.Windows.Forms.ColumnHeader();
          this.state = new System.Windows.Forms.ColumnHeader();
          this.value = new System.Windows.Forms.ColumnHeader();
          this.menuStrip1 = new System.Windows.Forms.MenuStrip();
          this.fileMenuStrip = new System.Windows.Forms.ToolStripMenuItem();
          this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
          this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.modelToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.optionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.filterFunctionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.filterInconsistentStatesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.filterInternalVccVariablesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
          this.toolStripContainer1 = new System.Windows.Forms.ToolStripContainer();
          this.statusStrip1 = new System.Windows.Forms.StatusStrip();
          this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
          this.toolStripProgressBar1 = new System.Windows.Forms.ToolStripProgressBar();
          this.visualStudioPanel = new System.Windows.Forms.Panel();
          this.toolStrip1 = new System.Windows.Forms.ToolStrip();
          this.toolStripButton_filterFunctions = new System.Windows.Forms.ToolStripButton();
          this.toolStripButton_filterStates = new System.Windows.Forms.ToolStripButton();
          this.toolStripButton_filterVariables = new System.Windows.Forms.ToolStripButton();
          this.toolStripLabel1 = new System.Windows.Forms.ToolStripLabel();
          this.labelCurrentModel = new System.Windows.Forms.ToolStripLabel();
          this.panel1 = new System.Windows.Forms.Panel();
          this.splitContainerLeftRight.Panel1.SuspendLayout();
          this.splitContainerLeftRight.Panel2.SuspendLayout();
          this.splitContainerLeftRight.SuspendLayout();
          this.leftPanel.SuspendLayout();
          this.menuStrip1.SuspendLayout();
          this.toolStripContainer1.ContentPanel.SuspendLayout();
          this.toolStripContainer1.TopToolStripPanel.SuspendLayout();
          this.toolStripContainer1.SuspendLayout();
          this.statusStrip1.SuspendLayout();
          this.visualStudioPanel.SuspendLayout();
          this.toolStrip1.SuspendLayout();
          this.panel1.SuspendLayout();
          this.SuspendLayout();
          // 
          // splitContainerLeftRight
          // 
          this.splitContainerLeftRight.Dock = System.Windows.Forms.DockStyle.Fill;
          this.splitContainerLeftRight.Location = new System.Drawing.Point(0, 0);
          this.splitContainerLeftRight.Name = "splitContainerLeftRight";
          // 
          // splitContainerLeftRight.Panel1
          // 
          this.splitContainerLeftRight.Panel1.Controls.Add(this.leftPanel);
          // 
          // splitContainerLeftRight.Panel2
          // 
          this.splitContainerLeftRight.Panel2.Controls.Add(this.stateValueView);
          this.splitContainerLeftRight.Panel2.Controls.Add(this.stateValueTextBox);
          this.splitContainerLeftRight.Size = new System.Drawing.Size(728, 431);
          this.splitContainerLeftRight.SplitterDistance = 477;
          this.splitContainerLeftRight.TabIndex = 0;
          // 
          // leftPanel
          // 
          this.leftPanel.Controls.Add(this.modelView);
          this.leftPanel.Controls.Add(this.executionStatesComboBox);
          this.leftPanel.Dock = System.Windows.Forms.DockStyle.Fill;
          this.leftPanel.Location = new System.Drawing.Point(0, 0);
          this.leftPanel.Name = "leftPanel";
          this.leftPanel.Size = new System.Drawing.Size(477, 431);
          this.leftPanel.TabIndex = 2;
          // 
          // modelView
          // 
          this.modelView.Dock = System.Windows.Forms.DockStyle.Fill;
          this.modelView.HideSelection = false;
          this.modelView.ImageIndex = 0;
          this.modelView.ImageList = this.imageList1;
          this.modelView.Location = new System.Drawing.Point(0, 21);
          this.modelView.Name = "modelView";
          this.modelView.SelectedImageIndex = 0;
          this.modelView.ShowNodeToolTips = true;
          this.modelView.Size = new System.Drawing.Size(477, 410);
          this.modelView.TabIndex = 0;
          this.modelView.BeforeExpand += new System.Windows.Forms.TreeViewCancelEventHandler(this.modelView_BeforeExpand);
          this.modelView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.modelView_AfterSelect);
          this.modelView.NodeMouseClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.modelView_nodeMouseClick);
          // 
          // imageList1
          // 
          this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
          this.imageList1.TransparentColor = System.Drawing.Color.Magenta;
          this.imageList1.Images.SetKeyName(0, "VSFolder_closed.bmp");
          this.imageList1.Images.SetKeyName(1, "VSObject_Constant.bmp");
          this.imageList1.Images.SetKeyName(2, "VSObject_Field.bmp");
          this.imageList1.Images.SetKeyName(3, "VSObject_Map.bmp");
          this.imageList1.Images.SetKeyName(4, "VSObject_MapItem.bmp");
          this.imageList1.Images.SetKeyName(5, "VSObject_Namespace.bmp");
          this.imageList1.Images.SetKeyName(6, "VSObject_Properties.bmp");
          this.imageList1.Images.SetKeyName(7, "VSObject_Structure.bmp");
          this.imageList1.Images.SetKeyName(8, "VSObject_Type.bmp");
          this.imageList1.Images.SetKeyName(9, "VSObject_TypeDef.bmp");
          this.imageList1.Images.SetKeyName(10, "VSObject_Union.bmp");
          this.imageList1.Images.SetKeyName(11, "VSObject_ValueType.bmp");
          this.imageList1.Images.SetKeyName(12, "VSObject_Method.bmp");
          this.imageList1.Images.SetKeyName(13, "Control_ErrorProvider.bmp");
          this.imageList1.Images.SetKeyName(14, "VSFolder_open.bmp");
          // 
          // executionStatesComboBox
          // 
          this.executionStatesComboBox.Dock = System.Windows.Forms.DockStyle.Top;
          this.executionStatesComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
          this.executionStatesComboBox.FormattingEnabled = true;
          this.executionStatesComboBox.ImeMode = System.Windows.Forms.ImeMode.Disable;
          this.executionStatesComboBox.Location = new System.Drawing.Point(0, 0);
          this.executionStatesComboBox.Name = "executionStatesComboBox";
          this.executionStatesComboBox.Size = new System.Drawing.Size(477, 21);
          this.executionStatesComboBox.TabIndex = 1;
          this.executionStatesComboBox.SelectedIndexChanged += new System.EventHandler(this.executionStatesComboBox_SelectedIndexChanged);
          // 
          // stateValueTextBox
          // 
          this.stateValueTextBox.Dock = System.Windows.Forms.DockStyle.Top;
          this.stateValueTextBox.Location = new System.Drawing.Point(0, 0);
          this.stateValueTextBox.Name = "stateValueTextBox";
          this.stateValueTextBox.ReadOnly = true;
          this.stateValueTextBox.Size = new System.Drawing.Size(247, 20);
          this.stateValueTextBox.TabIndex = 1;
          // 
          // stateValueView
          // 
          this.stateValueView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.lineno,
            this.state,
            this.value});
          this.stateValueView.Dock = System.Windows.Forms.DockStyle.Fill;
          this.stateValueView.FullRowSelect = true;
          this.stateValueView.GridLines = true;
          this.stateValueView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
          this.stateValueView.Location = new System.Drawing.Point(0, 20);
          this.stateValueView.Name = "stateValueView";
          this.stateValueView.ShowItemToolTips = true;
          this.stateValueView.Size = new System.Drawing.Size(247, 411);
          this.stateValueView.TabIndex = 0;
          this.stateValueView.UseCompatibleStateImageBehavior = false;
          this.stateValueView.View = System.Windows.Forms.View.Details;
          this.stateValueView.DoubleClick += new System.EventHandler(this.stateValueView_DoubleClick);
          // 
          // lineno
          // 
          this.lineno.Text = "Line";
          this.lineno.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
          this.lineno.Width = 55;
          // 
          // state
          // 
          this.state.Text = "State";
          this.state.Width = 66;
          // 
          // value
          // 
          this.value.Text = "Value";
          this.value.Width = 120;
          // 
          // menuStrip1
          // 
          this.menuStrip1.Dock = System.Windows.Forms.DockStyle.None;
          this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileMenuStrip,
            this.modelToolStripMenuItem,
            this.optionsToolStripMenuItem,
            this.helpToolStripMenuItem});
          this.menuStrip1.Location = new System.Drawing.Point(0, 0);
          this.menuStrip1.Name = "menuStrip1";
          this.menuStrip1.Size = new System.Drawing.Size(728, 24);
          this.menuStrip1.TabIndex = 2;
          this.menuStrip1.Text = "menuStrip1";
          // 
          // fileMenuStrip
          // 
          this.fileMenuStrip.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openToolStripMenuItem,
            this.toolStripSeparator1,
            this.exitToolStripMenuItem});
          this.fileMenuStrip.Name = "fileMenuStrip";
          this.fileMenuStrip.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.F)));
          this.fileMenuStrip.Size = new System.Drawing.Size(37, 20);
          this.fileMenuStrip.Text = "&File";
          // 
          // openToolStripMenuItem
          // 
          this.openToolStripMenuItem.Name = "openToolStripMenuItem";
          this.openToolStripMenuItem.Size = new System.Drawing.Size(112, 22);
          this.openToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
          this.openToolStripMenuItem.Text = "&Open...";
          this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
          // 
          // toolStripSeparator1
          // 
          this.toolStripSeparator1.Name = "toolStripSeparator1";
          this.toolStripSeparator1.Size = new System.Drawing.Size(109, 6);
          // 
          // exitToolStripMenuItem
          // 
          this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
          this.exitToolStripMenuItem.Size = new System.Drawing.Size(112, 22);
          this.exitToolStripMenuItem.Text = "Exit";
          this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
          // 
          // modelToolStripMenuItem
          // 
          this.modelToolStripMenuItem.Name = "modelToolStripMenuItem";
          this.modelToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.M)));
          this.modelToolStripMenuItem.Size = new System.Drawing.Size(53, 20);
          this.modelToolStripMenuItem.Text = "&Model";
          // 
          // optionsToolStripMenuItem
          // 
          this.optionsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.filterFunctionsToolStripMenuItem,
            this.filterInconsistentStatesToolStripMenuItem,
            this.filterInternalVccVariablesToolStripMenuItem});
          this.optionsToolStripMenuItem.Name = "optionsToolStripMenuItem";
          this.optionsToolStripMenuItem.Size = new System.Drawing.Size(61, 20);
          this.optionsToolStripMenuItem.Text = "&Options";
          // 
          // filterFunctionsToolStripMenuItem
          // 
          this.filterFunctionsToolStripMenuItem.Checked = true;
          this.filterFunctionsToolStripMenuItem.CheckOnClick = true;
          this.filterFunctionsToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
          this.filterFunctionsToolStripMenuItem.Name = "filterFunctionsToolStripMenuItem";
          this.filterFunctionsToolStripMenuItem.Size = new System.Drawing.Size(224, 22);
          this.filterFunctionsToolStripMenuItem.Text = "&Filter visualized function";
          this.filterFunctionsToolStripMenuItem.Click += new System.EventHandler(this.filterOptionsMenuItem_Click);
          // 
          // filterInconsistentStatesToolStripMenuItem
          // 
          this.filterInconsistentStatesToolStripMenuItem.Checked = true;
          this.filterInconsistentStatesToolStripMenuItem.CheckOnClick = true;
          this.filterInconsistentStatesToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
          this.filterInconsistentStatesToolStripMenuItem.Name = "filterInconsistentStatesToolStripMenuItem";
          this.filterInconsistentStatesToolStripMenuItem.Size = new System.Drawing.Size(224, 22);
          this.filterInconsistentStatesToolStripMenuItem.Text = "F&ilter inconsistent states";
          this.filterInconsistentStatesToolStripMenuItem.Click += new System.EventHandler(this.filterOptionsMenuItem_Click);
          // 
          // filterInternalVccVariablesToolStripMenuItem
          // 
          this.filterInternalVccVariablesToolStripMenuItem.Checked = true;
          this.filterInternalVccVariablesToolStripMenuItem.CheckOnClick = true;
          this.filterInternalVccVariablesToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
          this.filterInternalVccVariablesToolStripMenuItem.Name = "filterInternalVccVariablesToolStripMenuItem";
          this.filterInternalVccVariablesToolStripMenuItem.Size = new System.Drawing.Size(224, 22);
          this.filterInternalVccVariablesToolStripMenuItem.Text = "Filter internal VCC variables";
          this.filterInternalVccVariablesToolStripMenuItem.Click += new System.EventHandler(this.filterOptionsMenuItem_Click);
          // 
          // helpToolStripMenuItem
          // 
          this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem});
          this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
          this.helpToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
          this.helpToolStripMenuItem.Text = "Help";
          // 
          // aboutToolStripMenuItem
          // 
          this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
          this.aboutToolStripMenuItem.Size = new System.Drawing.Size(107, 22);
          this.aboutToolStripMenuItem.Text = "About";
          this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
          // 
          // toolStripContainer1
          // 
          // 
          // toolStripContainer1.ContentPanel
          // 
          this.toolStripContainer1.ContentPanel.AutoScroll = true;
          this.toolStripContainer1.ContentPanel.Controls.Add(this.splitContainerLeftRight);
          this.toolStripContainer1.ContentPanel.Controls.Add(this.statusStrip1);
          this.toolStripContainer1.ContentPanel.Size = new System.Drawing.Size(728, 453);
          this.toolStripContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
          this.toolStripContainer1.Location = new System.Drawing.Point(0, 0);
          this.toolStripContainer1.Name = "toolStripContainer1";
          this.toolStripContainer1.Size = new System.Drawing.Size(728, 477);
          this.toolStripContainer1.TabIndex = 3;
          this.toolStripContainer1.Text = "toolStripContainer1";
          // 
          // toolStripContainer1.TopToolStripPanel
          // 
          this.toolStripContainer1.TopToolStripPanel.Controls.Add(this.menuStrip1);
          // 
          // statusStrip1
          // 
          this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatusLabel1,
            this.toolStripProgressBar1});
          this.statusStrip1.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow;
          this.statusStrip1.Location = new System.Drawing.Point(0, 431);
          this.statusStrip1.Name = "statusStrip1";
          this.statusStrip1.Size = new System.Drawing.Size(728, 22);
          this.statusStrip1.TabIndex = 2;
          this.statusStrip1.Text = "statusStrip1";
          // 
          // toolStripStatusLabel1
          // 
          this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
          this.toolStripStatusLabel1.Size = new System.Drawing.Size(0, 17);
          // 
          // toolStripProgressBar1
          // 
          this.toolStripProgressBar1.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
          this.toolStripProgressBar1.Name = "toolStripProgressBar1";
          this.toolStripProgressBar1.Size = new System.Drawing.Size(100, 16);
          this.toolStripProgressBar1.Visible = false;
          // 
          // visualStudioPanel
          // 
          this.visualStudioPanel.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
          this.visualStudioPanel.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
          this.visualStudioPanel.Controls.Add(this.toolStrip1);
          this.visualStudioPanel.Dock = System.Windows.Forms.DockStyle.Top;
          this.visualStudioPanel.Location = new System.Drawing.Point(0, 0);
          this.visualStudioPanel.MaximumSize = new System.Drawing.Size(0, 25);
          this.visualStudioPanel.MinimumSize = new System.Drawing.Size(0, 25);
          this.visualStudioPanel.Name = "visualStudioPanel";
          this.visualStudioPanel.Size = new System.Drawing.Size(728, 25);
          this.visualStudioPanel.TabIndex = 0;
          // 
          // toolStrip1
          // 
          this.toolStrip1.BackColor = System.Drawing.SystemColors.GradientActiveCaption;
          this.toolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
          this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripButton_filterFunctions,
            this.toolStripButton_filterStates,
            this.toolStripButton_filterVariables,
            this.toolStripLabel1,
            this.labelCurrentModel});
          this.toolStrip1.Location = new System.Drawing.Point(0, 0);
          this.toolStrip1.Name = "toolStrip1";
          this.toolStrip1.Size = new System.Drawing.Size(728, 25);
          this.toolStrip1.TabIndex = 5;
          this.toolStrip1.Text = "toolStrip1";
          // 
          // toolStripButton_filterFunctions
          // 
          this.toolStripButton_filterFunctions.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
          this.toolStripButton_filterFunctions.CheckOnClick = true;
          this.toolStripButton_filterFunctions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
          this.toolStripButton_filterFunctions.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButton_filterFunctions.Image")));
          this.toolStripButton_filterFunctions.ImageTransparentColor = System.Drawing.Color.Magenta;
          this.toolStripButton_filterFunctions.Name = "toolStripButton_filterFunctions";
          this.toolStripButton_filterFunctions.Size = new System.Drawing.Size(23, 22);
          this.toolStripButton_filterFunctions.Text = "Filter functions";
          this.toolStripButton_filterFunctions.Click += new System.EventHandler(this.toolStripButton_filterClick);
          // 
          // toolStripButton_filterStates
          // 
          this.toolStripButton_filterStates.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
          this.toolStripButton_filterStates.CheckOnClick = true;
          this.toolStripButton_filterStates.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
          this.toolStripButton_filterStates.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButton_filterStates.Image")));
          this.toolStripButton_filterStates.ImageTransparentColor = System.Drawing.Color.Magenta;
          this.toolStripButton_filterStates.Name = "toolStripButton_filterStates";
          this.toolStripButton_filterStates.Size = new System.Drawing.Size(23, 22);
          this.toolStripButton_filterStates.Text = "Filter inconsistent states";
          this.toolStripButton_filterStates.ToolTipText = "Filter inconsistent states";
          this.toolStripButton_filterStates.Click += new System.EventHandler(this.toolStripButton_filterClick);
          // 
          // toolStripButton_filterVariables
          // 
          this.toolStripButton_filterVariables.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
          this.toolStripButton_filterVariables.CheckOnClick = true;
          this.toolStripButton_filterVariables.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
          this.toolStripButton_filterVariables.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButton_filterVariables.Image")));
          this.toolStripButton_filterVariables.ImageTransparentColor = System.Drawing.Color.Transparent;
          this.toolStripButton_filterVariables.Name = "toolStripButton_filterVariables";
          this.toolStripButton_filterVariables.Size = new System.Drawing.Size(23, 22);
          this.toolStripButton_filterVariables.Text = "Filter internal Boogie variables";
          this.toolStripButton_filterVariables.Click += new System.EventHandler(this.toolStripButton_filterClick);
          // 
          // toolStripLabel1
          // 
          this.toolStripLabel1.Name = "toolStripLabel1";
          this.toolStripLabel1.Size = new System.Drawing.Size(87, 22);
          this.toolStripLabel1.Text = "Current Model:";
          // 
          // labelCurrentModel
          // 
          this.labelCurrentModel.Name = "labelCurrentModel";
          this.labelCurrentModel.Size = new System.Drawing.Size(44, 22);
          this.labelCurrentModel.Text = "(unset)";
          // 
          // panel1
          // 
          this.panel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
          this.panel1.Controls.Add(this.toolStripContainer1);
          this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
          this.panel1.Location = new System.Drawing.Point(0, 25);
          this.panel1.Name = "panel1";
          this.panel1.Size = new System.Drawing.Size(728, 477);
          this.panel1.TabIndex = 2;
          // 
          // ModelViewer
          // 
          this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
          this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
          this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
          this.Controls.Add(this.panel1);
          this.Controls.Add(this.visualStudioPanel);
          this.MinimumSize = new System.Drawing.Size(300, 200);
          this.Name = "ModelViewer";
          this.Size = new System.Drawing.Size(728, 502);
          this.splitContainerLeftRight.Panel1.ResumeLayout(false);
          this.splitContainerLeftRight.Panel2.ResumeLayout(false);
          this.splitContainerLeftRight.Panel2.PerformLayout();
          this.splitContainerLeftRight.ResumeLayout(false);
          this.leftPanel.ResumeLayout(false);
          this.menuStrip1.ResumeLayout(false);
          this.menuStrip1.PerformLayout();
          this.toolStripContainer1.ContentPanel.ResumeLayout(false);
          this.toolStripContainer1.ContentPanel.PerformLayout();
          this.toolStripContainer1.TopToolStripPanel.ResumeLayout(false);
          this.toolStripContainer1.TopToolStripPanel.PerformLayout();
          this.toolStripContainer1.ResumeLayout(false);
          this.toolStripContainer1.PerformLayout();
          this.statusStrip1.ResumeLayout(false);
          this.statusStrip1.PerformLayout();
          this.visualStudioPanel.ResumeLayout(false);
          this.visualStudioPanel.PerformLayout();
          this.toolStrip1.ResumeLayout(false);
          this.toolStrip1.PerformLayout();
          this.panel1.ResumeLayout(false);
          this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.SplitContainer splitContainerLeftRight;
        private System.Windows.Forms.TreeView modelView;
        private System.Windows.Forms.ComboBox executionStatesComboBox;
        private System.Windows.Forms.Panel leftPanel;
        private System.Windows.Forms.ListView stateValueView;
        private System.Windows.Forms.ColumnHeader state;
        private System.Windows.Forms.ColumnHeader value;
        private System.Windows.Forms.ColumnHeader lineno;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripContainer toolStripContainer1;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileMenuStrip;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem modelToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem optionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem filterFunctionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem filterInconsistentStatesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.ToolStripMenuItem filterInternalVccVariablesToolStripMenuItem;
        private System.Windows.Forms.TextBox stateValueTextBox;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
        private System.Windows.Forms.ToolStripProgressBar toolStripProgressBar1;
        private System.Windows.Forms.Panel visualStudioPanel;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton toolStripButton_filterVariables;
        private System.Windows.Forms.ToolStripButton toolStripButton_filterStates;
        private System.Windows.Forms.ToolStripButton toolStripButton_filterFunctions;
        private System.Windows.Forms.ToolStripLabel toolStripLabel1;
        private System.Windows.Forms.ToolStripLabel labelCurrentModel;
    }
}

