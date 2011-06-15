//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Forms.UserControls;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Forms {

  public partial class VCCPane : UserControl {
    delegate void StringParameterDelegate(string value);
    delegate void EventArgsParameterDelegate(EventArgs value);
    delegate void GotFocusParameterDelegate(Window value);

    public VCCPane() {
      InitializeComponent();

      VerifyManager.OnProgressUpdateHandler += new VerifyManager.ProgressUpdateEventHandler(VerifyManger_OnProgressUpdateHandler);
      VerifyManager.OnFunctionValidatedHandler += new VerifyManager.FunctionValidatedEventHandler(VerifyManger_OnFunctionValidatedHandler);
      VerifyManager.OnJobsDoneHandler += new VerifyManager.JobsDoneEventHandler(VerifyManger_OnJobsDoneHandler);
      VerifyManager.OnExecuteHandler += new VerifyManager.ExecuteEventHandler(VerifyManager_OnExecuteHandler);
    }

    #region Registered event handler
    void VerifyManager_OnExecuteHandler(JobsExecuteEventArgs e) {
      UpdateVerifyButton("Cancel VCC");
    }
    void VerifyManger_OnJobsDoneHandler(JobDoneEventArgs e) {     
      UpdateVerifyButton("Verify Selected");
    }
    void VerifyManger_OnFunctionValidatedHandler(FunctionValidatedEventArgs e) {
      UpdateAfterEventReceived(e);
    }
    void VerifyManger_OnProgressUpdateHandler(ProgressUpdateEventArgs e) {
      UpdateAfterEventReceived(e);
    }
    internal void OnWindowActivated(Window GotFocus, Window LostFocus) {
      UpdateAfterEventWindowActivatedReceived(GotFocus);
    }
    #endregion

    #region Thread safety event actions
    private void UpdateVerifyButton(string value) {
      if (InvokeRequired) {
        // We're not in the UI thread, so we need to call BeginInvoke
        BeginInvoke(new StringParameterDelegate(UpdateVerifyButton), new object[] { value });
        return;
      }
      // Must be on the UI thread if we've got this far
      if (value == "Verify Selected") {
        foreach (VerifyListItem Item in verifyListBox1.Items) {
          if (Item.State == VerifyListItemState.Active)
            Item.State = VerifyListItemState.Normal;
        }
      }
      btnVerify.Text = value;
    }
    private void UpdateAfterEventWindowActivatedReceived(Window value) {
      if (InvokeRequired) {
        // We're not in the UI thread, so we need to call BeginInvoke
        BeginInvoke(new GotFocusParameterDelegate(UpdateAfterEventWindowActivatedReceived), new object[] { value });
        return;
      }

      if (value.Document != null && LastDocument != value.Document) {
        lbActiveFile.Text = value.Document.Name;
        verifyListBox1.DataSource = FunctionNames(value.Document);
        LastDocument = value.Document;
      }
      else {
        int count = 0;
        foreach (Document doc in AddInGlobals.DTE.Documents) {
          count += (doc.Name.EndsWith(".c") || doc.Name.EndsWith(".h")) ? 1 : 0;
        }
        if (count > 0)
          return;
        lbActiveFile.Text = "(no Solution or File open)";
        verifyListBox1.DataSource = null; 
      }
      
    }
    private void UpdateAfterEventReceived(EventArgs value) {
      if (InvokeRequired) {
        // We're not in the UI thread, so we need to call BeginInvoke
        BeginInvoke(new EventArgsParameterDelegate(UpdateAfterEventReceived), new object[] { value });
        return;
      }

      //Begin.. work...
      if (value is FunctionValidatedEventArgs) {
        DoTaskOnFunctionValidatedEvent(value);
      }
      else if (value is ProgressUpdateEventArgs) {
        DoTaskOnProgressUpdate(value);  
      }

      verifyListBox1.Invalidate();
    }
    private void DoTaskOnProgressUpdate(EventArgs value) {
      ProgressUpdateEventArgs e = value as ProgressUpdateEventArgs;
      foreach (VerifyListItem Item in verifyListBox1.Items) {
        if (((Item.Text.Split('(')[0] == e.FunctionName) && (e.Progress > 0))) {
          Item.State = VerifyListItemState.Active;
          Item.Progess = e.Progress;
        }
      }
    }
    private void DoTaskOnFunctionValidatedEvent(EventArgs value) {
      FunctionValidatedEventArgs e = value as FunctionValidatedEventArgs;
      foreach (VerifyListItem Item in verifyListBox1.Items) {
        if (Item.Text.Split('(')[0] == e.FunctionName) {
          if (e.State) {
            Item.State = VerifyListItemState.Verified;
          }
          else {
            Item.State = VerifyListItemState.NotVerified;
          }
        }
      }
    }
    #endregion

    private Document LastDocument = null;

    #region Properties
    private DTE2 _applicationObject;
    public DTE2 ApplicationObject {
      get { return _applicationObject; }
      set { _applicationObject = value; }
    }
    #endregion

    private Dictionary<Document, List<VerifyListItem>> History = new Dictionary<Document, List<VerifyListItem>>();

    //Stores the actual ListItems State to recall it later in this session
    private void StoreToHistory(Document doc) {
      
      if (History.ContainsKey(doc)) {
        History[doc] = verifyListBox1.DataSource as List<VerifyListItem>;
      }
      else {
        History.Add(doc, verifyListBox1.DataSource as List<VerifyListItem>);
      }
    }

    private void VerifyButton_Click(object sender, EventArgs e) {

      if (btnVerify.Text == "Cancel VCC") {
        VerifyManager.Cancel();
        return;
      }

      AddInGlobals.VCCMarkerManger.InvalidateAllMarkers();
      VerifyManager.Init();
      StringBuilder Functions = new StringBuilder();
      int count = 0;
      foreach (VerifyListItem Item in verifyListBox1.Items) {
        Item.State = VerifyListItemState.Normal;
        if (Item.Checked) {
          count++;
          string Fkt = String.Empty;
          int spos = Item.Text.IndexOf('(');
          Fkt = Item.Text.Substring(0, spos);
          Functions.Append(Fkt);
          Functions.Append(';');
        }
      }
      //If nothing selected, then is nothing to do...
      if (count == 0)
        return;

      VCCSettings sets = new VCCSettings(AddInGlobals.ActiveDocument.ProjectItem,
                                   Utilities.GetActiveConfigOfProject(AddInGlobals.ActiveDocument.ProjectItem.ContainingProject));

      VerifyJob Job = new VerifyJob(AddInGlobals.ActiveDocument.FullFileName,
                                   sets,
                                   Utilities.GetActivePlattformID(AddInGlobals.ActiveDocument.ProjectItem),
                                   Functions.ToString());
      VerifyManager.AddJob(Job);

     VerifyManager.Execute();
    }

    private List<VerifyListItem> FunctionNames(Document doc) {
      
      if (LastDocument != null)
        StoreToHistory(LastDocument);  //Save old list...

      List<VerifyListItem> FunctionNames = new List<VerifyListItem>();
      FunctionNames.Clear();

      if (doc != null && doc.ProjectItem != null && doc.ProjectItem.FileCodeModel != null) {
        CodeElements codeElements = doc.ProjectItem.FileCodeModel.CodeElements;
        if (codeElements != null)
        {
          try
          {
            foreach (CodeElement codeElement in codeElements)
            {
              if (codeElement.Kind == vsCMElement.vsCMElementFunction)
              {
                string add = "";
                foreach (CodeElement Child in codeElement.Children)
                {
                  EditPoint startPoint = Child.StartPoint.CreateEditPoint();
                  EditPoint endPoint = Child.EndPoint.CreateEditPoint();
                  string Text = startPoint.GetText(endPoint);
                  Text = Text.Trim();
                  add += Text + " ";
                }
                add = add.TrimEnd();
                add = RemoveComments(add);

                VerifyListItem ListEntry = new VerifyListItem(codeElement.Name + "(" + add + ")");
                FunctionNames.Add(ListEntry);
              }
            }
          }
          catch { }
        }
      }

      //Lookup History
      if (History.ContainsKey(doc)) {
        //if there a History use it...
        if (History[doc].Count == FunctionNames.Count)
          return History[doc];
      }
      return FunctionNames;
    }

    private string RemoveComments(string add) {
      //Remove any comment
      //Remove line comment
      while (add.IndexOf("//") > 0) {
        int spos = add.IndexOf("//");
        int epos = add.IndexOf(Environment.NewLine, spos);
        add = add.Remove(spos, epos - spos);
      }

      //Remove multiline comment
      while (add.IndexOf("/*") > 0) {
        int spos = add.IndexOf("/*");
        int epos = add.IndexOf("*/") + 2;
        add = add.Remove(spos, epos - spos);
      }

      //Remove all spaces!
      string[] addlines = add.Split(Environment.NewLine.ToCharArray());
      for (int i = 0; i < addlines.Length; i++) {
        addlines[i] = addlines[i].Trim();
      }

      add = String.Join(" ", addlines);
      return add;
    }

    private void verifyFunctionToolStripMenuItem_Click(object sender, EventArgs e) {
      VerifyManager.Init();

      string FunctionName = (verifyListBox1.SelectedItem as VerifyListItem).Text;
      FunctionName = FunctionName.Split('(')[0];

      VCCSettings sets = new VCCSettings(AddInGlobals.ActiveDocument.ProjectItem,
                                Utilities.GetActiveConfigOfProject(AddInGlobals.ActiveDocument.ProjectItem.ContainingProject));

      VerifyJob Job = new VerifyJob(AddInGlobals.ActiveDocument.FullFileName,
                                   sets,
                                   Utilities.GetActivePlattformID(AddInGlobals.ActiveDocument.ProjectItem),
                                   FunctionName);
      VerifyManager.AddJob(Job);
      VerifyManager.Execute();
    }

    private void verifyListBox1_MouseUp(object sender, MouseEventArgs e) {
      if (e.Button == MouseButtons.Right) {
        int index = verifyListBox1.IndexFromPoint(e.Location);
        if (index < 0)
          return;
        verifyListBox1.SetSelected(index, true);
        contextMenuStrip1.Show(sender as Control, e.X, e.Y);
      }
    }

    private void cancelVCCToolStripMenuItem_Paint(object sender, PaintEventArgs e) {
      cancelVCCToolStripMenuItem.Enabled = VerifyManager.isRunning;
    }

    private void verifyFunctionToolStripMenuItem_Paint(object sender, PaintEventArgs e) {
      verifyFunctionToolStripMenuItem.Enabled = !VerifyManager.isRunning;
    }

    private void cancelVCCToolStripMenuItem_Click(object sender, EventArgs e) {
      VerifyManager.Cancel();
    }

    public void RefreshData() {
      if (AddInGlobals.ActiveDocument.Document != null) {
        verifyListBox1.DataSource = FunctionNames(AddInGlobals.ActiveDocument.Document);
        LastDocument = AddInGlobals.ActiveDocument.Document;
      }
    }

  }


}
