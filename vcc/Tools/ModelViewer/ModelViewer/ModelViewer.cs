//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using VccModel;
using VccModel.Controller;
using Z3Model;

namespace VccModelViewer
{
  [ComVisible(true)]
  [Guid("15032E00-DDC4-44d1-927B-2A08C23D3F8F")]
  public sealed partial class ModelViewer : UserControl
  {
    ModelController modelController;
    readonly bool launchedFromAddin;

    private bool inhibitTriggerUpdate;

    private bool filterFunctions;
    public bool FilterFunctions
    {
      get { return filterFunctions; }
      set
      {
        if (value != filterFunctions)
        {
          filterFunctions = value;
          TriggerUpdate();
        }
      }
    }

    private bool filterStates;
    public bool FilterStates
    {
      get { return filterStates; }
      set
      {
        if (value != filterStates)
        {
          filterStates = value;
          TriggerUpdate();
        }
      }
    }

    private bool filterBoogieVariables;
    public bool FilterBoogieVariables
    {
      get { return filterBoogieVariables; }
      set
      {
        if (value != filterBoogieVariables)
        {
          filterBoogieVariables = value;
          TriggerUpdate();
        }
      }
    }

    enum Icons
    {
      FolderClosed = 0,
      FolderOpen   = 1,
      Field        = 2,
      Map          = 3,
      MapItem      = 4,
      Set          = 5,
      Properties   = 6,
      Structure    = 7,
      Type         = 8,
      TypeDef      = 9,
      Union        = 10, 
      ValueType    = 11,
      Method       = 12,
      Error        = 13
    };

    public event EventHandler<LineColumnChangedEventArgs> LineColumnChanged;
    public event EventHandler<ModelInformationChangedEventArgs> ModelInformationChanged;

    public ModelViewer() : this(true)
    {
    }

    public ModelViewer(bool launchedFromAddin)
    {
      this.launchedFromAddin = launchedFromAddin;

      InitializeComponent();
      this.AutoSize = true;
      LoadSettings();

      if (launchedFromAddin)
      {
        this.menuStrip1.Hide();
        this.statusStrip1.Hide();
      } else
      {
        this.visualStudioPanel.Hide();
      }
    }

    public void LoadModel(string sourceFileName, int lineNumber, int modelNumber)
    {
      if (sourceFileName != null)
      {
        string modelFileName = sourceFileName;
        if (Path.GetExtension(sourceFileName) != ".vccmodel")
        {
          modelFileName = Path.ChangeExtension(sourceFileName, ".vccmodel");
        }

        FileInfo fi = new FileInfo(modelFileName);
        if (fi.Exists)
        {
          int numberOfModels = ModelController.FindModelsInFile(modelFileName);
          if ((modelNumber < 0) || (modelNumber >= numberOfModels))
          {
            modelNumber = 0;
          }
          modelController = new ModelController();
          Cursor savedCursor = this.Cursor;
          this.Cursor = Cursors.WaitCursor;
          toolStripProgressBar1.Visible = true;
          modelController.LoadProgressChanged += modelController_LoadProgressChanged;
          modelController.LoadModel(sourceFileName, modelFileName, modelNumber);
          populateExecutionStates(GetExecutionStateFor(lineNumber));
          populateModel();
          populateModelMenu(modelNumber);
          modelController.LoadProgressChanged -= modelController_LoadProgressChanged;
          toolStripProgressBar1.Visible = false;
          toolStripStatusLabel1.Text = String.Format("{0} : Model {1}", modelFileName, modelNumber + 1);
          this.Cursor = savedCursor;

          this.labelCurrentModel.Text = fi.Name;
          OnModelInformationChanged(sourceFileName, modelNumber, modelController.ModelsInFile);
        }
        else
        {
          MessageBox.Show(String.Format("File does not exist:\n{0}", modelFileName), "File does not exist", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
      }
    }

    public void SwitchToModel( int modelId )
    {
      if (modelController != null)
      {
        string fileName = modelController.ModelFileName;
        int lineNumber = 0;
        int numberOfModels = modelController.ModelsInFile;

        if ((modelId < 0) || (modelId >= numberOfModels)) 
          return;

        ExecutionState execState = getCurrentExecutionState();
        if (execState != null)
        {
          lineNumber = execState.sourceInfo.Line;
        }
        LoadModel(fileName, lineNumber, modelId);
      }
    }

    public void ClearModel()
    {
      modelController = null;
      modelToolStripMenuItem.DropDownItems.Clear();
      executionStatesComboBox.Items.Clear();
      modelView.Nodes.Clear();
      ClearStateValueTable();
    }

    void modelController_LoadProgressChanged(object sender, ProgressChangedEventArgs e)
    {
      if (InvokeRequired)
        Invoke(new EventHandler<ProgressChangedEventArgs>(modelController_LoadProgressChanged), sender, e);
      else
        toolStripProgressBar1.Value = e.ProgressPercentage;
    }

    private void populateModelMenu()
    {
      modelToolStripMenuItem.DropDownItems.Clear();
      int modelIdx;

      for (modelIdx = 0; modelIdx < modelController.ModelsInFile; modelIdx++)
      {
        string menuEntryText = String.Format("Model {0}", modelIdx + 1);
        ToolStripMenuItem item = (ToolStripMenuItem)modelToolStripMenuItem.DropDownItems.Add(menuEntryText, null, modelMenuItemSelected);
        item.CheckOnClick = true;
        item.Tag = modelIdx;
      }
    }

    private void populateModelMenu(int modelNumber)
    {
      populateModelMenu();
      foreach (ToolStripMenuItem item in modelToolStripMenuItem.DropDownItems)
      {
        if (((int)item.Tag) == modelNumber)
        {
          item.Checked = true;
        }
      }
    }

    ExecutionState GetExecutionStateFor(int lineNumber)
    {
      List<ExecutionState> execStates = modelController.GetExecutionStates();
      execStates.Sort();
      ExecutionState result = null;
      foreach (ExecutionState exec in execStates)
      {
        if (!FilterStates || exec.good_state)
        {
          if (exec.sourceInfo.Line <= lineNumber)
          {
            result = exec;
          }
        }
      }
      return result;
    }


    void populateExecutionStates(ExecutionState execToSelect)
    {
      List<ExecutionState> execStates = modelController.GetExecutionStates();
      executionStatesComboBox.Items.Clear();
      execStates.Sort();
      int index = 0;
      int lineIndex = 0;

      foreach (ExecutionState exec in execStates)
      {
        if (!FilterStates || exec.good_state)
        {
          executionStatesComboBox.Items.Add(exec);
          if ((execToSelect != null) && (exec.CompareTo(execToSelect) <= 0))
          {
            lineIndex = index;
          }
          index++;
        }
      }
      executionStatesComboBox.SelectedIndex = lineIndex;
    }

    void populateModel()
    {
      oldSelectedNode = modelView.SelectedNode;
      List<TreeNode> oldRootNodes = new List<TreeNode>();
      foreach (TreeNode n in modelView.Nodes)
      {
        oldRootNodes.Add(n);
      }

      modelView.Nodes.Clear();

      List<FieldInfo> model = modelController.GetModel(getCurrentExecutionState());

      foreach (FieldInfo mi in model)
      {
        TreeNode tn = CreateModelNode(mi);
        if (tn != null)
        {
          modelView.Nodes.Add(tn);
        }
      }
      TreeNode pftn = DisplayPureFunctions(getCurrentExecutionState());
      if (pftn != null)
      {
        modelView.Nodes.Add(pftn);
      }

      UpdateStateValueTableCurrentExecutionState();

      UnfoldOldRootTreeNodes(oldRootNodes, modelView.Nodes);
      modelView.SelectedNode = newSelectedNode;
    }

    private TreeNode oldSelectedNode;
    private TreeNode newSelectedNode;

    private void UnfoldOldRootTreeNodes(IEnumerable<TreeNode> oldRootNodes, TreeNodeCollection currentNodes)
    {

      foreach (TreeNode node in oldRootNodes)
      {
        UnfoldOldTreeNodeCollection(node, currentNodes);
      }
    }

    private void UnfoldOldTreeNodeCollection(TreeNode oldNode, TreeNodeCollection currentNodes)
    {

      foreach (TreeNode newNode in currentNodes)
      {
        if (OldAndNewTreeNodeMatch(oldNode, newNode))
        {
          if (oldNode == oldSelectedNode)
          {
            newSelectedNode = newNode;
          }
          if (oldNode.IsExpanded)
          {
            newNode.Expand();
            foreach (TreeNode oldChildNode in oldNode.Nodes)
            {
              UnfoldOldTreeNodeCollection(oldChildNode, newNode.Nodes);

            }
          }
        }
      }

    }

    private static string RemoveEverythingAfter(string match, string old_value)
    {
      int index = old_value.IndexOf(match, 0);
      if (index >= 0)
      {
        return old_value.Remove(index);
      }
      return old_value;
    }

    private static bool OldAndNewTreeNodeMatch(TreeNode oldNode, TreeNode newNode)
    {
      /* Currently this code does nothing more than comparing the strings of the TreeNodes, which are
       * already unfolded. This works amazingly well, since items, which display values, are never unfolded.
       * Therefore this pronciple also works for items, which carry no Tags to a model.
       * */
      String oldText = oldNode.Text;
      String newText = newNode.Text;
      oldText = RemoveEverythingAfter("=", oldText);
      oldText = RemoveEverythingAfter("(Alias", oldText);
      newText = RemoveEverythingAfter("=", newText);
      newText = RemoveEverythingAfter("(Alias", newText);
      return oldText == newText;
    }

    TreeNode CreateModelNode(ModelInfo mi)
    {

      if (mi is DotInfo)
        return CreateDotNode((DotInfo)mi);
      if (mi is DotContainerInfo)
        return CreateDotContainerNode((DotContainerInfo)mi);
      if (mi is PrimitiveFieldInfo)
        return CreatePrimitiveFieldNode((PrimitiveFieldInfo)mi);
      if (mi is PtrSetEntry)
        return CreatePtrSetEntryNode((PtrSetEntry)mi);
      if (mi is PtrSetFieldInfo)
        return CreatePtrSetFieldInfoNode((PtrSetFieldInfo)mi);
      if (mi is MapFieldInfo)
        return CreateMapFieldNode((MapFieldInfo)mi);
      if (mi is ArrayInfo)
        return CreateArrayInfoNode((ArrayInfo)mi);
      if (mi is FieldInfo)
        return CreateFieldInfoNode((FieldInfo)mi);
      return null;
    }

    bool IsFilteredInternalVccVariable(string fieldName)
    {
      if ( FilterBoogieVariables && (fieldName != null)) {
        if (fieldName.Contains("#"))
        {
          return true;
        }
      }
      return false;
    }

    TreeNode CreateFieldInfoNode(FieldInfo fi)
    {
      if (IsFilteredInternalVccVariable(fi.FieldName))
      {
        return null;
      }

      string treeNodeDescription;
      int iconId;

      if (fi.FieldName != null)
      {
        if (FilterBoogieVariables)
        {
          // Since this field carries no relevant information at all, we should
          // just filter it out in the "Filtered View".
          return null;
        }

        treeNodeDescription = String.Format("{0} (Unknown type information)", fi.FieldName);
        iconId = (int)Icons.Field;
      }
      else
      {
        treeNodeDescription = String.Format("Not implemented yet: {0}", fi.GetType().Name);
        iconId = (int)Icons.Error;
      }
      return new TreeNode(treeNodeDescription, iconId, iconId);
    }

    void UnfoldModelInfo(TreeNode node, ModelInfo mi)
    {
      node.Nodes.Clear();
      if (mi is FieldInfo)
      {
        UnfoldFieldInfoHelper(node, (FieldInfo)mi);
      }

      if (mi is ArrayInfo)
      {
        UnfoldArrayInfo(node, (ArrayInfo)mi);
      }
      else if (mi is DotInfo)
      {
        UnfoldDotNode(node, (DotInfo)mi);
      }
      else if (mi is ObjectInfo)
      {
        UnfoldObjectNode(node, (ObjectInfo)mi);
      }

      else if (mi is PtrSetEntry)
      {
        UnfoldPtrSetEntryNode(node, (PtrSetEntry)mi);
      }
      else if (mi is PtrSetFieldInfo)
      {
        UnfoldPtrSetFieldInfoNode(node, (PtrSetFieldInfo)mi);
      }
      else if (mi is MapEntry)
      {
        UnfoldMapFieldEntryNode(node, (MapEntry)mi);
      }
    }

    static string GetAliasesList(FieldInfo fi, string excludeName)
    {
      string aliases = "";
      if (fi == null)
        return aliases;
  
      foreach (string alias in fi.Aliases)
      {
        if (!alias.Equals(excludeName))
        {
          if (aliases.Length > 0)
          {
            aliases += ", ";
          }
          aliases += alias;
        }
      }

      if (aliases.Length > 0)
      {
        return String.Format("(Aliases: {0})", aliases);
      }
      return aliases;
    }

    static void AddDummyNodeTo(TreeNode node)
    {
      node.Nodes.Add(new TreeNode("Dummy node"));
    }

    static TreeNode CreateDotContainerNode(DotContainerInfo o)
    {
      string type_info = "";
      string deref_value = "";
      if (o.Volatile) type_info += "volatile ";

      if ((o.DerefValue != null) && (o.Primitive))
      {
        deref_value = String.Format(" = {0}", o.DerefValue.DisplayValue);
      }
      TreeNode objectNode = new TreeNode(String.Format("{0}{3} : {2}{1} {4}",
        o.FieldName, o.FieldType, type_info, deref_value, GetAliasesList(o, o.FieldName)), 
        (int)Icons.Field, (int)Icons.Field) {Tag = o};
      AddDummyNodeTo(objectNode);
      return objectNode;
    }

    private static void appendToString(string text, ref string result)
    {
      if (result.Length > 0)
        result += ", ";
      result += text;
    }

    static TreeNode UnfoldFieldInfoHelper(TreeNode objectNode, FieldInfo o)
    {
      TreeNode statusNode = new TreeNode("Status", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      string typed_state = "";
      string status_tag = "";

      if ((o.HeapAddress != null) && (!string.IsNullOrEmpty(o.HeapAddress.Value)))
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Heap address: {0}", o.HeapAddress.Value), (int)Icons.Properties, (int)Icons.Properties));
      }
      if ((o.TimeStamp != null) && (!string.IsNullOrEmpty(o.TimeStamp.Value)))
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Timestamp: {0}", o.TimeStamp.Value), (int)Icons.Properties, (int)Icons.Properties));
      }
      if ((o.Owner != null) && (!string.IsNullOrEmpty(o.Owner.Value)))
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Owner: {0}", o.Owner.Value), (int)Icons.Properties, (int)Icons.Properties));
      }
      if ((o.ThreadOwner != null) && (!string.IsNullOrEmpty(o.ThreadOwner.Value)))
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Thread Owner: {0}", o.ThreadOwner.Value), (int)Icons.Properties, (int)Icons.Properties));
      }
      if (o.Closed) appendToString("closed", ref status_tag);
      if (o.Mutable) appendToString("mutable", ref status_tag);
      if (o.ThreadLocal) appendToString("thread local", ref status_tag);
      if (o.Claimable) { appendToString("claimable", ref status_tag); }
      if (o.Typed) appendToString("typed", ref typed_state);
      if (o.Volatile) appendToString("volatile", ref typed_state);
      if (o.ArrayElement) appendToString("array element", ref typed_state);

      if (status_tag.Length > 0)
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Status Tag: {0}", status_tag), (int)Icons.Properties, (int)Icons.Properties));
      }
      if (typed_state.Length > 0)
      {
        statusNode.Nodes.Add(new TreeNode(String.Format("Typed State Tag: {0}", typed_state), (int)Icons.Properties, (int)Icons.Properties));
      }

      if (statusNode.Nodes.Count > 0)
      {
        objectNode.Nodes.Add(statusNode);
      }

      if (o.Ghost)
      {
        statusNode.Nodes.Add(new TreeNode("Ghost field", (int)Icons.Properties, (int)Icons.Properties));
      }
      return statusNode;
    }

    void UnfoldObjectNode(TreeNode objectNode, DotContainerInfo o)
    {
      // Fields
      TreeNode fieldsNode = new TreeNode("Fields", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      foreach (DotInfo di in o.Dots)
      {
        FieldInfo fi = di.Field;
        if (fi != null)
        {
          TreeNode fi_node = CreateModelNode(fi);
          if (fi_node != null)
          {
            fieldsNode.Nodes.Add(fi_node);
          }
        }

        if ((di.Dots != null) && (di.Dots.Count > 0))
        {
          // This is a container for embedded structs
          //string fieldName = ModelController.GetFieldNameOfDotDereference(di.FieldName);
          string fieldName = di.FieldName;
          if (di.FieldType != null)
          {
            fieldName = String.Format("{0} : {1}*", fieldName, di.FieldType);
          }

          string aliases = GetAliasesList(di, di.FieldName);
          if (aliases.Length > 0)
          {
            fieldName += " " + aliases;
          }

          TreeNode structNode = new TreeNode(fieldName, (int)Icons.Structure, (int)Icons.Structure);
          UnfoldFieldInfoHelper(structNode, di);
          UnfoldObjectNode(structNode, di);

          if (structNode.Nodes.Count > 0)
          {
            fieldsNode.Nodes.Add(structNode);
          }
        }
      }
      if (fieldsNode.Nodes.Count > 0)
      {
        objectNode.Nodes.Add(fieldsNode);
      }
      UnfoldDotContainerNodeHelper(objectNode, o);
    }

    static TreeNode CreateDotNode(DotInfo o)
    {
      string type_info = "";
      if (o.Volatile) type_info += "volatile ";
      if ((o.Field != null) && (o.Field.FieldTypePartition != null))
      {
        type_info += String.Format("{0}*", o.Field.FieldTypePartition.DisplayValue);
      }
      else if (o.FieldTypePartition != null)
      {
        type_info += String.Format("{0}*", o.FieldTypePartition.DisplayValue);
      }

      string node_info = o.FieldName;
      if (type_info.Length > 0)
      {
        node_info += String.Format(" : {0}", type_info);
      }

      string aliases = GetAliasesList(o, o.FieldName);
      if (aliases.Length > 0)
      {
        node_info += " " + aliases;
      }

      TreeNode result = new TreeNode(node_info, (int)Icons.TypeDef, (int)Icons.TypeDef) {Tag = o};
      AddDummyNodeTo(result);
      return result;
    }

    void UnfoldDotNode(TreeNode node, DotInfo o)
    {
      //UnfoldDotContainerNodeHelper(node, o);
      UnfoldObjectNode(node, o);
    }

    void UnfoldDotContainerNodeHelper(TreeNode node, DotContainerInfo o)
    {
      // Functions
      TreeNode functionsNode = new TreeNode("Functions", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      foreach (FunctionInfo fi in o.Functions)
      {
        TreeNode functionNode = CreateFunctionNode(fi, o.FieldName, o.FieldPartition);
        if (functionNode != null)
        {
          functionsNode.Nodes.Add(functionNode);
        }
      }

      // Dots
      TreeNode dotsNode = new TreeNode("Field Addresses", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      foreach (DotInfo di in o.Dots)
      {
        dotsNode.Nodes.Add(CreateDotNode(di));
      }

      // Add created nodes to TreeView
      if (dotsNode.Nodes.Count > 0)
      {
        node.Nodes.Add(dotsNode);
      }
      if (functionsNode.Nodes.Count > 0)
      {
        node.Nodes.Add(functionsNode);
      }
    }

    static TreeNode CreateArrayInfoNode(ArrayInfo arrayInfo)
    {
      TreeNode result = new TreeNode(arrayInfo.FieldName, (int)Icons.Structure, (int)Icons.Structure) {Tag = arrayInfo};
      AddDummyNodeTo(result);

      return result;
    }

    void UnfoldArrayInfo(TreeNode node, ArrayInfo o)
    {
      node.Nodes.Clear();

      TreeNode nip = CreateModelNode(o.NotInterpretedPointer);
      if (nip != null)
      {
        nip.ImageIndex = (int)Icons.ValueType;
        nip.SelectedImageIndex = nip.ImageIndex;
        node.Nodes.Add(nip);
      }

      foreach (FieldInfo ai in o.ArrayInterpretations)
      {
        TreeNode oin = CreateModelNode(ai);
        if (oin != null)
        {
          node.Nodes.Add(oin);
        }
      }

      var keyList = new List<long>(o.ElementDots.Keys);
      keyList.Sort();
      foreach (var idx in keyList)
      {
        FieldInfo fi = o.ElementDots[idx];
        if (fi != null)
        {
          TreeNode elem = CreateModelNode(fi);
          //elem.Tag = fi;
          if (elem != null)
          {
            node.Nodes.Add(elem);
          }
        }
      }
    }

    TreeNode CreatePrimitiveFieldNode(PrimitiveFieldInfo o)
    {
      string field_node;
      if (IsFilteredInternalVccVariable(o.FieldName))
      {
        // these are internal "variables" of the Boogie model, which should not be displayed to the user.
        return null;
      }

      if (o.FieldTypePartition == null)
      {
        field_node = String.Format("{0} = {1}", o.FieldName, o.FieldValue);
      }
      else
      {
        string type_info = "";
        string value_string = "";
        if (o.Volatile) type_info += "volatile ";
        if (o.FieldValue != null) 
          value_string = String.Format(" = {0}", o.FieldValue);
        field_node = String.Format("{0}{1} : {3}{2}", o.FieldName, value_string, o.FieldType, type_info);
      }
      TreeNode result = new TreeNode(field_node, (int)Icons.ValueType, (int)Icons.ValueType) {Tag = o};
      return result;
    }

    TreeNode CreatePtrSetEntryNode(PtrSetEntry o)
    {
      string entry_name = o.TypePartition != null ? String.Format("{0} : {1}", o.FieldName, o.TypePartition.DisplayValue) : o.FieldName;
      TreeNode result = new TreeNode(entry_name, (int)Icons.Field, (int)Icons.Field) {Tag = o};
      if (o.Field.Type != PartitionType.Value)
      {
        UnfoldPtrSetEntryNode(result, o);
      }
      return result;
    }

    void UnfoldPtrSetEntryNode(TreeNode node, PtrSetEntry mi)
    {
      if (mi.fieldNameDatabase.ContainsKey(mi.Field))
      {
        FieldInfo fi = mi.fieldNameDatabase[mi.Field];
        UnfoldModelInfo(node, fi);
      }
    }

    static TreeNode CreatePtrSetFieldInfoNode(PtrSetFieldInfo o)
    {
      string type_info = "";
      if (o.Volatile) type_info += "volatile ";
      if (o.OwnsSetField)
      {
        type_info += "owns_set_t";
      }
      else
      {
        type_info += o.FieldType;
      }

      TreeNode result = new TreeNode(String.Format("{0} : {1}", o.FieldName, type_info), (int)Icons.Set, (int)Icons.Set) {Tag = o};
      AddDummyNodeTo(result);
      return result;
    }

    void UnfoldPtrSetFieldInfoNode(TreeNode node, PtrSetFieldInfo mi)
    {
      // Display elements and not elements in set.
      TreeNode IsInNode = new TreeNode("set_in", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      TreeNode NotInNode = new TreeNode("! set_in", (int)Icons.FolderOpen, (int)Icons.FolderOpen);
      node.Nodes.Add(IsInNode);
      node.Nodes.Add(NotInNode);
      foreach (PtrSetEntry entry in mi.Entries)
      {
        TreeNode newNode = CreateModelNode(entry);
        if (newNode != null)
        {
          if (entry.isInSet)
          {
            IsInNode.Nodes.Add(newNode);
          }
          else
          {
            NotInNode.Nodes.Add(newNode);
          }
        }
      }
    }


    TreeNode CreateMapFieldNode(MapFieldInfo m)
    {
      string type_info = "";
      if (IsFilteredInternalVccVariable(m.FieldName))
      {
        // these are internal "variables" of the Boogie model, which should not be displayed to the user.
        return null;
      }

      if (m.FieldTypePartition != null)
      {
        type_info = String.Format(" : {0}", m.FieldTypePartition.DisplayValue);
      }
      TreeNode result = new TreeNode(String.Format("{0}{1}", m.FieldName, type_info), (int)Icons.Map, (int)Icons.Map);
      foreach (MapEntry e in m.Entries)
      {
        string nodeText = String.Format("{0} --> {1}", e.KeyName, e.ValueName);
        TreeNode eNode = new TreeNode(nodeText, (int)Icons.MapItem, (int)Icons.MapItem) {Tag = e};
        if ((e.KeyField != null) || (e.ValueField != null))
        {
          AddDummyNodeTo(eNode);
        }
        result.Nodes.Add(eNode);
      }

      return result;
    }

    static void UnfoldMapFieldEntryNode(TreeNode node, MapEntry mi)
    {
      string nodeName;
      if (mi.KeyField != null)
      {
        nodeName = mi.KeyField.FieldName.Equals(mi.KeyName) ? String.Format("{0} : {1}", mi.KeyField.FieldName, mi.KeyField.FieldType) : String.Format("({1}*){2} == {0} : {1}", mi.KeyField.FieldName, mi.KeyField.FieldType, mi.KeyName);
        TreeNode tn = new TreeNode(nodeName, (int)Icons.MapItem, (int)Icons.MapItem) {Tag = mi.KeyField};
        AddDummyNodeTo(tn);
        node.Nodes.Add(tn);
      }
      if (mi.ValueField != null)
      {
        nodeName = mi.ValueField.FieldName.Equals(mi.ValueName) ? String.Format("{0} : {1}", mi.ValueField.FieldName, mi.ValueField.FieldType) : String.Format("({1}*){2} == {0} : {1}", mi.ValueField.FieldName, mi.ValueField.FieldType, mi.ValueName);
        TreeNode tn = new TreeNode(nodeName, (int)Icons.MapItem, (int)Icons.MapItem) {Tag = mi.ValueField};
        AddDummyNodeTo(tn);
        node.Nodes.Add(tn);
      }
    }

    TreeNode CreateFunctionNode(FunctionInfo o, string fieldName, Partition dependingPartition)
    {
      if (!isHiddenFunction(o.Function, dependingPartition))
      {
        TreeNode result = new TreeNode(modelController.GetStateStrippedFunctionStringWithResult(o.Function, fieldName, getCurrentExecutionState()), (int)Icons.Method, (int)Icons.Method)
                            {Tag = o};
        return result;
      }
      return null;
    }

    bool isHiddenFunction(Function f, Partition dependingPartition)
    {
      if (!FilterFunctions)
      {
        return false;
      }
      if (((f.Result == dependingPartition) && (hiddenFunctions.Contains(f.FunctionType)))
            || (f.FunctionArguments.Contains(dependingPartition) && hiddenFunctions.Contains(f.FunctionType)))
      {
        return true;
      }
      return false;
    }

    ExecutionState getCurrentExecutionState()
    {
      return (ExecutionState)executionStatesComboBox.SelectedItem;
    }

    void unfold(TreeNode p)
    {
      if (p.Tag == null) return;

      UnfoldModelInfo(p, (ModelInfo)p.Tag);
    }

    private void modelView_BeforeExpand(object sender, TreeViewCancelEventArgs e)
    {
      unfold(e.Node);
    }

    private void stateValueView_DoubleClick(object sender, EventArgs e)
    {
      ListViewItem lvi = (stateValueView.SelectedItems.Count > 0) ? stateValueView.SelectedItems[0] : null;
      if (lvi != null)
      {
        ExecutionState exec = (ExecutionState)lvi.Tag;
        for (int index = 0; index < executionStatesComboBox.Items.Count; index++)
        {
          ExecutionState execState = (ExecutionState)executionStatesComboBox.Items[index];
          if (exec.state == execState.state)
          {
            executionStatesComboBox.SelectedIndex = index;
          }
        }
      }
    }

    private void exitToolStripMenuItem_Click(object sender, EventArgs e)
    {
      if (!launchedFromAddin) {
        Application.Exit();
      }
    }

    private void openToolStripMenuItem_Click(object sender, EventArgs e)
    {
      OpenFileDialog openDialog = new OpenFileDialog
                                    {
                                      Filter =
                                        "VCC Model (*.vccmodel)|*.vccmodel|Text Files (*.txt)|*.txt|AllFiles (*.*)|*.*",
                                      FilterIndex = 0,
                                      RestoreDirectory = true
                                    };

      if (openDialog.ShowDialog() == DialogResult.OK)
      {
        LoadModel(openDialog.FileName, 1, 0);
      }
    }

    private void executionStatesComboBox_SelectedIndexChanged(object sender, EventArgs e)
    {
      populateModel();
      ExecutionState execState = getCurrentExecutionState();
      if ((execState != null) && (execState.sourceInfo != null) && (execState.sourceInfo.FileName != null))
      {
        OnLineColumnChanged(execState.sourceInfo.FileName, execState.sourceInfo.Line, execState.sourceInfo.Column);
      }
    }

    private void modelView_displayCurrentModelInfo(ModelInfo mi)
    {
      if (mi == null)
      {
        ClearStateValueTable();
        return;
      }
      String tableHeader = "";
      Dictionary<ExecutionState, string> dict = modelController.GetValuesStates(mi, ref tableHeader, getCurrentExecutionState());
      PopulateStateValueTable(tableHeader, dict);
    }

    private void modelView_nodeMouseClick(object sender, TreeNodeMouseClickEventArgs e)
    {
      TreeNode clickedNode = e.Node;
      modelView_displayCurrentModelInfo((ModelInfo)clickedNode.Tag);
    }

    private TreeNode DisplayPureFunctions(ExecutionState execState)
    {
      List<FunctionInfo> funcs = modelController.GetPureFunctions(execState);
      if (funcs.Count > 0)
      {
        TreeNode functionsNode = new TreeNode("Functions", (int)Icons.FolderOpen, (int)Icons.FolderOpen);

        foreach (FunctionInfo fi in funcs)
        {
          TreeNode functionNode = CreateFunctionNode(fi, fi.Function.FunctionType, execState.state);
          if (functionNode != null)
          {
            functionsNode.Nodes.Add(functionNode);
          }          
        }

        return functionsNode;
      }
      return null;
    }

    void ClearStateValueTable()
    {
      stateValueTextBox.Text = "";
      stateValueView.Items.Clear();
    }

    void PopulateStateValueTable(string tableHeader, Dictionary<ExecutionState, string> dictionary)
    {
      stateValueView.BeginUpdate();
      ClearStateValueTable();
      if (dictionary.Count > 0)
      {
        List<ExecutionState> execStates = new List<ExecutionState>();
        foreach(var key in dictionary.Keys) {
          execStates.Add(key);
        }
        execStates.Sort();
        string lastValueInState = null;
        foreach (ExecutionState exec in execStates)
        {
          if (!FilterStates || exec.good_state)
          {
            if (dictionary.ContainsKey(exec))
            {
              string valueInState = dictionary[exec];
              string[] lviTexts = new string[3];
              lviTexts[0] = exec.sourceInfo.ToString();
              lviTexts[1] = exec.state.Value;
              lviTexts[2] = valueInState;

              ListViewItem lvi = new ListViewItem(lviTexts) {Tag = exec, ToolTipText = valueInState};
              if (lastValueInState != null && lastValueInState != valueInState)
              {
                Font bold = new Font(lvi.Font, FontStyle.Bold);
                lvi.Font = bold;
              }
              lastValueInState = valueInState;
              stateValueView.Items.Add(lvi);
            }
          }
        }
        UpdateStateValueTableCurrentExecutionState();
        stateValueTextBox.Text = tableHeader;
      }
      stateValueView.EndUpdate();
    }

    void UpdateStateValueTableCurrentExecutionState()
    {
      ExecutionState currentState = getCurrentExecutionState();
      foreach (ListViewItem lvi in stateValueView.Items)
      {
        ExecutionState itemState = (ExecutionState)lvi.Tag;
        lvi.ForeColor = (itemState.state == currentState.state) ? Color.Red : Color.Black;
      }
    }

    readonly List<string> hiddenFunctions = new List<string> { 
        "$closed",
        "$dot",
        "$extent",
        "$extent_hint",
        "$fetch_from_domain",
        "$full_extent", 
        "$ghost_emb",
        "$ghost_ref", 
        "$idx", 
        "$index_within",
        "$int_to_ptr", 
        "$int_to_ptrset", 
        "$in_some_owns",
        "$is",
        "$local_value_is",
        "$local_value_is_ptr",
        "$mutable",
        "$owner",
        "$owns",
        "$ptr",
        "$ptrset_to_int",
        "$ptr_to_int", 
        "$read_i1",
        "$read_i2",
        "$read_i4",
        "$read_i8",
        "$read_ptr_m",
        "$read_u1",
        "$read_u2",
        "$read_u4",
        "$read_u8",
        "$read_version",
        "$ref", 
        "$ref_cnt",
        "$rev_ref_cnt_ptr",
        "$select.mem",
        "$select.sm",
        "$select.tm",
        "$sequential",
        "$set_in",
        "$set_in0",
        "$set_in2",
        "$set_in3",
        "$set_in4",
        "$span",
        "$spans_the_same",
        "$state_spans_the_same",
        "$st_owner",
        "$thread_local",
        "$thread_owner",
        "$timestamp",
        "$ts_emb", 
        "$ts_typed",
        "$typ", 
        "$typed"
      };

    private void modelMenuItemSelected(object sender, EventArgs e)
    {
      ToolStripMenuItem item = (ToolStripMenuItem)sender;
      if (item == null) return;

      int lineNumber = 0;
      int modelIdx = (int) item.Tag;
      ExecutionState execState = getCurrentExecutionState();
      if (execState != null)
      {
        lineNumber = execState.sourceInfo.Line;
      }
      LoadModel(modelController.ModelFileName, lineNumber, modelIdx);
    }

    private void modelView_AfterSelect(object sender, TreeViewEventArgs e)
    {
      if (e.Node != null)
      {
        modelView_displayCurrentModelInfo((ModelInfo)e.Node.Tag);
      }
    }

    private static void aboutToolStripMenuItem_Click(object sender, EventArgs e)
    {
      new AboutBox().Show();
    }

    private void filterOptionsMenuItem_Click(object sender, EventArgs e)
    {
      inhibitTriggerUpdate = true;
      FilterStates = filterInconsistentStatesToolStripMenuItem.Checked;
      FilterFunctions = filterFunctionsToolStripMenuItem.Checked;
      FilterBoogieVariables = filterInternalVccVariablesToolStripMenuItem.Checked;
      inhibitTriggerUpdate = false;
      SaveSettings();

      TriggerUpdate();
    }

    private void toolStripButton_filterClick(object sender, EventArgs e)
    {
      inhibitTriggerUpdate = true;
      FilterStates = toolStripButton_filterStates.Checked;
      FilterFunctions = toolStripButton_filterFunctions.Checked;
      FilterBoogieVariables = toolStripButton_filterVariables.Checked;
      inhibitTriggerUpdate = false;
      SaveSettings();

      TriggerUpdate();
    }

    void LoadSettings()
    {
      FilterStates = !Properties.ModelViewerSettings.Default.ShowInconsistentStates;
      FilterFunctions = !Properties.ModelViewerSettings.Default.ShowAllFunctions;
      FilterBoogieVariables = !Properties.ModelViewerSettings.Default.ShowInternalVccVariables;

      filterInconsistentStatesToolStripMenuItem.Checked = FilterStates;
      filterFunctionsToolStripMenuItem.Checked = FilterFunctions;
      filterInternalVccVariablesToolStripMenuItem.Checked = FilterBoogieVariables;

      toolStripButton_filterStates.Checked = FilterStates;
      toolStripButton_filterFunctions.Checked = FilterFunctions;
      toolStripButton_filterVariables.Checked = FilterBoogieVariables;
    }

    void SaveSettings()
    {
      Properties.ModelViewerSettings.Default.ShowInconsistentStates = !FilterStates;
      Properties.ModelViewerSettings.Default.ShowAllFunctions = !FilterFunctions;
      Properties.ModelViewerSettings.Default.ShowInternalVccVariables = !FilterBoogieVariables;

      Properties.ModelViewerSettings.Default.Save();
    }

    void TriggerUpdate()
    {
      if (toolStripButton_filterStates.Checked != FilterStates)
        toolStripButton_filterStates.Checked = FilterStates;
      if (toolStripButton_filterFunctions.Checked != FilterFunctions)
        toolStripButton_filterFunctions.Checked = FilterFunctions;
      if (toolStripButton_filterVariables.Checked != FilterBoogieVariables)
        toolStripButton_filterVariables.Checked = FilterBoogieVariables;
      if (filterInconsistentStatesToolStripMenuItem.Checked != FilterStates)
        filterInconsistentStatesToolStripMenuItem.Checked = FilterStates;
      if (filterFunctionsToolStripMenuItem.Checked != FilterFunctions)
        filterFunctionsToolStripMenuItem.Checked = FilterFunctions;
      if (filterInternalVccVariablesToolStripMenuItem.Checked != FilterBoogieVariables)
        filterInternalVccVariablesToolStripMenuItem.Checked = FilterBoogieVariables;

      if ((!inhibitTriggerUpdate) && (executionStatesComboBox.SelectedIndex >= 0))
      {
        populateExecutionStates((ExecutionState)executionStatesComboBox.SelectedItem);
        populateModel();
      }
    }

    private void OnLineColumnChanged(string fileName, int lineNumber, int columnNumber)
    {
      EventHandler<LineColumnChangedEventArgs> eventHandler = LineColumnChanged;
      if (eventHandler != null)
      {
        eventHandler(this, new LineColumnChangedEventArgs(fileName, lineNumber, columnNumber));
      }
    }

    private void OnModelInformationChanged(string fileName, int selectedModel, int numberOfModels)
    {
      EventHandler<ModelInformationChangedEventArgs> eventHandler = ModelInformationChanged;
      if (eventHandler != null)
      {
        eventHandler(this, new ModelInformationChangedEventArgs(fileName, selectedModel, numberOfModels));
      }
    }

    private void toolStripMenuItem1_Click(object sender, EventArgs e)
    {
      var items = this.stateValueView.SelectedItems;
      if (items.Count == 1) {
        var item = items[0];
        if (item.SubItems.Count >= 3)
          Clipboard.SetText(item.SubItems[2].Text);
      }
    }

    private void stateValueView_SelectedIndexChanged(object sender, EventArgs e)
    {
      this.contextMenuStrip1.Enabled = (this.stateValueView.SelectedItems.Count == 1);
    }

  }

  // This event is being signalled every time the selected line/column changes
  public class LineColumnChangedEventArgs : EventArgs
  {
    public LineColumnChangedEventArgs(string fileName, int lineNumber, int columnNumber)
    {
      this.fileName = fileName;
      this.lineNumber = lineNumber;
      this.columnNumber = columnNumber;
    }

    public string fileName;
    public int lineNumber;
    public int columnNumber;

  }	//end of class LineColumnChangedEventArgs

  // This event is being signalled every time the model information changes
  public class ModelInformationChangedEventArgs : EventArgs
  {
    public ModelInformationChangedEventArgs(string fileName, int selectedModel, int numberOfModels)
    {
      this.fileName = fileName;
      this.selectedModel = selectedModel;
      this.numberOfModels = numberOfModels;
    }

    public string fileName;
    public int selectedModel;
    public int numberOfModels;
  }

}
