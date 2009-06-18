//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing.Design;
using System.Windows.Forms.Design;
using System.Windows.Forms;

namespace VerifiedCCompilerAddin.ProjectExtender.TypeConverter {
  internal class VCCVersionEditor : UITypeEditor {

    private IWindowsFormsEditorService edSvc;
    
    public override UITypeEditorEditStyle GetEditStyle(System.ComponentModel.ITypeDescriptorContext context) {
      return System.Drawing.Design.UITypeEditorEditStyle.DropDown;
    }

    public override object EditValue(System.ComponentModel.ITypeDescriptorContext context, IServiceProvider provider, object value) {

      edSvc = (IWindowsFormsEditorService)provider.GetService(typeof(IWindowsFormsEditorService));
      
      if (edSvc != null) {

        //Create the control that will actually do the editing. This can be any standard Windows.Forms control or
        //a custom made user control
        ListBox bx = new ListBox();
        bx.Items.Add("(default)");
        bx.Items.Add("VCC 1");
        bx.Items.Add("VCC 2");        
        //Here we attch an event handler to the list box to close the combo box containing 
        //it when an item is selected
        bx.SelectedIndexChanged += new EventHandler(bx_SelectedIndexChanged);
        //Here, you instruct the propertygrid to show the control in a combo box
        edSvc.DropDownControl(bx);
        return (bx.SelectedItem != null ? ((bx.Text)) : value);
      }

      return value;
    }

    void bx_SelectedIndexChanged(object sender, EventArgs e) {
      edSvc.CloseDropDown();
    }

  }
}
