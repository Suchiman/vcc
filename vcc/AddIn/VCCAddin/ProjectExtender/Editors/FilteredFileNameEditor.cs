//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Drawing.Design;
using System.ComponentModel;

namespace VerifiedCCompilerAddin.ProjectExtender {

  internal class FilteredFileNameEditor : UITypeEditor {
    private OpenFileDialog ofd = new OpenFileDialog();

    public override UITypeEditorEditStyle GetEditStyle(
     ITypeDescriptorContext context) {
      return UITypeEditorEditStyle.Modal;
    }

    public override object EditValue(ITypeDescriptorContext context, IServiceProvider provider, object value) 
    {
      ofd.FileName = value.ToString();
      ofd.Filter = "Code File|*.c|All Files|*.*";
      ofd.CheckFileExists = true;
      ofd.Multiselect = false;

      if (ofd.ShowDialog() == DialogResult.OK) {
        return ofd.FileName;
      }
      return base.EditValue(context, provider, value);
    }
  }


}
