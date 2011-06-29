//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.ComponentModel;
using System.Text;
using VerifiedCCompilerAddin.ProjectExtender.Options;

namespace VerifiedCCompilerAddin.ProjectExtender.TypeConverter {
  
  public class VCCMasterFileConverter : ExpandableObjectConverter  {
    
    public override bool CanConvertTo(ITypeDescriptorContext context, Type destinationType) {

      if (destinationType == typeof(MasterFileOptions))
        return true;

      return base.CanConvertTo(context, destinationType);
    }

    public override object ConvertTo(ITypeDescriptorContext context, System.Globalization.CultureInfo culture, object value, Type destinationType) {

      if (destinationType == typeof(System.String) && value is MasterFileOptions) {
        MasterFileOptions mfo = (MasterFileOptions)value;

        StringBuilder sb = new StringBuilder();
        sb.AppendFormat("{0}", mfo.Active ? mfo.Filename: "");
        return sb.ToString();
      }

      return base.ConvertTo(context, culture, value, destinationType);
    }
  }
  
}
