//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using VerifiedCCompilerAddin.ProjectExtender.Options;
using System.IO;

namespace VerifiedCCompilerAddin.ProjectExtender.TypeConverter {
  
  public class VCCOptionsConverter : ExpandableObjectConverter {
    public override bool CanConvertTo(ITypeDescriptorContext context, Type destinationType) {

      if (destinationType == typeof(VCCOptions))
        return true;

      return base.CanConvertTo(context, destinationType);
    }

    public override object ConvertTo(ITypeDescriptorContext context, System.Globalization.CultureInfo culture, object value, Type destinationType) {

      if (destinationType == typeof(System.String) && value is VCCOptions) {
        VCCOptions vo = (VCCOptions)value;
        StringBuilder Info = new StringBuilder();
        if (vo.AdditionalCommandLineArguments.Length > 0)
          Info.AppendFormat("{0}{1}", Info.Length >0 ? " ": "",vo.AdditionalCommandLineArguments);
        if (vo.MasterFileOptions.Active)
          Info.AppendFormat("{0}{1}", Info.Length >0 ? " ": "", Path.GetFileName(vo.MasterFileOptions.Filename));
        
        return Info.ToString();
      }

      return base.ConvertTo(context, culture, value, destinationType);
    }
  }
}
