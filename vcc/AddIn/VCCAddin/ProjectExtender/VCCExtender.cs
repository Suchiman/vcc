//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using EnvDTE;
using System.Drawing;
using System.ComponentModel.Design;
using System.Runtime.Serialization;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using VerifiedCCompilerAddin.ProjectExtender.Options;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.ProjectExtender {


  public class VCCExtender : IFilterProperties {
    private IExtenderSite extenderSite { get; set; }
    private int siteCookie { get; set; }
    
    public void Init(int ExtenderCookie, EnvDTE.IExtenderSite ExtenderSite) {
      siteCookie = ExtenderCookie;
      extenderSite = ExtenderSite;
    }

    private VCCOptions vccoptions;
    
    [DisplayName("VCC Options")]
    public VCCOptions VCCOptions { get { return vccoptions; } set { vccoptions = value; } }

    #region IMyDynamicExtender Members

    public VCCExtender() {
      vccoptions = new VCCOptions(Utilities.ActiveProject());
    }
    
    private string m_Desc = String.Empty;

    
    
    #endregion

    #region IFilterProperties Members


    public vsFilterProperties IsPropertyHidden(string PropertyName) {
      if (PropertyName == "FileType") {
        return vsFilterProperties.vsFilterPropertiesAll;
      } else {
        return vsFilterProperties.vsFilterPropertiesNone;
      }
    }

    #endregion

  }

}
