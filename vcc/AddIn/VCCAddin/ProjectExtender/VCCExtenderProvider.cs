//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System.ComponentModel;
using EnvDTE;

namespace VerifiedCCompilerAddin.ProjectExtender {
  public class VCCExtenderProvider : EnvDTE.IExtenderProvider {
    private string m_DynamicExtenderName = "VCCProjectExtender";

    public string DynamicExtenderName { get { return m_DynamicExtenderName; } }

    #region IExtenderProvider Members

    public bool CanExtend(string ExtenderCATID, string ExtenderName, object ExtendeeObject) {
      System.ComponentModel.PropertyDescriptor extendeeCATIDProp = TypeDescriptor.GetProperties(ExtendeeObject)["ExternderCATID"];
      
      if (ExtenderName == m_DynamicExtenderName && ExtenderCATID == "{EE8299CB-19B6-4F20-ABEA-E1FD9A33B683}") {
        return true;
      }
      return false;
    }

    public object GetExtender(string ExtenderCATID, string ExtenderName, object ExtendeeObject, IExtenderSite ExtenderSite, int Cookie) {
      VCCExtender extender = null;

      if (CanExtend(ExtenderCATID, ExtenderName, ExtendeeObject)) {
        extender = new VCCExtender();
        extender.Init(Cookie, ExtenderSite);
      }

      return extender;
    }

    #endregion
  }

}
