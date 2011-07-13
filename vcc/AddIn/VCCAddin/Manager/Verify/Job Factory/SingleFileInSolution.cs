//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using EnvDTE;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class SingleFileInSolution : VerifyJob {
    public SingleFileInSolution() {
      SelectedItem sitem = AddInGlobals.DTE.SelectedItems.Item(1);
      ProjectItem pi = sitem.ProjectItem;

      VCCSettings sets = new VCCSettings(pi,
                                         Utilities.GetActiveConfigOfProject(pi.ContainingProject));

      this.Settings = sets;
      this.FullFileName = (string)pi.Properties.Item("FullPath").Value;
      this.Platform = Utilities.GetActivePlatformID(pi);

      // #warning Is not correct code here - Masterfile detection, from false project possible!
      if (AddinSettingsManager.UseMasterFile && FullFileName.ToLower().EndsWith(".h")) {
        this.FullFileName = Utilities.getMasterFileName();
      }

    }
  }
}
