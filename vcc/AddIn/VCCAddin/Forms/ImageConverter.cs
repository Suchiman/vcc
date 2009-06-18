//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Drawing;

namespace VerifiedCCompilerAddin.Forms {
  //This class is a simple "trick" to get an ressource bitmap as an stdole.StdPicture.
  //Needed for e.g. the VCCPane SetTabIcon
  public class ImageConverter : AxHost {
    public ImageConverter()
      : base("B8C5D611-C31C-4086-B138-8CAB23043CAE") {
    }

    public static stdole.StdPicture ImageToIPicture(Image image) {
      return (stdole.StdPicture)AxHost.GetIPictureDispFromPicture(image);
    }

  }
}


