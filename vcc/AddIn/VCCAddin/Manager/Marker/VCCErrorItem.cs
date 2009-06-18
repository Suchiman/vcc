//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;

namespace VerifiedCCompilerAddin.Manager.Marker {
  public class VCCErrorItem {
    public int Line;
    public int Pos;
    public string Type;
    public string FileName;
    public string Message;

    public VCCErrorItem() { }

    public VCCErrorItem(int Line, int Pos, string Type, string FileName, string Message) {
      this.Line = Line;
      this.Pos = Pos;
      this.FileName = FileName;
      this.Message = Message;
      this.Type = Type;
    }
  }
}
