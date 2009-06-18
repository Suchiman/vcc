//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using Microsoft.Cci;

namespace Microsoft.Research.Vcc
{
  internal sealed class SourceLocationWrapper : Token
  {
    internal SourceLocationWrapper(IPrimarySourceLocation sourceLocation)
    {
      this.sourceLocation = sourceLocation;
    }

    internal IPrimarySourceLocation sourceLocation;

    public override int Column
    {
      get
      {
        return this.sourceLocation.StartColumn;
      }
    }

    public override string Filename
    {
      get
      {
        IIncludedSourceLocation/*?*/ iloc = this.sourceLocation as IIncludedSourceLocation;
        if (iloc != null) return iloc.OriginalSourceDocumentName.Replace("\\\\", "\\");
        return this.sourceLocation.PrimarySourceDocument.Name.Value;
      }
    }

    public override int Line
    {
      get
      {
        IIncludedSourceLocation/*?*/ iloc = this.sourceLocation as IIncludedSourceLocation;
        if (iloc != null) return iloc.OriginalStartLine;
        return this.sourceLocation.StartLine;
      }
    }

    public override int Byte
    {
      get { return this.sourceLocation.StartIndex; }
    }

    public override string Value
    {
      get
      {
        if (this._val == null)
          this._val = this.sourceLocation.Source;
        return this._val;
      }
    }
    string _val;

  }
}
