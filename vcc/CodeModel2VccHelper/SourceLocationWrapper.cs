//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using Microsoft.Cci;

namespace Microsoft.Research.Vcc
{
  internal sealed class SourceLocationWrapper : Token, IPrimarySourceLocation
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

    public int EndLine
    {
      get
      {
        IIncludedSourceLocation/*?*/ iloc = this.sourceLocation as IIncludedSourceLocation;
        if (iloc != null) return iloc.OriginalEndLine;
        return this.sourceLocation.EndLine;
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


    #region IPrimarySourceLocation Members

    int IPrimarySourceLocation.EndColumn
    {
      get { return this.sourceLocation.EndColumn; }
    }

    int IPrimarySourceLocation.EndLine
    {
      get { 
        var iloc = this.sourceLocation as IIncludedSourceLocation;
        return iloc != null ? iloc.OriginalEndLine : this.sourceLocation.EndLine;
      }
    }

    IPrimarySourceDocument IPrimarySourceLocation.PrimarySourceDocument
    {
      get
      {
        var iloc = this.sourceLocation as IIncludedSourceLocation;
        return iloc != null ? iloc.SourceDocument as IPrimarySourceDocument : this.sourceLocation.SourceDocument as IPrimarySourceDocument;
      }
    }

    int IPrimarySourceLocation.StartColumn
    {
      get { return this.sourceLocation.StartColumn; }
    }

    int IPrimarySourceLocation.StartLine
    {
      get
      {
        var iloc = this.sourceLocation as IIncludedSourceLocation;
        return iloc != null ? iloc.OriginalStartLine : this.sourceLocation.StartLine;
      }
    }

    #endregion

    #region ISourceLocation Members

    bool ISourceLocation.Contains(ISourceLocation location)
    {
      throw new System.NotImplementedException();
    }

    int ISourceLocation.CopyTo(int offset, char[] destination, int destinationOffset, int length)
    {
      throw new System.NotImplementedException();
    }

    int ISourceLocation.EndIndex
    {
      get { throw new System.NotImplementedException(); }
    }

    int ISourceLocation.Length
    {
      get { throw new System.NotImplementedException(); }
    }

    ISourceDocument ISourceLocation.SourceDocument
    {
      get { throw new System.NotImplementedException(); }
    }

    string ISourceLocation.Source
    {
      get { throw new System.NotImplementedException(); }
    }

    int ISourceLocation.StartIndex
    {
      get { throw new System.NotImplementedException(); }
    }

    #endregion

    #region ILocation Members

    IDocument ILocation.Document
    {
      get { throw new System.NotImplementedException(); }
    }

    #endregion
  }
}
