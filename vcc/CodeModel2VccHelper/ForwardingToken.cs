//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc
{
  public class ForwardingToken : Token
  {
    public delegate string GetValue();

    public static string StringFormat(string s, object[] args)
    {
      return string.Format(s, args);
    }

    GetValue getval;
    internal Token tok;

    public ForwardingToken(Token tok, Token related, GetValue getval) {
      this.tok = tok;
      this.getval = getval;
      this.Related = related;
    }

    public ForwardingToken(Token tok, GetValue getval) 
      : this(tok, null, getval) {
    }

    public bool IsForwardedFrom(Token t)
    {
      return t == tok;
    }

    public override int Line
    {
      get { return tok.Line; }
    }

    public override int Column
    {
      get { return tok.Column; }
    }

    public override int Byte
    {
      get { return tok.Byte; }
    }

    public override string Filename
    {
      get { return tok.Filename; }
    }

    public override string Value
    {
      get { return getval(); }
    }
  }

  public class WarningSuppressingToken : ForwardingToken
  {
    int suppressCode;

    public WarningSuppressingToken(Token tok, int warning)
      : base(tok, delegate() { return tok.Value; })
    {
      suppressCode = warning;
    }

    public override bool SuppressWarning(int code)
    {
      return code == suppressCode || base.SuppressWarning(code);
    }
  }
}
