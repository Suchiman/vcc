using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Research.Vcc
{
  public class BoogieToken : Microsoft.Boogie.IToken
  {
    Token tok;
    Microsoft.Boogie.IToken related;

    public static Token Strip(Microsoft.Boogie.IToken t)
    {
      BoogieToken b = t as BoogieToken;
      if (b != null) return b.tok;
      return Token.NoToken;
    }

    public BoogieToken(Token tok) :
      this(tok, null) {
    }

    public BoogieToken(Token tok, Token related) {
      this.tok = tok;
      this.related = related == null ? null : new BoogieToken(related, null);
    }

    public Microsoft.Boogie.IToken Related {
      get { return this.related; }
    }

    #region IToken Members

    public bool IsValid
    {
      get { return true; }
    }

    public int col
    {
      get
      {
        return tok.Column;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public string filename
    {
      get
      {
        return tok.Filename;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public int kind
    {
      get
      {
        return 0;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public int line
    {
      get
      {
        return tok.Line;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public int pos
    {
      get
      {
        return tok.Byte;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public string val
    {
      get
      {
        return tok.Value;
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    #endregion
  }
}
