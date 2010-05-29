//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc
{
  public abstract class Token
  {
    abstract public int Line { get; }
    abstract public int Column { get; }
    abstract public int Byte { get; }
    abstract public string Filename { get; }
    abstract public string Value { get; }

    public virtual bool SuppressWarning(int code)
    {
      return false;
    }

    public static Token NoToken
    {
      get { return DummyToken.Instance; }
    }

    public Token Related { get; protected set; }
  }

  public class DummyToken : Token
  {
    static DummyToken inst;

    private DummyToken() { }

    public static DummyToken Instance
    {
      get
      {
        if (inst == null) inst = new DummyToken();
        return inst;
      }
    }

    public override int Line
    {
      get { return 0; }
    }

    public override int Column
    {
      get { return 0; }
    }

    public override int Byte
    {
      get { return 0; }
    }

    public override string Filename
    {
      get { return "no_file"; }
    }

    public override string Value
    {
      get { return ""; }
    }
  }

  public class LazyToken : Token
  {

    public delegate Token GetTokenDelegate();

    private readonly GetTokenDelegate getToken;

    public LazyToken(GetTokenDelegate getToken) {
      this.getToken = getToken;
    }

    private Token token;
    public Token DelayedToken {
      get {
        if (this.token == null) {
          this.token = this.getToken();
        }
        return this.token;
      }
    }

    public override int Line {
      get { return this.DelayedToken.Line; }
    }

    public override int Column {
      get { return this.DelayedToken.Column; }
    }

    public override int Byte {
      get { return this.DelayedToken.Byte; }
    }

    public override string Filename {
      get { return this.DelayedToken.Filename; }
    }

    public override string Value {
      get { return this.DelayedToken.Value; }
    }
  }
}
