using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Z3Model.Parser {
  public enum Tokentype {
    CBO,          // {
    CBC,          // }
    RESULTS,      // ->
    COLON,        // :
    SPACE,        // SPACE
    NEWLINE,      // NEWLINE \n
    LINEFEED,     // LINEFEED \r
    STRING        // OTHER TOKENS
  }


  public class Token
  {
    string _content;
    Tokentype _type;

    public Token (string Content, Tokentype Type){
      _content = Content;
      _type =Type;
    }

    public string Content {
      get { return _content; }
    }

    public Tokentype Type{
      get { return _type; }
    }

  }



  public class Tokenizer : IEnumerable , IEnumerator {
    string Data;
    List<Token> Tokenlist;

    public Token this[int i] {
      get { return Tokenlist[i]; }
    }

    public Tokenizer() {
      Tokenlist = new List<Token>();
    }
    
    public Tokenizer(string StringToParse)
      : this() {
      Data = StringToParse;
      CreateTokenList();
    }

    public List<Token> getTokenList() {
      return Tokenlist;
    }

    private void CreateTokenList() {
      CreateSubTokens(Data);
    }

    private void CreateSubTokens(string p) {
      string s = String.Empty;
      for (int i = 0; i < p.Length; i++) {
        switch (p[i]) {
          case '{':
            s = AddStringToken(s);
            AddNewTokenCBO("{");
            break;
          case '}':
            s = AddStringToken(s);
            AddNewTokenCBC("}");
            break;
          case ':':
            s = AddStringToken(s);
            AddNewTokenDoublePoint(":");
            break;
          case ' ':
            s = AddStringToken(s);
            AddNewTokenSpace(" ");
            break;
          case '\r':
            s = AddStringToken(s);
            AddNewTokenLineFeed("\r");
            break;
          case '\n':
            AddNewTokenNewLine("\n");
            break;
          default:
            s += p[i];
            break;
        }
      }
      AddStringToken(s);
    }

    private string AddStringToken(string s) {
      if (!String.IsNullOrEmpty(s)) {
        if (s == "->")
          AddNewTokenResults("->");
        else
         Tokenlist.Add(new Token(s, Tokentype.STRING));
        s = String.Empty;
      }
      return s;
    }
    
    private void AddNewTokenResults(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.RESULTS));
    }
    
    private void AddNewTokenNewLine(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.NEWLINE));
    }

    private void AddNewTokenLineFeed(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.LINEFEED));
    }

    private void AddNewTokenDoublePoint(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.COLON));
    }

    private void AddNewTokenSpace(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.SPACE));
    }

    private void AddNewTokenCBC(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.CBC));
    }

    private void AddNewTokenCBO(string raw) {
      Tokenlist.Add(new Token(raw, Tokentype.CBO));
    }





    public IEnumerator GetEnumerator() {
      return Tokenlist.GetEnumerator();
    }

    #region IEnumerator Members
    private int _pos;

    public object Current {
      get { return Tokenlist[_pos]; }
    }

    public Token CurrentToken {
      get { return Tokenlist[_pos]; }
    }

    public Token Next {
      get { MoveNext();  return Tokenlist[_pos]; }
    }

    public bool MoveNext()
    {
      if (_pos < Tokenlist.Count - 1) {
        _pos++;
        return true;
      }
      return false;
    }

    public void Reset() {
      _pos = 0;
    }

    #endregion
  }
}
