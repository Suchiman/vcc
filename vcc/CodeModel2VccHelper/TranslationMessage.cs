﻿using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Cci;

namespace Microsoft.Research.Vcc
{
  public class TranslationMessage : ISourceErrorMessage
  {
    private long code;
    private string msg;
    private bool isWarning;
    private ISourceLocation loc;
    private IEnumerable<ISourceLocation> relatedLocs;

    public TranslationMessage(ISourceLocation loc, int code, string msg, bool isWarning)
    {
      this.loc = loc;
      this.code = code;
      this.msg = msg;
      this.isWarning = isWarning;
    }

    public TranslationMessage(ISourceLocation loc, int code, string msg, bool isWarning, IEnumerable<ISourceLocation> relatedLocs)
      : this(loc, code, msg, isWarning)
    {
      this.relatedLocs = relatedLocs;
    }

    #region ISourceErrorMessage Members

    public ISourceLocation SourceLocation
    {
      get { return loc; }
    }

    public ISourceErrorMessage MakeShallowCopy(ISourceDocument targetDocument)
    {
      return new TranslationMessage(loc, (int)code, msg, isWarning);
    }

    #endregion

    #region IErrorMessage Members

    public object ErrorReporter
    {
      get { return this; }
    }

    public string ErrorReporterIdentifier
    {
      get { return "Vcc"; }
    }

    public long Code
    {
      get { return code; }
    }

    public bool IsWarning
    {
      get { return isWarning; }
    }

    public string Message
    {
      get { return msg; }
    }

    public ILocation Location
    {
      get { return loc; }
    }

    public IEnumerable<ILocation> RelatedLocations
    {
      get {
        if (this.relatedLocs == null) yield return loc;
        else
        {
          foreach (ILocation rLoc in this.relatedLocs)
            yield return rLoc;
        }
      }
    }

    #endregion
  }
}
