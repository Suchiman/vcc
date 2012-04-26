using System;
using Microsoft.Boogie;
using System.Collections.Generic;

namespace Microsoft.Research.Vcc.Cpp
{
  class VerificationErrorReporter : VerifierCallback
  {
    private string currentFunction = "<unknown>";
    private bool errorReported;
    private HashSet<IToken> reportedCallFailures = new HashSet<IToken>();

    public event EventHandler<ErrorReportedEventArgs> ErrorReported;
    public event EventHandler<VerificationFinishedEventArgs> VerificationFinished;
    
    public bool AnyErrorReported { get; private set; }

    public override void OnCounterexample(Counterexample ce, string reason)
    {
      if (!errorReported)
      {
        OnVerificationFinished(currentFunction, "failed");
        Utils.Log(String.Format("Verification of '{0}' failed.", this.currentFunction));
      }

      this.ReportCounterexample(ce, reason);
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnOutOfMemory(string reason)
    {
      OnVerificationFinished(currentFunction, "ran out of memory");
      Utils.Log(String.Format("Verification of '{0}' ran out of memory: {1}", this.currentFunction, reason));
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnTimeout(string reason)
    {
      OnVerificationFinished(currentFunction, "timed out");
      Utils.Log(String.Format("Verification of '{0}' timed out : {1}", this.currentFunction, reason));
      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public override void OnWarning(string msg)
    {
      OnVerificationFinished(currentFunction, "warning");
      Utils.Log(String.Format("Verification of '{0}' gave warning: {1}", this.currentFunction, msg));
    }

    public override void OnUnreachableCode(Implementation impl)
    {

      if (HasRequiresFalse(impl)) return;

      bool hasIFUnreachable = false;
      var unreachableMasters = new Microsoft.FSharp.Collections.FSharpSet<IdentifierExpr>(new List<IdentifierExpr>());
      var unreachableChildren = new Microsoft.FSharp.Collections.FSharpSet<IdentifierExpr>(new List<IdentifierExpr>());
      for (int i = impl.Blocks.Count - 1; i >= 0; i--)
      {
          Block b = impl.Blocks[i];
          if (HasAssertFalse(b))
          {
              foreach (var cmd in b.Cmds)
              {
                  PredicateCmd pred = cmd as PredicateCmd;
                  if (pred != null)
                  {
                      NAryExpr nary = pred.Expr as NAryExpr;
                      if (nary != null)
                      {
                          FunctionCall f = nary.Fun as FunctionCall;
                          if (f != null && f.Func.Name == "$expect_unreachable_master")
                          {
                              unreachableMasters = unreachableMasters.Add(f.Func.InParams.Last() as IdentifierExpr);
                              hasIFUnreachable = true;
                          }
                          else if (f != null && f.Func.Name == "$expect_unreachable_child")
                          {
                              unreachableChildren = unreachableChildren.Add(f.Func.InParams.Last() as IdentifierExpr);
                              hasIFUnreachable = true;
                          }
                      }
                  }
              }
          }
      }

      bool hasRealUnreachable = false;
      foreach (var id in unreachableChildren)
      {
          if (!unreachableMasters.Contains(id))
            hasRealUnreachable = true;
      }

      if (!hasRealUnreachable && hasIFUnreachable)
      {
        this.EndFunction();
        return;
      }

      var traceTokens = new List<IToken>();

      for (int i = impl.Blocks.Count - 1; i >= 0; i--) {
        Block b = impl.Blocks[i];
        foreach (var cmd in b.Cmds) {
          PredicateCmd pred = cmd as PredicateCmd;
          if (pred != null) {
            NAryExpr nary = pred.Expr as NAryExpr;
            if (nary != null) {
              FunctionCall f = nary.Fun as FunctionCall;
              if (f != null && f.Func.Name == "$expect_unreachable") return;   // Just restoring what existed. This is keeping some potentially easy to let through soundness warnings...
            }
          }
        }

        if (!IsTokenWithoutLocation(b.TransferCmd.tok))
          traceTokens.Add(b.TransferCmd.tok);
        else {
          for (int j = b.Cmds.Length - 1; j >= 0; j--)
          {
            if (!IsTokenWithoutLocation(b.Cmds[j].tok))
            {
              traceTokens.Add(b.Cmds[j].tok);
              break;
            }
          }
        }
      }


      OnVerificationFinished(currentFunction, "succeeded");
      Utils.Log(String.Format("Verification of '{0}' succeeded.", this.currentFunction));
      this.ReportUnreachable(traceTokens);

      this.AnyErrorReported = true;
      this.errorReported = true;
    }

    public void StartFunction(string functionName)
    {
      this.errorReported = false;
      this.currentFunction = functionName;
    }

    public bool EndFunction()
    {
      if (!this.errorReported) {
        OnVerificationFinished(currentFunction, "succeeded");
        Utils.Log(String.Format("Verification of '{0}' succeeded.", this.currentFunction));
        return true;
      }

      this.errorReported = false;
      return false;
    }

    private void OnVerificationFinished(string name, string outcome)
    {
      EventHandler<VerificationFinishedEventArgs> temp = VerificationFinished;
      if (temp != null)
      {
        temp(this, new VerificationFinishedEventArgs(name, outcome));
      }
    }

    private void ReportError(IToken tok, int errno, bool isWarning, string fmt, params object[] args)
    {
      var msg = args.Length > 0 ? String.Format(fmt, args) : fmt;
      
      EventHandler<ErrorReportedEventArgs> temp = ErrorReported;
      if (temp != null)
      {
        temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.filename, isWarning, tok.line, tok.col, errno, msg, false)));
      }

      Utils.Log(String.Format("{0}({1},{2}): {3} VC{4:0000}: {5}", tok.filename, tok.line, tok.col, (isWarning ? "warning" : "error"), errno, msg));
    }

    private void ReportUnreachable(IList<IToken> traceTokens)
    {
      if (traceTokens.Count == 0)
      {
        ReportError(new BoogieToken.Token(Token.NoToken), 9100, true, "Found unreachable code, but cannot figure out where it is.");
      }
      else
      {
        ReportError(traceTokens[0], 9100, true, "found unreachable code, possible soundness violation, please check the axioms or add an explicit assert(false)");

        var prevFile = traceTokens[0].filename;
        var prevLine = traceTokens[0].line;
        var prevCol = traceTokens[0].col;

        for (int i = traceTokens.Count - 1; i > 0; i--)
        {
          if (traceTokens[i].col == prevCol && traceTokens[i].line == prevLine && traceTokens[i].filename == prevFile)
            continue;

          ReportError(traceTokens[i], 9101, true, "trace to unreachable location");

          prevFile = traceTokens[i].filename;
          prevLine = traceTokens[i].line;
          prevCol = traceTokens[i].col;
        }
      }
    }

    private void ReportCounterexample(Counterexample ce, string message)
    {
      if (message != null) message = " (" + message + ")";
      else message = "";

      var rce = ce as ReturnCounterexample;
      if (rce != null)
      {
        IToken tok = rce.FailingReturn.tok;
        for (int i = rce.Trace.Length - 1; i >= 0; i--)
        {
          foreach (Cmd c in rce.Trace[i].Cmds)
          {
            var assrt = c as AssertCmd;
            if (assrt != null)
            {
              var nary = assrt.Expr as NAryExpr;
              if (nary != null)
              {
                var fcall = nary.Fun as FunctionCall;
                if (fcall != null && fcall.FunctionName == "$position_marker")
                {
                  tok = assrt.tok;
                }
              }
            }
          }
        }

        ReportOutcomePostconditionFailed(rce.FailingEnsures.tok, tok, message);
        return;
      }

      var ace = ce as AssertCounterexample;
      if (ace != null)
      {
        ReportOutcomeAssertFailed(ace.FailingAssert.tok,
                                  (ace.FailingAssert is LoopInvMaintainedAssertCmd
                                     ? "Loop body invariant"
                                     : ace.FailingAssert is LoopInitAssertCmd ? "Loop entry invariant" : "Assertion"),
                                  message);
        return;
      }

      var cce = ce as CallCounterexample;
      if (cce != null)
      {
        ReportOutcomePreconditionFailed(cce.FailingCall.tok, cce.FailingRequires, message);
        return;
      }

      System.Diagnostics.Debug.Assert(false, "Unexpected counter example type");
    }

    private void ReportOutcomeAssertFailed(IToken assertTok, string kind, string comment)
    {
      var errnoAndMsg = GetErrorNumber(assertTok.val, 9500);
      ReportError(assertTok, errnoAndMsg.Item1, false, "{0}{2} '{1}' did not verify.", kind, errnoAndMsg.Item2, comment);
      ReportAllRelated(assertTok);
    }

    private void ReportOutcomePreconditionFailed(IToken callTok, Requires req, string addComment)
    {
      string /*?*/ comment = req.Comment;
      IToken reqTok = req.tok;
      if (comment != null) comment = ": " + comment;
      else comment = "";
      comment += addComment;

      string reqMsg = reqTok.val;

      if (this.reportedCallFailures.Add(callTok))
      {
        ReportError(callTok, 9502, false,  "Call '{0}' did not verify{1}.", RemoveWhiteSpace(callTok.val), comment);
      }

      ReportRelated(reqTok, "Precondition: '{0}'.", reqMsg);
      ReportAllRelated(reqTok);      
    }

    private void ReportOutcomePostconditionFailed(IToken ensTok, IToken retTok, string comment)
    {
      string msg = ensTok.val;
      if (retTok.line == 0) retTok = ensTok;
      if (ensTok.line == 0) ensTok = retTok;

      var errnoAndMsg = GetErrorNumber(msg, -1);

      if (errnoAndMsg.Item1 == -1)
      {
        ReportError(retTok, 9501, false, "Post condition{0} '{1}' did not verify.", comment, msg);
        ReportRelated(ensTok, "Location of post condition.");
        ReportAllRelated(ensTok);
      }
      else
      {
        ReportError(retTok, errnoAndMsg.Item1, false, errnoAndMsg.Item2);
      }      
    }

    private void ReportAllRelated(IToken tok)
    {
      var btok = tok as BoogieToken.Token;

      while (btok != null && btok.Related != null) {
        ReportRelated(btok.Related.Value, ((IToken)btok.Related.Value).val + ".");
        btok = btok.Related.Value;
      }
    }

    private void ReportRelated(IToken tok, string fmt, params string[] args)
    {
      ReportError(tok, 9599, false, "(related information) " + fmt, args);
    }

    public static string RemoveWhiteSpace(string str)
    {
      return String.Join(" ", Array.FindAll(str.Split(new[] { '\n', '\r', '\t', ' ' }), s => !String.IsNullOrEmpty(s)));
    }

    private static Tuple<int, string> GetErrorNumber(string msg, int _default)
    {
      msg = RemoveWhiteSpace(msg);
      if (msg.StartsWith("#VCCERR:")) {
        int i = 8;
        while (i < msg.Length && char.IsDigit(msg[i])) i++;
        int num = int.Parse(msg.Substring(8, i - 8));
        msg = msg.Substring(i + 1);
        return Tuple.Create(num, msg);
      } 
        
      return Tuple.Create(_default, msg);
    }

    private static bool HasRequiresFalse(Implementation impl)
    {
      foreach (Requires req in impl.Proc.Requires)
      {
        LiteralExpr f = req.Condition as LiteralExpr;
        if (f != null && f.IsFalse) return true;
      }

      return false;
    }

    private static bool HasAssertFalse(Block b)
    {
      foreach (var cmd in b.Cmds)
      {
        PredicateCmd pred = cmd as PredicateCmd;
        if (pred != null)
        {
          LiteralExpr f = pred.Expr as LiteralExpr;
          if (f != null && f.IsFalse) return true;
        }
      }
      return false;
    }

    private static bool IsTokenWithoutLocation(IToken t)
    {
      return String.IsNullOrEmpty(t.filename) || t.filename == "<no file>";
    }
  }
}
