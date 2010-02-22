using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Cci;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc
{
  class CciErrorHandler
  {
    public CciErrorHandler() { }

    public CciErrorHandler(VccOptions commandLineOptions) {
      this.CommandLineOptions = commandLineOptions;
    }

    public VccOptions CommandLineOptions { get; set; }

    private readonly Dictionary<string, bool> reportedErrors = new Dictionary<string, bool>();

    public void Reset() {
      this.reportedErrors.Clear();
    }

    private bool WarningsAsErrors {
      get { return CommandLineOptions != null ? CommandLineOptions.WarningsAsErrors : false; }
    }

    private int WarningLevel {
      get { return CommandLineOptions != null ? CommandLineOptions.WarningLevel : 1; }
    }

    private bool WarningIsDisabled(long id) {
      if (CommandLineOptions == null) return false;
      return CommandLineOptions.DisabledWarnings.ContainsKey(id);
    }

    private bool RunningTestSuite {
      get { return CommandLineOptions != null ? CommandLineOptions.RunTestSuite : false; }
    }

    private bool VCLikeErrorMessages {
      get { return CommandLineOptions != null ? CommandLineOptions.VCLikeErrorMessages : false; }
    }

    private bool NoPreprocessor {
      get { return CommandLineOptions != null ? CommandLineOptions.NoPreprocessor : false; }
    }

    private bool DebugOnWarningOrError {
      get { return CommandLineOptions != null ? CommandLineOptions.DebugOnWarningOrError : false; }
    }

    public void HandleErrors(object sender, Microsoft.Cci.ErrorEventArgs args) {
      foreach (IErrorMessage error in args.Errors) {
        ISourceLocation/*?*/ sourceLocation = error.Location as ISourceLocation;
        if (sourceLocation == null) continue;
        if (this.DebugOnWarningOrError) System.Diagnostics.Debugger.Launch();
        bool isError = !error.IsWarning || WarningsAsErrors;
        if (!isError && GetWarningLevel(error.Code) > WarningLevel) continue;
        if (isError) VccCommandLineHost.ErrorCount++;
        CompositeSourceDocument/*?*/ compositeDocument = sourceLocation.SourceDocument as CompositeSourceDocument;
        if (compositeDocument != null) {
          foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sourceLocation)) {
            sourceLocation = sl;
            break;
          }
        }
        IPrimarySourceLocation/*?*/ primarySourceLocation = sourceLocation as IPrimarySourceLocation;
        if (primarySourceLocation == null) {
          Console.WriteLine(error.Message);
          continue;
        }
        string docName = primarySourceLocation.SourceDocument.Location;
        if (docName == null) docName = primarySourceLocation.SourceDocument.Name.Value;
        int startLine = primarySourceLocation.StartLine;
        int startColumn = primarySourceLocation.StartColumn;
        int endLine = primarySourceLocation.EndLine;
        int endColumn = primarySourceLocation.EndColumn;
        IncludedSourceLocation/*?*/ includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
        if (includedSourceLocation != null) {
          docName = includedSourceLocation.OriginalSourceDocumentName;
          if (docName != null) docName = docName.Replace("\\\\", "\\");
          startLine = includedSourceLocation.OriginalStartLine;
          endLine = includedSourceLocation.OriginalEndLine;
        }
        long id = error.IsWarning ? ErrorToId(error.Code) : error.Code;
        if (WarningIsDisabled(id)) return;

        StringBuilder msgBldr = new StringBuilder();
        msgBldr.AppendFormat("{0}({1},{2})", this.RunningTestSuite ? "testcase" : docName, startLine, startColumn);
        //if (commandLineOptions == null || (commandLineOptions.NoPreprocessor && !commandLineOptions.VCLikeErrorMessages))
        //  msgBldr.AppendFormat("-({0},{1})", endLine, endColumn);
        msgBldr.AppendFormat(" : {0} VC{1:0000}: {2}", isError ? "error" : "warning", this.RunningTestSuite && id < 9000 ? 0 : id, error.Message);

        string msg = msgBldr.ToString();
        if (reportedErrors.ContainsKey(msg)) return;
        Console.WriteLine(msg);
        reportedErrors[msg] = true;

        string firstErrFile = docName;
        int firstErrLine = startLine;

        foreach (ILocation relatedLocation in error.RelatedLocations) {
          ISourceLocation/*?*/ sloc = relatedLocation as ISourceLocation;
          if (sloc != null) {
            compositeDocument = sloc.SourceDocument as CompositeSourceDocument;
            if (compositeDocument != null) {
              foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sloc)) {
                sloc = sl;
                break;
              }
            }
            primarySourceLocation = sloc as IPrimarySourceLocation;
            if (primarySourceLocation == null) continue;
            docName = primarySourceLocation.SourceDocument.Location;
            if (docName == null) docName = primarySourceLocation.SourceDocument.Name.Value;
            startLine = primarySourceLocation.StartLine;
            startColumn = primarySourceLocation.StartColumn;
            endLine = primarySourceLocation.EndLine;
            endColumn = primarySourceLocation.EndColumn;
            includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
            if (includedSourceLocation != null) {
              docName = includedSourceLocation.OriginalSourceDocumentName;
              if (docName != null) docName = docName.Replace("\\\\", "\\");
              startLine = includedSourceLocation.OriginalStartLine;
              endLine = includedSourceLocation.OriginalEndLine;
            }
            if (docName != firstErrFile || firstErrLine != startLine) {
              Console.Write("{0}({1},{2})", this.RunningTestSuite ? "testcase" : docName, startLine, startColumn);
              if (this.NoPreprocessor && !this.VCLikeErrorMessages)
                Console.Write("-({0},{1})", endLine, endColumn);
              Console.WriteLine(" : (Location of symbol related to previous {0}.)", isError ? "error" : "warning");
            }
          }
          //TODO: deal with non source locations
        }
      }
    }

    static long ErrorToId(long code) {
      switch ((Cci.Ast.Error)code) {
        case Cci.Ast.Error.ExpressionStatementHasNoSideEffect:
          return 9001;
      }

      switch ((Vcc.Error)code) {
        case Vcc.Error.DiscardedContractAtDefinition:
          return 9002;
        case Vcc.Error.SizeOfUnknown:
          return 9003;
      }

      return code;
    }

    private static int GetWarningLevel(long warningCode) {
      if (9300 <= warningCode && warningCode < 9400) return 0; // soundness warnings - cannot be suppressed
      else if (warningCode == (long)Error.PotentialPrecedenceErrorInLogicalExpression) return 2;
      else if (warningCode == (long)Cci.Ast.Error.PotentialUnintendRangeComparison) return 2;
      else return 1;
    }
  }

  class VerificationErrorHandler
  {
    /// <summary>
    /// Enumeration of error codes for verification errors
    /// </summary>
    public enum ErrorCode : long
    {
      AssertionFailed = (long)Cci.Ast.Error.ToBeDefined + 1,
      PreconditionFailed,
      PostconditionFailed,
      RelatedInformation
    };

    public VerificationErrorHandler(VccOptions commandLineOptions) {
      this.commandLineOptions = commandLineOptions;
    }

    private readonly VccOptions commandLineOptions;
    private Dictionary<IToken, List<ErrorCode>> reportedVerificationErrors = new Dictionary<IToken, List<ErrorCode>>();
    private List<string> errors = new List<string>();

    public void ResetReportedErrors() {
      reportedVerificationErrors.Clear();
    }


    public void FlushErrors() {
      if (errors.Count > 0) {
        errors.Sort(string.CompareOrdinal);
        foreach (string e in errors) Console.WriteLine(e);
        errors.Clear();
      }
    }

    public void ReportCounterexample(Counterexample ce, string message) {
      if (commandLineOptions != null && commandLineOptions.XmlFormatOutput) return;
      if (message != null) message = " (" + message + ")";
      else message = "";

      if (commandLineOptions.PrintCEVModel) {
        cevModelWriter = VC.VCGen.ErrorReporter.ModelWriter;
        cevModelWriter.WriteLine("BEGINNING_OF_ERROR");
      }

      try {
        ReturnCounterexample/*?*/ rce = ce as ReturnCounterexample;
        if (rce != null) {
          IToken tok = rce.FailingReturn.tok;
          for (int i = rce.Trace.Length - 1; i >= 0; i--) {
            foreach (Cmd c in rce.Trace[i].Cmds) {
              AssertCmd assrt = c as AssertCmd;
              if (assrt != null) {
                NAryExpr nary = assrt.Expr as NAryExpr;
                if (nary != null) {
                  FunctionCall fcall = nary.Fun as FunctionCall;
                  if (fcall != null && fcall.FunctionName == "$position_marker") {
                    tok = assrt.tok;
                  }
                }
              }
            }
          }
          ReportOutcomePostconditionFailed(rce.FailingEnsures.tok, tok, message);
        }
        AssertCounterexample/*?*/ ace = ce as AssertCounterexample;
        if (ace != null) {
          ReportOutcomeAssertFailed(ace.FailingAssert.tok,
            (ace.FailingAssert is LoopInvMaintainedAssertCmd ? "Loop body invariant" :
           ace.FailingAssert is LoopInitAssertCmd ? "Loop entry invariant" : "Assertion"),
           message
            );
        }
        CallCounterexample/*?*/ cce = ce as CallCounterexample;
        if (cce != null)
          ReportOutcomePreconditionFailed(cce.FailingCall.tok, cce.FailingRequires, message);
      } finally {
        if (commandLineOptions.PrintCEVModel) {
          cevModelWriter.WriteLine("END_OF_ERROR");
          cevModelWriter.Flush();
          cevModelWriter = null;
        }
      }
    }

    private System.IO.TextWriter cevModelWriter;
    private void WriteCevError(string msg)
    {
        if (cevModelWriter != null)
            cevModelWriter.WriteLine(msg);
    }

    private bool ReportError(IToken tok, VerificationErrorHandler.ErrorCode code, string fmt, params string[] args) {
      if (ErrorHasBeenReported(tok, code)) return false;
      string msg = string.Format("{0}({1},{2}) : error {3}: {4}.",
                                 commandLineOptions.RunTestSuite ? "testcase" : tok.filename, tok.line, tok.col,
                                 ErrorCodeToString(code), string.Format(fmt, args));
      if (commandLineOptions.RunTestSuite)
        errors.Add(msg);
      else
        Console.WriteLine(msg);

      WriteCevError(msg);

      return true;
    }

    private void ReportRelated(IToken tok, string fmt, params string[] args) {
      string msg = string.Format("{0}({1},{2}) : error {3}: (related information) {4}.",
                                 commandLineOptions.RunTestSuite ? "testcase" : tok.filename, tok.line, tok.col,
                                 ErrorCodeToString(ErrorCode.RelatedInformation),
                                 string.Format(fmt, args));
      if (commandLineOptions.RunTestSuite)
        errors[errors.Count - 1] = errors[errors.Count - 1] + "\r\n" + msg;
      else
        Console.WriteLine(msg);
    }

    private void ReportOutcomePreconditionFailed(IToken callTok, Requires req, string addComment) {
      string/*?*/ comment = req.Comment;
      IToken reqTok = req.tok;
      if (comment != null) comment = ": " + comment; else comment = "";
      comment += addComment;

      // in case of testsuite, don't print the full paths to prelude
      // also skip line numbers as they change
      bool isPrelude = reqTok.filename.EndsWith(".bpl") || reqTok.filename.EndsWith(".bpl>");
      if (isPrelude && reqTok.line > 1) {
        string line = VccCommandLineHost.StandardPreludeLines[reqTok.line - 2];
        int idx = line.IndexOf("TOKEN:");
        if (idx > 0) {
          reqTok.val = line.Substring(idx + 7);
        } else {
          line = VccCommandLineHost.StandardPreludeLines[reqTok.line - 1];
          idx = line.IndexOf("requires");
          if (idx >= 0)
            reqTok.val = line.Substring(idx + 8);
          else
            reqTok.val = line;
        }
      }
      if (commandLineOptions != null && commandLineOptions.RunTestSuite && isPrelude) {
        reqTok.filename = "VccPrelude.bpl";
        reqTok.line = 0;
        reqTok.col = 0;
      }

      string reqMsg = reqTok.val;
      ErrorCode errNo = GetErrorNumber(ref reqMsg, ErrorCode.PreconditionFailed);

      if (IsStandaloneError(errNo)) {
        ReportError(callTok, errNo, "{0} (in call '{1}'){2}", reqMsg, RemoveWhiteSpace(callTok.val), comment);
      } else {
        if (ReportError(callTok, errNo, "Call '{0}' did not verify{1}", RemoveWhiteSpace(callTok.val), comment))
          ReportRelated(reqTok, "Precondition: '{0}'", reqMsg);
      }
    }

    private void ReportOutcomeAssertFailed(IToken assertTok, string kind, string comment) {
      string msg = assertTok.val;
      ErrorCode errNo = GetErrorNumber(ref msg, ErrorCode.AssertionFailed);
      if (IsStandaloneError(errNo))
        ReportError(assertTok, errNo, "{0}{1}", msg, comment);
      else
        ReportError(assertTok, errNo, "{0}{2} '{1}' did not verify", kind, msg, comment);

      BoogieToken btok = assertTok as BoogieToken;
      if (btok != null && btok.Related != null)
        ReportRelated(btok.Related, btok.Related.val);
    }

    private void ReportOutcomePostconditionFailed(IToken ensTok, IToken retTok, string comment) {
      string msg = ensTok.val;
      ErrorCode errNo = GetErrorNumber(ref msg, ErrorCode.PostconditionFailed);
      if (retTok.line == 0) retTok = ensTok;
      if (ensTok.line == 0) ensTok = retTok;

      if (IsStandaloneError(errNo))
        ReportError(retTok, errNo, "{0}{1}", msg, comment);
      else {
        if (ReportError(retTok, errNo, "Post condition{0} '{1}' did not verify", comment, msg) && retTok.line != 0)
          ReportRelated(ensTok, "Location of post condition");
      }
    }

    public void ReportOutcomeMethodSummary(VC.VCGen.Outcome outcome, string addInfo, string methodName, double startTime, IEnumerable<string> proverWarnings) {
      if (outcome != VC.VCGen.Outcome.Correct) VccCommandLineHost.ErrorCount++;
      if (!commandLineOptions.XmlFormatOutput) {
        string result = OutcomeToDescription(outcome);
        if (addInfo != null)
          result = addInfo;
        if (commandLineOptions != null && commandLineOptions.VCLikeErrorMessages) {
          if (outcome == VC.VCGen.Outcome.Correct)
            Console.Write("vcc : Verification of {0} {1}.", methodName, result);
          else
            Console.Write("vcc : error : Verification of {0} {1}.", methodName, result);
          if (commandLineOptions.TimeStats)
            Console.Write(" [{0:0.00}]", GetTime() - startTime);
          Console.WriteLine();
        } else {
          if (commandLineOptions != null && commandLineOptions.TimeStats) {
            double t = GetTime() - startTime;
            if (commandLineOptions.RunTestSuite) {
              if (TestRunner.testrunTimeStats != null)
                TestRunner.testrunTimeStats.AppendFormat("<method name=\"{0}\" time=\"{1:0.00}\"/>\r\n", methodName, t);
            } else
              Console.Write("[{0:0.00}s] ", t);
          }
          Console.WriteLine("{0}.", result);
        }
      }

      if (!commandLineOptions.RunTestSuite) {
        foreach (var proverWarning in proverWarnings) {
          Console.WriteLine("Prover warning: {0}", proverWarning);
        }
      }
    }

    private bool ErrorHasBeenReported(IToken tok, ErrorCode code) {
      List<ErrorCode> errorsForTok;
      if (!reportedVerificationErrors.TryGetValue(tok, out errorsForTok)) {
        errorsForTok = new List<ErrorCode>();
        reportedVerificationErrors[tok] = errorsForTok;
      }

      if (errorsForTok.Contains(code)) return true;
      errorsForTok.Add(code);
      return false;
    }

    public static string RemoveWhiteSpace(string str) {
      return String.Join(" ", Array.FindAll(str.Split(new char[] { '\n', '\r', '\t', ' ' }), s => !String.IsNullOrEmpty(s)));
    }

    private static ErrorCode GetErrorNumber(ref string msg, ErrorCode def) {
      msg = RemoveWhiteSpace(msg);
      if (msg.StartsWith("#VCCERR:")) {
        int i = 8;
        while (i < msg.Length && char.IsDigit(msg[i])) i++;
        int num = int.Parse(msg.Substring(8, i - 8));
        msg = msg.Substring(i + 1);
        return (ErrorCode)num;
      } else
        return def;
    }

    // 9000-9099 - warnings mapped in this file
    // 9100-9199 - warning generated in the translator
    // 9500-9599 - errors mapped in this file
    // 9600-9699 - errors generated in translator
    private static string ErrorCodeToString(ErrorCode errCode) {
      long id;
      switch (errCode) {
        case VerificationErrorHandler.ErrorCode.AssertionFailed:
          id = 9500; break;
        case VerificationErrorHandler.ErrorCode.PostconditionFailed:
          id = 9501; break;
        case VerificationErrorHandler.ErrorCode.PreconditionFailed:
          id = 9502; break;
        case VerificationErrorHandler.ErrorCode.RelatedInformation:
          id = 9599; break;
        default:
          id = (long)errCode; break;
      }
      return "VC" + id.ToString("0000");
    }

    private static string OutcomeToDescription(VC.VCGen.Outcome outcome) {
      switch (outcome) {
        case VC.VCGen.Outcome.Correct:
          return "succeeded";
        case VC.VCGen.Outcome.Inconclusive:
          return "was inconclusive";
        case VC.VCGen.Outcome.TimedOut:
          return "timed out";
        case VC.VCGen.Outcome.Errors:
          return "failed";
        case VC.VCGen.Outcome.OutOfMemory:
          return "ran out of memory";
        default:
          return "returned an unknown result";
      }
    }

    private static bool IsStandaloneError(VerificationErrorHandler.ErrorCode num) {
      return (int)num >= 8000 && (int)num < 8500;
    }

    internal static double GetTime() {
      return System.Environment.TickCount / 1000.0;
    }
  }
}
