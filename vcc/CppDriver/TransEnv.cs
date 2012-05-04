using System;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc.Cpp
{
    class TransEnv : TransHelper.TransEnv, IErrorSink
    {
        public event EventHandler<ErrorReportedEventArgs> ErrorReportedEvent;

        private bool errorReported;
        private readonly VccppOptions options;

        public override bool ErrorReported
        {
            get { return this.errorReported; }
        }

        public override bool ShouldDumpStack
        {
            get { return true; }
        }

        public override int PointerSizeInBytes
        {
            get { return this.options.PointerSize / 8; }
        }

        public VccppOptions VccppOptions
        {
          get { return this.options; }
        }

        public TransEnv(VccppOptions vccppOptions)
            : base(vccppOptions)
        {
            this.options = vccppOptions;
        }

        public override void Error(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
        {
            errorReported = true;
            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, false, tok.Line, tok.Column, code, msg, false)));
            }

            Utils.Log(String.Format("{0}({1},{2}): error VC{3:0000}: {4}", tok.Filename, tok.Line, tok.Column, code, msg));

            this.ReportRelated(false, related);

        }

        public override void Oops(Token tok, string msg)
        {
            if (!errorReported)
            {
                EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
                if (temp != null)
                {
                    temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, false, tok.Line, tok.Column, -1, msg, false)));
                }

                Utils.Log(String.Format("{0}({1},{2}): oops: {3}", tok.Filename, tok.Line, tok.Column, msg));
            }
        }

        public override void Warning(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
        {
            if (tok.SuppressWarning(code)) return;

            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, true, tok.Line, tok.Column, code, msg, false)));
            }

            Utils.Log(String.Format("{0}({1},{2}): warning VC{3:0000}: {4}", tok.Filename, tok.Line, tok.Column, code, msg));

            this.ReportRelated(true, related);

        }

        private void ReportRelated(bool isWarning, FSharp.Core.FSharpOption<Token> related)
        {
          if (FSharp.Core.FSharpOption<Token>.get_IsNone(related)) return;

          var tok = related.Value;           
          EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
          if (temp != null)
          {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, isWarning, tok.Line, tok.Column, 0, "", true)));          
          }
          
          Utils.Log(String.Format("{0}({1},{2}): {3} : (Location of symbol related to previous {3}.)", tok.Filename, tok.Line, tok.Column, isWarning ? "warning" : "error"));
        }

        public void Error(IToken tok, string msg)
        {
            errorReported = true;
            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.filename, false, tok.line, tok.col, -2, msg, false)));
            }

            Utils.Log(String.Format("{0}({1},{2}): Boogie error: {3}", tok.filename, tok.line, tok.col, msg));
        }
    }
}
