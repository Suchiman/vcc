using System;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc.Cpp
{
    class TransEnv : TransHelper.TransEnv, IErrorSink
    {
        public TransEnv(string[] pipeOperations, VccppOptions vccppOptions)
            : base(new TransOptions(pipeOperations, vccppOptions))
        {
        }

        public event EventHandler<ErrorReportedEventArgs> ErrorReportedEvent;

        private bool errorReported;

        public override bool ErrorReported
        {
            get { return this.errorReported; }
        }

        public override bool ShouldDumpStack
        {
            get { return true; }
        }

        public override void Error(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
        {
            errorReported = true;
            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, false, tok.Line, tok.Column, code, msg)));
            }

            Utils.Log(String.Format("{0}({1},{2}): error VC{3:0000}: {4}", tok.Filename, tok.Line, tok.Column, code, msg));
        }

        public override void Oops(Token tok, string msg)
        {
            if (!errorReported)
            {
                EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
                if (temp != null)
                {
                    temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, false, tok.Line, tok.Column, -1, msg)));
                }

                Utils.Log(String.Format("{0}({1},{2}): oops: {3}", tok.Filename, tok.Line, tok.Column, msg));
            }
        }

        public override int PointerSizeInBytes
        {
            get { return 4; }
        }

        public override void Warning(Token tok, int code, string msg, FSharp.Core.FSharpOption<Token> related)
        {
            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.Filename, true, tok.Line, tok.Column, code, msg)));
            }

            Utils.Log(String.Format("{0}({1},{2}): warning VC{3:0000}: {4}", tok.Filename, tok.Line, tok.Column, code, msg));
        }

        public void Error(IToken tok, string msg)
        {
            errorReported = true;
            EventHandler<ErrorReportedEventArgs> temp = ErrorReportedEvent;
            if (temp != null)
            {
                temp(this, new ErrorReportedEventArgs(new ErrorDetails(tok.filename, false, tok.line, tok.col, -2, msg)));
            }

            Utils.Log(String.Format("{0}({1},{2}): Boogie error: {3}", tok.filename, tok.line, tok.col, msg));
        }
    }
}
