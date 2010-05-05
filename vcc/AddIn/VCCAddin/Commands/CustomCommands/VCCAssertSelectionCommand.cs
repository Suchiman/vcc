//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  class VCCAssertSelectionCommand : VCCCommand {

    public VCCAssertSelectionCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCAssertSelection", "Asser&t selection to clipboard", "Assert selection to clipboard", (int)VCCMenuIcons.AssertSelection, VCCBindings.AssertSelection, CommandBarName.VCCMoreCommands) { 
    }

    private string getAssertString(string Code) {
      //Parse string!
      int open = 0;
      int close = 0;
      int last = 0;
                 
      StringBuilder toClip = new StringBuilder();

      for (int i = 0; i < Code.Length; i++) {
        switch (Code[i]) {
          case '/': if (i + 1 < Code.Length) {
                      if (Code[i + 1] == '/') {
                        //Found a // Comment
                        //Find end of line... and replace all with space to end
                        int posEnd = Code.IndexOf(Environment.NewLine, i);
                        for (int j = i; j < posEnd; j++) {
                          Code = Code.Insert(j, " ").Remove(j+1,1); 
                        }
                      }
                      if (Code[i + 1] == '*') {
                        //Found a /* Comment
                        //Find end of comment...
                        int posEnd = Code.IndexOf("*/", i) + 2;
                        for (int j = i; j < posEnd; j++) {
                          Code = Code.Insert(j, " ").Remove(j + 1, 1);
                        }
                      }
                    }
                    break;
          case '(': 
                    open++; 
                    break;
          case ')':
                    close++;
                    break;
          case '&':
                    if (i + 1 < Code.Length) {
                      if (Code[i + 1] == '&') {
                        // Found a double &&, Split Candidate!
                        if (open == close) {
                          //We can split here!
                          string Line = Code.Substring(last,i-last);
                          //Remove any LineBreak!
                          Line = Line.Replace(Environment.NewLine, "");
                          toClip.AppendFormat("assert({0});{1}", Line.Trim(), Environment.NewLine);
                          last = i+2;
                        }
                      }
                    }
                    break;
          default:
            break;
        }
      }

      if (last < Code.Length) {
        string Line = Code.Substring(last, Code.Length - last);
        //Remove any LineBreak!
        Line = Line.Replace(Environment.NewLine, "");
        toClip.AppendFormat("assert({0});{1}", Line.Trim(), Environment.NewLine);
      }

      return toClip.ToString();
    }

    private void CopySelectionAsAssertToClipboard()
    {
        try {

          String Text = AddInGlobals.ActiveDocument.getSelectedText();

        string Clip = getAssertString(Text);
        Clipboard.SetText(Clip);
        
        return ;
      }
      catch {
        return ;
      }

    }

    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      CopySelectionAsAssertToClipboard();
       return true;
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsCodeFile && !VerifyManager.isRunning)
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      else
        status = vsCommandStatus.vsCommandStatusInvisible;
    }
  }
}
