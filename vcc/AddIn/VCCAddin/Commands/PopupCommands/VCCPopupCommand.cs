//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using Microsoft.VisualStudio.CommandBars;

namespace VerifiedCCompilerAddin.Commands.CustomCommandBar {
  public class VCCPopupCommand {
    private readonly string caption;
    private readonly string name;
    private readonly int position;
    private readonly CommandBarName parentCmdBarName;

    public string Caption {
      get { return caption; }
    }
    public string Name {
      get { return name; }
    }
    public int Position {
      get { return position; }
    }

    public CommandBarName ParentCmdBarName {
    get { return parentCmdBarName;}
    }

    public CommandBarPopup Control { get; set; }

    private CommandBarPopup FindExistingPopUpControl() {
      CommandBar cmdBar = Utilities.GetCommandBar(ParentCmdBarName);
      CommandBarPopup popup = null;

      foreach (CommandBarControl Ctrl in cmdBar.Controls) {
        if (Ctrl.Caption == Caption) {
          popup = Ctrl as CommandBarPopup;
        }
      }
      return popup;
    }
    private CommandBarPopup FindExistingPopUpControl(CommandBar cmdBar)
    {
      CommandBarPopup popup = null;

      foreach (CommandBarControl Ctrl in cmdBar.Controls) {
        if (Ctrl.Caption == Caption) {
          popup = Ctrl as CommandBarPopup;
        }
      }
      return popup;
    }

    private CommandBarPopup CreatePopUpControl() {
      CommandBar cmdBar = Utilities.GetCommandBar(ParentCmdBarName);

      return cmdBar.Controls.Add(MsoControlType.msoControlPopup,
                                                  System.Type.Missing,
                                                  System.Type.Missing,
                                                  Position,
                                                  true) as CommandBarPopup;
    }

    private CommandBarPopup CreatePopUpControl(CommandBar cmdBar)
    {
      return cmdBar.Controls.Add(MsoControlType.msoControlPopup,
                                 System.Type.Missing,
                                 System.Type.Missing,
                                 Position,
                                 true) as CommandBarPopup;
    }

    public VCCPopupCommand(string Caption, string Name, int Position, CommandBar cmdBar)
    {
      this.name = Name;
      this.caption = Caption;
      this.position = Position;

      Control = FindExistingPopUpControl(cmdBar) ?? CreatePopUpControl(cmdBar);
      Control.Caption = this.Caption;
    }

    public VCCPopupCommand(string Caption, string Name, int Position, CommandBarName cmdBarName ) {
      this.name = Name;
      this.caption = Caption;
      this.position = Position;
      this.parentCmdBarName = cmdBarName;

      Control = FindExistingPopUpControl() ?? CreatePopUpControl();
      Control.Caption = this.Caption;    
    }
    
    public void Delete()
    {
      try {
        object Temp = Type.Missing;
        Control.Delete(Temp);
      }
      catch
      {}
    }
  }
}
