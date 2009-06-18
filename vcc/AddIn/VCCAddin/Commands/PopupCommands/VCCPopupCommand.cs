//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.CommandBars;

namespace VerifiedCCompilerAddin.Commands.CustomCommandBar {
  public class VCCPopupCommand {
    private string caption;
    private string name;
    private int position;
    private CommandBarName parentCmdBarName;
    private CommandBarPopup cmdBarPopup;

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
    public CommandBarPopup Control {
      get { return cmdBarPopup; }
      set { cmdBarPopup = value; }
    }
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

    private CommandBarPopup CreatePopUpControl() {
      CommandBar cmdBar = Utilities.GetCommandBar(ParentCmdBarName);
      CommandBarPopup popup = null;

      popup = cmdBar.Controls.Add(MsoControlType.msoControlPopup,
                                      System.Type.Missing,
                                      System.Type.Missing,
                                      Position,
                                      true) as CommandBarPopup;
      return popup;
    }
    public VCCPopupCommand(string Caption, string Name, int Position, CommandBarName cmdBarName ) {
      this.name = Name;
      this.caption = Caption;
      this.position = Position;
      this.parentCmdBarName = cmdBarName;

      Control = FindExistingPopUpControl();
      if (Control == null) {
        Control = CreatePopUpControl();
      }

      Control.Caption = this.Caption;
      Control.CommandBar.Name = this.Name;
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
