//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;

namespace VerifiedCCompilerAddin.Commands {
  public abstract class VCCCommand {
    private readonly string m_caption;
    private readonly string m_description;
    private readonly int m_iconIndex;
    private readonly AddIn m_addin;
    private readonly string m_VCCBindingString;
    private readonly CommandBarName[] m_CommandBarNames;

    public string Caption {
      get { return m_caption; }
    }

    public Command Command { get; set; }

    public DTE2 DTE { get; private set; }

    public string Name { get; private set; }


    protected VCCCommand(DTE2 dte, AddIn addin, string CommandName, string Caption, string Description, int IconIndex, string VCCBindingString, params CommandBarName[] cmdBarNames) {
      Command = null;
      m_addin = addin;
      DTE = dte;
      Name = CommandName;
      m_caption = Caption;
      m_description = Description;
      m_iconIndex = IconIndex;
      m_VCCBindingString = VCCBindingString;
      m_CommandBarNames = cmdBarNames;
    }

    //Add to cmdBarName Menu
    public void AddToVSCommandBar(CommandBarName cmdBarName) {
      Command.AddControl(Utilities.GetCommandBar(cmdBarName));
    }

    //Install Keybinding
    public void RegisterKeyBinding(string VCCBindingString) {
      object[] temp = new object[1];
      temp[0] = VCCBindingString;
      Command.Bindings = temp;
    }

    public virtual void Install() {
      object[] contextGUIDS = new object[] { };

      string CommandName = m_addin.ProgID + "." + Name;

      //if command exists use it!
      foreach (Command cmd in DTE.Commands) {
        if (cmd.Name == CommandName)
          Command = cmd;
      }

      // *** If not create it!
      if (Command == null) {
        Command = this.DTE.Commands.AddNamedCommand(
                  this.m_addin,
                  Name, m_caption, m_description,
                  m_iconIndex == 0 ? true : false, m_iconIndex,
                  ref contextGUIDS,
                  (int)vsCommandStatus.vsCommandStatusSupported +
                  (int)vsCommandStatus.vsCommandStatusEnabled);
      }

      if (m_VCCBindingString != null)
        RegisterKeyBinding(m_VCCBindingString);
      if (m_CommandBarNames != null) {
       foreach (CommandBarName barName in m_CommandBarNames)
          AddToVSCommandBar(barName);
      }
    }

    public virtual void Uninstall() {
      Command.Delete();
    }

    public abstract bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled);
    public abstract void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText);

  }
}
