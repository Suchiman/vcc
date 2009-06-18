//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;

namespace VerifiedCCompilerAddin.Commands {
  public abstract class VCCCommand {
    private string m_caption;
    private string m_description;
    private string m_commandName;
    private int m_iconIndex;
    private DTE2 m_dte;
    private AddIn m_addin;
    private string m_VCCBindingString;
    private CommandBarName[] m_CommandBarNames;

    public string Caption {
      get { return m_caption; }
    }

    private Command m_command = null;
    public Command Command {
      get { return m_command; }
      set { m_command = value; }
    }

    public DTE2 DTE {
      get { return m_dte; }
    }

    public string Name {
      get { return m_commandName; }
    }


    protected VCCCommand(DTE2 dte, AddIn addin, string CommandName, string Caption, string Description, int IconIndex, string VCCBindingString, params CommandBarName[] cmdBarNames) {
      m_addin = addin;
      m_dte = dte;
      m_commandName = CommandName;
      m_caption = Caption;
      m_description = Description;
      m_iconIndex = IconIndex;
      m_VCCBindingString = VCCBindingString;
      m_CommandBarNames = cmdBarNames;
    }

    //Add to cmdBarName Menu
    public void AddToVSCommandBar(CommandBarName cmdBarName) {
      Command.AddControl(Utilities.GetCommandBar(cmdBarName), 1);
    }

    //Install Keybinding
    public void RegisterKeyBinding(string VCCBindingString) {
      object[] temp = new object[1];
      temp[0] = VCCBindingString;
      Command.Bindings = temp;
    }

    public virtual void Install() {
      object[] contextGUIDS = new object[] { };

      string CommandName = m_addin.ProgID + "." + m_commandName;

      //if command exists use it!
      foreach (Command cmd in m_dte.Commands) {
        if (cmd.Name == CommandName)
          Command = cmd;
      }

      // *** If not create it!
      if (Command == null) {
        Command = this.m_dte.Commands.AddNamedCommand(
                  this.m_addin,
                  m_commandName, m_caption, m_description,
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
