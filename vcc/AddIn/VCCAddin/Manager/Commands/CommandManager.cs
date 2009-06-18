//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using VerifiedCCompilerAddin.Commands;
using EnvDTE80;
using EnvDTE;
using Microsoft.VisualStudio.CommandBars;
using VerifiedCCompilerAddin.Commands.CustomCommandBar;


namespace VerifiedCCompilerAddin.Manager.Commands {
  public class CommandManager {

    Dictionary<string, VCCCommand> Commands = new Dictionary<string, VCCCommand>();
    List<VCCPopupCommand> CommandBarPopups;
    private string _addInNameSpace;
    private DTE2 _applicationObject;
    private AddIn _addInInstance;

    public CommandManager(string AddInNameSpace, DTE2 applicationObject, AddIn addInInstance) {
      this._addInInstance = addInInstance;
      this._addInNameSpace = AddInNameSpace;
      this._applicationObject = applicationObject;
    }

    public VCCCommand ResolveCommand(String commandName) {
      if (commandName.StartsWith(_addInNameSpace)) {
        return Commands[commandName];
      }
      return null;
    }


    /// <summary>
    /// Installs Add-in Commands. All Commands are added to the collection Commands. 
    /// This Collection is used later to identify command instance to execute, query or delete command.
    /// </summary>
    public void InstallCommands() {
      InstallPopUps();
      InstallVCCCommands();
    }

    private void InstallPopUps()
    {
      CommandBar MenueBar = Utilities.GetCommandBar(CommandBarName.MenuBar);
      int position = 3; // (After File, Edit and View, in case that the language is other than de or en)

      foreach (CommandBarControl Ctrl in MenueBar.Controls) {
        if (Ctrl.Caption == "&Help" || Ctrl.Caption == "&Hilfe") {
          position = Ctrl.Index;
        }
      }

      VCCPopupCommand VCCMainMenu = new VCCPopupCommand("Verif&y", "VCCMainMenuBar", position, CommandBarName.MenuBar);
      VCCPopupCommand VCCRandomSeedMenu = new VCCPopupCommand("&Random Seed", "VCCRandomSeedMenuBar", 1, CommandBarName.VCCMain);
      VCCPopupCommand VCCToolsMenu = new VCCPopupCommand("More VCC Commands", "VCCMoreCommandsMenuBar", 1, CommandBarName.CodeWindow);
      AddInGlobals.VCCMainMenu = VCCMainMenu.Control;
      AddInGlobals.VCCRandomSeedMenu = VCCRandomSeedMenu.Control;
      AddInGlobals.VCCToolsMenu = VCCToolsMenu.Control;
      VCCRandomSeedMenu.Control.BeginGroup = true;

      CommandBarPopups = new List<VCCPopupCommand>();
      CommandBarPopups.Add(VCCRandomSeedMenu);
      CommandBarPopups.Add(VCCMainMenu);
      CommandBarPopups.Add(VCCToolsMenu);
    }
    private void InstallVCCCommands() {
      VCCCommand cmdVerifyCancel = new VCCCancelCommand(_applicationObject, _addInInstance);
      VCCCommand cmdShowVerifyError = new VCCShowErrorCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifySingleFile = new VCCSingleFileCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifySingleFileFunction = new VCCSingleFileFunctionCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifyTypeOnlyAdmissibility = new VCCTypeAdmissibilityTypeOnlyCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifyGroupOnlyAdmissibility = new VCCTypeAdmissibilityGroupCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifySingleProject = new VCCSingleProjectCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifySingleSolution = new VCCSingleSolutionCommand(_applicationObject, _addInInstance);
      VCCCommand cmdCustomVerifyFile = new VCCCustomFileCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifyProgress = new VCCProgressCommand(_applicationObject, _addInInstance);
      VCCCommand cmdVerifySingleFileInSolution = new VCCSingleFileInSolutionCommand(_applicationObject, _addInInstance);
      VCCCommand cmdViewVCCWindow = new VCCMenueViewCommand(_applicationObject, _addInInstance);
      VCCCommand cmdAssertSelection = new VCCAssertSelectionCommand(_applicationObject, _addInInstance);
      VCCCommand cmdViewVCCSettings = new VCCMenueSettings(_applicationObject, _addInInstance);
      VCCCommand cmdSwapAssertToAssume = new VCCSwapAssertToAssume(_applicationObject, _addInInstance);
      VCCCommand cmdSwapAssumeToAssert = new VCCSwapAssumeToAssert(_applicationObject, _addInInstance);
      VCCCommand cmdRndSeed0 = new VCCRandomSeedX(_applicationObject, _addInInstance, 0);
      VCCCommand cmdRndSeed1 = new VCCRandomSeedX(_applicationObject, _addInInstance, 1);
      VCCCommand cmdRndSeed2 = new VCCRandomSeedX(_applicationObject, _addInInstance, 2);
      VCCCommand cmdRndSeed3 = new VCCRandomSeedX(_applicationObject, _addInInstance, 3);
      VCCCommand cmdRndSeed4 = new VCCRandomSeedX(_applicationObject, _addInInstance, 4);
      VCCCommand cmdRndSeedAll = new VCCRandomSeedX(_applicationObject, _addInInstance, 9);
      VCCCommand cmdVCCCreateOpenBoogie = new VCCCreateBoogie(_applicationObject, _addInInstance);
      VCCCommand cmdViewErrorModel = new VCCViewErrorModel(_applicationObject, _addInInstance);
      VCCCommand cmdCreateFunctionModelFromErrorList = new VCCViewErrorModelFromErrorList(_applicationObject, _addInInstance);
      VCCCommand cmdLaunchZ3Viewer = new VCCLaunchZ3Visualizer(_applicationObject, _addInInstance);
      VCCCommand cmdLaunchZ3Inspector = new VCCLaunchZ3Inspector(_applicationObject, _addInInstance);

      //Add Commands
      Commands.Add(_addInNameSpace + cmdCreateFunctionModelFromErrorList.Name, cmdCreateFunctionModelFromErrorList);
      Commands.Add(_addInNameSpace + cmdRndSeedAll.Name, cmdRndSeedAll);
      Commands.Add(_addInNameSpace + cmdRndSeed4.Name, cmdRndSeed4);
      Commands.Add(_addInNameSpace + cmdRndSeed3.Name, cmdRndSeed3);
      Commands.Add(_addInNameSpace + cmdRndSeed2.Name, cmdRndSeed2);
      Commands.Add(_addInNameSpace + cmdRndSeed1.Name, cmdRndSeed1);
      Commands.Add(_addInNameSpace + cmdRndSeed0.Name, cmdRndSeed0);
      Commands.Add(_addInNameSpace + cmdViewVCCSettings.Name, cmdViewVCCSettings);
      Commands.Add(_addInNameSpace + cmdVCCCreateOpenBoogie.Name, cmdVCCCreateOpenBoogie);
      Commands.Add(_addInNameSpace + cmdViewErrorModel.Name, cmdViewErrorModel);
      Commands.Add(_addInNameSpace + cmdLaunchZ3Inspector.Name, cmdLaunchZ3Inspector);
      Commands.Add(_addInNameSpace + cmdLaunchZ3Viewer.Name, cmdLaunchZ3Viewer);
      Commands.Add(_addInNameSpace + cmdSwapAssumeToAssert.Name, cmdSwapAssumeToAssert);
      Commands.Add(_addInNameSpace + cmdSwapAssertToAssume.Name, cmdSwapAssertToAssume);
      Commands.Add(_addInNameSpace + cmdAssertSelection.Name, cmdAssertSelection);
      Commands.Add(_addInNameSpace + cmdCustomVerifyFile.Name, cmdCustomVerifyFile);
      Commands.Add(_addInNameSpace + cmdVerifySingleFile.Name, cmdVerifySingleFile);
      Commands.Add(_addInNameSpace + cmdVerifySingleFileFunction.Name, cmdVerifySingleFileFunction);
      Commands.Add(_addInNameSpace + cmdVerifyGroupOnlyAdmissibility.Name, cmdVerifyGroupOnlyAdmissibility);
      Commands.Add(_addInNameSpace + cmdVerifyTypeOnlyAdmissibility.Name, cmdVerifyTypeOnlyAdmissibility);
      Commands.Add(_addInNameSpace + cmdVerifySingleProject.Name, cmdVerifySingleProject);
      Commands.Add(_addInNameSpace + cmdVerifySingleSolution.Name, cmdVerifySingleSolution);
      Commands.Add(_addInNameSpace + cmdShowVerifyError.Name, cmdShowVerifyError);
      Commands.Add(_addInNameSpace + cmdVerifyCancel.Name, cmdVerifyCancel);
      Commands.Add(_addInNameSpace + cmdVerifyProgress.Name, cmdVerifyProgress);
      Commands.Add(_addInNameSpace + cmdVerifySingleFileInSolution.Name, cmdVerifySingleFileInSolution);
      Commands.Add(_addInNameSpace + cmdViewVCCWindow.Name, cmdViewVCCWindow);

      //Install Commands
      foreach (VCCCommand cmd in Commands.Values)
        cmd.Install();
    }

    /// <summary>
    /// Removes installes IDE Commands...
    /// </summary>
    public void Uninstall() {
      try {
        List<Command> commands = new List<Command>();
        foreach (Command command in _applicationObject.Commands)
          if (command.Name.StartsWith(_addInNameSpace))
            commands.Add(command);

        foreach (Command command in commands)
          command.Delete();
      } catch {
      }

      foreach (VCCPopupCommand Control in CommandBarPopups) {
        Control.Delete();
      }
    }


    public void QueryStatus(string commandName, vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      VCCCommand cmd = ResolveCommand(commandName);
      if (cmd != null) {
        cmd.QueryStatus(neededText, ref status, ref commandText);
      }
    }

    public void Exec(string CmdName, vsCommandExecOption ExecuteOption, ref object VariantIn, ref object VariantOut, ref bool Handled) {
      Handled = false;
      VCCCommand cmd = ResolveCommand(CmdName);
      if (cmd != null) {
        //This Commands don't change the ErrorMarkers
        //ErrorReporting via VCCShowErrorCommand, ProgressUpdate via VCCProgressCommand and ViewErrorModel Commands
        if (!(cmd is VCCShowErrorCommand) && !(cmd is VCCProgressCommand) && !(cmd is VCCViewErrorModel) && !(cmd is VCCViewErrorModelFromErrorList)) {
          AddInGlobals.VCCMarkerManger.InvalidateAllMarkers();
        }

        cmd.Exec(ExecuteOption, ref VariantIn, ref VariantOut, ref Handled);
      }
    }

  }
}
