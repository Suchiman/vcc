//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using EnvDTE80;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Marker;
using VerifiedCCompilerAddin.Manager.Commands;
using Microsoft.VisualStudio.CommandBars;
using VerifiedCCompilerAddin.Forms;
using Vcc2ModelViewer;

namespace VerifiedCCompilerAddin {
  public static class AddInGlobals {

    private static DTE2 _applicationObject = null;
    /// <summary>
    /// The top-level object in the Visual Studio automation object model. 
    /// </summary>
    internal static DTE2 DTE {
      get {
        if (_applicationObject == null) {
          _applicationObject = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(DTE)) as DTE2;
        }
        return _applicationObject;
      }
      set {
        _applicationObject = value;
      }
    }

    private static AddIn _addInInstance;
    /// <summary>
    /// Represents an add-in listed in the Add-In Manager dialog box and provides information about an add-in to other add-in objects. 
    /// In this case the add-in itself.
    /// </summary>
    internal static AddIn AddIn {
      get { return _addInInstance; }
      set { _addInInstance = value; }
    }

    private static MarkerManager _markerManager = null;
    /// <summary>
    /// Represents the MarkerManager that handels squigglies for VCC errors. 
    /// </summary>
    public static MarkerManager VCCMarkerManger {
      get { return _markerManager; }
      set { _markerManager = value; }
    }

    private static CommandManager _commandManager = null;
    /// <summary>
    /// Represents the CommandManager that installs / uninstalls the AddIn commands. 
    /// </summary>
    public static CommandManager VCCCommandManager {
      get { return _commandManager; }
      set { _commandManager = value; }
    }


    private static OutputWindowPane _owBuildPane = null;
    /// <summary>
    /// Represents a pane in the Output window. 
    /// In our add-in we create a window "Verification" where we display all vcc relevant informations or outputs.
    /// </summary>
    public static OutputWindowPane BuildPane {
      get {
        if (_owBuildPane == null)
          _owBuildPane = DTE.ToolWindows.OutputWindow.OutputWindowPanes.Add("Verification");
        _owBuildPane.Activate();
        return _owBuildPane;
      }
    }

    static int _extenderCookie;
    /// <summary>
    /// Represents the Cookie from installing the Visual-C Project Extender.
    /// </summary>
    internal static int ExtenderCookie {
      get { return _extenderCookie; }
      set { _extenderCookie = value; }
    }


    static ActiveDocument activeDocument;
    /// <summary>
    /// Represents the current Document
    /// </summary>
    internal static ActiveDocument ActiveDocument {
      get { return activeDocument; }
      set { activeDocument = value; }
    }


    //Is set on connect!
    static CommandBarPopup _VerifyMenue;
    internal static CommandBarPopup VCCMainMenu {
      get { return _VerifyMenue; }
      set { _VerifyMenue = value; }
    }

    static CommandBarPopup _VCCMenue;
    internal static CommandBarPopup VCCRandomSeedMenu {
      get { return _VCCMenue; }
      set { _VCCMenue = value; }
    }

    static CommandBarPopup _VCCToolsMenue;
    internal static CommandBarPopup VCCToolsMenu {
      get { return _VCCToolsMenue; }
      set { _VCCToolsMenue = value; }
    }

    //Is set on connect!
    static Window _vccWindow;
    internal static Window VCCWindow {
      get { return _vccWindow; }
      set { _vccWindow = value; }
    }

    static VCCPane _vccPane;
    internal static VCCPane VCCPane {
      get { return _vccPane; }
      set { _vccPane = value; }
    }

    //Is set on connect!
    static Window _vccModelViewerWindow;
    internal static Window ModelViewerWindow {
      get {
        if (_vccModelViewerWindow == null) {
          if (_ModelViewerObj == null) {
            Utilities.InstallModelViewerWindow(AddInGlobals.DTE, AddInGlobals.AddIn);
          }
        }
        return _vccModelViewerWindow;
      }
      set { _vccModelViewerWindow = value; }
    }

    static ModelViewer _ModelViewerObj;
    internal static ModelViewer ModelViewerObj {
      get {
        if (_ModelViewerObj == null) {
          Utilities.InstallModelViewerWindow(AddInGlobals.DTE, AddInGlobals.AddIn);
        }
        return _ModelViewerObj;
      }
      set { _ModelViewerObj = value; }
    }

    //Contains lastest ModelFileName, sets trought VerifyJob.Execute.
    static string _LastestModelFileName;
    internal static string LastestModelFileName {
      get { return _LastestModelFileName; }
      set { _LastestModelFileName = value; }
    }

    //Contains lastest FileName, sets trought LaunchModelViewer.
    static string _LastestFileName;
    internal static string LastestFileName {
      get { return _LastestFileName; }
      set { _LastestFileName = value; }
    }

    //Contains lastest WorkingDirectory, sets trought VCCLauncher for ModelViewer.
    static string _LastestWorkDir;
    internal static string LastestWorkDir {
      get { return _LastestWorkDir; }
      set { _LastestWorkDir = value; }
    }
  }
}
