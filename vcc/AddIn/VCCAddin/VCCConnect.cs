//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Security.Permissions;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using EnvDTE80;
using Extensibility;
using VerifiedCCompilerAddin.Manager.Commands;
using VerifiedCCompilerAddin.Manager.Marker;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;
using VerifiedCCompilerAddin.ProjectExtender;

namespace VerifiedCCompilerAddin {
  /// <summary>The object for implementing an Add-in.</summary>
  /// <seealso class='IDTExtensibility2' />
   
  public class VCCConnect : IDTExtensibility2, IDTCommandTarget {

    private DTE2 _applicationObject;
    private AddIn _addInInstance;
    private string _addInNameSpace = "VerifiedCCompilerAddin.VCCConnect.";
    
    //Document Events
    DocumentEvents docEvents = null;
    WindowEvents winEvents = null;
    TextEditorEvents txtEvents = null;
    OutputWindowEvents owEvents = null;
    SolutionEvents slnEvents = null;

    /// <summary>Implements the constructor for the Add-in object. Place your initialization code within this method.</summary>
    [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.ControlAppDomain)]
    public VCCConnect() {
      try {
        Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
        Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(Application_ThreadException);
        AppDomain currentDomain = AppDomain.CurrentDomain;
        currentDomain.UnhandledException += new UnhandledExceptionEventHandler(currentDomain_UnhandledException);
      } catch {
        //MessageBox.Show("Unhandled exception handlind is disabled!");
      }
    }

    void Application_ThreadException(object sender, System.Threading.ThreadExceptionEventArgs e) {
      Exception ex = e.Exception;

      StringBuilder ErrorStr = new StringBuilder();
      ErrorStr.AppendLine("Sorry!");
      ErrorStr.AppendLine("There was an unhandled exception in this tool, please report the following output via Product Studio!");
      ErrorStr.AppendLine("This Message is also copied to clipboard, to paste it!");
      ErrorStr.AppendLine("Please also attach all files required to reproduce this problem.");
      ErrorStr.AppendLine("");
      ErrorStr.AppendLine("The following error was unhandled:");
      ErrorStr.AppendLine("Error Source: " + ex.Source);
      ErrorStr.AppendLine("Version: " + Utilities.GetVCCVersionString());
      ErrorStr.AppendLine("Error Message:");
      ErrorStr.AppendLine(ex.Message);
      ErrorStr.AppendLine("Stack Trace:");
      ErrorStr.AppendLine(ex.StackTrace);

      MessageBox.Show(ErrorStr.ToString());
      Clipboard.SetText(ErrorStr.ToString());

      if (ex.Source == "VccModelViewer") {
        AddInGlobals.ModelViewerObj.Dispose();
        AddInGlobals.ModelViewerWindow.Close(vsSaveChanges.vsSaveChangesNo);
        Utilities.InstallModelViewerWindow(_applicationObject, _addInInstance);
      }

      // throw new NotImplementedException();
    }

    //Catches unhandled errors, and reports them to EventLog
    void currentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs args) {
      try {
        Exception e = (Exception)args.ExceptionObject;
      
        System.Diagnostics.EventLog appLog = new System.Diagnostics.EventLog();
        appLog.Source = "VCC-Addin Exception";

        StringBuilder ErrorStr = new StringBuilder();
        ErrorStr.AppendLine("The following error was unhandled:");
        ErrorStr.AppendLine("Error Message:");
        ErrorStr.AppendLine(e.Message);
        ErrorStr.AppendLine("Stack Trace:");
        ErrorStr.AppendLine(e.StackTrace);

        appLog.WriteEntry(ErrorStr.ToString(), System.Diagnostics.EventLogEntryType.Error);

      } catch {
        
      }
    }


    
    /// <summary>Implements the OnConnection method of the IDTExtensibility2 interface. Receives notification that the Add-in is being loaded.</summary>
    /// <param term='application'>Root object of the host application.</param>
    /// <param term='connectMode'>Describes how the Add-in is being loaded.</param>
    /// <param term='addInInst'>Object representing this Add-in.</param>
    /// <seealso class='IDTExtensibility2' />
    public void OnConnection(object application, ext_ConnectMode connectMode, object addInInst, ref Array custom) {
      _applicationObject = (DTE2)application;
      _addInInstance = (AddIn)addInInst;

      AddInGlobals.DTE = _applicationObject;
      AddInGlobals.AddIn = _addInInstance;
      AddInGlobals.ActiveDocument = ActiveDocument.getInstance(_applicationObject);
      
      if ((connectMode == ext_ConnectMode.ext_cm_AfterStartup) || (connectMode == ext_ConnectMode.ext_cm_Startup)) {

        VCCExtenderProvider extenderProvider = new VCCExtenderProvider();
        AddInGlobals.ExtenderCookie = _applicationObject.ObjectExtenders.RegisterExtenderProvider("{EE8299CB-19B6-4F20-ABEA-E1FD9A33B683}", extenderProvider.DynamicExtenderName, extenderProvider, String.Empty);
        
        
        try {
          if (AddInGlobals.VCCCommandManager == null) {
              AddInGlobals.VCCCommandManager = new CommandManager(_addInNameSpace, _applicationObject, _addInInstance);
              AddInGlobals.VCCCommandManager.InstallCommands();
          }

          if (AddInGlobals.VCCMarkerManger == null)
            AddInGlobals.VCCMarkerManger = new MarkerManager();
          
          InstallIDEEventHandler();
          Utilities.InstallVCCWindow(_applicationObject, _addInInstance, winEvents);
          if (AddinSettingsManager.ShowBallonTip)
             Utilities.InstallNotifyIcon();
        } catch (Exception ex) {
          if (System.Windows.Forms.MessageBox.Show("Add-in was not properly installed and is therefore not loaded." + Environment.NewLine + "Would you like to see the detailed message?", "Error", System.Windows.Forms.MessageBoxButtons.YesNo, System.Windows.Forms.MessageBoxIcon.Error) == System.Windows.Forms.DialogResult.Yes) {
            string DetailMessage = String.Format("Error Message:{0}{1}{0}{0}Source:{0}{2}{0}{0}Stack Trace:{0}{3}", Environment.NewLine, ex.Message, ex.Source, ex.StackTrace);
            Clipboard.SetText(DetailMessage);
            System.Windows.Forms.MessageBox.Show("The following information has been copied to the clipboard, please report this error at http://vcc.codeplex.com ." + Environment.NewLine + DetailMessage);
          }
        }
      }
    }

    /// <summary>Implements the OnDisconnection method of the IDTExtensibility2 interface. Receives notification that the Add-in is being unloaded.</summary>
    /// <param term='disconnectMode'>Describes how the Add-in is being unloaded.</param>
    /// <param term='custom'>Array of parameters that are host application specific.</param>
    /// <seealso class='IDTExtensibility2' />
    public void OnDisconnection(ext_DisconnectMode disconnectMode, ref Array custom) {
      
      try {
        //Stop a running VCC if Disconnecting...
        if (VerifyManager.isRunning)
          VerifyManager.Cancel();

        //Clean Up all Markers
        if (AddInGlobals.VCCMarkerManger != null)
          AddInGlobals.VCCMarkerManger.InvalidateAllMarkers();
        
        //Removes all VCC Addin Commands
        if (AddInGlobals.VCCCommandManager != null)
          AddInGlobals.VCCCommandManager.Uninstall();
        
        //Remove Event Handlers
        UninstallIDEEventHandler();
        if (AddinSettingsManager.ShowBallonTip)
            Utilities.RemoveNotifyIcon();
        _applicationObject.ObjectExtenders.UnregisterExtenderProvider(AddInGlobals.ExtenderCookie);

      }
      catch { }
    }

    /// <summary>Implements the OnAddInsUpdate method of the IDTExtensibility2 interface. Receives notification when the collection of Add-ins has changed.</summary>
    /// <param term='custom'>Array of parameters that are host application specific.</param>
    /// <seealso class='IDTExtensibility2' />		
    public void OnAddInsUpdate(ref Array custom) {
    }

    /// <summary>Implements the OnStartupComplete method of the IDTExtensibility2 interface. Receives notification that the host application has completed loading.</summary>
    /// <param term='custom'>Array of parameters that are host application specific.</param>
    /// <seealso class='IDTExtensibility2' />
    public void OnStartupComplete(ref Array custom) {
      
    }

    /// <summary>Implements the OnBeginShutdown method of the IDTExtensibility2 interface. Receives notification that the host application is being unloaded.</summary>
    /// <param term='custom'>Array of parameters that are host application specific.</param>
    /// <seealso class='IDTExtensibility2' />
    public void OnBeginShutdown(ref Array custom) {

    }

    #region IDTCommandTarget Members
    /// <summary>
    /// Generic command handler, handles all received commands and dispatch it to specific command
    /// </summary>
    /// <param name="CmdName"></param>
    /// <param name="ExecuteOption"></param>
    /// <param name="VariantIn"></param>
    /// <param name="VariantOut"></param>
    /// <param name="Handled"></param>
    public void Exec(string CmdName, vsCommandExecOption ExecuteOption, ref object VariantIn, ref object VariantOut, ref bool Handled) {
      AddInGlobals.VCCCommandManager.Exec(CmdName, ExecuteOption, ref VariantIn, ref VariantOut, ref Handled);
    }

    /// <summary>
    /// Generic command Queryhandler, queries status returns feedback to IDE if a command is aviable, enabled or the active Item-Text.
    /// </summary>
    /// <param name="commandName"></param>
    /// <param name="neededText"></param>
    /// <param name="status"></param>
    /// <param name="commandText"></param>
    public void QueryStatus(string commandName, vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      AddInGlobals.VCCCommandManager.QueryStatus(commandName, neededText, ref status, ref commandText);
    }

    #endregion


    #region Helper Methods


    
    /// <summary>
    /// Installs specified Eventhandler for updating VCC Window.
    /// </summary>
    private void InstallIDEEventHandler() {
      owEvents = _applicationObject.Events.get_OutputWindowEvents(null);
      docEvents = _applicationObject.Events.get_DocumentEvents(null);
      docEvents.DocumentOpened += new _dispDocumentEvents_DocumentOpenedEventHandler(docEvents_DocumentOpened);
      
      txtEvents = _applicationObject.Events.get_TextEditorEvents(null) as TextEditorEvents;
      txtEvents.LineChanged += new _dispTextEditorEvents_LineChangedEventHandler(TextEvents_LineChanged);
      
      winEvents = (EnvDTE.WindowEvents)_applicationObject.Events.get_WindowEvents(null);
      winEvents.WindowActivated += new _dispWindowEvents_WindowActivatedEventHandler(winEvents_WindowActivated);



      slnEvents = _applicationObject.Events.SolutionEvents;
      slnEvents.Opened += new _dispSolutionEvents_OpenedEventHandler(SolutionEvents_Opened);
      slnEvents.AfterClosing += new _dispSolutionEvents_AfterClosingEventHandler(SolutionEvents_AfterClosing);
    }  
    //Unregister IDE Events
    private void UninstallIDEEventHandler() {
      
      try {       
        if (docEvents!=null)
        docEvents.DocumentOpened -= docEvents_DocumentOpened;
        if (txtEvents!=null)
        txtEvents.LineChanged -= TextEvents_LineChanged;
        if (winEvents != null)
        winEvents.WindowActivated -= winEvents_WindowActivated;
        if (slnEvents != null) {
          slnEvents.AfterClosing -= SolutionEvents_AfterClosing;
          slnEvents.Opened -= SolutionEvents_Opened;
        }
      } catch 
      {}
    }

    #region IDE EventHandler
    /// <summary>
    /// Is called from IDE, if a line of Code was changed...
    /// </summary>
    /// <param name="StartPoint"></param>
    /// <param name="EndPoint"></param>
    /// <param name="Hint"></param>
    void TextEvents_LineChanged(TextPoint StartPoint, TextPoint EndPoint, int Hint) {
      if (AddInGlobals.ActiveDocument.Document != null) {
        AddInGlobals.VCCMarkerManger.LineChanged(AddInGlobals.ActiveDocument.FullFileName, StartPoint.Line);
      }
    }
    /// <summary>
    /// Called from IDE if a document is opened..
    /// </summary>
    /// <param name="Document"></param>
    void docEvents_DocumentOpened(Document Document) {
      //New file opens, lets check if we need to mark it!
      AddInGlobals.VCCMarkerManger.UpdateDocument(Document.FullName);
      //if (Document.FullName.EndsWith(".c") || Document.FullName.EndsWith(".h"))
      // Utilities.VCCRandomSeedMenu.Visible = true;
    }

    void winEvents_WindowActivated(Window GotFocus, Window LostFocus) {
     
      return;
    }

    void SolutionEvents_AfterClosing() {
     // Utilities.VCCRandomSeedMenu.Visible = false;
     // Utilities.VCCMainMenu.Visible = false;
    }

    void SolutionEvents_Opened() {
      //Random Seed & Verify Menu only visible for vcproj
      /*if (Utilities.ContainsVCPrjType(Utilities.GlobalDTE.Solution.Projects)) {
        Utilities.VCCMainMenu.Visible = true;
        Utilities.VCCRandomSeedMenu.Visible = true;
      } else {
        Utilities.VCCMainMenu.Visible = false;
        Utilities.VCCRandomSeedMenu.Visible = false;
      }*/

      //For the Moment allways visible
      AddInGlobals.VCCMainMenu.Visible = true;
      AddInGlobals.VCCRandomSeedMenu.Visible = true;
      

    }
    #endregion

  
    #endregion
  }


}