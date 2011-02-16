using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    ///
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the 
    /// IVsPackage interface and uses the registration attributes defined in the framework to 
    /// register itself and its components with the shell.
    /// </summary>
    // This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    // a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]
    // This attribute is used to register the informations needed to show the this package
    // in the Help/About dialog of Visual Studio.
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    // This attribute is needed to let the shell know that this package exposes some menus.
    [ProvideMenuResource("Menus.ctmenu", 1)]
    // This attribute makes sure this package is loaded and initialized when a solution exists
    [ProvideOptionPage(typeof(VccOptionPage), "VCC", "General", 101, 106, true)]
    [ProvideAutoLoad("{f1536ef8-92ec-443c-9ed7-fdadf150da82}")]
    [Guid(GuidList.guidVSPackagePkgString)]
    public sealed class VSPackagePackage : Package
    {
        
        /// <summary>
        ///     this helps to get the instance of the Packageclass from outside this class.
        ///     constructor is still public because Visual Studio calls the constructor
        /// </summary>
        private static VSPackagePackage instance;
        public static VSPackagePackage Instance
        {
            get
            {
                return instance;
            }
        }
        public VccOptionPage OptionPage
        {
            get
            {
                return this.GetDialogPage(typeof(VccOptionPage)) as VccOptionPage;
            }
        }
        /// <summary>
        ///     here the parameters of the last call of Vcc are stored
        /// </summary>
        internal static string LastAction { get; set; }

        /// <summary>
        /// Default constructor of the package.
        /// Inside this method you can place any initialization code that does not require 
        /// any Visual Studio service because at this point the package object is created but 
        /// not sited yet inside Visual Studio environment. The place to do all the other 
        /// initialization is the Initialize method.
        /// </summary>
        public VSPackagePackage()
        {
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this.ToString()));
            if (instance != null) throw new InvalidOperationException();
            instance = this;
            LastAction = "";
        }

        #region Commands

        private void CustomVerify(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            if (OptionPage != null)
            {
                VCCLauncher.CustomVerify(VSIntegration.ActiveFileFullName, OptionPage);
            }
        }

        /// <summary>
        ///     Launches VCC.exe to verify the active file
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void VerifyActiveFile(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            if (OptionPage != null)
            {
                VCCLauncher.VerifyFile(VSIntegration.ActiveFileFullName, OptionPage);
            }
        }

        private void CheckAdmissiblityOfStruct(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            if (OptionPage != null)
            {
                VCCLauncher.VerifyFunction( VSIntegration.ActiveFileFullName,
                                            VSIntegration.CurrentStructName,
                                            OptionPage);
            }
        }

        /// <summary>
        ///     Launches VCC.exe to verify the current function
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void VerifyCurrentFunction(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            VccOptionPage options = GetDialogPage(typeof(VccOptionPage)) as VccOptionPage;
            if (OptionPage != null)
            {
                VCCLauncher.VerifyFunction( VSIntegration.ActiveFileFullName,
                                            VSIntegration.CurrentFunctionName,
                                            OptionPage);
            }
        }

        private void ReVerify(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            VCCLauncher.LaunchVCC(LastAction);
        }

        private void Cancel(object sender, EventArgs e)
        {
            VCCLauncher.Cancel();
        }

        #endregion

        #region BeforeQueryStatusHandlers

        /// <summary>
        ///     Hides/Shows the VerifyMenu
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void VerifyMenu_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show menu
                    ((MenuCommand)sender).Visible = true;
                    
                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide menu
                    ((MenuCommand)sender).Visible = false;
                }
            }
        }

        /// <summary>
        ///     Renames the VerifyFile-Button
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void VerifyFile_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                ((OleMenuCommand)sender).Text = string.Format("Verify File: '{0}'", VSIntegration.ActiveFileName);
                if (VCCLauncher.VCCRunning)
                {
                    ((OleMenuCommand)sender).Enabled = false;
                }
                else
                {
                    ((OleMenuCommand)sender).Enabled = true;
                }
            }
        }

        /// <summary>
        ///     Disables/Enables and renames the VerifyFunction-Button
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void VerifyFunction_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.CurrentFunctionName != string.Empty)
                {
                    //// Name of current function is known.
                    ((OleMenuCommand)sender).Text = string.Format("Verify Function '{0}'", VSIntegration.CurrentFunctionName);
                    if (VCCLauncher.VCCRunning)
                    {
                        ((OleMenuCommand)sender).Enabled = false;
                    }
                    else
                    {
                        ((OleMenuCommand)sender).Enabled = true;
                    }
                }
                else
                {
                    //// There is no current function.
                    ((OleMenuCommand)sender).Text = "Verify Current Function";
                    ((OleMenuCommand)sender).Enabled = false;
                }
            }
        }

        void CheckAdmissiblityOfStruct_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.CurrentStructName != string.Empty)
                {
                    //// Name of current struct is known.
                    ((OleMenuCommand)sender).Text = string.Format("Check Admissibility of Struct '{0}'", VSIntegration.CurrentStructName);
                    if (VCCLauncher.VCCRunning)
                    {
                        ((OleMenuCommand)sender).Enabled = false;
                    }
                    else
                    {
                        ((OleMenuCommand)sender).Enabled = true;
                    }
                }
                else
                {
                    //// There is no current function.
                    ((OleMenuCommand)sender).Text = "Check Admissibility of Current Struct";
                    ((OleMenuCommand)sender).Enabled = false;
                }
            }
        }

        void Cancel_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VCCLauncher.VCCRunning && VSIntegration.IsCodeFile)
                {
                    ((OleMenuCommand)sender).Visible = true;
                }
                else
                {
                    ((OleMenuCommand)sender).Visible = false;
                }
            }
        }

        void ContextVerifyFunction_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.CurrentFunctionName != string.Empty)
                {
                    //// Name of current function is known.
                    ((OleMenuCommand)sender).Text = string.Format("Verify Function '{0}'", VSIntegration.CurrentFunctionName);
                    if (VCCLauncher.VCCRunning)
                    {
                        ((OleMenuCommand)sender).Enabled = false;
                    }
                    else
                    {
                        ((OleMenuCommand)sender).Enabled = true;
                    }
                }
                else
                {
                    //// There is no current function.
                    ((OleMenuCommand)sender).Text = "Verify Current Function";
                    ((OleMenuCommand)sender).Enabled = false;
                }

                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show entry
                    ((MenuCommand)sender).Visible = true;

                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide entry
                    ((MenuCommand)sender).Visible = false;
                }
            }

        }

        void ContextVerifyFile_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                ((OleMenuCommand)sender).Text = string.Format("Verify File: '{0}'", VSIntegration.ActiveFileName);
                if (VCCLauncher.VCCRunning)
                {
                    ((OleMenuCommand)sender).Enabled = false;
                }
                else
                {
                    ((OleMenuCommand)sender).Enabled = true;
                }

                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show entry
                    ((OleMenuCommand)sender).Visible = true;
                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide entry
                    ((OleMenuCommand)sender).Visible = false;
                }
            }
        }

        void ContextCheckAdmissibilityOfStruct_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.CurrentStructName != string.Empty)
                {
                    //// Name of current struct is known.
                    ((OleMenuCommand)sender).Text = string.Format("Check Admissibility of Struct '{0}'", VSIntegration.CurrentStructName);
                    if (VCCLauncher.VCCRunning)
                    {
                        ((OleMenuCommand)sender).Enabled = false;
                    }
                    else
                    {
                        ((OleMenuCommand)sender).Enabled = true;
                    }
                }
                else
                {
                    //// There is no current struct.
                    ((OleMenuCommand)sender).Text = "Check Admissibility of Current Struct";
                    ((OleMenuCommand)sender).Enabled = false;
                }

                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show entry
                    ((MenuCommand)sender).Visible = true;

                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide entry
                    ((MenuCommand)sender).Visible = false;
                }
            }
        }

        void ReVerify_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VCCLauncher.VCCRunning)
                {
                    ((OleMenuCommand)sender).Enabled = false;
                }
                else
                {
                    ((OleMenuCommand)sender).Enabled = true;
                }

                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show entry
                    ((OleMenuCommand)sender).Visible = true;
                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide entry
                    ((OleMenuCommand)sender).Visible = false;
                }
            }
        }

        void CustomVerify_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VCCLauncher.VCCRunning)
                {
                    ((OleMenuCommand)sender).Enabled = false;
                }
                else
                {
                    ((OleMenuCommand)sender).Enabled = true;
                }

                if (VSIntegration.IsCodeFile)
                {
                    //// active document is in C or C++ => show entry
                    ((OleMenuCommand)sender).Visible = true;
                }
                else
                {
                    //// there is no active document or it is not in C or C++ => hide entry
                    ((OleMenuCommand)sender).Visible = false;
                }
            }
        }

        #endregion

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initilaization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();

            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if (null != mcs)
            {

                //// Create the commands for the menu items.

                CommandID menuCommandID;
                OleMenuCommand OleMenuItem;

                //// Verify File
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyActiveFile);
                OleMenuItem = new OleMenuCommand(VerifyActiveFile, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(VerifyFile_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Check Admissibility of struct
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidCheckAdmissibilityOfCurrentStruct);
                OleMenuItem = new OleMenuCommand(CheckAdmissiblityOfStruct, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(CheckAdmissiblityOfStruct_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Verify Function
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyCurrentFunction);
                OleMenuItem = new OleMenuCommand(VerifyCurrentFunction, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(VerifyFunction_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Re-Verify
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidReVerify);
                OleMenuItem = new OleMenuCommand(ReVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ReVerify_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Custom Verify
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidCustomVerify);
                OleMenuItem = new OleMenuCommand(CustomVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(CustomVerify_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Cancel
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidCancel);
                OleMenuItem = new OleMenuCommand(Cancel, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(Cancel_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Verify File (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextVerifyActiveFile);
                OleMenuItem = new OleMenuCommand(VerifyActiveFile, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ContextVerifyFile_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Check Admissibility of struct (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextCheckAdmissibilityOfCurrentStruct);
                OleMenuItem = new OleMenuCommand(CheckAdmissiblityOfStruct, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ContextCheckAdmissibilityOfStruct_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Verify Function (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextVerifyCurrentFunction);
                OleMenuItem = new OleMenuCommand(VerifyCurrentFunction, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ContextVerifyFunction_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Re-Verify (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextReVerify);
                OleMenuItem = new OleMenuCommand(ReVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ReVerify_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Custom Verify (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextCustomVerify);
                OleMenuItem = new OleMenuCommand(CustomVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(CustomVerify_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Cancel (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextCancel);
                OleMenuItem = new OleMenuCommand(Cancel, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(Cancel_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Verifymenu
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyMenu);
                OleMenuItem = new OleMenuCommand(null, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(VerifyMenu_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);
            }
        }

        /// <summary>
        ///     is called when VS closes. Overridden to cancel VCC, if it's still running.
        /// </summary>
        /// <param name="canClose"></param>
        /// <returns></returns>
        protected override int QueryClose(out bool canClose)
        {
            VCCLauncher.Cancel();
            canClose = true;
            return VSConstants.S_OK;
        }
    }
}
