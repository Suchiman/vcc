using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;

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
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this));
            if (instance != null) throw new InvalidOperationException();
            instance = this;
            LastAction = "";
        }

        #region Commands

        private void Options(object sender, EventArgs e)
        {
            ShowOptionPage(typeof(VccOptionPage));
        }

        private void CustomVerify(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            if (OptionPage != null)
            {
                VCCLauncher.CustomVerify(VSIntegration.ActiveFileFullName, OptionPage);
            }
        }

        private void VerifyThis(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            if (OptionPage != null)
            {
                VCCLauncher.VerifyThis(VSIntegration.ActiveFileFullName,VSIntegration.CurrentSelection, VSIntegration.CurrentLine, OptionPage);
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

        private static void ReVerify(object sender, EventArgs e)
        {
            VSIntegration.DocumentsSavedCheck();
            VCCLauncher.LaunchVCC(LastAction);
        }

        private static void Cancel(object sender, EventArgs e)
        {
            VCCLauncher.Cancel();
        }

        #endregion

        #region BeforeQueryStatusHandlers
        //// All the handlers that handle the appearance of menus and menu entries are here


        /// <summary>
        ///     Verify Menu
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void VerifyMenu_BeforeQueryStatus(object sender, EventArgs e)
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
        ///     Cancel
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void Cancel_BeforeQueryStatus(object sender, EventArgs e)
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

        /// <summary>
        ///     Verify This
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void VerifyThis_BeforeQueryStatus(object sender, EventArgs e)
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

        /// <summary>
        ///     Verify File
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void VerifyFile_BeforeQueryStatus(object sender, EventArgs e)
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

        /// <summary>
        ///     Re-Verify
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void ReVerify_BeforeQueryStatus(object sender, EventArgs e)
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

        /// <summary>
        ///     Custom Verify
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void CustomVerify_BeforeQueryStatus(object sender, EventArgs e)
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
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this));
            base.Initialize();

            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if (null != mcs)
            {

                //// Create the commands for the menu items.

                //// Verify File
                CommandID menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyActiveFile);
                OleMenuCommand OleMenuItem = new OleMenuCommand(VerifyActiveFile, menuCommandID);
                OleMenuItem.BeforeQueryStatus += VerifyFile_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Options
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidOptions);
                OleMenuItem = new OleMenuCommand(Options, menuCommandID);
                mcs.AddCommand(OleMenuItem);

                //// Re-Verify
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidReVerify);
                OleMenuItem = new OleMenuCommand(ReVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += ReVerify_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Verify This
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyThis);
                OleMenuItem = new OleMenuCommand(VerifyThis, menuCommandID);
                OleMenuItem.BeforeQueryStatus += VerifyThis_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Custom Verify
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidCustomVerify);
                OleMenuItem = new OleMenuCommand(CustomVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += CustomVerify_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Cancel
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidCancel);
                OleMenuItem = new OleMenuCommand(Cancel, menuCommandID);
                OleMenuItem.BeforeQueryStatus += Cancel_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Verify File (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextVerifyActiveFile);
                OleMenuItem = new OleMenuCommand(VerifyActiveFile, menuCommandID);
                OleMenuItem.BeforeQueryStatus += VerifyFile_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);
                
                //// Verify This(Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextVerifyThis);
                OleMenuItem = new OleMenuCommand(VerifyThis, menuCommandID);
                OleMenuItem.BeforeQueryStatus += VerifyThis_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Re-Verify (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextReVerify);
                OleMenuItem = new OleMenuCommand(ReVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += ReVerify_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Custom Verify (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextCustomVerify);
                OleMenuItem = new OleMenuCommand(CustomVerify, menuCommandID);
                OleMenuItem.BeforeQueryStatus += CustomVerify_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Cancel (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextCancel);
                OleMenuItem = new OleMenuCommand(Cancel, menuCommandID);
                OleMenuItem.BeforeQueryStatus += Cancel_BeforeQueryStatus;
                mcs.AddCommand(OleMenuItem);

                //// Verifymenu
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyMenu);
                OleMenuItem = new OleMenuCommand(null, menuCommandID);
                OleMenuItem.BeforeQueryStatus += VerifyMenu_BeforeQueryStatus;
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
