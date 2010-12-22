﻿using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using System.Windows.Forms;
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
    // This attribute registers a tool window exposed by this package.
    [ProvideToolWindow(typeof(MyToolWindow))]
    // This attribute makes sure this package is loaded and initialized when a solution exists
    [ProvideOptionPage(typeof(VccOptionPage), "VCC", "General", 101, 106, true)]
    [ProvideAutoLoad("{f1536ef8-92ec-443c-9ed7-fdadf150da82}")]
    [Guid(GuidList.guidVSPackagePkgString)]
    public sealed class VSPackagePackage : Package
    {
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
        }


        /// <summary>
        /// This function is called when the user clicks the menu item that shows the 
        /// tool window. See the Initialize method to see how the menu item is associated to 
        /// this function using the OleMenuCommandService service and the MenuCommand class.
        /// </summary>
        private void ShowToolWindow(object sender, EventArgs e)
        {
            // Get the instance number 0 of this tool window. This window is single instance so this instance
            // is actually the only one.
            // The last flag is set to true so that if the tool window does not exists it will be created.
            ToolWindowPane window = this.FindToolWindow(typeof(MyToolWindow), 0, true);
            if ((null == window) || (null == window.Frame))
            {
                throw new NotSupportedException(Resources.CanNotCreateWindow);
            }
            IVsWindowFrame windowFrame = (IVsWindowFrame)window.Frame;
            Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(windowFrame.Show());
        }

        /// <summary>
        ///     Launches VCC.exe to verify the active file
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void VerifyActiveFile(object sender, EventArgs e)
        {
            if (!VSIntegration.ProjectSaved())
            {
                DialogResult dialogResult =
                    MessageBox.Show("There are unsaved documents. Would you like to save all documents before proceding?",
                                        "Unsaved Items",
                                        MessageBoxButtons.YesNoCancel,
                                        MessageBoxIcon.Question,
                                        MessageBoxDefaultButton.Button3);

                switch (dialogResult)
                {
                    case DialogResult.Cancel:
                        return;
                    case DialogResult.Yes:
                        VSIntegration.SaveAll();
                        break;
                    case DialogResult.No:
                    default:
                        break;
                }

            }
            
            VccOptionPage options = this.GetAutomationObject("VCC.General") as VccOptionPage;
            if (options != null)
            {
                VCCLauncher.VerifyFile(VSIntegration.ActiveFileFullName, options);
            }
        }

        /// <summary>
        ///     Launches VCC.exe to verify the current function
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void VerifyCurrentFunction(object sender, EventArgs e)
        {
            VccOptionPage options = this.GetAutomationObject("VCC.General") as VccOptionPage;
            if (options != null)
            {
                VCCLauncher.VerifyFunction( VSIntegration.ActiveFileFullName,
                                            VSIntegration.CurrentFunctionName,
                                            options);
            }
        }

        private void Cancel(object sender, EventArgs e)
        {
            VCCLauncher.Cancel();
        }

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
                if (VCCLauncher.VCCRunning())
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
                    if (VCCLauncher.VCCRunning())
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

        void Cancel_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VCCLauncher.VCCRunning() && VSIntegration.IsCodeFile)
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
                    if (VCCLauncher.VCCRunning())
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
                if (VCCLauncher.VCCRunning())
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
                MenuCommand menuItem;
                OleMenuCommand OleMenuItem;

                //// VCC Options
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidMyCommand);
                menuItem = new MenuCommand(ShowToolWindow, menuCommandID);
                mcs.AddCommand(menuItem);

                //// Verify File
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyActiveFile);
                OleMenuItem = new OleMenuCommand(VerifyActiveFile, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(VerifyFile_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                //// Verify Function
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyCurrentFunction);
                OleMenuItem = new OleMenuCommand(VerifyCurrentFunction, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(VerifyFunction_BeforeQueryStatus);
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

                //// Verify Function (Context Menu)
                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidContextVerifyCurrentFunction);
                OleMenuItem = new OleMenuCommand(VerifyCurrentFunction, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(ContextVerifyFunction_BeforeQueryStatus);
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

        protected override int QueryClose(out bool canClose)
        {
            VCCLauncher.Cancel();
            canClose = true;
            return VSConstants.S_OK;
        }
    }
}
