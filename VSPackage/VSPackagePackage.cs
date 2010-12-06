using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using System.Windows.Forms;
using MicrosoftResearch.VSPackage.Visual_Studio_Integration;

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
    [ProvideAutoLoad("{f1536ef8-92ec-443c-9ed7-fdadf150da82}")]
    [Guid(GuidList.guidVSPackagePkgString)]
    public sealed class VSPackagePackage : Package
    {

        private string vccPath = @"C:\Users\t-chworr\VCC\vcc\Host\bin\Debug\vcc.exe";
        private IVsOutputWindowPane pane;

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

        private void VerifyActiveFile(object sender, EventArgs e)
        {
            //// get the toplevel object for interaction with VS
            DTE dte = MyDTE.Instance;

            //// Prepare VCC-Process, execute it and read its Output

            string arguments = dte.ActiveDocument.FullName;
            ProcessStartInfo psi = new ProcessStartInfo(vccPath, dte.ActiveDocument.FullName);
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.CreateNoWindow = true;
            System.Diagnostics.Process vccProcess = new System.Diagnostics.Process();
            vccProcess.StartInfo = psi;
            vccProcess.Start();
            vccProcess.BeginOutputReadLine();

            //// Clear Verification Outputpane
            pane.Clear();

            //// Get notified when VCC sends Output Data
            vccProcess.OutputDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
        }

        private void VerifyCurrentFunction(object sender, EventArgs e)
        {

            //// get the toplevel object for interaction with VS
            DTE dte = MyDTE.Instance;

            //// Prepare VCC-Process, execute it and read its Output
            
            string arguments = String.Format("/F:{0} {1}", GetCurrentFunctionName(), dte.ActiveDocument.FullName);
            ProcessStartInfo psi = new ProcessStartInfo(vccPath, arguments);
            
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.CreateNoWindow = true;
            System.Diagnostics.Process vccProcess = new System.Diagnostics.Process();
            vccProcess.StartInfo = psi;
            vccProcess.Start();
            vccProcess.BeginOutputReadLine();

            //// Clear Verification Outputpane
            pane.Clear();
            
            //// Write Commandline-Command to Verification Outputpane
            pane.OutputString(string.Format("{0} {1}\n", vccPath, arguments));

            //// Get notified when VCC sends Output Data
            vccProcess.OutputDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
            
        }

        private string GetCurrentFunctionName()
        {
            DTE dte = (DTE)Package.GetGlobalService(typeof(DTE));

            //// Get CodeElement which represents the current function
            TextDocument textDocument = (TextDocument)dte.ActiveDocument.Object(null);
            VirtualPoint currentFunctionActivePoint = textDocument.Selection.ActivePoint;
            CodeElement currentFunctionCodeElement = currentFunctionActivePoint.CodeElement[vsCMElement.vsCMElementFunction];
            if (currentFunctionCodeElement != null)
            {
                return currentFunctionCodeElement.Name;
            }
            else
            {
                return string.Empty;
            }
        }



        void vccProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            // Write Output from VCC to Verification Outputpane
            if (pane != null)
            {
                pane.OutputString(String.Format("{0}\n",e.Data));
            }
        }


        /////////////////////////////////////////////////////////////////////////////
        // Overriden Package Implementation
        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initilaization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {

            Trace.WriteLine (string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();


            //// Prepare Outputpane "Verification"

            IVsOutputWindow outputwindow = (IVsOutputWindow)Package.GetGlobalService(typeof(SVsOutputWindow));
            Guid guidVerificationPane = new Guid("{1EE5916F-A3C7-403C-89D8-58C61285688F}");

            outputwindow.CreatePane(ref guidVerificationPane, "Verification", 1, 1);

            outputwindow.GetPane(ref guidVerificationPane, out pane);


            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if ( null != mcs )
            {
                // Create the command for the menu items.
                CommandID menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidMyCommand);
                MenuCommand menuItem = new MenuCommand(ShowToolWindow, menuCommandID );
                mcs.AddCommand(menuItem);

                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyActiveFile);
                menuItem = new MenuCommand(VerifyActiveFile, menuCommandID);
                mcs.AddCommand(menuItem);

                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyCurrentFunction);
                OleMenuCommand OleMenuItem = new OleMenuCommand(VerifyCurrentFunction, menuCommandID);
                OleMenuItem.BeforeQueryStatus += new EventHandler(OleMenuItem_BeforeQueryStatus);
                mcs.AddCommand(OleMenuItem);

                menuCommandID = new CommandID(GuidList.guidVSPackageCmdSet, (int)PkgCmdIDList.cmdidVerifyMenu);
                OleMenuItem = new OleMenuCommand(VerivyMenuDoNothing, menuCommandID);
                OleMenuItem.BeforeQueryStatus +=new EventHandler(OleMenuItem_BeforeQueryStatusMenu);
                mcs.AddCommand(OleMenuItem);

            }
        }


        void VerivyMenuDoNothing(object sender, EventArgs e)
        {
        }

        void OleMenuItem_BeforeQueryStatusMenu(object sender, EventArgs e)
        {
            DTE dte = MyDTE.Instance;
            Document document = (Document)dte.ActiveDocument;
            if (document.Language == "C/C++")
            {
                ((MenuCommand)sender).Visible = true;
            }
            else
            {
                ((MenuCommand)sender).Visible = false;
            }
        }

        void OleMenuItem_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (GetCurrentFunctionName() == string.Empty)
                {
                    ((OleMenuCommand)sender).Enabled = false;
                }
                else
                {
                    ((OleMenuCommand)sender).Enabled = true;
                    ((OleMenuCommand)sender).Text = string.Format("Verify function '{0}'",GetCurrentFunctionName());
                }
            }
        }
        #endregion
    }
}
