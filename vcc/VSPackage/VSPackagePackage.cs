namespace Microsoft.Research.Vcc.VSPackage
{
    using System;
    using System.ComponentModel.Design;
    using System.Diagnostics;
    using System.Globalization;
    using System.IO;
    using System.Runtime.InteropServices;
    using System.Text.RegularExpressions;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;

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
    //// This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    //// a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]
    //// This attribute is used to register the informations needed to show the this package
    //// in the Help/About dialog of Visual Studio.
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    //// This attribute is needed to let the shell know that this package exposes some menus.
    [ProvideMenuResource("Menus.ctmenu", 1)]
    //// This attribute makes sure this package is loaded and initialized when a solution exists
    [ProvideOptionPage(typeof(VccOptionPage), "VCC", "General", 101, 106, true)]
    [ProvideAutoLoad("{f1536ef8-92ec-443c-9ed7-fdadf150da82}")]
    [ProvideToolWindow(typeof(BvdToolWindow))]
    [Guid(GuidList.GuidVSPackagePkgString)]
    public sealed class VSPackagePackage : Package
    {
        public static VSPackagePackage Instance { get; private set; }

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
        internal static string LastArguments { get; set; }

        internal static string LastFilename { get; set; }

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
            if (Instance != null)
            {
                throw new InvalidOperationException();
            }

            Instance = this;
            LastArguments = LastFilename = string.Empty;
        }

        #region Commands

        private void Options(object sender, EventArgs e)
        {
            ShowOptionPage(typeof(VccOptionPage));
        }

        private void CustomVerify(object sender, EventArgs e)
        {
            if (OptionPage != null)
            {
                if (!VSIntegration.DocumentsSavedCheck(OptionPage))
                {
                    return;
                }

                VCCLauncher.CustomVerify(VSIntegration.StartFileName, OptionPage);
            }
        }

        private void VerifyThis(object sender, EventArgs e)
        {
            if (OptionPage != null)
            {
                if (!VSIntegration.DocumentsSavedCheck(OptionPage))
                {
                    return;
                }

                VCCLauncher.VerifyThis(VSIntegration.StartFileName, VSIntegration.ActiveFileFullName, VSIntegration.CurrentLine, OptionPage);
            }
        }

        private static readonly Regex OutFileSettingRegex = new Regex(@"/(o|(out)):(?<outdir>([^\s]+))", RegexOptions.Compiled);

        private string AdjustFileNameToOutDirIfRequested(string fileName)
        {
            if (this.OptionPage.UseAdditionalCommandlineArguments)
            {
                var match = OutFileSettingRegex.Match(this.OptionPage.AdditionalCommandlineArguments);
                if (match.Success)
                {
                    return Path.Combine(match.Groups["outdir"].Value, Path.GetFileName(fileName));
                }
            }

            return fileName;
        }

        private void ShowErrorModel(object sender, EventArgs e)
        {
            var window = this.FindToolWindow(typeof(BvdToolWindow), 0, true);
            if ((null == window) || (null == window.Frame))
            {
                throw new NotSupportedException("Can not create BvdToolWindow");
            }

            string modelFileName = Path.ChangeExtension(VSIntegration.StartFileName, ".model");
            modelFileName = AdjustFileNameToOutDirIfRequested(modelFileName);
            if (!File.Exists(modelFileName))
            {
                throw new FileNotFoundException("Cannot find BVD model file.", modelFileName);
            }

            BvdToolWindow.BVD.ReadModels(modelFileName, VSIntegration.CurrentErrorModel);
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
            if (OptionPage != null)
            {
                if (!VSIntegration.DocumentsSavedCheck(OptionPage))
                {
                    return;
                }

                VCCLauncher.VerifyFile(VSIntegration.StartFileName, OptionPage);
            }
        }

        /// <summary>
        ///     Launches VCC.exe to verify the active file without includes
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void VerifyActiveFileWithoutIncludes(object sender, EventArgs e)
        {
            if (OptionPage != null)
            {
                if (!VSIntegration.DocumentsSavedCheck(OptionPage))
                {
                    return;
                }

                VCCLauncher.VerifyFileWithoutIncludes(VSIntegration.StartFileName, VSIntegration.ActiveFileFullName, OptionPage);
            }
        }

        private void ReVerify(object sender, EventArgs e)
        {
            if (OptionPage != null)
            {
                if (VSIntegration.DocumentsSavedCheck(OptionPage))
                {
                    VCCLauncher.LaunchVCC(LastFilename, LastArguments);
                }
            }
        }

        private static void Cancel(object sender, EventArgs e)
        {
            VCCLauncher.Cancel();
        }

        private void InsertForall(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("∀");
        }

        private void InsertExists(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("∃");
        }

        private void InsertLambda(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("λ");
        }

        private void InsertIn(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("∈");
        }

        private void InsertIntersection(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("∩");
        }

        private void InsertUnion(object sender, EventArgs e)
        {
            VSIntegration.InsertIntoCurrentDocument("∪");
        }

        #endregion

        #region BeforeQueryStatusHandlers
        //// All the handlers that handle the appearance of menus and menu entries are here

        /// <summary>
        ///     Verify Menu
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private static void VerifyMenu_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                ((MenuCommand)sender).Visible = VSIntegration.IsCodeFile;
            }
        }

        /// <summary>
        ///     Cancel
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private static void Cancel_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                OleMenuCommand menuCommand = (OleMenuCommand)sender;

                if (VCCLauncher.VCCRunning && VSIntegration.IsCodeFile)
                {
                    menuCommand.Visible = true;
                }
                else
                {
                    menuCommand.Visible = false;
                }
            }
        }

        /// <summary>
        ///     Show Error Model
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private static void ShowErrorModel_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                if (VSIntegration.IsCodeFile && !VCCLauncher.VCCRunning && VSIntegration.CurrentLineHasError)
                {
                    ((OleMenuCommand)sender).Visible = true;
                }
                else
                {
                    ((OleMenuCommand)sender).Visible = false;
                }
            }
        }

        private static void ReVerify_BeforeQueryStatus(object sender, EventArgs e)
        {
            var cmd = sender as OleMenuCommand;
            if (cmd == null)
            {
                return;
            }

            cmd.Enabled = !VCCLauncher.VCCRunning && !string.IsNullOrEmpty(LastFilename);
            cmd.Visible = VSIntegration.IsCodeFile;
        }

        private static void CheckCodeFileAndVccNotRunning(object sender, EventArgs e)
        {
            var cmd = sender as OleMenuCommand;
            if (cmd == null)
            {
                return;
            }

            cmd.Enabled = !VCCLauncher.VCCRunning;
            cmd.Visible = VSIntegration.IsCodeFile;
        }

        /// <summary>
        ///     Verify File
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private static void VerifyFile_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                ((OleMenuCommand)sender).Text = string.Format(CultureInfo.InvariantCulture, "Verify File: '{0}'", VSIntegration.ActiveFileName);
                CheckCodeFileAndVccNotRunning(sender, e);
            }
        }

        /// <summary>
        ///     Verify File Without Includes
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private static void VerifyFileWithoutIncludes_BeforeQueryStatus(object sender, EventArgs e)
        {
            if (sender != null)
            {
                ((OleMenuCommand)sender).Text = string.Format(CultureInfo.InvariantCulture, "Verify File Without Includes: '{0}'", VSIntegration.ActiveFileName);
                CheckCodeFileAndVccNotRunning(sender, e);
            }
        }

        #endregion

        private void RegisterCommand(OleMenuCommandService mcs, EventHandler invokeHandler, EventHandler beforeQueryStatusHandler, params uint[] cmdIds)
        {
            foreach (var cmdId in cmdIds)
            {
                var menuCommandID = new CommandID(GuidList.GuidVSPackageCmdSet, (int)cmdId);
                var menuCommand = new OleMenuCommand(invokeHandler, menuCommandID);
                if (beforeQueryStatusHandler != null)
                {
                    menuCommand.BeforeQueryStatus += beforeQueryStatusHandler;
                }

                mcs.AddCommand(menuCommand);
            }
        }

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
                this.RegisterCommand(mcs, VerifyActiveFile, VerifyFile_BeforeQueryStatus, PkgCmdIDList.CmdidVerifyActiveFile, PkgCmdIDList.CmdidContextVerifyActiveFile);
                this.RegisterCommand(mcs, VerifyActiveFileWithoutIncludes, VerifyFileWithoutIncludes_BeforeQueryStatus, PkgCmdIDList.CmdidVerifyActiveFileWithoutIncludes, PkgCmdIDList.CmdidContextVerifyActiveFileWithoutIncludes);
                this.RegisterCommand(mcs, ReVerify, ReVerify_BeforeQueryStatus, PkgCmdIDList.CmdidReVerify, PkgCmdIDList.CmdidContextReVerify);
                this.RegisterCommand(mcs, VerifyThis, CheckCodeFileAndVccNotRunning, PkgCmdIDList.CmdidVerifyThis, PkgCmdIDList.CmdidContextVerifyThis);
                this.RegisterCommand(mcs, CustomVerify, CheckCodeFileAndVccNotRunning, PkgCmdIDList.CmdidCustomVerify, PkgCmdIDList.CmdidContextCustomVerify);
                this.RegisterCommand(mcs, Cancel, Cancel_BeforeQueryStatus, PkgCmdIDList.CmdidCancel, PkgCmdIDList.CmdidContextCancel);
                this.RegisterCommand(mcs, Options, null, PkgCmdIDList.CmdidOptions);
                this.RegisterCommand(mcs, ShowErrorModel, ShowErrorModel_BeforeQueryStatus, PkgCmdIDList.CmdidShowErrorModel);
                this.RegisterCommand(mcs, null, VerifyMenu_BeforeQueryStatus, PkgCmdIDList.CmdidVerifyMenu);
                this.RegisterCommand(mcs, InsertForall, null, PkgCmdIDList.CmdidMathSymbolForall);
                this.RegisterCommand(mcs, InsertExists, null, PkgCmdIDList.CmdidMathSymbolExists);
                this.RegisterCommand(mcs, InsertLambda, null, PkgCmdIDList.CmdidMathSymbolLambda);
                this.RegisterCommand(mcs, InsertIn, null, PkgCmdIDList.CmdidMathSymbolIn);
                this.RegisterCommand(mcs, InsertIntersection, null, PkgCmdIDList.CmdidMathSymbolIntersection);
                this.RegisterCommand(mcs, InsertUnion, null, PkgCmdIDList.CmdidMathSymbolUnion);
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
