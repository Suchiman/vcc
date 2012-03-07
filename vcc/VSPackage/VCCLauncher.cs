namespace Microsoft.Research.Vcc.VSPackage
{
    using System;
    using System.IO;
    using System.Reflection;
    using System.Text.RegularExpressions;
    using System.Threading;
    using System.Windows.Forms;
    using Microsoft.Win32;

    /// <summary>
    ///     This class is used to start VCC.exe with the correct parameters and to get the results from VCC.exe
    /// </summary>
    internal static class VCCLauncher
    {
        private static NotifyIcon notifyIcon;

        internal static string GetVccVersion()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            return assembly.GetName().Version.ToString();
        }

        private static string GetVSCOMNTOOLS()
        {
            string Version = VSIntegration.DTE.Version;      //returns something like 8.0
            string CleanVersionTag = Version.Replace(".", "");
            string VSDir = Environment.GetEnvironmentVariable(String.Format("VS{0}COMNTOOLS", CleanVersionTag));
            return VSDir;
        }

        internal static string GetCLPath(string ActivePlatform)
        {
            string vsToolDir = GetVSCOMNTOOLS();
            if (!String.IsNullOrEmpty(vsToolDir))
            {
                string CL = (ActivePlatform == "x64") ? "VC\\bin\\x86_amd64\\cl.exe" : "VC\\bin\\cl.exe";
                return vsToolDir.ToUpperInvariant().Replace("COMMON7\\TOOLS\\", CL);
            }
            else return null;
        }

        #region commands

        private static string GetArgumentsFromOptions(VccOptionPage options, bool respectCustomFlag)
        {
            string result = !respectCustomFlag || options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments :
                string.Empty;

            if (options.ShowZ3Inspector)
            {
                result += " /i";
            }

            result += " /bvd";

            return result;
        }

        internal static void VerifyThis(string filename, string currentFile, int line, VccOptionPage options)
        {
            string addArguments = GetArgumentsFromOptions(options, true);
            addArguments += String.Format(" /loc:\"{0}\":{1} ", currentFile, line);
            LaunchVCC(filename, addArguments);
        }

        internal static void CustomVerify(string filename, VccOptionPage options)
        {
            using (var customVerifyForm = new CustomVerifyForm(GetArgumentsFromOptions(options, false)))
            {
                if (customVerifyForm.ShowDialog() == DialogResult.OK)
                {
                    LaunchVCC(filename, customVerifyForm.Arguments);
                }
            }
        }

        internal static void VerifyFile(string filename, VccOptionPage options)
        {
            string addArguments = GetArgumentsFromOptions(options, true);
            LaunchVCC(filename, addArguments);
        }

        internal static void VerifyFileWithoutIncludes(string filename, string currentFile, VccOptionPage options)
        {
            string addArguments = GetArgumentsFromOptions(options, true);
            addArguments += " /ii:" + currentFile;
            LaunchVCC(filename, addArguments);
        }

        #endregion

        #region process handling

        /// <summary>
        ///     This string contains the Path of the Vcc-Executable.
        ///     User Input in Tools/Options/Vcc > Registry Entry > "vcc.exe"
        /// </summary>
        private static string VccPath
        {
            get
            {
                if (String.IsNullOrWhiteSpace(VSPackagePackage.Instance.OptionPage.VccExecutableFolder))
                {
                    using (var key = Registry.LocalMachine.OpenSubKey(@"Software\Microsoft Research\Vcc", false))
                    {
                        if (key != null)
                        {
                            return key.GetValue("vccExecutablePath", "vcc.exe") as string;
                        }
                        else
                        {
                            return "vcc.exe";
                        }
                    }
                }
                else
                {
                    return Path.Combine(VSPackagePackage.Instance.OptionPage.VccExecutableFolder, "vcc.exe");
                }
            }
        }

        internal static void LaunchVCC(string filename, string vccargs)
        {
            vccargs += " ";
            vccargs += VSIntegration.CurrentCompilerSettings.ToVccOptions();
            var clPath = GetCLPath(VSIntegration.CurrentPlatform);
            if (clPath != null) vccargs += String.Format(" /clpath:\"{0}\"", clPath);

            VSIntegration.ClearErrorsAndMarkers();
            VSIntegration.UpdateStatus("Verifying...", true);

            //// Clear Verification Outputpane
            VSIntegration.ClearAndShowPane();
            VSIntegration.WriteToPane("=== VCC started. ===");
            //// Write Commandline-Command to Verification Outputpane
            VSIntegration.WriteToPane(string.Format("Command Line: \"{0}\" {1}\n", VccPath, vccargs));
            //// Get notified when VCC-Process finishes.

            //// Start the process
            try
            {
                VCCRunning = true;

                // Get CL path //TODO generic clpath
                string cl11Path = Path.Combine(Environment.CurrentDirectory, "..\\..\\..\\..\\..\\..\\..\\Ext\\VS11\\cl.exe");

                // Subscribe to message events
                Utils.VccppEvent += new EventHandler<VccppEventArgs>(vccProcess_OutputDataReceived);

                // TODO vccargs
                int exitCode = VccppMain.ProcessFile(filename, cl11Path, null, vccargs, false);
                vccProcess_Exited(exitCode);

                //// When the process was started, remember the cmdlinearguments
                VSPackagePackage.LastArguments = vccargs;
                VSPackagePackage.LastFilename = filename;
            }
            catch (Exception)
            {
                VSIntegration.WriteToPane("Executing\n" + VccPath + "\nfailed.\n"
                    + "You can specify the folder in which the VCC executable is located in the menu under Verify/Vcc Options/Vcc/General.");
                VSIntegration.WriteToPane("=== Verification failed. ===");
                VSIntegration.UpdateStatus("Verification failed.", false);
            }
        }

        /// <summary>
        ///     Cancels the running VCC Process, if it exists
        /// </summary>
        internal static void Cancel()
        {
            if (VCCRunning)
            {
                // vccProcess_Exited(-1); TODO
            }
        }

        #endregion

        #region process observation

        //// This is set to true, when Verification fails.
        private static readonly Regex VCCErrorRegEx =
            new Regex(@"(?<path>(.*?))\(((?<line>([0-9]+))|(?<line>([0-9]+)),(?<column>([0-9]+)))\)\s:\s(((error\s(.*?):)\s(?<errormessage>(.*)))|(?<errormessage>\(Location of symbol related to previous error.\)))");
        private static readonly Regex VCCWarningRegEx =
            new Regex(@"(?<path>(.*?))\(((?<line>([0-9]+))|(?<line>([0-9]+)),(?<column>([0-9]+)))\)\s:(\swarning\s(.*?):)\s(?<errormessage>(.*))");

        private static void vccProcess_Exited(int exitCode)
        {
            VCCRunning = false;

            if (VSPackagePackage.Instance.OptionPage.ShowNotifications &&
                new IntPtr(VSIntegration.DTE.MainWindow.HWnd) != NativeMethods.GetForegroundWindow())
            {
                if (notifyIcon == null)
                {
                    notifyIcon = new NotifyIcon();
                    notifyIcon.Icon = VSPackage.Resources.VccIcon;
                    notifyIcon.Visible = true;
                    notifyIcon.Text = "Vcc " + GetVccVersion();
                }

                if (exitCode == 0)
                {
                    notifyIcon.ShowBalloonTip(4000, "Verification succeeded!", "Verification run completed successfully.", ToolTipIcon.Info);
                }
                else
                {
                    notifyIcon.ShowBalloonTip(4000, "Verification failed!", "Verification run completed with errors.", ToolTipIcon.Error);
                }
            }

            switch (exitCode)
            {
                case -1:
                    VSIntegration.WriteToPane("\n=== VCC was canceled. ===");
                    VSIntegration.UpdateStatus("Verification canceled.", false);
                    break;
                case 0:
                    Thread.Sleep(1000);
                    VSIntegration.WriteToPane("\n=== Verification succeeded. ===");
                    VSIntegration.UpdateStatus("Verification succeeded.", false);
                    break;
                case 2:
                    Thread.Sleep(1000);
                    VSIntegration.WriteToPane("Incorrect commandline arguments were used.");
                    VSIntegration.WriteToPane("\n=== Verification failed. ===\n");
                    VSIntegration.UpdateStatus("Verification failed.", false);
                    break;
                case 1:
                case 3:
                    Thread.Sleep(1000);
                    VSIntegration.WriteToPane("\n=== Verification failed. ===");
                    VSIntegration.UpdateStatus("Verification failed.", false);
                    break;
                default:
                    Thread.Sleep(1000);
                    VSIntegration.WriteToPane("\n=== VCC finished with unknown exitcode. ===");
                    VSIntegration.WriteToPane(exitCode.ToString());
                    break;
            }
        }

        private static void vccProcess_OutputDataReceived(object sender, VccppEventArgs e)
        {
            //// Write Output from VCC to Verification Outputpane
            if (e != null && !String.IsNullOrEmpty(e.Message))
            {
                VSIntegration.WriteToPane(e.Message);

                Match match;
                if ((match = VCCErrorRegEx.Match(e.Message)).Success)
                {
                    //// Add error to error list
                    VSIntegration.AddErrorToErrorList(
                      match.Groups["path"].Value,
                      match.Groups["errormessage"].Value,
                      Int32.Parse(match.Groups["line"].Value),
                      Microsoft.VisualStudio.Shell.TaskErrorCategory.Error);
                }
                else if ((match = VCCWarningRegEx.Match(e.Message)).Success)
                {
                    //// Add warning to error list
                    VSIntegration.AddErrorToErrorList(
                      match.Groups["path"].Value,
                      match.Groups["errormessage"].Value,
                      Int32.Parse(match.Groups["line"].Value),
                      Microsoft.VisualStudio.Shell.TaskErrorCategory.Warning);
                }
            }
        }

        internal static bool VCCRunning { get; private set; }

        #endregion
    }
}
