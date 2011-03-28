using System;
using System.Diagnostics;
using Process = System.Diagnostics.Process;
using System.Management;
using System.Text.RegularExpressions;
using Thread = System.Threading.Thread;
using Microsoft.Win32;
using System.Windows.Forms;
using Microsoft.VisualBasic;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     This class is used to start VCC.exe with the correct parameters and to get the results from VCC.exe
    /// </summary>
    internal static class VCCLauncher
    {
        
        #region commands

        private static string GetAddArguments(VccOptionPage options)
        {
            string result = options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments + " " :
                string.Empty;
            if (options.ShowZ3Inspector)
            {
                result += "/i ";
            }

            return result;
        }
        
        internal static void VerifyThis(string filename, string selection, int line, VccOptionPage options)
        {
            string addArguments = GetAddArguments(options);

            if (selection == string.Empty)
            {
                addArguments += String.Format("/loc:\"{0}\":{1} ", filename, line);
            }
            else
            {
                addArguments += String.Format("/f:{0} ", selection);
            }
            LaunchVCC(String.Format("{0}\"{1}\"", addArguments, filename));
        }

        internal static void CustomVerify(string filename, VccOptionPage options)
        {
            string addArguments = GetAddArguments(options);

            string userInput = Interaction.InputBox("Commandline arguments for vcc.exe:", "Custom Verify", addArguments) + " ";
            if (userInput == " ")
            {
                if (MessageBox.Show("Do you want to start verification without additional commandline arguments?",
                                    "Custom Verify",
                                    MessageBoxButtons.YesNo,MessageBoxIcon.Question,MessageBoxDefaultButton.Button1)
                                    == DialogResult.No)
                {
                    return;
                }
            }
            addArguments = userInput;
            LaunchVCC(String.Format("{0}\"{1}\"", addArguments, filename));
        }

        internal static void VerifyFile(string filename, VccOptionPage options)
        {
            string addArguments = GetAddArguments(options);
            LaunchVCC(String.Format("{0}\"{1}\"", addArguments, filename));
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
                if (VSPackagePackage.Instance.OptionPage.VccExecutableFolder == string.Empty)
                {
                    using (RegistryKey key = Registry.LocalMachine.OpenSubKey(@"Software\Microsoft Research\Vcc", false))
                    {
                        if (key != null)
                        {
                            string result = key.GetValue("vccExecutablePath") as string;
                            if (result != null)
                            {
                                return result;
                            }
                            else
                            {
                                return "vcc.exe";
                            }
                        }
                        else
                        {
                            return "vcc.exe";
                        }
                    }
                }
                else
                {
                    return VSPackagePackage.Instance.OptionPage.VccExecutableFolder + "\\vcc.exe";
                }
            }
        }

        private static Process vccProcess;

        internal static void LaunchVCC(string arguments)
        {
            errorOccurred = false;
            warningOccured = false;

            VSIntegration.initializeErrorList();
            VSIntegration.updateStatus("Verifying...", true);

            //// Prepare VCC-Process, execute it and read its Output            
            ProcessStartInfo psi = new ProcessStartInfo(string.Format("\"{0}\"", VccPath), arguments);
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.RedirectStandardError = true;
            psi.CreateNoWindow = true;
            vccProcess = new Process();
            vccProcess.StartInfo = psi;

            //// Clear Verification Outputpane
            VSIntegration.ClearPane();
            VSIntegration.WriteToPane("===VCC started.===\n");
            //// Write Commandline-Command to Verification Outputpane
            VSIntegration.WriteToPane(string.Format("Command Line: \"{0}\" {1}\n\n", VccPath, arguments));
            //// Get notified when VCC sends Output or Error Data
            vccProcess.OutputDataReceived += vccProcess_OutputDataReceived;
            vccProcess.ErrorDataReceived += vccProcess_OutputDataReceived;
            //// Get notified when VCC-Process finishes.
            vccProcess.EnableRaisingEvents = true;
            vccProcess.Exited += vccProcess_Exited;

            //// Finally start the process
            try
            {
                vccProcess.Start();
                vccProcess.BeginOutputReadLine();
                vccProcess.BeginErrorReadLine();
                //// When the process was started, remember the cmdlinearguments
                VSPackagePackage.LastAction = arguments;
            }
            catch (Exception)
            {
                vccProcess = null;
                VSIntegration.WriteToPane("Executing\n" + VccPath + "\nfailed.\n"
                    + "You can specify the folder in which the vcc executable is located in Tools/Options/Vcc.");
                VSIntegration.WriteToPane("\n===Verification failed.===\n");
                VSIntegration.updateStatus("Verification failed.", false);
            }
        }

        /// <summary>
        ///     Cancels the running VCC Process, if it exists
        /// </summary>
        internal static void Cancel()
        {
            if (VCCRunning)
            {
                int vccProcess_Id = vccProcess.Id;
                try
                {
                    vccProcess.Kill();
                }//try
                catch
                {
                    VSIntegration.WriteToPane("Canceling VCC failed.");
                }//catch

                foreach (Process subProcess in Process.GetProcesses())
                {
                    if (GetParentProcess(subProcess.Id) == vccProcess_Id)
                    {
                        try
                        {
                            subProcess.Kill();
                        }
                        catch
                        {
                            VSIntegration.WriteToPane("Canceling a subprocess of VCC failed.");
                        }
                    }//if
                }//foreach
            }//if
        }//method

        private static int GetParentProcess(int Id)
        {
            int parentPid = 0;
            using (ManagementObject mo = new ManagementObject("win32_process.handle='" + Id + "'"))
            {

                try
                {
                    mo.Get();
                    parentPid = Convert.ToInt32(mo["ParentProcessId"]);
                }
                catch { }
            }
            return parentPid;
        }

        #endregion

        #region process observation
        
        //// This is set to true, when Verification fails.
        private static bool errorOccurred;
        private static bool warningOccured;
        private static readonly Regex VCCErrorRegEx =
            new Regex(@"(?<path>(.*?))\(((?<line>([0-9]+))|(?<line>([0-9]+)),(?<column>([0-9]+)))\)\s:\s(((error\s(.*?):)\s(?<errormessage>(.*)))|(?<errormessage>\(Location of symbol related to previous error.\)))");
        private static readonly Regex VCCWarningRegEx =
            new Regex(@"(?<path>(.*?))\(((?<line>([0-9]+))|(?<line>([0-9]+)),(?<column>([0-9]+)))\)\s:(\swarning\s(.*?):)\s(?<errormessage>(.*))");

        private static void vccProcess_Exited(object sender, EventArgs e)
        {
            if (vccProcess != null)
            {
                switch (vccProcess.ExitCode)
                {
                    case -1:
                        vccProcess.CancelOutputRead();
                        vccProcess.CancelErrorRead();
                        vccProcess = null;
                        VSIntegration.WriteToPane("\n===VCC was canceled.===\n");
                        VSIntegration.updateStatus("Verification canceled.",false);
                        break;
                    case 0:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("\n===Verification succeeded.===\n");
                        VSIntegration.updateStatus("Verification succeeded.", false);
                        break;
                    case 2:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("Incorrect Commandline Arguments were used.\n");
                        VSIntegration.WriteToPane("\n===Verification failed.===\n");
                        VSIntegration.updateStatus("Verification failed.", false);
                        break;
                    case 1:
                    case 3:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("\n===Verification failed.===\n");
                        VSIntegration.updateStatus("Verification failed.", false);
                        break;
                    default:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("\n===VCC finished with unknown exitcode.===\n");
                        VSIntegration.WriteToPane(vccProcess.ExitCode + "\n");
                        break;
                }
            }
        }

        private static void vccProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            //// Write Output from VCC to Verification Outputpane
            if (e != null && e.Data != null)
            {
                if (VCCErrorRegEx.IsMatch(e.Data))
                {
                    //// This line is an errormessage.
                    if (!errorOccurred)
                    {
                        VSIntegration.WriteToPane("\nAn Error occured. See Error List for details.\n\n");
                        errorOccurred = true;
                    }

                    //// Add error to error list
                    Match match = VCCErrorRegEx.Match(e.Data);
                    VSIntegration.addErrorToErrorList(  match.Groups["path"].Value,
                                                        match.Groups["errormessage"].Value,
                                                        Int32.Parse(match.Groups["line"].Value),
                                                        Microsoft.VisualStudio.Shell.TaskErrorCategory.Error
                                                        );

                }
                else if (VCCWarningRegEx.IsMatch(e.Data))
                {
                    //// This line is a warning.
                    if (!errorOccurred && !warningOccured)
                    {
                        VSIntegration.WriteToPane("\nA warning occured. See Error List for details. (You can hide warnings in Tools/Options/Vcc)\n\n");
                        warningOccured = true;
                    }

                    //// Add warning to error list
                    Match match = VCCWarningRegEx.Match(e.Data);
                    VSIntegration.addErrorToErrorList(match.Groups["path"].Value,
                                                            match.Groups["errormessage"].Value,
                                                            Int32.Parse(match.Groups["line"].Value),
                                                            Microsoft.VisualStudio.Shell.TaskErrorCategory.Warning
                                                            );
                }
                else if (!e.Data.StartsWith("Exiting"))
                {
                    //// This line is not an errormessage.
                    VSIntegration.WriteToPane(String.Format("{0}\n", e.Data));
                }
            }
        }

        internal static bool VCCRunning
        {
            get
            {
                if (vccProcess != null && !vccProcess.HasExited)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        #endregion

    }
}
