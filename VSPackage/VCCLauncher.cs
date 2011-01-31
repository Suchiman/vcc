using System;
using System.Diagnostics;
using Process = System.Diagnostics.Process;
using System.Management;
using System.Text.RegularExpressions;
using Thread = System.Threading.Thread;
using Microsoft.Win32;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualBasic;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     This class is used to start VCC.exe with the correct parameters and to get the results from VCC.exe
    /// </summary>
    internal static class VCCLauncher
    {
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
        //// This is set to true, when Verification fails.
        private static bool errorOccurred = false;
        private static Regex VCCErrorRegEx =
            new Regex(@"(?<path>(.*?))\(((?<line>([0-9]+))|(?<line>([0-9]+)),(?<column>([0-9]+)))\)\s:(\s(.*?):)?\s(?<errormessage>(.*))");

        internal static void CustomVerify(string filename, VccOptionPage options)
        {
            string addArguments = options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments + " " :
                string.Empty;
            if (options.ShowZ3Inspector)
            {
                addArguments += "/i ";
            }
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
            string addArguments = options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments + " " :
                string.Empty;
            if (options.ShowZ3Inspector)
            {
                addArguments += "/i ";
            }
            LaunchVCC(String.Format("{0}\"{1}\"", addArguments, filename));
        }

        internal static void VerifyFunction(string filename, string function, VccOptionPage options)
        {
            string addArguments = options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments + " " :
                string.Empty;
            if (options.ShowZ3Inspector)
            {
                addArguments += "/i ";
            }
            LaunchVCC(String.Format("{0} /f:\"{1}\" \"{2}\"", addArguments, function, filename));
        }

        internal static void LaunchVCC(string arguments)
        {
            errorOccurred = false;

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
            vccProcess.OutputDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
            vccProcess.ErrorDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
            //// Get notified when VCC-Process finishes.
            vccProcess.EnableRaisingEvents = true;
            vccProcess.Exited += new EventHandler(vccProcess_Exited);

            //// Finally start the process
            try
            {
                vccProcess.Start();
                vccProcess.BeginOutputReadLine();
                vccProcess.BeginErrorReadLine();
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
                    case 1:
                    case 3:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("\n===Verification failed.===\n");
                        VSIntegration.updateStatus("Verification failed.", false);
                        break;
                    default:
                        Thread.Sleep(1000);
                        VSIntegration.WriteToPane("\n===VCC finished with unknown exitcode.===\n");
                        VSIntegration.WriteToPane(vccProcess.ExitCode.ToString() + "\n");
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
                                                        Int32.Parse(match.Groups["line"].Value)
                                                        );

                }
                else if (!e.Data.StartsWith("Exiting"))
                {
                    //// This line is not an errormessage.
                    VSIntegration.WriteToPane(String.Format("{0}\n", e.Data));
                }
            }
        }

        private static int GetParentProcess(int Id)
        {
            int parentPid = 0;
            using (ManagementObject mo = new ManagementObject("win32_process.handle='" + Id.ToString() + "'"))
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

        /// <summary>
        ///     Cancels the running VCC Process, if it exists
        /// </summary>
        internal static void Cancel()
        {
            if (VCCRunning)
            {
                try
                {
                    vccProcess.Kill();
                    foreach (Process subProcess in Process.GetProcesses())
                    {
                        if (GetParentProcess(subProcess.Id) == vccProcess.Id)
                        {
                            subProcess.Kill();
                        }//if
                    }//foreach
                }//try
                catch
                {
                    VSIntegration.WriteToPane("Canceling VCC failed.");
                }//catch
            }//if
        }//method

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
    }
}
