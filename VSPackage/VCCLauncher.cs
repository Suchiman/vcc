using System;
using System.Diagnostics;
using Process = System.Diagnostics.Process;
using Thread = System.Threading.Thread;
using System.Management;

namespace MicrosoftResearch.VSPackage
{
    /// <summary>
    ///     This class is used to start VCC.exe with the correct parameters and to get the results from VCC.exe
    /// </summary>
    internal static class VCCLauncher
    {
        private static string vccPath = @"C:\Users\t-chworr\VCC\vcc\Host\bin\Debug\vcc.exe";
        private static Process vccProcess;

        internal static void VerifyFile(string filename, VccOptionPage options)
        {
            string addArguments = options.UseAdditionalCommandlineArguments ? options.AdditionalCommandlineArguments + " " : string.Empty;
            LaunchVCC(String.Format("{0}\"{1}\"", addArguments, filename));
        }

        internal static void VerifyFunction(string filename, string function, VccOptionPage options)
        {
            string addArguments = options.UseAdditionalCommandlineArguments ?
                options.AdditionalCommandlineArguments + " " :
                string.Empty;
            LaunchVCC(String.Format("{0} /f:\"{1}\" \"{2}\"", addArguments, function, filename));
        }

        internal static void LaunchVCC(string arguments)
        {
            //// Prepare VCC-Process, execute it and read its Output            
            ProcessStartInfo psi = new ProcessStartInfo(string.Format("\"{0}\"",vccPath), arguments);
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.CreateNoWindow = true;
            vccProcess = new Process();
            vccProcess.StartInfo = psi;

            //// Clear Verification Outputpane
            VSIntegration.ClearPane();
            VSIntegration.WriteToPane("===VCC started.===\n");
            //// Write Commandline-Command to Verification Outputpane
            VSIntegration.WriteToPane(string.Format("Command Line: \"{0}\" {1}\n\n", vccPath, arguments));
            //// Get notified when VCC sends Output or Error Data
            vccProcess.OutputDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
            vccProcess.ErrorDataReceived += new DataReceivedEventHandler(vccProcess_OutputDataReceived);
            //// Get notified when VCC-Process finishes.
            vccProcess.EnableRaisingEvents = true;
            vccProcess.Exited += new EventHandler(vccProcess_Exited);

            //// Finally start the process
            vccProcess.Start();
            vccProcess.BeginOutputReadLine();
        }

        private static void vccProcess_Exited(object sender, EventArgs e)
        {
            if (vccProcess != null && vccProcess.ExitCode == -1)
            {
                VSIntegration.WriteToPane("===VCC was canceled.===\n");
            }
            else
            {
                VSIntegration.WriteToPane("===VCC finished.===\n");
            }
        }

        private static void vccProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            // Write Output from VCC to Verification Outputpane
            if (VCCRunning())
            {
                VSIntegration.WriteToPane(String.Format("{0}\n", e.Data));
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
            if (VCCRunning())
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

        internal static bool VCCRunning()
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
