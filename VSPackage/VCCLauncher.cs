using System;
using Microsoft.VisualStudio.Shell.Interop;
using EnvDTE;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;
using Process = System.Diagnostics.Process;

namespace MicrosoftResearch.VSPackage
{
    public static class VCCLauncher
    {
        private static string vccPath = @"C:\Users\t-chworr\VCC\vcc\Host\bin\Debug\vcc.exe";
        
        private static Lazy<IVsOutputWindowPane> _pane = new Lazy<IVsOutputWindowPane>(() =>
        {
            IVsOutputWindow outputwindow = (IVsOutputWindow)Package.GetGlobalService(typeof(SVsOutputWindow));
            Guid guidVerificationPane = new Guid("{1EE5916F-A3C7-403C-89D8-58C61285688F}");
            IVsOutputWindowPane result;
            
            outputwindow.CreatePane(ref guidVerificationPane, "Verification", 1, 1);
            outputwindow.GetPane(ref guidVerificationPane, out result);
            return result;
        });

        static IVsOutputWindowPane pane
        {
            get { return _pane.Value; }
        }
        
        public static void LaunchVCC(string arguments)
        {
            //// get the toplevel object for interaction with VS
            DTE dte = VSIntegration.dte;

            //// Prepare VCC-Process, execute it and read its Output
            ProcessStartInfo psi = new ProcessStartInfo(string.Format("\"{0}\"",vccPath), arguments);
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.CreateNoWindow = true;
            Process vccProcess = new Process();
            vccProcess.StartInfo = psi;

            //// Clear Verification Outputpane
            pane.Clear();
            pane.OutputString("===VCC started.===\n");
            //// Write Commandline-Command to Verification Outputpane
            pane.OutputString(string.Format("Command Line: {0} {1}\n\n", vccPath, arguments));
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

        static void vccProcess_Exited(object sender, EventArgs e)
        {
            pane.OutputString("===VCC finished.===\n");
        }

        static void vccProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            // Write Output from VCC to Verification Outputpane
            if (pane != null)
            {
                pane.OutputString(String.Format("{0}\n", e.Data));
            }
        }
    }
}
