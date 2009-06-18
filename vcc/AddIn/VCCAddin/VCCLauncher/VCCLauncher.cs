//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Diagnostics;
using System.IO;
using System.Management;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Marker;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;
using Process=System.Diagnostics.Process;
using Thread=System.Threading.Thread;

namespace VerifiedCCompilerAddin {
  
  public static class VCCLauncher {

    [DllImport("user32.dll")]
    static extern IntPtr GetActiveWindow();

    [DllImport("user32.dll")]
    static extern IntPtr GetForegroundWindow(); 


    /// <summary>
    /// Process ID of a running VCC instance
    /// </summary>
    //C:\hyperv\base\vm\inc\HtPrivateTypes.h(34) : fatal error C1083: Cannot open include file: 'SpecStrings.h': No such file or directory
    //([a-zA-Z\\.:]+)\(([0-9]+|([0-9]+),([0-9]+))\)\s:\s(.*?):\s(.*)    
    private static Regex VCCErrorRegEx = new Regex(@"(.*?)\(([0-9]+|([0-9]+),([0-9]+))\)\s:\s(.*?):\s(.*)");
    
    #region Properties

    private static Process _VCCProcess = null;
    public static Process VCCProcess {
      get { return _VCCProcess; }
    }

    private static string _VCCExecutable = String.Empty;
    public static string VCCExecuteable
    {
      get {
          _VCCExecutable = FindVCCExecutable();
          return _VCCExecutable;        
      }      
    }



    private static string FindVCCExecutable()
    {
      System.Reflection.Assembly asm = System.Reflection.Assembly.GetExecutingAssembly();
      string RootInstallPath = Directory.GetParent(Path.GetDirectoryName(asm.Location)).FullName;

      string ExeName = "vcc.exe";

      FileInfo fi = new FileInfo(Path.Combine(RootInstallPath, "binaries") + "\\" + ExeName);
      if (fi.Exists)
        return fi.FullName;
      else
        return ExeName;
    } 
    
    #endregion
    
    
    /// <summary>
    /// Starts a VCC process in a with a given arguments and workingdirectory.
    /// </summary>
    /// <param name="Arguments">Arguments for VCCExecutable</param>
    /// <param name="workdir">Working Directory, in with VCCExecutable should be executed</param>
    /// <param name="job">The current job we work on</param>
    /// <returns>If WaitForExit, than it returns the exitcode else the process id</returns>
    public static int LaunchVCC(string Arguments, string workdir, bool WaitForExit, VerifyJob job)
    {
      
      // Remember lastetst Workingdirectory for ModelViewer.
      AddInGlobals.LastestWorkDir = workdir;
      
      ProcessStartInfo pStartInfo = new ProcessStartInfo();
      pStartInfo.FileName = VCCExecuteable;
      pStartInfo.Arguments = Arguments;
      pStartInfo.WorkingDirectory = workdir;
      pStartInfo.RedirectStandardOutput = true;
      pStartInfo.RedirectStandardError = true;
      pStartInfo.UseShellExecute = false;
      pStartInfo.WindowStyle = ProcessWindowStyle.Hidden;
      pStartInfo.CreateNoWindow = true;

      //Add IDE PATH for CL Tool, to find mspdb80.dll
      string IDEPath = Utilities.GetIDEPath();
      if( pStartInfo.EnvironmentVariables["PATH"].EndsWith(";")) {
      pStartInfo.EnvironmentVariables["PATH"] += "" + IDEPath; } 
      else {
      pStartInfo.EnvironmentVariables["PATH"] += ";" + IDEPath;
      }

      _VCCProcess = new Process();
      _VCCProcess.StartInfo = pStartInfo;
      _VCCProcess.OutputDataReceived += new DataReceivedEventHandler(vp_OutputDataReceived);
      _VCCProcess.ErrorDataReceived += new DataReceivedEventHandler(vp_ErrorDataReceived);
     
      AddInGlobals.BuildPane.OutputString("=============== VCC starts ==============="+ Environment.NewLine);
      AddInGlobals.BuildPane.OutputString(String.Format("set PATH={0}{1}", pStartInfo.EnvironmentVariables["PATH"], Environment.NewLine));
      AddInGlobals.BuildPane.OutputString(String.Format("executing vcc in directory: {0}{1}", workdir, Environment.NewLine));
      AddInGlobals.BuildPane.OutputString(String.Format("{0} {1}{2}", VCCExecuteable, Arguments, Environment.NewLine));
      
      try { 
        _VCCProcess.Start(); 
      }
      catch (Exception ex)
        {
        AddInGlobals.BuildPane.OutputString("VCC can't be executed." + Environment.NewLine + ex.Message + Environment.NewLine);
        AddInGlobals.BuildPane.OutputString("=============== VCC ends ================" + Environment.NewLine);
        return -1;
      }

      _VCCProcess.BeginOutputReadLine();
      if (WaitForExit) {
        _VCCProcess.WaitForExit();
        AddInGlobals.BuildPane.OutputString("=============== VCC ends ================" + Environment.NewLine);
        AddInGlobals.BuildPane.ForceItemsToTaskList();
        
        string message;
                    
        if (job.FunctionToVerify != String.Empty)
        {
          message = String.Format("Function '{0}'", job.FunctionToVerify);
        }
        else
        {
          message = String.Format("File '{0}'", job.FullFileName);
        }

        IntPtr hwnd = new IntPtr(AddInGlobals.DTE.MainWindow.HWnd);
        IntPtr awnd = GetForegroundWindow();

        if ((hwnd != awnd) && AddinSettingsManager.ShowBallonTip)
        {
          if (_VCCProcess.ExitCode == 0)
          {
            Utilities.ReportSuccess(message);
          }
          else
          {
            Utilities.ReportFailure(message);
          }
        }

        return _VCCProcess.ExitCode;
      }

      return _VCCProcess.Id;
    }

    /// <summary>
    /// Callback function from LauncVCC Process, that delivers the stderr of the running process
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    static void vp_ErrorDataReceived(object sender, DataReceivedEventArgs e) {
      if (e.Data != null)
        AddInGlobals.BuildPane.OutputString(e.Data);
    }

    /// <summary>
    /// Callback function from LauncVCC Process, taht delivers the stdout of the running process
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    static void vp_OutputDataReceived(object sender, DataReceivedEventArgs e)
    {
      
      if (e.Data == null)   //if null when cmd is closed! Last Call!
        return;

      string Output = e.Data;

      //If Output starts with \\ then it means a servername so add two more \\ to the output
      if (Output.StartsWith("\\"))
        Output = "\\" + Output;

      //Progress Updates during vcc /stvs comes with xxx.xx% so lookup for an % in output.
      if (Output.EndsWith("%")) {
        AddInGlobals.DTE.ExecuteCommand("VerifiedCCompilerAddin.VCCConnect.showVCCProgress", Output);
        return;
      }

      if (Output.Contains("succeeded.") || Output.Contains("failed.")) {
        AddInGlobals.DTE.ExecuteCommand("VerifiedCCompilerAddin.VCCConnect.showVCCProgress", Output);       
      }

      Match m = VCCErrorRegEx.Match(Output);
                       
        if (m.Success)
        {          
          string Line = m.Groups[2].Value;
          string Pos = m.Groups[4].Value;
          if (Line.IndexOf(",") > 0)
            Line = m.Groups[3].Value;
          
          VCCErrorItem Entry = new VCCErrorItem();
          
          //Check if Path valid.
          String CurrentPath = m.Groups[1].Value;
          if (CurrentPath == String.Empty) {
            Entry.FileName = "__VCC_DONT_REPORTED_A_FILENAME__";
          } else {
            Entry.FileName = Path.GetFullPath(m.Groups[1].Value);
          }
          Entry.Message = m.Groups[6].Value;
          Entry.Line = 0;
          Entry.Type = m.Groups[5].Value;
          Int32.TryParse(Line, out Entry.Line);
          if (Entry.Line > 0)
            Entry.Line--;

          if (!Int32.TryParse(m.Groups[4].Value, out Entry.Pos))
              Entry.Pos = 0;
         
          if (e.Data.Contains("error"))
            AddInGlobals.BuildPane.OutputTaskItemString(Output + Environment.NewLine, EnvDTE.vsTaskPriority.vsTaskPriorityHigh, "BUILDCOMPILE", EnvDTE.vsTaskIcon.vsTaskIconCompile, Entry.FileName, Convert.ToInt32(Entry.Line) + 1, Entry.Message, true);
          else
            AddInGlobals.BuildPane.OutputTaskItemString(Output + Environment.NewLine, EnvDTE.vsTaskPriority.vsTaskPriorityLow, "BUILDCOMPILE", EnvDTE.vsTaskIcon.vsTaskIconComment, Entry.FileName, Convert.ToInt32(Entry.Line) + 1, Entry.Message, true);

          AddInGlobals.DTE.ExecuteCommand("VerifiedCCompilerAddin.VCCConnect.showVCCErrors", Entry.FileName + "ê" + Entry.Line + "ê" + Entry.Message + "ê" + Entry.Type);          
        } else {
          AddInGlobals.BuildPane.OutputString(Output + Environment.NewLine);
        }

        AddInGlobals.BuildPane.ForceItemsToTaskList();
    }

    /// <summary>
    /// Get the ParentProcessID of a given ProcessID
    /// </summary>
    /// <param name="Id">ProcessID where you will the parent from</param>
    /// <returns>The PID of the parent</returns>
    private static int GetParentProcess(int Id) {
      int parentPid = 0;
      using (ManagementObject mo = new ManagementObject("win32_process.handle='" + Id.ToString() + "'")) {

        try {
          mo.Get();
          parentPid = Convert.ToInt32(mo["ParentProcessId"]);
        }
        catch {          
        }
      }
      return parentPid;
    }

    /// <summary>
    /// Kills a running VCC Process...
    /// </summary>
    public static void Cancel()
    {
      try {
        if (!VCCProcess.HasExited)
        {
          VCCProcess.Kill();
          Thread.Sleep(300);
          Process[] SubProcesses = Process.GetProcesses();
          foreach (Process sp in SubProcesses) {
            if (GetParentProcess(sp.Id) == VCCProcess.Id) {
                sp.Kill();
            }
          }

          Thread.Sleep(250);
          //Search for Z3.exe or  Z3Dist, and Kill them!
          Process[] Processes = Process.GetProcesses();
          foreach (Process p in Processes)
            if ((p.ProcessName.ToLower() == "z3.exe") || (p.ProcessName.ToLower() == "z3org.exe"))
              p.Kill();
        }
      }
      catch { }
    }
  } 
}
