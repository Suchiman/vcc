//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;

namespace Microsoft.Research.Vcc
{
  class CCompilerHelper
  {
    public static IEnumerable<String> Preprocess(VccOptions commandLineOptions) {
      return Preprocess(commandLineOptions, !commandLineOptions.NoCompilerRun);
    }

    public static IEnumerable<String> Preprocess(VccOptions commandLineOptions, bool runCompiler) {
      if (commandLineOptions.NoPreprocessor) return commandLineOptions.FileNames;
      string savedCurentDir = Directory.GetCurrentDirectory();
      List<String> preprocessedFiles = new List<string>();
      try {
        foreach (string fileName in commandLineOptions.FileNames) {
          Directory.SetCurrentDirectory(Path.GetDirectoryName(fileName));
          //System.Console.WriteLine("{0} run compiler", System.DateTime.Now.Second);
          if (runCompiler) {
            RunCompiler(fileName, commandLineOptions);
            //System.Console.WriteLine("{0} run compiler done", System.DateTime.Now.Second);
            if (VccCommandLineHost.ErrorCount > 0) break;
          }
          preprocessedFiles.Add(RunPreprocessor(fileName, commandLineOptions));
          //System.Console.WriteLine("{0} cpp done", System.DateTime.Now.Second);
        }
      } catch {
        if (commandLineOptions.ClPath != null)
          Console.WriteLine("Error while running preprocessor " + commandLineOptions.ClPath);
        else
          Console.WriteLine("Please install Microsoft VC++ before using VCC. If already installed, please ensure that the VS80COMNTOOLS or VS90COMNTOOLS environment variable is set.");
        VccCommandLineHost.ErrorCount++;
      } finally {
        Directory.SetCurrentDirectory(savedCurentDir);
      }

      return preprocessedFiles;
    }

    private static StringBuilder GenerateClArgs(string fileName, VccOptions commandLineOptions) {
      StringBuilder args = new StringBuilder();
      args.Append("/nologo /TC");
      foreach (string ppOption in commandLineOptions.PreprocessorOptions) {
        args.Append(' ');
        args.Append(ppOption);
      }
      string/*?*/ vccHeaders = PathHelper.ProbeForVccHeaders(true);
      if (vccHeaders != null) {
        args.Append(" /I");
        args.Append(vccHeaders);
      }
      args.Append(" \"").Append(fileName).Append('\"');
      return args;
    }


    private static void ProcessOutput(string fileName, bool reportError, StringBuilder outputSB) {
      string output = outputSB.ToString();
      if (output.Length > 0) {
        output = output.Replace(Path.GetFileName(fileName) + "\r\n", "");
        if (output.Length > 0) Console.Write(output);
        if (reportError && (output.Contains(": error C") || output.Contains(": fatal error C")))
          VccCommandLineHost.ErrorCount++;
      }
    }

    private static void RunCompiler(string fileName, VccOptions commandLineOptions) {
      string args = GenerateClArgs(fileName, commandLineOptions).ToString();
      StartClProcess(fileName, "/c /Zs " + args, true, false, null, commandLineOptions);
    }

    private static string RunPreprocessor(string fileName, VccOptions commandLineOptions) {
      string args = GenerateClArgs(fileName, commandLineOptions).ToString();
      string outExtension = ".i";
      if (commandLineOptions.ModifiedPreprocessorFiles) outExtension += "." + System.Diagnostics.Process.GetCurrentProcess().Id.ToString();
      string outFileName = Path.ChangeExtension(fileName, outExtension);
      StartClProcess(fileName, args + " /E /D_PREFAST_ /DVERIFY /DVERIFY2 /D_USE_DECLSPECS_FOR_SAL", false, true, outFileName, commandLineOptions);
      return outFileName;
    }

    private static void StartClProcess(string fileName, string arguments, bool reportError, bool redirect, string outFileName, VccOptions commandLineOptions) {
      ProcessStartInfo info = ConfigureStartInfoForClVersion9Or8(commandLineOptions);
      info.Arguments = arguments;
      info.CreateNoWindow = true;
      info.RedirectStandardOutput = true;
      info.RedirectStandardError = true;
      info.UseShellExecute = false;

      StringBuilder output = new StringBuilder();
      StringBuilder errors = new StringBuilder();
      StreamWriter outFile = null;
      if (redirect) {
        outFile = new StreamWriter(outFileName);
      }

      try {
        using (Process process = Process.Start(info)) {
          process.OutputDataReceived += delegate(object sender, DataReceivedEventArgs args) {
            if (args.Data != null) {
              if (outFile != null) {
                if (VccCommandLineHost.currentPlugin != null)
                  VccCommandLineHost.currentPlugin.WritePreProcessed(outFile, args.Data);
                else
                  outFile.WriteLine(args.Data);
              } else
                output.Append(args.Data).Append("\r\n");
            }
          };

          process.ErrorDataReceived += delegate(object sender, DataReceivedEventArgs args) {
            if (args.Data != null) {
              errors.Append(args.Data).Append("\r\n");
            }
          };

          process.BeginErrorReadLine();
          process.BeginOutputReadLine();
          process.WaitForExit();

          if (VccCommandLineHost.currentPlugin != null) {
            VccCommandLineHost.currentPlugin.FinishPreProcessing(outFile);
          }

          ProcessOutput(fileName, reportError, output);
          ProcessOutput(fileName, reportError, errors);
        }
      } finally {
        if (outFile != null) outFile.Dispose();
      }
    }

    /// <summary>
    /// Determine the install location of cl.exe via the environment variables VS90COMNTOOLS and
    /// VS80COMNTOOLS and setup the start info to invoke the found instance of cl, unless an explicit 
    /// location has been given as command line option.
    /// </summary>
    private static ProcessStartInfo ConfigureStartInfoForClVersion9Or8(VccOptions commandLineOptions) {
      if (!String.IsNullOrEmpty(commandLineOptions.ClPath)) {
        ProcessStartInfo result = new ProcessStartInfo("\"" + commandLineOptions.ClPath + "\"");
        try {
          FileInfo clPath = new FileInfo(commandLineOptions.ClPath);
          string path = result.EnvironmentVariables["path"];
          result.EnvironmentVariables["path"] = path + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.FullName, @"Common7\IDE") + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.Parent.FullName, @"Common7\IDE");
          result.EnvironmentVariables["include"] =
              Path.Combine(clPath.Directory.Parent.Parent.FullName, @"VC\INCLUDE") + ";"
            + Path.Combine(clPath.Directory.Parent.Parent.Parent.FullName, @"VC\INCLUDE");

        } catch (Exception) { } // we only do a best effort to set the path
        return result;
      } else {
        string VSCOMNTOOLS = Environment.GetEnvironmentVariable("VS90COMNTOOLS");
        if (VSCOMNTOOLS == null) {
          VSCOMNTOOLS = Environment.GetEnvironmentVariable("VS80COMNTOOLS");
          if (VSCOMNTOOLS == null) {
            throw new FileNotFoundException();
          }
        }

        string vsDir = new DirectoryInfo(VSCOMNTOOLS).Parent.Parent.FullName;
        ProcessStartInfo info = new ProcessStartInfo(Path.Combine(vsDir, @"vc\bin\cl.exe"));
        string path = info.EnvironmentVariables["path"];
        info.EnvironmentVariables["path"] = path + ";" + Path.Combine(vsDir, @"Common7\IDE");
        info.EnvironmentVariables["include"] = Path.Combine(vsDir, @"VC\INCLUDE");
        return info;
      }
    }
  }
}
