//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;

namespace Microsoft.Research.Vcc
{
  class CCompilerHelper
  {
    public static IEnumerable<String> Preprocess(VccOptions commandLineOptions, out bool hasErrors) {
      hasErrors = false;
      if (commandLineOptions.NoPreprocessor) return commandLineOptions.FileNames;
      string savedCurentDir = Directory.GetCurrentDirectory();
      List<String> preprocessedFiles = new List<string>();
      try {
        foreach (string fileName in commandLineOptions.FileNames) {
          Directory.SetCurrentDirectory(Path.GetDirectoryName(fileName));
          string ppFileName = RunPreprocessor(fileName, commandLineOptions);
          if (String.IsNullOrEmpty(ppFileName)) {
            hasErrors = true;
            break;
          } else {
            preprocessedFiles.Add(ppFileName);
          }
        }
      } catch {
        if (commandLineOptions.ClPath != null)
          Console.WriteLine("Error while running preprocessor " + commandLineOptions.ClPath);
        else
          Console.WriteLine("Please install Microsoft VC++ before using VCC. If already installed, please ensure that the VS80COMNTOOLS or VS90COMNTOOLS environment variable is set.");
        hasErrors = true;
      } finally {
        Directory.SetCurrentDirectory(savedCurentDir);
      }

      return preprocessedFiles;
    }

    private static string GenerateClArgs(string fileName, VccOptions commandLineOptions) {
      StringBuilder args = new StringBuilder();
      args.Append("/nologo /TC");
      args.Append(" /E /D_PREFAST_ /DVERIFY /D_USE_DECLSPECS_FOR_SAL");
      if (commandLineOptions.NewSyntax) args.Append(" /DVERIFY2");

      foreach (string ppOption in commandLineOptions.PreprocessorOptions) {
        args.Append(' ');
        args.Append(ppOption);
      }
      string/*?*/ vccHeaders = PathHelper.GetVccHeaderDir(true);
      if (vccHeaders != null) {
        args.Append(" /I");
        args.Append(vccHeaders);
      }
      args.Append(" \"").Append(fileName).Append('\"');

      return args.ToString();
    }

    private static bool ProcessOutputAndReturnTrueIfErrorsAreFound(string fileName, string ppOutput) {
      bool hasErrors = false;
      if (ppOutput.Length > 0) {
        ppOutput = ppOutput.Replace(Path.GetFileName(fileName) + "\r\n", "");
        if (ppOutput.Length > 0) Console.Write(ppOutput);
        if (ppOutput.Contains(": error C") || ppOutput.Contains(": fatal error C"))
          hasErrors = true;
      }
      return hasErrors;
    }

    private static string RunPreprocessor(string fileName, VccOptions commandLineOptions) {
      string args = GenerateClArgs(fileName, commandLineOptions);
      string outExtension = ".i";
      if (commandLineOptions.ModifiedPreprocessorFiles) outExtension += "." + System.Diagnostics.Process.GetCurrentProcess().Id;
      string outFileName = Path.ChangeExtension(fileName, outExtension);
      if (StartClProcessAndReturnTrueIfErrorsAreFound(fileName, args, outFileName, commandLineOptions))
        return null;
      return outFileName;
    }

    private static bool StartClProcessAndReturnTrueIfErrorsAreFound(string fileName, string arguments, string outFileName, VccOptions commandLineOptions) {

      StringBuilder errors = new StringBuilder();
      ProcessStartInfo info = ConfigureStartInfoForClVersion9Or8(commandLineOptions);
      info.Arguments = arguments;
      info.CreateNoWindow = true;
      info.RedirectStandardOutput = true;
      info.RedirectStandardError = true;
      info.UseShellExecute = false;

      using (StreamWriter outFile = new StreamWriter(outFileName))
      using (Process process = Process.Start(info)) {
        process.OutputDataReceived += delegate(object sender, DataReceivedEventArgs args) {
          if (args.Data != null) outFile.WriteLine(args.Data);
        };

        process.ErrorDataReceived += delegate(object sender, DataReceivedEventArgs args) {
          if (args.Data != null) errors.AppendLine(args.Data);
        };

        process.BeginErrorReadLine();
        process.BeginOutputReadLine();
        process.WaitForExit();
      }
      return ProcessOutputAndReturnTrueIfErrorsAreFound(fileName, errors.ToString());
    }
    

    /// <summary>
    /// Determine the install location of cl.exe via the environment variables VS90COMNTOOLS
    /// and setup the start info to invoke the found instance of cl, unless an explicit 
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
        if (VSCOMNTOOLS == null) throw new FileNotFoundException();
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
