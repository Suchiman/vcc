//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;
using System.IO;
using System.Diagnostics;
using VerifiedCCompilerAddin.Resources;
using Microsoft.Win32;
using System.Reflection;
using System.Globalization;

namespace AddinInstallerClass {

  [RunInstaller(true)]
  public partial class AddInInstaller : Installer {

    public AddInInstaller()
    {
      InitializeComponent();
    }

    public override void Uninstall(System.Collections.IDictionary savedState)
    {
      base.Uninstall(savedState);
      InstallRoutines.UninstallAddIn(this.Context);
    }


    public override void Install(System.Collections.IDictionary stateSaver)
    {
      base.Install(stateSaver);
      InstallRoutines.InstallAddIn(this.Context);
    }


    public override void Commit(System.Collections.IDictionary savedState)
    {
      base.Commit(savedState);
    }
  }


  public static class PathSetting {
    public static void AddPathToPath(string VCCBinsPath, bool AllUsers)
    {
      RegistryKey hklm = Registry.LocalMachine;
      RegistryKey hkcu = Registry.CurrentUser;

      hklm = hklm.OpenSubKey("SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment", true);
      hkcu = hkcu.OpenSubKey("Environment", true);


      string hklmPath = hklm.GetValue("Path", String.Empty) as string;
      string hkcuPath = hkcu.GetValue("Path", String.Empty) as string;

      string[] hklmPaths = hklmPath.Split(';');
      string[] hkcuPaths = hkcuPath.Split(';');

      bool found = false;

      if (AllUsers) {
        foreach (string s in hklmPaths) {
          if (PathsAreEqual(s, VCCBinsPath)) {
            found = true;
            break;
          }
        }

        if (!found) {
          if (!hklmPath.EndsWith(";")) {
            hklm.SetValue("Path", hklmPath + ";" + VCCBinsPath);
          } else {
            hklm.SetValue("Path", hklmPath + VCCBinsPath);
          }
        }
      } else {
        foreach (string s in hkcuPaths) {
          if (PathsAreEqual(s, VCCBinsPath)) {
            found = true;
            break;
          }
        }

        if (!found) {
          if (!hkcuPath.EndsWith(";")) {
            hkcu.SetValue("Path", hkcuPath + ";" + VCCBinsPath);
          } else {
            hkcu.SetValue("Path", hkcuPath + VCCBinsPath);
          }
        }
      } //else
    } // Function

    private static bool PathsAreEqual(string path1, string path2)
    {
      if (path1 == path2) return true;
      if (String.IsNullOrEmpty(path1)) return false;
      if (String.IsNullOrEmpty(path2)) return false;
      try {
        return Path.GetFullPath(path1).ToLower(CultureInfo.InvariantCulture) == Path.GetFullPath(path2).ToLower(CultureInfo.InvariantCulture);
      } catch (ArgumentException) {
        return false;
      }
    }
  }



  public static class InstallRoutines {
    const string AddInName = "VerifiedCCompilerAddin.Addin";
    const string UserTypeName = "usertype.dat";
    const string VsVersions = "Visual Studio 2008";

    public static void InstallAddIn(InstallContext Context)
    {


      try {
        string asmLocation = typeof(AddInInstaller).Assembly.Location;
        string fileName = Path.Combine(Path.GetDirectoryName(asmLocation), AddInName);
        FileInfo fInfo = new System.IO.FileInfo(fileName);

        System.Xml.XmlDocument doc = new System.Xml.XmlDocument();
        System.Text.Encoding enc = System.Text.Encoding.ASCII;
        string sXML = enc.GetString(InstallResource.VerifiedCCompilerAddin);
        doc.LoadXml(sXML);

        bool found = false;
        foreach (System.Xml.XmlNode Node in doc["Extensibility"]["Addin"]) {
          if (Node.Name == "Assembly") {
            //Change the Assembly to point to where the Addin is installed
            Node.InnerText = asmLocation;
            found = true;
          }
        }

        if (!found) {
          throw new InstallException("XML File is missing the Assembley Element, installation is not complete");
        }

        string destFile = String.Empty;
        bool Everyone = false;

        if (Context != null) {
          if (Context.Parameters.ContainsKey("ALLUSERS")) {
            if (!string.IsNullOrEmpty(Context.Parameters["ALLUSERS"])) {
              Everyone = true;
            }
          }
        }

        //Install VCCBindir to Path
        Assembly asm = Assembly.GetExecutingAssembly();
        string ASMLocation = asm.Location.ToLower();
        string VCCBinDir = ASMLocation.Replace("verifiedccompileraddin.dll", "").Replace("addin", "Binaries");
        PathSetting.AddPathToPath(VCCBinDir, Everyone);

        if (!Everyone) {
          string FolderPath = GetVsDocFolder(VsVersions); //Users\Documents\Visual Studio 2008
          if (Directory.Exists(FolderPath)) {
            DirectoryInfo dirInfo = new System.IO.DirectoryInfo(GetVsAddinsFolder(VsVersions));  //Users\Documents\Visual Studio 2008\Addins
            if (!dirInfo.Exists)
              dirInfo.Create();
            destFile = Path.Combine(dirInfo.FullName, AddInName);
          }
        } else {
          string specialFolder = Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData);
          if (Directory.Exists(specialFolder)) {
            DirectoryInfo dirInfo = new System.IO.DirectoryInfo(Path.Combine(specialFolder, "Microsoft\\MSEnvShared\\Addins"));
            if (!dirInfo.Exists)
              dirInfo.Create();

            destFile = Path.Combine(dirInfo.FullName, AddInName);
          }
        }

        FileInfo destFileInfo = new FileInfo(destFile);
        if (destFileInfo.Exists) {
          destFileInfo.Delete();
        }

        doc.Save(destFile);

        //Copy usertype.dat
        string idePath = GetIDEPath();
        if (idePath != null && Directory.Exists(idePath)) {
          string userTypesFileName = Path.Combine(idePath, UserTypeName);
          if (!File.Exists(userTypesFileName)) {
            string Types = enc.GetString(InstallResource.usertype);
            TextWriter tw = File.CreateText(userTypesFileName);
            tw.WriteLine(Types);
            tw.Flush();
            tw.Close();
          } else {
            //File exists allready, add keywords that are missing...
            TextReader tr = File.OpenText(userTypesFileName);
            string FileContent = tr.ReadToEnd();
            tr.Close();
            string UserTypes = enc.GetString(InstallResource.usertype);
            UserTypes = UserTypes.Replace(Environment.NewLine, ";");

            TextWriter tw = File.AppendText(userTypesFileName);
            FileContent.Replace(Environment.NewLine, ";");
            List<string> ExistingTypesList = new List<string>(FileContent.Split(';'));

            foreach (String Type in UserTypes.Split(';'))
            {
              if (!ExistingTypesList.Contains(Type)){
                tw.WriteLine(Type);
              }
            }
            tw.Flush();
            tw.Close();
          }
        }

      } catch (InstallException) {
        throw;
      } catch (Exception ex) {
        throw new InstallException(ex.Message + Environment.NewLine + ex.StackTrace);
      }
    }

    public static void UninstallAddIn(InstallContext Context)
    {

      try {

        string destFile = String.Empty;

        string FolderPath = GetVsDocFolder(VsVersions); //Users\Documents\Visual Studio 2008
        if (Directory.Exists(FolderPath)) {
          DirectoryInfo dirInfo = new System.IO.DirectoryInfo(GetVsAddinsFolder(VsVersions));  //Users\Documents\Visual Studio 2008\Addins
          destFile = Path.Combine(dirInfo.FullName, AddInName);

          FileInfo fInfo = new System.IO.FileInfo(destFile);
          if (fInfo.Exists)
            fInfo.Delete();
        }

        string specialFolder = Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData);
        if (Directory.Exists(specialFolder)) {
          DirectoryInfo dirInfo = new System.IO.DirectoryInfo(Path.Combine(specialFolder, "Microsoft\\MSEnvShared\\Addins"));
          destFile = Path.Combine(dirInfo.FullName, AddInName);
          FileInfo fInfo = new System.IO.FileInfo(destFile);
          if (fInfo.Exists)
            fInfo.Delete();
        }


        string idePath = GetIDEPath();

        if (idePath != null && File.Exists(Path.Combine(idePath, UserTypeName)))
          File.Delete(Path.Combine(idePath, UserTypeName));
      } catch (Exception ex) {
        throw new InstallException(ex.Message + Environment.NewLine + ex.StackTrace);
      }
    }

    public static string GetVsDocFolder(string vsVersion)
    {
      return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), vsVersion);
    }

    public static string GetVsAddinsFolder(string vsVersion)
    {
      return Path.Combine(GetVsDocFolder(vsVersion), "Addins");
    }

    private static string GetVSCOMNTOOLS()
    {
      string VSDir = Environment.GetEnvironmentVariable("VS90COMNTOOLS");
      return VSDir;
    }

    private static string GetIDEPath()
    {
      string VPath = GetVSCOMNTOOLS();
      if (VPath != null) VPath = VPath.Replace("Tools", "IDE");
      return VPath;
    }
  }
}