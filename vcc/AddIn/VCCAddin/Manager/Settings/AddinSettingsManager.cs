//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.IsolatedStorage;
using EnvDTE;

namespace VerifiedCCompilerAddin.Manager.Settings {
  public static class AddinSettingsManager {

    private static Dictionary<string, string> Settings = ReadSettingsFromFile();
    private static int? randomSeed = null;
    public static int RandomSeed {
      get {
        return randomSeed.Value;
      }

      set {
        randomSeed = value;
      }
    }
    public static bool RandomSeedEnabled {
      get { return randomSeed.HasValue; }
      set
      {
        if (value == true)
          throw new InvalidOperationException();
        else
          randomSeed = null;
      }
    }
    public static string VCCCommandLineSwitches {
      get {
        return Settings["VCCCommandLineSwitches"];
      }
      set {
        Settings["VCCCommandLineSwitches"] = value;
      }
    }
    public static bool  VCCCommandLineSwitchesEnabled {
      get {
        return Convert.ToBoolean(Settings["VCCCommandLineSwitchesEnabled"]);
      }
      set {
        Settings["VCCCommandLineSwitchesEnabled"] = Convert.ToString(value);
      }
    }
        
    public static bool WarnForHeaderFile
    {
      get
      {
        return Convert.ToBoolean(Settings["WarnForHeaderFile"]);
      }
      set
      {
        Settings["WarnForHeaderFile"] = Convert.ToString(value);
      }
    }

    //Cames from EXTENDER !!!
    public static string MasterFileName {
      get {
        Project prj = Utilities.ActiveProject();
        if (prj.Globals.get_VariableExists("MasterFileName")) {
          return (string)prj.Globals["MasterFileName"];
        } else {
          return String.Empty;
        }
      }
    }
    public static bool UseMasterFile {
      get {
        Project prj = Utilities.ActiveProject();
        if (prj.Globals.get_VariableExists("ActiveMasterFile")) {
          return Convert.ToBoolean((string)prj.Globals["ActiveMasterFile"]);
        } else {
          return false;
        }

      }
    }
 

    private static void InitEmptySettings(Dictionary<string,string> settings) {
      settings.Clear();
      settings.Add("VCCCommandLineSwitches", "");
      settings.Add("VCCCommandLineSwitchesEnabled", "false");
      settings.Add("HintsEnabled", "false");
      settings.Add("VCCHint001", "false");
      settings.Add("VCCHint002", "false");
      settings.Add("VCCHint003", "false");
      settings.Add("MasterFileName", "");
      settings.Add("UseMasterFile", "false");
      settings.Add("WarnForHeaderFile", "true");
      settings.Add("ShowBallonTip", "false");
    }

    public static Dictionary<string, string> ReadSettingsFromFile()
    {
      Dictionary<string, string> result = new Dictionary<string, string>();
      InitEmptySettings(result);

      try {
        StreamReader reader = new StreamReader(new IsolatedStorageFileStream("Settings.txt", FileMode.Open, IsoFile));

        string[] SettingLines = reader.ReadToEnd().Replace(Environment.NewLine, "ê").Split('ê');
        reader.Close();

        foreach (string set in SettingLines) {
          int splitPos = set.IndexOf("=");
          if (splitPos >= 1)
          {
            string Key = set.Substring(0, splitPos);
            string Value = set.Substring(splitPos + 1); 
            result[Key] = Value;
          }
        }
      } catch {
        //Nothing...
      }
      return result;
    }

    public static void Save()
    {

      StreamWriter writer = new StreamWriter(new IsolatedStorageFileStream("Settings.txt", FileMode.OpenOrCreate, IsoFile));
      foreach (var item in Settings) {
        writer.WriteLine("{0}={1}", item.Key, item.Value);
      }
      writer.Close();
    }

    private static IsolatedStorageFile IsoFile
    {
      get { return IsolatedStorageFile.GetUserStoreForAssembly(); }
    }

    public static bool ShowBallonTip
    {
      get
      {
        return Convert.ToBoolean(Settings["ShowBallonTip"]);
      }
      set
      {
        Settings["ShowBallonTip"] = Convert.ToString(value);
      }
    }
  }
}
