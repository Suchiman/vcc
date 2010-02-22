//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Microsoft.Research.Vcc
{
  static class PathHelper
  {
    public static string/*?*/ GetVccHeaderDir(bool quoteResult) {
      if (cachedVccHeaderDirectory == null) {
        var dir = new FileInfo(typeof(PathHelper).Assembly.Location).Directory;
        while (dir != null && dir.Exists) {
          if (dir.GetFiles("vcc.h").Length > 0) {
            cachedVccHeaderDirectory = dir;
            goto terminateSearch;
          }
          foreach (DirectoryInfo subdir in dir.GetDirectories()) {
            if (string.Compare(subdir.Name, "Headers", true) == 0 && subdir.GetFiles("vcc.h").Length > 0) {
              cachedVccHeaderDirectory = subdir;
              goto terminateSearch;
            }
          }
          dir = dir.Parent;
        }
      }

    terminateSearch:

      if (cachedVccHeaderDirectory == null) return null;
      if (quoteResult && cachedVccHeaderDirectory.FullName.Contains(" "))
        return "\"" + cachedVccHeaderDirectory.FullName + "\"";
      else
        return cachedVccHeaderDirectory.FullName;
    }

    private static DirectoryInfo cachedVccHeaderDirectory;

    public static string/*?*/ PluginDir {
      get {
        PathHelper.GetVccHeaderDir(false);
        if (cachedVccHeaderDirectory == null) return null;
        DirectoryInfo[] candidates = cachedVccHeaderDirectory.Parent.GetDirectories("Plugins");
        if (candidates.Length > 0) return candidates[0].FullName;
        return null;
      }
    }

    public static string PreludePath(string basename)
    {
      if (basename.IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0)
        return basename;
      else {
        string headersDir = PathHelper.GetVccHeaderDir(false);
        if (headersDir != null) return System.IO.Path.Combine(headersDir, basename);
        return null;
      }
    }
  }
}
