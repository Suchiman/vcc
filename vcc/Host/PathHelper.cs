using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Microsoft.Research.Vcc
{
  static class PathHelper
  {
    public static string/*?*/ ProbeForVccHeaders(bool quoteResult) {
      if (cachedVccHeaderDirectory == null) {
        var dir = new FileInfo(typeof(PathHelper).Assembly.Location).Directory;
        while (dir != null && dir.Exists) {
          foreach (DirectoryInfo subdir in dir.GetDirectories()) {
            if (string.Compare(subdir.Name, "Headers", true) == 0 && subdir.GetFiles("vcc.h").Length > 0) {
              cachedVccHeaderDirectory = subdir;
              break;
            }
          }
          dir = dir.Parent;
        }
      }

      if (cachedVccHeaderDirectory == null) return null;
      if (quoteResult && cachedVccHeaderDirectory.FullName.Contains(" "))
        return "\"" + cachedVccHeaderDirectory.FullName + "\"";
      else
        return cachedVccHeaderDirectory.FullName;
    }

    private static DirectoryInfo cachedVccHeaderDirectory;

    public static string/*?*/ ProbeForPluginDir() {
      PathHelper.ProbeForVccHeaders(false);
      if (cachedVccHeaderDirectory == null) return null;
      DirectoryInfo[] candidates = cachedVccHeaderDirectory.Parent.GetDirectories("Plugins");
      if (candidates.Length > 0) return candidates[0].FullName;
      return null;
    }

    public static string ProbeForPrelude() {
      string headersDir = PathHelper.ProbeForVccHeaders(false);
      if (headersDir != null) return System.IO.Path.Combine(headersDir, "VccPrelude.bpl");
      return null;
    }
  }
}
