//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Windows.Forms;

namespace Vcc2ModelViewer
{
  static class ModelViewerMain
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    static void parseErrorCommandLineArguments(string err)
    {
      Console.WriteLine("Aborting parsing command line arguments:\n" + err);
      Environment.Exit(1);
    }

    static void printUsage()
    {
      Console.WriteLine("Usage: Vcc2ModelViewer [options] <filename>");
      Console.WriteLine("       filename           : Z3 model input file");
      Console.WriteLine("       options ");
      Console.WriteLine("          /l:<linenumber> : select state for specified line number in model");
      Console.WriteLine("          /m:<model>      : select a model to be displayed. (Starting at 0.)");
      Environment.Exit(0);
    }

    static void parseCommandLineArguments(string[] args, ref string fileName, ref int lineNumber, ref int modelNumber)
    {
      int idx;
      fileName = null;

      for (idx = 0; idx < args.Length; idx++)
      {
        if (args[idx].StartsWith("/"))
        {
          // parse command line parameter switches
          if (args[idx].StartsWith("/l:"))
          {
            string lineStr = args[idx].Substring(3);
            if (!Int32.TryParse(lineStr, out lineNumber))
            {
              string err = String.Format("Cannot parse line number information from \"{0}\"", lineStr);
              parseErrorCommandLineArguments(err);
            }
          }
          else if (args[idx].StartsWith("/m:")) 
          {
            string modelStr = args[idx].Substring(3);
            if (!Int32.TryParse(modelStr, out modelNumber))
            {
              string err = String.Format("Cannot parse line model information from \"{0}\"", modelStr);
              parseErrorCommandLineArguments(err);
            }
          }
          else if (args[idx].StartsWith("/?"))
          {
            printUsage();
          }
          else
          {
            string err = String.Format("Unknown command line argument \"{0}\".", args[idx]);
            parseErrorCommandLineArguments(err);
          }
        }
        else
        {
          // parse file name
          if (fileName != null)
          {
            parseErrorCommandLineArguments("Multiple inputs files specified.");
          }
          fileName = args[idx];
        }
      }
    }

    [STAThread]
    static void Main(string[] args)
    {
      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);

      string fileName = null;
      int lineNumber = -1;
      int modelNumber = 0;
      parseCommandLineArguments(args, ref fileName, ref lineNumber, ref modelNumber);

      ModelViewerMainForm viewer = new ModelViewerMainForm();
      if (fileName != null)
      {
        viewer.loadModel(fileName, lineNumber, modelNumber);
      }
      Application.Run(viewer);
    }
  }
}
