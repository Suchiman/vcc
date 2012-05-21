﻿using System.Text.RegularExpressions;

namespace Microsoft.Research.Vcc
{
  public class ErrorDetails
  {
    private static readonly Regex ErrorPattern =
      new Regex(@"(?<path>(.*?))\((?<line> \d+),(?<column> \d+)\)\s*:\s+(?<kind> (error|warning))\s+\w\w (?<errno>\d+):(?<msg>.*)", 
          RegexOptions.Compiled | RegexOptions.Singleline | RegexOptions.IgnorePatternWhitespace);

    private static readonly Regex RelatedErrorPattern =
      new Regex(@"(?<path>(.*?))\((?<line> \d+),(?<column> \d+)\)\s*:\s+(?<kind> (error|warning))\s+:\s+\(Location\ of\ symbol\ related\ to\ previous",
          RegexOptions.Compiled | RegexOptions.Singleline | RegexOptions.IgnorePatternWhitespace);


    public ErrorDetails(string filename, bool isWarning, int line, int column, int errno, string msg, bool isRelated)
    {
      this.Filename = filename;
      this.IsWarning = isWarning;
      this.Line = line;
      this.Column = column;
      this.ErrorNumber = errno;
      this.Message = msg;
      this.IsRelated = isRelated;
    }

    public string Filename { get; private set; }
    public bool IsWarning { get; private set; }
    public int Line { get; private set; }
    public int Column { get; private set; }
    public int ErrorNumber { get; private set; }
    public string Message { get; private set; }
    public bool IsRelated { get; private set; }

    public bool IsMatch(ErrorDetails other, bool compareMessage, bool compareLine, bool compareColumn)
    {
      return this.IsWarning == other.IsWarning
          && this.ErrorNumber == other.ErrorNumber
          && this.IsRelated == other.IsRelated
          && (!compareLine || this.Line == other.Line)
          && (!compareColumn || this.Column == other.Column)
          && (!compareMessage || this.Message == other.Message);
    }

    public override string ToString()
    {
      return this.IsRelated ?
        string.Format("{0}({1},{2}): {3} (Location of symbol related to previous {3}.)",
                      this.Filename, this.Line, this.Column, this.IsWarning ? "warning" : "error") :
        string.Format("{0}({1},{2}): {3} ##{4:0000}: {5}", this.Filename, this.Line, this.Column,
                      this.IsWarning ? "warning" : "error", this.ErrorNumber, this.Message);
    }

    public static ErrorDetails Parse(string str)
    {
      var match = ErrorPattern.Match(str);
      if (match.Success)
      {
        var filename = match.Groups["path"].Value;
        var line = int.Parse(match.Groups["line"].Value);
        var col = int.Parse(match.Groups["column"].Value);
        var isWarning = match.Groups["kind"].Value == "warning";
        var errno = int.Parse(match.Groups["errno"].Value);
        var msg = match.Groups["msg"].Value;
        return new ErrorDetails(filename, isWarning, line, col, errno, msg, false);
      }

      match = RelatedErrorPattern.Match(str);
      if (match.Success)
      {
        var filename = match.Groups["path"].Value;
        var line = int.Parse(match.Groups["line"].Value);
        var col = int.Parse(match.Groups["column"].Value);
        var isWarning = match.Groups["kind"].Value == "warning";
        return new ErrorDetails(filename, isWarning, line, col, 0, "", true);
      }

      return null;
    }
  }
}