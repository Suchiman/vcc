//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace Z3Model {
  public class Set {
    private List<string> _setList;

    public Set() {
      _setList = new List<string>();
    }

    public List<string> Elements {
      get {
        return _setList;
      }
    }

    public void AddElement(string ElementName)
    {
      if (ElementName.StartsWith("^") && !ElementName.StartsWith("^^"))
      {
        ElementName = ElementName.Substring(1);
      }

      _setList.Add(ElementName);
      _setList.Sort(Compares);
    }

    public override string ToString() {
      return "{" + String.Join("," + Environment.NewLine, _setList.ToArray()) + "}";
    }

    public string FilteredToString() {
      if (_setList.Count == 1)
        return ToString();

      List<string> Result = new List<string>();
      foreach(var item in _setList)
      {
        if (!item.StartsWith("$"))
            Result.Add(item);
      }

      return "{" + String.Join("," + Environment.NewLine, Result.ToArray()) + "}";
    }

    private static int Compares(string x, string y)
    {
        if (x.Contains("#writes"))
            return 1;

        if (y.Contains("#writes"))
            return 0;

        return (String.Compare(x, y));

    }

    public void Replace(string oldval, string newval)
    {
      List<string> _newList = new List<string>();
      foreach (string str in _setList)
      {
        if (str.Equals(oldval))
        {
          _newList.Add(newval);
        }
        else
        {
          _newList.Add(oldval);
        }
      }
      _setList = _newList;
    }
  }
}
