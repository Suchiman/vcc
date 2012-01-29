//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace Z3Model.Parser
{
  enum FunctionParserStages
  {
    None,
    Name,
    FunctionSet,
    Leftside,
    Rightside
  }

  public class FunctionParser
  {
    Tokenizer Tokenizer;
    FunctionParserStages Stage;
    List<Function> EntryList;
    private Dictionary<int, Partition> _PartitionMap;
    private Dictionary<string, List<Function>> _FunctionMap;

    public FunctionParser(Tokenizer Tokenizer, Dictionary<int, Partition> PartitionMap)
    {
      this.Tokenizer = Tokenizer;
      Stage = FunctionParserStages.None;
      EntryList = new List<Function>();
      _FunctionMap = new Dictionary<string, List<Function>>();
      _PartitionMap = PartitionMap;
    }

    public List<Function> GetEntryList()
    {
      return EntryList;
    }


    public Dictionary<string,List<Function>> Parse()
    {
      Stage = FunctionParserStages.None;
      String functionType = null;
      Function newEntry = null;

      //Solange es ein weiteres Token gibt
      while (Tokenizer.MoveNext())
      {


        if (Tokenizer.CurrentToken.Type == Tokentype.STRING && Stage == FunctionParserStages.None)
        {
          Stage = FunctionParserStages.Name;
          functionType = Tokenizer.CurrentToken.Content;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.STRING && Stage == FunctionParserStages.Leftside)
        {
          if (Tokenizer.CurrentToken.Content != "else")
          {
            if ((newEntry == null) && (functionType != null))
            {
              newEntry = new Function(functionType);
              if (!_FunctionMap.ContainsKey(functionType))
              {
                _FunctionMap[functionType] = new List<Function>();
              }
              _FunctionMap[functionType].Add(newEntry);
              EntryList.Add(newEntry);
            
            }
            if (newEntry != null)
              newEntry.AddArgument(_PartitionMap[Convert.ToInt32(Tokenizer.CurrentToken.Content.Substring(1))]);
          }
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.STRING && Stage == FunctionParserStages.Rightside)
        {
          if (Tokenizer.CurrentToken.Content != "#unspecified")
          {
            if (newEntry != null)
            {
              newEntry.SetResult(_PartitionMap[Convert.ToInt32(Tokenizer.CurrentToken.Content.Substring(1))]);
              newEntry = null;
            }
          }
          continue;
        }


        //Modifies Stages!
        if (Tokenizer.CurrentToken.Type == Tokentype.CBO && Stage == FunctionParserStages.Name)
        {
          Stage = FunctionParserStages.FunctionSet;
          continue;
        }

        //-> innerhalb von { ist danach rechte seite
        if (Tokenizer.CurrentToken.Type == Tokentype.RESULTS && Stage == FunctionParserStages.Leftside)
        {
          Stage = FunctionParserStages.Rightside;
          continue;
        }

        //Return innerhalb von { ist neuer Set
        if (Tokenizer.CurrentToken.Type == Tokentype.NEWLINE && Stage == FunctionParserStages.Rightside)
        {
          Stage = FunctionParserStages.Leftside;
          newEntry = null;
          continue;
        }

        //Return innerhalb von { ist neuer Set
        if (Tokenizer.CurrentToken.Type == Tokentype.NEWLINE && Stage == FunctionParserStages.FunctionSet)
        {
          Stage = FunctionParserStages.Leftside;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.CBC)
        {
          Stage = FunctionParserStages.None;
          continue;
        }


      }
      return _FunctionMap;

    }


  }
}
