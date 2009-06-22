//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace Z3Model.Parser
{
  enum PartitionParserStages
  {
    None,
    Identifier,
    Set,
    Value,
    ValueTyp
  }

  public class PartitionParser
  {
    Tokenizer Tokenizer;
    PartitionParserStages Stage;
    Dictionary<int, Partition> _PartitionMap;

    public Dictionary<int, Partition> getPartitionMap()
    {
      return _PartitionMap;
    }

    public PartitionParser(Tokenizer Tokenizer)
    {
      this.Tokenizer = Tokenizer;
      Stage = PartitionParserStages.None;
      _PartitionMap = new Dictionary<int, Partition>();
      Parse();
    }

    private void Parse()
    {
      Stage = PartitionParserStages.None;
      Partition newPartition = null;

      // Consume tokens until tokenizer returns end of file.
      while (Tokenizer.MoveNext())
      {

        if ((Tokenizer.CurrentToken.Type == Tokentype.STRING) && (Stage == PartitionParserStages.None))
        {
          Stage = PartitionParserStages.Identifier;
          newPartition = new Partition(Convert.ToInt32(Tokenizer.CurrentToken.Content.Substring(1)));
          continue;
        }

        if ((Tokenizer.CurrentToken.Type == Tokentype.STRING) && (Stage == PartitionParserStages.Set) && (newPartition != null))
        {
          Regex regexCallFormal = new Regex(@"^call[0-9]+formal@[^\s]*");
          if(!regexCallFormal.IsMatch(Tokenizer.CurrentToken.Content)){
            newPartition.AddSetElement(Tokenizer.CurrentToken.Content);
          }
          if (newPartition.Value == null)
          {
            newPartition.SetValue(Tokenizer.CurrentToken.Content);
          }
          continue;
        }

        if ((Tokenizer.CurrentToken.Type == Tokentype.STRING) && (Stage == PartitionParserStages.Value) && (newPartition != null))
        {
          newPartition.SetValue(Tokenizer.CurrentToken.Content);
          continue;
        }

        if ((Tokenizer.CurrentToken.Type == Tokentype.STRING) && (Stage == PartitionParserStages.ValueTyp) && (newPartition != null))
        {
          newPartition.SetType(PartitionType.Value);
          continue;
        }

        //Modifies Stages!

        if (Tokenizer.CurrentToken.Type == Tokentype.CBO)
        {
          Stage = PartitionParserStages.Set;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.CBC)
        {
          Stage = PartitionParserStages.None;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.RESULTS)
        {
          Stage = PartitionParserStages.Value;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.COLON)
        {
          Stage = PartitionParserStages.ValueTyp;
          continue;
        }

        if (Tokenizer.CurrentToken.Type == Tokentype.NEWLINE)
        {
          Stage = PartitionParserStages.None;
          if (newPartition != null)
          {
            _PartitionMap.Add(newPartition.Id, newPartition);
          }
          continue;
        }
      }
    }
  }
}
