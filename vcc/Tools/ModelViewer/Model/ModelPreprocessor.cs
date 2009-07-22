//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.IO;
using Z3Model;
using Z3Model.Parser;
using VccModel.Controller;

namespace VccModel.Preprocessor
{
  public static class ModelPreprocessor
  {

    public static Z3ModelVcc parse(string fileName)
    {
      TextReader tr = File.OpenText(fileName);

      string FileContent = tr.ReadToEnd();
      tr.Close();

      int pos_partition = FileContent.IndexOf("partitions:");
      int pos_function = FileContent.IndexOf("function interpretations:", pos_partition);
      int pos_end = FileContent.IndexOf("END_OF_MODEL", pos_function);

      string partitionContent = FileContent.Substring(pos_partition, pos_function - pos_partition);
      string functionContent = FileContent.Substring(pos_function + 26, pos_end - (pos_function + 26));

      return parseModel(partitionContent, functionContent);
    }

    public static Z3ModelVcc parse(string fileName, int modelInFile)
    {
      TextReader tr = File.OpenText(fileName);

      string FileContent = tr.ReadToEnd();
      tr.Close();

      List<int> partitionPositions = FindModelsInFile(FileContent);

      if ((0 <= modelInFile) && (modelInFile < partitionPositions.Count))
      {
        int pos_partition = FileContent.IndexOf("partitions:", partitionPositions[modelInFile]);
        int pos_function = FileContent.IndexOf("function interpretations:", pos_partition);
        int pos_end = FileContent.IndexOf("END_OF_MODEL", pos_function);
        string partitionContent, functionContent;

        partitionContent = FileContent.Substring(pos_partition, pos_function - pos_partition);
        if (pos_end <= (pos_function + 26))
        {
          // This should usually not happen, since this hints to an incomplete model.
          // Still there are models in the wild, which have a missing END_OF_MODEL marker.
          return new Z3ModelVcc();
        }

        functionContent = FileContent.Substring(pos_function + 26, pos_end - (pos_function + 26));
        return parseModel(partitionContent, functionContent);
      }
      return parse(fileName);
    }

    static void AddFileName(Z3ModelVcc model, Partition fileIndexPart, Partition fileNameTokenPart)
    {
      int fileIndex, specialCharPos;

      String fileNameToken = fileNameTokenPart.Value;
      if (!Int32.TryParse(fileIndexPart.Value, out fileIndex)) return;

      if (!fileNameToken.StartsWith("#file^")) return;

      fileNameToken = fileNameToken.Substring(6);
      do
      {
        specialCharPos = fileNameToken.IndexOf("?");
        if (specialCharPos >= 0)
        {
          // "?" is not a legal part of a windows file name. Therefore we assume, that the next two
          // characters are to be interpreted in hex format and reinserted as this position in the string.
          // If this fails, do not include the string as filename in the lookup table.
          String hexcode = fileNameToken.Substring(specialCharPos+1, 2);
          try {
            int charval = Int32.Parse(hexcode, System.Globalization.NumberStyles.AllowHexSpecifier);
            String replChar = String.Format("{0}", (char)charval);
            String srcPattern = String.Format("?{0}", hexcode);

            fileNameToken = fileNameToken.Replace(srcPattern, replChar);
          } 
          catch { 
            return; 
          }
        }
      } while (specialCharPos >= 0);

      if (fileNameToken.Length > 0)
      {
        if (!model.FilenameMap.ContainsKey(fileIndex))
        {
          model.FilenameMap[fileIndex] = fileNameToken;
        }
      }
    }

    static void AddExecutionState(Z3ModelVcc model, Partition state, Partition token)
    {
      if (FindExecutionState(model, state, token) != null)
      {
        return;
      }
      ExecutionState exec = new ExecutionState();
      Function goodStateFunction = null;
      if (model.FunctionMap.ContainsKey("$good_state"))
      {
        goodStateFunction = model.FunctionMap["$good_state"].Find(delegate(Function fn)
        {
          return ((fn.FunctionArguments[0] == state) && (!fn.Result.Value.Equals("true")));
        });
      }
      exec.good_state = (goodStateFunction == null);
      exec.state = state;
      exec.sourceInfo = new SourceLocation();
      exec.state.SetValue(exec.state.Set.Elements[0]);
      //cut off "#tok^"
      exec.sourceInfo.SetFromToken(token.Set.Elements[0], model.FilenameMap);
      if (model.FunctionMap.ContainsKey("$current_timestamp"))
      {
        Function ts_func = model.FunctionMap["$current_timestamp"].Find(delegate(Function fn) { return fn.FunctionArguments[0] == exec.state; });
        if (ts_func != null)
        {
          exec.timestamp = ts_func.Result;
          exec.timestamp.SetType(PartitionType.Value);
        }
      }
      state.SetType(PartitionType.State);
      token.SetType(PartitionType.LineNumberToken);
      model.ExecutionStates.Add(exec);
      model.ExecutionStates.Sort();
    }

    static ExecutionState FindExecutionState(Z3ModelVcc model, Partition state, Partition token)
    {
      SourceLocation lineNumber = new SourceLocation();
      lineNumber.SetFromToken(token.Set.Elements[0], model.FilenameMap);
      return FindExecutionState(model, state, lineNumber);
    }

    static ExecutionState FindExecutionState(Z3ModelVcc model, Partition state, SourceLocation lineNumber)
    {
      if ((lineNumber.Line != 0) || (lineNumber.Column != 0) || (lineNumber.FileName != null))
      {
        foreach (ExecutionState execState in model.ExecutionStates)
        {
          if ((execState.state == state) && (execState.sourceInfo == lineNumber))
          {
            return execState;
          }
        }
      }

      // OK. The precise state/line combination could not be found. Now just look for the correct state 
      // and ignore the line number information.
      foreach (ExecutionState execState in model.ExecutionStates)
      {
        if (execState.state == state)
        {
          return execState;
        }
      }

      return null;
    }

    static void PruneExecutionStates(Z3ModelVcc model)
    {
      ExecutionState remove = null;

      do
      {
        remove = null;

        foreach (ExecutionState execState in model.ExecutionStates)
        {
          // This is not really efficient, but should suffice for a few states:
          // Take the current line number for the execState and call FindExecutionState().
          // If another state is returned, this state can be savely removed, since
          // it contains no line number information, but a state, that is used in a valid execState.
          ExecutionState temp = FindExecutionState(model, execState.state, execState.sourceInfo);
          if (temp != execState)
          {
            remove = execState;
            break;
          }
        }

        if (remove != null) {
          model.ExecutionStates.Remove(remove);
        }
      } while (remove != null);
    }

    private static Partition FindDistinguishedType(string typeref, Z3ModelVcc model, Dictionary<string, Partition> distTpMap)
    {
      if (!typeref.StartsWith("dt") && !typeref.StartsWith("tc"))
        return null;

      if (typeref.Contains("!"))
      {
        typeref = typeref.Substring(0, typeref.IndexOf("!"));
      }
      if (typeref.Contains("@@"))
      {
        typeref = typeref.Substring(0, typeref.IndexOf("@@"));
      }

      string typeindex = typeref.Substring(2);

      // Use #distTp to find the type reference
      if (typeref.StartsWith("dt"))
      {
        string distTp = String.Format("#distTp{0}", typeindex);
        if (distTpMap.ContainsKey(distTp))
        {
          // Only add globals for non-pointer types
          if (!distTpMap[distTp].DisplayValue.EndsWith("*"))
          {
            return distTpMap[distTp];
          }
        }
      }

      // use $type_code_is to resolve the type
      if (typeref.StartsWith("tc"))
      {
        foreach (Function type_code_is in model.FunctionMap["$type_code_is"])
        {
          if (type_code_is.FunctionArguments[0].DisplayValue.Equals(typeindex))
          {
            return type_code_is.FunctionArguments[1];
          }
        }
      }

      return null;
    }

    public static Z3ModelVcc parseModel(string PartitionContent, string FunctionContent)
    {
      Z3ModelVcc model = new Z3ModelVcc();

      LinkedList<Partition> unresolved_types = new LinkedList<Partition>();
      Dictionary<string, Partition> distTpMap = new Dictionary<string, Partition>();

      Tokenizer tk = new Tokenizer(PartitionContent);
      PartitionParser pP = new PartitionParser(tk);

      Tokenizer tkF = new Tokenizer(FunctionContent);

      model.PartitionMap = pP.getPartitionMap();

      FunctionParser fP = new FunctionParser(tkF, model.PartitionMap);
      model.FunctionMap = fP.Parse();

      // Create the file name lookup table
      if (model.FunctionMap.ContainsKey("$file_name_is"))
      {
        foreach (Function f in model.FunctionMap["$file_name_is"])
        {
          AddFileName(model, f.FunctionArguments[0], f.FunctionArguments[1]);
        }
      }

      // Create all relevant information about meta/state/token information, that is available throughout the model.
      if (model.FunctionMap.ContainsKey("$good_state_ext"))
      {
        foreach (Function f in model.FunctionMap["$good_state_ext"])
        {
          AddExecutionState(model, f.FunctionArguments[1], f.FunctionArguments[0]);
        }
      }
      if (model.FunctionMap.ContainsKey("$local_value_is"))
      {
        foreach (Function f in model.FunctionMap["$local_value_is"])
        {
          AddExecutionState(model, f.FunctionArguments[0], f.FunctionArguments[1]);
        }
      }
      if (model.FunctionMap.ContainsKey("$local_value_is_bool"))
      {
        foreach (Function f in model.FunctionMap["$local_value_is_bool"])
        {
          AddExecutionState(model, f.FunctionArguments[0], f.FunctionArguments[1]);
        }
      }
      // Prune the execution states and remove all those entries, that contains no
      // line number information, and which state is defined with a valid code location.
      PruneExecutionStates(model);

      // Some functions have only been introduced to make Boogie2 happy. We will just
      // replace those functions with the original values.
      List<string> Boogie2TypeFunctions = new List<string>() { "$int_to_ptr", "$int_to_ptrset", "$ptr_to_int", "$ptrset_to_int" };
      foreach (string boogieFun in Boogie2TypeFunctions)
      {
        if (model.FunctionMap.ContainsKey(boogieFun))
        {
          foreach (Function boogieFunction in model.FunctionMap[boogieFun])
          {
            Partition result = boogieFunction.Result;
            Partition argument = boogieFunction.FunctionArguments[0];

            if (result.Id == argument.Id) continue;

            // Now find a function, which has the argument as a result and fill in the new result.
            // TODO

            foreach (string key in model.FunctionMap.Keys)
            {
              if (key == boogieFun) continue;

              foreach (Function fun in model.FunctionMap[key])
              {
                bool isModified = false;

                if (fun.Result == argument)
                {
                  // Replace the result of the Function
                  fun.SetResult(result);
                }

                // Also replace all arguments
                List<Partition> newArgs = new List<Partition>();
                foreach (Partition arg in fun.FunctionArguments)
                {
                  if (arg == argument)
                  {
                    newArgs.Add(result);
                    isModified = true;
                  }
                  else
                  {
                    newArgs.Add(arg);
                  }
                }
                if (isModified)
                {
                  fun.FunctionArguments = newArgs;
                }
              }
            }
          }
        }
      }

      // Set partition types for new state containers:
      if (model.FunctionMap.ContainsKey("$memory"))
      {
        foreach (Function memFunction in model.FunctionMap["$memory"])
        {
          memFunction.Result.SetType(PartitionType.Memory);
          memFunction.Result.SetValue(String.Format("$memory({0})", memFunction.FunctionArguments[0].DisplayValue));
        }
      }
      if (model.FunctionMap.ContainsKey("$statusmap"))
      {
        foreach (Function statusFunction in model.FunctionMap["$statusmap"])
        {
          statusFunction.Result.SetType(PartitionType.StatusMap);
          statusFunction.Result.SetValue(String.Format("$statusmap({0})", statusFunction.FunctionArguments[0].DisplayValue));
        }
      }
      if (model.FunctionMap.ContainsKey("$typemap"))
      {
        foreach (Function typeFunction in model.FunctionMap["$typemap"])
        {
          typeFunction.Result.SetType(PartitionType.TypeMap);
          typeFunction.Result.SetValue(String.Format("$typemap({0})", typeFunction.FunctionArguments[0].DisplayValue));
        }
      }


      // Set Partition type dot
      if (model.FunctionMap.ContainsKey("$dot"))
      {
        foreach (Function dotFunction in model.FunctionMap["$dot"])
        {
          dotFunction.Result.SetType(PartitionType.Dot);
        }
      }

      foreach (Function funct in model.FunctionMap["$typ"])
      {
        Partition p = funct.Result;
        p.Type = PartitionType.Type;
        if ((p.Set != null) && (!p.DisplayValue.StartsWith("#")))
        {
          if (!model.Types.Contains(p))
          {
            model.Types.Add(p);
            p.SetValue(ReplaceTypenames(p.Set.Elements[0]));
          }
        }
        else
        {
          if (!unresolved_types.Contains(p))
          {
            unresolved_types.AddLast(p);
          }
        }
      }

      if (model.FunctionMap.ContainsKey("$ptr_to"))
      {
        foreach (Function func in model.FunctionMap["$ptr_to"])
        {
          Partition result = func.Result;
          if (result.DisplayValue.StartsWith("#"))
          {
            distTpMap.Add(result.DisplayValue, result);

            result.SetValue(ReplaceTypenames(func.FunctionArguments[0].DisplayValue + "*"));
            model.Types.Add(result);
            if (unresolved_types.Contains(result))
            {
              unresolved_types.Remove(result);
            }
          }
        }
      }

      // Resolve unresolved types, until the list is empty.
      int startCount;
      do
      {
        startCount = unresolved_types.Count;
        LinkedListNode<Partition> current = unresolved_types.First;
        while (current != null)
        {
          Partition p = current.Value;
          if (model.FunctionMap.ContainsKey("$ptr_to"))
          {
            foreach (Function func in model.FunctionMap["$ptr_to"].FindAll(delegate(Function f) { return f.Result == p; }))
            {
              p.SetValue(ReplaceTypenames(func.FunctionArguments[0].DisplayValue + "*"));
              model.Types.Add(p);
              unresolved_types.Remove(p);
              break;
            }
          }
          if (model.FunctionMap.ContainsKey("$map_t"))
          {
            foreach (Function func in model.FunctionMap["$map_t"].FindAll(delegate(Function f) { return f.Result == p; }))
            {
              unresolved_types.Remove(p);
              break;
            }
          }

          current = current.Next;
        }
      }
      while ((unresolved_types.Count > 0) && (startCount != unresolved_types.Count));

      // Now we don't know anything concrete about the remaining types. Just assign $typ(X) as name.
      while (unresolved_types.Count > 0)
      {
        LinkedListNode<Partition> firstNode = unresolved_types.First;
        if (firstNode == null) 
          break;
        Partition unknownType = firstNode.Value;
        // We know, that there is at least one of this functions. Therefore unresolved_types.Count will decrease each loop.
        foreach (Function func in model.FunctionMap["$typ"].FindAll(delegate(Function f) { return f.Result == unknownType; }))
        {
          unknownType.SetValue(String.Format("$typ({0})", func.FunctionArguments[0].DisplayValue));
          model.Types.Add(unknownType);
          unresolved_types.Remove(unknownType);
          break;
        }
      }

      // Find array types and generate corresponding name mapping
      if (model.FunctionMap.ContainsKey("$array"))
      {
        foreach (Function arrayFunction in model.FunctionMap["$array"])
        {
          Partition arrayTypePartition = arrayFunction.Result;
          arrayTypePartition.SetType(PartitionType.Type);
          arrayTypePartition.SetValue(String.Format("{0}[{1}]",
            arrayFunction.FunctionArguments[0].DisplayValue, arrayFunction.FunctionArguments[1].Value));
        }
      }

      // Define pointers
      if (model.FunctionMap.ContainsKey("$ptr"))
      {
        foreach (Function ptrFunction in model.FunctionMap["$ptr"])
        {
          ptrFunction.Result.SetType(PartitionType.Pointer);
          if (ptrFunction.Result.DisplayValue.StartsWith("partition_"))
          {
            string typeString = ptrFunction.FunctionArguments[0].DisplayValue;
            string addrString = ptrFunction.FunctionArguments[1].DisplayValue;
            string ptrString = String.Format("({0}*){1}", typeString, addrString);
            ptrFunction.Result.SetValue(ptrString);
          }
        }
      }

      if (model.FunctionMap.ContainsKey("$map_t"))
      {
        foreach (Function mapFunction in model.FunctionMap["$map_t"])
        {
          Partition mapTypePartition = mapFunction.Result;
          mapTypePartition.SetType(PartitionType.Type);
          mapTypePartition.SetValue(String.Format("$map_t[{0} --> {1}]",
            mapFunction.FunctionArguments[0].DisplayValue, mapFunction.FunctionArguments[1].DisplayValue));
        }
      }


      if (model.FunctionMap.ContainsKey("$rev_ref_cnt_ptr"))
      {
        foreach (Function f in model.FunctionMap["$rev_ref_cnt_ptr"])
        {
          if ((f.Result.Value == null) || (f.Result.Value.StartsWith("call")))
          {
            if ((f.FunctionArguments[0].Value != null))
            {
              f.Result.SetValue(String.Format("$rev_ref_cnt_ptr({0})", f.FunctionArguments[0].Value));
            }
            else if ((f.FunctionArguments[0].Type == PartitionType.Pointer))
            {
              Function ptrFunction = model.FunctionMap["$ptr"].Find(delegate(Function fn) { return fn.Result == f.FunctionArguments[0]; });
              if (ptrFunction != null)
              {
                f.Result.SetValue(String.Format("$rev_ref_cnt_ptr($ptr({0},{1}))", ptrFunction.FunctionArguments[0].Value, ptrFunction.FunctionArguments[1].Value));
                // TODO?
                f.Result.SetType(PartitionType.Pointer);
              }
            }
          }
        }
      }

      if (model.FunctionMap.ContainsKey("$typ"))
      {
        foreach (Function typFunction in model.FunctionMap["$typ"])
        {
          if (typFunction.Result.Value == "$#ptrset")
          {
            typFunction.FunctionArguments[0].SetType(PartitionType.PtrSet);
          }
          if ((typFunction.Result.Value != null) && typFunction.Result.Value.StartsWith("$map_t"))
          {
            typFunction.FunctionArguments[0].SetType(PartitionType.Map);
          }
        }
      }

      // Now fill more values in the partitions
      List<string> BoogieTypes = new List<string>() { "i1", "i2", "i4", "i8", "u1", "u2", "u4", "u8" };
      foreach (string intType in BoogieTypes)
      {
        string uncheckedString = "$unchecked_" + intType;
        if (model.FunctionMap.ContainsKey(uncheckedString))
        {
          foreach (Function uncheckedFunction in model.FunctionMap[uncheckedString])
          {
            if (uncheckedFunction.FunctionArguments[0].Type == PartitionType.Value)
            {
              uncheckedFunction.Result.SetType(PartitionType.Value);
              uncheckedFunction.Result.SetValue(uncheckedFunction.FunctionArguments[0].Value);
            }
          }
        }
      }

      // Look for all types in the model and try to identify starting points
      foreach (Partition typePartition in model.Types)
      {
        // Look for $typ functions, which have the current type on the right hand side. 
        // If the argument of that function matches a third argument of $local_value_is, we
        // encountered a local value, which might be a starting point for the investigation.
        foreach (Function func in model.FunctionMap["$typ"].FindAll(delegate(Function f) { return f.Result == typePartition; }))
        {
          Partition var = func.FunctionArguments[0];
          if (model.FunctionMap.ContainsKey("$local_value_is"))
          {
            List<Function> localFunctions = model.FunctionMap["$local_value_is"].FindAll(delegate(Function f) { return f.FunctionArguments[3] == var; });
            if (localFunctions.Count > 0)
            {
              bool varNameSet = false;
              foreach (Function localFunc in localFunctions)
              {
                string varName = localFunc.FunctionArguments[2].Value;
                if (!varName.Contains("__vcc_alloc"))
                {
                  string shortname = varName.Substring(5);
                  if (shortname == "_this_") shortname = "this";

                  if (!varNameSet)
                  {
                    var.SetValue(shortname);
                    varNameSet = true;
                  }
                  var.AddAlias(shortname);
                }
              }
              if (varNameSet)
              {
                model.Variables.Add(var);
              }
            }
          }
        }
      }
      // Now look for all entries in $local_value_is, where the third argument is not in $typ and therefore not a pointer.
      // These entries are primitive, but should still be displayed.
      if (model.FunctionMap.ContainsKey("$local_value_is"))
      {
        foreach (Function local_val in model.FunctionMap["$local_value_is"])
        {
          Partition val = local_val.FunctionArguments[3];
          Partition type = local_val.FunctionArguments[4];

          List<Function> typFunctions = model.FunctionMap["$typ"].FindAll(delegate(Function f) { return f.FunctionArguments[0] == val; });
          if (typFunctions.Count == 0)
          {
            string varName = local_val.FunctionArguments[2].Value;
            if (!varName.Contains("__vcc_alloc"))
            {
              string local_name = varName.Substring(5);
              SourceLocation lineNumber = new SourceLocation();
              lineNumber.SetFromToken(local_val.FunctionArguments[1].DisplayValue, model.FilenameMap);
              string local_full_name = String.Format("{0} @ {1}", local_name, lineNumber.ToString());

              ExecutionState localExecState =
                FindExecutionState(model, local_val.FunctionArguments[0], local_val.FunctionArguments[1]);
              if (localExecState != null)
              {
                if (!type.DisplayValue.StartsWith("$map_t"))
                {
                  model.Locals.AddLocalVariable(localExecState, local_full_name, local_name, val, type, lineNumber);
                } else {
                  string mapTypeString = null;
                  Partition content = SelectResultIntToMap(model, val, ref mapTypeString);
                  if (content != null)
                  {
                    content.SetType(PartitionType.Map);
                    content.SetValue(mapTypeString);
                    model.Locals.AddLocalVariable(localExecState, local_name, local_name, content, type, lineNumber);
                  }
                }
              }
            }
          }
        }
      }

      if (model.FunctionMap.ContainsKey("$local_value_is_bool"))
      {
        foreach (Function local_val in model.FunctionMap["$local_value_is_bool"])
        {
          Partition val = local_val.FunctionArguments[3];
          val.SetType(PartitionType.Value);
          string varName = local_val.FunctionArguments[2].Value;
          if (!varName.Contains("__vcc_alloc"))
          {
            string local_name = varName.Substring(5);
            SourceLocation lineNumber = new SourceLocation();
            lineNumber.SetFromToken(local_val.FunctionArguments[1].DisplayValue, model.FilenameMap);
            string local_full_name = String.Format("{0} @ {1}", local_name, lineNumber.ToString());

            ExecutionState localExecState =
              FindExecutionState(model, local_val.FunctionArguments[0], local_val.FunctionArguments[1]);
            if (localExecState != null)
            {
              model.Locals.AddLocalVariable(localExecState, local_full_name, local_name, val, null, lineNumber);
            }
          }
        }
      }

      // Since the information about globals is currently rather limited, we try to guess the type here
      foreach (Function fun in model.FunctionMap["$ptr"])
      {
        Z3Model.Set aliases = fun.FunctionArguments[1].Set;
        string varname;

        if (aliases != null)
        {
          foreach (string addrinfo in aliases.Elements)
          {
            if (addrinfo.StartsWith("G#"))
            {
              string[] globalVar = addrinfo.Split('#');
              varname = globalVar[globalVar.Length - 2]; 
              aliases.Replace(addrinfo, varname);

              // this pointer references a global variable. So this could be the type
              model.Globals.AddGlobalVariable(varname, fun.Result, fun.FunctionArguments[0]);
            }

            if (addrinfo.StartsWith("Q#")) {
              string skolvar = GetQuantifierName(addrinfo, model.FilenameMap);
              aliases.Replace(addrinfo, skolvar);

              model.Globals.AddGlobalVariable(skolvar, fun.Result, fun.FunctionArguments[0]);
            }
          }
        }
      }

      foreach (KeyValuePair<int,Partition> globalPart in model.PartitionMap)
      {
        Partition p = globalPart.Value;
        if (p.Set != null) {
          foreach(string elem in p.Set.Elements) {
            if (elem.StartsWith("G#")) {
              string[] globalVar = elem.Split('#');
              string tpidx = globalVar[globalVar.Length-1];
              string globalName = globalVar[globalVar.Length - 2];

              Partition typePartition = FindDistinguishedType(tpidx, model, distTpMap);
              if (typePartition != null)
              {
                model.Globals.AddGlobalVariable(globalName, p, typePartition);
              }
              p.Set.Replace(elem, globalName);
            }
            if (elem.StartsWith("Q#"))
            {
              string skolvar = GetQuantifierName(elem, model.FilenameMap);

              SourceLocation sl = new SourceLocation();
              string[] skolemVar = elem.Split('#');
              string tpidx = skolemVar[skolemVar.Length - 1];
              Partition typePartition = FindDistinguishedType(tpidx, model, distTpMap);
              if (typePartition != null)
              {
                model.Globals.AddGlobalVariable(skolvar, p, typePartition);
              }
              p.Set.Replace(elem, skolvar);
            }
          }
        }
      }


      // Finally sort the execution states
      model.ExecutionStates.Sort();
      //model.Locals.PropagateLocalVariables(model.ExecutionStates);

      return model;
    }

    private static string GetQuantifierName(string addrinfo, Dictionary<int, string> filenameMap)
    {
      SourceLocation sl = new SourceLocation();
      sl.SetFromToken(addrinfo, filenameMap);
      string skolvar = addrinfo.Substring(2);
      skolvar = skolvar.Substring(0, skolvar.IndexOf("$"));
      skolvar = String.Format("{0} @ {1}", skolvar, sl.ToString());
      return skolvar;
    }

    private static string ReplaceTypenames(string p)
    {
      if (p.StartsWith("^^"))
      {
        return p;
      } 

      p = p.Replace("^^", "::");
      if (p.StartsWith("^"))
      {
        return p.Substring(1);
      }
      else
      {
        return p;
      }
    }

    private static List<int> FindModelsInFile(string FileContent)
    {
      List<int> retval = new List<int>();

      int pos = FileContent.IndexOf("partitions:");
      while (pos >= 0)
      {
        retval.Add(pos);
        pos = FileContent.IndexOf("partitions:", pos + 1);
      }

      return retval;
    }

    public static int FindNumberOfModelsInFile(string fileName)
    {
      TextReader tr = File.OpenText(fileName);

      string FileContent = tr.ReadToEnd();
      tr.Close();

      return FindModelsInFile(FileContent).Count;
    }

    private static Partition SelectResultIntToMap(Z3ModelVcc model, Partition content, ref string mapTypeString)
    {
      string funcname = "$int_to_map.";
      Partition retval = null;

      foreach (string key in model.FunctionMap.Keys)
      {
        if (key.StartsWith(funcname))
        {
          retval = SelectResult(model, key, content);
          if (retval != null)
          {
            mapTypeString = key.Substring(funcname.Length);
            return retval;
          }
        }
      }
      return null;
    }

    private static Partition SelectResult(Z3ModelVcc model, string functionType, Partition arg1)
    {
      if (model.FunctionMap.ContainsKey(functionType))
      {
        List<Function> functions = model.FunctionMap[functionType];
        if (functions == null)
        {
          return null;
        }
        functions = functions.FindAll(
          delegate(Function f)
          {
            return ((f.FunctionArguments[0] == arg1));
          });
        if (functions.Count == 1)
          return functions[0].Result;
      }
      return null;
    }
  
  }
}
