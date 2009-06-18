using System;
using System.Collections.Generic;
using Z3Model;
using Vcc2Model.Preprocessor;
using System.IO;
using System.Text.RegularExpressions;
using System.ComponentModel;

namespace Vcc2Model.Controller
{

  public class SourceLocation : IComparable, IEquatable<SourceLocation>
  {
    private int _line;
    private int _column;
    private string _filename, _shortfilename;

    public int Line
    {
      get { return _line; }
    }

    public int Column
    {
      get { return _column; }
    }

    public string FileName
    {
      get { return _filename; }
    }

    public string ShortFileName
    {
      get { return _shortfilename; }
    }

    public override string ToString()
    {
      string retval;
      string fn = "unknown";
      if ((_shortfilename != null) && (_shortfilename.Length > 0))
      {
        fn = _shortfilename;
      }
      retval = String.Format("{0} ({1}:{2})", fn, _line, _column);
      return retval;
    }

    public bool SetFromToken(string token, Dictionary<int, String> FilenameMap)
    {
      if (token.Contains("$"))
      {
        string fileinfotoken = token.Substring(token.IndexOf("$")+1);
        if (fileinfotoken.Contains("@@"))
        {
          fileinfotoken = fileinfotoken.Substring(0, fileinfotoken.IndexOf("@@"));
        }

        string filetoken;
        string linetoken;
        string coltoken;

        int fileindex;
        int hat_pos;

        hat_pos = fileinfotoken.IndexOf('^');
        if (hat_pos < 0) return false;

        filetoken = fileinfotoken.Substring(0, hat_pos);
        if (Int32.TryParse(filetoken, out fileindex))
        {
          // get the file name from the internal map in the model
          if (FilenameMap.ContainsKey(fileindex))
          {
            _filename = FilenameMap[fileindex];
            FileInfo fi = new FileInfo(_filename);
            _shortfilename = fi.Name;
          }
        }
        fileinfotoken = fileinfotoken.Substring(hat_pos + 1);

        // Parse line and column information
        hat_pos = fileinfotoken.IndexOf('.');
        if (hat_pos < 0) return false;

        linetoken = fileinfotoken.Substring(0, hat_pos);
        coltoken = fileinfotoken.Substring(hat_pos + 1);

        return Int32.TryParse(linetoken, out _line) && Int32.TryParse(coltoken, out _column);
      }
      return false;
    }

    public int CompareTo(object other)
    {
      if (!(other is SourceLocation)) return -1;

      SourceLocation other_loc = (SourceLocation)other;
      if (_line != other_loc._line)
      {
        return _line.CompareTo(other_loc._line);
      }
      if (_column != other_loc._column)
      {
        return _column.CompareTo(other_loc._column);
      }
      return 0;
    }

    public bool Equals(SourceLocation arg)
    {
      return (CompareTo(arg) == 0);
    }

    public override bool Equals(object arg)
    {
      if (!(arg is SourceLocation))
        return false;
      return (CompareTo(arg) == 0);
    }

    public static bool operator ==(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) == 0);
    }
    public static bool operator !=(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) != 0);
    }
    public static bool operator <(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) < 0);
    }
    public static bool operator >(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) > 0);
    }
    public static bool operator <=(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) <= 0);
    }
    public static bool operator >=(SourceLocation arg1, SourceLocation arg2)
    {
      return (arg1.CompareTo(arg2) >= 0);
    }
    public override int GetHashCode()
    {
      // Since the line information must be identical, if two elements are compared, this
      // is the required hash code for the dictionary access.
      return Line;
    }
  }

  public class ExecutionState : IComparable
  {
    public Partition state;
    //public Partition meta;
    public SourceLocation sourceInfo;
    public Partition timestamp;
    public bool good_state;

    public override string ToString()
    {
      string timestamp_str = "";
      if (timestamp != null)
      {
        timestamp_str = String.Format(", Timestamp {0}", timestamp.Value);
      }
      return String.Format("{0} : State {1}{2}", sourceInfo, state.Value, timestamp_str);
    }

    static int StateStringCompare(string arg1, string arg2)
    {
      // The first criteria is the length of the string. If they are not equal, just use the length of the string as criteria:
      // $state < $state@5, $state@9 < $state@10
      if (arg1.Length != arg2.Length)
      {
        return arg1.Length - arg2.Length;
      }
      // If the length is equal, we can use usual string comparison again, since this
      // will compare the strings with the same length in the way we expect it to be sorted.
      return arg1.CompareTo(arg2);
    }

    public int CompareTo(object other)
    {
      if (!(other is ExecutionState))
        return -1;
      ExecutionState otherExecState = ((ExecutionState)other);

      // First compare $meta and $state, since they just increase over time.
      if (state != otherExecState.state)
      {
        return StateStringCompare(state.Value, otherExecState.state.Value);
      }
      // The timestamp is not a comparison key, since it is directly related to state.
      // In this case, use the line number information as a sorting key.
      if (sourceInfo != otherExecState.sourceInfo)
      {
        return sourceInfo.CompareTo(otherExecState.sourceInfo);
      }
      // We are out of options. Claim, that both execution states are equal.
      return 0;
    }
  }

  public class GlobalVariables
  {
    protected Dictionary<string, Partition> _globals = new Dictionary<string, Partition>();
    protected Dictionary<string, Partition> _global_types = new Dictionary<string, Partition>();

    public List<string> GetListOfGlobalVariables()
    {
      List<string> retval = new List<string>();
      foreach (string identifier in _globals.Keys)
      {
        retval.Add(identifier);
      }
      return retval;
    }

    public Partition GetValueOfGlobalVariable(string varname)
    {
      if (_globals.ContainsKey(varname))
      {
        return _globals[varname];
      }
      return null;
    }

    public Partition GetTypeOfGlobalVariable(string varname)
    {
      if (_global_types.ContainsKey(varname))
      {
        return _global_types[varname];
      }
      return null;
    }

    public void AddGlobalVariable(string varname, Partition value, Partition type)
    {
      if (!_globals.ContainsKey(varname))
      {
        _globals[varname] = value;
      }

      if (!_global_types.ContainsKey(varname))
      {
        _global_types[varname] = type;
      }
    }
  }

  public class LocalVariables
  {
    protected Dictionary<ExecutionState, Dictionary<string, Partition>> _locals = new Dictionary<ExecutionState, Dictionary<string, Partition>>();
    protected Dictionary<ExecutionState, Dictionary<string, Partition>> _local_types = new Dictionary<ExecutionState, Dictionary<string, Partition>>();
    protected Dictionary<ExecutionState, Dictionary<string, string>>    _local_name = new Dictionary<ExecutionState, Dictionary<string, string>>();
    protected Dictionary<ExecutionState, List<string>> _local_name_clash = new Dictionary<ExecutionState, List<string>>();
    protected Dictionary<ExecutionState, Dictionary<string, SourceLocation>> _local_loc = new Dictionary<ExecutionState, Dictionary<string, SourceLocation>>();

    public List<string> GetListOfLocalVariables(ExecutionState loc)
    {
      List<string> retval = new List<string>();
      if (_locals.ContainsKey(loc))
      {
        foreach (string identifier in _locals[loc].Keys)
        {
          retval.Add(identifier);
        }
      }
      return retval;
    }

    public Partition GetValueOfLocalVariable(ExecutionState loc, string varname)
    {
      if (_locals.ContainsKey(loc))
      {
        if (_locals[loc].ContainsKey(varname))
        {
          return _locals[loc][varname];
        }
        if ((_local_name.ContainsKey(loc)) && (_local_name[loc].ContainsValue(varname)))
        {
          List<string> fullnames = GetFullNameOfLocalVariable(loc, varname);
          return _locals[loc][fullnames[0]];
        }
      }
      return null;
    }

    public List<KeyValuePair<SourceLocation, Partition>> GetValuesOfLocalVariable(ExecutionState loc, string varname)
    {
      List<KeyValuePair<SourceLocation, Partition>> retval = new List<KeyValuePair<SourceLocation, Partition>>();
      string shortname;

      if (varname == null) 
        return retval;

      if (_locals.ContainsKey(loc) && (_local_name.ContainsKey(loc)))
      {
        shortname = varname;
        if (_local_name[loc].ContainsKey(varname))
        {
          shortname = _local_name[loc][varname];
        }

        if (_local_name[loc].ContainsValue(shortname))
        {
          foreach (string fullname in GetFullNameOfLocalVariable(loc, shortname))
          {
            KeyValuePair<SourceLocation, Partition> pair = 
              new KeyValuePair<SourceLocation, Partition>(_local_loc[loc][fullname], _locals[loc][fullname]);
            retval.Add(pair);
          }
        }
      }
      return retval;
    }

    public Partition GetTypeOfLocalVariable(ExecutionState loc, string varname)
    {
      if ((_local_types.ContainsKey(loc)) && (_local_types[loc].ContainsKey(varname)))
      {
        return _local_types[loc][varname];
      }
      return null;
    }

    public string GetNameOfLocalVariable(ExecutionState loc, string varname)
    {
      if ((_local_types.ContainsKey(loc)) && (_local_types[loc].ContainsKey(varname)))
      {
        string shortname = _local_name[loc][varname];
        if (_local_name_clash[loc].Contains(shortname))
        {
          return varname;
        }
        else
        {
          return shortname;
        }
      }
      return null;
    }

    public string GetShortNameOfLocalVariable(ExecutionState loc, string varname)
    {
      if (_local_name.ContainsKey(loc))
      {
        if (_local_name[loc].ContainsKey(varname))
        {
          return _local_name[loc][varname];
        }
        if (_local_name[loc].ContainsValue(varname))
        {
          return varname;
        }
      }
      // The varname is not a local.
      return null;
    }

    public List<string> GetFullNameOfLocalVariable(ExecutionState loc, string varname)
    {
      List<string> retval = new List<string>();
      // First check, whether varname is already a full name:
      if (_locals.ContainsKey(loc) && _locals[loc].ContainsKey(varname))
      {
        retval.Add(varname);
      }
      // OK. It seems, that varname might be a short name
      else if ((_local_name.ContainsKey(loc)) && (_local_name[loc].ContainsValue(varname)))
      {
        foreach (var entry in _local_name[loc])
        {
          if (entry.Value.Equals(varname))
          {
            retval.Add(entry.Key);
          }
        }
      }
      // The varname is not a local.
      return retval;
    }

    public void AddLocalVariable(ExecutionState state, string varname, string shortname, Partition value, Partition type, SourceLocation location)
    {
      if (!_locals.ContainsKey(state))
      {
        _locals[state] = new Dictionary<string, Partition>();
      }
      if (!_locals[state].ContainsKey(varname))
      {
        _locals[state][varname] = value;
      }

      if (!_local_types.ContainsKey(state))
      {
        _local_types[state] = new Dictionary<string, Partition>();
      }
      if (!_local_types[state].ContainsKey(varname))
      {
        _local_types[state][varname] = type;
      }

      if (!_local_name.ContainsKey(state))
      {
        _local_name[state] = new Dictionary<string, string>();
      }
      if (!_local_name_clash.ContainsKey(state)) {
        _local_name_clash[state] = new List<string>();
      }

      if (!_local_name[state].ContainsKey(varname))
      {
        // handle clashed here...
        if (_local_name[state].ContainsValue(shortname))
        {
          _local_name_clash[state].Add(shortname);
        }
        _local_name[state][varname] = shortname;
      }

      if (!_local_loc.ContainsKey(state))
      {
        _local_loc[state] = new Dictionary<string, SourceLocation>();
      }
      if (!_local_loc[state].ContainsKey(varname))
      {
        if (location != null)
        {
          _local_loc[state][varname] = location;
        }
        else
        {
          _local_loc[state][varname] = state.sourceInfo;
        }
      }
    }
  }

  public class Z3ModelVcc2
  {
    public Dictionary<int, Partition> PartitionMap;
    public Dictionary<string, List<Function>> FunctionMap;
    public Dictionary<int, String> FilenameMap = new Dictionary<int,string>();

    public List<ExecutionState> ExecutionStates = new List<ExecutionState>();
    public List<Partition> Types = new List<Partition>();
    public List<Partition> Variables = new List<Partition>();
    public LocalVariables Locals = new LocalVariables();
    public GlobalVariables Globals = new GlobalVariables();

    public Dictionary<ExecutionState, List<FieldInfo>> models = new Dictionary<ExecutionState, List<FieldInfo>>();
    public Dictionary<ExecutionState, Dictionary<Partition, FieldInfo>> fieldNameDatabases = new Dictionary<ExecutionState, Dictionary<Partition, FieldInfo>>();
    public Dictionary<ExecutionState, List<FunctionInfo>> pureFunctions = new Dictionary<ExecutionState, List<FunctionInfo>>();
  }

  public class ModelController
  {
    Z3ModelVcc2 model;
    string _modelFileName;
    string _sourceFileName;
    int _modelNumber;
    int _modelsInFile;

    public ModelController()
    {
    }

    public string ModelFileName
    {
      get { return _modelFileName; }
    }

    public int ModelsInFile
    {
      get { return _modelsInFile; }
    }

    public int CurrentModel
    {
      get { return _modelNumber; }
    }

    public List<FieldInfo> GetModel(ExecutionState execState)
    {

      if ((model == null) || (model.models == null))
        return new List<FieldInfo>();
      return model.models[execState];
    }

    public List<ExecutionState> GetExecutionStates()
    {
      return model.ExecutionStates;
    }

    public List<FunctionInfo> GetPureFunctions(ExecutionState execState)
    {
      if ((model == null) || (model.pureFunctions == null))
        return new List<FunctionInfo>();
      return model.pureFunctions[execState];
    }

    public event EventHandler<ProgressChangedEventArgs> LoadProgressChanged;

    public void LoadModel(string sourceFileName, string modelFileName, int modelNumber)
    {
      if (modelFileName != null)
      {
        FileInfo fi = new FileInfo(modelFileName);
        if (fi.Exists)
        {
          model = ModelPreprocessor.parse(modelFileName, modelNumber);
          populateModels();

          _sourceFileName = sourceFileName;
          _modelFileName = modelFileName;
          _modelNumber = modelNumber;
          _modelsInFile = ModelPreprocessor.FindNumberOfModelsInFile(modelFileName);
        }
      }
    }

    public static int FindModelsInFile(string modelFileName)
    {
      return ModelPreprocessor.FindNumberOfModelsInFile(modelFileName);
    }

    void populateModels()
    {
      int numberOfStates = model.ExecutionStates.Count;
      int i = 0;
      foreach (ExecutionState exec in model.ExecutionStates)
      {
        // Invoke the event on the beginning of the loop, since we are sending 100% at the end of the loop anyway.
        OnProgressChanged((i * 100)/ numberOfStates);

        List<FieldInfo> newModel = new List<FieldInfo>();
        model.fieldNameDatabases[exec] = new Dictionary<Partition, FieldInfo>();
        model.pureFunctions[exec] = new List<FunctionInfo>();

        foreach (Partition variable in model.Variables)
        {
          FieldInfo modelInfo = LoadFieldInfo(variable, exec, variable.Value);
          newModel.Add(modelInfo);
        }

        foreach (string local_var in model.Locals.GetListOfLocalVariables(exec))
        {
          string shortname = model.Locals.GetNameOfLocalVariable(exec, local_var);
          Partition localVariablePartition = model.Locals.GetValueOfLocalVariable(exec, local_var);
          Partition localTypePartition = model.Locals.GetTypeOfLocalVariable(exec, local_var);
          FieldInfo modelInfo = LoadFieldInfo(localVariablePartition, exec, /*local_var*/ shortname , localTypePartition);
          newModel.Add(modelInfo);
        }

        foreach (string global_var in model.Globals.GetListOfGlobalVariables())
        {
          Partition globalVariablePartition = model.Globals.GetValueOfGlobalVariable(global_var);
          Partition globalTypePartition = model.Globals.GetTypeOfGlobalVariable(global_var);
          FieldInfo modelInfo = LoadFieldInfo(globalVariablePartition, exec, global_var, globalTypePartition);
          newModel.Add(modelInfo);
        }
        
        model.models.Add(exec, newModel);

        // Fill the List of user defined functions
        foreach (string funcName in model.FunctionMap.Keys)
        {
          if (funcName.StartsWith("#"))
          {
            foreach (Function func in model.FunctionMap[funcName])
            {
              if (func.FunctionArguments.Contains(exec.state) || 
                  !ListContainsStates(func.FunctionArguments))
              {
                FunctionInfo fi = new FunctionInfo();
                fi.ExecState = exec;
                fi.Function = func;
                model.pureFunctions[exec].Add(fi);
              }
              else
              {
                // Check whether a function argument is a struct passed by value:
                int index = 0;
                foreach (Partition partition in func.FunctionArguments)
                {
                  index++;
                  Partition pointer = FindValueStructInState(exec.state, partition);
                  if (pointer != null)
                  {
                    FunctionInfo fi = new FunctionInfo();
                    fi.ExecState = exec;
                    fi.Function = func;
                    model.pureFunctions[exec].Add(fi);

                    string temp_name = String.Format("{0}_arg{1}", funcName.Substring(1), index);
                    FieldInfo modelInfo = LoadFieldInfo(pointer, exec, temp_name);
                    partition.SetValue(modelInfo.FieldName);
                  }
                }
              }
            }
          }
        }


        i++;
      }
      OnProgressChanged(100);
    }

    private bool ListContainsStates(List<Partition> functionArguments)
    {
      foreach (ExecutionState exec in model.ExecutionStates)
      {
        foreach (Partition argPart in functionArguments)
        {
          if ((argPart == exec.state) ||
              (FindValueStructInState(exec.state, argPart) != null))
          {
            return true;
          }
        }
      }
      return false;
    }

    private void OnProgressChanged(int progress)
    {
      EventHandler<ProgressChangedEventArgs> temp = LoadProgressChanged;
      if (temp != null)
      {
        temp(this, new ProgressChangedEventArgs(progress, null));
      }
    }

    void LoadDotContainerInfo(Partition p, ExecutionState execState, string fieldName, DotContainerInfo result)
    {
      result.Functions = GetFunctionInfoList(p, execState);
      List<Function> dots = GetDotFunctionsFor(p);
      if ((dots != null) && (dots.Count > 0))
      {
        foreach (Function dot in dots)
        {
          string dotName;
          if (dot.FunctionArguments[1].Value != null)
          {
            String[] fieldList = dot.FunctionArguments[1].Value.Split('.');
            dotName = String.Format("&{0}->{1}", fieldName, fieldList[fieldList.Length - 1]);
          }
          else
          {
            Partition ptr_type = SelectResult("$typ", dot.FunctionArguments[0]);
            Partition owns_set_field = SelectResult("$owns_set_field", ptr_type);
            if (owns_set_field != null)
            {
              dotName = String.Format("&{0}->$owns", fieldName);
              dot.FunctionArguments[1].SetValue(ptr_type.Value + ".$owns");
            }
            else
            {
              dotName = String.Format("&{0}->(anonymous field)", fieldName);
            }
          }

          DotInfo dotInfo = LoadDotInfo(dot, execState, GetFieldNameOfDot(dotName));
          if (dotInfo != null)
          {
            result.Dots.Add(dotInfo);
          }
        }
      }
      else
      {
        // If p points to a primitive type, we should at least display the dereferenced value (if available)
        Partition selectResult = GetContentInState(p, execState);
        result.DerefValue = selectResult;
        return;
      }
    }

    private List<DotInfo> UnpackDotsOfDots(DotInfo dotInfo)
    {
      List<DotInfo> dotOfDots = new List<DotInfo>();
      if (dotInfo.Dots.Count > 0)
      {
        foreach (DotInfo dod in dotInfo.Dots)
        {
          dotOfDots.Add(dod);
          // Recurse trough dots of dots of dots...
          List<DotInfo> childredOfDotOfDots = UnpackDotsOfDots(dod);
          if (childredOfDotOfDots.Count > 0)
          {
            foreach (DotInfo childDod in childredOfDotOfDots)
            {
              dotOfDots.Add(childDod);
            }
          }
        }
      }
      return dotOfDots;
    }

    ObjectInfo LoadObjectInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      ObjectInfo result = new ObjectInfo();

      if (!model.fieldNameDatabases[execState].ContainsKey(p))
      {
        model.fieldNameDatabases[execState][p] = result;
      }

      LoadDotContainerInfo(p, execState, fieldName, result);

      return result;
    }

    ArrayInfo LoadArrayInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      if (!model.FunctionMap.ContainsKey("$idx")) return null;

      ArrayInfo info = new ArrayInfo();
      info.FieldName = fieldName + "[]";


      List<Function> idxFunctions = model.FunctionMap["$idx"].FindAll(delegate(Function fn) { return fn.FunctionArguments[0] == p; });
      if (idxFunctions.Count == 0)
        return null;

      Partition arrayElementType = SelectResult("$typ", p);
      Partition arrayRef = SelectResult("$ref", p);

      // we have an aray
      foreach (Function idx_fn in idxFunctions)
      {
        int idx;
        if (Int32.TryParse(idx_fn.FunctionArguments[1].Value, out idx))
        {
          string arrayIndexName = String.Format("{0}[{1}]", fieldName, idx);
          Partition field = GetContentInState(idx_fn.Result, execState);
          if (field != null)
          {
            DotInfo di = new DotInfo();
            di.FieldName = String.Format("&{0}", arrayIndexName);
            di.FieldPartition = idx_fn.Result;
            di.ExecState = execState;
            di.Field = null;
            di.FieldTypePartition = null;
            FieldInfo fi = LoadFieldInfo(field, execState, GetFieldNameOfDotDereference(di.FieldName), di);
            info.ElementDots[idx] = fi;
          }
          else
          {
            ObjectInfo oi = LoadObjectInfo(idx_fn.Result, execState, arrayIndexName, null);
            oi.FieldName = arrayIndexName;
            oi.FieldTypePartition = arrayElementType;
            info.ElementDots[idx] = (FieldInfo)oi;
          }
        }
      }


      foreach (Function ptrFunc in model.FunctionMap["$ptr"])
      {
        Partition array_ptr = ptrFunc.Result;
        if (model.FunctionMap.ContainsKey("$array"))
        {
          foreach (Function arrayFunc in model.FunctionMap["$array"])
          {
            Partition array_type = arrayFunc.Result;
            if (arrayFunc.FunctionArguments[0] == arrayElementType)
            {
              if (ptrFunc.FunctionArguments[0] == array_type)
              {
                if (ptrFunc.FunctionArguments[1] == arrayRef)
                {
                  //array interpretation with size: arrayFunc.FunctionArguments[1] and type arrayFunc.Result
                  array_ptr.SetValue(String.Format("{0}[]", fieldName));
                  FieldInfo arrayDot = LoadFieldInfo(array_ptr, execState, array_ptr.Value, array_type, null, true);
                  info.ArrayInterpretations.Add(arrayDot);
                }
              }
            }
          }
        }
      }

      info.NotInterpretedPointer = LoadFieldInfo(p, execState, fieldName, null, referringDotInfo, true);

      return info;
    }

    DotInfo LoadDotInfo(Function dot, ExecutionState execState, string fieldName)
    {
      DotInfo result = new DotInfo();
      // we have to assign them here, since we are not going through LoadFieldInfo!
      FillFieldInfoAttributes(result, fieldName, dot.Result, execState, null);

      if (!model.fieldNameDatabases[execState].ContainsKey(dot.Result))
      {
        model.fieldNameDatabases[execState][dot.Result] = result;
      }
      result.HeapAddress = GetRefOf(dot.Result);
      Partition field = GetContentInState(dot.Result, execState);
      if (field != null)
      {
        result.Field = LoadFieldInfo(field, execState, GetFieldNameOfDotDereference(fieldName), result);
      }

      if (SelectArgument("$ghost_path", dot.FunctionArguments[1], 0) != null)
      {
        result.Ghost = true;
      }
      if (EvalBooleanPartition(SelectResult("$is_primitive_volatile_field", dot.FunctionArguments[1])))
      {
        result.Volatile = true;
      }
      if (EvalBooleanPartition(SelectResult("$is_primitive_non_volatile_field", dot.FunctionArguments[1])))
      {
        result.Volatile = false;
      }

      //dot-of-dot
      LoadDotContainerInfo(dot.Result, execState, fieldName, result);

      result.Functions = GetFunctionInfoList(dot.Result, execState);
      return result;
    }

    FieldInfo LoadFieldInfo(Partition p, ExecutionState execState, string fieldName)
    {
      return LoadFieldInfo(p, execState, fieldName, null, null, false);
    }

    FieldInfo LoadFieldInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      return LoadFieldInfo(p, execState, fieldName, null, referringDotInfo, false);
    }

    FieldInfo LoadFieldInfo(Partition p, ExecutionState execState, string fieldName, Partition typePartition)
    {
      return LoadFieldInfo(p, execState, fieldName, typePartition, null, false);
    }

    FieldInfo UpdateFieldInfoAliases(FieldInfo fieldInfo, string fieldName)
    {
      // First of all: if the fieldName is not in the list of Aliases, just add it:
      fieldInfo.AddAlias(fieldName);

      // if the FieldName field is not set and both values differ, select the shorter name
      if ((fieldInfo.FieldName != null) && (fieldInfo.FieldName != fieldName))
      {
        if (fieldInfo.FieldName.Length > fieldName.Length)
        {
          fieldInfo.FieldName = fieldName;
        }
      }
      return fieldInfo;
    }

    FieldInfo LoadFieldInfo(Partition p, ExecutionState execState, string fieldName, Partition typePartition, DotInfo referringDotInfo, bool disableArrayLoading)
    {
      if (model.fieldNameDatabases[execState].ContainsKey(p))
      {
        return UpdateFieldInfoAliases(model.fieldNameDatabases[execState][p], fieldName);
      }
      FieldInfo info = null;

      // Delegate loading ArrayInfo through idx functions
      if (!disableArrayLoading)
      {
        info = LoadArrayInfo(p, execState, fieldName, referringDotInfo);
      }
      if (info == null)
      {
        switch (p.Type)
        {
          case PartitionType.Value:
          case PartitionType.State:
            if (typePartition == null)
            {
              info = LoadPrimitiveFieldInfo(p, execState, fieldName, referringDotInfo);
            }
            else
            {
              info = LoadPrimitiveFieldInfo(p, execState, fieldName, typePartition);
            }
            break;
          case PartitionType.Pointer:
            info = LoadObjectInfo(p, execState, fieldName, referringDotInfo);
            break;
          case PartitionType.PtrSet:
            info = LoadPtrSetInfo(p, execState, fieldName, referringDotInfo);
            break;
          case PartitionType.Map:
            if (typePartition == null)
            {
              info = LoadMapFieldInfo(p, execState, fieldName, referringDotInfo);
            }
            else
            {
              info = LoadMapFieldInfo(p, execState, fieldName, null, typePartition);
            }
            break;
          default:
            info = new FieldInfo();
            info.FieldName = fieldName;
            break;
        }
      }

      FillFieldInfoAttributes(info, fieldName, p, execState, referringDotInfo);

      return info;
    }

    void FillFieldInfoAttributes(FieldInfo info, string fieldName, Partition p, ExecutionState execState, DotInfo referringDotInfo)
    {
      info.ExecState = execState;

      info.FieldName = info.FieldName ?? fieldName;
      info.FieldPartition = info.FieldPartition ?? p;
      info.FieldTypePartition = info.FieldTypePartition ?? GetTypeOf(p);
      info.HeapAddress = info.HeapAddress ?? GetRefOf(p);
      info.Owner = info.Owner ?? GetOwnerOf(p, execState);
      info.TimeStamp = info.TimeStamp ?? GetTimestampOf(p, execState);

      info.ThreadOwner = info.ThreadOwner ?? GetThreadOwnerOf(p, execState);

      info.ThreadLocal = GetThreadLocalStateOf(p, execState);
      info.Closed = GetClosedStateOf(p, execState);
      info.Mutable = GetMutableStateOf(p, execState);
      info.Claimable = GetClaimableStateOf(p);
      info.Primitive = GetPrimitiveStateOfTypeOf(p);
      info.Typed = GetTypedStateOf(p, execState);
      info.ArrayElement = GetIsArrayElementStateOf(p, execState);
      // These type qualifiers are actually only for the pointer operation, but are visualized
      // still on the displayed value, since this is how the implementer specified the code.
      if (referringDotInfo != null)
      {
        info.Volatile = referringDotInfo.Volatile;
      }
      else
      {
        info.Volatile = GetIsVolatileStateOf(p, execState);
      }
      info.Dot = info.Dot ?? referringDotInfo;
    }


    PtrSetFieldInfo LoadPtrSetInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      PtrSetFieldInfo result = new PtrSetFieldInfo();
      if (referringDotInfo != null)
      {
        result.FieldTypePartition = GetTypeOf(referringDotInfo.FieldPartition);
      }
      result.FieldName = fieldName;

      List<PtrSetEntry> entries = new List<PtrSetEntry>();
      List<Partition> processedPartitions = new List<Partition>();
      List<string> setFunctions = GetSetFunctions();
      foreach (string functionType in setFunctions)
      {
        foreach (Function f in model.FunctionMap[functionType].FindAll(delegate(Function fn) { return fn.FunctionArguments[1] == p; }))
        {

          Partition entryPartition = f.FunctionArguments[0];
          if (!processedPartitions.Contains(entryPartition))
          {
            processedPartitions.Add(entryPartition);
            PtrSetEntry entry = new PtrSetEntry();
            entry.Field = entryPartition;
            entry.fieldNameDatabase = model.fieldNameDatabases[execState];
            entry.PtrFunction = model.FunctionMap["$ptr"].Find(delegate(Function fn) { return (fn.Result == entryPartition); });
            entry.strongestSetOperation = functionType;
            entry.isInSet = bool.Parse(f.Result.Value);
            entry.TypePartition = GetTypeOf(entryPartition);
            entries.Add(entry);
          }
        }
      }
      result.Entries = entries;

      /*
      result.OwnsSetField = false;
      result.IsVolatile = false;
      if (model.FunctionMap.ContainsKey("$owns_set_field"))
      {
        Function osf = model.FunctionMap["$owns_set_field"].Find(
          delegate(Function fn) { return fn.Result == referringDotInfo.DotFunction.FunctionArguments[1]; });
        if (osf != null)
        {
          result.OwnsSetField = true;
          result.IsVolatile = EvalBooleanPartition(SelectResult("$has_volatile_owns_set", osf.FunctionArguments[0]));
        }
      }
      */
      return result;
    }

    MapFieldInfo LoadMapFieldInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      if (referringDotInfo == null)
      {
        return LoadMapFieldInfo(p, execState, fieldName, null, null);
      }
      else
      {
        return LoadMapFieldInfo(p, execState, fieldName, referringDotInfo, referringDotInfo.FieldTypePartition);
      }
    }

    MapFieldInfo LoadMapFieldInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo, Partition fieldTypePartition)
    {
      MapFieldInfo result = new MapFieldInfo();
      Partition map_range_base = null;
      Partition map_domain_base = null;

      if (fieldTypePartition != null)
      {
        Partition map_range = null;
        Partition map_domain = null;

        result.FieldTypePartition = fieldTypePartition;
        map_range = SelectResult("$map_range", fieldTypePartition);
        map_domain = SelectResult("$map_domain", fieldTypePartition);

        if (map_range != null)
        {
          map_range_base = SelectArgument("$ptr_to", map_range, 0);
        }
        if (map_domain != null)
        {
          map_domain_base = SelectArgument("$ptr_to", map_domain, 0);
        }
      }
      result.FieldName = fieldName;

      List<MapEntry> entries = new List<MapEntry>();
      string selectMapString = SelectMapString(p.DisplayValue);

      if (model.FunctionMap.ContainsKey(selectMapString))
      {
        foreach (Function f in model.FunctionMap[selectMapString].FindAll(delegate(Function fn) { return fn.FunctionArguments[0] == p; }))
        {
          MapEntry entry = new MapEntry();
          entry.Key = f.FunctionArguments[1];
          entry.Value = f.Result;
          entry.fieldNameDatabase = model.fieldNameDatabases[execState];
          entry.MapDot = referringDotInfo;
          entry.MapName = fieldName;
          // Set backwards reference
          entry.MapContainer = result;
          entries.Add(entry);

          // Now try to find aliases for the pointed memory areas...
          string mapname = String.Format("{0}[{1}]", fieldName, entry.Key.DisplayValue);
          if (map_range_base != null)
          {
            if (map_range_base.DisplayValue == "void")
            {
              entry.KeyField = LoadFieldInfo(entry.Key, execState, entry.Key.DisplayValue);
            }
            else
            {
              Partition keyPart = SelectResult("$ptr", map_range_base, entry.Key);
              if (keyPart != null)
              {
                entry.KeyField = LoadFieldInfo(keyPart, execState, entry.Key.DisplayValue, map_range_base);
              }
            }
          }
          if (map_domain_base != null)
          {
            if (map_domain_base.DisplayValue == "void")
            {
              entry.ValueField = LoadFieldInfo(entry.Value, execState, mapname);
            }
            else
            {
              Partition valuePart = SelectResult("$ptr", map_domain_base, entry.Value);
              if (valuePart != null)
              {
                entry.ValueField = LoadFieldInfo(valuePart, execState, mapname, map_domain_base);
              }
            }
          }
        }
      }
      result.Entries = entries;
      return result;
    }

    PrimitiveFieldInfo LoadPrimitiveFieldInfo(Partition p, ExecutionState execState, string fieldName, DotInfo referringDotInfo)
    {
      Partition typePart = null;
      if (referringDotInfo != null)
      {
        typePart = GetTypeOf(referringDotInfo.FieldPartition);
      }
      return LoadPrimitiveFieldInfo(p, execState, fieldName, typePart);
    }

    PrimitiveFieldInfo LoadPrimitiveFieldInfo(Partition p, ExecutionState execState, string fieldName, Partition type)
    {
      PrimitiveFieldInfo result = new PrimitiveFieldInfo();
      result.FieldValue = p.Value;
      result.FieldTypePartition = type;
      return result;
    }

    Function GetFunctionInState(ExecutionState exec, Function function, ExecutionState currentExecutionState)
    {
      Partition oldState = currentExecutionState.state;
      foreach (Function f in model.FunctionMap[function.FunctionType])
      {
        bool match = true;
        for (int i = 0; i < function.FunctionArguments.Count; i++)
        {
          if (!
            (((function.FunctionArguments[i] != oldState) || (f.FunctionArguments[i] == exec.state)) &&
            ((function.FunctionArguments[i] == oldState) || (f.FunctionArguments[i] == function.FunctionArguments[i]))))
          {
            match = false;
          }
        }
        if (match)
        {
          return f;
        }
      }
      return null;
    }


    public Dictionary<ExecutionState, string> GetValuesStates(ModelInfo mi, ref string valueName, ExecutionState currentExecutionState)
    {
      Dictionary<ExecutionState, string> result = new Dictionary<ExecutionState, string>();
      Partition lastValuePartition = null;
      int lastValuePartitionIndex = 0;

      foreach (ExecutionState e in model.ExecutionStates)
      {
        string value = null;
        if (mi is PrimitiveFieldInfo)
        {
          PrimitiveFieldInfo pfi = (PrimitiveFieldInfo)mi;
          if (pfi.Dot != null)
          {
            Partition valuePartition = GetContentInState(pfi.Dot.DotPartition, e);
            if (valuePartition != null)
            {
              value = valuePartition.Value;
              valueName = pfi.FieldName;
            }
          }
          else
          {
            // This might be a local variable
            string local_var = model.Locals.GetShortNameOfLocalVariable(currentExecutionState, pfi.FieldName);

            List<KeyValuePair<SourceLocation, Partition>> localVariablePartitions = model.Locals.GetValuesOfLocalVariable(e, local_var);
            foreach (KeyValuePair<SourceLocation, Partition> pair in localVariablePartitions)
            {
              SourceLocation location = pair.Key;
              Partition localVariablePartition = pair.Value;
              ExecutionState newState = new ExecutionState();
              newState.state = e.state;
              newState.sourceInfo = location;
              newState.timestamp = e.timestamp;
              newState.good_state = e.good_state;
              value = localVariablePartition.Value;
              if (!result.ContainsKey(newState))
              {
                result.Add(newState, value);
              }
            }
            if (value != null)
            {
              valueName = local_var;
              value = null;
            }
          }
        }
        else if (mi is FunctionInfo)
        {
          FunctionInfo fi = (FunctionInfo)mi;
          Function f = GetFunctionInState(e, fi.Function, currentExecutionState);
          if (f != null)
          {
            value = f.Result.Value;
            if (value == null)
            {
              value = "";
            }
          }
          valueName = GetStateStrippedFunctionString(fi.Function, null, currentExecutionState);
        }
        else if (mi is MapEntry)
        {
          MapEntry me = (MapEntry)mi;
          MapFieldInfo mfi = me.MapContainer;

          if (me.MapDot != null)
          {
            Partition dot = me.MapDot.DotPartition;
            Partition newMap = GetContentInState(dot, e);
            if (newMap != null)
            {
              string selectMapString = SelectMapString(newMap.DisplayValue);

              Partition newValuePart = SelectResult(selectMapString, newMap, me.Key);
              if (newValuePart != null)
              {
                value = newValuePart.DisplayValue;
                valueName = String.Format("{0}[{1}]", me.MapName, me.KeyName);
              }
            }
          }
        }
        else if (mi is PtrSetFieldInfo)
        {
          PtrSetFieldInfo psfi = (PtrSetFieldInfo)mi;
          if ((psfi.Dot != null) && (psfi.Dot.DotPartition != null))
          {
            Partition dotPartition = psfi.Dot.DotPartition;
            Partition valuePartition = GetContentInState(dotPartition, e);
            if (valuePartition != null)
            {
              if (lastValuePartition != valuePartition)
              {
                lastValuePartition = valuePartition;
                lastValuePartitionIndex++;
              }
              value = psfi.FieldName + " (" + lastValuePartitionIndex + ")";
              valueName = psfi.FieldName;
            }
          }
        }
        else if (mi is PtrSetEntry)
        {
          if (currentExecutionState == e)
          {
            PtrSetEntry pse = (PtrSetEntry)mi;
            value = String.Format("{0}({1}) = {2}", pse.strongestSetOperation, pse.FieldName, pse.isInSet);
            valueName = pse.FieldName;
          }
        }
        else if (mi is DotContainerInfo)
        {
          DotContainerInfo di = (DotContainerInfo)mi;
          if (di.Dot != null)
          {
            Partition dotPartition = di.Dot.DotPartition;
            if (dotPartition != null)
            {
              Partition valuePartition = GetContentInState(dotPartition, e);
              if (valuePartition != null)
              {
                if (valuePartition.Type == PartitionType.Pointer)
                {
                  Function ptr = model.FunctionMap["$ptr"].Find(delegate(Function fn) { return fn.Result == valuePartition; });
                  string ptr_type = ptr.FunctionArguments[0].Value;
                  string ptr_ref = ptr.FunctionArguments[1].Value;

                  value = String.Format("({0}, {1})", ptr_type, ptr_ref);
                }
                else
                {
                  value = di.FieldName;
                }
                valueName = di.FieldName;
              }
            }
          } 
          else
          {
            // These are top level objects, that have to be treated differently.
            // This might be a local variable pointer. If this is a primitive type, we
            // should display the value, since the address of the pointer itself should not change.
            // (The address the pointer points to might change nevertheless.)
            valueName = di.FieldName;

            foreach (FieldInfo info in model.models[e])
            {
              if ((info != null) && (info.FieldName == valueName) && (info is DotContainerInfo))
              {
                DotContainerInfo dcinfo = (DotContainerInfo) info;
                if (dcinfo.Primitive)
                {
                  if (dcinfo.DerefValue != null)
                  {
                    value = dcinfo.DerefValue.DisplayValue;
                  }
                }
                else
                {
                  if ((dcinfo.FieldType != null) && (dcinfo.HeapAddress != null)) {
                    value = String.Format("({0}, {1})", dcinfo.FieldType, dcinfo.HeapAddress.Value);
                  }
                }
              }
            }
          }
        }
        if (value != null)
        {
          result.Add(e, value);
        }
      }
      return result;
    }

    public string GetStateStrippedFunctionString(Function function, string fieldName, ExecutionState execState)
    {
      string arguments = String.Join(", ", function.FunctionArguments.ConvertAll(delegate(Partition p)
      {
        if ((p.Type == PartitionType.State) && (execState.state == p))
          return "state";
        else if (model.fieldNameDatabases[execState].ContainsKey(p))
        {
          if ((fieldName != null) &&
              ((fieldName == model.fieldNameDatabases[execState][p].FieldName) ||
               (model.fieldNameDatabases[execState][p].Aliases.Contains(fieldName))))
          {
            return fieldName;
          }
          else
          {
            return model.fieldNameDatabases[execState][p].FieldName;
          }
        }
        else
        {
          return p.DisplayValue;
        }
      }).ToArray());
      string funcName = function.FunctionType;
      if (funcName.StartsWith("#")) { 
        funcName = funcName.Substring(1); 
      }
      return String.Format("{0}({1})", funcName, arguments);
    }

    public string GetStateStrippedFunctionStringWithResult(Function function, string fieldName, ExecutionState execState)
    {
      string resultValue;
      if ((function.Result.Type == PartitionType.State) && (function.Result == execState.state))
      {
        resultValue = "state";
      }
      else if (model.fieldNameDatabases[execState].ContainsKey(function.Result))
      {
        if ((fieldName != null) && (
             (fieldName == model.fieldNameDatabases[execState][function.Result].FieldName) ||
             (model.fieldNameDatabases[execState][function.Result].Aliases.Contains(fieldName))))
        {
          resultValue = fieldName;
        }
        else
        {
          resultValue = model.fieldNameDatabases[execState][function.Result].FieldName;
        }
      }
      else
      {
        resultValue = function.Result.DisplayValue;
      }
      return String.Format("{0} = {1}", GetStateStrippedFunctionString(function, fieldName, execState), resultValue);
    }


    public int GetSetPriority(string setString)
    {
      if (setString.Equals("$set_in"))
      {
        return 1;
      }
      return Int32.Parse(setString.Substring(7));
    }

    public List<string> GetSetFunctions()
    {
      List<string> setFunctions = new List<string>();
      Regex set_inRegex = new Regex("^[$]+set_in[0-9]*$");
      foreach (string functionType in model.FunctionMap.Keys)
      {
        if (set_inRegex.IsMatch(functionType))
        {
          setFunctions.Add(functionType);
        }

      }
      setFunctions.Sort(delegate(string s1, string s2) { return GetSetPriority(s1).CompareTo(GetSetPriority(s2)); });
      return setFunctions;
    }

    private Partition SelectResultIntToMap(Partition content, ref string mapTypeString)
    {
      string funcname = "$int_to_map.";
      Partition retval = null;

      foreach (string key in model.FunctionMap.Keys)
	    {
		    if (key.StartsWith(funcname)) {
          retval = SelectResult(key, content);
          if (retval != null)
          {
            mapTypeString = key.Substring(funcname.Length);
            return retval;
          }
        }
	    }
      return null;
    }

    private string SelectMapString(string mapTypeString)
    {
      return String.Format("$select.$map.{0}", mapTypeString.Replace("ptr.", "$ptr.").Replace(".ptr", ".$ptr"));
    }

    private Partition SelectResultMap(Partition intToMap, string mapTypeString)
    {
      string functionType = SelectMapString(mapTypeString);
      int nbArgument = 1;

      if (model.FunctionMap.ContainsKey(functionType))
      {
        List<Function> functions = model.FunctionMap[functionType];
        if (functions == null)
        {
          return null;
        }
        functions = functions.FindAll(delegate(Function fn) { return fn.FunctionArguments[0] == intToMap; });
        if ((functions.Count == 1) && (functions[0].FunctionArguments.Count > nbArgument))
        {
          return functions[0].FunctionArguments[nbArgument];
        }
      }
      return null;
    }

    private Partition GetContentInState(Partition field, ExecutionState execState)
    {
      Partition content = null;
      Partition state = execState.state;
      Partition memory = GetMemoryForState(state);
      Partition type = GetTypeOf(field);
      if (type != null)
      {
        switch (type.Value)
        {
          case "^^u1":
          case "^^u2":
          case "^^u4":
          case "^^u8":
          case "^^i1":
          case "^^i2":
          case "^^i4":
          case "^^i8":
            content = SelectResult("$read_" + type.Value.Substring(2), state, field);
            if (content != null)
            {
              content.SetType(PartitionType.Value);
            }
            break;
          case "^^bool":
            content = SelectResult("$select.mem", memory, field); 
            if (content != null)
            {
              content.SetType(PartitionType.Value);
            }
            break;
          case "$#ptrset":
            content = SelectResult("$select.mem", memory, field);
            if (content != null)
            {
              content.SetType(PartitionType.PtrSet);
            }
            break;
          default:
            if ((type.Value != null) && (type.Value.StartsWith("$map_t")))
            {
              content = SelectResult("$select.mem", memory, field);
              if (content != null)
              {
                // content is the address of the data where the map is stored
                //content.SetType(PartitionType.Map);
                string mapTypeString = null;
                content = SelectResultIntToMap(content, ref mapTypeString);
                if (content != null)
                {
                  content.SetType(PartitionType.Map);
                  content.SetValue(mapTypeString);
                }
              }
            }
            else
            {
              // First read a value from memory. This can be a simple value, but if
              // we also try to interprete this in addition with the type information,
              // we might end up with a pointer we can further dereference.
              content = SelectResult("$select.mem", memory, field);
              if ((content != null) && (type != null) && 
                  (content.Type == PartitionType.Value) && 
                  (type.Value.EndsWith("*")))
              {
                Partition baseType = SelectResult("$unptr_to", type);
                if (baseType != null)
                {
                  Partition ptrResult = SelectResult("$ptr", baseType, content);
                  if (ptrResult != null)
                  {
                    content = ptrResult;
                  }
                }
              }
              // Since our previous attempts failed to find values for this fieldm just try to
              // use $read_ptr_m to find a usable pointer.
              if (content == null)
              {
                content = SelectResult("$read_ptr_m", state, field);
              }
            }
            break;
        }
      }
      return content;
    }

    static string GetNormalisedFormForAddressOperator(string dotFieldName)
    {
      if (dotFieldName.StartsWith("&"))
      {
        string result = dotFieldName.Substring(1);
        int pos, lastPos = -1;
        do
        {
          pos = result.IndexOf("->", lastPos+1);
          if (pos >= 0)
            lastPos = pos;
        } while (pos != -1);

        if (lastPos >= 0)
        {
          result = result.Remove(lastPos, 2);
          result = result.Insert(lastPos, ".");
        }
        return result;
      }
      else
      {
        return dotFieldName;
      }
    }

    public static string GetFieldNameOfDotDereference(string dotFieldName)
    {
      if (dotFieldName.StartsWith("&"))
      {
        string normalizedName = dotFieldName.Substring(1);
        do
        {
          normalizedName = GetNormalisedFormForAddressOperator(normalizedName);
        } while (normalizedName.StartsWith("&"));

        return normalizedName;
      }
      return "*" + dotFieldName;
    }

    public static string GetFieldNameOfDot(string dotFieldName)
    {
      if (dotFieldName.StartsWith("&&"))
      {
        string normalizedName = dotFieldName;
        do
        {
          normalizedName = GetNormalisedFormForAddressOperator(normalizedName);
        } while (normalizedName.StartsWith("&&"));

        return normalizedName;
      }
      return dotFieldName;
    }


    List<FunctionInfo> GetFunctionInfoList(Partition p, ExecutionState execState)
    {
      List<FunctionInfo> result = new List<FunctionInfo>();
      foreach (Function f in GetDisplayFunctionsFor(p, execState))
      {
        //TODO: Change all functions, but set a visibility bit!
        FunctionInfo fi = new FunctionInfo();
        fi.ExecState = execState;
        fi.Function = f;
        result.Add(fi);
      }
      return result;
    }

    List<Function> GetDotFunctionsFor(Partition parent)
    {
      if (!model.FunctionMap.ContainsKey("$dot")) return null;
      return model.FunctionMap["$dot"].FindAll(delegate(Function fn)
      {
        return fn.FunctionArguments[0] == parent;
      });

    }

    private bool PartitionIsExecutionState(Partition part, out ExecutionState matchedState)
    {
      bool retval = false;
      matchedState = null;

      // First look directly at ExecutionStates
      foreach (ExecutionState execState in model.ExecutionStates)
      {
        if (execState.state == part)
        {
          matchedState = execState;
          return true;
        }
      }

      // Now match via $memory, $statusmap or $typemap
      string[] functionNames = new string[] { "$memory", "$statusmap", "$typemap" };
      foreach (string function in functionNames)
      {
        if (model.FunctionMap.ContainsKey(function))
        {
          foreach (Function func in model.FunctionMap[function])
          {
            if (func.Result == part)
            {
              Partition execStatePartition = func.FunctionArguments[0];
              // We know, that execStatePartition is a ExecutionState partition, i.e. this function will terminate 
              // in the first part of this function.
              return PartitionIsExecutionState(execStatePartition, out matchedState);
            }
          }
        }
      }

      return retval;
    }

    private bool FunctionContainsCurrentExecutionState(Function f, ExecutionState currentState)
    {
      bool retval = true;
      ExecutionState matchedState = null;

      foreach (Partition arg in f.FunctionArguments)
      {
        // If no execution state is contained in the FunctionArguments this function returns TRUE.
        // If currentState is contained in the FunctionArguments this function returns TRUE.
        // If currentState is contained in the FunctionArguments via $memory, $statusmap or $typemap this function returns TRUE.
        // If the FunctionArguments only refer to other ExecutionStates than currentState this function returns FALSE.

        if (PartitionIsExecutionState(arg, out matchedState))
        {
          // this is a partition matching a state
          if (matchedState == currentState) 
            return true;
          retval = false;
        }
      }
      return retval;
    }

    Regex preludeDefinedSk = new Regex(@"^([^.]*)\.sk\.[0-9]+.*");

    List<Function> GetDisplayFunctionsFor(Partition part, ExecutionState currentState)
    {
      List<Function> result = new List<Function>();
      foreach (string functionType in model.FunctionMap.Keys)
      {
        if (preludeDefinedSk.Match(functionType).Success)
          continue;
        foreach (Function f in model.FunctionMap[functionType])
        {
          if ((f.Result == part)|| (f.FunctionArguments.Contains(part)))
          {
            // Filter out all those functions, that have an execution state as their argument, which
            // is not equal to the current execution state.
            if (FunctionContainsCurrentExecutionState(f, currentState))
            {
              result.Add(f);
            }
          }
        }

      }
      return result;
    }

    Partition GetTypeOf(Partition p)
    {
      List<Function> list = model.FunctionMap["$typ"].FindAll(delegate(Function fn) { return fn.FunctionArguments[0] == p; });
      if (list.Count == 1)
        return list[0].Result;
      return null;
    }

    Partition GetRefOf(Partition p)
    {
      List<Function> list = model.FunctionMap["$ref"].FindAll(delegate(Function fn) { return fn.FunctionArguments[0] == p; });
      if (list.Count == 1)
        return list[0].Result;
      return null;
    }

    Partition GetOwnerOf(Partition p, ExecutionState execState)
    {
      return SelectResultFromStatusTag("$st_owner", p, execState);
    }

    Partition GetThreadOwnerOf(Partition p, ExecutionState execState)
    {
      return SelectResult("$thread_owner", execState.state, p);
    }

    Partition GetTimestampOf(Partition p, ExecutionState execState)
    {
      return SelectResultFromStatusTag("$st_timestamp", p, execState);
    }

    bool GetClosedStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResultFromStatusTag("$st_closed", p, execState));
    }

    bool GetThreadLocalStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResult("$thread_local", execState.state, p));
    }

    bool GetMutableStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResult("$mutable", execState.state, p));
    }

    bool GetTypedStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResultFromTypedStateTag("$ts_typed", p, execState));
    }

    bool GetIsArrayElementStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResultFromTypedStateTag("$ts_is_array_elt", p, execState));
    }

    bool GetIsVolatileStateOf(Partition p, ExecutionState execState)
    {
      return EvalBooleanPartition(SelectResultFromTypedStateTag("$ts_is_volatile", p, execState));
    }

    bool GetClaimableStateOf(Partition p)
    {
      return EvalBooleanPartition(GetPropertyOfTypeOf("$is_claimable", p));
    }

    bool GetPrimitiveStateOfTypeOf(Partition p)
    {
      return EvalBooleanPartition(GetPropertyOfTypeOf("$is_primitive", p));
    }

/*
    bool GetVolatileOwnsSetStateOf(Partition p)
    {
      return EvalBooleanPartition(GetPropertyOfTypeOf("$has_volatile_owns_set", p));
    }
*/

    Partition GetPropertyOfTypeOf(string function, Partition p)
    {
      Partition typePart = GetTypeOf(p);
      if (typePart != null)
      {
        return SelectResult(function, typePart);
      }
      return null;
    }

    Partition GetMemoryForState(Partition state)
    {
      // Return $memory for a state
      return GetAccessFunctionForState("$memory", state, PartitionType.Memory);
    }

/*
    Partition GetStatusMapForState(Partition state)
    {
      // Return $memory for a state
      return GetAccessFunctionForState("$statusmap", state, PartitionType.StatusMap);
    }
*/

/*
    Partition GetTypMapForState(Partition state)
    {
      // Return $memory for a state
      return GetAccessFunctionForState("$typemap", state, PartitionType.TypeMap);
    }
*/

    Partition GetAccessFunctionForState(string selector, Partition state, PartitionType ptype)
    {
      // Return $memory for a state
      if (model.FunctionMap.ContainsKey(selector))
      {
        foreach (Function fn in model.FunctionMap[selector].FindAll(delegate(Function function)
        {
          return ((function.FunctionArguments[0] == state) && (function.Result.Type == ptype));
        }))
        {
          return fn.Result;
        }
      }
      return null;
    }

    Partition SelectResultFromTypedStateTag(string functionType, Partition p, ExecutionState execState)
    {
      return SelectResultFromTag(functionType, p, execState, "$typemap", PartitionType.TypeMap);
    }

    Partition SelectResultFromStatusTag(string functionType, Partition p, ExecutionState execState)
    {
      return SelectResultFromTag(functionType, p, execState, "$statusmap", PartitionType.StatusMap);
    }

    Partition SelectResultFromTag(string functionType, Partition p, ExecutionState execState, string tag, PartitionType tagType)
    {
      string selectFunctionName;
      
      switch (tagType)
      {
        case PartitionType.Memory:
          selectFunctionName = "$select.mem";
          break;
        case PartitionType.StatusMap:
          selectFunctionName = "$select.sm";
          break;
        case PartitionType.TypeMap:
          selectFunctionName = "$select.tm";
          break;
        default:
          return null;
      }
      
      if (!model.FunctionMap.ContainsKey(functionType) ||
          !model.FunctionMap.ContainsKey(selectFunctionName) ||
          !model.FunctionMap.ContainsKey(tag))
        return null;

      foreach (Function func in model.FunctionMap[functionType])
      {
        foreach (Function selectFunc in model.FunctionMap[selectFunctionName].FindAll(delegate(Function fn) { return fn.Result == func.FunctionArguments[0]; }))
        {
          if ((selectFunc.FunctionArguments[0].Type == tagType) &&
              (selectFunc.FunctionArguments[1] == p) &&
              (GetStateForMap(tag, selectFunc.FunctionArguments[0]) == execState.state))
          {
            return func.Result;
          }
        }
      }
      return null;
    }

    Partition GetStateForMap(string mapstring, Partition mapentry)
    {
      if (model.FunctionMap.ContainsKey(mapstring))
      {
        foreach (Function map in model.FunctionMap[mapstring].FindAll(delegate(Function fn) { return fn.Result == mapentry; }))
        {
          return map.FunctionArguments[0];
        }
      }
      return null;
    }

    Partition SelectResult(string functionType, Partition arg1, Partition arg2)
    {
      if (!model.FunctionMap.ContainsKey(functionType))
        return null;

      List<Function> functions = model.FunctionMap[functionType].FindAll(
        delegate(Function f)
        {
          return ((f.FunctionArguments[0] == arg1) && (f.FunctionArguments[1] == arg2));
        });

      if (functions.Count == 1)
        return functions[0].Result;
      return null;
    }

    Partition SelectResult(string functionType, Partition arg1)
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

    Partition FindValueStructInState(Partition execState, Partition result) {
      string functionType = "$vs_ctor";
      if (model.FunctionMap.ContainsKey(functionType))
      {
        List<Function> functions = model.FunctionMap[functionType];
        if (functions == null)
        {
          return null;
        }
        functions = functions.FindAll(delegate(Function fn) { return (fn.Result == result) && (fn.FunctionArguments[0] == execState); });
        if (functions.Count == 1)
        {
          return functions[0].FunctionArguments[1];
        }
      }
      return null;
    }

    Partition SelectArgument(string functionType, Partition result, int nbArgument)
    {
      if (model.FunctionMap.ContainsKey(functionType))
      {
        List<Function> functions = model.FunctionMap[functionType];
        if (functions == null)
        {
          return null;
        }
        functions = functions.FindAll(delegate(Function fn) { return fn.Result == result; });
        if ((functions.Count == 1) && (functions[0].FunctionArguments.Count > nbArgument))
        {
          return functions[0].FunctionArguments[nbArgument];
        }
      }
      return null;
    }

    private static bool EvalBooleanPartition(Partition result)
    {
      if ((result != null) && (result.Value != null))
      {
        return result.Value.Equals("true");
      }
      return false;
    }
  }
}
