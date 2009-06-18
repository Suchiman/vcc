using System;
using System.Collections.Generic;
using Vcc2Model.Controller;
using Z3Model;

namespace Vcc2Model
{
  public class ModelInfo
  {
    public ExecutionState ExecState;

    public override string ToString()
    {
      string retval = "";
      if (ExecState != null)
        retval = String.Format("{0} in {1}", GetType().Name, ExecState.state.DisplayValue);
      return retval;
    }
  }

  public class EntryModelInfo : ModelInfo
  {
    public Dictionary<Partition, FieldInfo> fieldNameDatabase;
    public Function PtrFunction;

    protected string GetDisplayPartitionName(Partition p)
    {
      FieldInfo fi = null;
      if (fieldNameDatabase.ContainsKey(p))
      {
        fi = fieldNameDatabase[p];
      }
      if ((fi != null) && (fi.FieldName != null))
      {
        return fi.FieldName;
      }
      if (p.Value != null)
      {
        return p.Value;
      }
      if (PtrFunction != null)
      {
        string type = PtrFunction.FunctionArguments[0].Value;
        string refPointer = PtrFunction.FunctionArguments[1].Value;
        return String.Format("$ptr({0},{1})", type, refPointer);
      }
      return p.DisplayValue;
    }

  }

  public class PtrSetEntry : EntryModelInfo
  {
    public Partition Field;
    public Partition TypePartition;
    public bool isInSet;
    public string strongestSetOperation;

    public string FieldName
    {
      get
      {
        return GetDisplayPartitionName(Field);
      }
    }
  }

  public class MapEntry : EntryModelInfo
  {
    public Partition Key;
    public Partition Value;
    public DotInfo MapDot;
    public FieldInfo KeyField;
    public FieldInfo ValueField;
    public string MapName;

    public MapFieldInfo MapContainer;

    public string KeyName
    {
      get
      {
        return GetDisplayPartitionName(Key);
      }
    }

    public string ValueName
    {
      get
      {
        return GetDisplayPartitionName(Value);
      }
    }

  }
  public class FieldInfo : ModelInfo
  {
    private string fieldName;
    public string FieldName
    {
      get { return fieldName; }
      set
      {
        AddAlias(fieldName);
        fieldName = value;
      }
    }
    public List<string> Aliases = new List<string>();
    public string AliasesString
    {
      get
      {
        string aliases = "";
        if (Aliases.Count > 1)
        {
          foreach (string alias in Aliases)
          {
            if (aliases.Length > 0)
            {
              aliases += ", ";
            }
            aliases += alias;
          }

          if (aliases.Length > 0)
          {
            return aliases;
          }
        }
        return aliases; 
      }
    }

    public void AddAlias(string alias)
    {
      if ((alias!=null) && (!Aliases.Contains(alias)))
      {
        Aliases.Add(alias);
      }
    }

    public Partition FieldPartition;
    public Partition FieldTypePartition;
    public Partition HeapAddress;
    public Partition Owner;
    public Partition ThreadOwner;
    public Partition TimeStamp;

    public DotInfo Dot;

    public bool Closed;
    public bool ThreadLocal;
    public bool Mutable;
    public bool Claimable;
    public bool Primitive;
    public bool Typed;
    public bool Volatile;
    public bool ArrayElement;
    public bool Ghost;

    public string FieldType
    {
      get
      {
        if (FieldTypePartition == null)
          return "undefined";
        return FieldTypePartition.DisplayValue;
      }
    }
  }

  public class PrimitiveFieldInfo : FieldInfo
  {
    public string FieldValue;

    public override string ToString()
    {
      return String.Format("{0}\n\tValue: {1}", base.ToString(), FieldValue);
    }
  }

  public class PtrSetFieldInfo : FieldInfo
  {
    public List<PtrSetEntry> Entries;
    public bool OwnsSetField;

    public override string ToString()
    {
      String result = base.ToString();
      foreach (PtrSetEntry e in Entries)
      {
        result += String.Format("\n\t* {0}", e.FieldName);
      }
      return result;
    }
  }

  public class MapFieldInfo : FieldInfo
  {
    public List<MapEntry> Entries;
  }

  public class ObjectInfo : DotContainerInfo
  {


    public override string ToString()
    {
      String result = base.ToString();
      foreach (DotInfo di in Dots)
      {
        result += String.Format("\n\tdot: {0}", di);
      }
      foreach (FunctionInfo f in Functions)
      {
        result += String.Format("\n\t##{0}", f);
      }
      return result;
    }
  }


  public class ArrayInterpretationInfo : DotContainerInfo
  {
    public Partition ArraySize;
  }

  public class ArrayInfo : FieldInfo
  {
    public Dictionary<int, FieldInfo> ElementDots = new Dictionary<int, FieldInfo>();
    public List<FieldInfo> ArrayInterpretations = new List<FieldInfo>();
    public FieldInfo NotInterpretedPointer;
  }

  public class DotContainerInfo : FieldInfo
  {
    public List<DotInfo> Dots = new List<DotInfo>();
    public List<FunctionInfo> Functions;

    public Partition DerefValue;
  }

  public class DotInfo : DotContainerInfo
  {
    public FieldInfo Field;

    public Partition DotPartition
    {
      get
      {
        return FieldPartition;
      }
    }

    public override string ToString()
    {
      String result = DotPartition.Value;
      foreach (FunctionInfo f in Functions)
      {
        result += String.Format("\n\t{0}", f);
      }
      if (Field != null)
        result += Field.ToString();
      return result;
    }
  }

  public class FunctionInfo : ModelInfo
  {
    public Function Function;

    public List<FieldInfo> Arguments;
  }
}
