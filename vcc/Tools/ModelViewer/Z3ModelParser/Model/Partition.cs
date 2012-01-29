//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace Z3Model {

  public enum PartitionType
  {
    Unspecified,
    Type,
    State,
    Memory,
    StatusMap,
    TypeMap,
    //Meta,
    LineNumberToken,
    Pointer,
    Value,
    PtrSet,
    Dot,
    MapRef,
    Map,
  }

  public class Partition {
    private int _id;
    private Set _set;
    private PartitionType _type;
    private string _value;
    private List<string> _aliases = new List<string>();
  
    public Partition(int Id) {
      _id = Id;
      _type = PartitionType.Unspecified;
    }
    
    /// <summary>
    /// Model Id *xxx
    /// </summary>
    public int Id {
      get {
        return _id;
      }
    }

    public PartitionType Type
    {
      get {
        return _type;
      }
      set
      {
        _type = value;
        //_type = Type;
      }
    }

    public Set Set {
      get {
        return _set;
      }
      set {
        _set = value;
      }
    }

    public string Value {
      get {
        return _value;
      }
    }

    public List<string> Aliases 
    {
      get
      {
        return _aliases;
      }
    }

    public string DisplayValue
    {
      get
      {
        if ((_value != null) && !_value.Equals(""))
        {
          switch (_value)
          {
            case "^^u1":
              return "uint8_t";
            case "^^u2":
              return "uint16_t";
            case "^^u4":
              return "uint32_t";
            case "^^u8":
              return "uint64_t";

            case "^^i1":
              return "int8_t";
            case "^^i2":
              return "int16_t";
            case "^^i4":
              return "int32_t";
            case "^^i8":
              return "int64_t";

            case "^^bool":
              return "bool_t";
            case "^^void":
              return "void";
            case "^^claim":
              return "claim";

            case "$#ptrset":
              return "ptrset_t";
            case "$#thread_id":
              return "threadid_t";
            case "$#state_t":
              return "state_t";

            default:
              return _value;
          }
        }
        return String.Format("partition_{0}", _id);
      }
    }
    
    public void SetType(PartitionType partitionType)
    {
      _type = partitionType;
    }

    public void SetValue(string ValueString) {
      _value = ValueString;
    }

    public void AddAlias(string alias)
    {
      if (!_aliases.Contains(alias))
      {
        _aliases.Add(alias);
      }
    }

    public void AddSetElement(string ElementName) {
      if (_set == null)
        _set = new Set();

      _set.AddElement(ElementName);
    }

  }
}
