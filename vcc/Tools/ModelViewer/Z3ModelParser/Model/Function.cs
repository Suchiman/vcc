using System.Collections.Generic;

namespace Z3Model {
  public class Function { 
    private List<Partition> _functionArguments;
    private Partition _result;
    private string _functionType;

    public Function(string FunctionType) {
      _functionType = FunctionType;
      _functionArguments = new List<Partition>();
    }

    public List<Partition> FunctionArguments
    {
      get {
        return _functionArguments;
      }
      set
      {
        _functionArguments = value;
      }
    }

    public Partition Result {
      get {
        return _result;
      }
    }

    public string FunctionType
    {
      get
      {
        return _functionType;
      }
    }


    public void SetResult(Partition result)
    {
      _result = result;
    }

    public void AddArgument(Partition FunctionArgument) {
      FunctionArguments.Add(FunctionArgument);
    }

    
    /*
    public override string ToString()
    {
      string arguments = String.Join(", ", this.FunctionArguments.ConvertAll(delegate(Partition p) { return p.Value; }).ToArray());
      return String.Format("{0}({1}) = {2}", this.FunctionType, arguments, this.Result.Value);
    }*/
  }

}
