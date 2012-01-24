using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Collections;

namespace Microsoft.Research.Vcc.Cpp
{
  public class CppDriver
  {

    private readonly TransEnv env = new TransEnv();

    public void Init()
    {
      Transformers.init(env);
      Transformers.processPipeOptions(env);
    }

    public FSharpList<CAST.Top> ApplyTransformers(FSharpList<CAST.Top> decls)
    {
      return env.ApplyTransformers(decls);
    }
  }
}
