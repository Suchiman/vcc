//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc

  module DumpCodenInfo =
    open Microsoft.Research.Vcc
    open Microsoft.FSharp.Math
    open CAST

    [<System.ComponentModel.Composition.Export("Microsoft.Research.Vcc.Plugin")>]    
    type ContractGeneratorPlugin() =
      inherit Microsoft.Research.Vcc.Plugin()
      
      let dbg() = System.Diagnostics.Debugger.Break()
      let wr (s:string) = System.Console.Write(s)
      
      let pluginOptions = ref []
      let verifiedCOptions = ref null

      let die() = failwith "confused, will now die"
      
      let hasBoolAttr n = List.exists (function VccAttr (n', "true") -> n = n' | _ -> false)

      let commas (separator:string) p elems =
        let rec loop first = function
          | [] -> ()
          | e::es -> 
            if not first then wr separator
            p e
            loop false es
        loop true elems

      override this.IsModular() = false
      override this.Help() = "Don't panic!"
      override this.Name() = "codeinfo"
      override this.UseCommandLineOptions options = pluginOptions := [ for o in options -> o ]
      override this.UseVccOptions options = verifiedCOptions := options

      override this.Verify(filename, env, decls) = 
        let functionDependencies (fn:Function) =
          let pureDeps = new Dict<_,_>()
          let physDeps = new Dict<_,_>()
          
          let visit ctx self = function
            | CAST.Call(_, fn, _, _) -> 
              if ctx.IsPure then pureDeps.[fn] <- true else physDeps.[fn] <- true
              true
            | _ -> true
            
          match fn.Body with
            | None -> ()
            | Some e -> e.SelfCtxVisit(false, visit)
          
          List.iter (fun (e:Expr) -> e.SelfCtxVisit(true, visit)) (fn.Reads @ fn.Writes @ fn.Requires @ fn.Ensures)

          ( [ for k in pureDeps.Keys -> k ], [ for k in physDeps.Keys -> k] )
          
        let doFunction (fn:Function)  =
          let (pureDeps, physDeps) = functionDependencies fn
          let wrFn (fn : Function) = 
            wr (fn.Name + "(")
            commas ", " (fun t -> wr (t.ToString())) fn.Parameters 
            wr (") : " + fn.RetType.ToString())
          let wrAttr = 
            let wr' s = wr ("\t\t" + s + "\n")
            let wrVcc s (o:obj) = wr' ("vcc(" + s + "," + o.ToString() + ")")
            function 
              | SkipVerification -> wr' "skipverification"
              | IsAdmissibilityCheck -> wr' "isadmissibilitycheck"
              | NoAdmissibility -> wr' "no_admissibility"
              | IntBoogieAttr(s,n) -> wrVcc s n
              | BoolBoogieAttr(s,b) -> wrVcc s b
              | VccAttr(s,s1) -> wrVcc s s1
              | GroupDeclAttr(g) -> wr' ("def_group(" + g + ")")
              | InGroupDeclAttr(g) ->  wr' ("in_group(" + g + ")")
              | _ -> ()

          let wrDeps = function
            | [] -> wr "\t\t<none>\n"
            | fns -> for fn in fns do wr "\t\t"; wrFn fn; wr "\n"
            
          let wrAttrs = function
            | [] -> wr "\t\t<none>\n"
            | attrs -> List.iter wrAttr attrs

          let wrFlags (fn:Function) =
            if fn.IsPure || fn.IsSpec then
              if fn.IsPure then wr "\t\tispure\n"
              if fn.IsSpec then wr "\t\tspec\n"
            else
              wr "\t\t<none>\n"

          wr "function: "; wrFn fn; wr "\n"
          wr "\tflags:\n"
          wrFlags fn
          wr "\tpure dependencies:\n"
          wrDeps pureDeps
          wr "\tphysical dependencies:\n"
          wrDeps physDeps
          wr "\tattributes:\n"
          wrAttrs fn.CustomAttr
          wr "\n"
          
        for d in decls do
          match d with
            | Top.FunctionDecl(fn) -> doFunction fn
            | _ -> ()