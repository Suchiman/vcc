//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  open Microsoft.Cci

  type Stopwatch(name:string) =
     let stopwatch = new System.Diagnostics.Stopwatch()
     let mutable startCount = 0
     let mutable running = false
     let mutable handicap = int64 0
     member this.Running = running
     member this.Start() = 
       if running then
         failwith "stopwatch already running"
       startCount <- startCount + 1
       running <- true
       stopwatch.Start()
     member this.Stop() =
       if not running then
         failwith "stopwatch already running"
       running <- false
       stopwatch.Stop()
     member this.Elapsed = handicap + stopwatch.ElapsedMilliseconds
     member this.Name = name
     member this.Display() =
       System.String.Format("{0,30} {1:0.000}", name, float this.Elapsed / 1000.0)
     member this.ShouldReplace (that:Stopwatch) =
       if this = that then true
       else
         if name = that.Name then
           if that.Running then
             failwith "replaced stopwatch is running"
           handicap <- handicap + that.Elapsed
           true
         else false
     member this.Run f a =
       try
         this.Start()
         f a
       finally this.Stop()
       
    
  module Helper =
  
    type Transformer =
      | Expr of ((CAST.Expr -> CAST.Expr) -> CAST.Expr -> option<CAST.Expr>)
      | ExprCtx of (CAST.ExprCtx -> (CAST.Expr -> CAST.Expr) -> CAST.Expr -> option<CAST.Expr>)
      | Decl of (list<CAST.Top> -> list<CAST.Top>)
      | DoNothing
    
    type TransDesc =
      {
        Name : string;
        Func : Transformer;
        mutable Enabled : bool;
      }
      
    type Env (hostEnv:ISourceEditHost, opts:VccOptions) =
      let stopwatches = ref []
      let sw name = 
        let s = new Stopwatch (name)
        stopwatches := s :: !stopwatches
        s
      let swTransformers = sw "AST transformers"
      let swTranslator = sw "BPL translation"
      let swPruning = sw "Pruning"
      
      let errorReported = ref false
      let oopsed = ref false
      let currentId = ref 0
      let transformers = new GList<_>()
      let topDecls = ref []
      let times = new Dict<_,_>()
      let dumpTime = ref false
      let errorHandler (args:Microsoft.Cci.ErrorEventArgs) =
        if !errorReported then ()
        else
          for msg in args.Errors do
             if not msg.IsWarning then errorReported := true
             
      do hostEnv.Errors.Add errorHandler
     
      member this.Stopwatches = !stopwatches
      
      member this.SwTransformers = swTransformers
      member this.SwTranslator = swTranslator
      member this.SwPruning = swPruning
      
      member this.Oops (tok:Token, msg:string) =
        if not !errorReported then
          oopsed := true
          hostEnv.ReportError (new TranslationMessage (VisitorHelper.LocationFromToken tok, 9600, "OOPS: " + msg, false))
      
      member this.Panic (msg:string) =
        this.Oops (Token.NoToken, msg)
        this.Die ()
        
      member this.Die () : 'a =
        failwith "confused, will now die"
      
      // 9100 <= code <= 9199
      // see (and update) comments on the top of transformers.fs for the next available number
      member this.Warning (tok:Token, code, msg:string) =
        if not (tok.SuppressWarning code) then
          hostEnv.ReportError (new TranslationMessage (VisitorHelper.LocationFromToken tok, code, msg, true))
          
      // 9300 <= code <= 9399
      member this.GraveWarning (tok, code, msg:string) =
        this.Warning (tok, code, "[possible unsoundness]: " + msg)
      
      // 9601 <= code <= 9699
      // see (and update) comments on the top of transformers.fs for the next available number
      member this.Error (tok:Token, code, msg:string) =
        this.Error (tok, code, msg, None)
        
      // 9601 <= code <= 9699
      // see (and update) comments on the top of transformers.fs for the next available number
      member this.Error (tok:Token, code, msg:string, relatedTok : Token option) =
        let errorMsg = 
          match relatedTok with
          | None -> new TranslationMessage (VisitorHelper.LocationFromToken tok, code, msg, false)
          | Some relatedTok -> new TranslationMessage (VisitorHelper.LocationFromToken tok, code, msg, false, (Seq.singleton (VisitorHelper.LocationFromToken relatedTok)))       
        hostEnv.ReportError (errorMsg)
        
      member this.Options =
        opts
      
      member this.ErrorReported = !errorReported
      
      member this.ShouldContinue = not !errorReported
      
      member this.ShouldDumpStack = not !errorReported || !oopsed
      
      member this.UniqueId () =
        incr currentId
        !currentId
      
      member this.TopDecls = !topDecls
      
      member this.AddTransformer (name : string, func : Transformer) =
        let td = { Name = name; Func = func; Enabled = true } : TransDesc
        transformers.Add td
      
      member this.ApplyTransformers (decls : list<CAST.Top>) =
        let apply = function
          | Expr f -> CAST.deepMapExpressions f
          | ExprCtx f -> CAST.deepMapExpressionsCtx f
          | Decl f -> f
          | DoNothing -> (fun x -> x)
        let rec aux i decls =
          if i >= transformers.Count then 
            decls
          else 
            let t = transformers.[i]
            if not t.Enabled then
              aux (i + 1) decls
            else 
              topDecls := decls
              let start = System.DateTime.Now
              let decls' = apply t.Func decls
              let tm = System.DateTime.Now - start            
              times.[t.Name] <- tm.Ticks
              aux (i + 1) decls'
        try
          swTransformers.Start()
          try
            let res = aux 0 decls
            if !dumpTime then this.DumpTransformers()
            res
          with
            e -> 
              if this.ShouldContinue then rethrow ()
              else []
        finally
          swTransformers.Stop()

      // the extension interface
      member this.AddTransformerAt (name : string, func : Transformer, after : string, off : int) =
        let td = { Name = name; Func = func; Enabled = true } : TransDesc
        let rec add i =
          if i >= transformers.Count then
            failwith ("transformer not found " + after)
          else if transformers.[i].Name = after then
            transformers.Insert (i + off, td)
          else add (i + 1)
        add 0
        
      member this.AddTransformerAfter (name : string, func : Transformer, after : string) =
        this.AddTransformerAt (name, func, after, 1)
      
      member this.AddTransformerBefore (name : string, func : Transformer, after : string) =
        this.AddTransformerAt (name, func, after, 0)
      
      member this.DisableTransformers (prefix : string) =
        for t in transformers do
          if t.Name.StartsWith prefix then t.Enabled <- false
          
      member this.RemoveTransformer (name : string) =
        let rec aux i =
          if i >= transformers.Count then
            failwith "no such transformer"
          else if transformers.[i].Name = name then
            let t = transformers.[i]
            transformers.RemoveAt i
            t
          else aux (i + 1)
        aux 0
      
      member this.ShouldDumpTime () =
        dumpTime := true
          
      member this.DumpTransformers () =
        for t in transformers do
          let kind =
            match t.Func with
              | Expr _ -> "expr"
              | ExprCtx _ -> "Expr"
              | Decl _ -> "decl"
              | DoNothing -> "mark"
          System.Console.WriteLine ("{3:000.000}s {0} {1} {2}", kind, (if t.Enabled then "          " else "(disabled)"), t.Name, 
                                    double (lookupWithDefault times 0L t.Name) / 10000000.0)
                                    