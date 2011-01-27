//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.FSharp.Math
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.TranslatorUtils
  open Microsoft.Research.Vcc.Util
  open System
  
  module C = Microsoft.Research.Vcc.CAST
  module B = Microsoft.Research.Vcc.BoogieAST
  module IF = Microsoft.Research.Vcc.InformationFlow
  
  type MyFunc<'a> = delegate of unit -> 'a

  module Translator =
    type ClaimContext =
      {
        mutable ClaimChecks : list<B.Stmt>;
        mutable ClaimInits : list<B.Stmt>;
      }
      
    let globalHasIF = ref false

    type Env =
      {
        OldState : B.Expr
        WritesState : B.Expr
        Writes : list<C.Expr>
        WritesTime : B.Expr
        AtomicObjects : list<B.Expr>
        AtomicReads : list<B.Expr>
        ClaimContext : option<ClaimContext>
        InverseTranslation : Dict<B.Expr, list<C.Expr>>

        hasIF : bool
        IFPCNum : ref<int>
        IFBlockNum : ref<int>
        IFContexts : list<list<C.LabelId>*string>
        IFGrpID : ref<bigint>
      }

    let nestingExtents = false
    
    let initialEnv = { OldState = bOld bState
                       Writes = []
                       AtomicObjects = []
                       AtomicReads = []
                       WritesState = bOld bState
                       WritesTime = er "$bogus"
                       ClaimContext = None
                       InverseTranslation = null

                       hasIF = false
                       IFPCNum = ref 0
                       IFBlockNum = ref 0
                       IFContexts = []
                       IFGrpID = ref (bigint.Zero)
                     }
    
    let fieldName (f:C.Field) = f.Parent.Name + "." + f.Name


    let currentPC (env:Env) =
      match env.IFContexts with
        | [] -> die()
        | (_,pc) :: _ -> pc

    let parentPC (env:Env) =
      match env.IFContexts with
        | []
        | [_] -> die()
        | _::(_,pc)::_ -> pc

    let getLocalLabels (expr:C.Expr) =
      let labels = ref []
      let rec visitor self = function
        | C.Expr.Label(_,lbl) -> labels := lbl::!labels; false
        | C.Expr.If _
        | C.Expr.Loop _ -> false
        | _ -> true
      expr.SelfVisit(visitor)
      !labels

    let newIFContext (env:Env) expr =
      let jmpContext =
        match expr with
          | C.Expr.If(_,_,_,t,e) -> getLocalLabels t @ getLocalLabels e
          | C.Expr.Loop(_,_,_,_,e) -> getLocalLabels e
          | _ -> die()
      let newPCNum = incr env.IFPCNum; !(env.IFPCNum)
      let newPC = "FlowData#PC#"+(newPCNum.ToString())
      {env with IFContexts = (jmpContext,newPC)::env.IFContexts}

    let freshGrpID (env:Env) = env.IFGrpID := (!env.IFGrpID) + bigint.One; !env.IFGrpID

    let hasCustomAttr n = List.exists (function C.VccAttr (n', _) -> n = n' | _ -> false)
    
    let rec noUnions = function
      | C.Type.Ref td ->
        match td.Kind with           
          | C.TypeKind.Struct -> 
            // empty field list denote a forward declaration, in which case we do not know
            td.Fields <> [] && List.forall (fun (f:C.Field) -> noUnions f.Type) td.Fields
          | C.TypeKind.Union -> false
          | _ -> true
      | C.Array (t, _) -> noUnions t
      | _ -> true
    
    let mutable stateId = 0
    let saveState pref =      
      let oldState = pref + "State#" + stateId.ToString()
      stateId <- stateId + 1
      ([B.Stmt.VarDecl ((oldState, tpState), None);
        B.Stmt.Assign (er oldState, bState)], er oldState)

    let translate functionToVerify (helper:Helper.Env) (getPrelude:MyFunc<Microsoft.Boogie.Program>) decls =
      let ctx = TranslationState(helper)
      let helper = ctx.Helper
      let bv = BvTranslator(ctx)
      let preludeBodies = lazy (ToBoogieAST.getFunctionExpansions (getPrelude.Invoke()))
      
      let toTypeId = ctx.ToTypeId
      let castFromInt = ctx.CastFromInt
      let castToInt = ctx.CastToInt
      let trType = ctx.TrType
      let weight = ctx.Weight
      let vcc3 = helper.Options.Vcc3
      
      let captureStateAttrs suff (tok:Token) =
        if helper.Options.PrintCEVModel && suff <> "<skip>" then             
          let suff = if suff = "" then "" else " : " + suff
          [B.StringAttr ("captureState", String.Format ("{0}({1},{2}){3}", tok.Filename, tok.Line, tok.Column, suff))]
        else []

      let assumeSyncCS suff (env:Env) tok =
        let name = ctx.GetTokenConst tok
        let pred =
          match env.AtomicObjects with
            | [] -> "$full_stop_ext"
            | _ -> "$good_state_ext"
        let attrs = captureStateAttrs suff tok
        B.Stmt.Assume (attrs, bCall pred [er name; bState])

      let assumeSync = assumeSyncCS "<skip>"

      let captureState suff tok =
        B.Stmt.Assume (captureStateAttrs suff tok, bTrue)

      let addType t e =
        if vcc3 then
          match t with
            | C.Type.SpecPtr t -> bCall "$spec_ptr_cast" [e; toTypeId t]
            | C.Type.PhysPtr t -> bCall "$phys_ptr_cast" [e; toTypeId t]
            | _ -> e
        else
          match t with
            | C.Type.SpecPtr t
            | C.Type.PhysPtr t -> bCall "$ptr" [toTypeId t; e]
            | _ -> e
      
      let mapEqAxioms t =
        let t1, t2 =
          match t with
            | C.Type.Map (t1, t2) -> t1, t2
            | C.Type.Map (C.Type.ObjectT, C.Type.Bool)
            | _ -> die()
        let bt1 = trType t1
        let bt2 = trType t2
        let mapName = ctx.TypeIdToName (toTypeId t)
        let mt = B.Type.Ref mapName      
      
        let tp = B.Type.Ref mapName
        let mapTypeName = (mapName.Replace ("$#", "")).Replace ("$", "")
        let mapType = B.Type.Ref (mapName)
        let sel = "$select." + mapName
        let selMP = bCall sel [er "M"; er "p"]
        let stor  = "$store." + mapName
        let zero = "$zero." + mapName
        let eq = "$eq." + mapName
        let v = er "v"
        let v, inRange =
          let rangeAxiom rangeFn args = [B.Decl.Axiom (B.Expr.Forall (Token.NoToken, ["M", tp; "p", bt1], [], weight "select-map-eq", bCall rangeFn args))]
          match t2 with
            | C.Type.Integer _ -> bCall "$unchecked" [toTypeId t2; v], rangeAxiom "$in_range_t" [toTypeId t2; selMP]
            | C.Type.Claim
            | C.Type.SpecPtr _ -> v, rangeAxiom "$in_range_spec_ptr" [selMP]
            | C.Type.PhysPtr _ -> v, rangeAxiom "$in_range_phys_ptr" [selMP]
            | _ -> v, []
        let argRange = 
          match t1 with
            | C.Type.Integer _ -> 
              bCall "$in_range_t" [toTypeId t1; er "p"]
            | _ -> bTrue
        let selStorPP = 
          bImpl argRange (bEq (bCall sel [bCall stor [er "M"; er "p"; er "v"]; er "p"]) v)
        let selStorPQ =
          bInvImpl (bNeq (er "p") (er "q"))
                    (bEq (bCall sel [bCall stor [er "M"; er "q"; er "v"]; er "p"]) selMP)
        let selZero =
          let zeroVal = 
            match t2 with
              | C.Ptr _ ->
                if vcc3 then addType t2 (er "$null")
                else bInt 0              
              | C.Type.Claim
              | C.Type.MathInteger
              | C.Type.Integer _ -> bInt 0
              | C.Type.ObjectT _ -> er "$null"
              | C.Type.Ref({Kind = C.TypeKind.Record}) -> er "$rec_zero"
              | C.Type.Ref({Name = n; Kind = C.TypeKind.MathType}) -> 
                match n with 
                  | "ptrset" -> bCall "$set_empty" []
                  | "state_t" -> er "$state_zero"
                  | _ -> er "$struct_zero"
              | C.Type.Bool -> bFalse
              | C.Type.Map _ -> er ("$zero." + ctx.TypeIdToName(toTypeId t2))
              | _ -> die()
            
          bEq (bCall sel [er zero; er "p"]) zeroVal
        let t2Eq =
          match t2 with
            | C.Type.Map _ -> fun b1 b2 -> bCall ("$eq." + (ctx.TypeIdToName (toTypeId t2))) [b1; b2]
            | _ -> bEq
        let eqM1M2 = bCall eq [er "M1"; er "M2"]
        let eqM1M2Ax1 = bImpl (B.Expr.Forall(Token.NoToken, ["p", bt1], [], weight "select-map-eq", bImpl argRange (t2Eq (bCall sel [er "M1"; er "p"]) (bCall sel [er "M2"; er "p"]))))
                             eqM1M2
        let eqM1M2Ax2 = bImpl eqM1M2 (bEq (er "M1") (er "M2"))
        
        let eqRecAx = 
          let m1 = bCall "$rec_fetch" [er "r1"; er "f"]
          let m2 = bCall "$rec_fetch" [er "r2"; er "f"]
          let recEq = bCall "$rec_base_eq" [m1; m2]
          let mapEq = bCall eq [castFromInt mt m1; castFromInt mt m2]                    
          let vars = ["r1", tpRecord; "r2", tpRecord; "f", tpField; "R", tpCtype]
          let isRecFld = bCall "$is_record_field" [er "R"; er "f"; toTypeId (C.Type.Map (t1, t2))]
          B.Expr.Forall (Token.NoToken, vars, [[recEq; isRecFld]], weight "select-map-eq", bImpl mapEq recEq)
        
        let mpv = ["M", tp; "p", bt1; "v", bt2]
        let m1m2 = ["M1", tp; "M2", tp]
        [B.Decl.TypeDef mapName;
         B.Decl.Function (bt2, [], sel, ["M", tp; "p", bt1], None);
         B.Decl.Function (tp, [], stor, mpv, None);
         B.Decl.Function (B.Type.Bool, [], eq, m1m2, None);
         B.Decl.Const({Unique = false; Name = zero; Type = mapType});
         B.Decl.Axiom (B.Expr.Forall (Token.NoToken, mpv, [], weight "select-map-eq", selStorPP));
         B.Decl.Axiom (B.Expr.Forall (Token.NoToken, mpv @ ["q", bt1], [], weight "select-map-neq", selStorPQ));
         B.Decl.Axiom (B.Expr.Forall (Token.NoToken, m1m2, [[eqM1M2]], weight "select-map-eq", eqM1M2Ax1));
         B.Decl.Axiom (B.Expr.Forall (Token.NoToken, m1m2, [[eqM1M2]], weight "select-map-eq", eqM1M2Ax2));
         B.Decl.Axiom (bEq (castFromInt mt (bInt 0)) (er zero));
         B.Decl.Axiom (B.Expr.Forall (Token.NoToken, ["p", bt1], [], weight "select-map-eq", selZero));
         B.Decl.Axiom eqRecAx
        ] @ inRange
      
      let tryDecomposeDot = function
        | B.Expr.FunctionCall ("$dot", [p; f]) -> Some [p; f]
        | _ -> None

      let decomposeDot e = 
        match tryDecomposeDot e with
          | Some r -> r
          | None -> [bCall "$emb0" [e]; bCall "$field" [e]]

      let typedRead s p t =
        if vcc3 then
          let decomp p = List.rev (decomposeDot p)
          match t with
            | C.SpecPtr t ->
              bCall "$rd_spec_ptr" (s :: decomp p @ [toTypeId t])
            | C.PhysPtr t ->
              bCall "$rd_phys_ptr" (s :: decomp p @ [toTypeId t])
            | _ ->
              castFromInt (trType t) (bCall "$rd_inv" (s :: decomp p))
        else
          match t with
            | C.Ptr t ->
              bCall "$read_ptr" [s; p; toTypeId t]
            | C.Bool ->
              bCall "$read_bool" [s; p]
            | t ->                
              castFromInt (trType t) (bCall "$mem" [s; p])

      let varRef v = er (ctx.VarName v)

      let trVar (v:C.Variable) : B.Var =
        (ctx.VarName v, trType (v.Type))

      let typeVarName (tv : C.TypeVariable) = "^^TV#" + tv.Name
      let typeVarRef (tv : C.TypeVariable) = er (typeVarName tv)
      let trTypeVar (tv : C.TypeVariable) : B.Var = (typeVarName tv, trType C.Type.TypeIdT)
                  
        
      let trWhereVar (v:C.Variable) =
        let v' = trVar v
        match v.Type with
          | C.Type.Integer k -> (v', Some (bCall ("$in_range_" + C.Type.IntSuffix k) [varRef v]))
          | C.Type.PhysPtr t when vcc3 -> (v', None) // (v', Some (bCall "$is_phys_ptr" [varRef v; toTypeId t]))
          | C.Type.SpecPtr t when vcc3 -> (v', None) // (v', Some (bCall "$is_spac_ptr" [varRef v; toTypeId t]))
          | C.Type.PhysPtr _ -> (v', Some (bCall "$in_range_phys_ptr" [varRef v]))
          | C.Type.SpecPtr _ -> (v', Some (bCall "$in_range_spec_ptr" [varRef v]))
          | _ -> (v', None)
    
      let ptrType (expr:C.Expr) =
        match expr.Type with
          | C.Ptr C.Void -> failwith ("void* not supported " + expr.ToString())
          | C.Ptr t -> toTypeId t
          | _ -> failwith ("pointer type expected " + expr.ToString())        
      
      let stripType t e =
        match t with
          | C.Type.SpecPtr t
          | C.Type.PhysPtr t when not vcc3 ->
            bCall "$ref" [e]
          | _ -> e
        
      let convertArgs (fn:C.Function) args =
        let rec loop = function
          | ((f: C.Variable) :: ff, a :: aa) ->
            stripType f.Type a :: loop (ff, aa)
          | ([], a) -> a // varargs functions
          | _ -> helper.Die()
        loop (fn.InParameters, args)

      let stripFreeFromEnsures = function
        | C.Macro(_, "free_ensures", [e]) -> e
        | e -> e

      let isSetEmpty = function
        | C.Macro (_, "_vcc_set_empty", []) -> true
        | _ -> false
        
      let warnForIneffectiveOld token expr =
        if not (bContains "$s" expr) then
          helper.Warning (token, 9106, "'old', 'in_state', or 'when_claimed' in '" + token.Value + "' has no effect")

      let claimStateId = ref 0


      let rec trExpr (env:Env) expr =
        let self = trExpr env
        let selfs = List.map self
        let isFloatingPoint = function | C.Type.Primitive _ -> true | _ -> false
        try
          let res =
            match expr with
              | C.Expr.Cast ({ Type = C.Type.Integer k }, _, e') ->
                match e'.Type with
                  | C.Type.Bool ->
                    bCall "$bool_to_int" [self e']
                  | C.Type.Integer _ -> self e'
                  | C.Type.MathInteger -> self e'
                  | C.Type.ObjectT
                  | C.Ptr _ -> // TODO insert checks for casts here
                    bCall ("$ptr_to_" + C.Type.IntSuffix k) [self e']
                  | _ -> die()
              | C.Expr.Cast ({ Type = C.Type.Bool }, _, e') ->
                match e'.Type with
                  | C.Type.Integer _ ->
                    match e' with
                      | C.IntLiteral (_, ZeroBigInt) -> bFalse
                      | C.IntLiteral (_, OneBigInt) -> bTrue
                      | _ -> bCall "$int_to_bool" [self e']
                  | C.ObjectT
                  | C.Ptr _ ->
                    bCall "$ptr_neq" [self e'; er "$null"]
                  | C.Type.MathInteger _ ->
                    bCall "$int_to_bool" [self e']
                  | C.Type.SecLabel _ ->
                    self e'
                  | _ -> die()
              | C.Cast ({ Type = C.Type.MathInteger }, _, e') when e'.Type._IsPtr ->
                if vcc3 then                
                  bCall "$addr" [self e']
                else
                  bCall "$ref" [self e']
              | C.Expr.Cast (_, _, e') when expr.Type._IsPtr && e'.Type._IsPtr ->
                if vcc3 then
                  match expr.Type with
                    | C.SpecPtr _ -> bCall "$spec_ptr_cast" [self e'; ptrType expr]
                    | C.PhysPtr _ -> bCall "$phys_ptr_cast" [self e'; ptrType expr]
                    | _ -> die()
                else
                  bCall "$ptr_cast" [self e'; ptrType expr]
              | C.Expr.Cast ({ Type = C.Type.ObjectT }, _, C.Expr.IntLiteral(_, z)) when z = bigint.Zero -> er "$null"
              | C.Expr.Pure (_, e') -> self e'
              | C.Expr.Macro (c1, name, [C.Expr.Prim (c2, C.Op(_, C.Unchecked), _) as inner]) 
                  when name.StartsWith "unchecked" && c1.Type = c2.Type -> trExpr env inner
              | C.Expr.Prim (c, C.Op(opName, _), args) when isFloatingPoint c.Type ->
                let suffix = match c.Type with | C.Type.Primitive k -> C.Type.PrimSuffix k | _ -> die()
                let opName' = if args.Length = 1 then "u" + opName else opName
                let funcNameTbl = Map.ofList [ "+", "$add"; "-", "$sub"; "*", "$mul"; "/", "$div"; "u-", "$neg";
                                             "<", "$lt"; "<=", "$leq"; ">", "$gt"; ">=", "$geq" ]
                match funcNameTbl.TryFind opName' with
                  | Some(fName) -> bCall (fName + "_" + suffix)(selfs args)
                  | None -> 
                    helper.Error(expr.Token, 9701, "Operator '" + opName + "' not supported for floating point values")
                    bTrue
              | C.Expr.Prim (c, C.Op(opName, ch), args) ->
                let args = selfs args
                let targs = toTypeId c.Type :: args
                match opName with
                  | "&" -> bCall "$_and" targs
                  | "|" -> bCall "$_or" targs
                  | ">>" -> bCall "$_shr" args
                  | "<<" -> bCall "$_shl" targs
                  | "~" -> bCall "$_not" targs
                  | "^" -> bCall "$_xor" targs 
                  | "*" when ch <> C.Unchecked -> bCall "$op_mul" args
                  | _ -> 
                    if ch = C.Unchecked then
                      match opName with
                        | "+" -> bCall "$unchk_add" targs
                        | "-" -> bCall "$unchk_sub" targs
                        | "*" -> bCall "$unchk_mul" targs
                        | "/" -> bCall "$unchk_div" targs
                        | "%" -> bCall "$unchk_mod" targs
                        | _ -> B.Expr.Primitive (opName, args)
                    else if helper.Options.OpsAsFunctions then
                      match opName with
                        | "+" -> bCall "$op_add" targs
                        | "-" -> bCall "$op_sub" targs
                        // * is always translated like this
                        | "/" -> bCall "$op_div" targs
                        | "<" -> bCall "$op_lt" targs
                        | "<=" -> bCall "$op_le" targs
                        | ">" -> bCall "$op_gt" targs
                        | ">=" -> bCall "$op_ge" targs
                        | _ -> B.Expr.Primitive (opName, args)
                    else B.Expr.Primitive (opName, args)
              | C.Expr.Ref (_, v) -> 
                addType v.Type (varRef v)
              | C.Expr.IntLiteral (_, v) ->
                B.Expr.IntLiteral v
              | C.Expr.BoolLiteral (_, v) ->
                B.Expr.BoolLiteral v
              | C.Macro(ec, n, args) -> trMacro env ec n args    
              | C.Expr.Dot (c, o, f) ->
                if f.Parent.Kind = C.Record then
                  helper.Oops (c.Token, "record dot found " + expr.ToString())
                bCall "$dot" [self o; er (fieldName f)]
              | C.Expr.Index (_, arr, idx) ->
                if vcc3 then
                  bCall "$idx" [self arr; self idx]
                else
                  bCall "$idx" [self arr; self idx; ptrType arr]
              | C.Expr.Deref (_, p) -> typedRead bState (self p) expr.Type
              | C.Expr.Call (_, fn, targs, args) ->
                let args =  List.map ctx.ToTypeIdArraysAsPtrs targs @ convertArgs fn (selfs args)
                let args =
                  if fn.IsStateless then args
                  else bState :: args
                addType fn.RetType (bCall ("F#" + fn.Name) args)
              // TODO this is wrong for loop invariants and stuff (but the legacy vcc doesn't handle that correctly as well)
              | C.Expr.Old (ec, C.Macro (_, "_vcc_when_claimed", []), e) ->
                warnForIneffectiveOld ec.Token (self e)
                bSubst [("$s", er "$when_claimed_state")] (self e)
              | C.Expr.Old (ec, state, e) ->
                let be = self e
                warnForIneffectiveOld ec.Token be
                match state with 
                  | C.Macro (_, "prestate", []) -> bSubst [("$s", env.OldState)] be
                  | _ -> 
                    let state = state |> self |> bSubst [("$s", er "$$s")]
                    bSubst [("$s", state)] be
              | C.Expr.Result c ->
                addType c.Type (er "$result")
              | C.Expr.Quant (c, q) ->
                for v in q.Variables do
                  ctx.QuantVarTokens.[v] <- c.Token
                let invMapping = gdict()
                let body = trExpr { env with InverseTranslation = invMapping } q.Body
                let body =
                  match q.Condition, q.Kind with
                    | Some e, C.Forall -> bImpl (self e) body
                    | Some e, C.Exists -> bAnd (self e) body
                    | _, C.Lambda -> die()
                    | None, _ -> body                
                let supportedTypeForQuantification (v : C.Variable) =
                  match v.Type with
                    | C.Type.Ref({Kind = C.TypeKind.Struct|C.TypeKind.Union})
                    | C.Array _ ->
                      helper.Error(c.Token, 9696, "Cannot quantify over type '" + v.Type.ToString() + "' (bound variable is '" + v.Name + "').")
                      false
                    | _ -> true
                let vars = q.Variables |> List.filter supportedTypeForQuantification |> List.map trVar 
                let (body, triggers) = TriggerInference(helper, preludeBodies, c.Token, invMapping, vars).Run (body, List.map selfs q.Triggers)
                match q.Kind with
                  | C.Forall -> B.Forall (c.Token, vars, triggers, weight "user-forall", body)
                  | C.Exists -> B.Exists (c.Token, vars, triggers, weight "user-exists", body)
                  | C.Lambda -> die()
            
              | C.Expr.SizeOf(_, C.Type.TypeVar(tv)) ->bCall "$sizeof" [typeVarRef tv]
              | C.Expr.SizeOf(_, t) -> bInt t.SizeOf
              | C.Expr.This _ -> er "$_this"
              | _ ->         
                helper.Oops (expr.Token, "unhandled expr " + expr.ToString())
                er "$bogus"

          if env.InverseTranslation <> null then
            let cur = lookupWithDefault env.InverseTranslation [] res
            env.InverseTranslation.[res] <- expr :: cur
          res
        with 
          | Failure _ -> 
            helper.Error(expr.Token, 9600, "OOPS for expression " + expr.ToString())
            reraise()

      and trMacro env (ec : C.ExprCommon) n args = 
        let self = trExpr env
        let selfs = List.map self
        let trInvLabel = ctx.TrInvLabel
        match n, args with
          | "writes_check", [a] -> writesCheck env ec.Token false a
          | "prim_writes_check", [a] -> writesCheck env ec.Token true a
          | ("in_range_phys_ptr"|"in_range_spec_ptr") as in_range, [p] when vcc3 ->
            bCall ("$" + in_range) [self p]
          | in_range, args when in_range.StartsWith ("in_range") -> bCall ("$" + in_range) (selfs args)
          | ("unchecked_sbits"|"unchecked_ubits"), args ->
            bCall ("$" + n) (selfs args)
          | "_vcc_typed2", [_; a] when vcc3 && a.Type.IsFunctionPtr ->
            bCall "$valid_fnptr" [self a]
          | unchecked, args when unchecked.StartsWith ("unchecked_") -> bCall "$unchecked" (er ("^^" + unchecked.Substring (unchecked.IndexOf '_' + 1)) :: selfs args)
          | ("map_get"|"map_get_trig"), [a; b] ->
            match a.Type with
              | C.Type.Map (f, t) ->
                let fn = "$select." + (trType a.Type).ToString()
                let select = bCall fn [self a; stripType f (self b)]
                if n = "map_get" then addType t select else select
              | _ -> die()          
          | "map_zero", _ -> er ("$zero." + ctx.TypeIdToName(toTypeId ec.Type))
          | "map_updated", [a; b; c] ->
            match a.Type with
              | C.Type.Map (f, t) ->
                let fn = "$store." + (trType a.Type).ToString()
                bCall fn [self a; stripType f (self b); stripType t (self c)]
              | _ -> die()
          | "field", [C.Expr.UserData (_, ( :? C.Field as f))] ->
            er (fieldName f)
            
          | "rec_zero", [] -> er "$rec_zero"
          
          | "rec_fetch", [r; C.UserData(_, (:? C.Field as f))] ->
            let fetch = bCall "$rec_fetch" [self r; er (fieldName f)]
            match f.Type with
              | C.Type.Integer _ ->
                bCall "$unchecked" [toTypeId f.Type; fetch]
              | C.Ptr t when not vcc3 ->
                bCall "$ptr" [toTypeId t; fetch]
              | C.Bool ->
                bNeq fetch (bInt 0)
              | t ->
                castFromInt (trType t) fetch
            
          | "rec_update", [r; C.UserData(_, ( :? C.Field as f) ); v] ->
            bCall "$rec_update" [self r; er (fieldName f); trForWrite env f.Type v]
          
          | "rec_update_bv", [r; C.UserData(_, (:? C.Field as f)); bvSize; bvStart; bvEnd; v] ->
            bCall "$rec_update_bv" [self r; er (fieldName f); self bvSize; self bvStart; self bvEnd; trForWrite env f.Type v]
          
          | "vs_placeholder", [] -> er "$vs_placeholder"
          | "vs_placeholder2", [] -> er "$vs_placeholder2"
          | "vs_zero", [] -> er "$struct_zero"
          | "vs_fetch", [p] ->
            match p with
              | C.Expr.Ref _ ->
                self p
              | _ ->
                let (ptr, state, _) = vsTrans env p
                match ec.Type with
                  | C.Type.Ref ({ Kind = (C.Struct|C.Union) }) ->
                    bCall "$vs_ctor" [state;  ptr]
                  | _ ->
                    self (C.Deref (ec, C.Macro (p.Common, "vs_placeholder2", []))) 
                             |> bSubst [("$s", state); ("$vs_placeholder2", ptr)]        
                          
          | "vs_updated", [p; src] ->
            let (ptr, state, (bbase, tp)) = vsTrans env p
            match p.Type with
              | C.Ptr t ->
                let wr = trForWrite env t src
                bCall "$vs_ctor" [bCall "$update_int" [state; ptr; wr]; bCall "$vs_base" [bbase; tp]]
              | _ -> helper.Panic("non ptr type in vs_updated")
          
          | "vs_can_update", [up] ->
            match self up with
              | B.FunctionCall ("$vs_ctor", [arr; _]) ->
                bCall "$good_state" [arr]
              | _ -> helper.Panic("wrong thing in vs_can_update")
            
          | "by_claim", args ->
            self (C.Expr.Deref (ec, C.Expr.Macro (ec, "by_claim_ptr", args)))
          | "by_claim_ptr", [c; obj; ptr] ->
            bCall "$by_claim" [bState; self c; self obj; self ptr]
          | "current_claim", [] -> er "$claim"
          | "null", [] -> er "$null"
          | "dont_instantiate", [e] ->
            let arg = self e
            match e.Type with
              | C.Type.Integer _ -> bCall "$dont_instantiate_int" [arg]
              | C.Type.SpecPtr _
              | C.Type.PhysPtr _
              | C.Type.ObjectT  -> bCall "$dont_instantiate" [arg]
              | _ -> bCall "$dont_instantiate_int" [castToInt (trType e.Type) arg]
          | "_vcc_claims", [cl; cond] ->
            claims env (self cl) cond
          | "_vcc_in_domain", [s; C.Macro(_, "_vcc_use", [C.UserData(_, lbl); e1]); e2] ->
            bCall "$in_domain_lab" ((selfs [s;e1;e2]) @ [er (trInvLabel ((string)lbl))])
          | "_vcc_in_domain", args ->
              bCall "$in_domain_lab" ((selfs args) @ [er (trInvLabel "public")])
          | "_vcc_in_vdomain", [s; C.Macro(_, "_vcc_use", [C.UserData(_, lbl); e1]); e2] ->
            bCall "$in_vdomain_lab" ((selfs [s;e1;e2]) @ [er (trInvLabel ((string)lbl))])
          | "_vcc_in_vdomain", args ->
              bCall "$in_vdomain_lab" ((selfs args) @ [er (trInvLabel "public")])
          | "_vcc_sk_hack", [e] ->
            bCall "sk_hack" [self e]
          | "_vcc_ptr_eq_pure", [e1; e2] ->
            bEq (self e1) (self e2)
          | "_vcc_ptr_neq_pure", [e1; e2] ->
            bNeq (self e1) (self e2)
          | "_vcc_ptr_eq_null", [e1] ->
            bCall "$is_null" [self e1]
          | "_vcc_ptr_neq_null", [e1] ->
            bCall "$non_null" [self e1]
          | "trigger_hint", [e] ->
            if e.Type = C.Type.Bool then
              bCall "sk_hack" [self e]
            else
              bCall "sk_hack" [bCall "$instantiate_int" [ctx.CastToInt (ctx.TrType e.Type) (self e)]]

          // trigger inference will delete this guy
          | "trigger_level", [e] ->
            bCall "trigger_level" [self e]
            
          | ("_vcc_set_in"|"_vcc_set_in0"), [p; C.Macro (_, "_vcc_owns", _)] ->
            match p.Type with
              | C.Ptr t when not t.IsComposite ->
                helper.Warning (p.Token, 9104, "primitive pointers are unlikely to be in the owns set")
              | _ -> ()
            bCall ("$" + n.Substring 5) (selfs args)
                              
          | "reads_same", [ptr] ->
            let ptr = self ptr
            let expr = bCall "$mem" [bState; ptr]
            B.Primitive ("==", [expr; B.Old expr])
          
          | "keeps", [p1; p2] ->
            match p2.Type with
              | C.Ptr t when not t.IsComposite ->
                helper.Warning (p2.Token, 9104, "primitive pointers are unlikely to be in the owns set")
              | _ -> ()
            bCall "$keeps" [bState; self p1; self p2]         
          | ("inv_check" | "token_holder" | "_vcc_bv_lemma"), [e] ->
            self e
          | "ite", ([cond; th; el] as args) ->
            // TODO: check if this is still needed
            let cond' =
              match cond with
                | C.Expr.Ref (_, { Kind = C.QuantBound }) ->
                  bCall "$bool_id" [self cond]
                | _ -> self cond
            B.Ite (cond', self th, self el)
          | "can_use_frame_axiom_of", [C.Call (_, f, _, _)] ->
            bCall "$can_use_frame_axiom_of" [er ("cf#" + f.Name)]
          | "_vcc_typeof", [e] ->
            typeOf env e                
          | "_vcc_obj_eq", [e1; e2] ->
            bEq (self e1) (self e2)
          | "_vcc_obj_neq", [e1; e2] ->
            bNeq (self e1) (self e2)
          | ("bv_extract_signed" | "bv_extract_unsigned" | "bv_update" as name), args ->
            bCall ("$" + name) (selfs args)
          | "_vcc_extent", [arg] when (match arg.Type with C.Ptr t -> noUnions t | _ -> false) ->
            bCall "$struct_extent" [self arg]              
          | "_vcc_thread_local", [C.Dot (_, p, f)] when not f.IsVolatile && not f.Type.IsComposite && f.Parent.Kind = C.Struct ->
            self (C.Macro (ec, n, [p]))
          | "_vcc_union_active", [C.Dot (_, p, f)] ->
            bCall "$union_active" [bState; self p; er (fieldName f)]
          // avoid warning 9106
          | "keeps_stable", [C.Old (_, _, e1); e2] when not (bContains "$s" (self e1)) ->
             bEq (self e1) (self e2)
          | "keeps_stable", [e1; e2] ->
             bEq (self e1) (self e2)
          | "_vcc_gemb", [e] ->
            let rec strip_dots = function
              | C.Index (_, e, _)
              | C.Dot (_, e, _) -> strip_dots e
              | e -> e
            match strip_dots e with
              | C.Expr.Ref (_, v) as e when v.Kind = C.VarKind.ConstGlobal ->
                self e
              | x -> 
                helper.Error(e.Token, 9651, "gemb(...) applied to non-global", None)
                self x                
          | "instantiate_ptr", [e] ->
            bCall "$instantiate_ptr" [self e]
          | "state", [] -> bState
          | "is_atomic_obj", [e] ->
            let e = self e
            bMultiOr (List.map (bEq e) env.AtomicObjects)
          | "pure_outpar", [ C.Expr.Call(_, fn, targs, args); C.Expr.Ref(_, v); arg] ->
            let args =  List.map ctx.ToTypeIdArraysAsPtrs targs @ convertArgs fn (selfs args)
            let args =
              if fn.IsStateless then args
              else bState :: args
            bEq (addType v.Type (bCall ("F#" + fn.Name + "#OP#" + v.Name) args)) (self arg)
          | "stackframe", [] -> er "#stackframe"
          | "map_eq", [e1; e2] -> bCall ("$eq." + (ctx.TypeIdToName (toTypeId e1.Type))) [self e1; self e2]
          | "float_literal", [C.Expr.UserData(_, f)] ->
            match f with
              | :? float as f -> ctx.GetFloatConst f
              | :? single as s -> ctx.GetFloatConst ((float)s)
              | _ -> die()
          | name, [e1; e2] when name.StartsWith("_vcc_deep_struct_eq.") || name.StartsWith("_vcc_shallow_struct_eq.") ->
            B.FunctionCall(name, [self e1; self e2])
          | n, _ when Simplifier.alwaysPureCalls.ContainsKey n ->
            let signature = Simplifier.alwaysPureCalls.[n]
            let rec aux acc idx (args:list<C.Expr>) =
              if idx >= signature.Length then
                xassert (args = [])
                bCall ("$" + n.Substring 5) (List.rev acc)
              else
                let assertFirstIsState() = 
                  match args with
                    | arg :: _ ->
                      match arg.Type with
                        | C.MathTypeRef "state_t" -> ()
                        | _ -> die()
                    | _ -> die()
                          
                match signature.[idx] with
                  | 't' -> 
                    match args with
                      | arg :: rest ->
                        aux (typeOf env arg :: self arg :: acc) (idx + 1) rest
                      | [] -> 
                        helper.Error (ec.Token, 9615, "expecting more arguments in call to " + n, None)
                        aux acc signature.Length []
                  | 'S'
                  | 's' 
                  | 'a'
                  | 'p'
                  | 'i'
                  | '.' ->
                    if (signature.[idx] = 's' || signature.[idx] = 'S') then assertFirstIsState()
                    match args with
                      | arg :: rest ->
                        aux (self arg :: acc) (idx + 1) rest
                      | [] -> 
                        helper.Error (ec.Token, 9615, "expecting more arguments in call to " + n, None)
                        aux acc signature.Length []
                  | _ -> die()
            aux [] 0 args
          | "_vcc_current_context", [] -> IF.getPC
          | "_vcc_expect_unreachable_child", [e] -> B.Expr.FunctionCall("$expect_unreachable_child",[trExpr env e])
          | "_vcc_label_of", [e] -> IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) (IF.exprLevel false e)
          | "_vcc_seclabel_bot", [] -> B.Expr.Ref "$lblset.bot"
          | "_vcc_seclabel_top", [] -> B.Expr.Ref "$lblset.top"
          | "_vcc_lblset_leq", [l1;l2] -> B.Expr.FunctionCall ("$lblset.leq", [trExpr env l1; trExpr env l2])
          | "_vcc_is_member", [p; c] -> B.Expr.FunctionCall("$ptrclub.isMember", [trExpr env p; trExpr env c])
          | _ ->
            helper.Oops (ec.Token, sprintf "unhandled macro %s" n)
            er "$bogus"                


      and typeOf env (e:C.Expr) =
        match e.Type with
          | C.ObjectT ->
            match e with
              | C.Expr.Macro (_, "_vcc_as_array", [arr; sz]) ->
                bCall "$array" [typeOf env arr; trExpr env sz]
              | _ ->
                bCall "$typ" [trExpr env e]
          | C.Ptr t -> toTypeId t 
          | C.FunctionPtr decl -> er "$fnptr_type"
          | t ->
            helper.Error (e.Token, 9616, t.ToString() + " is not a pointer", None)
            er "$bogus"


      and writesCheck env tok prim (e:C.Expr) =
        if helper.Options.OmitReadWriteChecking then
          bTrue
        else
          match e.Type with
            | C.ObjectT
            | C.Ptr _ ->
              inWritesOrIrrelevant false env (trExpr env e) (if prim then Some e else None)
            | C.MathTypeRef "ptrset" ->
              xassert (not prim)
              writesMultiCheck false env tok (fun p -> bCall "$set_in" [p; trExpr env e])
            | _ -> 
              helper.Error (e.Token, 9617, "unsupported thing passed to writes check (" + e.ToString() + ")", None)
              er "$bogus"

      and writesInclusion env env'  = ()

      and readsCheck env isWf (p:C.Expr) =
        if helper.Options.OmitReadWriteChecking then
          [B.Stmt.MkAssert (afmte 8511 "disabled reads check of {0}" [p], bTrue)]
        else
          let cond id msg name (args : C.Expr list) = 
            (afmte id msg args.Tail, trExpr env (C.Macro ({p.Common with Type = C.Bool }, name, args)))
          let (codeTp, codeTh, suff) =
            if isWf then (8501, 8502, " (in well-formedness check)")
            else         (8511, 8512, "")
          let tp =
            if helper.Options.RunTestSuite || vcc3 then []
            else [cond codeTp ("{0} is typed" + suff) "_vcc_typed2" [cState;p] ]
          let th =
            match p with
              | C.Dot (_, p', f) when not f.Parent.IsUnion ->
                let msg, isAtomic =
                  match env.AtomicReads with
                    | [] -> "", []
                    | _ ->
                      "or atomically updated ", List.map (bEq (trExpr env p')) env.AtomicReads
                let tok, prop = 
                  if f.IsVolatile then
                    cond codeTh ("{0} is mutable " + msg + "(accessing volatile field " + f.Name + ")" + suff) "_vcc_mutable" [cState; p']
                  else
                    if vcc3 && not f.Type.IsComposite then
                      cond codeTh ("{0} is thread local " + msg + "(accessing field " + f.Name + ")" + suff) "_vcc_thread_local" [cState; p']
                    else
                      cond codeTh ("{0} is thread local" + suff) "_vcc_thread_local2" [cState; p]
                tok, bMultiOr (prop :: isAtomic)
              | _ -> 
                let msg, isAtomic =
                  match env.AtomicReads with
                    | [] -> "", []
                    | _ ->
                       // TODO: shouldn't that be $volatile_span()?
                      " or atomically updated", List.map (fun o -> bCall "$set_in" [trExpr env p; bCall "$span" [o]]) env.AtomicReads
                let tok, prop =
                  if vcc3 then
                    cond codeTh ("{0} is thread local" + msg + suff) "_vcc_thread_local" [cState; p]
                  else
                    cond codeTh ("{0} is thread local" + msg + suff) "_vcc_thread_local2" [cState; p]
                tok, bMultiOr (prop :: isAtomic)
          List.map B.Stmt.MkAssert (tp @ [th])
      
      and writesMultiCheck use_wr env tok f =
        let name = "#writes" + ctx.TokSuffix tok
        let p = er name
        let precond = 
          if nestingExtents then 
            bAnd (f p) (bOr (bCall "$is_primitive" [bCall "$typ" [p]]) (bCall "$is_non_primitive" [bCall "$typ" [p]]))
          else f p
        B.Expr.Forall (Token.NoToken, [(name, tpPtr)], dont_inst p, weight "dont-inst", bImpl precond (inWritesOrIrrelevant use_wr env p None))
         
      and inWritesOrIrrelevant use_wr env (e:B.Expr) (origPrim:option<C.Expr>) =
        let prim = origPrim.IsSome
        let atomicWr =
          if prim then
            env.AtomicObjects |>
              List.map (fun o -> bCall "$set_in" [e; bCall "$volatile_span" [bState; o]]) |>
              bMultiOr
          else bFalse
        let pred = if prim || use_wr then "$writable" else "$top_writable"
        if vcc3 then
          let ch = 
            if prim then
              bCall "$writable_prim" [bState; env.WritesTime; e]
            else
              bCall pred (bState :: env.WritesTime :: [e])
          bOr atomicWr ch
          (*
          match origPrim with
            | Some (C.Dot (_, p, f)) when f.Parent.Kind = C.Struct ->
            | _ -> *)
        else
          bOr atomicWr (bCall pred [bState; env.WritesTime; e])
        
      and isInWrites (env:Env) (p:B.Expr) =
        let trWrites acc (e:C.Expr) =
          let test =
            match e.Type with
              | C.ObjectT
              | C.Ptr _ -> bEq p (trExpr env e)
              | C.MathTypeRef "ptrset" -> 
                if isSetEmpty e then bFalse
                else bCall "$set_in" [p; trExpr env e]
              | _ -> helper.Error (e.Token, 9618, "unsupported writes clause " + e.ToString(), None); er "$bogus"
          bOr acc test
        List.fold trWrites bFalse env.Writes

      and claimIn env claim s expr =
        let repl = [("$claim", claim); ("$s", s); ("$when_claimed_state", bState)]
        let doClosed = function
          | B.Expr.FunctionCall ("$closed", args) -> Some (B.Expr.FunctionCall ("$claimed_closed", args))
          | _ -> None
        (bSubst repl (trExpr env expr)).Map doClosed
        
      and claims env claim expr =
        let cs = "#cs" + (!claimStateId).ToString()
        incr claimStateId
        let ms = [er cs]
        let use_claim = bCall "$valid_claim" (ms @ [claim])
        B.Expr.Forall (Token.NoToken, [(cs, tpState)], [[use_claim]], weight "claims",
             bImpl use_claim (claimIn env claim (er cs) expr))      
      
      and vsTrans env p =
        let theBase = ref p
        let lastType = ref None
        let rec findPath = function
          | C.Dot (c, e, f) -> 
            lastType := Some f.Parent
            C.Dot (c, findPath e, f)
          | C.Index (c, e, i) ->
            C.Index (c, findPath e, i)
          | e ->
            theBase := e
            C.Macro (e.Common, "vs_placeholder", [])                    
        let path = findPath p
        match (!theBase).Type with
          | C.MathTypeRef "struct" -> ()
          | _ ->
            helper.Oops (p.Token, "expected $struct type, got " + (!theBase).Type.ToString() + " for " + p.ToString())
        let bbase = trExpr env !theBase
        if (!lastType).IsNone then
          helper.Oops (p.Token, "vsTrans on " + p.ToString())
        let tp = toTypeId (C.Type.Ref ((!lastType).Value))
        let (state, bbase, vsBase) = 
          match bbase with 
            | B.FunctionCall("$vs_ctor", [state; (B.FunctionCall("$vs_base", [bbase; _]) as vsBase)]) -> (state, bbase, vsBase)
            | _ -> bCall "$vs_state" [bbase], bbase, bCall "$vs_base" [bbase; tp]
        let ptr = trExpr env path |> bSubst [("$vs_placeholder", vsBase)]
        (ptr, state, (bbase, tp))      
      
      and trForWrite env t (e2:C.Expr) =
        match e2.Type with
          | C.MathTypeRef "struct" -> die()
          | _ -> ()
        let e2' = stripType t (trExpr env e2)
        match e2.Type with
          | C.Ptr _ ->
            match t with 
              | C.Ptr _ when not vcc3 -> e2'
              | _ -> bCall "$ptr_to_int" [e2']
          | C.Type.Integer _ -> e2'
          | _ ->
            castToInt (trType t) e2'

      let repl e = bSubst [("$$s", er "$s")] e
      
      let trExpr env expr = repl (trExpr env expr)
      let isInWrites e p = repl (isInWrites e p)
      let typeOf env e = repl (typeOf env e)
      let readsCheck env isWf p = 
        List.map (function B.Stmt.Assert (_, t, e) -> B.Stmt.MkAssert (t, repl e) | _ -> die()) (readsCheck env isWf p)
      let objectWritesCheck env expr = repl (inWritesOrIrrelevant false env expr None)
      let claimIn env claim s expr = repl (claimIn env claim s expr)
      let claims env claim expr = repl (claims env claim expr)
      let trForWrite env t e2 = repl (trForWrite env t e2)
      
      let trMacro = ()
      let writesCheck env tok prim (e:C.Expr) = ()
      let writesMultiCheck env tok f = ()
      let vsTrans env p = ()
      let inWritesOrIrrelevant = ()
      
      let trLabel (label:C.LabelId) = label.Name

      let stateChanges (env:Env) =
        if env.Writes = [] then
          bCall "$writes_nothing" [env.WritesState; bState]
        else if vcc3 then
          let inWr = isInWrites env (er "#p") |> bSubst [("$s", env.WritesState)]
          let wrSet = B.Lambda (Token.NoToken, [("#p", tpPtr)], [], inWr)
          bCall "$modifies" [env.WritesState; bState; wrSet]
        else
          let p = er "#p"
          let t = er "#t"
          let inWr = isInWrites env p |> bSubst [("$s", env.WritesState)]
          let base_ = bOr (bCall "$irrelevant" [env.WritesState; p]) inWr
          
          let quant prop =
            let newState = bCall prop [bState; p]
            let stateEq = bCall (prop + "_eq") [env.WritesState; bState; p]
            B.Expr.Forall (Token.NoToken, ["#p", tpPtr], [[newState]], weight ("writes-" + prop.Replace ("$", "")), bOr base_ stateEq)
            
          let timestamp = bCall "$timestamp_post" [env.WritesState; bState]
          bMultiAnd [quant "$mem"; quant "$st"; quant "$ts"; timestamp]
              
      let rec hasSideEffect = function
        | C.Expr.Ref _
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _ -> false
        | C.Expr.Cast (_, _, e) -> hasSideEffect e
        | C.Expr.Prim (_, _, args) -> List.exists hasSideEffect args      
        | _ -> true
        
      let claimedObjCheck env tok doClaim obj =
        let bobj = trExpr env obj
        let wr = objectWritesCheck env bobj
        let own = List.map (bEq (bCall "$owner" [bState; bobj])) env.AtomicObjects
        let ref_cnt = bCall "$ref_cnt" [bState; bobj]
        let ref_cnt_plus_one =
          B.Expr.Primitive ((if doClaim then "+" else "-"), [ref_cnt; B.Expr.IntLiteral (bigint.One)])
        let tok = 
          let cl_or_uncl = if doClaim then "claim" else "unclaim"
          afmtet tok 8008 ("{0} is non-writable and its owner is not listed in atomic(...) (and thus is impossible to " + cl_or_uncl + ")") [obj]
        let tok2 = 
          afmtet tok 8009 "type of object {0} was not marked with vcc(claimable)" [obj]
        [B.Stmt.MkAssert (tok, bMultiOr (wr :: own));
         B.Stmt.MkAssert (tok2, bCall "$is_claimable" [typeOf env obj]);
         B.Stmt.Call (C.bogusToken, [], "$write_ref_cnt", [bobj; ref_cnt_plus_one]);
         B.Stmt.MkAssume (B.Expr.Primitive (">=", [ref_cnt; B.Expr.IntLiteral (bigint.Zero)]))]
               
      let claimId = ref 0            
      
      let trClaim (env:Env) upgrade tok (local:C.Variable) args =
        match args with
          | C.Pure (_, expr) :: objects ->
            let claim = "claim#" + (!claimId).ToString()
            incr claimId
            let conditions = TransUtil.splitConjunction expr
            let inState = claimIn env (er claim)
            
            let didAlloc = bImpl (bNeq (er claim) (er "$no_claim"))
            let mkInitAssert s (expr:C.Expr) =
              let tok = afmtet tok 8520 "chunk {0} of the claim initially holds" [expr]
              B.Stmt.MkAssert (tok, didAlloc (inState s expr))
              
            let mkAdmAssert s (expr:C.Expr) =
              let tok = afmtet tok 8521 "chunk {0} of the claim holds after a step of the machine" [expr]
              B.Stmt.MkAssert (tok, inState s expr)
            
            let rf n = er (claim + n)
            
            let doObj obj =
              let obj' = trExpr env obj
              let tok' = afmtet tok 8528 "object {0} is closed before claiming it" [obj]
              B.Stmt.MkAssert (tok', bCall "$closed" [bState; obj']) :: claimedObjCheck env tok true obj
            
            let doClaim (obj:C.Expr) =
              let obj' = trExpr env obj
              let tokWrite = afmtet tok 8023 ("{0} is non-writable and (and thus is impossible to upgrade)") [obj]
              let tokWrap = afmtet tok 8024 "the claim {0} is not wrapped before upgrade" [obj]
              let tokRef = afmtet tok 8025 "the claim {0} has outstanding claims" [obj]
              [B.Stmt.MkAssert (tokWrap, bCall "$wrapped" [bState; obj'; er "^^claim"]);
               B.Stmt.MkAssert (tokRef, bEq (bCall "$ref_cnt" [bState; obj']) (bInt 0));
               B.Stmt.MkAssert (tokWrite, objectWritesCheck env obj')]
            
            let killClaim obj =
              B.Call (C.bogusToken, [], "$kill_claim", [trExpr env obj])
                        
            let wrChecks = 
              if upgrade then
                // first checking for conditions and then killing all at once
                // allows for aliasing between claims
                (List.map doClaim objects |> List.concat) @
                  List.map killClaim objects
              else
                List.map doObj objects |> List.concat
            
            let claimAdm =
              [B.Stmt.MkAssume (inState (rf "s1") expr);
               B.Stmt.MkAssume (bCall "$valid_claim_impl" [rf "s0"; rf "s2"]);
               B.Stmt.MkAssume (bCall "$claim_transitivity_assumptions" ([rf "s1"; rf "s2"; er claim; er (ctx.GetTokenConst tok)]));
               ] @
               List.map (mkAdmAssert (rf "s2")) conditions @
               [B.Stmt.MkAssume bFalse]
            let rand = claim + "doAdm"
            let claimAdm cond =
              [B.Stmt.If (bAnd cond (er rand), B.Stmt.Block (B.Stmt.VarDecl ((rand, B.Type.Bool), None) :: claimAdm), B.Stmt.Block [])]
               
            let initial cond = 
              [B.Stmt.MkAssume (didAlloc (bCall "$claim_initial_assumptions" [bState; er claim; er (ctx.GetTokenConst tok)]))] @
              List.map (mkInitAssert bState) conditions @
              claimAdm cond @
              [B.Stmt.MkAssume (didAlloc (claims env (er claim) expr))]
            
            let claims_obj = List.map (fun e -> B.Stmt.MkAssume (bCall (if upgrade then "$claims_upgrade" else "$claims_obj") [er claim; trExpr env e])) objects 
            
            let assign = 
              [B.Stmt.Assign (varRef local, if vcc3 then er claim else bCall "$ref" [er claim]);
               ctx.AssumeLocalIs tok local;
               assumeSyncCS "claim constructed" env tok]
                                      
            let initial =
              match env.AtomicObjects with
                | [] -> initial bTrue
                | _ ->
                  let ctx = env.ClaimContext.Value
                  let cond = bNeq (er claim) (er "$no_claim")
                  ctx.ClaimChecks <- initial cond @ ctx.ClaimChecks
                  ctx.ClaimInits <- B.Stmt.MkAssume (bEq (er claim) (er "$no_claim")) :: ctx.ClaimInits 
                  []
                  
            [B.Stmt.VarDecl ((claim, tpPtr), None);
             B.Stmt.VarDecl ((claim + "s0", tpState), None);
             B.Stmt.VarDecl ((claim + "s1", tpState), None);
             B.Stmt.VarDecl ((claim + "s2", tpState), None);
            ] @
            wrChecks @
            [B.Stmt.Assign ((rf "s0"), bState)] @
            [B.Stmt.Call (tok, [claim], "$alloc_claim", [])] @
            claims_obj @
            assign @
            initial
            
          | _ ->           
            helper.Oops (tok, "wrong format of a claim")
            []

      let setWritesTime tok env wr =
        let name = "#wrTime" + ctx.TokSuffix tok
        let p = er "#p"
        let inWritesAt = bCall "$in_writes_at" [er name; p]
        let env = { env with Writes = wr; WritesTime = er name }
        let defWrites =
          if vcc3 then
            B.Stmt.MkAssume (bCall "$def_writes" [bState; er name; B.Expr.Lambda (Token.NoToken, [("#p", tpPtr)], [],  (isInWrites env p))])
          else
            B.Stmt.MkAssume (B.Expr.Forall (Token.NoToken, [("#p", tpPtr)], [[inWritesAt]], weight "begin-writes", bEq inWritesAt (isInWrites env p)))
        let init =
          [B.Stmt.VarDecl ((name, B.Type.Int), None);
           B.Stmt.MkAssume (bEq (er name) (bCall "$current_timestamp" [bState]));
           defWrites]
        (init, env)
                  
      let trUnclaim env tok = function
        | claim :: objects ->
          let claim' = trExpr env claim
          let doObj obj =
            let obj' = trExpr env obj
            let tok = afmtet tok 8522 "object {0} was claimed by {1}" [obj; claim]
            B.Stmt.MkAssert (tok, bCall "$claims_obj" [claim'; obj']) :: claimedObjCheck env tok false obj
          let tr = trExpr env
          let rec different acc = function
            | x :: xs ->
              let mkDiff y =
                let tok = afmtet tok 8010 "object {0} might equal {1}" [x; y]
                B.Stmt.MkAssert (tok, bNeq (tr x) (tr y))
              different (List.map mkDiff xs @ acc) xs
            | [] -> acc
          let allowWrite = 
            B.Stmt.MkAssert (afmtet tok 8523 "the disposed claim {0} is writable" [claim], objectWritesCheck env claim')
          let different = different [] objects
          let decrements = List.map doObj objects |> List.concat
          let call = B.Stmt.Call (tok, [], "$unclaim", [claim'])
          allowWrite :: different @ [call] @ decrements @ [assumeSyncCS "claim disposed" env tok]
        | _ -> die()
      
      let trAtomic trStmt env (ec:C.ExprCommon) objs body =
        let rec split acc = function
          | C.Stmt (_, C.Macro (_, "begin_update", [])) :: xs -> List.rev acc, xs
          | C.Block (_, xs, _) :: xs' -> split acc (xs @ xs')
          | x :: xs -> split (x :: acc) xs
          | [] -> [], List.rev acc
        
        let getType (obj : C.Expr) (bobj : B.Expr)=
          match obj.Type with
            | C.Ptr(_) -> ptrType obj
            | C.Type.ObjectT -> bCall "$typ" [bobj]
            | _ -> die()
          
        let (objs, claims) = List.partition (fun (e:C.Expr) -> e.Type <> C.Type.SpecPtr C.Claim) objs
        let (before, after) =
          match body with
            | C.Block (_, lst, _) -> split [] lst
            | _ -> [], [body]
        let (save, oldState) = saveState "beforeAtomic"
        let ctx = { ClaimChecks = []; ClaimInits = [] }
        let (atomicInits, atomicObjs) =
          let init = ref []
          let saveRef e =
            let e' = trExpr env e
            let tmp = "atomicObj#" + (helper.UniqueId()).ToString()
            init := B.Stmt.VarDecl ((tmp, tpPtr), None) :: B.Stmt.Assign (er tmp, e') :: !init
            (er tmp, e)
          let res = List.map saveRef objs
          (!init, res)
          
        let preEnv = { env with AtomicReads = List.map fst atomicObjs }
        let env' = { preEnv with AtomicObjects = List.map fst atomicObjs ;
                                 OldState = oldState ;
                                 ClaimContext = Some ctx }
        let flmap f l = List.map f l |> List.concat
        let checkInv (bobj, (obj:C.Expr)) =
          match obj.Type with
            | C.Ptr (C.Type.Ref td) -> 
              let mkAssert (e:C.Expr) =
                let tok = afmtet obj.Token 8524 "chunk {0} of invariant of {1} holds after atomic" [e; obj]
                B.Stmt.MkAssert (tok, trExpr env' e |> bSubst [("$_this", bobj)])
              td.Invariants |> List.map TransUtil.splitConjunction |> List.concat |> List.map mkAssert
            | _ ->
              [B.Stmt.MkAssert (afmte 8525 "invariant of {0} holds after atomic" [obj],
                              bCall "$inv2" [oldState; bState; bobj; getType obj bobj])]
        
        let valid_claims =
          [for c in claims ->
            B.Stmt.MkAssert (afmte 8526 "claim {0} is valid" [c],
                             bCall "$valid_claim" [bState; trExpr env c])]
                           
        let before =
          if before = [] then []
          else valid_claims @ flmap (trStmt preEnv) before
        
        // this sets env'.ClaimContext things
        let atomicAction = flmap (trStmt env') after
          
        [B.Stmt.Call (ec.Token, [], "$atomic_havoc", []);
         assumeSyncCS "inside atomic" env ec.Token;
         ] @ 
        atomicInits @
        before @
        valid_claims @
        [for (bobj, obj) in atomicObjs do
             yield B.Stmt.MkAssert (afmte 8527 "{0} is closed (for atomic(...))" [obj], bCall "$closed" [bState; bobj])
             yield B.Stmt.MkAssume (bCall "$inv" [bState; bobj; getType obj bobj])
             ] @
        save @
        ctx.ClaimInits @
        atomicAction @ 
        ctx.ClaimChecks @
        flmap checkInv atomicObjs @
        [assumeSync env ec.Token]
        
      
      
      let callConvCnt = ref 0
      let rec trStmt (env:Env) (stmt:C.Expr) =
        let self = trStmt env
        let cmt () = 
          let c = B.Stmt.Comment (((stmt.ToString ()).Replace ("\n", " ")).Replace ("\r", ""))
          if helper.Options.PrintCEVModel then
            B.Stmt.Block [c; captureState "" stmt.Token]
          else
            c
        let doCall (c:C.ExprCommon) (res : C.Variable list) fn (name:string) targs args =
          let name' = 
            if name.StartsWith "_vcc_" then "$" + name.Substring 5 
            else name
          if env.AtomicObjects <> [] then
            match fn with
              | Some (f : C.Function) when f.IsSpec && f.IsPure -> ()
              | _ ->
                match name' with
                  | "$wrap"
                  | "$unwrap"
                  | "$deep_unwrap" 
                  | "$static_wrap"
                  | "$static_unwrap"
                  | "$alloc" ->
                    helper.Error (c.Token, 9626, name.Substring 5 + "(...) cannot be used inside atomic update/read", None)
                  | "$bump_volatile_version"
                  | "$unclaim"
                  | "$set_closed_owner"
                  | "$giveup_closed_owner"
                  | "$set_closed_owns" -> ()
                  | name when name.StartsWith("lambda#") -> ()
                  | name ->
                    helper.Error (c.Token, 9627, "ordinary functions like " + name + "(...) cannot be used inside atomic update/read", None)
          let args =  List.map (trExpr env) args
          let args, resultIsObjT =
            match fn with
              | Some (f:C.Function) ->
                let resObj =
                  match f.RetType with
                    | C.Ptr _ when f.Name.StartsWith "_vcc_" -> true // not quite sure, maybe not needed
                    | C.ObjectT -> true
                    | _ -> false
                if f.Parameters.Length + (if f.RetType = C.Type.Void then 0 else 1) <> args.Length + res.Length ||
                   f.InParameters.Length <> args.Length
                then
                  helper.Oops (c.Token, "wrong number of parms")
                convertArgs f args, resObj
              | None -> args, false
          let varIsObjT =
            match res with
              | [(v:C.Variable)] when v.Type = C.ObjectT -> true
              | _ -> false
          let args =
            match args, name' with
              | [B.Expr.FunctionCall ("$dot", [p; f])], "$union_reinterpret" -> [p; f]
              | _ -> args
          let resBuf, tail = 
            if not vcc3 && resultIsObjT <> varIsObjT then
              let tmp = "#callConv#" + (!callConvCnt).ToString()
              incr callConvCnt
              let vardecl = B.Stmt.VarDecl ((tmp, if resultIsObjT then tpPtr else B.Type.Int), None)
              let resV = res.Head
              let rs =
                if resultIsObjT then
                  bCall "$ref" [er tmp]
                else
                  match resV.Type with
                    | C.Ptr t ->
                      bCall "$ptr" [toTypeId t; er tmp]
                    | _ -> die()
              let assign = B.Stmt.Assign (varRef resV, rs)
              let vname,_ = trVar resV
              let setSecLabel = if (env.hasIF) then
                                  match name' with
                                    | ("$stack_alloc"|"$alloc"|"$spec_alloc"|"$spec_alloc_array"|"$alloc_claim") ->
                                     [IF.setPLabel stmt.Token (er tmp) (B.Expr.Ref "$lblset.top")   // Uninitialised memory is high
                                      assumeSync env stmt.Token
                                      IF.setPMeta stmt.Token (er tmp) IF.getPC
                                      assumeSync env stmt.Token
                                      IF.setLLabel ("FlowData#"+vname) (B.Expr.Ref "$lblset.bot")   // This is the label of the pointer, not the raw data
                                      IF.setLMeta ("FlowData#"+vname) IF.getPC]
                                    | _ -> []
                                else []
              [tmp], [vardecl; assign] @ setSecLabel
            else
              let setSecLabel = if (env.hasIF) then
                                  match name' with
                                    | ("$stack_alloc"|"$alloc"|"$spec_alloc"|"$spec_alloc_array"|"$alloc_claim") ->
                                     [IF.setPLabel stmt.Token (er (fst (trVar res.Head))) (B.Expr.Ref "$lblset.top")   // Uninitialised memory is high
                                      assumeSync env stmt.Token
                                      IF.setPMeta stmt.Token (er (fst (trVar res.Head))) IF.getPC
                                      assumeSync env stmt.Token
                                      IF.setLLabel ("FlowData#"+(fst (trVar res.Head))) (B.Expr.Ref "$lblset.bot")   // This is the label of the pointer, not the raw data
                                      IF.setLMeta ("FlowData#"+(fst (trVar res.Head))) IF.getPC]
                                    | _ -> []
                                else []
              List.map ctx.VarName res, setSecLabel
          let syncEnv =
            match name' with
              | "$wrap"
              | "$unwrap" 
              | "$static_wrap"
              | "$static_unwrap"
              | "$static_wrap_non_owns"
              | "$atomic_havoc"
              | "$havoc_others"
              | "$unwrap_check" -> { env with AtomicObjects = [er "$no_such_thing"] }
              | _ -> env
          let targs = List.map toTypeId targs
          [cmt (); B.Stmt.Call (c.Token, resBuf, name',  targs @ args); assumeSync syncEnv c.Token] @ tail

        try         
          match stmt with
            | C.Expr.Block (_, stmts, _) -> 
              List.concat (List.map self stmts)
            | C.Expr.Comment (_, s) -> 
              [B.Stmt.Comment s]
            | C.Expr.Assert (_, C.Expr.Macro (_, "_vcc_bv_lemma", [e]), []) -> 
              [cmt (); B.Stmt.MkAssert (stmt.Token, bv.TrBvExpr env e)]
            | C.Expr.Assert (_, C.Expr.Macro (_, "reads_check_normal", [e]), []) ->
              cmt () :: readsCheck env false e            
            | C.Expr.Assert (_, e, []) -> 
              [cmt (); B.Stmt.MkAssert (stmt.Token, trExpr env e)]
            | C.Expr.Assert(ec, e, trigs) -> helper.Oops(ec.Token, "non-empty triggers on assert"); trStmt env (C.Expr.Assert(ec, e, []))
            | C.Expr.Assume (_, e) -> 
              [cmt (); B.Stmt.MkAssume (trExpr env e)]
            | C.Expr.Return (c, s) ->
              match s with
              | None -> [cmt (); B.Stmt.MkAssert (c.Token, bCall "$position_marker" []); B.Stmt.Goto (c.Token, ["#exit"])]
              | (Some e) -> 
                [cmt (); B.Stmt.Assign (B.Expr.Ref "$result", stripType e.Type (trExpr env e)); B.Stmt.MkAssert (c.Token, bCall "$position_marker" []); B.Stmt.Goto (c.Token, ["#exit"])]
            | C.Expr.Macro (_, "havoc", [e ;t]) ->
              [cmt (); B.Stmt.Call (e.Token, [], "$havoc", [trExpr env e; trExpr env t]); assumeSync env e.Token]
            | C.Expr.Macro (_, "_vcc_add_member", [p; c]) ->
              let bClub = trExpr env c
              [B.Stmt.Assign(bClub, B.Expr.FunctionCall("$ptrclub.addMember", [trExpr env p; bClub]))
               B.Stmt.MkAssume(B.Expr.FunctionCall("is_active_ptrclub", [bClub]))]
            | C.Expr.Macro (_, "_vcc_downgrade_to", [C.Expr.Ref _ as v; e]) ->
              let setLabels = [IF.setLLabel ("FlowData#"+(trExpr env v).ToString()) (IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) (IF.exprLevel false e))
                               IF.setLMeta ("FlowData#"+(trExpr env v).ToString()) (B.Expr.Ref "$lblset.bot")]
              let tokNotEqual = afmte 9717 "{0} == {1}" [v; e]
              let tokHighCtxt = afmte 9718 "context is low" [stmt]
              [cmt(); B.Stmt.MkAssert (tokNotEqual, B.Expr.Primitive("==", [trExpr env v; trExpr env e])); B.Stmt.MkAssert (tokHighCtxt, B.Expr.FunctionCall("$lblset.leq",  [IF.getPC; B.Expr.Ref "$lblset.bot"]))] @ setLabels
            | C.Expr.Macro (_, "_vcc_downgrade_to", [C.Expr.Deref (_, var) as v; e]) ->
              let setLabels =
                [IF.setPLabel stmt.Token (trExpr env var) (IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) (IF.exprLevel false e))
                 assumeSync env stmt.Token
                 IF.setPMeta stmt.Token (trExpr env var) (B.Expr.Ref "$lblset.bot")
                 assumeSync env stmt.Token]
              let tokNotEqual = afmte 9717 "{0} = {1}" [v; e]
              let tokHighCtxt = afmte 9718 "context is low" [stmt]
              [cmt(); B.Stmt.MkAssert (tokNotEqual, B.Expr.Primitive("==", [trExpr env v; trExpr env e])); B.Stmt.MkAssert (tokHighCtxt, B.Expr.FunctionCall("$lblset.leq",  [IF.getPC; B.Expr.Ref "$lblset.bot"]))] @ setLabels
            | C.Macro(_, "test_classifier_validity_check", [C.Expr.Quant(ec, {Kind = C.QuantKind.Forall; Variables = [p]; Triggers = trigs; Condition = cond; Body = body})]) ->
              let tokClass = afmte 0000 "the provided test classifier is valid" [stmt]
              let bodyLabel = IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) (IF.exprLevel false body)
              ctx.QuantVarTokens.[p] <- ec.Token
              let p,pt = trVar p
              let bodyCheck = B.Expr.FunctionCall("$seclbl.leq", [B.Expr.ArrayIndex(bodyLabel, [B.Expr.Ref p]); B.Expr.Ref "$seclbl.bot"])
              let conditioned =
                match cond with
                  | None -> bodyCheck
                  | Some c -> B.Primitive("==>", [trExpr env c; bodyCheck])
              [B.Stmt.MkAssert (tokClass,
                                B.Expr.Forall(Token.NoToken,
                                              [p,pt],
                                              [], [],
                                              conditioned))]
            | C.Expr.MemoryWrite (_, e1, e2) when (not env.hasIF) ->
              let e2' =
                match e1.Type with
                  | C.Ptr t -> trForWrite env t e2
                  | _ -> die()
              let e1' = trExpr env e1
              let write_call_args =
                if vcc3 then
                  (List.rev (decomposeDot e1')) @ [e2']
                else [e1'; e2']
              [cmt (); 
               B.Stmt.Call (C.bogusToken, [], "$write_int", write_call_args); 
               assumeSync env e1.Token]
            | C.Expr.MemoryWrite (_, e1, e2) when env.hasIF ->
              let e2' =
                match e1.Type with
                  | C.Ptr t -> trForWrite env t e2
                  | _ -> die()
              let memLoc = trExpr env e1
              let asPointer =
                match e1.Type with
                  | C.Type.PhysPtr (C.Type.PhysPtr _ | C.Type.SpecPtr _) -> true
                  | _ -> false
              let secLabel = IF.contextify (IF.exprLevel asPointer e2)
              let secLabelExpr = IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) secLabel
              [cmt ();
               B.Stmt.Call (C.bogusToken, [], "$write_int", [memLoc; e2']); 
               assumeSync env e1.Token
               IF.setPLabel C.bogusToken (memLoc) (secLabelExpr);
               assumeSync env e1.Token
               IF.setPMeta C.bogusToken (memLoc) (IF.getPC);
               assumeSync env e1.Token]
            | C.Expr.VarWrite (_, [v], C.Expr.Macro (c, "claim", args)) ->
              cmt() :: trClaim env false c.Token v args
              
            | C.Expr.VarWrite (_, [v], C.Expr.Macro (c, "upgrade_claim", args)) ->
              cmt() :: trClaim env true c.Token v args
              
            | C.Expr.Stmt (_, C.Expr.Macro (c, "unclaim", args)) ->
              cmt() :: trUnclaim env c.Token args
            
            | C.Expr.Atomic (ec, objs, body) ->
              captureState "" ec.Token :: trAtomic trStmt env ec objs body
              
            | C.Expr.VarWrite (_, vs, C.Expr.Call (c, fn, targs, args)) -> 
              doCall c vs (Some fn) fn.Name targs args @ List.map (fun v -> ctx.AssumeLocalIs c.Token v) vs
              
            | C.Expr.Stmt (_, C.Expr.Call (c, fn, targs, args))        -> 
              doCall c [] (Some fn) fn.Name targs args
            | C.Expr.Macro (c, (("_vcc_reads_havoc"|"_vcc_havoc_others"|"_vcc_unwrap_check"|"_vcc_set_owns"|
                                  "_vcc_giveup_closed_owner"|"_vcc_set_closed_owner"| 
                                  "_vcc_static_wrap"|"_vcc_static_wrap_non_owns"|"_vcc_static_unwrap") as name), args) -> 
              doCall c [] None name [] args
            | C.Expr.Stmt (_, C.Expr.Macro (c, (("_vcc_unwrap"|"_vcc_wrap"|"_vcc_deep_unwrap"|"_vcc_from_bytes"|"_vcc_to_bytes") as name), args)) ->
              doCall c [] None name [] args         
              
            | C.Expr.VarWrite (c, [v], C.Expr.Macro(c', "_vcc_new_club", [l])) ->
              [cmt ()
               B.Stmt.Assign (varRef v, B.Expr.FunctionCall("$ptrclub.construct", [B.Expr.Ref "$ptrclub.empty"; trExpr env l]))]
            | C.Expr.VarWrite (c, [v], e) when (not env.hasIF) ->
              cmt () ::
              B.Stmt.Assign (varRef v, stripType v.Type (trExpr env e)) ::
              ctx.AssumeLocalIs c.Token v :: []
            | C.Expr.VarWrite (c, [v], e) when env.hasIF ->
              match v.Type with
                | C.Type.Ref s when s.Name.StartsWith("$map_t.") ->
                  cmt () ::
                  B.Stmt.Assign (varRef v, stripType v.Type (trExpr env e)) ::
                  ctx.AssumeLocalIs c.Token v :: []
                | C.Type.Map _ ->
                  cmt () ::
                  B.Stmt.Assign (varRef v, stripType v.Type (trExpr env e)) ::
                  ctx.AssumeLocalIs c.Token v :: []
                | _ ->
                  let (vname,_) = trVar v
                  let asPointer =
                    match v.Type with
                      | C.Type.PhysPtr _ | C.Type.SpecPtr _ -> true
                      | _ -> false
                  let secLabel = IF.contextify (IF.exprLevel asPointer e)
                  cmt () ::
                  B.Stmt.Assign (varRef v, stripType v.Type (trExpr env e)) ::
                  IF.setLLabel ("FlowData#"+vname) (IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) secLabel) ::
                  IF.setLMeta ("FlowData#"+vname) IF.getPC ::
                  ctx.AssumeLocalIs c.Token v :: []
            | C.Expr.If (ec, cl, c, s1, s2) ->
              let prefix,suffix,innerEnv = 
                if (env.hasIF)              
                 then let explicitClassif = ref false;
                      let classifier = match cl with
                                            | None -> B.Expr.FunctionCall("#classifier#default", [])
                                            | Some cl -> explicitClassif := true; trExpr env cl
                      let cl =
                        match cl with
                          | None -> C.Expr.BoolLiteral(ec, false)
                          | Some cl -> cl
                      let tokHighClassif = afmte 9720 "test classifier is low" [cl]
                      let tokHighCondition = afmte 9719 "test condition is as low as specified by the test classifier" [c; cl]
                      let condLevel = IF.secLabelToBoogie (trExpr env) (fun v -> fst(trVar v)) (IF.exprLevel false c)
                      let classifier = if (!explicitClassif) then B.Expr.Lambda(Token.NoToken, ["CLS#ptr",B.Type.Ref "$ptr"], [], B.Expr.FunctionCall("$select.$map_t..$ptr_to..^^void.^^bool", [classifier; B.Expr.Ref "CLS#ptr"]))
                                                             else IF.makePermissiveUpgrade (trExpr env) (fun v -> fst(trVar v)) trType c classifier
                      let getMapElement map index =
                        B.Expr.ArrayIndex(map, [index])
                      let condLevelCheck = B.Stmt.MkAssert(tokHighCondition,
                                                         B.Expr.Forall(Token.NoToken,
                                                                       ["ptr#CLC",B.Type.Ref "$ptr"],
                                                                       [],
                                                                       [],
                                                                       B.Expr.Primitive("==>",
                                                                                        [B.Expr.Primitive("!", [getMapElement classifier (B.Expr.Ref "ptr#CLC")])
                                                                                         B.Expr.Primitive("==", [B.Expr.ArrayIndex(condLevel, [B.Expr.Ref "ptr#CLC"])
                                                                                                                 B.Expr.Ref "$seclbl.bot"])
                                                                                        ])
                                                                      )
                                                        )
                      let env' = newIFContext env stmt
                      let setPC = [B.Stmt.VarDecl((currentPC env', B.Type.Ref "$labelset"), None)
                                   B.Stmt.Assign(B.Expr.Ref (currentPC env'), IF.getPC)
                                   IF.setPC (stmt.Token)
                                            (B.Expr.Lambda(Token.NoToken,
                                                           ["ptr#setPC", B.Type.Ref "$ptr"],
                                                           [],
                                                           B.Expr.Ite(getMapElement classifier (B.Expr.Ref "ptr#setPC"),
                                                                      B.Expr.Ref "$seclbl.top",
                                                                      B.Expr.Ref "$seclbl.bot")))
                                   assumeSync env stmt.Token]
                      let resetPC = [IF.setPC (stmt.Token) (B.Expr.Ref (currentPC env'))
                                     assumeSync env stmt.Token]
                      condLevelCheck :: setPC, resetPC, env'
                else [],[],env
              captureState "" ec.Token ::
              B.Stmt.Comment ("if (" + c.ToString() + ") ...") ::
              prefix @
              [B.Stmt.If (trExpr env c, B.Stmt.Block (trStmt innerEnv s1), B.Stmt.Block (trStmt innerEnv s2))] @
              suffix
            | C.Expr.Loop (comm, invs, writes, variants, s) ->
              let (save, oldState) = saveState "loop"
              let env = { env with OldState = oldState }
              let (bump, wrCheck, env) =
                match writes with
                  | [] -> ([], [], env)
                  | fst :: _ ->
                    let env' = { env with WritesState = oldState }
                    let (init, env') = setWritesTime fst.Token env' writes
                    let name = "#loopWrites^" + (ctx.TokSuffix fst.Token)
                    let p = er name
                    let impl = 
                      if vcc3 then
                        let repl = function
                          | B.FunctionCall ("$top_writable", args) -> Some (bCall "$listed_in_writes" args)
                          | _ -> None
                        bImpl ((objectWritesCheck env' p).Map repl) (objectWritesCheck env p)
                      else
                        bImpl (objectWritesCheck env' p) (objectWritesCheck env p)
                    let tok = afmtet fst.Common.Token 8011 "writes clause of the loop might not be included writes clause of the function" []
                    let bump =  [B.Stmt.Call (tok, [], "$bump_timestamp", []); assumeSync env tok]
                    let check = [B.Stmt.MkAssert (tok, B.Forall (Token.NoToken, [name, tpPtr], [[bCall "$dont_instantiate" [p]]], weight "dont-inst", impl))]
                    (bump, init @ check, env')
              let body =
                B.Stmt.While (bTrue, 
                  List.map (fun (e:C.Expr) -> (e.Token, trExpr env e)) invs,
                  B.Stmt.Block ([B.Stmt.MkAssume (stateChanges env);
                                 B.Stmt.MkAssume (bCall "$timestamp_post" [env.OldState; bState]);
                                 assumeSync env comm.Token] @
                                 List.map (ctx.AssumeLocalIs comm.Token) ctx.SoFarAssignedLocals @
                                 trStmt env s @
                                 [captureState "after loop iter" comm.Token] ))
              bump @ save @ wrCheck @ [body; assumeSync env comm.Token]
                
            | C.Expr.VarDecl (b, v, _) when env.hasIF ->
              if v.Kind = C.Parameter || v.Kind = C.SpecParameter || v.Kind = C.OutParameter then []
              else
                let (v', w) = trWhereVar v
                let vname,_ = v'
                cmt() ::
                B.Stmt.VarDecl (v', w) ::
                ctx.AssumeLocalIs b.Token v ::
                B.Stmt.VarDecl (("FlowData#"+vname,B.Type.Ref "$flowdata"),None) ::
                IF.setLLabel ("FlowData#"+vname) (B.Expr.Ref "$lblset.top") ::
                IF.setLMeta ("FlowData#"+vname) (B.Expr.Ref "$lblset.bot") :: []
            | C.Expr.VarDecl (b, v, _) when (not env.hasIF) ->
              if v.Kind = C.Parameter || v.Kind = C.SpecParameter || v.Kind = C.OutParameter then []
              else
                let (v', w) = trWhereVar v
                cmt() ::
                B.Stmt.VarDecl (v', w) ::
                ctx.AssumeLocalIs b.Token v :: []
            | C.Expr.Goto (c, l) when (not env.hasIF) -> [cmt (); B.Stmt.Goto (c.Token, [trLabel l])]
            | C.Expr.Goto (c,l) when (env.hasIF) ->
              let curPC = currentPC env
              let targetPC = snd(List.find (fun (lbls,_) -> List.exists (fun lbl -> lbl = l) lbls) env.IFContexts)
              if curPC = targetPC then [cmt (); B.Stmt.Goto (c.Token, [trLabel l])]
                                  else [cmt()
                                        B.Stmt.Block [B.Stmt.MkAssert (afmte 9716 "the target label's context is at least as high as the jump's" [stmt],
                                                                     B.Expr.FunctionCall("$lblset.leq",
                                                                                         [IF.getPC
                                                                                          IF.getLabel (B.Expr.Ref(targetPC))]))
                                                      B.Stmt.Goto (c.Token, [trLabel l])]]
            | C.Expr.Label (c, l) -> [B.Stmt.Label (c.Token, trLabel l)]
            
            | C.Expr.Macro (_, "ignore_me", []) -> []
            | C.Expr.Macro (_, "inlined_atomic", [C.Expr.Macro (_, "ignore_me", [])]) -> []

            | e when not (hasSideEffect e) -> []
            
            | _ -> 
              helper.Oops (stmt.Token, "unhandled stmt " + stmt.ToString())
              []
          with
            | Failure _ ->
              helper.Error(stmt.Token, 9600, "OOPS for statement " + stmt.ToString())
              reraise()

      let trHeader (header:C.Function) =
        let env = { initialEnv with Writes = header.Writes }
        let te e = trExpr env e
        let pureEq =
          if header.IsPure && header.RetType <> C.Void then 
            let parms = 
              (if header.IsStateless then [] else [bState]) @ [for tv in header.TypeParameters -> typeVarRef tv] 
                                                            @ [for v in header.InParameters -> varRef v]
            let tok = TransUtil.afmtt header.Token 8022 "the pure function '{0}' is underspecified; please supply ensures(result == ...) contract matching the implementation" [header.Name]
            [B.FreeEnsures (bEq (er "$result") (bCall ("F#" + header.Name) parms))]
          else []
        let (writes, ensures) =
          let check (writes, ensures) = function
            | C.Call (_, { Name = "_vcc_public_writes" }, _, [s]) ->
              (s :: writes, ensures)
            | e -> (writes, e :: ensures)
          match List.fold check ([], []) header.Ensures with
            | ([], e) -> (header.Writes, e)
            | acc -> acc
        let ensures = List.rev ensures
        let stateCondition =
          if header.IsPure then
            if writes <> [] then die() // should have been caught earlier!
            []
          else
            [B.FreeEnsures (stateChanges { env with Writes = writes });                    
             B.Modifies "$s";
             B.Modifies "$cev_pc";
             ]
        let tEnsures = function
          | C.Macro(_, "free_ensures", [e]) -> B.FreeEnsures(te e)
          | e -> B.Ensures(e.Token, te e)
        let rec splitConjuncts = function
          | C.Prim(_, C.Op("&&", _), [e1;e2]) -> splitConjuncts e1 @ splitConjuncts e2
          | e -> [e]
        let proc =
          let outPars, inPars = List.map trVar (header.OutParameters), List.map trTypeVar header.TypeParameters @ List.map trVar (header.InParameters)
          let outPars = if header.RetType = C.Type.Void then outPars else ("$result", trType header.RetType) :: outPars
          { Name      = header.Name
            InParms   = inPars
            OutParms  = outPars
            Locals    = []
            Body      = None
            Contracts = 
              [for e in header.Requires |> List.map splitConjuncts |> List.concat -> B.Requires (e.Token, te e)] @
              [for e in ensures -> tEnsures e] @
              pureEq @ stateCondition @
              [B.FreeEnsures (bCall "$call_transition" [bOld bState; bState])]
            Attributes = 
              [ for attr in header.CustomAttr do
                  match attr with
                    | C.IntBoogieAttr (key, value) -> yield (B.ExprAttr (key, bInt value))
                    | C.BoolBoogieAttr (key, value) -> yield (B.ExprAttr (key, B.Expr.BoolLiteral value))
                    | C.VccAttr(C.AttrSkipVerification, _) -> yield (B.ExprAttr ("verify", bFalse))
                    | C.VccAttr ("extra_options", o) -> yield (B.StringAttr ("vcc_extra_options", o))
                    | C.VccAttr (C.AttrBvLemmaCheck, o) -> yield (B.StringAttr ("vcc_bv_lemma_check", o))
                    | C.VccAttr _ -> yield! []
                    | C.ReadsCheck _ -> yield! []
                     
              ] } : B.ProcData
        (proc, env)

      let toFieldRef (f:C.Field) =
        let fieldRef = er (fieldName f)
        fieldRef
        
      let toBaseType (f:C.Field) =
        let baset = 
          match f.Type with
            | C.Array (t, _) -> t
            | t -> t
        baset
          
      let trField3 (td:C.TypeDecl) (f:C.Field) =
        xassert (td.Kind <> C.Union)
        if TransUtil.hasCustomAttr "as_array" f.CustomAttr then
          failwith "as_array fields not supported yet in vcc3"
        let tdname = er ("^" + td.Name)
        let def =
          [B.Decl.Const ({ Name = fieldName f
                           Type = B.Type.Ref "$field"
                           Unique = true } : B.ConstData)]
        let args = [tdname; toFieldRef f; toTypeId (toBaseType f); bBool f.IsVolatile]
        let args = if f.IsSpec then args else args @ [bInt f.ByteOffset]
        let axs =
          match f.Type with
            | C.Array (_, sz) ->              
              [B.Decl.Axiom (bCall (if f.IsSpec then "$def_ghost_arr_field" else "$def_phys_arr_field") (args @ [bInt sz]))]
            | C.Type.Ref (td) when td.Name.Contains "##" && f.ByteOffset = 0 ->
              let args = args |> Seq.take 3 |> Seq.toList
              [B.Decl.Axiom (bCall "$def_group" args)]
            | _ -> 
              [B.Decl.Axiom (bCall (if f.IsSpec then "$def_ghost_field" else "$def_phys_field") args)]
        def @ axs
      
      let trCompositeExtent (td:C.TypeDecl) =
        xassert (not td.IsUnion)
        let we = er ("^" + td.Name)
        let s = er "s"
        let forallRM id trig body = B.Expr.Forall (Token.NoToken, [("p", tpPtr); ("q", tpPtr); ("s", tpState)], trig, weight id, body)
        let eq = bEq (er "q")
        let oneField (f:C.Field) =
          if f.Type.IsComposite then
            let dot = bCall "$dot" [er "p"; er (fieldName f)]
            match f.Type with
              | C.Type.Array (_, sz) ->
                bCall "$in_composite_array" [er "q"; dot; bInt sz]
              | _ -> eq dot
          else bFalse
        let eqs = td.Fields |> List.map oneField
        let eqs = (eq (er "p")) :: eqs
        let inExt = bCall "$in" [er "q"; bCall "$composite_extent" [er "s"; er "p"; er ("^" + td.Name)]]
        let body = bEq inExt (bMultiOr eqs)          
        forallRM "composite-extent-def" [[inExt]] body        

      let trField (td:C.TypeDecl) (f:C.Field) =
        let tok = td.Token
        let we = er ("^" + td.Name)
        let isUnion = td.Kind = C.Union
        let isComp (f:C.Field) = f.Type.IsComposite
        let s = er "#s"
        let useIs = true
        let (p, pv) =
          if useIs then er "#p", ("#p", tpPtr)
          else bCall "$ptr" [we; er "#r"], ("#r", B.Type.Int)
        let weTyped = bCall "$typed2" [s; p; we] 
        
        let forallRM id trig body = B.Expr.Forall (Token.NoToken, [pv; ("#s", tpState)], trig, weight id, body)              
        let fieldRef = toFieldRef f
        let baset = toBaseType f
        let dot = bCall "$dot" [p; fieldRef]
        let dott = toTypeId baset
        let ptrref = 
          if f.IsSpec then bCall "$ghost_ref" [p; fieldRef]            
          else           
            if f.Type.IsComposite || td.GenerateFieldOffsetAxioms || helper.Options.GenerateFieldOffsetAxioms then
              B.Primitive ("+", [bCall "$ref" [p]; bInt f.ByteOffset])
            else
              bCall "$physical_ref" [p; fieldRef]
        let dotdef = bAnd (bEq dot (bCall "$ptr" [dott; ptrref])) (bCall "$extent_hint" [dot; p])
        let dotdef = if useIs then bInvImpl (bCall "$is" [p; we]) dotdef else dotdef
        let dotdef = B.Forall (Token.NoToken, [pv], [[dot]], weight "def-field-dot", dotdef)
        // ghost fields we treat alike regardless if in union or struct
        let isUnion = if f.IsSpec then false else isUnion          
        
        let fieldoff = bEq (bCall "$field_offset" [fieldRef]) (bInt f.ByteOffset)
        
        let isActive = bEq (bCall "$active_option" [s; p]) fieldRef
        let noInline = if TransUtil.hasCustomAttr "as_array" f.CustomAttr then "no_inline_" else ""
        let emb =
          match f.Type with
            | C.Array (_, sz) -> 
              bCall ("$" + noInline + "array_field_properties") [fieldRef; dott; bInt sz; bBool isUnion; bBool f.IsVolatile]
            | _ ->
              let tsOfDot = bCall "$ts" [s; dot]
              let statusOfDot = bCall "$st" [s; dot]
              let emb = bCall "$field_properties" [s; p; fieldRef; dott; bBool f.IsVolatile]
              let triggers = [ (* [dot; bCall "$typed" [s; p]]; *) [tsOfDot]; [statusOfDot]]
              let emb =
                if isUnion then bInvImpl (bAnd weTyped isActive) emb
                else            bInvImpl weTyped emb
              forallRM "def-field-typed" triggers emb
        
        (*        
        let maybeArrayProp name =
          let whostyped = if isUnion then (bAnd weTyped isActive) else weTyped
          let dname = "$" + name
          let res =
            match f.Type with
              | C.Array (_, sz) -> bCall ("$is_" + name + "_array") [s; dot; dott; bInt sz]
              | _ -> bCall dname [s; dot]
          bImpl (bAnd whostyped (bCall dname [s; p])) res
            
        let mutableTrans =
          forallRM "def-field-trans" [if f.Type = baset then [bCall "$st" [s; dot]]
                                      else bCall "$typed" [s; p] :: (if useIs then [bCall "$is" [p; we]] else [])]
                                     (if f.IsVolatile then maybeArrayProp "mutable"
                                      else bAnd (maybeArrayProp "mutable") (maybeArrayProp "thread_local"))
        *)
        
        let unionAxioms () =
          [B.Decl.Axiom (bCall "$is_union_field" [we; fieldRef])]
        
        let primVolatile() =
          B.Decl.Axiom (bCall "$static_field_properties" [fieldRef; we]) ::
            match f.Type with
              | C.Array (t, sz) when not t.IsComposite ->
                if TransUtil.hasCustomAttr "as_array" f.CustomAttr then
                  []
                elif f.IsVolatile then
                  [B.Decl.Axiom (bCall "$is_primitive_embedded_volatile_array" [fieldRef; bInt sz; dott])]
                else
                  [B.Decl.Axiom (bCall "$is_primitive_embedded_array" [fieldRef; bInt sz])]
              | t when t.IsComposite -> [] 
              | _ when f.IsVolatile ->
                [B.Decl.Axiom (bCall "$is_primitive_volatile_field" [fieldRef])]
              | _ ->
                [B.Decl.Axiom (bCall "$is_primitive_non_volatile_field" [fieldRef])]
           
        let arraySize() =
            match f.Type with
              | C.Array (t, sz) ->
                [B.Decl.Axiom (bEq (bCall "$embedded_array_size" [fieldRef; toTypeId t]) (bInt sz))]
              | _ -> []
          
        let fldOffsetAxioms =
          //if td.GenerateFieldOffsetAxioms || helper.Options.GenerateFieldOffsetAxioms then
            (if f.IsSpec then [] else [B.Decl.Axiom fieldoff]) @ [B.Decl.Axiom dotdef]
          //else []

        [B.Decl.Const ({ Name = fieldName f
                         Type = B.Type.Ref "$field"
                         Unique = true } : B.ConstData)] @
        primVolatile() @
        arraySize() @
        fldOffsetAxioms @
        [B.Decl.Axiom emb] @ 
        //(if f.Type.IsComposite then [] else [B.Decl.Axiom mutableTrans]) @
        (if isUnion then unionAxioms () else []) @
        (if f.Name = "$owns" then [B.Decl.Axiom (bEq (bCall "$owns_set_field" [we]) fieldRef)] else [])
        
        
        
      let trStructEq deep (td:C.TypeDecl) =
        let deepStr = if deep then "deep" else "shallow"
        let vars = [("#p1", tpStruct); ("#p2", tpStruct)]
        let s1 = er "#p1"
        let s2 = er "#p2"
        let idx = er "#i"
        let eqFunName typeName deep = "_vcc_" + deep + "_struct_eq." + typeName
        let eqFun = B.Function(B.Type.Bool, [], eqFunName td.Name deepStr, vars, None)
        let typeRef = toTypeId (C.Type.Ref td)
        let fldEqual inUnion (f : C.Field) =
          let rec read arrayElementType v = 
            let state = bCall "$vs_state" [v]
            let fldAccess = 
              match arrayElementType with
                | None -> fun v f -> bCall "$dot" [bCall "$vs_base" [v; typeRef]; er (fieldName f)]
                | Some t -> fun v f -> bCall "$idx" [ bCall "$dot" [bCall "$vs_base" [v; typeRef]; er (fieldName f)]; idx; toTypeId t]
            function
              | C.Bool -> bCall "$read_bool" [state; fldAccess v f]
              | C.Integer _
              | C.Ptr _ // using $read_ptr() would add type information, but this isn't needed
              | C.Map(_,_) -> bCall "$mem" [state; fldAccess v f]
              | C.Array(t, n) -> 
                match arrayElementType with
                  | Some _ ->
                    helper.Error(f.Token, 9656, "equality for structures with nested arrays not supported", Some(td.Token))
                    die()
                  | None -> read (Some t) v t 
              | _ -> die()
          let read = read None
          match f.Type with // _vcc_deep_struct_eq.S($vs_ctor(#s2, $dot(#p, T.s1)),
            | C.Ref _ when not deep && not inUnion -> None
            | C.Ref td' ->
                let dot v = 
                  bCall "$vs_ctor"
                    [bCall "$vs_state" [v];
                     bCall "$dot" [bCall "$vs_base" [v; typeRef]; er (fieldName f)]]
                let funName = eqFunName td'.Name (if deep then "deep" else "shallow")
                Some(bCall funName [dot s1; dot s2]) 
            | C.Array(t,n) -> 
              let cond = B.Expr.Primitive ("==>", [ bAnd (B.Expr.Primitive("<=", [bInt 0; idx])) (B.Expr.Primitive("<", [idx; bInt n]));
                                                      bEq (read s1 f.Type) (read s2 f.Type) ])
              Some(B.Forall(Token.NoToken, [("#i", B.Int)], [], weight "array-structeq", cond))
            | C.Map(_,_) ->                 
              helper.Warning(f.Token, 9108, "structured type equality treats map equality as map identity")
              Some(bEq (read s1 f.Type) (read s2 f.Type))

            | _ -> Some(bEq (read s1 f.Type) (read s2 f.Type))
        let andOpt e = function 
          | None -> e
          | Some e' -> bAnd e e'
        let eqCall = bCall (eqFunName td.Name deepStr) [s1; s2]
        let eqExpr = 
          match td.Kind with
          | C.TypeKind.Struct -> td.Fields |> List.map (fldEqual false) |> List.fold andOpt bTrue
          | C.TypeKind.Union ->
            let getAO v = bCall "$active_option" [bCall "$vs_state" [v]; bCall "$vs_base" [v; typeRef] ]
            let aoEq = bEq (getAO s1) (getAO s2)
            let fldEqualCond (f : C.Field) =
              match fldEqual true f with
                | Some expr -> B.Primitive("==>", [bEq (getAO s1) (er (fieldName f)); expr])
                | None -> die()
            td.Fields |> List.map fldEqualCond |> List.fold bAnd aoEq
            //activeOptionEq = 
            //bEq (bCall "$active_option" [s; p]) fieldRef
          | _ -> die()
        let eqForall = B.Forall(Token.NoToken, vars, [[eqCall]], weight "eqdef-structeq", bEq eqCall eqExpr)
        [eqFun; B.Axiom eqForall]


      let imax x y = if x < y then y else x
      let imin x y = if x < y then x else y
      
      let typeNesting types =
        let withNest = List.map (fun td -> (td, ctx.TypeDepth (C.Type.Ref td))) types  
        let withNest = List.sortWith (fun (_, x) -> fun (_, y) -> x - y) withNest
        let nestings = gdict()
        let isNested = gdict()
        for (td, n) in withNest do
          if n = 1 then
            isNested.Add((td, td), (0, 0))
            nestings.Add(td, [0, td])
          else
            let lst = ref [0, td]
            for f in td.Fields do
              match f.Type with
                | C.Type.Ref ({Kind = C.Union|C.Struct} as td') ->
                  lst := List.map (fun (l, t) -> (l + 1, t)) nestings.[td'] @ !lst
                | _ -> ()
            nestings.Add(td, !lst)
            for (l, td') in !lst do
              match isNested.TryGetValue ((td', td)) with
                | true, (x, y) ->
                  isNested.[(td', td)] <- (imin l x, imax l y)
                | _ -> isNested.[(td', td)] <- (l, l)
        
        let aux res (td, n) =
          let name td = toTypeId (C.Type.Ref td)
          let aux' res (td', n') =
            let expr =
              match isNested.TryGetValue ((td', td)) with
                | true, (x, y) when td' <> td -> bCall "$is_nested_range" [name td'; name td; bInt x; bInt y]
                | _ -> bNot (bCall "$is_nested" [name td'; name td])
            B.Decl.Axiom expr :: res
          let selfLev = B.Decl.Axiom (bEq (bCall "$nesting_level" [name td]) (bInt n))
          selfLev :: List.fold aux' res withNest
        List.fold aux [] withNest
        
        // The code below tries to optimize the size of the encoding of the tree.
        // For a sample HV source (ca 300 types, 150 second or higher level, 150 types with single or
        // no nesting), it reduces from 90K axioms to 30K (just levels) down to 15k (taking single
        // or no nesting into account). There is however no evidence that it would help, those is_nested(...)
        // terms shouldn't polute the search space in any way.
        (*
        let parents = gdict()
        for td in types do
          for td' in nestings.[td] do
            if td <> td' then
              match parents.TryGetValue td' with
                | true, Some x when x = td -> ()
                | false, _ -> parents.[td'] <- Some td
                | true, Some _ -> parents.[td'] <- None
                | _ -> ()
        
        
        let aux res (td, n) =
          let name td = toTypeId (C.Type.Ref td)
          let aux' res (td', n') =
            if n' >= n then res
            else
              match parents.TryGetValue td' with
                | false, _
                | true, Some _ -> res
                | true, None ->                
                  let expr = bEq (bCall "$is_nested" [name td'; name td]) (bBool (isNested.ContainsKey (td', td)))
                  B.Decl.Axiom expr :: res
          let selfLev = B.Decl.Axiom (bEq (bCall "$nesting_level" [name td]) (bInt n))
          let selfDesc =
            match parents.TryGetValue td with
              | false, _ -> bCall "$no_known_nesting" [name td]
              | true, Some td' -> bCall "$single_known_nesting" [name td; name td']
              | true, None -> bCall "$is_known_type" [name td]
          selfLev :: B.Decl.Axiom selfDesc :: List.fold aux' res withNest
        List.fold aux [] withNest
        *)
        
        

      let trCompositeType (td:C.TypeDecl) =
        let tok = td.Token
        let we = er ("^" + td.Name)
        let isUnion = td.Kind = C.Union
        let isComp (f:C.Field) =
          match f.Type with
            | C.Array _ when TransUtil.hasCustomAttr "as_array" f.CustomAttr -> true
            | _ -> f.Type.IsComposite

        let p = er "#p"
        let q = er "#q"
        let r = er "#r"
        let s = er "#s"
        
        let substOld l (e:B.Expr) =
          let rec outsideOld = function
            | B.Expr.Old e -> Some (e.Map insideOld)
            | _ -> None
          and insideOld = function
            | B.Expr.Ref n -> _try_assoc n l
            | _ -> None          
          e.Map outsideOld

        let s1 = er "#s1"
        let s2 = er "#s2"
        let s1s2p = [("#s1", tpState); ("#s2", tpState); ("#p", tpPtr)]
        let s2r = [("#s2", tpState); ("#r", B.Type.Int)]
        let s1s2pwe = [s1; s2; p; we];
        
        let is_claimable = ref false
        let owns_set_is_volatile = ref false
          
        List.iter (function C.VccAttr ("claimable", _) -> is_claimable := true
                          | C.VccAttr ("volatile_owns", _) -> owns_set_is_volatile := true
                          | _ -> ()) td.CustomAttr
                                                        
        let stripLabel = function
          | C.Macro(_, "labeled_invariant", [_; i]) -> i                  
          | i -> i
        let gatherByLabel invs = 
          let dict = new Dict<_,_>()
          let add = 
            let add' lbl i =
              match dict.TryGetValue(lbl) with
                | true, invs -> dict.[lbl] <- i::invs
                | false, _ -> dict.[lbl] <- [i]
            function
              | C.Macro(_, "labeled_invariant", [C.Macro(_, lbl, []); i]) -> add' lbl i
              | i -> add' "public" i
          List.iter add invs
          [ for kv in dict -> (kv.Key, List.rev kv.Value) ]
          
        let removeTrivialEqualities (bExpr : B.Expr) =
          let rec rte = function
            | B.Primitive("==", [be1; be2]) when be1 = be2 -> Some(bTrue)
            | B.Primitive("!=", [be1; be2]) when be1 = be2 -> Some(bFalse)
            | B.Primitive("||", [be1; be2]) -> Some(bOr (be1.Map(rte)) (be2.Map(rte)))
            | B.Primitive("&&", [be1; be2]) -> Some(bAnd (be1.Map(rte)) (be2.Map(rte)))
            | B.Primitive("==>", [be1; be2]) -> Some(bImpl (be1.Map(rte)) (be2.Map(rte)))
            | _ -> None
          bExpr.Map(rte)
            
        let doInv e = 
          e |> 
            trExpr initialEnv |>
            substOld [("$s", s1)] |>
            bSubst [("$_this", p); ("$s", s2)]
        let inv exprs = bMultiAnd ((if vcc3 then bTrue else bCall "$typed" [s2; p]) :: (exprs |> List.map doInv) ) 
        let invcall = bCall "$inv2" s1s2pwe
        let typedPtr = bCall "$ptr" [we; r]
        let invlabcall lbl = bCall "$inv_lab" [s2; typedPtr; er (ctx.TrInvLabel lbl)]
        let normalizeLabeledInvariant = bSubst [("#s1", er "#s2"); ("#p", typedPtr)] >> removeTrivialEqualities
        let invlab (lbl,exprs) = 
          let invcall = invlabcall lbl
          B.Forall(Token.NoToken, s2r, [[invcall]], weight "eqdef-inv", (bEq (invcall) (inv exprs |> normalizeLabeledInvariant)))
        let inv = B.Forall (Token.NoToken, s1s2p, [[invcall]], weight "eqdef-inv", (bEq invcall (inv (td.Invariants |> List.map stripLabel))))
        let labeledInvs = td.Invariants |> gatherByLabel |> List.map invlab |> List.map (fun e -> B.Axiom(e))
        
        let extentCall extentName meta union1 (fields:list<C.Field>) =
          let auxPtr r =
            bCall "$ptr" [we; r]
          let auxDot r f =
            bCall "$dot" [auxPtr r; toFieldRef f] 
          let auxTyped r f =
            bCall "$typed" [s; auxDot r f]
          let typedCond r (f:C.Field) = 
            if meta && union1 && not f.IsSpec then
              bEq (bCall "$active_option" [s; auxPtr r]) (toFieldRef f)
            else bTrue
          let qvars = (if meta then [("#s", tpState)] else []) @ [("#q", tpPtr); ("#r", B.Type.Int)]
          let args = (if meta then [s] else []) @ [q; auxPtr r]
          let bExtentCall = bCall extentName args
          let inExtent (fld:C.Field) dot =
            let common = (if meta then [s] else []) @ [q; dot]
            match fld.Type with
              | C.Array (t, sz) -> 
                let add = [toTypeId t; bInt sz]
                if TransUtil.hasCustomAttr "as_array" fld.CustomAttr then
                  let args = (if meta then [s] else []) @ [q; bCall "$as_array" (dot :: add)]
                  bCall extentName args
                else
                  if t.IsComposite then bCall (extentName.Replace ("$in_", "$in_array_")) (common @ add)
                  else bCall "$in_array" ([q; dot] @ add)
              | t ->            
                if t.IsComposite then bCall extentName common
                else bEq q dot
          let isAField = bMultiOr (List.map (function fld -> bAnd (inExtent fld (auxDot r fld)) (typedCond r fld)) fields)
          let body = bOr (bEq q (auxPtr r)) isAField
          B.Forall (Token.NoToken, qvars, [[bExtentCall]], weight ("eqdef-extent-" + extentName.Replace ("$", "")), bEq bExtentCall body)

        let spansCalls (fields:list<C.Field>) =
          let maybeArrayLift r f prop =
            let dot = bCall "$dot" [r; toFieldRef f]
            match f.Type with
              | C.Type.Array (t, sz) ->
                let idx = bCall "$idx" [dot; er "#i"; toTypeId t]
                B.Forall (Token.NoToken, [("#i", B.Type.Int)], [[idx]], weight "array-span",
                       bInvImpl (bCall "$in_range" [bInt 0; er "#i"; bInt (sz - 1)]) (prop idx))
              | _ -> prop dot

          let auxDot r f =
            bCall "$dot" [r; toFieldRef f] 
          let qvars = [("#p", tpPtr); ("#s1", tpState); ("#s2", tpState)]
          let args = [s1; s2; p; we]
          let bSpansCall = bCall "$state_spans_the_same" args
          let bNonVolatileSpansCall = bCall "$state_nonvolatile_spans_the_same" args
          let mkForall call fields =
            B.Forall (Token.NoToken, qvars, [[call]], weight "eqdef-span", bEq call 
              (bMultiAnd (List.map (function fld -> maybeArrayLift p fld (fun idx -> bCall "$mem_eq" [s1; s2; idx])) fields)))
          (mkForall bSpansCall fields, mkForall bNonVolatileSpansCall (List.filter (fun fld -> not fld.IsVolatile) fields))
        
        let extentProp propName twostate union1 includeSelf primFieldProp (fields:list<C.Field>) =
          let auxPtr r =
            bCall "$ptr" [we; r]
          let auxDot r f =
            bCall "$dot" [auxPtr r; toFieldRef f] 
          let typedCond r (f:C.Field) = 
            if union1 && not f.IsSpec then
              bEq (bCall "$active_option" [(if twostate then (er "#s2") else er "#s1"); auxPtr r]) (toFieldRef f)
            else bTrue
          let qvars = ("#s1", tpState) :: (if twostate then [("#s2", tpState)] else []) @ [("#r", B.Type.Int)]
          let states = er "#s1" :: (if twostate then [er "#s2"] else []) 
          let prop p = bCall ("$extent_" + propName) (states @ [p])
          let hasProp (fld:C.Field) dot =
            match fld.Type with
              | C.Array (t, sz) ->
                if TransUtil.hasCustomAttr "as_array" fld.CustomAttr then
                  prop (bCall "$as_array" [dot; toTypeId t; bInt sz])
                else 
                  let idx = bCall "$idx" [dot; er "#i"; toTypeId t]
                  let fieldProp = if t.IsComposite then prop else primFieldProp
                  B.Forall (Token.NoToken, [("#i", B.Type.Int)], 
                         //[[bCall ("$" + propName) (states @ [idx])]],
                         [[idx]], weight "array-extentprop",
                         bInvImpl (bCall "$in_range" [bInt 0; er "#i"; bInt (sz - 1)]) (fieldProp idx))
              | t -> if t.IsComposite then prop dot else primFieldProp dot
              
          let allHaveProp = bMultiAnd (List.map (function fld -> bInvImpl (typedCond r fld) (hasProp fld (auxDot r fld))) fields)
          let body = if includeSelf then bAnd (bCall ("$" + propName) (states @ [auxPtr r])) allHaveProp else allHaveProp
          let bExtentCall = prop (auxPtr r)
          B.Forall (Token.NoToken, qvars, [[bExtentCall]], weight "eqdef-extentprop", bEq bExtentCall body)
        
        let allFields = 
          match td.Fields with
            | x when vcc3 -> x
            | [] -> []
            | lst -> ({ Name = "$owns" 
                        Token = td.Token
                        Type = C.Type.PtrSet
                        Parent = td 
                        IsSpec = true 
                        IsVolatile = !owns_set_is_volatile
                        Offset = C.FieldOffset.Normal 0 
                        CustomAttr = []
                        UniqueId = C.unique() } : C.Field) :: lst
        let primFields = List.filter (function fld -> not (isComp (fld))) allFields
        let in_full_extent_of = extentCall "$in_full_extent_of" false false allFields
        
        let in_extent_of = 
          if nestingExtents then
            let q = bCall "$ptr" [we; er "#r"]
            let in_ext = bCall "$in_extent_of" [er "#s"; er "#p"; q] 
            let depth = ctx.TypeDepth (C.Type.Ref td)
            let rec is_emb acc lev emb in_range =
              if lev >= depth then acc
              else
                let emb = bCall "$emb" [er "#s"; emb]
                let here =
                  if in_range then bCall "$is_emb_at_lev" [er "#p"; emb; q; bInt lev]
                  else bEq q emb
                is_emb (bOr acc here) (lev + 1) emb in_range
            let is_emb = is_emb (bEq (er "#p") q) 1 (er "#p")
            let quant in_range = 
              B.Forall (Token.NoToken, ["#s", tpState; "#p", tpPtr; "#r", B.Int], 
                        [[in_ext]], weight "eqdef-extent",
                        if in_range then bImpl in_ext (is_emb true)
                        else bImpl (is_emb false) in_ext)
            bAnd (quant true) (quant false)
          else
            if isUnion then
              extentCall "$in_extent_of" true true allFields
            else
              let def = extentCall "$in_extent_of" true false allFields
              if noUnions (C.Type.Ref td) then
                match def with
                  | B.Forall (tok, vars, [[extent]], attrs, body) ->
                    let us = bCall "$ptr" [we; er "#r"]
                    let d1 = B.Forall (tok, vars, [[extent]], attrs, bEq extent (bCall "$in_struct_extent_of" [er "#q"; us]))
                    if ctx.TypeDepth (C.Type.Ref td) = 2 then
                      let consq = bOr (bEq (er "#q") us) (bEq (bCall "$emb" [er "#s"; er "#q"]) us)
                      bAnd d1 (B.Forall (tok, vars, [[extent]], attrs, bInvImpl (bCall "$typed" [er "#s"; us]) (bEq extent consq)))
                    else d1
                  | _ -> die()
              else def        
        
        let in_span_of = extentCall "$in_span_of" false false primFields   
        let ignorePrimField _ = bTrue     
        let readAnyIsZero field = bEq (bCall "$mem" [ er "#s1"; field ]) (bInt 0)
        let extentProps = List.map B.Decl.Axiom
                              [extentProp "mutable" false isUnion true ignorePrimField allFields;
                               extentProp "is_fresh" true isUnion true ignorePrimField allFields;
                               extentProp "zero" false isUnion false readAnyIsZero td.Fields ]
        
        let (state_spans_the_same, state_nonvolatile_spans_the_same) = spansCalls primFields
        
        let firstOptionTyped =
          let ptr = bCall "$ptr" [we; r]
          let firstOptionTypedCall = bCall "$first_option_typed" [s; ptr]
          let firstOptionTypedAxiom f =
            B.Decl.Axiom (B.Expr.Forall (Token.NoToken, [("#r", B.Type.Int); ("#s", tpState)], 
                                         [[firstOptionTypedCall]], 
                                         weight "typedef-union_active",
                                         bInvImpl firstOptionTypedCall (bCall "$union_active" [s; ptr; toFieldRef f])))
          match List.tryFind (fun (f:C.Field) -> not f.IsSpec) allFields with
            | Some f when isUnion -> [firstOptionTypedAxiom f]
            | _ -> []
        
        let forward =
          [bDeclUnique tpCtype ("^" + td.Name)] @ firstOptionTyped
                          
        let forward =
          if vcc3 then
            let defAx = bCall "$def_composite_type" [we; bInt td.SizeOf; B.Expr.BoolLiteral !is_claimable; B.Expr.BoolLiteral !owns_set_is_volatile]
            forward @ [B.Decl.Axiom defAx]
          else
            forward @ 
              [B.Decl.Axiom (bCall "$is_composite" [we]);
               B.Decl.Axiom (bEq (bCall "$ptr_level" [we]) (bInt 0))]
           
        let volatile_owns = B.Decl.Axiom (bEq (bCall "$has_volatile_owns_set" [we]) (B.Expr.BoolLiteral !owns_set_is_volatile))
        let claimable = B.Decl.Axiom (bEq (bCall "$is_claimable" [we]) (B.Expr.BoolLiteral !is_claimable))
           
        let forward = 
          match td.GenerateEquality with
          | C.StructEqualityKind.NoEq -> forward
          | C.StructEqualityKind.ShallowEq -> (trStructEq false td) @ forward
          | C.StructEqualityKind.DeepEq -> (trStructEq true td) @ (trStructEq false td) @ forward
        match td.Fields with
          //| [] -> forward
          | _ when vcc3 ->
            forward @ 
               [ B.Decl.Axiom inv; B.Decl.Axiom (trCompositeExtent td) ] 
                 @ List.concat (List.map (trField3 td) allFields)
          | _ -> 
            forward @ 
              [B.Decl.Axiom (bEq (bCall "$sizeof" [we]) (bInt td.SizeOf));
               B.Decl.Axiom inv ] @ labeledInvs @
                [B.Decl.Axiom in_full_extent_of;
                 B.Decl.Axiom in_extent_of;
                 B.Decl.Axiom in_span_of;
                 B.Decl.Axiom state_spans_the_same;
                 B.Decl.Axiom state_nonvolatile_spans_the_same;
                 claimable;
                 volatile_owns
                 ] @ extentProps 
                   @ List.concat (List.map (trField td) allFields)
      
      let trRecord (td:C.TypeDecl) =
        let intKind = function
          | C.Type.Integer _ as t -> toTypeId t
          | _ -> er "^^mathint"
        let trRecField (f:C.Field) =
          [B.Decl.Const { Unique = true; Name = fieldName f; Type = B.Type.Ref "$field" };
           B.Decl.Axiom (bCall "$is_record_field" [toTypeId (C.Type.Ref td); er (fieldName f); toTypeId f.Type]);
           B.Decl.Axiom (bEq (bCall "$record_field_int_kind" [er (fieldName f)]) (intKind f.Type))]
        List.map trRecField td.Fields |> List.concat
        
      let trMathType (td:C.TypeDecl) =
        match td.Name with
          | "state_t"
          | "thread_id_t"
          | "tptr"
          | "typeid_t"
          | "ptrset" -> []
          | origname ->
            let name = "$#" + origname
            let t = B.Type.Ref name
            let typeDef = [B.Decl.TypeDef name]
            let (additions, kind) = 
              match td.Kind with
                | C.FunctDecl _ -> (typeDef, "fnptr")
                | C.Record -> (trRecord td, "record")
                | _ -> (typeDef, "math")
            let def = if vcc3 then "$def_" else "$is_"
                        
            [B.Decl.Const { Unique = true
                            Name = "^" + name
                            Type = tpCtype };           
             B.Decl.Axiom (bCall (def + kind + "_type") [er ("^" + name)])
             ] @ additions

      let trPureFunction (h:C.Function) =
        if not h.IsPure || h.RetType = C.Void then []
        else
          let parameters =  List.map trTypeVar h.TypeParameters @ List.map trVar h.InParameters
          let qargs = (if h.IsStateless then [] else [("#s", tpState)]) @ parameters
          let fForPureContext (t, suffix) =
            let suffix = if suffix = "" then "" else "#" + suffix
            let fname = "F#" + h.Name + suffix
            let retType = trType t
            let fappl = bCall fname (List.map (fun ((vi,vt):B.Var) -> er vi) qargs)
            let fdecl = B.Decl.Function (retType, [], fname, qargs, None)
            (fappl, fdecl)
          let env = { initialEnv with Writes = h.Writes }
          let te e = trExpr env e
          // it is unsound to include these in the axiom
          let tens = function
            | C.Macro (_, ("in_range_phys_ptr"|"in_range_spec_ptr"), [_]) -> bTrue
            | e -> trExpr env e
          let ensures = bMultiAnd (List.map (stripFreeFromEnsures >> tens) h.Ensures)
          let requires = bMultiAnd (List.map te h.Requires)
          let fname = "F#" + h.Name
          let retType = trType h.RetType
          (*
          match h.Ensures with
            // note that it is in general unsafe to strip casts from "result", e.g. this:
            //   ispure bool bar() ensures(result == 7);
            // which generates (int)result == 7, might lead to inconsistency (because we assume result
            // of type bool to be 0 or 1)
            | [C.Prim (_, C.Op ("==", _), [C.Result (_); _])] -> ()
            | lst ->
              helper.Warning (h.Token, 0, "wrong " + String.concat "; " (lst |> List.map (fun e -> e.ToString())))
            *)  
          let (fappls, fdecls) = (h.RetType, "") :: List.map (fun (v:C.Variable) -> (v.Type, "OP#" + v.Name)) h.OutParameters |> List.map fForPureContext |> List.unzip

          let fappl  = fappls.Head
          let subst = bSubst (("$s", er "#s") :: List.zip ("$result" :: List.map (fun (v:C.Variable) -> "OP#" + v.Name) h.OutParameters)  fappls )
          let defBody = subst (bImpl requires ensures)
          if h.IsStateless && bContains "#s" defBody then
            helper.Error (h.Token, 9650, "the specification refers to memory, but function is missing a reads clause", None)
          let defAxiom =
            if (defBody = bTrue) then [] 
            else if qargs = [] then [B.Decl.Axiom defBody]
            else [B.Decl.Axiom (B.Expr.Forall(Token.NoToken, qargs, List.map (fun x -> [x]) fappls, weight "eqdef-userfun", defBody))]
          let fnconst = "cf#" + h.Name
          let defconst = B.Decl.Const { Unique = true; Name = fnconst; Type = B.Type.Ref "$pure_function" }
          let frameAxiom =
            let containsGenerateFrameAxiom = hasCustomAttr C.AttrFrameaxiom h.CustomAttr
            if (not containsGenerateFrameAxiom) || h.IsStateless then 
              []
            else
              let readsRef (e:C.Expr) =
                match e.Type with
                  | C.Ptr t -> 
                    if not t.IsComposite then
                      (bTrue, te (C.Deref ({ e.Common with Type = t }, e)), trType t)
                    else
                      (bCall "$closed" [er "#s"; te e], bCall "$read_version" [er "#s"; te e], tpVersion)
                  | C.MathTypeRef "ptrset" ->
                    match e with 
                      | C.Macro(_, "_vcc_array_range", [_; ptr; length]) -> 
                        (bTrue, bCall "$mem_range" [er "#s"; te ptr; te length], B.Type.Int)
                      | _ -> helper.Error(e.Token, 9619, "ptrset in reads clause must be an array_range")
                             (bTrue, er "$bogus", tpVersion)
                  | _ ->
                    helper.Error (e.Token, 9619, "non-pointer reads clauses are not supported", None)
                    (bTrue, er "$bogus", tpVersion)
              let (conds, refs, types) = List.unzip3 (List.map readsRef h.Reads)
              let pTypes = List.map snd parameters
              let pRefs = List.map (fun ((vi,vt):B.Var) -> er vi) parameters
              let framename = fname + "#frame"
              let framedecl = B.Decl.Function (retType, [], framename, List.map (fun t -> ("", t)) (pTypes @ types), None)
              
              let pre = bMultiAnd (bCall "$full_stop" [er "#s"] :: bCall "$can_use_frame_axiom_of" [er fnconst] :: conds)
              let post = bEq fappl (bCall framename (pRefs @ refs))
              [framedecl; B.Decl.Axiom (B.Expr.Forall(Token.NoToken, qargs, [[fappl]], weight "frameaxiom", subst (bImpl pre post)))]
          let typeInfo =
            let arg i t = B.Decl.Axiom (bCall "$function_arg_type" [er fnconst; bInt i; toTypeId t])
            arg 0 h.RetType :: (h.Parameters |> List.mapi (fun i v -> arg (i + 1) v.Type))
          fdecls @ [defconst] @ defAxiom @ frameAxiom @ typeInfo

      let sanityChecks env (h:C.Function) =
        // we disable that by default for now, it seems to be too much of a hassle
        if not (hasCustomAttr "postcondition_sanity" h.CustomAttr) then []
        else
          let checks = List.map stripFreeFromEnsures h.Ensures
          match checks with
            | [] -> []
            | lst ->
              let var = "#postcondition_sanity"
              let repl = function
                | B.Expr.Ref "$s" -> Some (er "#sanityState")
                | B.Old _ as expr -> Some expr
                | _ -> None
              let subst (expr:B.Expr) = expr.Map repl
              let mkAssert (e:C.Expr) =
                match e with
                  | C.Macro (_, "reads_check_wf", [a]) -> 
                    match readsCheck env true a |> List.rev with
                      | B.Stmt.Assert (_, t, e) :: _ -> B.Stmt.MkAssert (t, e |> subst)
                      | _ -> die()
                  | _ -> B.Stmt.MkAssert (e.Token, trExpr env e |> subst)
              let assumes = h.Ensures |> List.map (stripFreeFromEnsures>> trExpr env >> subst >> B.Stmt.MkAssume)
              let state = B.Stmt.MkAssume (stateChanges env |> subst)
              let goodstuff = B.Stmt.MkAssume (bCall "$full_stop" [bState] |> subst)
              [B.Stmt.VarDecl ((var, B.Type.Bool), None);
               B.Stmt.VarDecl (("#sanityState", tpState), None);
               B.Stmt.If (er var, B.Stmt.Block (assumes @ state :: goodstuff :: List.map mkAssert lst), B.Stmt.Block []);
               B.Stmt.MkAssume (bNot (er var))]
        
      let hasStartHere (stmt:B.Stmt) =
        let found = ref false
        let repl = function
          | B.Stmt.Assume (_, (B.FunctionCall ("$start_here", []) as call)) ->
            found := true
            Some (B.Stmt.Assume ([B.ExprAttr ("start_checking_here", bTrue)], call))
          | _ -> None
        let newBody = B.mapStmt repl stmt
        if !found then Some newBody
        else None        
      
      let trTop decl =
        try 
          match decl with
            | C.Top.FunctionDecl h ->
              if h.Name.StartsWith "_vcc_" && not (h.Name.StartsWith "_vcc_match") then 
                []
              else
                ctx.NewFunction()
                let (proc, env) = trHeader h
                let (init, env) = setWritesTime h.Token env h.Writes
                let hasIF = IF.scanForIFAnnotations decl
                let env = {env with hasIF = hasIF}
                if hasIF then globalHasIF := true
                let assumeMutability e =
                  let te = trExpr env
                  let e' = te e 
                  let assump =
                    match e.Type with
                      | C.Ptr t ->                      
                        bCall (if t.IsComposite then "$thread_owned" else "$mutable") [bState; e']
                      | C.ObjectT ->
                        bCall "$thread_owned_or_even_mutable" [bState; e']
                      | C.MathTypeRef "ptrset" ->
                        let mut extOf name =
                          let p = er "#p"
                          let triggers =
                            match extOf with
                              | Some q ->
                                [[bCall "$extent_hint" [p; q]]]
                              | _ ->
                                List.map (fun n -> [bCall n [bState; p]]) ["$st"; "$ts"]
                          B.Expr.Forall (Token.NoToken, [("#p", tpPtr)], triggers,
                                         weight "begin-writes2",
                                         bInvImpl (bCall "$set_in" [p; e']) (bCall name [bState; p]))

                        match e' with
                          | B.Expr.FunctionCall (("$set_universe" | "$set_empty"), []) ->
                            // writes(set_universe()) is for debugging, so disable the immediate false that we could conclude
                            // writes(set_empty()) doesn't mean anything, so also ignore it
                            bTrue
                          | B.Expr.FunctionCall (("$struct_extent" | "$extent" | "$full_extent"), args) when vcc3 ->
                            let args = if args.Length = 1 then bState :: args else args
                            bCall "$extent_mutable" args
                          | B.Expr.FunctionCall ("$span", [o]) when vcc3 ->
                            bCall "$mutable" [bState; o]
                          | B.Expr.FunctionCall (("$struct_extent" | "$extent" | "$full_extent" | "$span"), args) ->
                            if vcc3 then bCall "$initially_mutable" [bState; e']
                            else mut (Some (List.head (List.rev args))) "$mutable"
                          | _ -> 
                            if vcc3 then bCall "$initially_thread_owned_or_mutable" [bState; e']
                            else mut None "$thread_owned_or_even_mutable"                            
                      | _ -> bTrue
                  B.Stmt.MkAssume assump
                  
                let inParams = h.Parameters |> List.filter (fun v -> v.Kind <> C.VarKind.OutParameter)
                let inParamLabels = if env.hasIF then List.collect (fun (v:CAST.Variable) -> [B.Stmt.VarDecl (("FlowData#P#"+(v.Name),B.Type.Ref "$flowdata"), None)]) inParams
                                                 else []
                let init = List.map (ctx.AssumeLocalIs h.Token) inParams @ inParamLabels @ init                    
                
                let can_frame =
                  if List.exists (function C.ReadsCheck _ -> true | _ -> false) h.CustomAttr then []
                  else
                    [B.Stmt.MkAssume (bCall "$can_use_all_frame_axioms" [bState])]
                
                let doBody (s:CAST.Expr) =
                  let secDecls =
                    if (env.hasIF) then [B.Stmt.VarDecl(("FlowData#initPC", B.Type.Ref "$flowdata"), None)
                                         IF.setLLabel "FlowData#initPC" (B.Expr.Ref "$lblset.bot")
                                         IF.setPC (s.Token) (IF.getLabel (IF.getLData "FlowData#initPC"))
                                         assumeSync env s.Token]
                                   else []
                  B.Stmt.Block (B.Stmt.MkAssume (bCall "$function_entry" [bState]) ::
                                B.Stmt.VarDecl(("#stackframe", B.Type.Int), None) ::
                                secDecls @
                                (assumeSyncCS "function entry" env h.Token ::
                                 can_frame @
                                 init @
                                 List.map assumeMutability h.Writes @
                                 trStmt {env with IFContexts = (getLocalLabels s,"FlowData#initPC")::(env.IFContexts)} s @ 
                                 [B.Stmt.Label (Token.NoToken, "#exit")] @
                                 sanityChecks env h))
                let theBody =
                  if functionToVerify = null || functionToVerify = h.Name then h.Body
                  else None
                let proc = { proc with Body = Option.map doBody theBody }
                let proc =
                  match proc.Body with
                    | Some b ->
                      match hasStartHere b with
                        | Some newBody ->
                          { proc with Body = Some newBody; Attributes = B.Attribute.ExprAttr ("selective_checking", bTrue) :: proc.Attributes }
                        | None -> proc
                    | _ -> proc
                trPureFunction h @ [B.Decl.Proc proc]
            | C.Top.TypeDecl td ->
              match td.Kind with
                | C.TypeKind.Union
                | C.TypeKind.Struct -> trCompositeType td
                | C.TypeKind.Record
                | C.TypeKind.FunctDecl _
                | C.TypeKind.MathType -> trMathType td
            | C.Top.GeneratedAxiom(e, _)
            | C.Top.Axiom e ->
              let res = trExpr initialEnv e
              let seenState = ref false
              let replMS = function
                | B.Expr.Ref "$s" -> seenState := true; Some (er "#s")
                | B.Expr.Old _ ->
                  failwith "axiom mentions old"
                | _ -> None
              let res = res.Map replMS
              let res, vars = 
                if !seenState then (bImpl (bCall "$good_state" [er "#s"]) res), [("#s", tpState)] else res, []
                //if !seenState then res, [("#s", tpState)] else res, []
              let res =
                match vars, res with
                  | [], _ -> res
                  | _, B.Expr.Forall (tok, vars2, triggers, attrs, body) ->
                    B.Expr.Forall (tok, vars @ vars2, triggers, attrs, body)
                  | _, _ -> 
                    B.Expr.Forall (Token.NoToken, vars, [], weight "user-axiom", res)
              [B.Decl.Axiom res]
            | C.Top.Global ({ Kind = C.ConstGlobal ; Type = C.Ptr t } as v, _) ->
              if vcc3 then
                [bDeclUnique tpPtr (ctx.VarName v);
                 B.Decl.Axiom (bCall "$def_global" [varRef v; toTypeId t])]
              else
                [bDeclUnique B.Type.Int (ctx.VarName v)]
            | C.Top.Global _ -> die()
          with
            | Failure _ ->
              helper.Error(decl.Token, 9600, "OOPS for declaration " + decl.ToString())
              reraise()

      let archSpecific() = 
        let ptrSizeInBytes = !C.PointerSizeInBytes
        //let archComment = B.Comment ("Configuration for sizeof(void *) == " + ptrSizeInBytes.ToString())
        let sizeOfPtrAxiom = B.Axiom(bEq (er "$arch_ptr_size") (bInt ptrSizeInBytes))
        let startOfSpecPtrRange = B.Axiom(bEq (er "$arch_spec_ptr_start") (er ("$max.u" + (ptrSizeInBytes.ToString()))))
        [[sizeOfPtrAxiom; startOfSpecPtrRange]]

      let main () =
        let res = List.map trTop decls
        
        let types = List.fold (fun acc -> function
                                     | C.Top.TypeDecl ({ Kind = (C.Union|C.Struct)} as td) -> td :: acc
                                     | _ -> acc) [] decls
        let tn = if nestingExtents then typeNesting types else []

        List.concat (archSpecific() @ res @ [tn; ctx.FlushDecls mapEqAxioms])
        
      helper.SwTranslator.Run main ()
