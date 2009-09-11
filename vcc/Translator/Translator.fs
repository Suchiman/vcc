//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.FSharp.Math
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  open System
  
  module C = Microsoft.Research.Vcc.CAST
  module B = Microsoft.Research.Vcc.BoogieAST
  
  module Translator =
    type ClaimContext =
      {
        mutable ClaimChecks : list<B.Stmt>;
        mutable ClaimInits : list<B.Stmt>;
      }
      
    type Env =
      {
        OldState : B.Expr;
        WritesState : B.Expr;
        Writes : list<C.Expr>;
        WritesTime : B.Expr;
        AtomicObjects : list<B.Expr>;
        AtomicReads : list<B.Expr>;
        ClaimContext : option<ClaimContext>;
        FunctionCleanup : List<C.Expr> ref;
      }

    let nestingExtents = false
    
    let defaultWeights = [("writes", 0); ("select", 0); ("def-field-dot", 0); ("", 1)]
    
    let die() = failwith "confused, will now die"

    let rec _try_assoc elem = function
      | [] -> None
      | (a,b) :: _ when elem = a -> Some b
      | _ :: tail -> _try_assoc elem tail
    
    let _list_mem elem = List.exists (fun e -> e = elem)

    let xassert cond =
      if cond then ()
      else die()
    
    let notok = B.noToken
    let er = B.Expr.Ref
    let bState = er "$s"
    let cState = C.Expr.Macro({C.ExprCommon.Bogus with Type = C.Type.MathState}, "state", [])
    let bOld x = B.Expr.Old x
    let bTrue = B.Expr.BoolLiteral true
    let bFalse = B.Expr.BoolLiteral false
    let bEq a b = B.Expr.Primitive ("==", [a; b])
    let bNeq a b = B.Expr.Primitive ("!=", [a; b])    
    let bNot a = B.Expr.Primitive ("!", [a])
    let bCall a b = B.Expr.FunctionCall (a, b)
    let bInt (n : int32) = B.IntLiteral (new Math.BigInt(n))
    let bBool v = B.BoolLiteral v
    let dont_inst p = [[bCall "$dont_instantiate" [p]]]
    
    let bContains name (expr:B.Expr) = 
      let seen = ref false
      let check = function
        | B.Expr.Ref name' when name = name' -> seen := true; None
        | _ -> None
      expr.Map check |> ignore
      !seen
    
    let afmte id msg exprs =
      (TransUtil.afmte id msg exprs).Token
    
    let afmtet tok id msg (objs:list<C.Expr>) =
      (TransUtil.forwardingToken tok None (fun () -> TransUtil.afmt id msg [ for o in objs -> o.Token.Value ])).Token

    let tpPtr = B.Type.Ref "$ptr"
    let tpPrimitive = B.Type.Ref "$primitive"
    let tpStruct = B.Type.Ref "$struct"
    let tpPtrset = B.Type.Ref "$ptrset"
    let tpCtype = B.Type.Ref "$ctype"
    let tpState = B.Type.Ref "$state"
    let tpVersion = B.Type.Ref "$version"
    let tpToken = B.Type.Ref "$token"
    let tpLabel = B.Type.Ref "$label"
    
    let bImpl a b = 
      match a, b with
        | _, B.Expr.BoolLiteral true -> bTrue
        | B.Expr.BoolLiteral true, _ -> b
        | _ -> B.Expr.Primitive ("==>", [a; b])
    
    let bOr x y =
      match (x, y) with
        | (B.Expr.BoolLiteral false, e)
        | (e, B.Expr.BoolLiteral false) -> e
        | (a, b) -> B.Expr.Primitive ("||", [a; b])
      
    let bInvImpl a b =
      if true then bImpl a b
      else
        match a, b with
          | _, B.Expr.BoolLiteral true -> bTrue
          | B.Expr.BoolLiteral true, _ -> b
          | _ -> bOr b (B.Expr.Primitive ("!", [a]))
    
    let bAnd x y =
      match (x, y) with
        | (B.Expr.BoolLiteral true, e)
        | (e, B.Expr.BoolLiteral true) -> e
        | (a, b) -> B.Expr.Primitive ("&&", [a; b])
    
    let bMultiAnd = List.fold bAnd bTrue    
    let bMultiOr = List.fold bOr bFalse
    
    let bSubst args (e:B.Expr) =
      e.Map (function 
                | B.Expr.Ref n -> _try_assoc n args
                | _ -> None)
    
    let initialEnv = { OldState = bOld bState; 
                       Writes = []; 
                       AtomicObjects = []; 
                       AtomicReads = [];
                       WritesState = bOld bState;  
                       WritesTime = er "$bogus";
                       ClaimContext = None;
                       FunctionCleanup = ref []
                     }
    
    let fieldName (f:C.Field) = f.Parent.Name + "." + f.Name
    
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
    
    let max = function
      | x :: xs -> List.fold (fun acc e -> if e > acc then e else acc) x xs
      | [] -> 0
      
    let mutable stateId = 0
    let saveState pref =      
      let oldState = pref + "State#" + stateId.ToString()
      stateId <- stateId + 1
      ([B.Stmt.VarDecl ((oldState, tpState), None);
        B.Stmt.Assign (er oldState, bState)], er oldState)
    
    type BvOp =
      | BinSame of string
      | BinDifferent of string * string
      | BinPredDifferent of string * string
      | UnarySame of string
          
    let translate functionToVerify (helper:Helper.Env) decls =
      let quantVarTokens = new Dict<_,_>()
      let tokenConstantNames = new Dict<_,_>()
      let tokenConstants = ref []
      let soFarAssignedLocals = ref []      
      let fileIndices = new Dict<_,_>()
      let generatedBvOps = new Dict<_,_>()
      let conversionTypes = new Dict<_,_>()
      let mapTypes = new Dict<_,_>()
      let distinctTypes = new Dict<_,_>()
      let typeCodes = new Dict<_,_>()
      let invLabels = new Dict<_,_>()
      let invLabelConstants = ref []
      let floatLiterals = new Dict<_,_>()
      
      let weights = defaultWeights
      let weight (id:string) =
        let w =
          let rec aux = function
            | (pref, w) :: rest ->
              if id.StartsWith pref then w
              else aux rest
            | [] -> failwith "weight"
          aux weights
        if w = 1 then []
        else [B.ExprAttr ("weight", bInt w)]
      
      let rec toTypeId' translateArrayAsPtr t =
        let internalizeType t bt =      
          let rec isDerivedFromTypeVar = function
            | C.Type.TypeVar _ -> true
            | C.Ptr(t)
            | C.Array(t, _) -> isDerivedFromTypeVar t
            | C.Map(from, _to) -> List.exists isDerivedFromTypeVar [from; _to]
            | _ -> false
          if not (distinctTypes.ContainsKey t) && not (isDerivedFromTypeVar t) then
            distinctTypes.Add (t, distinctTypes.Count + 1)
            let cd = { Unique = true 
                       Name = "#distTp" + distinctTypes.Count.ToString() 
                       Type = tpCtype } : B.ConstData
            let eq = bEq (er cd.Name) bt
            tokenConstants := B.Decl.Const cd :: B.Decl.Axiom eq :: !tokenConstants
          bt
        match t with
          | C.Type.Bool -> er "^^bool"
          | C.Type.Integer kind -> er ("^^" + C.Type.IntSuffix kind)
          | C.Type.MathInteger -> er "^^mathint"
          | C.Type.Primitive kind -> er ("^^" + C.Type.PrimSuffix kind) 
          | C.Type.Void -> er "^^void"
          | C.Type.Ptr tp ->
            internalizeType t (bCall "$ptr_to" [toTypeId' false tp])
          | C.Type.ObjectT -> toTypeId' false (C.Ptr C.Void)
          | C.Type.Array (tp, _) when translateArrayAsPtr ->
            internalizeType (C.Type.Ptr tp) (bCall "$ptr_to" [toTypeId' translateArrayAsPtr tp])
          | C.Type.Array (tp, sz) ->
            internalizeType t (bCall "$array" [toTypeId' translateArrayAsPtr tp; B.Expr.IntLiteral(new bigint(sz))])
            //bCall "$array" [toTypeId tp; bInt sz]
          | C.Type.Map (range, dom) -> 
            internalizeType t (bCall "$map_t" [toTypeId' false range; toTypeId' false dom])
          | C.Type.Ref { Name = n; Kind = (C.MathType|C.Record|C.FunctDecl _) } -> er ("^$#" + n)
          | C.Type.Ref td -> er ("^" + td.Name)
          | C.Type.TypeIdT -> er "^$#typeid_t"
          | C.Type.Claim -> er "^^claim"
          | C.Type.TypeVar({Name = id}) -> er ("^^TV#" + id)
          | C.Type.Volatile(t) -> 
            helper.Panic("volatile type modifier survived")
            toTypeId'  false t
            
      let toTypeId = toTypeId' false
    
      let getTypeCode t =
        match typeCodes.TryGetValue t with
          | true, n -> n
          | _ ->
            // needed here, as it might add stuff to distinctTypes
            let typeId = toTypeId t
            match distinctTypes.TryGetValue t with
              | true, n ->
                typeCodes.[t] <- n
                n
              | _ ->
                let n = -(typeCodes.Count + 1)
                typeCodes.[t] <- n
                tokenConstants := B.Decl.Axiom (bCall "$type_code_is" [bInt (- n); typeId]) :: !tokenConstants
                n
      
      let getTypeSuffix t = 
        let tc = getTypeCode t
        if tc < 0 then "#tc" + (-tc).ToString()
        else "#dt" + tc.ToString()
      
      let tokSuffix (t:Token) = 
        let fidx =
          if t.Filename = null || t.Filename = "no_file" then 0
          else
            match fileIndices.TryGetValue t.Filename with
              | true, idx -> idx
              | _ ->
                let idx = fileIndices.Count + 1
                fileIndices.Add (t.Filename, idx)
                let sb = new System.Text.StringBuilder()
                for c in t.Filename do
                  if System.Char.IsLetterOrDigit c || c = '.' || c = '_' then
                    sb.Append c |> ignore
                  else
                    (sb.Append '?').Append(System.String.Format("{0:X2}", (int)c)) |> ignore
                let name = "#file^" + sb.ToString()
                let constdata = { Name = name; Type = tpToken; Unique = true } : B.ConstData
                let axiom = B.Decl.Axiom (bCall "$file_name_is" [bInt idx; er name])
                tokenConstants := B.Decl.Const constdata :: axiom :: !tokenConstants
                idx
        System.String.Format ("${0}^{1}.{2}", fidx, t.Line, t.Column)
    
      let rec typeDepth = function
        | C.Type.Ref td ->
          1 + max (List.map (fun (f:C.Field) -> typeDepth f.Type) td.Fields)
        | C.Type.Array (t, _) -> typeDepth t
        | C.Type.Claim -> 1
        | t ->
          if t.IsComposite then
            helper.Oops (C.bogusToken, "strange type " + t.ToString())
          1
                  
      let registerToken name =
        if not (tokenConstantNames.ContainsKey name) then 
          tokenConstantNames.Add (name, true)
          let constdata = { Name = name; Type = tpToken; Unique = true } : B.ConstData
          tokenConstants := B.Decl.Const constdata :: !tokenConstants
      
      let trInvLabel (lbl:string) =
        let result = "l#" + lbl;
        if not (invLabels.ContainsKey(result)) then 
          invLabels.Add(result, true)
          let constdata = { Name = result; Type = tpLabel; Unique = true } : B.ConstData
          invLabelConstants := B.Decl.Const constdata :: !invLabelConstants
        result

      let getTokenConst tok =
        let name = "#tok" + tokSuffix tok
        registerToken name
        name
      
      let assumeSync (env:Env) tok =
        let name = getTokenConst tok
        let pred =
          match env.AtomicObjects with
            | [] -> "$full_stop_ext"
            | _ -> "$good_state_ext"
        B.Stmt.Assume (bCall pred [er name; bState])

      let getFloatConst (f : float) =
        match floatLiterals.TryGetValue f with
          | true, e -> e
          | false, _ ->
            let floatName = "floatLiteral#" + helper.UniqueId().ToString()
            let t = B.Type.Ref "$primitive"
            let decl = B.Const( {Unique = true; Name = floatName; Type = t } )
            let result = B.Expr.Ref floatName
            floatLiterals.Add(f, result)
            tokenConstants := decl :: !tokenConstants
            result
      
      let rec typeIdToName = function
        | B.Expr.Ref s -> s
        | B.Expr.FunctionCall (f, a) -> f + ".." + String.concat "." (List.map typeIdToName a)
        | t -> helper.Panic ("cannot compute name for type expression " + t.ToString()); ""        
      
      let rec trType (t:C.Type) : B.Type =
        match t with
          | C.Type.MathInteger
          | C.Type.Integer _ 
          | C.Type.Ptr _ -> B.Type.Int
          | C.Type.Primitive _ -> tpPrimitive
          | C.Type.Bool -> B.Type.Bool
          | C.Type.ObjectT -> tpPtr
          | C.Type.TypeIdT -> tpCtype
          | C.Type.Map (t1, C.Type.Ref({Kind = C.Union|C.Struct})) -> 
            trType (C.Type.Map (t1, C.Type.MathStruct))
          | C.Type.Map (t1, t2) ->
            let bt1 = trType t1
            let bt2 = trType t2
            let mapName = typeIdToName (toTypeId t)
            let typeId = mapName
            let tp = B.Type.Ref typeId
            if not (mapTypes.ContainsKey typeId) then
              mapTypes.Add (typeId, true)
              let ite = "$ite." + (mapName.Replace ("$#", "")).Replace ("$", "")
              let mapType = B.Type.Ref (typeId)
              let sel = "$select." + typeId
              let stor  = "$store." + typeId
              let v = er "v"
              let v, inRange =
                match t2 with
                  | C.Type.Integer _ -> 
                    bCall "$unchecked" [toTypeId t2; v], 
                      [B.Decl.Axiom (B.Expr.Forall (["M", tp; "p", bt1], [], weight "select-map-eq", 
                                                    bCall "$in_range_t" [toTypeId t2; bCall sel [er "M"; er "p"]]))]
                  | _ -> v, []
              let selStorPP = 
                bEq (bCall sel [bCall stor [er "M"; er "p"; er "v"]; er "p"]) v
              let selStorPQ =
                bInvImpl (bNeq (er "p") (er "q"))
                          (bEq (bCall sel [bCall stor [er "M"; er "p"; er "v"]; er "q"]) (bCall sel [er "M"; er "q"]))
              let mpv = ["M", tp; "p", bt1; "v", bt2]
              let fns = [B.Decl.TypeDef typeId;
                         B.Decl.Function (mapType, [B.StringAttr("external", "ITE"); B.StringAttr("bvz", "ITE"); B.StringAttr("bvint", "ITE")], ite, ["c", B.Type.Bool; "a", mapType; "b", mapType]);
                         B.Decl.Function (bt2, [], sel, ["M", tp; "p", bt1]);
                         B.Decl.Function (tp, [], stor, mpv);
                         B.Decl.Axiom (B.Expr.Forall (mpv, [], weight "select-map-eq", selStorPP));
                         B.Decl.Axiom (B.Expr.Forall (mpv @ ["q", bt1], [], weight "select-map-neq", selStorPQ));
                        ] @ inRange
              tokenConstants := fns @ !tokenConstants
            tp
          | C.Type.Ref ({ Kind = C.Record }) -> B.Type.Ref "$record"
          | C.Type.Ref ({ Name = n; Kind = (C.MathType|C.FunctDecl _) }) ->
            match n with
              | "ptrset" -> tpPtrset
              | "struct" -> tpStruct
              | "state_t" -> tpState
              | _ -> B.Type.Ref ("$#" + n)
          | C.Type.Volatile _
          | C.Type.Claim
          | C.Type.Array _ 
          | C.Type.Void
          | C.Type.TypeVar _
          | C.Type.Ref _ ->
            helper.Panic ("wrong type survived: " + t.ToString())

      let castSuffix t = 
        let rec suff = function
          | B.Type.Bool -> "bool"
          | B.Type.Int -> "int"
          | B.Type.Map ([f], t) -> "map." + suff f + "." + suff t
          | B.Type.Ref n -> n.Replace ("$", "")
          | t -> helper.Panic ("wrong type in castSuffix " + t.ToString())
        let suff = suff t
        match suff with
          // predefined in the prelude
          | "record"
          | "version"
          | "ptr"
          | "bool"
          | "int"
          | "ptrset" -> suff
          // possible need to generate conversion function
          | _ ->
            if not (conversionTypes.ContainsKey suff) then
              conversionTypes.Add (suff, true)
              let toIntName = "$" + suff + "_to_int"
              let toInt = B.Decl.Function (B.Type.Int, [], toIntName, [("x", t)])
              let fromIntName = "$int_to_" + suff
              let fromInt = B.Decl.Function (t, [], fromIntName, [("x", B.Type.Int)])
              let both = bCall fromIntName [bCall toIntName [er "#x"]]
              let ax1 = B.Decl.Axiom (B.Expr.Forall ([("#x", t)], [], weight "conversion", bEq (er "#x") both)) 
              tokenConstants := toInt :: fromInt :: ax1 :: !tokenConstants
            suff
      
      let castToInt t e =
        match t with
          | B.Type.Int -> e
          | _ -> bCall ("$" + castSuffix t + "_to_int") [e]
      
      let castFromInt t e =
        match t with
          | B.Type.Int -> e
          | _ -> bCall ("$int_to_" + castSuffix t) [e]
     
      let typedRead s p t =
        match t with
          | C.Ptr t ->
            bCall "$read_ptr" [s; p; toTypeId t]
          | C.Integer k ->
            bCall ("$read_" + C.Type.IntSuffix k) [s; p]
          | C.Bool ->
            bCall "$read_bool" [s; p]
          | t ->                
            castFromInt (trType t) (bCall "$read_any" [s; p])

      let varName (v:C.Variable) =
        if v.Name.IndexOf '#' >= 0 || v.Name.IndexOf '.' >= 0 then
          if v.Kind = C.VarKind.ConstGlobal then
            "G#" + v.Name + getTypeSuffix v.Type
          else
            v.Name
        else
          match v.Kind with
            | C.VarKind.QuantBound -> "Q#" + v.Name + (tokSuffix quantVarTokens.[v]) + getTypeSuffix v.Type
            | C.VarKind.ConstGlobal -> "G#" + v.Name + getTypeSuffix v.Type
            | C.VarKind.SpecParameter -> "SP#" + v.Name
            | C.VarKind.OutParameter -> "OP#" + v.Name
            | C.VarKind.Parameter -> "P#" + v.Name
            | C.VarKind.SpecLocal -> "SL#" + v.Name
            | C.VarKind.Local -> "L#" + v.Name
            | C.VarKind.Global -> die()
      
      let varRef v = er (varName v)

      let trVar (v:C.Variable) : B.Var =
        (varName v, trType (v.Type))

      let typeVarName (tv : C.TypeVariable) = "^^TV#" + tv.Name
      let typeVarRef (tv : C.TypeVariable) = er (typeVarName tv)
      let trTypeVar (tv : C.TypeVariable) : B.Var = (typeVarName tv, trType C.Type.TypeIdT)
                  
        
      let trWhereVar (v:C.Variable) =
        let v' = trVar v
        match v.Type with
          | C.Type.Integer k -> (v', Some (bCall ("$in_range_" + C.Type.IntSuffix k) [varRef v]))
          | _ -> (v', None)
    
      let ptrType (expr:C.Expr) =
        match expr.Type with
          | C.Ptr C.Void -> failwith ("void* not supported " + expr.ToString())
          | C.Ptr t -> toTypeId t
          | _ -> failwith ("pointer type expected " + expr.ToString())        
      
      let addType t e =
        match t with
          | C.Type.Ptr t ->
            bCall "$ptr" [toTypeId t; e]
          | _ -> e
      
      let stripType t e =
        match t with
          | C.Type.Ptr t ->
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

      let assumeLocalIs tok (l:C.Variable) =
        let pos = getTokenConst tok
        let name = "#loc." + l.Name
        if not (tokenConstantNames.ContainsKey name) then
          soFarAssignedLocals := l :: !soFarAssignedLocals
        registerToken name
        let valIs suff v = bCall ("$local_value_is" + suff) [bState; er pos; er name; v; toTypeId l.Type]
        let cond =
          match l.Type with
            | C.Ptr _ -> 
              let v' = addType l.Type (varRef l)
              bAnd (valIs "" (bCall "$ptr_to_int" [v'])) (valIs "_ptr" v')
            | _ -> valIs "" (castToInt (trType l.Type) (varRef l))
        B.Stmt.Assume cond
  (* CLG Counter Example visualizer code starts here *)
      let n = ref 0

      let loc_or_glob k =
        let str =
          match k with
            | C.Parameter -> "cev_parameter"
            | C.SpecParameter -> "cev_local"
            | C.OutParameter -> "cev_local"
            | C.Local -> "cev_local"
            | C.SpecLocal -> "cev_local"
            | C.Global -> "cev_global"
            | C.ConstGlobal -> "cev_global"
            | C.QuantBound -> "cev_implicit"
        er str

      let cevPrintVarOK (str : string) k = 
        match k with 
        | C.QuantBound -> false
        | _ -> not (str.Contains "ite") (* CLG: what other vars are added in automatically? *)

      let cevNIncr () = n := !n + 1;

      let cevSavePos n pos = B.Stmt.Assume (bEq (bCall "#cev_save_position" [B.Expr.IntLiteral(BigInt.Parse((n).ToString()))]) (er pos) );

      let cevVarsIntroed = new System.Collections.Generic.Dictionary<C.Variable, bool>()

      let cevVarIntro tok (incr : bool) (l:C.Variable) = 
        if helper.Options.PrintCEVModel && (cevPrintVarOK l.Name l.Kind) && (not (cevVarsIntroed.ContainsKey l)) then
            cevVarsIntroed.Add(l, true)
            let pos = getTokenConst tok 
            let name = "#loc." + l.Name
            registerToken name
            let valIs v = bCall "#cev_var_intro" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); (loc_or_glob l.Kind); er name; v; toTypeId l.Type] 
            let cond =       
                match l.Type with
                | C.Ptr _ ->  
                    let v' = addType l.Type (varRef l)
                    (valIs (bCall "$ptr_to_int" [v']))
                | _ -> valIs (castToInt (trType l.Type) (varRef l))
            if incr then
                let assume = [cevSavePos !n pos; B.Stmt.Assume cond]
                cevNIncr ();                  
                assume                
            else [B.Stmt.Assume cond; ]
        else []

      let cevInitCall tok = 
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = B.Stmt.Assume(bCall "#cev_init" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString()))])
            let posSave = cevSavePos !n pos
            cevNIncr ()
            let name = "#loc.$s"
            registerToken name
            let intro_state = [B.Stmt.Assume (bCall "#cev_var_intro" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); (er "cev_implicit"); er name; er "$s"; er "^$#state_t"])]
            let posSave2 = cevSavePos !n pos
            let assume = [valIs; posSave;] @ intro_state @ [posSave2]
            cevNIncr ();
            assume
        else []

      let cevStateUpdate tok =
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_var_update" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); (er "cev_implicit"); er "#loc.$s"; er "$s"]
            let assume = [cevSavePos !n pos; B.Stmt.Assume valIs]
            cevNIncr ();
            assume
        else []

      let cevVarUpdate tok (incr : bool) (l:C.Variable) =
        if helper.Options.PrintCEVModel && (cevPrintVarOK l.Name l.Kind) then
            if cevVarsIntroed.ContainsKey l then 
                let pos = getTokenConst tok 
                let name = "#loc." + l.Name
                registerToken name (* CLG - things are different for pointer updates! *)
                let valIs suff v = bCall ("#cev_var_update" + suff) [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); (loc_or_glob l.Kind); er name; v]
                let cond =
                    match l.Type with
                    | C.Ptr _ -> 
                        let v' = addType l.Type (varRef l)
                        (valIs "_ptr" (bCall "$ptr_to_int" [v']))
                    | _ -> valIs "" (castToInt (trType l.Type) (varRef l))
                let assume = [cevSavePos !n pos; B.Stmt.Assume cond]
                if incr then cevNIncr ()
                [B.Stmt.Assume cond;]
            else cevVarIntro tok incr l
        else []

      let rec cevVarUpdateList tok (l:C.Variable list) = // fixme: make sure we save state after this and increment n
        if helper.Options.PrintCEVModel then
            match l with
            | v :: vs -> if cevPrintVarOK v.Name v.Kind then (cevVarUpdate tok false v) @ (cevVarUpdateList tok vs) else (cevVarUpdateList tok vs)  
            | [] -> cevNIncr(); []
        else []

      let rec processEList (elist : C.Expr list) varlist = 
        match elist with
        | e :: [] -> let _, vlist = lastStmtVars e varlist
                     vlist
        | e :: es -> let (_, vlist) = lastStmtVars e varlist
                     processEList es vlist
        | [] -> varlist 

      and lastStmtVars (body : C.Expr) varlist = 
        match body with
        | C.Expr.Prim(ec, o, elist) ->
            let varlist' = processEList elist varlist 
            if not (List.isEmpty elist) then 
                let (t, _) = lastStmtVars (List.hd (List.rev elist)) varlist
                (t, varlist')
            else
                (ec.Token, varlist)
        | C.Expr.Call(ec, f, tlist, elist) -> 
            let varlist' = processEList elist varlist 
            if not (List.isEmpty elist) then
                let (t, _) = lastStmtVars (List.hd (List.rev elist)) varlist
                (t, varlist')            
            else
                (ec.Token, varlist')
        | C.Expr.IntLiteral(ec, bi) -> 
            ec.Token, varlist
        | C.Expr.BoolLiteral(ec, b) -> 
            ec.Token, varlist
        | C.Expr.Deref(ec, e) -> 
            lastStmtVars e varlist 
        | C.Expr.Dot(ec, e, f) -> 
            lastStmtVars e varlist
        | C.Expr.Index(ec, e1, e2) -> 
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.Cast(ec, cs, e) -> lastStmtVars e varlist
        | C.Expr.Quant(ec, qd) -> (* CLG fixme *)
            let b = qd.Body 
            lastStmtVars b varlist
        | C.Expr.Result(ec) -> 
            ec.Token, varlist
        | C.Expr.Old(ec, e1, e2) -> 
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.VarDecl(ec, v) -> 
            ec.Token, varlist
        | C.Expr.VarWrite(ec, vlist, e) -> 
            lastStmtVars e (varlist @ vlist) (* FIXME *)
        | C.Expr.MemoryWrite(ec, e1, e2) -> (* CLG this memory write probably updates a var - do something different for that? *)
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.If(ec, e1, e2, e3) -> 
            let t, vlist' = lastStmtVars e1 varlist
            let t', vlist'' = lastStmtVars e2 vlist'
            lastStmtVars e3 vlist''
        | C.Expr.Loop(ec, elist1, elist2, e) -> 
            lastStmtVars e varlist
        | C.Expr.Goto(ec, lid) -> ec.Token, varlist
        | C.Expr.Label(ec, lid) -> ec.Token, varlist
        | C.Expr.Block(ec, elist) -> 
            if not (List.isEmpty elist) then
                let vlist = processEList elist varlist
                let revlist = List.rev elist
                let h = List.hd revlist
                let tok, vlist' = lastStmtVars h vlist
                (tok, vlist)                
            else (ec.Token, varlist)
        | C.Expr.Assert(ec, e) -> lastStmtVars e varlist
        | C.Expr.Assume(ec, e) -> lastStmtVars e varlist
        | C.Expr.Pure(ec, e) -> lastStmtVars e varlist
        | C.Expr.Return(ec, eopt) -> 
            match eopt with
            | Some(e) -> lastStmtVars e varlist
            | None -> ec.Token, varlist
        | C.Expr.Atomic(ec, elist, e) -> 
            let vlist = processEList elist varlist
            let _, vlist' = lastStmtVars e vlist
            if not (List.isEmpty elist) then
                lastStmtVars (List.hd (List.rev elist)) vlist' (* CLG I think elist is the atomic block, right? *)
            else (ec.Token, vlist')
        | C.Expr.Comment(ec, s) -> ec.Token, varlist
        | C.Expr.Stmt(ec, e) -> 
            lastStmtVars e varlist
        | C.Expr.Macro(ec, s, elist) -> 
            if not (List.isEmpty elist) then
                lastStmtVars (List.hd (List.rev elist)) varlist
            else (ec.Token, varlist)
        | C.Expr.UserData(ec, o) ->  
            ec.Token, varlist
        | _ -> 
            (body.Token, varlist)

      let cevRegLoopBody (stmt : C.Expr) body = 
        if helper.Options.PrintCEVModel then
          let pos1 = getTokenConst stmt.Token
          let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); er "loop_register"]
          let retval = [B.Stmt.Assume valIs; cevSavePos !n pos1]
          retval
        else []    
        
      let cevCondMoment tok =
        if helper.Options.PrintCEVModel then
          let pos = getTokenConst tok
          let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); er "conditional_moment"; ] 
          let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
          cevNIncr (); retval
        else []
      
      let cevFunctionCall tok = (* put in updates for stuff referenced in args, I think *)
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_function_call" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString()))]
            let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
            cevNIncr ();
            retval
        else []
            
      let cevBranchChoice tok branchTaken =
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(BigInt.Parse((!n).ToString())); branchTaken; ] 
            let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
            cevNIncr ();
            retval
        else []
(*        CLG Counter Example visualizer code ends here *)

      let isSetEmpty = function
        | C.Macro (_, "_vcc_set_empty", []) -> true
        | _ -> false
        
      let warnForIneffectiveOld token expr =
        if not (bContains "$s" expr) then
          helper.Warning (token, 9106, "'old', 'in_state', or 'when_claimed' in '" + token.Value + "' has no effect")

      let bvOps =
        [ "+", BinSame "add";
          "-", BinSame "sub"; 
          "*", BinSame "mul"; 
          "/", BinDifferent ("sdiv", "udiv"); 
          "%", BinDifferent ("srem", "urem"); 
          "&", BinSame "and"; 
          "|", BinSame "or"; 
          "^", BinSame "xor"; 
          "<<", BinSame "shl"; 
          ">>", BinDifferent ("ashr", "lshr"); 
          "<", BinPredDifferent ("slt", "ult");
          ">", BinPredDifferent ("sgt", "ugt");
          "<=", BinPredDifferent ("sle", "ule");
          ">=", BinPredDifferent ("sge", "uge");
          "u~", UnarySame "not";
          ]
      
      let bvType (expr:C.Expr) = function
          | C.Integer k ->
            let (sz, _) = k.SizeSign
            B.Type.Bv sz
          | C.Bool as t -> trType t
          | tp ->
            helper.Error (expr.Token, 9689, "type '" + tp.ToString() + "' is not supported for bitvector translation (in " + expr.Token.Value + ")")            
            B.Type.Int
            
      let bvSignExtensionOp fromBits toBits =
        let boogieName = "$bv_sign_ext_" + fromBits.ToString() + "_" + toBits.ToString()
        if generatedBvOps.ContainsKey boogieName then boogieName
        else
          generatedBvOps.Add(boogieName, true)
          let retTp = B.Type.Bv toBits
          let fromTp = B.Type.Bv fromBits
          let fn = B.Decl.Function (retTp, [B.Attribute.StringAttr("bvbuiltin", "sign_extend " + (toBits - fromBits).ToString())], boogieName, [("p", fromTp)])
          tokenConstants := fn :: !tokenConstants
          boogieName
            
      let bvZeroExtend fromBits toBits e =  B.BvConcat(B.Expr.BvLiteral (new bigint(0), toBits - fromBits), e)
            
      let bvExtend (fromType : C.Type) (toType : C.Type) e =
        let fromBits = 8 * fromType.SizeOf
        let toBits = 8 * toType.SizeOf
        if fromBits >= toBits then e 
        elif fromType.IsSignedInteger then bCall (bvSignExtensionOp fromBits toBits) [e]
        else bvZeroExtend fromBits toBits e
          
      let bvOpFor expr tp name =
        let bvt = bvType expr tp
        let (signedName, unsignedName, nargs, boolRes) =
          match _try_assoc name bvOps with
            | Some (UnarySame name) -> (name, name, 1, false)
            | Some (BinSame name)   -> (name, name, 2, false)
            | Some (BinDifferent (n1, n2)) -> (n1, n2, 2, false)
            | Some (BinPredDifferent (n1, n2)) -> (n1, n2, 2, true)
            | None -> 
              helper.Oops (expr.Token, "unknown operator '" + name + "' bv_lemma(..." + expr.Token.Value + "...)")
              die()
        let (sz, sign) = 
          match tp with
            | C.Integer k -> k.SizeSign
            | _ -> die()
        let bvname = "bv" + (if sign then signedName else unsignedName)
        let boogieName = "$bv_" + bvname + sz.ToString()
        if generatedBvOps.ContainsKey boogieName then boogieName
        else
          generatedBvOps.Add (boogieName, true)
          let bvt = B.Type.Bv sz
          let parms = [for i = 1 to nargs do yield ("p" + i.ToString(), bvt)]
          let retTp = if boolRes then B.Type.Bool else bvt
          let fn = B.Decl.Function (retTp, [B.Attribute.StringAttr ("bvbuiltin", bvname)], boogieName, parms)
          tokenConstants := fn :: !tokenConstants
          boogieName
      
      let rec trBvExpr env expr =
        let self = trBvExpr env
        let selfs = List.map self
        let selfExtend toType (expr : C.Expr) = bvExtend expr.Type  toType (self expr)
        let selfsExtend toType = List.map (selfExtend toType)
        match expr with
          | C.Expr.Prim (_, C.Op (("!="|"==") as opName, _), [e1; e2]) ->
            let args = 
              if e1.Type.SizeOf = e2.Type.SizeOf then [self e1; self e2]
              elif e1.Type.SizeOf < e2.Type.SizeOf then [selfExtend e2.Type e1; self e2]
              else [self e1; selfExtend e1.Type e2]
            B.Expr.Primitive (opName, args)

          | C.Expr.Prim (_, C.Op (("&&"|"||"|"==>"|"<==>"|"!") as opName, _), args) ->
            B.Expr.Primitive (opName, selfs args)
            
          | C.Expr.Prim (_, C.Op (("+"|"-"|"*"|"/"|"%"), C.Checked), _) ->
            helper.Error (expr.Token, 9659, "operators in bv_lemma(...) need to be unchecked (expression: " + expr.Token.Value + ")")
            er "$err"
            
          | C.Expr.Prim (c, (C.Op((">>"|"<<") as op, _)), [arg1; arg2]) when c.Type.SizeOf = 8 ->
           let bArg1 = self arg1
           let bArg2 = bvZeroExtend (arg2.Type.SizeOf * 8) 64 (self arg2)
           bCall (bvOpFor expr arg1.Type op) [bArg1; bArg2]
          
          | C.Expr.Prim (c, C.Op ("-", _), [arg]) ->
            trBvExpr env (C.Expr.Prim(c, C.Op("-", C.CheckedStatus.Processed), [C.Expr.IntLiteral(c, new bigint(0)); arg]))
            
          | C.Expr.Prim (_, C.Op (("<"|">"|"<="|">=") as opName, _), [e1; e2]) ->
            let args, opType = 
              if e1.Type.SizeOf = e2.Type.SizeOf then [self e1; self e2], e1.Type
              elif e1.Type.SizeOf < e2.Type.SizeOf then [selfExtend e2.Type e1; self e2], e2.Type
              else [self e1; selfExtend e1.Type e2], e1.Type
            bCall (bvOpFor expr opType opName) args
            
          | C.Expr.Prim (_, C.Op (op, _), args) ->
            let op =
              if args.Length = 1 then "u" + op else op
            bCall (bvOpFor expr expr.Type op) (selfsExtend expr.Type args)
            
          | C.Expr.Quant (c, ({ Kind = C.Forall } as q)) ->
            for v in q.Variables do
              quantVarTokens.[v] <- c.Token
            let body = self q.Body
            let body =
              match q.Condition, q.Kind with
                | Some e, C.Forall -> bImpl (self e) body
                | None, _ -> body                
                | _ -> die()
            let trVar v =
              (varName v, bvType expr v.Type)
            let vars = List.map trVar q.Variables
            B.Forall (vars, [], weight "user-bv", body)
          
          | C.Expr.Ref (_, ({ Kind = C.QuantBound } as v)) ->
            varRef v
          
          | C.Expr.IntLiteral (c, v) when v >= bigint.Zero ->          
            B.Expr.BvLiteral (v, c.Type.SizeOf * 8)
            
          | C.Expr.IntLiteral (c, v) when v < bigint.Zero ->
            trBvExpr env (C.Expr.Prim(c, C.Op("-", C.CheckedStatus.Processed), [C.Expr.IntLiteral(c, -v)]))
          
          | C.Expr.Macro (_, "in_range_u1", [e])
          | C.Expr.Macro (_, "in_range_i1", [e]) when e.Type.SizeOf <= 1 ->
            bTrue

          | C.Expr.Macro (_, "in_range_u2", [e])
          | C.Expr.Macro (_, "in_range_i2", [e]) when e.Type.SizeOf <= 2 ->
            bTrue

          | C.Expr.Macro (_, "in_range_u4", [e])
          | C.Expr.Macro (_, "in_range_i4", [e]) when e.Type.SizeOf <= 4 ->
            bTrue
            
          | C.Expr.Macro (_, "in_range_u8", [e])
          | C.Expr.Macro (_, "in_range_i8", [e]) when e.Type.SizeOf <= 8 ->
            bTrue
          
          | C.Expr.Macro (_, "unchecked_u4", [e])
          | C.Expr.Macro (_, "unchecked_i4", [e]) when e.Type.SizeOf <= 4 -> self e
          
          | C.Expr.Macro (_, "unchecked_u8", [e])
          | C.Expr.Macro (_, "unchecked_i8", [e]) when e.Type.SizeOf <= 8 -> self e
          
          | C.Expr.BoolLiteral (_, v) -> B.BoolLiteral v
          
          | C.Expr.Cast (c, ch, e) ->
            match e.Type, c.Type with
              | src, dst when src = dst -> self e
              | C.Integer k, C.Bool ->
                B.Expr.Primitive ("!=", [trBvExpr env e; B.Expr.BvLiteral (bigint.Zero, fst k.SizeSign)])
              | C.Integer _ as src, (C.Integer _ as dst) when ch <> C.CheckedStatus.Checked ->
                if src.SizeOf = dst.SizeOf then self e
                elif src.SizeOf < dst.SizeOf then selfExtend dst e
                else B.BvExtract(self e, dst.SizeOf * 8, 0)
              | src, dst -> 
                helper.Error (expr.Token, 9690, "cast from " + src.ToString() + " to " + dst.ToString() + " is not supported in bv_lemma(...)")
                er "$err"
          
          | _ ->
            helper.Error (expr.Token, 9660, "unsupported expression in bv_lemma(...): " + expr.Token.Value + " (" + expr.ToString() + ")")
            er "$err"
      
      let claimStateId = ref 0
          
      let rec trExpr (env:Env) expr =
        let self = trExpr env
        let selfs = List.map self
        let isFloatingPoint = function | C.Type.Primitive _ -> true | _ -> false
        match expr with
          | C.Expr.Cast ({ Type = C.Type.Integer k }, _, e') ->
            match e'.Type with
              | C.Type.Bool ->
                bCall "$bool_to_int" [self e']
              | C.Type.Integer _ -> self e'
              | C.Type.MathInteger -> self e'
              | C.Type.Ptr _ ->
                bCall ("$ptr_to_" + C.Type.IntSuffix k) [self e']
              | _ -> die()
          | C.Expr.Cast ({ Type = C.Type.Bool }, _, e') ->
            match e'.Type with
              | C.Type.Integer _ ->
                match e' with
                  | C.IntLiteral (_, ZeroBigInt) -> bFalse
                  | C.IntLiteral (_, OneBigInt) -> bTrue
                  | _ -> bCall "$int_to_bool" [self e']
              | C.Type.Ptr _ ->
                bCall "$ptr_neq" [self e'; er "$null"]
              | _ -> die()
          | C.Expr.Cast (_, _, e') when expr.Type._IsPtr && e'.Type._IsPtr ->
            bCall "$ptr_cast" [self e'; ptrType expr]
          | C.Expr.Pure (_, e') -> self e'
          | C.Expr.Macro (c1, name, [C.Expr.Prim (c2, C.Op(_, C.Unchecked), _) as inner]) 
              when name.StartsWith "unchecked" && c1.Type = c2.Type -> trExpr env inner
          | C.Expr.Prim (c, C.Op(opName, _), args) when isFloatingPoint c.Type ->
            let suffix = match c.Type with | C.Type.Primitive k -> C.Type.PrimSuffix k | _ -> die()
            let opName' = if args.Length = 1 then "u" + opName else opName
            let funcNameTbl = Map.of_list [ "+", "$add"; "-", "$sub"; "*", "$mul"; "/", "$div"; "u-", "$neg";
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
              | "*" when ch <> C.Unchecked -> bCall "$_mul" args
              | _ -> 
                if ch = C.Unchecked then
                  match opName with
                    | "+" -> bCall "$unchk_add" targs
                    | "-" -> bCall "$unchk_sub" targs
                    | "*" -> bCall "$unchk_mul" targs
                    | "/" -> bCall "$unchk_div" targs
                    | "%" -> bCall "$unchk_mod" targs
                    | _ -> B.Expr.Primitive (opName, args)
                else
                  B.Expr.Primitive (opName, args)
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
            bCall "$idx" [self arr; self idx; ptrType arr]
          | C.Expr.Deref (_, p) -> typedRead bState (self p) expr.Type
          | C.Expr.Call (_, fn, targs, args) ->
            let args =  List.map (toTypeId' true) targs @ convertArgs fn (selfs args)
            let args = 
              if fn.IsStateless then args
              else bState :: args
            addType fn.RetType (bCall ("#" + fn.Name) args)
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
              quantVarTokens.[v] <- c.Token
            let body = self q.Body
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
            let triggers = List.map selfs q.Triggers
            // the nested ptr_cast(ptr(...)) gets into triggers and causes trouble later
            let rec stripPtrCast (expr:B.Expr) = 
              let aux = function
                | B.FunctionCall ("$ptr_cast", [B.FunctionCall ("$ptr", [_; r]); t]) ->
                  Some (stripPtrCast (bCall "$ptr" [t; r]))
                | _ -> None
              expr.Map aux              
            match q.Kind with
              | C.Forall -> B.Forall (vars, triggers, weight "user-forall", stripPtrCast body)
              | C.Exists -> B.Exists (vars, triggers, weight "user-exists", stripPtrCast body)
              | C.Lambda -> die()
          
          | C.Expr.SizeOf(_, C.Type.TypeVar(tv)) ->bCall "$sizeof" [typeVarRef tv]
          | C.Expr.SizeOf(_, t) -> bInt t.SizeOf
          | _ ->         
            helper.Oops (expr.Token, "unhandled expr " + expr.ToString())
            er "$bogus"

      and trMacro env (ec : C.ExprCommon) n args = 
        let self = trExpr env
        let selfs = List.map self
        match n, args with
          | "writes_check", [a] -> writesCheck env ec.Token false a
          | "prim_writes_check", [a] -> writesCheck env ec.Token true a
          | in_range, args when in_range.StartsWith ("in_range") -> bCall ("$" + in_range) (selfs args)
          | ("unchecked_sbits"|"unchecked_ubits"), args ->
            bCall ("$" + n) (selfs args)
          | unchecked, args when unchecked.StartsWith ("unchecked_") -> bCall "$unchecked" (er ("^^" + unchecked.Substring (unchecked.IndexOf '_' + 1)) :: selfs args)
          | "map_get", [a; b] ->
            match a.Type with
              | C.Type.Map (f, t) ->
                let fn = "$select." + (trType a.Type).ToString()
                addType t (bCall fn [self a; stripType f (self b)])
              | _ -> die()
          | "map_updated", [a; b; c] ->
            match a.Type with
              | C.Type.Map (f, t) ->
                let fn = "$store." + (trType a.Type).ToString()
                bCall fn [self a; stripType f (self b); stripType t (self c)]
              | _ -> die()
          | "field", [C.Expr.Dot (_, _, f)] ->
            er (fieldName f)
            
          | "rec_zero", [] -> er "$rec_zero"
          
          | "rec_fetch", [r; C.UserData(_, (:? C.Field as f))] ->
            let fetch = bCall "$rec_fetch" [self r; er (fieldName f)]
            match f.Type with
              | C.Type.Integer _ ->
                bCall "$unchecked" [toTypeId f.Type; fetch]
              | C.Ptr t ->
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
          | "this", [] -> er "$_this"
          | "null", [] -> er "$null"
          | "dont_instantiate", [e] ->
            let arg = self e
            match e.Type with
              | C.Type.Integer _ -> bCall "$dont_instantiate_int" [arg]
              | C.Type.Ptr _
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
          
          | "boogie_quote", [C.Macro (_, s, [])] ->
            ToBoogieAST.parse s
          | ("inv_check" | "token_holder" | "_vcc_bv_lemma"), [e] ->
            self e
          | "ite", ([cond; th; el] as args) ->
            let name =
              match trType th.Type with
                | B.Type.Ref name -> (name.Replace ("$#", "")).Replace ("$", "")
                | B.Type.Int ->
                  if th.Type._IsPtr then "ptr" else "int"
                | B.Type.Bool -> "bool"
                | _ -> die()
            let args =
              match args with
                | C.Expr.Ref (_, { Kind = C.QuantBound }) as first :: rest ->
                  bCall "$bool_id" [self first] :: selfs rest
                | _ -> selfs args
            bCall ("$ite." + name) args
          | "can_use_frame_axiom_of", [C.Call (_, f, _, _)] ->
            bCall "$can_use_frame_axiom_of" [er ("cf#" + f.Name)]
          | "_vcc_typeof", [e] ->
            typeOf env e                
          | "_vcc_containing_struct", [p; C.Dot (_, _, f)] ->
            bCall "$containing_struct" [self p; er (fieldName f)]
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
          | "_vcc_union_active", [p; C.Dot (_, _, f)] ->
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
          | "stackframe", [] -> er "#stackframe"
          | "float_literal", [C.Expr.UserData(_, f)] ->
            match f with
              | :? float as f -> getFloatConst f
              | :? single as s -> getFloatConst ((float)s)
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
          | _ ->
            helper.Oops (ec.Token, sprintf "unhandled macro %s\n" n)
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
      
      and readsCheck env isWf (p:C.Expr) =
        if helper.Options.OmitReadWriteChecking then
          [B.Assert (afmte 8511 "disabled reads check of {0}" [p], bTrue)]
        else
          let cond id msg name (args : C.Expr list) = 
            (afmte id msg args.Tail, trExpr env (C.Macro ({p.Common with Type = C.Bool }, name, args)))
          let (codeTp, codeTh, suff) =
            if isWf then (8501, 8502, " (in well-formedness check)")
            else         (8511, 8512, "")
          let tp =
            if helper.Options.RunTestSuite then []
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
                    // This doesn't work with field inlining.
                    // cond codeTh ("{0} is thread local " + msg + "(accessing field " + f.Name + ")" + suff) "_vcc_thread_local2" p'
                    cond codeTh ("{0} is thread local" + suff) "_vcc_thread_local2" [cState; p]
                tok, bMultiOr (prop :: isAtomic)
              | _ -> 
                let msg, isAtomic =
                  match env.AtomicReads with
                    | [] -> "", []
                    | _ ->
                      " or atomically updated", List.map (fun o -> bCall "$set_in" [trExpr env p; bCall "$span" [o]]) env.AtomicReads
                let tok, prop =
                  cond codeTh ("{0} is thread local" + msg + suff) "_vcc_thread_local2" [cState; p]
                tok, bMultiOr (prop :: isAtomic)
          List.map B.Assert (tp @ [th])
      
      and writesMultiCheck use_wr env tok f =
        let name = "#writes" + tokSuffix tok
        let p = er name
        let precond = 
          if nestingExtents then 
            bAnd (f p) (bOr (bCall "$is_primitive" [bCall "$typ" [p]]) (bCall "$is_non_primitive" [bCall "$typ" [p]]))
          else f p
        B.Expr.Forall ([(name, tpPtr)], dont_inst p, weight "dont-inst", bImpl precond (inWritesOrIrrelevant use_wr env p None))
         
      and inWritesOrIrrelevant use_wr env (e:B.Expr) (origPrim:option<C.Expr>) =
        let prim = origPrim.IsSome
        let atomicWr =
          if prim then
            env.AtomicObjects |>
              List.map (fun o -> bCall "$set_in" [e; bCall "$volatile_span" [bState; o]]) |>
              bMultiOr
          else bFalse
        (*match origPrim with
            | Some (C.Dot (_, p, f)) when f.Parent.Kind = C.Struct ->
              trExpr env p*)
        let pred = if prim || use_wr then "$writable" else "$top_writable"
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
        B.Expr.Forall ([(cs, tpState)], [[use_claim]], weight "claims",
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
          | C.Type.Ptr _ ->
            match t with 
              | C.Type.Ptr _ -> e2'
              | _ -> bCall "$ptr_to_int" [e2']
          | C.Type.Integer _ -> e2'
          | _ ->
            castToInt (trType t) e2'

      let repl e = bSubst [("$$s", er "$s")] e
      
      let trExpr env expr = repl (trExpr env expr)
      let isInWrites e p = repl (isInWrites e p)
      let typeOf env e = repl (typeOf env e)
      let readsCheck env isWf p = 
        List.map (function B.Stmt.Assert (t, e) -> B.Stmt.Assert (t, repl e) | _ -> die()) (readsCheck env isWf p)
      let inWritesOrIrrelevant env expr o = repl (inWritesOrIrrelevant false env expr o)
      let claimIn env claim s expr = repl (claimIn env claim s expr)
      let claims env claim expr = repl (claims env claim expr)
      let trForWrite env t e2 = repl (trForWrite env t e2)
      
      let trMacro = ()
      let writesCheck env tok prim (e:C.Expr) = ()
      let writesMultiCheck env tok f = ()
      let vsTrans env p = ()
      
      
      
      let trLabel (label:C.LabelId) = label.Name

      let stateChanges (env:Env) =
        if env.Writes = [] then
          bCall "$writes_nothing" [env.WritesState; bState]
        else
          let p = er "#p"
          let t = er "#t"
          let inWr = isInWrites env p |> bSubst [("$s", env.WritesState)]
          let base_ = bOr (bCall "$irrelevant" [env.WritesState; p]) inWr
          
          let quant prop =
            let newState = bCall prop [bState; p]
            let stateEq = bCall (prop + "_eq") [env.WritesState; bState; p]
            B.Expr.Forall (["#p", tpPtr], [[newState]], weight ("writes-" + prop.Replace ("$", "")), bOr base_ stateEq)
            
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
        let wr = inWritesOrIrrelevant env bobj None
        let own = List.map (bEq (bCall "$owner" [bState; bobj])) env.AtomicObjects
        let ref_cnt = bCall "$ref_cnt" [bState; bobj]
        let ref_cnt_plus_one =
          B.Expr.Primitive ((if doClaim then "+" else "-"), [ref_cnt; B.Expr.IntLiteral (Math.BigInt.One)])
        let tok = 
          let cl_or_uncl = if doClaim then "claim" else "unclaim"
          afmtet tok 8008 ("{0} is non-writable and its owner is not listed in atomic(...) (and thus is impossible to " + cl_or_uncl + ")") [obj]
        let tok2 = 
          afmtet tok 8009 "type of object {0} was not marked with vcc(claimable)" [obj]
        [B.Stmt.Assert (tok, bMultiOr (wr :: own));
         B.Stmt.Assert (tok2, bCall "$is_claimable" [typeOf env obj]);
         B.Stmt.Call (C.bogusToken, [], "$write_ref_cnt", [bobj; ref_cnt_plus_one]);
         B.Stmt.Assume (B.Expr.Primitive (">=", [ref_cnt; B.Expr.IntLiteral (Math.BigInt.Zero)]))]
               
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
              B.Stmt.Assert (tok, didAlloc (inState s expr))
              
            let mkAdmAssert s (expr:C.Expr) =
              let tok = afmtet tok 8521 "chunk {0} of the claim holds after a step of the machine" [expr]
              B.Stmt.Assert (tok, inState s expr)
            
            let rf n = er (claim + n)
            
            let doObj obj =
              let obj' = trExpr env obj
              let tok' = afmtet tok 8528 "object {0} is closed before claiming it" [obj]
              B.Stmt.Assert (tok', bCall "$closed" [bState; obj']) :: claimedObjCheck env tok true obj
            
            let doClaim (obj:C.Expr) =
              let obj' = trExpr env obj
              let tokWrite = afmtet tok 8023 ("{0} is non-writable and (and thus is impossible to upgrade)") [obj]
              let tokWrap = afmtet tok 8024 "the claim {0} is not wrapped before upgrade" [obj]
              let tokRef = afmtet tok 8025 "the claim {0} has outstanding claims" [obj]
              [B.Assert (tokWrap, bCall "$wrapped" [bState; obj'; er "^^claim"]);
               B.Assert (tokRef, bEq (bCall "$ref_cnt" [bState; obj']) (bInt 0));
               B.Assert (tokWrite, inWritesOrIrrelevant env obj' None)]
            
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
              [B.Stmt.Assume (inState (rf "s1") expr);
               B.Stmt.Assume (bCall "$valid_claim_impl" [rf "s0"; rf "s2"]);
               B.Stmt.Assume (bCall "$claim_transitivity_assumptions" ([rf "s1"; rf "s2"; er claim; er (getTokenConst tok)]));
               ] @
               List.map (mkAdmAssert (rf "s2")) conditions @
               [B.Stmt.Assume bFalse]
            let rand = claim + "doAdm"
            let claimAdm cond =
              [B.Stmt.If (bAnd cond (er rand), B.Stmt.Block (B.Stmt.VarDecl ((rand, B.Type.Bool), None) :: claimAdm), B.Stmt.Block [])]
               
            let initial cond = 
              [B.Stmt.Assume (didAlloc (bCall "$claim_initial_assumptions" [bState; er claim; er (getTokenConst tok)]))] @
              List.map (mkInitAssert bState) conditions @
              claimAdm cond @
              [B.Stmt.Assume (didAlloc (claims env (er claim) expr))]
            
            let claims_obj = List.map (fun e -> B.Stmt.Assume (bCall (if upgrade then "$claims_upgrade" else "$claims_obj") [er claim; trExpr env e])) objects 
            
            let assign = 
              [B.Stmt.Assign (varRef local, bCall "$ref" [er claim]);
               assumeLocalIs tok local;
               assumeSync env tok]
                                      
            let initial =
              match env.AtomicObjects with
                | [] -> initial bTrue
                | _ ->
                  let ctx = env.ClaimContext.Value
                  let cond = bNeq (er claim) (er "$no_claim")
                  ctx.ClaimChecks <- initial cond @ ctx.ClaimChecks
                  ctx.ClaimInits <- B.Stmt.Assume (bEq (er claim) (er "$no_claim")) :: ctx.ClaimInits 
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
        let name = "#wrTime" + tokSuffix tok
        let p = er "#p"
        let inWritesAt = bCall "$in_writes_at" [er name; p]
        let env = { env with Writes = wr; WritesTime = er name }
        let init =
          [B.Stmt.VarDecl ((name, B.Type.Int), None);
           B.Stmt.Assume (bEq (er name) (bCall "$current_timestamp" [bState]));
           B.Stmt.Assume (B.Expr.Forall ([("#p", tpPtr)], [[inWritesAt]], weight "begin-writes", bEq inWritesAt (isInWrites env p)))]
        (init, env)
                  
      let trUnclaim env tok = function
        | claim :: objects ->
          let claim' = trExpr env claim
          let doObj obj =
            let obj' = trExpr env obj
            let tok = afmtet tok 8522 "object {0} was claimed by {1}" [obj; claim]
            B.Stmt.Assert (tok, bCall "$claims_obj" [claim'; obj']) :: claimedObjCheck env tok false obj
          let tr = trExpr env
          let rec different acc = function
            | x :: xs ->
              let mkDiff y =
                let tok = afmtet tok 8010 "object {0} might equal {1}" [x; y]
                B.Stmt.Assert (tok, bNeq (tr x) (tr y))
              different (List.map mkDiff xs @ acc) xs
            | [] -> acc
          let allowWrite = 
            B.Stmt.Assert (afmtet tok 8523 "the disposed claim {0} is writable" [claim], inWritesOrIrrelevant env claim' None)
          let different = different [] objects
          let decrements = List.map doObj objects |> List.concat
          let call = B.Stmt.Call (tok, [], "$unclaim", [claim'])
          allowWrite :: different @ [call] @ decrements @ [assumeSync env tok]
        | _ -> die()
      
      let trAtomic trStmt env (ec:C.ExprCommon) objs body =
        let rec split acc = function
          | C.Stmt (_, C.Macro (_, "begin_update", [])) :: xs -> List.rev acc, xs
          | C.Block (_, xs) :: xs' -> split acc (xs @ xs')
          | x :: xs -> split (x :: acc) xs
          | [] -> [], List.rev acc
        
        let getType (obj : C.Expr) (bobj : B.Expr)=
          match obj.Type with
            | C.Type.Ptr(_) -> ptrType obj
            | C.Type.ObjectT -> bCall "$typ" [bobj]
            | _ -> die()
          
        let (objs, claims) = List.partition (fun (e:C.Expr) -> e.Type <> C.Ptr C.Claim) objs
        let (before, after) =
          match body with
            | C.Block (_, lst) -> split [] lst
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
                B.Stmt.Assert (tok, trExpr env' e |> bSubst [("$_this", bobj)])
              td.Invariants |> List.map TransUtil.splitConjunction |> List.concat |> List.map mkAssert
            | _ ->
              [B.Stmt.Assert (afmte 8525 "invariant of {0} holds after atomic" [obj],
                              bCall "$inv2" [oldState; bState; bobj; getType obj bobj])]
        
        let valid_claims =
          [for c in claims ->
            B.Stmt.Assert (afmte 8526 "claim {0} is valid" [c],
                           bCall "$valid_claim" [bState; trExpr env c])]
                           
        let before =
          if before = [] then []
          else valid_claims @ flmap (trStmt preEnv) before
        
        // this sets env'.ClaimContext things
        let atomicAction = flmap (trStmt env') after
          
        [B.Stmt.Call (ec.Token, [], "$atomic_havoc", []);
         assumeSync env ec.Token] @ 
        atomicInits @
        before @
        valid_claims @
        [for (bobj, obj) in atomicObjs do
             yield B.Stmt.Assert (afmte 8527 "{0} is closed (for atomic(...))" [obj], bCall "$closed" [bState; bobj])
             yield B.Stmt.Assume (bCall "$inv" [bState; bobj; getType obj bobj])
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
        let cmt () = B.Stmt.Comment ((stmt.ToString ()).Replace ("\n", " "))
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
              | [p; B.Expr.FunctionCall ("$dot", [_; f])], "$union_reinterpret" -> [p; f]
              | _ -> args
          let resBuf, tail = 
            if resultIsObjT <> varIsObjT then
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
              [tmp], [vardecl; assign]
            else List.map varName res, []
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
        
        match stmt with
          | C.Expr.Block (_, stmts) -> 
            List.concat (List.map self stmts)
          | C.Expr.Comment (_, s) -> 
            [B.Stmt.Comment s]
          | C.Expr.Assert (_, C.Expr.Macro (_, "_vcc_bv_lemma", [e])) -> 
            [cmt (); B.Stmt.Assert (stmt.Token, trBvExpr env e)]
//          | C.Expr.Assert (_, C.Expr.Macro (_, "reads_check_cond_wf", [cond; e])) ->
//            let addCond = function
//              | B.Assert (t, e) -> B.Assert (t, bImpl (trExpr env cond) e)
//              | _ -> die()
//            cmt () :: (List.map addCond (readsCheck env true e))
//          | C.Expr.Assert (_, C.Expr.Macro (_, "reads_check_wf", [e])) ->
//            cmt () :: readsCheck env true e
          | C.Expr.Assert (_, C.Expr.Macro (_, "reads_check_normal", [e])) ->
            cmt () :: readsCheck env false e            
          | C.Expr.Assert (_, e) -> 
            // In general this is a good idea, but FELT sometimes creates expressions
            // with dummy tokens and we can do nothing about it
            //if e.Token.line = 0 then
            //  failwith ("trying to put dummy token on assertion " + e.ToString())
            [cmt (); B.Stmt.Assert (stmt.Token, trExpr env e)]
          | C.Expr.Assume (_, e) -> 
            [cmt (); B.Stmt.Assume (trExpr env e)]
          | C.Expr.Return (c, s) ->
            match s with
            | None -> [cmt (); B.Stmt.Assert (c.Token, bCall "$position_marker" []); B.Stmt.Goto (c.Token, ["#exit"])]
            | (Some e) -> 
              [cmt (); B.Stmt.Assign (B.Expr.Ref "$result", stripType e.Type (trExpr env e)); B.Stmt.Assert (c.Token, bCall "$position_marker" []); B.Stmt.Goto (c.Token, ["#exit"])]
          | C.Expr.Macro (_, "havoc", [e ;t]) ->
            [cmt (); B.Stmt.Call (e.Token, [], "$havoc", [trExpr env e; trExpr env t]); assumeSync env e.Token] @ (cevStateUpdate e.Token)
          | C.Expr.MemoryWrite (_, e1, e2) ->
            let e2' =
              match e1.Type with
                | C.Ptr t -> trForWrite env t e2
                | _ -> die()
            [cmt (); 
             B.Stmt.Call (C.bogusToken, [], "$write_int", [trExpr env e1; e2']); 
             assumeSync env e1.Token] @ (cevStateUpdate e1.Token)
            
          | C.Expr.VarWrite (_, [v], C.Expr.Macro (c, "claim", args)) ->
            cmt() :: trClaim env false c.Token v args @ (cevVarUpdate c.Token true v)
            
          | C.Expr.VarWrite (_, [v], C.Expr.Macro (c, "upgrade_claim", args)) ->
            cmt() :: trClaim env true c.Token v args @ (cevVarUpdate c.Token true v)
            
          | C.Expr.Stmt (_, C.Expr.Macro (c, "unclaim", args)) ->
            cmt() :: trUnclaim env c.Token args
          
          | C.Expr.Atomic (ec, objs, body) ->
            trAtomic trStmt env ec objs body
            
          | C.Expr.VarWrite (_, vs, C.Expr.Call (c, fn, targs, args)) -> 
            let cevlist = cevFunctionCall c.Token
            let cevlist = if List.isEmpty vs then cevlist else cevlist @ (cevVarUpdateList c.Token vs) @ (cevStateUpdate c.Token)
            doCall c vs (Some fn) fn.Name targs args @ List.map (fun v -> assumeLocalIs c.Token v) vs @ cevlist
            
          | C.Expr.Stmt (_, C.Expr.Call (c, fn, targs, args))        -> 
            let cevList = cevFunctionCall c.Token
            let stateUpdate = cevStateUpdate c.Token
            doCall c [] (Some fn) fn.Name targs args @ cevList @ stateUpdate         
          | C.Expr.Macro (c, (("_vcc_reads_havoc"|"_vcc_havoc_others"|"_vcc_unwrap_check"|
                                "_vcc_static_wrap"|"_vcc_static_wrap_non_owns"|"_vcc_static_unwrap") as name), args) -> 
            doCall c [] None name [] args
          | C.Expr.Stmt (_, C.Expr.Macro (c, (("_vcc_unwrap"|"_vcc_wrap"|"_vcc_deep_unwrap"|"_vcc_from_bytes"|"_vcc_to_bytes") as name), args)) ->
            doCall c [] None name [] args         
            
          | C.Expr.VarWrite (c, [v], e) ->
            [cmt (); B.Stmt.Assign (varRef v, stripType v.Type (trExpr env e)); assumeLocalIs c.Token v] @ (cevVarUpdate c.Token true v)
          
          | C.Expr.If (_, c, s1, s2) ->
            let prefix = cevCondMoment c.Token
            let thenBranch = cevBranchChoice c.Token (er "took_then_branch")
            let elseBranch = cevBranchChoice c.Token (er "took_else_branch")
            prefix @
            [B.Stmt.Comment ("if (" + c.ToString() + ") ..."); 
             B.Stmt.If (trExpr env c, B.Stmt.Block (thenBranch @ trStmt env s1), B.Stmt.Block (elseBranch @ trStmt env s2))]
          | C.Expr.Loop (comm, invs, writes, s) ->
            let (save, oldState) = saveState "loop"
            let env = { env with OldState = oldState }
            let condMoment = cevCondMoment stmt.Token
            let regLoopBody = cevRegLoopBody stmt s
            let (bump, wrCheck, env) =
              match writes with
                | [] -> ([], [], env)
                | fst :: _ ->
                  let env' = { env with WritesState = oldState }
                  let (init, env') = setWritesTime fst.Token env' writes
                  let name = "#loopWrites^" + (tokSuffix fst.Token)
                  let p = er name
                  let impl = bImpl (inWritesOrIrrelevant env' p None) (inWritesOrIrrelevant env p None)
                  let tok = afmtet fst.Common.Token 8011 "writes clause of the loop might not be included writes clause of the function" []
                  let bump =  [B.Stmt.Call (tok, [], "$bump_timestamp", []); assumeSync env tok]
                  let check = [B.Stmt.Assert (tok, B.Forall ([name, tpPtr], [[bCall "$dont_instantiate" [p]]], weight "dont-inst", impl))]
                  (bump, init @ check, env')
            let tok, vlist = lastStmtVars stmt []
            let arbitraryLoopIter = cevVarUpdateList comm.Token vlist
            let body =
              B.Stmt.While (bTrue, 
                List.map (fun (e:C.Expr) -> (e.Token, trExpr env e)) invs,
                B.Stmt.Block (B.Stmt.Assume (stateChanges env) :: 
                B.Stmt.Assume (bCall "$timestamp_post" [env.OldState; bState]) ::
                assumeSync env comm.Token :: 
                List.map (assumeLocalIs comm.Token) !soFarAssignedLocals @
                  regLoopBody @ arbitraryLoopIter @
                  trStmt env s))
            bump @ save @ wrCheck @ [body; assumeSync env comm.Token]
              
          | C.Expr.VarDecl (b, v) ->
            let ls = if v.Kind = C.Parameter then cevVarIntro b.Token true v else []
            if v.Kind = C.Parameter || v.Kind = C.SpecParameter || v.Kind = C.OutParameter then []
            else
              let (v, w) = trWhereVar v
              [cmt(); B.Stmt.VarDecl (v, w)] @ ls

          | C.Expr.Goto (c, l) -> [cmt (); B.Stmt.Goto (c.Token, [trLabel l])]
          | C.Expr.Label (c, l) -> [B.Stmt.Label (c.Token, trLabel l)]
          
          | C.Expr.Macro (_, "ignore_me", []) -> []
          | C.Expr.Macro (_, "inlined_atomic", [C.Expr.Macro (_, "ignore_me", [])]) -> []

          | C.Expr.Macro (_, "function_cleanup", args) ->
            env.FunctionCleanup := !env.FunctionCleanup @ args
            []
          
          | e when not (hasSideEffect e) -> []
          
          | _ -> 
            helper.Oops (stmt.Token, "unhandled stmt " + stmt.ToString())
            []
      
      let trHeader (header:C.Function) =
        let env = { initialEnv with Writes = header.Writes }
        let te e = trExpr env e
        let pureEq =
          if header.IsPure && header.RetType <> C.Void then 
            let parms = 
              (if header.IsStateless then [] else [bState]) @ [for tv in header.TypeParameters -> typeVarRef tv] 
                                                            @ [for v in header.Parameters -> varRef v]
            let tok = TransUtil.afmtt header.Token 8022 "the pure function '{0}' is underspecified; please supply ensures(result == ...) contract matching the implementation" [header.Name]
            [B.Ensures (tok.Token, bEq (er "$result") (bCall ("#" + header.Name) parms))]
          else []
        let (writes, ensures) =
          let check (writes, ensures) = function
            | C.Call (_, { Name = "_vcc_public_writes" }, _, [s]) ->
              (s :: writes, ensures)
            | e -> (writes, e :: ensures)
          match List.fold check ([], []) header.Ensures with
            | ([], e) -> (header.Writes, e)
            | acc -> acc
        let stateCondition =
          if header.IsPure then
            if writes <> [] then helper.Error (header.Token, 9623, "writes specified on a pure function", None)
            []
          else
            [B.FreeEnsures (stateChanges { env with Writes = writes });                    
             B.Modifies "$s";
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
                    | C.SkipVerification -> yield (B.ExprAttr ("verify", bFalse))
                    | C.NoAdmissibility -> yield! []
                    | C.IsAdmissibilityCheck -> yield! []
                    | C.VccAttr ("extra_options", o) -> yield (B.StringAttr ("vcc_extra_options", o))
                    | C.VccAttr _ -> yield! []
                    | C.GroupDeclAttr _ -> yield! []
                    | C.InGroupDeclAttr _ -> yield! []
                    | C.ReadsCheck _ -> yield! []
                     
              ] } : B.ProcData
        (proc, env)

      let toFieldRef (f:C.Field) =
        let fieldRef = er (fieldName f)
        fieldRef
        

      let trField (td:C.TypeDecl) (f:C.Field) =
        let toBaseType (f:C.Field) =
          let baset = 
            match f.Type with
              | C.Array (t, _) -> t
              | t -> t
          baset
          
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
        
        let forallRM id trig body = B.Expr.Forall ([pv; ("#s", tpState)], trig, weight id, body)              
        let fieldRef = toFieldRef f
        let baset = toBaseType f
        let dot = bCall "$dot" [p; fieldRef]
        let dott = toTypeId baset
        let ptrref = 
          if f.IsSpec then bCall "$ghost_ref" [p; fieldRef]            
          else           
            if td.GenerateFieldOffsetAxioms || helper.Options.GenerateFieldOffsetAxioms then
              B.Primitive ("+", [bCall "$ref" [p]; bInt f.ByteOffset])
            else
              bCall "$physical_ref" [p; fieldRef]
        let dotdef = bAnd (bEq dot (bCall "$ptr" [dott; ptrref])) (bCall "$extent_hint" [dot; p])
        let dotdef = if useIs then bInvImpl (bCall "$is" [p; we]) dotdef else dotdef
        let dotdef = B.Forall ([pv], [[dot]], weight "def-field-dot", dotdef)
        // ghost fields we treat alike regardless if in union or struct
        let isUnion = if f.IsSpec then false else isUnion          
        
        let fieldoff = bEq (bCall "$field_offset" [fieldRef]) (bInt f.ByteOffset)
        
        let isActive = bEq (bCall "$active_option" [s; p]) fieldRef
        let noInline = if TransUtil.hasBoolAttr "no_inline" f.CustomAttr then "no_inline_" else ""
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
                if TransUtil.hasBoolAttr "no_inline" f.CustomAttr then
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
           
           
        let fldOffsetAxioms =
          //if td.GenerateFieldOffsetAxioms || helper.Options.GenerateFieldOffsetAxioms then
            (if f.IsSpec then [] else [B.Decl.Axiom fieldoff]) @ [B.Decl.Axiom dotdef]
          //else []

        [B.Decl.Const ({ Name = fieldName f
                         Type = B.Type.Ref "$field"
                         Unique = true } : B.ConstData)] @
        primVolatile() @
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
        let eqFun = B.Function(B.Type.Bool, [], eqFunName td.Name deepStr, vars)
        let typeRef = toTypeId (C.Type.Ref td)
        let fldEqual inUnion (f : C.Field) =
          let rec read arrayElementType v = 
            let state = bCall "$vs_state" [v]
            let fldAccess = 
              match arrayElementType with
                | None -> fun v f -> bCall "$dot" [bCall "$vs_base" [v; typeRef]; er (fieldName f)]
                | Some t -> fun v f -> bCall "$idx" [ bCall "$dot" [bCall "$vs_base" [v; typeRef]; er (fieldName f)]; idx; toTypeId t]
            function
              | C.Integer k -> bCall ("$read_" + C.Type.IntSuffix k) [state; fldAccess v f]
              | C.Ptr t -> bCall "$read_ptr" [state; fldAccess v f; toTypeId t]
              | C.Bool -> bCall "$read_bool" [state; fldAccess v f]
              | C.Map(_,_) -> bCall "$read_any" [state; fldAccess v f]        
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
              Some(B.Forall([("#i", B.Int)], [], weight "array-structeq", cond))
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
        let eqForall = B.Forall(vars, [[eqCall]], weight "eqdef-structeq", bEq eqCall eqExpr)
        [eqFun; B.Axiom eqForall]


      let imax x y = if x < y then y else x
      let imin x y = if x < y then x else y
      
      let typeNesting types =
        let withNest = List.map (fun td -> (td, typeDepth (C.Type.Ref td))) types  
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
            | C.Array _ when TransUtil.hasBoolAttr "no_inline" f.CustomAttr -> true
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
        let is_thread_local = ref false
          
        List.iter (function C.VccAttr ("claimable", _) -> is_claimable := true
                          | C.VccAttr ("volatile_owns", _) -> owns_set_is_volatile := true
                          | C.VccAttr ("thread_local_storage", _) -> is_thread_local := true
                          | _ -> ()) td.CustomAttr
                                             
        if !is_claimable && !is_thread_local then
          helper.Error(tok, 9665, "Type '" + td.Name + "' cannot be marked as both claimable and thread_local_storage")
                                             
        let volatile_owns = B.Decl.Axiom (bEq (bCall "$has_volatile_owns_set" [we]) (B.Expr.BoolLiteral !owns_set_is_volatile))
        let claimable = B.Decl.Axiom (bEq (bCall "$is_claimable" [we]) (B.Expr.BoolLiteral !is_claimable))
        let threadLocal = 
          if !is_thread_local then [B.Decl.Axiom (bCall "$is_thread_local_storage" [we])]
          else []
           
           
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
        let inv exprs = bMultiAnd (bCall "$typed" [s2; p] :: (exprs |> List.map doInv) ) 
        let invcall = bCall "$inv2" s1s2pwe
        let typedPtr = bCall "$ptr" [we; r]
        let invlabcall lbl = bCall "$inv_lab" [s2; typedPtr; er (trInvLabel lbl)]
        let normalizeLabeledInvariant = bSubst [("#s1", er "#s2"); ("#p", typedPtr)] >> removeTrivialEqualities
        let invlab (lbl,exprs) = 
          let invcall = invlabcall lbl
          B.Forall(s2r, [[invcall]], weight "eqdef-inv", (bEq (invcall) (inv exprs |> normalizeLabeledInvariant)))
        let inv = B.Forall (s1s2p, [[invcall]], weight "eqdef-inv", (bEq invcall (inv (td.Invariants |> List.map stripLabel))))
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
                if TransUtil.hasBoolAttr "no_inline" fld.CustomAttr then
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
          B.Forall (qvars, [[bExtentCall]], weight ("eqdef-extent-" + extentName.Replace ("$", "")), bEq bExtentCall body)

        let spansCalls (fields:list<C.Field>) =
          let maybeArrayLift r f prop =
            let dot = bCall "$dot" [r; toFieldRef f]
            match f.Type with
              | C.Type.Array (t, sz) ->
                let idx = bCall "$idx" [dot; er "#i"; toTypeId t]
                B.Forall ([("#i", B.Type.Int)], [[idx]], weight "array-span",
                       bInvImpl (bCall "$in_range" [bInt 0; er "#i"; bInt (sz - 1)]) (prop idx))
              | _ -> prop dot

          let auxDot r f =
            bCall "$dot" [r; toFieldRef f] 
          let qvars = [("#p", tpPtr); ("#s1", tpState); ("#s2", tpState)]
          let args = [s1; s2; p; we]
          let bSpansCall = bCall "$state_spans_the_same" args
          let bNonVolatileSpansCall = bCall "$state_nonvolatile_spans_the_same" args
          let mkForall call fields =
            B.Forall (qvars, [[call]], weight "eqdef-span", bEq call 
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
                if TransUtil.hasBoolAttr "no_inline" fld.CustomAttr then
                  prop (bCall "$as_array" [dot; toTypeId t; bInt sz])
                else 
                  let idx = bCall "$idx" [dot; er "#i"; toTypeId t]
                  let fieldProp = if t.IsComposite then prop else primFieldProp
                  B.Forall ([("#i", B.Type.Int)], 
                         //[[bCall ("$" + propName) (states @ [idx])]],
                         [[idx]], weight "array-extentprop",
                         bInvImpl (bCall "$in_range" [bInt 0; er "#i"; bInt (sz - 1)]) (fieldProp idx))
              | t -> if t.IsComposite then prop dot else primFieldProp dot
              
          let allHaveProp = bMultiAnd (List.map (function fld -> bInvImpl (typedCond r fld) (hasProp fld (auxDot r fld))) fields)
          let body = if includeSelf then bAnd (bCall ("$" + propName) (states @ [auxPtr r])) allHaveProp else allHaveProp
          let bExtentCall = prop (auxPtr r)
          B.Forall (qvars, [[bExtentCall]], weight "eqdef-extentprop", bEq bExtentCall body)
        
        
        let allFields = 
          match td.Fields with
            | [] -> []
            | lst -> ({ Name = "$owns" 
                        Token = td.Token
                        Type = C.Type.PtrSet
                        Parent = td 
                        IsSpec = true 
                        IsVolatile = !owns_set_is_volatile
                        Offset = C.FieldOffset.Normal 0 
                        CustomAttr = [] } : C.Field) :: lst
        let primFields = List.filter (function fld -> not (isComp (fld))) allFields
        let in_full_extent_of = extentCall "$in_full_extent_of" false false allFields
        
        let in_extent_of = 
          if nestingExtents then
            let q = bCall "$ptr" [we; er "#r"]
            let in_ext = bCall "$in_extent_of" [er "#s"; er "#p"; q] 
            let depth = typeDepth (C.Type.Ref td)
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
              B.Forall (["#s", tpState; "#p", tpPtr; "#r", B.Int], 
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
                  | B.Forall (vars, [[extent]], attrs, body) ->
                    let us = bCall "$ptr" [we; er "#r"]
                    let d1 = B.Forall (vars, [[extent]], attrs, bEq extent (bCall "$in_struct_extent_of" [er "#q"; us]))
                    if typeDepth (C.Type.Ref td) = 2 then
                      let consq = bOr (bEq (er "#q") us) (bEq (bCall "$emb" [er "#s"; er "#q"]) us)
                      bAnd d1 (B.Forall (vars, [[extent]], attrs, bInvImpl (bCall "$typed" [er "#s"; us]) (bEq extent consq)))
                    else d1
                  | _ -> die()
              else def        
        
        let in_span_of = extentCall "$in_span_of" false false primFields   
        let ignorePrimField _ = bTrue     
        let readAnyIsZero field = bEq (bCall "$read_any" [ er "#s1"; field ]) (bInt 0)
        let extentProps = List.map B.Decl.Axiom
                              [extentProp "mutable" false isUnion true ignorePrimField allFields;
                               extentProp "is_fresh" true isUnion true ignorePrimField allFields;
                               extentProp "zero" false isUnion false readAnyIsZero td.Fields ]
        
        let (state_spans_the_same, state_nonvolatile_spans_the_same) = spansCalls primFields
        
        let firstOptionTyped =
          let ptr = bCall "$ptr" [we; r]
          let firstOptionTypedCall = bCall "$first_option_typed" [s; ptr]
          let firstOptionTypedAxiom f =
            B.Decl.Axiom (B.Expr.Forall ([("#r", B.Type.Int); ("#s", tpState)], 
                                         [[firstOptionTypedCall]], 
                                         weight "typedef-union_active",
                                         bInvImpl firstOptionTypedCall (bCall "$union_active" [s; ptr; toFieldRef f])))
          match List.tryFind (fun (f:C.Field) -> not f.IsSpec) allFields with
            | Some f when isUnion -> [firstOptionTypedAxiom f]
            | _ -> []
        
        let forward =
          [B.Decl.Const { Unique = true
                          Type = tpCtype
                          Name = "^" + td.Name };
           B.Decl.Axiom (bCall "$is_composite" [we]);
           B.Decl.Axiom (bEq (bCall "$ptr_level" [we]) (bInt 0))] @
           firstOptionTyped
        let forward = 
          match td.GenerateEquality with
          | C.StructEqualityKind.NoEq -> forward
          | C.StructEqualityKind.ShallowEq -> (trStructEq false td) @ forward
          | C.StructEqualityKind.DeepEq -> (trStructEq true td) @ (trStructEq false td) @ forward
        match td.Fields with
          | [] -> forward
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
                 ] @ threadLocal 
                   @ extentProps 
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
            let ite = B.Decl.Function (t, [B.StringAttr ("bvint", "ITE"); B.StringAttr ("bvz", "ITE"); B.StringAttr ("external", "ITE")], 
                                       "$ite." + origname, [("cond", B.Type.Bool); ("th", t); ("el", t)])
            let typeDef = [B.Decl.TypeDef name; ite]
            let (additions, kind) = 
              match td.Kind with
                | C.FunctDecl _ -> (typeDef, "fnptr")
                | C.Record -> (trRecord td, "record")
                | _ -> (typeDef, "math")
                        
            [B.Decl.Const { Unique = true
                            Name = "^" + name
                            Type = tpCtype };           
             B.Decl.Axiom (bCall ("$is_" + kind + "_type") [er ("^" + name)])
             ] @ additions

      let trPureFunction (h:C.Function) =
        if not h.IsPure || h.RetType = C.Void then []
        else
          let env = { initialEnv with Writes = h.Writes }
          let te e = trExpr env e
          let fname = "#" + h.Name
          let retType = trType h.RetType
          let parameters =  List.map trTypeVar h.TypeParameters @ List.map trVar h.Parameters
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
          let ensures = bMultiAnd (List.map (stripFreeFromEnsures>> te) h.Ensures)
          let qargs = (if h.IsStateless then [] else [("#s", tpState)]) @ parameters
          let fappl = bCall fname (List.map (fun ((vi,vt):B.Var) -> er vi) qargs)
          let subst = bSubst [("$result", fappl); ("$s", er "#s")]
          let defBody = subst ensures
          if h.IsStateless && bContains "#s" defBody then
            helper.Error (h.Token, 9650, "the specification refers to memory, but function is missing a reads clause", None)
          let defAxiom =
            if (defBody = bTrue) then [] 
            else if qargs = [] then [B.Decl.Axiom defBody]
            else [B.Decl.Axiom (B.Expr.Forall(qargs, [[fappl]], weight "eqdef-userfun", defBody))]
          let fnconst = "cf#" + h.Name
          let defconst = B.Decl.Const { Unique = true; Name = fnconst; Type = B.Type.Ref "$pure_function" }
          let frameAxiom =
            let containsGenerateFrameAxiom = _list_mem (C.VccAttr("frameaxiom", "")) h.CustomAttr
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
                  | _ ->
                    helper.Error (e.Token, 9619, "non-pointer reads clauses are not supported", None)
                    (bTrue, er "$bogus", tpVersion)
              let (conds, refs, types) = List.unzip3 (List.map readsRef h.Reads)
              let framename = fname + "#frame"
              let framedecl = B.Decl.Function (retType, [], framename, List.map (fun t -> ("", t)) types)
              
              let pre = bMultiAnd (bCall "$full_stop" [er "#s"] :: bCall "$can_use_frame_axiom_of" [er fnconst] :: conds)
              let post = bEq fappl (bCall framename refs)
              [framedecl; B.Decl.Axiom (B.Expr.Forall(qargs, [[fappl]], weight "frameaxiom", subst (bImpl pre post)))]
          let typeInfo =
            let arg i t = B.Decl.Axiom (bCall "$function_arg_type" [er fnconst; bInt i; toTypeId t])
            arg 0 h.RetType :: (h.Parameters |> List.mapi (fun i v -> arg (i + 1) v.Type))
          [B.Decl.Function (retType, [], fname, qargs); defconst] @ defAxiom @ frameAxiom @ typeInfo

      let sanityChecks env (h:C.Function) =
        // we disable that by default for now, it seems to be too much of a hassle
        if not (_list_mem (C.CustomAttr.VccAttr ("postcondition_sanity", "true")) h.CustomAttr) then []
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
                      | B.Stmt.Assert (t, e) :: _ -> B.Stmt.Assert (t, e |> subst)
                      | _ -> die()
                  | _ -> B.Stmt.Assert (e.Token, trExpr env e |> subst)
              let assumes = h.Ensures |> List.map (stripFreeFromEnsures>> trExpr env >> subst >> B.Stmt.Assume)
              let state = B.Assume (stateChanges env |> subst)
              let goodstuff = B.Assume (bCall "$full_stop" [bState] |> subst)
              [B.Stmt.VarDecl ((var, B.Type.Bool), None);
               B.Stmt.VarDecl (("#sanityState", tpState), None);
               B.Stmt.If (er var, B.Stmt.Block (assumes @ state :: goodstuff :: List.map mkAssert lst), B.Stmt.Block []);
               B.Stmt.Assume (bNot (er var))]
        
      let hasStartHere (stmt:B.Stmt) =
        let found = ref false
        let repl = function
          | B.Assume (B.FunctionCall ("$start_here", [])) ->
            found := true
            None          
          | _ -> None
        B.mapStmt repl stmt |> ignore
        !found
      
      let trTop decl =      
        match decl with
          | C.Top.FunctionDecl h ->
            if h.Name.StartsWith "_vcc_" && not (h.Name.StartsWith "_vcc_match") then 
              []
            else
              soFarAssignedLocals := []
              let (proc, env) = trHeader h
              let (init, env) = setWritesTime h.Token env h.Writes              
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
                            | None ->
                              List.map (fun n -> [bCall n [bState; p]]) ["$st"; "$ts"]
                        B.Expr.Forall ([("#p", tpPtr)], triggers,
                                       weight "begin-writes2",
                                       bInvImpl (bCall "$set_in" [p; e']) (bCall name [bState; p]))

                      match e' with
                        | B.Expr.FunctionCall (("$set_universe" | "$set_empty"), []) ->
                          // writes(set_universe()) is for debugging, so disable the immediate false that we could conclude
                          // writes(set_empty()) doesn't mean anything, so also ignore it
                          bTrue
                        | B.Expr.FunctionCall (("$struct_extent" | "$extent" | "$full_extent"), args) ->
                          mut (Some (List.hd (List.rev args))) "$mutable"
                        | _ -> mut None "$thread_owned_or_even_mutable"
                    | _ -> bTrue
                B.Stmt.Assume assump
              let cevInit = cevInitCall h.Token
              let cevInit = cevInit @ List.fold (fun accum -> fun v -> (cevVarIntro h.Token true) v @ accum) [] (List.filter (fun (v : C.Variable) -> v.Kind <> C.VarKind.OutParameter) h.Parameters)
              let _ = if helper.Options.PrintCEVModel then cevNIncr ()
              let init = List.map (assumeLocalIs h.Token) (List.filter (fun (v : C.Variable) -> v.Kind <> C.VarKind.OutParameter) h.Parameters) @ init @ cevInit
                  
              let can_frame =
                if List.exists (function C.ReadsCheck _ -> true | _ -> false) h.CustomAttr then []
                else
                  [B.Stmt.Assume (bCall "$can_use_all_frame_axioms" [bState])]
              
              let doBody s =
                let cleanup = ref []
                B.Stmt.Block (B.Stmt.Assume (bCall "$function_entry" [bState]) ::
                               B.Stmt.VarDecl(("#stackframe", B.Type.Int), None) ::
                               assumeSync env h.Token ::
                               can_frame @
                               init @
                               List.map assumeMutability h.Writes @
                               trStmt {env with FunctionCleanup = cleanup} s @ 
                               [B.Stmt.Label (Token.NoToken, "#exit")] @
                               (!cleanup |> List.map (trStmt env) |> List.concat) @
                               sanityChecks env h) 
              let theBody =
                if functionToVerify = null || functionToVerify = h.Name then h.Body
                else None
              let proc = { proc with Body = Option.map doBody theBody }
              let proc =
                match proc.Body with
                  | Some b when hasStartHere b ->
                    { proc with Attributes = B.Attribute.ExprAttr ("has_start_here", bTrue) :: proc.Attributes }
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
              //XXX if !seenState then (bImpl (bCall "$good_state" [er "#s"]) res), [("#s", tpState)] else res, []
              if !seenState then res, [("#s", tpState)] else res, []
            let res =
              match vars, res with
                | [], _ -> res
                | _, B.Expr.Forall (vars2, triggers, attrs, body) ->
                  B.Expr.Forall (vars @ vars2, triggers, attrs, body)
                | _, _ -> 
                  B.Expr.Forall (vars, [], weight "user-axiom", res)
            [B.Decl.Axiom res]
          | C.Top.Global ({ Kind = C.ConstGlobal ; Type = C.Ptr t } as v, _) ->
            [B.Decl.Const ({ Unique = true; Name = varName v; Type = B.Type.Int })]
          | C.Top.Global _ -> die()

      
      let main () =
        let res = List.map trTop decls
        
        let types = List.fold (fun acc -> function
                                     | C.Top.TypeDecl ({ Kind = (C.Union|C.Struct)} as td) -> td :: acc
                                     | _ -> acc) [] decls
        let tn = if nestingExtents then typeNesting types else []
        List.concat (res @ [tn; !invLabelConstants; !tokenConstants])
        
      helper.SwTranslator.Run main ()
