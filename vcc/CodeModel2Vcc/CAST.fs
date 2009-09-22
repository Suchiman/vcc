//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

module Microsoft.Research.Vcc.CAST
  open System.Text
  open Microsoft.FSharp.Math
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  
  type Id = string

  type VarKind =    
    | Parameter
    | SpecParameter
    | OutParameter
    | Local
    | SpecLocal
    | Global
    | ConstGlobal
    | QuantBound
    
  type QuantKind =
    | Forall
    | Exists
    | Lambda
    // sum and stuff here
  
  let bogusToken = Token.NoToken
  
  let mathTypeCache = new Dict<string, obj>()
  
  type FieldOffset =
    | Normal of int
    | BitField of int * int * int // byte-offset, bit-offset, bit-size
  
  type IntKind =
    | UInt8
    | Int8
    | UInt16
    | Int16
    | UInt32
    | Int32
    | UInt64
    | Int64
    
    member this.SizeSign =
      match this with
        | IntKind.UInt8  -> (8, false)
        | IntKind.UInt16 -> (16, false)
        | IntKind.UInt32 -> (32, false)
        | IntKind.UInt64 -> (64, false)
        | IntKind.Int8   -> (8, true)
        | IntKind.Int16  -> (16, true)
        | IntKind.Int32  -> (32, true)
        | IntKind.Int64  -> (64, true)
    
    override this.ToString () =
      let (sz, sign) = this.SizeSign
      (if sign then "int" else "uint") + sz.ToString() + "_t"

  type PrimKind =
    | Float32
    | Float64

  type StructEqualityKind =
    | NoEq
    | ShallowEq
    | DeepEq

  type LabelId = 
    {
      Name:string;
    }
  
  type CustomAttr =
    | SkipVerification
    | IsAdmissibilityCheck
    | NoAdmissibility
    | IntBoogieAttr of string * int
    | BoolBoogieAttr of string * bool
    | VccAttr of string * string
    | GroupDeclAttr of string
    | InGroupDeclAttr of string
    | ReadsCheck of Function

  and TypeKind =
    | Struct
    | Union
    | MathType
    | FunctDecl of Function
    | Record
    
  and 
    [<StructuralEquality(false); StructuralComparison(false)>]
    Field =    
    {
      Token:Token;
      mutable Name:string;
      Type:Type;
      Parent:TypeDecl;
      IsSpec:bool;    
      mutable IsVolatile:bool;
      Offset:FieldOffset;
      CustomAttr:list<CustomAttr>
    }
    
    override this.ToString () =
      let postfix =
        match this.Offset with
          | BitField (off, bo, bs) -> sprintf " /* @%d.%d, sz:%d */" off bo bs
          | Normal off -> sprintf " /* @%d */" off
      (if this.IsSpec then "__spec " else "") + 
        (if this.IsVolatile then "volatile " else "") +
          this.Type.ToString() + " " + this.Name + postfix
    
    member this.ByteOffset =
      match this.Offset with
        | FieldOffset.Normal n -> n
        | _ -> die()
  
  
  and 
    [<StructuralEquality(false); StructuralComparison(false)>]
    TypeDecl =
    {
      Token:Token;    
      mutable Kind: TypeKind;
      mutable Name: string;
      mutable Fields: list<Field>;
      mutable Invariants: list<Expr>;
      mutable CustomAttr:list<CustomAttr>;
      mutable SizeOf: int;
      mutable IsNestedAnon: bool;
      mutable GenerateEquality: StructEqualityKind;
      mutable GenerateFieldOffsetAxioms: bool;
      mutable IsSpec: bool;
      Parent : TypeDecl option;
      IsVolatile : bool
    }
    
    override this.ToString () =
      (match this.Kind with
        | Struct -> "struct "
        | Record -> "record "
        | Union -> "union "
        | MathType -> "_math "
        | FunctDecl d -> "_fnptr ") + this.Name
    
    member this.IsUnion = this.Kind = Union
    
    member this.Declaration () =
      let prInv = function
        | Macro(_, "labeled_invariant", [Macro(_, "", _); i]) -> "invariant " + i.ToString()
        | Macro(_, "labeled_invariant", [Macro(_, lbl, _); i]) -> "invariant " + lbl + ": " + i.ToString()
        | e -> "invariant " + e.ToString()
      this.ToString () + " {\n  " + String.concat ";\n  " [for f in this.Fields -> f.ToString ()] + ";\n" +
        String.concat "" [for i in this.Invariants -> prInv i + ";\n" ] + "}\n"
        
  and Type =
    | Void
    | Integer of IntKind
    | Primitive of PrimKind
    | Bool
    | Volatile of Type
    | PhysPtr of Type
    | SpecPtr of Type
    | Ref of TypeDecl
    | Array of Type * int
    | TypeIdT
    | Claim                // the claim_t in C is actually Claim*
    | Map of Type * Type   // t1 -> t2
    | ObjectT
    | MathInteger
    | TypeVar of TypeVariable
    
    member this.WriteTo b =
      let wr = wrb b
      match this with
        | Void -> wr "void"
        | Integer x -> wr (x.ToString())
        | Primitive x -> wr (x.ToString())
        | Bool -> wr "_Bool"
        | TypeIdT -> wr "_TypeId"
        | PhysPtr t -> t.WriteTo b; wr "*"
        | SpecPtr t -> t.WriteTo b; wr "^"
        | Volatile t -> wr "volatile "; t.WriteTo b
        | Type.Ref d -> wr (d.ToString ())
        | Array (t, sz) -> t.WriteTo b; wr ("[" + sz.ToString() + "]")
        | Map (t1, t2) -> t2.WriteTo b; wr "["; t1.WriteTo b; wr "]"
        | Claim -> wr "claim_t"
        | ObjectT -> wr "obj_t"
        | MathInteger -> wr "mathint"
        | TypeVar({Name = id}) -> wr id
    
    override this.ToString () = toString (this.WriteTo)
    
    static member MkPtr(t,isSpec) = if isSpec then Type.SpecPtr(t) else Type.PhysPtr(t)
    
    static member MkPtrToStruct (td:TypeDecl) = Type.MkPtr(Type.Ref(td), td.IsSpec)
    
    member this._IsArray =
      match this with
        | Array _ -> true
        | _ -> false
    
    member this._IsPtr =
      match this with
        | ObjectT
        | SpecPtr _
        | PhysPtr _ -> true
        | _ -> false
        
    member this.IsPtrTo td =
      match this with 
        | SpecPtr td'
        | PhysPtr td' when td = td' -> true
        | _ -> false
    
    member this.IsComposite =
      match this with
        | Void
        | Integer _
        | MathInteger
        | Primitive _
        | Bool
        | PhysPtr _
        | SpecPtr _
        | TypeIdT
        | ObjectT
        | Type.Ref { Kind = (FunctDecl _|MathType|Record) }
        | Map _ -> false
        | TypeVar _
        | Claim
        | Type.Ref { Kind = (Struct|Union) } -> true
        | Volatile(t)
        | Array (t, _) -> t.IsComposite

    member this._IsInteger =
      match this with 
      | Integer _ -> true
      | _         -> false

    member this.IsSignedInteger =
      match this with 
      | Integer sz -> sz |> Type.sizeSign |> snd
      | _          -> false
      
    member this.Deref =
      match this with
      | SpecPtr t 
      | PhysPtr t -> t
      | t -> t
      
    member this.DerefSoP =
      match this with
      | SpecPtr t -> t, true
      | PhysPtr t -> t, false  
      | t -> t, false
    
    // those should be treated as immutable
    static member MathTd name = 
      match mathTypeCache.TryGetValue name with
        // if we get the type of the cache right, F# complains about invalid forward type references
        | true, td -> (td :?> TypeDecl)
        | false, _ ->
          let td =
            { 
              Token = bogusToken
              Name = name
              Kind = MathType
              Fields = []
              SizeOf = 1
              Invariants = []
              CustomAttr = []
              IsNestedAnon = false
              GenerateEquality = NoEq
              GenerateFieldOffsetAxioms = false
              Parent = None
              IsVolatile = false
              IsSpec = true
            }
          mathTypeCache.Add (name, td)
          td
 
    static member Math name = Type.Ref (Type.MathTd name)    
    static member Bogus = Type.Math "$$bogus$$"
    static member PtrSet = Type.Math "ptrset"
    static member MathStruct = Type.Math "struct"
    static member MathState = Type.Math "state_t"
    static member FieldT = Type.Math "field_t"
    static member Byte = Type.Integer IntKind.UInt8
    
    static member private sizeSign t = t.SizeSign
    static member private primSize = function
      | PrimKind.Float32 -> 4
      | PrimKind.Float64 -> 8

    static member ConversionIsLossless = function
      | Integer from, Integer to_ ->
        let (sz1, signed1) = Type.sizeSign from
        let (sz2, signed2) = Type.sizeSign to_
        (signed1 = signed2 && sz1 >= sz2) || (not signed1 && signed2 && sz1 > sz2)
      | MathInteger, Integer _ -> false
      | _ -> true
              
    member this.SizeOf =
      match this with
        | Integer k -> fst (Type.sizeSign k) / 8
        | Primitive k -> Type.primSize k
        | SpecPtr _
        | PhysPtr _
        | ObjectT -> 8
        | Volatile t -> t.SizeOf
        | Type.Ref td -> td.SizeOf
        | Array (t, sz) -> t.SizeOf * sz
        | Bool
        | Void
        | Claim
        | TypeIdT
        | Map _
        | TypeVar _ 
        | MathInteger -> 1
      
    static member IntSuffix k =
      match Type.sizeSign k with
        | (sz, true) -> System.String.Format ("i{0}", sz / 8)
        | (sz, false) -> System.String.Format ("u{0}", sz / 8)

    static member SwitchSignedness = function
      | IntKind.UInt8  -> IntKind.Int8
      | IntKind.UInt16 -> IntKind.Int16
      | IntKind.UInt32 -> IntKind.Int32
      | IntKind.UInt64 -> IntKind.Int64
      | IntKind.Int8   -> IntKind.UInt8
      | IntKind.Int16  -> IntKind.UInt16
      | IntKind.Int32  -> IntKind.UInt32
      | IntKind.Int64  -> IntKind.UInt64

    static member ToUnsigned = function
      | IntKind.UInt8
      | IntKind.Int8   -> IntKind.UInt8
      | IntKind.UInt16 
      | IntKind.Int16  -> IntKind.UInt16
      | IntKind.UInt32
      | IntKind.Int32  -> IntKind.UInt32
      | IntKind.UInt64
      | IntKind.Int64  -> IntKind.UInt64

    static member PrimSuffix = function
      | PrimKind.Float32 -> "f4"
      | PrimKind.Float64 -> "f8"
                
    static member IntRange : IntKind -> Math.BigInt * Math.BigInt =
      memoize (fun k ->
                  let mkBigInt (n : int32) = new Math.BigInt(n)
                  let sub bi1 bi2 = bi1 - bi2
                  let zero = Math.BigInt.Zero
                  let one = Math.BigInt.One
                  let two = mkBigInt 2
                  let (sz, signed) = Type.sizeSign k
                  if signed then
                    let x = Math.BigInt.Pow(two, (mkBigInt (sz - 1)))
                    (sub zero x, sub x one)
                  else
                    (zero, sub (Math.BigInt.Pow(two, (mkBigInt sz))) one))
                    
    member this.Subst(typeSubst : Dict<TypeVariable, Type>) =
      let rec subst = function
          | TypeVar tv -> 
            match typeSubst.TryGetValue(tv) with
              | true, t ->  t
              | false, _ -> TypeVar tv
          | SpecPtr(t) -> SpecPtr(subst t)
          | PhysPtr(t) -> PhysPtr(subst t)
          | Volatile(t) -> Volatile(subst t)
          | Array(t, n) -> Array(subst t, n)
          | Map(t1, t2) -> Map(subst t1, subst t2)
          | t -> t
      subst this

        
  and 
    [<StructuralEquality(false); StructuralComparison(false)>]
    Variable = 
      { 
        Name:Id; 
        Type:Type; 
        Kind:VarKind; 
      }
    
      member this.WriteTo b =
        match this.Kind with
          | SpecLocal 
          | SpecParameter -> wrb b "spec "
          | ConstGlobal -> wrb b "const "
          | OutParameter -> wrb b "out "
          | _ -> ()
        this.Type.WriteTo b
        wrb b (" " + this.Name)      
      
      member this.IsSpec = 
        match this.Kind with 
          | SpecParameter
          | OutParameter
          | SpecLocal
          | QuantBound -> true
          | _ -> false

      override this.ToString () = toString (this.WriteTo)
 
  and 
    TypeVariable = 
      {
        Name:Id;
      }
      
      member this.WriteTo b = wrb b (this.Name)
 
  and ExprCommon = 
    {
      Token:Token;
      Type:Type;
    }

  and 
    [<StructuralEquality(false); StructuralComparison(false)>]
    Function = 
    {
      Token:Token;
      IsSpec:bool;
      mutable OrigRetType:Type;
      mutable RetType:Type;
      mutable Name:Id;
      mutable Parameters:list<Variable>;
      mutable TypeParameters:list<TypeVariable>
      mutable Requires:list<Expr>;
      mutable Ensures:list<Expr>;
      mutable Writes:list<Expr>;
      mutable Reads:list<Expr>;
      mutable CustomAttr:list<CustomAttr>;
      mutable Body:option<Expr>;
      mutable IsProcessed:bool;
    }
    
    member this.InParameters = [ for p in this.Parameters do if p.Kind <> VarKind.OutParameter then yield p ]
    
    member this.OutParameters = [ for p in this.Parameters do if p.Kind = VarKind.OutParameter then yield p ]
    
    member this.IsPure =
      if this.Name.StartsWith "_vcc_" && this.Writes = [] then
        true
      //else if this.Name.StartsWith "fnptr#" && this.Writes = [] then
      //  true // HACK
      else
        List.exists (function VccAttr(("frameaxiom"|"is_pure"|"specmacro"), "") -> true | _ -> false) this.CustomAttr 
      
    member this.IsStateless =
      this.IsPure && this.Reads = []

    member this.Specialize(targs : list<Type>, includeBody : bool) =
      if targs.Length = 0 then this else       
        let typeSubst = new Dict<_,_>()
        let varSubst = new Dict<_,_>()
        List.iter2 (fun tv t -> typeSubst.Add(tv, t)) this.TypeParameters targs 
        let sv v = 
          let v' = { v with Type = v.Type.Subst(typeSubst) } : Variable
          varSubst.Add(v,v')
          v'
        let pars = List.map sv this.Parameters // do this first to populate varSubst
        let se (e : Expr) = e.SubstType(typeSubst, varSubst)
        let ses = List.map se
        { this with OrigRetType = this.OrigRetType.Subst(typeSubst);
                    RetType = this.RetType.Subst(typeSubst);
                    Parameters = pars;
                    Requires = ses this.Requires;
                    Ensures = ses this.Ensures;
                    Writes = ses this.Writes;
                    Reads = ses this.Reads;
                    TypeParameters = [];
                    Body = if includeBody then Option.map se this.Body else None }

    override this.ToString () : string = 
      let b = StringBuilder()
      let wr (s:string) = b.Append s |> ignore
      if this.IsSpec then wr "spec " else ()
      this.RetType.WriteTo b; wr " "
      doArgsAndTArgsb b (fun (p:Variable) -> p.WriteTo b) (fun (tp:TypeVariable) -> tp.WriteTo b) (this.Name) this.Parameters this.TypeParameters 
      wr "\n"
      
      let doList pref lst =
        for (e:Expr) in lst do
          wr "  "; wr pref; wr " ";
          e.WriteTo System.Int32.MinValue b
          wr ";\n";
      doList "requires" (this.Requires)
      doList "ensures" (this.Ensures)
      doList "reads" (this.Reads)
      doList "writes" (this.Writes)        
        
      b.ToString()              
      
  
  and QuantData = 
    {
      Kind:QuantKind;
      Variables:list<Variable>;
      Triggers:list<list<Expr>>;
      Condition:option<Expr>;
      Body:Expr;
    }


  and CheckedStatus =
  | Processed
  | Checked
  | Unchecked
    override this.ToString() : string =
      match this with
      | Processed -> ""
      | Checked -> "checked"
      | Unchecked -> "unchecked"

  and Op =
  | Op of string * CheckedStatus
    override this.ToString() : string =
      match this with
      | Op(name, _checked) -> _checked.ToString() + name
    member this.IsEqOrIneq =
      match this with
      | Op("==", _) -> true
      | Op("!=", _) -> true
      | _ -> false
    member this.IsEq =
      match this with
      | Op("==", _) -> true
      | _ -> false
    member this.IsIneq =
      match this with
      | Op("!=", _) -> true
      | _ -> false
    member this.IsChecked =
      match this with
      | Op(_, Checked) -> true
      | _              -> false
    member this.IsUnchecked =
      match this with
      | Op(_, Unchecked) -> true
      | _                -> false
    member this.IsProcessed =
      match this with
      | Op(_, Processed) -> true
      | _                -> false
    member this.OpName =
      match this with
      | Op(opName, _) -> opName

  and ExprCtx =
    {
      IsPure : bool;
    }
    static member PureCtx = { IsPure = true }
    
  and Expr =
    | Ref of ExprCommon * Variable    
    | Prim of ExprCommon * Op * list<Expr>
    | Call of ExprCommon * Function * list<Type> * list<Expr>
    | IntLiteral of ExprCommon * bigint
    | BoolLiteral of ExprCommon * bool
    | Deref of ExprCommon * Expr
    | Dot of ExprCommon * Expr * Field   // computes address of the field
    | Index of ExprCommon * Expr * Expr  // computes address of an array element
    | Cast of ExprCommon * CheckedStatus * Expr   // take the type from ExprCommon
    | Quant of ExprCommon * QuantData
    | Result of ExprCommon
    | Old of ExprCommon * Expr * Expr           // the first expression refers to ``when''
    | SizeOf of ExprCommon * Type
    
    // stmt-like expressions
    | VarDecl of ExprCommon * Variable
    | VarWrite of ExprCommon * list<Variable> * Expr
    | MemoryWrite of ExprCommon * Expr * Expr
    | If of ExprCommon * Expr * Expr * Expr
    // invariants * writes * body
    | Loop of ExprCommon * list<Expr> * list<Expr> * Expr // TODO use record
    | Goto of ExprCommon * LabelId
    | Label of ExprCommon * LabelId
    // token is taken from expr
    | Assert of ExprCommon * Expr
    | Assume of ExprCommon * Expr
    | Pure of ExprCommon * Expr
    | Block of ExprCommon * list<Expr>
    | Return of ExprCommon * option<Expr>
    | Atomic of ExprCommon * list<Expr> * Expr
    | Comment of ExprCommon * string
    // for now used to mark calls to be interpreted as statements
    | Stmt of ExprCommon * Expr
    
    // This is for expressions with side effects, or expression that get compiled away
    // later. This include:
    //   assignment     =
    //   assignment-op  +=, -=, >>=, ...
    //   address of     &
    //   incr/decr      pre++, pre--, post++, post--
    //   loops
    | Macro of ExprCommon * string * list<Expr>
    | UserData of ExprCommon * obj
    
    override this.ToString () = toString (this.WriteTo 0)
  
    static member MkDot(ec, expr:Expr, field:Field) = 
      let t = match field.Type with | Array(t, _) -> t | t -> t
      let isSpec = match expr.Type with
                    | _ when field.IsSpec -> true
                    | SpecPtr _ -> true
                    | _ -> false
      Expr.Dot({ec with Type = Type.MkPtr(t, isSpec)}, expr, field)
  
    static member MkDot(expr:Expr, field:Field) = Expr.MkDot(expr.Common, expr, field)
  
    member x.Common =
      match x with
        | Ref (e, _)
        | Prim (e, _, _)
        | Expr.Call (e, _, _, _)
        | IntLiteral (e, _)
        | BoolLiteral (e, _) 
        | Deref (e, _)
        | Dot (e, _, _)
        | Index (e, _, _)
        | Cast (e, _, _)
        | Quant (e, _)
        | Result (e)
        | Old (e, _, _)        
        | Macro (e, _, _)
        | VarWrite (e, _, _)
        | MemoryWrite (e, _, _)
        | If (e, _, _, _)
        | Loop (e, _, _, _)
        | Goto (e, _)
        | Label (e, _)
        | Assert (e, _)
        | Assume (e, _)
        | Block (e, _)
        | Return (e, _)
        | Atomic (e, _, _)
        | Comment (e, _)
        | VarDecl (e, _)
        | Stmt (e, _)
        | Pure (e, _)
        | UserData(e, _)
        | SizeOf(e, _)
          -> e

    member this.Visit (ispure : bool, f: ExprCtx -> Expr -> bool) : unit =
      let rec visit ctx e =
        if f ctx e then
          match e with
            | Return (_, None)
            | Goto _
            | Label _
            | Comment _ 
            | Ref _
            | IntLiteral _
            | BoolLiteral _
            | VarDecl _
            | Macro(_, _, [])
            | Call(_, _, _, [])
            | UserData _
            | SizeOf _
            | Result _ -> ()
            | Prim (_, _, es)
            | Block (_, es) 
            | Call (_, _, _, es)
            | Macro (_, _, es) -> List.iter (visit ctx) es
            | Deref (_, e)
            | Dot (_, e, _)
            | VarWrite (_, _, e)
            | Stmt (_, e)
            | Return (_, Some e)
            | Cast (_, _, e) -> visit ctx e
            | Pure (_, e) 
            | Assert (_, e) 
            | Assume (_, e) -> paux e
            | MemoryWrite (_, e1, e2) 
            | Index (_, e1, e2) -> visit ctx e1; visit ctx e2
            | Old (_, e1, e2) -> paux e1; visit ctx e2
            | Quant (_, q) -> List.iter pauxs q.Triggers; Option.iter paux q.Condition; paux q.Body
            | If (_, cond, s1, s2) -> visit ctx cond; visit ctx s1; visit ctx s2
            | Loop (_, invs, writes, s) -> pauxs invs; pauxs writes; visit ctx s
            | Atomic (c, exprs, s) -> pauxs exprs; visit ctx s


      and paux = visit ExprCtx.PureCtx
      and pauxs = List.iter (visit ExprCtx.PureCtx)
        
      visit { IsPure = ispure } this  

    member this.SelfCtxVisit (ispure : bool, f : ExprCtx -> (Expr -> unit) -> Expr -> bool) : unit =        
      let rec aux ctx (e:Expr) = e.Visit (ispure, f')
      and f' ctx e = f ctx (aux ctx) e
      this.Visit (ispure, f')

    member this.SelfVisit (f : (Expr -> unit) -> Expr -> bool) : unit =        
      let rec aux ctx (e:Expr) = e.Visit (false, f')
      and f' ctx e = f (aux ctx) e
      this.Visit (false, f')
    
    member this.HasSubexpr (f : Expr -> bool) : bool =
      let found = ref false
      let check _ expr =
        if !found then false
        elif f expr then
          found := true
          false
        else true
      this.SelfVisit check
      !found
          
       

    /// When f returns Some(x), this is replaced by x, otherwise Map is applied
    /// recursively to children, including application of g to children expressions.
    member this.Map (ispure : bool, f : ExprCtx -> Expr -> option<Expr>) : Expr =        
      let rec map ctx e =
        let apply f args =        
          let foundSome = ref false
          let supplyDefault arg =
            match f arg with
              | None -> arg
              | Some arg' -> foundSome := true; arg'
          let args' = List.map supplyDefault args 
          !foundSome, args'

        let applyList f argsLists =
          let foundSome = ref false
          let apply' args =
            match apply f args with
              | true, args' -> foundSome := true; args'
              | false, args' -> args'
          let argsLists' = List.map apply' argsLists
          !foundSome, argsLists'

        let supplyDefault arg defArg = 
          match arg with 
            | Some arg' -> arg'
            | None -> defArg

        let construct1 ctor arg =
          match arg with
            | None -> None
            | Some arg' -> Some (ctor arg')
        let construct2 ctor arg1 arg2 defArg1 defArg2 =
          match arg1, arg2 with
            | None, None -> None
            | _ -> Some (ctor (supplyDefault arg1 defArg1) (supplyDefault arg2 defArg2) )
        let construct3 ctor arg1 arg2 arg3 defArg1 defArg2 defArg3=
          match arg1, arg2, arg3 with
            | None, None, None -> None
            | _ -> Some (ctor (supplyDefault arg1 defArg1) (supplyDefault arg2 defArg2) (supplyDefault arg3 defArg3))
        let constructList ctor f args =
          match apply f args with
            | false, _ -> None
            | true, args' -> Some(ctor args')
        match f ctx e with
          | Some e as se -> se
          | None ->
            match e with
              | Return (_, None)
              | Goto _
              | Label _
              | Comment _ 
              | Ref _
              | IntLiteral _
              | BoolLiteral _
              | VarDecl _
              | Macro(_, _, [])
              | Call(_, _, _, [])
              | UserData _
              | SizeOf _
              | Result _ -> None
              | Prim (c, op, es) ->  constructList (fun args ->  Prim (c, op, args)) (map ctx) es
              | Call (c, fn, tas, es) -> constructList  (fun args -> Call (c, fn, tas, args)) (map ctx) es
              | Macro (c, op, es) -> constructList (fun args -> Macro (c, op, args)) (map ctx) es
              | Deref (c, e) -> construct1 (fun arg ->  Deref (c, arg)) (map ctx e)
              | Dot (c, e, f) -> construct1 (fun arg -> Dot (c, arg, f)) (map ctx e)
              | Index (c, e1, e2) -> construct2 (fun arg1 arg2 -> Index (c, arg1, arg2)) (map ctx e1) (map ctx e2) e1 e2
              | Cast (c, ch, e) -> construct1 (fun arg -> Cast (c, ch, arg)) (map ctx e)
              | Old (c, e1, e2) -> construct2 (fun arg1 arg2 -> Old (c, arg1, arg2)) (paux e1) (map ctx e2) e1 e2
              | Quant (c, q) ->
                let rTriggers, triggers = applyList paux q.Triggers
                let rCond, cond = 
                  match q.Condition with
                    | None -> false, None
                    | Some cond -> 
                      match paux cond with
                        | None -> false, Some cond
                        | Some cond' -> true, Some cond'
                let rBody, body = 
                  match paux q.Body with
                    | None -> false, q.Body
                    | Some body' -> true, body'
                if not rTriggers && not rCond && not rBody then None 
                else Some(Quant (c, { q with Triggers = triggers; Condition = cond; Body = body }))
              | VarWrite (c, v, e) -> construct1 (fun arg -> VarWrite (c, v, arg)) (map ctx e)
              | MemoryWrite (c, e1, e2) -> construct2 (fun arg1 arg2 -> MemoryWrite (c, arg1, arg2)) (map ctx e1) (map ctx e2) e1 e2
              | Assert (c, e) -> construct1 (fun arg -> Assert (c, arg)) (paux e)
              | Assume (c, e) -> construct1 (fun arg -> Assume (c, arg)) (paux e)
              | Return (c, Some e) -> construct1 (fun arg -> Return (c, Some arg)) (map ctx e)
              | If (c, cond, s1, s2) -> construct3 (fun a1 a2 a3 -> If (c, a1, a2, a3)) (map ctx cond) (map ctx s1) (map ctx s2) cond s1 s2
              | Loop (c, invs, writes, s) -> 
                let rInvs, invs' = apply paux invs
                let rWrites, writes' = apply paux writes
                let rS, s' = match map ctx s with | None -> false, s | Some s' -> true, s'
                if not rInvs && not rWrites && not rS then None else Some(Loop(c, invs', writes', s'))
              | Atomic (c, exprs, s) -> 
                let rExprs, exprs' = apply paux exprs
                let rS, s' = match map ctx s with | None -> false, s | Some s' -> true, s'
                if not rExprs && not rS then None else Some(Atomic(c, exprs', s'))
              | Block (c, ss) -> constructList (fun args-> Block (c, args)) (map ctx) ss
              | Stmt (c, e) -> construct1 (fun arg -> Stmt (c, arg)) (map ctx e)
              | Pure (c, e) -> construct1 (fun arg -> Pure (c, arg)) (paux e)
      and paux = map ExprCtx.PureCtx
      
      match map { IsPure = ispure } this with
        | None -> this
        | Some this' -> this'
    
    member this.SubstType(typeSubst : Dict<TypeVariable, Type>, varSubst : Dict<Variable, Variable>) =
      let sc c = { c with Type = c.Type.Subst(typeSubst) } : ExprCommon
      let varSubst = new Dict<_,_>(varSubst) // we add to it, so make a copy first
      let sv v = 
        match varSubst.TryGetValue(v) with
          | true, v' -> v'
          | false, _ ->
            let v' = { v with Type = v.Type.Subst(typeSubst) }
            varSubst.Add(v,v')
            v'
      let repl self = 
        let selfs = List.map self
        function
          | Return (_, None) 
          | Goto _
          | Label _
          | Comment _
          | IntLiteral _
          | BoolLiteral _
          | UserData _
          | MemoryWrite _
          | Assert _
          | Assume _
          | Pure _
          | Block _ 
          | Stmt _
          | If _
          | Atomic _
          | Loop _  -> None
          | Ref(c, v) -> Some(Ref(sc c, sv v))
          | VarDecl(c, v) -> Some(VarDecl(c, sv v))
          | Result c -> Some(Result(sc c))
          | Prim (c, op, es) ->  Some(Prim(sc c, op, selfs es))
          | Call (c, fn, tas, es) -> Some(Call(sc c, fn, List.map (fun (t : Type) -> t.Subst(typeSubst)) tas, selfs es))
          | Macro (c, op, es) -> Some(Macro(sc c, op, selfs es))
          | Deref (c, e) -> Some(Deref(sc c, self e))
          | Dot (c, e, f) -> Some(Dot(sc c, self e, f))
          | Index (c, e1, e2) -> Some(Index(sc c, self e1, self e2))
          | Cast (c, ch, e) -> Some(Cast(sc c, ch, self e))
          | Old (c, e1, e2) -> Some(Old(sc c, self e1, self e2))
          | Quant (c, q) -> Some(Quant(sc c, {q with Triggers = List.map selfs q.Triggers; Condition = Option.map self (q.Condition); Body = self (q.Body)}))
          | VarWrite (c, vs, e) -> Some(VarWrite(sc c, List.map sv vs, self e))
          | Return (c, Some e) -> Some(Return(sc c, Some(self e)))
          | SizeOf(c, t) -> Some(SizeOf(c, t.Subst(typeSubst)))
      this.SelfMap(repl)
    
    member this.SelfCtxMap (ispure : bool, f : ExprCtx -> (Expr -> Expr) -> Expr -> option<Expr>) : Expr =        
      let rec aux ctx (e:Expr) = e.Map (ispure, f')
      and f' ctx e = f ctx (aux ctx) e
      this.Map (ispure, f')

    member this.SelfMap (f : (Expr -> Expr) -> Expr -> option<Expr>) : Expr =        
      let rec aux ctx (e:Expr) = e.Map (false, f')
      and f' ctx e = f (aux ctx) e
      this.Map (false, f')

    member this.Subst (subst : System.Collections.Generic.Dictionary<Variable, Expr>) =
      let repl self =
        let substVar v = 
          match subst.TryGetValue(v) with
            | true, Ref(_, v') -> v'
            | true, _ -> die()
            | _ -> v
        function
          | Ref (_, v) -> 
            match subst.TryGetValue v with
              | true, e -> Some (e)
              | _ -> None
          | VarDecl (c, v) when subst.ContainsKey v -> Some (Block (c, []))
          | VarWrite (c, v, e) -> Some (VarWrite (c, List.map substVar v, self e))
          | _ -> None
      this.SelfMap repl
                
    member x.Type = x.Common.Type
    member x.Token = x.Common.Token
    
    member x.WriteTo (ind:int) b : unit =
      let f e = (e:Expr).WriteTo (ind + 2) b
      let fe e = (e:Expr).WriteTo System.Int32.MinValue b
      let wr = wrb b
      let doArgs = doArgsb b fe
      let doInd ind = 
        if ind > 0 then wr (System.String(' ', ind))
      doInd ind
      match x with
        | Ref (_, v) -> wr (v.Name)
        | Prim (_, op, args) -> doArgs (op.ToString()) args
        | Expr.Call (_, fn, tArgs, args) -> doArgsAndTArgsb b  fe (fun (t:Type) -> t.WriteTo b) fn.Name args tArgs
        | BoolLiteral (_, v) -> wr (if v then "true" else "false")
        | IntLiteral (_, l) -> wr (l.ToString())
        | Deref (_, e) -> wr "*("; fe e; wr ")"
        | Dot (_, e, fld) -> 
          let fieldName = if fld.Name <> "" then fld.Name else "'" + (fld.ToString()) + "'"
          wr "("; fe e; wr "->"; wr fieldName; wr ")"
        | Index (_, e, off) -> fe e; wr "["; fe off; wr "]"
        | Cast (_, ch, e) -> 
          wr "("; wr (ch.ToString()); wr " "; wr (x.Type.ToString()); wr ")"; fe e;
          //wr " tok: "; wr x.Common.Token.Value; wr "]"
        | Quant (_, q) ->
          match q.Kind with
            | Exists -> wr "exists("
            | Forall -> wr "forall("
            | Lambda -> wr "lambda("
          for v in q.Variables do
            wr (v.Type.ToString())
            wr " "
            wr v.Name
            wr "; "
          match q.Condition with
            | Some e -> fe e; wr "; "
            | None -> ()
          fe q.Body
          wr ")"
        | Result (_) -> wr "result"
        | Old (_, w, e) -> doArgs "old" [w; e]
        | Pure (_, e) -> doArgs "pure" [e]
        | Expr.Macro (_, op, args) -> doArgs ("@" + op) args        

        | VarDecl (_, v) ->
          wr (v.Type.ToString()); wr " "; wr v.Name; wr ";\n"
        | VarWrite (_, [v], e) ->
          wr v.Name; wr " := "; fe e; wr ";\n"
        | VarWrite (_, vs, e) -> 
          let bogus = { Type = Type.Void; Token = Token.NoToken } : ExprCommon
          doArgs "" (List.map (fun v -> Ref(bogus, v)) vs); wr " = "; fe e; wr ";\n"
        | MemoryWrite (_, d, s) ->
          wr "*"; fe d; wr " := "; fe s; wr ";\n"
        | Goto (_, l) -> wr "goto "; wr l.Name; wr ";\n"
        | Label (_, l) -> wr l.Name; wr ":\n"
        | Assert (_, e) -> wr "assert "; fe e; wr ";\n"
        | Assume (_, e) -> wr "assume "; fe e; wr ";\n"
        | Return (_, Some (e)) -> wr "return "; fe e; wr ";\n"
        | Return (_, None) -> wr "return;\n"
        | If (_, cond, th, el) ->
          wr "if ("; fe cond; wr ")\n"; f th; doInd ind; wr "else\n"; f el
        | Loop (_, invs, writes, body) ->
          wr "loop\n";
          for i in invs do
            doInd (ind + 4)
            wr "invariant ";
            fe i;
            wr ";\n"
          for w in writes do
            doInd (ind + 4)
            wr "writes ";
            fe w;
            wr ";\n"
          f body        
        | Atomic (c, args, body) ->
          doArgs "atomic" args
          f (Block (c, [body]))
        | Block (_, stmts) ->
          wr "{\n";
          for s in stmts do 
            match s with
              | Macro (_, _, _) -> 
                f s
                wr "\n"
              | _ -> f s
          doInd ind
          wr "}\n"
        | Stmt (_, e) ->
          wr "stmt "
          fe e
          wr ";\n"
        | Comment (_, s) ->
          wr "// "; wr s; wr "\n"
        | UserData (_, o) ->
          wr "userdata("; wr (o.ToString()); wr ") : "; wr (o.GetType().Name)
        | SizeOf(_, t) ->
          wr "sizeof("; wr (t.ToString()); wr ")"
          
  let (|ETrue|_|) = function
      | BoolLiteral (_, true) -> Some (ETrue)
      | _ -> None
    
  let (|EFalse|_|) = function
      | BoolLiteral (_, false) -> Some (EFalse)
      | _ -> None
    
  let (|Ptr|_|) = function
      | PhysPtr t  
      | SpecPtr t -> Some(t)
      | _ -> None
      
  let (|PtrSoP|_|) = function
    | PhysPtr t -> Some(t,false)
    | SpecPtr t -> Some(t,true)
    | _ -> None
    
  let (|MathTypeRef|_|) = function
    | Type.Ref ({ Name = n; Kind = MathType }) -> Some (MathTypeRef n)
    | _ -> None
    
  let bogusEC = { Token = bogusToken; Type = Type.Bogus } : ExprCommon
  // there is an initialization bug in the F# compiler, it seems
  // that this code is executed before the Type class if fully constructed
  // and thus we get null as Type instead of Bool/Void (they are unitary)
  let voidBogusEC () = { bogusEC with Type = Void }
  let boolBogusEC () = { bogusEC with Type = Bool }
  type ExprCommon with
    static member Bogus = bogusEC
  
  let bogusExpr = Expr.Macro (ExprCommon.Bogus, "bogus", [])
  type Expr with    
    static member Bogus = bogusExpr
    
    static member True = BoolLiteral(boolBogusEC(), true)
    static member False = BoolLiteral(boolBogusEC(), false)
    
    static member MkBlock exprs =
      let rec last = function
        | [x] -> x
        | _ :: xs -> last xs
        | [] -> die()
      match exprs with
        | [e] -> e
        | [] -> Block (voidBogusEC(), [])
        | stmts -> 
          let lstmt = last stmts 
          Block ({ bogusEC with Type = lstmt.Type; Token = lstmt.Token }, stmts)
  
    static member MkAssert (expr:Expr) =
      Assert ({ expr.Common with Type = Void }, expr)
    
    static member MkAssume (expr:Expr) =
      Assume ({ expr.Common with Type = Void }, expr)
      
    static member ToUserData (o:obj) =
      assert (o <> null)
      UserData(ExprCommon.Bogus, o)
      
  let (|FunctionPtr|_|) = function
    | Ptr (Type.Ref { Kind = FunctDecl f }) -> Some f
    | _ -> None
  
  type [<StructuralEquality(false); StructuralComparison(false)>] Top =
    | Global of Variable * Expr option
    | TypeDecl of TypeDecl
    | FunctionDecl of Function
    | Axiom of Expr
    | GeneratedAxiom of Expr * Top
    
    override this.ToString () = toString (this.WriteTo)
    
    member this.WriteTo b =
      let wr = wrb b
      match this with
        | Global (v, None) -> wr (v.ToString() + ";\n")
        | Global (v, Some e) -> wr (v.ToString()); wr " = "; e.WriteTo System.Int32.MinValue b; wr ";\n"
        | TypeDecl d -> wr (d.Declaration())
        | FunctionDecl d -> 
          wr (d.ToString())
          match d.Body with
            | Some e ->
              e.WriteTo 0 b
              wr "\n"
            | None -> ()
        | Axiom e -> wr "axiom "; e.WriteTo System.Int32.MinValue b; wr ";\n"
        | GeneratedAxiom(e, origin) ->
          wr "axiom (from "; wr (origin.ToString()); wr ") "; e.WriteTo System.Int32.MinValue b; wr ";\n"
    
    member this.MapExpressions f =
      let pf = f true
      let pfs = List.map pf      
      match this with
        | Top.Axiom e -> Top.Axiom (pf e)
        | Top.GeneratedAxiom(e, origin) -> Top.GeneratedAxiom(pf e, origin)
        | Top.FunctionDecl x ->
          // do the body first, so the function can see VarDecls for parameters
          // before they are actually used in contracts
          x.Body <- Option.map (f false) x.Body
          x.Requires <- pfs x.Requires
          x.Ensures <- pfs x.Ensures
          x.Reads <- pfs x.Reads
          x.Writes <- pfs x.Writes
          this
        | Top.TypeDecl td ->
          td.Invariants <- pfs td.Invariants; this
        | Top.Global(v, Some e) -> Top.Global(v, Some(pf e))
        | Top.Global(_, None) -> this

    member this.VisitExpressions f =
      let pf = f true
      let pfs = List.iter pf      
      match this with
        | Top.Axiom e -> pf e
        | Top.GeneratedAxiom(e, origin) -> pf e
        | Top.FunctionDecl x ->
          Option.iter (f false) x.Body
          pfs x.Requires
          pfs x.Ensures
          pfs x.Reads
          pfs x.Writes
        | Top.TypeDecl td -> pfs td.Invariants
        | Top.Global(v, Some e) -> pf e
        | Top.Global(_, None) -> ()

  let mapExpressions f decls =
    List.map (fun (d:Top) -> d.MapExpressions f) decls
  
  let optExprMap f = Option.map (fun (e:Expr) -> e.SelfMap f)
  
  let deepMapExpressionsCtx f decls =
    let aux ispure (e:Expr) =
      e.SelfCtxMap (ispure, f)
    mapExpressions aux decls
  
  let deepMapExpressions f decls =
    let aux _ (e:Expr) =
      e.SelfMap f
    mapExpressions aux decls
  
  let visitExpressions f decls = 
    List.iter (fun (d:Top) -> d.VisitExpressions f) decls
  
  let deepVisitExpressionsCtx f decls =
    let aux ispure (e:Expr) =
      e.SelfCtxVisit (ispure, f)
    visitExpressions aux decls
  
  let deepVisitExpressions f decls =
    let aux _ (e:Expr) =
      e.SelfVisit f
    visitExpressions aux decls

  let deepMapDecl f decl =
    match [decl] |> deepMapExpressions f with
      | [decl'] -> decl'
      | _ -> die()
