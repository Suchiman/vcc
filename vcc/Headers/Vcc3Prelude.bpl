//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Boogie types
// ----------------------------------------------------------------------------

// for each C type there is a value of $ctype Boogie type
type $ctype; 

// a typed pointer -- i.e. a pair of a $ctype and an int
type $ptr;

// name of a C field -- each field of a C struct gets a constant of this type
//   like A.b for field b of struct A.
type $field; 

// describes a kind of a C type:
//  - primitive (includes mathematical types (sequences, maps, sets) and pointers)
//  - array
//  - composite
//  - thread_id_t
//  - claim
type $kind; 

// for float and double
type $primitive;

// for structs passed by value
type $struct;

// used for constants describing position in the source code (debugging/model printing only)
type $token;

type $state = [$ptr]int;

// A constant is generated for each pure function, used for ordering frame axioms.
type $pure_function;

// For labeled contracts.
type $label;

// ----------------------------------------------------------------------------
// built-in types
// ----------------------------------------------------------------------------

function $kind_of($ctype) returns($kind);

// these are the only possible kinds
const unique $kind_composite : $kind;
const unique $kind_primitive : $kind;
const unique $kind_array : $kind;
const unique $kind_thread : $kind;

function $sizeof($ctype)returns(int); // in bytes


// for types for which $in_range_t(...) is defined
function $as_in_range_t($ctype) returns($ctype);

function {:inline true} $def_flat_type(t:$ctype, sz:int) returns(bool)
  { $sizeof(t) == sz && $type_branch(t) == $ctype_flat }
function {:inline true} $def_primitive_type(t:$ctype, sz:int) returns(bool)
  { $def_flat_type(t, sz) && $is_primitive(t) }
function {:inline true} $def_composite_type(t:$ctype, sz:int, claimable:bool, has_volatile_owns:bool) returns(bool)
  { $def_flat_type(t, sz) && $is_composite(t) && $is_claimable(t) == claimable && $has_volatile_owns_set(t) == has_volatile_owns }
function {:inline true} $def_integer_type(t:$ctype, sz:int) returns(bool)
  { $def_primitive_type(t, sz) && $as_in_range_t(t) == t }

//Notation: types get a ^ as prefix...
const unique ^^i1: $ctype;
const unique ^^i2: $ctype;
const unique ^^i4: $ctype;
const unique ^^i8: $ctype;
const unique ^^u1: $ctype;
const unique ^^u2: $ctype;
const unique ^^u4: $ctype;
const unique ^^u8: $ctype;
const unique ^^void: $ctype;
const unique ^^bool: $ctype;
const unique ^^f4: $ctype;
const unique ^^f8: $ctype;
const unique ^^object: $ctype;
const unique ^^field: $ctype;

// struct A will get ^A :$ctype
axiom $def_integer_type(^^i1, 1);
axiom $def_integer_type(^^i2, 2);
axiom $def_integer_type(^^i4, 4);
axiom $def_integer_type(^^i8, 8);
axiom $def_integer_type(^^u1, 1);
axiom $def_integer_type(^^u2, 2);
axiom $def_integer_type(^^u4, 4);
axiom $def_integer_type(^^u8, 8);

axiom $def_primitive_type(^^f4, 4);
axiom $def_primitive_type(^^f8, 8);
axiom $def_primitive_type(^^bool, 1);
axiom $def_primitive_type(^^void, 1);
axiom $def_primitive_type(^^object, 1);
axiom $def_primitive_type(^^field, 1);

const unique ^^claim: $ctype;
const unique ^^root_emb: $ctype;
const unique ^^mathint: $ctype;
const unique ^$#thread_id_t: $ctype;
const unique ^$#ptrset : $ctype;
const unique ^$#state_t : $ctype;
const unique ^$#struct : $ctype;

const unique ^$#seq_version : $ctype;
const unique ^$#vol_version : $ctype;

// wrap writes owner & closed of different objects

axiom $def_primitive_type(^^mathint, 1);
axiom $def_composite_type(^^claim, 1, true, false);
axiom $def_composite_type(^^root_emb, 1, false, false);
axiom $def_primitive_type(^$#ptrset, 1);
axiom $def_flat_type(^$#thread_id_t, 1) && $is_threadtype(^$#thread_id_t);
axiom $def_primitive_type(^$#state_t, 1);
axiom $def_primitive_type(^$#struct, 1);

type $ctype_branch;
function $type_branch($ctype) : $ctype_branch;

const unique $ctype_flat : $ctype_branch;
const unique $ctype_ptr : $ctype_branch;
const unique $ctype_spec_ptr : $ctype_branch;
const unique $ctype_map : $ctype_branch;
const unique $ctype_ghost : $ctype_branch;

// inverse functions here (unptr_to, map_domain, map_range) are for the prover
// so it knows that int*!=short*, i.e. * is injective

// pointers to types
function $ptr_to($ctype) returns($ctype);
function $spec_ptr_to($ctype) returns ($ctype);
function $ghost($ctype) returns($ctype);
function $type_project_0($ctype) returns($ctype);

const $arch_ptr_size : int; // arch-specific; to be defined by a compiler-generated axiom

axiom (forall #n:$ctype :: {$ptr_to(#n)} $type_project_0($ptr_to(#n)) == #n && $type_branch($ptr_to(#n)) == $ctype_ptr);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $type_project_0($spec_ptr_to(#n)) == #n && $type_branch($spec_ptr_to(#n)) == $ctype_spec_ptr);
axiom (forall #n:$ctype :: {$ghost(#n)} $type_project_0($ghost(#n)) == #n && $type_branch($ghost(#n)) == $ctype_ghost);

axiom (forall #n:$ctype :: {$ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);

function $map_t($ctype, $ctype) returns($ctype);
function $map_domain($ctype) returns($ctype);
function $map_range($ctype) returns($ctype);

axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $map_domain($map_t(#r,#d)) == #d && $map_range($map_t(#r,#d)) == #r && $type_branch($map_t(#r,#d)) == $ctype_map);

// Lack of {:inline true} makes it possible to trigger on $is_primitive(...)
// $is_primitive(t) should be only used when it is known that t is really
// primitive, i.e. not in a precondition or in a premise of an implication
// (unless guarded by a trigger). If it is unknown, use $is_primitive_ch(...).
// The same applies to $is_composite(...) and $is_arraytype(...).

function {:weight 0} $is_primitive(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_primitive }

function {:inline true} $is_primitive_ch(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_primitive }

function {:weight 0} $is_composite(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_composite }

function {:inline true} $is_composite_ch(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_composite }

function {:weight 0} $is_arraytype(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_array }

function {:inline true} $is_arraytype_ch(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_array }

function {:weight 0} $is_threadtype(t:$ctype) returns(bool)
  { $kind_of(t) == $kind_thread }

function {:inline true} $is_thread(p:$ptr) returns(bool)
  { $is_threadtype($typ(p)) }

function {:inline true} $is_ptr_to_composite(p:$ptr) returns(bool)
  { $kind_of($typ(p)) == $kind_composite }

function $field_offset($field) returns(int);
function $field_parent_type($field) returns($ctype);

function $is_non_primitive(t:$ctype) returns(bool);
axiom (forall t:$ctype :: {:weight 0} {$is_composite(t)} $is_composite(t) ==> $is_non_primitive(t));
axiom (forall t:$ctype :: {:weight 0} {$is_arraytype(t)} $is_arraytype(t) ==> $is_non_primitive(t));
axiom (forall t:$ctype :: {:weight 0} {$is_threadtype(t)} $is_threadtype(t) ==> $is_non_primitive(t));

axiom (forall t:$ctype :: {$is_composite(t)} $is_composite(t) ==> $type_branch(t) == $ctype_flat);

function {:inline true} $is_non_primitive_ch(t:$ctype) returns(bool)
  { $kind_of(t) != $kind_primitive }

function {:inline true} $is_non_primitive_ptr(p:$ptr) returns(bool)
  { $is_non_primitive($typ(p)) }

axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $is_primitive($map_t(#r,#d)));
axiom (forall #n:$ctype :: {$ptr_to(#n)} $is_primitive($ptr_to(#n)));
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $is_primitive($spec_ptr_to(#n)));
axiom (forall #n:$ctype :: {$is_primitive(#n)} $is_primitive(#n) ==> !$is_claimable(#n));

const $me_ref : int;
function $me() returns($ptr);
axiom $in_range_spec_ptr($me_ref);
axiom $me() == $ptr(^$#thread_id_t, $me_ref);

function {:inline true} $current_state(s:$state) returns($state) { s }

function {:inline true} $mem(s:$state, p:$ptr) returns(int)
  { s[p] }
function {:inline true} $read_any(s:$state, p:$ptr) returns(int)
  { s[p] }
function {:inline true} $mem_eq(s1:$state, s2:$state, p:$ptr) returns(bool)
  { $mem(s1, p) == $mem(s2, p) }

// ----------------------------------------------------------------------------
// typed pointers
// ----------------------------------------------------------------------------

function $typ($ptr)returns($ctype);
function $ref($ptr)returns(int);
function $ptr($ctype,int)returns($ptr);
axiom (forall #t:$ctype, #b:int :: {:weight 0} $typ($ptr(#t,#b))==#t);
axiom (forall #t:$ctype, #b:int :: {:weight 0} $ref($ptr(#t,#b))==#b);
//axiom (forall #p:$ptr :: {$typ(#p)} {$ref(#p)} $ptr($typ(#p), $ref(#p)) == #p);

// Use for computing references for ghost fields, instead of saying p+$field_offset(f) we use
// $ghost_ref(p,f). This has an added bonus that the embedding and path can be the same reagrdless
// of the current $meta
function $ghost_ref($ptr, $field) returns(int);

const unique $f_nonghost : $field;
function $ghost_emb($ptr) returns($ptr);
function $ghost_path($ptr) returns($field);

axiom (forall p:$ptr, f:$field, t:$ctype :: {:weight 0} {$ptr($ghost(t), $ghost_ref(p, f))}
  $ghost_emb($ptr($ghost(t), $ghost_ref(p, f))) == p && 
  $ghost_path($ptr($ghost(t), $ghost_ref(p, f))) == f );

axiom (forall r:int, t:$ctype :: {:weight 0} {$ptr(t, r)}
  $type_branch(t) != $ctype_ghost ==> $ghost_path($ptr(t, r)) == $f_nonghost);

axiom (forall p:$ptr, f:$field :: {$ghost_ref(p, f)}
  $in_range_spec_ptr($ghost_ref(p,f)));

function $physical_ref(p:$ptr, f:$field) returns(int);
// Make the physical fields, when we do not generate offset axioms, behave like ghost fields
//  (this is unsound, but not so much).
//axiom (forall p:$ptr, f:$field :: {:weight 0} {$physical_ref(p, f)}
//  $ghost_emb($physical_ref(p, f)) == p && $ghost_path($physical_ref(p, f)) == f );

function $array_path(basefield:$field, off:int) returns($field);

function $is_base_field($field) returns(bool);

function $array_path_1($field) returns($field);
function $array_path_2($field) returns(int);
axiom (forall fld:$field, off:int :: {:weight 0} {$array_path(fld, off)}
  !$is_base_field($array_path(fld, off)) &&
  $array_path_1($array_path(fld, off)) == fld &&
  $array_path_2($array_path(fld, off)) == off);

const $null:$ptr;
axiom $null == $ptr(^^void, 0);

//typed pointer test
function {:weight 0} $is(p:$ptr, t:$ctype) : bool
  { $typ(p)==t }
// create $ptr(...) function symbol, so later we have something to trigger on
axiom (forall #p:$ptr, #t:$ctype :: {$is(#p, #t)} $is(#p, #t) ==> #p == $ptr(#t, $ref(#p)));

function {:inline true} $ptr_cast(#p:$ptr,#t:$ctype) returns($ptr)
  { $ptr(#t, $ref(#p)) }

function {:inline true} $read_ptr(S:$state, p:$ptr, t:$ctype) returns($ptr)
  { $ptr(t, $mem(S, p)) }

//dot and (its "inverse") emb/path access
function $dot($ptr,$field) returns ($ptr);
function {:inline false} $emb(S:$state,#p:$ptr) returns ($ptr)
  { $int_to_ptr(S[$dot(#p, $f_emb)]) }
function {:inline true} $path(S:$state,#p:$ptr) returns($field)
  { $int_to_field(S[$dot(#p, $f_path)]) }

function {:inline true} $def_phys_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool, off:int) : bool
  { $is_base_field(f) && $field_parent_type(f) == partp &&
    $field_offset(f) == off &&
    (forall p:$ptr :: {$dot(p, f)} $is(p, partp) ==> $dot(p, f) == $ptr(tp, $ref(p) + off)) &&
    (forall S:$state, p:$ptr :: 
      {:vcc3 "builtin"}
      {S[$dot($dot(p, f), $f_typed)]}
    $typed2(S, p, partp) ==>
      $typed(S, $dot(p, f)) &&
      $emb(S, $dot(p, f)) == p &&
      $path(S, $dot(p, f)) == f &&
      //!$is_array_elt(S, $dot(p, f)) &&
      $is_volatile(S, $dot(p, f)) == isvolatile )
  }

function {:inline true} $def_ghost_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool) : bool
  { $is_base_field(f) && $field_parent_type(f) == partp &&
    (forall p:$ptr :: {$dot(p, f)} $is(p, partp) ==> $dot(p, f) == $ptr($ghost(tp), $ghost_ref(p, f))) &&
    (forall S:$state, p:$ptr :: 
      {:vcc3 "builtin"}
      {S[$dot($dot(p, f), $f_typed)]}
    $typed2(S, p, partp) ==>
      $typed(S, $dot(p, f)) &&
      $emb(S, $dot(p, f)) == p &&
      $path(S, $dot(p, f)) == f &&
      //!$is_array_elt(S, $dot(p, f)) &&
      $is_volatile(S, $dot(p, f)) == isvolatile )
  }

function {:inline true} $def_common_field(f:$field, tp:$ctype) : bool
  { $is_base_field(f) && 
    (forall p:$ptr :: {$dot(p, f)} $dot(p, f) == $ptr($ghost(tp), $ghost_ref(p, f)))
  }

function {:inline true} $def_writes(S:$state, time:int, ptrs:$ptrset) : bool
  {
    (forall p:$ptr :: {:vcc3 "L1"} {S[p]}
      $ghost_path(p) == $f_typed ==>
        $set_in($ghost_emb(p), ptrs) ==> $in_writes_at(time, $ghost_emb(p)) && $thread_owned_or_even_mutable(S, $ghost_emb(p)))
  }

const unique $f_typed : $field;
const unique $f_emb : $field;
const unique $f_path : $field;
const unique $f_is_volatile : $field;
const unique $f_timestamp : $field;
const unique $f_owns : $field;
const unique $f_owner : $field;
const unique $f_version : $field;
const unique $f_closed : $field;
const unique $f_ref_cnt : $field;

axiom $def_common_field($f_typed, ^^bool);
axiom $def_common_field($f_emb, ^^object);
axiom $def_common_field($f_path, ^^field);
axiom $def_common_field($f_is_volatile, ^^bool);
axiom $def_common_field($f_timestamp, ^^mathint);
axiom $def_common_field($f_owns, ^$#ptrset);
axiom $def_common_field($f_owner, ^^object);
axiom $def_common_field($f_version, ^^mathint);
axiom $def_common_field($f_closed, ^^bool);
axiom $def_common_field($f_ref_cnt, ^^mathint);

// it has no further embedding (not embedded inside another struct)
function {:inline true} $is_object_root(S:$state, p:$ptr) returns(bool)
  { $emb(S, p) == $ptr(^^root_emb, $ref(p)) }

function $is_volatile(S:$state, p:$ptr) returns(bool)
  { $int_to_bool(S[$dot(p, $f_is_volatile)]) }

axiom (forall S:$state, p:$ptr ::
  {:vcc3 "todo"}
  {$is_volatile(S, p)} $good_state(S) && $is_volatile(S, p) ==> $is_primitive_ch($typ(p)));

// just tmp
function {:inline true} $is_malloc_root(S:$state, p:$ptr) returns(bool)
  { $is_object_root(S, p) }

function $current_timestamp(S:$state) returns(int);
axiom (forall S:$state, p:$ptr ::
  {:vcc3 "todo"}
  {:weight 0} {$timestamp(S, p)}
  $timestamp(S, p) <= $current_timestamp(S) ||
  !$typed(S, p)
  );

function {:inline true} $is_fresh(M1:$state, M2:$state, p:$ptr) returns(bool)
  { $current_timestamp(M1) < $timestamp(M2, p) && $timestamp(M2, p) <= $current_timestamp(M2) }

function $in_writes_at(time:int, p:$ptr) returns(bool);

function {:inline true} $writable(S:$state, begin_time:int, p:$ptr) returns(bool)
  { $mutable(S, p) && ($in_writes_at(begin_time, p) || $timestamp(S, p) >= begin_time) }

function {:inline true} $top_writable(S:$state, begin_time:int, p:$ptr) returns(bool)
  { ($in_writes_at(begin_time, p) || $timestamp(S, p) >= begin_time)
    && $thread_owned_or_even_mutable(S, p) }

function {:inline true} $modifies(S0:$state, S1:$state, W:$ptrset) : bool
  { S1 == (lambda p:$ptr :: if !W[p] && !$irrelevant(S0, p) then S0[p] else S1[p]) &&
    $timestamp_post(S0, S1) }


// ----------------------------------------------------------------------------
// state
// ----------------------------------------------------------------------------
var $s: $state;

function $good_state($state) returns(bool);
function $invok_state($state) returns(bool);

function $has_volatile_owns_set(t:$ctype) returns(bool);
function $is_claimable($ctype) returns(bool);

function $owner(S:$state, p:$ptr) : $ptr
  { $int_to_ptr(S[$dot(p, $f_owner)]) }
function $closed(S:$state, p:$ptr) : bool
  { $int_to_bool(S[$dot(p, $f_closed)]) }
function $timestamp(S:$state, p:$ptr) : int
  { S[$dot(p, $f_timestamp)] }

function $typed(S:$state, p:$ptr) : bool
  { $int_to_bool(S[$dot(p, $f_typed)]) }

function $position_marker() : bool
  { true }

function {:weight 0} $owns(S:$state, p:$ptr) : $ptrset
  { $int_to_ptrset(S[$dot(p, $f_owns)]) }

function {:inline true} $wrapped(S:$state, #p:$ptr, #t:$ctype) returns(bool)
  { $closed(S, #p) && $owner(S, #p) == $me() && $typed2(S, #p, #t) && $kind_of(#t) != $kind_primitive && $is_non_primitive(#t) }

function {:inline true} $irrelevant(S:$state, p:$ptr) returns(bool)
  { $owner(S, p) != $me() || ($is_primitive_ch($typ(p)) && $closed(S, p)) }

function {:weight 0} $mutable(S:$state, p:$ptr) returns(bool)
  { $typed(S, p) && 
    ($typed(S, p) ==>
      if $is_primitive_ch($typ(p)) then 
        $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p)) 
      else
        $owner(S, p) == $me() && !$closed(S, p))
  }

function {:inline true} $thread_owned(S:$state, p:$ptr) returns(bool)
  { $typed(S, p) && $owner(S, p) == $me() }

function {:inline true} $thread_owned_or_even_mutable(S:$state, p:$ptr) returns(bool)
  { $typed(S, p) &&
    if $is_primitive_ch($typ(p)) then
      $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p))
    else
      $owner(S, p) == $me()
  }

function $in_range_phys_ptr(#r:int) returns(bool)
  { $in_range(0, #r, $arch_spec_ptr_start) }
function $in_range_spec_ptr(#r:int) returns(bool)
  { 0 == #r || #r > $arch_spec_ptr_start }
const $arch_spec_ptr_start : int; // arch-specific; to be defined by a compiler-generated axiom

function {:inline true} $typed2(S:$state, #p:$ptr, #t:$ctype) returns(bool)
  { $is(#p, #t) && $typed(S, #p) }

axiom (forall S:$state, #r:int, #t:$ctype ::
  {:vcc3 "todo"}
  {$typed(S, $ptr(#t, #r))}
  $typed(S, $ptr(#t, #r)) && $in_range_phys_ptr(#r) ==> $in_range_phys_ptr(#r + $sizeof(#t) - 1));

function {:inline true} $typed2_phys(S:$state, #p:$ptr, #t:$ctype) returns (bool)
  { $typed2(S, #p, #t) && ($typed2(S, #p, #t) ==> $in_range_phys_ptr($ref(#p))) }

function {:inline true} $typed2_spec(S:$state, #p:$ptr, #t:$ctype) returns (bool)
  { $typed2(S, #p, #t) && ($typed2(S, #p, #t) ==> $in_range_spec_ptr($ref(#p))) }

function {:inline true} $ptr_eq(p1:$ptr,p2:$ptr) returns(bool)
  { $ref(p1) == $ref(p2) }

function {:inline true} $ptr_neq(p1:$ptr,p2:$ptr) returns(bool)
  { $ref(p1) != $ref(p2) }

function {:inline true} $is_primitive_field_of(S:$state, #f:$ptr, #o:$ptr) returns(bool)
  { $is_primitive_ch($typ(#f)) && $emb(S, #f) == #o }


// ----------------------------------------------------------------------------
// thread locality
// ----------------------------------------------------------------------------

// for model reading
function $is_domain_root(S:$state, p:$ptr) returns(bool)
  { true }

function $in_wrapped_domain(S:$state, p:$ptr) returns(bool);
/*
  { (exists q:$ptr :: {$set_in2(p, $ver_domain($read_version(S, q)))} 
            $set_in(p, $ver_domain($read_version(S, q)))
         && $wrapped(S, q, $typ(q)) 
         && $is_domain_root(S, q)
         ) }
*/

function {:inline true} $thread_local_np(S:$state, p:$ptr) returns(bool)
  { !$is_primitive_ch($typ(p)) && 
    ($owner(S, p) == $me() || $in_wrapped_domain(S, p)) 
//     ($wrapped(S, $root(S, p), $typ($root(S, p))) && $set_in(p, $domain(S, $root(S, p)))))
  }

// required for reading
function $thread_local(S:$state, p:$ptr) returns(bool)
  { $typed(S, p) &&
    (($is_primitive_ch($typ(p)) && (!$is_volatile(S, p) || !$closed(S, $emb(S, p))) && $thread_local_np(S, $emb(S, p))) ||
     $thread_local_np(S, p)) }

function {:inline true} $thread_local2(S:$state, #p:$ptr, #t:$ctype) returns(bool)
  { $is(#p, #t) && $thread_local(S, #p) }
 

// ----------------------------------------------------------------------------
// Boogie/Z3 hacks
// ----------------------------------------------------------------------------

// Used as a trigger when we don't want the quantifier to be instantiated at all
//   (for example we just assert it or have it as a precondition)
// It could be any symbol that is not used anywhere else.
function $dont_instantiate($ptr) returns(bool);
function $dont_instantiate_int(int) returns(bool);
function $dont_instantiate_state($state) returns(bool);

// Voodoo, don't read it.
function $instantiate_int(int) returns(bool);
function $instantiate_bool(bool) returns(bool);
function $instantiate_ptr($ptr) returns(bool);
function $instantiate_ptrset($ptrset) returns(bool);
// Voodoo end.

// ----------------------------------------------------------------------------
// ownership protocol
// ----------------------------------------------------------------------------
function {:inline true} $inv(#s1:$state, #p:$ptr, typ:$ctype) returns (bool)
  { $inv2(#s1, #s1, #p, typ) }

function {:inline true} $inv2nt(S1:$state, S2:$state, p:$ptr) returns (bool)
  { $inv2(S1, S2, p, $typ(p)) }

// We generate axioms like:
//   inv2(S1,S2,p,T) <==> invariant of T
// for each struct/union T.
function $inv2(#s1:$state, #s2:$state, #p:$ptr, typ:$ctype) returns (bool);

// $in_extent_of(S, p1, p2) means that p1 is a pointer nested (at any level, including 0!) in p2
// $in_full_extent_of(...) assumes it can be any of the union fields, not only the selected one


function $function_entry($state) returns(bool);

function $full_stop($state) returns(bool);
function {:inline true} $full_stop_ext(t:$token, S:$state) returns(bool)
  { $good_state_ext(t, S) && $full_stop(S) }

function $file_name_is(id:int, tok:$token) returns(bool);

axiom (forall S:$state :: {$function_entry(S)}
  $function_entry(S) ==> $full_stop(S) && $current_timestamp(S) >= 0);

axiom (forall S:$state :: {$full_stop(S)}
  $full_stop(S) ==> $good_state(S) && $invok_state(S));

axiom (forall S:$state :: {$invok_state(S)}
  $invok_state(S) ==> $good_state(S));

function {:inline true} $closed_is_transitive(S:$state) returns (bool)
  { 
    (forall #p:$ptr,#q:$ptr ::
      {:vcc3 "todo"}
      {$set_in(#p, $owns(S, #q))}
      $good_state(S) &&
      $set_in(#p, $owns(S, #q)) && $closed(S, #q) ==> $closed(S, #p)  && $ref(#p) != 0)
 } 
        
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------

function $call_transition(S1:$state, S2:$state) returns(bool);

// Assumed after each meta/state update, means that the meta corresponds to the state
// and where in the source did the update happen.
function $good_state_ext(id:$token, S:$state) returns(bool);
axiom (forall id:$token, S:$state :: {$good_state_ext(id, S)}
  $good_state_ext(id, S) ==> $good_state(S));

// ----------------------------------------------------------------------------
// model-viewer support
// ----------------------------------------------------------------------------

function $local_value_is(S1:$state, pos:$token, local_name:$token, val:int, t:$ctype) returns(bool);
function $local_value_is_ptr(S1:$state, pos:$token, local_name:$token, val:$ptr, t:$ctype) returns(bool);
function $read_ptr_m(S:$state, p:$ptr, t:$ctype) returns($ptr);

function $type_code_is(x:int, tp:$ctype) returns(bool);
// idx==0 - return type
function $function_arg_type(fnname:$pure_function, idx:int, tp:$ctype) returns(bool);


// ----------------------------------------------------------------------------
// Procedures
// ----------------------------------------------------------------------------

procedure $write_int(p:$ptr, v:int);
  modifies $s;
  ensures $s == (lambda q:$ptr :: if p != q then old($s)[q] else v);
  ensures $timestamp_post_strict(old($s), $s);

function {:inline true} $timestamp_is_now(S:$state, p:$ptr) returns(bool)
  { $timestamp(S, p) == $current_timestamp(S) }

function {:inline true} $now_writable(S:$state, p:$ptr) returns(bool)
  { $timestamp_is_now(S, p) && $mutable(S, p) }

function {:inline true} $timestamp_post(M1:$state, M2:$state) returns(bool)
  { $current_timestamp(M1) <= $current_timestamp(M2) &&
    (forall p:$ptr :: 
      {:vcc3 "todo"} {:weight 0}
      {$timestamp(M2, p)}
      $timestamp(M1, p) <= $timestamp(M2, p)) &&
    $call_transition(M1, M2)
  }

function {:inline true} $timestamp_post_strict(M1:$state, M2:$state) returns(bool)
  { $current_timestamp(M1) < $current_timestamp(M2) &&
    (forall p:$ptr ::
      {:vcc3 "todo"} {:weight 0}
      {$timestamp(M2, p)} 
      $timestamp(M1, p) <= $timestamp(M2, p)) &&
    $call_transition(M1, M2)
  }



// -----------------------------------------------------------------------
// Span & extent
// -----------------------------------------------------------------------

function $full_extent(#p:$ptr) returns($ptrset);
function $extent(S:$state, #p:$ptr) returns($ptrset);

function $span(S:$state, o:$ptr) returns($ptrset)
  { (lambda p:$ptr :: o == p || $emb(S, p) == o) }
function $first_option_typed(S:$state, #p:$ptr) returns(bool);

function {:inline true} $struct_extent(#p:$ptr) returns($ptrset)
  { $full_extent(#p) }

function $volatile_span(S:$state, #p:$ptr) returns($ptrset);
axiom (forall S:$state, p:$ptr, q:$ptr ::
  {:vcc3 "todo"}
  {$set_in(p, $volatile_span(S, q))}
  $set_in(p, $volatile_span(S, q)) <==> p == q || ($is_volatile(S, p) && $set_in(p, $span(S, q))));


// -----------------------------------------------------------------------
// Sets of pointers
// -----------------------------------------------------------------------

type $ptrset = [$ptr]bool;

function {:inline true} $set_in(p:$ptr,s:$ptrset) : bool
  { s[p] }

function $set_empty() : $ptrset
  { (lambda o:$ptr :: false) }

function $set_singleton(p:$ptr) : $ptrset
  { (lambda o:$ptr :: o == p) }

function $non_null_set_singleton(p:$ptr) : $ptrset
  { (lambda o:$ptr :: $ptr_neq(p, $null) && p == o) }

function $set_union(A:$ptrset, B:$ptrset) : $ptrset
  { (lambda o:$ptr :: A[o] || B[o]) }

function $set_difference(A:$ptrset, B:$ptrset) : $ptrset
  { (lambda o:$ptr :: A[o] && !B[o]) }

function $set_intersection(A:$ptrset, B:$ptrset) : $ptrset
  { (lambda o:$ptr :: A[o] && B[o]) }

// TODO: need to think about these
function $set_subset(A:$ptrset, B:$ptrset) : bool
  { (forall o:$ptr :: {$set_in(o, A)} {$set_in(o, B)} $set_in(o, A) ==> $set_in(o, B)) }

// to be used only positively
function $set_eq($ptrset, $ptrset) returns (bool);
axiom (forall #a: $ptrset, #b: $ptrset :: {:weight 0} {$set_eq(#a,#b)}
  (forall #o: $ptr :: {:weight 0} {$dont_instantiate(#o)} $set_in(#o, #a) <==> $set_in(#o, #b)) ==> $set_eq(#a, #b));
axiom (forall #a: $ptrset, #b: $ptrset :: {:weight 0} {$set_eq(#a,#b)}
  $set_eq(#a, #b) ==> #a == #b);

function $set_cardinality($ptrset) returns(int);

axiom ($set_cardinality($set_empty()) == 0);
axiom (forall p:$ptr :: {:weight 0} $set_cardinality($set_singleton(p)) == 1);

function $set_universe() returns($ptrset);
axiom (forall #o: $ptr :: {:weight 0} {$set_in(#o, $set_universe())} $set_in(#o, $set_universe()));

function $set_disjoint(s1:$ptrset, s2:$ptrset) returns(bool);
function $id_set_disjoint(p:$ptr, s1:$ptrset, s2:$ptrset) returns(int);

axiom (forall p:$ptr, s1:$ptrset, s2:$ptrset :: {:weight 0} {$set_disjoint(s1, s2), $set_in(p, s1)}
  $set_disjoint(s1, s2) && $set_in(p, s1) ==> 
    $id_set_disjoint(p, s1, s2) == 1);
axiom (forall p:$ptr, s1:$ptrset, s2:$ptrset :: {:weight 0} {$set_disjoint(s1, s2), $set_in(p, s2)}
  $set_disjoint(s1, s2) && $set_in(p, s2) ==> 
    $id_set_disjoint(p, s1, s2) == 2);

axiom (forall s1:$ptrset, s2:$ptrset :: {:weight 0} {$set_disjoint(s1, s2)}
  (forall p:$ptr :: {$dont_instantiate(p)}
     ($set_in(p, s1) ==> !$set_in(p, s2)) && ($set_in(p, s2) ==> !$set_in(p, s1))) 
  ==> $set_disjoint(s1, s2));


function $set_in3($ptr, $ptrset) returns(bool);
function $set_in2($ptr, $ptrset) returns(bool);

//function $in_some_owns($ptr) returns(bool);
//axiom (forall p:$ptr, S1:$state, p1:$ptr :: 
//  {:weight 0} {$set_in(p, $owns(S1, p1))}
//  $set_in(p, $owns(S1, p1)) ==> $in_some_owns(p));
//
//axiom (forall p:$ptr, S1:$state, p1:$ptr :: 
//  {:weight 0} {$set_in2(p, $owns(S1, p1)), $in_some_owns(p)}
//  $set_in(p, $owns(S1, p1)) <==> $set_in2(p, $owns(S1, p1)));

axiom (forall p:$ptr, s:$ptrset :: {:weight 0} {$set_in(p, s)}
  $set_in(p, s) <==> $set_in2(p, s));
axiom (forall p:$ptr, s:$ptrset :: {:weight 0} {$set_in(p, s)}
  $set_in(p, s) <==> $set_in3(p, s));

function $set_in0($ptr, $ptrset) returns(bool);
axiom (forall p:$ptr, s:$ptrset :: {:weight 0} {$set_in0(p, s)}
  $set_in(p, s) <==> $set_in0(p, s));


function sk_hack(bool) returns(bool);
function $start_here() returns(bool);


// --------------------------------------------------------------------------------
// Arithmetic
// --------------------------------------------------------------------------------

function {:inline true} $in_range(min:int, val:int, max:int) returns(bool)
  { min <= val && val <= max }

function {:inline true} $bool_to_int(v:bool) returns(int)
  { if v then 1 else 0 }

function {:inline true} $int_to_bool(x:int) returns(bool)
  { x != 0 }

// TODO check if still needed
// a hack, used when parameter to ITE is a quntified variable to prevent Z3 from crashing
function {:weight 0} $bool_id(x:bool) returns(bool) { x }


const $min.i1:int;
const $max.i1:int;
const $min.i2:int;
const $max.i2:int;
const $min.i4:int;
const $max.i4:int;
const $min.i8:int;
const $max.i8:int;
const $max.u1:int;
const $max.u2:int;
const $max.u4:int;
const $max.u8:int;

axiom ($min.i1 == -128);
axiom ($max.i1 ==  127);
axiom ($min.i2 == -32768);
axiom ($max.i2 ==  32767);
axiom ($min.i4 == -(65536*32768)  );
axiom ($max.i4 ==   65536*32768 -1);
axiom ($min.i8 == -(65536*65536*65536*32768)  );
axiom ($max.i8 ==   65536*65536*65536*32768 -1);
axiom ($max.u1 ==  255);
axiom ($max.u2 ==  65535);
axiom ($max.u4 ==  65536*65536-1);
axiom ($max.u8 ==  65536*65536*65536*65536-1);

function {:inline true} $in_range_i1(x:int) returns(bool) { $in_range($min.i1, x, $max.i1) }
function {:inline true} $in_range_i2(x:int) returns(bool) { $in_range($min.i2, x, $max.i2) }
function {:inline true} $in_range_i4(x:int) returns(bool) { $in_range($min.i4, x, $max.i4) }
function {:inline true} $in_range_i8(x:int) returns(bool) { $in_range($min.i8, x, $max.i8) }
function {:inline true} $in_range_u1(x:int) returns(bool) { $in_range(0, x, $max.u1) }
function {:inline true} $in_range_u2(x:int) returns(bool) { $in_range(0, x, $max.u2) }
function {:inline true} $in_range_u4(x:int) returns(bool) { $in_range(0, x, $max.u4) }
function {:inline true} $in_range_u8(x:int) returns(bool) { $in_range(0, x, $max.u8) }
function {:inline true} $in_range_ptr(p:$ptr) returns(bool) { $in_range_u8($ref(p)) }

function {:inline true} $in_range_div_i1(x:int, y:int) returns(bool) { y != -1 || x != $min.i1 }
function {:inline true} $in_range_div_i2(x:int, y:int) returns(bool) { y != -1 || x != $min.i2 }
function {:inline true} $in_range_div_i4(x:int, y:int) returns(bool) { y != -1 || x != $min.i4 }
function {:inline true} $in_range_div_i8(x:int, y:int) returns(bool) { y != -1 || x != $min.i8 }

function {:weight 0} $byte_ptr_subtraction(p1:$ptr, p2:$ptr) returns(int)
  { $ref(p1) - $ref(p2) }
function $_pow2(int) returns(int);
axiom 
$_pow2(0) == 1 && $_pow2(1) == 2 && $_pow2(2) == 4 && $_pow2(3) == 8 && $_pow2(4) == 16 && $_pow2(5) == 32 &&
$_pow2(6) == 64 && $_pow2(7) == 128 && $_pow2(8) == 256 && $_pow2(9) == 512 && $_pow2(10) == 1024 && $_pow2(11) ==
 2048 && $_pow2(12) == 4096 && $_pow2(13) == 8192 && $_pow2(14) == 16384 && $_pow2(15) == 32768 && $_pow2(16) ==
65536 && $_pow2(17) == 131072 && $_pow2(18) == 262144 && $_pow2(19) == 524288 && $_pow2(20) == 1048576 && $_pow2(21)
== 2097152 && $_pow2(22) == 4194304 && $_pow2(23) == 8388608 && $_pow2(24) == 16777216 && $_pow2(25) == 33554432 &&
$_pow2(26) == 67108864 && $_pow2(27) == 134217728 && $_pow2(28) == 268435456 && $_pow2(29) == 536870912 && $_pow2(30)
== 1073741824 && $_pow2(31) == 2147483648 && $_pow2(32) == 4294967296 && $_pow2(33) == 8589934592 && $_pow2(34) ==
17179869184 && $_pow2(35) == 34359738368 && $_pow2(36) == 68719476736 && $_pow2(37) == 137438953472 && $_pow2(38) ==
274877906944 && $_pow2(39) == 549755813888 && $_pow2(40) == 1099511627776 && $_pow2(41) == 2199023255552 && $_pow2(42)
== 4398046511104 && $_pow2(43) == 8796093022208 && $_pow2(44) == 17592186044416 && $_pow2(45) == 35184372088832
&& $_pow2(46) == 70368744177664 && $_pow2(47) == 140737488355328 && $_pow2(48) == 281474976710656 && $_pow2(49) ==
562949953421312 && $_pow2(50) == 1125899906842624 && $_pow2(51) == 2251799813685248 && $_pow2(52) == 4503599627370496
&& $_pow2(53) == 9007199254740992 && $_pow2(54) == 18014398509481984 && $_pow2(55) == 36028797018963968 && $_pow2(56)
== 72057594037927936 && $_pow2(57) == 144115188075855872 && $_pow2(58) == 288230376151711744 && $_pow2(59) ==
 576460752303423488 && $_pow2(60) == 1152921504606846976 && $_pow2(61) == 2305843009213693952 && $_pow2(62) ==
4611686018427387904 && $_pow2(63) == 9223372036854775808;

function $in_range_ubits(bits:int, v:int) returns(bool)
  { $in_range(0, v, $_pow2(bits) - 1) }

function $unchecked_sbits(bits:int, v:int) returns(int);
axiom (forall bits:int, v:int :: {$unchecked_sbits(bits, v)}
  $in_range_sbits(bits, $unchecked_sbits(bits, v)) &&
  ($in_range_sbits(bits, v) ==> $unchecked_sbits(bits, v) == v));

function $in_range_sbits(bits:int, v:int) returns(bool)
  { $in_range(-$_pow2(bits-1), v, $_pow2(bits-1) - 1) }

function $unchecked_ubits(bits:int, v:int) returns(int);
axiom (forall bits:int, v:int :: {$unchecked_ubits(bits, v)}
  $in_range_ubits(bits, $unchecked_ubits(bits, v)) &&
  ($in_range_ubits(bits, v) ==> $unchecked_ubits(bits, v) == v));

function $_or(t:$ctype, x:int, y:int) returns(int);
function $_xor(t:$ctype, x:int, y:int) returns(int);
function $_and(t:$ctype, x:int, y:int) returns(int);
function $_not(t:$ctype, x:int) returns(int);

function {:weight 0} $unchk_add(t:$ctype, x:int, y:int) returns(int) { $unchecked(t, x + y) }
function {:weight 0} $unchk_sub(t:$ctype, x:int, y:int) returns(int) { $unchecked(t, x - y) }
function {:weight 0} $unchk_mul(t:$ctype, x:int, y:int) returns(int) { $unchecked(t, x * y) }
function {:weight 0} $unchk_div(t:$ctype, x:int, y:int) returns(int) { $unchecked(t, x / y) }
function {:weight 0} $unchk_mod(t:$ctype, x:int, y:int) returns(int) { $unchecked(t, x % y) }

function {:weight 0} $_shl(t:$ctype, x:int, y:int) returns(int)
  { $unchecked(t, x * $_pow2(y)) }
function {:weight 0} $_shr(x:int, y:int) returns(int)
  { x / $_pow2(y) }

function $bv_extract_signed(val:int, val_bitsize:int, from:int, to:int) returns(int);
function $bv_extract_unsigned(val:int, val_bitsize:int, from:int, to:int) returns(int);
function $bv_update(val:int, val_bitsize:int, from:int, to:int, repl:int) returns(int);

axiom (forall x:int, from:int, to:int, xs:int, val:int :: 
 { $bv_update(x, xs, from, to, val) }
 0 <= from && from < to && to <= xs ==>
 0 <= val && val < $_pow2(to - from) ==> 
   0 <= $bv_update(x, xs, from, to, val) && $bv_update(x, xs, from, to, val) < $_pow2(xs));

axiom (forall from:int, to:int, xs:int :: 
 { $bv_update(0, xs, from, to, 0) }
 0 <= from && from < to && to <= xs ==> $bv_update(0, xs, from, to, 0) == 0);

axiom (forall from:int, to:int, val:int, x:int, xs:int :: 
  {$bv_extract_signed($bv_update(x, xs, from, to, val), xs, from, to)}
  0 <= from && from < to && to <= xs ==>
  -$_pow2(to - from - 1) <= val && val < $_pow2(to - from - 1) ==> 
    $bv_extract_signed($bv_update(x, xs, from, to, val), xs, from, to) == val);

axiom (forall from:int, to:int, val:int, x:int, xs:int :: 
  {$bv_extract_unsigned($bv_update(x, xs, from, to, val), xs, from, to)}
  0 <= from && from < to && to <= xs ==>
  0 <= val && val < $_pow2(to - from) ==> 
    $bv_extract_unsigned($bv_update(x, xs, from, to, val), xs, from, to) == val);

axiom (forall from:int, to:int, x:int, xs:int :: 
  {$bv_extract_signed(x, xs, from, to)}
  0 <= from && from < to && to <= xs ==>
  $in_range(-$_pow2(to - from - 1), $bv_extract_signed(x, xs, from, to), $_pow2(to - from - 1) - 1));

axiom (forall from:int, to:int, x:int, xs:int :: 
  {$bv_extract_unsigned(x, xs, from, to)}
  0 <= from && from < to && to <= xs ==>
  $in_range(0, $bv_extract_unsigned(x, xs, from, to), $_pow2(to - from) - 1));

axiom (forall from:int, to:int, val:int, x:int, xs:int, from2:int, to2:int :: 
  {$bv_extract_signed($bv_update(x, xs, from, to, val), xs, from2, to2)}
  0 <= from && from < to && to <= xs ==>
  0 <= from2 && from2 < to2 && to2 <= xs ==>
  (to2 <= from || to <= from2) ==>
  $bv_extract_signed($bv_update(x, xs, from, to, val), xs, from2, to2) == $bv_extract_signed(x, xs, from2, to2));

axiom (forall from:int, to:int, val:int, x:int, xs:int, from2:int, to2:int :: 
  {$bv_extract_unsigned($bv_update(x, xs, from, to, val), xs, from2, to2)}
  0 <= from && from < to && to <= xs ==>
  0 <= from2 && from2 < to2 && to2 <= xs ==>
  (to2 <= from || to <= from2) ==>
  $bv_extract_unsigned($bv_update(x, xs, from, to, val), xs, from2, to2) == $bv_extract_unsigned(x, xs, from2, to2));

axiom (forall from:int, to:int, xs:int ::
  {$bv_extract_signed(0, xs, from, to)}
  0 <= from && from < to && to <= xs ==>
    $bv_extract_signed(0, xs, from, to) == 0);

axiom (forall from:int, to:int, xs:int ::
  {$bv_extract_unsigned(0, xs, from, to)}
  0 <= from && from < to && to <= xs ==>
    $bv_extract_unsigned(0, xs, from, to) == 0);

axiom (forall from:int, to:int, val:int, xs:int ::
  {$bv_extract_unsigned(val, xs, from, to)}
  0 <= from && from < to && to <= xs && 0 <= val ==>
    $bv_extract_unsigned(val, xs, from, to) == (val / $_pow2(from)) % $_pow2(to - from));

axiom (forall from:int, to:int, val:int, xs:int ::
  {$bv_extract_signed(val, xs, from, to)}
  0 <= from && from < to && to <= xs && 0 <= val && ((val / $_pow2(from)) % $_pow2(to - from) < $_pow2(to - from - 1)) ==>
    $bv_extract_signed(val, xs, from, to) == (val / $_pow2(from)) % $_pow2(to - from));

axiom (forall from:int, to:int, val:int, xs:int ::
  {$bv_extract_signed(val, xs, from, to)}
  0 <= from && from < to && to <= xs && 0 <= val && ((val / $_pow2(from)) % $_pow2(to - from) >= $_pow2(to - from - 1)) ==>
    $bv_extract_signed(val, xs, from, to) == $_pow2(to - from - 1) - (val / $_pow2(from)) % $_pow2(to - from));

/*
axiom (forall from:int, to:int, val:int :: {$sign_extend(from, to, $_bv_extract(val, to, 0, from))}
  (-$_pow2(from - 1) <= val && val < $_pow2(from - 1) ==> $sign_extend(from, to, $bv_extract(val, to, 0, from)) == val));

axiom (forall from:int, to:int, val:int :: {$sign_extend(from, to, val)}
  -$_pow2(from - 1) <= $sign_extend(from, to, val) && $sign_extend(from, to, val) < $_pow2(from - 1));

axiom (forall as:int, val:int, vs:int, from:int, to:int, bs:int ::
  {$bv_concat(0, as, $bv_extract(val, vs, from, to), bs)}
  as >= 1 ==>
    $bv_concat(0, as, $bv_extract(val, vs, from, to), bs) >= 0 &&
    $bv_concat(0, as, $bv_extract(val, vs, from, to), bs) < $_pow2(to - from));
    
axiom (forall s:int, from:int, to:int :: {$bv_extract(0, s, from, to)} 
  $bv_extract(0, s, from, to) == 0);

axiom (forall s1:int, s2: int :: {$bv_concat(0, s1, 0, s2)} 
  $bv_concat(0, s1, 0, s2) == 0);
*/

function $unchecked(t:$ctype, val:int) returns(int);
function $in_range_t(t:$ctype, x:int) returns(bool);

axiom (forall val:int :: {$in_range_t(^^i1, val)} $in_range_t(^^i1, val) <==> $in_range_i1(val));
axiom (forall val:int :: {$in_range_t(^^i2, val)} $in_range_t(^^i2, val) <==> $in_range_i2(val));
axiom (forall val:int :: {$in_range_t(^^i4, val)} $in_range_t(^^i4, val) <==> $in_range_i4(val));
axiom (forall val:int :: {$in_range_t(^^i8, val)} $in_range_t(^^i8, val) <==> $in_range_i8(val));
axiom (forall val:int :: {$in_range_t(^^u1, val)} $in_range_t(^^u1, val) <==> $in_range_u1(val));
axiom (forall val:int :: {$in_range_t(^^u2, val)} $in_range_t(^^u2, val) <==> $in_range_u2(val));
axiom (forall val:int :: {$in_range_t(^^u4, val)} $in_range_t(^^u4, val) <==> $in_range_u4(val));
axiom (forall val:int :: {$in_range_t(^^u8, val)} $in_range_t(^^u8, val) <==> $in_range_u8(val));
axiom (forall val:int :: {$in_range_t(^^mathint, val)} $in_range_t(^^mathint, val));

axiom (forall t:$ctype, val:int :: {$unchecked(t, val)} $in_range_t(t, val) ==> $unchecked(t, val) == val);
axiom (forall t:$ctype, val:int :: {$unchecked(t, val)} $in_range_t(t, $unchecked(t, val)));

axiom (forall val:int :: { $unchecked(^^u1, $unchecked(^^i1, val)) } $in_range_u1(val) ==> $unchecked(^^u1, $unchecked(^^i1, val)) == val);
axiom (forall val:int :: { $unchecked(^^u2, $unchecked(^^i2, val)) } $in_range_u2(val) ==> $unchecked(^^u2, $unchecked(^^i2, val)) == val);
axiom (forall val:int :: { $unchecked(^^u4, $unchecked(^^i4, val)) } $in_range_u4(val) ==> $unchecked(^^u4, $unchecked(^^i4, val)) == val);
axiom (forall val:int :: { $unchecked(^^u8, $unchecked(^^i8, val)) } $in_range_u8(val) ==> $unchecked(^^u8, $unchecked(^^i8, val)) == val);
axiom (forall val:int :: { $unchecked(^^i1, $unchecked(^^u1, val)) } $in_range_i1(val) ==> $unchecked(^^i1, $unchecked(^^u1, val)) == val);
axiom (forall val:int :: { $unchecked(^^i2, $unchecked(^^u2, val)) } $in_range_i2(val) ==> $unchecked(^^i2, $unchecked(^^u2, val)) == val);
axiom (forall val:int :: { $unchecked(^^i4, $unchecked(^^u4, val)) } $in_range_i4(val) ==> $unchecked(^^i4, $unchecked(^^u4, val)) == val);
axiom (forall val:int :: { $unchecked(^^i8, $unchecked(^^u8, val)) } $in_range_i8(val) ==> $unchecked(^^i8, $unchecked(^^u8, val)) == val);

// The semantics of $_and/$_or/...
//   Clip the number given to the appropriate range (i.e. take the lowest N bits) and perform the operation.

axiom (forall t:$ctype, x:int, y:int, z:int :: { x % $_pow2(y), $_and(t, x, z) } 
  $in_range_t(t, x) &&
  $in_range_t(t, $_pow2(y) - 1) &&
  x >= 0 ==>
    x % $_pow2(y) == $_and(t, x, $_pow2(y) - 1));

axiom (forall i: int, j: int :: { i / j }  0 <= i && 0 < j ==> i / j <= i);

axiom (forall i: int, j: int :: { i / j }  i > 0 && j > 0 ==> i - j < (i / j) * j && (i / j) * j <= i);
axiom (forall i: int :: { i / i }  i != 0 ==> i / i == 1);
axiom (forall i: int :: { 0 / i }  i != 0 ==> 0 / i == 0);

// from Spec# prelude, needs review
axiom (forall x: int, y: int :: { x % y } { x / y } x % y == x - x / y * y);
axiom (forall x: int, y: int :: { x % y } 0 <= x && 0 < y ==> 0 <= x % y && x % y < y);
axiom (forall x: int, y: int :: { x % y } 0 <= x && y < 0 ==> 0 <= x % y && x % y < 0 - y);
axiom (forall x: int, y: int :: { x % y } x <= 0 && 0 < y ==> 0 - y < x % y && x % y <= 0);
axiom (forall x: int, y: int :: { x % y } x <= 0 && y < 0 ==> y < x % y && x % y <= 0);
// Those three use +/- in triggers, won't work in Z3
//axiom (forall x: int, y: int :: { (x + y) % y } 0 <= x && 0 <= y ==> (x + y) % y == x % y);
//axiom (forall x: int, y: int :: { (y + x) % y } 0 <= x && 0 <= y ==> (y + x) % y == x % y);
//axiom (forall x: int, y: int :: { (x - y) % y } 0 <= x - y && 0 <= y ==> (x - y) % y == x % y);

// Too expensive
//axiom (forall a: int, b: int, d: int :: { a % d, b % d } 2 <= d && a % d == b % d && a < b ==> a + d <= b);

axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } 0 <= x && $in_range_t(t, x) ==> 0 <= $_and(t, x, y) && $_and(t, x, y) <= x);
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } 0 <= x && 0 <= y && $in_range_t(t, x) && $in_range_t(t, y) ==> $_and(t, x, y) <= x && $_and(t, x, y) <= y);
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } 0 <= x && 0 <= y && $in_range_t(t, x) && $in_range_t(t, y) ==> 0 <= $_or(t, x, y) && $_or(t, x, y) <= x + y);
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } 0 <= x && 0 <= y && $in_range_t(t, x) && $in_range_t(t, y) ==> x <= $_or(t, x, y) && y <= $_or(t, x, y));
axiom (forall t:$ctype, x: int, y: int, z: int :: { $_or(t, x,y), $_pow2(z) } 
  0 <= x && 0 <= y && 0 <= z && z < 64 && x < $_pow2(z) && y < $_pow2(z) && $in_range_t(t, x) && $in_range_t(t, y) ==> $_or(t, x, y) < $_pow2(z) );

axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } $in_range_u1(x) && $in_range_u1(y) ==> $in_range_u1($_or(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } $in_range_u2(x) && $in_range_u2(y) ==> $in_range_u2($_or(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } $in_range_u4(x) && $in_range_u4(y) ==> $in_range_u4($_or(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } $in_range_u8(x) && $in_range_u8(y) ==> $in_range_u8($_or(t, x, y)));

axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } $in_range_u1(x) && $in_range_u1(y) ==> $in_range_u1($_and(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } $in_range_u2(x) && $in_range_u2(y) ==> $in_range_u2($_and(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } $in_range_u4(x) && $in_range_u4(y) ==> $in_range_u4($_and(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } $in_range_u8(x) && $in_range_u8(y) ==> $in_range_u8($_and(t, x, y)));

axiom (forall t:$ctype, x: int, y: int :: { $_xor(t, x, y) } $in_range_u1(x) && $in_range_u1(y) ==> $in_range_u1($_xor(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_xor(t, x, y) } $in_range_u2(x) && $in_range_u2(y) ==> $in_range_u2($_xor(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_xor(t, x, y) } $in_range_u4(x) && $in_range_u4(y) ==> $in_range_u4($_xor(t, x, y)));
axiom (forall t:$ctype, x: int, y: int :: { $_xor(t, x, y) } $in_range_u8(x) && $in_range_u8(y) ==> $in_range_u8($_xor(t, x, y)));

axiom (forall t:$ctype, x: int :: { $_not(t, x) }  $in_range_t(t, $_not(t, x)));

//axiom (forall t:$ctype, x: int :: { $_not(t, x) } $in_range_u4(x) ==> $in_range_u4($_not(t, x)));
//axiom (forall t:$ctype, x: int :: { $_not(t, x) } $in_range_u8(x) ==> $in_range_u8($_not(t, x)));

axiom (forall t:$ctype, x: int :: { $_or(t, x, $_not(t, x)) }  $_or(t, x, $_not(t, x)) == $_not(t, 0));
axiom (forall t:$ctype, x: int :: { $_and(t, x, $_not(t, x)) }  $_and(t, x, $_not(t, x)) == 0);
axiom (forall t:$ctype, x: int :: { $_or(t, x, 0) }  $in_range_t(t, x) ==> $_or(t, x, 0) == x);
axiom (forall t:$ctype, x: int :: { $_or(t, x, $_not(t, 0)) }  $_or(t, x, $_not(t, 0)) == $_not(t, 0));
axiom (forall t:$ctype, x: int :: { $_or(t, x, x) } $in_range_t(t, x) ==>  $_or(t, x, x) == x);
axiom (forall t:$ctype, x: int :: { $_and(t, x, 0) }  $_and(t, x, 0) == 0);
axiom (forall t:$ctype, x: int :: { $_and(t, x, $_not(t, 0)) } $in_range_t(t, x) ==>  $_and(t, x, $_not(t, 0)) == x);
axiom (forall t:$ctype, x: int :: { $_and(t, x, x) } $in_range_t(t, x) ==> $_and(t, x,x) == x);
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, $_or(t, x, y), y) } $_and(t, $_or(t, x, y), y) == y);
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, $_or(t, x, y), x) } $_and(t, $_or(t, x, y), x) == x);
axiom (forall t:$ctype, x: int :: { $_xor(t, x, 0) }  $in_range_t(t, x) ==> $_xor(t, x, 0) == x);
axiom (forall t:$ctype, x: int :: { $_xor(t, x, x) }  $_xor(t, x, x) == 0);
axiom (forall t:$ctype, x: int :: { $_xor(t, x, $_not(t, 0)) }  $_xor(t, x, $_not(t, 0)) == $_not(t, x));
axiom (forall t:$ctype, x: int :: { $_not(t, $_not(t, x)) }  $in_range_t(t, x) ==> $_not(t, $_not(t, x)) == x);
axiom (forall t:$ctype, x: int, y: int :: { $_or(t, x, y) } $_or(t, x, y) == $_or(t, y, x));
axiom (forall t:$ctype, x: int, y: int :: { $_xor(t, x, y) } $_xor(t, x, y) == $_xor(t, y, x));
axiom (forall t:$ctype, x: int, y: int :: { $_and(t, x, y) } $_and(t, x, y) == $_and(t, y, x));
  

// extra function symbol for multiplication to prevent z3 from applying commutativity half-heartedly
function {:weight 0} $_mul(x:int, y:int) returns (int) { x * y }

// --------------------------------------------------------------------------------
// Conversion functions
// --------------------------------------------------------------------------------

function $ptr_to_int($ptr) returns(int);
function $int_to_ptr(int) returns($ptr);
axiom (forall p:$ptr :: $int_to_ptr($ptr_to_int(p)) == p);

function $field_to_int($field) returns(int);
function $int_to_field(int) returns($field);
axiom (forall p:$field :: $int_to_field($field_to_int(p)) == p);

function $ptrset_to_int($ptrset) returns(int);
function $int_to_ptrset(int) returns($ptrset);
axiom (forall p:$ptrset :: $int_to_ptrset($ptrset_to_int(p)) == p);

function $ptr_to_u8($ptr) returns(int);
function $ptr_to_i8($ptr) returns(int);
function $ptr_to_u4($ptr) returns(int);
function $ptr_to_i4($ptr) returns(int);
function $ptr_to_u2($ptr) returns(int);
function $ptr_to_i2($ptr) returns(int);
function $ptr_to_u1($ptr) returns(int);
function $ptr_to_i1($ptr) returns(int);

axiom ($ptr_to_u8($null) == 0);
axiom ($ptr_to_i8($null) == 0);
axiom ($ptr_to_u4($null) == 0);
axiom ($ptr_to_i4($null) == 0);
axiom ($ptr_to_u2($null) == 0);
axiom ($ptr_to_i2($null) == 0);
axiom ($ptr_to_u1($null) == 0);
axiom ($ptr_to_i1($null) == 0);

function {:inline true} $u8_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $i8_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $u4_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $i4_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $u2_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $i2_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $u1_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }
function {:inline true} $i1_to_ptr(x : int) returns($ptr) { $ptr(^^void, x)  }

axiom (forall p:$ptr :: { $ptr_to_u8(p) } $in_range_u8($ref(p)) ==> $ptr_to_u8(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_i8(p) } $in_range_i8($ref(p)) ==> $ptr_to_i8(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_u4(p) } $in_range_u4($ref(p)) ==> $ptr_to_u4(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_i4(p) } $in_range_i4($ref(p)) ==> $ptr_to_i4(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_u2(p) } $in_range_u2($ref(p)) ==> $ptr_to_u2(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_i2(p) } $in_range_i2($ref(p)) ==> $ptr_to_i2(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_u1(p) } $in_range_u1($ref(p)) ==> $ptr_to_u1(p) == $ref(p));
axiom (forall p:$ptr :: { $ptr_to_i1(p) } $in_range_i1($ref(p)) ==> $ptr_to_i1(p) == $ref(p));

// --------------------------------------------------------------------------------
// Floating point arithmetic - currently uninterpreted
// --------------------------------------------------------------------------------

function $add_f4(x:$primitive, y:$primitive) returns($primitive);
function $sub_f4(x:$primitive, y:$primitive) returns($primitive);
function $mul_f4(x:$primitive, y:$primitive) returns($primitive);
function $div_f4(x:$primitive, y:$primitive) returns($primitive);
function $neg_f4(x:$primitive) returns($primitive);
function $lt_f4(x:$primitive, y:$primitive) returns(bool);
function $leq_f4(x:$primitive, y:$primitive) returns(bool);
function $gt_f4(x:$primitive, y:$primitive) returns(bool);
function $geq_f4(x:$primitive, y:$primitive) returns(bool);
function $add_f8(x:$primitive, y:$primitive) returns($primitive);
function $sub_f8(x:$primitive, y:$primitive) returns($primitive);
function $mul_f8(x:$primitive, y:$primitive) returns($primitive);
function $div_f8(x:$primitive, y:$primitive) returns($primitive);
function $neg_f8(x:$primitive) returns($primitive);
function $lt_f8(x:$primitive, y:$primitive) returns(bool);
function $leq_f8(x:$primitive, y:$primitive) returns(bool);
function $gt_f8(x:$primitive, y:$primitive) returns(bool);
function $geq_f8(x:$primitive, y:$primitive) returns(bool);

// --------------------------------------------------------------------------------
// Counter Example Visualizer things
// --------------------------------------------------------------------------------

type cf_event;
type var_locglob;

const unique conditional_moment : cf_event;
const unique took_then_branch : cf_event;
const unique took_else_branch : cf_event;

const unique loop_register : cf_event;
const unique loop_entered : cf_event;
const unique loop_exited : cf_event;

const unique cev_local : var_locglob;
const unique cev_global : var_locglob;
const unique cev_parameter : var_locglob;
const unique cev_implicit : var_locglob;

function #cev_init(n:int) returns(bool);
function #cev_save_position(n:int) returns($token);
function #cev_var_intro(n:int, locorglob:var_locglob, name:$token, val:int, typ: $ctype) returns(bool);
function #cev_var_update(n:int, locorglob:var_locglob, name:$token, val:int) returns(bool);
function #cev_control_flow_event(n:int, tag : cf_event) returns(bool);
function #cev_function_call(n:int) returns(bool);

var $cev_pc : int;

procedure $cev_step(pos: $token);
  modifies $cev_pc;
  ensures #cev_save_position(old($cev_pc)) == pos;
  ensures $cev_pc == old($cev_pc) + 1;

procedure $cev_pre_loop(pos: $token) returns (oldPC: int);
  modifies $cev_pc;
  ensures #cev_control_flow_event(old($cev_pc), conditional_moment);
  ensures #cev_save_position(old($cev_pc)) == pos;
  ensures oldPC == old($cev_pc) && $cev_pc == old($cev_pc) + 1;

// That's all folks.
