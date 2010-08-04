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

type $state;

type $heap; // = [$ptr][$field][$ptr]int;
type $object; // = [$field][$ptr]int;
type $sub_object; // = [$ptr]int;

type $owner;
type $closed;
type $timestamps;
type $typed;

type $roots; // = [$ptr]$ptr;
type $embs; // = [$ptr]$ptr;

// A constant is generated for each pure function, used for ordering frame axioms.
type $pure_function;

// For labeled contracts.
type $label;

type $labelset; // TODO remove that

// ----------------------------------------------------------------------------
// built-in types
// ----------------------------------------------------------------------------

function $kind_of($ctype) : $kind;

// these are the only possible kinds
const unique $kind_composite : $kind;
const unique $kind_primitive : $kind;
const unique $kind_array : $kind;
const unique $kind_thread : $kind;

function $sizeof($ctype): int; // in bytes


// for types for which $in_range_t(...) is defined
function $as_in_range_t($ctype) : $ctype;

function {:inline true} $def_flat_type(t:$ctype, sz:int) : bool
  { $sizeof(t) == sz && $type_branch(t) == $ctype_flat }
function {:inline true} $def_primitive_type(t:$ctype, sz:int) : bool
  { $def_flat_type(t, sz) && $is_primitive(t) }
function {:inline true} $def_composite_type(t:$ctype, sz:int, claimable:bool, has_volatile_owns:bool) : bool
  { $def_flat_type(t, sz) && $is_composite(t) && $is_claimable(t) == claimable && $has_volatile_owns_set(t) == has_volatile_owns }
function {:inline true} $def_integer_type(t:$ctype, sz:int) : bool
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

// inverse functions here (unptr_to, map_domain, map_range) are for the prover
// so it knows that int*!=short*, i.e. * is injective

// pointers to types
function $ptr_to($ctype) : $ctype;
function $spec_ptr_to($ctype) returns ($ctype);
function $type_project_0($ctype) : $ctype;

const $arch_ptr_size : int; // arch-specific; to be defined by a compiler-generated axiom

axiom (forall #n:$ctype :: {$ptr_to(#n)} $type_project_0($ptr_to(#n)) == #n && $type_branch($ptr_to(#n)) == $ctype_ptr);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $type_project_0($spec_ptr_to(#n)) == #n && $type_branch($spec_ptr_to(#n)) == $ctype_spec_ptr);

axiom (forall #n:$ctype :: {$ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);

function $map_t($ctype, $ctype) : $ctype;
function $map_domain($ctype) : $ctype;
function $map_range($ctype) : $ctype;

axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $map_domain($map_t(#r,#d)) == #d && $map_range($map_t(#r,#d)) == #r && $type_branch($map_t(#r,#d)) == $ctype_map);

// Lack of {:inline true} makes it possible to trigger on $is_primitive(...)
// $is_primitive(t) should be only used when it is known that t is really
// primitive, i.e. not in a precondition or in a premise of an implication
// (unless guarded by a trigger). If it is unknown, use $is_primitive_ch(...).
// The same applies to $is_composite(...) and $is_arraytype(...).

function $is_primitive(t:$ctype) : bool
  { $kind_of(t) == $kind_primitive }

// TODO: look for occurences and see if the value can be determined at VC generetion time
function {:inline true} $is_primitive_ch(t:$ctype) : bool
  { $kind_of(t) == $kind_primitive }

function $is_composite(t:$ctype) : bool
  { $kind_of(t) == $kind_composite }

function {:inline true} $is_composite_ch(t:$ctype) : bool
  { $kind_of(t) == $kind_composite }

function $is_arraytype(t:$ctype) : bool
  { $kind_of(t) == $kind_array }

function {:inline true} $is_arraytype_ch(t:$ctype) : bool
  { $kind_of(t) == $kind_array }

function $is_threadtype(t:$ctype) : bool
  { $kind_of(t) == $kind_thread }

function {:inline true} $is_thread(p:$ptr) : bool
  { $is_threadtype($typ(p)) }

function {:inline true} $is_ptr_to_composite(p:$ptr) : bool
  { $kind_of($typ(p)) == $kind_composite }

function $is_math_type(t:$ctype) returns(bool);
axiom (forall t:$ctype :: {$is_math_type(t)} $is_math_type(t) ==> $is_primitive(t));

function $field_offset($field) : int;
function $field_parent_type($field) : $ctype;
function $is_ghost_field($field) : bool;

function $is_non_primitive(t:$ctype) : bool;
axiom (forall t:$ctype :: {$is_composite(t)} $is_composite(t) ==> $is_non_primitive(t));
axiom (forall t:$ctype :: {$is_arraytype(t)} $is_arraytype(t) ==> $is_non_primitive(t));
axiom (forall t:$ctype :: {$is_threadtype(t)} $is_threadtype(t) ==> $is_non_primitive(t));

function {:inline true} $is_non_primitive_ch(t:$ctype) : bool
  { $kind_of(t) != $kind_primitive }

function {:inline true} $is_non_primitive_ptr(p:$ptr) : bool
  { $is_non_primitive($typ(p)) }


axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $is_primitive($map_t(#r,#d)));
axiom (forall #n:$ctype :: {$ptr_to(#n)} $is_primitive($ptr_to(#n)));
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $is_primitive($spec_ptr_to(#n)));

axiom (forall #n:$ctype :: {$is_primitive(#n)} $is_primitive(#n) ==> !$is_claimable(#n));

const $me_ref : int;
function $me() : $ptr;
axiom $in_range_spec_ptr($me_ref) && $me_ref != 0;
axiom $me() == $ptr(^$#thread_id_t, $me_ref);

function {:inline true} $current_state(s:$state) : $state { s }

function $roots(s:$state) : $roots;
function $embs(s:$state) : $embs;
function $heap(s:$state) : $heap;

function $mem(s:$state, p:$ptr) : int;  // FIXME

function {:inline true} $root(s:$state, p:$ptr) : $ptr
  { $rd.$roots($roots(s), p) }

// TODO merge with $emb() ? 
function {:inline true} $remb(s:$state, p:$ptr) : $ptr
  { $rd.$embs($embs(s), p) }

function {:inline true} $rd1(s:$state, p:$ptr) : $object { $rd.$heap($heap(s), p) }
function {:inline true} $rd2(s:$state, p:$ptr, f:$field) : $sub_object { $rd.$object($rd1(s, p), f) }
function {:inline true} $rd3(s:$state, p:$ptr, f:$field, q:$ptr) : int { $rd.$sub_object($rd2(s, p, f), q) }
function {:inline true} $rd(s:$state, p:$ptr, f:$field) : int { $rd3(s, $root(s, p), f, p) }

function $relative_field_of(p:$ptr, q:$ptr) : $field;
function $container(p:$ptr, f:$field) : $ptr;

function $dot2($ptr,$field) : $ptr;

axiom (forall p, q: $ptr :: {$relative_field_of(p, q)}
  $dot2(p, $relative_field_of(p, q)) == q &&
  $container(q, $relative_field_of(p, q)) == p);

/* NMM
function {:inline true} $mem(s:$state, p:$ptr) : int
  { s[p] }
function {:inline true} $read_any(s:$state, p:$ptr) : int
  { s[p] }
function {:inline true} $mem_eq(s1:$state, s2:$state, p:$ptr) : bool
  { $mem(s1, p) == $mem(s2, p) }
*/

// ----------------------------------------------------------------------------
// typed pointers
// ----------------------------------------------------------------------------

function $typ($ptr): $ctype;
function $ref($ptr): int;
function $ptr($ctype,int): $ptr;
axiom (forall #t:$ctype, #b:int :: $typ($ptr(#t,#b))==#t);
axiom (forall #t:$ctype, #b:int :: $ref($ptr(#t,#b))==#b);
//axiom (forall #p:$ptr :: {$typ(#p)} {$ref(#p)} $ptr($typ(#p), $ref(#p)) == #p);

// Use for computing references for ghost fields, instead of saying p+$field_offset(f) we use
// $ghost_ref(p,f). This has an added bonus that the embedding and path can be the same reagrdless
// of the current $meta
function $ghost_ref($ptr, $field) : int;

function $ghost_emb($ptr) : $ptr;
function $ghost_path($ptr) : $field;

axiom (forall p:$ptr, f:$field, t:$ctype :: {$ptr(t, $ghost_ref(p, f))}
  $ghost_emb($ptr(t, $ghost_ref(p, f))) == p && 
  $ghost_path($ptr(t, $ghost_ref(p, f))) == f );

axiom (forall p:$ptr, f:$field :: {$ghost_ref(p, f)}
  $ghost_ref(p,f) > $arch_spec_ptr_start);

function $physical_ref(p:$ptr, f:$field) : int;
// Make the physical fields, when we do not generate offset axioms, behave like ghost fields
//  (this is unsound, but not so much).
//axiom (forall p:$ptr, f:$field :: {$physical_ref(p, f)}
//  $ghost_emb($physical_ref(p, f)) == p && $ghost_path($physical_ref(p, f)) == f );

function $array_path(basefield:$field, off:int) : $field;

function $is_base_field($field) : bool;

function $array_path_1($field) : $field;
function $array_path_2($field) : int;
axiom (forall fld:$field, off:int :: {$array_path(fld, off)}
  !$is_base_field($array_path(fld, off)) &&
  $array_path_1($array_path(fld, off)) == fld &&
  $array_path_2($array_path(fld, off)) == off);

const $null:$ptr;
axiom $null == $ptr(^^void, 0);

//typed pointer test
function $is(p:$ptr, t:$ctype) : bool
  { $typ(p)==t }
// create $ptr(...) function symbol, so later we have something to trigger on
axiom (forall #p:$ptr, #t:$ctype :: {$is(#p, #t)} $is(#p, #t) ==> #p == $ptr(#t, $ref(#p)));

function {:inline true} $ptr_cast(#p:$ptr,#t:$ctype) : $ptr
  { $ptr(#t, $ref(#p)) }

/* NMM
function {:inline true} $read_ptr(S:$state, p:$ptr, t:$ctype) : $ptr
  { $ptr(t, $mem(S, p)) }
*/

//dot and (its "inverse") emb/path access
function $dot($ptr,$field) returns ($ptr);
function {:inline false} $emb(S:$state,p:$ptr) returns ($ptr)
  { $rd.$embs($embs(S), p) }
function {:inline true} $path(S:$state,p:$ptr) : $field
  { $relative_field_of($emb(S, p), p) }

/* NMM
axiom (forall p:$ptr, S:$state ::
  {$dot(p, $f_typed), S[p]}
  {$dot(p, $f_typed), S[$dot(p, $f_is_volatile)]}
  $instantiate_int(S[$dot(p, $f_typed)]));
*/

function $is_sequential_field($field) : bool;

function {:inline true} $def_phys_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool, off:int) : bool
  { $is_base_field(f) && $field_parent_type(f) == partp &&
    (!isvolatile ==> $is_sequential_field(f)) &&
    $field_offset(f) == off &&
    true
    //(forall p:$ptr :: {$dot(p, f)} $is(p, partp) ==> $dot(p, f) == $ptr(tp, $ref(p) + off)) &&
    /*
    (forall S:$state, p:$ptr :: 
      {$rds(S, $dot(p, f), $f_typed)}
	  {$emb(S, $dot(p, f))}
    $typed2(S, p, partp) ==>
      $typed(S, $dot(p, f)) &&
      $emb(S, $dot(p, f)) == p &&
      // $path(S, $dot(p, f)) == f &&
      $is_volatile(S, $dot(p, f)) == isvolatile )
      */
  }

function {:inline true} $def_ghost_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool) : bool
  { $is_base_field(f) && $field_parent_type(f) == partp && $is_ghost_field(f) &&
    (!isvolatile ==> $is_sequential_field(f)) &&
    true
    //(forall p:$ptr :: {$dot(p, f)} $is(p, partp) ==> $dot(p, f) == $ptr(tp, $ghost_ref(p, f))) &&
    /*
    (forall S:$state, p:$ptr :: 
      {$rds(S, $dot(p, f), $f_typed)}
	  {$emb(S, $dot(p, f))}
    $typed2(S, p, partp) ==>
      $typed(S, $dot(p, f)) &&
      $emb(S, $dot(p, f)) == p &&
      // $path(S, $dot(p, f)) == f &&
      $is_volatile(S, $dot(p, f)) == isvolatile )
      */
  }

function {:inline true} $def_common_field(f:$field, tp:$ctype) : bool
  { $is_base_field(f) && $is_ghost_field(f) &&
    true
    //(forall p:$ptr :: {$dot(p, f)} $dot(p, f) == $ptr(tp, $ghost_ref(p, f)))
    /*&&
    (forall p:$ptr, S:$state :: {$rd(S, p, f)}
      if $is_primitive_ch($typ(p)) then
        $emb(S, $dot(p, f)) == $emb(S, p)
      else
        $emb(S, $dot(p, f)) == p)*/
  }

function {:inline true} $def_writes(S:$state, time:int, ptrs:$ptrset) : bool
  {
    (forall p:$ptr :: {$rd.$typed($f_typed(S), $root(S, p), p)}
      $set_in(p, ptrs) ==> $in_writes_at(time, p) && $thread_owned_or_even_mutable(S, p))
/*
    (forall p:$ptr :: {:vcc3 "L1"} {S[p]}
      $ghost_path(p) == $f_typed ==>
        $set_in($ghost_emb(p), ptrs) ==> $in_writes_at(time, $ghost_emb(p)) && $thread_owned_or_even_mutable(S, $ghost_emb(p)))
        */
  }

function $f_typed($state) : $typed;
function $f_timestamp($state) : $timestamps;
function $f_owner($state) : $owner;
function $f_closed($state) : $closed;

const unique $f_owns : $field;
const unique $f_ref_cnt : $field;

axiom $def_common_field($f_owns, ^$#ptrset);
axiom $def_common_field($f_ref_cnt, ^^mathint);

// it has no further embedding (not embedded inside another struct)
function {:inline true} $is_object_root(S:$state, p:$ptr) : bool
  { $emb(S, p) == $ptr(^^root_emb, $ref(p)) }

/*
axiom (forall S:$state, p:$ptr ::
  {:vcc3 "todo"}
  {$is_volatile(S, p)} $good_state(S) && $is_volatile(S, p) ==> $is_primitive_ch($typ(p)));
*/

// just tmp
function {:inline true} $is_malloc_root(S:$state, p:$ptr) : bool
  { $is_object_root(S, p) }


function $current_timestamp(S:$state) : int;
/*
function foobar($state,$ptr) : bool;
axiom (forall S:$state, p:$ptr ::
  {:vcc3 "todo"}
  {$timestamp(S, p)}
  foobar(S,p)
//  $timestamp(S, p) <= $current_timestamp(S) ||
//  !$typed(S, p)
  );
*/

function {:inline true} $is_fresh(M1:$state, M2:$state, p:$ptr) : bool
  { $current_timestamp(M1) < $timestamp(M2, p) && $timestamp(M2, p) <= $current_timestamp(M2) }

function $in_writes_at(time:int, p:$ptr) : bool;

function {:inline true} $writable(S:$state, begin_time:int, p:$ptr) : bool
  { $is_non_primitive_ch($typ(p)) &&
    ($mutable(S, p) && ($timestamp(S, p) >= begin_time || $in_writes_at(begin_time, p))) }

function {:inline true} $writable_prim(S:$state, begin_time:int, p:$ptr, f:$field) : bool
  { $writable(S, begin_time, p) }

function {:inline true} $top_writable(S:$state, begin_time:int, p:$ptr) : bool
  { $is_non_primitive_ch($typ(p)) &&
    ($owner(S, p) == $me() && ($timestamp(S, p) >= begin_time || $in_writes_at(begin_time, p))) }

function {:inline true} $in(p:$ptr, s:$ptrset) : bool
  { s[p] } 

function {:inline true} $modifies(S0:$state, S1:$state, W:$ptrset) : bool
  { (forall p:$ptr :: {$root(S1, p)} $owner(S0, p) == $me() ==> $root(S0, p) == $root(S1, p) || $in($root(S0, p), W)) &&
    (forall p:$ptr :: {$remb(S1,p)} $owner(S0, $remb(S0, p)) == $me() ==> $remb(S0, p) == $remb(S1, p) || $in($root(S0, $remb(S0, p)), W)) &&
    (forall p:$ptr :: {$rd1(S1, p)} $owner(S0, p) == $me() ==> $rd1(S0, p) == $rd1(S1, p) || $in(p, W)) &&
    $timestamp_post(S0, S1) }

function {:inline true} $preserves_thread_local(S0:$state, S1:$state) : bool
  { (forall p:$ptr :: {$thread_local(S1, p)}
       $thread_local(S0, p) ==> $thread_local(S1, p)) }

/* NMM
function {:inline true} $writes_nothing(S0:$state, S1:$state) : bool
  { (forall p:$ptr :: {S1[p]} S0[p] == S1[p] || !$thread_local(S0, p)) &&
    $preserves_thread_local(S0, S1) &&
    $timestamp_post(S0, S1) }
*/

function {:inline true} $writes_nothing(S0:$state, S1:$state) : bool
  { $modifies(S0, S1, $set_empty()) &&
    $preserves_thread_local(S0, S1) }


// ----------------------------------------------------------------------------
// state
// ----------------------------------------------------------------------------
var $s: $state;

function $good_state($state) : bool;
function $invok_state($state) : bool;

function $has_volatile_owns_set(t:$ctype) : bool;
function $is_claimable($ctype) : bool;

function {:inline false} $owner(S:$state, p:$ptr) : $ptr
  { $rd.$owner($f_owner(S), $root(S, p), p) }
function {:inline false} $closed(S:$state, p:$ptr) : bool
  { $rd.$closed($f_closed(S), $root(S, p), p) }
function {:inline false} $timestamp(S:$state, p:$ptr) : int
  { $rd.$timestamps($f_timestamp(S), $root(S, p), p) }
function {:inline false} $typed(S:$state, p:$ptr) : bool
  { $rd.$typed($f_typed(S), $root(S, p), p) }
function {:inline false} $ref_cnt(S:$state, p:$ptr) : int
  { $rd(S, p, $f_ref_cnt) }

function $position_marker() : bool
  { true }

function $owns(S:$state, p:$ptr) : $ptrset
  { $int_to_ptrset($rd(S, p, $f_owns)) }

function {:inline true} $wrapped(S:$state, #p:$ptr, #t:$ctype) : bool
  { $closed(S, #p) && $owner(S, #p) == $me() && $typed2(S, #p, #t) && $is_non_primitive_ch(#t) }

function {:inline true} $irrelevant(S:$state, p:$ptr) : bool
  { $owner(S, p) != $me() || ($is_primitive_ch($typ(p)) && $closed(S, p)) }

function {:inline true} $mutable(S:$state, p:$ptr) : bool
  { $typed(S, p) && 
    ( // $typed(S,p)==>
      if $is_primitive_ch($typ(p)) then 
        $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p)) 
      else
        $owner(S, p) == $me() && !$closed(S, p))
  }

function {:inline true} $object_of(S:$state, p:$ptr) : $ptr
  { if $is_primitive_ch($typ(p)) then $emb(S, p)
    else p }

function {:inline true} $thread_owned(S:$state, p:$ptr) : bool
  { $typed(S, p) && $owner(S, p) == $me() }

function {:inline true} $thread_owned_or_even_mutable(S:$state, p:$ptr) : bool
  { $typed(S, p) &&
    if $is_primitive_ch($typ(p)) then
      $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p))
    else
      $owner(S, p) == $me()
  }

function $in_range_phys_ptr(#r:int) : bool
  { $in_range(0, #r, $arch_spec_ptr_start) }
function $in_range_spec_ptr(#r:int) : bool
  { 0 == #r || #r > $arch_spec_ptr_start }
const $arch_spec_ptr_start : int; // arch-specific; to be defined by a compiler-generated axiom

function {:inline true} $is_ghost_ptr(p:$ptr) returns(bool)
  { $in_range_spec_ptr($ref(p)) }

function {:inline true} $typed2(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is(#p, #t) && $typed(S, #p) }

/*
axiom (forall S:$state, #r:int, #t:$ctype ::
  {:vcc3 "todo"}
  {$typed(S, $ptr(#t, #r))}
  $typed(S, $ptr(#t, #r)) && $in_range_phys_ptr(#r) ==> $in_range_phys_ptr(#r + $sizeof(#t) - 1));
*/

function {:inline true} $typed2_phys(S:$state, #p:$ptr, #t:$ctype) returns (bool)
  { $typed2(S, #p, #t) && ($typed2(S, #p, #t) ==> $in_range_phys_ptr($ref(#p))) }

function {:inline true} $typed2_spec(S:$state, #p:$ptr, #t:$ctype) returns (bool)
  { $typed2(S, #p, #t) && ($typed2(S, #p, #t) ==> $in_range_spec_ptr($ref(#p))) }

function {:inline true} $ptr_eq(p1:$ptr,p2:$ptr) : bool
  { $ref(p1) == $ref(p2) }

function {:inline true} $ptr_neq(p1:$ptr,p2:$ptr) : bool
  { $ref(p1) != $ref(p2) }

function {:inline true} $is_primitive_field_of(S:$state, #f:$ptr, #o:$ptr) : bool
  { $is_primitive_ch($typ(#f)) && $emb(S, #f) == #o }


// ----------------------------------------------------------------------------
// thread locality
// ----------------------------------------------------------------------------

// for model reading
function $is_domain_root(S:$state, p:$ptr) : bool
  { true }

function $in_wrapped_domain(S:$state, p:$ptr) : bool;
/*
  { (exists q:$ptr :: {$set_in2(p, $ver_domain($read_version(S, q)))} 
            $set_in(p, $ver_domain($read_version(S, q)))
         && $wrapped(S, q, $typ(q)) 
         && $is_domain_root(S, q)
         ) }
*/

function {:inline true} $thread_local_np(S:$state, p:$ptr) : bool
  { !$is_primitive_ch($typ(p))
  && $owner(S, $root(S, p)) == $me()
//     ($wrapped(S, $root(S, p), $typ($root(S, p))) && $set_in(p, $domain(S, $root(S, p)))))
  }

// required for reading
function $thread_local(S:$state, p:$ptr) : bool
  { $typed(S, p) &&
    if $is_primitive_ch($typ(p)) then
      /*(!$is_volatile(S, p) || !$closed(S, $emb(S, p))) &&*/ $thread_local_np(S, $emb(S, p))
    else
      $thread_local_np(S, p) }

function {:inline true} $thread_local2(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is(#p, #t) && $thread_local(S, #p) }
 

// ----------------------------------------------------------------------------
// Boogie/Z3 hacks
// ----------------------------------------------------------------------------

// Used as a trigger when we don't want the quantifier to be instantiated at all
//   (for example we just assert it or have it as a precondition)
// It could be any symbol that is not used anywhere else.
function $dont_instantiate($ptr) : bool;
function $dont_instantiate_int(int) : bool;
function $dont_instantiate_state($state) : bool;

// Voodoo, don't read it.
function $instantiate_int(int) : bool;
function $instantiate_bool(bool) : bool;
function $instantiate_ptr($ptr) : bool;
function $instantiate_ptrset($ptrset) : bool;
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


function $function_entry($state) : bool;

function $full_stop($state) : bool;
function {:inline true} $full_stop_ext(t:$token, S:$state) : bool
  { $good_state_ext(t, S) && $full_stop(S) }

function $file_name_is(id:int, tok:$token) : bool;

axiom (forall S:$state :: {$function_entry(S)}
  $function_entry(S) ==> $full_stop(S) && $current_timestamp(S) >= 0);

axiom (forall S:$state :: {$full_stop(S)}
  $full_stop(S) ==> $good_state(S) && $invok_state(S));

axiom (forall S:$state :: {$invok_state(S)}
  $invok_state(S) ==> $good_state(S));

// ----------------------------------------------------------------------------
// System invariants
// ----------------------------------------------------------------------------

function {:inline true} $closed_is_transitive(S:$state) returns (bool)
  { 
    (forall p:$ptr,q:$ptr ::
      {$set_in_pos(p, $owns(S, q))}
      $good_state(S) &&
      $set_in(p, $owns(S, q)) && $closed(S, q) ==> $closed(S, p)) // && $ref(p) != 0)
  } 

axiom (forall S:$state, p:$ptr, q:$ptr :: {$set_in_pos(p, $owns(S, q)), $is_non_primitive($typ(p))}
  $good_state(S) &&
  $closed(S, q) && $is_non_primitive($typ(p)) ==>
      ($set_in(p, $owns(S, q)) <==> $owner(S, p) == q));

axiom (forall S:$state, #r:int, #t:$ctype :: {$owns(S, $ptr(#t, #r)), $is_arraytype(#t)}
  $good_state(S) ==>
    $is_arraytype(#t) ==> $owns(S, $ptr(#t, #r)) == $set_empty());

axiom (forall S:$state, #p:$ptr, #t:$ctype :: {$inv(S, #p, #t)}
  $invok_state(S) && $closed(S, #p) ==> $inv(S, #p, #t));

axiom (forall S:$state :: {$good_state(S)}
  $good_state(S) ==> $closed_is_transitive(S));

axiom (forall S:$state :: {$good_state(S)}
  $good_state(S) ==>
    (forall p:$ptr :: {$typed(S,p)} /*{$rds(S, p, $f_closed)}*/ $closed(S, p) ==> $typed(S, p)) &&
    $closed_is_transitive(S)
    );
        
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------

function $call_transition(S1:$state, S2:$state) : bool;

// Assumed after each meta/state update, means that the meta corresponds to the state
// and where in the source did the update happen.
function $good_state_ext(id:$token, S:$state) : bool;
axiom (forall id:$token, S:$state :: {$good_state_ext(id, S)}
  $good_state_ext(id, S) ==> $good_state(S));

// ----------------------------------------------------------------------------
// model-viewer support
// ----------------------------------------------------------------------------

function $local_value_is(S1:$state, pos:$token, local_name:$token, val:int, t:$ctype) : bool;
function $local_value_is_ptr(S1:$state, pos:$token, local_name:$token, val:$ptr, t:$ctype) : bool;
function $read_ptr_m(S:$state, p:$ptr, t:$ctype) : $ptr;

function $type_code_is(x:int, tp:$ctype) : bool;
// idx==0 - return type
function $function_arg_type(fnname:$pure_function, idx:int, tp:$ctype) : bool;


// ----------------------------------------------------------------------------
// Procedures
// ----------------------------------------------------------------------------

function $update(h:$heap, r:$ptr, f:$field, p:$ptr, v:int) : $heap
  { $wr.$heap( h, r, $wr.$object( $rd.$heap(h, r), f, $wr.$sub_object($rd.$object($rd.$heap(h, r), f), p, v) ) ) }  
//  { h[ r := h[r][ f := h[r][f][ p := v ] ] ] }

/*
axiom 
   (forall h:$heap, r:$ptr, f:$field, p:$ptr, v:int :: {$update(h,r,f,p,v)}
     (forall r2:$ptr, f2:$field, p2:$ptr :: {$update(h,r,f,p,v)[r2][f2][p2]}
        if r == r2 && f == f2 && p == p2 then
          $update(h,r,f,p,v)[r2][f2][p2] == v
        else
          h[r2][f2][p2] == $update(h,r,f,p,v)[r2][f2][p2]));
*/

function $specials_eq(S0:$state, S:$state) : bool
  { 
    $f_typed(S0) == $f_typed(S) &&
    $f_timestamp(S0) == $f_timestamp(S) &&
    $f_closed(S0) == $f_closed(S) &&
    $f_owner(S0) == $f_owner(S) &&
    $embs(S0) == $embs(S) &&
    $roots(S0) == $roots(S)
  }

procedure $write_int(p:$ptr, f:$field, v:int);
  modifies $s;
  ensures $specials_eq(old($s), $s);
  ensures $heap($s) == $update($heap(old($s)), $root(old($s), p), f, p, v);
  ensures $timestamp_post_strict(old($s), $s);

function {:inline true} $timestamp_is_now(S:$state, p:$ptr) : bool
  { $timestamp(S, p) == $current_timestamp(S) }

function {:inline true} $now_writable(S:$state, p:$ptr) : bool
  { $timestamp_is_now(S, p) && $mutable(S, p) }

function {:inline true} $timestamp_post(M1:$state, M2:$state) : bool
  { $current_timestamp(M1) <= $current_timestamp(M2) &&
    /*(forall p:$ptr :: 
      {:vcc3 "todo"} {:weight 0}
      {$timestamp(M2, p)}
      $timestamp(M1, p) <= $timestamp(M2, p)) &&*/
    $call_transition(M1, M2)
  }

function {:inline true} $timestamp_post_strict(M1:$state, M2:$state) : bool
  { $current_timestamp(M1) < $current_timestamp(M2)
    /*&&
    (forall p:$ptr ::
      {:vcc3 "todo"} {:weight 0}
      {$timestamp(M2, p)} 
      $timestamp(M1, p) <= $timestamp(M2, p))*/ &&
    $call_transition(M1, M2)
  }

procedure $set_owns(p:$ptr, owns:$ptrset);
  // writes p
  modifies $s;
  // TOKEN: the owner is non-primitive
  requires $is_composite_ch($typ(p));
  // TOKEN: the owner is mutable
  requires $mutable($s, p);
  ensures $heap($s) == $update($heap(old($s)), $root(old($s), p), $f_owns, p, $ptrset_to_int(owns));
  ensures $specials_eq(old($s), $s);
  ensures $timestamp_post_strict(old($s), $s);

// ----------------------------------------------------------------------------
// Allocation
// ----------------------------------------------------------------------------

procedure $spec_alloc(t:$ctype) returns(r:$ptr);
  modifies $s;
  ensures $typed2($s, r, t);
  ensures $mutable($s, r);

  ensures (forall p:$ptr :: // TODO add trigger
    $set_in(p, $span($s, r)) ==> $timestamp_is_now($s, p));

  ensures $writes_nothing(old($s), $s);

  ensures !$typed(old($s), r);
  ensures $is_malloc_root($s, r);
  ensures $is_object_root($s, r);
  ensures $first_option_typed($s, r);
  ensures $in_range_spec_ptr($ref(r));

// ----------------------------------------------------------------------------
// Wrap/unwrap
// ----------------------------------------------------------------------------

// These are only for triggering and consistency checking; they have no definitions
function $pre_wrap($state) : bool;
function $pre_unwrap($state) : bool;
function $pre_static_wrap($state) : bool;
function $pre_static_unwrap($state) : bool;
function $post_unwrap(S1:$state, S2:$state) : bool;

function $wrap_writes(S0:$state, S:$state, o:$ptr) : bool
{
  // TODO both of these takes the entire current heap, maybe restrict it to the owns set?
  (forall q:$ptr, f:$field :: {$rd3(S, o, f, q)}
     $rd3(S, o, f, q) == $rd3(S0, $root(S0, q), f, q))
  && (forall p:$ptr :: {$rd1(S, p)} $rd1(S, p) == $rd1(S0, p) || p == o)
  && $f_typed(S) == $f_typed(S0)
  && $f_closed(S) == $wr.$closed($f_closed(S0), o, o, true)
  && $f_timestamp(S) == $wr.$timestamps($f_timestamp(S0), o, o, $current_timestamp(S))
  && (forall r,p:$ptr :: {$rd.$owner($f_owner(S), r, p)}
        if $set_in(r, $owns(S0, o)) then 
          $rd.$owner($f_owner(S), r, p) == if p == r then o else $owner(S0, p)
        else 
          $rd.$owner($f_owner(S), r, p) == $rd.$owner($f_owner(S0), r, p))
  && $embs(S0) == $embs(S)
  &&
  (forall p:$ptr :: {$root(S, p)}
     $root(S, p) == $root(S0, p) ||
     ($root(S, p) == o && (p == o || $set_in($root(S0, p), $owns(S0, o)))))
  && $timestamp_post_strict(S0, S)
}

function $is_unwrapped(S0:$state, S:$state, o:$ptr) : bool
{
  $mutable(S, o) && 

  (forall p:$ptr :: {$rd1(S, p)} $rd1(S, p) == $rd1(S0, $root(S0, p)))
  && $f_typed(S) == $f_typed(S0)
  && $f_closed(S) == $wr.$closed($f_closed(S0), o, o, false)
  && $f_timestamp(S) == $wr.$timestamps($f_timestamp(S0), o, o, $current_timestamp(S))
  && (forall r,p:$ptr :: {$rd.$owner($f_owner(S), r, p)}
        if $root(S0, r) == o then
          $rd.$owner($f_owner(S), r, p) == if p == r then $me() else $owner(S0, p)
        else 
          $rd.$owner($f_owner(S), r, p) == $rd.$owner($f_owner(S0), r, p))
  && $embs(S0) == $embs(S)
  &&
  (forall p:$ptr :: {$root(S, p)}
     ($root(S0, p) != o && $root(S0, p) == $root(S, p)) ||
     ($root(S0, p) == o && ($root(S, p) == p || $owner(S0, p) != o))
     )
  &&
  (forall p:$ptr :: {$set_in_pos(p, $owns(S0, o))}
    $set_in(p, $owns(S0, o)) ==>
      $wrapped(S, p, $typ(p)) && $timestamp_is_now(S, p)) &&

  // $preserves_thread_local(S0, S)

  $timestamp_post_strict(S0, S) &&
  $post_unwrap(S0, S)
}

axiom(forall s: $state, p: $ptr :: {$owner(s, p)}
  $owner(s, p) == $me() ==> $root(s, p) == p);

axiom (forall S:$state, r:$ptr :: {$owner(S, r)}
  $is_composite_ch($typ($owner(S, r))) ==>
    $root(S, r) == $root(S, $owner(S, r)));

procedure $unwrap(o:$ptr, T:$ctype);
  modifies $s;
  // TOKEN: the object has no outstanding claims
  requires ! $is_claimable(T) || $ref_cnt($s, o) == 0;
  // TOKEN: OOPS: pre_unwrap holds
  requires $pre_unwrap($s);

  ensures $is_unwrapped(old($s), $s, o);

function $is_wrapped(S0:$state, S:$state, o:$ptr) : bool
{
  $wrap_writes(S0, S, o) &&
  $wrapped(S, o, $typ(o)) &&
  $timestamp_is_now(S, o) &&

  (forall p:$ptr :: {$set_in_pos(p, $owns(S0, o))}
    $set_in(p, $owns(S0, o)) ==> $owner(S, p) == o) &&

  ($is_claimable($typ(o)) ==> $ref_cnt(S0, o) == 0 && $ref_cnt(S, o) == 0)

  // $set_in(o, $domain(S, o))
  // $preserves_thread_local(S0, S)
}

procedure $wrap(o:$ptr, T:$ctype);
  // writes o, $owns($s, o)
  modifies $s;
  // TOKEN: OOPS: pre_wrap holds
  requires $pre_wrap($s);
  // TOKEN: the wrapped type is non primitive
  requires $is_non_primitive_ch($typ(o));
  // TOKEN: the object being wrapped is mutable
  requires $mutable($s, o);
  // TOKEN: everything in the owns set is wrapped
  requires (forall p:$ptr :: {$dont_instantiate(p)} $set_in0(p, $owns($s, o)) ==> $wrapped($s, p, $typ(p)));

  ensures $is_wrapped(old($s), $s, o);


// ----------------------------------------------------------------------------
// Admissibility
// ----------------------------------------------------------------------------

function $spans_the_same(S1:$state, S2:$state, p:$ptr, t:$ctype) : bool
  { (forall f:$field :: {$dot(p, f)}
      ($is_ghost_field(f) || $field_parent_type(f) == t) ==>
        $rd(S1, p, f) == $rd(S2, p, f) || f == $f_ref_cnt) }

function $nonvolatile_spans_the_same(S1:$state, S2:$state, p:$ptr, t:$ctype) : bool
  { (forall f:$field :: {$dot(p, f)}
      ($is_ghost_field(f) || $field_parent_type(f) == t) ==>
        $rd(S1, p, f) == $rd(S2, p, f) ||
        /*$is_volatile(S1, $dot(p, f)) ||*/ f == $f_ref_cnt) }

function $good_for_admissibility(S:$state) : bool;
function $good_for_post_admissibility(S:$state) : bool;

function {:inline true} $stuttering_pre(S:$state, p:$ptr) : bool
  { (forall q: $ptr :: {$closed(S, q)} $closed(S, q) ==> $inv(S, q, $typ(q))) &&
    $good_for_admissibility(S)
  }

function {:inline true} $admissibility_pre(S:$state, p:$ptr) : bool
  { $closed(S, p) && $inv(S, p, $typ(p)) && $stuttering_pre(S, p) }

procedure $havoc_others(p:$ptr, t:$ctype);
  modifies $s;
  // TOKEN: the state was not modified
  requires $good_for_admissibility($s);
  ensures $is_stuttering_check() || $spans_the_same(old($s), $s, p, t);
  ensures $nonvolatile_spans_the_same(old($s), $s, p, t);
  ensures $closed($s, p);
  ensures $typed($s, p);
  ensures $closed_is_transitive($s);
  ensures $good_state($s);
  ensures $good_for_post_admissibility($s);
  ensures (forall q: $ptr :: {$closed($s, q)}
    $closed(old($s), q) || $closed($s, q) ==>
      ($spans_the_same(old($s), $s, q, $typ(q)) && $closed(old($s), q) == $closed($s, q)) || 
      ($inv2(old($s), $s, q, $typ(q)) && $nonvolatile_spans_the_same(old($s), $s, q, $typ(q))));
  ensures (forall q:$ptr ::  {$set_in_pos(q, $owns(old($s), p))}
            $set_in(q, $owns(old($s), p)) ==>
              $ref_cnt(old($s), q) == $ref_cnt($s, q));
  ensures $timestamp_post(old($s), $s);

// ----------------------------------------------------------------------------
// Can-unwrap checks
// ----------------------------------------------------------------------------

function $is_stuttering_check() : bool;
function $is_unwrap_check() : bool;
function {:inline true} $is_admissibility_check() : bool
  { !$is_stuttering_check() && !$is_unwrap_check() }

function $good_for_pre_can_unwrap(S:$state) : bool;
function $good_for_post_can_unwrap(S:$state) : bool;

function {:inline true} $unwrap_check_pre(S:$state, p:$ptr) : bool
  { $wrapped(S, p, $typ(p)) && 
    (! $is_claimable($typ(p)) || $ref_cnt(S, p) == 0) &&
    $inv(S, p, $typ(p)) &&
    (forall q: $ptr :: {$closed(S, q)} $closed(S, q) ==> $inv(S, q, $typ(q))) &&
    $good_for_pre_can_unwrap(S)
  }

procedure $unwrap_check(o:$ptr);
  modifies $s;
  // TOKEN: the state was not modified
  requires $good_for_pre_can_unwrap($s);
  ensures $good_state($s);
  ensures $good_for_post_can_unwrap($s);

  ensures $spans_the_same(old($s), $s, o, $typ(o));

  ensures $is_unwrapped(old($s), $s, o);


// -----------------------------------------------------------------------
// Laballed invariants
// -----------------------------------------------------------------------

function $in_domain_lab(S:$state, p:$ptr, q:$ptr, l:$label) : bool
  { $in_domain(S, p, q) }
function $in_vdomain_lab(S:$state, p:$ptr, q:$ptr, l:$label) : bool
  { $in_vdomain(S, p, q) }
function $inv_lab(S:$state, p:$ptr, l:$label) : bool;

axiom (forall S:$state, p:$ptr, q:$ptr, l:$label :: {$in_domain_lab(S, p, q, l)}
  $in_domain_lab(S, p, q, l) ==> $inv_lab(S, p, l));

axiom (forall S:$state, p:$ptr, q:$ptr, l:$label :: {$in_vdomain_lab(S, p, q, l)}
  $in_vdomain_lab(S, p, q, l) ==> $inv_lab(S, p, l));

// -----------------------------------------------------------------------
// Domains
// -----------------------------------------------------------------------

function $in_domain(S:$state, p:$ptr, q:$ptr) : bool;
function $in_vdomain(S:$state, p:$ptr, q:$ptr) : bool;

axiom (forall S:$state, p:$ptr :: {$in_domain(S, p, $root(S, p))}
  $full_stop(S) && $wrapped(S, $root(S, p), $typ($root(S, p))) ==> $in_domain(S, p, $root(S, p)));

// -----------------------------------------------------------------------
// Span & extent
// -----------------------------------------------------------------------

function $full_extent(#p:$ptr) : $ptrset;
function $extent(S:$state, #p:$ptr) : $ptrset;

function $span(S:$state, o:$ptr) : $ptrset
  { (lambda p:$ptr :: o == p || $emb(S, p) == o) }
function $first_option_typed(S:$state, #p:$ptr) : bool;

function {:inline true} $struct_extent(#p:$ptr) : $ptrset
  { $full_extent(#p) }

/*
function $volatile_span(S:$state, #p:$ptr) : $ptrset;
axiom (forall S:$state, p:$ptr, q:$ptr ::
  {:vcc3 "todo"}
  {$set_in(p, $volatile_span(S, q))}
  $set_in(p, $volatile_span(S, q)) <==> p == q || ($is_volatile(S, p) && $set_in(p, $span(S, q))));
*/

// ----------------------------------------------------------------------------
// Records
// ----------------------------------------------------------------------------

type $record;
const $rec_zero : $record;
function $rec_update(r:$record, f:$field, v:int) : $record;
function $rec_fetch(r:$record, f:$field) : int;

function {:inline true} $rec_update_bv(r:$record, f:$field, val_bitsize:int, from:int, to:int, repl:int) : $record
  { $rec_update(r, f, $bv_update($rec_fetch(r, f), val_bitsize, from, to, repl)) }

axiom (forall f:$field :: {$rec_fetch($rec_zero, f)} $rec_fetch($rec_zero, f) == 0);

axiom (forall r:$record, f:$field, v:int :: {$rec_fetch($rec_update(r, f, v), f)}
  $rec_fetch($rec_update(r, f, v), f) == $unchecked($record_field_int_kind(f), v));

axiom (forall r:$record, f:$field :: {$rec_fetch(r, f)}
  $in_range_t($record_field_int_kind(f), $rec_fetch(r, f)));

axiom (forall r:$record, f1:$field, f2:$field, v:int :: {$rec_fetch($rec_update(r, f1, v), f2)}
  $rec_fetch($rec_update(r, f1, v), f2) == $rec_fetch(r, f2) || f1 == f2);

function $is_record_type(t:$ctype) : bool;
function $is_record_field(parent:$ctype, field:$field, field_type:$ctype) : bool;

axiom (forall t:$ctype :: {$is_record_type(t)} $is_record_type(t) ==> $is_primitive(t));

function $as_record_record_field($field) : $field;
axiom (forall p:$ctype, f:$field, ft:$ctype :: {$is_record_field(p, f, ft), $is_record_type(ft)}
  $is_record_field(p, f, ft) && $is_record_type(ft) ==> $as_record_record_field(f) == f);

function $record_field_int_kind(f:$field) : $ctype;

function $rec_eq(r1:$record, r2:$record) : bool
  { r1 == r2 }
function $rec_base_eq(x:int, y:int) : bool
  { x == y }

function $int_to_record(x:int) : $record;
function $record_to_int(r:$record) : int;

axiom (forall r:$record :: {$record_to_int(r)} $int_to_record($record_to_int(r)) == r);

axiom (forall r1:$record, r2:$record :: {$rec_eq(r1, r2)}
  $rec_eq(r1, r2) <==
  (forall f:$field :: $rec_base_eq($rec_fetch(r1, f), $rec_fetch(r2, f))));

axiom (forall r1:$record, r2:$record, f:$field ::
 {$rec_base_eq($rec_fetch(r1, f), $rec_fetch(r2, $as_record_record_field(f)))}
 $rec_base_eq($rec_fetch(r1, f), $rec_fetch(r2, f)) <==
   $rec_eq($int_to_record($rec_fetch(r1, f)), $int_to_record($rec_fetch(r2, f))));


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

function {:inline true} $set_add_element(S:$ptrset, e:$ptr) : $ptrset
  { $set_union(S, $set_singleton(e)) }

function {:inline true} $set_remove_element(S:$ptrset, e:$ptr) : $ptrset
  { $set_difference(S, $set_singleton(e)) }

// to be used only positively
function $set_eq($ptrset, $ptrset) returns (bool);
axiom (forall #a: $ptrset, #b: $ptrset :: {$set_eq(#a,#b)}
  (forall #o: $ptr :: {$dont_instantiate(#o)} $set_in(#o, #a) <==> $set_in(#o, #b)) ==> $set_eq(#a, #b));
axiom (forall #a: $ptrset, #b: $ptrset :: {$set_eq(#a,#b)}
  $set_eq(#a, #b) ==> #a == #b);

function $set_cardinality($ptrset) : int;

axiom ($set_cardinality($set_empty()) == 0);
axiom (forall p:$ptr :: $set_cardinality($set_singleton(p)) == 1);

function $set_universe() : $ptrset;
axiom (forall #o: $ptr :: {$set_in(#o, $set_universe())} $set_in(#o, $set_universe()));

function $set_disjoint(s1:$ptrset, s2:$ptrset) : bool;
function $id_set_disjoint(p:$ptr, s1:$ptrset, s2:$ptrset) : int;

axiom (forall p:$ptr, s1:$ptrset, s2:$ptrset :: {$set_disjoint(s1, s2), $set_in(p, s1)}
  $set_disjoint(s1, s2) && $set_in(p, s1) ==> 
    $id_set_disjoint(p, s1, s2) == 1);
axiom (forall p:$ptr, s1:$ptrset, s2:$ptrset :: {$set_disjoint(s1, s2), $set_in(p, s2)}
  $set_disjoint(s1, s2) && $set_in(p, s2) ==> 
    $id_set_disjoint(p, s1, s2) == 2);

axiom (forall s1:$ptrset, s2:$ptrset :: {$set_disjoint(s1, s2)}
  (forall p:$ptr :: {$dont_instantiate(p)}
     ($set_in(p, s1) ==> !$set_in(p, s2)) && ($set_in(p, s2) ==> !$set_in(p, s1))) 
  ==> $set_disjoint(s1, s2));

function $set_in_pos($ptr, $ptrset) : bool;
axiom (forall p:$ptr, s:$ptrset :: {$set_in(p, s)}
  $set_in(p, s) ==> $set_in_pos(p, s));

//function $set_in3($ptr, $ptrset) : bool;
//function $set_in2($ptr, $ptrset) : bool;

//function $in_some_owns($ptr) : bool;
//axiom (forall p:$ptr, S1:$state, p1:$ptr :: 
//  {$set_in(p, $owns(S1, p1))}
//  $set_in(p, $owns(S1, p1)) ==> $in_some_owns(p));
//
//axiom (forall p:$ptr, S1:$state, p1:$ptr :: 
//  {$set_in2(p, $owns(S1, p1)), $in_some_owns(p)}
//  $set_in(p, $owns(S1, p1)) <==> $set_in2(p, $owns(S1, p1)));

//axiom (forall p:$ptr, s:$ptrset :: {$set_in(p, s)}
//  $set_in(p, s) <==> $set_in2(p, s));
//axiom (forall p:$ptr, s:$ptrset :: {$set_in(p, s)}
//  $set_in(p, s) <==> $set_in3(p, s));

function $set_in0(p:$ptr, s:$ptrset) : bool
  { $set_in(p, s) }


function sk_hack(bool) : bool;
function $start_here() : bool;


// --------------------------------------------------------------------------------
// Arithmetic
// --------------------------------------------------------------------------------

function {:inline true} $in_range(min:int, val:int, max:int) : bool
  { min <= val && val <= max }

function {:inline true} $bool_to_int(v:bool) : int
  { if v then 1 else 0 }

function {:inline true} $int_to_bool(x:int) : bool
  { x != 0 }

// TODO check if still needed
// a hack, used when parameter to ITE is a quntified variable to prevent Z3 from crashing
function $bool_id(x:bool) : bool { x }


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

function {:inline true} $in_range_i1(x:int) : bool { $in_range($min.i1, x, $max.i1) }
function {:inline true} $in_range_i2(x:int) : bool { $in_range($min.i2, x, $max.i2) }
function {:inline true} $in_range_i4(x:int) : bool { $in_range($min.i4, x, $max.i4) }
function {:inline true} $in_range_i8(x:int) : bool { $in_range($min.i8, x, $max.i8) }
function {:inline true} $in_range_u1(x:int) : bool { $in_range(0, x, $max.u1) }
function {:inline true} $in_range_u2(x:int) : bool { $in_range(0, x, $max.u2) }
function {:inline true} $in_range_u4(x:int) : bool { $in_range(0, x, $max.u4) }
function {:inline true} $in_range_u8(x:int) : bool { $in_range(0, x, $max.u8) }
function {:inline true} $in_range_ptr(p:$ptr) : bool { $in_range_u8($ref(p)) }

function {:inline true} $in_range_div_i1(x:int, y:int) : bool { y != -1 || x != $min.i1 }
function {:inline true} $in_range_div_i2(x:int, y:int) : bool { y != -1 || x != $min.i2 }
function {:inline true} $in_range_div_i4(x:int, y:int) : bool { y != -1 || x != $min.i4 }
function {:inline true} $in_range_div_i8(x:int, y:int) : bool { y != -1 || x != $min.i8 }

function $byte_ptr_subtraction(p1:$ptr, p2:$ptr) : int
  { $ref(p1) - $ref(p2) }
function $_pow2(int) : int;
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

axiom $unchecked(^^u4, -1) == $max.u4;
axiom $unchecked(^^u4, $max.u4 + 1) == 0;
axiom $unchecked(^^u8, -1) == $max.u8;
axiom $unchecked(^^u8, $max.u8 + 1) == 0;

function $in_range_ubits(bits:int, v:int) : bool
  { $in_range(0, v, $_pow2(bits) - 1) }

function $unchecked_sbits(bits:int, v:int) : int;
axiom (forall bits:int, v:int :: {$unchecked_sbits(bits, v)}
  $in_range_sbits(bits, $unchecked_sbits(bits, v)) &&
  ($in_range_sbits(bits, v) ==> $unchecked_sbits(bits, v) == v));

function $in_range_sbits(bits:int, v:int) : bool
  { $in_range(-$_pow2(bits-1), v, $_pow2(bits-1) - 1) }

function $unchecked_ubits(bits:int, v:int) : int;
axiom (forall bits:int, v:int :: {$unchecked_ubits(bits, v)}
  $in_range_ubits(bits, $unchecked_ubits(bits, v)) &&
  ($in_range_ubits(bits, v) ==> $unchecked_ubits(bits, v) == v));

function $_or(t:$ctype, x:int, y:int) : int;
function $_xor(t:$ctype, x:int, y:int) : int;
function $_and(t:$ctype, x:int, y:int) : int;
function $_not(t:$ctype, x:int) : int;

function $unchk_add(t:$ctype, x:int, y:int) : int { $unchecked(t, x + y) }
function $unchk_sub(t:$ctype, x:int, y:int) : int { $unchecked(t, x - y) }
function $unchk_mul(t:$ctype, x:int, y:int) : int { $unchecked(t, x * y) }
function $unchk_div(t:$ctype, x:int, y:int) : int { $unchecked(t, x / y) }
function $unchk_mod(t:$ctype, x:int, y:int) : int { $unchecked(t, x % y) }

function $_shl(t:$ctype, x:int, y:int) : int
  { $unchecked(t, x * $_pow2(y)) }
function $_shr(x:int, y:int) : int
  { x / $_pow2(y) }

function $bv_extract_signed(val:int, val_bitsize:int, from:int, to:int) : int;
function $bv_extract_unsigned(val:int, val_bitsize:int, from:int, to:int) : int;
function $bv_update(val:int, val_bitsize:int, from:int, to:int, repl:int) : int;

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

function $unchecked(t:$ctype, val:int) : int;
function $in_range_t(t:$ctype, x:int) : bool;

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
function $_mul(x:int, y:int) returns (int) { x * y }

// --------------------------------------------------------------------------------
// Conversion functions
// --------------------------------------------------------------------------------

function $ptr_to_int($ptr) : int;
function $int_to_ptr(int) : $ptr;
axiom (forall p:$ptr :: $int_to_ptr($ptr_to_int(p)) == p);

function $field_to_int($field) : int;
function $int_to_field(int) : $field;
axiom (forall p:$field :: $int_to_field($field_to_int(p)) == p);

function $ptrset_to_int($ptrset) : int;
function $int_to_ptrset(int) : $ptrset;
axiom (forall p:$ptrset :: $int_to_ptrset($ptrset_to_int(p)) == p);

function $ptr_to_u8($ptr) : int;
function $ptr_to_i8($ptr) : int;
function $ptr_to_u4($ptr) : int;
function $ptr_to_i4($ptr) : int;
function $ptr_to_u2($ptr) : int;
function $ptr_to_i2($ptr) : int;
function $ptr_to_u1($ptr) : int;
function $ptr_to_i1($ptr) : int;

axiom ($ptr_to_u8($null) == 0);
axiom ($ptr_to_i8($null) == 0);
axiom ($ptr_to_u4($null) == 0);
axiom ($ptr_to_i4($null) == 0);
axiom ($ptr_to_u2($null) == 0);
axiom ($ptr_to_i2($null) == 0);
axiom ($ptr_to_u1($null) == 0);
axiom ($ptr_to_i1($null) == 0);

function {:inline true} $u8_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $i8_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $u4_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $i4_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $u2_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $i2_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $u1_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }
function {:inline true} $i1_to_ptr(x : int) : $ptr { $ptr(^^void, x)  }

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

function $add_f4(x:$primitive, y:$primitive) : $primitive;
function $sub_f4(x:$primitive, y:$primitive) : $primitive;
function $mul_f4(x:$primitive, y:$primitive) : $primitive;
function $div_f4(x:$primitive, y:$primitive) : $primitive;
function $neg_f4(x:$primitive) : $primitive;
function $lt_f4(x:$primitive, y:$primitive) : bool;
function $leq_f4(x:$primitive, y:$primitive) : bool;
function $gt_f4(x:$primitive, y:$primitive) : bool;
function $geq_f4(x:$primitive, y:$primitive) : bool;
function $add_f8(x:$primitive, y:$primitive) : $primitive;
function $sub_f8(x:$primitive, y:$primitive) : $primitive;
function $mul_f8(x:$primitive, y:$primitive) : $primitive;
function $div_f8(x:$primitive, y:$primitive) : $primitive;
function $neg_f8(x:$primitive) : $primitive;
function $lt_f8(x:$primitive, y:$primitive) : bool;
function $leq_f8(x:$primitive, y:$primitive) : bool;
function $gt_f8(x:$primitive, y:$primitive) : bool;
function $geq_f8(x:$primitive, y:$primitive) : bool;

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

function #cev_init(n:int) : bool;
function #cev_save_position(n:int) : $token;
function #cev_var_intro(n:int, locorglob:var_locglob, name:$token, val:int, typ: $ctype) : bool;
function #cev_var_update(n:int, locorglob:var_locglob, name:$token, val:int) : bool;
function #cev_control_flow_event(n:int, tag : cf_event) : bool;
function #cev_function_call(n:int) : bool;

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

// --------------------------------------------------------------------------------
// Map axioms
// --------------------------------------------------------------------------------

/*
function $rd.MAP(m:MAP, p:PTR) : VAL;
function $wr.MAP(m:MAP, p:PTR, v:VAL) : MAP;
axiom (forall m:MAP, p:PTR, v:VAL :: $rd.MAP($wr.MAP(m, p, v), p) == v);
axiom (forall m:MAP, p1,p2:PTR, v:VAL :: $rd.MAP($wr.MAP(m, p1, v), p2) == $rd.MAP(m, p2) || p1 == p2);
*/

function $rd.$heap(m:$heap, p:$ptr) : $object;
function $wr.$heap(m:$heap, p:$ptr, v:$object) : $heap;
axiom (forall m:$heap, p:$ptr, v:$object :: $rd.$heap($wr.$heap(m, p, v), p) == v);
axiom (forall m:$heap, p1,p2:$ptr, v:$object :: $rd.$heap($wr.$heap(m, p1, v), p2) == $rd.$heap(m, p2) || p1 == p2);

function $rd.$object(m:$object, p:$field) : $sub_object;
function $wr.$object(m:$object, p:$field, v:$sub_object) : $object;
axiom (forall m:$object, p:$field, v:$sub_object :: $rd.$object($wr.$object(m, p, v), p) == v);
axiom (forall m:$object, p1,p2:$field, v:$sub_object :: $rd.$object($wr.$object(m, p1, v), p2) == $rd.$object(m, p2) || p1 == p2);

function $rd.$sub_object(m:$sub_object, p:$ptr) : int;
function $wr.$sub_object(m:$sub_object, p:$ptr, v:int) : $sub_object;
axiom (forall m:$sub_object, p:$ptr, v:int :: $rd.$sub_object($wr.$sub_object(m, p, v), p) == v);
axiom (forall m:$sub_object, p1,p2:$ptr, v:int :: $rd.$sub_object($wr.$sub_object(m, p1, v), p2) == $rd.$sub_object(m, p2) || p1 == p2);

function $rd.$roots(m:$roots, p:$ptr) : $ptr;
function $wr.$roots(m:$roots, p:$ptr, v:$ptr) : $roots;
axiom (forall m:$roots, p:$ptr, v:$ptr :: $rd.$roots($wr.$roots(m, p, v), p) == v);
axiom (forall m:$roots, p1,p2:$ptr, v:$ptr :: $rd.$roots($wr.$roots(m, p1, v), p2) == $rd.$roots(m, p2) || p1 == p2);

function $rd.$embs(m:$embs, p:$ptr) : $ptr;
function $wr.$embs(m:$embs, p:$ptr, v:$ptr) : $embs;
axiom (forall m:$embs, p:$ptr, v:$ptr :: $rd.$embs($wr.$embs(m, p, v), p) == v);
axiom (forall m:$embs, p1,p2:$ptr, v:$ptr :: $rd.$embs($wr.$embs(m, p1, v), p2) == $rd.$embs(m, p2) || p1 == p2);

/*
function $rd.MAP(m:MAP, p1,p2:$ptr) : VAL;
function $wr.MAP(m:MAP, p1,p2:$ptr, v:VAL) : MAP;
axiom (forall m:MAP, p1,p2:$ptr, v:VAL :: $rd.MAP($wr.MAP(m, p1, p2, v), p1, p2) == v);
axiom (forall m:MAP, p1,p2,q1,q2:$ptr, v:VAL :: $rd.MAP($wr.MAP(m, p1, p2, v), q1, q2) == $rd.MAP(m, q1, q2) || (p1 == q1 && p2 == q2));
*/

function $rd.$timestamps(m:$timestamps, p1,p2:$ptr) : int;
function $wr.$timestamps(m:$timestamps, p1,p2:$ptr, v:int) : $timestamps;
axiom (forall m:$timestamps, p1,p2:$ptr, v:int :: $rd.$timestamps($wr.$timestamps(m, p1, p2, v), p1, p2) == v);
axiom (forall m:$timestamps, p1,p2,q1,q2:$ptr, v:int :: $rd.$timestamps($wr.$timestamps(m, p1, p2, v), q1, q2) == $rd.$timestamps(m, q1, q2) || (p1 == q1 && p2 == q2));

function $rd.$typed(m:$typed, p1,p2:$ptr) : bool;
function $wr.$typed(m:$typed, p1,p2:$ptr, v:bool) : $typed;
axiom (forall m:$typed, p1,p2:$ptr, v:bool :: $rd.$typed($wr.$typed(m, p1, p2, v), p1, p2) == v);
axiom (forall m:$typed, p1,p2,q1,q2:$ptr, v:bool :: $rd.$typed($wr.$typed(m, p1, p2, v), q1, q2) == $rd.$typed(m, q1, q2) || (p1 == q1 && p2 == q2));

function $rd.$owner(m:$owner, p1,p2:$ptr) : $ptr;
function $wr.$owner(m:$owner, p1,p2:$ptr, v:$ptr) : $owner;
axiom (forall m:$owner, p1,p2:$ptr, v:$ptr :: $rd.$owner($wr.$owner(m, p1, p2, v), p1, p2) == v);
axiom (forall m:$owner, p1,p2,q1,q2:$ptr, v:$ptr :: $rd.$owner($wr.$owner(m, p1, p2, v), q1, q2) == $rd.$owner(m, q1, q2) || (p1 == q1 && p2 == q2));

function $rd.$closed(m:$closed, p1,p2:$ptr) : bool;
function $wr.$closed(m:$closed, p1,p2:$ptr, v:bool) : $closed;
axiom (forall m:$closed, p1,p2:$ptr, v:bool :: $rd.$closed($wr.$closed(m, p1, p2, v), p1, p2) == v);
axiom (forall m:$closed, p1,p2,q1,q2:$ptr, v:bool :: $rd.$closed($wr.$closed(m, p1, p2, v), q1, q2) == $rd.$closed(m, q1, q2) || (p1 == q1 && p2 == q2));

// That's all folks.
