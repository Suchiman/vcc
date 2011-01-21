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

// a typed pointer -- i.e. a pair of a $field and a $ptr
type $ptr;

// name of a C field -- each field of a C struct gets a constant of this type
//   like A.b for field b of struct A.
type $field; 

type $base = $ptr;

// for float and double
type $primitive;

// for structs passed by value
type $struct;

// used for constants describing position in the source code (debugging/model printing only)
type $token;

type $state;

type $object = [$field][$ptr]int;
type $owner = [$ptr]$ptr;
type $closed = [$ptr]bool;
type $timestamps = [$ptr]int;
type $roots = [$ptr]$ptr;

// A constant is generated for each pure function, used for ordering frame axioms.
type $pure_function;

// For labeled contracts.
type $label;

type $labelset;

// ----------------------------------------------------------------------------
// Type algebra
// ----------------------------------------------------------------------------

type $ctype_branch;
function $type_branch($ctype) : $ctype_branch;

const unique $ctype_flat : $ctype_branch;
const unique $ctype_ptr : $ctype_branch;
const unique $ctype_spec_ptr : $ctype_branch;
const unique $ctype_map : $ctype_branch;
const unique $ctype_array : $ctype_branch;

// inverse functions here (unptr_to, map_domain, map_range) are for the prover
// so it knows that int*!=short*, i.e. * is injective

// pointers to types
function $ptr_to($ctype) : $ctype;
function $spec_ptr_to($ctype) : $ctype;
function $type_project_0($ctype) : $ctype;

axiom (forall #n:$ctype :: {$ptr_to(#n)} $type_project_0($ptr_to(#n)) == #n && $type_branch($ptr_to(#n)) == $ctype_ptr);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $type_project_0($spec_ptr_to(#n)) == #n && $type_branch($spec_ptr_to(#n)) == $ctype_spec_ptr);

axiom (forall #n:$ctype :: {$ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $sizeof($ptr_to(#n)) == $arch_ptr_size);

function $map_t($ctype, $ctype) : $ctype;
function $map_domain($ctype) : $ctype;
function $map_range($ctype) : $ctype;

axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $map_domain($map_t(#r,#d)) == #d && $map_range($map_t(#r,#d)) == #r && $type_branch($map_t(#r,#d)) == $ctype_map);

// ----------------------------------------------------------------------------
// Type properties
// ----------------------------------------------------------------------------

function $sizeof($ctype): int; // in bytes

// for types for which $in_range_t(...) is defined
function $as_in_range_t($ctype) : $ctype;

function {:inline true} $def_flat_type(t:$ctype, sz:int) : bool
  { $sizeof(t) == sz && $type_branch(t) == $ctype_flat }

function {:inline true} $def_primitive_type(t:$ctype, sz:int) : bool
  { $def_flat_type(t, sz) && $is_primitive(t) }

function {:inline true} $def_math_type(t:$ctype) : bool
  { $def_primitive_type(t, 1) && $is_math_type(t) }

function {:inline true} $def_fnptr_type(t:$ctype) : bool
  { $def_primitive_type(t, $arch_ptr_size) && $is_fnptr_type(t) }

function {:inline true} $def_record_type(t:$ctype) : bool
  { $def_primitive_type(t, 1) && $is_record_type(t) }

function {:inline true} $def_composite_type(t:$ctype, sz:int, claimable:bool, has_volatile_owns:bool) : bool
  { 
     $def_flat_type(t, sz) && 
     $is_non_primitive(t) && 
     $is_claimable(t) == claimable && 
     (if has_volatile_owns then $is_volatile_field($f_owns(t)) else $is_sequential_field($f_owns(t))) &&
     true }
function {:inline true} $def_integer_type(t:$ctype, sz:int) : bool
  { $def_primitive_type(t, sz) && $as_in_range_t(t) == t }

function $is_primitive(t:$ctype) : bool;
function {:inline true} $is_non_primitive(t:$ctype) : bool
  { !$is_primitive(t) }
function {:inline true} $is_non_primitive_ptr(p:$ptr) : bool
  { $is_non_primitive($typ(p)) }

function $is_claimable($ctype) : bool;
function $is_group_type($ctype) : bool;

// These three are not really used for anything, get rid of them?
function $is_math_type(t:$ctype) : bool;
function $is_fnptr_type(t:$ctype) : bool;
function $is_record_type(t:$ctype) : bool;

axiom (forall #r:$ctype, #d:$ctype :: {$map_t(#r,#d)} $is_primitive($map_t(#r,#d)));
axiom (forall #n:$ctype :: {$ptr_to(#n)} $is_primitive($ptr_to(#n)));
axiom (forall #n:$ctype :: {$spec_ptr_to(#n)} $is_primitive($spec_ptr_to(#n)));

axiom (forall #n:$ctype :: {$is_primitive(#n)} $is_primitive(#n) ==> !$is_claimable(#n));

// ----------------------------------------------------------------------------
// Pointers
// ----------------------------------------------------------------------------

function {:inline true} $typ(p:$ptr): $ctype
  { $field_type($field(p)) }
function $addr($ptr): int;
function $base($ptr): $base;
function $field($ptr) : $field;
function $ptr($field,$base): $ptr;
axiom (forall t:$field, b:$base :: $field($ptr(t,b))==t);
axiom (forall t:$field, b:$base :: $base($ptr(t,b))==b);
axiom (forall p:$ptr :: {$base(p)} {$field(p)} $ptr($field(p), $base(p)) == p); // PERF 6.7%

function {:inline true} $non_null(p:$ptr) : bool
  { !$is_null(p) }
function $is_null(p:$ptr) : bool;
axiom (forall p:$ptr :: {$addr(p)} $addr(p) == 0 <==> $is_null(p));

// ----------------------------------------------------------------------------
// Field algebra
// ----------------------------------------------------------------------------

type $field_kind;
const unique $fk_base : $field_kind;
const unique $fk_owns : $field_kind;
const unique $fk_ref_cnt : $field_kind;
const unique $fk_vol_version : $field_kind;
const unique $fk_allocation_root : $field_kind;
const unique $fk_as_array_first : $field_kind;
const unique $fk_emb_array : $field_kind;

function $field_kind($field) : $field_kind;

function {:inline true} $is_base_field(f:$field) : bool
  { $field_kind(f) == $fk_base }

// Meaning it's not some pointer casted to an unrelated type.
// When casting to something odd, the field_parent_type() will point
// to something that no field_type() returns. TODO what about f_root(something) ?
function {:inline true} $is_proper(p:$ptr) : bool
  { $field_parent_type($field(p)) == $typ($base(p)) }

//typed pointer test
function $is(p:$ptr, t:$ctype) : bool
  { $typ(p) == t }

function $spec_ptr_cast(p:$ptr, t:$ctype) : $ptr;
function $phys_ptr_cast(p:$ptr, t:$ctype) : $ptr;

axiom (forall p:$ptr :: {$spec_ptr_cast(p, $typ(p))} {$in_range_spec_ptr(p)}
  $in_range_spec_ptr(p) ==> $spec_ptr_cast(p, $typ(p)) == p);
axiom (forall p:$ptr :: {$phys_ptr_cast(p, $typ(p))} {$in_range_phys_ptr(p)}
  $in_range_phys_ptr(p) ==> $phys_ptr_cast(p, $typ(p)) == p);
axiom (forall p:$ptr, t:$ctype :: {$addr($spec_ptr_cast(p, t))}
  $addr($spec_ptr_cast(p, t)) == $addr(p));
axiom (forall p:$ptr, t:$ctype :: {$addr($phys_ptr_cast(p, t))}
  $addr($phys_ptr_cast(p, t)) == $addr(p));
function {:inline true} $cast_props(p:$ptr, t:$ctype, c:$ptr) : bool
  { $typ(c) == t && $is_null(c) == $is_null(p) && $field(c) == $as_field_with_type($field(c), t) }
axiom (forall p:$ptr, t:$ctype :: {$spec_ptr_cast(p, t)}
  $cast_props(p, t, $spec_ptr_cast(p, t)) && $in_range_spec_ptr($spec_ptr_cast(p, t)));
axiom (forall p:$ptr, t:$ctype :: {$phys_ptr_cast(p, t)}
  $cast_props(p, t, $phys_ptr_cast(p, t)) && $in_range_phys_ptr($phys_ptr_cast(p, t)));
axiom (forall p:$ptr :: {$in_range_phys_ptr(p)}
  !$in_range_phys_ptr(p) ==> $in_range_spec_ptr(p));

/*
Doesn't seem needed.

axiom (forall p:$ptr, f:$field :: {$has_field_at0($typ(p), f), $phys_ptr_cast(p, $field_type(f))}
   $is_proper(p) &&
   $in_range_phys_ptr(p) &&
   $has_field_at0($typ(p), f) ==>
     $phys_ptr_cast(p, $field_type(f)) == $dot(p, f));

axiom (forall p:$ptr, f:$field :: {$has_field_at0($typ(p), f), $spec_ptr_cast(p, $field_type(f))}
   $is_proper(p) &&
   $in_range_spec_ptr(p) &&
   $has_field_at0($typ(p), f) ==>
     $spec_ptr_cast(p, $field_type(f)) == $dot(p, f));
*/

axiom (forall p:$ptr :: 
   {$spec_ptr_cast(p, $field_parent_type($field(p))), $has_field_at0($field_parent_type($field(p)), $field(p))}
   $is_proper(p) && $in_range_phys_ptr(p) && $has_field_at0($field_parent_type($field(p)), $field(p)) ==>
     $phys_ptr_cast(p, $field_parent_type($field(p))) == $emb1(p));

// This may be unsound.
//axiom (forall p:$ptr :: {$is_group_type($field_type($field(p)))}
//  $is_proper(p) && $is_group_type($field_type($field(p))) ==>
//    $field(p) == $f_group_root($field_type($field(p))));

function $dot(p:$ptr, f:$field) : $ptr
  { $ptr(f, p) }

axiom (forall p:$ptr, f:$field :: {$addr($dot(p, f))}
  $is_phys_field(f) && $is_proper($dot(p, f)) ==>
     $addr($dot(p, f)) == $addr(p) + $field_offset(f));

// PERF 5.7%
axiom (forall p:$ptr, f:$field :: {$dot(p, f)}
     ($in_range_spec_ptr(p) || $is_ghost_field(f) ==> $in_range_spec_ptr($dot(p, f)))
  && ($in_range_phys_ptr(p) && $is_phys_field(f) ==> $in_range_phys_ptr($dot(p, f))) 
  && ($is_proper($dot(p, f)) ==> $non_null(p) ==> $non_null($dot(p, f))));

function {:inline true} $emb1(p:$ptr) : $ptr
  { $base(p) }
function {:inline true} $emb(S:$state,p:$ptr) : $ptr
  { $emb0(p) }
function $emb0(p:$ptr) : $ptr
  { if $is_primitive($typ(p)) then $base(p) else p }

function $is_semi_sequential_field($field) : bool;
function $is_sequential_field($field) : bool;
function $is_volatile_field($field) : bool;
function $as_primitive_field($field) : $field;
function $as_composite_field($field) : $field;
function $as_field_with_type($field,$ctype) : $field;
function {:inline true} $as_ptr_with_type(p:$ptr, t:$ctype) : $ptr
  { $ptr($as_field_with_type($field(p), t), $base(p)) }
function $field_type($field) : $ctype;
function $has_field_at0($ctype, $field) : bool;


axiom (forall f:$field :: {$field_parent_type(f)} $is_non_primitive($field_parent_type(f)));// PERF 0.6%

function {:inline true} $def_field_family(partp:$ctype, f:$field, tp:$ctype) : bool
  { 
    $field_parent_type(f) == partp &&
    $field_type(f) == tp &&
    $as_field_with_type(f, tp) == f &&
    ($is_primitive(tp) ==> $as_primitive_field(f) == f) &&
    (!$is_primitive(tp) ==> $as_composite_field(f) == f) &&
    $field_arr_root(f) == f &&
    true
  }

function {:inline true} $def_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool) : bool
  { 
    $def_field_family(partp, f, tp) &&
    (!isvolatile ==> $is_sequential_field(f)) &&
    (isvolatile ==> $is_volatile_field(f)) &&
    true
  }

function {:inline true} $def_phys_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool, off:int) : bool
  { $def_field(partp, f, tp, isvolatile) &&
    $field_offset(f) == off &&
    $is_phys_field(f) &&
    $is_base_field(f) && 
    (off == 0 ==> $has_field_at0(partp, f)) &&
    true
  }

function {:inline true} $def_ghost_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool) : bool
  { $def_field(partp, f, tp, isvolatile) &&
    $is_base_field(f) && 
    $is_ghost_field(f) &&
    true
  }

function {:inline true} $def_group(partp:$ctype, f:$field, tp:$ctype) : bool
  { $def_phys_field(partp, f, tp, false, 0) &&
    $f_group_root(tp) == f &&
    $is_group_type(tp) &&
    true
  }

function $f_group_root($ctype) : $field;
function $f_root($ctype) : $field;
function $f_owns($ctype) : $field;
function $f_ref_cnt($ctype) : $field;
function $f_vol_version($ctype) : $field;

function {:inline true} $def_special_field(partp:$ctype, f:$field, tp:$ctype, fk:$field_kind) : bool
  {
    $def_field_family(partp, f, tp) &&
    $field_kind(f) == fk &&
    true
  }

function {:inline true} $def_special_ghost_field(partp:$ctype, f:$field, tp:$ctype, fk:$field_kind) : bool
  { $def_special_field(partp, f, tp, fk) && $is_ghost_field(f) }

const unique $primitive_emb_type : $ctype;

axiom (forall t:$ctype :: {$f_root(t)}
  if $is_non_primitive(t) then
    $def_special_field(t, $f_root(t), t, $fk_allocation_root)
  else
    $def_special_field($primitive_emb_type, $f_root(t), t, $fk_allocation_root) &&
    $is_sequential_field($f_root(t))
    );

axiom (forall t:$ctype :: {$f_owns(t)} 
  $is_non_primitive(t) ==>
    $def_special_ghost_field(t, $f_owns(t), ^$#ptrset, $fk_owns));
axiom (forall t:$ctype :: {$f_ref_cnt(t)} 
  $is_non_primitive(t) ==>
    $def_special_ghost_field(t, $f_ref_cnt(t), ^^mathint, $fk_ref_cnt) &&
    $is_volatile_field($f_ref_cnt(t)));
axiom (forall t:$ctype :: {$f_vol_version(t)} 
  $is_non_primitive(t) ==>
    $def_special_ghost_field(t, $f_vol_version(t), ^$#volatile_version_t, $fk_vol_version) &&
    $is_semi_sequential_field($f_vol_version(t)));

// ----------------------------------------------------------------------------
// Built-in types and constants
// ----------------------------------------------------------------------------

const $arch_ptr_size : int; // arch-specific; to be defined by a compiler-generated axiom

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
axiom $def_math_type(^^object);
axiom $def_math_type(^^field);

const unique ^^claim: $ctype;
const unique ^^mathint: $ctype;
const unique ^$#ptrset : $ctype;
const unique ^$#state_t : $ctype;
const unique ^$#volatile_version_t : $ctype;
const unique ^$#struct : $ctype;

const unique ^$#seq_version : $ctype;
const unique ^$#vol_version : $ctype;

axiom $def_composite_type(^^claim, 1, true, false);
axiom $def_composite_type(^$#volatile_version_t, 1, false, false);
axiom $def_math_type(^^mathint);
axiom $def_math_type(^$#ptrset);
axiom $def_math_type(^$#state_t);
axiom $def_math_type(^$#struct);

function $field_offset($field) : int;
function $field_parent_type($field) : $ctype;
function $is_ghost_field($field) : bool;
function $is_phys_field($field) : bool;

const $null:$ptr;
axiom $addr($null) == 0;
axiom $in_range_spec_ptr($null) && $in_range_phys_ptr($null);

const unique ^$#thread_id_t: $ctype;
function $me() : $ptr;
axiom $def_composite_type(^$#thread_id_t, 1, false, true);
axiom $field($me()) == $f_root(^$#thread_id_t);
axiom $in_range_spec_ptr($me());
axiom $non_null($me());
axiom $is_proper($me());
function $is_threadtype(t:$ctype) : bool { t == ^$#thread_id_t }

// ----------------------------------------------------------------------------
// Arrays
// ----------------------------------------------------------------------------

function $field_arr_index($field) : int;
function $field_arr_root($field) : $field;
function $field_arr_size($field) : int;

function $field_arr_ctor($field, int) : $field;
axiom (forall f:$field :: {$field_arr_index(f)}
  f == $field_arr_ctor($field_arr_root(f), $field_arr_index(f)));
axiom (forall f:$field, i:int :: {$field_arr_ctor(f, i)}
  $field_arr_root($field_arr_ctor(f, i)) == f && $field_arr_index($field_arr_ctor(f, i)) == i);

function {:inline true} $def_phys_arr_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool, off:int, sz:int) : bool
  { $def_field(partp, f, tp, isvolatile) &&
    $field_offset(f) == off &&
    $is_phys_field(f) &&
    $field_arr_size(f) == sz &&
    $field_arr_index(f) == 0 &&
    $field_kind(f) == $fk_emb_array &&
    true
  }

function {:inline true} $def_ghost_arr_field(partp:$ctype, f:$field, tp:$ctype, isvolatile:bool, sz:int) : bool
  { $def_field(partp, f, tp, isvolatile) &&
    $is_ghost_field(f) &&
    $field_arr_size(f) == sz &&
    $field_arr_index(f) == 0 &&
    $field_kind(f) == $fk_emb_array &&
    true
  }

function $idx(p:$ptr, i:int) : $ptr
  {
    $dot($base(p), $field_plus($field(p), i))
  }

axiom (forall p:$ptr, i:int :: {$addr($idx(p, i))}
//  $is_proper($idx(p, i)) ==>
    $addr($idx(p, i)) == $addr(p) + $sizeof($typ(p)) * i);
axiom (forall p:$ptr, i:int :: {$idx(p, i)}
  $is_proper($idx(p, i)) ==>
    $non_null(p) ==> $non_null($idx(p, i)));

axiom (forall p:$ptr, i:int :: {$idx(p, i)}
  ($in_range_phys_ptr(p) || $in_range_phys_ptr($base(p)))
  && $is_proper($idx(p, i)) ==> $in_range_phys_ptr($idx(p, i)));

function $field_plus($field, int) : $field;
axiom (forall f:$field, i:int :: {$field_plus(f, i)}
  $field_kind($field_plus(f, i)) == $field_kind(f) &&
  $field_arr_root($field_plus(f, i)) == $field_arr_root(f) &&
  $field_arr_index($field_plus(f, i)) == $field_arr_index(f) + i &&
  $field_arr_size($field_plus(f, i)) == $field_arr_size(f) &&
  $field_type($field_plus(f, i)) == $field_type(f) &&
  $as_field_with_type($field_plus(f, i), $field_type(f)) == $field_plus(f, i) &&
  $is_sequential_field($field_plus(f, i)) == $is_sequential_field(f) &&
  $is_phys_field($field_plus(f, i)) == $is_phys_field(f) &&
  ($in_range(0, $field_arr_index(f) + i, $field_arr_size(f) - 1) ==> 
     $field_parent_type($field_plus(f, i)) == $field_parent_type($field_arr_root(f))) &&
  true
  );

function $is_array(S:$state, p:$ptr, T:$ctype, sz:int) : bool
{   
     $is(p, T)
  && $is_proper(p)
  && $field_arr_size($field(p)) >= $field_arr_index($field(p)) + sz
  && $field(p) == $field_plus($field_arr_root($field(p)), $field_arr_index($field(p)))
  && $field_kind($field(p)) != $fk_base
  && $field_arr_index($field(p)) >= 0
  && $is_non_primitive($typ($emb(S, p)))
}

function $is_thread_local_array(S:$state, p:$ptr, T:$ctype, sz:int) : bool
{
     $is_array(S, p, T, sz)
  && if $is_primitive(T) then $thread_local(S, p)
     else (forall i:int :: {$owner(S, $idx(p, i))} 0 <= i && i < sz ==> $thread_local(S, $idx(p, i)))
}

function $is_mutable_array(S:$state, p:$ptr, T:$ctype, sz:int) : bool
{
     $is_array(S, p, T, sz)
  && if $is_primitive(T) then $mutable(S, $emb0(p))
     else (forall i:int :: {$owner(S, $idx(p, i))} 0 <= i && i < sz ==> $mutable(S, $idx(p, i)))
}

function $array_range(S:$state, p:$ptr, T:$ctype, sz:int) : $ptrset
  { $array_range_no_state(p, T, sz) }

function {:inline true} $mem_range(s:$state, p:$ptr, sz:int) : int
  { $mem_range_heap($heap(s), p, sz) }
function  $mem_range_heap(s:$object, p:$ptr, sz:int) : int;

function $guess_index(p:$ptr, r:$ptr) : int;

axiom (forall p:$ptr, n:int :: {$guess_index(p, $idx(p, n))}
  $guess_index(p, $idx(p, n)) == n);

axiom (forall h:$object, r:$ptr, f:$field, v:int, p:$ptr, sz:int ::
      !$in_range(0, $guess_index(p, $ptr(f, r)), sz - 1) ||
      $ptr(f, r) != $idx(p, $guess_index(p, $ptr(f, r))) ==>
         $mem_range_heap($update(h, r, f, v), p, sz) == $mem_range_heap(h, p, sz));

axiom (forall S0, S1:$state, p:$ptr, sz:int ::
  {$call_transition(S0, S1), $mem_range(S1, p, sz)}
  $call_transition(S0, S1) &&
  (forall i:int :: {$dont_instantiate_int(i)} 
    $in_range(0, i, sz - 1) ==> $mem(S0, $idx(p, i)) == $mem(S1, $idx(p, i))) ==>
  $mem_range(S0, p, sz) == $mem_range(S1, p, sz));

// $index_within(p, arr) = ($ref(p) - $ref(arr)) / $sizeof($typ(arr))
// To avoid using division, we define a category of simple indices. 
//   $simple_index(p, arr) iff p == arr[k].f1.f2.f3...fN, where N >= 0.
// We're only interested in simple indices for verification.
function $index_within(p:$ptr, arr:$ptr) : int;
function $simple_index(p:$ptr, arr:$ptr) : bool;

function $array_range_no_state(p:$ptr, T:$ctype, sz:int) : $ptrset
  { if $is_primitive(T) then
      (lambda q:$ptr :: $is_proper(q) &&
                        $emb0(q) == $emb0(p) &&
                        $typ(q) == T &&
                        $field_arr_root($field(q)) == $field_arr_root($field(p)) &&
                        $in_range(0, $field_arr_index($field(q)) - $field_arr_index($field(p)), sz - 1) &&
                        $field_kind($field(q)) != $fk_base)
    else
      (lambda q:$ptr :: $is_proper(q) && $in(q, $full_extent($idx(p, $index_within(q, p)))))
  }

// ----------------------------------------------------------------------------
// As-array
// ----------------------------------------------------------------------------

// to be used when you allocate just the array itself
function $array($ctype, int) : $ctype;
function $element_type($ctype) : $ctype;
function $array_length($ctype) : int;
axiom (forall T:$ctype, s:int :: {$array(T, s)} 
     true
  && $element_type($array(T, s)) == T 
  && $array_length($array(T, s)) == s 
  && $is_non_primitive($array(T, s))
  && !$is_claimable($array(T, s))
  && $type_branch($array(T, s)) == $ctype_array
  && $is_sequential_field($f_owns($array(T, s)))
);
axiom (forall T:$ctype, s:int :: {$sizeof($array(T, s))} $sizeof($array(T, s)) == $sizeof(T) * s);

// the first fields of as_array
function $array_emb($ctype, int) : $field;

function {:inline true} $as_array_first_index(p:$ptr) : $ptr
  { $dot(p, $array_emb($element_type($typ(p)), $array_length($typ(p)))) }

axiom (forall p:$ptr, t:$ctype, sz:int :: {$phys_ptr_cast($as_ptr_with_type(p, $array(t, sz)), t)}
  $phys_ptr_cast($as_ptr_with_type(p, $array(t, sz)), t) == $as_array_first_index(p));

axiom (forall p:$ptr, t:$ctype, sz:int :: {$spec_ptr_cast($as_ptr_with_type(p, $array(t, sz)), t)}
  $spec_ptr_cast($as_ptr_with_type(p, $array(t, sz)), t) == $as_array_first_index(p));

axiom (forall t:$ctype, sz:int :: {$array_emb(t, sz)} {$array(t, sz)}
  $def_field_family($array(t, sz), $array_emb(t, sz), t) &&
  $field_kind($array_emb(t, sz)) == $fk_as_array_first &&
  $is_phys_field($array_emb(t, sz)) &&
  $field_offset($array_emb(t, sz)) == 0 &&
  $field_arr_size($array_emb(t, sz)) == sz &&
  $is_sequential_field($array_emb(t, sz)) &&
  true);

function $as_array(p:$ptr, T:$ctype, sz:int) : $ptr
  {
      if $field($emb1(p)) == $f_root($array(T, sz)) &&
         $field(p) == $array_emb(T, sz) then
        $emb1(p)
      else
        $phys_ptr_cast($null, $array(T, sz))
  }

// ----------------------------------------------------------------------------
// Global state components
// ----------------------------------------------------------------------------

var $s: $state;

function $f_timestamp($state) : $timestamps;
function $f_owner($state) : $owner;
function $f_closed($state) : $closed;
function $roots(s:$state) : $roots;
function $heap(s:$state) : $object;
// returns a pointer for a given address and type
function $typemap($owner) : [int, $ctype]$ptr;


function {:inline true} $root(s:$state, p:$ptr) : $ptr
  { $roots(s)[p] }

function {:inline true} $rd_inv(s:$state, f:$field, p:$ptr) : int { $rd(s,p,f) }
function {:inline true} $rd(s:$state, p:$ptr, f:$field) : int { $heap(s)[f][p] }

function {:inline true} $rdtrig(s:$state, p:$ptr, f:$field) : int { $rd(s, p, f) }
//function $rdtrig(s:$state, p:$ptr, f:$field) : bool;
//function $rdtrigX(s:$state, p:$ptr, f:$field) : bool;
//axiom (forall s:$state, p:$ptr, f:$field :: {$rd(s, p, f)} $rdtrigX(s, p, f));

function {:inline true} $rd_spec_ptr(s:$state, f:$field, p:$ptr, t:$ctype) : $ptr
  { $spec_ptr_cast($int_to_ptr($rd(s, p, f)), t) }

function {:inline true} $rd_phys_ptr(s:$state, f:$field, p:$ptr, t:$ctype) : $ptr
  { $phys_ptr_cast($int_to_ptr($rd(s, p, f)), t) }

function {:inline true} $current_state(s:$state) : $state { s }

function {:inline false} $owner(S:$state, p:$ptr) : $ptr
  { $f_owner(S)[p] }
function {:inline false} $closed(S:$state, p:$ptr) : bool
  { $is_proper(p) && $f_closed(S)[p] }
function {:inline false} $timestamp(S:$state, p:$ptr) : int
  { $f_timestamp(S)[p] }
function {:inline false} $ref_cnt(S:$state, p:$ptr) : int
  { $rd(S, p, $f_ref_cnt($typ(p))) }

function $position_marker() : bool
  { true }

function {:inline true} $owns_inline(S:$state, p:$ptr) : $ptrset
  { $int_to_ptrset($rd(S, p, $f_owns($typ(p)))) }

function $owns(S:$state, p:$ptr) : $ptrset
  { $owns_inline(S, p) }

function {:inline true} $keeps(S:$state, #l:$ptr, #p:$ptr) : bool
  { $set_in(#p, $owns(S, #l)) }

function {:inline true} $wrapped(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is(#p,#t) && $is_proper(#p) && $owner(S, #p) == $me() && $closed(S, #p) && $is_non_primitive(#t) }

function {:inline true} $nested(S:$state, p:$ptr) : bool
  { $typ($owner(S, p)) != ^$#thread_id_t }

function {:inline true} $irrelevant(S:$state, p:$ptr) : bool
  { $owner(S, p) != $me() || ($is_primitive($typ(p)) && $closed(S, p)) }

function $mutable(S:$state, p:$ptr) : bool
  {  $is_proper(p) &&
     $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p)) 
  }

function {:inline true} $thread_owned(S:$state, p:$ptr) : bool
  { $owner(S, $emb(S, p)) == $me() }

function {:inline true} $thread_owned_or_even_mutable(S:$state, p:$ptr) : bool
  {
    if $is_primitive($typ(p)) then
      $owner(S, $emb(S, p)) == $me() && !$closed(S, $emb(S, p))
    else
      $owner(S, p) == $me()
  }

function {:inline true} $initially_mutable(S:$state, W:$ptrset) : bool
  {
    (forall p: $ptr ::
       { $owner(S, p) } { $closed(S, p) } { $owner(S, $emb0(p)) } { $closed(S, $emb0(p)) }
       $set_in(p, W) ==> $mutable(S, p))
  }

function {:inline true} $initially_thread_owned_or_mutable(S:$state, W:$ptrset) : bool
  {
    (forall p: $ptr :: 
       { $owner(S, p) } { $closed(S, p) } { $owner(S, $emb0(p)) } { $closed(S, $emb0(p)) }
       $set_in(p, W) ==> $thread_owned_or_even_mutable(S, p))
  }


function $in_range_phys_ptr(p:$ptr) : bool;
function $in_range_spec_ptr(p:$ptr) : bool;

const $arch_spec_ptr_start : int; // arch-specific; to be defined by a compiler-generated axiom

function {:inline true} $is_ghost_ptr(p:$ptr) : bool
  { $in_range_spec_ptr(p) }

function {:inline true} $is_spec_ptr(p:$ptr, t:$ctype) : bool
  { $is(p, t) && $in_range_spec_ptr(p) }

function {:inline true} $is_phys_ptr(p:$ptr, t:$ctype) : bool
  { $is(p, t) && $in_range_phys_ptr(p) }

/*
axiom (forall S:$state, #r:int, #t:$ctype ::
  {:vcc3 "todo"}
  {$typed(S, $ptr(#t, #r))}
  $typed(S, $ptr(#t, #r)) && $in_range_phys_ptr(#r) ==> $in_range_phys_ptr(#r + $sizeof(#t) - 1));
*/


// This was often used in invariants. Typed now means thread-local, so it doesn't work in invariants.
function {:inline true} $typed2_phys(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is_phys_ptr(#p, #t) }

function {:inline true} $typed2_spec(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is_spec_ptr(#p, #t) }

axiom (forall S:$state, p:$ptr :: {$addr(p), $owner(S, $root(S, p))}
  $good_state(S) ==>
    $owner(S, $root(S, p)) == $me() ==>
      $typemap($f_owner(S))[$addr(p), $typ(p)] == p);

function $ptr_eq(p1:$ptr, p2:$ptr) : bool
  { $addr(p1) == $addr(p2) }

function {:inline true} $ptr_neq(p1:$ptr,p2:$ptr) : bool
  { !$ptr_eq(p1, p2) }

function $byte_ptr_subtraction(p1:$ptr, p2:$ptr) : int
  { $addr(p1) - $addr(p2) }

function {:inline true} $is_primitive_field_of(S:$state, #f:$ptr, #o:$ptr) : bool
  { $is_primitive($typ(#f)) && $emb(S, #f) == #o }


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
  { !$is_primitive($typ(p))
  && $is_proper(p)
  && $owner(S, $root(S, p)) == $me()
//     ($wrapped(S, $root(S, p), $typ($root(S, p))) && $set_in(p, $domain(S, $root(S, p)))))
  }

// required for reading
function $thread_local(S:$state, p:$ptr) : bool
  { 
    $is_proper(p) &&
    if $is_primitive($typ(p)) then
      ($is_sequential_field($field(p)) || !$closed(S, $emb(S, p))) && $thread_local_np(S, $emb(S, p))
    else
      $thread_local_np(S, p) }

function {:inline true} $thread_local2(S:$state, #p:$ptr, #t:$ctype) : bool
  { $is(#p, #t) && $thread_local(S, #p) }

function {:inline true} $typed2(S:$state, p:$ptr, t:$ctype) : bool
  { $thread_local2(S, p, t) }

function {:inline true} $typed(S:$state, p:$ptr) : bool
  { $thread_local(S, p) }

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

function sk_hack(bool) : bool;
function $start_here() : bool;

function $expect_unreachable() : bool;
function $expect_unreachable_master(id:int) : bool;
function $expect_unreachable_child(id:int) : bool;

// ----------------------------------------------------------------------------
// System invariants
// ----------------------------------------------------------------------------

function $function_entry($state) : bool;
function $good_state($state) : bool;
function $invok_state($state) : bool;
function $full_stop($state) : bool;

function {:inline true} $inv(#s1:$state, #p:$ptr, typ:$ctype) : bool
  { $inv2(#s1, #s1, #p, typ) }

function {:inline true} $inv2nt(S1:$state, S2:$state, p:$ptr) : bool
  { $inv2(S1, S2, p, $typ(p)) }

// We generate axioms like:
//   inv2(S1,S2,p,T) <==> invariant of T
// for each struct/union T.
function $inv2(#s1:$state, #s2:$state, #p:$ptr, typ:$ctype) : bool;

function {:inline true} $full_stop_ext(t:$token, S:$state) : bool
  { $good_state_ext(t, S) && $full_stop(S) }

function $file_name_is(id:int, tok:$token) : bool;

axiom (forall S:$state :: {$function_entry(S)}
  $function_entry(S) ==> $full_stop(S) && $current_timestamp(S) >= 0);

axiom (forall S:$state :: {$full_stop(S)}
  $full_stop(S) ==> $good_state(S) && $invok_state(S));

axiom (forall S:$state :: {$invok_state(S)}
  $invok_state(S) ==> $good_state(S));


// Assumed after each meta/state update, means that the meta corresponds to the state
// and where in the source did the update happen.
function $good_state_ext(id:$token, S:$state) : bool;
axiom (forall id:$token, S:$state :: {$good_state_ext(id, S)}
  $good_state_ext(id, S) ==> $good_state(S));

function {:inline true} $closed_is_transitive(S:$state) : bool
  { 
    (forall p:$ptr,q:$ptr ::
      {$set_in_pos(p, $owns_inline(S, q))}
      $good_state(S) &&
      $set_in(p, $owns_inline(S, q)) && $closed(S, q) ==> 
         $is_non_primitive($typ(p)) && 
         $owner(S, p) == q &&
         $closed(S, p) && 
         $non_null(p) &&
         true
         )
  } 

/*
axiom (forall S:$state, p:$ptr, q:$ptr :: {$set_in_pos(p, $owns(S, q)), $is_non_primitive($typ(p))}
  $good_state(S) &&
  $closed(S, q) && $is_non_primitive($typ(p)) ==>
      ($set_in(p, $owns(S, q)) <==> $owner(S, p) == q));
*/

axiom (forall S:$state, #r:$base, #t:$ctype, #f:$field, #sz:int :: {$owns(S, $ptr($as_field_with_type(#f,$array(#t,#sz)), #r))}
  $good_state(S) ==>
    $owns(S, $ptr($as_field_with_type(#f,$array(#t,#sz)), #r)) == $set_empty());

axiom (forall S:$state, #p:$ptr, #t:$ctype :: {$inv(S, #p, #t)}
  $invok_state(S) && $closed(S, #p) ==> $inv(S, #p, #t));

axiom (forall S:$state :: {$good_state(S)}
  $good_state(S) ==> $closed_is_transitive(S));

axiom(forall S: $state, p: $ptr :: {$closed(S, p)}
  $good_state(S) ==>
    $closed(S, p) ==> $non_null(p));

// Root axioms
axiom(forall S: $state, p: $ptr :: {$owner(S, p)} {$root(S, p)}
  $good_state(S) ==>
  $owner(S, p) == $me() ==> $is_proper(p) && $non_null(p) && $is_non_primitive($typ(p)) && $is_proper(p) && $root(S, p) == p);

// PERF 3.2%
axiom (forall S:$state, r:$ptr :: {$owner(S, r)}
  $good_state(S) ==>
    $non_null($owner(S, r)) && $is_proper($owner(S, r)) &&
    ($typ($owner(S, r)) != ^$#thread_id_t ==>
      $is_proper(r) && $non_null(r) && $is_non_primitive($typ(r)) &&
      ($is_sequential_field($f_owns($typ($owner(S, r)))) ==> $root(S, r) == $root(S, $owner(S, r)))
    ));

axiom (forall S:$state, p:$ptr ::
  {$root(S, $root(S, p))}
  $good_state(S) ==> $root(S, $root(S, p)) == $root(S, p));

function $call_transition(S1:$state, S2:$state) : bool;

// Type-system-derived
// TODO: this one should also go, we should always use $unchecked(...) when reading from memory
axiom (forall S:$state, p:$ptr, f:$field, t:$ctype ::
  {$rdtrig(S, p, $as_field_with_type(f, $as_in_range_t(t)))}
  $good_state(S) ==>
    $in_range_t(t, $rd(S, p, $as_field_with_type(f, $as_in_range_t(t)))));

// ----------------------------------------------------------------------------
// State updates
// ----------------------------------------------------------------------------

function {:inline true} $update(h:$object, r:$ptr, f:$field, v:int) : $object
  { h[ f := h[f][ r := v ] ] }

function {:inline true} $havoc_at(S0:$state, S:$state, p:$ptr, f:$field) : bool
  { $heap(S) == $update($heap(S0), p, f, $rd(S, p, f)) }

function {:inline true} $specials_eq(S0:$state, S:$state) : bool
  { 
    $f_timestamp(S0) == $f_timestamp(S) &&
    $f_closed(S0) == $f_closed(S) &&
    $f_owner(S0) == $f_owner(S) &&
    $roots(S0) == $roots(S)
  }


function {:inline true} $meta_eq(s1:$state, s2:$state) : bool
  { $specials_eq(s1, s2) }

function {:inline true} $mutable_increases(s1:$state, s2:$state) : bool
  { (forall p:$ptr :: {$owner(s2, p)} {$closed(s2, p)} {$root(s2, p)}
        $mutable(s1, p) ==> $mutable(s2, p)) }

procedure $write_int(f:$field, p:$ptr, v:int);
  modifies $s;
  ensures $specials_eq(old($s), $s);
  ensures $heap($s) == $update($heap(old($s)), p, f, v);
  ensures $timestamp_post_strict(old($s), $s);

procedure $write_int_local(h0:[$ptr]int, f:$field, p:$ptr, v:int) returns(h:[$ptr]int);
  modifies $s;
  ensures $specials_eq(old($s), $s);
  ensures $heap($s) == $update($heap(old($s)), p, f, v);
  ensures $timestamp_post_strict(old($s), $s);
  ensures h == h0[p := v];
  ensures old($heap($s))[f] == h0;
  ensures $heap($s)[f] == h;

function {:inline true} $rd_local(h:[$ptr]int, f:$field, p:$ptr) : int
  { h[p] }

function {:inline true} $rd_spec_ptr_local(h:[$ptr]int, f:$field, p:$ptr, t:$ctype) : $ptr
  { $spec_ptr_cast($int_to_ptr(h[p]), t) }

function {:inline true} $rd_phys_ptr_local(h:[$ptr]int, f:$field, p:$ptr, t:$ctype) : $ptr
  { $phys_ptr_cast($int_to_ptr(h[p]), t) }

procedure $write_ref_cnt(p:$ptr, v:int);
  modifies $s;
  ensures $specials_eq(old($s), $s);
  ensures $heap($s) == $update($heap(old($s)), p, $f_ref_cnt($typ(p)), v);
  ensures $timestamp_post_strict(old($s), $s);

function $updated_owns(S0:$state, S:$state, o:$ptr, owns:$ptrset) : bool
  { $heap(S) == $update($heap(S0), o, $f_owns($typ(o)), $ptrset_to_int(owns)) }

procedure $set_owns(p:$ptr, owns:$ptrset);
  // writes p
  modifies $s;
  // TOKEN: the owner is non-primitive
  requires $is_non_primitive($typ(p));
  // TOKEN: the owner is mutable
  requires $mutable($s, p);
  ensures $updated_owns(old($s), $s, p, owns);
  ensures $specials_eq(old($s), $s);
  ensures $timestamp_post_strict(old($s), $s);

// ----------------------------------------------------------------------------
// Timestamps/writes
// ----------------------------------------------------------------------------

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

function $writes_at(time:int) : $ptrset;

function {:inline true} $in_writes_at(time:int, p:$ptr) : bool
  { $in(p, $writes_at(time)) }

function {:inline true} $def_writes(S:$state, time:int, ptrs:$ptrset) : bool
  { $writes_at(time) == ptrs }

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

procedure $bump_timestamp();
  modifies $s;
  ensures
    $f_closed($s) == $f_closed(old($s)) &&
    $f_owner($s) == $f_owner(old($s)) &&
    $roots($s) == $roots(old($s)) &&
    $heap($s) == $heap(old($s));
  // I'm not sure if this is neccessary
  ensures $f_timestamp($s) == $f_timestamp(old($s))[$null := $current_timestamp($s)];
  ensures $current_timestamp(old($s)) < $current_timestamp($s);

function {:inline true} $is_fresh(M1:$state, M2:$state, p:$ptr) : bool
  { $current_timestamp(M1) < $timestamp(M2, p) && $timestamp(M2, p) <= $current_timestamp(M2) }

function {:inline true} $writable(S:$state, begin_time:int, p:$ptr) : bool
  { $is_non_primitive($typ(p)) && $is_proper(p) &&
    ($mutable(S, p) && ($timestamp(S, p) >= begin_time || $in_writes_at(begin_time, p))) }

function {:inline true} $writable_prim(S:$state, begin_time:int, p:$ptr) : bool
  { $is_primitive($typ(p)) && $is_proper(p) &&
    ($mutable(S, $emb0(p)) && ($timestamp(S, $emb0(p)) >= begin_time || $in_writes_at(begin_time, p))) }

function {:inline true} $listed_in_writes(S:$state, begin_time:int, p:$ptr) : bool
  { $in_writes_at(begin_time, p) }

function {:inline true} $top_writable(S:$state, begin_time:int, p:$ptr) : bool
  { if $is_primitive($typ(p)) then
      $writable_prim(S, begin_time, p)
    else
      ($owner(S, p) == $me() && ($timestamp(S, p) >= begin_time || $in_writes_at(begin_time, p))) }

function {:inline true} $not_written(S0:$state, p:$ptr, W:$ptrset) : bool
  { $owner(S0, $root(S0, p)) == $me() && !$in($root(S0, p), W) }
// TODO: { if $closed(S0, p) then $in($root(S0, p), W) else $in(p, W) }

function {:inline false} $modifies(S0:$state, S1:$state, W:$ptrset) : bool
  { (forall p:$ptr :: {$root(S1, p)} $not_written(S0, p, W) ==> $root(S0, p) == $root(S1, p)) &&
    (forall p:$ptr, f:$field :: {$rdtrig(S1, p, f)} $not_written(S0, p, W) && !$in($dot(p, f), W) ==> 
           ($is_sequential_field(f) || !$closed(S0, p) || $is_sequential_field(f) ==> $rd(S0, p, f) == $rd(S1, p, f))) &&

    (forall p:$ptr :: {$f_timestamp(S1)[p]} 
      ($not_written(S0, p, W) ==>
        $f_timestamp(S1)[p] == $f_timestamp(S0)[p]) &&
      ($f_timestamp(S1)[p] >= $f_timestamp(S0)[p])) &&
    (forall p:$ptr :: {$f_closed(S1)[p]} $not_written(S0, p, W) ==> $f_closed(S1)[p] == $f_closed(S0)[p]) &&
    (forall p:$ptr :: {$f_owner(S1)[p]} $not_written(S0, p, W) ==> $f_owner(S0)[p] == $f_owner(S1)[p]) &&
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
  { $modifies(S0, S1, $set_empty()) }
  //  $preserves_thread_local(S0, S1) }

// --------------------------------------------------------------------------------
// Frame axiom ordering
// --------------------------------------------------------------------------------

function $frame_level($pure_function) : int;
const $current_frame_level : int;

// assumed at the beginning of all ``normal'' functions (i.e., not frame axiom read checks)
// the $state is there only as a placeholder
function {:inline true} $can_use_all_frame_axioms(s:$state) returns(bool)
  { (forall f:$pure_function :: {$frame_level(f)} $frame_level(f) < $current_frame_level) }

function {:inline true} $can_use_frame_axiom_of(f:$pure_function) returns(bool)
  { $frame_level(f) < $current_frame_level }


// reads checking

function $reads_check_pre(s:$state) returns(bool);
function $reads_check_post(s:$state) returns(bool);
procedure $reads_havoc();
  modifies $s;
  // TOKEN: called nothing before reads_havoc()
  requires $reads_check_pre($s);
  ensures $reads_check_post($s);
  ensures $call_transition(old($s), $s);


// ----------------------------------------------------------------------------
// Allocation
// ----------------------------------------------------------------------------

function $is_in_stackframe(#sf:int, p:$ptr) : bool;

function {:inline true} $is_allocated0(S0:$state, S:$state, r:$ptr, t:$ctype) : bool
{    true
  && $is(r, t)
  && $is_proper(r)
  && $writes_nothing(S0, S)
  && $heap(S) == $heap(S0)
  && $timestamp_post_strict(S0, S)
  && $owner(S0, r) != $me()
  && $is_malloc_root(S, r)
  && $field(r) == $f_root(t)
}

function {:inline true} $is_allocated(S0:$state, S:$state, r:$ptr, t:$ctype) : bool
{    $is_allocated0(S0, S, r, t)
  &&
    if $is_primitive(t) then
      (   $mutable(S, $emb0(r))
       && $timestamp_is_now(S, $emb0(r)))
    else
      (    $extent_mutable(S, r)
        && $extent_is_fresh(S0, S, r)
        && $first_option_typed(S, r))
}

// just tmp
function {:inline true} $is_malloc_root(S:$state, p:$ptr) : bool
  { $is_object_root(S, p) }
function {:inline true} $is_object_root(S:$state, p:$ptr) : bool
  { $is_object_root_ptr(p) }
function $is_object_root_ptr(p:$ptr) : bool;

procedure $stack_alloc(t:$ctype, sf:int, spec:bool) returns (r:$ptr);
  modifies $s;
  ensures $is_allocated(old($s), $s, r, t);
  ensures $is_in_stackframe(sf, r);
  ensures spec ==> $in_range_spec_ptr(r);
  ensures !spec ==> $in_range_phys_ptr(r);

procedure $stack_free(sf:int, x:$ptr);
  modifies $s;
  // TOKEN: the extent of the object being reclaimed is mutable
  requires if $is_primitive($typ(x)) then $mutable($s, x) else $extent_mutable($s, x);
  // TOKEN: the pointer being reclaimed was returned by stack_alloc()
  requires $is_in_stackframe(sf, x);

  ensures if $is_primitive($typ(x)) then $modifies(old($s), $s, $set_singleton($emb0(x))) else $modifies(old($s), $s, $extent(old($s), x));
  ensures $heap($s) == $heap(old($s));

procedure $spec_alloc(t:$ctype) returns(r:$ptr);
  modifies $s;
  ensures $is_allocated(old($s), $s, r, t);
  ensures $in_range_spec_ptr(r);

procedure $spec_alloc_array(t:$ctype, sz:int) returns(r:$ptr);
  modifies $s;
  ensures $is_allocated(old($s), $s, r, $array(t, sz));
  ensures $in_range_spec_ptr(r);

procedure $alloc(t:$ctype) returns(r:$ptr);
  modifies $s;
  ensures $non_null(r) ==>
            $is_allocated(old($s), $s, r, t) && 
            $in_range_phys_ptr(r) && 
            $is_malloc_root($s, r);
  ensures $is_null(r) ==>
            $is(r, t) &&
            $writes_nothing(old($s), $s) && $heap(old($s)) == $heap($s);

procedure $free(x:$ptr);
  // writes extent(x)
  modifies $s;
  // TOKEN: the extent of the object being reclaimed is mutable
  requires $extent_mutable($s, x);
  // TOKEN: the pointer being reclaimed was returned by malloc()
  requires $is_malloc_root($s, x);

  ensures $modifies(old($s), $s, $extent(old($s), x));
  ensures $heap($s) == $heap(old($s));

// ----------------------------------------------------------------------------
// Wrap/unwrap
// ----------------------------------------------------------------------------

// These are only for triggering and consistency checking; they have no definitions
function $pre_wrap($state) : bool;
function $pre_unwrap($state) : bool;
function $pre_static_wrap($state) : bool;
function $pre_static_unwrap($state) : bool;
function $post_unwrap(S1:$state, S2:$state) : bool;

function $is_unwrapped_dynamic(S0:$state, S:$state, o:$ptr) : bool
{
     $is_unwrapped(S0, S, o)
  && $f_timestamp(S) == 
       (lambda r:$ptr :: if $owner(S0, r) == o || r == o then $current_timestamp(S) else $f_timestamp(S0)[r])
  && $f_owner(S) == 
       (lambda r:$ptr :: if $owner(S0, r) == o then $me() else $f_owner(S0)[r])
}

function $is_unwrapped(S0:$state, S:$state, o:$ptr) : bool
{
     true
  && $mutable(S, o)
  && $heap(S) == $heap(S0)
  && $owns(S0, o) == $owns(S, o)
  && (forall p:$ptr :: {$root(S, p)}
        ($root(S0, p) != o && $root(S0, p) == $root(S, p)) ||
        ($root(S0, p) == o && ($root(S, p) == p || $owner(S0, p) != o)))
  && $f_closed(S) == $f_closed(S0)[o := false]
  && $timestamp_post_strict(S0, S)
  && $post_unwrap(S0, S)
  && $typemap($f_owner(S0)) == $typemap($f_owner(S))
   /*
  &&
  (forall p:$ptr :: {$set_in_pos(p, $owns(S0, o))}
    $set_in(p, $owns(S0, o)) ==>
      $wrapped(S, p, $typ(p)) && $timestamp_is_now(S, p))
  // $preserves_thread_local(S0, S)
*/
}

function $is_wrapped_dynamic(S0:$state, S:$state, o:$ptr) : bool
{
     $is_wrapped(S0, S, o, $owns(S0, o))
  && $heap(S) == $heap(S0)
  && $owns(S0, o) == $owns(S, o)
  && $f_owner(S) == 
       (lambda r:$ptr :: if $set_in(r, $owns(S0, o)) then o else $f_owner(S0)[r])
}

function $is_wrapped(S0:$state, S:$state, o:$ptr, owns:$ptrset) : bool
{
     true
  && $f_closed(S) == $f_closed(S0)[o := true]
  && $f_timestamp(S) == $f_timestamp(S0)[o := $current_timestamp(S)]
  && (forall p:$ptr :: {$root(S, p)}
        $root(S, p) == $root(S0, p) ||
        ($root(S, p) == o && (p == o || $set_in($root(S0, p), owns))))
  && $wrapped(S, o, $typ(o))
  && ($is_claimable($typ(o)) ==> $ref_cnt(S0, o) == 0 && $ref_cnt(S, o) == 0)
  && $timestamp_post_strict(S0, S)
  && $typemap($f_owner(S0)) == $typemap($f_owner(S))

/*
  $timestamp_is_now(S, o) &&
  (forall p:$ptr :: {$set_in_pos(p, $owns(S0, o))}
    $set_in(p, $owns(S0, o)) ==> $owner(S, p) == o) &&
  // $set_in(o, $domain(S, o))
  // $preserves_thread_local(S0, S)
*/
}

procedure $unwrap(o:$ptr, T:$ctype);
  modifies $s;
  // TOKEN: the object has no outstanding claims
  requires ! $is_claimable(T) || $ref_cnt($s, o) == 0;
  // TOKEN: OOPS: pre_unwrap holds
  requires $pre_unwrap($s);

  ensures $is_unwrapped_dynamic(old($s), $s, o);

procedure $wrap(o:$ptr, T:$ctype);
  // writes o, $owns($s, o)
  modifies $s;
  // TOKEN: OOPS: pre_wrap holds
  requires $pre_wrap($s);
  // TOKEN: the wrapped type is non primitive
  requires $is_non_primitive($typ(o));
  // TOKEN: the object being wrapped is mutable
  requires $mutable($s, o);
  // TOKEN: everything in the owns set is wrapped
  requires (forall p:$ptr :: {$dont_instantiate(p)} $set_in0(p, $owns($s, o)) ==> $wrapped($s, p, $typ(p)));

  ensures $is_wrapped_dynamic(old($s), $s, o);

///////

function $take_over(S:$state, l:$ptr, o:$ptr) : $state;
function $release(S0:$state, S:$state, #l:$ptr, #p:$ptr) : $state;

axiom (forall S:$state, l:$ptr, p:$ptr :: {$take_over(S, l, p)}
  $f_owner($take_over(S, l, p)) == $f_owner(S)[p := l]);

axiom (forall S0,S:$state, l:$ptr, p:$ptr :: {$release(S0, S, l, p)}
  $f_owner($release(S0, S, l, p)) == $f_owner(S)[p := $me()] &&
  $f_timestamp($release(S0, S, l, p)) == $f_timestamp(S)[p := $current_timestamp(S0)]
  );

procedure $static_unwrap(o:$ptr, S:$state);
  modifies $s;
  // TOKEN: the object has no outstanding claims
  requires ! $is_claimable($typ(o)) || $ref_cnt($s, o) == 0;
  // TOKEN: OOPS: pre_static_unwrap holds
  requires $pre_static_unwrap($s);

  ensures $is_unwrapped(old($s), $s, o);
  ensures $f_owner($s) == $f_owner(S);
  ensures $f_timestamp($s) == $f_timestamp(S)[o := $current_timestamp(S)];


procedure $static_wrap(o:$ptr, S:$state, owns:$ptrset);
  modifies $s;
  // TOKEN: OOPS: pre_static_wrap must hold
  requires $pre_static_wrap($s);
  // TOKEN: the wrapped type is non primitive
  requires $is_non_primitive($typ(o));
  // TOKEN: the object being wrapped is mutable
  requires $mutable($s, o);

  ensures $is_wrapped(old($s), $s, o, owns);
  ensures $heap($s) == $update($heap(old($s)), o, $f_owns($typ(o)), $ptrset_to_int(owns));
  ensures $f_owner($s) == $f_owner(S);


procedure $static_wrap_non_owns(o:$ptr, S:$state);
  modifies $s;
  // TOKEN: OOPS: pre_static_wrap must hold
  requires $pre_static_wrap($s);
  // TOKEN: the wrapped type is non primitive
  requires $is_non_primitive($typ(o));
  // TOKEN: the object being wrapped is mutable
  requires $mutable($s, o);

  ensures $is_wrapped(old($s), $s, o, $owns(old($s), o));
  ensures $heap($s) == $heap(old($s));
  ensures $f_owner($s) == $f_owner(S);

// ----------------------------------------------------------------------------
// Admissibility & unwrap checks
// ----------------------------------------------------------------------------

function $spans_the_same(S1:$state, S2:$state, p:$ptr, t:$ctype) : bool
  { $owns(S1, p) == $owns(S2, p) &&
    (forall f:$field :: {$rdtrig(S2, p, f)}
      $is_proper($dot(p, f)) && f != $f_ref_cnt(t) ==> $rd(S1, p, f) == $rd(S2, p, f)) }

function $nonvolatile_spans_the_same(S1:$state, S2:$state, p:$ptr, t:$ctype) : bool
  { (forall f:$field :: {$rdtrig(S2, p, f)}
      // ref_cnt is always volatile
      $is_proper($dot(p, f)) && $is_sequential_field(f)
        ==> $rd(S1, p, f) == $rd(S2, p, f)) }

function $good_for_admissibility(S:$state) : bool;
function $good_for_post_admissibility(S:$state) : bool;
function $admissibility_start(p:$ptr, t:$ctype) : bool
  { $is(p, t) && $is_proper(p) }

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
  ensures 
    if $is_stuttering_check() then 
      $nonvolatile_spans_the_same(old($s), $s, p, t)
    else 
      $owns(old($s), p) == $owns($s, p) &&
      $spans_the_same(old($s), $s, p, t);
  ensures $closed($s, p);
  ensures $closed_is_transitive($s);
  ensures $good_state($s);
  ensures $good_for_post_admissibility($s);

  ensures (forall q: $ptr :: {$closed($s, q)} {$closed(old($s), q)}
    $closed(old($s), q) || $closed($s, q) ==>
      ($spans_the_same(old($s), $s, q, $typ(q)) && $closed(old($s), q) == $closed($s, q)) || 
      ($inv2(old($s), $s, q, $typ(q)) && $nonvolatile_spans_the_same(old($s), $s, q, $typ(q))));
  ensures (forall q:$ptr ::  {$set_in_pos(q, $owns(old($s), p))}
            $set_in(q, $owns(old($s), p)) ==>
              $ref_cnt(old($s), q) == $ref_cnt($s, q));
  ensures $timestamp_post(old($s), $s);

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


// --------------------------------------------------------------------------------
// Claims
// --------------------------------------------------------------------------------

function $claims_obj(claim:$ptr, obj:$ptr) : bool;
function $valid_claim(S:$state, claim:$ptr) : bool;

axiom (forall S:$state, c:$ptr :: {$full_stop(S), $valid_claim(S, c)}
  $full_stop(S) && $closed(S, c) ==> $valid_claim(S, c));

axiom (forall S:$state, c:$ptr :: {$valid_claim(S, c)}
  $valid_claim(S, c) ==> $closed(S, c) && $invok_state(S));

function {:inline true} $claim_initial_assumptions(#s1:$state, c:$ptr, tok:$token) : bool
  { $good_state_ext(tok, #s1) &&
    $closed_is_transitive(#s1) &&
    true
  }

function {:inline true} $inv2_when_closed(#s1:$state, #s2:$state, #p:$ptr, typ:$ctype) returns (bool)
  { (!$closed(#s1, #p) && !$closed(#s2, #p)) || ($inv2(#s1, #s2, #p, typ) && $nonvolatile_spans_the_same(#s1, #s2, #p, typ)) }

function {:inline true} $claim_transitivity_assumptions(#s1:$state, #s2:$state, c:$ptr, tok:$token) : bool
  { $full_stop_ext(tok, #s1) &&
    $good_state_ext(tok, #s2) &&
    $closed_is_transitive(#s1) &&
    $closed_is_transitive(#s2) &&
    (forall #p:$ptr :: {$closed(#s1,#p)} {$closed(#s2,#p)} $inv2_when_closed(#s1,#s2,#p,$typ(#p))) &&
    $valid_claim(#s1, c) &&
    $closed(#s2, c) &&
    true
    }

function {:inline true} $valid_claim_impl(S0:$state, S1:$state) : bool
  { (forall r:$ptr, f:$field :: {$closed(S1, $ptr($as_field_with_type(f, ^^claim), r))}
       $is($ptr(f, r), ^^claim) ==> 
       $closed(S0, $ptr(f, r)) && $closed(S1, $ptr(f, r)) ==> $valid_claim(S1, $ptr(f, r))) }

function $claims_claim(c1:$ptr, c2:$ptr) : bool;
axiom (forall c1:$ptr, c2:$ptr :: {$claims_claim(c1, c2)}
  $is(c1, ^^claim) && $is(c2, ^^claim) &&
  (forall S:$state :: $valid_claim(S, c1) ==> $closed(S, c2)) 
  ==>
  $claims_claim(c1, c2));

axiom (forall S:$state, c1:$ptr, c2:$ptr :: {$valid_claim(S, c1), $claims_claim(c1, c2)}
  $valid_claim(S, c1) && $claims_claim(c1, c2) ==> $valid_claim(S, c2));

axiom (forall S:$state, c:$ptr, o:$ptr ::
    {$closed(S, c), $claims_obj(c, o)}
    $good_state(S) ==>
      $claims_obj(c, o) && $closed(S, c) ==> $instantiate_ptrset($owns(S, o)) && $closed(S, o) && $ref_cnt(S, o) > 0);

axiom (forall S:$state, c:$ptr, o:$ptr ::
    {$valid_claim(S, c), $claims_obj(c, o)}
    $valid_claim(S, c) && $claims_obj(c, o) ==> $inv(S, o, $typ(o)));

axiom (forall S:$state, c:$ptr, r:$ptr, f:$field ::
    {$valid_claim(S, c), $claims_obj(c, $ptr($as_field_with_type(f, ^^claim), r))}
    $is($ptr(f, r), ^^claim) ==>
    $valid_claim(S, c) && $claims_obj(c, $ptr(f, r)) ==>
      $valid_claim(S, $ptr(f, r)));

function {:weight 0} $not_shared(S:$state, p:$ptr) returns(bool)
  { $wrapped(S, p, $typ(p)) && (!$is_claimable($typ(p)) || $ref_cnt(S, p) == 0) }

function {:weight 0} $claimed_closed(s:$state, p:$ptr) returns(bool)
  { $closed(s, p) }

axiom (forall S:$state, p:$ptr :: {$invok_state(S), $claimed_closed(S, p)}
  $invok_state(S) && $claimed_closed(S, p) ==> $inv(S, p, $typ(p)));

// called at the beginning of an atomic block to simulate other threads
procedure $atomic_havoc();
  modifies $s;
  ensures $writes_nothing(old($s), $s);

  ensures (forall p:$ptr, f:$field ::
    {$not_shared(old($s), p), $rdtrig($s, p, f)}
    $not_shared(old($s), p) ==> $rd($s, p, f) == $rd(old($s), p, f));
  ensures $timestamp_post_strict(old($s), $s);

const unique $no_claim : $ptr;
axiom $no_claim == $spec_ptr_cast($null, ^^claim);

procedure $alloc_claim() returns(r:$ptr);
  modifies $s;
  ensures $is_allocated0(old($s), $s, r, ^^claim);
  ensures $timestamp_is_now($s, r);
  ensures $wrapped($s, r, ^^claim);
  ensures $in_range_spec_ptr(r);
  ensures $owns($s, r) == $set_empty();
  ensures $ref_cnt($s, r) == 0;
  ensures r != $no_claim;

function {:inline true} $claim_killed(S0:$state, S:$state, c:$ptr) : bool
{
  $f_closed(S) == $f_closed(S0)[ c := false ] &&
  $f_timestamp(S) == $f_timestamp(S0) &&
  $f_owner(S) == $f_owner(S0) &&
  $heap(S) == $heap(S0) &&
  $good_state(S)
}

// FIXME should it havoc non thread local state?
procedure $unclaim(c:$ptr);
  modifies $s;
  // TOKEN: the claim is wrapped
  requires $wrapped($s, c, ^^claim);
  // TOKEN: the claim has no outstanding references
  requires $ref_cnt($s, c) == 0;
  ensures $claim_killed(old($s), $s, c);

procedure $kill_claim(c:$ptr);
  modifies $s;
  ensures $claim_killed(old($s), $s, c);

function $claims_upgrade(the_new:$ptr, the_old:$ptr) : bool
  { (forall o:$ptr :: $claims_obj(the_old, o) ==> $claims_obj(the_new, o)) }

function $account_claim(S:$state, c:$ptr, o:$ptr) : bool
  { $good_state(S) && $closed(S, c) && $claims_obj(c, o) }

function $claim_no(S:$state, o:$ptr, idx:int) : $ptr;
function $claim_idx(o:$ptr, c:$ptr) : int;

axiom (forall S:$state, c:$ptr, o:$ptr :: {$account_claim(S, c, o)}
  $account_claim(S, c, o) ==>
    $claim_no(S, o, $claim_idx(o, c)) == c &&
    0 <= $claim_idx(o, c) && $claim_idx(o, c) < $ref_cnt(S, o));
    
// --------------------------------------------------------------------------------
// Ownership transfers with closed objects
// --------------------------------------------------------------------------------

procedure $set_closed_owner(#p:$ptr, owner:$ptr);
  // writes #p, owner
  modifies $s;
  // TOKEN: the owner is composite
  requires $is_non_primitive($typ(owner));
  // TOKEN: the object is non-primitive
  requires $is_non_primitive($typ(#p));
  // TOKEN: the object is owned by the current thread
  requires $owner($s, #p) == $me();
  // TOKEN: the object is closed
  requires $closed($s, #p);
  // TOKEN: the owner is closed
  requires $closed($s, owner);
  // TOKEN: the owner has volatile owns set
  requires $is_volatile_field($f_owns($typ(owner)));

  ensures $f_closed($s) == $f_closed(old($s));
  ensures $f_timestamp($s) == $f_timestamp(old($s));
  ensures $f_owner($s) == $f_owner(old($s))[ #p := owner ];
  ensures $roots($s) == (lambda q:$ptr :: if $root(old($s), q) == #p then $root($s, q) else $root(old($s), q));
  ensures $updated_owns(old($s), $s, owner, $set_union($set_singleton(#p), $owns(old($s), owner)));
  ensures $set_in(#p, $owns($s, owner));

function {:inline true} $new_ownees(S:$state, o:$ptr, owns:$ptrset) returns($ptrset)
  { $set_difference(owns, $owns(S, o)) }

procedure $set_closed_owns(owner:$ptr, owns:$ptrset);
  // writes owner, $new_ownees(owner, owns)
  modifies $s;
  // TOKEN: the owner is composite
  requires $is_non_primitive($typ(owner));
  // TOKEN: all newly owned objects are wrapped
  requires (forall p:$ptr :: {$dont_instantiate(p)} {sk_hack($set_in0(p, $owns($s, owner)))}
    $set_in(p, $new_ownees($s, owner, owns)) ==> $wrapped($s, p, $typ(p)));
  // TOKEN: the owner is closed
  requires $closed($s, owner);
  // TOKEN: the owner has volatile owns set
  requires $is_volatile_field($f_owns($typ(owner)));

  ensures $f_closed($s) == $f_closed(old($s));
  ensures $f_timestamp($s) == $f_timestamp(old($s));
  ensures $f_owner($s) == (lambda q:$ptr :: if $in(q, owns) then owner else $f_owner(old($s))[q]);
  ensures $roots($s) == (lambda q:$ptr :: if $in($root(old($s), q), owns) then $root($s, q) else $root(old($s), q));
  ensures $updated_owns(old($s), $s, owner, owns);

procedure $giveup_closed_owner(#p:$ptr, owner:$ptr);
  // writes owner
  modifies $s;
  // TOKEN: the owner is composite
  requires $is_non_primitive($typ(owner));
  // TOKEN: the object is owned by the owner
  requires $set_in(#p, $owns($s, owner));
  // TOKEN: the owner is closed
  requires $closed($s, owner);
  // TOKEN: the owner has volatile owns set
  requires $is_volatile_field($f_owns($typ(owner)));

  ensures $f_closed($s) == $f_closed(old($s));
  ensures $f_timestamp($s) == $f_timestamp(old($s));
  ensures $f_owner($s) == $f_owner(old($s))[ #p := $me() ];
  ensures $roots($s) == (lambda q:$ptr :: if $root(old($s), q) == $root(old($s), owner) then $root($s, q) else $root(old($s), q));
  ensures $updated_owns(old($s), $s, owner, $set_difference($owns(old($s), owner), $set_singleton(#p)));

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

axiom (forall S:$state, p:$ptr, q:$ptr :: {$in_domain(S, p, q)}
  $in_domain(S, p, q) ==> 
    $root(S, p) == q &&
    $wrapped(S, q, $typ(q)) &&
    $closed(S, p) &&
    $set_in(p, $domain(S, q)) &&
    $inv(S, p, $typ(p)) && 
    $set_in0(p, $owns(S, $owner(S, p))));

axiom (forall S:$state, p:$ptr, q:$ptr :: {$in_domain(S, p, q)}
  $full_stop(S) && $set_in(p, $domain(S, q)) && $wrapped(S, q, $typ(q)) ==> $in_domain(S, p, q));

axiom (forall S:$state, q,r:$ptr :: { $in_domain(S, r, $root(S, q)) }
     $in_domain(S, q, $root(S, q)) && 
     $is_sequential_field($f_owns($typ(q))) &&
     $set_in0(r, $owns(S, q)) ==>
        $owner(S, r) == q && 
        $root(S, r) == $root(S, q) && 
        $in_domain(S, r, $root(S, q)));

type $version;
function $ver_domain($version) : $ptrset;
function $int_to_version(int) : $version;
function {:inline true} $read_version(S:$state, p:$ptr) : $version
  { $int_to_version($f_timestamp(S)[p]) }
function {:inline true} $domain(S:$state, p:$ptr) : $ptrset
  { $ver_domain($read_version(S, p)) }

axiom (forall S:$state, p:$ptr, q:$ptr, r:$ptr :: 
  { $set_in(q, $domain(S, p)), $in_vdomain(S, r, p) }
  $is_volatile_field($f_owns($typ(q))) && 
  $set_in(q, $domain(S, p)) &&
  (forall S1:$state :: 
      $inv(S1, q, $typ(q)) && 
      $read_version(S1, p) == $read_version(S, p) &&
      $domain(S1, p) == $domain(S, p)
      ==> $set_in0(r, $owns(S1, q)))
    ==> $in_vdomain(S, r, p) && $set_in0(r, $owns(S, q)));

axiom (forall S:$state, p:$ptr, q:$ptr :: 
  { $in_vdomain(S, p, q) } $in_vdomain(S, p, q) ==> $in_domain(S, p, q));

function $fetch_from_domain(v:$version, p:$ptr, f:$field) : int;

axiom (forall S:$state, p:$ptr, d:$ptr, f:$field :: // PERF 17.5%
  { $rdtrig(S, p, f), $set_in(p, $domain(S, d)), $is_sequential_field(f) }
  $set_in(p, $domain(S, d)) && $is_sequential_field(f) ==>
    $rd(S, p, f) == $fetch_from_domain($read_version(S, d), p, f));

/*
axiom (forall p:$ptr, S1:$state, S2:$state, q:$ptr :: 
  {:weight 0} {$set_in(q, $domain(S1, p)), $call_transition(S1, S2)}
    $instantiate_bool($set_in(q, $domain(S2, p))));
    
axiom (forall p:$ptr, S1:$state, S2:$state, q:$ptr :: 
  {:weight 0} {$set_in(q, $ver_domain($read_version(S1, p))), $call_transition(S1, S2)}
    $instantiate_bool($set_in(q, $ver_domain($read_version(S2, p)))));
*/

function $in_claim_domain(p:$ptr, c:$ptr) : bool;
axiom (forall p:$ptr, c:$ptr :: {$in_claim_domain(p, c)}
  (forall s:$state :: {$dont_instantiate_state(s)} $valid_claim(s, c) ==> $closed(s, p)) ==>
    $in_claim_domain(p, c));

function $by_claim(S:$state, c:$ptr, obj:$ptr, ptr:$ptr) : $ptr
  { ptr }

function $claim_version($ptr) : $version;

axiom (forall S:$state, p:$ptr, c:$ptr, f:$field :: 
  {$in_claim_domain(p, c), $rdtrig(S, p, f)}
  {$by_claim(S, c, p, $dot(p, f))}
  $good_state(S) &&
  $closed(S, c) && $in_claim_domain(p, c) && $is_sequential_field(f) ==>
    $in_claim_domain(p, c) &&
    $rd(S, p, f) == $fetch_from_domain($claim_version(c), p, f)
    );

// -------------------------------------------------------------------------------------
// the volatile domain
// -------------------------------------------------------------------------------------

type $vol_version;

function $int_to_vol_version(int) : $vol_version;

function {:inline true} $volatile_version_addr(p:$ptr) : $ptr { $dot(p, $f_vol_version($typ(p))) }

function $read_vol_version(S:$state, p:$ptr) : $vol_version
  { $int_to_vol_version($rd(S, p, $f_vol_version($typ(p)))) }

function $fetch_from_vv(v:$vol_version, p:$ptr, f:$field) : int;

function {:inline true} $fetch_vol_field(S:$state, p:$ptr, f:$field) : int
  { $fetch_from_vv($read_vol_version(S, p), p, f) }

// the approver always needs to approve itself and be of obj_t type
// TODO drop "t" from both
function $is_approved_by(t:$ctype, approver:$field, subject:$field) : bool;
function $is_owner_approved(t:$ctype, subject:$field) returns(bool);

axiom (forall S:$state, t:$ctype, p:$ptr, approver:$field, subject:$field ::
  { $is_approved_by(t, approver, subject), $rd(S, p, subject) }
  $full_stop(S) &&
  $is_approved_by(t, approver, subject) &&
  $closed(S, p) &&
  ($int_to_ptr($rd(S, p, approver)) == $me() ||
   $int_to_ptr($fetch_vol_field(S, p, approver)) == $me()) ==>
    $rd(S, p, subject) == $fetch_vol_field(S, p, subject)
    );

function {:inline true} $inv_is_approved_by_ptr(S1:$state, S2:$state, this:$ptr, approver:$ptr, subject:$field) returns(bool)
{
  $rd(S1, this, subject) == $rd(S2, this, subject) ||
  $is_null(approver) ||
  (!$is_threadtype($typ(approver)) && $inv2nt(S1, S2, approver) ) ||
  ($is_threadtype($typ(approver)) && $read_vol_version(S1, this) != $read_vol_version(S2, this) )
}

// poor-man's version of approvals (used in some testcases)
function $depends(S1:$state, S2:$state, dependant:$ptr, this:$ptr) : bool
  { $spans_the_same(S1, S2, this, $typ(this)) || 
    $inv2_when_closed(S1, S2, dependant, $typ(dependant)) ||
    $is_threadtype($typ(dependant)) }

function {:inline true} $inv_is_approved_by(S1:$state, S2:$state, this:$ptr, approver:$field, subject:$field) returns(bool)
{
  $inv_is_approved_by_ptr(S1, S2, this, $int_to_ptr($rd(S1, this, approver)), subject)
}

axiom (forall S:$state, p:$ptr, t:$ctype, subject:$field ::
  { $is_owner_approved(t, subject), $rd(S, p, subject) }
  $full_stop(S) &&
  $closed(S, p) &&
  $is_owner_approved(t, subject) &&
  $owner(S, p) == $me() ==>
    $rd(S, p, subject) == $fetch_vol_field(S, p, subject));

/*
axiom (forall S1:$state, S2:$state, p:$ptr, t:$ctype, subject:$field ::
  { $is_owner_approved(t, subject), $post_unwrap(S1, S2), $rd(S1, p, subject) }
  $instantiate_int($rd(S2, p, subject)));
*/

function {:inline true} $inv_is_owner_approved(S1:$state, S2:$state, this:$ptr, subject:$field) returns(bool)
{
  $inv_is_approved_by_ptr(S1, S2, this, $owner(S1, this), subject)
}

procedure $bump_volatile_version(p:$ptr);
  modifies $s;
  ensures $specials_eq(old($s), $s);
  ensures $havoc_at(old($s), $s, p, $f_vol_version($typ(p)));
  ensures $timestamp_post_strict(old($s), $s);
  ensures $read_vol_version(old($s), p) != $read_vol_version($s, p);
  ensures $timestamp_post_strict(old($s), $s);

// -----------------------------------------------------------------------
// Span & extent
// -----------------------------------------------------------------------

function $composite_extent(S:$state, r:$ptr, t:$ctype) : $ptrset;

function $extent(S:$state, r:$ptr) : $ptrset
  { (lambda p:$ptr :: $is_proper(p) && $composite_extent(S, r, $typ(r))[$emb0(p)]) }

const $full_extent_state : $state;
function $full_extent(r:$ptr) : $ptrset
  { (lambda p:$ptr :: $is_proper(p) && $composite_extent($full_extent_state, r, $typ(r))[$emb0(p)]) }

function $span(o:$ptr) : $ptrset
  { (lambda p:$ptr :: $is_proper(p) && $emb0(p) == o) }
function $first_option_typed(S:$state, #p:$ptr) : bool;

function {:inline true} $struct_extent(#p:$ptr) : $ptrset
  { $full_extent(#p) }

function $extent_mutable(S:$state, r:$ptr) : bool
  { $mutable(S, r) && 
    (forall p:$ptr :: {$extent_hint(p, r)} $in(p, $composite_extent(S, r, $typ(r))) ==> $mutable(S, p)) }
  
axiom (forall S:$state, T:$ctype, sz:int, p:$ptr :: {$extent_mutable(S, $as_ptr_with_type(p, $array(T, sz)))}
  $extent_mutable(S, $as_ptr_with_type(p, $array(T, sz))) ==> $is_mutable_array(S, $as_array_first_index(p), T, sz));

axiom (forall S:$state, T:$ctype, sz:int, p:$ptr :: {$mutable(S, $as_ptr_with_type(p, $array(T, sz)))}
  $in_range_phys_ptr(p) ==>
  $is_primitive(T) && $mutable(S, $as_ptr_with_type(p, $array(T, sz))) ==> $is_mutable_array(S, $as_array_first_index(p), T, sz));

function $extent_is_fresh(S0:$state, S:$state, r:$ptr) : bool
  { $timestamp_is_now(S, r) &&
    (forall p:$ptr :: {$extent_hint(p, r)} $in(p, $composite_extent(S, r, $typ(r))) ==> $timestamp_is_now(S, p)) }

function $volatile_span(S:$state, q:$ptr) : $ptrset
  { (lambda p:$ptr :: $is_proper(p) && $is_volatile_field($field(p)) && $emb0(p) == q) }

function $extent_hint(p:$ptr, q:$ptr) : bool;
axiom (forall p:$ptr, q:$ptr, r:$ptr :: {$extent_hint(p, q), $extent_hint(q, r)}
  $extent_hint(p, q) && $extent_hint(q, r) ==> $extent_hint(p, r));
axiom (forall p:$ptr, f:$field :: {$dot(p, $as_composite_field(f))}
  $extent_hint($dot(p, $as_composite_field(f)), p));
// axiom (forall p:$ptr :: {$typ(p)} !$is_primitive($typ(p)) ==> $extent_hint(p, p));

// ----------------------------------------------------------------------------
// Value structs
// ----------------------------------------------------------------------------

const $struct_zero : $struct;

axiom $good_state($vs_state($struct_zero));

function {:inline true} $vs_base(s:$struct, t:$ctype) : $ptr
  { $phys_ptr_cast($vs_base_ref(s), t) }
function $vs_base_ref($struct) : $ptr;

function $vs_state($struct) : $state;
axiom (forall s:$struct :: $good_state($vs_state(s)));

function $vs_ctor(S:$state, p:$ptr) : $struct;
axiom (forall S:$state, p:$ptr :: {$vs_ctor(S, p)}
  $good_state(S) ==>
    $phys_ptr_cast($vs_base_ref($vs_ctor(S, p)), $typ(p)) == p && $vs_state($vs_ctor(S, p)) == S);

axiom (forall f:$field, t:$ctype :: { $rdtrig($vs_state($struct_zero), $vs_base($struct_zero, t), f) }
  $rd($vs_state($struct_zero), $vs_base($struct_zero, t), f) == 0);

function {:inline true} $mem(s:$state, p:$ptr) : int
  { $rd(s, $base(p), $field(p)) }

function $update_int(S:$state, p:$ptr, v:int) returns ($state);
axiom (forall S:$state, p:$ptr, v:int :: {$update_int(S, p, v)}
  $specials_eq(S, $update_int(S, p, v)) &&
  $heap($update_int(S, p, v)) == $update($heap(S), $base(p), $field(p), v));

// typedness and writes check are handled by the assignment translation
procedure $havoc(o:$ptr, t:$ctype);
  modifies $s;
  requires $is(o, t);
  ensures $specials_eq(old($s), $s);
  ensures (forall p:$ptr, f:$field :: {$rdtrig($s, p, f)}  
    $composite_extent(old($s), o, t)[p] || $rd(old($s), p, f) == $rd($s, p, f));

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

function $is_record_field(parent:$ctype, field:$field, field_type:$ctype) : bool;

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
// Globals
// -----------------------------------------------------------------------

function $program_entry_point(s:$state) returns(bool);
function $program_entry_point_ch(s:$state) returns(bool);

axiom (forall S:$state :: {$program_entry_point(S)} $program_entry_point(S) ==> $program_entry_point_ch(S));

function {:inline true} $def_global(p:$ptr, t:$ctype) : bool
  { $is(p, t) &&
    $is_object_root_ptr(p) &&
    true }

function {:inline true} $is_global(p:$ptr, t:$ctype) returns(bool)
  { 
    (forall S:$state :: {$program_entry_point(S)} $program_entry_point(S) ==> 
      $extent_mutable(S, p) && $owns(S, p) == $set_empty())
  }

function {:inline true} $is_global_array(p:$ptr, T:$ctype, sz:int) returns(bool)
  { 
    (forall S:$state :: {$program_entry_point(S)} $program_entry_point(S) ==> 
      $is_mutable_array(S, p, T, sz))
  }

// -----------------------------------------------------------------------
// Sets of pointers
// -----------------------------------------------------------------------

type $ptrset = [$ptr]bool;

function {:inline true} $in(p:$ptr, s:$ptrset) : bool
  { s[p] } 

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
function $set_eq($ptrset, $ptrset) : bool;
axiom (forall #a: $ptrset, #b: $ptrset :: {$set_eq(#a,#b)}
  (forall #o: $ptr :: {$dont_instantiate(#o)} $set_in(#o, #a) <==> $set_in(#o, #b)) ==> $set_eq(#a, #b));
axiom (forall #a: $ptrset, #b: $ptrset :: {$set_eq(#a,#b)}
  $set_eq(#a, #b) ==> #a == #b);

function $set_cardinality($ptrset) : int;

axiom ($set_cardinality($set_empty()) == 0);
axiom (forall p:$ptr :: {$set_singleton(p)} $set_cardinality($set_singleton(p)) == 1);

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

function {:inline false} $set_in_pos(p:$ptr, s:$ptrset) : bool; // { $set_in(p, s) }
axiom (forall p:$ptr, s:$ptrset :: {$set_in(p, s)} $set_in(p, s) ==> $set_in_pos(p, s));

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


// --------------------------------------------------------------------------------
// Function pointers
// --------------------------------------------------------------------------------

function $get_fnptr(no:int, t:$ctype) : $ptr
  { $ptr($f_root(t), $get_fnptr_ref(no)) }

function $get_fnptr_ref(no:int) : $base;
function $get_fnptr_inv(rf:$base) : int;
axiom (forall no:int :: $get_fnptr_inv($get_fnptr_ref(no)) == no);

function $valid_fnptr($ptr) : bool;

axiom (forall no:int, t:$ctype :: {$get_fnptr(no, t)}
  $is_fnptr_type(t) ==> 
    $in_range_phys_ptr($get_fnptr(no, t)) && $valid_fnptr($get_fnptr(no, t)));

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
function {:inline true} $in_range_ptr(p:$ptr) : bool { $in_range_u8($addr(p)) }

function {:inline true} $in_range_div_i1(x:int, y:int) : bool { y != -1 || x != $min.i1 }
function {:inline true} $in_range_div_i2(x:int, y:int) : bool { y != -1 || x != $min.i2 }
function {:inline true} $in_range_div_i4(x:int, y:int) : bool { y != -1 || x != $min.i4 }
function {:inline true} $in_range_div_i8(x:int, y:int) : bool { y != -1 || x != $min.i8 }

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
function $op_mul(x:int, y:int) : int { x * y }

// extra function symbols for arithmetic operations to allow triggering and congruence closure on
// arithmetic expressions - use of these in the generated Boogie has to be requeste explicitly

function $op_add(t:$ctype, x:int, y:int) : int { x + y }
function $op_sub(t:$ctype, x:int, y:int) : int { x - y }
function $op_div(t:$ctype, x:int, y:int) : int { x / y }
function $op_lt(t:$ctype, x:int, y:int) : bool { x < y }
function $op_le(t:$ctype, x:int, y:int) : bool { x <= y }
function $op_gt(t:$ctype, x:int, y:int) : bool { x > y }
function $op_ge(t:$ctype, x:int, y:int) : bool { x >= y }

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

function $u8_to_ptr(x : int) : $ptr;
function $i8_to_ptr(x : int) : $ptr;
function $u4_to_ptr(x : int) : $ptr;
function $i4_to_ptr(x : int) : $ptr;
function $u2_to_ptr(x : int) : $ptr;
function $i2_to_ptr(x : int) : $ptr;
function $u1_to_ptr(x : int) : $ptr;
function $i1_to_ptr(x : int) : $ptr;

axiom (forall k:int :: { $u8_to_ptr(k) } $addr($u8_to_ptr(k)) == k);
axiom (forall k:int :: { $u4_to_ptr(k) } $addr($u4_to_ptr(k)) == k);
axiom (forall k:int :: { $u2_to_ptr(k) } $addr($u2_to_ptr(k)) == k);
axiom (forall k:int :: { $u1_to_ptr(k) } $addr($u1_to_ptr(k)) == k);

axiom (forall p:$ptr :: { $ptr_to_u8(p) } $in_range_u8($addr(p)) ==> $ptr_to_u8(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_i8(p) } $in_range_i8($addr(p)) ==> $ptr_to_i8(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_u4(p) } $in_range_u4($addr(p)) ==> $ptr_to_u4(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_i4(p) } $in_range_i4($addr(p)) ==> $ptr_to_i4(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_u2(p) } $in_range_u2($addr(p)) ==> $ptr_to_u2(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_i2(p) } $in_range_i2($addr(p)) ==> $ptr_to_i2(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_u1(p) } $in_range_u1($addr(p)) ==> $ptr_to_u1(p) == $addr(p));
axiom (forall p:$ptr :: { $ptr_to_i1(p) } $in_range_i1($addr(p)) ==> $ptr_to_i1(p) == $addr(p));

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

// ----------------------------------------------------------------------------
// VCC model-viewer support
// ----------------------------------------------------------------------------

function $local_value_is(S1:$state, pos:$token, local_name:$token, val:int, t:$ctype) : bool;
function $local_value_is_ptr(S1:$state, pos:$token, local_name:$token, val:$ptr, t:$ctype) : bool;
function $read_ptr_m(S:$state, p:$ptr, t:$ctype) : $ptr;

function $type_code_is(x:int, tp:$ctype) : bool;
// idx==0 - return type
function $function_arg_type(fnname:$pure_function, idx:int, tp:$ctype) : bool;

// That's all folks.
