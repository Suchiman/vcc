//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#ifndef _VCCP_H
#define _VCCP_H

/*** 
 *** Core annotation language
 ***/

#define block __block
#define assert __assert
#define assume __assume
#define axiom __axiom __specification a =
#define ensures __ensures
#define exists __exists
#define forall __forall
#define invariant __invariant
#define lambda __lambda
#define old(address) __old(address)
#define reads __reads
#define requires __requires
#define spec(...) __specification __VA_ARGS__
#define writes __writes
#define unchecked(...) __unchecked(__VA_ARGS__)
#define this __this

/*** 
 *** Types
 ***/

#define _concat_identifiers2(a,b) a ## b
#define _concat_identifiers(a,b) _concat_identifiers2(a,b)
#define SPEC_TYPE(name) typedef struct _concat_identifiers(_vcc_math_type_, name) {char _vcc_marker_for_math_type;} name;

typedef void *_vcc_obj_t;
#define obj_t _vcc_obj_t

typedef __int64 _vcc_integer_t;
#define mathint _vcc_integer_t

SPEC_TYPE(ptrset)
SPEC_TYPE(typeid_t)
SPEC_TYPE(thread_id_t)
SPEC_TYPE(state_t) // for in_state


typedef thread_id_t *_vcc_thread_id;
#define thread_id _vcc_thread_id

typedef unsigned __int64 _vcc_size_t;

typedef _Bool bool;
#define false ((bool)0)
#define true ((bool)1)

/*** 
 *** Specification predicates and functions
 ***/

ptrset _vcc_array_range(obj_t , _vcc_size_t);
#define array_range(...) _vcc_array_range(__VA_ARGS__)
ptrset _vcc_array_members(obj_t , _vcc_size_t);
#define array_members(...) _vcc_array_members(__VA_ARGS__)
obj_t _vcc_as_array(obj_t , _vcc_size_t);
#define as_array(ptr, sz) _vcc_as_array(ptr, sz)
bool _vcc_in_array(obj_t, obj_t, _vcc_size_t);
#define in_array(ptr, arr, sz) _vcc_in_array(ptr, arr, sz)
bool _vcc_closed(obj_t );
#define closed(...) _vcc_closed(__VA_ARGS__)
obj_t _vcc_containing_struct(obj_t p, obj_t fld);
#define containing_struct(p, t, f) ((t *)_vcc_containing_struct(p, &(((t*)0)->f)))
bool _vcc_depends(obj_t , obj_t );
#define depends(...) _vcc_depends(__VA_ARGS__)
ptrset _vcc_domain(obj_t q);
#define domain(p) _vcc_domain(p)
obj_t _vcc_emb(obj_t );
#define emb(...) _vcc_emb(__VA_ARGS__)
ptrset _vcc_extent(obj_t );
#define extent(...) _vcc_extent(__VA_ARGS__)
ptrset _vcc_full_extent(obj_t );
#define full_extent(...) _vcc_full_extent(__VA_ARGS__)
bool _vcc_in_domain(obj_t p, obj_t q);
#define in_domain(p,q) _vcc_in_domain(p,q)
bool _vcc_in_vdomain(obj_t p, obj_t q);
#define in_vdomain(p,q) _vcc_in_vdomain(p,q)
obj_t _vcc_use(const char *, obj_t);
#define use(labelExpr, o) _vcc_use(#labelExpr, o)
bool _vcc_inv(obj_t );
#define inv(...) _vcc_inv(__VA_ARGS__)
bool _vcc_inv2(obj_t );
#define inv2(...) _vcc_inv2(__VA_ARGS__)
bool _vcc_inv2_when_closed(obj_t );
#define inv2_when_closed(...) _vcc_inv2_when_closed(__VA_ARGS__)
bool _vcc_is(obj_t p, typeid_t t);
#define is(p, t) _vcc_is(p, typeidof(t))
bool _vcc_is_array(obj_t , _vcc_size_t);
#define is_array(...) _vcc_is_array(__VA_ARGS__)
bool _vcc_is_array_emb(obj_t , _vcc_size_t, obj_t );
#define is_array_emb(...) _vcc_is_array_emb(__VA_ARGS__)
bool _vcc_is_claimable(typeid_t t);
#define is_claimable(x) _vcc_is_claimable(_vcc_typeof(x))
bool _vcc_is_fresh(obj_t );
#define is_fresh(...) _vcc_is_fresh(__VA_ARGS__)                  
bool _vcc_is_malloc_root(obj_t );
#define is_malloc_root(...) _vcc_is_malloc_root(__VA_ARGS__)
bool _vcc_is_mutable_array(obj_t , _vcc_size_t);
#define is_mutable_array(...) _vcc_is_mutable_array(__VA_ARGS__)
bool _vcc_is_object(obj_t );
#define is_object(...) _vcc_is_object(__VA_ARGS__)
bool _vcc_is_object_root(obj_t );
#define is_object_root(...) _vcc_is_object_root(__VA_ARGS__)
bool _vcc_is_ptr_to_composite(obj_t );
#define is_ptr_to_composite(...) _vcc_is_ptr_to_composite(__VA_ARGS__)
bool _vcc_is_thread(obj_t );
#define is_thread(...) _vcc_is_thread(__VA_ARGS__)
bool _vcc_is_thread_local_array(obj_t , _vcc_size_t);
#define is_thread_local_array(...) _vcc_is_thread_local_array(__VA_ARGS__)
thread_id _vcc_me();
#define me(...) _vcc_me(__VA_ARGS__)
bool _vcc_mutable(obj_t );
#define mutable(x) _vcc_mutable(x)
bool _vcc_nested(obj_t );
#define nested(...) _vcc_nested(__VA_ARGS__)
obj_t _vcc_owner(obj_t );
#define owner(...) _vcc_owner(__VA_ARGS__)
ptrset _vcc_owns(obj_t );
#define owns(x) _vcc_owns(x)
ptrset _vcc_span(obj_t );
#define span(...) _vcc_span(__VA_ARGS__)
bool _vcc_thread_local2(obj_t );
#define thread_local(x) _vcc_thread_local2(x)
bool _vcc_typed2(obj_t );
#define typed(x) _vcc_typed2(x)
typeid_t _vcc_typeof(obj_t );
#define typeidof(t) (_vcc_typeof((t*)0))
bool _vcc_wrapped(obj_t );
#define wrapped(x) _vcc_wrapped(x)
bool _vcc_not_shared(obj_t);
#define not_shared _vcc_not_shared
bool _vcc_union_active(obj_t,obj_t);
#define union_active(u,f) _vcc_union_active((u),&((u)->f))
#define union_active_anon(u,f) _vcc_union_active((u), (u)::f)
bool _vcc_program_entry_point();
#define program_entry_point _vcc_program_entry_point
obj_t _vcc_gemb(obj_t);
#define gemb(...) _vcc_gemb(__VA_ARGS__)
bool _vcc_keeps(obj_t, ...);
#define keeps(...)  _vcc_keeps(this , __VA_ARGS__)
bool _vcc_is_non_primitive_ptr(obj_t o);
#define is_non_primitive_ptr _vcc_is_non_primitive_ptr
bool _vcc_extent_mutable(obj_t o);
#define extent_mutable _vcc_extent_mutable
bool _vcc_extent_is_fresh(obj_t o);
#define extent_is_fresh _vcc_extent_is_fresh
bool _vcc_extent_zero(obj_t o);
#define extent_zero _vcc_extent_zero

template<typename T>
bool _vcc_approves(obj_t approver, T expr);
#define approves(...) _vcc_approves(__VA_ARGS__)

ptrset _vcc_non_null_set_singleton(obj_t );
ptrset _vcc_non_null_array_range(obj_t , _vcc_size_t);

bool _vcc_obj_eq(obj_t, obj_t);
#define obj_eq(a,b) _vcc_obj_eq(a,b)
bool _vcc_obj_neq(obj_t, obj_t);
#define obj_neq(a,b) _vcc_obj_neq(a,b)

/* Set theory */
obj_t _vcc_dummy_set_element();
ptrset _vcc_create_set(obj_t, ...);
#define SET(...) _vcc_create_set(_vcc_dummy_set_element(), __VA_ARGS__)
ptrset _vcc_set_singleton(obj_t );
#define set_singleton(x) _vcc_set_singleton(x)
ptrset _vcc_set_difference(ptrset,ptrset);
#define set_difference(x,y) _vcc_set_difference(x,y)
ptrset _vcc_set_union(ptrset,ptrset);
#define set_union(x,y) _vcc_set_union(x,y)
ptrset _vcc_set_intersection(ptrset,ptrset);
#define set_intersection(x,y) _vcc_set_intersection(x,y)
bool _vcc_set_in(obj_t ,ptrset);
#define set_in(x,y) _vcc_set_in(x,y)
bool _vcc_set_in0(obj_t ,ptrset);
#define set_in0(x,y) _vcc_set_in0(x,y)
bool _vcc_set_in2(obj_t ,ptrset);
#define set_in2(x,y) _vcc_set_in2(x,y)
bool _vcc_set_eq(ptrset,ptrset);
#define set_eq(x,y) _vcc_set_eq(x,y)
#define set_equal(x,y) _vcc_set_eq(x,y)
bool _vcc_set_disjoint(ptrset,ptrset);
#define set_disjoint(x,y) _vcc_set_disjoint(x,y)
bool _vcc_set_subset(ptrset,ptrset);
#define set_subset(x,y) _vcc_set_subset(x,y)
ptrset _vcc_set_empty();
#define set_empty() _vcc_set_empty()
ptrset _vcc_set_universe();
#define set_universe() _vcc_set_universe()
_vcc_size_t _vcc_set_cardinality(ptrset);
#define set_cardinality(...) _vcc_set_cardinality(__VA_ARGS__)

/*** 
 *** Specification procedures
 ***/

// treated specially when calling _vcc_* procedures
bool _vcc_is_atomic_obj(int parameter_index);

void _vcc_deep_unwrap(obj_t p, typeid_t t)
  writes (p);
#define deep_unwrap(x) _vcc_deep_unwrap(x, _vcc_typeof(x))

void _vcc_giveup_closed_owner(obj_t obj, obj_t owner)
  requires (_vcc_is_atomic_obj(1));
#define giveup_closed_owner(x, y) _vcc_giveup_closed_owner(x, y)

#define giveup_owner(p, owner) set_owns(owner, set_difference(owns(owner), set_singleton(p)))

void _vcc_reads_havoc();
#define reads_havoc _vcc_reads_havoc

void _vcc_havoc_others(obj_t p, typeid_t t);
#define havoc_others(p) _vcc_havoc_others(p, _vcc_typeof(p))

void _vcc_set_closed_owner(obj_t obj, obj_t owner)
  writes (obj)
  requires (_vcc_is_atomic_obj(1));
#define set_closed_owner(x, y) _vcc_set_closed_owner(x, y)

ptrset _vcc_new_ownees(obj_t owner, ptrset owns);
#define new_ownees _vcc_new_ownees

void _vcc_set_closed_owns(obj_t owner, ptrset owns)
  writes (new_ownees(owner, owns))
  requires (_vcc_is_atomic_obj(0));
#define set_closed_owns(x, y) _vcc_set_closed_owns(x, y)

#define set_owner(p, owner) set_owns(owner, set_union(set_singleton(p), owns(owner)))

void _vcc_set_owns(obj_t obj, ptrset owns)
  writes (obj);
#define set_owns(x, y) _vcc_set_owns(x, y)

void _vcc_union_reinterpret(obj_t u, obj_t fld)
  writes (extent(u));
#define union_reinterpret(u,f) _vcc_union_reinterpret(u,&((u)->f))

void _vcc_unwrap(obj_t p, typeid_t t)
  writes (p);
#define unwrap(x) _vcc_unwrap(x, _vcc_typeof(x))

void _vcc_wrap(obj_t p, typeid_t t)
  writes (p, owns(p));
#define wrap(x) _vcc_wrap(x, _vcc_typeof(x))

void _vcc_bump_volatile_version(obj_t p)
  writes (p);
#define bump_volatile_version(x) _vcc_bump_volatile_version(x)

void _vcc_free(obj_t p)
  writes (p)
  writes (extent(p));

void _vcc_stack_free(mathint, obj_t p);

template<typename T> obj_t _vcc_stack_alloc(mathint);

template<typename T> T *_vcc_spec_alloc();
template<typename T> T *_vcc_spec_alloc_array(unsigned int n);

obj_t _vcc_alloc(typeid_t);

#define spec_malloc _vcc_spec_alloc
#define spec_malloc_array _vcc_spec_alloc_array

void _vcc_atomic_op(obj_t, ...);
// do not change this expression - the compiler relies on exactly this structure to find the information it needs
#define atomic_op(op, ...) (_vcc_atomic_op(__VA_ARGS__, op))
#define atomic_read(op, ...) atomic_op(op, __VA_ARGS__, (0))

/* Block constructs (the while loop is just a dummy) */
bool _vcc_atomic(obj_t, ...);
#define atomic(...) while(_vcc_atomic(__VA_ARGS__))
void _vcc_begin_update();
#define begin_update() _vcc_begin_update()

bool _vcc_expose(obj_t);
#define expose(obj) while(_vcc_expose(obj))

bool _vcc_spec_code();
#define speconly while(_vcc_spec_code()) __specification

bool _vcc_skinny_expose(obj_t, ...);
#define skinny_expose(...) while(_vcc_skinny_expose(__VA_ARGS__))

bool _vcc_domain_updated_at(obj_t p, ptrset wr);
#define domain_updated_at _vcc_domain_updated_at

/*** 
 *** Memory reinterpretation
 ***/

void _vcc_join_arrays(obj_t arr1, obj_t arr2)
  writes(extent(arr1), extent(arr2));
void _vcc_split_array(obj_t arr, _vcc_size_t sz)
  writes(extent(arr));
void _vcc_to_bytes(obj_t obj)
  writes(extent(obj));
void _vcc_from_bytes(obj_t obj, ...)
  writes(extent(obj));

#define join_arrays _vcc_join_arrays
#define split_array _vcc_split_array
#define to_bytes _vcc_to_bytes
#define from_bytes _vcc_from_bytes


/*** 
 *** Function attributes 
 ***/

#define frameaxiom vcc_attr("frameaxiom", "")
#define isadmissibilitycheck __declspec(Microsoft.Contracts.IsAdmissibilityCheck)
#define ispure vcc_attr("is_pure", "")
#define no_reads_check vcc_attr("no_reads_check", "")
#define postconditionsanity vcc_attr("postcondition_sanity", "true")
#define reads_check(f) vcc_attr("is_reads_check", #f)
#define skipverification __declspec(Microsoft.Contracts.SkipVerification)
#define usevccoptions(o) vcc_attr("extra_options", o)
#define vcc_attr(k, v) __declspec(Microsoft.Contracts.StringVccAttr, k, v)
#define _vcc_attr_asm_routine vcc_attr("asm_routine", "true")


/*** 
 *** Type attributes 
 ***/
#define backing_member vcc_attr("backing_member", "")
#define member_name(n) vcc_attr("member_name", #n)
#define no_admissibility __declspec(Microsoft.Contracts.NoAdmissibility)
#define register __specification

#ifdef VCC_NO_SPLITS
#define vcs_force_splits(n)
#define vcs_keep_going(n)
#else
#define vcs_force_splits(n) \
	__declspec(Microsoft.Contracts.IntBoogieAttr, "vcs_max_splits", n) \
	__declspec(Microsoft.Contracts.IntBoogieAttr, "vcs_max_cost", 1)
#define vcs_keep_going(n) \
	__declspec(Microsoft.Contracts.IntBoogieAttr, "vcs_max_keep_going_splits", n)
#endif

#define vcc(x) _concat_identifiers(_vcc_attr_, x)
#define _vcc_attr_volatile_owns vcc_attr("volatile_owns", "true")
#define _vcc_attr_claimable vcc_attr("claimable", "true")
#define _vcc_attr_inline vcc_attr("inline", "true")
#define _vcc_attr_no_inline vcc_attr("no_inline", "true")
#define _vcc_attr_dynamic_owns vcc_attr("dynamic_owns", "true")
#define _vcc_attr_atomic_inline vcc_attr("atomic_inline", "true")
#define _vcc_attr_thread_local_storage vcc_attr("thread_local_storage", "true")
#define _vcc_attr_record vcc_attr("record", "true")
#define _vcc_attr_verified vcc_attr("status", "verified")
#define _vcc_attr_specified vcc_attr("status", "specified")


/* Groups */
#define in_group(n) __declspec(Microsoft.Contracts.InGroupDeclAttr, #n)
#define def_group(n, ...) struct __VA_ARGS__ __declspec(Microsoft.Contracts.GroupDeclAttr, #n) { };
bool _vcc_inv_group(const char *, bool);
#define inv_group(n, i) __invariant (_vcc_inv_group(#n, i))

/*** 
 *** Helper macros
 ***/

#define always(c,e) \
  requires(wrapped(c) && valid_claim(c) && claims(c, e)) \
  ensures(wrapped(c) && valid_claim(c))
#define claimp(n) __specification claim_t n
#define maintains(...) requires(__VA_ARGS__) ensures(__VA_ARGS__)
#define returns(e) ensures(result == (e))

#define speccast(_TYPE_, _EXPR_) ((_TYPE_)(_EXPR_))
#define speccast_uc(_TYPE_, _EXPR_) (unchecked((_TYPE_)(_EXPR_)))
#define nospeccast(_TYPE_, _EXPR_) (_EXPR_)

#define generic_instance(_F_, ...) _F_ __VA_ARGS__

#define out_param(p) \
  writes(p) \
  maintains(mutable(p) && is_object_root(p))

#define weak_out_param(p) \
  writes(p) \
  ensures(mutable(p) && unchanged(emb(p)))

#define unchanged(e) (old(e) == (e))
#define wrapped0(p) (wrapped(p) && ref_cnt(p) == 0)
#define wrapped_dom(p) (wrapped(p) && in_domain((p), (p)))
#define on_unwrap(e) invariant(old(closed(this)) && !closed(this) ==> (e))


/*** 
 *** Special annotation constructs
 ***/

bool _vcc_public_writes(ptrset);
#define public_writes(s) ensures(_vcc_public_writes(s))

template<typename T> bool _vcc_shallow_struct_eq(T* s, T* t);
#define shallow_eq(s1, s2) _vcc_shallow_struct_eq(s1, s2)

template<typename T> bool _vcc_deep_struct_eq(T* s, T* t);
#define deep_eq(s1, s2) _vcc_deep_struct_eq(s1, s2)

bool _vcc_boogie(const char*);
#define boogie(...) _vcc_boogie(__VA_ARGS__)

bool _vcc_bv_lemma(bool);
#define bv_lemma(e) assert(_vcc_bv_lemma(unchecked( e )))

bool _vcc_start_here();
#define start_here _vcc_start_here

template<typename T>
T _vcc_in_state(state_t state, T expr);
#define in_state(state, expr) _vcc_in_state(state, expr)

state_t _vcc_skip_wf();
#define skip_wf(e) _vcc_in_state(_vcc_skip_wf(), e)

state_t _vcc_current_state();
#define current_state() _vcc_current_state()

state_t _vcc_when_claimed();
#define when_claimed(e) _vcc_in_state(_vcc_when_claimed(), e)

/*** 
 *** Triggering hacks
 ***/

int _vcc_sk_hack(bool);
#define sk_hack(x) _vcc_sk_hack(x)
bool _vcc_dont_instantiate(obj_t );
#define dont_instantiate(...) _vcc_dont_instantiate(__VA_ARGS__)
bool _vcc_dont_instantiate_int(int);
#define dont_instantiate_int(...) _vcc_dont_instantiate_int(__VA_ARGS__)
bool _vcc_dont_instantiate_size_t(_vcc_size_t);
#define dont_instantiate_size_t(...) _vcc_dont_instantiate_size_t(__VA_ARGS__)

bool _vcc_match_ulong(unsigned __int64 x)
   ensures(result);
bool _vcc_match_long(__int64 x)
   ensures(result);

/*** 
 *** Claims
 ***/
struct _vcc_claim_struct { };
typedef struct _vcc_claim_struct *_vcc_claim_t;
#define claim_t _vcc_claim_t

claim_t _vcc_claim(obj_t, ...);
#define claim _vcc_claim

bool _vcc_claims(claim_t, bool);
#define claims _vcc_claims

void _vcc_unclaim(claim_t, ...);
#define unclaim _vcc_unclaim

bool _vcc_claims_obj(claim_t, obj_t);
#define claims_obj _vcc_claims_obj

bool _vcc_claims_claim(claim_t, obj_t);
#define claims_claim _vcc_claims_claim

_vcc_size_t _vcc_ref_cnt(obj_t);
#define ref_cnt _vcc_ref_cnt


bool _vcc_valid_claim(claim_t);
#define valid_claim _vcc_valid_claim

bool _vcc_in_claim_domain(obj_t, claim_t);
#define in_claim_domain _vcc_in_claim_domain

state_t _vcc_by_claim(claim_t c);
#define by_claim(c, e) _vcc_in_state(_vcc_by_claim(c), e)

bool _vcc_always_by_claim(claim_t c, obj_t o);
#define always_by_claim _vcc_always_by_claim

/*** 
 *** Obsolete
 ***/
enum _vcHintKind {
  _vcMemorySafety,
  _vcStructuralIntegrity,
  _vcFunctionalCorrectness
};
bool __vcAssumeHintKind(enum _vcHintKind kind);
#define hint(_KIND_, _EXPR_) assert(__vcAssumeHintKind(_vc ## _KIND_) || (_EXPR_)); assume(_EXPR_)

#define ref_cnt_ptr

#define _vcc_compat_region(p,s) _vcc_SAL_region(p,s)
#define _vcc_compat_typed(p,s) _vcc_SAL_typed(p,s)
#define _vcc_compat_overlaps(x,y) (!(set_disjoint(x,y)))

bool _vcc_SAL_typed(obj_t , _vcc_size_t);
ptrset _vcc_SAL_region(obj_t , _vcc_size_t);
ptrset _vcc_SAL_region_non_null(obj_t , _vcc_size_t);

// Misc
char *get___FUNCTION__();
void __annotation(...);
void __debugbreak(...);

#endif // _VCCP_H
