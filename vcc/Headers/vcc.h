//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#ifndef _VCC_H
#define _VCC_H

#include <crtdefs.h>

#ifndef VERIFY

#ifndef __cplusplus
typedef int bool;
#endif

#ifndef assert
#define assert(...)
#endif

#define bv_lemma(e)
#define out_param(o)
#define weak_out_param(o)
#define wrap(x)
#define unwrap(x)
#define wraps(x,y)
#define unwraps(x,y)
#define deep_unwrap(x)
#define union_reinterpret(...)
#define set_owner(x, y)
#define set_owns(x, y)
#define giveup_closed_owner(x, y)
#define giveup_owner(x, y)
#define set_closed_owns(x, y)
#define set_closed_owner(x, y)
#define begin_update()
#define vcc_attr(k, v)
#define on_unwrap(e)
#define skinny_expose(...)
#define join_arrays(...)
#define split_array(...)
#define to_bytes(...)
#define from_bytes(...)

#define block
#define atomic(...)
#define atomic_op(op, ...) (op)
#define atomic_read(op, ...) (op)
#define always(...)
#define expose(...)
#define public_writes(s)
#define allocates()
#define assume(...)
#define axiom(...)
#define ensures(...)
#define requires(...)
#define maintains(...)
#define reads(...)
#define returns(e)
#define spec(...)
#define skipverification
#define frameaxiom
#define no_admissibility
#define backing_member
#define member_name(n)
#define unchecked(...) (__VA_ARGS__)
#define speccast(_TYPE_, _EXPR_) (_EXPR_)
#define speccast_uc(_TYPE_, _EXPR_) (_EXPR_)
#define nospeccast(_TYPE_, _EXPR_) ((_TYPE_)_EXPR_)
#define speconly(...)
#define writes(...)
#define vcs_force_splits(n) 
#define vcs_keep_going(n) 
#define invariant(...)
#define as_array(ptr, sz) (ptr)
#define hint(_KIND_,_EXPR_)
#define isadmissibilitycheck
#define ispure
#define vcc(...)
#define in_group(...)
#define def_group(...)
#define inv_group(...)
#define claimp(n)
#define unclaim(...)
#define usevccoptions(...)
#define by_claim(c, e) (e)
#define skip_wf(_EXPR_) (_EXPR_)
#define SPEC_TYPE(name)
#define generic_instance(_F_, ...) (_F_)

#else

#include <vccp.h>

#endif

#endif // _VCC_H
