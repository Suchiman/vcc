#include <vcc2test.h>

vcc(atomic_inline) unsigned int InterlockedIncrement(volatile unsigned int *p) {
  *p = *p + 1;
  return *p;
}

vcc(atomic_inline) unsigned int InterlockedDecrement(volatile unsigned int *p) {
  *p = *p - 1;
  return *p;
}


typedef struct vcc(claimable) _Protector {
  int dummy;
} Protector;

typedef struct vcc(claimable) vcc(volatile_owns) _Rundown {
  spec( volatile claim_t self_claim; )
  spec( obj_t protected_obj; )
  volatile unsigned int count;

  spec( volatile bool alive; )
  spec( volatile bool enabled; )
  spec( Protector enabled_protector; )
  invariant( old(closed(&enabled_protector)) ==> unchanged(enabled) && unchanged(alive) )

  invariant( !alive ==> !enabled && count == 0 )
  // cannot set both count and alive in one step
  invariant( old(count) > 0 && old(alive) ==> alive )
  invariant( alive ==>
               keeps(protected_obj) &&
               keeps(self_claim) &&
               claims_obj(self_claim, this) &&
               ref_cnt(self_claim) == count )
  invariant(old(alive) ==> unchanged(self_claim) )

  invariant( !enabled ==> count <= old(count) )

} Rundown;

void InitializeRundown(Rundown *r spec(obj_t obj) claimp(out rdi))
  writes(extent(r), obj)
  requires(wrapped(obj))
  ensures(claims_obj(rdi, r))
  ensures(claims_obj(rdi, &r->enabled_protector))
  ensures(is_fresh(rdi) && wrapped0(rdi) && claims(rdi, r->enabled && r->protected_obj == obj))
  ensures(ref_cnt(r) == 2)
  ensures(wrapped(&r->enabled_protector) && ref_cnt(&r->enabled_protector) == 1)
  ensures(wrapped(r))
{
  spec(claim_t s1;)

  r->count = 0;
  set_owns(r, SET());
  set_owns(&r->enabled_protector, SET());
  speconly( r->protected_obj = obj; r->alive = false; r->enabled = false; )
  wrap(r);

  assert(not_shared(r));

  speconly(
    atomic(r) {
      s1 = claim(r, true);
      begin_update();
      r->alive = true;
      r->enabled = true;
      r->self_claim = s1;
      set_closed_owner(obj, r);
      set_closed_owner(s1, r);
    }
    wrap(&r->enabled_protector);
    // we can still claim r->enabled, as the state is havoced only at the BEGINNING of the atomic,
    // not at the end
    rdi = claim(r, &r->enabled_protector, r->enabled && r->protected_obj == obj);
  )
}

void ReferenceRundown(Rundown *r claimp(out res) claimp(rdi))
  // we want a claim to our enabled rundown
  always(rdi, closed(r) && r->enabled )
  // we will give out a fresh claim with zero ref_cnt
  ensures(wrapped0(res) && is_fresh(res))
  // the claim will reference r->self_claim, will guarantee that the protected_obj is closed and the rundown is initialized
  ensures(claims(res, claims_obj(res, r->self_claim) && closed(r) && closed(r->protected_obj) && r->count > 0))
{
  spec( claim_t c; )

  atomic(r, rdi) {
    assume(r->count < 0xffffffff);
    InterlockedIncrement(&r->count);
    speconly( res = claim(r->self_claim, r->count > 0 && closed(r->protected_obj) && when_claimed(r->self_claim) == r->self_claim); )
  }
}

void DereferenceRundown(Rundown *r claimp( h ))
  // we write the claim, what will we do with it? we're not telling.
  writes(h)
  // we want a claim to self_claim with no outstanding references guaranteeing that rundown is fully initialized
  requires(wrapped0(h) && claims(h, claims_obj(h, r->self_claim) && closed(r) && r->count > 0))
{
  atomic(h, r) {
    unclaim(h, r->self_claim);
    InterlockedDecrement(&r->count);
  }
}

typedef struct vcc(claimable) _Resource {
  volatile int x;
  Rundown rd;
} Resource;

typedef struct _RundownContainer {
  Resource *rsc;
  bool enabled;
  spec( claim_t rd_claim; )

  invariant( keeps(rd_claim, &rsc->rd, &rsc->rd.enabled_protector) )

  invariant( claims_obj(rd_claim, &rsc->rd) )
  invariant( claims_obj(rd_claim, &rsc->rd.enabled_protector) )

  invariant( ref_cnt(&rsc->rd.enabled_protector) == 1 )
  invariant( ref_cnt(&rsc->rd) == 2 )
  invariant( ref_cnt(rd_claim) == 0 )

  invariant( claims(rd_claim, when_claimed(rsc)->rd.enabled == when_claimed(enabled)) )
  invariant( claims(rd_claim, when_claimed(rsc)->rd.alive) )
  invariant( claims(rd_claim, when_claimed(rsc)->rd.protected_obj == when_claimed(rsc)) )
} RundownContainer;

void InitializeRundownContainer(Resource *rsc, RundownContainer *cont)
  writes(span(cont), rsc, extent(&rsc->rd))
  requires(mutable(&rsc->rd.enabled_protector))
  requires(wrapped(rsc))
  ensures(wrapped(cont))
{
  cont->rsc = rsc;
  InitializeRundown(&rsc->rd spec(rsc) spec(out cont->rd_claim));
  cont->enabled = 1;

  wrap(cont);
}

void FinalizeRundownContainer(RundownContainer *cont)
  writes(cont)
  maintains(wrapped(cont))
  requires(cont->enabled)
  ensures(!cont->enabled)
{
  spec(claim_t tmp;)
  Rundown *rd;
  Resource *r1;

  unwrap(cont);
  cont->enabled = 0;
  speconly(
    rd = &cont->rsc->rd;
    atomic(rd) {
      assert(valid_claim(cont->rd_claim));
      unclaim(cont->rd_claim, rd, &rd->enabled_protector);
      unwrap(&rd->enabled_protector);
      begin_update();
      rd->enabled = 0;
    }
    wrap(&rd->enabled_protector);
    tmp = claim(rd, &rd->enabled_protector, rd->alive && !rd->enabled && rd->protected_obj == when_claimed(cont->rsc));
    giveup_owner(cont->rd_claim, cont);
    cont->rd_claim = tmp;
    set_owner(cont->rd_claim, cont);
    wrap(cont);
  )
}

Resource *KillRundownContainerDead(RundownContainer *cont)
  writes(cont)
  requires(wrapped(cont))
  ensures(old(cont->rsc) == result)
  ensures(wrapped(result))
  requires(!cont->enabled)
  requires(is_malloc_root(cont))
{
  Resource *rsc;
  unsigned int cur_count;
  Rundown *rd = &cont->rsc->rd;
  spec(claim_t tmp, is_zero; )

  unwrap(cont);
    rsc = cont->rsc;

    do
      invariant(ref_cnt(cont->rd_claim) == 0)
      invariant(wrapped(cont->rd_claim))
      writes(cont->rd_claim)
    {
      atomic (cont->rd_claim, rd) {
        cur_count = rd->count;
        speconly(
           if (cur_count == 0) {
             is_zero = claim(cont->rd_claim, rd->count == 0);
           })
      }
    } while (cur_count != 0);

    speconly(
      atomic(rd) {
        assert(valid_claim(is_zero));
        assert(valid_claim(cont->rd_claim));
        unclaim(is_zero, cont->rd_claim);
        unclaim(cont->rd_claim, rd, &rd->enabled_protector);
        unwrap(&rd->enabled_protector);
        begin_update();
        tmp = rd->self_claim;
        giveup_closed_owner(rd->self_claim, rd);
        rd->alive = 0;
      }
      unclaim(tmp, rd);
      unwrap(rd);
    )
  free(cont);
  return rsc;
}

void UseRundown(Resource *a, Rundown *r claimp(rdi))
  always(rdi, closed(r) && r->protected_obj == a && r->enabled )
{
  spec( claim_t ac; )
  ReferenceRundown(r spec(out ac) spec(rdi));
  atomic(rdi, ac, a) {
    a->x = 12;
  }
  DereferenceRundown(r spec(ac));
}

/*`
Verification of _Rundown#adm succeeded.
Verification of _RundownContainer#adm succeeded.
Verification of InitializeRundown succeeded.
Verification of ReferenceRundown succeeded.
Verification of DereferenceRundown succeeded.
Verification of InitializeRundownContainer succeeded.
Verification of FinalizeRundownContainer succeeded.
Verification of KillRundownContainerDead succeeded.
Verification of UseRundown succeeded.
`*/
