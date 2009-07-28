#include <vcc.h>

spec(
  struct vcc(claimable) Protector {
    int dummy;
  };
)

spec(
  struct vcc(claimable) AbstractStruct
  {
    volatile int value;
    struct Protector protector;

    // By claiming the protector, a claim that abs->value doesn't change can be
    // established.
    invariant(closed(&protector) ==> unchanged(value))
    // The other case.
    invariant(value == 2*old(value) || unchanged(value))
  };
)

struct ConcreteStruct
{
  int value;
  spec(struct AbstractStruct *abs;)
  invariant(keeps(abs, &abs->protector))
  // We need to make sure no one has a claim on the protector, as we will want
  // to crack it open.
  invariant(ref_cnt(&abs->protector) == 0)
  invariant(value == abs->value)
};

int writeStruct(struct ConcreteStruct *s, int v)
  maintains(wrapped(s))
  writes(s)
  requires(v == 2*s->value)
{
  spec(claim_t c;)

  // Need to save s->abs into a local, otherwise when using s->abs in claims,
  // the prover thinks that it might change (which is true).
  spec( struct AbstractStruct *abs = s->abs; )

  // We unwrap s and immediately take a claim on the protector and abs.
  // The claim holds initially, because we're right after unwrap, therefore
  // the invariant of s has to hold.
  // The claim holds after step of the machine, because the abs and its protector
  // stay closed, therefore abs->value cannot change.
  unwrap(s);
  speconly(c = claim(&abs->protector, abs, abs->value == when_claimed(abs->value));)

  s->value = v;

  atomic(abs) {
    // Here actions of other thread happen ('atomic havoc')
    //
    // Note the missing c in the atomic clause: if we had listed c above, the
    // compiler generates an assert(valid_claim(c)) here and _after_
    // begin_update().  If you look below, you see that the second one cannot
    // hold.  The first assert we do explicitly, allowing us to still make use
    // of the claim here.
    
    // We use the claim, so we know abs->value didn't change.
    assert(valid_claim(c));
    // We get rid of the claim...
    unclaim(c, &abs->protector, abs);
    // ...so that we can crack the protector open.
    unwrap(&abs->protector);

    // begin_update() gives you write access to s, and also makes
    // the two state invariant of s to be checked between here and
    // the end of the atomic block. Otherwise it does not modify the
    // heap.
    begin_update();
    speconly(s->abs->value = v;)
  }
  // We close the protector again.
  wrap(&abs->protector);
  // Then we can close s. Note that there was no other atomic havoc since the
  // beginning of atomic, which is why the invariant of s holds.
  wrap(s);
}

/*`
Verification of AbstractStruct#adm succeeded.
Verification of ConcreteStruct#adm succeeded.
Verification of writeStruct succeeded.
`*/
