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

    // This makes it possible, if you claim the protector, to have
    // a claim that abs->value doesn't change.
    invariant(closed(&protector) ==> this->value == old(this->value))
    // The other case.
    invariant(this->value == 2*old(this->value) || this->value == old(this->value))
  };
)

struct ConcreteStruct
{
  int value;
  spec(struct AbstractStruct *abs;)
  invariant(keeps(this->abs, &this->abs->protector))
  // We need to make sure noone has a claim on the protector,
  // as we will want to crack it open.
  invariant(ref_cnt(&this->abs->protector) == 0)
  invariant(this->value == this->abs->value)
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

  // We unwrap s and immedietly take a claim on the protector and abs.
  //   The claim holds initially, because we're right after unwrap, therefore
  //   the invariant of s has to hold.
  //   The claim holds after step of the machine, because the abs and its protector
  //   stay closed, therefore abs->value cannot change.
  unwrap(s);
  speconly(c = claim(&abs->protector, abs, abs->value == when_claimed(abs->value));)

  s->value = v;

  atomic(abs) {
    // Here actions of other thread happen.
    
    // We use the claim, so we know abs->value didn't change.
    assert(valid_claim(c));
    // We get rid of the claim, so...
    unclaim(c, &abs->protector, abs);
    // ...we can crack the protector open.
    unwrap(&abs->protector);

    // begin_update() gives you write access to 's', and also makes
    // the two state invariant of s to be checked between here and
    // the end of the atomic block. Otherwise it does not modify the
    // heap.
    begin_update();
    speconly(s->abs->value = v;)
  }
  // We close back the protector,
  wrap(&abs->protector);
  // so we can close s. Note that there was no other-threads-havoc since the beginning
  // of atomic, which is why invariant of 's' holds.
  wrap(s);
}

/*`
Verification of AbstractStruct#adm succeeded.
Verification of ConcreteStruct#adm succeeded.
Verification of writeStruct succeeded.
`*/
