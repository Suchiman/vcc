#include <vcc.h>

/*{counter}*/
struct vcc(claimable) Counter {
  volatile unsigned v;
  invariant(v > 0)
  invariant(v == old(v) || v == old(v) + 1)
};
/*{reading}*/
struct Reading {
  struct Counter *n;
  volatile unsigned r;
  spec(claim_t c;)
  invariant(keeps(c) && claims_obj(c, n))
  invariant(n->v >= r)
};
/*{endreading}*/
void create_reading(struct Counter *n claimp(c))
  requires(wrapped(c) && claims_obj(c, n))
  writes(c)
{
  struct Reading k;
  k.r = 0;
  k.n = n;
  k.c = c;
  wrap(&k);
  unwrap(&k);
}

/*`
`*/
