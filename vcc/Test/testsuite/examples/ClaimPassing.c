#include <vcc2test.h>

struct vcc(claimable) A {
  volatile int x;
  invariant( old(this->x) == this->x || old(this->x) + 1 == this->x )
};

void incr(struct A *a, int *res claimp(c) claimp(*cres))
  writes(c, res, cres)
  always(c, closed(a))
  // there will be a single new child to c
  ensures(ref_cnt(c) == old(ref_cnt(c)) + 1)
  // the claim we return will be child of c
  ensures(claims_obj(*cres, c))
  // it will be wrapped
  ensures(wrapped(*cres))
  // it will be childless (so we'll be free to kill it)
  ensures(ref_cnt(*cres) == 0)
  // and it will guarantee a condition about a->x
  ensures(claims(*cres, a->x >= when_claimed(*res)))
  // and it was not allocated before
  ensures(is_fresh(*cres))
  // we don't free any of our out parameters
  ensures(mutable(res) && mutable(cres))
{
  spec( claim_t c1; )
  int val;

  atomic(c,a) {
    val = a->x;
  }
  
  *res = val;
  speconly( c1 = claim(c, when_claimed(*res) <= a->x); )
  speconly( *cres = c1; )
}

void use_case()
{
  struct A *a;
  int tmp;
  spec( claim_t c; )
  spec( claim_t c2; )

  a = malloc(sizeof(*a));
  a->x = 0;
  wrap(a);

  speconly( c = claim(a, true); )
  incr(a, &tmp spec(c) spec( &c2) );
  assert(wrapped(c));
 
  assert(valid_claim(c2));
  assert(tmp <= skip_wf(a->x));

  assert(wrapped(c));

  unclaim(c2, c);
  unclaim(c, a);
  unwrap(a);
  free(a);
}

/*`
Verification of A#adm succeeded.
Verification of incr succeeded.
Verification of use_case succeeded.
`*/
