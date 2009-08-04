#include <vcc2test.h>

struct vcc(claimable) A {
  volatile int x;
  invariant( unchanged(x) || old(x) + 1 == x )
};

void incr(struct A *a, int *res claimp(c) claimp(out cres))
  writes(c, res)
  always(c, closed(a))
  // there will be a single new child to c
  ensures(ref_cnt(c) == old(ref_cnt(c)) + 1)
  // the claim we return will be child of c
  ensures(claims_obj(cres, c))
  // it will be wrapped without any references
  ensures(wrapped0(cres))
  // and it will guarantee a condition about a->x
  ensures(claims(cres, a->x >= when_claimed(*res)))
  // and it was not allocated before
  ensures(is_fresh(cres))
  // we don't free the result parameter
  ensures(mutable(res))
{
  int val;

  atomic(c,a) {
    val = a->x;
  }
  
  *res = val;
  speconly( cres = claim(c, when_claimed(*res) <= a->x); )
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
  incr(a, &tmp spec(c) spec(out c2) );
  assert(wrapped(c));
 
  assert(valid_claim(c2));
  assert(tmp <= a->x);

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
