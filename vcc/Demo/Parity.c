#include <vcc.h>

typedef struct vcc(claimable) _A {
  volatile int f;
  invariant(unchanged(f) || f == unchecked(old(f) + 2))
  // unchecked() used as work-around for issue 5883
} A;

int check_parity(A *a spec(out claim_t p))
  requires(wrapped(a))
  writes(a)
  ensures(wrapped(a) && ref_cnt(a) == old(ref_cnt(a)) + 1)
  ensures(wrapped(p) && claims(p, (a->f & 1) == result))
{
  int tmp;
  atomic (a) {
    tmp = a->f;
    bv_lemma(forall (int x; (x&1) == ((x+2) & 1)));
    spec(p = claim(a, (a->f & 1) == (tmp & 1));)
  }
  return (tmp & 1);
}

int check_parity_b(A *a spec(claim_t c) spec(out claim_t p))
  requires(wrapped(c) && claims(c, closed(a)))
  writes(c)
  ensures(wrapped(c) && ref_cnt(c) == old(ref_cnt(c)) + 1)
  ensures(wrapped(p) && claims(p, (a->f & 1) == result))
{
  int tmp;
  atomic (a, c) {
    tmp = a->f;
    bv_lemma(forall (int x; (x&1) == ((x+2) & 1)));
    spec(p = claim(c, (a->f & 1) == (tmp & 1));)
  }
  return (tmp & 1);
}

/*`
Verification of _A#adm succeeded.
Verification of check_parity succeeded.
Verification of check_parity_b succeeded.
Verification of check_parity#bv_lemma#0 succeeded.
Verification of check_parity_b#bv_lemma#0 succeeded.
`*/
