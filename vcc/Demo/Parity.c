#include <vcc2.h>

typedef struct vcc(claimable) _A {
  volatile int f;
  invariant(unchanged(f) || f == old(f) + 2)
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
    speconly(p = claim(a, (a->f & 1) == (tmp & 1));)
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
    speconly(p = claim(c, (a->f & 1) == (tmp & 1));)
  }
  return (tmp & 1);
}
