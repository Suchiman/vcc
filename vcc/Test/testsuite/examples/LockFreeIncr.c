#include <vcc.h>

vcc(atomic_inline) int InterlockedCompareExchange(volatile int *Destination, int Exchange, int Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}

struct A {
  volatile int x;
  invariant( old(this->x) == this->x || old(this->x) + 1 == this->x )
};

void LockFreeIncr(struct A *a claimp(c))
  writes(c)
  always(c, closed(a))
  ensures(ref_cnt(c) == old(ref_cnt(c)))
{
  int y;
  int z;
  spec( claim_t c1, c2; )

  atomic(c,a) {
    y = a->x;
    spec( c1 = claim(c, y <= a->x); )
  }
  
  if (y >= 0x7fffffff) {
    unclaim(c1, c);
    return;
  }

  atomic(c, c1, a) {
    z = InterlockedCompareExchange(&a->x, y+1, y);
    spec( c2 = claim(c, y < a->x); )
  }

  atomic(c, c2, a) {
    z = a->x;
  }

  assert(y < z);

  unclaim(c1, c);
  unclaim(c2, c);
}

/*`
Verification of A#adm succeeded.
Verification of LockFreeIncr succeeded.
`*/
