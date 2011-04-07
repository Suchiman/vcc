//`/newsyntax
#include <vcc.h>

_(atomic_inline) int InterlockedCompareExchange(volatile int *Destination, int Exchange, int Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}

struct A {
  volatile int x;
  _(invariant \old(\this->x) == \this->x || \old(\this->x) + 1 == \this->x)
};

void LockFreeIncr(struct A *a _(ghost \claim c))
  _(writes c)
  _(always c, a->\consistent)
  _(ensures c->\claim_count == \old(c->\claim_count))
{
  int y;
  int z;
   _(ghost \claim c1, c2;) 

  _(atomic c,a) {
    y = a->x;
     _(ghost c1 = \make_claim({c}, y <= a->x);) 
  }
  
  if (y >= 0x7fffffff) {
    _(ghost \destroy_claim(c1, {c}));
    return;
  }

  _(atomic c, c1, a) {
    z = InterlockedCompareExchange(&a->x, y+1, y);
     _(ghost c2 = \make_claim({c}, y < a->x);) 
  }

  _(atomic c, c2, a) {
    z = a->x;
  }

  _(assert y < z)

  _(ghost \destroy_claim(c1, {c}));
  _(ghost \destroy_claim(c2, {c}));
}

/*`
Verification of A#adm succeeded.
Verification of LockFreeIncr succeeded.
`*/
