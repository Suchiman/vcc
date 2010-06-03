#include <vcc.h>
//--
_(axiom \forall unsigned a,b; a > b && b != 0 ==> a % b == (a - b) % b)
_(axiom \forall unsigned a,b; a < b && b != 0 ==> a % b == a)

unsigned mod(unsigned a, unsigned b)
  _(ensures \result == a % b)
{
  unsigned res = a;

  for (;;)
    _(invariant a % b == res % b)
  {
    if (res < b) break;
    res -= b;
  }
  return res;
}
/*`
Verification of mod succeeded.
`*/
