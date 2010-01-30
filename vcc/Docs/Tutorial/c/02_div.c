#include <vcc.h>
//--
axiom(forall(unsigned a,b; a > b && b != 0 ==> a % b == (a - b) % b));
axiom(forall(unsigned a,b; a < b && b != 0 ==> a % b == a));//--
unsigned mod(unsigned a, unsigned b)
  ensures(result == a % b)
{
  unsigned res = a;

  for (;;)
    invariant( a % b == res % b )
  {
    if (res < b) break;
    res -= b;
  }
  return res;
}
/*`
Verification of mod succeeded.
`*/
