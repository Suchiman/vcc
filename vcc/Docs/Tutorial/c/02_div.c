#include <vcc.h>

axiom(forall(unsigned a,b; a > b && b != 0 ==> a % b == (a - b) % b));
axiom(forall(unsigned a,b; a < b && b != 0 ==> a % b == a));

unsigned mod(unsigned a, unsigned b)
  ensures(result == a % b)
{
  spec( unsigned a0 = a; )

  while (a >= b)
    invariant( a % b == a0 % b )
  {
    a -= b;
  }
  return a;
}
