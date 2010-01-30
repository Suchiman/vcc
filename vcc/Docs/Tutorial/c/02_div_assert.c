#include <vcc.h>
//--
axiom(forall(unsigned a,b; a > b && b != 0 ==> a % b == (a - b) % b));
axiom(forall(unsigned a,b; a < b && b != 0 ==> a % b == a));
unsigned anything()
  reads(set_universe()); //--
unsigned mod(unsigned a, unsigned b)
{
  unsigned res = a;

  // check that invariant initially holds
  assert(a % b == res % b);

  // start an arbitrary loop iteration
  res = anything();
  assume(a % b == res % b); // assume the invariant
  if (res < b) goto theEnd;
  res -= b;
  assert(a % b == res % b); // check the invariant
  assume(false); // end of an iteration

theEnd:
  assert(res == a % b); // translation of ensures
  return res;
}
/*`
Verification of mod succeeded.
`*/
