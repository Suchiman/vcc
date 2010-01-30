#include <vcc.h>

unsigned isqrt(unsigned x)
  //-- FIXME: with no requires Z3 doesn't come back
  requires( x <= 0xfffe0001 ) //--
  ensures( result*result <= x && x < (result+1)*(result+1))
{
  unsigned r = 0;
  while ((r+1)*(r+1) <= x)
    invariant( r*r <= x )
  {
    r++;
  }
  return r;
}
/*{out}*/
/*`
Verification of isqrt failed.
testcase(9,10) : error VC8004: (r+1)*(r+1) might overflow.
`*/
