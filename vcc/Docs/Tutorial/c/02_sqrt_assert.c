#include <vcc.h>

unsigned anything()
  reads(set_universe());

void isqrt(unsigned x)
  requires( x < 0xfffe0001 )
{
  unsigned r = 0, s;
  
  assert(r*r <= x); // invariant initially holds
  r = anything();
  assume(r*r <= x);
  if ((r+1)*(r+1) <= x) {
    r++;
    assert(r*r <= x);
    assume(false);
  }
  assert( r*r <= x && x < (r+1)*(r+1));
}
/*`
Verification of isqrt succeeded.
`*/
