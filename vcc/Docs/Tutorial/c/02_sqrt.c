#include <vcc.h>

// from Spec# tutorial

unsigned isqrt(unsigned x)
  requires(x < 0xfffe0001)
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

