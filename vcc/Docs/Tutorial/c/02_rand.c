#include <vcc.h>
#include <stdlib.h>

unsigned random(unsigned bound)
  _(requires bound > 0)
  _(ensures \result < bound)
{
  return _(unchecked)((unsigned)rand()) % bound;
}
/*`
Verification of random succeeded.
`*/
