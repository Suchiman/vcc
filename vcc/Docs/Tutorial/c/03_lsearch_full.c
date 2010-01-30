#include "vcc.h"
#include <limits.h>

unsigned lsearch(int elt, int *ar, unsigned sz)
  requires( wrapped( as_array( ar, sz ) ) )
  ensures(result != UINT_MAX ==> ar[result] == elt)
  ensures(result == UINT_MAX ==> 
            forall(unsigned i; i < sz ==> ar[i] != elt))
{
  unsigned i;
  for (i = 0; i < sz; i++)
    invariant(forall(unsigned j; j < i ==> ar[j] != elt))
  {
    if (ar[i] == elt) return i;
  }

  return UINT_MAX;
}
/*`
Verification of lsearch succeeded.
`*/
