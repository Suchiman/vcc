#include "vcc.h"
#include <limits.h>

void increment(int *x)
    requires(mutable(x))
    requires(*x < INT_MAX)
    writes(x)
    ensures(*x == old(*x) + 1)
{
    *x = *x + 1;
}

/*`
Verification of increment succeeded.
`*/
