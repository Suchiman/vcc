#include <vcc.h>

typedef unsigned int UINT;

void divide(UINT x, UINT d, UINT *q, UINT *r)
_(requires d > 0 && q != r)
_(writes q,r)
_(ensures x == d*(*q) + *r && *r < d)
{
  UINT lq = 0;
  UINT lr = x;
  while (lr >= d)
  _(invariant x == d*lq + lr)
  {
    lq++;
    lr -= d;
  }
  *q = lq;
  *r = lr;
}

/*`
Verification of divide succeeded.
`*/
