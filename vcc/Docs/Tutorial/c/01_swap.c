#include <vcc.h>

/*{swap}*/
void swap(int *p, int *q)
  writes(p,q)
  ensures(*p == old(*q) && *q == old(*p))
{
  int tmp;
  tmp = *p;
  *p = *q;
  *q = tmp;
}
/*`
Verification of swap succeeded.
`*/
