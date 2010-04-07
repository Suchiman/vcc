#include <vcc.h>

/*{swap}*/
void swap(int *p, int *q)
  // will be spec( writes p, q )
  weak_out_param(p)
  weak_out_param(q)
  _(ensures *p == \old(*q) && *q == \old(*p))
{
  int tmp;
  tmp = *p;
  *p = *q;
  *q = tmp;
}
/*`
Verification of swap succeeded.
`*/
