#include <vcc.h>

#define writable(x) 1
/*{swap}*/
void swap(int *p, int *q)
//--
  weak_out_param(p)
  weak_out_param(q) //--
{
  int tmp;
  _(assume writable(p) && writable(q)) // from the writes clause

  _(assert \thread_local(p))
  tmp = *p;
  _(assert writable(p))
  _(assert \thread_local(q))
  *p = *q;
  _(assert writable(q))
  *q = tmp;

  _(assert *p == \old(*q) && *q == \old(*p)) // ensures
}
/*{out}*/
/*`
Verification of swap succeeded.
`*/
