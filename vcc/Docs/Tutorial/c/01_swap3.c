#include <vcc.h>

#define writable(x) 1
/*{swap}*/
void swap(int *p, int *q)
//--
  weak_out_param(p)
  weak_out_param(q) //--
{
  int tmp;
  assume(writable(p) && writable(q)); // from the writes clause

  assert(thread_local(p));
  tmp = *p;
  assert(writable(p));
  assert(thread_local(q));
  *p = *q;
  assert(writable(q));
  *q = tmp;

  assert(*p == old(*q) && *q == old(*p)); // ensures
}
/*{out}*/
/*`
Verification of swap succeeded.
`*/
