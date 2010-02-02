#include <vcc.h>

#define writable(p) 1 // TODO

/*{beg}*/
void boundedIncr(int *p)
//--
weak_out_param(p)//--
{
  assume(writable(p)); // from writes clause

  assert(thread_local(p));
  if (*p < 100) {
    assert(writable(p));
    (*p)++;
  }

  assert(old(*p) < 100 ==> *p == old(*p) + 1); // ensures
}
/*`
Verification of boundedIncr succeeded.
`*/
