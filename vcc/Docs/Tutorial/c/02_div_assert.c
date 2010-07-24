#include <vcc.h>

typedef unsigned int UINT;

void divide(UINT x, UINT d, UINT *q, UINT *r)
{
  // assume the precondition
  _(assume d > 0 && q != r)
  UINT lq = 0;
  UINT lr = x;

  // check that the invariant holds on loop entry
  _(assert x == d*lq + lr)
  
  // start an arbitrary iteration
  // havoc variables modified in the loop  
  havoc(lq);
  havoc(lr);
  // assume that the loop invariant holds
  _(assume x == d*lq + lr)
  // jump out if the loop terminated
  if (!(lr >= d) goto loopExit;
  {
    lq++;
    lr -= d;
  }
  // check that the loop preserves the invariant
  _(assert x == d*lq + lr)
  // end of the loop
  _(assume \false)

  loopExit:
  *q = lq;
  *r = lr;
  // assert postcondition
  _(assert x == d*(*q) + *r && *r < d)
}
