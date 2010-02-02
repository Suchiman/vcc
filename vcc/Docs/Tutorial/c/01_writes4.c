#include <vcc.h>

void boundedIncr(int *p)
  weak_out_param(p)
  ensures(old(*p) < 100 ==> *p == old(*p) + 1);

int x, y;

/*{foo}*/
int foo()
  requires(x == 12)
  writes(&y)
{
  y = 42;
  boundedIncr(&x);
  assert(x == 13 && y == 42);
}
/*{out}*/
/*`
Verification of foo failed.
testcase(15,3) : error VC8510: Assertion 'x is writable in call to boundedIncr(&x)' did not verify.
`*/
