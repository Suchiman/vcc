#include <vcc.h>

struct A {
  int a;
  spec(int b; )
};

void foo(struct A *x)
  writes(span(x))
{
  spec(  int k; )

  assert(true);
  x->a = 10;
  speconly( x->b = 20; )
  speconly( k = x->b; )
  assert(false);
}
