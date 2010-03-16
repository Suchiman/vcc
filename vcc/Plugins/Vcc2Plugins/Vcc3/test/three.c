#include <vcc.h>

struct A {
  int a;
  spec(int b; )
};

void foo(struct A *x)
  writes(span(x))
{

  x->a = 10;
  speconly( x->b = 20; )
  assert(false);
}
