#include <vcc.h>

struct vcc(dynamic_owns) A {
  int a;
  spec(int b; )
};

void foo(struct A *x)
  maintains(wrapped(x))
  writes(x)
  ensures(x->b > x->a)
{
  spec(  int k; )

  unwrap(x);
  speconly( k = x->b; )
  x->a = 10;
  speconly( x->b = 20; )
  assert(x->b > x->a);
  wrap(x);
}
