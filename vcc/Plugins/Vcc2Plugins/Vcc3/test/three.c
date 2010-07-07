#include <vcc.h>

struct vcc(dynamic_owns) A {
  int a;
  spec(int b; )
};

void foo(struct A *x)
  requires(wrapped(x))
  writes(x)
{
  spec(  int k; )

  unwrap(x);
  speconly( k = x->b; )
  x->a = 10;
  speconly( x->b = 20; )
  wrap(x);
}
