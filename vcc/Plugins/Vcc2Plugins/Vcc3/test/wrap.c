#include <vcc.h>

struct vcc(dynamic_owns) A {
  int a;
  spec(int b; )
  invariant(a < b)
};

void foo(struct A *x)
  requires(wrapped(x))
  ensures(wrapped(x))
  writes(x)
{
  unwrap(x);
  x->a = 10;
  speconly( x->b = 20; )
  wrap(x);
  assert(false);
}
