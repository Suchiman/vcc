//`/newsyntax
#include "vcc.h"

struct S {
  int a;
  _(ghost int b)
};

void foo() {
  struct S s;
  struct S *p = &s;
  _(assert p->\valid && !\ghost(p))
  _(assert (&p->a)->\valid && !\ghost(&p->a))
  _(assert (&p->b)->\valid && \ghost(&p->b))
}
/*`
Verification of foo succeeded.
`*/
