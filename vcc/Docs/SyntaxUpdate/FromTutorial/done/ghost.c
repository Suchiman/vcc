#include "vcc.h"

struct S {
  int a;
  _(ghost int b)
};

void foo() {
  struct S s;
  struct S *p = &s;
  _(assert \valid(p) && !\ghost(p))
  _(assert \valid(&p->a) && !\ghost(&p->a))
  _(assert \valid(&p->b) && \ghost(&p->b))
}
/*`
Verification of foo succeeded.
`*/