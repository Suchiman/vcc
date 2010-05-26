#include <vcc.h>
#include <stdlib.h>

_(claimable) struct T
{
    struct S c;
};

typedef _(claimable) struct R
{
    struct S c;
};

void foo() {
  struct T *t = (struct T *)malloc(sizeof(struct T));
  _(assert t == NULL || \is_claimable(\typeof(t)))
  struct R *r = (struct R *)malloc(sizeof(struct R));
  _(assert r == NULL || \is_claimable(\typeof(r)))
}
/*`
Verification of foo succeeded.
`*/
