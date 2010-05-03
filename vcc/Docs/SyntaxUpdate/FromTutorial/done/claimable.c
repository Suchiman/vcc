#include <vcc.h>
#include <stdlib.h>

_(claimable) struct T
{
    struct S c;
};

void foo() {
  struct T *t = (struct T *)malloc(sizeof(struct T));
  _(assert t == NULL || \is_claimable(\typeof(t)))
}
/*`
Verification of foo succeeded.
`*/
