
#include <vcc.h>

typedef struct S { int a; } S;
typedef struct T { int b; } T;

void foo(void *p)
  _(requires p \is struct S || p \is T)
  _(requires \thread_local(p))
{
  _(assert p \is S || p \is struct T)
}
/*`
Verification of foo succeeded.
`*/