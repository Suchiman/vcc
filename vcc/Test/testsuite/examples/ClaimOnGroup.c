//`/newsyntax
#include <vcc.h>

struct F {
  int value;

  _(group  _(claimable) g)

  _(:g)
  volatile int vol;

  _(invariant \mine(\this::g))
};

int test(struct F *f)
  _(requires \wrapped(f::g))
  _(writes f::g)
{
  _(ghost \claim c)

  _(assert ((struct F::g*)f)->\closed)

  _(ghost c = \make_claim({f::g}, (f::g)->\closed))
}

/*`
Verification of F#adm succeeded.
Verification of test succeeded.
`*/
