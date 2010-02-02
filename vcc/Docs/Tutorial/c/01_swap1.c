#include "01_swap.c"

/*{foo}*/
int x, y, z;

void foo()
  writes(&x, &y, &z)
  requires(x == 1 && y == 2)
{
  z = 3;
  swap(&x, &y);
  assert(x == 2 && y == 1 && z == 3);
}
/*`
Verification of swap succeeded.
Verification of foo succeeded.
`*/
