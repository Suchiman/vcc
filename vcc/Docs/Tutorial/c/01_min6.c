#include <vcc.h>

int min(int a, int b)
  requires( true )
  ensures( result <= a && result <= b )
{
  bv_lemma(forall(int x; (x & (-1)) == x));
  bv_lemma(forall(int a,b; (a - (a - b)) == b));
  return unchecked(a - ((a - b) & -(a > b)));
}
/*`
Verification of min succeeded.
`*/
