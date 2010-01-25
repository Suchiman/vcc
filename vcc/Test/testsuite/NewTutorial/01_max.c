#include <vcc.h>

int max(int a, int b)
  ensures(result == (a < b ? b : a))
{
  //return a > b ? a : b;
  //Push those two to the prelude:
  bv_lemma(forall(int x; (x & (-1)) == x));
  bv_lemma(forall(int a,b; (a - (a - b)) == b));
  return unchecked(a - ((a - b) & -(a < b)));
}
