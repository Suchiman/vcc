#include <vcc.h>

int min(int a, int b)
{
  int res;
  _(assume \true)
  if (a <= b) 
    res = a;
  else res = b;
  _(assert res <= a && res <= b)
}

int main()
{
  int x, y, z;
  _(assert \true)
  z = min(x, y);
  _(assume z <= x && z <= y)
  _(assert z <= x)
  return 0;
}

/*`
Verification of min succeeded.
Verification of main succeeded.
`*/
