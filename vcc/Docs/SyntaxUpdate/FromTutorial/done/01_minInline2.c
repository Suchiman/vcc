#include <vcc.h>

int main()
{
  int x,y,z;
  if (x <= y)
    z = x;
  else z = y;
  _(assert z <= x && z <= y)
  return(0);
}
/*`
Verification of main succeeded.
`*/
