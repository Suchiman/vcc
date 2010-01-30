#include <vcc.h>

int min(int a, int b)
{
  int res;
  assume(true);
  res = a < b ? a : b;
  assert(res <= a && res <= b);
}

#define LIMIT 1000
int main()
{
  int position = 0, newPos;
  // ...
  assert(true);
  position = min(newPos, LIMIT);
  assume(position <= newPos && position <= LIMIT);
  assert(position <= LIMIT);
  // ...
  return 0;
}

/*`
Verification of min succeeded.
Verification of main succeeded.
`*/
