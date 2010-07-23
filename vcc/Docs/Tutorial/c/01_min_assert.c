#include <vcc.h>

int min(int a, int b)
{
  int \result; 
  // assume precondition of min(a,b)
  _(assume \true)
  if (a <= b) 
    \result = a;
  else \result = b;
  // assert postcondition of min(a,b)
  _(assert \result <= a && \result <= b)
}

int main()
{
  int \result;
  // assume precondition of main()
  _(assert \true)
  int x, y, z;
  // z = min(x,y);
  {
    int _res; 
    // assert precondition of min(x,y)
    _(assert \true)
    // assume postcondition of min(x,y) 
    _(assume _res <= x && _res <= y)
    z = _res;
  }
  _(assert z <= x)
  \result = 0;
  // assert postcondition of main()
  _(assert \true)
}

