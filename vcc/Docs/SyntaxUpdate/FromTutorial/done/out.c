#include <vcc.h>

void foo(_(out int o)) 
  _(ensures o == 5)
{
  o = 5;
}		

/*
void bar() {
  spec(int p;)
  foo(spec(out p));
  assert(p == 5);
}
*/