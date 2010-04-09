
#include <vcc.h>

int foo(int *i)
  _(requires \valid(i))
  _(requires \thread_local(i))
  _(ensures \result == \old(*i))
{
  return *i;
}