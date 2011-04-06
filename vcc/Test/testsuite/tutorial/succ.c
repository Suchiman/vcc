#include "vcc.h"
#include <limits.h>

int succ(int i)
  ensures(result == i+1)
{
	return i+1;
}

int succ_INT_MAX(int i)
  requires(i<INT_MAX)
  ensures(result == i+1)
{
	return i+1;
}

int succ_unchecked(int i)
  ensures(result == unchecked(i+1))
{
	return unchecked(i+1);
}

/*`
Verification of succ failed.
testcase(7,9) : error VC8004: i+1 might overflow.
Verification of succ_INT_MAX succeeded.
Verification of succ_unchecked succeeded.
`*/
