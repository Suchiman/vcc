#include <limits.h>
#include <vcc.h>

unsigned binary_search(int val, int *buf, unsigned len)
  requires(is_thread_local_array(buf, len))                                     // buf[0..len] is valid, locally owned
  requires(forall(unsigned i,j; i < j && j < len ==> buf[i] <= buf[j]))         // buffer is sorted
  ensures(result != UINT_MAX ==> buf[result] == val)                            // val found
  ensures(result == UINT_MAX ==> forall(unsigned i; i < len ==> buf[i] != val)) // val not found
{
  unsigned low, high, mid;
  low = 0; high = len;
  while (low < high)
    invariant(high <= len)
    invariant(forall(unsigned i; i < low              ==> buf[i] <  val))       // val isn't to the left of low
    invariant(forall(unsigned i; high <= i && i < len ==> buf[i] >= val))       // val isn't to the right of high
  {
    mid = low + (high - low) / 2;
    if (buf[mid] < val)             low = mid + 1;
    else                            high = mid;
  }

  if (low < len && buf[low] == val) return low;
  else                              return UINT_MAX;
}

/*`
Verification of binary_search succeeded.
`*/