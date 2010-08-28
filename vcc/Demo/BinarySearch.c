#include <vcc.h>
#include <limits.h>

unsigned binary_search(int val, int *arr, unsigned len) 
  requires(is_mutable_array(arr, len))
  requires(forall(unsigned i,j; i < j && j < len ==> arr[i] <= arr[j]))
  ensures(result != UINT_MAX ==> arr[result] == val)
  ensures(result == UINT_MAX ==> forall(unsigned i; i < len ==> arr[i] != val))
{
  unsigned low, high, mid;
  low = 0; high = len;
  while (low < high)
    invariant(high <= len)
    invariant(forall(unsigned i; i < low              ==> arr[i] <  val))
    invariant(forall(unsigned i; high <= i && i < len ==> arr[i] >= val))
  {
    mid = low + (high - low) / 2;
    if (arr[mid] < val)             low = mid + 1;
    else                            high = mid;
  }

  if (low < len && arr[low] == val) return low;
  else                              return UINT_MAX;
}

/*`
Verification of binary_search succeeded.
`*/
