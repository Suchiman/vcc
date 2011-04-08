#include <vcc.h>

/*{begin}*/
int is_sorted(int *arr, unsigned len)
  _(requires \thread_local_array(arr, len))
  _(ensures \result == \forall unsigned i, j; i < j && j < len ==> arr[i] <= arr[j])
{
  if (len <= 1)
    return 1;
  if (!(arr[len - 2] <= arr[len - 1]))
    return 0;
  return is_sorted(arr, len - 1);
}
/*{end}*/
/*`
`*/
