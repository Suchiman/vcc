#include <vcc.h>

/*{beginsp}*/
_(logic bool sorted(int *arr, unsigned len) =
  \forall unsigned i, j; i <= j && j < len ==> arr[i] <= arr[j])
/*{endsp}*/
/*{beginso}*/
void sort(int *arr, unsigned len)
  _(writes \array_range(arr, len))
  _(ensures sorted(arr, len))
/*{endso}*/;
/*{beginim}*/
int check_sorted(int *arr, unsigned len)
  _(requires \thread_local_array(arr, len))
  _(ensures \result == sorted(arr, len))
{
  if (len <= 1)
    return 1;
  if (!(arr[len - 2] <= arr[len - 1]))
    return 0;
  return is_sorted(arr, len - 1);
}
/*{endim}*/
/*`
`*/
