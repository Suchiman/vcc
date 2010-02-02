#include <vcc.h>

#define my_mutable_array(arr, len) (forall(unsigned i; {arr + i} i < len ==> mutable(arr + i)))

/*{swap}*/
void swap(int *p, int *q)
  writes(p, q)
  requires(mutable(p) && mutable(q))
  ensures(mutable(p) && mutable(q))
  ensures(*p == old(*q) && *q == old(*p))
{
  int tmp;
  tmp = *p;
  *p = *q;
  *q = tmp;
}
/*{partition}*/
unsigned partition(int *arr, unsigned len, unsigned pivotIdx)
  writes(array_range(arr, len))
  requires(my_mutable_array(arr, len))
  requires(pivotIdx < len)
  requires(len > 0)

  ensures(my_mutable_array(arr, len))
  ensures(forall(unsigned k; {arr[k]} k < result; arr[k] <= old(arr[pivotIdx])))
  ensures(forall(unsigned k; {arr[k]} result < k && k < len; arr[k] >= old(arr[pivotIdx])))
  ensures(result < len)
{
  unsigned i, j;
  int pivot;
  
  pivot = arr[pivotIdx];

  i = 0;
  j = len - 1;
  while (i < j)
    writes(array_range(arr, len))
    invariant(my_mutable_array(arr, len))
    invariant(forall(unsigned k; {arr[k]} k < i; arr[k] <= pivot))
    invariant(forall(unsigned k; {arr[k]} j < k && k < len; arr[k] >= pivot))
    invariant(j < len)
  {
    if (arr[i] <= pivot) {
      i++;
    } else if (arr[j] >= pivot) {
      j--;
    } else {
      swap(arr + i, arr + j);
      i++;
      j--;
    }
  }

  return j;
}

#if 0
spec(ispure bool iszero(int x) returns(x == 0); )
/*{qsort}*/
void qsort(int *arr, unsigned len)
  writes(array_range(arr, len))
  requires(my_mutable_array(arr, len))
  ensures(my_mutable_array(arr, len))
  ensures(forall(unsigned i; {arr[i]} i < len - 1; arr[i] <= arr[i+1]))
  ensures(forall(unsigned k; {&arr[k]} {sk_hack(iszero(arr[k]))} k < len; exists(unsigned k0; k0 < len; arr[k] == old(arr[k0]))))
{
  unsigned idx;

  if (len <= 1) return;

  idx = partition(arr, len, len / 2);

  qsort(arr, idx);
  assert(forall(unsigned k; {&arr[k]} {sk_hack(iszero(arr[k]))} k < len; exists(unsigned k0; k0 < len; arr[k] == old(arr[k0]))));
  assume(false);
  if (idx < len)
    qsort(arr + idx + 1, len - idx - 1);
}
#endif
