#include <vcc.h>

#define my_mutable_array(arr, len) (forall(unsigned i; {arr + i} i < len ==> mutable(arr + i)))

/*{swap}*/
void swap(int *p, int *q)
  writes(p, q)
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
  ensures(forall(unsigned k; {arr[k]} k < result; arr[k] <= arr[result]))
  ensures(arr[result] == old(arr[pivotIdx]))
  ensures(forall(unsigned k; {arr[k]} result < k && k < len; arr[k] > arr[result]))
  ensures(result <= len)
  ensures(forall(unsigned k; {&arr[k]} k < len; exists(unsigned k0; k0 < len; arr[k] == old(arr[k0]))))
{
  unsigned i, storeIdx;
  int pivot;
  // next two lines will go away, replaced with a predefined label
  spec(state_t pre;)
  speconly(pre = current_state(); )
  
  pivot = arr[pivotIdx];
  storeIdx = 0;

  swap(arr + pivotIdx, arr + len - 1);

  for (i = 0; i < len - 1; i++)
    writes(array_range(arr, len - 1))
    invariant(my_mutable_array(arr, len))
    invariant(forall(unsigned k; {arr[k]} k < storeIdx; arr[k] <= pivot))
    invariant(forall(unsigned k; {arr[k]} storeIdx <= k && k < i; arr[k] > pivot))
    invariant(storeIdx <= i && i <= len - 1)
    invariant(forall(unsigned k; {arr[k]} k < len; exists(unsigned k0; k0 < len; arr[k] == in_state(pre, arr[k0]))))
  {
    if (arr[i] <= pivot) {
      swap(arr + i, arr + storeIdx);
      storeIdx++;
    }
  }

  swap(arr + storeIdx, arr + len - 1);

  return storeIdx;
}
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
