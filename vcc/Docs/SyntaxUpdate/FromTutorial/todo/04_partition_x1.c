//`/newsyntax
#include <vcc.h>

#define my_mutable_array(arr, len) (\forall unsigned i; {arr + i} i < len ==> \mutable(arr + i))

/*{swap}*/
void swap(int *p, int *q)
  _(writes p, q)
  _(ensures \mutable(p) && \mutable(q))
  _(ensures *p == \old(*q) && *q == \old(*p))
{
  int tmp;
  tmp = *p;
  *p = *q;
  *q = tmp;
}
/*{partition}*/
unsigned partition(int *arr, unsigned len, unsigned pivotIdx)
  _(writes \array_range(arr, len))
  _(requires my_mutable_array(arr, len))
  _(requires pivotIdx < len)
  _(requires len > 0)

  _(ensures my_mutable_array(arr, len))
  _(ensures \forall unsigned k; {arr[k]} k < \result ==> arr[k] <= arr[\result])
  _(ensures arr[\result] == \old(arr[pivotIdx]))
  _(ensures \forall unsigned k; {arr[k]} \result < k && k < len ==> arr[k] > arr[\result])
  _(ensures \result <= len)
  _(ensures \forall unsigned k; {&arr[k]} k < len ==> \exists unsigned k0; k0 < len && arr[k] == \old(arr[k0]))
{
  unsigned i, storeIdx;
  int pivot;
  // next two lines will go away, replaced with a predefined label
  _(ghost \state pre;)
  _(ghost pre = \now();)
  
  pivot = arr[pivotIdx];
  storeIdx = 0;

  swap(arr + pivotIdx, arr + len - 1);

  for (i = 0; i < len - 1; i++)
    _(writes \array_range(arr, len - 1))
    _(invariant my_mutable_array(arr, len))
    _(invariant \forall unsigned k; {arr[k]} k < storeIdx ==> arr[k] <= pivot)
    _(invariant \forall unsigned k; {arr[k]} storeIdx <= k && k < i ==> arr[k] > pivot)
    _(invariant storeIdx <= i && i <= len - 1)
    _(invariant \forall unsigned k; {arr[k]} k < len ==> \exists unsigned k0; k0 < len && arr[k] == \at(pre, arr[k0]))
  {
    if (arr[i] <= pivot) {
      swap(arr + i, arr + storeIdx);
      storeIdx++;
    }
  }

  swap(arr + storeIdx, arr + len - 1);

  return storeIdx;
}
_(ghost _(pure) bool iszero(int x) _(returns x == 0);)
/*{qsort}*/
void qsort(int *arr, unsigned len)
  _(writes \array_range(arr, len))
  _(requires my_mutable_array(arr, len))
  _(ensures my_mutable_array(arr, len))
  _(ensures \forall unsigned i; {arr[i]} i < len - 1 ==> arr[i] <= arr[i+1])
  _(ensures \forall unsigned k; {&arr[k]} {:hint iszero(arr[k])} k < len ==> \exists unsigned k0; k0 < len && arr[k] == \old(arr[k0]))
{
  unsigned idx;

  if (len <= 1) return;

  idx = partition(arr, len, len / 2);

  qsort(arr, idx);
  _(assert \forall unsigned k; {&arr[k]} {:hint iszero(arr[k])} k < len ==> \exists unsigned k0; k0 < len && arr[k] == \old(arr[k0]))
  _(assume \false)
  if (idx < len)
    qsort(arr + idx + 1, len - idx - 1);
}

/*`
Verification of swap succeeded.
Verification of partition succeeded.
Verification of qsort succeeded.
`*/
