#include <vcc.h>
#include <stdio.h> // R

#define MAXLEN 1000 // R

spec(typedef mathint intmset[int];)
spec(ispure unsigned mark(unsigned i) returns(i);)

spec(ispure bool mark3(unsigned i, int e) returns(mark(i) >= 0 && mark(2*i+1) >= 0 && mark(2*i+2) >= 0);)

spec(ispure bool isHeapExcept(struct Heap *h, unsigned p)
  reads(set_universe())
  returns(
    forall(unsigned i; {h->sh[mark(i)]} i >= h->len; h->sh[mark(i)] == lambda(int e; (mathint)0)) &&
    forall(unsigned i; int e; {h->sh[mark(i)][e]} {sk_hack(mark3(i,e))}
             i < h->len; h->sh[mark(i)][e] >= 0 && (i != p ==> h->sh[i][e] > 0 ==> e >= h->heap[i])));
    )

#define isheap(h) isHeapExcept(h,h->len)
#define tcAt(h,i,e) ((e == h->heap[i] ? 1 : 0) + h->sh[2*i+1][e] + h->sh[2*i+2][e])

spec(ispure bool tcExcept(struct Heap *h, unsigned p)
  reads(set_universe())
  returns(forall(unsigned i; {h->sh[mark(i)]} i != p && i < h->len; 
    h->sh[mark(i)] == lambda(int e; tcAt(h,i,e))));
 )

struct Heap {              // R
  // public:
  unsigned len;            // R
  spec(intmset abs;)

  // private:
  int heap[MAXLEN];        // R
  spec(intmset sh[unsigned];)
  invariant(sh[0] == abs)
  invariant(len <= MAXLEN)
  invariant(tcExcept(this, len))
  invariant(isheap(this))
};                         // R

void init(struct Heap *h) // R
  writes(span(h))
  ensures(wrapped(h) && h->len == 0 && h->abs == lambda(int e; (mathint)0))
{ // R
  speconly(
    h->sh = lambda(unsigned i; int e; (mathint)0);
    h->abs = h->sh[0];
  )
  h->len = 0; // R
  wrap(h);
} // R

void insert(struct Heap *h, int elt) // R
  requires(h->len < MAXLEN - 1)
  maintains(wrapped(h))
  writes(h)
  ensures(h->abs == lambda(int k; k == elt ? old(h->abs)[k] + 1 : old(h->abs)[k]))
  ensures(h->len == old(h->len) + 1)
{ // R
  unsigned p; // R

  unwrap(h);
  p = h->len; // R
  h->len = h->len + 1; // R
  h->heap[p] = elt; // R

  assert(mark3(p,0));

  while (p > 0) // R
    invariant(unchanged(h->len) && unchanged(h->sh[0]))
    invariant(p < h->len)
    invariant(isheap(h))
    writes(span(h))
    invariant(tcExcept(h, p))
    invariant(h->sh[p] == lambda(int e; (e == elt ? -1 : 0) + tcAt(h, p, e)))
    invariant(h->heap[p] == elt)
  { // R
    unsigned p2; // R

    p2 = (p - 1) / 2; // R

    assert(mark3(p2,0));
    //assert(mark3(p,0));
    assert(mark3(2*p+1,0));
    assert(mark3(2*p+2,0));

    assert(h->sh[p2][h->heap[2*p+2]] >= 0);
    assert(h->sh[p2][h->heap[2*p+1]] >= 0);

//    assert(2*p+2 < h->len && h->heap[2*p+2] != elt ==> h->sh[p2][h->heap[2*p+2]] > 0);
//    assert(2*p+1 < h->len && h->heap[2*p+1] != elt ==> h->sh[p2][h->heap[2*p+1]] > 0);

    if (h->heap[p] < h->heap[p2]) { // R
      int tmp; // R
      tmp = h->heap[p]; // R
      h->heap[p] = h->heap[p2]; // R
      h->heap[p2] = tmp; // R
//      assert(2*p+2 < h->len ==> h->sh[p2][h->heap[2*p+2]] > 0);
//      assert(2*p+1 < h->len ==> h->sh[p2][h->heap[2*p+1]] > 0);
    } else  // R
    {
      speconly( h->sh[p] = lambda(int e; tcAt(h, p, e)); )
      p = p2; 
      break; // R
    }

    speconly( h->sh[p] = lambda(int e; tcAt(h, p, e)); )
    p = p2; // R
  } // R

  while (p > 0)
    invariant(unchanged(h->len) && unchanged(h->sh[0]))
    invariant(p < h->len)
    invariant(isheap(h))
    writes(span(h))
    invariant(tcExcept(h, p))
    invariant(h->sh[p] == lambda(int e; (e == elt ? -1 : 0) + tcAt(h, p, e)))
    invariant(h->heap[p] <= elt)
    invariant(p == 0 || h->sh[p][h->heap[p]] > 0)
  {
    unsigned p2;

    p2 = (p - 1) / 2;

//    assert(mark3(p,0));
    assert(mark3(p2,0));

    assert(h->sh[p2][h->heap[p]] > 0);

//    assert(h->heap[p] >= h->heap[p2]);

    speconly( h->sh[p] = lambda(int e; tcAt(h, p, e)); )
    p = p2;
  }

//  assert(p == 0);
  speconly( h->sh[p] = lambda(int e; tcAt(h, p, e)); )
  speconly( h->abs = h->sh[p]; )
  wrap(h);
} // R

int extractMin(struct Heap *h) // R
  requires(h->len > 0)
  maintains(wrapped(h))
  writes(h)
  ensures(old(h->abs)[result] > 0)
  ensures(forall(int e; old(h->abs)[e] > 0 ==> result <= e))
  ensures(h->abs == lambda(int k; k == result ? old(h->abs)[k] - 1 : old(h->abs)[k]))
  ensures(h->len == old(h->len) - 1)
{ // R
  int res; // R
  int last;
  unsigned p; // R

  unwrap(h);
  res = h->heap[0]; // R
  assert(mark3(0,0));
  assert(forall(int e; old(h->abs)[e] > 0 ==> res <= e));
  h->len--; // R

  p = h->len;
  assert(mark3(p,0));
  last = h->heap[p];
  assert(h->sh[p][last] == 1);
  speconly(h->sh[p] = lambda(int e; (mathint)0); )


  if (p == 0) goto end;

  p = (p - 1) / 2;
  assert(mark3(p,0));

  while (p > 0)
    invariant(unchanged(h->sh[0]))
    invariant(p < h->len)
    invariant(isheap(h))
    writes(&h->sh)
    invariant(tcExcept(h, p))
    invariant(h->sh[p] == lambda(int e; (e == last ? 1 : 0) + tcAt(h, p, e)))
  {
    assert(mark3(p,0));
    assert(mark3((p-1)/2,0));
    speconly( h->sh[p] = lambda(int e; tcAt(h, p, e)); )
    p = (p - 1) / 2;
  }

  h->heap[0] = h->heap[h->len];
  speconly( h->sh[0] = lambda(int e; tcAt(h, p, e)); )

  p = 0; // R
  while (p < h->len) // R
    invariant(unchanged(h->len))
    invariant(h->sh[0] == lambda(int k; k == res ? old(h->abs)[k] - 1 : old(h->abs)[k]))
    writes(span(h))
    invariant(isHeapExcept(h, p))
    invariant(tcExcept(h, h->len))
  { // R
    unsigned l = 2*p+1, r = 2*p+2;  // R
    unsigned smallest = p; // R
    if (l < h->len && h->heap[l] < h->heap[p]) // R
      smallest = l; // R
    if (r < h->len && h->heap[r] < h->heap[smallest]) // R
      smallest = r; // R
    if (smallest != p) { // R
      int tmp; // R
      tmp = h->heap[p]; // R
      h->heap[p] = h->heap[smallest]; // R
      h->heap[smallest] = tmp; // R
      assert(mark3(smallest, 0));
      assert(mark3(p, 0));
      speconly( h->sh[smallest] = lambda(int e; tcAt(h, smallest, e)); )
      p = smallest; // R
    } else // R
    {
      assert(isheap(h));
      break; // R
    } // R
  } // R

end: ;
  speconly( h->abs = h->sh[0]; )
  wrap(h);

  return res; // R
} // R
 

spec( typedef unsigned idxseq_t[mathint]; )
spec( typedef unsigned perm_t[unsigned]; )

#define indexesOK() \
  forall(int e; mathint i; 0 <= i && i < h.abs[e] ==> indexes[e][i] < len && old(arr[indexes[e][i]]) == e) && \
  forall(int e1, e2; mathint i1, i2; \
           0 <= i1 && 0 <= i2 && \
           i1 < h.abs[e1] && i2 < h.abs[e2] && (e1 != e2 || i1 != i2) ==> indexes[e1][i1] != indexes[e2][i2])

#define validPerm(N) \
  (forall(unsigned a, b; a < b && b < N  ==> perm[a] != perm[b]) && \
   forall(unsigned a; a < N ==> perm[a] < len && old(arr[perm[a]]) == arr[a]))

#define sorted(N) \
  (forall(unsigned a, b; a < b && b < N ==> arr[a] <= arr[b]))

void heapSort(int *arr, unsigned len // R
              spec(out perm_t perm)
             ) // R
  requires(len < MAXLEN)
  maintains(is_mutable_array(arr, len))
  writes(array_range(arr, len))
  ensures(validPerm(len) && sorted(len))
{ // R
  struct Heap h; // R
  unsigned i; // R
  spec( idxseq_t indexes[int]; )

  init(&h); // R

  for (i = 0; i < len; ++i) // R
    writes(&h)
    invariant(i <= len)
    invariant(wrapped(&h) && h.len == i)
    invariant(indexesOK())
    invariant(forall(int e; mathint k; 0 <= k && k < h.abs[e] ==> indexes[e][k] < i))
  {
    insert(&h, arr[i]); // R
    speconly( indexes[arr[i]][h.abs[arr[i]] - 1] = i; )
  }

  assume(is_mutable_array(arr, len)); // TODO // A

  for (i = 0; i < len; ++i) // R
    writes(&h, array_range(arr, len))
    invariant(i <= len)
    invariant(wrapped(&h) && h.len == len - i)
    invariant(is_mutable_array(arr, len))
    invariant(indexesOK())
    invariant(forall(int e; mathint a; unsigned b; 0 <= a && a < h.abs[e] && b < i ==> indexes[e][a] != perm[b]))
    invariant(forall(unsigned a; int e; a < i; h.abs[e] > 0 ==> e >= arr[a]))
    invariant(validPerm(i) && sorted(i))
  {
    arr[i] = extractMin(&h); // R
    speconly( perm[i] = indexes[arr[i]][h.abs[arr[i]]]; )
  }

  unwrap(&h);
}

#define ASSERT(E) { bool tmp = E; assert(tmp); } // A
void heapSortTestHarness() // R
{
  int arr[] = { 42, 13, 42 }; // R
  spec(unsigned perm[unsigned];)
  heapSort(arr, 3 // R
           spec(out perm)
          );  // R
  ASSERT(arr[0] <= arr[1] && arr[1] <= arr[2]); // R
  ASSERT(arr[0] == 13 && arr[1] == 42 && arr[2] == 42); // R
} // R

#ifndef VERIFY                                // A
int main()                                    // A
{                                             // A
  unsigned i;                                 // A
  int arr[] = { 3, 1, 2, 42, 0, 4 };          // A
  #define LEN (sizeof(arr) / sizeof(arr[0]))  // A
  heapSort(arr, LEN);                         // A
  for (i = 0; i < LEN; ++i)                   // A
    printf("%d ", arr[i]);                    // A
  printf("\n");                               // A
}                                             // A 
#endif                                        // A
