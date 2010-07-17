#include <vcc.h>
#include <stdlib.h>

_(logic bool sorted(int *buf, unsigned len) =
  \forall unsigned i, j; i < j && j < len ==> buf[i] <= buf[j])

_(typedef unsigned perm_t[unsigned]; )

_(logic bool is_permutation(perm_t perm, unsigned len) =
  (\forall unsigned i, j;
    i < j && j < len ==> perm[i] != perm[j]))

_(logic bool is_permuted(\state s, int *buf, unsigned len, 
                         perm_t perm) =
  \forall unsigned i; i < len ==> 
      perm[i] < len && \at(s, buf[ perm[i] ]) == buf[i])

_(logic perm_t swap(perm_t p, unsigned i, unsigned j) =
      \lambda unsigned k; k == i ? p[j] : k == j ? p[i] : p[k])

void bozo_sort(int *buf, unsigned len _(out perm_t perm))
  _(writes \array_range(buf, len))
  _(ensures sorted(buf, len))
  _(ensures is_permutation(perm, len))
  _(ensures is_permuted(\old(\now()), buf, len, perm))
{
  _(ghost \state s0 = \now() )

  _(ghost perm = \lambda unsigned i; i) 

  if (len == 0) return;

  for (;;)
    _(invariant \mutable_array(buf, len))
    _(invariant is_permutation(perm, len))
    _(invariant is_permuted(s0, buf, len, perm))
  {
    int tmp;
    unsigned i = _(unchecked)((unsigned)rand()) % len; 
    unsigned j = _(unchecked)((unsigned)rand()) % len; 

    tmp = buf[i];
    buf[i] = buf[j];
    buf[j] = tmp;
    _(ghost perm = swap(perm, i, j) )

    for (i = 0; i < len - 1; ++i)
      _(invariant sorted(buf, i + 1))
    {
      if (buf[i] > buf[i + 1]) break;
    }

    if (i == len - 1) break;
  }
}

/*`
Verification of bozo_sort succeeded.
`*/
