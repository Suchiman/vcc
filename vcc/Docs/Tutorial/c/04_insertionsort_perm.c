#include <vcc.h>

_(logic bool sorted(int *buf, unsigned len) =
  \forall unsigned i, j; i < j && j < len ==> buf[i] <= buf[j])

_(typedef unsigned perm_t[unsigned]; )

_(logic bool is_permutation(perm_t perm, unsigned len) =
  (\forall unsigned i, j;
    i < j && j < len ==> perm[i] != perm[j]))

_(logic bool is_permuted(\state s, int *buf, unsigned len, perm_t perm) =
  \forall unsigned i; {perm[i]} i < len ==> perm[i] < len && \in_state(s, buf[ perm[i] ]) == buf[i])

void insertion_sort(int *buf, unsigned len _(out perm_t perm))
  _(writes \array_range(buf, len))
  _(ensures sorted(buf, len))
  _(ensures is_permutation(perm, len))
  _(ensures is_permuted(\old(\current_state()), buf, len, perm))
{
  unsigned i, j;
  int v;
  _(ghost \state s0 = \current_state() )
  _(ghost perm_t perm2 )

  _(ghost perm = \lambda unsigned i; i) 

  for (i = 1; i < len; ++i)
    _(invariant sorted(buf, i))
    _(invariant \mutable_array(buf, len))
    _(invariant is_permutation(perm, len))
    _(invariant is_permuted(s0, buf, len, perm))
  {
    _(ghost unsigned tmp = perm[i])
    v = buf[i];
    j = i - 1;
    _(ghost perm2 = perm )
    for (;;) 
      _(invariant is_permutation(perm2, len))
      _(invariant \forall unsigned k; perm2[k] == (k == j + 1 ? tmp : perm[k]))
      _(invariant is_permuted(s0, buf, len, perm))
      _(invariant j <= i - 1)
      _(invariant sorted(buf, i))
      _(invariant j == i - 1 || buf[i - 1] <= buf[i])
      _(invariant \forall unsigned k; j < k && k <= i ==> buf[k] >= v)
      _(invariant \mutable_array(buf, len))
    {
      if (buf[j] > v) {
        buf[j + 1] = buf[j];
        _(ghost perm[j + 1] = perm[j] )
        _(assert perm2[j + 1] == tmp)
        _(ghost perm2[j + 1] = perm2[j] )
        _(ghost perm2[j] = tmp )
        if (_(unchecked)(j--) == 0) break;
      } else 
        break;
    }
    buf[_(unchecked)(j + 1)] = v;
    _(ghost perm[_(unchecked)(j + 1)] = tmp)
    _(assert perm2 == perm)
  }
}
