#include <vcc.h>

_(logic bool sorted(int *buf, unsigned len) =
  \forall unsigned i, j; i < j && j < len ==> buf[i] <= buf[j])

_(typedef unsigned perm_t[unsigned]; )

_(_(pure) bool match_ij(unsigned i, unsigned j) _(returns \true); )

_(logic bool is_perm(perm_t perm, unsigned len) =
  (\forall unsigned i, j; {perm[i],perm[j]} {match_ij(i,j)} 
    match_ij(i+1,j) && match_ij(i-1,j) && 
    match_ij(i,j+1) && match_ij(i,j-1) && 
    match_ij(i,j) && i < j && j < len ==> perm[i] != perm[j]))

_(logic bool is_permuted(\state s, int *buf, unsigned len, perm_t perm) =
  \forall unsigned i; {perm[i]} i < len ==> perm[i] < len && \in_state(s, buf[ perm[i] ]) == buf[i])

//_(_(pure) perm_t update_perm(perm_t p, unsigned i, unsigned v)
//  _(returns \lambda unsigned k; i == k ? v : p[k]))

void insertion_sort(int *buf, unsigned len _(out perm_t perm))
  _(requires len < 100)
  _(writes \array_range(buf, len))
  _(ensures sorted(buf, len))
  _(ensures is_perm(perm, len))
  _(ensures is_permuted(\old(\current_state()), buf, len, perm))
{
  unsigned i, j;
  int v;
  _(ghost \state s0 = \current_state() )

  _(ghost perm = \lambda unsigned i; i) 

  for (i = 1; i < len; ++i)
    _(invariant sorted(buf, i))
    _(invariant \mutable_array(buf, len))
    _(invariant is_perm(perm, len))
    _(invariant is_permuted(s0, buf, len, perm))
  {
    _(ghost unsigned tmp = perm[i])
    v = buf[i];
    j = i - 1;
    for (;;) 
      _(invariant is_perm((\lambda unsigned k; k == j + 1 ? tmp : perm[k]), len))
      _(invariant is_permuted(s0, buf, len, perm))
      _(invariant j <= i - 1)
      _(invariant sorted(buf, i))
      _(invariant (j == i - 1 && buf[i] == v) || (sorted(buf, i + 1) && perm[j + 1] == perm[j + 2]))
      _(invariant \forall unsigned k; j < k && k <= i ==> buf[k] >= v)
      _(invariant \mutable_array(buf, len))
      _(writes \array_range(buf, i + 1))
    {
      if (buf[j] > v) {
        buf[j + 1] = buf[j];
        _(ghost perm[j + 1] = perm[j] )
        if (_(unchecked)(j--) == 0) break;
      } else 
        break;
    }
    buf[_(unchecked)(j + 1)] = v;
    _(ghost perm[_(unchecked)(j + 1)] = tmp)
  }
}
