//`/newsyntax
#include "02_rand.c"

/*{begin}*/
_(logic bool sorted(int *buf, unsigned len) =
  \forall unsigned i, j; i < j && j < len ==> buf[i] <= buf[j])

void bozo_sort(int *buf, unsigned len)
  _(writes \array_range(buf, len))
  _(ensures sorted(buf, len))
{
  if (len == 0) return;

  for (;;)
    _(invariant \mutable_array(buf, len))
  {
    int tmp;
    unsigned i = random(len), j = random(len);

    tmp = buf[i];
    buf[i] = buf[j];
    buf[j] = tmp;

    for (i = 0; i < len - 1; ++i)
      _(invariant sorted(buf, i + 1))
    {
      if (buf[i] > buf[i + 1]) break;
    }

    if (i == len - 1) break;
  }
}
/*{out}*/
/*`
Verification of random succeeded.
Verification of bozo_sort succeeded.
`*/
