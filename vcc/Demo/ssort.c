//`/newsyntax
#include <vcc.h>

void ssort(int *a, unsigned int len)
  _(writes \array_range(a, len))
  _(ensures \forall unsigned int n, m; n < m && m < len ==> a[n] <= a[m])
{
  unsigned int i, j, k;
  int tmp;
  for (i = 0; i < len; i++)
    _(invariant \forall unsigned int n, m; n < m && m < i ==> a[n] <= a[m]) // sorted up to i
    _(invariant \forall unsigned int n, m; n < i && i <= m && m < len ==> a[n] <= a[m]) // only larger values after i
  {
    for (j = i, k = i; j < len; j++)
      _(invariant i <= j && j <= len && i <= k && k < len)
      _(invariant \forall unsigned int n; i <= n && n < j ==> a[k] <= a[n])
    {
      if (a[j] < a[k]) k = j;
    }

    tmp = a[i];
    a[i] = a[k];
    a[k] = tmp;
  }
}

void foo() {
  int a[10] = {6, 5, 4, 3, 2, 3, 4, 5, 7, 8};
  ssort(a, 10);
  _(assert a[2] <= a[4])
}

/*`
Verification of ssort succeeded.
Verification of foo succeeded.
`*/
