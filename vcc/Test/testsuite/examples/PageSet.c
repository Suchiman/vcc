#include <stdlib.h>

#include <vcc.h>

typedef unsigned __int32 UINT32;
typedef unsigned __int64 UINT64, *PUINT64;

typedef struct _PAGE_SET
{
    UINT32 PageCount;          
    UINT32 PagesAllocated;
    PUINT64 Array;

    invariant(PagesAllocated <= PageCount)
    invariant(keeps(as_array(Array, PageCount)))
    invariant(typed(as_array(Array, PageCount)))

} PAGE_SET, *PPAGE_SET;

void Init(
    PPAGE_SET PageSet,
    UINT32 PageCount,
    UINT64 Array[]
    )
    writes(extent(PageSet), as_array(Array, PageCount))
    requires(wrapped(as_array(Array, PageCount)))
    ensures(PageSet->PageCount == PageCount)
    ensures(PageSet->PagesAllocated == 0)
    ensures(PageSet->Array == Array)
    ensures(wrapped(PageSet))
{
    PageSet->Array = Array;
    PageSet->PageCount = PageCount;
    PageSet->PagesAllocated = 0;
    wrap(PageSet);
}

void CallInit() {
  PAGE_SET *ps = (PAGE_SET *)malloc(sizeof(PAGE_SET));
  PUINT64 arr = malloc(sizeof(UINT64) * 100);
  if (ps != NULL && arr != NULL) {
    wrap(as_array(arr, 100));
    Init(ps, 100, arr);
  }
}

/*`
Verification of _PAGE_SET#adm succeeded.
Verification of Init succeeded.
Verification of CallInit succeeded.
`*/
