
#include <vcc.h>

typedef unsigned __int32 UINT32;
typedef unsigned __int64 UINT64;
typedef void VOID, *PVOID;

#define NULL ((void*)0)

typedef struct _ENTRY
{
    union
    {
        struct
        {
            UINT64 : 6;
            UINT64 f : 1;
            UINT64 g : 1;
            UINT64 h: 1;
            UINT64 : 1;
            UINT64 : 11;
            UINT64 : 3;
            UINT64 : 40;
        };

        backing_member UINT64 AsUINT64;
    };

    union
    {
        backing_member struct _ENTRY *NextPfn;
        UINT64 Available2;
    };
    
    invariant(keeps())
} ENTRY, *PENTRY;

typedef struct vcc(dynamic_owns) _RESERVE
{
    void *Compartment;
    void *ListHead;   
    UINT64 ListDepth;

    spec(UINT64 ListIndex[struct _ENTRY *];)
    spec(PENTRY _ListHead;)
    
    invariant(Compartment != NULL)
    invariant(_ListHead == (PENTRY)ListHead)
    invariant(_ListHead == NULL <==> ListDepth == 0)
    invariant(_ListHead == NULL || set_in0(_ListHead, owns(this)))
    invariant(forall(obj_t p; {set_in0(p,owns(this))} set_in0(p, owns(this)) ==> is(p, ENTRY) && typed(p)))
    invariant(forall(PENTRY p; { set_in0(p->NextPfn, owns(this)) } set_in0(p, owns(this)) ==> ((p->NextPfn == NULL) || set_in0(p->NextPfn, owns(this)))))
    invariant(forall(PENTRY p; { set_in0(p, owns(this)) } set_in0(p, owns(this)) ==> (p->NextPfn == NULL) || ListIndex[p] == ListIndex[p->NextPfn] + 1))
    invariant(forall(PENTRY p; { set_in0(p, owns(this)) } set_in0(p, owns(this)) ==> (ListIndex[p] == 0 <==> p->NextPfn == NULL)))
    invariant(_ListHead != NULL ==> ListIndex[_ListHead] + 1 == ListDepth)
    invariant(forall(PENTRY p; { set_in0(p, owns(this)) } set_in0(p, owns(this)) ==> ListDepth > ListIndex[p]))
    invariant(forall(PENTRY p; { set_in0(p, owns(this)) } set_in0(p, owns(this)) ==> p->h&& (p->f || p->g)))
    
} RESERVE, *PRESERVE;

void
Check(PRESERVE Reserve)
    maintains(wrapped(Reserve))
{
    UINT64 pageCount;
    PENTRY pfn;

    assert(Reserve != NULL);
    assert(Reserve->Compartment != NULL);
        
    for (pfn = (PENTRY) Reserve->ListHead, pageCount = Reserve->ListDepth;
         pageCount != 0;
         pfn = pfn->NextPfn, pageCount--)
    invariant(pfn == NULL || set_in0(pfn, owns(Reserve)))
    invariant(pfn == NULL <==> pageCount == 0)
    invariant(pfn == NULL || pageCount == Reserve->ListIndex[pfn] + 1)
    {
      assert(pfn != NULL);
      assert(pfn->h);
      assert(pfn->f || pfn->g);
    }

    assert(pfn == NULL);
    return;
}

/*`
Verification of _ENTRY#adm succeeded.
Verification of _RESERVE#adm succeeded.
Verification of Check succeeded.
`*/
