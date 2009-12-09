/*
 * This file provides a sample implementation of singly-linked lists.
 */
#include "vcc.h"

/*
 * Required forward definitions/declarations.
 */
#define NULL    (void*)0
#define MAXUINT 0x7FFFFFFF

#ifdef VERIFY
/*
 * Forward declaration of ghost type _SINGLE_LIST_MANAGER.
 */
typedef struct _SINGLE_LIST_MANAGER SINGLE_LIST_MANAGER, *PSINGLE_LIST_MANAGER;
#endif

/*
 * Define singly-linked list entry.
 */
typedef struct _SINGLE_LIST_ENTRY
{
    struct _SINGLE_LIST_ENTRY *Next;

    // For proving the structure of the list, we also need a pointer
    // to the previous item in the list.
    spec(struct _SINGLE_LIST_ENTRY *Back;)
    // Each list entry contains a back link to its corresponding list manager.
    spec(SINGLE_LIST_MANAGER ^Manager;)
} SINGLE_LIST_ENTRY, *PSINGLE_LIST_ENTRY;

#ifdef VERIFY
/*
 * This ghost type _SINGLE_LIST_MANAGER is used to manage the list entries in an abstract way.
 * The list manager is the owner of all list entries and contains a pointer to that
 * designated SINGLE_LIST_ENTRY, that is considered as the list head.
 * The list structure is modeled by a map of pointers to SINGLE_LIST_ENTRY structs to integer
 * values in the range of 0 to size-1, which represent the position of the pointer in
 * the list.
 */
spec(
struct vcc(dynamic_owns) _SINGLE_LIST_MANAGER
{
    // Number of entries in the list
    unsigned int size;
    // Pointer to the designated list head
    spec(PSINGLE_LIST_ENTRY ListHead;)
    // A map for the housekeeping of the order of referenced pointers in the list.
    spec(unsigned int index[PSINGLE_LIST_ENTRY];)

    // All objects are of the type SINGLE_LIST_ENTRY
    invariant(forall(obj_t p; {set_in(p,owns(this))}
        set_in(p,owns(this)) ==> is(p,SINGLE_LIST_ENTRY) && typed_phys(p)))

    // The "Manager" back-pointer of each LIST_ENTRY points back to this list manager.
    invariant(forall(PSINGLE_LIST_ENTRY p; {set_in(p,owns(this))}
        set_in(p,owns(this)) ==> p->Manager == this))

    // The ListHead is owned by the list manager.
    invariant(set_in(ListHead,owns(this)))

    // Each list entry, that can be reached via a Next is also in the ownership
    // domain of the list manager. Additionally each Back of an entry p->Next points
    // back to p.
    invariant(forall(PSINGLE_LIST_ENTRY p; {set_in(p->Next, owns(this))} {sk_hack(set_in(p->Next, owns(this)))}
        set_in(p,owns(this)) && p->Next != NULL ==> set_in(p->Next,owns(this)) && p->Next->Back == p))
    // Each list entry, that can be reached via a Back is also in the ownership
    // domain of the list manager. Additionally each Next of an entry p->Back points
    // back to p.
    invariant(forall(PSINGLE_LIST_ENTRY p; {set_in(p->Back, owns(this))} {sk_hack(set_in(p->Back, owns(this)))}
        set_in(p,owns(this)) && p->Back != NULL ==> set_in(p->Back,owns(this)) && p->Back->Next == p))

    // The index[] map always increases by 1 for each object that can be reached by
    // an Flink pointer. Except if the Flink points to the list head, which implies
    // the end of the list.
    //
    // The {sk_hack(set_in(p->Next,owns(this)))} trigger introduces a witness of
    // an set_in(p->Next,owns(this)) entry, that is required for the prove to succeed.
    invariant(forall(PSINGLE_LIST_ENTRY p; {set_in(p, owns(this))} {sk_hack(set_in(p->Next,owns(this)))}
        set_in(p,owns(this)) && p->Next != NULL ==> index[p] + 1 == index[p->Next]))

    // Specify index[] for well known objects.
    invariant(index[ListHead] == 0)

    // Specify range of the index[] map.
    invariant(forall(PSINGLE_LIST_ENTRY e; {set_in(e,owns(this))}
        set_in(e,owns(this)) ==> 0 <= index[e] && index[e] < size))

    // Each element in the list is only contained once.
    invariant(forall(PSINGLE_LIST_ENTRY e1, e2; {set_in(e1,owns(this)), set_in(e2,owns(this))}
        set_in(e1,owns(this)) && set_in(e2,owns(this)) && e1 != e2 ==> index[e1] != index[e2]))

};
)

typedef struct _SINGLE_LIST_MANAGER SINGLE_LIST_MANAGER, *PSINGLE_LIST_MANAGER;

#endif


/**
 * InitializeSingleListHead
 * ========================
 *
 * The InitializeSingleListHead initalises an entry of type PSINGLE_LIST_ENTRY as
 * list head of a singly-linked list.
 *
 * Parameters:
 *   ListHead : Pointer to the SINGLE_LIST_ENTRY structure that represents the head
 *              of the list. On return, ListHead->Next points to NULL.
 *
 * Return Value:
 *   None
 */
void InitializeSingleListHead( PSINGLE_LIST_ENTRY ListHead )
    requires(mutable(ListHead))
    writes(extent(ListHead))
    ensures(set_eq(owns(ListHead->Manager),SET(ListHead)))
    ensures(ListHead->Manager->ListHead == ListHead)
    ensures(ListHead->Manager->size == 1)
    ensures(wrapped(ListHead->Manager))
    ensures(is_fresh(ListHead->Manager))
{
    spec(SINGLE_LIST_MANAGER ^ListManager;)
    ListHead->Next = NULL;
    speconly(ListHead->Back = NULL;)

speconly(
    ListManager = spec_malloc<SINGLE_LIST_MANAGER>();
    ListManager->size = 1;
    ListManager->index[ListHead] = 0;
    ListManager->ListHead = ListHead;

    set_owns(ListHead,SET());
    set_owns(ListManager,SET(ListHead));
    ListHead->Manager = ListManager;
    wrap(ListHead);
    wrap(ListManager);
    )
}


/**
 * PopEntryList
 * ============
 *
 * The PopEntryList routine removes the first entry from a singly linked list of
 * SINGLE_LIST_ENTRY structures.
 *
 * Parameters:
 *   ListHead : Pointer to the SINGLE_LIST_ENTRY structure that represents the head
 *              of the list. On return, ListHead->Next points to the beginning of
 *              the list with the first entry removed.
 *
 * Return Value:
 *   PopEntryList returns a pointer to the entry removed from the list,
 *   or NULL if the list is currently empty.
 */
PSINGLE_LIST_ENTRY PopEntryList( PSINGLE_LIST_ENTRY ListHead )
    requires(set_in(ListHead, owns(ListHead->Manager)))
    requires(ListHead == ListHead->Manager->ListHead)
    requires(closed(ListHead))
    maintains(wrapped(ListHead->Manager))
    ensures(old(ListHead->Manager->size) == 1 ==> result == NULL)
    ensures(old(ListHead->Manager->size) > 1 ==> result == old(ListHead->Next))
    ensures(result == NULL ==> unchanged(ListHead->Manager->size))
    ensures(result == NULL ==> set_equal(owns(ListHead->Manager),old(owns(ListHead->Manager))))
    ensures(result != NULL ==> ListHead->Manager->size == old(ListHead->Manager->size) - 1)
    ensures(result != NULL ==> set_equal(owns(ListHead->Manager),set_difference(old(owns(ListHead->Manager)),SET(result))))
    ensures(result != NULL ==> wrapped(result))
    writes(ListHead->Manager)
{
    PSINGLE_LIST_ENTRY FirstEntry;
    spec(SINGLE_LIST_MANAGER ^ListManager = ListHead->Manager;)

    assert(in_domain(ListHead,ListManager));
    assert(ListHead->Next != NULL ==> in_domain(ListHead->Next,ListManager));

    FirstEntry = ListHead->Next;
    if (FirstEntry != NULL)
    {
        assert(ListManager->size > 1);
        assert(FirstEntry->Next != NULL ==> in_domain(FirstEntry->Next, ListManager));
        expose(ListManager) {
            expose(ListHead) {
                ListHead->Next = FirstEntry->Next;
speconly(
                if (ListHead->Next != NULL) {
                    expose(ListHead->Next) {
                        ListHead->Next->Back = ListHead;
                    }
                }
)
            }
speconly(
            ListManager->size = ListManager->size - 1;
            giveup_owner(FirstEntry,ListManager);
            ListManager->index = lambda(PSINGLE_LIST_ENTRY x; set_in(x,owns(ListManager));
                                        (x == ListHead) ? 0 : ListManager->index[x]-1);
)
            assert(old(ListManager->size) > 0);
            assert(ListManager->size == old(ListManager->size) - 1);
        }
    }
    return FirstEntry;
}


/**
 * PushEntryList
 * =============
 *
 * The PushEntryList routine inserts an entry at the beginning of a singly-linked list
 * of SINGLE_LIST_ENTRY structures.
 *
 * Parameters:
 *   ListHead : Pointer to the SINGLE_LIST_ENTRY structure that serves as the list header.
 *   Entry    : Pointer to SINGLE_LIST_ENTRY structure that represents the entry to be
 *              inserted on the list.
 *
 * Return Value:
 *   None
 */
void
PushEntryList( PSINGLE_LIST_ENTRY ListHead, PSINGLE_LIST_ENTRY Entry )
    maintains(wrapped(ListHead->Manager))
    maintains(set_in(ListHead, owns(ListHead->Manager)))
    requires(ListHead == ListHead->Manager->ListHead)
    requires(mutable(Entry))
    requires(ListHead->Manager->size < MAXUINT)
    ensures(set_in(Entry, owns(ListHead->Manager)))
    ensures(ListHead->Manager->size == old(ListHead->Manager->size) + 1)
    ensures(set_eq(owns(ListHead->Manager), set_union(old(owns(ListHead->Manager)), SET(Entry))))
    writes(ListHead->Manager, extent(Entry))
{
    spec(SINGLE_LIST_MANAGER ^ListManager = ListHead->Manager;)

    assert(in_domain(ListHead, ListManager));
    assert(ListHead->Next != NULL ==> in_domain(ListHead->Next, ListManager));

    expose(ListManager) {
        Entry->Next = ListHead->Next;

speconly(
        set_owns(Entry, SET());
        Entry->Back = ListHead;
        Entry->Manager = ListManager;
        wrap(Entry);

        if (Entry->Next != NULL) {
            expose(Entry->Next) {
                Entry->Next->Back = Entry;
            }
        }
)
        expose(ListHead) {
            ListHead->Next = Entry;
        }

speconly(
        ListManager->size = ListManager->size + 1;
        set_owner(Entry,ListManager);
        ListManager->index = lambda(PSINGLE_LIST_ENTRY x; set_in(x,owns(ListManager));
                                    (x==Entry)?ListManager->index[ListHead] + 1 :
                                        ((ListManager->index[x]<= ListManager->index[ListHead])?ListManager->index[x]:ListManager->index[x] + 1 ));
)
    }
}

/*`
Verification of _SINGLE_LIST_MANAGER#adm succeeded.
Verification of InitializeSingleListHead succeeded.
Verification of PopEntryList succeeded.
Verification of PushEntryList succeeded.
`*/
