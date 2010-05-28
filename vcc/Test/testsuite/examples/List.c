/*
 * This file provides a sample implementation of doubly-linked lists.
 */
#include "vcc.h"

/*
 * Required forward definitions/declarations.
 */
#define MAXUINT 0x7FFFFFFF

/*
 * Forward declaration of ghost type _LIST_MANAGER.
 */
spec(typedef struct _LIST_MANAGER LIST_MANAGER, ^PLIST_MANAGER;)

/*
 * Define the doubly-linked list type.
 */
typedef struct _LIST_ENTRY
{
    struct _LIST_ENTRY *Flink;
    struct _LIST_ENTRY *Blink;

    // Each list entry contains a back link to its corresponding list manager.
    spec(PLIST_MANAGER Manager;)
} LIST_ENTRY, *PLIST_ENTRY;

/*
 * This ghost type _LIST_MANAGER is used to manage the list entries in an abstract way.
 * The list manager is the owner of all list entries and contains a pointer to that
 * designated LIST_ENTRY, that is considered as the list head.
 * The list structure is modeled by a map of pointers to LIST_ENTRY structs to integer
 * values in the range of 0 to size-1, which represent the position of the pointer in
 * the list.
 */
spec(
struct vcc(dynamic_owns) _LIST_MANAGER
{
    // Number of entries in the list
    unsigned size;
    // Pointer to the designated list head
    spec(PLIST_ENTRY ListHead;)
    // A map for the housekeeping of the order of referenced pointers in the list.
    spec(unsigned index[PLIST_ENTRY];)

    // The "Manager" back-pointer of each LIST_ENTRY points back to this list manager.
    invariant(forall(PLIST_ENTRY p; {keeps(p)}
        keeps(p) ==> p->Manager == this))

    // The ListHead is owned by the list manager.
    invariant(keeps(ListHead))

    // Each list entry, that can be reached via a Flink is also in the ownership
    // domain of the list manager. Additionally each Blink of an entry p->Flink points
    // back to p.
    invariant(forall(PLIST_ENTRY p; {keeps(p->Flink)} {sk_hack(keeps(p->Flink))}
        keeps(p) ==> keeps(p->Flink) && p->Flink->Blink == p))

    // Each list entry, that can be reached via a Blink is also in the ownership
    // domain of the list manager. Additionally each Flink of an entry p->Blink points
    // back to p.
    invariant(forall(PLIST_ENTRY p; {keeps(p->Blink)} {sk_hack(keeps(p->Blink))}
        keeps(p) ==> keeps(p->Blink) && p->Blink->Flink == p))

    // The index[] map always increases by 1 for each object that can be reached by
    // an Flink pointer. Except if the Flink points to the list head, which implies
    // the end of the list.
    //
    // The {sk_hack(keeps(p->Flink))} trigger introduces a witness of
    // a keeps(p->Flink) entry, that is required for the prove to succeed.
    invariant(forall(PLIST_ENTRY p; {keeps(p)} {sk_hack(keeps(p->Flink))}
        keeps(p) && p->Flink != ListHead ==> index[p] + 1 == index[p->Flink]))

    // Specify index[] for well known objects.
    invariant(index[ListHead] == 0)
    invariant(index[ListHead->Blink] == size - 1)

    // Specify range of the index[] map.
    invariant(forall(PLIST_ENTRY e; {keeps(e)}
        keeps(e) ==> index[e] < size))

    // Each element in the list is only contained once.
    invariant(forall(PLIST_ENTRY e1, e2; {keeps(e1), keeps(e2)}
        keeps(e1) && keeps(e2) && e1 != e2 ==> index[e1] != index[e2]))

};)

/**
 * InitializeListHead
 * ==================
 *
 * The InitializeListHead routine initializes a LIST_ENTRY structure that represents
 * the head of a doubly-linked list.
 *
 * Parameters:
 *   ListHead : Pointer to a LIST_ENTRY structure that serves as the list header.
 *
 * Return Value:
 *   None
 */
void InitializeListHead( PLIST_ENTRY ListHead )
    writes(span(ListHead))
    ensures(wrapped(ListHead->Manager) && is_fresh(ListHead->Manager))
    ensures(set_eq(owns(ListHead->Manager),SET(ListHead)))
    ensures(ListHead->Manager->size == 1)
    ensures(ListHead->Manager->ListHead == ListHead)
{
    spec(PLIST_MANAGER ListManager;)
    ListHead->Flink = ListHead->Blink = ListHead;

spec(
    ListManager = spec_malloc<LIST_MANAGER>();
    ListManager->size = 1;
    ListManager->index[ListHead] = 0;
    ListManager->ListHead = ListHead;
    ListHead->Manager = ListManager;
    set_owns(ListManager,SET(ListHead));
    wrap(ListHead);
    wrap(ListManager);
    )
}

/**
 * IsListEmpty
 * ===========
 *
 * The IsListEmpty routine indicates whether a doubly linked list of LIST_ENTRY structures is empty.
 *
 *
 * Parameters:
 *   ListHead : Pointer to a LIST_ENTRY structure that represents the head of the list.
 *
 * Return Value:
 *   IsListEmpty returns TRUE if there are currently no entries in the list and FALSE otherwise.
 */
bool IsListEmpty( PLIST_ENTRY ListHead )
    requires(wrapped(ListHead->Manager))
    requires(set_in(ListHead, owns(ListHead->Manager)))
    returns(ListHead->Manager->size == 1)
{
    assert(set_in(ListHead->Flink,owns(ListHead->Manager)));
    return ListHead->Flink == ListHead;
}

/**
 * RemoveEntryList
 * ===============
 *
 * The RemoveEntryList routine removes an entry from a doubly linked list of LIST_ENTRY structures.
 * The entry to be removed must not be the list head.
 *
 * Parameters:
 *   Entry : Pointer to the LIST_ENTRY structure that represents the entry to be removed.
 *
 * Return Value:
 *   RemoveEntryList returns TRUE if the list is empty and FALSE otherwise.
 */
bool RemoveEntryList( PLIST_ENTRY Entry )
    requires(set_in(Entry,owns(Entry->Manager)))
    requires(Entry != Entry->Manager->ListHead)
    maintains(wrapped(Entry->Manager))
    writes(Entry->Manager)
    returns(old(Entry->Manager)->size==1)
    ensures(wrapped(Entry) && is_fresh(Entry))
    ensures(set_equal(owns(Entry->Manager),set_difference(old(owns(Entry->Manager)),SET(Entry))))
    ensures(Entry->Manager->size == old(Entry->Manager->size) - 1)
    ensures(old(Entry->Manager->ListHead)==old(Entry->Manager)->ListHead)
    ensures(forall(PLIST_ENTRY le; set_in(le,old(owns(Entry->Manager))); unchanged(emb(le))))
{
    PLIST_ENTRY Blink, Flink;
    spec(PLIST_MANAGER ListManager = Entry->Manager;)

    assert(in_domain(Entry,ListManager));
    assert(in_domain(Entry->Blink,ListManager));
    assert(in_domain(Entry->Flink,ListManager));

    Blink = Entry->Blink;
    Flink = Entry->Flink;
    expose (ListManager) {
        expose (Blink) {
            Blink->Flink = Flink;
        }
        expose (Flink) {
            Flink->Blink = Blink;
        }

spec(
        ListManager->size--;
        giveup_owner(Entry,ListManager);
        ListManager->index = lambda(PLIST_ENTRY x; set_in(x,owns(ListManager));
            ListManager->index[x] < ListManager->index[Entry]
                ? ListManager->index[x]
                : ListManager->index[x] - 1);
        )
    }

    return Flink == Blink;
}

/**
 * RemoveHeadList
 * ==============
 *
 * The RemoveHeadList routine removes an entry from the beginning of a doubly linked
 * list of LIST_ENTRY structures. This function must only be called, if the list
 * is not empty.
 *
 * Parameters:
 *   ListHead : Pointer to the LIST_ENTRY structure that serves as the list header.
 *
 * Return Value:
 *   RemoveHeadList returns a pointer to the entry removed from the list.
 */
PLIST_ENTRY RemoveHeadList( PLIST_ENTRY ListHead )
    requires(ListHead->Flink != ListHead->Manager->ListHead)
    maintains(set_in(ListHead,owns(ListHead->Manager)))
    maintains(wrapped(ListHead->Manager))
    writes(ListHead->Manager)
    returns(old(ListHead->Flink))
    ensures(unchanged(ListHead->Manager))
    ensures(unchanged(ListHead->Manager->ListHead))
    ensures(forall(PLIST_ENTRY le; set_in(le,old(owns(ListHead->Manager))); unchanged(emb(le))))
    ensures(ListHead->Manager->size == old(ListHead->Manager->size) - 1)
    ensures(set_equal(owns(ListHead->Manager),set_difference(old(owns(ListHead->Manager)),SET(result))))
    ensures(set_in(result, old(owns(ListHead->Manager))))
    ensures(wrapped(result))
{
    PLIST_ENTRY Flink, Entry;
    spec(PLIST_MANAGER ListManager = ListHead->Manager;)

    assert(in_domain(ListHead,ListManager));
    assert(in_domain(ListHead->Flink,ListManager));
    assert(in_domain(ListHead->Flink->Flink,ListManager));

    Entry = ListHead->Flink;
    Flink = ListHead->Flink->Flink;
    expose(ListManager) {
        expose(ListHead) {
            ListHead->Flink = Flink;
        }
        expose(Flink) {
            Flink->Blink = ListHead;
        }

spec(
        ListManager->size--;
        giveup_owner(Entry,ListManager);
        ListManager->index = lambda(PLIST_ENTRY x; set_in(x,owns(ListManager));
            ListManager->index[x] < ListManager->index[Entry]
                ? ListManager->index[x]
                : ListManager->index[x] - 1);
        )
    }
    return Entry;
}

/**
 * RemoveTailList
 * ==============
 *
 * The RemoveTailList routine removes an entry from the end of a doubly linked list of
 * LIST_ENTRY structures. The list must not be empty.
 *
 * Parameters:
 *   ListHead : Pointer to the LIST_ENTRY structure that serves as the list header.
 *
 * Return Value:
 *   RemoveTailList returns a pointer to the entry that was at the tail of the list.
 */
PLIST_ENTRY RemoveTailList( PLIST_ENTRY ListHead )
    requires(ListHead->Manager->size > 1)
    requires(ListHead->Blink != ListHead->Manager->ListHead)
    maintains(set_in(ListHead,owns(ListHead->Manager)))
    maintains(wrapped(ListHead->Manager))
    writes(ListHead->Manager)
    returns(old(ListHead->Blink))
    ensures(unchanged(ListHead->Manager))
    ensures(unchanged(ListHead->Manager->ListHead))
    ensures(forall(PLIST_ENTRY le; set_in(le,old(owns(ListHead->Manager))); unchanged(emb(le))))
    ensures(ListHead->Manager->size == old(ListHead->Manager->size) - 1)
    ensures(set_equal(owns(ListHead->Manager),set_difference(old(owns(ListHead->Manager)),SET(result))))
    ensures(wrapped(result))
{
    PLIST_ENTRY Blink, Entry;
    spec(PLIST_MANAGER ListManager = ListHead->Manager;)

    assert(in_domain(ListHead,ListManager));
    assert(in_domain(ListHead->Blink,ListManager));
    assert(in_domain(ListHead->Blink->Blink,ListManager));

    Entry = ListHead->Blink;
    Blink = ListHead->Blink->Blink;
    expose(ListManager) {
        expose(ListHead) {
            ListHead->Blink = Blink;
        }
        expose(Blink) {
            Blink->Flink = ListHead;
        }

spec(
        ListManager->size--;
        giveup_owner(Entry,ListManager);
        ListManager->index = lambda(PLIST_ENTRY x; set_in(x,owns(ListManager));
            ListManager->index[x] < ListManager->index[Entry]
                ? ListManager->index[x]
                : ListManager->index[x] - 1);
        )
    }
    return Entry;
}

/**
 * InsertTailList
 * ==============
 *
 * The InsertTailList routine inserts an entry at the tail of a doubly linked list of
 * LIST_ENTRY structures.
 *
 * Parameters:
 *   ListHead : Pointer to the LIST_ENTRY structure that represents the head of
 *              the list.
 *   Entry    : Pointer to a LIST_ENTRY structure that represents the entry to
 *              be inserted in the list.
 *
 * Return Value:
 *   None
 */
void InsertTailList( PLIST_ENTRY ListHead, PLIST_ENTRY Entry )
    requires(mutable(Entry))
    requires(ListHead->Manager->size < MAXUINT)
    maintains(wrapped(ListHead->Manager))
    maintains(set_in(ListHead,owns(ListHead->Manager)))
    writes(ListHead->Manager,span(Entry))
    ensures(unchanged(ListHead->Manager))
    ensures(unchanged(ListHead->Manager->ListHead))
    ensures(forall(PLIST_ENTRY le; set_in(le,old(owns(ListHead->Manager))); unchanged(emb(le))))
    ensures(set_in(Entry,owns(ListHead->Manager)))
    ensures(ListHead->Manager->size == old(ListHead->Manager->size) + 1)
    ensures(set_eq(owns(ListHead->Manager),set_union(old(owns(ListHead->Manager)),SET(Entry))))
    ensures(unchanged(ListHead->Manager->ListHead))
    ensures(Entry->Manager == ListHead->Manager)
{
    spec(PLIST_MANAGER ListManager = ListHead->Manager;)

    assert(in_domain(ListHead,ListManager));
    assert(in_domain(ListHead->Blink,ListManager));

    Entry->Flink = ListHead;
    Entry->Blink = ListHead->Blink;
    spec(Entry->Manager = ListManager;)

    wrap(Entry);
    expose(ListManager) {
        expose(ListHead->Blink) {
            ListHead->Blink->Flink = Entry;
        }
        expose(ListHead) {
            ListHead->Blink = Entry;
        }

spec(
        ListManager->size++;
        set_owner(Entry,ListManager);

        if (ListHead == ListManager->ListHead) {
            ListManager->index[Entry] = ListManager->size - 1;
        } else {
            ListManager->index = lambda(PLIST_ENTRY x; set_in(x,owns(ListManager));
                x==Entry
                    ? ListManager->index[ListHead]
                    : (ListManager->index[x] < ListManager->index[ListHead]
                        ? ListManager->index[x]
                        : ListManager->index[x] + 1));
        }
        )
    }
}

/**
 * InsertHeadList
 * ==============
 *
 * The InsertHeadList routine inserts an entry at the head of a doubly-linked list of
 * LIST_ENTRY structures.
 *
 * Parameters:
 *   ListHead  : Pointer to the LIST_ENTRY structure that represents the head
 *               of the list.
 *   Entry     : Pointer to a LIST_ENTRY structure that represents the entry to
 *               be inserted into the list.
 *
 * Return Value:
 *   None
 */
void InsertHeadList( PLIST_ENTRY ListHead, PLIST_ENTRY Entry )
    requires(mutable(Entry))
    requires(ListHead->Manager->size < MAXUINT)
    maintains(wrapped(ListHead->Manager))
    maintains(set_in(ListHead,owns(ListHead->Manager)))
    writes(ListHead->Manager,span(Entry))
    ensures(unchanged(ListHead->Manager))
    ensures(unchanged(ListHead->Manager->ListHead))
    ensures(forall(PLIST_ENTRY le; set_in(le,old(owns(ListHead->Manager))); unchanged(emb(le))))
    ensures(set_in(Entry,owns(ListHead->Manager)))
    ensures(ListHead->Manager->size == old(ListHead->Manager->size) + 1)
    ensures(set_eq(owns(ListHead->Manager),set_union(old(owns(ListHead->Manager)),SET(Entry))))
    ensures(Entry->Manager == ListHead->Manager)
{
    spec(PLIST_MANAGER ListManager = ListHead->Manager;)

    assert(in_domain(ListHead,ListManager));
    assert(in_domain(ListHead->Flink,ListManager));

    Entry->Blink = ListHead;
    Entry->Flink = ListHead->Flink;
    spec(Entry->Manager = ListManager;)
    wrap(Entry);
    expose(ListManager) {
        expose(ListHead->Flink) {
            ListHead->Flink->Blink = Entry;
        }
        expose(ListHead) {
            ListHead->Flink = Entry;
        }

spec(
        ListManager->size++;
        set_owner(Entry,ListManager);
        ListManager->index = lambda(PLIST_ENTRY x; set_in(x,owns(ListManager));
            x==Entry
                ? ListManager->index[ListHead] + 1
                : (ListManager->index[x] <= ListManager->index[ListHead]
                    ? ListManager->index[x]
                    : ListManager->index[x] + 1));
        )
    }
}

/*`
Verification of _LIST_MANAGER#adm succeeded.
Verification of InitializeListHead succeeded.
Verification of IsListEmpty succeeded.
Verification of RemoveEntryList succeeded.
Verification of RemoveHeadList succeeded.
Verification of RemoveTailList succeeded.
Verification of InsertTailList succeeded.
Verification of InsertHeadList succeeded.
`*/
