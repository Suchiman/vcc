/*
 * This file provides a sample implementation of doubly-linked lists.
 */
#include "list.h"

void InitializeListHead( PLIST_ENTRY ListHead )
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

bool IsListEmpty( PLIST_ENTRY ListHead )
{
    assert(set_in(ListHead->Flink,owns(ListHead->Manager)));
    return ListHead->Flink == ListHead;
}

bool RemoveEntryList( PLIST_ENTRY Entry )
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

PLIST_ENTRY RemoveHeadList( PLIST_ENTRY ListHead )
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

PLIST_ENTRY RemoveTailList( PLIST_ENTRY ListHead )
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

void InsertTailList( PLIST_ENTRY ListHead, PLIST_ENTRY Entry )
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

void InsertHeadList( PLIST_ENTRY ListHead, PLIST_ENTRY Entry )
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
