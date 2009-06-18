#include <vcc.h>

#ifndef VERIFY
#define bool int
#define true 1
#define false 0
#else
#define NULL 0
#endif

typedef void *PVOID;


typedef struct Node {
    struct Node *next;

    invariant( depends(owner(this), this) )
} *PNODE;

vcc(atomic_inline) PNODE InterlockedCompareExchangePointer(volatile PNODE *Destination, PNODE Exchange, PNODE Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}


typedef struct Node *PNode;
struct vcc(volatile_owns) Stack {
    volatile PNode hd;

    invariant (this->hd == NULL <==> owns(this) == set_empty())
    invariant (this->hd != NULL <==> owns(this) != set_empty())
    invariant (this->hd != NULL <==>set_in(this->hd, owns(this)))
    invariant (this->hd != NULL ==>
                  forall(struct Node *x; {set_in(x, owns(this))} 
                    set_in(x, owns(this)) ==> typed(x) && x->next != this->hd)
               && forall(struct Node *x; {set_in(x->next, owns(this))} 
                    set_in(x, owns(this)) && x->next != NULL ==> set_in(x->next, owns(this)))
               && forall(struct Node *x; struct Node *y; {x->next, y->next} 
                    set_in(x, owns(this)) && set_in(y, owns(this)) && x->next == y->next ==> x == y))
};

bool push(struct Node *n, struct Stack *s claimp(c))
    always (c, closed(s))
    requires (owns(n) == set_empty())
    writes (extent(n))
{
    while (true) 
        invariant (mutable(n) && owns(n) == set_empty())
    {
        struct Node *h;
        bool ok;

        atomic(c, s) {
          h = s->hd;
	}
        n->next = h;

        ok = false;
	atomic(c, s) {
            wrap(n);
	    begin_update();
        ok = (InterlockedCompareExchangePointer(&s->hd, n, h) == h);
        if (ok) {
                assert(!set_in(n, owns(s)));
                set_closed_owner(n, s);
            }
	}

        if (ok) 
            return true;
	
	unwrap(n);
    }
}

#ifdef NOTYET
bool pop(struct Node **n, struct Stack *s)
    invariant (closed(s) && typed(s))
//    requires (mutable(s->hd))
    requires (mutable(*n))
    ensures ((owns(s) == set_empty()) ==> result == false)
    ensures (set_in(s->hd, owns(s)) ==> result == true)
    writes (*n, s->hd)
{
    assume (s->hd != NULL);
    assume (owns(s) != set_empty());
    
    while (true) 
    invariant (/*mutable(s->hd) && */mutable(*n) && owns(s) != set_empty())
    {
        struct Node *h;
        bool ok;

        h = volatile(s->hd, owns(s) != set_empty());

        assume false;

/*
        if (h == NULL)
        {
            assert (owns(s) == set_empty());
            return false;
        }
        else 
*/
//        {

            assert (owns(s) != set_empty());
            ok = false;
            atomic(s, owns(s) != set_empty()) {
                // InterlockedCompareExchange(&s->hd, h, h->next)
                if (s->hd == h) {
                    s->hd = h->next;
                    ok = true;
                    giveup_owner(h, s);
                    unwrap(h);
                    assert(!set_in(h, owns(s)));
                    *n = h;
                }
            }

            if (ok) 
                return true;

//        }
    }
}
#endif
/*`
Verification of Node#adm succeeded.
Verification of Stack#adm succeeded.
Verification of push succeeded.
`*/
