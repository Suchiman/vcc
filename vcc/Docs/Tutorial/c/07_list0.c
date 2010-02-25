#include <vcc2test.h>

struct Node {
  struct Node *next;
  int data;
};

typedef struct Node *PNode;

struct vcc(dynamic_owns) List {
  spec( bool val[int]; ) // public

  struct Node *head;
  invariant(head != NULL ==> set_in(head, owns(this)))
  invariant(forall(struct Node *n; {set_in(n->next, owns(this))}
            set_in(n, owns(this)) ==> n->next == NULL || set_in(n->next, owns(this))))
  invariant(forall(struct Node *n; {set_in(n, owns(this))}
            set_in(n, owns(this)) ==> val[n->data]))
};

struct List *mklist()
  ensures(result != NULL ==> wrapped(result) && result->val == lambda(int k; false))
{
  struct List *l = malloc(sizeof(*l));
  if (l == NULL) return l;
  l->head = NULL;
  speconly(
    set_owns(l, SET());
    l->val = lambda(int k; false);
  )
  wrap(l);
  return l;
}

int add(struct List *l, int k)
  requires(wrapped(l))
  ensures(wrapped(l))
  ensures(result == 0 ==> l->val == lambda(int p; old(l->val)[p] || p == k))
  ensures(result != 0 ==> l->val == old(l->val))
  writes(l)
{
  struct Node *n = malloc(sizeof(*n));
  if (n == NULL) return -1;
  expose(l) {
    n->next = l->head;
    n->data = k;
    l->head = n;
    wrap(n);
    speconly(
      set_owns(l, set_union(owns(l), SET(n)));
      l->val = lambda(int z; z == k || l->val[z]);
    )
  }
  return 0;
}

int member(struct List *l, int k)
  requires(wrapped(l))
  // partial specification, ==> instead of <==>
  ensures(result != 0 ==> l->val[k])
{
  struct Node *n;

  n = l->head;

  if (n == NULL)
    return 0;

  for (;;)
    invariant(set_in(n, owns(l)))
  {
    if (n->data == k)
      return 1;
    n = n->next;
    if (n == NULL)
      return 0;
  }
}
/*`
Verification of List#adm succeeded.
Verification of mklist succeeded.
Verification of add succeeded.
Verification of member succeeded.
`*/
