#include <vcc2test.h>

/*{types}*/
struct Node {
  struct Node *next;
  int data;
};

struct vcc(dynamic_owns) List {
  struct Node *head;
  spec( bool val[int]; )
  invariant(head != NULL ==> keeps(head))
  invariant(forall(struct Node *n;
                {set_in(n->next, owns(this))}
                keeps(n) ==> n->next == NULL || keeps(n->next)))
  invariant(forall(struct Node *n; 
                {set_in(n, owns(this))}
                keeps(n) ==> val[n->data]))
};
/*{init}*/
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
/*{add}*/
int add(struct List *l, int k)
  requires(wrapped(l))
  ensures(wrapped(l))
  ensures(result != 0 ==> l->val == old(l->val))
  ensures(result == 0 ==>
       forall(int p; l->val[p] == (old(l->val)[p] || p == k)))
  writes(l)
/*{endspec}*/
{
  struct Node *n = malloc(sizeof(*n));
  if (n == NULL) return -1;
  expose(l) {
    n->next = l->head;
    n->data = k;
    wrap(n);
    l->head = n;
    speconly(
      set_owns(l, set_union(owns(l), SET(n)));
      l->val = lambda(int z; z == k || l->val[z]);
    )
  }
  return 0;
}
/*{member}*/
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
/*{out}*/
/*`
Verification of List#adm succeeded.
Verification of mklist succeeded.
Verification of add succeeded.
Verification of member succeeded.
`*/
