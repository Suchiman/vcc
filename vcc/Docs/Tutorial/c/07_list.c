#include <vcc2test.h>

struct Node {
  struct Node *next;
  int data;
};

typedef struct Node *PNode;

struct vcc(dynamic_owns) List {
  spec( bool val[int]; ) // public

  struct Node *head;
  spec( bool followers[PNode][int]; )
  invariant(val == followers[head])
  invariant(head != NULL ==> set_in(head, owns(this)))
  invariant(followers[NULL] == lambda(int k; false))
  invariant(forall(struct Node *n; {set_in(n->next, owns(this))}
            set_in(n, owns(this)) ==> n->next == NULL || set_in(n->next, owns(this))))
  invariant(forall(struct Node *n; {set_in(n, owns(this))} {sk_hack(set_in(n->next, owns(this)))}
            set_in(n, owns(this)) ==> 
                       forall(int e; followers[n][e] <==> followers[n->next][e] || e == n->data)))
};

struct List *mklist()
  ensures(result != NULL ==> wrapped(result) && result->val == lambda(int k; false))
{
  struct List *l = malloc(sizeof(*l));
  if (l == NULL) return NULL;
  l->head = NULL;
  speconly(
    set_owns(l, SET());
    l->followers = lambda(struct Node *n; int k; false);
    l->val = l->followers[l->head];
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
      l->followers[n] = lambda(int z; l->followers[n->next][z] || z == k);
      //-- l->followers = lambda(struct Node *p; int z; p == n ? (l->followers[p->next][z] || z == k) : l->followers[p][z]); //--
      l->val = l->followers[n];
    )
  }
  return 0;
}

int member(struct List *l, int k)
  requires(wrapped(l))
  ensures(result != 0 <==> l->val[k])
{
  struct Node *n;

  n = l->head;

  if (n == NULL)
    return 0;

  for (;;)
    invariant(l->val[k] <==> l->followers[n][k])
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
