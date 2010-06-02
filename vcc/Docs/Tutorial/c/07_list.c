#include <vcc.h>
#include <stdlib.h>

struct Node {
  struct Node *next;
  int data;
};

/*{type}*/
struct vcc(dynamic_owns) List {
  spec( bool val[int]; )
  struct Node *head;
  spec( bool followers[struct Node *][int]; )
  invariant(val == followers[head])
  invariant(head != NULL ==> keeps(head))
  invariant(followers[NULL] == lambda(int k; false))
  invariant(forall(struct Node *n;
                {set_in(n->next, owns(this))}
                keeps(n) ==> n->next == NULL || keeps(n->next)))
  invariant(forall(struct Node *n; 
                {set_in(n, owns(this))}
                keeps(n) ==> 
                   forall(int e; 
                      followers[n][e] <==> 
                      followers[n->next][e] || e == n->data)))
};
/*{init}*/
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
      set_owns(l, set_union(owns(l), SET(n))); /*{specupdate}*/
      l->followers[n] = 
        lambda(int z; l->followers[n->next][z] || z == k);
      l->val = l->followers[n]; /*{updateend}*/
    )
  }
  return 0;
}
/*{member}*/
int member(struct List *l, int k)
  requires(wrapped(l))
  ensures(result != 0 <==> l->val[k])/*{endspec}*/
{
  struct Node *n;

  for (n = l->head; n; n = n->next)
    invariant(n != NULL ==> set_in(n, owns(l)))
    invariant(l->val[k] <==> l->followers[n][k])
  {
    if (n->data == k)
      return 1;
  }
  return 0;
}
/*{out}*/
/*`
Verification of List#adm succeeded.
Verification of mklist succeeded.
Verification of add succeeded.
Verification of member succeeded.
`*/
