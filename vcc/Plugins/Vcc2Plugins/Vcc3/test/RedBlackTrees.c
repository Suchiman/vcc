/*
Copyright (c) 2010, Microsoft Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

   * Neither the name of Microsoft Corporation nor the names of its
     contributors may be used to endorse or promote products derived from this
     software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

#include <vcc2test.h>

typedef struct Node {
  bool red;
  int key, value;
  struct Node *left, *right, *parent;

} Node, *PNode;

spec(ispure bool intToBool(int ) returns(true); )

spec(typedef bool PNodeSet[PNode];)
spec(ispure PNode mark(PNode p) returns(p); )
spec(ispure bool doMark(PNode p) returns(mark(p) == p); )

spec(ispure bool mark2(PNode p); )

#define DEF(F,D)  spec(vcc(specmacro) bool F returns(split_conjunctions(D)); )

DEF(rb_lrclosed(struct Tree *t, PNode p, PNode x),
  forall(PNode n; {mark(n)} t->R[t->root][n] && mark(n)->parent == NULL ==> n == x || n == t->root) &&
  forall(PNode n,m; {t->R[mark(n)][m]} t->R[mark(n)][m] == (t->R[t->root][n] && ((n == p && m == x) || m == n || t->R[n->left][m] || t->R[n->right][m]))))

DEF(rb_istree(struct Tree *t),
  !t->R[t->root][NULL] && // just for triggering
  (t->root != NULL ==> t->R[t->root][t->root] && t->root->parent == NULL) &&
  forall(PNode m; !t->R[NULL][m]) && 
  forall(PNode n, m; {t->R[mark(n)->left][m]} t->R[t->root][n] ==> t->R[mark(n)->left][m] ==> m->key < n->key) &&
  forall(PNode n, m; {t->R[mark(n)->right][m]} t->R[t->root][n] ==> t->R[mark(n)->right][m] ==> m->key > n->key) &&

  forall(PNode m; {t->R[t->root][m]} {mark2(m)} {sk_hack(mark2(m))} t->R[t->root][m] <==> set_in(m, owns(t))) &&
  forall(PNode n, m; {t->R[n][m]} t->R[n][m] ==> t->R[t->root][m] && t->R[t->root][n]) &&
  forall(PNode n; {mark(n)} t->R[t->root][n] && mark(n)->left != NULL ==> t->R[n][n->left] && n->left->parent == n) &&
  forall(PNode n; {mark(n)} t->R[t->root][n] && mark(n)->right != NULL ==> t->R[n][n->right] && n->right->parent == n) &&
  forall(PNode n; {mark(n)} t->R[t->root][n] && mark(n)->parent != NULL ==> t->R[t->root][n->parent] && (n->parent->left == n || n->parent->right == n)) &&
  forall(PNode n; {t->R[t->root][n]} t->R[t->root][n] ==> t->R[n][n]) &&
  true
)

DEF(rb_abs(struct Tree *t),
  forall(PNode m; {t->abs[m->key]} t->R[t->root][m] ==> t->abs[m->key] == m->value) &&
  forall(int k; {t->abs[k]} forall(PNode m; t->R[t->root][m] ==> m->key != k) ==> t->abs[k] == 0))

struct vcc(dynamic_owns) Tree {
  PNode root;
  spec( int abs[int]; )
  spec( PNodeSet R[PNode]; )

  invariant(rb_istree(this))
  invariant(rb_abs(this))
  invariant(rb_lrclosed(this, NULL, NULL))
};

void tree_init(struct Tree *t)
  writes(span(t))
  ensures(wrapped(t))
{
  t->root = NULL;
  spec( t->R = lambda(PNode m; lambda(PNode n; false)); )
  spec( t->abs = lambda(int k; 0); )
  spec( set_owns(t, set_empty()); )
  wrap(t);
}


PNode tree_find(struct Tree *t, int key)
  requires(wrapped(t))
  ensures(result != NULL ==> t->R[t->root][result] && result->key == key)
  ensures(result == NULL ==> forall(PNode n; t->R[t->root][n] ==> n->key != key))
{
  PNode p;
  p = t->root;
  while (p) 
    invariant(p == NULL || t->R[t->root][mark(p)])
    invariant(forall(PNode n; n->key == key && t->R[t->root][n] ==> t->R[p][n]))
  {
    if (key < p->key)
      p = p->left;
    else if (key > p->key)
      p = p->right;
    else
      return p;
  }
  return NULL;
}

int tree_lookup(struct Tree *t, int key)
  requires(wrapped(t))
  ensures(result == t->abs[key])
{
  PNode r = tree_find(t, key);
  if (r == NULL) {
    assert(inv(t));
    return 0;
  } else
    return r->value;
}

void tree_insert(struct Tree *t, PNode x) 
  maintains(wrapped(t))
  writes(t, span(x))
  requires(!t->R[t->root][x])
  requires(forall(PNode n; t->R[t->root][n] ==> n->key != x->key))
  ensures(forall(PNode n; t->R[t->root][n] <==> n == x || old(t->R[t->root][n])))
  ensures(unchanged(x->key) && unchanged(x->value))
{
  PNode p, n;

  unwrap(t);

  x->left = x->right = x->parent = NULL;

  p = t->root;

  assert(doMark(p));
  spec( t->abs = lambda(int k; k == x->key ? x->value : t->abs[k]); )
  set_owns(t, set_union(owns(t), SET(x)));
  spec( t->R[x] = lambda(PNode n; n == x); )

  if (p == NULL) {
    t->root = x;
    wrap(x);
    wrap(t);
    return;
  }

  spec( t->R[t->root] = lambda(PNode n; n == x || old(t->R[t->root][n])); )
  assert(doMark(t->root));
  assert(t->R[t->root][x]);

  while (1) 
    writes(set_difference(owns(t), SET(x)), &t->R)
    invariant(forall(obj_t p; set_in(p,owns(t)) ==> p == x || wrapped(p)) && mutable(x))
    invariant(unchanged(t->R[t->root]) && unchanged(t->R[NULL]) && unchanged(t->R[x]))
    invariant(rb_istree(t))
    invariant(rb_abs(t))
    invariant(rb_lrclosed(t, p, x))
    invariant(t->R[t->root][p])
    invariant(p != x && !t->R[p->left][x] && !t->R[p->right][x])
    invariant(forall(PNode n; t->R[t->root][n] ==> n == x || n->key != x->key))
  {
    if (x->key < p->key) {
      n = p->left;
      if (n == NULL) {
        expose(p) { p->left = x; }
        break;
      }
    } else if (x->key > p->key) {
      n = p->right;
      if (n == NULL) {
        expose(p) { p->right = x; }
        break;
      }
    } else {
      assert(false);
    }
    assert(doMark(p) && doMark(n));
    spec( t->R[n] = lambda(PNode k; k == x || t->R[n][k]); )
    p = n;
  }


  x->parent = p;
  wrap(x);  
  assert(forall(obj_t p; set_in(p,owns(t)) ==> p == x || wrapped(p)) && wrapped(x));
  wrap(t);
}

void left_rotate(struct Tree *t, PNode x) 
  requires(t->R[t->root][x] && x->right != NULL)
  maintains(wrapped(t))
  writes(t)
  ensures(unchanged(t->R[t->root]) && unchanged(t->abs))
{
  PNode y;

  unwrap(t);
  assert(doMark(x)); unwrap(x);
  y = x->right;
  assert(doMark(y)); unwrap(y);

  x->right = y->left;
  if (y->left != NULL)
      expose(y->left) y->left->parent = x;
  y->parent = x->parent;
  if (x->parent == NULL) {
    t->root = y;
  } else {
    assert(doMark(x->parent));
    expose (x->parent)
     if (x->parent->left == x)
        x->parent->left = y;
     else
        x->parent->right = y;
  }

  y->left = x;
  x->parent = y;
  wrap(x); wrap(y);

  assert(doMark(x->right));
  assert(forall(PNode m; {sk_hack(t->R[y][m])} {t->R[x->right][m]} t->R[x->right][m] ==> m->key > x->key));

  speconly( t->R[y] = t->R[x]; )
  speconly( t->R[x] = lambda(PNode m; m == x || t->R[x->left][m] || t->R[x->right][m]); )

  wrap(t);
}

void right_rotate(struct Tree *t, PNode y) 
  requires(t->R[t->root][y] && y->left != NULL)
  maintains(wrapped(t))
  writes(t)
  ensures(unchanged(t->R[t->root]) && unchanged(t->abs))
{
  PNode x;

  unwrap(t);
  assert(doMark(y)); unwrap(y);
  x = y->left;
  assert(doMark(x)); unwrap(x);

  y->left = x->right;
  if (x->right != NULL)
      expose(x->right) x->right->parent = y;
  x->parent = y->parent;
  if (y->parent == NULL) t->root = x;
  else{
    assert(doMark(y->parent));
    expose(y->parent)
     if (y->parent->left == y)
        y->parent->left = x;
     else
        y->parent->right = x;
  }

  x->right = y;
  y->parent = x;

  wrap(x); wrap(y);

  assert(doMark(y->left));
  assert(forall(PNode m; {sk_hack(t->R[x][m])} {t->R[y->left][m]} t->R[y->left][m] ==> m->key < y->key));

  speconly( t->R[x] = t->R[y]; )
  speconly( t->R[y] = lambda(PNode m; m == y || t->R[y->left][m] || t->R[y->right][m]); )

  wrap(t);
}

#if 0
void rb_rotate(struct Tree *t, PNode x)
{
  tree_insert(t, x);

  x->red = true;
  while (x != t->root && x->parent->red) {
    if (x->parent == x->parent->parent->left) {
      y = x->parent->parent->right;
      if (y->red) {
        // case 1 - change the colors
        x->parent->red = false;
        y->red = false;
        x->parent->parent->red = true;
        // Move x up the tree
        x = x->parent->parent;
      } else {
        if (x == x->parent->right) {
          // case 2 - move x up and rotate
          x = x->parent;
          left_rotate(t, x);
        }
        // case 3
        x->parent->red = false;
        x->parent->parent->red = true;
        right_rotate(t, x->parent->parent);
      }
    } else {
      y = x->parent->parent->left;
      if (y->red) {
        // case 1 - change the colors
        x->parent->red = false;
        y->red = false;
        x->parent->parent->red = true;
        // Move x up the tree
        x = x->parent->parent;
      } else {
        if (x == x->parent->left) {
          // case 2 - move x up and rotate
          x = x->parent;
          right_rotate(t, x);
        }
        // case 3
        x->parent->red = false;
        x->parent->parent->red = true;
        left_rotate(t, x->parent->parent);
      }
      
    }
  }
  t->root->red = false;
}
#endif
