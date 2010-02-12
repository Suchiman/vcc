#include <vcc.h>
#include <stdlib.h>

struct SafeString {
  unsigned capacity, len;
  char *content;
  invariant(len < capacity)
  invariant(content[len] == '\0')
  invariant(keeps(as_array(content, capacity)))
};

spec(typedef struct SafeString * sstr_map[unsigned]; )

spec(ispure bool match(unsigned i, unsigned j) returns(true); )
spec(ispure mathint do_mod(mathint a, mathint b); )

axiom(forall(mathint a, b; {do_mod(a,b)} a >= 0 && b > 0 ==> 0 <= do_mod(a, b) && do_mod(a,b) < b));
axiom(forall(mathint a; {do_mod(a,a)} a > 0 ==> do_mod(a, a) == 0));
axiom(forall(mathint a, b; {do_mod(a,b)} a >= 0 && b > 0 && do_mod(a, b) < b - 1 ==> 
  do_mod(a + 1, b) == do_mod(a, b) + 1));
axiom(forall(mathint a, b; {do_mod(a,b)} a >= 0 && b > 0 && do_mod(a, b) == b - 1 ==> 
  do_mod(a + 1, b) == 0));

#define mod(a,b) ((unsigned)(do_mod((mathint)(a), (mathint)(b))))

struct Hashtable {
  unsigned *keys;
  struct SafeString **values;
  unsigned size;
  spec(sstr_map elts;)

  invariant(size > 0)

  invariant(keeps(as_array(keys, size), as_array(values, size)))

  invariant(forall(unsigned k; {match(k,0)} match(k,0) && elts[k] != NULL ==> 
    exists(unsigned d; {match(d,1)}
      match(d,1) &&
      forall(unsigned i; {match(i,2)} match(i,2) && i < d ==> values[mod(k + i, size)] != NULL) &&
      keys[mod(k + d, size)] == k &&
      values[mod(k + d, size)] == elts[k])))
  invariant(forall(unsigned k, i; {match(i,3),match(k,4)} match(i,3) && match(k, 4) && i < size ==>
    values[i] != NULL && keys[i] == k ==> elts[k] == values[i]))
};

int h_insert(struct Hashtable *h, unsigned k, struct SafeString *s)
  writes(h)
  requires(h->elts[k] == NULL)
  requires(s != NULL)
  requires(wrapped(h))
  ensures(result == 0 ==> h->elts[k] == s && forall(unsigned i; h->elts[i] == old(h->elts[i]) || i == k))
  ensures(result != 0 ==> h->elts == old(h->elts))
{
  unsigned i ,d;
  int res = 0;


  expose(h) {
    expose(as_array(h->keys, h->size)) {
      expose(as_array(h->values, h->size)) {

        // i = k % h->size;
	assume(i == mod(k, h->size));
	d = 0;

        for (;;)
	  invariant(i < h->size && d < h->size)
	  invariant(i == mod(k + d, h->size))
	  invariant(d >= 0)
          invariant(forall(unsigned j; {match(j,2)} match(j,2) && j < d ==> h->values[mod(k + j, h->size)] != NULL))
	{
	  if (h->values[i] == NULL)
	    break;

	  if (++i >= h->size)
	    i = 0;
	  d++;

	  if (d >= h->size) { 
	    res = -1;
	    break;
	  }
	}

	if (res == 0) {
  	assert(match(d,1));

          h->values[i] = s;
          h->keys[i] = k;
          speconly(h->elts[k] = s;)
	}
      }
    }
  }

  return res;
}

struct SafeString *h_find(struct Hashtable *h, unsigned k)
requires(wrapped(h))
ensures(result == h->elts[k])
{
unsigned i, d;

        // i = k % h->size;
	assume(i == mod(k, h->size));
	d = 0;

        for (;;)
	  invariant(i < h->size && d < h->size)
	  invariant(i == mod(k + d, h->size))
	  invariant(d >= 0)
          invariant(forall(unsigned j; {match(j,1)} match(j,1) && j < d ==> h->keys[mod(k + j, h->size)] != k || h->values[mod(k + j, h->size)] == NULL))
	{
	  assert(inv(h));
          assert(match(k,0));
          assert(match(d,2));

	  if (h->values[i] == NULL) {
            assert(match(d,1));
            assert(match(d+1,1));
	    return NULL;
	  }


	  if (h->keys[i] == k) {
	    assert(match(i,3) && match(k,4));
	    return h->values[i];
	  }

	  if (++i >= h->size)
	    i = 0;
	  d++;

	  if (d >= h->size) { 
	    assume(false);
	    return NULL;
	  }
	}
}

