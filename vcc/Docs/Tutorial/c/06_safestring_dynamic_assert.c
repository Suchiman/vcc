/*{include}*/
#include <vcc.h>
#include <stdlib.h>
/*{obj}*/
struct SafeString {
  unsigned capacity, len;
  char *content;
  invariant(len < capacity)
  invariant(content[len] == '\0')
  invariant(keeps(as_array(content, capacity)))
};
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
{
  obj_t cont;
  assume(wrapped(s));
  assume(s->len < s->capacity - 1);

  cont = as_array(s->content, s->capacity); // for readability
  // unwrap s
  assert(wrapped(s));
  cont->theOwner = me(); // transfer 1
  s->consistencyFlag = false;
  // unwrap cont
  assert(wrapped(cont));
  c->consistencyFlag = false;
  
  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  // wrap c
  assert(mutable(cont));
  assert(inv(cont));
  cont->consistencyFlag = true;
  // wrap s
  assert(mutable(s));
  cont->theOwner = s; // transfer 2
  assert(inv(s));
  s->consistencyFlag = true;
  // ensures
  assert(wrapped(s));
}
