/*{include}*/
#include <vcc.h>
#include <stdlib.h>
/*{obj}*/
struct SafeString {
  unsigned capacity, len;
  char *content;
  _(invariant len < capacity)
  _(invariant content[len] == '\0')
  _(invariant \mine((\any[capacity])content))
};
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
{
  \object cont;
  _(assume \wrapped(s))
  _(assume s->len < s->capacity - 1)

  cont = (\any[s->capacity])(s->content); // for readability
  // unwrap s
  _(assert \wrapped(s))
  cont->theOwner = \me; // transfer 1
  s->consistencyFlag = false;
  // unwrap cont
  _(assert \wrapped(cont))
  c->consistencyFlag = false;
  
  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  // wrap c
  _(assert \unwrapped(cont))
  _(assert \inv(cont))
  cont->consistencyFlag = true;
  // wrap s
  _(assert \unwrapped(s))
  cont->theOwner = s; // transfer 2
  _(assert \inv(s))
  s->consistencyFlag = true;
  // ensures
  _(assert \wrapped(s))
}
