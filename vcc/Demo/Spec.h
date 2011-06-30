#pragma once

void memzero(unsigned int *b, unsigned int size)
  writes(as_array(b, size))
  maintains(wrapped(as_array(b, size)))
  ensures(forall(unsigned int i; i < size ==> b[i] == 0));

#define _InterlockedCompareExchange(T)                                         \
vcc(atomic_inline)                                                             \
T InterlockedCompareExchange(volatile T *Destination, T Exchange, T Compare) { \
  if (*Destination == Compare) {                                               \
    *Destination = Exchange;                                                   \
    return Compare;                                                            \
  }                                                                            \
  else {                                                                       \
    return *Destination;                                                       \
  }                                                                            \
}                                                                              \

typedef unsigned __int64 uint64_t;
typedef unsigned __int8  uint8_t;

vcc(atomic_inline)
int InterlockedBitSet(volatile uint64_t *v, uint64_t pos) {
  int result = (((*v) >> pos) & 1) == 1;
  *v |= (1ULL << pos);
  return result;
}
