#pragma once

spec(typedef bool BITMAP32[unsigned int];)

spec(
ispure BITMAP32 ToBm32(unsigned int n);
)

spec(
ispure BITMAP32 Bm32Singleton(unsigned int i)
  returns(lambda(unsigned int j; true; (bool)(j == i)));
)

spec(
ispure BITMAP32 Bm32Union(BITMAP32 bm1, BITMAP32 bm2)
  returns(lambda(unsigned int i; i < 32; bm1[i] ? true : bm2[i]));
)

spec(
ispure BITMAP32 Bm32Intersect(BITMAP32 bm1, BITMAP32 bm2)
  returns(lambda(unsigned int i; i < 32;  bm1[i] ? bm2[i] : false));
)

spec(
ispure BITMAP32 Bm32SymmetricDiff(BITMAP32 bm1, BITMAP32 bm2)
  returns(lambda(unsigned int i; i < 32; (bool)!(bm1[i] <==> bm2[i])));
)

spec(
ispure BITMAP32 Bm32Invert(BITMAP32 bm)
  returns(lambda(unsigned int i; i < 32; !bm[i]));
)

spec(
ispure bool Bm32SpecialValue(unsigned int bm)
  returns(true);
)

axiom(forall(unsigned int i; i < 32 ==> !ToBm32(0)[i]));
axiom(forall(unsigned int i; i < 32 ==> ToBm32((unsigned int)-1)[i]));
axiom(forall(unsigned int i; { ToBm32(1UI32 << i) } i < 32 ==> ToBm32(1UI32 << i) == Bm32Singleton(i)));
axiom(forall(unsigned int x; {Bm32SpecialValue(x)} forall(unsigned int i; i < 32 ==> !ToBm32(x)[i]) ==> x == 0));
axiom(forall(unsigned int x; {Bm32SpecialValue(x)} forall(unsigned int i; i < 32 ==> ToBm32(x)[i]) ==> x == (unsigned int)-1));
axiom(forall(unsigned int x, y; { ToBm32(x & y) } ToBm32(x & y) == Bm32Intersect(ToBm32(x), ToBm32(y))));
axiom(forall(unsigned int x, y; { ToBm32(x | y) } ToBm32(x | y) == Bm32Union(ToBm32(x), ToBm32(y))));
axiom(forall(unsigned int x, y; { ToBm32(x ^ y) } ToBm32(x ^ y) == Bm32SymmetricDiff(ToBm32(x), ToBm32(y))));
axiom(forall(unsigned int x;   { ToBm32(~x) } ToBm32(~x) == Bm32Invert(ToBm32(x))));
axiom(forall(unsigned int x, i; i < 32 ==> ToBm32(x)[i] <==> (x & (1UI32 << i)) != 0));

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
