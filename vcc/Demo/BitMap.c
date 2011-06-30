#include "BitMap.h"

void InitializeBitMap (BITMAP *BitMap, unsigned int *BitMapBuffer, unsigned int Size)
{
  BitMap->Size = Size;
  BitMap->Buffer = BitMapBuffer;
  memzero(BitMapBuffer, Size/32);

  spec(BitMap->BM = lambda(unsigned int i; i < Size; false);)
  wrap(BitMap);
}

void SetBit (BITMAP *BitMap, unsigned int BitNumber)
{
  spec(unsigned old, new;)

  expose(BitMap) {
  expose(as_array(BitMap->Buffer, BitMap->Size/32)) {

    BitMap->Buffer[BitNumber/32] |= 1 << BitNumber % 32;

    bv_lemma(forall(unsigned x, i, j; j < 32 && i != j ==>
        (BIT_SELECT(x, i) <==> BIT_SELECT(x | (1 << j), i))));

    spec(BitMap->BM[BitNumber] = true;)
  }}
}

/*`
Verification of _BITMAP#adm succeeded.
Verification of InitializeBitMap succeeded.
Verification of SetBit succeeded.
Verification of SetBit#bv_lemma#0 succeeded.
Verification of TestBit#reads succeeded.
`*/
