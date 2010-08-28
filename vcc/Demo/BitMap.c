
#include "BitMap.h"

void InitializeBitMap (BITMAP *BitMap, unsigned int *BitMapBuffer, unsigned int Size)
{
  BitMap->Size = Size;
  BitMap->Buffer = BitMapBuffer;
  memzero(BitMapBuffer, Size/32);

  speconly(BitMap->BM = lambda(unsigned int i; i < Size; false);)
  wrap(BitMap);
}

void SetBit (BITMAP *BitMap, unsigned int BitNumber)
{
  expose(BitMap) {
  expose(as_array(BitMap->Buffer, BitMap->Size /32)) {
    BitMap->Buffer[BitNumber/32] |= 1 << BitNumber % 32;
    speconly(BitMap->BM = 
       lambda(unsigned int i; true; i == BitNumber ? true : BitMap->BM[i]);)
  }}
}

/*`
Verification of _BITMAP#adm succeeded.
Verification of InitializeBitMap succeeded.
Verification of SetBit succeeded.
Verification of TestBit#reads succeeded.
`*/
