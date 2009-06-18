
#pragma once

#include <vcc2.h>
#include "spec.h"

// |-------------------------------------------------------|
// | 010...                                                |
// |-------------------------------------------------------|
//   <--                    Size                       -->

typedef struct _BITMAP {
  unsigned int Size;      // Number of bits in bit map
  unsigned int *Buffer;   // Memory to store the bit map

  #pragma region invariant

  // private invariants
  invariant(Size > 0 && Size % 32 == 0)
  invariant(keeps(as_array(Buffer, Size / 32)))

  // public abstraction
  spec(bool BM[unsigned int];) // unsigned int --> {true, false}
  invariant(forall(unsigned int i; i < Size ==> 
        BM[i] == ToBm32(Buffer[i/32])[i%32]))
  // ToBm32: unsigned int -> ( {0..31} -> {true, false} )

  #pragma endregion
} BITMAP;

void InitializeBitMap (BITMAP *BitMap, unsigned int * BitMapBuffer, unsigned int Size)
  #pragma region contract
  writes(extent(BitMap), as_array(BitMapBuffer, Size / 32))
  requires(wrapped(as_array(BitMapBuffer, Size / 32)))
  requires(Size > 0 && Size % 32 == 0)
  ensures(wrapped(BitMap))
  ensures(BitMap->Size == Size)
  ensures(forall(unsigned int i; i < Size ==> BitMap->BM[i] == false))
  #pragma endregion 
  ;

void SetBit (BITMAP *BitMap, unsigned int BitNumber)
  #pragma region contract
  writes(BitMap)
  maintains(wrapped(BitMap))
  requires(BitNumber < BitMap->Size)
  ensures(forall(unsigned int i; 
      BitMap->BM[i] == (i == BitNumber ? true : old(BitMap->BM[i]))))
  ensures(unchanged(BitMap->Size))
  #pragma endregion
  ;

ispure unsigned __int8 TestBit (BITMAP *BitMap, unsigned int BitNumber)
  #pragma region contract
  reads(&BitMap->BM)
  requires(wrapped(BitMap))
  requires(0 < BitNumber && BitNumber < BitMap->Size)
  ensures(result == BitMap->BM[BitNumber])
  #pragma endregion
  ;
