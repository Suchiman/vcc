#pragma once

#include <vcc.h>

vcc(atomic_inline) int __interlockedcompareexchange(volatile int *Destination, int Exchange, int Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}
