#ifndef _SPRITZ_H_
#define _SPRITZ_H_

#include <stdlib.h>

#define N 256

typedef unsigned char byte;

typedef unsigned char byte;
byte i, j, k, z, a, w;

byte S[N];

void initialiseState();
void absorb(byte *I, size_t iLength);
void shuffle();
byte drip();

#endif
