#include <string.h>

#include "spritz.h"

byte low(byte b) {
  return b & 0x0F;
}

byte high(byte b) {
  return (b & 0xF0) >> 4;
}

void swap(byte* a, byte* b) {
  byte tmp = *a;
  *a = *b;
  *b = tmp;
}

byte gcd(byte u, byte v) {
  if(u == v) {
    return u;
  }

  if(u == 0) {
    return v;
  }

  if(v == 0) {
    return u;
  }

  if(~u & 1) {
    if (v & 1) {
      return(gcd(u >> 1, v));
    }
    else {
      return gcd(u >> 1, v >> 1) << 1;
    }
  }

  if(~v & 1) {
    return gcd(u, v >> 1);
  }

  if(u > v) {
    return gcd((u - v) >> 1, v);
  }

  return gcd((v - u) >> 1, u);
}

byte output() {
  byte z;

  z = S[j + S[i + S[z + k]]];

  return z;
}

void update() {
  i = i + w;
  j = k + S[j + S[i]];
  k = i + k + S[j];
  swap(&S[i],&S[j]);
}

void crush() {
  size_t v;

  for(v = 0; v < (N/2); v++) {
    if(S[v] > S[N - 1 - v]) {
      swap(&S[v],&S[N - 1 - v]);
    }
  }
}

void whip(size_t r) {
  size_t v;

  for(v = 0; v < r; v++) {
    update();
  }

  do {
    w = w + 1;
  } while(gcd(w,(byte)N) != (byte)1);
}

void shuffle() {
  whip(N * 2);
  crush();
  whip(N * 2);
  crush();
  whip(N * 2);
  a = 0;
}

byte drip() {
  if(a != 0) {
    shuffle();
  }

  update();

  return output();
}

void absorbStop() {
  if(a == (N/2)) {
    shuffle();
  }

  a = a + 1;
}

void absorbNibble(byte x) {
  if(a == (N/2)) {
    shuffle();
  }

  swap(&S[a],&S[(N/2) + x]);

  a = a + 1;
}

void absorbByte(byte b) {
  absorbNibble(low(b));
  absorbNibble(high(b));
}

void absorb(byte *I, size_t iLength) {
  size_t v;

  for(v = 0; v < iLength; v++) {
    absorbByte(I[v]);
  }
}

void initialiseState() {
  i = j = k = z = a = 0;

  w = 1;

  size_t v;

  for(v = 0; v < N; v++) {
    S[v] = v;
  }
}
