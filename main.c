#include <stdio.h>
#include <string.h>

#include "spritz.h"

#define MAX_KEY_LENGTH 245
#define FILE_NOT_FOUND 2
#define INVALID_ARGUMENT 22

enum ProgramAction {
  ENCRYPT,
  DECRYPT,
  HASH
};

void printUsage() {
  printf("Usage:\n");
  printf("spritz [action] [key file] [input file] [output file]\n");
  printf("Options:\n");
  printf("action: encrypt / decrypt / hash.\n");
  printf("key file: path to key file (max key length is 245 chars.\n");
  printf("input file: path to input file.\n");
  printf("output file: path to output file.\n");
}

int main(int argc, char* argv[]) {

  byte key[MAX_KEY_LENGTH] = {0};
  enum ProgramAction programAction;
  size_t keyLength;
  FILE *keyFile = stdin;
  FILE *inFile = stdin;
  FILE *outFile = stdout;

  if(argc < 3) {
    printUsage();
    return INVALID_ARGUMENT;
  }

  if(strcmp(argv[1], "encrypt") == 0) {
    programAction = ENCRYPT;
  } else if(strcmp(argv[1], "decrypt") == 0) {
    programAction = DECRYPT;
  } else if(strcmp(argv[1], "hash") == 0) {
    programAction = HASH;
  } else {
    printf("Unknown program action.\n");
    printUsage();
    return INVALID_ARGUMENT;
  }

  keyFile = fopen(argv[2],"rb");
  if(keyFile == NULL) {
    printf("Key file not found.");
    return FILE_NOT_FOUND;
  }
  fgets(key, MAX_KEY_LENGTH, keyFile);
  keyLength = strlen(key);
  fclose(keyFile);

  initialiseState();
  absorb(key, keyLength);

  inFile = fopen(argv[3], "rb");
  if(inFile == NULL) {
    printf("Input file not found.");
    return FILE_NOT_FOUND;
  }

  outFile = fopen(argv[4],"wb");
  if(outFile == NULL) {
    printf("Output file not found.");
    return FILE_NOT_FOUND;
  }

  if(a != 0) {
    shuffle();
  }

  int inputChar;

  if(programAction == ENCRYPT) {

    inputChar = fgetc(inFile);
    while(inputChar != EOF) {

      byte r;

      r = (byte)inputChar + drip();

      fputc(r, outFile);
      inputChar = fgetc(inFile);
    }

  } else if (programAction == DECRYPT) {

    inputChar = fgetc(inFile);
    while(inputChar != EOF) {
      byte r;

      r = (byte)inputChar - drip();

      fputc(r, outFile);
      inputChar = fgetc(inFile);
    }

  } else if (programAction == HASH) {
    printf("Not implemented yet.");
  }

  fclose(inFile);
  fclose(outFile);

  return 0;
}
