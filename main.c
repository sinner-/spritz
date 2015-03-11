#include <stdio.h>
#include <string.h>
#include <time.h>

#include "spritz.h"

#define FILE_NOT_FOUND 2
#define EXEC_FORMAT_ERROR 8
#define INVALID_ARGUMENT 22

#define MAX_KEY_LENGTH 245
#define IV_LENGTH 10

enum ProgramAction
{
  ENCRYPT,
  DECRYPT,
  HASH
};

void printUsage()
{
  printf("Usage:\n");
  printf("spritz [action] [key] [input file] [output file]\n");
  printf("Options:\n");
  printf("action: encrypt / decrypt / hash.\n");
  printf("key: key string (max key length is 245 chars.\n");
  printf("input file: path to input file.\n");
  printf("output file: path to output file.\n");
}

int main(int argc, char* argv[])
{
  enum ProgramAction programAction;
  char* key;
  size_t keyLength;
  byte iv[IV_LENGTH];
  size_t iter;
  FILE *inFile = stdin;
  FILE *outFile = stdout;
  int inputChar;

  if(argc < 3)
  {
    printUsage();
    return INVALID_ARGUMENT;
  }

  if(strcmp(argv[1], "encrypt") == 0)
  {
    programAction = ENCRYPT;
  }
  else if(strcmp(argv[1], "decrypt") == 0)
  {
    programAction = DECRYPT;
  }
  else if(strcmp(argv[1], "hash") == 0)
  {
    programAction = HASH;
  }
  else
  {
    printf("Unknown program action.\n");
    printUsage();
    return INVALID_ARGUMENT;
  }

  key = argv[2];
  keyLength = strlen(key);

  inFile = fopen(argv[3], "rb");
  if(inFile == NULL)
  {
    printf("Input file not found.");
    return FILE_NOT_FOUND;
  }

  outFile = fopen(argv[4],"wb");
  if(outFile == NULL)
  {
    printf("Output file not found.");
    return FILE_NOT_FOUND;
  }

  if(programAction == ENCRYPT)
  {
    srand(time(NULL));

    for (iter = 0; iter < IV_LENGTH; iter++)
    {
      iv[iter] = (byte)rand();
    }

    fwrite(iv, 1, IV_LENGTH, outFile);
  }
  else if (programAction == DECRYPT)
  {
    int readiv;

    readiv = fread(iv, 1, IV_LENGTH, inFile);

    if (readiv != IV_LENGTH)
    {
      printf("Could not read iv.");
      return EXEC_FORMAT_ERROR;
    }
  }

  initialiseState();
  absorb((byte*)key, keyLength);
  absorbStop();
  absorb(iv, IV_LENGTH);

  inputChar = fgetc(inFile);

  if(programAction == ENCRYPT)
  {
    while(inputChar != EOF) {

      byte r;

      r = ((byte)inputChar) + drip();

      fputc(r, outFile);

      inputChar = fgetc(inFile);
    }

  }
  else if (programAction == DECRYPT)
  {
    while(inputChar != EOF) {

      byte r;

      r = ((byte)inputChar) - drip();

      fputc(r, outFile);

      inputChar = fgetc(inFile);
    }

  }
  else if (programAction == HASH)
  {
    printf("Not implemented yet.");
  }

  fclose(inFile);
  fclose(outFile);

  return 0;
}
