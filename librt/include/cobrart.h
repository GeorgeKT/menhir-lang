#ifndef COBRART_H
#define COBRART_H

#include <stdint.h>

typedef struct 
{
    const char* data;
    uint64_t len;
    uint64_t offset;
} COBRAString;

int print(COBRAString* s);
int println(COBRAString* s);

#endif