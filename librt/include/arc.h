#ifndef ARC_H
#define ARC_H

#include <stdlib.h>

typedef struct
{
    size_t ref_count;
    size_t data[];
} Arc;

#endif