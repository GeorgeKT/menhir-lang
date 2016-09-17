#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include "cobrart.h"


int print(COBRAString* s)
{
    const char* ptr = s->data + s->offset;
    const char* end = s->data + s->offset + s->len;
    while(ptr < end)
    {
        int ret = write(STDOUT_FILENO, s->data + s->offset, s->len);
        if(ret < 0)
        {
            if(errno == EINTR)
                continue;
            else
                return ret;
        }
        else 
        {
            ptr += ret;
        }
    }

    return s->len;
}

int println(COBRAString* s)
{
    int ret = print(s);
    if(ret < 0)
        return ret;
    printf("\n");
    return ret + 1;
}