#include "arc.h"

void* arc_alloc(size_t size)
{
    Arc* ptr = malloc(size + sizeof(Arc));
    ptr->ref_count = 1;
    return ptr->data;
}

void arc_inc_ref(void* ptr) 
{
    Arc* arc = (Arc*)(ptr - sizeof(Arc) / sizeof(void*));
    __sync_fetch_and_add(&arc->ref_count, 1);
}

void arc_dec_ref(void* ptr)
{
    Arc* arc = (Arc*)(ptr - sizeof(Arc) / sizeof(void*));
    size_t count = __sync_sub_and_fetch(&arc->ref_count, 1);
    if(count == 0)
        free(arc);
}