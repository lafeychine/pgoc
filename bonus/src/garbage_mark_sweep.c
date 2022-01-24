#include "alloc.h"

#ifdef ALLOC_GARBAGE_COLLECTOR
static int is_in_range_heap(void *ptr)
{
    chunk_t *chunk = arena.first_chunk;

    if (!((void *)arena.first_chunk < ptr && ptr < (void *)arena.top_chunk)) {
        return (0);
    }
    while (chunk != arena.top_chunk) {
        if (chunk == MEM_TO_CHUNK(ptr)) {
            return (1);
        }
        chunk = GET_NEXT_CHUNK(chunk);
    }
    return (0);
}

void check_heap(chunk_t *chunk)
{
    void *ptr = CHUNK_TO_MEM(chunk);

    if (IS_ACTIVE_CHUNK(chunk)) {
        return;
    }
    SET_ACTIVE_CHUNK(chunk);
    while (ptr < CHUNK_TO_MEM(chunk) + GET_SIZE(chunk) - CHUNK_SIZE) {
        if (is_in_range_heap(*(void **)ptr)) {
            check_heap(MEM_TO_CHUNK(*(void **)ptr));
        }
        ptr = ptr + POINTER_ALIGNMENT;
    }
}

void launch_garbage_collector(void)
{
    void *ptr = arena.stack;

    while (ptr > (void *)(&(ptr))) {
        if (is_in_range_heap(*(void **)ptr)) {
            check_heap(MEM_TO_CHUNK(*(void **)ptr));
        }
        ptr = ptr - POINTER_ALIGNMENT;
    }
}
#endif /* ALLOC_GARBAGE_COLLECTOR */
