#include "alloc.h"

#ifdef ALLOC_GARBAGE_COLLECTOR
__attribute__((constructor)) void init_garbage_collector(void)
{
    int variable;

    arena.stack = &(variable);
}

static void unmark_all_node(void)
{
    chunk_t *chunk = arena.first_chunk;
    chunk_t *tmp_chunk;

    if (!(chunk)) {
        return (0);
    }
    while (chunk != arena.top_chunk) {
        if (IS_ACTIVE_CHUNK(chunk)) {
            CLEAR_ACTIVE_CHUNK(chunk);
        } else {
            tmp_chunk = GET_PREV_CHUNK(chunk);
            my_free(chunk);
            if (tmp_chunk == arena.top_chunk) {
                break;
            }
            chunk = tmp_chunk;
        }
        chunk = GET_NEXT_CHUNK(chunk);
    }
    return (0);
}

void check_garbage_collector(size_t size)
{
    arena.allocation_mem += size;
    arena.allocation_nb += 1;
    if ((arena.allocation_nb >= (size_t)ALLOCATION_THRESHOLD) ||
        (arena.allocation_mem >= (size_t)MEMORY_THRESHOLD)) {
        launch_garbage_collector();
        unmark_all_node();
        arena.allocation_mem = 0;
        arena.allocation_nb = 0;
    }
}
#endif /* ALLOC_GARBAGE_COLLECTOR */
