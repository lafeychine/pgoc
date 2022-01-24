#include "alloc.h"
#include <unistd.h>

chunk_t *split_top(size_t size)
{
    chunk_t *chunk = arena.top_chunk;
    size_t old_size = GET_SIZE(arena.top_chunk);

    SET_SIZE_HEAD(chunk, size);
    arena.top_chunk = GET_NEXT_CHUNK(chunk);
    CLEAR_FLAGS(arena.top_chunk);
    SET_SIZE_HEAD(arena.top_chunk, old_size - size);
    return (chunk);
}

static int push_break(size_t increase, size_t old_size)
{
    void *ptr = sbrk(increase);

    SET_SIZE_HEAD(arena.top_chunk, increase + old_size);
    return (ptr != (void *)-1);
}

static int first_push_break(size_t increase)
{
    arena.top_chunk = sbrk(increase);
    if (arena.top_chunk == (void *)-1) {
        return (0);
    }
    initialise_bins();
#if defined SHOW_MEMORY || ALLOC_GARBAGE_COLLECTOR
    arena.first_chunk = arena.top_chunk;
#endif /* SHOW_MEMORY || ALLOC_GARBAGE_COLLECTOR */
    CLEAR_FLAGS(arena.top_chunk);
    SET_SIZE_HEAD(arena.top_chunk, increase);
    return (1);
}

chunk_t *alloc_top(size_t size)
{
    size_t page = getpagesize();
    size_t increase = ALIGN_CEIL(size, page);

    if (arena.top_chunk == NULL) {
        if (!(first_push_break(increase))) {
            return (NULL);
        }
    }
    if (size + CHUNK_SIZE > GET_SIZE(arena.top_chunk)) {
        if (!(push_break(increase, GET_SIZE(arena.top_chunk)))) {
            return (NULL);
        }
    }
    return (split_top(size));
}

void release_top(void)
{
    size_t page = getpagesize();
    size_t decrease;

    if (GET_SIZE(arena.top_chunk) > page * 2) {
        decrease = ALIGN(GET_SIZE(arena.top_chunk) - page, page);
        SET_SIZE_HEAD(arena.top_chunk, GET_SIZE(arena.top_chunk) - decrease);
        brk(GET_NEXT_CHUNK(arena.top_chunk));
    }
}
