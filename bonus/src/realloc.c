#include "alloc.h"
#include <string.h>

static chunk_t *split_chunk(chunk_t *chunk, size_t size)
{
    size_t old_size = GET_SIZE(chunk);
    chunk_t *free;

    SET_SIZE_HEAD(chunk, size);
    free = GET_NEXT_CHUNK(chunk);
    CLEAR_FLAGS(free);
    SET_SIZE_HEAD(free, old_size - size);
    link_forward_bin(GET_UNSORTED_BIN(arena), free);
    return (chunk);
}

static chunk_t *malloc_copy_free(chunk_t *chunk, size_t size)
{
    chunk_t *ptr = my_malloc(size);
    size_t size_chunk;
    size_t size_copy;

    if (ptr && chunk) {
        size_chunk = GET_SIZE(chunk) - CHUNK_SIZE;
        size = size - CHUNK_SIZE;
        size_copy = ((size_chunk < size) ? size_chunk : size);
        memcpy(CHUNK_TO_MEM(ptr), CHUNK_TO_MEM(chunk), size_copy);
    }
    return (ptr);
}

static chunk_t *realloc_alloc(chunk_t *chunk, size_t size)
{
    size_t size_merge = GET_SIZE(chunk) + GET_SIZE(GET_NEXT_CHUNK(chunk));
    int is_mergeable = IS_PREV_CHUNK_MERGEABLE(GET_NEXT_CHUNK(chunk));

    if ((is_mergeable) && (size + MIN_CHUNK_SIZE <= size_merge)) {
        merge_forward(chunk);
        if (chunk == arena.top_chunk) {
            split_top(size);
        }
        return (chunk);
    }
    return (malloc_copy_free(chunk, size));
}

chunk_t *my_realloc(chunk_t *chunk, size_t size)
{
    if (!(chunk)) {
        return (malloc_copy_free(chunk, size));
    }
    if (size + MIN_CHUNK_SIZE <= GET_SIZE(chunk)) {
        return (split_chunk(chunk, size));
    } else if (size + MIN_CHUNK_SIZE < GET_SIZE(chunk)) {
        return (chunk);
    }
    return (realloc_alloc(chunk, size));
}
