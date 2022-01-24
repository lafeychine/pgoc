#include "alloc.h"

chunk_t *merge_backward(chunk_t *chunk)
{
    size_t size = GET_SIZE(chunk);

    if (IS_PREV_CHUNK_MERGEABLE(chunk)) {
        chunk = GET_PREV_CHUNK(chunk);
        unlink_bin(chunk);
        SET_SIZE_HEAD(chunk, GET_SIZE(chunk) + size);
    }
    return (chunk);
}

void merge_forward(chunk_t *chunk)
{
    chunk_t *tmp = GET_NEXT_CHUNK(chunk);

    if (tmp != arena.top_chunk) {
        if (!(IS_PREV_CHUNK_MERGEABLE(GET_NEXT_CHUNK(tmp)))) {
            return;
        }
        unlink_bin(tmp);
    } else {
        arena.top_chunk = chunk;
    }
    SET_SIZE_HEAD(chunk, GET_SIZE(chunk) + GET_SIZE(tmp));
}
