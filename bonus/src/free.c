#include "alloc.h"

void my_free(chunk_t *chunk)
{
    if (IS_FAST_RANGE(GET_SIZE(chunk))) {
        put_fast_bin(chunk);
        return;
    }
    chunk = merge_backward(chunk);
    merge_forward(chunk);
    if (chunk == arena.top_chunk) {
        release_top();
        return;
    }
    SET_PREV_CHUNK_MERGEABLE(GET_NEXT_CHUNK(chunk));
    SET_SIZE_FOOT(chunk, GET_SIZE(chunk));
    link_forward_bin(GET_UNSORTED_BIN(arena), chunk);
}
