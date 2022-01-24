#include "alloc.h"

chunk_t *get_fast_bin(size_t size)
{
    chunk_t **list = GET_FAST_BIN(arena, GET_FAST_INDEX(size));
    chunk_t *chunk;

    if (!(*(list))) {
        return (NULL);
    }
    chunk = *(list);
    *(list) = chunk->next;
    return (chunk);
}

void put_fast_bin(chunk_t *node)
{
    chunk_t **list = GET_FAST_BIN(arena, GET_FAST_INDEX(GET_SIZE(node)));

    node->next = *(list);
    *(list) = node;
}
