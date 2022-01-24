#include "alloc.h"

void unsorted_get_fastbins(void)
{
    size_t size = MIN_CHUNK_SIZE;
    chunk_t *node;

    while (size < MAX_FAST_SIZE) {
        node = get_fast_bin(size);
        while (node) {
            link_forward_bin(GET_UNSORTED_BIN(arena), node);
            node = get_fast_bin(size);
        }
        size = size + ALLOC_ALIGNMENT;
    }
}

chunk_t *unsorted_dispatch(size_t size)
{
    chunk_t *unsorted = GET_UNSORTED_BIN(arena);
    chunk_t *node = unsorted->next;
    chunk_t *tmp;

    if (unsorted->next == unsorted->prev) {
        return (NULL);
    }
    while (node != unsorted) {
        tmp = node->next;
        unlink_bin(node);
        if (GET_SIZE(node) == size) {
            return (node);
        }
        put_bin(node);
        node = tmp;
    }
    return (NULL);
}
