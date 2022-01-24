#include "alloc.h"

chunk_t *get_bin(size_t size)
{
    chunk_t *bin = GET_BIN(arena, GET_INDEX(size));
    chunk_t *node = bin->next;

    if (bin->next == bin->prev) {
        return (NULL);
    }
    while (node != bin) {
        if (GET_SIZE(node) == size) {
            unlink_bin(node);
            return (node);
        }
        node = node->next;
    }
    return (NULL);
}

void put_bin(chunk_t *chunk)
{
    chunk_t *bin = GET_BIN(arena, GET_INDEX(GET_SIZE(chunk)));
    chunk_t *node = bin->next;
    chunk_t *tmp;

    while (node != bin) {
        tmp = node->next;
        if (GET_SIZE(node) >= GET_SIZE(chunk)) {
            link_forward_bin(node->prev, chunk);
            return;
        }
        node = tmp;
    }
    link_forward_bin(bin->prev, chunk);
}
