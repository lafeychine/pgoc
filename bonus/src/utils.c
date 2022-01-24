#include "alloc.h"

void initialise_bins(void)
{
    size_t index = 0;
    chunk_t *bins;

    for (index = 0; index < NUMBER_BINS; index++) {
        bins = GET_BIN(arena, index);
        bins->next = bins;
        bins->prev = bins;
    }
}

void link_forward_bin(chunk_t *chunk_bin, chunk_t *chunk)
{
    chunk->next = chunk_bin->next;
    chunk->prev = chunk_bin;
    chunk_bin->next->prev = chunk;
    chunk_bin->next = chunk;
}

void unlink_bin(chunk_t *chunk)
{
    chunk->prev->next = chunk->next;
    chunk->next->prev = chunk->prev;
}
