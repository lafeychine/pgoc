#include "alloc.h"
#include <stddef.h>

static chunk_t *my_large_malloc(size_t size)
{
    chunk_t *chunk = NULL;

    unsorted_get_fastbins();
    chunk = unsorted_dispatch(size);
    chunk = ((chunk) ? chunk : get_bin(size));
    return ((chunk) ? chunk : alloc_top(size));
}

static chunk_t *my_small_malloc(size_t size)
{
    chunk_t *chunk = get_bin(size);

    chunk = ((chunk) ? chunk : unsorted_dispatch(size));
    if (!(chunk)) {
        unsorted_get_fastbins();
        chunk = unsorted_dispatch(size);
    }
    return ((chunk) ? chunk : alloc_top(size));
}

static chunk_t *my_fast_malloc(size_t size)
{
    chunk_t *chunk = get_fast_bin(size);

    return ((chunk) ? chunk : my_small_malloc(size));
}

chunk_t *my_malloc(size_t size)
{
    if (size == 0) {
        return (NULL);
    }
    if (IS_FAST_RANGE(size)) {
        return (my_fast_malloc(size));
    }
    if (IS_SMALL_RANGE(size)) {
        return (my_small_malloc(size));
    }
    return (my_large_malloc(size));
}
