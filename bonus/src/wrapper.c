#include "alloc.h"
#include <pthread.h>
#include <string.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void set_alloc_flags(chunk_t *chunk, __attribute__((unused)) size_t size)
{
    if (chunk) {
#ifdef SHOW_MEMORY
        SET_REAL_SIZE(chunk, size);
        CLEAR_CHUNK_FREE(chunk);
#endif /* SHOW_MEMORY */
#ifdef ALLOC_GARBAGE_COLLECTOR
        CLEAR_ACTIVE_CHUNK(chunk);
#endif /* ALLOC_GARBAGE_COLLECTOR */
        CLEAR_PREV_CHUNK_MERGEABLE(GET_NEXT_CHUNK(chunk));
    }
}

void *malloc(size_t size)
{
    chunk_t *chunk;

    pthread_mutex_lock(&(mutex));
#ifdef ALLOC_GARBAGE_COLLECTOR
    check_garbage_collector(size);
#endif /* ALLOC_GARBAGE_COLLECTOR */
    chunk = my_malloc(PAD_SIZE(size));
    set_alloc_flags(chunk, size);
    pthread_mutex_unlock(&(mutex));
    return ((chunk) ? CHUNK_TO_MEM(chunk) : NULL);
}

void *realloc(void *ptr, size_t size)
{
    chunk_t *pointer = ((ptr) ? MEM_TO_CHUNK(ptr) : NULL);
    chunk_t *chunk;

    pthread_mutex_lock(&(mutex));
    chunk = my_realloc(pointer, PAD_SIZE(size));
    set_alloc_flags(chunk, size);
    pthread_mutex_unlock(&(mutex));
    return ((chunk) ? CHUNK_TO_MEM(chunk) : NULL);
}

void free(__attribute__((unused)) void *ptr)
{
#ifndef ALLOC_GARBAGE_COLLECTOR
    if (!(ptr)) {
        return;
    }
    pthread_mutex_lock(&(mutex));
#ifdef SHOW_MEMORY
    SET_CHUNK_FREE(MEM_TO_CHUNK(ptr));
#endif /* SHOW_MEMORY */
    my_free(MEM_TO_CHUNK(ptr));
    pthread_mutex_unlock(&(mutex));
#endif /* ALLOC_GARBAGE_COLLECTOR */
}

void *calloc(size_t nmemb, size_t size)
{
    chunk_t *chunk;

    pthread_mutex_lock(&(mutex));
#ifdef ALLOC_GARBAGE_COLLECTOR
    check_garbage_collector(size);
#endif /* ALLOC_GARBAGE_COLLECTOR */
    chunk = my_malloc(PAD_SIZE(size * nmemb));
    set_alloc_flags(chunk, size);
    pthread_mutex_unlock(&(mutex));
    if (!(chunk)) {
        return (NULL);
    }
    memset(CHUNK_TO_MEM(chunk), 0, PAD_SIZE(size * nmemb) - CHUNK_SIZE);
    return (CHUNK_TO_MEM(chunk));
}
