#ifndef ALLOC_H_
#define ALLOC_H_

/* === [INCLUDE FILES] ====================================================== */

#include <limits.h>
#include <stddef.h>

/* === [INCLUDE FILES] ====================================================== */
/* === [MAGIC NUMBERS] ====================================================== */

/* Note: Every field are magic dependant */
/* sizeof(size_t) and sizeof(pointers) must be 8 */

/* Default alignment at double word */
#define ALLOC_ALIGNMENT (sizeof(size_t) * 2)

/* Size of chunk_t */
#ifdef SHOW_MEMORY
#define CHUNK_SIZE 32
#else
#define CHUNK_SIZE 16
#endif /* SHOW_MEMORY */

/* Minimum size of a chunk */
#define MIN_CHUNK_SIZE (CHUNK_SIZE + ALLOC_ALIGNMENT)

/* Fast bins infos */
#define MAX_FAST_SIZE 128
#define NUMBER_FAST_BINS ((MAX_FAST_SIZE - MIN_CHUNK_SIZE) / ALLOC_ALIGNMENT)

/* Small bins infos */
#define MAX_SMALL_SIZE 512
#define NUMBER_SMALL_BINS ((MAX_SMALL_SIZE - MIN_CHUNK_SIZE) / ALLOC_ALIGNMENT)

/* Large bins infos */
#define NUMBER_LARGE_BINS 71

/* Bins infos */
#define NUMBER_BINS (1 + NUMBER_SMALL_BINS + NUMBER_LARGE_BINS)

/* Flags for chunk’s uses */
#define FLAG_PREV_CHUNK_MERGEABLE 0x1
#define FLAG_GC_ACTIVE_CHUNK 0x2
#define FLAG_SM_FREE_CHUNK 0x4

/* Bits for mask off the flags */
#define FLAG_BITS                                                              \
    (FLAG_PREV_CHUNK_MERGEABLE | FLAG_GC_ACTIVE_CHUNK | FLAG_SM_FREE_CHUNK)

#ifdef ALLOC_GARBAGE_COLLECTOR

/* Default pointer alignment */
#define POINTER_ALIGNMENT 4

/* Number of allocation’s threshold before launching Garbage collection */
/* (Default: 50 Allocations) */
#define ALLOCATION_THRESHOLD 1 // 50

/* Memory’s threshold before launching Garbage collection */
/* (Default: 1Mb) */
#define MEMORY_THRESHOLD 1 //(1 * 1000 * 1000)

#endif /* ALLOC_GARBAGE_COLLECTOR */

/* === [MAGIC NUMBERS] ====================================================== */
/* === [FORWARD DECLARATION] ================================================ */

/* Arena: Information about actual thread */
typedef struct arena_s arena_t;

/* Chunk: Data of a chunk */
typedef struct chunk_s chunk_t;

/* Arena: Global variable */
/* Note: This variable is defined this header file, and thus, will be defined */
/* multiple times. It is possible because it is a "tentative definition". */
/* -> ISO/IEC 9998:TC2 [6.9.2] */

arena_t arena;

/* === [FORWARD DECLARATION] ================================================ */
/* === [STRUCTURES] ========================================================= */

struct arena_s {
    chunk_t *fast_bins[NUMBER_FAST_BINS];
    chunk_t *bins[NUMBER_BINS * 2];
    chunk_t *top_chunk;
    arena_t *next;
#if defined SHOW_MEMORY || ALLOC_GARBAGE_COLLECTOR
    chunk_t *first_chunk;
#endif /* SHOW_MEMORY || ALLOC_GARBAGE_COLLECTOR */
#ifdef ALLOC_GARBAGE_COLLECTOR
    void *stack;
    size_t allocation_nb;
    size_t allocation_mem;
#endif /* ALLOC_GARBAGE_COLLECTOR */
};

struct chunk_s {
    size_t prev_size;
    size_t size;
#ifdef SHOW_MEMORY
    size_t real_size;
#endif /* SHOW_MEMORY */
    chunk_t *next;
    chunk_t *prev;
};

/* === [STRUCTURES] ========================================================= */
/* === [MACROS HELPERS] ===================================================== */

/* === [ALIGNMENT MANAGEMENT] === */

/* Set alignement */
#define ALIGN(_size, _align) ((_size) & ~((_align)-1))

/* Alignment ceil */
#define ALIGN_CEIL(_size, _align) ALIGN((_size) + (_align)-1, (_align))

/* Conversion from chunk to user pointers, and back */
#define CHUNK_TO_MEM(_chunk) ((void *)(((char *)(_chunk)) + CHUNK_SIZE))
#define MEM_TO_CHUNK(_mem) ((chunk_t *)(((char *)(_mem)) - CHUNK_SIZE))

/* Pad a size with the chunk meta-data */
#define PAD_REAL_SIZE(_size) ((_size) + CHUNK_SIZE + (ALLOC_ALIGNMENT - 1))

#define PAD_SIZE(_size)                                                        \
    ((size_t)(PAD_REAL_SIZE(_size) <= MIN_CHUNK_SIZE)                          \
         ? MIN_CHUNK_SIZE                                                      \
         : (ALIGN(PAD_REAL_SIZE(_size), ALLOC_ALIGNMENT)))

/* === [ALIGNMENT MANAGEMENT] === */
/* === [CHUNK’S FLAG MANAGEMENT] === */

/* Flag in size’s chunk. Previous adjacent chunk use’s state */
#define IS_PREV_CHUNK_MERGEABLE(_chunk)                                        \
    ((_chunk)->size & FLAG_PREV_CHUNK_MERGEABLE)
#define SET_PREV_CHUNK_MERGEABLE(_chunk)                                       \
    ((_chunk)->size |= FLAG_PREV_CHUNK_MERGEABLE)
#define CLEAR_PREV_CHUNK_MERGEABLE(_chunk)                                     \
    ((_chunk)->size &= ~(FLAG_PREV_CHUNK_MERGEABLE))

#ifdef ALLOC_GARBAGE_COLLECTOR

/* Flag in size’s chunk. Mark chunk as active chunk */
#define IS_ACTIVE_CHUNK(_chunk) ((_chunk)->size & FLAG_GC_ACTIVE_CHUNK)
#define SET_ACTIVE_CHUNK(_chunk) ((_chunk)->size |= FLAG_GC_ACTIVE_CHUNK)
#define CLEAR_ACTIVE_CHUNK(_chunk) ((_chunk)->size &= ~(FLAG_GC_ACTIVE_CHUNK))

#endif /* ALLOC_GARBAGE_COLLECTOR */

#ifdef SHOW_MEMORY

/* Flag in size’s chunk. Mark chunk as freed */
#define IS_CHUNK_FREE(_chunk) ((_chunk)->size & FLAG_SM_FREE_CHUNK)
#define SET_CHUNK_FREE(_chunk) ((_chunk)->size |= FLAG_SM_FREE_CHUNK)
#define CLEAR_CHUNK_FREE(_chunk) ((_chunk)->size &= ~(FLAG_SM_FREE_CHUNK))

#endif /* SHOW_MEMORY */

#define CLEAR_FLAGS(_chunk) ((_chunk)->size &= ~(FLAG_BITS))

/* === [CHUNK’S FLAG MANAGEMENT] === */
/* === [ACTUAL CHUNK MANAGEMENT] === */

/* Get size, ignoring flag’s bits */
#define GET_SIZE(_chunk) ((_chunk)->size & ~(FLAG_BITS))

/* Set size, without distrubing its flag’s bits */
#define SET_SIZE_HEAD(_chunk, _new_size)                                       \
    ((_chunk)->size = (((_chunk)->size & FLAG_BITS) | (_new_size)))

/* Set size at footer, when chunk is freed */
#define SET_SIZE_FOOT(_chunk, _new_size)                                       \
    (((chunk_t *)(((char *)(_chunk)) + (_new_size)))->prev_size = (_new_size))

#ifdef SHOW_MEMORY

/* Get real size */
#define GET_REAL_SIZE(_chunk) ((_chunk)->real_size)

/* Set real size */
#define SET_REAL_SIZE(_chunk, _new_size) ((_chunk)->real_size) = (_new_size)

#endif /* SHOW_MEMORY */

/* === [ACTUAL CHUNK MANAGEMENT] === */
/* === [ADJACENT CHUNK MANAGEMENT] === */

/* Pointer to the next chunk */
#define GET_NEXT_CHUNK(_chunk)                                                 \
    ((chunk_t *)(((char *)(_chunk)) + GET_SIZE(_chunk)))

/* Pointer to the previous chunk */
#define GET_PREV_CHUNK(_chunk)                                                 \
    ((chunk_t *)(((char *)(_chunk)) - (_chunk)->prev_size))

/* === [ADJACENT CHUNK MANAGEMENT] === */
/* === [BINS GETTER] === */

/* Get chunk’s bin by index */
#define GET_BIN(_arena, _index)                                                \
    ((chunk_t *)(((char *)&((_arena).bins[(_index) * (2)])) -                  \
                 offsetof(chunk_t, next)))

/* Get the next chunk’s bin: Equals to ++(arena.bins) */
#define GET_NEXT_BIN(_chunk)                                                   \
    ((chunk_t *)(((char *)(_chunk)) + sizeof(chunk_t *) << 1))

/* Get chunk’s bin by index */
#define GET_FAST_BIN(_arena, index) ((chunk_t **)&((_arena).fast_bins[index]))

/* Get unsorted chunk’s bin */
#define GET_UNSORTED_BIN(_arena) (GET_BIN(_arena, 0))

/* === [BINS GETTER] === */
/* === [BINS RANGE-INDEX] === */

/* Check if a size correspond to a fastbin */
#define IS_FAST_RANGE(_size) (((_size) >> 7) == 0)

/* Check if a size correspond to a small range */
#define IS_SMALL_RANGE(_size) (((_size) >> 9) == 0)

/* Check if a size correspond to a large range */
#define IS_LARGE_RANGE(_size) (!(IS_SMALL_RANGE(_size)))

/* Get fast chunk’s bin index */
#define GET_FAST_INDEX(_size)                                                  \
    (((_size) >> 4) - (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT))

/* Get small chunk’s bin index */
#define GET_SMALL_INDEX(_size)                                                 \
    (((_size) >> 4) - (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT) + 1)

/* Get large chunk’s index */
#define GET_LARGE_INDEX(_size)                                                 \
    (((_size >> 6) <= 39)                                                      \
         ? ((size_t)(_size >> 6) + 25 - (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT))    \
         : (((_size >> 9) <= 24)                                               \
                ? ((size_t)(_size >> 9) + 60 -                                 \
                   (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT))                         \
                : (((_size >> 12) <= 16)                                       \
                       ? ((size_t)(_size >> 12) + 81 -                         \
                          (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT))                  \
                       : (((_size >> 15) <= 7)                                 \
                              ? ((size_t)(_size >> 15) + 95 -                  \
                                 (MIN_CHUNK_SIZE / ALLOC_ALIGNMENT))           \
                              : (NUMBER_BINS - 1)))))

#define GET_INDEX(_size)                                                       \
    ((IS_SMALL_RANGE(_size)) ? GET_SMALL_INDEX(_size) : GET_LARGE_INDEX(_size))

/* === [BINS RANGE-INDEX] === */

/* === [MACROS HELPERS] ===================================================== */
/* === [PROTOTYPES] ========================================================= */

/* Bins management */
chunk_t *get_fast_bin(size_t size);
chunk_t *get_bin(size_t size);
void put_fast_bin(chunk_t *node);
void put_bin(chunk_t *node);

/* Bins utils */
void link_forward_bin(chunk_t *chunk_bin, chunk_t *chunk);
void unlink_bin(chunk_t *chunk);
void initialise_bins(void);

/* Merge functions */
chunk_t *merge_backward(chunk_t *chunk);
void merge_forward(chunk_t *chunk);

/* Unsorted bin functions */
chunk_t *unsorted_dispatch(size_t size);
void unsorted_get_fastbins(void);

/* Top chunk functions */
chunk_t *alloc_top(size_t size);
chunk_t *split_top(size_t size);
void release_top(void);

/* Real functions */
chunk_t *my_malloc(size_t size);
chunk_t *my_realloc(chunk_t *ptr, size_t size);
void my_free(chunk_t *ptr);

#ifdef SHOW_MEMORY

/* Show alloc memory */
void show_alloc_mem(void);

#endif /* SHOW_MEMORY */

#ifdef ALLOC_GARBAGE_COLLECTOR

/* Garbage collector */
void check_garbage_collector(size_t size);
void launch_garbage_collector(void);

#endif /* ALLOC_GARBAGE_COLLECTOR */

/* === [PROTOTYPES] ========================================================= */

#endif /* ALLOC_H_ */
