#include "alloc.h"
#include <unistd.h>

#ifdef SHOW_MEMORY
static void put_nbr(size_t ptr, const char *base, unsigned int base_length)
{
    if (ptr >= base_length) {
        put_nbr(ptr / base_length, base, base_length);
    } else if (base_length == 16) {
        write(1, "0x", 2);
    }
    write(1, base + ptr % base_length, 1);
}

static void print_chunk(chunk_t *chunk)
{
    char const base[] = "0123456789abcdef";
    put_nbr((size_t)CHUNK_TO_MEM(chunk), base, 16);
    write(1, " - ", 3);
    put_nbr((size_t)CHUNK_TO_MEM(chunk) + GET_REAL_SIZE(chunk), base, 16);
    write(1, " : ", 3);
    put_nbr(GET_REAL_SIZE(chunk), "0123456789", 10);
    write(1, " bytes\n", 7);
}

static void print_break(void)
{
    write(1, "break: ", 7);
    put_nbr((size_t)sbrk(0), "0123456789abcdef", 16);
    write(1, "\n", 1);
}

void show_alloc_mem(void)
{
    chunk_t *tmp = arena.first_chunk;

    print_break();
    if (!(tmp))
        return;
    while (tmp != arena.top_chunk) {
        if (!(IS_CHUNK_FREE(tmp))) {
            print_chunk(tmp);
        }
        tmp = GET_NEXT_CHUNK(tmp);
    }
}
#else
void show_alloc_mem(void)
{
    write(1, "Enable SHOW_MEMORY flag to use this functionality\n", 50);
}
#endif /* SHOW_MEMORY */
