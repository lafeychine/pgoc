override SRC			=	src/bins.c			\
					src/fast.c			\
					src/free.c			\
					src/garbage.c			\
					src/garbage_mark_sweep.c	\
					src/malloc.c			\
					src/merge.c			\
					src/realloc.c			\
					src/show.c			\
					src/top.c			\
					src/unsorted.c			\
					src/utils.c			\
					src/wrapper.c

BUILDDIR			=	build

override OBJ			=	$(SRC:%.c=$(BUILDDIR)/%.o)

CC				=	gcc
override CPPFLAGS		=	-Iinclude -D_GNU_SOURCE -DALLOC_GARBAGE_COLLECTOR
override CFLAGS			+=	-W -Wall -pedantic -fPIC -std=gnu11
override RMFLAGS		+=	-r

LD				=	$(CC)
override LDFLAGS		+=	-shared

NAME				=	garbage_collector.so



all:				$(NAME) 

clean:
				$(RM) $(RMFLAGS) $(BUILDDIR) $(NAME)


$(NAME):			$(OBJ)
				$(LD) $(LDFLAGS) $(LDLIBS) $^ -o $@



.SECONDEXPANSION:
$(BUILDDIR)/%.o:		override CPPFLAGS += -MT $@ -MMD -MP -MF $(@:.o=.d)
$(BUILDDIR)/%.o:		%.c | $$(@D)/.
		            	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(BUILDDIR)/%.c:		%.l | $$(@D)/.
	 			$(LEX) -o$@ $(LFLAGS) $<

$(BUILDDIR)/%.c:		%.y | $$(@D)/.
				$(YACC) $(YFLAGS) $< -o $@

$(BUILDDIR)/.:
				mkdir $@

$(BUILDDIR)%/.:
				mkdir -p $@

.PRECIOUS:			$(BUILDDIR)/. $(BUILDDIR)%/.

.PHONY:				all clean submake

-include $(OBJ:.o=.d)
