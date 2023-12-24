CC=clang
CFLAGS=-O3 -g -fomit-frame-pointer -Isrc/libdivsufsort/include -Isrc
OBJDIR=obj
LDFLAGS=

$(OBJDIR)/%.o: src/../%.c
	@mkdir -p '$(@D)'
	$(CC) $(CFLAGS) -c $< -o $@

APP := lzsa

OBJS += $(OBJDIR)/src/lzsa.o
OBJS += $(OBJDIR)/src/dictionary.o
OBJS += $(OBJDIR)/src/expand_block_v1.o
OBJS += $(OBJDIR)/src/expand_block_v2.o
OBJS += $(OBJDIR)/src/expand_context.o
OBJS += $(OBJDIR)/src/expand_inmem.o
OBJS += $(OBJDIR)/src/expand_streaming.o
OBJS += $(OBJDIR)/src/frame.o
OBJS += $(OBJDIR)/src/matchfinder.o
OBJS += $(OBJDIR)/src/shrink_block_v1.o
OBJS += $(OBJDIR)/src/shrink_block_v2.o
OBJS += $(OBJDIR)/src/shrink_context.o
OBJS += $(OBJDIR)/src/shrink_inmem.o
OBJS += $(OBJDIR)/src/shrink_streaming.o
OBJS += $(OBJDIR)/src/stream.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/divsufsort.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/divsufsort_utils.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/sssort.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/trsort.o

all: $(APP)

$(APP): $(OBJS)
	$(CC) $^ $(LDFLAGS) -o $(APP)

clean:
	@rm -rf $(APP) $(OBJDIR)

