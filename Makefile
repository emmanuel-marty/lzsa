CC=clang
CFLAGS=-O3 -fomit-frame-pointer -Isrc/libdivsufsort/include -Isrc -DHAVE_CONFIG_H
OBJDIR=obj
LDFLAGS=
STRIP=strip

$(OBJDIR)/%.o: src/../%.c
	@mkdir -p '$(@D)'
	$(CC) $(CFLAGS) -c $< -o $@

APP := lzsa

OBJS := $(OBJDIR)/src/lzsa.o
OBJS += $(OBJDIR)/src/lib.o
OBJS += $(OBJDIR)/src/inmem.o
OBJS += $(OBJDIR)/src/stream.o
OBJS += $(OBJDIR)/src/frame.o
OBJS += $(OBJDIR)/src/matchfinder.o
OBJS += $(OBJDIR)/src/shrink_v1.o
OBJS += $(OBJDIR)/src/shrink_v2.o
OBJS += $(OBJDIR)/src/expand_v1.o
OBJS += $(OBJDIR)/src/expand_v2.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/divsufsort.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/sssort.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/trsort.o
OBJS += $(OBJDIR)/src/libdivsufsort/lib/utils.o

all: $(APP)

$(APP): $(OBJS)
	@mkdir -p ../../bin/posix
	$(CC) $^ $(LDFLAGS) -o $(APP)
	$(STRIP) $(APP)

clean:
	@rm -rf $(APP) $(OBJDIR)

