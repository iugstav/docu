CC=clang

CFILES = $(wildcard *.c)
OBJDIR = .build/

OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(CFILES))
DEPS = $(OBJS:.o=.d)

all: $(OBJDIR) main

main: $(OBJS)
	$(CC) $(CFLAGS) -o main $(OBJS)

-include $(DEPS)

$(OBJS):
	$(CC) -c $(patsubst $(OBJDIR)%.o,%.c,$(@)) -o $(@)

$(OBJDIR):
	mkdir -p $@


.PHONY: clean
clean:
	rm -f $(OBJS) $(DEPS) main
