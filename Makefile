CSC ?= csc -O3 -lfa2
RM ?= rm -f

SRCS = kylling.scm
BIN = kylling

.PHONY: all clean

all: kylling

$(BIN): $(SRCS)
	$(CSC) -o $@ $(SRCS)

clean:
	$(RM) $(BIN)
