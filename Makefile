UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	BIN_DIR = bin_linux
endif
ifeq ($(UNAME_S),Darwin)
	BIN_DIR = bin_macos
endif

files := $(wildcard *.lisp)
names := $(files:.lisp=)

.PHONY: all clean $(names)

all: $(names)

$(names): %: $(BIN_DIR)/% man/man1/%.1

$(BIN_DIR)/%: %.lisp build_binary.sh Makefile
	mkdir -p $(BIN_DIR)
	./build_binary.sh $<
	mv $(@F) $(BIN_DIR)/

man/man1/%.1: %.lisp build_manual.sh Makefile
	mkdir -p man/man1
	./build_manual.sh $<
	mv $(@F) man/man1/

clean:
	rm -rf $(BIN_DIR) man
