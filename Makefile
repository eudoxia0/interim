SML := sml
SMLFLAGS := -Cprint.depth=10
MLTON := mlton

BIN := interim

CM_FILE := interim.cm
MLB_FILE := interim.mlb

VENDOR_DIR := vendor
PARSIMONY := $(VENDOR_DIR)/parsimony
PARSIMONY_URL := https://github.com/eudoxia0/parsimony.git

SRC := src/*.sig src/*.sml

.PHONY: examples

all: compile

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(PARSIMONY): $(VENDOR_DIR)
	git clone $(PARSIMONY_URL) $(PARSIMONY)

compile: $(SRC) $(PARSIMONY)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(BIN): $(SRC) $(PARSIMONY)
	$(MLTON) $(MLB_FILE)

examples: $(BIN)
	./$(BIN) examples/hello.int   examples/hello.c
	./$(BIN) examples/fib.int     examples/fib.c
	./$(BIN) examples/sqlite3.int examples/sqlite3.c
	./$(BIN) examples/region.int  examples/region.c
	./$(BIN) examples/record.int  examples/record.c
	$(CC) examples/hello.c -o examples/hello
	$(CC) examples/fib.c -o examples/fib
	$(CC) examples/sqlite3.c -o examples/sqldemo -lsqlite3
	$(CC) examples/region.c -o examples/region
	$(CC) examples/record.c -o examples/record

clean:
	rm -rf $(VENDOR_DIR)
	rm $(BIN)
