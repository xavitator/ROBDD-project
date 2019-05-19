
OCAMLMAKEFILE = OCamlMakefile

RAPPORT_EXP = rapport/exemple
RESULT = robdd

TAR = palmer_durand.tar.gz
TAR_DIR = $(basename $(basename $(TAR)))

ML_FILES = \
		src/dot.ml \
		src/implementation.ml \
		src/worse.ml\
		src/xor.ml\
		src/combinat.ml\
		src/queen.ml\
		src/test.ml 

LY_DIR = src/lex_parser

LEX_FILE = $(LY_DIR)/lexer.mll

YACC_FILE = $(LY_DIR)/parser.mly

BENCH_FILE = $(LY_DIR)/lexer.ml \
		$(LY_DIR)/parser.ml \
		src/bench.ml
BENCH_MLI_FILE = $(LY_DIR)/parser.mli

QUEEN_FILE = 

SUDOKU_FILE = src/sudoku.ml

ALL_FILES = $(ML_FILES) \
		$(BENCH_FILE) \
		$(QUEEN_FILE) \
		$(SUDOKU_FILE)

PACKS = ocamlgraph

SOURCES = $(MLI_FILES) $(ML_FILES)

DOT_FILE := $(shell find $(RAPPORT_EXP) -name '*.dot')

PDF_FILE := $(DOT_FILE:$(RAPPORT_EXP)%.dot=$(RAPPORT_EXP)%.pdf)

.PHONY: all
all: nc

.PHONY: help
help:
	@printf "Useful commands for this Makefile:\\n"
	@printf " - help : prints this message\\n"
	@printf " - robdd : compile all\\n"
	@printf " - queen : compile queen\\n"
	@printf " - sudoku : compile sudoku\\n"
	@printf " - bench : compile bench\\n"
	@printf " - exec : compiles and runs\\n"
	@printf " - bytecode (bc) : compiles in bytecode\\n"
	@printf " - nativecode (nc) : compiles in nativecode\\n"
	@printf " - cleanup : removes compiled files, keeps executables\\n"
	@printf " - clean : removes all compiled files\\n"
	@printf " - clean-all : removes all generated files (doc includes)\\n"
	@printf " - clean-doc : removes all docs files\\n"
	@printf " - fclean : same as clean-all but delete the tar's file too\\n"
	@printf " - re : cleans all and recompiles\\n"

$(RESULT): nc

queen :
	@make $(RESULT) SOURCES="$(SOURCES) $(QUEEN_FILE)"

sudoku :
	@make $(RESULT) SOURCES="$(SOURCES) $(SUDOKU_FILE)"

bench :
	@ocamllex $(LEX_FILE)
	@ocamlyacc $(YACC_FILE)
	@make $(RESULT) SOURCES="$(BENCH_MLI_FILE) $(SOURCES) $(BENCH_FILE)"

.PHONY: exec
exec: $(RESULT)
	./$(RESULT)

.PHONY: fclean
fclean: clean-all
	rm -f $(ALL_FILES:%.ml=%.o) $(ALL_FILES:%.ml=%.cmi) $(ALL_FILES:%.ml=%.cmx)
	rm -f $(TAR)

.PHONY: re
re: fclean $(RESULT)

.PHONY: tar
tar: $(TAR)

$(RAPPORT_EXP)%.pdf: $(RAPPORT_EXP)%.dot
	@dot $< -Tpdf -o $@

dotToPdf: $(PDF_FILE)
	@printf "Dot to pdf dans %s\n" $(RAPPORT_EXP)

$(TAR): fclean
	mkdir -p $(TAR_DIR)
	cp -r src/ Makefile $(OCAMLMAKEFILE) README.md $(TAR_DIR)
	tar -czf $@ $(TAR_DIR)
	rm -rf $(TAR_DIR)

include $(OCAMLMAKEFILE)
