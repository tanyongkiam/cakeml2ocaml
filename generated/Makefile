# Makefile for building clean IL types and conversion modules
#
# Prerequisites: cake64.cmi and cake64.cmx must already exist
#   (built separately with unlimited stack: ulimit -s unlimited)
#
# Targets:
#   make il_types      - Build clean IL type modules
#   make conv          - Build conversion modules (depends on cake64 + il_types)
#   make all           - Build everything
#   make clean         - Remove il_types build artifacts (not cake64)

OCAMLOPT_C = ocamlfind ocamlopt -package zarith -c

# Runtime
RUNTIME_DIR = ../runtime

# Flags
OFLAGS = -O2 -g -I $(RUNTIME_DIR)

# IL type source files (order matters for dependencies)
IL_TYPE_SRCS = il_types/common.ml il_types/flat_lang.ml il_types/clos_lang.ml \
               il_types/bvl_lang.ml il_types/bvi_lang.ml il_types/data_lang.ml \
               il_types/word_lang.ml il_types/stack_lang.ml il_types/lab_lang.ml

IL_TYPE_CMXS = $(IL_TYPE_SRCS:.ml=.cmx)

# Conversion source files (order matters: common_conv first, then clos before flat/bvl/bvi/data)
CONV_SRCS = il_types/common_conv.ml il_types/clos_lang_conv.ml \
            il_types/flat_lang_conv.ml il_types/bvl_lang_conv.ml \
            il_types/bvi_lang_conv.ml il_types/data_lang_conv.ml \
            il_types/word_lang_conv.ml il_types/stack_lang_conv.ml \
            il_types/lab_lang_conv.ml

CONV_CMXS = $(CONV_SRCS:.ml=.cmx)

.PHONY: all il_types conv clean

all: il_types conv

# --- IL type modules ---

il_types: $(IL_TYPE_CMXS)

il_types/common.cmx: il_types/common.ml
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/flat_lang.cmx: il_types/flat_lang.ml il_types/common.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/clos_lang.cmx: il_types/clos_lang.ml il_types/common.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/bvl_lang.cmx: il_types/bvl_lang.ml il_types/clos_lang.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/bvi_lang.cmx: il_types/bvi_lang.ml il_types/clos_lang.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/data_lang.cmx: il_types/data_lang.ml il_types/common.cmx il_types/clos_lang.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/word_lang.cmx: il_types/word_lang.ml il_types/common.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/stack_lang.cmx: il_types/stack_lang.ml il_types/common.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

il_types/lab_lang.cmx: il_types/lab_lang.ml il_types/common.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types $<

# --- Conversion modules (require cake64.cmi) ---

conv: $(CONV_CMXS)

il_types/common_conv.cmx: il_types/common_conv.ml cake64.cmi $(IL_TYPE_CMXS)
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/clos_lang_conv.cmx: il_types/clos_lang_conv.ml il_types/common_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/word_lang_conv.cmx: il_types/word_lang_conv.ml il_types/common_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/flat_lang_conv.cmx: il_types/flat_lang_conv.ml il_types/common_conv.cmx il_types/clos_lang_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/bvl_lang_conv.cmx: il_types/bvl_lang_conv.ml il_types/clos_lang_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/bvi_lang_conv.cmx: il_types/bvi_lang_conv.ml il_types/clos_lang_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/data_lang_conv.cmx: il_types/data_lang_conv.ml il_types/common_conv.cmx il_types/clos_lang_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/stack_lang_conv.cmx: il_types/stack_lang_conv.ml il_types/common_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

il_types/lab_lang_conv.cmx: il_types/lab_lang_conv.ml il_types/common_conv.cmx
	$(OCAMLOPT_C) $(OFLAGS) -I il_types -I . $<

# --- Clean ---

clean:
	rm -f il_types/*.cmi il_types/*.cmx il_types/*.o
