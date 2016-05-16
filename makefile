SWIPL_VERSION_STRING=$(shell swipl --version)
SWIPL_FLAGS= 
PROBE= version 7

ifneq (,$(findstring $(PROBE),$(SWIPL_VERSION_STRING)))
  SWIPL_FLAGS += --traditional
endif

output.pl: src/common.pl src/compiler.pl src/io.pl src/main.pl src/module_handler.pl src/printer.pl src/syntax.pl src/translator.pl src/typechecker.pl
	swipl $(SWIPL_FLAGS) -q -s src/compiler.pl -g "public_7_processFileForSwipl('src/main.pl', 'output.pl')." -t halt.
