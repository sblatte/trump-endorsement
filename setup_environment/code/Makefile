SHELL=bash
include ../../shell_functions.make


all: $(foreach lang, R,../output/$(lang)_packages.txt)

include ../../generic.make

../output/R_packages.txt: packages.R | $(OOPR)
	$(R) $<
