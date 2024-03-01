SHELL=bash
include ../../shell_functions.make

all: $(foreach num, 1 2, ../output/table$(num).htm) $(foreach num, $(shell seq 1 5), ../output/tablea$(num).htm) ../output/figure1.png

include ../../generic.make

../output/figure1.png $(foreach num, 1 2, ../output/table$(num).htm) $(foreach num, $(shell seq 1 5), ../output/tablea$(num).htm): Final_Code.R | ../input/poll_2022_weighted_final_topost.csv $(OOPR)
	$(R) $< 

