OOPR = ../output ../temp ../report ../input #Order-only pre-reqs
JULIA_OOPR = ../output ../input/Project.toml run.sbatch #Order-only pre-reqs for running julia scripts 

../output ../report ../temp ../input ../temp_report:
	mkdir $@

.PRECIOUS: ../../%

../../%: #Generic recipe to produce outputs from upstream tasks
	$(MAKE) -C $(subst output/,code/,$(dir $@)) ../output/$(notdir $@)

.PHONY: wipe

wipe:
	$(WIPE)