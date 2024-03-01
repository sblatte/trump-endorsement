FUNCTIONS = $(shell cat ../../shell_functions.sh)
STATA = @$(FUNCTIONS); stata_with_flag
R = @$(FUNCTIONS); R_pc_and_slurm
PYTHON = @$(FUNCTIONS); python_pc_and_slurm
PIP = @$(FUNCTIONS); pip_install_pc_and_slurm
JULIA = @$(FUNCTIONS); julia_pc_and_slurm
WIPE = @$(FUNCTIONS); wipe_directory

#If 'make -n' option is invoked
ifneq (,$(findstring n,$(MAKEFLAGS)))
STATA = STATA produce $@ using
R = R produce $@ using
PYTHON = PYTHON produce $@ using
PIP = PIP produce $@ using
JULIA = JULIA produce $@ using
WIPE := wipe_directory
endif
