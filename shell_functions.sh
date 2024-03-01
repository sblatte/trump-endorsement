`##This file defines shell functions that
# 1. Improve Makefiles' readability by compartmentalizing all the "if" statement around SLURM vs local executables.
# 2. Cause Stata to report an error to Make when the Stata log file end at an error.`;

stata_with_flag() {
	stata_pc_and_slurm $@;
	if [ "$1" == "--no-job-name" ]; then
		shift;
	fi ;
	LOGFILE_NAME=$(basename ${1%.*}.log);
	if grep -q '^r([0-9]*);$' ${LOGFILE_NAME}; then
		echo "STATA ERROR: There are errors in the running of ${1} file";
		echo "Exiting Status: $(grep '^r([0-9]*);$' ${LOGFILE_NAME} | head -1)";
		exit 1;
	fi
} ;

stata_pc_and_slurm() {
	if command -v sbatch > /dev/null ; then
		command1="module load stata/17.0";
		if [ "$1" == "--no-job-name" ]; then
			shift;
			command2="stata-se -e $@";
			print_info Stata $@;
        	sbatch -W --export=command1="$command1",command2="$command2" run.sbatch;
		else
			command2="stata-se -e $@";
			jobname1="${1%.*}_";
        	jobname2=$(echo ${@:2} | sed -e "s/ /_/g");
			print_info Stata $@;
			sbatch -W --export=command1="$command1",command2="$command2" --job-name="$jobname1$jobname2" run.sbatch;
		fi;
	else
        if [ "$1" == "--no-job-name" ]; then
            shift;
        fi;
        print_info Stata $@;

	if [[ "$USER" == "jgottlieb" || "$USER" == "noahsobel-lewin" || "$USER" == "scottblatte" || "$USER" == "xiaoyangzhang" ]] ; then
		stata-mp -e $@;
	else
	        stata-se -e $@;
	fi;
	fi;
} ;

R_pc_and_slurm() {
	if command -v sbatch > /dev/null ; then
		command1="module load R/4.0/4.0.2";
		if [ "$1" == "--no-job-name" ]; then
			shift;
			command2="Rscript $@";
			print_info R $@;
        	sbatch -W --export=command1="$command1",command2="$command2" run.sbatch;
		else
			command2="Rscript $@";
			jobname1="${1%.*}_";
        	jobname2=$(echo ${@:2} | sed -e "s/ /_/g");
			print_info R $@;
			sbatch -W --export=command1="$command1",command2="$command2" --job-name="$jobname1$jobname2" run.sbatch;
		fi;
	else
        if [ "$1" == "--no-job-name" ]; then
            shift;
        fi;
        print_info R $@;
        Rscript $@;
	fi
} ;

python_pc_and_slurm() {
	if command -v sbatch > /dev/null ; then
		command1="module load python/booth/3.10";
		if [ "$1" == "--no-job-name" ]; then
			shift;
			command2="python3 $@";
			print_info Python $@;
        	sbatch -W --export=command1="$command1",command2="$command2" run.sbatch;
		else
			command2="python3 $@";
			jobname1="${1%.*}_";
			jobname2=$(echo ${@:2} | sed -e "s/ /_/g");
			print_info Python $@;
			sbatch -W --export=command1="$command1",command2="$command2" --job-name="$jobname1$jobname2" run.sbatch;
		fi;
	else
        if [ "$1" == "--no-job-name" ]; then
            shift;
        fi;
        print_info Python $@;
        python3 $@;
	fi
} ;

pip_install_pc_and_slurm() {
	if command -v sbatch > /dev/null ; then
		command1="module load python/booth/3.10";
		if [ "$1" == "--no-job-name" ]; then
			shift;
			command2="pip3 install -r $@";
			print_info pip3 install $@;
        	sbatch -W --export=command1="$command1",command2="$command2" run.sbatch;
		else
			command2="pip3 install -r $@";
			jobname1="${1%.*}_";
        	jobname2=$(echo ${@:2} | sed -e "s/ /_/g");
			print_info pip3 install $@;
			sbatch -W --export=command1="$command1",command2="$command2" --job-name="$jobname1$jobname2" run.sbatch;
		fi;
	else
        if [ "$1" == "--no-job-name" ]; then
            shift;
        fi;
        print_info pip3 install $@;
        pip3 install -r $@;
	fi
} ;

julia_pc_and_slurm() {
	if command -v sbatch > /dev/null ; then
		command1="module load julia/1.6.6";
		if [ "$1" == "--no-job-name" ]; then
			shift;
			command2="julia $@";
			print_info Julia $@;
			sbatch -W --export=command1="$command1",command2="$command2" run.sbatch;
		else
			command2="julia $@";
			jobname1="${1%.*}_";
			jobname2=$(echo ${@:2} | sed -e "s/ /_/g");
			print_info Julia $@;
			sbatch -W --export=command1="$command1",command2="$command2" --job-name="$jobname1$jobname2" run.sbatch;
		fi;
	else
        if [ "$1" == "--no-job-name" ]; then
            shift;
        fi;
        print_info Julia $@;
        julia $@;
	fi
} ;

clean_task() {
	find ${1} -type l -delete;
	PARENT_DIR=${1%/code};
	rm -f ${1}/*.log;
	rm -rf ${PARENT_DIR}/input ${PARENT_DIR}/output ${1}/slurmlogs;
} ;

print_info() {
	software=$1;
	shift;
	if [ $# == 1 ]; then
		echo "Running ${1} via ${software}, waiting...";
    else
        echo "Running ${1} via ${software} with args = ${@:2}, waiting...";
	fi
} ;

wipe_directory() {
	for dir in input output temp report; do
	if [ -d ../$dir ]; then 
		rm -r ../$dir; 
		echo "Directory $dir has been deleted."; 
	fi;
	done;
	echo "Directory was succesfully wiped.";
}
