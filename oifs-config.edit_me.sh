#
#
#   oifs-config.edit_me.sh
#
#
#   This script sets the environment for OpenIFS 48r1 + HAMlite
#
#   Configured for LUMI-C (EuroHPC/CSC).
#
#   Read this script using the command:
#
#   source ./oifs-config.edit_me.sh
#
#
#--- set machine specific settings -----------------------------

export OIFS_HOST="csc"
export OIFS_PLATFORM="lumi"

#---Base code assumes openifs-48r1 and openifs-expt are installed
#---in $HOME. Either these can be changed by the user------------
export OIFS_HOME="/scratch/project_462000807/${USER}/oifs-hamlite-coupling"

#---Slurm project used for the build and test allocations.
#---Storage lives under project_462000807; CPU billing units are on
#---project_462001382 (same as the IFS-FESOM builds in this tree).
export OIFS_ACCOUNT="${OIFS_ACCOUNT:-project_462001382}"

source ${OIFS_HOME}/hpc-csc-config.edit_me.sh

#--- if OIFS_DEBUGGER is set, oifs-run can use it to activate debugger ---------
export OIFS_DEBUGGER="DDT"

#--- set principal OIFS variables ----------------------------------------------

export OIFS_CYCLE=48r1

#---It is recommended that the openifs-expt and oifs_data dir
#---exist in a location designed for permanent storage-----------
export OIFS_EXPT="/scratch/project_462000807/${USER}/test/48r1_hamlite"
#export OIFS_DATA_DIR="/scratch/project_462000807/${USER}/test/oifs-hamlite-coupling/openifs-data"
#export OIFS_DATA_DIR="/scratch/project_462001382/hhermann/oifs_data/48r1"
export OIFS_DATA_DIR="/scratch/project_462000807/hhermann/hamlite-coupling-data/oifs_data/48r1"

#---Set the path for the arch directory. Depending on system,i.e.,
#---all libs are installed on the sytem, this is not required,
#---so set to an empty string OIFS_ARCH=""
#export OIFS_ARCH="./arch/${OIFS_HOST}/${OIFS_PLATFORM}/gnu/12.2.0"   # LUMI/23.09: its cray-mpich no longer works on the LUMI network
export OIFS_ARCH="./arch/${OIFS_HOST}/${OIFS_PLATFORM}/gnu/13.3.0"

#---Path to the executable for 3d global model. This is the
#---default path for the exe, produced by openifs-test.sh.
#---DP means double precision. To run single precision change
#---DP to SP
export OIFS_EXEC="${OIFS_HOME}/build/bin/ifsMASTER.DP"

#---Default assumed paths, only change if you know what you are doing
export OIFS_TEST="${OIFS_HOME}/scripts/build_test"
export OIFS_RUN_SCRIPT="${OIFS_HOME}/scripts/exp_3d"
export OIFS_LOGFILE="${OIFS_HOME}/oifs_test_log.txt"

alias oenv="env -0 | sort -z | tr '\0' '\n' | grep -a OIFS_"

echo -e "\nOpenIFS environment variables are:"
echo "------------------------------------------------------"
env -0 | sort -z | tr '\0' '\n' | grep -a OIFS_
echo

#---Path to the executable for the SCM. This is the
#---default path for the exe, produced by openifs-test.sh.
#---DP means double precision. To run single precision change
#---DP to SP
export SCM_EXEC="${OIFS_HOME}/build/bin/MASTER_scm.DP"

#---Default assumed paths, only change if you know what you are doing
export SCM_TEST="${OIFS_HOME}/scripts/scm"
export SCM_VERSIONDIR="${OIFS_EXPT}/scm_openifs/48r1"
export SCM_PROJDIR="${SCM_VERSIONDIR}/scm-projects"
export SCM_RUNDIR="${SCM_PROJDIR}/ref48r1"
export SCM_LOGFILE="${SCM_RUNDIR}/scm_run_log.txt"

alias scm_env="env -0 | sort -z | tr '\0' '\n' | grep -a SCM_"

echo -e "\nSCM environment variables are:"
echo "------------------------------------------------------"
env -0 | sort -z | tr '\0' '\n' | grep -a SCM_
echo
