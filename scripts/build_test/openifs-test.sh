#!/usr/bin/env bash
#
#  This script is a wrapper script for the configure, build and test 
#  of stand-alone openifs. 

set -eu
set -o pipefail

## Functions
log_echo ()
{
  builtin echo "$@" | tee -a "$OIFS_LOGFILE"
}

usage_info()
{
  log_echo "----------------------------------------------------------------"
  log_echo "usage_info in openifs-test called because" "$1"
  log_echo "----------------------------------------------------------------"
  log_echo "Openifs-test.sh is a wrapper for building and running tests, based" 
  log_echo "on ifs-test in a stand-alone openifs."
  log_echo ""
  log_echo "openifs-test.sh usage :"
  log_echo "openifs-test.sh -b -c -t"
  log_echo "where :"
  log_echo "-h is help which returns basic usage options and exits"
  log_echo "-c to create/configure the sources that are required to build"
  log_echo "-b to build the source"
  log_echo "-j number of threads used in build." 
  log_echo "-l list the t21 tests available (note 4dvar, ecv etc are not available in openifs)"
  log_echo "-t to run ctest, if the code is built" 
  log_echo "----------------------------------------------------------------"
  exit 1 
}

if [ "$#" -eq 0 ]; then
  usage_info "no arguments supplied"
fi

# set variables with defaults
PRINT_HELP=0
DO_CREATE=0
DO_BUILD=0
DO_TEST=0
#PLATFORM="hpc2020"
LABEL_REGEX='.*'
LABEL_EXCLUDE_REGEX=''
LIST_LABELS=0
NUM_THREADS=0
TESTS_REGEX=''
TESTS_EXCLUDE_REGEX=''

while getopts hbcj:lL:R:tx: OPTION ; do
    case "${OPTION}" in
        h) PRINT_HELP=1 ;;
        b) DO_BUILD=1 ;;
        c) DO_CREATE=1 ;;
        l) LIST_LABELS=1 ;;
        L) LABEL_REGEX="${OPTARG}" ;;
        R) TESTS_REGEX="${OPTARG}" ;;
        t) DO_TEST=1 ;;
        j) NUM_THREADS="${OPTARG}" ;;
        x) TESTS_EXCLUDE_REGEX="${OPTARG}" ;;
        # Unknown options are to be passed to ifs-bundle build.
        # First unknown option triggers end of processing options to
        # git-ifstest
        ?) break
            ;;
    esac
done
shift "$((OPTIND-1))"

# Check $OIFS_HOME exists and the a user cd to $OIFS_HOME
if [ -n "${OIFS_HOME-}" ] && [ -n "${OIFS_LOGFILE-}" ];  then 
  log_echo "[INFO]: OIFS_HOME path exists and is $OIFS_HOME"
  log_echo "[INFO]: log file for openifs-test is $OIFS_LOGFILE"
  if [ -d "$OIFS_HOME" ]; then
    log_echo "[INFO]: cd $OIFS_HOME"
    cd "$OIFS_HOME" || { log_echo "[ERROR]: $OIFS_HOME does not exist - EXIT" ; exit 1; }
    if [ -e "${OIFS_LOGFILE-}" ]; then 
      rm "${OIFS_LOGFILE}" # remove logfile so not adding to old logs
    fi
  fi
else
  echo "[ERROR]: OIFS_HOME variable is unset or empty." 
  echo "         Please edit and source oifs-config.edit_me.sh"
  echo "         to set OIFS_HOME to the OpenIFS directory." 
  echo "         EXITING"

  exit 1
fi

if [[ ${PRINT_HELP} == 1 ]] ; then
  usage_info "help requested"
fi

# If No action options (create, build or test) are passed, then assume do all stages
# NB confusing as logic is reversed
if [[ ${DO_CREATE} == 0 && ${DO_BUILD} == 0 && ${DO_TEST} == 0 ]] ; then
    DO_CREATE=1
    DO_BUILD=1
    DO_TEST=1
    log_echo "[INFO]: -c, -b or -t not provided as arguments, "
    log_echo "        create sources (-c), build (-b) and test (-t) will all execute"
fi

# Use the OIFS_HOST and OIFS_PLATFORM defined in oifs-config.edit_me.sh to initialise
# PLATFORM AND SITE
PLATFORM=$OIFS_PLATFORM
SITE="$OIFS_HOST"
#
# Then check whether --arch is a command line argument  
# If provided, identify the platform using the arch/<site>/<platform>/<compiler>/<compiler_version>
# Following command assumes this dir structure and search for dir in path that comes 2 after 
# arch. Check the arguments for --arch, this takes precedence and reset OIFS_ARCH and 
# resets PLATFORM AND SITE
#
for arg in "$@" ; do
  option=$(echo "${arg}" | awk -F= '{print $1}')
  if [[ $option == "--arch" ]] ; then
    log_echo "[WARNING]: Using command line architecture ${arg} rather OIFS_ARCH in config file" 
    OIFS_ARCH=$(echo "${arg}" | awk -F= '{print $2}')
    #
    target_directory=arch
    #
    if [[ -n "$OIFS_ARCH" && "$OIFS_ARCH" != "local" ]] ; then
      PLATFORM=$(echo "$OIFS_ARCH" | grep -oP "(?<=/$target_directory/).*" | awk -F'/' '{print $2}')
      SITE=$(echo "$OIFS_ARCH" | grep -oP "(?<=/$target_directory/).*" | awk -F'/' '{print $1}')
    else 
      PLATFORM="local"
      SITE="local"
    fi
  fi
done

log_echo "[INFO]: Site/host of platform = $SITE and the platform = $PLATFORM"

# set number of threads for the system
# default number of threads taken from the IFS. 
# Note: Default threads = 8 for all systems other than ATOS. 
#       This works on Mac M1 but is potentially too high for older systems
if [[ "$PLATFORM" == "hpc2020" ]] ; then
    readonly DEFAULT_NUM_THREADS=64
else
    readonly DEFAULT_NUM_THREADS=8
fi

if [[ "$NUM_THREADS" == 0 ]] ; then
  NUM_THREADS=$DEFAULT_NUM_THREADS
  log_echo "[INFO]: -j not provided as argument, hence using defaults."
  log_echo "        Number of threads = $NUM_THREADS, which is default for $PLATFORM."
else
  log_echo "[INFO]: Number of threads defined as argument using -j"
  log_echo "        Number of threads = $NUM_THREADS for $PLATFORM"
fi

readonly DO_CREATE
readonly DO_BUILD
readonly DO_TEST
readonly PLATFORM
readonly NUM_THREADS
readonly TESTS_REGEX
readonly TESTS_EXCLUDE_REGEX

if [[ "${DO_CREATE}" -eq 0 ]] && [[ "${DO_BUILD}" -eq 0 ]]; then
    # Will not detect "-j <DEFAULT_NUM_THREADS>" used incorrectly
    [[ "${NUM_THREADS}" -ne ${DEFAULT_NUM_THREADS} ]] && \
                                              unused "-j" "create or build"
fi
if [[ "${DO_TEST}" -eq 0 ]]; then
    [[ "${LIST_LABELS}" -eq 1 ]]                   && unused "-l" "test"
    # Will not detect "-L .*" used incorrectly
    [[ "${LABEL_REGEX}" != '.*' ]]                 && unused "-L" "test"
    [[ -n "${TESTS_REGEX}" ]]                      && unused "-R" "test"
    [[ -n "${TESTS_EXCLUDE_REGEX}" ]]              && unused "-x" "test"
fi

if [[ "$PLATFORM" == "hpc2020" ]] ; then
  IGT_BUILD_LAUNCHER="${IGT_BUILD_LAUNCHER-srun -c ${NUM_THREADS} --mem=60GB --time=60}"
  IGT_TEST_LAUNCHER="${IGT_TEST_LAUNCHER-salloc -N 2 --qos=np}"
else
  IGT_BUILD_LAUNCHER="${IGT_BUILD_LAUNCHER:-}"
  IGT_TEST_LAUNCHER="${IGT_TEST_LAUNCHER:-}"
fi
readonly IGT_BUILD_LAUNCHER
readonly IGT_TEST_LAUNCHER

#
# Default flags (can be negated by choosing the opposite option):
IFS_BUILD_EXTRA_FLAGS[0]="--with-single-precision"
IFS_BUILD_EXTRA_FLAGS[1]="--init-snan"
# Default flags for openifs-only and with-scmec so always built (cannot be negated):
IFS_BUILD_EXTRA_FLAGS[2]="--openifs-only"
IFS_BUILD_EXTRA_FLAGS[3]="--with-scmec"
IFS_BUILD_EXTRA_FLAGS[4]="--arch=$OIFS_ARCH"

# If $1 begins with "-" it must contain some flags (or a separator)
if [[ ${1:-} =~ ^- ]] ; then
    FLAGS=1 # Start in option-processing mode
else
    FLAGS=0 # No options; start in argument-processing mode
fi

for arg in "$@" ; do
    if [[ ${FLAGS} -eq 1 ]] ; then
        if [[ ${arg} == "--" ]] ; then
            # There is a separator; stop processing flags, switch to
            # arguments
            FLAGS=0
        else
            IFS_BUILD_EXTRA_FLAGS+=("${arg}")
        fi
    fi
done

if [[ ${DO_CREATE} == 1 ]] ; then
  # Check if this release of ifs-bundle includes the self-titled
  # script
  if [[ -x "./openifs-bundle" ]] ; then
      log_echo "[INFO]: Create command is ./openifs-bundle create --threads=${NUM_THREADS} --shallow --update"
      ./openifs-bundle create \
          --threads="${NUM_THREADS}" \
          --shallow \
          --update
      log_echo "[INFO]: See $OIFS_LOGFILE for details of bundle download and create."
      log_echo "[INFO]: Create step of build complete, moving onto build" 
  fi
fi

if [[ ${DO_BUILD} == 1 ]] ; then
    log_echo "[INFO]: Build launcher - ${IGT_BUILD_LAUNCHER}"
    # Check if this release of ifs-bundle includes the self-titled script
    if [[ -x "./openifs-bundle" ]]; then
        log_echo "[INFO]: Build command - ${IGT_BUILD_LAUNCHER} ./openifs-bundle build \
--threads=${NUM_THREADS} ${IFS_BUILD_EXTRA_FLAGS[@]:-}"
        ${IGT_BUILD_LAUNCHER} ./openifs-bundle build \
            --threads="${NUM_THREADS}" \
            "${IFS_BUILD_EXTRA_FLAGS[@]:-}" 
        log_echo "[INFO]: build step complete"
        log_echo "[INFO]: See $OIFS_HOME/build/build.log for build details"
        
    fi    
fi

if [[ ${DO_TEST} == 1 ]] ; then
    if [[ "${LIST_LABELS}" == 1 ]] ; then
        grep LABELS ./source/ifs-test/tests/*/CMakeLists.txt \
          | tr -s " " \
          | cut -d" " -f5- \
          | tr -d ")" \
          | tr " " "\\n" \
          | sort \
          | uniq
    else
        log_echo "[INFO]: Test launcher: ${IGT_TEST_LAUNCHER}"
        cd ./build
        . ./env.sh
        # Don't add ' || true' to this - breaks use within 'git
        # bisect' (see IGT-85)
        ${IGT_TEST_LAUNCHER} ctest --tests-regex "^ifs_.*(${TESTS_REGEX})" \
              --label-regex "${LABEL_REGEX}" \
              --label-exclude "${LABEL_EXCLUDE_REGEX}" \
              --exclude-regex "${TESTS_EXCLUDE_REGEX}" \ | tee -a "$OIFS_LOGFILE"
        if grep -q '100% tests passed, 0 tests failed' "$OIFS_LOGFILE"; then
          log_echo "[INFO]: Good news - ctest has passed" 
          log_echo "        openifs is ready for experiment and SCM testing"
          #ctest_success=true
        else 
          log_echo "[ERROR]:Some or all of ctest has failed - EXIT" 
          log_echo "        Please check $OIFS_HOME/oifs_test_log.txt "
          log_echo "        for failed experiments and further info"
          exit 1
        fi
    fi
fi







