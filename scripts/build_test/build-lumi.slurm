#!/bin/bash
#SBATCH --job-name=oifs-hamlite-build
#SBATCH --account=project_462001382
#SBATCH --partition=small
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --time=03:00:00
#SBATCH --output=%x-%j.out
#
#  Batch build of OpenIFS 48r1 + HAMlite on LUMI-C.
#
#  The interactive path (openifs-test.sh -cb, which wraps the build in an
#  srun) dies with the login session. Use this instead for the long builds:
#
#      cd $OIFS_HOME
#      source ./oifs-config.edit_me.sh
#      ./scripts/build_test/openifs-test.sh -c -j 16     # create, needs network
#      sbatch scripts/build_test/build-lumi.slurm        # build, offline
#
#  The create step MUST run on a login node: LUMI compute nodes have no
#  outbound network, so ecbundle cannot clone from github.com there.

set -eu

cd "${SLURM_SUBMIT_DIR}"

source ./oifs-config.edit_me.sh

# Do not let openifs-test.sh wrap the build in another srun, we are
# already inside an allocation.
export IGT_BUILD_LAUNCHER=""

./scripts/build_test/openifs-test.sh -b \
    -j "${SLURM_CPUS_ON_NODE:-64}" \
    --arch="${OIFS_ARCH}" \
    --clean \
    --without-single-precision

echo "Build finished. Executable expected at: ${OIFS_EXEC}"
