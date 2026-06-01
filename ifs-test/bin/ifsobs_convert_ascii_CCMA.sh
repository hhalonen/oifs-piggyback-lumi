#!/bin/bash

set -ex

NPOOLS=$(grep NPROC params | cut -d '=' -f 2)
DBNAME=CCMA
INPUT_FILE=$1

# Create an empty odb1 database
rm -rf ${DBNAME}.obs
mkdir ${DBNAME}.obs
cd ${DBNAME}.obs
sh ../create_ioassign -n ${NPOOLS} -l ${DBNAME}
cd ..


# Set up environment required by odb1
export ODB_CMA=CCMA
export IOASSIGN=${DBNAME}.obs/IOASSIGN
export ODB_SRCPATH_${DBNAME}=${DBNAME}.obs
export ODB_DATAPATH_${DBNAME}=${DBNAME}.obs
export ODB_IO_METHOD=4
export ODB_IO_GRPSIZE=$NPOOLS
export WDIR="."
export OMP_NUM_THREADS=$(grep NTHREAD params | cut -d '=' -f 2)

# Link to the files for the various pools 
if [[ ${NPOOLS} -gt 1 ]] ; then 
  ln -sf ${INPUT_FILE} ${INPUT_FILE}_1  
  for pool in $(seq 2 $NPOOLS) ; do 
    ln -sf  dummy_obs_formated.txt ${INPUT_FILE}_${pool}
  done  
fi  

source ./set_launcher.bash

LAUNCH=""
if [[ -z "$LAUNCH" ]]; then
  if [[ -z "$LAUNCHER_NTHREAD_FLAG" ]]; then
    LAUNCH="$LAUNCHER $LAUNCHER_NPROC_FLAG 1 "
  else
    LAUNCH="$LAUNCHER $LAUNCHER_NPROC_FLAG 1 $LAUNCHER_NTHREAD_FLAG $OMP_NUM_THREADS "
  fi
fi

# Convert ascii to odb1
if [[ ${NPOOLS} -eq 1 ]] ; then 
  $LAUNCH ./convert_dbase -i ${INPUT_FILE} -o ${DBNAME} -f1 ascii -f2 odb1
else
  $LAUNCH ./convert_dbase -i ${INPUT_FILE}_{poolno} -o ${DBNAME} -f1 ascii -f2 odb1 -np ${NPOOLS}
fi  

# Cleaning 
for pool in {1..${NPOOLS}} ; do 
  rm -f ${INPUT_FILE}_${pool}
done  
