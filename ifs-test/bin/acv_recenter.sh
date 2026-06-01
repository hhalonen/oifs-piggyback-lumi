#!/bin/bash

set -x 

if [ $# != 2 ]; then
    echo "Recentering script must be called with 2 arguments : resolution of test case, and target source branch name "
    exit -1
fi

if ls enspert* 1> /dev/null 2>&1; then
    echo "Carrying out recentering of perturbations in directory $PWD"
else
    echo "This script must be called from the test directory in which recentering is to be carried out."
    exit -1
fi

resol=$1
branch_name=$2

data_path=/tmp/tmpdir/$USER/git/ifs-builds/$branch_name/source/ifs-test/tests/$resol/4dvardata
scaling=5
# resolution name in tests is lower case, file names here seem to have resolution upper case
resol_uc=${resol^^}


rm -f enspert_${resol_uc}_00?

for paramid in 130 133 138 152 155 ; do
#for paramid in 130 ; do
  #
  # Sum
  #
  avg_pert_filename=avg_pert_${resol_uc}_${paramid}
  [[ -f sum_pert_${resol_uc}_${paramid} ]] && rm sum_pert_${resol_uc}_${paramid}
  [[ -f ${avg_pert_filename} ]] && rm ${avg_pert_filename}
  #
  grib_copy -w param=${paramid}  ${data_path}/enspert_${resol_uc}_001 pert_${resol_uc}_${paramid}_1
  cp pert_${resol_uc}_${paramid}_1 sum_pert_${resol_uc}_${paramid}
  for imem in {2..5} ; do    
    grib_copy -w param=${paramid}   ${data_path}/enspert_${resol_uc}_00${imem} pert_${resol_uc}_${paramid}_${imem}
    mars  <<EOF1
        READ, 
          SOURCE    = "sum_pert_${resol_uc}_${paramid}",
          FIELDSET  = ctr
        READ, 
          SOURCE    = "pert_${resol_uc}_${paramid}_${imem}",
          FIELDSET  = mem
        COMPUTE, 
          FORMULA   = "mem+ctr", 
          TARGET    = "sum_pert_${resol_uc}_${paramid}_new"
EOF1
    mv sum_pert_${resol_uc}_${paramid}_new sum_pert_${resol_uc}_${paramid}
  done 
  #
  # Compute the mean
  #
  mars  <<EOF2
      READ, 
        SOURCE    = "sum_pert_${resol_uc}_${paramid}",
        FIELDSET  = ctr
      COMPUTE, 
        FORMULA   = "ctr/${scaling}", 
        TARGET    = "${avg_pert_filename}"
EOF2
  rm -f sum_pert_${resol_uc}_${paramid}
  #
  # Remove the mean
  #
  for imem in {1..5} ; do    
    mars  <<EOF3
        READ, 
          SOURCE    = "pert_${resol_uc}_${paramid}_${imem}",
          FIELDSET  = mem
        READ, 
          SOURCE    = "${avg_pert_filename}",
          FIELDSET  = mean
        COMPUTE, 
          FORMULA   = "mem-mean", 
          TARGET    = "ens_pert_${resol_uc}_${paramid}_${imem}_centered"
EOF3
    rm -f pert_${resol_uc}_${paramid}_${imem}
  done
  rm -f ${avg_pert_filename}
done

#
# Convert to sp
#

for imem in {1..5} 
do
  for paramid in 130 133 138 152 155 
  do
    gptosp.exe -g ens_pert_${resol_uc}_${paramid}_${imem}_centered -S enspertsp.${paramid}_00${imem}
  done
done

#
# Concatenate
#
for imem in {1..5} ; do    
  rm -f enspert_${resol_uc}_00${imem}
  touch enspert_${resol_uc}_00${imem}
  for paramid in 130 133 138 152 155 ; do
    cat ens_pert_${resol_uc}_${paramid}_${imem}_centered >> enspert_${resol_uc}_00${imem}
    rm -f ens_pert_${resol_uc}_${paramid}_${imem}_centered
  done
done

exit 0 

#
# Convert to sp
#

for imem in {1..5} 
do
  for paramid in 130 133 138 152 155 
  do
    gptosp.exe -g enspert.${paramid}_00${imem} -S enspertsp.${paramid}_00${imem}
  done
done
