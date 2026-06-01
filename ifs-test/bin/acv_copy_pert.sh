#!/bin/bash
#
# Convert to sp
#
if ls enspert.* 1> /dev/null 2>&1; then
    echo "Copy perturbations in directory $PWD"
else
    echo "This script must be called from the test directory in which recentering is to be carried out."
    exit -1
fi

for imem in {1..5} 
do
  #
  # Grid point
  #
  for paramid in 133
  do
    for icopy in {1..2} ; do
      typeset -Z2 jmem=$(( $imem + ${icopy}*5 ))
      ln -s enspert.${paramid}_00${imem} enspert.${paramid}_0${jmem}
    done
  done
  #
  # Spectral
  #
  for paramid in 130 138 152 155 
  do
    for icopy in {1..2} ; do
      typeset -Z2 jmem=$(( $imem + ${icopy}*5 ))
      ln -s enspertsp.${paramid}_00${imem} enspertsp.${paramid}_0${jmem}
    done
  done
done
