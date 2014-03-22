#!/bin/bash

echo "Begin running jenkins benchmark script for Harlan..."
set -x

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift

if [ "$CHECKOUT" == "" ]; then
  CHECKOUT=`pwd`
fi
if [ "$JENKINS_GHC" == "" ]; then
  export JENKINS_GHC=7.6.3
fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi
# if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh" ]; then
#   source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh
# fi

echo "Running benchmarks remotely on server `hostname`"

which cabal
cabal --version

unset GHC
unset GHC_PKG
unset CABAL

set -e

# Switch to where the benchmarks are
# ----------------------------------------
cd "$CHECKOUT"/
rm -f run_benchmarks.exe
make bench_deps

export TRIALS=1

# Parfunc account, registered app in api console:
CID=905767673358.apps.googleusercontent.com
SEC=2a2H57dBggubW1_rqglC7jtK

# Harlan doc ID:  
# Oops, I made one table as rrnewton:
#   TABID=1h2w9Y41hbiDqCTWj3EhFRZbURVWo71Iw2ZbUw8Om
# Here's the one owned by parfunc:
TABID=1laMlzZ-qTCIqJake4ootb-B-ObRVITirmBkeEH3k
# https://www.google.com/fusiontables/data?docid=1laMlzZ-qTCIqJake4ootb-B-ObRVITirmBkeEH3k


# Enable upload of benchmarking data to a Google Fusion Table:
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload=$TABID --clientid=$CID --clientsecret=$SEC $*
# ./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=Harlan_bench_data --clientid=$CID --clientsecret=$SEC $*
