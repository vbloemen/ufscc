#!/bin/bash

# compiled as follows:
#     ./configure --without-sylvan --disable-dependency-tracking --prefix ${INSTALL}
#     make -j64 -Wall
#     make install

# Assuming file structure:
# [ppopp_benchmarks] # current directory
# - [failures]
# - [models]
# - - [beem]
# - - [data]
# - - [random]
# - - - graph_gen
# - [results]
# - benchmark.sh
# - parse_output.py

# Variables
TIMEOUT_TIME="600s" # 10 minutes timeout
OFFLINE_SCC="${HOME}/code/hong/scc-par/scc"

# "variables" in this folder
BENCHDIR=`pwd`
TMPFILE="${BENCHDIR}/test.out" # Create a temporary file to store the output
FAILFOLDER="${BENCHDIR}/failures"
SELECTEDBEEMFOLDER="${BENCHDIR}/models/selected_beem"
BEEMFOLDER="${BENCHDIR}/models/beem"
OFFLINEFOLDER="${BENCHDIR}/models/offline"
SYNTHETICFOLDER="${BENCHDIR}/models/selected_synthetic"
OFFLINESYNTHETICFOLDER="${BENCHDIR}/models/offline_synthetic"
OUTPUT_DVE="${BENCHDIR}/results/results-dve.csv"
OUTPUT_OFFLINE="${BENCHDIR}/results/results-offline.csv"
OUTPUT_SYNTHETIC="${BENCHDIR}/results/results-synthetic.csv"
OUTPUT_OFFLINESYNTHETIC="${BENCHDIR}/results/results-offline_synthetic.csv"
CORRECT_FILE="${BENCHDIR}/results/results-orig-tarjan.csv"

touch ${TMPFILE}
trap "exit" INT #ensures that we can exit this bash program

# create new output file, or append to the exisiting one
create_results() {
    output_file=${1}
    if [ ! -e ${output_file} ]
    then 
        touch ${output_file}
        # Add column info to output CSV
        echo "model,alg,workers,time,date,sccs,ustates,utrans,tstates,ttrans,initstates,selfloop,claimdead,claimfound,claimsuccess" > ${output_file}
    fi
}

create_results ${OUTPUT_DVE}
create_results ${OUTPUT_OFFLINE}
create_results ${OUTPUT_SYNTHETIC}
create_results ${OUTPUT_OFFLINESYNTHETIC}


# test_dve MODEL ALG WORKERS
# e.g. test_dve at.4.dve ufscc 64
test_dve() {
    model=${1}
    alg=${2}
    workers=${3}
    base=${model%.dve} # without the .dve
    echo "Running ${alg} on ${base} with ${workers} worker(s)"
    timeout ${TIMEOUT_TIME} dve2lts-mc --strategy=${alg} --threads=${workers} -s28 ${BEEMFOLDER}/${model} &> ${TMPFILE}
    python parse_output.py "${base}" "${alg}" "${workers}" "${FAILFOLDER}" "${CORRECT_FILE}" "${TMPFILE}" "${OUTPUT_DVE}"
}

# test_all_dve ALG WORKERS
# e.g. test_all_dve ufscc 64
test_all_dve() {
    alg=${1}
    workers=${2}
    for file in ${BEEMFOLDER}/*.dve
    do
        name=${file##*/} # remove preceding path
        test_dve ${name} ${alg} ${workers}
    done
}

# test_all_dve ALG WORKERS
# e.g. test_all_dve ufscc 64
test_all_selected_dve() {
    alg=${1}
    workers=${2}
    for file in ${SELECTEDBEEMFOLDER}/*.dve
    do
        name=${file##*/} # remove preceding path
        test_dve ${name} ${alg} ${workers}
    done
}


# test_offline MODEL ALG WORKERS
# e.g. test_offline livej.bin ufscc 64
test_offline() {
    model=${1}
    alg=${2}
    if [ "$2" == "hong" ]
    then
        algnumber=2
    elif [ "$2" == "tarjan" ]
    then
        algnumber=3
    elif [ "$2" == "ufscc" ]
    then
        algnumber=5
    fi
    workers=${3}
    base=${model%.bin} # without the .bin
    echo "Running ${alg} (${algnumber}) on ${base} with ${workers} worker(s)"
    timeout ${TIMEOUT_TIME} ${OFFLINE_SCC} ${OFFLINEFOLDER}/${model} ${workers} ${algnumber} &> ${TMPFILE}
    python parse_output.py "${base}" "${alg}" "${workers}" "${FAILFOLDER}" "${CORRECT_FILE}" "${TMPFILE}" "${OUTPUT_OFFLINE}" 
}



# test_offline MODEL ALG WORKERS
# e.g. test_offline_synthetic livej.bin ufscc 64
test_offline_synthetic() {
    model=${1}
    alg=${2}
    if [ "$2" == "hong" ]
    then
        algnumber=2
    elif [ "$2" == "tarjan" ]
    then
        algnumber=3
    elif [ "$2" == "ufscc" ]
    then
        algnumber=5
    fi
    workers=${3}
    base=${model%.bin} # without the .bin
    echo "Running ${alg} (${algnumber}) on ${base} with ${workers} worker(s)"
    timeout ${TIMEOUT_TIME} ${OFFLINE_SCC} ${OFFLINESYNTHETICFOLDER}/${model} ${workers} ${algnumber} &> ${TMPFILE}
    python parse_output.py "${base}" "${alg}" "${workers}" "${FAILFOLDER}" "${CORRECT_FILE}" "${TMPFILE}" "${OUTPUT_OFFLINESYNTHETIC}" 
}


test_all_offline() {
    alg=${1}
    workers=${2}
    for file in ${OFFLINEFOLDER}/*.bin
    do
        name=${file##*/} # remove preceding path
        test_offline ${name} ${alg} ${workers}
    done
}

test_all_offline_synthetic() {
    alg=${1}
    workers=${2}
    for file in ${OFFLINESYNTHETICFOLDER}/*.bin
    do
        name=${file##*/} # remove preceding path
        test_offline_synthetic ${name} ${alg} ${workers}
    done
}


# test_synthetic MODEL ALG WORKERS
test_synthetic() {
    model=${1}
    alg=${2}
    workers=${3}
    base=${model%.prm.spins} # without the .spins
    echo "Running ${alg} on ${base} with ${workers} worker(s)"
    timeout ${TIMEOUT_TIME} prom2lts-mc --strategy=${alg} --threads=${workers} -s28 ${SYNTHETICFOLDER}/${model} &> ${TMPFILE}
    python parse_output.py "${base}" "${alg}" "${workers}" "${FAILFOLDER}" "${CORRECT_FILE}" "${TMPFILE}" "${OUTPUT_SYNTHETIC}"
}


test_all_synthetic() {
    alg=${1}
    workers=${2}
    for file in ${SYNTHETICFOLDER}/*.prm.spins
    do
        name=${file##*/} # remove preceding path
        test_synthetic ${name} ${alg} ${workers}
    done
}


do_dve_tests() {
    test_all_dve tarjan 1
    test_all_dve ufscc 1
    test_all_dve ufscc 2
    test_all_dve ufscc 4
    test_all_dve ufscc 6
    test_all_dve ufscc 8
    test_all_dve ufscc 12
    test_all_dve ufscc 16
    test_all_dve ufscc 24
    test_all_dve ufscc 32
    test_all_dve ufscc 48
    test_all_dve ufscc 64
    test_all_dve renault 1
    test_all_dve renault 2
    test_all_dve renault 4
    test_all_dve renault 6
    test_all_dve renault 8
    test_all_dve renault 12
    test_all_dve renault 16
    test_all_dve renault 24
    test_all_dve renault 32
    test_all_dve renault 48
    test_all_dve renault 64
}


# about 30 minutes per series of tests
do_synthetic_tests() {
    test_all_synthetic tarjan 1
    test_all_synthetic ufscc 1
    test_all_synthetic ufscc 2
    test_all_synthetic ufscc 4
    test_all_synthetic ufscc 6
    test_all_synthetic ufscc 8
    test_all_synthetic ufscc 12
    test_all_synthetic ufscc 16
    test_all_synthetic ufscc 24
    test_all_synthetic ufscc 32
    test_all_synthetic ufscc 48
    test_all_synthetic ufscc 64
    test_all_synthetic renault 1
    test_all_synthetic renault 2
    test_all_synthetic renault 4
    test_all_synthetic renault 6
    test_all_synthetic renault 8
    test_all_synthetic renault 12
    test_all_synthetic renault 16
    test_all_synthetic renault 24
    test_all_synthetic renault 32
    test_all_synthetic renault 48
    test_all_synthetic renault 64
}


do_offline_tests() {
    test_all_offline tarjan 1
    test_all_offline ufscc 1
    test_all_offline ufscc 2
    test_all_offline ufscc 4
    test_all_offline ufscc 6
    test_all_offline ufscc 8
    test_all_offline ufscc 12
    test_all_offline ufscc 16
    test_all_offline ufscc 24
    test_all_offline ufscc 32
    test_all_offline ufscc 48
    test_all_offline ufscc 64
    test_all_offline hong 1
    test_all_offline hong 2
    test_all_offline hong 4
    test_all_offline hong 6
    test_all_offline hong 8
    test_all_offline hong 12
    test_all_offline hong 16
    test_all_offline hong 24
    test_all_offline hong 32
    test_all_offline hong 48
    test_all_offline hong 64
}


# create_offline_model MODEL
create_offline_model() {
    MODEL=${1}
    FILE="${BEEMFOLDER}/${MODEL}.dve"
    cd ${OFFLINESYNTHETICFOLDER}
    dve2lts-mc --strategy=dfs --threads=1 -s28 ${FILE} > ${MODEL}.out
    python parse_ltsmin_models.py ${MODEL}.out > ${MODEL}.edge
    ./convert ${MODEL}.edge ${MODEL}.bin -GMInputFormat=EDGE -GMOutputFormat=BIN
    rm ${MODEL}.edge
    rm ${MODEL}.out
    echo ""
    echo "testing ${MODEL}"
    echo ""
    echo "hong-1"
    ./scc ${MODEL}.bin 1 2 | grep "running_time"
    echo "tarjan-1"
    ./scc ${MODEL}.bin 1 3 | grep "running_time"
    echo "ufscc-1"
    ./scc ${MODEL}.bin 1 5 | grep "running_time"
    echo "hong-64"
    ./scc ${MODEL}.bin 64 2 | grep "running_time"
    echo "ufscc-64"
    ./scc ${MODEL}.bin 64 5 | grep "running_time"
    cd ${BENCHDIR}
}


do_offline_synthetic_tests() {
    test_all_offline_synthetic tarjan 1
    test_all_offline_synthetic ufscc 1
    test_all_offline_synthetic ufscc 2
    test_all_offline_synthetic ufscc 4
    test_all_offline_synthetic ufscc 6
    test_all_offline_synthetic ufscc 8
    test_all_offline_synthetic ufscc 12
    test_all_offline_synthetic ufscc 16
    test_all_offline_synthetic ufscc 24
    test_all_offline_synthetic ufscc 32
    test_all_offline_synthetic ufscc 48
    test_all_offline_synthetic ufscc 64
    test_all_offline_synthetic hong 1
    test_all_offline_synthetic hong 2
    test_all_offline_synthetic hong 4
    test_all_offline_synthetic hong 6
    test_all_offline_synthetic hong 8
    test_all_offline_synthetic hong 12
    test_all_offline_synthetic hong 16
    test_all_offline_synthetic hong 24
    test_all_offline_synthetic hong 32
    test_all_offline_synthetic hong 48
    test_all_offline_synthetic hong 64
}

do_renault_tests() {
    test_all_synthetic renault 2
    test_all_synthetic renault 4
    test_all_synthetic renault 6
    test_all_synthetic renault 8
    test_all_synthetic renault 12
    test_all_synthetic renault 16
    test_all_synthetic renault 24
    test_all_synthetic renault 32
    test_all_synthetic renault 48
    
    test_all_dve renault 2
    test_all_dve renault 4
    test_all_dve renault 6
    test_all_dve renault 8
    test_all_dve renault 12
    test_all_dve renault 16
    test_all_dve renault 24
    test_all_dve renault 32
    test_all_dve renault 48
}


extra_ufscc() {
    test_all_selected_dve ufscc 1
    test_all_selected_dve ufscc 2
    test_all_selected_dve ufscc 4
    test_all_selected_dve ufscc 6
    test_all_selected_dve ufscc 8
    test_all_selected_dve ufscc 12
    test_all_selected_dve ufscc 16
    test_all_selected_dve ufscc 24
    test_all_selected_dve ufscc 32
    test_all_selected_dve ufscc 48
    test_all_selected_dve ufscc 64
}


extra_renault() {
    test_all_selected_dve renault 1
    test_all_selected_dve renault 1
    test_all_selected_dve renault 2
    test_all_selected_dve renault 4
    test_all_selected_dve renault 6
    test_all_selected_dve renault 8
    test_all_selected_dve renault 12
    test_all_selected_dve renault 16
    test_all_selected_dve renault 24
    test_all_selected_dve renault 32
    test_all_selected_dve renault 48
    test_all_selected_dve renault 64
}

test_all_synthetic tarjan 1


for i in `seq 1 40`;
do
    test_all_dve tarjan 1
    extra_renault
done    

for i in `seq 1 18`;
do
    extra_ufscc
done    


for i in `seq 1 8`;
do
    test_all_dve ufscc 64
done    

for i in `seq 1 40`;
do
    test_all_dve tarjan 1
    test_all_dve renault 64
done    


# Wiki links:  http://haselgrove.id.au/wikipedia.htm
# Patents:     https://snap.stanford.edu/data/cit-Patents.html
# Pokec:       https://snap.stanford.edu/data/soc-pokec.html
# LiveJournal: https://snap.stanford.edu/data/soc-LiveJournal1.html

# SSCA2:       GTgraph default
# random:      GTgraph default
# R-MAT:       GTgraph default (n=10^7, m=10^8)







