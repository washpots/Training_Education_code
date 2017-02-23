#!/bin/bash

cd /code/parallel/pspectralclustering
#make clean all
cd testdata
#ABSSTART=$(date +%s.%N)
#START=$(date +%s.%N)
mpiexec -n 4 ../compute_distance --t_nearest_neighbor 6 --input input_data_psc.txt --output distance.txt
#DIFF=$(echo "$(date +%s.%N) - $START" | bc)
#printf "It took %.6f seconds" $DIFF
#START=$(date +%s.%N)
mpiexec -n 2 ../distance_to_similarity --input distance.txt --output similarity.txt
#DIFF=$(echo "$(date +%s.%N) - $START" | bc)
#printf "It took %.6f seconds" $DIFF
#START=$(date +%s.%N)
spacenum=$(($1 * 10))
mpiexec -n 2 ../evd --eigenvalue $1 --eigenspace $spacenum --input similarity.txt --eigenvalues_output eigenvalues.txt --eigenvectors_output eigenvectors.txt
#DIFF=$(echo "$(date +%s.%N) - $START" | bc)
#printf "It took %.6f seconds" $DIFF
#START=$(date +%s.%N)
mpiexec -n 1 ../kmeans --num_clusters $1 --input eigenvectors.txt --output result.txt
#DIFF=$(echo "$(date +%s.%N) - $START" | bc)
#printf "It took %.6f seconds\n" $DIFF
#DIFF=$(echo "$(date +%s.%N) - $ABSSTART" | bc)
#printf "Runtime = %.3f seconds\n" $DIFF > time.txt