#!/bin/bash

cd /code/parallel/pspectralclustering/testdata
ABSSTART=$(date +%s.%N)
START=$(date +%s.%N)
mpiexec -n 1 ../compute_distance --t_nearest_neighbor 6 --input input_data_psc.txt --output distance.txt
DIFF=$(echo "$(date +%s.%N) - $START" | bc)
printf "It took %.6f seconds" $DIFF
START=$(date +%s.%N)
mpiexec -n 1 ../distance_to_similarity --input distance.txt --output similarity.txt
DIFF=$(echo "$(date +%s.%N) - $START" | bc)
printf "It took %.6f seconds" $DIFF
START=$(date +%s.%N)
mpiexec -n 1 ../evd --eigenvalue 3 --eigenspace 100 --input similarity.txt --eigenvalues_output eigenvalues.txt --eigenvectors_output eigenvectors.txt
DIFF=$(echo "$(date +%s.%N) - $START" | bc)
printf "It took %.6f seconds" $DIFF
START=$(date +%s.%N)
mpiexec -n 1 ../kmeans --num_clusters 3 --input eigenvectors.txt --output result_serial.txt
DIFF=$(echo "$(date +%s.%N) - $START" | bc)
printf "It took %.6f seconds\n" $DIFF
DIFF=$(echo "$(date +%s.%N) - $ABSSTART" | bc)
printf "Runtime = %.3f seconds\n" $DIFF > time.txt

awk -F',' 'BEGIN{OFS = ","}{print $1, $2}' input_data.txt > temp1.txt
awk -F' ' '{print $1}' result_serial.txt > temp2.txt
paste -d , temp1.txt temp2.txt > temp3.txt
echo '"xdata","ydata","category"' > temp4.txt 
cat temp4.txt temp3.txt > output_sspectral_clustering.csv
mv output_sspectral_clustering.csv /education/output
