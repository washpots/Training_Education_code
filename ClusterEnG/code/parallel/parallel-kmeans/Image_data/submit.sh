#!/bin/bash

#awk -F, 'BEGIN{OFS = " "}{print NR, $1, $2, $3}' /code/parallel/parallel-kmeans/Image_data/input_data.csv > /code/parallel/parallel-kmeans/Image_data/input_data.txt
cd /code/parallel/parallel-kmeans
#make clean all
cd Image_data
../seq_main -i input_data.txt -n $1
#mpirun -n 2 ../mpi_main -i input_data.txt -n $1
awk -F' ' '{print $2}' input_data.txt.membership > result.txt
