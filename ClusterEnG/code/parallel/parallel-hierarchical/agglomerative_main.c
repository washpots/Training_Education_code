        
        #include <stdio.h>
        #include <stdlib.h>
        
        #include <mpi.h>
        
        int main(int argc, char **argv) {
            
            long int sum, partial_sum;
            int     procID, numProc, mpiNameLen, ierr;
            int     i, j, N = 12170000, avgNumData, start_row, end_row, num_rows_to_send, num_rows_received, num_rows_to_receive, sender;
            char    mpiName[MPI_MAX_PROCESSOR_NAME];
            double  startTime, endTime, totalTime, maxTime;
            int   *data, *data2;
            MPI_Status status;
            
            MPI_Init(&argc, &argv);
        
            MPI_Comm_rank(MPI_COMM_WORLD, &procID);
            MPI_Comm_size(MPI_COMM_WORLD, &numProc);
            MPI_Get_processor_name(mpiName, &mpiNameLen);
            int root_process = 0;
            
            startTime = MPI_Wtime();
            if (procID == 0) {
                data = (int*) malloc(N * sizeof(int));
                for (i=0; i<N; i++) {
                    data[i] = i;
                }
                avgNumData = N / numProc;

                for(i = 1; i < numProc; i++) {
                    start_row = i*avgNumData + 1;
                    end_row   = (i + 1)*avgNumData;
        
                    if((N - end_row) < avgNumData)
                       end_row = N - 1;
                       
                    num_rows_to_send = end_row - start_row + 1;
        
                    ierr = MPI_Send( &num_rows_to_send, 1 , MPI_INT,
                          i, 2001, MPI_COMM_WORLD);
                    
                    ierr = MPI_Send( &data[start_row], num_rows_to_send, MPI_INT,
                          i, 2001, MPI_COMM_WORLD);
                 }
                 sum = 0;
                 for(i = 0; i < avgNumData + 1; i++) {
                    sum += data[i];   
                 } 
        
                //printf("sum %i calculated by root process\n", sum);
                for(i = 1; i < numProc; i++) {
                    
                    ierr = MPI_Recv( &partial_sum, 1, MPI_LONG, MPI_ANY_SOURCE,
                          2002, MPI_COMM_WORLD, &status);
          
                    sender = status.MPI_SOURCE;
        
                    //printf("Partial sum %i returned from process %i\n", partial_sum, sender);
             
                    sum += partial_sum;
                 }
                 printf("The grand total is: %ld\n", sum);
                 free(data);
            }
            
            else {
             ierr = MPI_Recv( &num_rows_to_receive, 1, MPI_INT, 
                   root_process, 2001, MPI_COMM_WORLD, &status);
             data2 = (int*) malloc(N * sizeof(int)); 
             ierr = MPI_Recv( data2, num_rows_to_receive, MPI_INT, 
                   root_process, 2001, MPI_COMM_WORLD, &status);
    
             num_rows_received = num_rows_to_receive;
             
             partial_sum = 0;
             for(i = 0; i < num_rows_received; i++) {
                partial_sum += data2[i];
             }
             ierr = MPI_Send( &partial_sum, 1, MPI_LONG, root_process, 
                   2002, MPI_COMM_WORLD);
                free(data2);
          }
                
            //distMatrix    = (float**) malloc(N * sizeof(float*));
            //distMatrix[0] = (float*)  malloc(N * sizeof(float));
        
            //for (i=1; i<numProc; i++)
            //    distMatrix[i] = distMatrix[i-1] + N;
            
            
            endTime = MPI_Wtime();
            totalTime = endTime - startTime;
            
            MPI_Reduce(&totalTime, &maxTime, 1, MPI_DOUBLE,
                   MPI_MAX, 0, MPI_COMM_WORLD);
                   
            if (procID == 0) {
                printf("Correct answer: %.1f\n",  0.5 * N * (N - 1));
                printf("Total time (s) = %f\n", maxTime);
            }
            
            MPI_Finalize();
            return(0);
        }
