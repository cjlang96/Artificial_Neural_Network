MODULE output_format
   USE param_def
   USE param_func
   USE errors
   
   IMPLICIT NONE

   CONTAINS
      ! Outputs the results
      SUBROUTINE output (model, out_file)
         IMPLICIT NONE

         CHARACTER(LEN=:), ALLOCATABLE :: file
         INTEGER                       :: I
         INTEGER                       :: J
         INTEGER                       :: min_index
         TYPE(param), INTENT(IN)       :: model                 
         INTEGER                       :: num_input
         INTEGER                       :: num_nodes
         CHARACTER*(*), INTENT(INOUT)  :: out_file
         INTEGER                       :: pos
         DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)

         ! Get the output file name
         IF (out_file == '') THEN
            WRITE(*,*)'Output file (no extension): '
            READ(*,*) out_file
            CALL error_badPath(out_file(1:INDEX(out_file,'/')))

            pos = INDEX(out_file, ' ')
         ELSE 
            pos = INDEX(out_file,'.', BACK= .true.)
         END IF

         ALLOCATE(CHARACTER(pos - 1) :: file)
         file = out_file(1:pos - 1)

         ! Get the data stored in the ANN
         num_nodes = Get_num_nodes(model)
         num_input = Get_num_input(model)
         min_index = Get_min_index(model)

         ALLOCATE(weights(num_nodes, num_input))  

         weights = Get_weights(model)

         ! Write a file which can be used to update the 
         !   weights with new data 
         OPEN(3, FILE = file // '.out')

         WRITE(3, *)num_nodes

         WRITE(3, *)num_input
 
         DO I = 1, num_nodes
            DO J = 1, num_input + 1
               WRITE(3, *)weights(I,J)
            END DO	 
         END DO

         ! Write a file which has the weights for the node
         !   with the smallest total error
         OPEN(3, FILE = file // '.best')

         WRITE(3, *)'1'

         WRITE(3, *)num_input
 
         DO J = 1, num_input + 1
            WRITE(3, *)weights(min_index,J)	 
         END DO

         CLOSE(3)
      END SUBROUTINE output
END MODULE output_format
