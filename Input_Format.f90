MODULE input_format
   USE param_def
   USE param_func
   USE errors

   IMPLICIT NONE

   CONTAINS
      ! Stores the input data
      SUBROUTINE input (model, out_file)
         IMPLICIT NONE

         INTEGER                                       :: I
         CHARACTER*50                                  :: in_file
         INTEGER                                       :: iostatus
         INTEGER                                       :: J
         DOUBLE PRECISION                              :: max_rand_weight
         TYPE(param), INTENT(INOUT)                    :: model
         INTEGER                                       :: need_weights
         INTEGER                                       :: num_data
         INTEGER                                       :: num_input
         INTEGER                                       :: num_nodes
         CHARACTER*(*), INTENT(INOUT)                  :: out_file
         INTEGER                                       :: pm
         DOUBLE PRECISION                              :: rate
         INTEGER                                       :: seed
         DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: training_input
         INTEGER, DIMENSION(:), ALLOCATABLE            :: training_output
         DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: weights
   
         ! Get the input file name
         WRITE(*,*)'Enter input file name: '
         READ(*,*) in_file
         CALL error_badFile(in_file)

         OPEN (UNIT = 1, FILE = in_file)

         ! Store the number of data points
         READ(1,*,iostat = iostatus) num_data
         CALL error_badFormat(iostatus, checkFormatEOF)
         CALL error_badNatural(num_data)
         CALL Set_num_data(model, num_data)

         ! Store the number of inputs
         READ(1,*,iostat = iostatus) num_input
         CALL error_badFormat(iostatus, checkFormatEOF)
         CALL error_badNatural(num_data)
         CALL Set_num_input(model, num_input)

         ALLOCATE(training_input(num_data, num_input))
         ALLOCATE(training_output(num_data))

         ! Get the training data
         DO I = 1, num_data
            DO J = 1, num_input
               READ(1,*,iostat = iostatus) training_input(I, J)
               CALL error_badFormat(iostatus, checkFormatEOF)
            END DO
            READ(1,*,iostat = iostatus) training_output(I)
            CALL error_badFormat(iostatus, checkFormatEOF)
         END DO 
      
         CLOSE(1)

         ! Store the training data
         CALL Set_training_input(model, training_input)
         CALL Set_training_output(model, training_output)

         WRITE(*,*)'Do you need new weights? '
 
         READ(*,*) need_weights
         CALL error_notTrueFalse(need_weights)

         ! Creates new random weights if needed, otherwise
         !   the old weights are stored
         IF (need_weights == 1) THEN
            out_file = ''   

            ! Store the number of nodes
            WRITE(*,*)'Number of nodes: '
            READ(*,*) num_nodes
            CALL error_badNatural(num_nodes)
            CALL Set_num_nodes(model, num_nodes)

            ALLOCATE(weights(num_nodes,num_input+1))

            ! Set the pseudo-random number generator
            WRITE(*,*)'Seed: '
            READ(*,*) seed
            CALL SRAND(seed)

            ! Get the maximum allowed random weight
            WRITE(*,*)'Max random weight: '            
            READ(*,*) max_rand_weight
            CALL error_negativeReal(max_rand_weight)

            ! Randomly generate the initial weights
            DO I = 1, num_nodes
               DO J = 1, num_input + 1
                  IF (RAND() < 0.5) THEN
                     pm = -1
                  ELSE
                     pm = 1
                  END IF
                  weights(I, J) = RAND() * max_rand_weight * pm
               END DO
            END DO

            ! Store the weights
            CALL Set_weights(model, weights)
         ELSE
            ! Store the old weights
            CALL input_weights(model, out_file)
         END IF

         ! Store the learning rate
         WRITE(*,*)'Learning rate: '
         READ(*,*) rate
         CALL error_nonPositiveReal(rate)
         CALL Set_rate(model, rate)
      END SUBROUTINE input

      ! Stores the old weights
      SUBROUTINE input_weights (model, out_file)

         IMPLICIT NONE

         INTEGER                       :: I
         INTEGER                       :: iostatus
         INTEGER                       :: J
         TYPE(param), INTENT(INOUT)    :: model
         INTEGER                       :: num_input
         INTEGER                       :: num_nodes
         CHARACTER*(*), INTENT(INOUT)  :: out_file
         DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)

         ! Get the file name of the old weights
         WRITE(*,*)'File name: '
         READ(*,*) out_file
         CALL error_badPath(out_file(1:INDEX(out_file,'/')))
         CALL error_badFile(out_file)

         OPEN(UNIT = 1, FILE = out_file)

         ! Store the number of nodes
         READ(1, *, iostat=iostatus) num_nodes
         CALL error_badFormat(iostatus, checkFormatEOF)
         CALL error_badNatural(num_nodes)
         CALL Set_num_nodes(model, num_nodes)

         ! Get the number of inputs
         READ(1, *, iostat=iostatus) num_input
         CALL error_badFormat(iostatus, checkFormatEOF)
         CALL error_badNatural(num_input)

         ALLOCATE(weights(num_nodes, num_input + 1))

         ! Get the old weights
         DO I = 1, num_nodes
            DO J = 1, num_input
               READ(1, *, iostat=iostatus) weights(I,J)
               CALL error_badFormat(iostatus, checkFormatEOF)
            END DO
         END DO

         CLOSE(1)

         ! Store the old weights
         CALL Set_weights(model, weights)
      END SUBROUTINE input_weights

      ! Checks if the EOF has been reached
      LOGICAL FUNCTION checkFormatEOF (iostatus)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: iostatus
         checkFormatEOF = iostatus.NE.0
      END FUNCTION checkFormatEOF
END MODULE input_format
