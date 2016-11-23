MODULE find_weights
   USE param_def
   USE param_func

   IMPLICIT NONE

   CONTAINS
      ! Teaches the ANN using the input data
      SUBROUTINE learning(model)
         IMPLICIT NONE

         INTEGER                       :: I
         INTEGER                       :: J
         INTEGER                       :: K
         TYPE(param), INTENT(INOUT)    :: model
         INTEGER                       :: num_data
         INTEGER                       :: num_input
         INTEGER                       :: num_nodes
         DOUBLE PRECISION              :: rate
         DOUBLE PRECISION, ALLOCATABLE :: training_input(:,:)
         INTEGER, ALLOCATABLE          :: training_output(:)
         DOUBLE PRECISION              :: weighted_derivative
         DOUBLE PRECISION              :: weighted_output
         DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)
         DOUBLE PRECISION              :: X

         ! Get the data stored in the ANN
         num_data = Get_num_data(model)
         num_nodes = Get_num_nodes(model)
         num_input = Get_num_input(model)
         rate = Get_rate(model)

         ALLOCATE(weights(num_nodes, num_input))
         ALLOCATE(training_input(num_data, num_input))
         ALLOCATE(training_output(num_data))

         weights = Get_weights(model)
         training_input = Get_training_input(model)
         training_output = Get_training_output(model)

         ! Teach the ANN by modifying the weights
         DO I = 1, num_data
            DO J = 1, num_nodes
               ! Calculate the value to use in the activation function
               !   and its derivative
               X = 0
         
               DO K = 1, num_input
                  X = X + training_input(I, K) * weights(J, K)
               END DO
  
               X = X + 1 * weights(J, num_input + 1)

               ! Calculate the value of the activation function and its 
               !   derivative given the current weights
               weighted_output = f(X)
               weighted_derivative = fprime(X)

               ! Update the input weights
               DO K = 1, num_input
                  weights(J, K) = weights(J, K) - 2  * rate * (weighted_output &
                                - training_output(I)) * weighted_derivative    &
                                * training_input(I, K) 
               END DO

               ! Update the bias node weight (note that the value of the 
               !   bias node is always one)
               weights(J, num_input + 1) = weights(J, num_input + 1) - 2 * rate   &
                                         * (weighted_output - training_output(I)) &
                                         * weighted_derivative
            END DO
         END DO

         ! Store the updated weights
         CALL Set_weights(model, weights)
      END SUBROUTINE learning

      ! Determines which node has the best weights 
      !   (i.e. the smallest total error)
      SUBROUTINE best_weights (model)
         IMPLICIT NONE

         DOUBLE PRECISION              :: error
         INTEGER                       :: I
         INTEGER                       :: J
         INTEGER                       :: K
         DOUBLE PRECISION              :: min_error
         INTEGER                       :: min_index
         TYPE(param), INTENT(INOUT)    :: model
         INTEGER                       :: num_data
         INTEGER                       :: num_input
         INTEGER                       :: num_nodes
         INTEGER, ALLOCATABLE          :: training_output(:)
         DOUBLE PRECISION, ALLOCATABLE :: training_input(:,:)
         DOUBLE PRECISION              :: weighted_output
         DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)
         DOUBLE PRECISION              :: X

         ! Get the data stored in the ANN
         num_data = Get_num_data(model)
         num_input = Get_num_input(model)
         num_nodes = Get_num_nodes(model)

         ALLOCATE(training_output(num_data))
         ALLOCATE(training_input(num_data,num_input))
         ALLOCATE(weights(num_nodes,num_input))

         training_output = Get_training_output(model)
         training_input = Get_training_input(model)
         weights = Get_weights(model)

         ! Calculate the first total error
         min_error = 0
         min_index = 1

         DO I = 1, num_data
            ! Calculate the value to use in the activation function
            X = 0
         
            DO K = 1, num_input
               X = X + training_input(I, K) * weights(1, K)
            END DO

            X = X + 1 * weights(J, num_input + 1)

            ! Calculate the value of the activation function given 
            !   the current weights
            weighted_output = f(X)

            ! Add to the total error of the node
            min_error = min_error + (weighted_output - training_output(I)) ** 2
         END DO

         ! Calculate the total error for every other node and find the minimum
         IF (num_nodes > 1) THEN
            DO J = 2, num_nodes
               error = 0

               DO I = 1, num_data
                  X = 0
         
                  ! Calculate the value to use in the activation function
                  DO K = 1, num_input
                     X = X + training_input(I, K) * weights(1, K)
                  END DO

                  X = X + 1 * weights(J, num_input + 1)

                  ! Calculate the value of the activation function given 
                  !   the current weights
                  weighted_output = f(X)

                  ! Add to the total error of the node
                  error = error + (weighted_output - training_output(I)) ** 2
               END DO

               ! Determine if the error is the smallest thus far and
               !   store the node index if so
               IF (error < min_error) THEN
                  min_index = J
               END IF
            END DO
         END IF

         ! Store the index of the node with the smallest error
         CALL Set_min_index(model, min_index)
      END SUBROUTINE best_weights

      ! The activation function
      DOUBLE PRECISION FUNCTION f (x)
         IMPLICIT NONE
  
         DOUBLE PRECISION, INTENT(IN) :: x
         
         f = 0.5 * (DTANH(x) + 1)
      END FUNCTION f

      ! The derivative of the activation function
      DOUBLE PRECISION FUNCTION fprime (x)
         IMPLICIT NONE

         DOUBLE PRECISION, INTENT(IN) :: x
 
         fprime = 0.5 / (DCOSH(x)) ** 2
      END FUNCTION fprime 
END MODULE find_weights
  
