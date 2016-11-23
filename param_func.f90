MODULE param_func
   USE param_def
   USE errors

   IMPLICIT NONE

   CONTAINS
      ! Gets the index of the node with the smallest total error
      INTEGER FUNCTION Get_min_index (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         Get_min_index = model%min_index
      END FUNCTION Get_min_index

      ! Gets the number of data points
      PURE INTEGER FUNCTION Get_num_data (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         Get_num_data = model%num_data
      END FUNCTION Get_num_data

      ! Gets the number of inputs
      PURE INTEGER FUNCTION Get_num_input (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         Get_num_input = model%num_input
      END FUNCTION Get_num_input

      ! Gets the number of nodes
      PURE INTEGER FUNCTION Get_num_nodes (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         Get_num_nodes = model%num_nodes
      END FUNCTION Get_num_nodes
 
      ! Gets the learning rate
      DOUBLE PRECISION FUNCTION Get_rate (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         Get_rate = model%rate
      END FUNCTION Get_rate

      ! Gets the training inputs
      FUNCTION Get_training_input (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         DOUBLE PRECISION        :: Get_training_input(Get_num_data(model), &
                                                       Get_num_input(model))
         Get_training_input = model%training_input(:,:)
      END FUNCTION Get_training_input

      ! Gets the training outputs
      FUNCTION Get_training_output (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         INTEGER                 :: Get_training_output(Get_num_data(model))
         Get_training_output = model%training_output(:)
      END FUNCTION Get_training_output

      ! Gets the weights
      FUNCTION Get_weights (model)
         IMPLICIT NONE
         TYPE(param), INTENT(IN) :: model
         DOUBLE PRECISION        :: Get_weights(Get_num_nodes(model), &
                                               Get_num_input(model) + 1)
         Get_weights = model%weights(:,:)
      END FUNCTION Get_weights

      ! Stores the index of the node with the smallest total error
      SUBROUTINE Set_min_index (model, min_index)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT) :: model
         INTEGER, INTENT(IN)        :: min_index
         CALL error_badNatural(min_index)
         model%min_index = min_index
      END SUBROUTINE Set_min_index

      ! Stores the number of data points
      SUBROUTINE Set_num_data (model, num_data)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT) :: model
         INTEGER, INTENT(IN)        :: num_data
         CALL error_badNatural(num_data)
         model%num_data = num_data
      END SUBROUTINE Set_num_data

      ! Stores the number of inputs
      SUBROUTINE Set_num_input (model, num_input)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT) :: model
         INTEGER, INTENT(IN)        :: num_input
         CALL error_badNatural(num_input)
         model%num_input = num_input
      END SUBROUTINE Set_num_input

      ! Stores the number of nodes
      SUBROUTINE Set_num_nodes (model, num_nodes)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT) :: model
         INTEGER, INTENT(IN)        :: num_nodes
         CALL error_badNatural(num_nodes)
         model%num_nodes = num_nodes
      END SUBROUTINE Set_num_nodes

      ! Stores the learning rates
      SUBROUTINE Set_rate (model, rate)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT)   :: model
         DOUBLE PRECISION, INTENT(IN) :: rate
         CALL error_nonPositiveReal(rate)
         model%rate = rate
      END SUBROUTINE Set_rate

      ! Stores the training inputs
      SUBROUTINE Set_training_input (model, training_input)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT)   :: model
         DOUBLE PRECISION, INTENT(IN) :: training_input(:,:)
         model%training_input = training_input(:,:)
      END SUBROUTINE Set_training_input

      ! Stores the training outputs
      SUBROUTINE Set_training_output (model, training_output)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT) :: model
         INTEGER, INTENT(IN)        :: training_output(:)
         model%training_output = training_output(:)
      END SUBROUTINE Set_training_output

      ! Stores the weights
      SUBROUTINE Set_weights (model, weights)
         IMPLICIT NONE
         TYPE(param), INTENT(INOUT)   :: model
         DOUBLE PRECISION, INTENT(IN) :: weights(:,:)
         model%weights = weights(:,:)
      END SUBROUTINE Set_weights
END MODULE param_func
