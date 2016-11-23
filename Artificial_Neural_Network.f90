!  ************************************************************
!  *                Artificial Neural Network                 *
!  *                                                          *
!  * Developed by Christopher Lang in November, 2016. A       *
!  * feedforward, back-propagating artificial neural network  *
!  * which can either teach sets of random weights and select *
!  * the best set or teach previously generated weights.      *
!  ************************************************************

PROGRAM ANN
   USE input_format
   USE output_format
   USE param_def
   USE param_func
   USE find_weights

   IMPLICIT NONE

   TYPE(param)  :: model
   CHARACTER*50 :: out_file
   
   ! Store input data
   CALL input(model, out_file)

   ! Use input data to teach the ANN
   CALL learning(model)

   ! Find the best set of weights
   CALL best_weights(model)

   ! Output the results
   CALL output(model, out_file)
END PROGRAM ANN
   
