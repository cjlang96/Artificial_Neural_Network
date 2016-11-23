! Reformat data for image recognition of 
!   the numbers 1 - 9
PROGRAM MNIST_Format
   IMPLICIT NONE

   INTEGER          :: I
   INTEGER          :: J
   CHARACTER*50     :: in_file
   INTEGER          :: num_cols
   INTEGER          :: num_data
   INTEGER          :: num_input
   INTEGER          :: num_rows
   CHARACTER*50     :: out_file
   INTEGER          :: output
   DOUBLE PRECISION :: value

   ! Get input file name
   WRITE(*,*)'Enter input file name: '
   READ(*,*)in_file

   ! Get output file name
   WRITE(*,*)'Enter output file name: '
   READ(*,*)out_file

   ! Get training output value
   WRITE(*,*)'Enter training output value: '
   READ(*,*)output

   OPEN(UNIT=1,FILE=in_file)

   OPEN(UNIT=3,FILE=out_file)

   ! Store the number of data points
   READ(1,*)num_data
   WRITE(3,*)num_data
 
   ! Get the dimensions of the images
   READ(1,*)num_rows
   READ(1,*)num_cols

   num_input = num_rows * num_cols

   ! Store the number of inputs
   WRITE(3,*)num_input

   ! Normalize and store the values 
   !   of the inputs and outputs
   DO I = 1, num_data
      DO J = 1, num_input
         READ(1,*)value
         WRITE(3,*)value/255
      END DO
   
      WRITE(3,*)output
   END DO

   CLOSE(1)
   CLOSE(3)
END PROGRAM MNIST_Format
