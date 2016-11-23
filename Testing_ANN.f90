PROGRAM Testing_ANN

   IMPLICIT NONE

   DOUBLE PRECISION              :: ans
   INTEGER                       :: done
   INTEGER                       :: I
   INTEGER                       :: J
   CHARACTER*50                  :: file
   DOUBLE PRECISION, ALLOCATABLE :: input(:)
   INTEGER                       :: num_input
   INTEGER                       :: num_nodes
   DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)
   DOUBLE PRECISION, ALLOCATABLE :: X(:)

   ! Get weights
   WRITE(*,*)'File name: '
   READ(*,*) file

   OPEN(UNIT = 1, FILE = file)

   ! Get the number of nodes
   READ(1, *) num_nodes

   ! Get the number of inputs
   READ(1, *) num_input

   ALLOCATE(weights(num_nodes,num_input))
   ALLOCATE(input(num_input))
   ALLOCATE(X(num_nodes))

   ! Get the weights
   DO I = 1, num_nodes
      DO J = 1, num_input
         READ(1, *) weights(I,J)
      END DO
   END DO

   CLOSE(1)

   DO 
      ! Get the inputs
      DO J = 1, num_input
         WRITE(*,'(I5,A2)')J, ': '
    
         READ(*,*)input(J)
      END DO

      ! Initialize the answer
      ans = 0
 
      DO I = 1, num_nodes
         ! Calculate the value to use in the activation function
         X(I) = 0

         DO J = 1, num_input - 1
            X(I) = X(I) + input(J) * weights(I,J)
         END DO

         X(I) = X(I) + 1 * weights(I, num_input)

         ! Calculate the average value of the activation function 
         !   over each of the nodes
         ans = ans + f(X(I)) / num_nodes
      END DO

      WRITE(*,*) ans
   
      WRITE(*,*)'Exit? '
      READ(*,*) done
 
      IF (done == 1) EXIT
   END DO

   CONTAINS
      ! The activation function
      DOUBLE PRECISION FUNCTION f (x)
         IMPLICIT NONE
  
         DOUBLE PRECISION, INTENT(IN) :: x
         
         f = 0.5 * (DTANH(x) + 1)
      END FUNCTION f
      
END PROGRAM Testing_ANN