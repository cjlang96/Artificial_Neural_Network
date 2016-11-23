! Creates training data for an AND gate
PROGRAM AND_GATE_EXAMPLES

   IMPLICIT NONE
   
   INTEGER          :: I
   CHARACTER*50     :: file
   INTEGER          :: num_data
   INTEGER          :: seed
   DOUBLE PRECISION :: r
   
   ! Get file name
   WRITE(*,*)'File name: '
   READ(*,*) file

   ! Set seed for pseudo-random number generator
   WRITE(*,*)'Seed: '
   READ(*,*) seed
   CALL SRAND(seed)

   ! Get the number of data points
   WRITE(*,*)'Number of data points: '
   READ(*,*) num_data

   OPEN(3, FILE = file)

   WRITE(3, *)num_data

   WRITE(3, *)'2'

   ! Randomly generate training data
   DO I = 1, num_data
      r = RAND()

      IF (r < 0.25) THEN 
         WRITE(3, *)'1'
         WRITE(3, *)'1' 
         WRITE(3, *)'1'
      ELSE IF (r < 0.5) THEN 
         WRITE(3, *)'1'
         WRITE(3, *)'0' 
         WRITE(3, *)'0'
      ELSE IF (r < 0.75) THEN 
         WRITE(3, *)'0'
         WRITE(3, *)'1' 
         WRITE(3, *)'0'
      ELSE 
         WRITE(3, *)'0'
         WRITE(3, *)'0' 
         WRITE(3, *)'0'
      END IF
   END DO
END PROGRAM AND_GATE_EXAMPLES