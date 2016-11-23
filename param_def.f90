MODULE param_def
  IMPLICIT NONE

  ! Stores all of the data necessary to teach the ANN
  TYPE param
      INTEGER                       :: min_index
      INTEGER                       :: num_data
      INTEGER                       :: num_input
      INTEGER                       :: num_nodes
      DOUBLE PRECISION              :: rate
      DOUBLE PRECISION, ALLOCATABLE :: training_input(:,:)
      INTEGER, ALLOCATABLE          :: training_output(:)
      DOUBLE PRECISION, ALLOCATABLE :: weights(:,:)
  END TYPE param
END MODULE param_def