MODULE errors
  IMPLICIT NONE

  CONTAINS
    ! Determines if the file exists
    SUBROUTINE error_badFile (file)
      IMPLICIT NONE

      LOGICAL                   :: exists
      CHARACTER*(*), INTENT(IN) :: file

      INQUIRE(FILE = file, EXIST = exists)

      IF(.NOT.exists) THEN
         WRITE(*,'(A)') 'error: The input file does not exist'
         STOP
      END IF
    END SUBROUTINE error_badFile

    ! Determines if the format of the input is incorrect 
    SUBROUTINE error_badFormat (iostatus, badFormat)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: iostatus

      INTERFACE
         LOGICAL FUNCTION badFormat (int)
           INTEGER, INTENT(IN) :: int
         END FUNCTION badFormat
      END INTERFACE

      IF(badFormat(iostatus)) THEN
         WRITE(*,'(A)')'error: Correct the format of the input'
         STOP
      END IF
    END SUBROUTINE error_badFormat
    
    ! Determines if the number is a natural number
    SUBROUTINE error_badNatural (natural)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: natural

      IF(natural.LT.1) THEN
         WRITE(*,*) 'error: The inputted natural number must be positive'
         STOP
      END IF
    END SUBROUTINE error_badNatural

    ! Determines if the path exists
    SUBROUTINE error_badPath (path)
      IMPLICIT NONE

      CHARACTER*(*), INTENT(IN) :: path
      LOGICAL                   :: exists

      INQUIRE(FILE = path, EXIST = exists)

      IF(.NOT.exists) THEN
         WRITE(*,'(A)') 'error: The output path does not exist'
         STOP
      END IF
    END SUBROUTINE error_badPath

    ! Determines if the real number is negative
    SUBROUTINE error_negativeReal (val)
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: val

      IF(val.LT.0) THEN
         WRITE(*,'(A)') 'error: The real number must be non-negative'
         STOP
      END IF
    END SUBROUTINE error_negativeReal

    ! Determines if the real number is non-positive
    SUBROUTINE error_nonPositiveReal (val)
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: val

      IF(val.LE.0) THEN
         WRITE(*,'(A)') 'error: The real number must be positive'
         STOP
      END IF
    END SUBROUTINE error_nonPositiveReal

    !Determines if the value is neither true nor false
    SUBROUTINE error_notTrueFalse (val)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: val

      IF((val.NE.1).AND.(val.NE.0)) THEN
         WRITE(*,'(A)') 'error: The number must be one or zero'
         STOP
      END IF
    END SUBROUTINE error_notTrueFalse
END MODULE errors