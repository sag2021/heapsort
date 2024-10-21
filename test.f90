!
! Test code. 
! 
! Tests that array is actually sorted and tests that perm vector
! actually maps indices correctly. 
!
! 
PROGRAM main

  USE HEAP_SORT_MODULE

  IMPLICIT NONE 

  INTEGER(HEAP_SORT_INT),PARAMETER    :: N = 11112
  REAL(HEAP_SORT_REAL)  ,DIMENSION(N) :: rand,sorted
  INTEGER(HEAP_SORT_INT),DIMENSION(N) :: perm

  INTEGER(HEAP_SORT_INT) :: i 

  ! Generate random vector 
  CALL RANDOM_NUMBER(rand)
  sorted = rand

  ! Check not already sorted
  IF(.NOT. is_sorted(rand)) THEN
    PRINT *,"Initial array is not sorted"
  ENDIF
   
  ! Sort data in ascending order
  CALL heap_sort(sorted,N,perm)

  ! Check sorted array is sorted
  IF(is_sorted(sorted)) THEN
    PRINT *,"Sorted array is sorted. Test PASSED"
  ELSE
    PRINT *,"Sorted array is NOT sorted. Test FAILED"
    STOP "FAIL"
  ENDIF

  ! Check permutation vector maps values correctly
  DO i=1,SIZE(perm)
    IF(rand(perm(i)) .ne. sorted(i)) THEN
      PRINT *,"Permutation produces wrong mapping. Test FAILED"
      STOP "FAIL"
    ENDIF
  ENDDO  
  PRINT *,"Permutation map correct. Test PASSED"

  STOP "PASS"

  CONTAINS

LOGICAL FUNCTION is_sorted(array)

  IMPLICIT NONE

  ! INPUT
  REAL(HEAP_SORT_REAL),DIMENSION(:),INTENT(IN) :: array
  
  ! LOCAL
  INTEGER :: i

  DO i=1,SIZE(array)-1
    IF(array(i+1) < array(i)) THEN
      is_sorted = .FALSE.
      RETURN
    ENDIF 
  ENDDO
  is_sorted = .TRUE.

END FUNCTION

END PROGRAM
