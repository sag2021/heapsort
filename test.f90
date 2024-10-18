
!
! Test code
! 
PROGRAM main

  USE HEAP_SORT_MODULE

  IMPLICIT NONE 

  REAL(HEAP_SORT_REAL)  ,DIMENSION(19) :: rand,sorted
  INTEGER(HEAP_SORT_INT),DIMENSION(19) :: perm

  INTEGER(HEAP_SORT_INT) :: i 

  ! Generate random vector 
  CALL RANDOM_NUMBER(rand)
  sorted = rand

  ! Sort data
  CALL heap_sort(sorted,INT(19,KIND=HEAP_SORT_INT),perm)

  DO i=1,SIZE(rand,1)
    PRINT *,i,perm(i),rand(i),sorted(i)
  ENDDO

END PROGRAM
