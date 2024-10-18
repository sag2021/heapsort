! BSD-3-Clause 
!
! Copyright 2024 S.A Gilchrist
!
! Redistribution and use in source and binary forms, with or without modification, 
! are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, 
! this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice, 
! this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors 
! may be used to endorse or promote products derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, 
! INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A !PARTICULAR PURPOSE ARE DISCLAIMED. 
! IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
! OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT !LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
! OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
! EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!

! Fortran 2003 functions for sorting arrays using the heapsort
! algorithim. 
!
! DESC: Performs the in-place sorting of a 1D vector of real 
!       numbers. 
!
!       An optional second argument provides a vector containing the 
!       permutation of the elements, i.e. 
!
!       sorted(perm(n)) = unsorted(n),
!
!       where UNSORTED the unsorted data, PERM is the permutation
!       vector, and SORTED is the vector after sorting, and N is 
!       and integer. Note that in practice, since the sorting is
!       in place, the unsorted vector would need to be saved 
!       before sorting.
!
!  NOTES:
!
!    o The input type is REAL(HEAP_SORT_REAL), where HEAP_SORT_REAL
!      is defined in the module.
!
!    o The permutation vector has type INTEGER(HEAP_SORT_INT),
!      where HEAP_SORT_INT is defined in the module.
!
!    o The bulk of the code is F90/F95, however the F2003 
!      ISO_C_BINDING module is used to define the integer type
!      as a C_SIZE_T. The C_SIZE_T type because it is always
!      big enough to address all the memory, although this might
!      not be the case in Fortran since the INTEGER type is always
!      signed.
!

MODULE heap_sort_module

  USE ISO_C_BINDING,ONLY:C_SIZE_T,C_DOUBLE

  ! Define precision of internal variables
  INTEGER,PARAMETER,PRIVATE :: FP = C_DOUBLE
  INTEGER,PARAMETER,PRIVATE :: IT = C_SIZE_T

  ! Define precision for external code to use 
  INTEGER,PARAMETER :: HEAP_SORT_REAL = FP
  INTEGER,PARAMETER :: HEAP_SORT_INT  = IT

  ! Define overloaded swap function 
  INTERFACE swap 
    MODULE PROCEDURE swap_real
    MODULE PROCEDURE swap_integer
  END INTERFACE
 
  ! Declare swap as internal subroutines
  PRIVATE :: swap,swap_real,swap_integer

CONTAINS

SUBROUTINE heap_sort(data_in,nsize,perm)
!
! Sort and array of data in acsending order using the 
! heapsort algorithim
!
  IMPLICIT NONE 

  ! INPUT
  INTEGER(IT),INTENT(IN) :: nsize
   
  ! INPUT/OUTPUT 
  REAL(FP),DIMENSION(nsize),INTENT(INOUT):: data_in  
 
  ! OPTIONAL INPUT/OUTPUT
  INTEGER(IT),DIMENSION(nsize),OPTIONAL,INTENT(INOUT) :: perm

  ! LOCAL VARIABLES
  REAL(FP)    :: temp
  INTEGER(IT) :: i,i0
  INTEGER(IT),PARAMETER :: i1 = 1
 
  ! Get size of data
  i0 = SIZE(data_in,1)
 
  ! Build permutation vector 
  IF(PRESENT(perm)) THEN
    DO i=1,i0
      perm(i) = i
    ENDDO
  ENDIF

  ! Build heap
  CALL build_heap(data_in,nsize,perm)
  
  ! Perform heapsort
  SORTING_LOOP : DO i=i0,1,-1
  
    ! Swap first node with the node n from the end
    CALL swap(data_in(1),data_in(i)) 

    ! Record permutation
    IF(PRESENT(perm)) CALL swap(perm(1),perm(i))  

    ! Downshift the first element until it reaches
    ! the proper location in the heap.
    CALL downshift(data_in,i1,i-i1,nsize,perm)
  
  ENDDO SORTING_LOOP
  
END SUBROUTINE

! ----------------------------------------------------------------------

SUBROUTINE build_heap(data_in,nsize,perm)
!
! Takes a vector of data and arranges it into a heap
!
  IMPLICIT NONE 

  ! INPUT
  INTEGER(C_SIZE_T),INTENT(IN) :: nsize
  
  ! INPUT/OUTPUT
  REAL(FP),DIMENSION(nsize),INTENT(INOUT) :: data_in

  ! OPTIONAL INPUT/OUTPUT
  INTEGER(IT),DIMENSION(nsize),OPTIONAL,INTENT(INOUT) :: perm

  ! LOCAL
  INTEGER(IT) :: n,i0,pnode  
  
  ! Get size of array
  i0 = SIZE(data_in,1)

  ! Get index of last parent node
  pnode = FLOOR(0.5D0*i0)
  
  ! Downshift data until heap is built
  HEAP_BUILD_LOOP : DO n=pnode,1,-1

    IF(PRESENT(perm)) THEN 
      CALL downshift(data_in,n,i0,nsize,perm)
    ELSE
      CALL downshift(data_in,n,i0,nsize)
    ENDIF 

  ENDDO HEAP_BUILD_LOOP

END SUBROUTINE

! ----------------------------------------------------------

SUBROUTINE downshift(tree,inode,lnode,nsize,perm)
!
! Down shifts the element in the tree at node inode using
! the Heap-Sort downshifting rules (explained below).
!
!                 o ---- inode (initial node)
!                / \
!               /   \
!              /     \
!             o       o ---- inital child node                      
!            / \     / \
!           /   \   /   \   
!          o     o o     o
!
!  NODE LABELING: 
! 
!  Nodes are numbered as follows
!
!         1
!        2 3
!      4 5 6 7
!
!                           [p] ---- pnode
!                           / \
!                          /   \
! cnode1 = 2*cpnode ---- [c1]  [c2] ---- cnode2 = 2*pnode+1
!
!
!  DOWNSHIFT PROCESS:
!
!  Let pnode denote the index of the node that is moved through
!  the tree. 
!
!  Let cnode1 and cnode2 be indices of the two child nodes that 
!  branch from pnode at any given point (since pnode moves, cnode1
!  and cnode2 will vary)
!
!  Let lnode denote the last node in the tree.
!
!  1) Set initial value pnode = inode 
!
!  2) If pnode has no child nodes, then EXIT 
!
!  3) Compute indices of child nodes 
!      
!     cnode1 =     2*pnode
!     cnode2 = MIN(2*pnode + 1,lnode)
!
!     The min. prevents the node for exceeding bounds of the tree
!
!  4) Compute max(c1,c2), where c1 and c2 are the values 
!     at the nodes with index cnode1 and cnode2 respectively.
!
!  5) If max(c1,c2) > p, then swap pnode with the child node with
!     max(c1,c2) and GOTO 2.
!
!  6) If max(c1,c2) < p EXIT LOOP
!
!  BEFORE A DOWNSHIFT:
!
!                [p] ---- pnode
!                / \
!               /   \
! cnode1 ---- [c1]  [c2] ---- cnode2
!
! AFTER A DOWNSHIFT:
!
!  IF c2 > c1 AND c2 > p THEN
!
!                [c2] ---- cnode2
!                / \
!               /   \
! cnode1 ---- [c1]  [p] ---- pnode (active node)
!
!  If the value stored in the active/parent node (pnode) is smaller than
!  one of its child nodes, then the parent and child node are exchanged.
!  The process is repeated for the active node, which is now in a 
!  new position and has new child nodes. The process stops when both 
!  child nodes are less than the active, or when the active node hits
!  the bottom of the tree.
!


  IMPLICIT NONE 

  ! INPUT 
  INTEGER(C_SIZE_T),INTENT(IN) :: nsize
  INTEGER(IT)      ,INTENT(IN) :: inode  ! Initial active node 
  INTEGER(IT)      ,INTENT(IN) :: lnode  ! Index of the last node in the tree

  ! INPUT/OUTPUT 
  REAL(FP),DIMENSION(nsize),INTENT(INOUT) :: tree

  INTEGER(IT),DIMENSION(nsize),OPTIONAL,INTENT(INOUT) :: perm
  
  ! LOCAL
  INTEGER(IT) :: pnode      ! Index of the parent node 
  INTEGER(IT) :: cnode1     ! Index of the first child node 
  INTEGER(IT) :: cnode2     ! Index of the second child node
  INTEGER(IT) :: cnode_max  ! Index of child node that contains the largest
                            ! value (not the largest index -- cnode2 always 
                            ! has a larger index by construction)

  REAL(FP) :: test_value ! The largest value stored in the child node.
                         ! This value is compared to the parent 
                         ! 

  ! STEP 1: Set the current node to the initial node
  pnode = inode

  ! Down shift until lnode is reached or 
  ! until the value at the current node is large
  ! than both its child nodes.
  DOWNSHIFT_LOOP : DO 

    ! STEP 2: If pnode has no child nodes, then exit loop
    IF(2*pnode > lnode) EXIT DOWNSHIFT_LOOP

    ! STEP 3: Compute the index of the two nodes which branch off
    !         cnode
    cnode1 = 2*pnode
    cnode2 = MIN(2*pnode + 1,lnode)

    ! STEP 4 AND 5: 
    ! Determine which child node has the largest value
    ! and set it to test_val, this value will be compared with
    ! the value at the parent (active node).
    IF(tree(cnode1) .le. tree(cnode2)) THEN
      test_value = tree(cnode2)
      cnode_max  = cnode2
    ELSE 
      test_value = tree(cnode1)
      cnode_max  = cnode1
    ENDIF 

    ! STEP 6 and 7: If the max. value of one of the child nodes
    ! exceeds the value of the parent, then swap the nodes, else 
    ! the parent node is in the correct position in the tree, 
    ! so return from subroutine.
    IF(tree(pnode) < test_value) then 

      ! Swap the elements at the active and child nodes
      CALL swap(tree(pnode),tree(cnode_max)) 
      
      ! Record permutation
      IF(PRESENT(perm)) CALL swap(perm(pnode),perm(cnode_max))

      ! Move the active node to the position in the tree
      ! of the old child node 
      pnode = cnode_max

    ELSE
      RETURN
    ENDIF 

  ENDDO DOWNSHIFT_LOOP

END SUBROUTINE

! ----------------------------------------------------------------------

PURE SUBROUTINE swap_real(A,B)
!
!  Swap the values A and B 
!  A -> B, B -> A
!
!  C = A
!  A = B
!  B = C
!
!
  IMPLICIT NONE 

  ! INPUT/OUTPUT
  REAL(FP),INTENT(INOUT) :: A,B

  ! LOCAL 
  REAL(FP) :: C 

  ! C -> A, A -> B, B -> A
  C = A
  A = B
  B = C

END SUBROUTINE

! ----------------------------------------------------------------------

PURE SUBROUTINE swap_integer(A,B)
!
!  Swap the values A and B 
!  A -> B, B -> A
!
!  C = A
!  A = B
!  B = C
!
!
  IMPLICIT NONE 

  ! INPUT/OUTPUT
  INTEGER(IT),INTENT(INOUT) :: A,B

  ! LOCAL 
  INTEGER(IT) :: C 

  ! C -> A, A -> B, B -> A
  C = A
  A = B
  B = C

END SUBROUTINE

END MODULE
