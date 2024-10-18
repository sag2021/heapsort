# Heapsort

Heapsort subroutine for Fortran. Developed for a long-forgotten project. Code is Fortran 2003, 
because it makes selecting INTEGER easier. 

Main subroutine: heap_sort(data_in,nsize,perm)

- data_in(nsize): A vector of reals to be sorted. The sorting is performed in place. 
- nsize: The number of points in data_in
- perm(nsize): A vector that maps positions in the sorted vector back to the original, i.e. unsorted(perm(i)) = sorted(i)

# Test

The test program sorts a random vector of reals and computes the permutation vector too.
It then checks that the sorted vector is actually sorted (in ascending order). It also
checks that the permutation vector performs the correct mapping of indices. 

Compiled and tested using gfortran 8.3:
- gfortran -O3 heapsort.f90 test.f90 -o test -std=f2003
