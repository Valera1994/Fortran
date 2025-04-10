module Matrix_process
   use Environment
   implicit none
 
contains
   subroutine Matrix(N, M, A)
     integer, intent(in) :: N, M  
     real(R_), allocatable, intent(out) :: A(:,:)
     integer :: i, j, Npadded
        
     Npadded = N + Modulo(-N, 32/R_)
     !$omp allocate(A) align(32)
     allocate(A(Npadded, M), source = 0.0)
     ! Заполнение матрицы по строкам
     do concurrent (j = 1:M)
         !$omp simd aligned(A)
         do i = 1, N
             A(i, j) = j * (i + 5)
         end do
         !$omp end simd
     end do
    end subroutine Matrix
   
   pure function Ma(A, B) result(C)
      real(R_), intent(in) :: A(:, :), B(:)
      real(R_), allocatable :: C(:)          
      integer :: i, j      
    
      allocate(C(1:UBound(A, 2)), source = 0.0)      
      do concurrent (j = 1:UBound(A, 2))
         do i = 1, UBound(A, 1)      
            C(j) = C(j) + A(i, j) * B(i)
         end do
      end do      
   end function Ma
end module Matrix_process
