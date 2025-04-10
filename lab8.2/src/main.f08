program lab_8_2
   use Environment
   use Array_io
   use Matrix_process

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: N = 0, M = 0, Npadded = 0
   real(R_), allocatable    :: A(:, :), B(:), C(:) 
   
   call N_M(input_file, N, M) 
   Npadded = N + Modulo(-N, 32/R_)
   !$omp allocate(A) align(32)
   allocate(A(Npadded, M), source = 0.0)
   
   !$OMP PARALLEL SECTIONS
   !$OMP SECTION 
   call Matrix(N, M, A)   
   !$OMP SECTION 
   B = ReadB(input_file, Npadded)
   !$OMP END PARALLEL SECTIONS
   
   call OutputAB(output_file, A, B, N, M)   
 
   C = Ma(A, B)
   
   call OutputC(output_file, C, M)
end program lab_8_2
