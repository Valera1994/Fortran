module Array_io
    use Environment    
    implicit none
    
contains
   subroutine N_M(input_file, N, M)    
      character(*), intent(in)   :: input_file 
      integer, intent(out) :: N, M
      integer     :: In = 0  

      open (file=input_file, newunit=In)
         read (In, '(2i3)') N, M
      close (In)
   end subroutine N_M
   
   function ReadB(input_file, Npadded) result(B)
      character(*), intent(in) :: input_file
       integer, intent(in) :: Npadded
         real(R_), allocatable :: B(:)  
         integer :: In = 0, N = 0, i = 0
        
         open(file=input_file, newunit=In)
            read (In, '(i2)') N 
            allocate (B(Npadded))            
            B = 0                
            read (In, *) (B(i), i = 1, N)       
         close(In)
   end function ReadB     

   subroutine OutputAB(output_file, A, B, N, M)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: A(:, :), B(:)
      integer, intent(in)       :: N, M
      integer :: Out = 0, i = 0

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, *)
         write (Out, '(a)') "Матрица"
         write (Out, '('//M//'f6.1)') (A(i, :), i = 1, N)   
         write (Out, *)
         write (Out, '(a)') "Вектор"
         write (Out, '('//N//'f6.1)') (B(i), i = 1, N)
         write (Out, *)
      close (Out)  
   end subroutine OutputAB
   
   subroutine OutputC(output_file, C, M)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: C(:)
      integer, intent(in)       :: M
      integer :: Out = 0, i = 0

      open (file=output_file, encoding=E_, newunit=Out, position='append')         
         write (Out, '(a)') "Результат умножение вектора на матрицу"
         write (Out, '('//M//'f8.1)') (C(i), i = 1, M)
      close (Out)  
   end subroutine OutputC
end module Array_IO
