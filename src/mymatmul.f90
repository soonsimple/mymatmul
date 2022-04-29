module mymatmul
   implicit none
   integer, parameter :: dp = selected_real_kind(p=15)
   private

   public :: dp, say_hello, mymatmuls1, mymatmuls2, mymatmulf

contains
   subroutine say_hello
      print *, "Hello, mymatmul!"
   end subroutine say_hello

   subroutine mymatmuls1(a, b, c, m, n)
      integer :: m, n
      real(kind=dp), dimension(1:m, 1:n), intent(in) :: a
      real(kind=dp), dimension(1:n, 1:m), intent(in) :: b
      real(kind=dp), dimension(1:m, 1:m), intent(out):: c
      integer :: i, j, k

      do i = 1, m
         do j = 1, m
            c(i, j) = 0._dp
            do k = 1, n
               c(i, j) = c(i, j) + a(i, k)*b(k, j)
            end do
         end do
      end do
   end subroutine

   subroutine mymatmuls2(a, b, cc)
      integer :: m, n
      real(kind=dp), dimension(:, :), intent(in) :: a
      real(kind=dp), dimension(:, :), intent(in) :: b
      !real(kind=dp), dimension(1:size(a, 1), 1:size(a, 1)), intent(out) :: cc
      real(kind=dp), dimension(:, :), intent(out) :: cc
      integer :: i, j, k

      m = size(a, 1)
      n = size(a, 2)

      do i = 1, m
         do j = 1, m
            cc(i, j) = 0._dp
            do k = 1, n
               cc(i, j) = cc(i, j) + a(i, k)*b(k, j)
            end do
         end do
      end do
   end subroutine

   function mymatmulf(a, b)
      integer :: m, n
      real(kind=dp), dimension(:, :), intent(in) :: a
      real(kind=dp), dimension(:, :), intent(in) :: b
      real(kind=dp), dimension(1:size(a, 1), 1:size(a, 1)) :: mymatmulf
      !real(kind=dp), dimension(:, :) :: mymatmulf
      ! error: Array 'mymatmulf' cannot have a deferred shape
      integer :: i, j, k

      m = size(a, 1)
      n = size(a, 2)

      do i = 1, m
         do j = 1, m
            mymatmulf(i, j) = 0._dp
            do k = 1, n
               mymatmulf(i, j) = mymatmulf(i, j) + a(i, k)*b(k, j)
            end do
         end do
      end do
   end function

end module mymatmul
