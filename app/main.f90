program main
   use mymatmul
   implicit none
   !integer, parameter :: dp = selected_real_kind(p=15)
   integer, parameter :: m = 2, n = 3
   real(kind=dp), dimension(1:m, 1:n) :: a
   real(kind=dp), dimension(1:n, 1:m) :: b
   real(kind=dp), dimension(1:m, 1:m):: c, d
   integer :: i, j, k

   a = reshape((/(((i + j), j=1, n), i=1, m)/), (/m, n/))
   b = reshape((/(((i*j), j=1, m), i=1, n)/), (/n, m/))
   c = 0_dp

   call mymatmuls1(a, b, c, m, n)

   d = matmul(a, b)

   print '(3f9.3)', (a(i, :), i=1, 2)
   print '(2f9.3)', (b(:, i), i=1, 2)

   print '(2f9.3)', (c(i, :), i=1, 2)
   print '(2f9.3)', (d(i, :), i=1, 2)

   c = mymatmulf(a, b)

   print '(2f9.3)', (c(i, :), i=1, 2)

   call mymatmuls2(a, b, c)

   print '(2f9.3)', (c(i, :), i=1, 2)

end program main
