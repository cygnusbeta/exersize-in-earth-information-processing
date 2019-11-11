program test
    implicit none
    integer, parameter :: n = 10 ** 8
    real :: k, pi, sum = 0
    do k = 0, n
        sum = sum + (-1) ** k / (2 * k + 1)
    end do
    print *, 'sum = ', sum

    pi = 4 * sum
    print *, 'pi = ', pi
    stop
end program test
