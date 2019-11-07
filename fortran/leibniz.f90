program test
    implicit none
    real :: k, sum = 0, n = 10 ** 8, pi
    do k = 0, n
        sum = sum + (-1) ** k / (2 * k + 1)
    end do
    print *, 'sum = ', sum

    pi = 4 * sum
    print *, 'pi = ', pi
    stop
end program test
