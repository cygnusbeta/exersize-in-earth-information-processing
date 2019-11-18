program temp1
    implicit none
    integer, parameter :: np = 32, nk = 3
    integer :: i, k
    real, dimension (np, nk) :: x
    real, dimension (nk) :: ave, std
    real :: sum_ave, sum_2nd_moment

    open (11, file = '../fortran2/body.csv', status = 'old')
    do i = 1, np
        read (11, *) (x(i, k), k = 1, nk)
    end do
    close (11)

    do k = 1, nk
        sum_ave = 0.0
        do i = 1, np
            sum_ave = sum_ave + x(i, k)
        end do
        ave(k) = sum_ave / real(np)

        sum_2nd_moment = 0.0
        do i = 1, np
            sum_2nd_moment = sum_2nd_moment + x(i, k) ** 2
        end do
        std(k) = sqrt(sum_2nd_moment / real(np) - ave(k) ** 2)
    end do

    print *, 'ave = ', ave
    print *, 'std = ', std

    stop
end program temp1

