program temp1
    implicit none
    integer, parameter :: np = 32, nk = 3
    integer :: i, k, k_plus_1, k_x, k_y
    real, dimension (np, nk) :: x, x_minus_ave
    real, dimension (nk) :: ave, std, cov, a, b ! cov: [x1x2, x2x3, x3x1]
    real :: sum_ave, sum_2nd_moment, sum_cov, cov_, x_minus_ave_1, x_minus_ave_2

    open (11, file = '../fortran3/body.csv', status = 'old')
    do i = 1, np
        read (11, *) (x(i, k), k = 1, nk)
    end do
    close (11)

    do k = 1, nk
!        ave
        sum_ave = 0.0
        do i = 1, np
            sum_ave = sum_ave + x(i, k)
        end do
        ave(k) = sum_ave / real(np)

!        sd
        sum_2nd_moment = 0.0
        do i = 1, np
            sum_2nd_moment = sum_2nd_moment + x(i, k) ** 2
        end do
        std(k) = sqrt(sum_2nd_moment / real(np) - ave(k) ** 2)

!        x_minus_ave
        do i = 1, np
            x_minus_ave(i, k) = x(i, k) - ave(k)
        end do
    end do

    do k = 1, nk
!        cov
        if (k == 3) then
            k_plus_1 = 1
        else
            k_plus_1 = k + 1
        end if
        sum_cov = 0.0
!        print *, k_plus_1
        do i = 1, np
!            x_minus_ave_1 = x_minus_ave(i, k)
!            x_minus_ave_2 = x_minus_ave(i, k_plus_1)
!            print *, cov_
            sum_cov = sum_cov + x_minus_ave(i, k) * x_minus_ave(i, k_plus_1)
!            print *, sum_cov
        end do
        cov(k) = sum_cov / real(np)
    end do

    print *, 'ave = ', ave
    print *, 'std = ', std
!    print *, x
!    print *, x_minus_ave
    print *, 'cov = ', cov

    stop
end program temp1
