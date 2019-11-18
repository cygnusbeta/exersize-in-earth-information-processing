program temp1
    implicit none
    integer, parameter :: np = 32, nk = 3
    integer :: i, k, k_plus_1, k_x, k_y
    real, dimension (np, nk) :: x, x_minus_ave
    real, dimension (nk) :: ave, std, cov, a, b, rho, r2, r ! cov: [x1x2, x2x3, x3x1]
    real :: sum_ave, sum_2nd_moment, sum_cov, cov_, x_minus_ave_1, x_minus_ave_2, sum_y_f, var

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
    print *, ''
    print *, '[x: 身長, y: 手の大きさ], [y: 手の大きさ, z: 足の大きさ], [z: 足の大きさ, x: 身長]'
    print *, 'cov = ', cov

    do k = 1, nk
!        a, b, rho, r ** 2, r
        if (k == 3) then
            k_plus_1 = 1
        else
            k_plus_1 = k + 1
        end if

        k_x = k
        k_y = k_plus_1

        var = std(k_x) ** 2
        a(k) = cov(k) / std(k_x) ** 2
        b(k) = ave(k_y) - a(k) * ave(k_x)
        rho(k) = cov(k) / (std(k_x) * std(k_y))

        sum_y_f = 0.0
        do i = 1, np
            sum_y_f = sum_y_f + (x(i, k_y) - (a(k) * x(i, k_x) + b(k))) ** 2
        end do
        r2(k) = 1 - sum_y_f / (std(k_y) ** 2 * real(np))
        r(k) = sqrt(r2(k))
    end do

    print *, 'a = ', a
    print *, 'b = ', b
    print *, 'rho = ', rho
    print *, 'r2 = ', r2
    print *, 'r = ', r

    stop
end program temp1
