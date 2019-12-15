program temp1
    implicit none
    integer, parameter :: np = 32, nk = 3
    integer :: i, k, k_plus_1, k_x, k_y, k_x1, k_x2, j, k_plus_2
    real, dimension (np, nk) :: x, x_minus_ave
    real, dimension (nk) :: ave, std, cov, a, b, rho, r2, r, b_mul, det_s, s_t, s_e, r2_mul, r_mul, r2_mul_adjusted, rho_par ! cov: [x1x2, x2x3, x3x1]
    real :: sum_ave, sum_2nd_moment, sum_cov, cov_, x_minus_ave_1, x_minus_ave_2, sum_y_f, var, f_i, sum_s_e
    real, dimension (nk, 2) :: a_mul, c, lambda ! lambda: [k, λ_1,2]
    real, dimension (nk, 2, 2) :: s, s_inv, varcov, z ! z: [k, λ_1,2, z_1,2]
    real, dimension (3, 3) :: varcov_3d, varcov_3d_stdzn

    open (11, file = '../fortran7/body.csv', status = 'old')
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
    print *, '- single regression analysis -'
    print *, '[(x = x: 身長, y = y: 手の大きさ),'
    print *, ' (x = y: 手の大きさ, y = z: 足の大きさ),'
    print *, ' (x = z: 足の大きさ, y = x: 身長)]'
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

!    multiple regression analysis
    do k = 1, nk
        k_plus_1 = k + 1
        k_plus_2 = k + 2
        if (k_plus_1 > 3) then
            k_plus_1 = k_plus_1 - 3
        end if
        if (k_plus_2 > 3) then
            k_plus_2 = k_plus_2 - 3
        end if

        k_y = k
        k_x1 = k_plus_1
        k_x2 = k_plus_2

        c(k, 1) = cov(k) * real(np) ! cov(y, x1): cov(x1x2) = cov(k)
        c(k, 2) = cov(k_plus_2) * real(np) ! cov(y, x2): cov(x1x3) = cov(k_plus_2)

        s(k, 1, 1) = std(k_x1) ** 2 * real(np)
        s(k, 2, 1) = cov(k_plus_1) * real(np) ! cov(x1, x2): cov(x2x3) = cov(k_plus_1)
        s(k, 1, 2) = s(k, 2, 1)
        s(k, 2, 2) = std(k_x2) ** 2 * real(np)

        det_s(k) = s(k, 1, 1) * s(k, 2, 2) - s(k, 1, 2) * s(k, 2, 1)
        s_inv(k, 1, 1) = s(k, 2, 2) / det_s(k)
        s_inv(k, 2, 1) = -1 * s(k, 2, 1) / det_s(k)
        s_inv(k, 1, 2) = s_inv(k, 2, 1)
        s_inv(k, 2, 2) = s(k, 1, 1) / det_s(k)

        do j = 1, 2
            a_mul(k, j) = s_inv(k, j, 1) * c(k, 1) + s_inv(k, j, 2) * c(k, 2)
        end do

!        b_mul: [
!           b_(y = x: 身長, x1 = y: 手の大きさ, x2 = z: 足の大きさ), b_(y = y: 手の大きさ, x1 = z: 足の大きさ, x2 = x: 身長), b_(y = z: 足の大きさ, x1 = x: 身長, x2 = y: 手の大きさ)
!        ]
        b_mul(k) = ave(k_y) - (a_mul(k, 1) * ave(k_x1) + a_mul(k, 2) * ave(k_x2))

        s_t(k) = std(k_y) ** 2 * real(np)

        sum_s_e = 0.0
        do i = 1, np
            f_i = a_mul(k, 1) * x(i, k_x1) + a_mul(k, 2) * x(i, k_x2) + b_mul(k)
            sum_s_e = sum_s_e + (x(i, k_y) - f_i) ** 2
        end do
        s_e(k) = sum_s_e

        r2_mul(k) = 1.0 - s_e(k) / s_t(k)
        r_mul(k) = sqrt(r2_mul(k))
        r2_mul_adjusted(k) = 1.0 - ((s_e(k) / real(np - 2 - 1)) / (s_t(k) / real(np - 1)))

!        x1 y: x1x2 rho(k)
!        x2 x1: x2x3 rho(k_plus_1)
!        x2 y: x1x3 rho(k_plus_2)

!        rho_par: ρ_(x1 y, x2)
        rho_par(k) = (rho(k) - rho(k_plus_1) * rho(k_plus_2)) / (sqrt(1.0 - rho(k_plus_1) ** 2) * sqrt(1.0 - rho(k_plus_2) ** 2))
    end do

    print *, ''
    print *, '- multiple regression analysis -'
    print *, '[(y = x: 身長, x1 = y: 手の大きさ, x2 = z: 足の大きさ),'
    print *, ' (y = y: 手の大きさ, x1 = z: 足の大きさ, x2 = x: 身長),'
    print *, ' (y = z: 足の大きさ, x1 = x: 身長, x2 = y: 手の大きさ)]'
    print *, 'a_mul = ', a_mul
    do k = 1, nk
        print *, 'a_mul(', k, ', *, *) = ', a_mul(k, :)
    end do
    print *, 'b_mul = ', b_mul
    print *, 'c = ', c
    print *, 's = ', s
    print *, 'det_s = ', det_s
    print *, 's_inv = ', s_inv
    print *, 's_t = ', s_t
    print *, 's_e = ', s_e
    print *, 'r2_mul = ', r2_mul
    print *, 'r_mul = ', r_mul
    print *, 'r2_mul_adjusted = ', r2_mul_adjusted
    print *, 'rho_par: ρ_(x1 y, x2) = ', rho_par

!    pca
    do k = 1, nk
        varcov = s / (np - 1)
    end do

    print *, ''
    print *, '- pca -'
    print *, 'k = 1: (x = x: 身長, y = y: 手の大きさ)'
    print *, 'k = 2: (x = y: 手の大きさ, y = z: 足の大きさ)'
    print *, 'k = 3: (x = z: 足の大きさ, y = x: 身長)'
    print *, '(np - 1) = ', np - 1
    print *, 's = ', s
    print *, 'varcov = ', varcov

    do k = 1, nk
        lambda(k, 1) = ((varcov(k, 1, 1) + varcov(k, 2, 2)) + sqrt(((varcov(k, 1, 1) + varcov(k, 2, 2)) ** 2 - 4 * &
                (varcov(k, 1, 1) * varcov(k, 2, 2) - varcov(k, 2, 1) * varcov(k, 1, 2))))) / 2
        lambda(k, 2) = ((varcov(k, 1, 1) + varcov(k, 2, 2)) - sqrt(((varcov(k, 1, 1) + varcov(k, 2, 2)) ** 2 - 4 * &
                (varcov(k, 1, 1) * varcov(k, 2, 2) - varcov(k, 2, 1) * varcov(k, 1, 2))))) / 2

        print *, ''
        print *, 'k: ', k
        do j = 1, 2
            z(k, j, 1) = -1 * varcov(k, 1, 2) / sqrt(varcov(k, 1, 2) ** 2 + (varcov(k, 1, 1) - lambda(k, j)) ** 2)
            z(k, j, 2) = (varcov(k, 1, 1) - lambda(k, j)) / sqrt(varcov(k, 1, 2) ** 2 + (varcov(k, 1, 1) - lambda(k, j)) ** 2)
            print *, 'lambda', j, ': ', lambda(k, j)
            print *, 'z: ', z(k, j, 1), z(k, j, 2)
        end do
    end do

    print *, ''
    print *, '- multivariate pca (non-standardized) -'
    print *, '(x: 身長, y: 手の大きさ, z: 足の大きさ)'
!    print *, 'np', np
!    print *, 'real(np - 1.0)', real(np - 1.0)
!    print *, 'std', std
!    print *, 'nk', nk
    varcov_3d(1, 1) = std(1) ** 2 * real(np) / real(np - 1.0)
    varcov_3d(2, 1) = cov(1) * real(np) / real(np - 1.0)
    varcov_3d(3, 1) = cov(3) * real(np) / real(np - 1.0)
    varcov_3d(2, 2) = std(2) ** 2 * real(np) / real(np - 1.0)
    varcov_3d(3, 2) = cov(2) * real(np) / real(np - 1.0)
    varcov_3d(3, 3) = std(3) ** 2 * real(np) / real(np - 1.0)
    varcov_3d(1, 2) = varcov_3d(2, 1)
    varcov_3d(1, 3) = varcov_3d(3, 1)
    varcov_3d(2, 3) = varcov_3d(3, 2)

    call multivariate_pca(varcov_3d)

    print *, ''
    print *, '- multivariate pca (standardized) -'
    print *, '(x: 身長, y: 手の大きさ, z: 足の大きさ)'
    !    print *, 'np', np
    !    print *, 'real(np - 1.0)', real(np - 1.0)
    !    print *, 'std', std
    !    print *, 'nk', nk
    varcov_3d_stdzn(1, 1) = std(1) ** 2 * real(np) / real(np - 1.0) / (std(1) ** 2 * real(np) / real(np - 1.0))
    varcov_3d_stdzn(2, 1) = cov(1) * real(np) / real(np - 1.0) / (std(1) * std(2) * real(np) / real(np - 1.0))
    varcov_3d_stdzn(3, 1) = cov(3) * real(np) / real(np - 1.0) / (std(3) * std(1) * real(np) / real(np - 1.0))
    varcov_3d_stdzn(2, 2) = std(2) ** 2 * real(np) / real(np - 1.0) / (std(2) ** 2 * real(np) / real(np - 1.0))
    varcov_3d_stdzn(3, 2) = cov(2) * real(np) / real(np - 1.0) / (std(2) * std(3) * real(np) / real(np - 1.0))
    varcov_3d_stdzn(3, 3) = std(3) ** 2 * real(np) / real(np - 1.0) / (std(3) ** 2 * real(np) / real(np - 1.0))
    varcov_3d_stdzn(1, 2) = varcov_3d_stdzn(2, 1)
    varcov_3d_stdzn(1, 3) = varcov_3d_stdzn(3, 1)
    varcov_3d_stdzn(2, 3) = varcov_3d_stdzn(3, 2)

    call multivariate_pca(varcov_3d_stdzn)

    stop

contains
    subroutine multivariate_pca(varcov_3d_intent)
        implicit none
        integer :: o, l, m, n
        real :: y_len
        real, dimension (3) :: lambda_mul
        real, dimension (3, 3) :: ev_mul, varcov_3d_sub
        real, dimension (3, 3), intent(in) :: varcov_3d_intent
        real, dimension (3) :: u_, y

        u_(1) = 1.0
        u_(2) = 0.0
        u_(3) = 0.0
        varcov_3d_sub = varcov_3d_intent

        do o = 1, 3
            do l = 1, 20
                y(1) = varcov_3d_sub(1, 1) * u_(1) + varcov_3d_sub(1, 2) * u_(2) + varcov_3d_sub(1, 3) * u_(3)
                y(2) = varcov_3d_sub(2, 1) * u_(1) + varcov_3d_sub(2, 2) * u_(2) + varcov_3d_sub(2, 3) * u_(3)
                y(3) = varcov_3d_sub(3, 1) * u_(1) + varcov_3d_sub(3, 2) * u_(2) + varcov_3d_sub(3, 3) * u_(3)
                y_len = sqrt(y(1) ** 2 + y(2) ** 2 + y(3) ** 2)
                u_(1) = y(1) / y_len
                u_(2) = y(2) / y_len
                u_(3) = y(3) / y_len
    !                print *, '> l =', l
    !                print *, '> y =', y
    !                print *, '> u_ =', u_
            end do

            y(1) = varcov_3d_sub(1, 1) * u_(1) + varcov_3d_sub(1, 2) * u_(2) + varcov_3d_sub(1, 3) * u_(3)
            y(2) = varcov_3d_sub(2, 1) * u_(1) + varcov_3d_sub(2, 2) * u_(2) + varcov_3d_sub(2, 3) * u_(3)
            y(3) = varcov_3d_sub(3, 1) * u_(1) + varcov_3d_sub(3, 2) * u_(2) + varcov_3d_sub(3, 3) * u_(3)

            !            print *, '> y =', y

            ev_mul(o, 1) = u_(1)
            ev_mul(o, 2) = u_(2)
            ev_mul(o, 3) = u_(3)
            lambda_mul(o) = (y(1) * u_(1) + y(2) * u_(2) + y(3) * u_(3)) / (u_(1) ** 2 + u_(2) ** 2 + u_(3) ** 2)

            do m = 1, 3
                do n = 1, 3
                    varcov_3d_sub(m, n) = varcov_3d_sub(m, n) - lambda_mul(o) * u_(m) * u_(n)
                end do
            end do

            print *, 'lambda_mul', o, ': ', lambda_mul(o)
            print *, 'ev_mul: ', ev_mul(o, 1), ev_mul(o, 2), ev_mul(o, 3)
        end do
    end subroutine multivariate_pca
end program temp1
