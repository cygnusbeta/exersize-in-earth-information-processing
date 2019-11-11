program temp1
    implicit none
    integer, parameter :: nm = 12
    integer :: i
    real :: mean, sum_mean, sum_2nd_moment, second_moment, var, sd
    real, dimension (nm) :: temp

    open (11, file = '../fortran2/temp.txt', status = 'old')
    do i = 1, nm
        read (11, *) temp(i)
        write (6, *) temp(i)
    end do
    close (11)

    !    Annual avarage of temperature
    sum_mean = 0.0
    do i = 1, nm
        sum_mean = sum_mean + temp(i);
    end do
    mean = sum_mean / real(nm);
    print *, 'mean = ', mean

    !    Standard deviation
    sum_2nd_moment = 0.0
    do i = 1, nm
        sum_2nd_moment = sum_2nd_moment + temp(i) ** 2;
    end do
    second_moment = sum_2nd_moment / real(nm);
    var = second_moment - mean ** 2
    !    print *, 'var = ', var
    sd = sqrt(var)
    print *, 'sd = ', sd

    stop
end program temp1

