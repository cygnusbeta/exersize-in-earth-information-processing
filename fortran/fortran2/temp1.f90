program temp1

    implicit none
    integer, parameter :: nm = 12
    integer :: i
    real :: mean, sum
    real, dimension (nm) :: temp

    open (11, file = '../fortran2/temp.txt', status = 'old')
    do i = 1, nm
        read (11, *) temp(i)
        write (6, *) temp(i)
    end do
    close (11)

    ! Annual avarage of temperature
    sum=0.0
    do i = 1, nm
        sum = sum + temp(i);
    end do
    mean = sum / real(nm);
    print *, 'mean = ', mean

    stop

end program temp1

