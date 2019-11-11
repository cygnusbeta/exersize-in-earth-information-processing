program test
    implicit none
    real :: r, s
    real, parameter :: pi = 3.141592
    write(6, *) "Input r"
    read(5, *) r
    s = pi * r ** 2.0
    write(6, *) "S = ", s
    stop
end program test