program test
    implicit none
    real :: t, s
    write(6, *) "Input temp"
    read(5, *) t
    if (t > 35) then
        write(*, *) "Extreme Hot Day"
    end if
    stop
end program test