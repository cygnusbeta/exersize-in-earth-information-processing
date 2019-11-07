program test
    implicit none
    integer :: y
    write(6, *) "Input year"
    read(5, *) y
    if (mod(y, 400) == 0 .or. mod(y, 4) == 0 .and. mod(y, 100) /= 0) then
        write(*, *) "うるう年です"
    end if
    stop
end program test