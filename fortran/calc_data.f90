program calc
    implicit none
    integer i
    integer, parameter :: im = 32, jm = 1
    integer, dimension(im, jm) :: tall
    integer, dimension(im, jm) :: hand
    integer, dimension(im, jm) :: foot

    open(10, FILE="../data.txt", ACTION="READ")
        DO i = 1, 32
            READ(10, *) tall(i, 1), hand(i, 2), foot(i, 3)
        END DO
    close(10)

    write(*,*) tall
    write(*,*) hand
    write(*,*) foot

    
end program calc
