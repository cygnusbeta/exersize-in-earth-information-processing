program test
    implicit none
    real :: Gc = 6.67408e-11, M = 5.972 * 10 ** 24, R = 6371e3, g
    print *, '万有引力定数 G =', Gc
    print *, '地球質量 M =', M
    print *, '地球半径 R =', R
    g = Gc * M / R ** 2
    print *, ''
    print *, '重力加速度 g =', g
    stop
end program test
