Attribute VB_Name = "Module2"
Option Explicit

'Sub PrintArray(Data As Variant, Cl As Range)
'    Cl.Resize(UBound(Data, 1), UBound(Data, 2)) = Data
'End Sub

Sub Hyouzyunnka()
    Dim np As Integer: np = 32
    Dim nk As Integer: nk = 3
    Dim i As Integer, k As Integer, k_plus_1 As Integer, k_x As Integer, k_y As Integer, k_x1 As Integer, k_x2 As Integer, j As Integer, k_plus_2 As Integer
    Dim x() As Double, x_minus_ave() As Double, x_stdzn() As Double
    ReDim x(32, 3), x_minus_ave(32, 3), x_stdzn(32, 3)
    Dim ave() As Double, std() As Double, cov() As Double, a() As Double, b() As Double, rho() As Double, r2() As Double, r() As Double, b_mul() As Double, det_s() As Double, s_t() As Double, s_e() As Double, r2_mul() As Double, r_mul() As Double, r2_mul_adjusted() As Double, rho_par() As Double ' cov: [x1x2, x2x3, x3x1]
    ReDim ave(3), std(3), cov(3), a(3), b(3), rho(3), r2(3), r(3), b_mul(3), det_s(3), s_t(3), s_e(3), r2_mul(3), r_mul(3), r2_mul_adjusted(3), rho_par(3) ' cov: [x1x2, x2x3, x3x1]
    Dim sum_ave As Double, sum_2nd_moment As Double, sum_cov As Double, cov_ As Double, x_minus_ave_1 As Double, x_minus_ave_2 As Double, sum_y_f As Double, Var, f_i As Double, sum_s_e As Double
    Dim a_mul() As Double, c() As Double
    ReDim a_mul(3, 2), c(3, 2)
    Dim s() As Double, s_inv() As Double, varcov() As Double, varcov_stdzn() As Double
    ReDim s(3, 2, 2), s_inv(3, 2, 2), varcov(3, 2, 2), varcov_stdzn(3, 2, 2)
    Dim varcov_3d() As Double, varcov_3d_stdzn() As Double, stdzn_matrix() As Double
    ReDim varcov_3d(3, 3), varcov_3d_stdzn(3, 3), stdzn_matrix(3, 3)

    For i = 1 To 32
        For j = 1 To 3
            x(i, j) = Selection.Cells(i, j)
        Next
    Next

    
    For k = 1 To nk
'        ave
        sum_ave = 0#
        For i = 1 To np
            sum_ave = sum_ave + x(i, k)
        Next
        ave(k) = sum_ave / CDbl(np)

'        sd
        sum_2nd_moment = 0#
        For i = 1 To np
            sum_2nd_moment = sum_2nd_moment + x(i, k) ^ 2
        Next
        std(k) = Sqr(sum_2nd_moment / CDbl(np) - ave(k) ^ 2)

'        x_minus_ave
        For i = 1 To np
            x_minus_ave(i, k) = x(i, k) - ave(k)
        Next
    Next

    For k = 1 To nk
'        cov
        If (k = 3) Then
            k_plus_1 = 1
        Else
            k_plus_1 = k + 1
        End If
        sum_cov = 0#
'        ' Debug.Print  k_plus_1
        For i = 1 To np
'x_minus_ave_1 = x_minus_ave(i, k)
'x_minus_ave_2 = x_minus_ave(i, k_plus_1)
'            ' Debug.Print  cov_
            sum_cov = sum_cov + x_minus_ave(i, k) * x_minus_ave(i, k_plus_1)
'            ' Debug.Print  sum_cov
        Next
        cov(k) = sum_cov / CDbl(np)
    Next

    ' PrintArray ave, ActiveWorkbook.Worksheets("Sheet1").[A1]
    ' Debug.Print  "std = ": ' Debug.Print  std
'    ' Debug.Print  x
'    ' Debug.Print  x_minus_ave
    ' Debug.Print  ""
    ' Debug.Print  "- single regression analysis -"
    ' Debug.Print  "[(x = x: 身長, y = y: 手の大きさ),"
    ' Debug.Print  " (x = y: 手の大きさ, y = z: 足の大きさ),"
    ' Debug.Print  " (x = z: 足の大きさ, y = x: 身長)]"
    ' Debug.Print  "cov = ": ' Debug.Print  cov

    For k = 1 To nk
'        a, b, rho, r ^ 2, r
        If (k = 3) Then
            k_plus_1 = 1
        Else
            k_plus_1 = k + 1
        End If

        k_x = k
        k_y = k_plus_1

        Var = std(k_x) ^ 2
        a(k) = cov(k) / std(k_x) ^ 2
        b(k) = ave(k_y) - a(k) * ave(k_x)
        rho(k) = cov(k) / (std(k_x) * std(k_y))

        sum_y_f = 0#
        For i = 1 To np
            sum_y_f = sum_y_f + (x(i, k_y) - (a(k) * x(i, k_x) + b(k))) ^ 2
        Next
        r2(k) = 1 - sum_y_f / (std(k_y) ^ 2 * CDbl(np))
        r(k) = Sqr(r2(k))
    Next

    ' Debug.Print  "a = ": ' Debug.Print  a
    ' Debug.Print  "b = ": ' Debug.Print  b
    ' Debug.Print  "rho = ": ' Debug.Print  rho
    ' Debug.Print  "r2 = ": ' Debug.Print  r2
    ' Debug.Print  "r = ": ' Debug.Print  r

'    multiple regression analysis
    For k = 1 To nk
        k_plus_1 = k + 1
        k_plus_2 = k + 2
        If (k_plus_1 > 3) Then
            k_plus_1 = k_plus_1 - 3
        End If
        If (k_plus_2 > 3) Then
            k_plus_2 = k_plus_2 - 3
        End If

        k_y = k
        k_x1 = k_plus_1
        k_x2 = k_plus_2

        c(k, 1) = cov(k) * CDbl(np) ' cov(y, x1): cov(x1x2) = cov(k)
        c(k, 2) = cov(k_plus_2) * CDbl(np) ' cov(y, x2): cov(x1x3) = cov(k_plus_2)

        s(k, 1, 1) = std(k_x1) ^ 2 * CDbl(np)
        s(k, 2, 1) = cov(k_plus_1) * CDbl(np) ' cov(x1, x2): cov(x2x3) = cov(k_plus_1)
        s(k, 1, 2) = s(k, 2, 1)
        s(k, 2, 2) = std(k_x2) ^ 2 * CDbl(np)

        det_s(k) = s(k, 1, 1) * s(k, 2, 2) - s(k, 1, 2) * s(k, 2, 1)
        s_inv(k, 1, 1) = s(k, 2, 2) / det_s(k)
        s_inv(k, 2, 1) = -1 * s(k, 2, 1) / det_s(k)
        s_inv(k, 1, 2) = s_inv(k, 2, 1)
        s_inv(k, 2, 2) = s(k, 1, 1) / det_s(k)

        For j = 1 To 2
            a_mul(k, j) = s_inv(k, j, 1) * c(k, 1) + s_inv(k, j, 2) * c(k, 2)
        Next

'        b_mul: [
'           b_(y = x: 身長, x1 = y: 手の大きさ, x2 = z: 足の大きさ), b_(y = y: 手の大きさ, x1 = z: 足の大きさ, x2 = x: 身長), b_(y = z: 足の大きさ, x1 = x: 身長, x2 = y: 手の大きさ)
'        ]
        b_mul(k) = ave(k_y) - (a_mul(k, 1) * ave(k_x1) + a_mul(k, 2) * ave(k_x2))

        s_t(k) = std(k_y) ^ 2 * CDbl(np)

        sum_s_e = 0#
        For i = 1 To np
            f_i = a_mul(k, 1) * x(i, k_x1) + a_mul(k, 2) * x(i, k_x2) + b_mul(k)
            sum_s_e = sum_s_e + (x(i, k_y) - f_i) ^ 2
        Next
        s_e(k) = sum_s_e

        r2_mul(k) = 1# - s_e(k) / s_t(k)
        r_mul(k) = Sqr(r2_mul(k))
        r2_mul_adjusted(k) = 1# - ((s_e(k) / CDbl(np - 2 - 1)) / (s_t(k) / CDbl(np - 1)))

'        x1 y: x1x2 rho(k)
'        x2 x1: x2x3 rho(k_plus_1)
'        x2 y: x1x3 rho(k_plus_2)

'        rho_par: ρ_(x1 y, x2)
        rho_par(k) = (rho(k) - rho(k_plus_1) * rho(k_plus_2)) / (Sqr(1# - rho(k_plus_1) ^ 2) * Sqr(1# - rho(k_plus_2) ^ 2))
    Next

    ' Debug.Print  ""
    ' Debug.Print  "- multiple regression analysis -"
    ' Debug.Print  "[(y = x: 身長, x1 = y: 手の大きさ, x2 = z: 足の大きさ),"
    ' Debug.Print  " (y = y: 手の大きさ, x1 = z: 足の大きさ, x2 = x: 身長),"
    ' Debug.Print  " (y = z: 足の大きさ, x1 = x: 身長, x2 = y: 手の大きさ)]"
    ' Debug.Print  "a_mul = ": ' Debug.Print  a_mul
    For k = 1 To nk
        ' Debug.Print  "a_mul(": ' Debug.Print  k: ' Debug.Print  ", *, *) = ": ' Debug.Print  a_mul(k, 3): ' Debug.Print  a_mul(k, 2)
    Next
    ' Debug.Print  "b_mul = ", b_mul
    ' Debug.Print  "c = ", c
    ' Debug.Print  "s = ", s
    ' Debug.Print  "det_s = ", det_s
    ' Debug.Print  "s_inv = ", s_inv
    ' Debug.Print  "s_t = ", s_t
    ' Debug.Print  "s_e = ", s_e
    ' Debug.Print  "r2_mul = ", r2_mul
    ' Debug.Print  "r_mul = ", r_mul
    ' Debug.Print  "r2_mul_adjusted = ", r2_mul_adjusted
    ' Debug.Print  "rho_par: ρ_(x1 y, x2) = ", rho_par

'    pca
    Dim ix As Double, iy As Double
    For k = 1 To nk
        For ix = 1 To 2
            For iy = 1 To 2
                varcov(k, ix, iy) = s(k, ix, iy) / (np - 1)
            Next
        Next
    Next

    ' Debug.Print  ""
    ' Debug.Print  "- pca (non-standardized) -"
    ' Debug.Print  "k = 1: (x = x: 身長, y = y: 手の大きさ)"
    ' Debug.Print  "k = 2: (x = y: 手の大きさ, y = z: 足の大きさ)"
    ' Debug.Print  "k = 3: (x = z: 足の大きさ, y = x: 身長)"
    ' Debug.Print  "(np - 1) = ", np - 1
    ' Debug.Print  "s = ", s
    ' Debug.Print  "varcov = ", varcov

    ' Call pca(varcov)

    ' Debug.Print  ""
    ' Debug.Print  "- pca (standardized) -"
    ' Debug.Print  "k = 1: (x = x: 身長, y = y: 手の大きさ)"
    ' Debug.Print  "k = 2: (x = y: 手の大きさ, y = z: 足の大きさ)"
    ' Debug.Print  "k = 3: (x = z: 足の大きさ, y = x: 身長)"
    ' Debug.Print  "(np - 1) = ", np - 1

    For k = 1 To nk
        k_plus_1 = k + 1
        k_plus_2 = k + 2
        If (k_plus_1 > 3) Then
            k_plus_1 = k_plus_1 - 3
        End If
        If (k_plus_2 > 3) Then
            k_plus_2 = k_plus_2 - 3
        End If

        k_y = k
        k_x1 = k_plus_1
        k_x2 = k_plus_2

        varcov_stdzn(k, 1, 1) = varcov(k, 1, 1) / (std(k_x1) ^ 2 * CDbl(np) / CDbl(np - 1))
        varcov_stdzn(k, 2, 1) = varcov(k, 2, 1) / (std(k_plus_1) * std(k_plus_2) * CDbl(np) / CDbl(np - 1))
        varcov_stdzn(k, 1, 2) = varcov_stdzn(k, 2, 1)
        varcov_stdzn(k, 2, 2) = varcov(k, 2, 2) / (std(k_x2) ^ 2 * CDbl(np) / CDbl(np - 1))
        
        For ix = 1 To 2
            For iy = 1 To 2
'                Worksheets("Sheet2").Range("A3:B4").Cells(ix, iy).Value = varcov_stdzn(1, ix, iy)
'                Worksheets("Sheet2").Range("D3:E4").Cells(ix, iy).Value = varcov_stdzn(2, ix, iy)
'                Worksheets("Sheet2").Range("G3:H4").Cells(ix, iy).Value = varcov_stdzn(3, ix, iy)
            Next
        Next
        
        For ix = 1 To np
            Worksheets("Sheet2").Range("A8:C39").Cells(ix, k).Value = (x(ix, k) - ave(k)) / Sqr((std(k) ^ 2 * CDbl(np) / CDbl(np - 1)))
        Next
    Next

'    s(k, 1, 1) = std(k_x1) ^ 2 * CDbl(np)
'    s(k, 2, 1) = cov(k_plus_1) * CDbl(np) ' cov(x1, x2): cov(x2x3) = cov(k_plus_1)
's(k, 1, 2) = s(k, 2, 1)
'    s(k, 2, 2) = std(k_x2) ^ 2 * CDbl(np)

    ' Debug.Print  "varcov_stdzn = ", varcov_stdzn
    Worksheets("Sheet2").Select
End Sub

