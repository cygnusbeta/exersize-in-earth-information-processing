Attribute VB_Name = "Module1"
Sub pca()
 ' -- プログラム中の変数の定義 --
 Dim NPARAM As Integer ' 変数の数 （列数）
 Dim NSAMPL As Integer ' サンプル数（行数）
 Dim x() As Double ' 観測データ（配列データ）
 Dim cov() As Double ' 共分散行列（配列データ）
 Dim ave() As Double ' 変数ごとの平均値（配列データ）
 Dim SUM As Double ' 合計値
 Dim DSAMPL As Double ' サンプル数（実数）
 Dim i As Integer
 Dim j As Integer
 Dim n As Integer
 ' -- 選択領域のデータや解析する変数の数 --
 NPARAM = Selection.Columns.Count
 NSAMPL = Selection.Rows.Count
 DSAMPL = CDbl(NSAMPL) ' 実数でも定義
 If NPARAM = 1 And NSAMPL = 1 Then
 MsgBox "エラー：解析するデータ領域を選択してマクロを実行してください"
 Exit Sub
 End If
 ' -- 配列の準備 --
 ReDim x(NPARAM, NSAMPL), cov(NPARAM, NPARAM), ave(NPARAM)

 ' -- データの読み込み --
 For n = 1 To NSAMPL
 For j = 1 To NPARAM
 x(j, n) = Selection.Cells(n, j)
 Next
 Next

 ' -- 解析変数ごとの平均値計算 --
 For j = 1 To NPARAM
 SUM = 0
 For n = 1 To NSAMPL
 SUM = SUM + x(j, n)
 Next
 ave(j) = SUM / DSAMPL
 Next

 ' -- 偏差の計算 --
 For j = 1 To NPARAM
 For n = 1 To NSAMPL
 x(j, n) = x(j, n) - ave(j)
 Next
 Next

 ' -- 共分散行列の計算 --
 For j = 1 To NPARAM
 For i = 1 To NPARAM
 SUM = 0
 For n = 1 To NSAMPL
 SUM = SUM + x(i, n) * x(j, n)
 Next
 cov(i, j) = SUM / (DSAMPL - 1)
 Next
 Next

 '-- ワークシート上への表示（１） データ数・平均 --
 Worksheets("Sheet3").Select
 Cells(1, 1) = "サンプル数"
 Cells(2, 1) = "変数の数"
 Cells(1, 2) = NSAMPL
 Cells(2, 2) = NPARAM
 For j = 1 To NPARAM
 Cells(3 + j, 1) = "平均値（" & CStr(j) & "）"
 Cells(3 + j, 2).NumberFormatLocal = "#.##" ' 実数は小数点以下 2 桁で表示
 Cells(3 + j, 2) = ave(j)
 Next

 '-- ワークシート上への表示（２）共分散行列 --
 Range(Cells(1, 4), Cells(1, 7)).Merge True
 Range(Cells(1, 4), Cells(1, 7)) = "共分散行列"
 For j = 1 To NPARAM
 Cells(2, 4 + j) = "変数（" & CStr(j) & "）"
 Next
 For i = 1 To NPARAM
 Cells(2 + i, 4) = "変数（" & CStr(i) & "）"
 Next
 For j = 1 To NPARAM
 For i = 1 To NPARAM
 Cells(2 + i, 4 + j).NumberFormatLocal = "#.##"
 Cells(2 + i, 4 + j) = cov(i, j)
 Next
 Next

 Dim EV0(), EV1(), EVEC(), LMD() As Double
 ReDim EV0(NPARAM), EV1(NPARAM), EVEC(NPARAM, NPARAM), LMD(NPARAM)
 
 '-- 固有値・固有ベクトルの解法（べき乗法）--
    For l = 1 To NPARAM '主成分番号カウント
   
       
        For i = 1 To NPARAM  '固有ベクトル初期値設定
            EV0(i) = 0
        Next
        EV0(1) = 1

        For lp = 1 To 20 'べき乗法計算　(k:計算繰り返し数)
        
            ' 固有ベクトルの演算
            For i = 1 To NPARAM
                SUM = 0
                For j = 1 To NPARAM
                    SUM = SUM + cov(i, j) * EV0(j)
                Next
                EV1(i) = SUM
            Next
            
            ' 固有ベクトルの企画化（単位ベクトル化）
            SUM = 0
            For i = 1 To NPARAM
                SUM = SUM + EV1(i) * EV1(i)
            Next
            For i = 1 To NPARAM
                EV0(i) = EV1(i) / Sqr(SUM)
            Next
        
        Next
    
        ' 固有値計算
        For i = 1 To NPARAM
            EVEC(i, l) = EV0(i)
        Next
        SUM = 0: TMP = 0:
        For i = 1 To NPARAM
            SUM = SUM + EV0(i) * EV1(i)
            TMP = TMP + EV0(i) * EV0(i)
        Next
        LMD(l) = SUM / TMP
        
        ' 次の主成分計算用の分散行列の算出
        For i = 1 To NPARAM
            For j = 1 To NPARAM
                cov(i, j) = cov(i, j) - LMD(l) * EVEC(i, l) * EVEC(j, l)
            Next
        Next
    
    Next

    Cells(7, 4).Value = "主成分分析"
    For l = 1 To NPARAM
        Cells(5 + 3 * l, 4).Value = "第" & l & "主成分"
        Cells(5 + 3 * l, 5).Value = "固有値"
        Cells(5 + 3 * l, 6).Value = LMD(l)
        Cells(5 + 3 * l, 7).Value = 100# * LMD(l) / (LMD(1) + LMD(2) + LMD(3)) & "%"
        
        Cells(6 + 3 * l, 5).Value = "固有ベクトル"
        For i = 1 To NPARAM
            Cells(6 + 3 * l, 5 + i).Value = EVEC(i, l)
        Next
    Next
    
    Dim r As Integer, c As Integer, t As Integer
    
    Cells(17, 4).Value = "主成分得点"
    For r = 1 To NSAMPL
        For c = 1 To NPARAM
            Dim v As Double: v = 0#
            For t = 1 To NPARAM
                v = v + x(t, r) * EVEC(t, c)
            Next
            Cells(17 + r, 3 + c).Value = v
        Next
    Next
    
    ' -- セルの幅の自動調節 （最後に行う）--
    Range("A:Z").EntireColumn.AutoFit

End Sub
