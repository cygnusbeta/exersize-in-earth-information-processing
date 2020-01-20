Attribute VB_Name = "Module1"
Sub pca()
 ' -- �v���O�������̕ϐ��̒�` --
 Dim NPARAM As Integer ' �ϐ��̐� �i�񐔁j
 Dim NSAMPL As Integer ' �T���v�����i�s���j
 Dim x() As Double ' �ϑ��f�[�^�i�z��f�[�^�j
 Dim cov() As Double ' �����U�s��i�z��f�[�^�j
 Dim ave() As Double ' �ϐ����Ƃ̕��ϒl�i�z��f�[�^�j
 Dim SUM As Double ' ���v�l
 Dim DSAMPL As Double ' �T���v�����i�����j
 Dim i As Integer
 Dim j As Integer
 Dim n As Integer
 ' -- �I��̈�̃f�[�^���͂���ϐ��̐� --
 NPARAM = Selection.Columns.Count
 NSAMPL = Selection.Rows.Count
 DSAMPL = CDbl(NSAMPL) ' �����ł���`
 If NPARAM = 1 And NSAMPL = 1 Then
 MsgBox "�G���[�F��͂���f�[�^�̈��I�����ă}�N�������s���Ă�������"
 Exit Sub
 End If
 ' -- �z��̏��� --
 ReDim x(NPARAM, NSAMPL), cov(NPARAM, NPARAM), ave(NPARAM)

 ' -- �f�[�^�̓ǂݍ��� --
 For n = 1 To NSAMPL
 For j = 1 To NPARAM
 x(j, n) = Selection.Cells(n, j)
 Next
 Next

 ' -- ��͕ϐ����Ƃ̕��ϒl�v�Z --
 For j = 1 To NPARAM
 SUM = 0
 For n = 1 To NSAMPL
 SUM = SUM + x(j, n)
 Next
 ave(j) = SUM / DSAMPL
 Next

 ' -- �΍��̌v�Z --
 For j = 1 To NPARAM
 For n = 1 To NSAMPL
 x(j, n) = x(j, n) - ave(j)
 Next
 Next

 ' -- �����U�s��̌v�Z --
 For j = 1 To NPARAM
 For i = 1 To NPARAM
 SUM = 0
 For n = 1 To NSAMPL
 SUM = SUM + x(i, n) * x(j, n)
 Next
 cov(i, j) = SUM / (DSAMPL - 1)
 Next
 Next

 '-- ���[�N�V�[�g��ւ̕\���i�P�j �f�[�^���E���� --
 Worksheets("Sheet3").Select
 Cells(1, 1) = "�T���v����"
 Cells(2, 1) = "�ϐ��̐�"
 Cells(1, 2) = NSAMPL
 Cells(2, 2) = NPARAM
 For j = 1 To NPARAM
 Cells(3 + j, 1) = "���ϒl�i" & CStr(j) & "�j"
 Cells(3 + j, 2).NumberFormatLocal = "#.##" ' �����͏����_�ȉ� 2 ���ŕ\��
 Cells(3 + j, 2) = ave(j)
 Next

 '-- ���[�N�V�[�g��ւ̕\���i�Q�j�����U�s�� --
 Range(Cells(1, 4), Cells(1, 7)).Merge True
 Range(Cells(1, 4), Cells(1, 7)) = "�����U�s��"
 For j = 1 To NPARAM
 Cells(2, 4 + j) = "�ϐ��i" & CStr(j) & "�j"
 Next
 For i = 1 To NPARAM
 Cells(2 + i, 4) = "�ϐ��i" & CStr(i) & "�j"
 Next
 For j = 1 To NPARAM
 For i = 1 To NPARAM
 Cells(2 + i, 4 + j).NumberFormatLocal = "#.##"
 Cells(2 + i, 4 + j) = cov(i, j)
 Next
 Next

 Dim EV0(), EV1(), EVEC(), LMD() As Double
 ReDim EV0(NPARAM), EV1(NPARAM), EVEC(NPARAM, NPARAM), LMD(NPARAM)
 
 '-- �ŗL�l�E�ŗL�x�N�g���̉�@�i�ׂ���@�j--
    For l = 1 To NPARAM '�听���ԍ��J�E���g
   
       
        For i = 1 To NPARAM  '�ŗL�x�N�g�������l�ݒ�
            EV0(i) = 0
        Next
        EV0(1) = 1

        For lp = 1 To 20 '�ׂ���@�v�Z�@(k:�v�Z�J��Ԃ���)
        
            ' �ŗL�x�N�g���̉��Z
            For i = 1 To NPARAM
                SUM = 0
                For j = 1 To NPARAM
                    SUM = SUM + cov(i, j) * EV0(j)
                Next
                EV1(i) = SUM
            Next
            
            ' �ŗL�x�N�g���̊�扻�i�P�ʃx�N�g�����j
            SUM = 0
            For i = 1 To NPARAM
                SUM = SUM + EV1(i) * EV1(i)
            Next
            For i = 1 To NPARAM
                EV0(i) = EV1(i) / Sqr(SUM)
            Next
        
        Next
    
        ' �ŗL�l�v�Z
        For i = 1 To NPARAM
            EVEC(i, l) = EV0(i)
        Next
        SUM = 0: TMP = 0:
        For i = 1 To NPARAM
            SUM = SUM + EV0(i) * EV1(i)
            TMP = TMP + EV0(i) * EV0(i)
        Next
        LMD(l) = SUM / TMP
        
        ' ���̎听���v�Z�p�̕��U�s��̎Z�o
        For i = 1 To NPARAM
            For j = 1 To NPARAM
                cov(i, j) = cov(i, j) - LMD(l) * EVEC(i, l) * EVEC(j, l)
            Next
        Next
    
    Next

    Cells(7, 4).Value = "�听������"
    For l = 1 To NPARAM
        Cells(5 + 3 * l, 4).Value = "��" & l & "�听��"
        Cells(5 + 3 * l, 5).Value = "�ŗL�l"
        Cells(5 + 3 * l, 6).Value = LMD(l)
        Cells(5 + 3 * l, 7).Value = 100# * LMD(l) / (LMD(1) + LMD(2) + LMD(3)) & "%"
        
        Cells(6 + 3 * l, 5).Value = "�ŗL�x�N�g��"
        For i = 1 To NPARAM
            Cells(6 + 3 * l, 5 + i).Value = EVEC(i, l)
        Next
    Next
    
    Dim r As Integer, c As Integer, t As Integer
    
    Cells(17, 4).Value = "�听�����_"
    For r = 1 To NSAMPL
        For c = 1 To NPARAM
            Dim v As Double: v = 0#
            For t = 1 To NPARAM
                v = v + x(t, r) * EVEC(t, c)
            Next
            Cells(17 + r, 3 + c).Value = v
        Next
    Next
    
    ' -- �Z���̕��̎������� �i�Ō�ɍs���j--
    Range("A:Z").EntireColumn.AutoFit

End Sub
