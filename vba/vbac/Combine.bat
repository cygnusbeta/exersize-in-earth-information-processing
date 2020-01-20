cd %~dp0
copy /Y ..\code.vb .\src\vba.xlsm\Module1.bas
copy /Y ..\vba.xlsm .\bin\
cscript vbac.wsf combine
move /Y .\bin\vba.xlsm ..\
del .\src\vba.xlsm\Module1.bas
