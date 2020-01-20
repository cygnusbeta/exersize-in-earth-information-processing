cd %~dp0
copy /Y ..\vba_report2\vba2.xlsm .\bin\
cscript vbac.wsf decombine
move /Y .\src\vba2.xlsm\Module1.bas ..\Module1.vb
move /Y .\src\vba2.xlsm\Module2.bas ..\Module2.vb
del .\bin\vba2.xlsm

