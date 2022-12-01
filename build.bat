@echo off
@REM By: Youssef ELMOUMEN @ ELTE -2022
setlocal
set projectName=%1
if "%projectName%"=="" (
    echo Please input the project name
    exit /b 1
)
if exist %projectName%.exe (
    del %projectName%.exe
)
set prjName=%projectName%.prj
set srcfile=%projectName%.icl

if not exist %prjName% (
    .\cpm.exe project %projectName% create
)
@REM if exist %srcfile% (
@REM     if not %srcfile%==0 (
@REM         echo. >> %srcfile%
@REM         echo Start = "Hello World" >> %srcfile%
@REM     )
@REM )


.\cpm.exe project %prjName% build > temp.txt
findstr /V /R "Analyzing.*" temp.txt > success.txt

if exist %projectName%.exe (
    echo [32m
    echo O SUCCESSFUL BUILD
    more success.txt
    echo ----------------------------------------
    
    .\%projectName%
    echo.
    echo.
    echo [0m
) else (
    echo [31m
    echo X BUILD FAILED
    echo ----------------------------------------
    more temp.txt
    echo [0m
)
endlocal