projectName=$1
clear
if [ -z $projectName ]; then
    echo "Please input the project name"
    exit 1
fi
if [ -f $projectName ]; then
    rm $projectName
fi

# concat projectName with .prj
prjName=$projectName".prj"
srcfile=$projectName".icl"

if [ ! -f $prjName ]; then
    ./bin/cpm project $projectName create
fi

if [ -f $srcfile ] && [ $(cat $srcfile | wc -l) == 0 ]; then
    echo "" >>$srcfile
    echo "import StdEnv" >>$srcfile
    echo "" >>$srcfile
    echo "Start = \"Hello World\"" >>$srcfile
fi

log=$(./bin/cpm project $prjName build)
log=$(echo "$log" | sed '/Analyzing/d')
log=$(echo "$log" | sed '/Bring up to date/d')
log=$(echo "$log" | sed '/Warning:/d')
log=$(echo "$log" | sed '/Generating/d')
log=$(echo "$log" | sed '/Compiling/d')
log=$(echo "$log" | sed '/Linking/d')
log=$(echo "$log" | sed '/First found at/d')
log=$(echo "$log" | sed '/Also found at/d')

if [ -f $projectName ]; then
    echo '-----------------'
    echo -e "\033[32m✅Build success\033[0m"
    echo -e "\033[32m----------------------------------------\033[0m"
    echo -e "\033[32m$log\033[0m"
    ./$projectName
    echo
    echo

else

    # show Something went wrong please check the log in red

    echo -e "\033[31m❌Build failed\033[0m"
    echo -e "\033[31m----------------------------------------\033[0m"
    echo -e "\033[31m$log\033[0m"
fi

# write a batch version of that script
# Path: build.bat
# @echo off
# setlocal
# set projectName=%1
# if "%projectName%"=="" (
#     echo Please input the project name
#     exit /b 1
# )
# if exist %projectName% (
#     del %projectName%
# )
# set prjName=%projectName%.prj
# set srcfile=%projectName%.icl
# if not exist %prjName% (
#     bin\cpm project %projectName% create
# )
# if exist %srcfile% (
#     if not %srcfile%==0 (
#         echo. >> %srcfile%
#         echo Start = "Hello World" >> %srcfile%
#     )
# )
# set log=%~dp0bin\cpm project %prjName% build
# set log=%log:"=%"
# set log=%log:~0,-1%
# set log=%log:Analyzing=%
# set log=%log:Bring up to date=%
# set log=%log:Warning:=%
# set log=%log:Generating=%
# set log=%log:Compiling=%
# set log=%log:Linking=%
# set log=%log:First found at=%
# set log=%log:Also found at=%
# if exist %projectName% (
#     echo -----------------
#     echo ✅Build success
#     echo ----------------------------------------
#     echo %log%
#     %projectName%
#     echo.
#     echo.
# ) else (
#     echo ❌Build failed
#     echo ----------------------------------------
#     echo %log%
# )
# endlocal
