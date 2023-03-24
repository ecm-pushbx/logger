@echo off

rem set LIBS=-ILIBS/
set LIBS=

if "%1" == "" goto buildlist
set OUTNAME=%2
if "%OUTNAME%" == "" set OUTNAME=%1.COM


if exist %OUTNAME% del %OUTNAME% >nul
echo Building %OUTNAME%...
nasm %1.ASM %LIBS% -fbin -O9 -o %OUTNAME%
if not exist %OUTNAME% goto end

if not exist ..\..\BIN\NUL mkdir ..\..\BIN

if not exist ..\..\BIN\%OUTNAME% goto NoOldVer

:NoOldVer
copy /y %OUTNAME% ..\..\BIN\ >NUL
dir ..\..\BIN\%OUTNAME%
goto end

:buildlist

CALL %0 LOG-COM LOGGER.COM
CALL %0 LOG-SYS LOGGER.SYS

:end