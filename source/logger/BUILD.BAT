@echo off

rem set LIBS=-ILIBS/
set LIBS=

if "%1" == "" goto buildlist
set OUTNAME=%2
if "%OUTNAME%" == "" set OUTNAME=%1.COM


if exist %OUTNAME% del %OUTNAME% >nul
echo Building %1...
nasm %1.ASM %LIBS% -fbin -O9 -o %OUTNAME%
if not exist %OUTNAME% goto end

if not exist ..\..\BIN\NUL mkdir ..\..\BIN

if not exist ..\..\BIN\%OUTNAME% goto NoOldVer

:NoOldVer
copy /y %OUTNAME% ..\..\BIN\ >NUL
dir ..\..\BIN\%OUTNAME%
goto end

:buildlist

CALL %0 LOG LOGGER.COM
CALL %0 LOGGER LOGGER.SYS

:end