@echo off

rem set LIBS=-ILIBS/
set LIBS=

if "%1" == "" goto buildlist
if "%EXT%" == "" set EXT=COM

if exist %1.%EXT% del %1.%EXT% >nul
echo Building %1...
nasm %1.ASM %LIBS% -fbin -O9 -o %1.%EXT%
if not exist %1.%EXT% goto end

if not exist ..\..\BIN\NUL mkdir ..\..\BIN

if not exist ..\..\BIN\%1.%EXT% goto NoOldVer

:NoOldVer
copy /y %1.%EXT% ..\..\BIN\ >NUL
dir ..\..\BIN\%1.%EXT%
goto end

:buildlist

rem set EXT=SYS
CALL %0 LOGGER


:end