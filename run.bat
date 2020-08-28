@echo off

rem SET ORICUTRON="..\..\..\oricutron-iss2-debug-stratoric\"
rem SET ORICUTRON="..\..\..\oricutron-iss3\"
SET ORICUTRON="..\..\..\oricutron\"
SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%

SET ROM=sedsd
rem -DWITH_SDCARD_FOR_ROOT=1 
rem 

rem xa.exe src/stratoric.asm -o stratbuild.rom

rem md5sum B7STRA40.ROM
rem md5sum stratbuild.rom

ca65.exe --cpu 6502 -ttelestrat --include-dir %CC65%\asminc\ src/stratoric.asm -DORIX=1 -o stratbuild.ld65
%CC65%\ld65.exe -tnone   stratbuild.ld65 -o stratbuild.rom



rem %CC65%\ca65.exe -DWITH_SDCARD_FOR_ROOT=1 -ttelestrat --include-dir %CC65%\asminc\ src/%ROM%2.asm -o %ROM%.ld65  --debug-info --verbose
rem %CC65%\ld65.exe -DWITH_SDCARD_FOR_ROOT=1 -tnone  %ROM%.ld65 -o %ROM%.rom  -Ln shell.sym



IF "%1"=="NORUN" GOTO End

copy %ROM%.rom %ORICUTRON%\roms\ > NUL
copy stratbuild.rom %ORICUTRON%\roms\
cd %ORICUTRON%
oricutron -mt  --symbols "%ORIGIN_PATH%\xa_labels_orix.txt"

:End
cd %ORIGIN_PATH%
rem %OSDK%\bin\MemMap "%ORIGIN_PATH%\xa_labels_orix.txt" memmap.html O docs/telemon.css

