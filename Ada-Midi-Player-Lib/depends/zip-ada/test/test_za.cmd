copy /b ..\zipada.exe      .
copy /b ..\comp_zip.exe    .
copy /b ..\random_data.exe .

@echo off
echo.
echo  ==================================================
echo       Testing the ZipAda tool with Comp_Zip
echo  ==================================================
echo.

if exist test_za??.zip del test_za??.zip
if exist test_iz??.zip del test_iz??.zip
if exist test_kzip.zip del test_kzip.zip
if exist test_7z_?.zip del test_7z_?.zip
if exist test_zopf.zip del test_zopf.zip

rem Have a badly compressible file (random.bin), a bit more than 2**n because of rare repetitions
if not exist rnd_1.bin for /l %%i in (0,10,100) do random_data %%i rnd_%%i.bin
if not exist random.bin random_data 8300
if not exist random_and_text.mix copy /b random.bin+*.txt random_and_text.mix
if exist test_rz.ReZip.html del test_rz.ReZip.html

set files=*.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.bin
if exist *.ldf set files=%files% *.ldf
if exist *.mdf set files=%files% *.mdf
if exist *.mdb set files=%files% *.mdb
if exist *.sql set files=%files% *.sql

zipada     -esh              test_zash     %files%
zipada     -edf              test_zadf     %files%
zipada     -ed1              test_zad1     %files%
zipada     -ed2              test_zad2     %files%
zipada     -ed3              test_zad3     %files%
zipada     -el1              test_zal1     %files%
zipada     -el2              test_zal2     %files%
zipada     -el3              test_zal3     %files%
zipada     -ep1              test_zap1     %files%
zipada     -ep2              test_zap2     %files%
zip        -1                test_izd1     %files%
zip        -6                test_izd6     %files%
zip        -9                test_izd9     %files%
zip        -9 -Z bzip2       test_izb9     %files%
7z a -tzip -mx=9 -mm=deflate test_7z_d     %files%
7z a -tzip -mx=9 -mm=LZMA    test_7z_l     %files%

if not "%1" == "full" goto skip
echo  --
echo  --  If you need a coffee now, it's the right time to have one or two...
echo  --
zipada     -er1              test_zar1     %files%
zipada     -er2              test_zar2     %files%
zipada     -er3              test_zar3     %files%
zipada     -er4              test_zar4     %files%
kzip                         test_kzip     %files%
advzip -a -4                 test_zopf.zip %files%
:skip

rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------
set year=%date:~-4,4%
set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%
set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%
set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%
set min=%time:~3,2%
set nice_date=%year%-%month%-%day%_%hour%.%min%
rem --------------------------

dir /OS test_za??.zip test_iz??.zip test_kzip.zip test_7z_?.zip test_zopf.zip |find ".zip" >test_za_%nice_date%.log
ren Zip.Compress.Deflate.zcd Zip.Compress.Deflate.%nice_date%.zcd

echo.
if exist test_zar1.zip comp_zip test_zash test_zar1 -q2
if exist test_zar2.zip comp_zip test_zash test_zar2 -q2
if exist test_zar3.zip comp_zip test_zash test_zar3 -q2
if exist test_zar4.zip comp_zip test_zash test_zar4 -q2
comp_zip test_zash test_zadf -q2
comp_zip test_zash test_zad1 -q2
comp_zip test_zash test_zad2 -q2
comp_zip test_zash test_zad3 -q2
comp_zip test_zash test_zal1 -q2
comp_zip test_zash test_zal2 -q2
comp_zip test_zash test_zal3 -q2
comp_zip test_zash test_zap1 -q2
comp_zip test_zash test_zap2 -q2
comp_zip test_zash test_izd9 -q2
comp_zip test_zash test_izb9 -q2
comp_zip test_zash test_7z_d -q2
comp_zip test_zash test_7z_l -q2
if exist test_kzip.zip comp_zip test_zash test_kzip -q2
if exist test_zopf.zip comp_zip test_zash test_zopf -q2

pause
