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
set nice_date=%year%-%month%-%day%

rem --------------------------

call clean_za
cd ..
cd ..

set ver=%1
if "%1"=="" echo *** No revision number given, putting XXX
if "%1"=="" set ver=XXX

set root=zip-ada

set files=%root%/z*.txt %root%/appnote.txt %root%/za*.xls %root%/*.gpr %root%/*.prj %root%/*.pra %root%/*_za.cmd %root%/make_one.cmd
set files=%files% %root%/zip_lib/*.ad*
set files=%files% %root%/test/*.ad* %root%/test/za_gcov.cmd %root%/test/test*.cmd %root%/test/bench*.cmd %root%/test/prof.cmd
set files=%files% %root%/demo/*.ad* 
set files=%files% %root%/doc/*.txt
set files=%files% %root%/tools/*.ad* %root%/tools/rez*.cmd %root%/tools/verif.* %root%/tools/adactl*.cmd %root%/tools/save.cmd %root%/tools/clean*.cmd
set files=%files% %root%/extras/*.ad* %root%/extras/*.a %root%/extras/*.rc %root%/extras/*.rbj %root%/extras/*.ico %root%/extras/*.pl %root%/extras/w*.cmd

zipada -ep2 za_%nice_date%_%ver%.zip %files%

cd %root%/tools