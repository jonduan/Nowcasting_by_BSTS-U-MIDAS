@echo off
setlocal EnableDelayedExpansion
set "search1=issn"
set "replace1=NOOPissn"
set "search2=url"
set "replace2=NOOPurl"
set "search3=doi"
set "replace3=NOOPdoi"
set "search4=isbn"
set "replace4=NOOPisbn"
set "search5=arxivId"
set "replace5=NOOParxivId"
set "textfile=UvicThesis.bib"
set "newfile=myUvicThesis.bib"
if exist "%newfile%" del "%newfile%"
for /F "usebackq delims=" %%L in ("%textfile%") do (
set "Line=%%L"
set "Line=!Line:%search1%=%replace1%!"
set "Line=!Line:%search2%=%replace2%!"
set "Line=!Line:%search3%=%replace3%!"
set "Line=!Line:%search4%=%replace4%!"
set "Line=!Line:%search5%=%replace5%!"
if not "!Line: =!"=="" echo !Line!>>"%newfile%"
)
endlocal 