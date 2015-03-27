xcopy /s /y ..\..\fgeom\src\* .
xcopy /s /y ..\src\* .
xcopy /s /y ..\examples\* .
xcopy /s /y ..\examples\Shapes\lwgl4.hs .
hastec lwgl4.hs -O2 
rem --opt-all
pause