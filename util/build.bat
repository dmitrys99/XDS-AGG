@echo off
set folder=%CD%
cls
cd ..
call build.bat %1
cd %folder%