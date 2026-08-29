@echo off
rem ---------------------------------------------------------------------------
rem Builds and runs the Free Pascal test program.
rem
rem Needs fpc on the PATH, or a Lazarus installation in the usual place.
rem
rem Usage:  build-and-run.cmd
rem ---------------------------------------------------------------------------
setlocal

set "HERE=%~dp0"
set "REPO=%HERE%..\.."
set "OUT=%HERE%out"

set "FPC="
for /f "delims=" %%F in ('where fpc 2^>nul') do if not defined FPC set "FPC=%%F"

if not defined FPC (
  for /f "delims=" %%D in ('dir /b /o-n "C:\lazarus\fpc" 2^>nul') do (
    if not defined FPC if exist "C:\lazarus\fpc\%%D\bin\x86_64-win64\fpc.exe" (
      set "FPC=C:\lazarus\fpc\%%D\bin\x86_64-win64\fpc.exe"
    )
  )
)

if not defined FPC (
  echo(
  echo ERROR: could not find fpc.exe. Put it on the PATH and try again.
  exit /b 1
)

if not exist "%OUT%" mkdir "%OUT%"

echo Using %FPC%
"%FPC%" -Mdelphi -Fu"%REPO%" -Fu"%HERE%" -FU"%OUT%" -FE"%OUT%" "%HERE%FpcAutoMapperTests.lpr"
if errorlevel 1 (
  echo(
  echo ERROR: compilation failed.
  exit /b 1
)

echo(
rem --format=plain gives readable output; the default is NUnit XML.
"%OUT%\FpcAutoMapperTests.exe" --all --format=plain
exit /b %errorlevel%
