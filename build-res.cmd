@echo off
rem ---------------------------------------------------------------------------
rem Compiles AutoMapperTester.rc into AutoMapperTester.res.
rem
rem The .res is a compiled binary and is excluded by .gitignore, so a clean
rem clone does not have one. The Delphi IDE and MSBuild both regenerate a
rem missing project .res automatically, so those builds need nothing extra.
rem A direct dcc32 build does not, and fails with:
rem
rem     Error: E1026 File not found: 'AutoMapperTester.res'
rem
rem Run this first in that case, or any time you want the resource rebuilt
rem from the checked-in .rc rather than from the IDE project options.
rem
rem Usage:  build-res.cmd
rem ---------------------------------------------------------------------------
setlocal

set "PROJDIR=%~dp0"
set "RCFILE=%PROJDIR%AutoMapperTester.rc"
set "RESFILE=%PROJDIR%AutoMapperTester.res"

rem brcc32 (the legacy Borland compiler) cannot handle the numeric RT_MANIFEST
rem type used by this script, so use Microsoft's rc.exe, which Delphi ships and
rem which its own build system uses.
set "RC="
if defined BDS if exist "%BDS%\bin\rc.exe" set "RC=%BDS%\bin\rc.exe"

if not defined RC (
  for /f "delims=" %%D in ('dir /b /o-n "%ProgramFiles(x86)%\Embarcadero\Studio" 2^>nul') do (
    if not defined RC if exist "%ProgramFiles(x86)%\Embarcadero\Studio\%%D\bin\rc.exe" (
      set "RC=%ProgramFiles(x86)%\Embarcadero\Studio\%%D\bin\rc.exe"
    )
  )
)

if not defined RC (
  echo(
  echo ERROR: could not locate rc.exe.
  echo Run this from a Delphi command prompt, or run rsvars.bat first so that
  echo the BDS environment variable points at your Delphi installation.
  exit /b 1
)

if not exist "%RCFILE%" (
  echo ERROR: %RCFILE% not found.
  exit /b 1
)

echo Compiling AutoMapperTester.rc
"%RC%" /fo "%RESFILE%" "%RCFILE%"
if errorlevel 1 (
  echo(
  echo ERROR: resource compilation failed.
  exit /b 1
)

echo Wrote %RESFILE%
endlocal
exit /b 0
