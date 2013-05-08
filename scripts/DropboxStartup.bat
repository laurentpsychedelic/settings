@echo off
rem Every second, check to see if volume is mounted
echo Waiting for volume...
:keepwaiting
ping -n 1 -w 1000 127.0.0.1 > nul
if not exist T:\ goto keepwaiting
start "Dropbox" "C:\Documents and Settings\Laurent_dev\Application Data\Dropbox\bin\Dropbox.exe"