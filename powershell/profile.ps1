$Shell = $Host.UI.RawUI
# Window size
$max_size=$shell.MaxPhysicalWindowSize
$shell.BufferSize.Width=$max_size.Width
$shell.BufferSize.Height=9999
$shell.WindowSize.Width=$max_size.Width
$shell.WindowSize.Height=$max_size.Height
# Background
#$shell.BackgroundColor = "White"
#$shell.ForegroundColor = "Black"
#Refresh
Clear-Host
# Path
$env:Path = "$profile\..\scripts;" + $env:Path
# Aliases
Set-Alias lal ls
set-alias size getfoldersize.ps1
set-alias uninstall uninstall_WPA_series.ps1
set-alias AttendanceBookEmail send_attendance_book_by_email.ps1
set-alias search search.ps1
# Excel
function excel-start { $excel = new-object -com Excel.Application; $excel.visible = $true; $excel; }
function excel-invoke { $excel.workbooks.psbase.gettype().invokemember($args[1],[Reflection.BindingFlags]::InvokeMethod,$null,$args[0],$args[2],$null); }
function excel-getfield { $excel.workbooks.psbase.gettype().invokemember($args[1],[Reflection.BindingFlags]::GetField,$null,$args[0],$args[2],$null); }
function excel-open { $book = (excel-invoke $excel.workbooks Open $args[0]); $book; }
function excel-close { $res = excel-invoke $excel.workbooks Close; $res; }
function excel-closebook { $res = excel-invoke $args[0] Close; $res; }
function excel-selectsheet { $res = excel-invoke $excel.worksheets.Item($args[0]) Select; $res; }
function excel-getsheet { $sheet = $excel.worksheets.Item($args[0]); $sheet; }
# Load HKCR
New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT