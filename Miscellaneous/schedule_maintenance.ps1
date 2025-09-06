param(
  [ValidateSet('Hourly','Daily','AtLogOn')]
  [string]$Mode = 'Hourly',
  [int]$Hours = 4,
  [string]$DailyTime = '09:00',
  [string]$TaskName = 'MaintenanceRun',
  [string]$TaskDescription = 'Runs maintenance_run.bat to clean up and push changes',
  [string]$ScriptPath = ''
)

# Default to the maintenance_run.bat in the same folder as this script
if (-not $ScriptPath -or -not (Test-Path -LiteralPath $ScriptPath)) {
  $ScriptPath = Join-Path -Path $PSScriptRoot -ChildPath 'maintenance_run.bat'
}

if (-not (Test-Path -LiteralPath $ScriptPath)) {
  throw "Cannot find maintenance_run.bat at `"$ScriptPath`""
}

$action = New-ScheduledTaskAction -Execute 'cmd.exe' -Argument "/c `"$ScriptPath`""

switch ($Mode) {
  'Hourly' { 
    if ($Hours -lt 1) { throw "Hours must be >= 1" }
    $trigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddMinutes(1) -RepetitionInterval (New-TimeSpan -Hours $Hours) -RepetitionDuration ([TimeSpan]::MaxValue)
  }
  'Daily'  { 
    $time = [DateTime]::Parse($DailyTime)
    $trigger = New-ScheduledTaskTrigger -Daily -At $time.TimeOfDay
  }
  'AtLogOn' {
    $trigger = New-ScheduledTaskTrigger -AtLogOn
  }
}

$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Password -RunLevel Highest
$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable

# Remove if an old task exists
if (Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue) {
  Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
}

Register-ScheduledTask -TaskName $TaskName -Description $TaskDescription -Action $action -Trigger $trigger -Principal $principal -Settings $settings

Write-Host "Scheduled task '$TaskName' registered. Mode=$Mode; Hours=$Hours; DailyTime=$DailyTime; ScriptPath=$ScriptPath"
