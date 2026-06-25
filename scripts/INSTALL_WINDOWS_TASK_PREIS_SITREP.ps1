$ErrorActionPreference = 'Stop'
$TaskName = 'PREIS_Ebola_SitRep_Monitor'
$BatFile = 'D:\PREIS_Ebola_DRC_Sitrep_FV_12.06.26\scripts\run_preis_sitrep_once.bat'
$Action = New-ScheduledTaskAction -Execute 'cmd.exe' -Argument ('/c "' + $BatFile + '"')
$Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddMinutes(1) -RepetitionInterval (New-TimeSpan -Minutes 5) -RepetitionDuration (New-TimeSpan -Days 3650)
$Settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -WakeToRun -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -MultipleInstances IgnoreNew
Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Description 'PREIS Ebola DRC SitRep monitor every 5 minutes' -Force | Out-Null
Write-Output ('TASK_INSTALLED_OK: ' + $TaskName)
