PROGRAM MotorControl
VAR
  StartButton, StopButton, OverloadRelay : BOOL;
  MotorRunning, AlarmLight : BOOL;
END_VAR

NETWORK 1: Motor Start/Stop Circuit
|--| |--StartButton--+--| |--MotorRunning--|( )--MotorRunning--|
|                    |
|                    +--|/|--StopButton--|
|                    |
|                    +--|/|--OverloadRelay--|
|

NETWORK 2: Alarm Light Control
|--|/|--OverloadRelay--|(  )--AlarmLight--|
|