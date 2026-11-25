#Include "windows.bi"
#Define __FB_WIN64__
#If Defined (__FB_WIN64__) 
#LibPath "C:\msys64\mingw64\lib"
#Else
#LibPath "/usr/lib"
#EndIf
#Define EXTCHAR Chr(255)
#Include "fbgfx.bi" ' se carga antes de windows.bi para evitar duplicates..o conflictos
#Include Once "win/mmsystem.bi" '' FUNCIONES MIDIde windows!!!! perousaremos RtmidiC por hora
#If __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf
#Include Once "crt.bi"
'------------------------------------
#Include "window9.bi"

dim Shared As String ROLLDIR
Dim  As Integer lp=InStrRev(Command(0),"\")
ROLLDIR =Mid(Command(0),1,lp) ' tiene la barra ES DONDE ESTA ROLLMUSIC.EXE

Var prr=Open_Process(Create_Process(ROLLDIR +"RollMusic.exe",,ABOVE_NORMAL_PRIORITY_CLASS) )


WaitLoadProcess(prr)

''''KillProcess(prr,22)
WaitExitProcess(prr)
? GetExitCode(prr)


'''Sleep(2000)


CloseHandle(prr)
