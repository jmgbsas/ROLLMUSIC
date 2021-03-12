#If Defined(__FB_WIN32__)
   #LibPath "C:\msys64\mingw64\lib"
   #Else
   #LibPath "/usr/lib"
   #EndIf
#define EXTCHAR Chr(255)
#Include "fbgfx.bi"
#Include "windows.bi" ' en winuser.bi esta el mouse o screen event 
#Include Once "win/mmsystem.bi" '' FUNCIONES MIDIde windows!!!! perousaremos RtmidiC por hora
#If __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf

' Main ¿?
' desde aca llamo a RollMusic y vuelvo cuando quiero

'A Windows based example but the same idea applies to Linux


Dim As string  exename = "RollMusic.exe"
Dim As String  desde = "5"
Dim As String  hasta = "6"
Dim direp as ZString Ptr  
Dim num As Integer 
Dim hola As String = "hola desde main"
direp = StrPtr(hola)
''Const cmdline "5 6"
Common Shared mensaje As String 
mensaje = "hola jose"

num = @direp

'Print (*pmt).a  '' or Print pmt->a
Dim As String cmdline =>  desde + " "+ hasta + " " + Str(num) 'Str(cptr(integer, direp))

' Run( cmdline )
'Shell ("start RollMusic 5 6")

'Print "termine estoy al pedo aleluya"
'sleep
'If result = -1 Then
'    Print "Error running "; exename
'Else
'    Print "Exit code:"; result
'End If

'dim argv as ZSTRING PTR PTR = __FB_ARGV__
'for i as integer = 0 to __FB_ARGC__ -1
'    print *argv[i]
'next i

Run ( exename, cmdline)
END



' no solo llama sin 