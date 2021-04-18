 
#Include once  "file.bi"
#include "mod_rtmidi_c.bi"
#Inclib  "rtmidi.dll"

dim Shared midiin As   RtMidiInPtr 
dim Shared midiout As  RtMidiOutPtr
Declare Sub noteon	( note As UByte, vel As UByte,canal As integer) 
Declare sub noteoff( note As UByte, canal As integer)
Declare Sub note2on	( note1 As UByte, note2 As UByte, vel As UByte,canal As integer)
Declare Sub note2off	( note1 As UByte, note2 As UByte, canal As integer)
Declare Sub note3on	( note1 As UByte, note2 As UByte, note3 As UByte, vel As UByte,canal As integer)
Declare Sub note3off	( note1 As UByte, note2 As UByte, note3 As UByte, canal As integer)
Declare Sub note4on	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, vel As UByte,canal As integer)
Declare Sub note4off	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, canal As integer)
Declare Sub note5on	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, note5 As UByte, vel As UByte, canal As integer)
Declare Sub note5off	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte,note5 As UByte, canal As integer)
Declare Function restar (notaRoll As Integer) As Integer
Declare Sub PlayRoll ()
Declare Sub duracion (dura As Integer)

Dim Shared message(1 To 21) As UByte ' message output 
' maximo seria para un acorde de 5 por ejemplo
' 5 notas + velocidad y canal = 7 bytes...para note on
' si tomamos 10 dedos de las 2 manos serian 10 notas+ vel + canal = 12
' si agregamos velocidad distinta para cada nota serian
' 20 mas canal = 21 bytes...maximo 
' podriamos agregar presion promedio aftertouch etc pero serin mensajes independientes 
Dim errorString As ZString Ptr
Dim Shared p as UBYTE Ptr = @message(1) 
Dim size As UInteger<64> 
Dim sizeptr As UInteger<64> Ptr = @size
Dim Shared As UInteger portsin, portsout 
Dim Shared As Double tiempoPatron=60 ' cuantas negras enun minuto
Dim Shared As Double old_time=0
'elpatron esla negra ej I=60ergo todo sera relativo  la negra
Dim Shared As float relDur (1 To 72) => { _ 
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375,_
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375,_
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375,_
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375}

Dim Shared As Integer play =0 
''On Error Goto errorhandler
