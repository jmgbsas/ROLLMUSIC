 
#Include once  "file.bi"
'#include "mod_rtmidi_c.bi"
''#Inclib  "rtmidi.dll" 'usa librerias estaticas 
'#Inclib  "rtmidi"  '''uso al dedeisco rtmidi.dll
'#include "fbthread.bi"
'#Include "crt.bi" ' QSORT


Declare Function restar (notaRoll As Integer) As Integer
Declare Sub PlayRoll () 
'declare Sub playAll() 
Declare Sub duracion (old_time As double,tiempoFigura As double)
Declare Sub pedaloff(portsal As ubyte)
Declare Sub allSoundoff(canal As UByte,portsal As ubyte)
Declare Sub alloff(canal As UByte,portsal As UByte)
Declare Sub listports()
Declare Sub sleep5dm()
Declare Function sumar( ByVal ind As integer) As Integer
'Declare Sub trasponerRoll(cant As integer)
'Declare Sub trasponerGrupo(cant As integer)
'Declare Sub moverZonaRoll(cant As Integer)
'Declare Sub correcciondeNotas()

Dim Shared message(1 To 21) As UByte ' message output ' puede ser de hasta 1024 bytes 
Dim Shared msgin  (1 To 8192) As UByte ' message in futuro secuencia veremos si ahce falta 
' maximo seria para un acorde de 5 por ejemplo
' 5 notas + velocidad y canal = 7 bytes...para note on
' si tomamos 10 dedos de las 2 manos serian 10 notas+ vel + canal = 12
' si agregamos velocidad distinta para cada nota serian
' 20 mas canal = 21 bytes...maximo 
' podriamos agregar presion promedio aftertouch etc pero serin mensajes independientes 
'Dim errorString As ZString Ptr
Dim Shared p as UBYTE Ptr = @message(1) 
Dim size As UInteger<64> 
'Dim sizeptr As UInteger<64> Ptr = @size
Dim Shared As UInteger portsin=0, portsout =0
Dim Shared As integer tiempoPatron=60 ' cuantas negras en un minuto default
Dim Shared As Double old_time_on=0,old_time_off=0,old_time_on_int=0,old_time_off_int=0
Static Shared As Integer jply=0, finplay=0,indEscala=1
Dim Shared As double FactortiempoPatron=1
'elpatron esla negra ej I=60 ergo todo sera relativo A la negra q dura 1 seg
' 09-06-2021 agregue 0 en relDur para evitar cancelaciones pero ojo puede 
' tapar otros errores qu el tiempo se calcule com ocero y siga y 
'se pierda el tiemp anterior supongamso enun espacio en blanco agregado 
' en una ligadura pero eso lo prohibimos por ahroa como uso y listo
Dim Shared As float relDur (0 To 182) => {0, _  
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 1 9 
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 10 18
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 19 27
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ '28 36
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.208333,0.01041666, _ '37 45
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 46 54
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 55 63
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ '  64 72
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ ' 73 81
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.208333,0.01041666, _ '82 90
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 91 99
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 100 108
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 109 117
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ ' 118 126
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.208333,0.01041666, _ ' 127 135
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 136 144
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 145 153
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 154 162
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ '163 171
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.0208333,0.01041666,0,0} '172 181

Dim Shared As Integer play =0,playb=0, portin, numero, numeroFrac,cambioescala=0
Dim Shared As UInteger portout=0 

Static Shared As string listout(), listin ()
Static Shared As integer listoutAbierto(), listinAbierto ()
Dim Shared As String *2  listCanal(1 To 16) => {" 1"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9","10","11","12","13","14","15","16"}


Dim Shared As String * 1 cifra 
Dim Shared As String  digito, digitoFrac
 
'----------------------------
' PLAY ALL NEW
Declare Function QCompare Cdecl (Byval e1 As Any Ptr, Byval e2 As Any Ptr) As Integer
Type Re   ' de relaciones
 ordRelDur As Integer ' orden de reldur ascendente
 relDur As Double ' durcion relativa a la negra
 Dur As Integer   ' nuemrcion de figura
 SonyLiga As Integer
End Type
'en vec debo agregar la nota piano
Type vec
   tiempoFigura    As Integer
   tiempoFiguraOld As Integer
   audio           As Integer ' 0 silencio, 1 o >0 suena
   audioOld        As Integer ' 0 silencio no audio > 0 suena old o anterior..
   DUR             As Integer 'DUR
   notapiano       As integer ' del piano real
   notapianoOld    As Integer
   liga            As Integer
   ligaold         As Integer
   i1              As Integer
   i1old           As Integer
   old_time        As Integer
   old_timeold     As Integer
   inst            As Integer
   canal           As Integer
   port            As integer 
   pista           As Integer
   vol             As integer
End Type
Static Shared pasoCol (0 To 384) As vec




Declare Sub noteon	( note As UByte, vel As UByte,canal As UByte,portsal As UByte)
Declare Sub noteSimple	( pasoCol() As vec, cntold As integer, vel As UByte,canal As UByte,tiempoDur As Double,velpos As integer)
'Declare Sub AcordeIguales ( pasoCol() As vec,cnt As UByte,cntold As UByte, vel As UByte,canal As UByte,tiempoDur As double) 
'Declare Sub AcordeOffIguales	( pasoCol() As vec, cnt As UByte,cntold As UByte, canal As UByte)
'Declare Sub AcordeDistintos ( pasoCol() As vec,cnt As UByte,cntold As UByte, vel As UByte, canal As UByte,tiempoDur As double) 
'Declare Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte,cntold As UByte, canal As UByte,tiempoDur As Double)
'Declare Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As Double)
'Declare Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As double)
Declare Function vol (dura As UByte, vel As UByte) As ubyte
Declare sub noteoff( note As UByte, canal As UByte,portsal As UByte )
Declare Sub limpiarLigaduras(cnt As UByte,pasoCol() As vec)
Dim Shared As Integer ligaglobal=0 ', ligaglobalc (1 To 32)
'Relacion de nR indice de Roll, con nE semitono, para evitar calculos.
Dim Shared As integer relnRnE(0 To 132) => { _  ' indice de Roll vs nota ..semitono
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1 }

Static Shared indiceaudio (0 To 384) As Integer 
Type PGE  ' parametros guia escala 
 tipoescala As Integer
 notaescala As Integer
 alteracion As Integer ' 3=sos, 2 bem
 posicion   As integer
End Type

Dim Shared guiaEscala  (1 To 100) As PGE ' suponemos  100 cambios de escala en una pista
' en la posicion 1 se cargar� la escala leida de la pista al inicio
' la escal debe guarar los 3 parametros Tipo escala , notaescala y alteracion en un principio basico
