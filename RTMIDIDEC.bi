 
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
Declare Sub CTRL1205 ()

common Shared message() As UByte 'cambiado a shred message output ' puede ser de hasta 1024 bytes
Dim message(1 To 21) As UByte 'agregado
Dim Shared msgin  (1 To 8192) As UByte ' message in futuro secuencia veremos si ahce falta 
' maximo seria para un acorde de 5 por ejemplo
' 5 notas + velocidad y canal = 7 bytes...para note on
' si tomamos 10 dedos de las 2 manos serian 10 notas+ vel + canal = 12
' si agregamos velocidad distinta para cada nota serian
' 20 mas canal = 21 bytes...maximo 
' podriamos agregar presion promedio aftertouch etc pero serin mensajes independientes 
'Dim errorString As ZString Ptr
Common Shared p as UBYTE Ptr 'cambiado a shared
p= @message(1) 
Common Shared pp As Integer Ptr 'para callback futuros datos usuario
Dim size As UInteger<64> 
'Dim sizeptr As UInteger<64> Ptr = @size
Dim Shared As UInteger portsin=0, portsout =0 'constantes, 
'no confundir con portin y portout que son variables

Dim Shared As integer tiempoPatron=240 ' cuantas negras en un minuto default
Dim Shared As integer tiempoPatronEjec=240
Dim Shared As Double old_time_on=0,old_time_off=0,old_time_on_int=0,old_time_off_int=0
Static Shared As Integer jply=0, indEscala=1
Dim Shared As double FactortiempoPatron=1
'elpatron esla negra ej I=60 ergo todo sera relativo A la negra q dura 1 seg
' 09-06-2021 agregue 0 en relDur para evitar cancelaciones pero ojo puede 
' tapar otros errores qu el tiempo se calcule com ocero y siga y 
'se pierda el tiemp anterior supongamso enun espacio en blanco agregado 
' en una ligadura pero eso lo prohibimos por ahroa como uso y listo
' si I=240 -> I=60 *4 -> time H=0,031250 / 4 =0,0078125
' una X a una velocidad de I=240 tiene un tiempo de 0,078125 seg
' en la 1er linea de figuras..
' pero porlalineamas chica 37 a 45 seria mi Tick mas chico [TickChco]
Static Shared As Double TickPlay =0.01041666 ''seg 5 miliseg.. para I=240
Static Shared As Double TickChico=0.01041666 ''seg 5 miliseg.. para I=240
Static shared As Double x3H = 0.0 , resta=0.0
' ambos Ticks deberin ser el  valor mas chico de la tabla y es tresillo de W 0.01041666

' " O "," P "," I "," L "," F "," E "," X "," H "," W ",   <-- la 8 es H
' I NEGRA, X SEMIFUSA, H GARRAPATEA,
'2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,[0.0208333] ,0.01041666, _ '37 45
' (*)  un tresillo toma dos negras y lo divide en 3 partes
'La figura musical que sigue a la garrapatea es la semigarrapatea. Ambas son figuras musicales en desuso. 
'Explicación 
'    La garrapatea o cuartifusa equivale a 1/128 de redonda.
'    La semigarrapatea equivale a 1/256 de redonda, es decir, a la mitad de una garrapatea. 
'
'Las figuras musicales más usadas son: (7) 
'Redonda, Blanca, Negra, Corchea, Semicorchea, Fusa, Semifusa.
' O SEA EL VALOR MAS CHICO USADO ES EL TRESILLO  DE X SEMIFUSA(0.06250) O SEA 0.041666
' Y CUANTOS DE ESOS EN UN A NEGRA? RESPECTO DE LA NEGRA UNA X ES 1/16, DOS DE ELLAS DIVIDIDO 3
' ES EL TRESILLO DE X O SEA 2/(16*3)= 1/48 DE LA NEGRA = 0.041666. ESTO PARA MRGRA=1 SEG
' O SEA TEMPO = 60. PARA UN TEMPO DE 240 TODO SERA 4 VECES MAS CHICO 0.041666/4= 0.01041666
' POR ESO TOMAMOS DOS VALORES MAS DE TEMPO MAS CHICOS PARA PODER TOCAR UNA SEMIFUSA A 240 DE TEMPO!!
' en ctrl-m frbo hascer una regla que parta de la nota en que estoy y tenga separaciones de 6
'--------------------------------------------------------------------------
' 96  x tresillo desemigarrapaea (0.01041666)= 1     negra
' 48  x tresillo desemigarrapaea (0.01041666)= 0.5   corchea
' 24  x tresillo desemigarrapaea (0.01041666)= 0.25  semicorchea
' 12  x tresillo desemigarrapaea (0.01041666)= 0.125  fusa
'  6  x tresillo desemigarrapaea (0.01041666)= 0.0625 semifusa
'-----------------------------------------------------------------
'  3  x tresillo desemigarrapaea (0.01041666)= 0.03125  garrapatea
' 1.5 x tresillo desemigarrapaea (0.01041666)= 0.015625 semi garrapatea
    
Dim Shared As float relDur (0 To 185) => {0, _  
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 1 9 
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 10 18
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 19 27
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ '28 36
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.0208333,0.01041666, _ '37 45
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 46 54
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 55 63
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ '  64 72
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ ' 73 81
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.0208333,0.01041666, _ '82 90
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 91 99
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 100 108
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 109 117
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ ' 118 126
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.0208333,0.01041666, _ ' 127 135
4 ,2 , 1.0, 0.50,0.250,0.1250 ,0.06250,0.031250,0.0156250, _ ' 136 144
5 ,2.5,1.25,0.625,0.3125,0.15625,0.078125,0.0390625,0.01953125,_ ' 145 153
6 ,3 , 1.5, 0.75,0.375,0.1875 ,0.09375,0.046875,0.0234375, _ ' 154 162
7 ,3.5,1.75,0.875,0.4375,0.21875,0.109375,0.0546875,0.02734375, _ '163 171
2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,0.0208333,0.01041666,0,0,0,0,0} '172 183

Dim Shared As Double durcla (1 To 45, 1 To 2) => { _
{0.01041666,45},_
{0.0156250,9},_
{0.01953125,18},_
{0.0208333,44},_
{0.0234375,27},_
{0.02734375,36},_
{0.031250,8},_
{0.0390625,17},_
{0.041666,43},_
{0.046875,26},_
{0.0546875,35},_
{0.06250,7},_
{0.078125,16},_
{0.083333,42},_
{0.09375,25},_
{0.109375,34},_
{0.1250,6},_
{0.15625,15},_
{0.166666,41},_
{0.1875,24},_ 
{0.21875,33},_
{0.250,5},_
{0.3125,14},_
{0.333333,40},_
{0.375,23},_
{0.4375,32},_
{0.50,4},_
{0.625,13},_
{0.666666,39},_
{0.75,22},_
{0.875,31},_
{1.0,3},_
{1.25,12},_
{1.333333,38},_
{1.5,21},_
{1.75,30},_
{2,2},_
{2.5,11},_
{2.666666,37},_
{3,20},_ 
{3.5,29},_
{4,1},_  
{5,10} ,_
{6,19} ,_
{7,28}}



' DurXTick me da segun la duracion la cantidad de ticks o columnas
' que dura al principio habra un On y en la columna final un OFF
' eso solo para las dur no ligadas, hasta la 90.
' si empieza con una ligada(i+) el off estara en la dur no ligada (L)
 'redondeando...3w vale 1 -- 96 * 0,01041666 seg = 1 seg
' y 3w son 0,01041666 seg si negra = 1 seg en  tempo 60
Dim Shared As integer DurXTick (0 To 185) => {0, _  ' 0 cambiado a 1 9-05-2025
 384, 192, 96 , 48, 24, 12, 6,  3,  2, _      ' 1  9
 480, 240, 120, 60, 30, 15, 7,  4,  2, _     ' 10 18
 576, 288, 144, 72, 36, 18, 9,  4,  2,_      ' 19 27
 672, 336, 168, 84, 42, 21, 10, 5,  3, _     ' 28 36
 256, 128, 64 , 32, 16, 8 , 4 , 2 , 1, _     ' 37 45 
 384, 192, 96 , 48, 24, 12, 6,  3,  2, _      ' 46 54  
 480, 240, 120, 60, 30, 15, 7,  4,  2, _     ' 55 63  
 576, 288, 144, 72, 36, 18, 9,  4,  2, _     ' 64 72  
 672, 336, 168, 84, 42, 21, 10, 5,  3, _     ' 73 81  
 256, 128, 64 , 32, 16, 8 , 4 , 2 , 1, _     ' 82 90   
 384, 192, 96 , 48, 24, 12, 6,  3,  2, _      ' 91 99  
 480, 240, 120, 60, 30, 15, 7,  4,  2, _     ' 100 108
 576, 288, 144, 72, 36, 18, 9,  4,  2, _     ' 109 117
 672, 336, 168, 84, 42, 21, 10, 5,  3, _     ' 118 126
 256, 128, 64 , 32, 16, 8 , 4 , 2 , 1, _     ' 127 135 
 384, 192, 96 , 48, 24, 12, 6,  3,  2, _      ' 136 144
 480, 240, 120, 60, 30, 15, 7,  4,  2, _     ' 145 153
 576, 288, 144, 72, 36, 18, 9,  4,  2, _     ' 154 162
 672, 336, 168, 84, 42, 21, 10, 5,  3, _     ' 163 171
 256, 128, 64 , 32, 16, 8 , 4 , 2 , 1, 0,0,0,0,0 }   ' 172 183 

' 96 son 1 seg para negra=60 entonces el tick valdra 1/96=10.41666666 mseg es 3W
' el mas chico son 1 parte o sea 10,41 mseg!! son e ldoble de los 5 mseg que usaba!
' entonces una negra abarcara 96 columnas 
' cols en 15 min = 15*60*96= 86400 columnas!
Const NO=0 ''ESTA CONSTANTE ESTA DUPLICADA Y EL COMPILADOR NO SE DA CUENTA JAJAJAJA
' ME DA ERROR EN EL USO DE LOS SIGUIENTES NO,,,, MAL EL COMPILADOR AMBOS DEC SON INCLUDE
' Y ROLLDEC ESTA ANTES QUE ESTE... CHANNN
Dim Shared As Integer play= NO,playb = NO, portin, numero, numeroFrac,cambioescala=0
Dim Shared As Integer portout=0 
Dim Shared As Double numfloat =0
Static Shared As string listout(), listin ()
Static Shared As integer listoutAbierto(), listinAbierto (),listoutCreado(), listinCreado ()
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
   vol             As Integer
   onoff           As Integer  '2 on, 1 off
End Type
Static Shared pasoCol (0 To 384) As vec




Declare Sub noteon	( note As UByte, vel As UByte,canal As UByte,portsal As UByte,i1 As Integer)
Declare Sub noteSimple	( pasoCol() As vec, cntold As integer, vel As UByte,canal As UByte,tiempoDur As Double,velpos As integer)
'Declare Sub AcordeIguales ( pasoCol() As vec,cnt As UByte,cntold As UByte, vel As UByte,canal As UByte,tiempoDur As double) 
'Declare Sub AcordeOffIguales	( pasoCol() As vec, cnt As UByte,cntold As UByte, canal As UByte)
'Declare Sub AcordeDistintos ( pasoCol() As vec,cnt As UByte,cntold As UByte, vel As UByte, canal As UByte,tiempoDur As double) 
'Declare Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte,cntold As UByte, canal As UByte,tiempoDur As Double)
'Declare Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As Double)
'Declare Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As double)
Declare Function vol (dura As UByte, vel As UByte) As ubyte
Declare sub noteoff( note As UByte, canal As UByte,portsal As UByte,i1 As Integer )
Declare Sub limpiarLigaduras(cnt As UByte,pasoCol() As vec)
Dim Shared As Integer ligaglobal=0 ', ligaglobalc (1 To 32)
'Relacion de nR indice de Roll, con nsE semitono, para evitar calculos.
Dim Shared As integer relnRnE(0 To 132) => { _  ' notapiano real vs nota ..semitono
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1, _
12,11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1 }

Static Shared indiceaudio (0 To 384) As Integer 
Type PGE  ' parametros guia escala 
 tipoescala As Integer
 notaescala As Integer
 alteracion As Integer ' 3=sos, 2 bem
 posicion   As Integer 
End Type

Dim Shared guiaEscala  (1 To 100) As PGE ' suponemos  100 cambios de escala en una pista
' en la posicion 1 se cargará la escala leida de la pista al inicio
' la escal debe guarar los 3 parametros Tipo escala , notaescala y alteracion en un principio basico
Type partesdura
 As UByte nota 
 As UByte dura
 As UByte onoff ' on 1, off 0
 As UByte velmidi
 As UByte nRk 
End Type

Dim Shared  duras(1 To 24, 1 To 3) As partesdura
Dim Shared duramidi (1 To 3) As UByte
Dim shared As UByte velmidi=0
' duras (x,y) , (x) las notas que pueden almacenarse de un acorde 12 (on+off)
' (y) las 3 duraciones como max ligadas para cada nota.
Dim Shared filtro As UByte=0
Dim Shared As UByte nacordeon (1 To 10),nacordeoff (1 To 10),naco=0,naco2=0,terminar_metronomo=0
Dim Shared As Integer posiacorde=0
Declare FUNCTION FiguraEquivalente(DURk As double) As ubyte
Dim Shared As Integer contcode=0,metronomo_si,sonidopista_si

Type notacallback
  As UByte nota 
  As UByte dato1
  As UByte vel
  As Double durk
  As Integer partes
End Type
Dim Shared  As notacallback notamidi ( 1 To 24) 
Dim Shared  As Double  durafig(1 To 24)
Type midicod Field=1
 As UByte modo ' dato1
 As UByte nota 
 As UByte  vel
 As Integer partes 
End Type

Redim shared CargaIn ( 1 To 384000) As midicod
'''Redim shared CargaInRoll ( 1 To 384000) As midicod

