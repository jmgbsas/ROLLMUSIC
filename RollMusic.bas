' apertura de ports en play  
' tiempoPatron a entero no tiene porque ser double, se graba en archivo
' Se agrego formar acordes aun sin nota en el lugar elegido, se deb eentrar al duracion
' Triadas desde Tonica completo Mayor Menor Disminuido formacion y play 
' fix consumo cpu S5=2 y fueradefoco=0 eliminado
' Funciona Acorde en Tonica triaca,,Ctrl+clik derecho luego seguir con mayor hasta no inversion
' SE ELIMINO DE 'Q' la configuracion de tama�os, proporciones y font
' se agrego nverEscalasAuxiliares y nanchofig a RollMusic.ini
' se agrego en Ver, si se ven o no las Escalas Auxiliares en el grafico
' TODO MULTIKEY IR PASANDO DE A POCO PROBANDO A E.SCANCODE MULTIKEY ES UNA BASURA REPITE EL COMANDO MIL VECES
' paso previo para armar acordes: necesitamos poder INGRESAR CAMBIOS DE ESCALA  y guardarlos en la secuencia
' pero al tocar se saltean como si no existieran,,,al retroceder o avanzar en la secuencia se debe ir actualizando
' la escala en uso, esto permite al ingresar un acorder construirlo en base a la escala usada en ese tramo.
' usaremos xml para leer y escribir musicxml e intercambia rocn otros programas
' antes que midi despue salgun dia haremos midi no se veremos.... 
' http://xmlsoft.org/examples/index.html
' YA cierra todas las sesiones de rollmusic desde control
' futuro grabar mxold y algo mas para conservar el tama�o de la ventana y el tama�o del font
' usado por el usuario !!!! OK Y AANDA
' el borrado de columna esta defectuoso hay que dar 0 y luego 12 x en toda la octava para
' que borre mejor usaremos marcas de zona para borrar.
' dejo de andar marcado de zonas porque habia un exit do en COMEDIT=False con mousex>50
' se movio zonas dentro de mousex> 50 y luefo COMEDIT=false volvio a funcionar
 ' se intento usar Byte en vez de Ubyte para usar negativos pero hay qu emodificar mucho
 ' se usara IF variabrlee > 127 par ausar por ejemplo Vol > 127 para indicar escalas...
 ' Esta nota base...es Tonica 3era 5ta 7ma ...
 ' uso ctrl+click para ingresar notas nuevas en Edit, sino al pasar a Ctrl-M u otras acciones
 ' entraba notas no deseadas..
 ' 11-12-2021 redusco la camtidad de partes a 20 partes_falta (1 To 20), partes_sobra(1 To 20)
 ' rooloop 2673 menu contextual acordes desarrollo 06-12-2021
 ' correccion Clcik end EDIT 06-12-2021 s3=0 movido a y > 50 
 ' correccion de abrir nota si menor=mayor no hace nada, allevantar click rompia todo
 ' v23 fraccionar automaticamente en COMEDIT cursor al poner notas menores o 
 ' mayores en duracion a otra nota en acorde existente, tambien armar acordes desde una nota
 ' existente como tonica mayores menores etc,,buscar al tonica si consideramos es una 3era
 ' o una 5ta..
 ' V22 agregamso menu contextual en lectura con click derecho para acordes falta desarrollar
 ' v22  SetStateMenu(hmessages,1102,3) o  SetStateMenu(hmessages,1103,0) check items menu
 ' V22 abrir nota se ajuto final dejaba una columna vacia
 ' V21 SE AJSUTO MOVER LA VENTANA DRAGANDO LA CINTA SUPERIOR FUNCIONA MEJOR
 ' V21 TREADdETACH DE tHEREADlOOP Y THREAD1 PLAY CLOSE PORTS ETC EN EL CIERRE DE CONTROL 
 'V21 ESTRUCTURO ACORDESONIGUALES Y COLOCO ALLOF EN VARIAS PARTES,Q,FIN PLAY, P.
 ' V19 TOCA BASTANTE BIEN ACORDES IGUALES CON SILENCIOS EN SU FORMACION
 ' Y CALCOMPAS AHROA INCLUYE SILENCIOS 
 ' v14 ...AOI-NUEVO PERFECTO TODOS LOS ACORDES IGUALES EN ACORDES TODAS LAS POSICIONES DBEN ESAR LLENAS
 ' CON NOTAS CON SONIDO O SIN SONIDO PERO TODOS CON LA MISMA CANTIDAD DE NOTAS POR AHORA UNAS PODRAN SONAR
 ' OTRAS NO SEGUIR CON MAS PREUBAS...ANDA OK CON LOS POCOS CASOS QUE TENGO...
 ' V10 FRACCIONADOR divido la nota seleccionada en n partes 
  ' v8 fix nucle dur=0 nota=181 sino el borrado de notas anda mal
  ' toda celda debe tener 0,181 nada de 181,181...eso se cambio
  ' V7 CRASH DE SPACE EN PLAY, Y VER ACORDES DISTINTOS SI SE PUEDE CAMBIAR UNA NOTA LARGA
  ' EN 2 CORTAS AUTOAMTICAMENTE PARA PONER EN ACORDE OTRAS 2 MAS CHIVAS EL:
  ' P    ==> L+I* || DISCERNIR (1) |P|     DE (2)| P   |
  ' L+I      L+I                   |L| I         | L I |
  ' EL ULTIMO CASO (2) NO SE PUEDE EN ROLL , EL (1) SI
  ' O SEA QUE EL PROGRAMA AUTOMATICAMNETE PARTA UNA NOTA LARGA COMO P I O ETC
  ' FRENTE A OTRAS EN ACORDE MAS CHICAS Y UNIDAS O NO...
  ' V5 CORREGIDO, V6 CORREGIDO OTRAS COSAS,,QUEDA CRASH DE PLAY CON SPACE...
  ' 08-11 V5 anda mejor qu ela V4 solo que la ligadura I+I+I la toca como I+I
  ' LE FALTA UNA NEGRA DE DURACION, EL RESTO LO TOCA BIEN!!!
  ' SIN TOCAR CASI NADA SOLO ELIMINAR EL ANALISIS DE LIGA EN PLAYALL
  ' SEGUIR CORREGIR CON EL USO DE LSO CAMPOS NUEVOS Y AL TERMINAR 
  ' ELIMINAR LOS CAMPOS DE VEC QUE NO SE USEN
' ------------------------------
'  -Wc -fstack-usage genera un archivo en runtime *.su con el uso del stack
' por cada funcoin el default total es 1024k, -t 1024 no haria falta colocarlo
' en la linea de comando
' https://gcc.gnu.org/onlinedocs/gnat_ugn/index.html#SEC_Contents
'====> clave DEVF (desarrollo futuro, comentarios en lugares apr adessarrollar mas
' funcionalidades.)
' -------------------------------------------------
#define __FB_WIN64__
#if defined (__FB_WIN64__) 
#libpath "C:\msys64\mingw64\lib"
#else
#libpath "/usr/lib"
#endif
#define EXTCHAR Chr(255)


#include "mod_rtmidi_c.bi"
#Inclib  "rtmidi.dll" 'usa librerias estaticas 
'#Inclib  "rtmidi"  '''uso al dedeisco rtmidi.dll
#include "fbthread.bi"
#include "crt.bi" ' QSORT
 
#define WIN_INCLUDEALL
#include once "windows.bi"
#include once "win/shlobj.bi"

'#Include Once "/win/commctrl.bi"
'#Include "crt/stdio.bi"
#include "file.bi"
#include "fbgfx.bi" ' se carga antes de windows.bi para evitar duplicates..o conflictos
'#Include Once "win/mmsystem.bi" '' FUNCIONES MIDIde windows!!!! perousaremos RtmidiC por hora
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif

' Nota: algun dia si quiero midifile intentar usar una libreria de C pura 
' C:\IT64\AREAWORKAUX\MIDI-LIBRARY\midilib-master\midilib-master\freeBasic

Sub getfiles(ByRef File As OpenFileName,flag As String, accion As String)
    Dim As ZString * 2048 SELFILE
    Dim As String MYFILTER=flag+Chr(0)
    With File
  .lStructSize = SizeOf(OpenFileName)
  .hwndOwner = NULL
  .hInstance = NULL
  .lpstrFilter = StrPtr(MYFILTER)
  .nFilterIndex = 0
  .lpstrFile = @SELFILE
  .nMaxFile = 2048
  .lpstrFileTitle = NULL
  .nMaxFileTitle = 0
  .lpstrInitialDir = @"nosuch:\"
  .lpstrTitle = @"Open"
  .Flags = 4096
  .nFileOffset = 0
  .nFileExtension = 0
  .lpstrDefExt = NULL
    End With
    If accion="open" Then
    GetOpenFileName(@File)
    EndIf
    If accion="save" Then
    GetSaveFileName(@File)
    EndIf

End Sub
' FILE DIALOG adicionales
 
'--------

Dim Shared file As OpenFileName
Dim Shared As String myfilter
myfilter  = "Roll Files"+Chr(0)  +"*.roll"+Chr(0)
'myfilter += "Ini files"+Chr(0)   +"*.ini;*.txt;*.cfg"+Chr(0)
myfilter += "Rtk  Files"+Chr(0)   +"*.rtk"+Chr(0)


Open "AAAAA-test.TXT" For Output As 5
Dim Shared As Integer abierto=0
Common Shared  mensaje As Integer 
'''  end file dialog  
' para GTK Gtk:list()
'#Include Once "crt.bi"
#include once "gtk/gtk.bi"

' This is our data identification string to store data in list items
Const list_item_data_key ="list_item_data"
' fin GTK
' -- INICIO openGL GLFW 3.1.1 
' habilitamos OPENGL para ver si podemso mostrar la grabacion de ejecucion
' de entrada por MIDI-IN.
#Include once "glfw3.bi"
If glfwInit()=GL_FALSE then
  print "error: can't init GLFW"
  beep : sleep : end 1
end If
Dim As GLFWwindow ptr  win
' ----FIN OPENGL 

'===============================
#include "ROLLDEC.BI"
Dim Shared As Integer pd1, fa1 
pd1 = GetCurrentProcessId()  

''Open "midebug.txt" For Output As #1
 Open "midebug"+ "["+Str(pd1)+"]" + ".txt" For Output As #1
Print #1,"start"
Print #1,"PID DE ESTE PROCESO ",pd1



'Open "mivector.txt" For Output As #3
'Open "miplayall.txt" For Output As #4
'Open "test-AAAAA.TXT" For Output As #5
'print #1, "version para ceros!!!!!! "
'Dim fcon As Integer 
'fcon=freefile
'Open cons  for Output As #8

''Open "figuras.txt" For Output As #1
Print #1,Date;Time
' secuenciador de 9 octavas estereo, modo Piano Roll,hace uso de
'letras para las duraciones en vez de rectangulos...
' edicion modificacion insercion,,,12 eventos c/u con
'nota, duracion,volumen ,paneo, pitch bend e instrumento
' version actual front end solamente 1 track...no reproduce
' no genera midi todavia..
'-------------
''
'=======================
'--------------
#include "string.bi"
#include once "cairo/cairo.bi"
#include "midiinfo.bi"
'==============================
'#Include "NOTAS.bi"
#include "RTMIDIDEC.bi"
'==============================
#include "ROLLCONTROLDEC.bi"
'=============================
' iup start
' SOLO PARA WINDOWS LEE MIDI FILES PERO FALTA FUNCIONES PARA GRABAR!!!
'---> #Include once "foro/fmidi.bi"
#include once "fbthread.bi"
#include "foro/window9.bi"

'#Include "crt/win32/unistd.bi"
#inclib "ntdll"
#include "win/ntdef.bi"

Dim Shared fs As Integer 
fs = FreeFile 
Open "secuencia.txt" For Output As #fs

Const NEWLINE = !"\n"

'tempo I=160, seri equivalente a O=40 como l maxima cantdad de ticks de O es
' eldela figura mas peque�a=128 ticks...40 * 128 = 5120 por minuto.
' Si deseo un secuecnia de CantMin minutos
tempo=160  ' negra=160
CantMin=15
'NotaBaja=1 : NotaAlta=128

Print #1, "__FB_ARGV__ ",__FB_ARGV__
Print #1, "__FB_ARGC__ ",__FB_ARGC__
'Dim direp As ZString  Ptr
'Dim dires As String
Common Shared As Integer ubirtk, ubiroll
Print #1,"__FB_ARGC__ ", __FB_ARGC__
Dim As Integer com_usarmarco =0
For ix = 0 To __FB_ARGC__
  Print #1, "arg "; ix; " = '"; Command(ix); "'"''

 If ix=1 Then
  
 ubirtk = InStr (LCase(Command(ix)),".rtk")
 ubiroll=  InStr(LCase(Command(ix)),".roll")

 If ubirtk > 0 or ubiroll>0 Then
   ntk=0 
   titulos(0)=Command(ix)
 Else
    desde= CInt(Command(ix))
'    pmTk(ntk).desde=desde
   Instancia=1    
 EndIf
 Print #1,"ubirtk ",ubirtk
 Print #1,"ubiroll ",ubiroll
    'sigue en roolloop principio
 EndIf
 If ix=2 Then
  hasta= CInt (Command(ix))
 ' pmTk(ntk).hasta=hasta
    Instancia=2
 EndIf
 
 If ix=3 Then
  titu=  (Command(ix))
     Instancia=3
 EndIf

 If ix=4 Then
  instru=  CUByte (Command(ix))
     Instancia=4
 EndIf

 If ix=5 Then
  pid1=  CInt (Command(ix))
     Instancia=5
 EndIf

 If ix=6 Then
  com_usarmarco=  CInt (Command(ix))
     Instancia=6
 EndIf

Next ix
'Dim Shared As Integer pd1, fa1 

'pd1 = GetCurrentProcessId()  
'Open "midebug" + "["+Str(pd1)+"]" + ".txt" For Output As #1

''Open "midebug.txt" For Output As #1
'Print #1,"start"
'Print #1,"PID DE ESTE PROCESO ",pd1
fa1=FreeFile
Open "procesos.txt" For Append As #fa1
If pid1=0 And ix < 3 Then
 pid1=pd1
Else
  If pid1 <>0 Then
     Print #fa1,pd1
  EndIf 
EndIf 
Close fa1

Sleep 100


If desde = 0 And hasta = 0 Then
 Print #1,"intervalo no dado usando default!"
 desde => 4  ' -> 3  
 hasta => 8  ' -> 6 le debo restar la octava oculta +1
 
' internamente no usamos cero , empezamos desde 1 pero en roll se ve tal cual es
' va desde 0 a 7 ergo de 4 a 8 el default es la eleccion en roll 3 a 7 pero al 7 es de
' ontrol o sea es de 3 a 6 y la de control no aparece en el grafico...
' l a3 es al 4t octava y la 6 es al 7ma..pues empieza desde 0 en la visualizacion.
' asi coincidimos con lo que se usa en piano la ocatava se numera desde 0.. 
 'pmTk(ntk).desde=desde
 'pmTk(ntk).hasta=hasta
EndIf
' calculo teorico a tiempopatron 160 , pero roollmusic arranca a 120
CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...o pasos
'CantTicks=76800
CantTicks=1000 ' 3 MINUTOS A NEGRA 160/min=500 Y Q TODAS SEAN FUSA
' 4000*30=120000 x 16=1920000
'  

Dim Shared As paso compas (1 To CantTicks) 'cada item es la posicion en donde

desdevector = desde
hastavector = hasta
estoyEnOctava =desde
estoyEnOctavaOld =desde
' test test el maximo es desde=1...,0 ... hasta=9 ..,115 y el default es 
'           el default   desde=4..,39 ... hasta=8 ...102 
' --------
NB => 0 + (desde-1) * 13   ' 39 , Notapiano=36, nR=39 -coincide no sobra nada
NA => 11 + (hasta-1) * 13  ' 102, Notapiano= 83, nR=89 - no coincide sobra desde
Print #1,"NB, NA",NB,NA 
' sobra desde 90 a 102 inclisive o sea 13 posiciones...
' automatiando podemos decier para cualqueir definicion de intervalo de octavas que
' CALCULO DE POSICION DE LA INFORMACION DE ACORDES:
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default ->11+ 6*13 +1=90  ==> 11 + 7*13=102
' PARA EL MAxIMO SERIA
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default 9-2 ->11+ 7*13 +1=103  ==> 11 + 8*13=115
' O sea maximo desde 103 a 115 son las posiciones libres...
' vamos a reservar en una posicion dada para la info de acordes por ejemplo en la 
' maxima 103 para octava0,104 octava1,105 octava2, 106 oct3,107 oc4,108 oct5
' 109 oct6, 110 oct 7...ergo quedan libres 111,112,113,114,115  

ReDim (Roll.trk ) (1 To CantTicks,NB To NA) ' Roll de trabajo en Pantalla

'Print #1,"instru ",instru
' ojo debe se NB al reducir octabas NB cambia
If instru > 0 Then
  Roll.trk(1,NA).inst = CUByte(instru)
  patchsal=instru
EndIf
'Print #1,"Roll.trk(1,NA).inst ",Roll.trk(1,NA).inst
'Print #1,"NB ",NB
'Print #1,"NA ",NA

'Print #1,"desde ",desde
'Print #1,"hasta ",hasta

param.Roll=Roll
param.ubiroll=ubiroll
param.ubirtk=ubirtk


Dim  As Integer  ctres=1 ' 5 octavas por track
Dim As Integer lim1 

lim1=1 ' lim3 vale 25 se reserva el ultimo par avalores de control, no alcanza
' 26-01-2022 la zona de control debe ser 1 octava mas o sea lim3=25
ReDim (Track(00).trk ) (1 To CantTicks,1 To lim3) ' lo usa instancia sin cancion
ReDim (Track(01).trk ) (1 To CantTicks,1 To lim3) ' lo usa sin instancia
ReDim (Track(02).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(03).trk ) (1 To ctres,1 To lim1)
ReDim (Track(04).trk ) (1 To ctres,1 To lim1)
ReDim (Track(05).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(06).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(07).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(08).trk ) (1 To ctres,1 To lim1)
ReDim (Track(09).trk ) (1 To ctres,1 To lim1)
ReDim (Track(10).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(11).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(12).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(13).trk ) (1 To ctres,1 To lim1)
ReDim (Track(14).trk ) (1 To ctres,1 To lim1)
ReDim (Track(15).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(16).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(17).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(18).trk ) (1 To ctres,1 To lim1)
ReDim (Track(19).trk ) (1 To ctres,1 To lim1)
ReDim (Track(20).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(21).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(22).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(23).trk ) (1 To ctres,1 To lim1)
ReDim (Track(24).trk ) (1 To ctres,1 To lim1)
ReDim (Track(25).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(26).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(27).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(28).trk ) (1 To ctres,1 To lim1)
ReDim (Track(29).trk ) (1 To ctres,1 To lim1)
ReDim (Track(30).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(31).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(32).trk ) (1 To Ctres,1 To lim1)

' c/u de estos track es redimensionable preserve en la 1era dimension 
' o sea en las posiciones, lo que debo hacer es cargar estos Tracks
' con eventos pero para mostrarlso usaria el Roll 
' suponemos que cada track solo puede tener acordes de hasta 12 notas,1 to lim2
' en el momento de la carga de Roll cargar tambien Track �?
' creo que no solo debo copiar en el momento antes de grabar o reproducir
' debere hAcer otro play PlayTracks en donde se ejecutara todos los tracks
' barriendo la posicion comun a todos y las notas de cada uno EN SINCRONIA CON LA VISUALIZACION!
' ENTONCES EN PLAYALL RECORRO TRACKS PERO NO EL DE VISUALIZACION,,,,,VEREMOS.,

ReDim (RollAux.trk) ( 1 To CantTicks,NB To NA )
CantCompas = 40 * CantMin
' 600 COMPASES DE 32 POSICIONES 14,74 MBYTES , 235 MBYTES 16 TRACKS
' si  I=160 -> O=40 , 40 compases/min en 15 min=600 compases
' 600 * 128ticks maximo de H = 76800 ticks
' 5 min seria / 3 = 200 compases, aprox 5Mbytes *16 = 80 mbytes 16 trcks
' maso menso funciona pero debo empezar desde cero de vuelta....


'''Dim Shared As ubyte Insercion (1 To 128, 1 To 12000)
' si inserto en un track o canal debo desplazar a todos!!!
'  de modoque tener una insercion para c7useracrisimo...
' solo resta usar copia y Redim preserve

' indice notas, fin compas, nota con puntillo o ligadura,
' se almcena en el heap porque es shared
' 12k posiciones de 96 notas o 128 con help y espacios ...8.388.608 bytes 8 megas
/'
Dim l As Integer
For l = 1 To 65
print #1, l;" ";figura(l)
Next l
''Close aca estaba habilitado el close humm ah pero esta comentado

End
'/
''https://www.freebasic.net/forum/viewtopic.php?t=15127
'print #1,"NroCol, ancho, anchofig ",NroCol, ANCHO, anchofig
' ------------ play de usuario - datos por midiin -------------------
' 16 CANALES DE ENTRADA, PAR AL REPRODUCION O ARMADO
' SUMAREMOS SIEMRE ENTRE AMBAS FORMAS NO AMS DE 32 PORQUE AL REPRODUCIR
' SE SUMAN ESTOS 16 , OSEA OUTPUT+INPUT <=32 SINO EN LA REPRODUCCION
' TENDRIASMO UN MAZIMO DE 32+16=48...Y LA POLIFONIA DE RTMIDI
   
ReDim (Toca(01).trk ) (1 To Ctres) 
ReDim (Toca(02).trk ) (1 To Ctres) 
ReDim (Toca(03).trk ) (1 To Ctres) 
ReDim (Toca(04).trk ) (1 To Ctres) 
ReDim (Toca(05).trk ) (1 To Ctres) 
ReDim (Toca(06).trk ) (1 To Ctres) 
ReDim (Toca(07).trk ) (1 To Ctres) 
ReDim (Toca(08).trk ) (1 To Ctres) 
ReDim (Toca(09).trk ) (1 To Ctres) 
ReDim (Toca(10).trk ) (1 To Ctres) 
ReDim (Toca(11).trk ) (1 To Ctres) 
ReDim (Toca(12).trk ) (1 To Ctres) 
ReDim (Toca(13).trk ) (1 To Ctres) 
ReDim (Toca(14).trk ) (1 To Ctres) 
ReDim (Toca(15).trk ) (1 To Ctres) 
ReDim (Toca(16).trk ) (1 To Ctres) 
ReDim (Toca(17).trk ) (1 To Ctres) 
ReDim (Toca(18).trk ) (1 To Ctres) 
ReDim (Toca(19).trk ) (1 To Ctres) 
ReDim (Toca(20).trk ) (1 To Ctres) 
ReDim (Toca(21).trk ) (1 To Ctres) 
ReDim (Toca(22).trk ) (1 To Ctres) 
ReDim (Toca(23).trk ) (1 To Ctres) 
ReDim (Toca(24).trk ) (1 To Ctres) 
ReDim (Toca(25).trk ) (1 To Ctres) 
ReDim (Toca(26).trk ) (1 To Ctres) 
ReDim (Toca(27).trk ) (1 To Ctres) 
ReDim (Toca(28).trk ) (1 To Ctres) 
ReDim (Toca(28).trk ) (1 To Ctres) 
ReDim (Toca(30).trk ) (1 To Ctres) 
ReDim (Toca(31).trk ) (1 To Ctres) 
ReDim (Toca(32).trk ) (1 To Ctres) 

'1) tomar de midin los eventos
'2) si como vienen imprimirlos para ir viendo que sale
'3) luego de verificados los eventos, guardarloe es el vector Toca
'   colocandole el timestamp con una presicion mayo ra 15 mseg,
' consideramos I=250  por minuto (240ms) => L=500 (120ms),=>F=1000(60ms)
' E=2000 (30ms), H=4000 (15ms).
' usar rtmidi_in_create cdecl alias "rtmidi_in_create" (byval api as RtMidiApi, byval clientName as zstring ptr, byval queueSizeLimit as uinteger) as RtMidiInPtr
' para modificar el limite de la cola , default 1024 odriamos pasarlo a mas
' 4096 o 8192 valro tipico ,,,de un buffer

'--------------------------
Dim As String driver

posmouseOld = 0:posmouse = 0
COMEDIT = False:resize = False
po = @octaroll
*po = hasta -1 ' test 09-09-2021 
s1=0:s2=0:s3=0:s4=0:s5=0:s6=0:s7=0:s8=0
If font=0 Then 
 font=18
EndIf
indaux=0:carga=0
' -----------------------------------------------------------------------
' notas redonda (O),blanca(P),negra(I),corchea(C),semicorchea(S), Fusa(F),Semifusa(E)
' O P I L F E W H 
' puntillo O* P*  C* elsigno  *
' -puntillo resta en vez de sumar -O -P -C ...-U
' -------------------------------------------------------------------------
BordeSupRoll = Int((ALTO ) /18) ' (1400 )/18 integer = 77
inc_Penta = Int((ALTO - BordeSupRoll) /(40)) ' 26 double 1330/66 para 1400 resolu

' *******************************************************++
BordeSupRoll = BordeSupRoll -  66* inc_Penta ' de inicio muestro octava 4 la central C3
' *************************************************+
' inc_Penta=separacion de lineas
'---------------------
gap1= anchofig* 2315/1000 ' 81 default
gap2= (914 * gap1) /1000 ' 74 default
gap3= (519 * gap1) /1000 ' 42 default

'print #1,"gap1 ",gap1
'---------
Dim ffini As Integer 
Dim As String sfont,smxold,smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig,sVerCifradoAcordes

ffini=FreeFile
Open "./RollMusic.ini" For Input As #ffini
Line Input #ffini, sfont
Line Input #ffini, smxold
Line Input #ffini, smyold
Line Input #ffini, sancho
Line Input #ffini, salto
Line Input #ffini, sdeltaip
Line Input #ffini, sVerEscalasAuxiliares
Line Input #ffini, sanchofig
Line Input #ffini, sVerCifradoAcordes

'Print #1,"sfont, smxold, smyold,sANCHO,sALTO..  ",sfont, smxold, smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig

cerrar ffini
Sleep 100


nfont=ValInt(sfont)
nmxold=ValInt(smxold)
nmyold=ValInt(smyold)
nancho=ValInt(sancho)
nalto=ValInt(salto)
ndeltaip=ValInt(sdeltaip)
nVerEscalasAuxiliares=ValInt(sVerEscalasAuxiliares)
nanchofig =ValInt(sanchofig)
nVerCifradoAcordes=ValInt(sVerCifradoAcordes)

Print #1,"nanchofig " ,nanchofig
If nfont > 0 Then
  font=nfont
EndIf
If nmxold <> 0 Then
   mxold=nmxold
EndIf
If nmyold <> 0 Then
   myold=nmyold
EndIf

If ndeltaip <> 0 Then
   inc_Penta=ndeltaip
EndIf
If nanchofig <> 0 Then
   anchofig=nanchofig
   gap1= anchofig* 2315/1000
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) - 4
   ANCHO3div4 = ANCHO *3 / 4 
EndIf
'Print #1,"NROCOL AL INICIO, ANCHO, anchofig ",NroCol, ANCHO, anchofig

'---------
If mxold=0 And myold=0 Then
GetMouse mxold,myold, , MouseButtons
EndIf

posicion = 1 ' comienzo del roll
'indice   = 1  ' numero de nota al comienzo del programa B8
espacio = 0
backspace = 0
fijarEspacio=0

'--FFT FREE FONT-
Var Shared ft => FreeType()
'' Load a font with FreeType
Common Shared As FT_Face ftface
FT_New_Face( ft, "Bebaskai.otf", 0, @ftface )

' ========== CONTROL DEL NRO DE OCTAVAS MOSTRADO SE PODRA PONER PARA EL USUARIO
' VER SI SE PUEDE USAR ARRAYS POR PORCIONES
'----- -FIN
'----- MIDI MICROSOFT
'https://docs.microsoft.com/en-us/windows/win32/multimedia/midi-functions
'DIM CAN As UINT
'CAN= midiOutGetNumDevs()
'print #1, "MIDI NUM DEVS ";CAN
 
'-----
' ancho de figura,separaciondelasmismas en pantalla anchofig
'' ---------------  LOOP 1 ---------------
 On Error Goto errorhandler
' enviamos esto a una sub ROLLLoop, creaPenta esta al principio y no tiene declare
' el declare falla si se usa con este tipo de vector udt no se puede usar declare
'stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


'Dim Shared nombreport As ZString Ptr
'Dim Shared midiout (0 To 3) As  RtMidiOutPtr ' abrir hasta 32 dispositivos
'Dim Shared midiin As RtMidiInPtr
'Dim Shared As Integer porterror
Dim Shared nombreport As ZString Ptr

midiin(0)     = rtmidi_in_create_default()
midiout(0) = rtmidi_out_create_default()


'print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout(0))
portsin  =  port_count(midiin(0))
Dim i1 As Integer
Print #1, "portsin  "; portsin
Print #1, "portsout "; portsout

ReDim  listOutAbierto (0 To portsout)
ReDim  listInAbierto  (0 To portsin)

'listOutAbierto(0)=1
listInAbierto(1)=1

Dim Shared nombreOut(0 To portsout) As ZString Ptr
Dim Shared nombreIn (0 To portsin)  As ZString Ptr

'Dim i1 As integer

'For i= 1 To portsin - 1
' midiin = rtmidi_in_create_default ( )
' Print #1,"creando default ",i
'Next i

For i1 = 0 To portsin -1 
    nombrein(i1) = port_name(midiin(0), i1)
    Print #1, *nombrein(i1)
Next i1  
Print #1,"-----------------------------"
For i1 = 0 To portsout -1 
    nombreOut(i1) = port_name(midiout(0), i1)
    Print #1, *nombreout(i1)
Next i1  

'---------------------
Dim As Long porterror
open_port midiout(0),0, nombreOut(0)
    Select Case porterror
      Case RTMIDI_ERROR_WARNING ' esto da cero �? y es eror�?
        Print #1, "RTMIDI_ERROR_WARNING",porterror
      Case RTMIDI_ERROR_DEBUG_WARNING
        Print #1, "RTMIDI_ERROR_DEBUG_WARNING"
        Close
        End

      Case RTMIDI_ERROR_UNSPECIFIED
        Print #1,"RTMIDI_ERROR_UNSPECIFIED"
        Close
        End

      Case RTMIDI_ERROR_NO_DEVICES_FOUND
        Print #1,"RTMIDI_ERROR_NO_DEVICES_FOUND"
        Close
        End

      Case RTMIDI_ERROR_INVALID_DEVICE
        Print #1,"RTMIDI_ERROR_INVALID_DEVICE"
        Close
        End

      Case RTMIDI_ERROR_MEMORY_ERROR
        Print #1,"RTMIDI_ERROR_MEMORY_ERROR"
        Close
        End

      Case RTMIDI_ERROR_INVALID_PARAMETER
        Print #1,"RTMIDI_ERROR_INVALID_PARAMETER"
        Close
        End

      Case RTMIDI_ERROR_INVALID_USE
        Print #1,"RTMIDI_ERROR_INVALID_USE"
        Close
        End

      Case RTMIDI_ERROR_DRIVER_ERROR
        Print #1,"RTMIDI_ERROR_DRIVER_ERROR!"
        Close
        End

      Case RTMIDI_ERROR_SYSTEM_ERROR
        Print #1,"RTMIDI_ERROR_SYSTEM_ERROR"
        Close
        End

      Case RTMIDI_ERROR_THREAD_ERROR
        Print #1,"RTMIDI_ERROR_THREAD_ERROR"
        Close
        End
    End Select
Sleep 1000

Print #1,"Microsoft No se usa en este programa con este algoritmo es muy inestable"
Print #1,"-------------------------------------"

close_port midiout(0)
''=====> nunc ausar mientras el programa funciona out_free   midiout(0)
'' cerrar y abrir ports pero nunca liberar memoria 
listOutAbierto(0)=0


'Sleep 2000

'For i=0 To portsout -1
'   Print "midiout ",i, *nombreOut(i)  
'   close_port midiout(i)
'   out_free   midiout(i)
'   Print ,"cierro ",*nombreOut(i)
'Next i'

'/
'========================== 
#include "WinGUI.bi"
#include "RTMIDISUB.bas"
#Include "ROLLTRACKS.bas"
#include "ROLLSUB.BAS"
'===========================
 Dim As Integer MenuFlag=0, LoopFlag=0 

'========================== 
#include "ROllLoop.BAS"
'========================== 
' aca puedo llamar a varios thread 1 por vez segun el instrumento editado
' o sea 1 vector de roll distinto para casa Thread o llamar a cada Vector
' con el mismo Thread para verlo en pantalla. Se veria 1 por vez o si queremos podriamos
' ver mas de 1 pero apra eso deberia llamar a roll music mas de una vez 
' y eso lo haria desde call roll 
'----------------
#include "ROLLMIDI.BAS"

'----------------

'>>>>>>portsout = portout
'>>>>>*nombreport = ""
' SE ABRE MAS DE UN PUERTO SI SE DESEA ,,
' CUADNO TENGA ABIERTO SMAS DE UN PUERTO DEBERE ASIGNAR CIERTOS PUERTOS A CADA PISTA
' LUEGO EN CADA PISTA PUEDO TENER UN PUERTO O DISPOSITIVO CADA UNO CON SUS 16 CNALES
' Y EN CADA CANAL SUS 128 INSTRUMENTOS.,,LUEGO VEREMOS ESO DE LOS BANCOS ETC
   ' nombreport = port_name(midiout, portsout)
    
' para abrir un port no hace falta el nombre en el comando ,puede estar en vacio ""
' por ello vamos a grabar en el archivo el numero de port , podre abrir solo con el nombre
' sin el numero de port? el nombr eidentifica mas al dispositivo,,,
' deberia guardar ambos nro port y nombre,,,,ufff
' i la pc no cambi ade configuracion de hardware lso numeros serian siempre lso mismos
' par also ports y no haria falta los nombres, pero debo dejar constancia cual era el dispositivo
' o los dispositivos usados....para poder elegir el port en el menu debo cerrar el port actual
' y abrir el otor esto de ponerlo en el inicio esta mal

'Print #1,"  VA A ABRIR EL PORT portsout, nombreport ",portsout, *nombreport
'open_port (midiout, portsout, nombreport)

'print #1,"  "
'If Roll.trk(1,NA).inst > 0 Then
' ChangeProgram ( Roll.trk(1,NA).inst , 0)
'EndIf
' print #1,"ChangeProgram inst ", Roll.trk(1,NA).inst
'Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
'Print #1,"INSTANCIA ",instancia
' carga de opciones iniciales RollMusic.ini

''https://www.freebasic.net/forum/viewtopic.php?f=3&t=22821&p=204270&hilit=loop+through+an+array+with+the+pointer#p204270

Dim As Integer  tilde1102=MF_UNCHECKED,tilde1103=MF_UNCHECKED  
Dim As Integer anchoK, altoK
anchoK = GetSystemMetrics(SM_CXSCREEN)
'altoK = GetSystemMetrics(SM_CYSCREEN)
'-------------
/'
Dim As Integer g1, h1,h2
Dim As  byte Ptr p5,p6

Dim hnro As Integer
' prueba desarrolllo escalas 
 For g1 =1 To 47
    Print #1, escala(g1).nombre
    hnro=escala(g1).nropasos 
    p5= escala(g1).pasos 
    For h1 = 1 To hnro -1
     
     Print #1, *p5;",";  ' impresion de intervalos de la escala
     p5=p5+1 
    Next h1
    Print #1, *p5
' CONSTRUCCION DE LA ESCALA
 Dim As Integer k3=1
   p6= escala(g1).pasos
    For h2 = 1 To hnro -1
     
     'Print #1,NotasEscala(k3);" " ;  ' impresion de la escala con las notas c c# d  etc
     Print #1, 12-k3+1;" ";
     k3= *p6 + k3
     
     p6=p6+1 
    Next h2
    Print #1, 12-k3+1 
    'Print #1, NotasEscala(k3)

 Next g1
 '/
 ' ahora debo traducir las notas a notas de Roll e incorporarlas al vector
 ' las notas  en Roll van de 0 a 11 para el indice son los semitonos, pero se cargan 
 ' los valores 1 a 12. En Roll C=12 ...B=1 ergo si la escala dice 12=B debe ser 1
 ' o sea =12-valor+1 (12-1+1) o podemos crear el vector de escalas al reves de 12 a 1 pero
 ' se complicaria todo para analizar. ergo invertimos y listo 1 a 12 es 12 a 1...
 ' Entonces para taducir a Roll hago 12-valor+1 . pero par aanalizar y mostrar es valor
 ' ejemplo triada=1,3,5=C,E,G, en Roll seria = 12,10,8
 
 ' genial puedo recorrer un array con un pointer!!!!
'-------
nroversion="0.4560 en desarrollo:grabar archivos MIDI-IN"
'4536-> 1) Repeticion con 1 pista de Track. 2) luego con cancion.- Pendiente
acercade = "RollMusic Version "+ nroVersion +" Autor Jose M Galeano, Buenos Aires Argentina 2021-2022.Mi primer aplicacion gr�fica. En esta version Solo ejecuta las secuencias " + _
 "a base de algoritmos sin una linea conductora de tiempos. Solo se basa en las duraciones de las notas. " + _
 "Los algoritmos pueden fallar en condiciones no estudiadas o no detectadas durante la entrada de datos " + _
 "o su ejecucion. Programado en OS:Windows7, Proc:AMD Phenom-II Black Edition. " + _
 "Usa Cairo como libreria de graficos, Rtmidi como libreria midi, " + _
 "Editor de c�digo FbEdit. Echo en Freebasic como hobby.FreeBASIC Compiler - Version 1.09.0 (2021-12-31), built for win64 (64bit) " + _
 "Copyright (C) 2004-2021 The FreeBASIC development team. " +_ 
 "standalone" + _
 "mail:galeanoj2005@gmail.com"
 
'------------
Static Shared As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8
 
If ix < 3 And ubirtk=0 And ubiroll=0 And menuabierto=0 Then ' rollmusic CON control
  menuabierto=1 ' evita apertura de mas de un menu
  instancia=0
  hwndC = OpenWindow("RollMusic Ctrl V "+ nroversion,10,10,ANCHOSYSTEM*0.91 ,ALTOSYSTEM*0.91,,WS_EX_ACCEPTFILES   )
''UpdateInfoXserver()

  hwndListBox= ListBoxGadget(3,80,40,290,670,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )
' botton todo o nada , sonido o mudo para todas las pistas
  ButtonGadget(5,60,20,20,20,"S")
  SendMessage(GadgetID(3),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
 ' TextGadget(4,250,10,240,20,, SS_SIMPLE  )
 
' check para encender o apagar, sonido de salida de c/pista  
  cbxnum(1) =  CheckBox_New( 60 ,  40, 20, 20, "",, hwndc) 
  cbxnum(2) =  CheckBox_New( 60 ,  60, 20, 20, "",, hwndc)
  cbxnum(3) =  CheckBox_New( 60 ,  80, 20, 20, "",, hwndc)
  cbxnum(4) =  CheckBox_New( 60 , 100, 20, 20, "",, hwndc)
  cbxnum(5) =  CheckBox_New( 60 , 120, 20, 20, "",, hwndc)
  cbxnum(6) =  CheckBox_New( 60 , 140, 20, 20, "",, hwndc) 
  cbxnum(7) =  CheckBox_New( 60 , 160, 20, 20, "",, hwndc) 
  cbxnum(8) =  CheckBox_New( 60 , 180, 20, 20, "",, hwndc)
  cbxnum(9) =  CheckBox_New( 60 , 200, 20, 20, "",, hwndc)
  cbxnum(10) = CheckBox_New( 60 , 220, 20, 20, "",, hwndc)
  cbxnum(11) = CheckBox_New( 60 , 240, 20, 20, "",, hwndc)
  cbxnum(12) = CheckBox_New( 60 , 260, 20, 20, "",, hwndc)
  cbxnum(13) = CheckBox_New( 60 , 280, 20, 20, "",, hwndc) 
  cbxnum(14) = CheckBox_New( 60 , 300, 20, 20, "",, hwndc) 
  cbxnum(15) = CheckBox_New( 60 , 320, 20, 20, "",, hwndc)
  cbxnum(16) = CheckBox_New( 60 , 340, 20, 20, "",, hwndc)
  cbxnum(17) = CheckBox_New( 60 , 360, 20, 20, "",, hwndc)
  cbxnum(18) = CheckBox_New( 60 , 380, 20, 20, "",, hwndc)
  cbxnum(19) = CheckBox_New( 60 , 400, 20, 20, "",, hwndc) 
  cbxnum(20) = CheckBox_New( 60 , 420, 20, 20, "",, hwndc) 
  cbxnum(21) = CheckBox_New( 60 , 440, 20, 20, "",, hwndc)
  cbxnum(22) = CheckBox_New( 60 , 460, 20, 20, "",, hwndc)
  cbxnum(23) = CheckBox_New( 60 , 480, 20, 20, "",, hwndc)
  cbxnum(24) = CheckBox_New( 60 , 500, 20, 20, "",, hwndc)
  cbxnum(25) = CheckBox_New( 60 , 520, 20, 20, "",, hwndc) 
  cbxnum(26) = CheckBox_New( 60 , 540, 20, 20, "",, hwndc) 
  cbxnum(27) = CheckBox_New( 60 , 560, 20, 20, "",, hwndc)
  cbxnum(28) = CheckBox_New( 60 , 580, 20, 20, "",, hwndc)
  cbxnum(29) = CheckBox_New( 60 , 600, 20, 20, "",, hwndc)
  cbxnum(30) = CheckBox_New( 60 , 620, 20, 20, "",, hwndc)
  cbxnum(31) = CheckBox_New( 60 , 640, 20, 20, "",, hwndc)
  cbxnum(32) = CheckBox_New( 60 , 660, 20, 20, "",, hwndc) 



  EVENTc=0

'---------------------------LISTA DE EJECUCIONES------------
  hwndListEjec= ListBoxGadget(4,430,40,290,670,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )

  ButtonGadget(6,380,20,20,20,"S")


  SendMessage(GadgetID(4),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
' BS_PUSHLIKE se hune el boton al seelccioanrlo
' CHECK PARA ESCUCHAR SONIDO de las ejecuciones desde teclado grabadas
  cbxejec(1) =  CheckBox_New( 380 ,  40, 20, 20, "",, hwndC) 
  cbxejec(2) =  CheckBox_New( 380 ,  60, 20, 20, "",, hwndC)
  cbxejec(3) =  CheckBox_New( 380 ,  80, 20, 20, "",, hwndC)
  cbxejec(4) =  CheckBox_New( 380 , 100, 20, 20, "",, hwndC)
  cbxejec(5) =  CheckBox_New( 380 , 120, 20, 20, "",, hwndC)
  cbxejec(6) =  CheckBox_New( 380 , 140, 20, 20, "",, hwndC) 
  cbxejec(7) =  CheckBox_New( 380 , 160, 20, 20, "",, hwndC) 
  cbxejec(8) =  CheckBox_New( 380 , 180, 20, 20, "",, hwndC)
  cbxejec(9) =  CheckBox_New( 380 , 200, 20, 20, "",, hwndC)
  cbxejec(10) = CheckBox_New( 380 , 220, 20, 20, "",, hwndC)
  cbxejec(11) = CheckBox_New( 380 , 240, 20, 20, "",, hwndC)
  cbxejec(12) = CheckBox_New( 380 , 260, 20, 20, "",, hwndC)
  cbxejec(13) = CheckBox_New( 380 , 280, 20, 20, "",, hwndC) 
  cbxejec(14) = CheckBox_New( 380 , 300, 20, 20, "",, hwndC) 
  cbxejec(15) = CheckBox_New( 380 , 320, 20, 20, "",, hwndC)
  cbxejec(16) = CheckBox_New( 380 , 340, 20, 20, "",, hwndC)
  cbxejec(17) = CheckBox_New( 380 , 360, 20, 20, "",, hwndc)
  cbxejec(18) = CheckBox_New( 380 , 380, 20, 20, "",, hwndc)
  cbxejec(19) = CheckBox_New( 380 , 400, 20, 20, "",, hwndc) 
  cbxejec(20) = CheckBox_New( 380 , 420, 20, 20, "",, hwndc) 
  cbxejec(21) = CheckBox_New( 380 , 440, 20, 20, "",, hwndc)
  cbxejec(22) = CheckBox_New( 380 , 460, 20, 20, "",, hwndc)
  cbxejec(23) = CheckBox_New( 380 , 480, 20, 20, "",, hwndc)
  cbxejec(24) = CheckBox_New( 380 , 500, 20, 20, "",, hwndc)
  cbxejec(25) = CheckBox_New( 380 , 520, 20, 20, "",, hwndc) 
  cbxejec(26) = CheckBox_New( 380 , 540, 20, 20, "",, hwndc) 
  cbxejec(27) = CheckBox_New( 380 , 560, 20, 20, "",, hwndc)
  cbxejec(28) = CheckBox_New( 380 , 580, 20, 20, "",, hwndc)
  cbxejec(29) = CheckBox_New( 380 , 600, 20, 20, "",, hwndc)
  cbxejec(30) = CheckBox_New( 380 , 620, 20, 20, "",, hwndc)
  cbxejec(31) = CheckBox_New( 380 , 640, 20, 20, "",, hwndc)
  cbxejec(32) = CheckBox_New( 380 , 660, 20, 20, "",, hwndc) 
'---------------------------------------------------------
' CHECK PARA GRABAR O SEA ARMA LA PISTA PARA RECIBIR DATOS MIDI-IN
  ButtonGadget(7,410,20,20,20,"G")  
 ' ButtonGadget(8,450,0,100,30,"PARAR",BS_RADIOBUTTON )
 ' ButtonGadget(9,580,0,120,30,"GRABAR",BS_RADIOBUTTON  )
Var IMGP=Load_image(".\recur\Parar.bmp")
Var IMGG=Load_image(".\recur\Grabar.bmp")
Var IMGE=Load_image(".\recur\Ejec.bmp")

' pistas de ejec MIDI-IN
GroupGadget(8,450,0,55,40,"")
ButtonImageGadget(9,450,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(10,490,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(14,530,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )

' pistas manuales 
GroupGadget(13,100,0,55,40,"")
ButtonImageGadget(11,100,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(12,140,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(15,180,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )


 'rbparar = RadioButton_New( 450 , 10, 40, 20, "P",BS_LEFTTEXT , hwndC) '65
 'rbgrabar =RadioButton_New( 500 , 10, 40, 20, "G",, hwndC) ' 80





' checks para habilitar grabacion en una pista de MIDI-IN
  cbxgrab(1) =  CheckBox_New( 410 ,  40, 20, 20, "",, hwndC) 
  cbxgrab(2) =  CheckBox_New( 410 ,  60, 20, 20, "",, hwndC)
  cbxgrab(3) =  CheckBox_New( 410 ,  80, 20, 20, "",, hwndC)
  cbxgrab(4) =  CheckBox_New( 410 , 100, 20, 20, "",, hwndC)
  cbxgrab(5) =  CheckBox_New( 410 , 120, 20, 20, "",, hwndC)
  cbxgrab(6) =  CheckBox_New( 410 , 140, 20, 20, "",, hwndC) 
  cbxgrab(7) =  CheckBox_New( 410 , 160, 20, 20, "",, hwndC) 
  cbxgrab(8) =  CheckBox_New( 410 , 180, 20, 20, "",, hwndC)
  cbxgrab(9) =  CheckBox_New( 410 , 200, 20, 20, "",, hwndC)
  cbxgrab(10) = CheckBox_New( 410 , 220, 20, 20, "",, hwndC)
  cbxgrab(11) = CheckBox_New( 410 , 240, 20, 20, "",, hwndC)
  cbxgrab(12) = CheckBox_New( 410 , 260, 20, 20, "",, hwndC)
  cbxgrab(13) = CheckBox_New( 410 , 280, 20, 20, "",, hwndC) 
  cbxgrab(14) = CheckBox_New( 410 , 300, 20, 20, "",, hwndC) 
  cbxgrab(15) = CheckBox_New( 410 , 320, 20, 20, "",, hwndC)
  cbxgrab(16) = CheckBox_New( 410 , 340, 20, 20, "",, hwndC)
  cbxgrab(17) = CheckBox_New( 410 , 360, 20, 20, "",, hwndc)
  cbxgrab(18) = CheckBox_New( 410 , 380, 20, 20, "",, hwndc)
  cbxgrab(19) = CheckBox_New( 410 , 400, 20, 20, "",, hwndc) 
  cbxgrab(20) = CheckBox_New( 410 , 420, 20, 20, "",, hwndc) 
  cbxgrab(21) = CheckBox_New( 410 , 440, 20, 20, "",, hwndc)
  cbxgrab(22) = CheckBox_New( 410 , 460, 20, 20, "",, hwndc)
  cbxgrab(23) = CheckBox_New( 410 , 480, 20, 20, "",, hwndc)
  cbxgrab(24) = CheckBox_New( 410 , 500, 20, 20, "",, hwndc)
  cbxgrab(25) = CheckBox_New( 410 , 520, 20, 20, "",, hwndc) 
  cbxgrab(26) = CheckBox_New( 410 , 540, 20, 20, "",, hwndc) 
  cbxgrab(27) = CheckBox_New( 410 , 560, 20, 20, "",, hwndc)
  cbxgrab(28) = CheckBox_New( 410 , 580, 20, 20, "",, hwndc)
  cbxgrab(29) = CheckBox_New( 410 , 600, 20, 20, "",, hwndc)
  cbxgrab(30) = CheckBox_New( 410 , 620, 20, 20, "",, hwndc)
  cbxgrab(31) = CheckBox_New( 410 , 640, 20, 20, "",, hwndc)
  cbxgrab(32) = CheckBox_New( 410 , 660, 20, 20, "",, hwndc) 


'---------------------------
'StatusBarGadget(1,"StatusBarGadget")

  hMessages=Create_Menu()
  MenName1=MenuTitle(hMessages,"Archivo")
  MenName2=MenuTitle(hMessages,"Nueva Cancion")
  MenName3=MenuTitle(hMessages,"Crear Pistas")
  MenName4=MenuTitle(hMessages,"Ver")
  MenName5=MenuTitle(hMessages,"Cambiar Tiempo Y Ritmo")
  MenName6=MenuTitle(hMessages,"Reproducir")
  MenName7=MenuTitle(hMessages,"Opciones")
  MenName8=MenuTitle(hMessages,"Info")

MenuItem(1005,MenName1, "Na.Cargar archivo de Cancion")
MenuItem(1006,MenName1, "Cargar directorio de Cancion con Pistas separados")
MenuItem(1007,MenName1, "Grabar Cancion")
MenuItem(1008,MenName1, "Na.Grabar Cancion Como")
MenuItem(1009,MenName1, "Na.Exportar Cancion a midi")
MenuItem(1010,MenName1, "Cargar una Pista (rtk � roll) externa en Cancion")
MenuItem(1011,MenName1, "Grabar una Pista de la Cancion con modificaciones, carga pista si no hubiera cargada")
MenuItem(1012,MenName1, "Copia una pista a otra  nueva en cancion")
MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "Grabar una Pista rtk a roll TrackaRoll")
Menubar(MenName1)
'''MenuItem(1015,MenName1, "Grabar Pistas MIDI-IN")
MenuItem(1016,MenName1, "Cargar Pistas MIDI-IN")
MenuItem(1019,MenName1, "Salir")


MenuItem(1020,MenName2, "Nombre o T�tulo (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "Tiempo I=60 por omision")
MenuItem(1022,MenName2, "Na.Ritmo 4/4 por omision")
MenuItem(1023,MenName2, "Na.Duracion Estimada Min.(Por Omision 3 estimada)")
MenuItem(1024,MenName2, "Na.Crear Cancion en un solo archivo")
MenuItem(1025,MenName2, "Crear un directorio de Cancion con Pistas separadas")
MenuItem(1026,MenName2, "Na.Ver Lista Tracks de la Cancion (Nombre y numero)")
MenuItem(1027,MenName2, "Na.Modificar Nombre de Pistas de Cancion")


MenuItem(1028,MenName3, "Cambia Octavas, si rango es mayor al anterior, se borran datos  (0,1,2,3,4,5,6,7,8)")
MenuItem(1029,MenName3, "Na.Seleccion rango de 3 octava repetidas 2 veces ")
MenuItem(1030,MenName3, "Na.Octavas de Instrumetnos Estandares")
MenuItem(1031,MenName3, "Na.Seleccion Canal (futuro se repetira por comodidad...)")
MenuItem(1040,MenName3, "Cambia Instrumento por orden Alfabetico")
MenuItem(1050,MenName3, "Cambia Instrumento por orden Num�rico")
MenuItem(1060,MenName3, "Crea pista aislada con lo elegido y reemplaza la existente en la edicion")
MenuItem(1061,MenName3, "Crear Pista en la Cancion en Edicion, Con lo elegido")
MenuItem(1062,MenName3, "Crear Instancia de RollMusic Sin Control alguno Con lo elegido")



MenuItem(1070,MenName4,"Ver Escalas auxiliares ajustadas", MF_CHECKED)
MenuItem(1071,MenName4,"Ver Cifrado de Acordes", MF_CHECKED)
  
MenuItem(1080,MenName5,"TEMPO")
MenuItem(1081,MenName5,"Factor para Aumentar velocidad de ejecucion, No se graba en archivo 1,5 o 0,5 etc")
MenuItem(1082,MenName5,"Na. TEMPO por nombres, Lento,adagio etc y control fino")
MenuItem(1083,MenName5,"Na. TEMPO insertar cambio de tempo")
MenuItem(1084,MenName5,"Na. TEMPO borrar cambio de tempo")
MenuItem(1085,MenName5,"Na. TEMPO ver marcas de cambio de tempo")
MenuItem(1086,MenName5,"Na. TEMPO ocultar marcas de tempo")
MenuItem(1087,MenName5,"Na. TEMPO incremento de tempo gradual alcanzado en N compases")

/' futuro agregar limite menor de c/rango con opcion de incrementarlo hasta el tope
  d esu rango 
Negras por minuto	 tempo
40-43	Grave
44-47	Largo
48-51	Larghetto
52-54	Adagio
55-65	Andante
66-69	Andantino
70-95	Moderato
96-112	Allegretto
113-120	Allegro
121-140	Vivace
141-175	Presto
176-208	Prestissimo
'/    

  
MenuItem(1090,MenName6,"Reproducir desde la posicion o en el rango ajustado")
MenuItem(1091,MenName6,"Fijar Repeticiones de un numero de Compases elegido como zona")
MenuItem(1092,MenName6,"Reproducir MIDI-IN (teclado) por  MIDI-OUT.  ")
MenuItem(1093,MenName6,"Detener Reproduccion MIDI-IN (teclado) por  MIDI-OUT. (test de Input) ")


MenuItem(1100,MenName7,"Usar MARCO de Ventana ",MF_UNCHECKED)
MenuItem(1101,MenName7,"Usar MARCO de Ventana en instancias",MF_UNCHECKED)

MenuItem(1102,MenName7,"Fracciona Acorde [Con <> Duraciones], notas similares en una pista (no hay silencios)",MF_UNCHECKED  )
MenuItem(1103,MenName7,"Fracciona NOTA o Acorde [CDD], agregando silencios en una pista ",MF_UNCHECKED  )
MenuItem(1104,MenName7,"Fracciona [CDD], notas automaticamente en una pista ",MF_CHECKED  )
MenuItem(1105,MenName7,"No Fraccionar, NO Usar Acordes iguales ", MF_UNCHECKED )
MENUITEM(1106,MenName7,"Seleccionar TIPO DE ESCALA PRINCIPAL de la PISTA (Por omision Mayor)")
MENUITEM(1107,MenName7,"Seleccionar NOTA DE LA ESCALA ESCALA PRINCIPAL DE LA PISTA (Por omision C )")
MENUITEM(1108,MenName7,"Trabajar con sostenidos (Por omision Sostenidos #)",MF_CHECKED )
MENUITEM(1109,MenName7,"Trabajar con bemoles ",MF_UNCHECKED )
MenuItem(1111,MenName7,"Insertar escala libre en la Posicion actual (Pasozona1)")
MenuItem(1112,MenName7,"Insertar escala Alternativa de la Principal en la Posicion actual (Pasozona1)")
MenuItem(1113,MenName7,"Usar metronomo para Tocar MIDI-IN)",MF_CHECKED)
MenuItem(1110,MenName8,"Acerca de")
End If
' default de FRACCIOANR autodur 
   usarAcordesIguales=1
   TipoFrac="autodur"
metronomo_si=1
usarmarcoins=0
usarmarco=0 
If com_usarmarco =0 Then
   usarmarco=0
   usarmarcoOld=0   
Else
   usarmarco=com_usarmarco  
   usarmarcoOld=usarmarco
EndIf
' condicion inicial para ver o no escalas auxiliares en el grafico
Select Case nVerEscalasAuxiliares
  Case 0             
       SetStateMenu(hMessages,1070,0)
  Case 3
       SetStateMenu(hMessages,1070,3)
End Select

' condicion inicial para ver o no cifrado acorde  en el grafico
Select Case nVerCifradoAcordes
  Case 0             
       SetStateMenu(hMessages,1071,0)
  Case 3
       SetStateMenu(hMessages,1071,3)
End Select


'AddKeyboardShortcut(hwndC,FCONTROL,VK_A,1006) 'CTRL+A ABRIR PISTAS

' opengl funciona bien, en futuro usare opnegl para otro roll grafico adicional
' o cambiar el actual. Tal vez para mostrar un roll comun de barras horizontales
' al grabar midi desde teclado (muchos meses adelante ja, 
' ideas sobran tiempo no hay y organizar el codigo de a poco optimizarlo
'usar listas enlazadas ? podria ser, faltaria ahcer un prototipo a ver si es
'rapido o no ventajas y desventajas o hacer un mix com arrays)
/'
sub FramebuffersizefunCB GLFWCALLBACK (win As GLFWwindow ptr, w as long, h as long)
  'print "FramebuffersizefunCB " & w & " x " & h
  glViewport(0,0,w,h)
end sub

Sub correwin( vwin As GLFWwindow Ptr, ta As Any Ptr)
glfwMakeContextCurrent(vwin) 
glfwSetFramebufferSizeCallback(vwin,@FramebuffersizefunCB)

While glfwWindowShouldClose(vwin)=GL_FALSE andalso glfwGetKey(vwin,GLFW_KEY_ESCAPE)=GL_FALSE
glfwMakeContextCurrent(vwin)

' press [ESC] or close the window with the mouse or press [ALT] + [F4]
'while glfwWindowShouldClose(win2)=GL_FALSE andalso glfwGetKey(win,GLFW_KEY_ESCAPE)=GL_FALSE
  dim as double t=glfwGetTime()
  glClearColor(.5+cos(t)*.5, 0, 0,1)
  glClear(GL_COLOR_BUFFER_BIT)
  glfwSwapBuffers(vwin)
  glfwPollEvents() ' handle OS events (window,keyboard,joystick,mouse ...)
  sleep 20
Wend
    Close
    End 0


End Sub
'/
'---------------------------- main -----------------
#define EventKeyDown &h100
' Dim Shared As Any Ptr mutex no se usa todavia exoermental de sync
' Dim Shared As Any Ptr cond
' Dim Shared As String txt
' Dim As Any Ptr pt
' Dim Shared As Integer ok = 0
Dim  As Integer Terminar=0, gi = 0

'Print #1,"ANTES ANCHO , ALTO ", ANCHO, ALTO
If mxold > 0 Then

'MoveWindow( hWnd , 1, 1 , ANCHO - mxold, ALTO - myold, TRUE )
'Print #1,"rollmusic.bas 742: ANCHO,nancho ",ANCHO, nancho
  If ANCHO = nancho Then
  ANCHO= nancho -mxold 
  EndIf
'Print #1,"rollmusic.bas 745: ANCHO resultante  ",ANCHO

  AnchoInicial=ANCHO
  If anchofig=0 Then
    anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
  EndIf
  NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tama�o figuras, nota guia 6 columnas "B_8_[ "
  ANCHO3div4 = ANCHO *3 / 4
  gap1= anchofig* 2315/1000 ' 81 default
  gap2= (914 * gap1) /1000 ' 74 default
  gap3= (519 * gap1) /1000 ' 42 default
  mxold=0
EndIf
If myold > 0 Then
  If ALTO = nalto Then
    ALTO= nalto -myold 
  EndIf

 AltoInicial=ALTO
 myold=0
EndIf
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


'Print #1,"nfont, nmxold, nmyold, nancho,nalto  ",nfont, nmxold, nmyold,nancho,nalto
    param.ancho = ANCHO 
    param.alto = ALTO
'Print #1,"DESPUES ANCHO , ALTO ", ANCHO, ALTO
'''mxold=0:myold=0

abrirRoll=0
'pistacreada=0
Dim As Integer k=0


'------------
Do
  COMEDIT = False
param.titulo ="RollMusic Ctrl V "+ nroversion
'Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
'Print #1,"inicio ubound roll.trk ", UBound(param.Roll.trk,2)
'Print #1,"iniio lbound roll.trk ", LBound(param.Roll.trk,2)
'Print #1, "abrirRoll=1 And cargacancion=1 ",abrirRoll,cargacancion

  If abrirRoll=1 And cargacancion=1 Then
     abrirRoll=0
  '   Print #1," ENTRA A CARGAR PISTAS  cargaCancion = ",cargaCancion
     param.encancion=0
     ResetAllListBox(3)
     Resetear (pmTk()) 

      CargarPistasEnCancion ()
 '   Print #1,"CARGAR PISTAS cargacancion = ",cargaCancion 
     ''CANCIONCARGADA=TRUE
     ROLLCARGADO=False
     '''lo hace tab-cargaCancion=0
     param.encancion=1
     
   If pid1=0 And ix < 3 Then
      pid1=pd1
   EndIf
 ' Print #1,"cALL rOLLLOOP I) cargaCancion ES 1 SI O SI ",cargaCancion
   If CANCIONCARGADA=True  Then
     ntk=0 '16-03-2022
    threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))

   Else     ''''''''RollLoop ( param) '<--con esto anda
     cargacancion=0
   EndIf 
 'Print #1,"ENTRA A CARGAR PISTAS cargaCancion ES 1 SI O SI ",cargaCancion   
    ''''cargacancion=0 esto me ponia en cero antes que lo use el thread!!!!
    ''' RollLoop(param)
    ''Sleep 200 ' NO HACE FALTA AHORA sin este retardo no le da teimpo al thread de cargar a Roll
    abrirRoll=0
  Else
    If abrirRoll=1 And cargacancion=0 Then
       CANCIONCARGADA=False
       ''cargaCancion=0  
       param.encancion=0 
       Print #1,"cALL rOLLLOOP II) "
       threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
       ''RollLoop ( param)
       abrirRoll=0
    EndIf
  EndIf


     
  If ix < 3 Then 
    instancia=0
'PREPARADO PARA EL FUTURO OTRA PANTALLA GRAFICA OPENGL
 ''win = glfwCreateWindow(800,600,"Track OPENGL" )
'' Dim ta As Any Ptr = threadcall correwin(win,ta)
    Do
     eventC=WaitEvent()
'WindowStartDraw(hwndC)
'  fillrectdraw(40,40,&h888888)
'  TextDraw(10,10,NombreCancion,-1,&hff0000)
'StopDraw
   
     Select Case EVENTC 
       Case EventMenu
  If NombreCancion > "" And S5=0 Then 
     SetForegroundWindow(hwndC)
  EndIf
         Select Case EventNumber

           Case 1006   '<=========== CARGAR CANCION
             'cargamso todos los tracks
             ' ok anda bien, una vez cagados se permuta en memoria con TAB
             ' o haciedno click en la lista
               nombre=""
              ROLLCARGADO=False
             Sleep 20
             If NombreCancion > "" And cargaCancion=0 Then 
                  NombreCancion = ""
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  cargarDirectorioCancion(NombreCancion)
                  CANCIONCARGADA=False
                  ntk=0
                  If Tope >0 Then ' tenia datos se supone q pudo abrir Roll y abrirRoll=0
                     CargarPistasEnCancion ()
                     If tope=0 Then  ' directorio fallido
                        NombreCancion = ""
                        cargacancion=0
                        param.encancion=0
                        abrirRoll=2 ' roll ya esta abierto abre mas abajo
                     Else
                        cargacancion=1
                        param.encancion=1
                        abrirRoll=3 ' para evitar que abra rolloop de nuevo
                     EndIf

                  Else
                  ' si Tope=0 no cargo nada ni abrio roll posiblemente
                     NombreCancion = "" ' serai como una carga inicial...  
                     cargaCancion=1
                     abrirRoll=0 ' para que abra rolloop nunca abrio tal vez
                  EndIf   

             EndIf
             If NombreCancion = "" Then
                nombre=""
                ntk=0
               ' pistacreada=0
                CANCIONCARGADA=False
                cargarDirectorioCancion(NombreCancion)
                param.encancion=1
               If abrirRoll=2 Then ' ver rollloop roll esta cargado vengo a cargar cancion de nuevo
               ' por ejemplo tenia solo un roll abierto
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  CargarPistasEnCancion ()
                  If tope=0 Then
                    NombreCancion = "" ' directorio fallido
                  EndIf
                  CANCIONCARGADA=True
                  param.encancion=1

               EndIf
             EndIf
             
             If abrirRoll=0 And NombreCancion > ""  Then
                abrirRoll=1
                cargaCancion=1
                Print #1,"SALE A CARGAR rOLL 1ERA VEZ ABRIRROLL=1 EXIT DO"
                Exit Do                 
             EndIf
  '           Print #1,"termino 1006 va a abrir Roll"
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1007 '<============ grabar cancion bosquejo
' 26-02-2022 desarrollo           
           If CANCIONCARGADA =True  Then
             Dim As String nombreg
            ROLLCARGADO=False 
           If NombreCancion > ""  Then
               GrabarCancion()
           EndIf
          MenuNew=0           
          carga=1
              
           EndIf
          SetForegroundWindow(hwnd)
           Case 1010 '<================ Cargar Pista externa a cancion
   '        Print #1,"entro a 1010 Cargar Pista externa a cancion"
           ROLLCARGADO=False 
            Dim As String nombreg
            
            getfiles(file,myfilter,"save")
            nombreg=*file.lpstrFile
            If nombreg = "" Then
               Print #1,"exit select por nombreg vacio "
               Exit Select 
            Else
               nombre=nombreg   
            EndIf
            If NombreCancion > ""  Then 
               ImportarPistaExterna() ' estoy en cancion importando  una pista rtk
            EndIf   
          MenuNew=0           
          carga=1
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------            
           Case 1011 ' <======= Grabar una Pista de la Cancion con modificaciones, que son tracks
    '        print #1,"entro a 1011 esto lo hace menu de Roll tambien" '' jmg probar es nuevo...
 ' copiamos logica Rolla Track 
   '         print #1, "Click Grabando a disco pista modif con GrabarRollaTrack ",nombre
            Dim As String nombreg
            ROLLCARGADO=FALSE 
           If NombreCancion > ""  Then
              GrabarRollaTrack(0)
           EndIf
          MenuNew=0           
          carga=1
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1012 ' <====== Grabar Pista Como, Copia una pista a otra  nueva nueva
   '        print #1,"entro a 1012 Grabar Pista Como, Copia una pista a otra  nueva nueva"
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            If nombre = "" Then
               getfiles(file,myfilter,"save")
               nombreg=*file.lpstrFile
               If nombreg = "" Then
                  print #1,"exit select por nombreg vacio "
                  Exit Select 
               Else
                  nombre=nombreg   
               EndIf
           EndIf
           If NombreCancion > ""  Then 
              GrabarCopiadePista() ' estoy en cancion copiando una pista desde otra pista
           EndIf   
          MenuNew=0           
          carga=1
'-----------------------------------------------------------------------
           Case 1014  ' <============= TRACK A ROLL
           TrackaRoll (Track(), ntk , Roll ) ' no usa ubirtk
           GrabarArchivo(0)
           SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1015 '<========== Grabar MIDI-In aca sera para grabar 
   ' no se usa para ejecuciones, la grabacion se hace en STOP
'-----------------------------------------------------------------------
           Case 1016 '<============Cargar MIDI-IN
      Dim As String nombrea,myfil
       print #1,"EN Cargar midi-in nombre ",nombreMidiIn
      Dim As String lugar
       If  NombreCancion= "" Then
lugar= BrowseForFolder( NULL, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, "c:\" )
       Else
          lugar=NombreCancion
       EndIf
          If lugar = "" Then
          Else
''             nombreMidiIn=nombrea
             ''aca hay que hacerun loop
              Print #1,"lugar ";lugar
              CargarPistasEjec lugar, ntkp
  
              Dim j As integer
              For  j=1 To ntkp
                 If  tocaparam(j).nombre > "" Then
'nombre debe estar sin extension,las ejecuciones tienen un orden estricto
' vamos a tenerque igualar la cantidad de ticks en todas las pistas de modo
' que el ordende las pistas sea indistinto,elnumero de la pista ejec esta ensu archivo
'veremos si funciona cualqueira sea el orden en el disco alcargar se ordenara por
' ese numero Toca().orden,si funciona tal vez loaplicariamos a roll (mucho trabajo porahora queda asi) 
 
                   ntoca=j
'             pmTk(j+32).MaxPos=Toca(j).maxpos
                   pmTk(j+32).portout=0
 '             If  Toca(j).maxpos > MaxPos Then
 '                  Maxpos=Toca(j).maxpos
 '             EndIf
               EndIf 
           Next j 
           tocatope=ntoca
      EndIf 
       

'-----------------------------------------------------------------------
           Case 1019  ''<============= SALIR TERMINA ROLL
            terminar=1
             
            Exit Do
'-----------------------------------------------------------------------
           Case 1020 ' <=========== Entrar Nombre o T�tulo de la Cancion     
               NombreCancion = ""
               pathdir=""
               EntrarNombreCancion(NombreCancion)
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1021 ' <=========== Entar Tempo 
             menuOldStr="[TEMPO]"
             nombreArchivo="0"
               thread3= ThreadCall EntrarTeclado()
              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1025 ' <======== Crear un directorio de Cancion con Pistas separadas
               CrearDirCancion (NombreCancion)
               If NombreCancion > "" Then
                  param.encancion=1
               EndIf    
          SetForegroundWindow(hwnd)        
'-----------------------------------------------------------------------
           Case 1028 ' <========== seleccion octavas menores a 1 9 
               seloctava (desde, hasta)
               *po = hasta -1
                posn=1
                Nuevo (Roll,1 )
                param.ubiroll=ubiroll
                param.ubirtk=ubirtk

                posn=0
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1031 ' <========  SELECCION DE CANAL DE LA PISTA (10 DRUMS)
'-----------------------------------------------------------------------               
           Case 1040 ' <========== seleccion de instrumento por orden Alfabetico
               selInstORdenAlfa (instru)
                If CANCIONCARGADA =TRUE  Then
               Else
                  'midisal = midiout(portout)
                  ntk=0
                EndIf
                portsal=pmTk(ntk).portout
                
 ' los canales van de 0 a 15 (1 a 16) no se si en todos los dispositivos
 ' van de 0 a 15 o en alguno de 1 a 16 opto por 0 a 15                
             ''  If pmTk(ntk).canalsalida > 0 Then
            '      ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de neuvo 13-02-2022 �
             ''  EndIf
               If instru=0 Then instru=1 EndIf
                Roll.trk(1,NA).inst= CUByte(instru)
                Track(ntk).trk(1,1).nnn=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg
              If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos > 1 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 1  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  


              MenuNew=0           
              carga=1
             SetForegroundWindow(hwnd)    
'-----------------------------------------------------------------------
           Case 1050 ' <=========== seleccion de instrumento por orden Numerico
               selInstORdenNum (instru)
               If CANCIONCARGADA =TRUE Then
               Else
                 ' midisal = midiout(portout)
                 ntk=0
               EndIf
             ' no se cuadno funciona esto  si midisal y canal tienen valores 
             ' la seleccion de instrumento se ahce tanto par auna pista aislada como no
               portsal=pmTk(ntk).portout
      '         If pmTk(ntk).canalsalida > 0 Then
          '         ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de nuevo
       '        EndIf

               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).nnn =CUByte(instru)
              ' grabar el track 
   '         print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg

              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos > 1 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 1  And ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  

              MenuNew=0           
              carga=1
              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1060 ' <========== crea track y reemplaza al existente en la edicion
               'If ntk=0 Then  ' no se cargo ningun track
               '   *po = hasta -1
               '   posn=1
               '   Nuevo(Roll,1 )
               '   posn=0
                  instruOld=instru
                  Roll.trk(1,NA).inst= CUByte(instru)
                  Track(ntk).trk(1,1).nnn= CUByte(instru)
                  'ChangeProgram ( CUByte (instru) , 0)
               'EndIf   
               If abrirRoll=0 Then
Print #1,"1060 abrirRoll=0 entro"
                  abrirRoll=1
                  cargacancion=0
                  If reiniciar=1 Then
                     ThreadDetach(threadloop)
                     usarmarcoOld=usarmarco
                     reiniciar=1
                  EndIf
                  If reiniciar=0 Then
                     reiniciar=1
                     usarmarcoOld=usarmarco
                  EndIf   
                  Print #1,"sale de 1060 abrirrol,reiniciar, cargacancion ", abrirRoll, reiniciar, cargacancion
                  Exit Do
               EndIf
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1061 ' <====== crear pista en cancion con lo elegido
               
               ntk = CountItemListBox(3)+ 1
   '            Print #1,"creando Pista nto ",ntk
               If ntk > 32 Then
                  print #1,"exit select ntk> 32"
                  Exit Select
               EndIf 

               If instru=0 Then 
                  instru=1
               EndIf
   '            print #1,"instru en 1061 , toma el ultimo ",instru
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
   '            Print #1, "NombrePista en 1061 sin nro track ",NombrePista
   '            print #1,"porque se resetea? pathdir",pathdir
               If CANCIONCARGADA=true Or NombreCancion <> "" Then
                 ' arm� el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
    '           print #1, "NombrePista en 1061",NombrePista
               AddListBoxItem(3, NombrePista)
               
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
    '           print #1,"nombre en 1061",nombre
               CantTicks=1000
    '           Print #1,"CantTicks ",CantTicks
               
               ''' para cuando las pistas esten juntas en un archivo ->ZGrabarTrack(ntk)
               If ntk=1 Then
                  ReDim (Roll.trk ) (1 To CantTicks,NB To NA)
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               Else
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               EndIf
               ' EL TRACK SE CREA AL GrabarRollaTrack y debe tener CantTicks
               NB => 0 + (desde-1) * 13   
               NA => 11 + (hasta-1) * 13  

               titulos(ntk)=nombre
               pmTk(ntk).desde=desde
               pmTk(ntk).hasta=hasta
               pmTk(ntk).NB=NB
               pmTk(ntk).NA=NA                  
               pmTk(ntk).MaxPos=2
               pmTk(ntk).posn=0
               pmTk(ntk).notaold=0                  
               pmTk(ntk).Ticks=4000
               ' usamos encancion=1 para grabar dentro de la cancion

               NombrePista="" 
               posicion=1
               posn=0
               MaxPos=2
               nota=0
               dur=0
               tope=ntk
               *po = hasta -1
               GrabarTrack(ntk)
               abrirRoll=0
               If ntk=1 Then 
                  abrirRoll=1
                  cargacancion=0
                  CANCIONCARGADA=TRUE
               EndIf
               If ntk>=2 Then
                 cargacancion=1
                 abrirRoll=0
                 CANCIONCARGADA=FALSE
               EndIf
               Exit Do                 
               
' FALTA CREAR LA PISTA !!! jmg ERO PUEDO USAR UNA PISTA YA CREADA EN 1011
' la graba igual desde roll parece pero debe ser en orden
          SetForegroundWindow(hwnd)            
'-----------------------------------------------------------------------
           Case 1062 ' <======== crear instancia independiente sin control
 ' ponerle diferente color y/o tama�o para poder distinguirlo adma sde l nombre
 ' estudiar si puedo hacer IPC entre Menus de GUI pero son loop tambien no creo.
             Print #fa1,pd1       
             Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
'           Case 1063
' ThreadDetach(threadloop)
' sefini=1
'cairo_surface_destroy( surface )
' cairo_destroy(c)

'-----------------------------------------------------------------------

           Case 1070
              nVerEscalasAuxiliares=GetStateMenu(hmessages,1070)
              Select Case nVerEscalasAuxiliares 
                     Case  3 
                    nVerEscalasAuxiliares=0
                    SetStateMenu(hmessages,1070,0)
                     Case 0
                    nVerEscalasAuxiliares=3
                    SetStateMenu(hmessages,1070,3)

              End Select
              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1071
              nVerCifradoAcordes=GetStateMenu(hmessages,1071)
              Select Case nVerCifradoAcordes 
                     Case  3 
                    nVerCifradoAcordes=0
                    SetStateMenu(hmessages,1071,0)
                     Case 0
                    nVerCifradoAcordes=3
                    SetStateMenu(hmessages,1071,3)

              End Select
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1080 ' tempo
              nombreArchivo="0"
              menuOldStr="[TEMPO]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1081 ' factor de multi tempo
              nombreArchivo="0"
              menuOldStr="[FACTOR]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1090 ' Reproducir cancion
          
          '    Dim As Any Ptr thplayC = ThreadCall  playCancion(track())
          '    CONTROL1 = 1
              If Cplay = 0 And MaxPos > 1 Then
                 GrabarPenta=0:naco=0:naco2=0
                 CPlay=1
                 If NombreCancion > "" Then
                    If play=1 Or playb=1 Then
                       CONTROL1=1 ' DETIENE EL PLAY VEREMOS
                       playloop=0:playloop2=0
                       play=0 : playb=0
                       Sleep 20
                    EndIf 
    '                Print #1,"USANDO PLAYCANCION"
                    thread1 = ThreadCall  playCancion(Track())
                 EndIf
             EndIf   

             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1091 ' <=========== Repeticiones de un nro de compases
             If pasoZona1 > 0 And pasoZona2 > 0  Then
                menuOldStr="[NROREP]"
                nombreArchivo="0"
                thread3= ThreadCall EntrarTeclado()
             Else
               MessBox ("Repeticiones", "Debe entrar una zona de campases, Ctrl-clik en comienzo y Ctrl-click final")
             EndIf
             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1092
             abrirMIDIin=1
'If abrirMIDIin=1 Then ' grabacion desde teclado ...entr mal los valores por ahora
'   abrirMIDIin=0
              open_port (midiin(pmTk(ntk+32).portin ),pmTk(ntk+32).portin, *nombrein( pmTk(ntk+32).portin ) )
              set_callback midiin(pmTk(ntk+32).portin ), @mycallback, p
' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(pmTk(ntk+32).portin ), 1, 2, 4)
              teclado=1 
              jgrb=0
'------------hace falta abrir la salida
Print #1,"abriendo port...."
Dim k1 As Integer

  
   k1=CInt(pmTk(ntk+32).portout)
    
   Print #1,"midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
     Print #1,"No se usa Microsoft"
   Else
     If listoutAbierto( k1) = 0 Then
        midiout(k1) = rtmidi_out_create_default ( )
        open_port midiout(k1),k1, nombreOut(k1)
        Dim As integer    porterror=Err 
        listoutAbierto( k1) = 1
        Print #1,"abro ",*nombreOut(k1)

    Select Case porterror
      Case RTMIDI_ERROR_WARNING
        Print #1, "RTMIDI_ERROR_WARNING"

      Case RTMIDI_ERROR_DEBUG_WARNING
        Print #1, "RTMIDI_ERROR_DEBUG_WARNING"
        Close
        End

      Case RTMIDI_ERROR_UNSPECIFIED
        Print #1,"RTMIDI_ERROR_UNSPECIFIED"
        Close
        End

      Case RTMIDI_ERROR_NO_DEVICES_FOUND
        Print #1,"RTMIDI_ERROR_NO_DEVICES_FOUND"
        Close
        End

      Case RTMIDI_ERROR_INVALID_DEVICE
        Print #1,"RTMIDI_ERROR_INVALID_DEVICE"
        Close
        End

      Case RTMIDI_ERROR_MEMORY_ERROR
        Print #1,"RTMIDI_ERROR_MEMORY_ERROR"
        Close
        End

      Case RTMIDI_ERROR_INVALID_PARAMETER
        Print #1,"RTMIDI_ERROR_INVALID_PARAMETER"
        Close
        End

      Case RTMIDI_ERROR_INVALID_USE
        Print #1,"RTMIDI_ERROR_INVALID_USE"
        Close
        End

      Case RTMIDI_ERROR_DRIVER_ERROR
        Print #1,"RTMIDI_ERROR_DRIVER_ERROR!"
        Close
        End

      Case RTMIDI_ERROR_SYSTEM_ERROR
        Print #1,"RTMIDI_ERROR_SYSTEM_ERROR"
        Close
        End

      Case RTMIDI_ERROR_THREAD_ERROR
        Print #1,"RTMIDI_ERROR_THREAD_ERROR"
        Close
        End
    End Select
   EndIf
 EndIf 

 Print #1,"Port usando en Play teclado ",portout
Print #1,"-------------------------------------"
''End If

'-------------------------------
           Case 1093
             abrirMIDIin=2
   cancel_callback(midiin(pmTk(ntk+32).portin ))
   Dim k1 As Integer
   k1=pmTk(ntk+32).portout
   Print #1,"midiout ",k1, *nombreOut(k1)
   alloff( pmTk(ntk+32).canalsalida,k1 )  
   listoutAbierto(k1)=0
   close_port midiout(k1)
   teclado=0




'-----------------------------------------------------------------------
           Case 1100 '<======== usar o no, marco de ventana de Roll
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarco
             usarmarco=GetStateMenu (hMessages,1100)
             Select Case usarmarco
               Case 0             
                  SetStateMenu(hMessages,1100,3)
                  usarmarco=3
               Case 3
                  SetStateMenu(hMessages,1100,0)
                  usarmarco=0
             End Select
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1101 ' marco o no marco par ainstancias
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarcoins
             usarmarcoins=GetStateMenu (hMessages,1101)
             Select Case usarmarcoins
               Case 0             
                  SetStateMenu(hMessages,1101,3)
                  usarmarcoins=3
               Case 3
                  SetStateMenu(hMessages,1101,0)
                  usarmarcoins=0
             End Select
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1102
                 usarAcordesIguales=1
                 TipoFrac="igualdur" 
                 SetStateMenu(hmessages,1102,3)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1103
                 usarAcordesIguales=1
                 TipoFrac="tododur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,3)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1104
                 usarAcordesIguales=1
                 TipoFrac="autodur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,3)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1105
                 usarAcordesIguales=0
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,3)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1106 ' <======== escala de la secuencia, similar a la de instrumentos
               pasozona1=0
               selTipoEscala (tipoescala_num_ini)

' GRABADO EN grabaPos(1,1).inst = CUByte(tipoescala) ' 20-12-2021 - tipoescala en uso
' CUADNO QUEIRO UN CAMBIO PUEDO DEJAR UN ACOLUMNA VACIA Y PONER TODO ESTA INFO
' PERO DEBO INDICAR AL PROGRAM QUE SALTEE ESTA COLUMNA CREO CON TENER NOTA=181 Y DUR181
' PODRI AINDICAR ESO DEBO PROBARLO Y USAR LSO DEMAS CAMPOS PARA INTRODUCIR ALGUN  CAMBIO               
'''            Roll.trk(1,NA).vol= CUByte(tipoescala + 127) ' a partir de 128
''               Print #1,"Roll.trk(1,NA).vol ",Roll.trk(1,NA).vol
''               END
''               Track(ntk).trk(1,1).vol=CUByte(tipoescala + 127)
              ' grabar el track 
'' NOTA: LA VARIABLES DE ESCALA DE TODA LA SECUENCIA TIENEN SUBFIJOS _STR O _NUM
'' LAS QUE SON PARA USO DE ESCLAS EN POSICIONES NO LO TIENEN
      '      Print #1,"tipo de escala seleccionado ", tipoescala_num_ini
              
' -------cadena de escala, construye dsde C hay que hacer las otras esclas
    ' C,D,E,F,G,A,B,Bb,Ab,Gb ver las debo pedir escala y 1er nota desde donde empieza uff
      '        Print #1,"armarescla desde 1106"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
' --------------------------   
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1107 ' <======== usamos sostenidos o bemoles ???
              pasozona1=0
              selNotaEscala (notaescala_num_ini)
 
       '       Print #1, "seleccion de Nota de la escala num  ",notaescala_num
       '       Print #1,"armarescla desde 1107"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)

            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1108 '<======= alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="sos" ' grabado en grabaLim(1,1).pan  = CUByte(3)
              SetStateMenu(hmessages,1108,3)  
              SetStateMenu(hmessages,1109,0)
            ' si hay nombre de archivo grabar sino no   
      ''        GrabarArchivo()
       '       Print #1,"armarescla desde 1108"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
          
            SetForegroundWindow(hwnd)
' --------------------------   
           Case 1109 ' <======== alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="bem" ' grabado en grabaLim(1,1).pan  = CUByte(2)
              SetStateMenu(hmessages,1108,0)  
              SetStateMenu(hmessages,1109,3) 
       '       Print #1,"armarescla desde 1109"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
      
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1110
   
             MessBox ("", acercade)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1111 '<========== cambiode escala
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                selTipoEscala (tipoescala_num)
                selNotaEscala (notaescala_num) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala_num
                guiaEscala(indEscala).notaescala=notaescala_num
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1111 TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
             EndIf
            SetForegroundWindow(hwnd)             
'-----------------------------------------------------------------------
           Case 1112 '<========= cambiode a escala Alternativa de la Principal
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                Dim As String notastr
                'tipoescala_num,notaescala_num se pisan de modo q no hace falta inicializar
                EscalaAlternativa (tipoescala_num,notaescala_num) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala_num
                guiaEscala(indEscala).notaescala=notaescala_num
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1112 Alternativa TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
             EndIf
            SetForegroundWindow(hwnd)  
            Case 1113 ' usar metronomo
                
             metronomo_si=GetStateMenu(hmessages,1113)
              Select Case metronomo_si 
                     Case  1 
                    metronomo_si=0
                    SetStateMenu(hmessages,1113,0)
                     Case 0
                    metronomo_si=1
                    SetStateMenu(hmessages,1113,1)

              End Select
              SetForegroundWindow(hwnd)
            Case 1114
                metronomo_si=0
                   
         End Select
'-----------------------------------------------------------------------
       Case eventgadget
              '   SetForegroundWindow(hwndC)
      ' el codigo anterior que traia de disco esta en notas
       If eventnumber()=3 Then  ' VK_LBUTTON ?
         borrapos=0
 ' esto servia cuando cargaba solo la lista y no los tracks en 1006
 ' pero ahroa solo deberia hacer switch no cargar de disco sino
 ' directamente cargar Roll desde el numero de track correspondiente
 ' en memoria       
      '       print #1,"CLICK EN LISTA"
             ROLLCARGADO=FALSE
             CANCIONCARGADA=TRUE
             Dim item As String
             Dim As Integer ubi1,ubi2
          '   item= "                        "
          '   setgadgettext(4,item)
              
             item=GetListBoxText(3,GetItemListBox(3))
             Print #1,"item 1580 ",item
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf
         '    setgadgettext(4,item)
             item=Trim(item)
      '       Print "item ",item
             If item > "" Then
             '  Dim nombre1 As String
             '   nombre1= NombreCancion + "\"+item +".rtk"
             '   print #1," NUEVO eventgadget click en lista nombre", nombre1
              ubirtk=3 ' ahora indice carga desde lista o memoria
             ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
             ' pero si quiero volver a disco solo debo resetear ubirtk=0
              ntk=sacarNtk(item) ' este ntk no sirve para boorar
 ' aca no copia track a Roll
         nombre= titulos(ntk)
      '   Print #1,"ntk, nombre ",ntk, nombre

'--------------------------------------------------------------

   clickpista=1 ' simula SC_TAB el cual carga el track a Roll
 
'--------------------------------------------------------------
' este ntk sirve para identificar el ntk del arcchivo t dle vector
' pero el ntk de la lista es otro vector y al borrar el indice cambia
' debo obtener el indice primero                
'' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT
                If WM_VKEYTOITEM Then '
       '           print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
                If EventKEY = VK_DELETE Then 
       '          print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
        '            print #1,"LISTABOX EventKeyDown borrar ntk",ntk
         '           print #1,"LISTBOX  titulos(ntk)= ",titulos(ntk)
                    copiarATemp (titulos(ntk),pistas(ntk))
                    BorrarPista (titulos(ntk))
                    titulos(ntk)=""
                    pistas(ntk)=""
                    pmTk(ntk).desde=0
                    pmTk(ntk).hasta=0
                    pmTk(ntk).NB=0
                    pmTk(ntk).NA=0                  
                    pmTk(ntk).MaxPos=0
                    pmTk(ntk).posn=0
                    pmTk(ntk).notaold=0                  
                    pmTk(ntk).Ticks=0
                    pmTk(ntk).portout=portout
                    Sleep 1
                    'SetItemListBox(3,ind+1) no funca
                    'SetGadgetState(3,1) no funca 
                    borrar=0
                  EndIf
                EndIf 
 
           ' aca no debe leer a disco solo conmutar de track en track
            EndIf
                                  
         '       Print #1," CLICK EN LISTA FIN "
                  
             EndIf
  
       EndIf

       If eventnumber()=5 Then
       
         If cntsuena =0 Then
            SuenaTodo=0
         EndIf

         If cntsuena =Tope And suenaTodo >= 1 Then
            SuenaTodo=1
            cntsuena=0
         EndIf
 
         Select Case  SuenaTodo
            Case 0
             SuenaTodo=1
           Case Is >= 1
             SuenaTodo=0
         End Select
         For i=1 To tope 
            CheckBox_SetCheck(ByVal cbxnum(i), SuenaTodo)
            cntsuena+=SuenaTodo
          Next i
         SuenaTodo=3
       EndIf
'------------------
' revisar CheckBox_GetCheck de las ejecuciones

'//////////////// BOTON ROJO GRABA EJEC //////////////////

      If eventnumber()= 10 And GrabarEjec=0 Then ' BOTON GRABAR ROJO
         jgrb=0:repro=0
         For k=1 To 32 
           If CheckBox_GetCheck( cbxgrab(k))= 1 Then 
              ntoca=k 'ntoca es la  pista ejec que se esta grabando
             
           EndIf
         Next k
         tocatope=tocatope+1
         pmTk(ntoca+32).MaxPos=0
         
' mil negras a I=60 son 192 * mil ticks (16 minutos a I=60)
' a i=240 todo *4---192*4*1000=768000(16min a I=240)
'la idea es que el usuario grabe a I=60 o I=120 384000
' pero cada nota requiere 2 eventos on y off se multiplicaria por 2
         ReDim (Toca(ntoca).trk ) (1 To 384000)  'I=120
                     tocaparam(ntoca).delta=0
                     tocaparam(ntoca).nombre =""
                     tocaparam(ntoca).maxpos =0
                     tocaparam(ntoca).orden=0
                    tocaparam(ntoca).orden=ntoca
         Redim  CargaIn (1 To 4000) 
         pmTk(ntoca+32).portout=0
         SetGadgetstate(9,0)
         GrabarEjec=1
         arrancaPlay=0
         EntrarNombrePista  tocaparam(ntoca).nombre
          ntkp=ntoca 
         AddListBoxItem(4, tocaparam(ntoca).nombre,ntoca-1)
         If   NombreCancion >"" Then        
             Titulos(ntkp+32)=NombreCancion+"\("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
         else
             Titulos(ntkp+32)="("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
         EndIf
      EndIf

'//////////////// BOTON NEGRO STOP EJEC  //////////////////


      If eventnumber()= 9 Then ' BOTON STOP NEGRO DE MIDI-IN
         SetGadgetstate(10,0)
         If GrabarEjec=1 Then
            Print #1,"STOP:pmTk(ntoca+32).MaxPos ",pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).orden=ntoca
   '         Print #1,"stop MaxPos ",pmTk(ntoca).MaxPos
            GrabarEjec=0
            repro=0
            arrancaPlay=0

' -------cargamos toca
          k=0
         Dim As Integer i1=1, j =0, partes, pj
   ' tocaparam(ntoca).delta toma valor desde la 2dapista grabada  en PlayTocaAll
         If  tocaparam(ntoca).delta > 0 And ntoca >1 Then
             partes=tocaparam(ntoca).delta/TickChico
             Print #1,"STOP: numero de partes de retardo ",partes 
             k=partes
             pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos+partes
'             Toca(ntoca).maxpos=pmTk(ntoca+32).MaxPos
'             print #1,"STOP Toca(ntoca).maxpos, ntoca ",Toca(ntoca).maxpos,ntoca
             For pj=1 To partes 
               Toca(ntoca).trk(pj).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(pj).nota = 0
               Toca(ntoca).trk(pj).vel  = 0
              Next pj

         EndIf
          k=partes+1
         Do 
           if k=pmTk(ntoca+32).MaxPos+1  Then
              Exit Do
           EndIf  
     '  Print #1,"CargaIn(i1).modo ",CargaIn(i1).modo
            Select Case  CargaIn( i1).modo
               Case 144,128
            Toca(ntoca).trk(k).modo = CargaIn( i1).modo
            Toca(ntoca).trk(k).nota = CargaIn( i1).nota
            Toca(ntoca).trk(k).vel  = CargaIn( i1).vel
                 i1 = i1 +1    
              If i1=jgrb+1 Then
                 Exit Do 
              EndIf           
           End Select
           If CargaIn( i1).partes > 0 Then
           '   Print #1,"CargaIn(i1).partes ",CargaIn(i1).partes
              For j=1 To CargaIn( i1).partes 
                k=k+1
                
               Toca(ntoca).trk(k).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' un retardo de tick
               Toca(ntoca).trk(k).vel  = 0
            
              Next j
           Else
             k=k+1
               Toca(ntoca).trk(k).modo = 0 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' no hay retardo de tick
               Toca(ntoca).trk(k).vel  = 0
                 
           EndIf
          k=k+1
         Loop
             jgrb=0
'----------------------grabar archivo de pista 
      Dim As String nombreg,myfil
  ' una cosa es grabar una pista y otra todas las pistas
  ' aca estamos grabando  una  sola pista,la marca da con G alpulsar el Boton Rojo
' esta funcion deberia estar en STOP 
      print #1,"EN Grabar midi-in nombre ",tocaparam(ntoca).nombre
''        nombreMidiIn=Toca(ntoca).nombre
   'y el path dondeesta? cuadnograbopareceque no 
  ' al cargar si debo afinar eso....   
      Dim As ejec toc(1 To tocaparam(ntoca).maxpos)
      Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",ntoca   
      Print #1,"tocaparam(ntoca).maxpos),ntoca ",tocaparam(ntoca).maxpos, ntoca
    
       For j As Integer =1 To   tocaparam(ntoca).maxpos
              toc(j).modo=Toca(ntoca).trk(j).modo
              toc(j).nota=Toca(ntoca).trk(j).nota
              toc(j).vel=Toca(ntoca).trk(j).vel
            Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
       Next j
      Dim tocap As ejecparam = tocaparam(ntoca)
      Print #1,"PARAMETROS EJEC nombre ",tocaparam(ntoca).nombre
      Print #1,"PARAMETROS EJEC mapos ",tocaparam(ntoca).maxpos
      Print #1,"PARAMETROS EJEC orden ",tocaparam(ntoca).orden
      Print #1,"PARAMETROS EJEC delta ",tocaparam(ntoca).delta
      maxgrb=tocap.maxpos
' para una sola pista grabada el maxgrb es el maxpos de esa pista
      If  maxgrb > 0 And ntoca > 1 Then
          If  maxgrb <  tocap.maxpos Then ' para cada pista se define tocap de nuevo  
              maxgrb=tocap.maxpos
'------------adapto la grabacion anterior a mayo rlongitud              
               ReDim  Preserve (Toca(ntoca-1).trk) (1 To maxgrb)
''''           pmTk(ntoca-1 +32).MaxPos=maxgrb ' conservo la long original
' el asunto es que igualo las  longitudes para que no reviente el play, pero
' mantengo las longitudes para menor espacio en disco al grabar
' al cargar reconstruyo en memoria igualo todo de nuevo         
          EndIf
      EndIf 
      GrabarMidiIn (toc(), tocatope,tocap)   
         Else
            CONTROL1=1  
         EndIf


'----------------
      EndIf

'//////////////// BOTON VERDE PLAY EJEC //////////////////

      If eventnumber()= 14 And repro=0 Then ' BOTON PLAY VERDE DE MIDI-IN
            repro=1
            CONTROL1=0
            Dim p As Integer Ptr
            p=@ntoca
            t1play=Timer
            threadG  = ThreadCreate (@PlayTocaAll, p)
             '' PlayTocaAll(p)
      EndIf
'//////////////// BOTON ROJO GRABAR EN PENTA //////////////////

      If eventnumber()= 15 Then 
         GrabarPenta=1
      EndIf 
'-------------------------------
      If eventnumber()= 11 Then
         SetGadgetstate(12,0)
         GrabarPenta=0
''      If NombreCancion > "" Then ' detiene todo pista aisalda o cancion 
            If play=1 Or playb=1 Or CPlay=1 Then
               CONTROL1=1 ' DETIENE EL PLAY 
               playloop=0:playloop2=0
               play=0 : playb=0:CPlay=0
               Sleep 20
            EndIf 
      ' EndIf
      EndIf   

      If eventnumber()= 12 Then
         SetGadgetstate(11,0)
         If Cplay = 0 And MaxPos > 1 Then
            CPlay=1
            If NombreCancion > "" Then
               If play=1 Or playb=1 Then
                  CONTROL1=1 ' DETIENE EL PLAY 
                  playloop=0:playloop2=0
                  play=0 : playb=0
                  Sleep 20
               EndIf 
               thread1 = ThreadCall  playCancion(Track())
            EndIf
         EndIf   

      EndIf
      SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
       Case EventClose  ''<==== SALIR TERMINA ROLL lax de win control???
        ''si ponemos aca da asercion de cairo.c 
        terminar=1
        Exit Do     
     End Select
    'SetFocus (hwndc) 
    
   Loop
'-----------------------------------------------------------------------
  Else
      param.titulo ="RollMusic Editor" ' esto no sale si no hay marco
  '    Print #1,"cALL rOLLLOOP III)"
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      ThreadWait threadloop
      cerrar(0)  
  End If
'-----------------------------------------------------------------------
  If terminar=1 Then
     Exit Do 
  EndIf
    
Loop
'-----------------------------------------------------------------------
salir() ''<==== SALIR TERMINA ROLL
Sleep 100
Kill "procesos.txt"

'----FIN CONTROL-------------------
'   threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1)) 
'   ThreadWait threadloop
 
'RollLoop ( param)

End 0

'---------fin iup---    
errorhandler:
Dim As Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error  Rollmusic detected ", er, posicion, MaxPos
Print #1,Erl, Erfn,Ermn,Err

Print #1,"------------------------------------"
ErrorNumber = Err
ErrorLine = Erl


ProgError(0) = "No error"
ProgError(1) = "Illegal function call"
ProgError(2) = "File not found signal"
ProgError(3) = "File I/O error"
ProgError(4) = "Out of memory"
ProgError(5) = "Illegal resume"
ProgError(6) = "Out of bounds array access"
ProgError(7) = "Null Pointer Access"
ProgError(8) = "No privileges"
ProgError(9) = "interrupted signal"
ProgError(10) = "illegal instruction signal"
ProgError(11) = "floating point error signal "
ProgError(12) = "segmentation violation signal"
ProgError(13) = "Termination request signal"
ProgError(14) = "abnormal termination signal"
ProgError(15) = "quit request signal"
ProgError(16) = "return without gosub"
ProgError(17) = "end of file"


Print #1,"ERROR = ";ProgError(ErrorNumber); " on line ";ErrorLine
Print #1,"Error Function: "; *Erfn()
'Dim ers As Integer = 12 - nota +(estoyEnOctava ) * 13 
Print #1, "12 -nota +(estoyEnOctava ) * 13) "; ers
Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )


