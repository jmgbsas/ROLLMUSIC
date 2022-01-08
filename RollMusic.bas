' tiempoPatron a entero no tiene porque ser double, se graba en archivo
' Se agrego formar acordes aun sin nota en el lugar elegido, se deb eentrar al duracion
' Triadas desde Tonica completo Mayor Menor Disminuido formacion y play 
' fix consumo cpu S5=2 y fueradefoco=0 eliminado
' Funciona Acorde en Tonica triaca,,Ctrl+clik derecho luego seguir con mayor hasta no inversion
' SE ELIMINO DE 'Q' la configuracion de tamaños, proporciones y font
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
' futuro grabar mxold y algo mas para conservar el tamaño de la ventana y el tamaño del font
' usado por el usuario !!!! OK Y AANDA
' el borrado de columna esta defectuoso hay que dar 0 y luego 12 x en toda la octava para
' que borre mejor usaremos marcas de zona para borrar.
' dejo de andar marcado de zonas porque habia un exit do en Comedit=False con mousex>50
' se movio zonas dentro de mousex> 50 y luefo Comedit=false volvio a funcionar
 ' se intento usar Byte en vez de Ubyte para usar negativos pero hay qu emodificar mucho
 ' se usara IF variabrlee > 127 par ausar por ejemplo Vol > 127 para indicar escalas...
 ' Esta nota base...es Tonica 3era 5ta 7ma ...
 ' uso ctrl+click para ingresar notas nuevas en Edit, sino al pasar a Ctrl-M u otras acciones
 ' entraba notas no deseadas..
 ' 11-12-2021 redusco la camtidad de partes a 20 partes_falta (1 To 20), partes_sobra(1 To 20)
 ' rooloop 2673 menu contextual acordes desarrollo 06-12-2021
 ' correccion Clcik end EDIT 06-12-2021 s3=0 movido a y > 50 
 ' correccion de abrir nota si menor=mayor no hace nada, allevantar click rompia todo
 ' v23 fraccionar automaticamente en comEdit cursor al poner notas menores o 
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
 
#define WIN_INCLUDEALL
#Include Once "windows.bi"
#Include Once "/win/commctrl.bi"
#include "crt/stdio.bi"
#Include "file.bi"

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
' end file dialog  
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
' para GTK Gtk:list()
#Include Once "crt.bi"
#Include Once "gtk/gtk.bi"

' This is our data identification string to store data in list items
Const list_item_data_key ="list_item_data"
' fin GTK
' -- INICIO openGL GLFW 3.1.1 
/'
#Include once "glfw3.bi"
If glfwInit()=GL_FALSE then
  print "error: can't init GLFW"
  beep : sleep : end 1
end If
Dim As GLFWwindow ptr  win
' ----FIN OPENGL 
'/
'===============================
#Include "ROLLDEC.BI"
Dim Shared As Integer pd1, fa1 
pd1 = GetCurrentProcessId()  
Open "midebug"+ "["+Str(pd1)+"]" + ".txt" For Output As #1
print #1,"start"
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
#Include "string.bi"
#Include Once "cairo/cairo.bi"
#Include "midiinfo.bi"
'==============================
'#Include "NOTAS.bi"
#Include "RTMIDIDEC.bi"
'==============================
#Include "ROLLCONTROLDEC.bi"
'=============================
' iup start
#Include once "foro/fmidi.bi"
#Include Once "fbthread.bi"
#Include "foro/window9.bi"

'#Include "crt/win32/unistd.bi"
#inclib "ntdll"
#Include "win/ntdef.bi"
'#Include "nanosleep/mod_nanosleep.bi"
'#Include "nanosleep/mod_nanosleep_dll.bi"
'#Inclib "nanosleep_dll"
''#Include "Afx/windowsxx.bi"

' prueba de secuencia 
Dim Shared fs As Integer 
fs = FreeFile 
Open "secuencia.txt" For Output As #fs

Const NULL = 0
Const NEWLINE = !"\n"
' iup fin


' ROLL PARAEL GRAFICO PERO LOS TRCKS PODRIAN SER DISTINTOS
'Dim Shared As dat Roll  (1 To 128 , 1 To 19200)

' dur debe ser el 1er elemento para que ande bien el sort

' PUEDO TENER UN SHARED DINAMICO GRACIAS A TYPE !!!



'tempo I=160, seri equivalente a O=40 como l maxima cantdad de ticks de O es
' eldela figura mas pequeña=128 ticks...40 * 128 = 5120 por minuto.
' Si deseo un secuecnia de CantMin minutos
tempo=160  ' negra=160
CantMin=15
'NotaBaja=1 : NotaAlta=128

Print #1, "__FB_ARGV__ ",__FB_ARGV__
Print #1, "__FB_ARGC__ ",__FB_ARGC__
'Dim direp As ZString  Ptr
'Dim dires As String
Common Shared As integer ubirtk, ubiroll
Print #1,"__FB_ARGC__ ", __FB_ARGC__
Dim As Integer com_usarmarco =0
For ix = 0 To __FB_ARGC__
  print #1, "arg "; ix; " = '"; Command(ix); "'"''

 If ix=1 Then
  
  'C:\IT\JMGROLL01\[1]AAA.rtk
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
 print #1,"ubirtk ",ubirtk
 print #1,"ubiroll ",ubiroll
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
 print #1,"intervalo no dado usando default!"
 desde => 4  ' 1 3   4 a 8 decia
 hasta => 8  ' 9 7 hasta-1
 'pmTk(ntk).desde=desde
 'pmTk(ntk).hasta=hasta
EndIf

CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...o pasos
'CantTicks=76800
CantTicks=4000 ' 3 MINUTOS A NEGRA 160/min=500 Y Q TODAS SEAN FUSA
' 4000*30=120000 x 16=1920000
'  

Dim Shared As paso compas (1 To CantTicks) 'cada item es la posicion en donde

desdevector = desde
hastavector = hasta
estoyEnOctava =desde
estoyEnOctavaOld =desde
' test test
' --------
NB => 0 + (desde-1) * 13   ' 39 
NA => 11 + (hasta-1) * 13  ' 102

ReDim (Roll.trk ) (1 To CantTicks,NB To NA) ' Roll de trabajo en Pantalla

Print #1,"instru ",instru
' ojo debe se NB al reducir octabas NB cambia
If instru > 0 Then
  Roll.trk(1,NA).inst = CUByte(instru)
EndIf
Print #1,"Roll.trk(1,NA).inst ",Roll.trk(1,NA).inst
Print #1,"NB ",NB
Print #1,"NA ",NA

Print #1,"desde ",desde
Print #1,"hasta ",hasta

param.Roll=Roll
param.ubiroll=ubiroll
param.ubirtk=ubirtk


Dim  AS Integer  ctres=1 ' 5 octavas por track
Dim As Integer lim1 
lim1=1
ReDim (Track(00).trk ) (1 To CantTicks,1 To lim2) ' lo usa instancia sin cancion
ReDim (Track(01).trk ) (1 To CantTicks,1 To lim2) ' lo usa sin instancia
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
' en el momento de la carga de Roll cargar tambien Track ¿?
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
Dim As String driver

posmouseOld = 0:posmouse = 0
comEdit = FALSE:resize = FALSE
po = @octaroll
*po = hasta -1 ' test 09-09-2021 
s1=0:s2=0:s3=0:s4=0:s5=0:s6=0:s7=0:s8=0 
font=18
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
Dim As String sfont,smxold,smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig

ffini=FreeFile
Open "RollMusic.ini" For Input As ffini
Line Input #ffini, sfont
Line Input #ffini, smxold
Line Input #ffini, smyold
Line Input #ffini, sancho
Line Input #ffini, salto
Line Input #ffini, sdeltaip
Line Input #ffini, sVerEscalasAuxiliares
Line Input #ffini, sanchofig
Print #1,"sfont, smxold, smyold,sANCHO,sALTO..  ",sfont, smxold, smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig

Close ffini

nfont=ValInt(sfont)
nmxold=ValInt(smxold)
nmyold=ValInt(smyold)
nancho=ValInt(sancho)
nalto=ValInt(salto)
ndeltaip=ValInt(sdeltaip)
nVerEscalasAuxiliares=ValInt(sVerEscalasAuxiliares)
nanchofig =ValInt(sanchofig)

If nfont > 0 Then
  font=nfont
EndIf
If nmxold > 0 Then
   mxold=nmxold
EndIf
If nmyold > 0 Then
   myold=nmyold
EndIf

If ndeltaip > 0 Then
   inc_Penta=ndeltaip
EndIf
If nanchofig > 0 Then
   gap1= anchofig* 2315/1000
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) - 4 
EndIf
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
 On Error GoTo errorhandler
' enviamos esto a una sub ROLLLoop, creaPenta esta al principio y no tiene declare
' el declare falla si se usa con este tipo de vector udt no se puede usar declare
'stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)

Dim Shared nombreport As ZString Ptr
'========================== 
#Include "RTMIDISUB.bas"
#Include "rolltracks.bas"
#Include "ROLLSUB.BAS"
'===========================
 Dim As Integer MenuFlag=0, LoopFlag=0 

'========================== 
#Include "ROllLoop.BAS"
'========================== 
' aca puedo llamar a varios thread 1 por vez segun el instrumento editado
' o sea 1 vector de roll distinto para casa Thread o llamar a cada Vector
' con el mismo Thread para verlo en pantalla. Se veria 1 por vez o si queremos podriamos
' ver mas de 1 pero apra eso deberia llamar a roll music mas de una vez 
' y eso lo haria desde call roll 
'----------------
#Include "ROLLMIDI.BAS"

'----------------

midiout = rtmidi_out_create_default()
'print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout)
Dim i1 As integer
'Print #1, "portsin  "; portsin
Print #1, "portsout "; portsout
For i1 = 0 to portsout -1 
    nombreport = port_name(midiout, i1)
    print #1, *nombreport
Next i1  

portsout = portout
*nombreport = ""

open_port (midiout,portsout, nombreport)

'print #1,"  "
If Roll.trk(1,NA).inst > 0 Then
 ChangeProgram ( Roll.trk(1,NA).inst , 0)
EndIf
 print #1,"ChangeProgram inst ", Roll.trk(1,NA).inst
Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
Print #1,"INSTANCIA ",instancia
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

 
'------------
Dim As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8
If ix < 3 Then ' rollmusic CON control
  instancia=0
  hwndC = OpenWindow("RollMusic Control ver 0.4.3.2",10,10,anchoK*3/4,alto*4/5,,WS_EX_ACCEPTFILES   )
''UpdateInfoXserver()
  hwndListBox= ListBoxGadget(3,10,10,240,650,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )


  SendMessage(GadgetID(3),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
  TextGadget(4,250,10,240,20,, SS_SIMPLE  )
 ' GetTextExtentPoint32 PARA DETERMINAR EL ANCHO EN PIXELS DE UN TEXTO
 ' EL SCROLL VERTICAL APARECE CUANDO SE SOBREPASA LSO ITEM QUE SE PUEDEN VER 

 

  EVENTc=0

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
MenuItem(1007,MenName1, "Na.Grabar Cancion")
MenuItem(1008,MenName1, "Na.Grabar Cancion Como")
MenuItem(1009,MenName1, "Na.Exportar Cancion a midi")
MenuItem(1010,MenName1, "Cargar una Pista (rtk ó roll) externa en Cancion")
MenuItem(1011,MenName1, "Grabar una Pista de la Cancion con modificaciones, carga pista si no hubiera cargada")
MenuItem(1012,MenName1, "Copia una pista a otra  nueva en cancion")
MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "Salir")


MenuItem(1020,MenName2, "Nombre o Título (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "Tiempo I=60 por omision")
MenuItem(1022,MenName2, "Na.Ritmo 4/4 por omision")
MenuItem(1023,MenName2, "Na.Duracion Estimada Min.(Por Omision 3 estimada)")
MenuItem(1024,MenName2, "Na.Crear Cancion en un solo archivo")
MenuItem(1025,MenName2, "Crear un directorio de Cancion con Pistas separadas")
MenuItem(1026,MenName2, "Na.Ver Lista Tracks de la Cancion (Nombre y numero)")
MenuItem(1027,MenName2, "Na.Modificar Nombre de Pistas de Cancion")


MenuItem(1028,MenName3, "Cambia Octavas, si rango es mayor al anterior, se borran datos  (-1,0,1,2,3,4,5,6,7)")
MenuItem(1029,MenName3, "Na.Seleccion rango de 3 octava repetidas 2 veces ")
MenuItem(1030,MenName3, "Na.Octavas de Instrumetnos Estandares")
MenuItem(1031,MenName3, "Na.Seleccion Canal")
MenuItem(1040,MenName3, "Cambia Instrumento por orden Alfabetico")
MenuItem(1050,MenName3, "Cambia Instrumento por orden Numérico")
MenuItem(1060,MenName3, "Crea pista aislada con lo elegido y reemplaza la existente en la edicion")
MenuItem(1061,MenName3, "Crear Pista en la Cancion en Edicion, Con lo elegido")
MenuItem(1062,MenName3, "Crear Instancia de RollMusic Sin Control alguno Con lo elegido")



MenuItem(1070,MenName4,"Ver Escalas auxiliares ajustadas", MF_CHECKED)
  
MenuItem(1080,MenName5,"TEMPO")
  
MenuItem(1090,MenName6,"Reproducir desde la posicion o en el rango ajustado")

MenuItem(1100,MenName7,"Usar MARCO de Ventana ",MF_UNCHECKED)
MenuItem(1101,MenName7,"Usar MARCO de Ventana en instancias",MF_UNCHECKED)

MenuItem(1102,MenName7,"Acordes distintos a iguales, Fracciona notas similares en una Columna en una pista (no hay silencios)",MF_UNCHECKED  )
MenuItem(1103,MenName7,"Acordes distintos a iguales, Fracciona todas las notas agregando silencios en una columna en una pista ",MF_UNCHECKED  )
MenuItem(1104,MenName7,"Acordes distintos a iguales, Fracciona notas automaticamente en Columna de una pista ",MF_CHECKED  )
MenuItem(1105,MenName7,"No Fraccionar, NO Usar Acordes iguales ", MF_UNCHECKED )
MENUITEM(1106,MenName7,"Seleccionar TIPO DE ESCALA de la secuencia (Por omision Mayor)")
MENUITEM(1107,MenName7,"Seleccionar NOTA DE LA ESCALA ESCALA (Por omision C )")
MENUITEM(1108,MenName7,"Trabajar con sostenidos (Por omision Sostenidos #)",MF_CHECKED )
MENUITEM(1109,MenName7,"Trabajar con bemoles ",MF_UNCHECKED )
MenuItem(1111,MenName7,"Cambio de escala en la Posicion actual (Pasozona1), se borra todo lo que haya y se salta en la ejecucion, ajustar antes alteracion # o b ")

MenuItem(1110,MenName8,"Acerca de")
End If
' default de FRACCIOANR autodur 
   usarAcordesIguales=1
   TipoFrac="autodur"

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
Dim  As Integer Terminar=0

Print #1,"ANTES ANCHO , ALTO ", ANCHO, ALTO
If mxold > 0 Then

'MoveWindow( hWnd , 1, 1 , ANCHO - mxold, ALTO - myold, TRUE )
Print #1,"rollmusic.bas 742: ANCHO,nancho ",ANCHO, nancho
  If ANCHO = nancho Then
  ANCHO= nancho -mxold 
  EndIf
Print #1,"rollmusic.bas 745: ANCHO resultante  ",ANCHO

  AnchoInicial=ANCHO
  anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
  NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "
  ANCHO3div4 = ANCHO *3 / 4
  gap1= anchofig* 2315/1000 ' 81 default
  gap2= (914 * gap1) /1000 ' 74 default
  gap3= (519 * gap1) /1000 ' 42 default
  mxold=0
EndIf
If myold > 0 Then
  If ALTO = nalto Then
    ALTO= nalto -myold 
  endif

 AltoInicial=ALTO
 myold=0
EndIf
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


Print #1,"nfont, nmxold, nmyold, nancho,nalto  ",nfont, nmxold, nmyold,nancho,nalto
    param.ancho = ANCHO 
    param.alto = ALTO
Print #1,"DESPUES ANCHO , ALTO ", ANCHO, ALTO
'''mxold=0:myold=0

abrirRoll=0


Do
param.titulo ="RollMusic Ver 0.4.4.0"
Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
Print #1,"inicio ubound roll.trk ", UBound(param.Roll.trk,2)
Print #1,"iniio lbound roll.trk ", lBound(param.Roll.trk,2)


  If abrirRoll=1 And cargacancion=1  Then
     CargarPistasEnCancion ()
     CANCIONCARGADA=TRUE
     '''lo hace tab cargaCancion=0
     param.encancion=1
     
   If pid1=0 And ix < 3 Then
      pid1=pd1
   EndIf
    threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
    cargacancion=0
    ''' RollLoop(param)
    ''Sleep 200 ' NO HACE FALTA AHORA sin este retardo no le da teimpo al thread de cargar a Roll
  Else
    If abrirRoll=1 And cargacancion=0 Then
       CANCIONCARGADA=FALSE
       ''cargaCancion=0  
       param.encancion=0 
       threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
    EndIf
  EndIf


     
  If ix < 3 Then 

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
         Select case EventNumber
           Case 1006   ' CARGAR CANCION
             'cargamso todos los tracks
             ' ok anda bien, una vez cagados se permuta en memoria con TAB
             ' o haciedno click en la lista
             If NombreCancion > "" And cargaCancion=0 Then
                NombreCancion = ""
                param.encancion=0
                ResetAllListBox(3)
                Resetear (pmTk()) 
               cargarDirectorioCancion(NombreCancion)
               CANCIONCARGADA=FALSE
               ntk=0
               cargaCancion=1
               CargarPistasEnCancion ()
               cargaCancion=0
               param.encancion=1
             EndIf
             If NombreCancion = "" Then
               cargarDirectorioCancion(NombreCancion)
               param.encancion=1
             EndIf
             
             If abrirRoll=0 And NombreCancion > ""  Then
                abrirRoll=1
                cargaCancion=1
                Exit Do                 
             EndIf
             print #1,"termino 1006 va a abrir Roll"

           Case 1010
           print #1,"entro a 1010 Cargar Pista externa a cancion"
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            
            getfiles(file,myfilter,"save")
            nombreg=*file.lpstrFile
            If nombreg = "" Then
               print #1,"exit select por nombreg vacio "
               Exit Select 
            Else
               nombre=nombreg   
            EndIf
            If NombreCancion > ""  Then 
               ImportarPistaExterna() ' estoy en cancion importando  una pista rtk
            EndIf   
          MenuNew=0           
          carga=1
            
           Case 1011 ' Grabar una Pista de la Cancion con modificaciones, que son tracks
            print #1,"entro a 1011 esto lo hace menu de Roll tambien" '' jmg probar es nuevo...
 ' copiamos logica Rolla Track 
            print #1, "Click Grabando a disco pista modif con RollaTrack ",nombre
            Dim As String nombreg
            ROLLCARGADO=FALSE 
           If NombreCancion > ""  Then
              GrabarRollaTrack(0)
           EndIf
          MenuNew=0           
          carga=1

           Case 1012 ' Grabar Pista Como, Copia una pista a otra  nueva nueva
           print #1,"entro a 1012 Grabar Pista Como, Copia una pista a otra  nueva nueva"
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
          
           Case 1014  ''<==== SALIR TERMINA ROLL
            terminar=1
             
            Exit Do
           Case 1020     
               NombreCancion = ""
               pathdir=""
               EntrarNombreCancion(NombreCancion)
           Case 1021
             menuOldStr="[TEMPO]"
             nombreArchivo="0"
               thread3= ThreadCall EntrarTeclado()
    
           Case 1025
               CrearDirCancion (NombreCancion)
               If NombreCancion > "" Then
                  param.encancion=1
               EndIf            
           Case 1028 ' seleccion octavas menores a 1 9 
               seloctava (desde, hasta)
               *po = hasta -1
                posn=1
                Nuevo (Roll,1 )
                param.ubiroll=ubiroll
                param.ubirtk=ubirtk

                posn=0
           Case 1040 ' seleccion de instrumento por orden Alfabetico
               selInstORdenAlfa (instru)
              ' ChangeProgram ( CUByte (instru) , 0)
               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).inst=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "Click Grabando inst a disco pista con RollaTrack ",nombre
            Dim As String nombreg
              ROLLCARGADO=FALSE 
              If NombreCancion > ""  Then
                GrabarRollaTrack(0)
              EndIf
              MenuNew=0           
              carga=1

                
           Case 1050 ' seleccion de instrumento por orden Numerico
               selInstORdenNum (instru)
              ' ChangeProgram ( CUByte (instru) , 0)
               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).inst=CUByte(instru)
              ' grabar el track 
            print #1, "Click Grabando inst a disco pista con RollaTrack ",nombre
            Dim As String nombreg
              ROLLCARGADO=FALSE 
              If NombreCancion > ""  Then
                GrabarRollaTrack(0)
              EndIf
              MenuNew=0           
              carga=1


           Case 1060 ' crea track y reemplaza al existente en la edicion
               'If ntk=0 Then  ' no se cargo ningun track
               '   *po = hasta -1
               '   posn=1
               '   Nuevo(Roll,1 )
               '   posn=0
                  instruOld=instru
                  Roll.trk(1,NA).inst= CUByte(instru)
                  Track(ntk).trk(1,1).inst=CUByte(instru)
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
           Case 1061
               print #1,"En 1061 crear pista en cancion con lo elegido"
               
               ntk = CountItemListBox(3)+ 1

               If ntk > 32 Then
                  print #1,"exit select ntk> 32"
                  Exit Select
               EndIf 
               print #1,"ntk creado pista nro ", ntk
               If instru=0 Then 
                  instru=1
               EndIf
               print #1,"instru en 1061 , toma el ultimo ",instru
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
               Print #1, "NombrePista en 1061 sin nro track ",NombrePista
               print #1,"porque se resetea? pathdir",pathdir
               If CANCIONCARGADA Or NombreCancion <> "" Then
                 ' armó el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
               print #1, "NombrePista en 1061",NombrePista
              AddListBoxItem(3, NombrePista)
              
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
               print #1,"nombre en 1061",nombre
               ''' para cuando las pistas esten juntas en un archivo ->ZGrabarTrack(ntk)
               ReDim (Roll.trk ) (1 To CantTicks,NB To NA)
               titulos(ntk)=nombre
               pmTk(ntk).desde=desde
               pmTk(ntk).hasta=hasta
               pmTk(ntk).NB=NB
               pmTk(ntk).NA=NA                  
               pmTk(ntk).MaxPos=1
               pmTk(ntk).posn=0
               pmTk(ntk).notaold=0                  
               pmTk(ntk).Ticks=4000
               ' usamos encancion=1 para grabar dentro de la cancion
               GrabarRollaTrack(0)   
               NombrePista="" 
               posicion=0
               posn=0
               MaxPos=1
               nota=0
               dur=0
               If abrirRoll=0 Then 
                  abrirRoll=1
                  Exit Do
               EndIf
' FALTA CREAR LA PISTA !!! jmg ERO PUEDO USAR UNA PISTA YA CREADA EN 1011
' la graba igual desde roll parece pero debe ser en orden            
           Case 1062
 ' ponerle diferente color y/o tamaño para poder distinguirlo adma sde l nombre
 ' estudiar si puedo hacer IPC entre Menus de GUI pero son loop tambien no creo.
             Print #fa1,pd1       
             Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))

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
              
           Case 1080
              nombreArchivo="0"
              menuOldStr="[TEMPO]"
              thread3= ThreadCall EntrarTeclado()
           Case 1090 
          
          '    Dim As Any Ptr thplayC = ThreadCall  playCancion(track())
          '    CONTROL1 = 1
              If Cplay = 0 And MaxPos > 1 Then
                 CPlay=1
                 If CANCIONCARGADA Then
                    Print #1,"USANDO PLAYCANCION"
                    thread1 = ThreadCall  playCancion(Track())
                 EndIf
                 Cplay=0
             EndIf   
      
              menunew=0

           Case 1100
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
                 
           Case 1102
                 usarAcordesIguales=1
                 TipoFrac="igualdur" 
                 SetStateMenu(hmessages,1102,3)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
                 
           Case 1103
                 usarAcordesIguales=1
                 TipoFrac="tododur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,3)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
                 
           Case 1104
                 usarAcordesIguales=1
                 TipoFrac="autodur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,3)
                 SetStateMenu(hmessages,1105,0)
                 

           Case 1105
                 usarAcordesIguales=0
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,3)
                 
           Case 1106 ' escala de la secuencia, similar a la de instrumentos
               selTipoEscala (tipoescala)
               tipoescala_num=tipoescala
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
            Print #1,"tipo de escala seleccionado ", tipoescala_num
              
' -------cadena de escala, construye dsde C hay que hacer las otras esclas
    ' C,D,E,F,G,A,B,Bb,Ab,Gb ver las debo pedir escala y 1er nota desde donde empieza uff
      '        cadenaes=""
              Print #1,"armarescla desde 1106"
              cadenaes="":cadenaes_inicial=""
              armarescala(cadenaes)
              cadenaes_inicial=cadenaes 
              
' --------------------------   
           Case 1107 ' usamos sostenidos o bemoles ???
              selNotaEscala (notaescala)
               notaescala_num=notaescala 
               print #1, "seleccion de Nota de la escala num  ",notaescala_num
      '        cadenaes=""
              Print #1,"armarescla desde 1107"
              cadenaes="":cadenaes_inicial=""
              armarescala(cadenaes)
              cadenaes_inicial=cadenaes 

           Case 1108 ' alteraciones sotenidos o bemoles
              alteracion_str="sos" ' grabado en grabaLim(1,1).pan  = CUByte(3)
              SetStateMenu(hmessages,1108,3)  
              SetStateMenu(hmessages,1109,0)
            ' si hay nombre de archivo grabar sino no   
      ''        GrabarArchivo()
      '        cadenaes=""
              Print #1,"armarescla desde 1108"
              cadenaes="":cadenaes_inicial=""
              armarescala(cadenaes)
              cadenaes_inicial=cadenaes
               

' --------------------------   
           Case 1109 ' alteraciones sotenidos o bemoles
              alteracion_str="bem" ' grabado en grabaLim(1,1).pan  = CUByte(2)
              SetStateMenu(hmessages,1108,0)  
              SetStateMenu(hmessages,1109,3) 
              cadenaes=""
              Print #1,"armarescla desde 1109"
              cadenaes_inicial=""
              armarescala(cadenaes)
              cadenaes_inicial=cadenaes  
            

           Case 1110
   
             MessBox ("", acercade)
           Case 1111 'cambiode escala
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                cadenaes=""
                selTipoEscala (tipoescala)
                selNotaEscala (notaescala) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala
                guiaEscala(indEscala).notaescala=notaescala
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1111 TIPOESCALA NOTAESCALA ",tipoescala, notaescala
            EndIf              
         End Select
       Case eventgadget
      ' el codigo anterior que traia de disco esta en notas
       If eventnumber()=3 Then
         borrapos=0
 ' esto servia cuando cargaba solo la lista y no los tracks en 1006
 ' pero ahroa solo deberia hacer switch no cargar de disco sino
 ' directamente cargar Roll desde el numero de track correspondiente
 ' en memoria       
             print #1,"CLICK EN LISTA"
             ROLLCARGADO=FALSE
             Dim item As String
             Dim As Integer ubi1,ubi2
             item= "                        "
             setgadgettext(4,item)
              
             item=GetListBoxText(3,GetItemListBox(3))
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf
             setgadgettext(4,item)
             item=Trim(item)
             If item > "" Then
                nombre= NombreCancion + "\"+item +".rtk"
                print #1," NUEVO eventgadget click en lista nombre", nombre
              ubirtk=3 ' ahora indice carga desde lista o memoria
             ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
             ' pero si quiero volver a disco solo debo resetear ubirtk=0
                ntk=sacarNtk(item) ' este ntk no sirve para boorar
                Tracks (ntk , 1,Roll)
' este ntk sirve para identificar el ntk del arcchivo t dle vector
' pero el ntk de la lista es otro vector y al borrar el indice cambia
' debo obtener el indice primero                
'' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT  
                If WM_VKEYTOITEM Then '
                  print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
                 If EventKEY = VK_DELETE Then 
                 print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
                    print #1,"LISTABOX EventKeyDown borrar ntk",ntk
                    print #1,"LISTBOX  titulos(ntk)= ",titulos(ntk)
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
                    Sleep 1
                    'SetItemListBox(3,ind+1) no funca
                    'SetGadgetState(3,1) no funca 
                    borrar=0
                  EndIf
                 EndIf 
                Else 
           ' aca no debe leer a disco solo conmutar de track en track
                TrackaRoll (Track(), ntk , Roll ) ' no usa ubirtk
                Sleep 100
                print #1,">>> ntk cargado, nombre ",ntk, nombre
                print #1,"llama a RecalCompas para ntk ",ntk
                ReCalCompas(Roll)
                Sleep 20 
                ''mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
                print #1,"Fin RecalCompas para ntk ",ntk
                item=""
                EndIf  
                print #1," CLICK EN LISTA FIN "
             EndIf 
       EndIf

       Case EventClose  ''<==== SALIR TERMINA ROLL lax de win control???
        ''si ponemos aca da asercion de cairo.c 
        terminar=1
        Exit Do     
     End Select

   Loop
  Else
      param.titulo ="RollMusic Editor" ' esto no sale si no hay marco
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      ThreadWait threadloop
      cerrar(0)  
  End If

  If terminar=1 Then
     Exit Do 
  EndIf
    
Loop

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

 

