' ya reproduce 2 tracks y forma acordes de 2 melodias...
' 2 problemas se agrego inst en posicion 1 no lim2 dado que la snotas se barren de 
' 1 a lim2 y la informacion ya es necesaria para 1 vertical...
' el 1er problema debo diferenciar bien cuadno grabo track nuevo en cancion
' como copia de un track exisente (y aexiste), de un roll grabarlo en cancion
' como track (ya existe), y pro ultimo grabar un track de cancion con sus
' modificaicones,,,sin cancion parece que y ahacia eso o sea grabar un track
' cargado sin cancion su smodificaciones,,,(creo solo consideraba track(0)
' luego pasocol se le agrego en vec al campo inst para tener la info
' del inst en cada nota porque en el sort pueden cambiar de posicion
' ergo la solucion final por ahora sera enviar la info de instrumento 
' para cada nota pero par ano enviar tantos comandos simplemente
' comparo el inst enviado anteriorment eocn el actual si es igual no envio nada
' si es distinto vuelvo a enviar el changeprogram...
' --------------- 
'0.4.5.2: contine en dat ulon pra el time...cancion 17-10-2021
' y agraba bien crear cancion o cargarla y luego elegi run instrumento o no
' y crear una pist aen lacancion con lo elegido , luego de creado se debe grabar en roll
' por ahora se graba al pista en Roll ahi toma el path y el nombre y graba a disco sino
' solo esta en la ista gadget pero no existe fisicamente...ok
' Podria ahce rtodo eso en Control ? no se veremos,,,
' revisar cancion al crear hace lio ...y al crear una pista nueva, solo grabando
' no se da cuenta habria que cargar la cancion de nuevo automaricamente !!!
' lo que pas si creo una nuev apista desde roll y hago roollaTrack silo graba
' en cancion a hroa no se si queda en al lista.
'0.4.5.1: correccion grabacio y lectura add X5 Y5 para pasade 16 bits a 20 bits
' y Cint a Cubyte estaba mal la conversion 
' 1) perfeccionar algo mas el Play de una Pista y luego
' 2) pasar a Play de todos las pistas en simultaneo,para ello se modificara el PlayAll
'  o se hará uno nuevo en vez de barrer vericalmente una sola pista debera barrer
' todas las pistas cargadas. y el putno limite NA o NB será del ultimo Track o Pista
' ejecutado. Luego de terminado eso ,,pero esto se hara con Tracks no con Roll
' o sea debo empezar con un playAll de un Track y luego pasarlo a todas las pistas
' o las que se quieran tocar... 
' ------------------------------
'  -Wc -fstack-usage genera un archivo en runtime *.su con el uso del stack
' por cada funcoin el default total es 1024k, -t 1024 no haria falta colocarlo
' en la linea de comando
' https://gcc.gnu.org/onlinedocs/gnat_ugn/index.html#SEC_Contents
'====> clave DEVF (desarrollo futuro, comentarios en lugares apr adessarrollar mas
' funcionalidades.)
' -------------------------------------------------
' TAREAS PENDIENTES AL 24-10-2021
' HAY QUE PROBAR TODAS LAS FUNCIONES DE EDICION DESPUES DE TANTOS CAMBIOS 
' EN CANCION TRACKS MAXPOS POSN ETC ETC, CREAR CANCION NUEVA, USARLA MODIFICARLA
' DE TODAS LOS MODOS POSIBLES.GRABARLA VOLVERLA A CARGAR ETC..24-10-2021.
' CREAR PLAYTRACKS PARA EL PLAY SIPMULTANEO DE TODA AL CANCION CON OPCION 
' A PONE MUDO UN TRACK O PISTA. PODRIAMOS AGREGAR UNA OPCION DE EDITAR O NO A
' C/PISTA.CHECKS=MUTE/PLAY, EDIT/LOCK. -> 2 RADIO BUTTON PARA C/PISTA
 
#define WIN_INCLUDEALL
#Include Once "windows.bi"
#Include Once "/win/commctrl.bi"
#include "crt/stdio.bi"
#Include "file.bi"


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
myfilter += "All Files"+Chr(0)   +"*.*"+Chr(0)


Common Shared  mensaje As Integer 
' end file dialog  
#Define __FB_WIN64__
#If Defined (__FB_WIN64__) 
#LibPath "C:\msys64\mingw64\lib"
#Else
#LibPath "/usr/lib"
#EndIf
#Define EXTCHAR Chr(255)
#Include "fbgfx.bi"
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
Open "midebug.txt" For Output As #1
Print #1,"start"


'Open "mivector.txt" For Output As #3
'Open "miplayall.txt" For Output As #4
'Open "test-AAAAA.TXT" For Output As #5
'Print #1, "version para ceros!!!!!! "
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
'===============================
#Include "ROLLDEC.BI"
'==============================
'#Include "NOTAS.bi"
#Include "RTMIDIDEC.bi"
'==============================
#Include "ROLLCONTROLDEC.bi"
'=============================
' iup start
#Include once "foro/fmidi.bi"
#Include "fbthread.bi"
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

'Print #1, "__FB_ARGV__ ",__FB_ARGV__
'Print #1, "__FB_ARGC__ ",__FB_ARGC__
'Dim direp As ZString  Ptr
'Dim dires As String
Dim As integer ubirtk, ubiroll
Print #1,"__FB_ARGC__ ", __FB_ARGC__
For ix = 0 To __FB_ARGC__
  Print #1, "arg "; ix; " = '"; Command(ix); "'"''

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

Next ix


If desde = 0 And hasta = 0 Then
 Print #1,"intervalo no dado usando default!"
 desde => 4  ' 1 3   4 a 8 decia
 hasta => 8  ' 9 7 hasta-1
 'pmTk(ntk).desde=desde
 'pmTk(ntk).hasta=hasta
EndIf

CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...o pasos
'CantTicks=76800
CantTicks=4000 ' 3 MINUTOS A NEGRA 160/min=500 Y Q TODAS SEAN FUSA
 

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
ReDim (Track(0).trk ) (1 To CantTicks,1 To lim2) ' lo usa instancia sin cancion
ReDim (Track(1).trk ) (1 To CantTicks,1 To lim2) ' lo usa sin instancia
ReDim (Track(2).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(3).trk ) (1 To ctres,1 To lim1)
ReDim (Track(4).trk ) (1 To ctres,1 To lim1)
ReDim (Track(5).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(6).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(7).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(8).trk ) (1 To ctres,1 To lim1)
ReDim (Track(9).trk ) (1 To ctres,1 To lim1)
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
Print #1, l;" ";figura(l)
Next l
Close
End
'/
''https://www.freebasic.net/forum/viewtopic.php?t=15127
Print #1,"NroCol, ancho, anchofig ",NroCol, ANCHO, anchofig
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

Print #1,"gap1 ",gap1

GetMouse mxold,myold, , MouseButtons

posicion = 1 ' comienzo del roll
'indice   = 1  ' numero de nota al comienzo del programa B8
espacio = 0
backspace = 0
fijarEspacio=0

'--FFT FREE FONT-
Var Shared ft => FreeType()
'' Load a font with FreeType
Dim Shared As FT_Face ftface
FT_New_Face( ft, "Bebaskai.otf", 0, @ftface )

' ========== CONTROL DEL NRO DE OCTAVAS MOSTRADO SE PODRA PONER PARA EL USUARIO
' VER SI SE PUEDE USAR ARRAYS POR PORCIONES
'----- -FIN
'----- MIDI MICROSOFT
'https://docs.microsoft.com/en-us/windows/win32/multimedia/midi-functions
'DIM CAN As UINT
'CAN= midiOutGetNumDevs()
'Print #1, "MIDI NUM DEVS ";CAN
 
'-----
' ancho de figura,separaciondelasmismas en pantalla anchofig
'' ---------------  LOOP 1 ---------------
 On Error GoTo errorhandler
' enviamos esto a una sub ROLLLoop, creaPenta esta al principio y no tiene declare
' el declare falla si se usa con este tipo de vector udt no se puede usar declare
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


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
Dim nombreport As ZString Ptr
midiout = rtmidi_out_create_default()
'Print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout)
Dim i1 As integer
'Print #1, "portsin  "; portsin
'Print #1, "portsout "; portsout
For i1 = 0 to portsout -1 
    nombreport = port_name(midiout, i1)
    Print #1, *nombreport
Next i1  

portsout = portout
*nombreport = ""

open_port (midiout,portsout, nombreport)

Print #1,"  "
If Roll.trk(1,NA).inst > 0 Then
 ChangeProgram ( Roll.trk(1,NA).inst , 0)
EndIf
 Print #1,"ChangeProgram inst ", Roll.trk(1,NA).inst
Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
Print #1,"INSTANCIA ",instancia

If ix < 3 Then ' rollmusic CON control
  instancia=0
  hwndC = OpenWindow("RollMusic Control ver 0.4.3.2",10,10,ancho*3/4,alto*4/5,,WS_EX_ACCEPTFILES   )
''UpdateInfoXserver()
  hwndListBox= ListBoxGadget(3,10,10,240,650,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )


  SendMessage(GadgetID(3),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
  TextGadget(4,250,10,240,20,, SS_SIMPLE  )
 ' GetTextExtentPoint32 PARA DETERMINAR EL ANCHO EN PIXELS DE UN TEXTO
 ' EL SCROLL VERTICAL APARECE CUANDO SE SOBREPASA LSO ITEM QUE SE PUEDEN VER 
  Dim As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8
 

  EVENTc=0

'StatusBarGadget(1,"StatusBarGadget")

  hMessages=Create_Menu()
  MenName1=MenuTitle(hMessages,"Archivo")
  MenName2=MenuTitle(hMessages,"Nueva Cancion")
  MenName3=MenuTitle(hMessages,"Crear Pistas")
  MenName4=MenuTitle(hMessages,"Ver desde Posicion")
  MenName5=MenuTitle(hMessages,"Cambiar Tiempo Y Ritmo")
  MenName6=MenuTitle(hMessages,"Reproducir")
  MenName7=MenuTitle(hMessages,"Opciones")
  MenName8=MenuTitle(hMessages,"Ayuda")

MenuItem(1005,MenName1, "Na.Cargar archivo de Cancion")
MenuItem(1006,MenName1, "Cargar directorio de Cancion con Pistas separados")
MenuItem(1007,MenName1, "Na.Grabar Cancion")
MenuItem(1008,MenName1, "Na.Grabar Cancion Como")
MenuItem(1009,MenName1, "Na.Exportar Cancion a midi")
MenuItem(1010,MenName1, "Na.Cargar una sola Pista(o track) de Cancion")
MenuItem(1011,MenName1, "Grabar una Pista de la Cancion con modificaciones, carga pista si no hubiera cargada")
MenuItem(1012,MenName1, "Copia una pista a otra  nueva nueva en cancion")
MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "Salir")


MenuItem(1020,MenName2, "Nombre o Título (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "Na.Tiempo I=60 por omision")
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



  MenuItem(1070,MenName4,"5 Menu")
  MenuItem(1080,MenName5,"6 Menu")
  MenuItem(1090,MenName6,"Reproducir desde la posicion o en el rango ajustado")
  MenuItem(1100,MenName7,"Usar MARCO de Ventana ")
  MenuItem(1101,MenName7,"No Usar MARCO de Ventana ")
  MenuItem(1110,MenName8,"9 Menu")
End If

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
abrirRoll=0

Do
param.titulo ="RollMusic Ver 0.4.4.0"
Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
Print #1,"inicio ubound roll.trk ", UBound(param.Roll.trk,2)
Print #1,"iniio lbound roll.trk ", lBound(param.Roll.trk,2)
  If abrirRoll=1 Then
    threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
    ''''RollLoop ( param)
    ''abrirRoll=2 ' roll ya abierto
    Sleep 100 ' sin este retardo no le da teimpo al thread de cargar a Roll
    ' y CargarPistasEnCancion no puede cargar proque no hay Roll
    ' QU EPSA SI LLAMO  VECES??
''no se lo banca     threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p2))
  EndIf
  If cargaCancion=1 Then
     CANCIONCARGADA=FALSE
     CargarPistasEnCancion ()
     CANCIONCARGADA=TRUE
     cargaCancion=0
     param.encancion=1
  Else
    CANCIONCARGADA=FALSE
    cargaCancion=0  
    param.encancion=0 
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
             Print #1,"termino 1006 va a abrir Roll"
 
            Case 1011 ' Grabar una Pista de la Cancion con modificaciones, que son tracks
            Print #1,"entro a 1011 esto lo hace menu de Roll tambien" '' jmg probar es nuevo...
 ' copiamos logica Rolla Track 
            Print #1, "Click Grabando a disco pista modif con RollaTrack ",nombre
            Dim As String nombreg
            ROLLCARGADO=FALSE 
            'If nombre = "" Then
            '   getfiles(file,myfilter,"save")
            '   nombreg=*file.lpstrFile
            '   If nombreg = "" Then
            '      Print #1,"exit select por nombreg vacio "
            '      Exit Select 
            '   Else
            '      nombre=nombreg   
            '   EndIf
            'EndIf
           If NombreCancion > ""  Then
              GrabarRollaTrack(0)
           EndIf
          MenuNew=0           
          carga=1

           Case 1012 ' Grabar Pista Como, Copia una pista a otra  nueva nueva
           Print #1,"entro a 1012 Grabar Pista Como, Copia una pista a otra  nueva nueva"
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            If nombre = "" Then
               getfiles(file,myfilter,"save")
               nombreg=*file.lpstrFile
               If nombreg = "" Then
                  Print #1,"exit select por nombreg vacio "
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
            Case 1020     
               NombreCancion = ""
               pathdir=""
               EntrarNombreCancion(NombreCancion)
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
               Roll.trk(1,NA).inst=CUByte(instru)  
            Case 1050 ' seleccion de instrumento por orden Numerico
               selInstORdenNum (instru)
              ' ChangeProgram ( CUByte (instru) , 0)
               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).inst=CUByte(instru)
               Roll.trk(1,NA).inst=CUByte(instru)
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
                  abrirRoll=1
                  If reiniciar=1 Then
                     ThreadDetach(threadloop)
                     usarmarcoOld=usarmarco
                     reiniciar=1
                  EndIf
                  If reiniciar=0 Then
                     reiniciar=1
                     usarmarcoOld=usarmarco
                  EndIf   
                  
                  Exit Do
               EndIf
            Case 1061
               Print #1,"En 1061 crear pista en cancion con lo elegido"
               
               ntk = CountItemListBox(3)+ 1
               If ntk > 32 Then
                  Print #1,"exit select ntk> 32"
                  Exit Select
               EndIf 
               Print #1,"ntk creado pista nro ", ntk
               If instru=0 Then 
                  instru=1
               EndIf
               Print #1,"instru en 1061 ",instru
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
               Print #1,"pathdir",pathdir
               If pathdir="" Then
                  SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + "Debe Crear un directorio de Cancion Con el Nombre de Cancion Elegido")
                  Print #1,"exit select donde miercoles se va? duplica ventana"
                  Exit Select 
               Else
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
               Print #1, "NombrePista en 1061",NombrePista
              AddListBoxItem(3, NombrePista)
              
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
               Print #1,"nombre en 1061",nombre
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
             Shell ("start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) )                
            Case 1070
              MessBox("","5 Menu")
            Case 1080
              MessBox("","6 Menu")
            Case  1090 
              CPlay=1
              Dim As Any Ptr thplayC = ThreadCall  playCancion(Track())
            Case  1100
                usarmarcoOld=usarmarco
                usarmarco=1
            Case 1101
                 usarmarcoOld=usarmarco            
                 usarmarco=0
            Case  1110
             MessBox ("","Acerca de este programa")
              End
          End Select
      Case eventgadget
      ' el codigo anterior que traia de disco esta en notas
       If eventnumber()=3 Then
         borrapos=0
 ' esto servia cuando cargaba solo la lista y no los tracks en 1006
 ' pero ahroa solo deberia hacer switch no cargar de disco sino
 ' directamente cargar Roll desde el numero de track correspondiente
 ' en memoria       
             Print #1,"CLICK EN LISTA"
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
                Print #1," NUEVO eventgadget click en lista nombre", nombre
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
                  Print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
                 If EventKEY = VK_DELETE Then 
                 Print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
                    Print #1,"LISTABOX EventKeyDown borrar ntk",ntk
                    Print #1,"LISTBOX  titulos(ntk)= ",titulos(ntk)
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
                Print #1,">>> ntk cargado, nombre ",ntk, nombre
                Print #1,"llama a RecalCompas para ntk ",ntk
                ReCalCompas(Roll)
                Sleep 20 
                ''mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
                Print #1,"Fin RecalCompas para ntk ",ntk
                item=""
                EndIf  
                Print #1," CLICK EN LISTA FIN "
             EndIf 
       EndIf

      Case EventClose 
       Close:End 0
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

End 0





'----FIN CONTROL-------------------
'   threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1)) 
'   ThreadWait threadloop
 
'RollLoop ( param)

End 0

'---------fin iup---    
errorhandler:
Dim As Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error  MAIN detected ", er, posicion, MaxPos
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

 

