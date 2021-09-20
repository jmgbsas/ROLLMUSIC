'18-09-2021 DOBLE CLIK EN UN ARCHIVO RTK O ROLL ABRE EL PROGRAM Y CARGA EL ARCHIVO
' SI LA EXTENSION ESTA ASOCIADA AL PROGRAMA
' 09-09-2021: error yendo y viniendo con octavas, las notas cargadas en la misma
' octava suenen diferente hay corrimiento de 1 octava parece chequear
' dragar ventana desde cinta fix. en mousex=1048 anda mejor
' VERSION DE PRUENA 0.4.1.0.0 INTEGRAMOS UNA GUI Y 2 GRAFICAS UNA SIN USO
' PREPARADO PARA 2 PANTALLAS GRAFICAS LA OTRA ES OPENGL WIN COMENTADA  
' COMPILAMOS CON fbc64  -s gui rollMusicControl.bas RollMusic.bas -x RollMusic.exe
' con el tiempo pasar toa a e.scancode  event If e.scancode = SC_P And Play=1
' anduvo mejor sino no cortaba el play en secuecnias largas...
' se puede usar llamdo desde ROllMusicControl o standAlone.
' 18-08-2021 envio de cambio de programa patch INSTRUMENTO EN ROLL.TRK(1,1)
' FUNCIONO,,,
' Correccion de notas cuadno se usa pocas OCTAVAS PianoNota no es correcta. LISTO OK
' USAR CALLROLL CON SHELL OK LISTO
' ROLLMUSIC-0.3.0.0.0-U-TRACKS CARGA Y GRABACION DE TRACKS ANDA BIEN,
' para conectar instancias podriamos usar ZEROMQ
' O ALGUN PTR A UNA UDT CON DATOS COMUNES 
' AHROA LE PUSIMOS REDIM PRESERVE PARA TODO ANDA OK, DE MODO QUE EL VECTOR ROLL
' ES CHICO AL INICIO Y SE VA AGRANDANDO SEGUN NECESIDAD 
' ROLLMUSIC-0.1.0.0.0-U-TRACKS
' ADOPTAMOS LIBRERIA PAR LEER Y GRABAR MIDI midilib de github
'---------------------------------------------
' PLAN 0.1.0.0.0-U-TRACKS :
' i) Vamos a testear si funciona la conversion 01-08-2021
' ii) dejamos por ahora la creacion de songs por medio de tracks con formato Roll
' y pasamos a grabar perfomance real midi desde teclado en tracks midi de Roll o midi con midilib, 
' pero para grabar usamos un tick o pulso de ritmo o una pista de roll o track ya
' grabada pro el usuario y a medida que la escucha puede empezar a tocar
' al 5to pulso [1 2 3 4],5 y con ese ritmo pedimos toque el usuario
' a medida que toca vamos calculando las duraciones en base a calCompas,,
' y almacenando las mismas en el formato de track comprimido donde no 
' interviene el tiempo para nada no hay ticks guardados,hay tick de posicion,
' todo es relativo y solo depende del tiempo ajustado, por default usara 
' el tiempo que ajusto el usuario el cual quedara grabado  en un archivo aparte
' por ahora como nombredelTema.cfg configuracion del teme  *.cfg
' ademas del tiempo de ejecucion se pondran otros parametros para grabar
' como instrumento efectos volumen del canal etc etc.,..tambien el usuario
' podrá cambiarlo en el archivo plano previamente sin necesidad de editarlo 
' en el programa. Todo se puede hacer en formato Roll. Pero vermos o no la convenicncia
' de usar el formato midi para grabar a archivo y luego volverlo a cargar..
' para eso Roll graba y carga transformando de *.trk a *.Roll 
' en esta version ya tenemos un alibreria par agrabar y leer *.mid 
'----------------------------------------------------------------------
' TRACKS: 1) 1ER ETAPA GRABAMOS TRACKS A PARTIR DEL VECTOR DE VISUALIZACION
' EN EDICION. listo ok
'    2DA ETAPA) CARGAMOS TRACKS Y PASAMOS AL DE VISUALIZACION PARA EDITAR
'  UNO QUE SE ELIJA..EMPEZAREMOS CON UN SOLO TRACK EL 1..O SEA GRABAR Y CARGAR 
' EN FORMATO TRACK Y EDITAR EN FORMATO VISUALIZACION, LEUGO SUPERADA ESA ETAPA
' SEGUIMSO AGREGANDO TRACKS---listo ok
' 3) si cargamos un trak desde disco, se convierte y carga a visualizacion ahi se modifica 
' y al GRABAR convierte de nuevo a track sin cambiar el nombre o sea reemplaza 
' el track cargado con su modificacion. Podriamos grabar en formato roll 
' si hace falta renombrando el archivo eliminando [nn] del principio
' asi Roll pensara que no se cargo un track y se grabara como *.Roll.
'
' 9.9 cambiamos la sdimensiones de lugar, asi maxpos es la 1era y podemos
' redimensionar con preserve !! algunaso archivos *.roll fueron reconvertidos
' ESTANDARIZACION OCTAVAS Y NUMEROS DE NOTAS DE PIANO EMPIEZAN EN CERO DESDE
' OCTAVA -1 --V ok. cambio masivo de variabel semitono va de 0 a 11
' cuadno nota ocupa el lugar de semitono se resta 1 porque semitono va de 0 a 11
' las notas letras van de 1 a 12 eso sigue igual pues ausencia de nota es 0
' de C a B van de 12 a 1 decreciente internamente limites de vector 0 a 115
' sub restar se adecuo al desplazamiento -1 de semitono etc etc etc gran cambio..! 
' 9.8 ya anda pasar como parametro Roll, ahora creacion de tracks
' seleccion de track y visualizacion del elegido grabacio etc gran trabajo!
' 9.7 ubicar nro compas entrando el numero en [Ver] ok
' 9.7 el RollLoop lo envie a una Sub con thread, de ese modo podre
' llamar varias pistas graficas a la vez pongamosle 8 par a8 instrumentos
' debere ahcer 8 pantallas o superficies ¿?
' 9.6 uso de thread para grafico, parametrizacion gap1 gap2y gap3
' 9.6 ajustar ancho de columnas Y FONT con F2 F3,luego F9 F10 font solamente
' 9.5 acordestriadas a partir de una fundamental. pendiente
' 9.5 NUEVO ARCHIVO BORRA nombre y todo lo de 'Q'.
' -> en desa: 9.5 repetir play zona grabado y marcado en Roll 
' Ubicar Home, End de secuecnia pulsando esas teclas.
' copiar una zona 1 o mas veces en la posicion elegida
'0.0.8.9.2.0 11-07-2021  mover zona a la derecha o izquierda con M + Click en la pos elegida
' despues haremos dragado, como de trasponer tambien pero mucho mas adelante,,,
' la idea es hacer algo parecido a trasponer,,pero horizontal ALT-M
' los espacios vacios los dejaremos con dur=0 nota=181
' insert tambien se hara mas facil usando estos metodos cambiaremos en 
' versiones futuras....
 
'01-07-2021 se agrego loop ya sea total o por zona funciona se termina con P
'01-07-2021 ya no permite entrar notas con click mucho antes de maxpos
'01-07-2021 posicionado cursor ctrl-m, pasoZona1 y 2 seusan en playAll tambien 
' ver ciertos problemas de play de las notas que subsistem !!!
'0.0.8.9.0.0 seleccion de una sola nota en el 3er Ctrl-Click para una accion, en modo lectura.
'0.0.8.8.1.0 seleccion de ZONA para accion con CTRL-Click en lectura 2 puntos.
'           borrado de zona con Q como siempre. ok 27-06-2021
' 26-06-2021:trasponer UP or Down ok
' 24-06-2021: trasponer..tiene un defecto cuando hace el pasaje de una octava a otra 
' hay una zona muerta ojo ajustar ,,,yo me complique todo con esta separacion o salto
' hay una linea muerta entre octavas!!!
' 22-05-2021:WHEEL LUEGO CTRL CAMBIA ESPACIADO DE LINEAS, REEMPLAZO BOLD POR NORMAL 
' 20-06-2021: ha que corregir el caso DESAFIO-LIGA-ACORDE-3-O-sin-O-ok
'  dura poco el sonido de la ligadura
' en el otro anda bien se corrigio uno se jodio el anterior!
' 12-06-2021: fix cursor durante play
' 12-06-2021: CAMBIO DEL ORDEN EN SEPARADUR DE LSO RANGOS DE BUSQUEDA
' 11-06-2021: scroll horizontal durante play
' 11-06-2021:[NUEVO] fix se agrego que limpie toda la secuecnia hasta el final.
' 11-06-2021 posible desarrollo: usar teclado para entrada de notas al estilo
' de entrada por teclado de la PC..los numeros 1 al 0 de duraciones seran
' cdefgabcde = 1,2,3,4,5,6,7,8,9,0...otra octava cualqueira seran los semitonos
'
' ------------------------------
'  -Wc -fstack-usage genera un archivo en runtime *.su con el uso del stack
' por cada funcoin el default total es 1024k, -t 1024 no haria falta colocarlo
' en la linea de comando
' https://gcc.gnu.org/onlinedocs/gnat_ugn/index.html#SEC_Contents
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
Type poli ' para guardar la secuencia
 dur As  Integer  ' duracion , tambien tendra rasguidos distintos programables por usuario o fijos
 nota As UByte  ' en un futuro contendra nota, octava, canal etc 
 vol As  UByte  ' volumen
 pan As  UByte  ' paneo
 pb  As  UByte  ' pitch bend
 inst As UByte ' instrumento para cada nota podra ser distinto
 tick As Integer ' 128 tiene la redonda, 1 la cuartifusa
 posn As Integer ' de roll
 acorde  As ubyte  ' 1 a 12 , son el se hara el sort    
End Type 

Type sec
 As poli trk(Any, any)
End Type

'-------------------------------
Type dat Field=1
 nota As UByte  ' en un futuro contendra nota, octava, canal etc 
 dur As  UByte  ' duracion , tambien tendra rasguidos distintos programables por usuario o fijos
 vol As  UByte  ' volumen
 pan As  UByte  ' paneo
 pb  As  UByte  ' pitch bend
 inst As UByte ' instrumento para cada nota podra ser distinto
End Type
Type inst
 As dat trk(Any, Any)
End Type


Dim Shared As Long CONTROL1 = 0
' ROLL PARAEL GRAFICO PERO LOS TRCKS PODRIAN SER DISTINTOS
'Dim Shared As dat Roll  (1 To 128 , 1 To 19200)

' dur debe ser el 1er elemento para que ande bien el sort

' PUEDO TENER UN SHARED DINAMICO GRACIAS A TYPE !!!
Dim Shared As Integer NB , NA, CantTicks, tempo, CantMin,CantCompas

Dim Shared As inst Roll ' para visualizar y tocar
 

Dim Shared Track  (0 To 64) As sec ' tracks para guardar,.. y tocar 



'tempo I=160, seri equivalente a O=40 como l maxima cantdad de ticks de O es
' eldela figura mas pequeña=128 ticks...40 * 128 = 5120 por minuto.
' Si deseo un secuecnia de CantMin minutos
tempo=160  ' negra=160
CantMin=15
'NotaBaja=1 : NotaAlta=128

Dim Shared ix As Integer=0
'Print #1, "__FB_ARGV__ ",__FB_ARGV__
'Print #1, "__FB_ARGC__ ",__FB_ARGC__
'Dim direp As ZString  Ptr
'Dim dires As String
Dim Shared titu As String
Dim Shared As Integer instru, instruOld
Dim Shared As Integer ubirtk, ubiroll

Print #1,"__FB_ARGC__ ", __FB_ARGC__
For ix = 0 To __FB_ARGC__
  Print #1, "arg "; ix; " = '"; Command(ix); "'"''

 If ix=1 Then
  desde= CInt(Command(ix))
  'C:\IT\JMGROLL01\[1]AAA.rtk
 ubirtk = InStr (LCase(Command(ix)),".rtk")
 ubiroll=  InStr(LCase(Command(ix)),".roll")
 If ubirtk > 0 or ubiroll>0 Then 
     nombre=Command(ix)
 EndIf
 Print #1,"ubirtk ",ubirtk
 Print #1,"ubiroll ",ubiroll
    'sigue en roolloop principio
 EndIf
 If ix=2 Then
  hasta= CInt (Command(ix))
 EndIf
 
 If ix=3 Then
  titu=  (Command(ix))
 EndIf

 If ix=4 Then
  instru=  CUByte (Command(ix))
 EndIf

Next ix


If desde = 0 And hasta = 0 Then
 Print #1,"intervalo no dado usando default!"
 desde => 4  ' 1 3   4 a 8 decia
 hasta => 8  ' 9 7 hasta-1
 
EndIf

Dim Shared As Integer desdevector
Dim Shared As Integer hastavector

CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...o pasos
'CantTicks=76800
CantTicks=4000 ' 3 MINUTOS A NEGRA 160/min=500 Y Q TODAS SEAN FUSA
 
Type paso Field=1
 Posi As Integer
 nro  As Integer 
End Type
Type pasa Field=1 
  As cairo_t Ptr c
  As inst Roll
  As String  titulo
  As Integer ancho
  As Integer alto
End Type

Dim Shared As paso compas (1 To CantTicks) 'cada item es la posicion en donde

desdevector = desde
hastavector = hasta
' test test
' --------
NB => 0 + (desde-1) * 13   ' 26 para 3 = 0     reemplazar 0 por NB
NA => 11 + (hasta-1) * 13  ' 115 para  9 = 115  reemplazr 115 por NA

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

' memorias de Tracks..por ahora igual que Roll de trabajo luego veremos si achicamos
' mas parecido a midi.,,,,8 tracks ocupasn 0,544 giga 32 1,8 gigas de memoria virtual
'Dim Shared As Integer lim2=12, ctres=CantTicks
Dim Shared As Integer lim2=12
Dim AS Integer  ctres=CantTicks ' 5 octavas por track 
' c/inst puede tocar como una persona hasta 12 notas juntas de acorde
' entonces no se justifica tener en un solo instrumento una polifonia mas de 12
' ni de 108,,,ergo puedo poner mas trakcs o mas longitud
' asi ocupa,recalcular.., 270k , 64 ocupara 550 y 128 ocupara 1 giga...listo!
' lo hare de 32 tracks 275 mgbytes !
'CREAR UN MENU ACA DE CREACION DE TRACKS SEGUN SE ELIJA SE VAN EJECUTANDO LOS
' REDIM Y LUEGO SE PUEDE ELEGIR CADA UNO PARA EDITAR, PARA ELLOS
' SE PASARA SU NOMBRE A RollLoop EN UN SELECT CASE , CASE1 ROLL1 CASE2 ROLL2 ETC
' debo modificar todo el programa para incorporar el indice del inst o Track
' Sigo enviando  Roll, pero cuadno lo uso debo especificar cual track estoy usando
' creo 4 tracks por default a pedido del ususraio se pueden agregar mas
'=============================================================
' ctres SERA DINAMICO A MEDIDA QUE SE ENTRA MAS NOTAS SE IRA EXTENDIENDO LAS POSICIONES
'============== IMPLEMENTARLO AL FINAL 

ReDim (Track(0).trk ) (1 To Ctres,1 To lim2)
ReDim (Track(1).trk ) (1 To Ctres,1 To lim2)
ReDim (Track(2).trk ) (1 To Ctres,1 To lim2)
ReDim (Track(3).trk ) (1 To ctres,1 To lim2)
ReDim (Track(4).trk ) (1 To ctres,1 To lim2)
' c/u de estos track es redimensionable preserve en la 1era dimension 
' o sea en las posiciones, lo que debo hacer es cargar estos Tracks
' con eventos pero para mostrarlso usaria el Roll 
' suponemos que cada track solo puede tener acordes de hasta 12 notas,1 to lim2
' en el momento de la carga de Roll cargar tambien Track ¿?
' creo que no solo debo copiar en el momento antes de grabar o reproducir
' debere hAcer otro play PlayTracks en donde se ejecutara todos los tracks
' barriendo la posicion comun a todos y las notas de cada uno EN SINCRONIA CON LA VISUALIZACION!
' ENTONCES EN PLAYALL RECORRO TRACKS PERO NO EL DE VISUALIZACION,,,,,VEREMOS.,


Dim Shared As inst RollAux
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
Dim Shared As Integer  ANCHO
Dim Shared As Integer  ALTO, deltaip=0
Dim Shared As Double   BordeSupRoll, inc_Penta
Dim Shared As Integer  AnchoInicial,AltoInicial
Dim Shared As FLOAT font, deltaipf=0, lockip=0 
Dim Shared q As String * 1
Dim Shared As UByte s1, s2, s3, s4, s5,s6, s7 ,s8 ',s9
Dim Shared escala As float = 1.0
Dim Shared translado As Integer = 1
''https://www.freebasic.net/forum/viewtopic.php?t=15127
ANCHO = GetSystemMetrics(SM_CXSCREEN)
ALTO = GetSystemMetrics(SM_CYSCREEN)
ANCHO = ANCHO *11/12
ALTO = (ALTO -25)*11/12
AnchoInicial=ANCHO
AltoInicial=ALTO
anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "

Dim As String driver

' CAIRO NO SOPORTA LA ñ!!! ESO ERA TODO!!!!
Dim shared As Integer i,  posmouse, posmouseOld,incWheel, edity1,edity2,octavaloop
Dim Shared As Integer octaroll
posmouseOld = 0:posmouse = 0
Dim Shared As BOOLEAN comEdit, resize
comEdit = FALSE:resize = FALSE
Dim Shared po As Integer Ptr
' son 9 octavas y 1 para escribir? o escribe en la ultima ?
po = @octaroll
'''*po = 8
*po = hasta -1 ' test 09-09-2021 
s1=0:s2=0:s3=0:s4=0:s5=0:s6=0:s7=0:s8=0 
''font=18 haremos font funciona de anchofig O NroCol o ANCHO
' para 35 font=18 =  18 /35 = 514/1000
'font=anchofig * 515 /1000 '' 18 default
'font=anchofig * 510 /1000 + (35-anchofig)* (anchofig ^2 - 1225) /1000
font=18
Dim Shared e As EVENT
Dim Shared rmerr As Integer

indaux=0:carga=0
' -----------------------------------------------------------------------
' notas redonda (O),blanca(P),negra(I),corchea(C),semicorchea(S), Fusa(F),Semifusa(U)
' O P I L F E # (listo todas mis notas!!!)
' puntillo O* P*  C* elsigno  *
' -puntillo resta en vez de sumar -O -P -C ...-U
' O P I L F E # (redonda, blanca,corchea, semicorchea, Fusa,Semifusa)
' no meconviene escribir encima de las lineas debo cmbiar todo para
' escibirsolo sobre espacios hgmos ROLL4
' -------------------------------------------------------------------------
BordeSupRoll = Int((ALTO ) /18) ' (1400 )/18 integer = 77
inc_Penta = Int((ALTO - BordeSupRoll) /(40)) ' 26 double 1330/66 para 1400 resolu

' *******************************************************++
BordeSupRoll = BordeSupRoll -  66* inc_Penta ' de inicio muestro octava 4 la central C3
' *************************************************+
' inc_Penta=separacion de lineas
'---------------------
Dim Shared As Integer mxold,myold, w,h
gap1= anchofig* 2315/1000 ' 81 default
gap2= (914 * gap1) /1000 ' 74 default
gap3= (519 * gap1) /1000 ' 42 default

Print #1,"gap1 ",gap1

GetMouse mxold,myold, , MouseButtons


Dim  Shared c As cairo_t  Ptr
Dim  Shared cm As cairo_t  Ptr
Dim  Shared c3 As cairo_t  Ptr

Dim Shared As Integer stride, nro_penta,IhWnd,Mhwnd,Style,desktopwidth,desktopheight
posicion = 1 ' comienzo del roll
'indice   = 1  ' numero de nota al comienzo del programa B8
espacio = 0
backspace = 0
fijarEspacio=0
'amedida que nos movemos ira incrementando o decrementando
Dim Shared surface As Any Ptr
' ------------------------ windows controls ---------
'---ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
'---Dim Shared As hWnd hwnd 
'---hwnd = Cast(hwnd,IhWnd)
Dim comienzo As Integer = 0
'--FFT FREE FONT-
Var Shared ft => FreeType()

'' Load a font with FreeType
Dim Shared As FT_Face ftface

FT_New_Face( ft, "Bebaskai.otf", 0, @ftface )
'''Dim Shared cface as cairo_font_face_t Ptr


' ========== CONTROL DEL NRO DEOCTAVASMOSTRADO SEPODRAPONER PARA EL USUARIO
' VER SI SE PUEDE USAR ARRAYS PORPROCIONES

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
#Include "ROLLSUB.BAS"
'===========================
'ScreenControl  SET_DRIVER_NAME,"GDI"
Dim Shared As hWnd hwnd,hwndMenu 
'hwnd = Cast(hwnd,IhWnd)
'hwndmENU = Cast(hwnd,MhWnd)

 Dim Shared As UINT codsalida=0
 Dim shared As Any Ptr lpExitCode
 Dim As Integer MenuFlag=0, LoopFlag=0 

'========================== 
#Include "ROllLoop.BAS"
'========================== 




'''#Include "ROllMenu.BAS"

' aca puedo llamar a varios thread 1 por vez segun el instrumento editado
' o sea 1 vector de roll distinto para casa Thread o llamar a cada Vector
' con el mismo Thread para verlo en pantalla. Se veria 1 por vez o si queremos podriamos
' ver mas de 1 pero apra eso deberia llamar a roll music mas de una vez 
' y eso lo haria desde call roll 
'----------------
#Include "ROLLMIDI.BAS"

'----------------
'Dim pRoll As Any Ptr
''USAR PIRULO.EXE COMO LLAMADOR, USA WINEXEC Y ES SIMILAR A SHELL
' MEJOR NO PRODUCE CONSOLA Y DA MAS PARAMETROS 

'  Dim tloop As Any Ptr = ThreadCall RollLoop(c, Roll )
Dim Shared  As pasa param 
    param.c= c
    param.Roll = Roll
    param.titulo = titu
    param.ancho = ANCHO
    param.alto = ALTO
    p1=@param
Common Shared As hwnd hwndC    
#Include "mod_rtmidi_c.bi"    
'----------------------------
Dim nombreport As ZString Ptr
midiout = rtmidi_out_create_default()
'Print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout)
Dim i1 As integer
'Print #1, "portsin  "; portsin
'Print #1, "portsout "; portsout
For i1 = 0 to portsout -1 
    nombreport = port_name(midiout, i1)
 '   Print #1, *nombre
Next i1  

portsout = portout
*nombreport = ""

open_port (midiout,portsout, nombreport)

Print #1,"  "
If Roll.trk(1,NA).inst > 0 Then
 ChangeProgram ( Roll.trk(1,NA).inst , 0)
EndIf
 Print #1,"ChangeProgram inst ", Roll.trk(1,NA).inst

COMMON Shared As Long eventc
Common Shared As hwnd hwndListBox

If ix < 3 Then ' rollmusic CON control
 
 
hwndC = OpenWindow("RollMusic Control",10,10,ancho*3/4,alto*4/5,,WS_EX_ACCEPTFILES   )
''UpdateInfoXserver()
hwndListBox= ListBoxGadget(3,10,10,240,650,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL )
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
MenName3=MenuTitle(hMessages,"Pistas")
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
MenuItem(1011,MenName1, "Grabar una Pista en Edicion en la Cancion")
MenuItem(1012,MenName1, "Na.Grabar una Pista de cancion Como")
MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "Salir")


MenuItem(1020,MenName2, "Nombre o Título (fecha por omision)")
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



' opengl
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

Dim Shared As Any Ptr mutex
Dim Shared As Any Ptr cond
Dim Shared As String txt
Dim As Any Ptr pt
Dim Shared As Integer ok = 0
Dim As Integer Terminar=0,abrirRoll=0
abrirRoll=0
Do
param.titulo ="RollMusic"

  If abrirRoll=1 Then
    threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
    '''RollLoop ( param)
    ''''abrirRoll=2
  EndIf

  If ix < 3 Then 

'PREPARADO PARA EL FUTURO OTRA PANTALLA GRAFICA OPENGL
 ''win = glfwCreateWindow(800,600,"Track OPENGL" )
'' Dim ta As Any Ptr = threadcall correwin(win,ta)
    Do
     eventC=WaitEvent
'WindowStartDraw(hwndC)
'  fillrectdraw(40,40,&h888888)
'  TextDraw(10,10,NombreCancion,-1,&hff0000)
'StopDraw
     Select Case EVENTC 
       Case EventMenu 
         Select case EventNumber
     '  Case 1005,1007 To 1016,1018 to 1019, 1021 To 1024, 1026,1027
     '       MessBox("","Menu no habilitado")
            Case 1006   ' CARGAR CANCION  
             cargarDirectorioCancion(NombreCancion)
             GrabarRollaTrack(1,1) ' se usa solo como dir y carga de tracks 
             ' en la lista y en los track o pistas correspondientes
             If abrirRoll=0 Then
                abrirRoll=1
                Exit Do
             EndIf
 
            Case 1011 ' graba pista en edicion en la cancion
            Print #1,"entro a 1011 en elmenu"
             ResetAllListBox(3)
             GrabarRollaTrack(1,1)
            Case 1014 ' salir 
              Terminar=1
              cerrar(0)
              Exit Do
            Case 1017 ' grabarpista
             If NombreCancion <>"" Then ' es una cancion grabamos en el directorio de la cancion
                GrabarPistaCancion=1
             Else 
                GrabarPistaCancion=0
             EndIf      
            Case 1020     
               EntrarNombreCancion(NombreCancion)
            Case 1025
               CrearDirCancion (NombreCancion)            
            Case 1028 ' seleccion octavas menores a 1 9 
               seloctava (desde, hasta)
               *po = hasta -1
                Nuevo(Roll,1 )
            Case 1040 ' seleccion de instrumento por orden Alfabetico
               selInstORdenAlfa (instru)
               ChangeProgram ( CUByte (instru) , 0)
               Roll.trk(1,NA).inst= CUByte(instru)

            Case 1050 ' seleccion de instrumento por orden Numerico
               selInstORdenNum (instru)
               ChangeProgram ( CUByte (instru) , 0)
               Roll.trk(1,NA).inst= CUByte(instru)
            Case 1060 ' crea track y reemplaza al existente en la edicion
               If ntk=0 Then  ' no se cargo ningun track
                  *po = hasta -1
                  Nuevo(Roll,1 )
                  instruOld=instru
                  Roll.trk(1,NA).inst= CUByte(instru)
                  ChangeProgram ( CUByte (instru) , 0)
               EndIf   
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
               EntrarNombrePista(NombrePista)
               numpista += 1
               If instru=0 Then 
                  instru=1
               EndIf
               If NombrePista ="" Then
                NombrePista = str(numpista) +"-"+ RTrim(Mid(NombreInst(instru), 1,21))
               Else
                NombrePista = str(numpista) +"-"+ NombrePista 
                
               EndIf

              AddListBoxItem(3, NombrePista)
              NombrePista=""
               If abrirRoll=0 Then
                  abrirRoll=1
                  Exit Do
               EndIf
' FALTA CREAR LA PISTA !!! jmg
            
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
         If eventnumber()=3 Then
             Dim item As String
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
                Print #1," eventgadget click en lista nombre", nombre
                lineadecomando=1
                cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina
                TrackaRoll (Track(), ntk , Roll )
                Print #1,"ntk cargado, nombre ",ntk, nombre
                ReCalCompas(Roll)
                item=""
             EndIf  
         EndIf

      Case EventClose 
       Close:End 0
     End Select
   Loop
Else
  param.titulo ="RollMusic"
''''  threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
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
Print #1,"Error detected ", er, posicion, MaxPos
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

 

