' V5.7.5.0.0 72 FIGURAS SE AGREGO H/2 = W, LA w de antesse reemplazo por X
' REDIMENSION VISUAL Y FISICA FUNCIOANA BIEN SI SE ACHICA EL VECTOR
' PERO SI SE AGRANDA ES INESTABLE -ERGO SI CARGO UN ARCHIVO REDUCIDO
' EN OCTAVAS NOSE PODRA EXPANDIR HAY QUE LIMITAR ESA POSIBILIDAD !!! 
'  ERROR 1 ) A CORREGIR:
' TRABAJAR CON DATOS DE DISCO TRBJO_02.ROLL DE ESTE DIR
' Y CARGAR MNUALMENTE ES LO QUE FALA EL CORTE EN EL 3ER COMPAS
' VER PARA CARGR PAPEL O CRGA-OK-MANUAL-NO-OK.png.aleatoraiamente
' verificar creo sepudohaber corregido<---
' ERROR 2 ) al pulsar notas en teclado enuna octava que no se permite
' editar se congela todo, solo sepuede volver dando ctrl-M seguido de
' ctrl-P en la octava de edcicon 
' calculamal loscortes de compas en manual por ahora se corrige
' - ok:se reemplazo organizacompases por RecalCompas
' - ok:estando con un nro de octavas n1 y si se carga archivo con n2
'   ajusta automticamente el editor a n2..
' ------------------------------

Open "midebug.txt" for Output As #1

Open "mivector.txt" for Output As #3

''Open cons  for Output As #1

''Open "figuras.txt" For Output As #1

' secuenciador de 9 octavas estereo, modo Piano Roll,hace uso de
'letras para las duraciones en vez de rectangulos...
' edicion modificacion insercion,,,12 eventos c/u con
'nota, duracion,volumen ,paneo, pitch bend e instrumento
' version actual front end solamente 1 track...no reproduce
' no genera midi todavia..
'-------------
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

''
#Include Once "cairo/cairo.bi"
#Include "ROLLDEC.BI"
#Include "NOTAS.bi"
Type dat Field=1
 nota As ubyte
 dur As UByte  ' duracion
 vol As UByte  ' volumen
 pan As UByte  ' paneo
 pb  As UByte  ' pitch bend
 inst As UByte ' instrumento para cada nota podra ser distinto
End Type


' ROLL PARAEL GRAFICO PERO LOS TRCKS PODRIAN SER DISTINTOS
'Dim Shared As dat Roll  (1 To 128 , 1 To 19200)
Type inst
 As dat trk(Any, any)
End Type
' PUEDO TENER UN SHARED DINAMICO GRACIAS A TYPE !!!
Dim Shared As Integer NB , NA, CantTicks, tempo, CantMin,CantCompas


Dim Shared As inst Roll
'tempo I=160, seri equivalente a O=40 como l maxima cantdad de ticks de O es
' eldela figura mas pequeña=128 ticks...40 * 128 = 5120 por minuto.
' Si deseo un secuecnia de CantMin minutos
tempo=160  ' negra=160
CantMin=15
'NotaBaja=1 : NotaAlta=128

Dim ix As Integer
'Print #1, "__FB_ARGV__ ",__FB_ARGV__
'Print #1, "__FB_ARGC__ ",__FB_ARGC__
Dim direp As zstring  Ptr
Dim dires As String

For ix = 0 To __FB_ARGC__
 '     Print #1, "arg "; ix; " = '"; Command(ix); "'"''

 If ix=1 Then
  desde= CInt(Command(ix))
 EndIf
 If ix=2 Then
  hasta= CInt (Command(ix))
 EndIf

 If ix=3 Then
  dires=  (Command(ix))
 EndIf


Next ix
Common Shared mensaje As String

Dim diren As Integer
diren = Cint(dires)
direp = @diren
Print #1, "arg desde "; desde
Print #1, "arg hasta "; hasta
Print #1, "dires   "; dires

Print #1, "recibi "; *direp
Print #1, "common "; mensaje

If desde=0 And hasta=0 Then
 desde => 1  ' 1 3
 hasta => 9  ' 9 7
EndIf

Dim Shared As Integer desdevector
Dim Shared As Integer hastavector

CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...
CantTicks=76800
Dim Shared As integer compas (1 To CantTicks) 'cada item es la posicion en donde
desdevector = desde
hastavector = hasta
NB => 1 + (desde-1) * 13   ' 27 para 3
NA => 12 + (hasta-1) * 13  ' 90 para  7

ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
Dim Shared As inst RollAux
ReDim (RollAux.trk) (NB To NA , 1 To CantTicks)
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
Dim Shared As Integer  ALTO
Dim Shared As Double   BordeSupRoll, inc_Penta
Dim Shared As Integer  AnchoInicial,AltoInicial
Dim Shared As FLOAT font
Dim Shared q As String * 1
Dim Shared As UByte s1, s2, s3, s4, s5,s6 , s7 ',s8,s9
Dim escala As float = 1.0
Dim translado As float = 1.0
''https://www.freebasic.net/forum/viewtopic.php?t=15127
ANCHO = GetSystemMetrics(SM_CXSCREEN)
ALTO = GetSystemMetrics(SM_CYSCREEN)
ANCHO = ANCHO
ALTO = ALTO  -25
AnchoInicial=ANCHO
AltoInicial=ALTO
anchofig=35
NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "

ScreenControl  SET_DRIVER_NAME,"GDI" ' le da foco a la aplicacion si uso GDI
' pero llamando al programa con winExec con opcion SW_RESTORE no hay necesidad
' y puedousar  directx!!

' con Directx nunca tomaelfoco se lodebe dar elusuario
'nofuncionaningun comndo de winuser.bipara tomar el foco...
'''''ScreenControl POLL_EVENTS '200
'ScreenControl SET_WINDOW_POS ' (100) 'Sets the current program window position, in desktop coordinates.

' https://www.freebasic.net/forum/viewtopic.php?f=6&t=27555
Dim As String driver


ScreenRes ANCHO, ALTO, 32,1,  GFX_NO_FRAME Or GFX_HIGH_PRIORITY


ScreenControl GET_WINDOW_POS, x0, y0


''ScreenControl SET_WINDOW_POS, 10,10
'ScreenControl 103,"Directx" ' cambio ja
' CAIRO NO SOPORTA LA ñ!!! ESO ERA TODO!!!!
Dim As Integer i, octava, posmouse, posmouseOld,incWheel, altofp11,edity1,edity2
altofp11=ALTO:posmouseOld = 0:posmouse = 0
Dim Shared As BOOLEAN comEdit, resize
comEdit = FALSE:resize = FALSE
Dim po As Integer Ptr
po = @octava
*po = 8
s1=0:s2=0:s3=0:s4=0:s5=0:s6=0:s7=0':s8=0:s9=0
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
Dim As Integer mxold,myold, w,h

GetMouse mxold,myold, , MouseButtons


Dim  Shared c As cairo_t  Ptr
Dim  Shared cm As cairo_t  Ptr
Dim  c3 As cairo_t  Ptr

Dim As Integer stride, nro_penta,IhWnd,Style,desktopwidth,desktopheight
posicion = 1 ' comienzo del roll
'indice   = 1  ' numero de nota al comienzo del programa B8
espacio = 0
backspace = 0
fijarEspacio=0
'amedida que nos movemos ira incrementando o decrementando
Dim Shared surface As Any Ptr
' ------------------------ windows controls ---------
ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
Dim As hWnd hwnd = Cast(hwnd,IhWnd)

Dim comienzo As Integer = 0
'--FFT FREE FONT-
var Shared ft => FreeType()

'' Load a font with FreeType
Dim Shared as FT_Face ftface

FT_New_Face( ft, "Bebaskai.otf", 0, @ftface )
' ========== CONTROL DEL NRO DEOCTAVASMOSTRADO SEPODRAPONER PARA EL USUARIO
' VER SI SE PUEDE USAR ARRAYS PORPROCIONES

'----- -FIN
' ancho de figura,separaciondelasmismas en pantalla anchofig
'' ---------------  LOOP 1 ---------------
'' On Error Goto errorhandler:

Do

edity1 = 15 ' botton Edit bordeSup
edity2 = 30 ' botton Edit bordeInf




'' Create a cairo drawing context, using the FB screen as surface.
'' l originalestba mal sizeof(integer ) es mu chico debe ser 4
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


Var surface = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, ALTO, stride)
Var c = cairo_create(surface)

ScreenLock()

'' Measure the text, used with SDL_RenderCopy() to draw it without scaling

' https://www.cairographics.org/tutorial/

''cairo_translate(c, 100, 100)

If comEdit = TRUE  and octavaEdicion = estoyEnOctava Then
 cairo_set_source_rgba(c, 0.6, 0.6, 0.7, 1)
Else
 cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
EndIf
cairo_paint(c)
cairo_set_line_cap(c, CAIRO_LINE_CAP_ROUND)
cairo_set_line_width(c, 1)
cairo_set_source_rgba(c, 0, 0, 0, 1)


If s1 = 1 Then
 s1= 0
EndIf
If s2 = 1 Then
 s2=0
EndIf
If s6 = 1 Then
 s6=0
EndIf


inc_Penta = Int((ALTO -1) /40)
'llena la surface con nro_penta
nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)
'Print nro_penta


'  cairo_save (c)
'  cairo_scale (c, escala, 1)
cairo_set_antialias (c, CAIRO_ANTIALIAS_DEFAULT) 'hace mas lental cosa pero nomeafecta
' usemos 8 octavas y una para pie de pagina
' podemos reducir !!! y dejar ciertas octavas por instrumento
' cada isntrumetnotendriaundefinicion distintde roll con redim?
' no sepeude salvo dentro de un Type?
'
' desde=3 hasta=7
'  rango= hasta
For i = desde To hasta ' nro_penta

 creaPenta (c, i, po,InicioDeLectura )
 If *po = 99 Then
  *po = hasta - 1
  Exit For
 EndIf

Next

''' cairo_restore (c) esto es solo para translado

cairo_stroke(c) ' Escribe desde la fuente source a la mask ...(peden ser varias fuentes)

Var surf2 = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, 50, stride)
Var cm = cairo_create(surf2)
' cm no lo estoy usando por ahora pero las surf son transparentes
'a lo de abajo, si doy click podria responderme la otra superficie,sigue
'siendo lamisma ,intentare usar view de fb,,,,
' y sea enmenu o con SC_L puedo cargar desde disco,...sepierdela
' posicion aprentemente elcursorqueda clavado en 1 o 0 y no navega 5.7.3.1
' solo pasando por Edicion vuelve a navegar

menu(c,cm, posicion,menuNro)

cairo_stroke(cm)

cairo_stroke(c)


botones(hWnd, c ,cm, ANCHO,ALTO)


If menuaccion=1111 Then ' no sirve las aciones perforan
 cairo_move_to(c, 9*(ANCHO/10),50)
 Var surf3 = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO/10, ALTO, stride)
 Var c3 = cairo_create(surf3)
 menuAcc (c3)
 cairo_stroke(c3)

EndIf

ScreenUnLock()



'' ---------------  LOOP 2 ---------------
Do While InKey =""
'If comienzo=0 Then
' ScreenControl  SET_DRIVER_NAME,"Directx" ' cambio ja
' ScreenRes ANCHO, ALTO, 32,2, GFX_NO_FRAME,GFX_HIGH_PRIORITY
' ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
' Dim As hWnd hwnd = Cast(hwnd,IhWnd)
' comienzo=1
'EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_M)  Then ' modificar con X o insertar con Insert y I
 cursorVert = 1
 cursorHori = 1
 agregarNota=0
 menuMouse = 0
EndIf
If MultiKey(SC_CONTROL) And MultiKey(SC_N)  Then 'modificar con nombre de nota
 cursorVert = 2
 cursorHori = 2
 agregarNota= 1
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_P)   Then 'PARAR cursor MEJOR CON MOUSE ?
 cursorVert = 0
 cursorHori = 0
 agregarNota = 0
 menuMouse = 0
 '' notadur=0
EndIf

If MultiKey(SC_DOWN)  Then
 If cursorVert=0 Then
  If s1=0 Then
   s1=1
   BordeSupRoll = BordeSupRoll -  inc_Penta
  EndIf

  If BordeSupRoll <= - AltoInicial * 2.8  Then
   BordeSupRoll =  - AltoInicial * 2.8
  EndIf
  Exit Do
 EndIf
EndIf

If MultiKey(SC_PAGEDOWN) Then

 If s1=0 Then
  s1=1
  BordeSupRoll = BordeSupRoll - inc_Penta * 6
 EndIf
 If BordeSupRoll <= - AltoInicial * 2.8 Then
  BordeSupRoll =  - AltoInicial * 2.8
 EndIf


 Exit Do
EndIf

If MultiKey(SC_PAGEUP ) Then
 'beep
 If s2=0 Then
  s2=1
  BordeSupRoll = BordeSupRoll + inc_Penta * 6
 EndIf
 If BordeSupRoll >= AltoInicial * 0.5  Then
  BordeSupRoll =  AltoInicial * 0.5
 EndIf

 Exit Do

EndIf

If MultiKey(SC_PLUS) Then  '13 , ligadura
 mas=1
 Exit Do
EndIf

If  MultiKey(SC_KEYPADPLUS) Then '78
 '    ALTO = ALTO + 2 'inc_Penta
 '      If ALTO >= AltoInicial Then
 '         ALTO =  Altoinicial
 '      EndIf


 '   cairo_set_source_surface (c, surface, ANCHO, ALTO)

 '   Sleep 10
 '    Exit Do
 '------------------------------
 cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
 cairo_paint(c)
 cairo_stroke(c)
 cairo_destroy(c)


 ALTO = ALTO + inc_Penta/2

 'Print "ALTO+ ", ALTO
 If ALTO >= AltoInicial Then
  ALTO =  AltoInicial
 EndIf

 cairo_set_source_surface (c, surface, ANCHO, ALTO)
 cairo_paint(c)
 Exit Do

EndIf

If MultiKey(SC_MINUS) Then
 '    cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1 'evita fondo negro y flick
 '    cairo_paint(c)

 '   ALTO = AltoInicial/1.5
 '   cairo_set_source_surface (c, surface, ANCHO, ALTO ) ' ALTO)
 '   Exit Do
 ' LA ZONA QUE DEJA ESTA POSICION EN LA PARTE INFERIOR SE USARA PARA
 ' HACER AJUSTES DE VELOCIDAD CON CURVAS O MOSTRAR CONTROLES ADICIONALES
 cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
 cairo_paint(c)
 cairo_stroke(c)
 cairo_destroy(c)

 ALTO = ALTO - inc_Penta/2

 If ALTO <= AltoInicial/3 Then
  ALTO= AltoInicial/3
 EndIf

 cairo_set_source_surface (c, surface, ANCHO, AltoInicial ) ' ALTO)
 cairo_paint(c)
 Exit Do

EndIf
If MultiKey(SC_CONTROL) And MultiKey (SC_RIGHT) Then
 posicion=posicion + NroCol/2
 If posicion > MaxPos Then
  posicion = MaxPos
 EndIf
 posishow=posicion
 Sleep 100
EndIf
If MultiKey(SC_CONTROL) And MultiKey (SC_LEFT) Then
 posicion=posicion - NroCol/2
 If posicion < 1 Then
  posicion = 1
 EndIf
 posishow=posicion
 Sleep 100
EndIf

'   escala = escala - 0.1
If MultiKey(SC_RIGHT)  Then ' <======== RIGHT
 If  mouseY < 50  Then ' seleccion de menu, mouse sobre cinta + teclas
  menuNro=menuNro+1
  If menuNro > 3 Then
   menuNro=0
   menuNew=0
  EndIf
  menuNew=menuNro
  While Inkey <> "": Wend
  Sleep 350
  Exit Do
 Else
  'kNroCol cantidad scroll de NrocOL)
  If comEdit = FALSE Then
   posicion = posicion + 1
   kNroCol= Int(posicion/NroCol)
   If  (kNroCol > 0) And (posicion = NroCol * kNroCol) And (posicion < MaxPos)Then
    iniciodelectura = iniciodelectura +  NroCol
    If inicioDeLEctura > MaxPos Then
     inicioDeLEctura = inicioDeLEctura -NroCol
    EndIf
   EndIf
   If posicion > MaxPos -1  Then
    posicion = MaxPos -1
   EndIf
  Else
   curpos= curpos + 1 ' mueve cursor cuando Roll se detiene (posicion)
   If curpos > NroCol  Then
    curpos = NroCol
   EndIf

  EndIf
  Sleep 100
  Exit Do
 EndIf
EndIf
'   escala = escala + 0.1
If MultiKey(SC_LEFT)  Then '  <========== LEFT
 If  mouseY < 50  Then  ' seleccion de menu
  menuNro=menuNro - 1
  menuNew=menuNro
  If menuNro < 0 Then
   menuNro=3
   menuNew=3
  EndIf
  While Inkey <> "": Wend
  Sleep 350
  Exit Do
 Else
  'MOVER ROLL IZQUIERDA NO CURSOR
  If comEdit = FALSE Then
   Dim kNroCol As Integer ' cntidad de scroll de 66
   posicion = posicion - 1
   kNroCol= Int(posicion/NroCol)
   If  kNroCol > 0 And (posicion = NroCol*kNroCol)  Then
    iniciodelectura = iniciodelectura - NroCol
   EndIf
   If iniciodelectura < 0 Then
    iniciodelectura = 0
   EndIf
   If posicion < 1 Then
    posicion = 1
   EndIf

  Else
   curpos = curpos - 1 ' <=== MOVER CURSOR IZQ
   If curpos < 0 Then
    curpos = 0
   EndIf

  EndIf
  Sleep 100
  Exit Do
 EndIf
EndIf
' https://freebasic.net/forum/viewtopic.php?t=15100
' interfiere con ALT+ tAB del usuario apr amirar otra apliccion
' no va
'If MultiKey(SC_ALT)  Then
'   escala = escala - 2
'EndIf


' ALTUR DE LA VENTANA ,SE PODRA? VEMOS si se pudo con movewindow
' https://www.freebasic.net/forum/viewtopic.php?t=15127
' DISPONIBLE F4 F5
If  MultiKey (SC_F5)  Then
 Exit Do
EndIf

If  MultiKey (SC_F4)    Then
 Exit Do
EndIf

If MultiKey (SC_F8)  Then
 If comEdit = FALSE Then
  ' MOVE VENTANA
  Dim As Integer w,h
  w=ANCHO:h=ALTO
  ALTO = ALTO + inc_Penta
  If ALTO >= altoInicial - 1  Then
   ALTO = altoInicial  - 1
  EndIf
  MoveWindow( hWnd , 0, (0+ALTO-h)\2, ANCHO,ALTO, TRUE )
  altofp11 = ALTO
 EndIf
 Exit Do
EndIf

'----------
' SIZE ANCHO F5
' F5 SCANCODE 63 , F6 64
If MultiKey(SC_CONTROL) And MultiKey (SC_F5)   Then
 If comEdit = FALSE Then

  translado = translado - 2
  '      cairo_destroy(c)
  '      cairo_surface_destroy(Surface)

  '      ScreenControl SET_WINDOW_POS, 0,0

  '      ANCHO = ANCHO - 4 * inc_Penta
  '      If ANCHO <=  ANCHO * 0.3 Then
  '         ANCHO =   ANCHO * 0.3
  '      EndIf
  '     GoTo reinicio
 EndIf

 Exit Do

EndIf
If MultiKey (SC_F2)  Then

 escala = escala - 0.01
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey (SC_F6)  Then
 If comEdit = FALSE Then

  translado = translado + 2
  '      cairo_destroy(c)
  '      cairo_surface_destroy(Surface)
  '      ScreenControl SET_WINDOW_POS, 0,0

  '     ANCHO = ANCHO + 4 * inc_Penta
  '     If ANCHO >= anchoInicial - 1  Then
  '        ANCHO = anchoInicial - 1
  '     EndIf

  '      GoTo reinicio

 EndIf

 Exit Do
EndIf
If MultiKey (SC_F3)  Then

 escala = escala + 0.01
 Exit Do
EndIf
' PRUEBAS DE GRABACION DEL VECTOR ROLL es sencillo porque grabo todo
' o cargo todo,,
' https://www.freebasic.net/forum/viewtopic.php?f=2&t=26636&p=246435&hilit=array+load+save#p246435
' ===================================
if Multikey (SC_F11) Then '  <========= Grabar  Roll Disco  F11
 'cada posicion tendre 48bits osea 6bytes..
 ' luego 12000 posiicones si estuviera todo completo serian 9216000 bytes
 ' y grabo..9mbytes, seria 1 Track,,295 mbytes para 32 tracks
 '  Print #1, "Grabando a disco Roll F11 "
 Open "Trabajo.roll" For Binary access write As #2

 Dim Trabajo (NB To NA, 1 To Maxpos) As dat
 Dim grabaPos (1,1)  As dat
 Dim as integer i1, i2
 Dim As Integer y1,y2,y3,y4
 Dim As String a1,a2,a3,a4 ,x
 x= Bin(MaxPos,16)
 '  Print #1,"Posicion ",Posicion
 'Print "string representando ", x
 a1=Mid(x,1,4)
 a2=Mid(x,5,4)
 a3=Mid(x,9,4)
 a4=Mid(x,13,4)
 'Print #1,"a1 a2 a3 a4 ",a1, a2 ,a3, a4

 y1= CInt("&B"+a1)
 y2= CInt("&B"+a2)
 y3= CInt("&B"+a3)
 y4= CInt("&B"+a4)
 ' Print #1, "y1,y2,y3,y4", y1,y2,y3,y4
 ' grabamos maxpos en 4 ubyte
 grabaPos(1,1).nota = y1
 grabaPos(1,1).dur  = y2
 grabaPos(1,1).vol  = y3
 grabaPos(1,1).pan  = y4
 ' ------------------------------------
 ' hacemoslo mismo para l ultim not que se grabo que teng el 65
 ' esa es lapapa recorremos todas las dur de las notas en donde este el 65
 ' endur tomamosla nota y esala grabamos en pb o ins
 ' cuadno la cargamos lo ahcemos en notaold !!!! y Vuala!!!
 ' -------------------------------------
 Dim As Integer i,j,notafinal
 For i = NB To NA
  If Roll.trk(i,posicion+1).dur = 65 Then
   notafinal= Roll.trk(i,posicion).nota
   '      Print #1, "ENCONTRO NOTA FINAL ", notafinal
   '      Print "ENCONTRO NOTA FINAL ", notafinal
   ' esten la posiciond ela ultimanotapero grasoerror
   ' o seaque no tiene duracion solo el34
   ' esta mal ...
   Exit For
  EndIf
 Next
 grabaPos(1,1).pb = notafinal
 ' Grabacion de Trabajo
 ' ------------------------------------
 For i1 = NB To NA
  For i2 = 1 To MaxPos
   Trabajo(i1,i2)=Roll.trk(i1,i2 )
  Next i2
 Next i1

 '  Print #1,"MaxPos grabada en Trabajo ",MaxPos

 Put #2, ,grabaPos(1,1)
 Put #2, ,Trabajo()
 Close 2
 While InKey <> "": Wend
 sleep 150

EndIf
' cargar Roll y MaxPos de disco

If MultiKey(SC_L)  Then ' <======== load Roll
  If carga=0 Then
   CargaArchivo()
  EndIf 
 ' colocar algo visual que indique quesehizo la grabcion
EndIf

if Multikey (SC_F12) Then
 dim as integer i1, i2
 ' testeo solo en la 1er octva por ahora
 Print #1,
 for i1 = 1 to 12
  for i2= 1 to posicion
   print #1, Roll.trk(i1, i2).nota;"-";
  next i2
  print #1,
  for i2= 1 to posicion
   print #1, Roll.trk(i1, i2).dur;"-";
  next i2
  print #1,
  print #1,"------------------------"
 next i1
 While Inkey <> "": Wend
 sleep 150
 Close 2
endif
If MultiKey (SC_F10) Then
 font = font + 1
 Sleep 150
 If font > 24 Then
  font=24 ' pisa la 1er duracion mas de ahi....
 EndIf
 Exit Do
EndIf

If MultiKey(SC_ESCAPE) Then
 '  ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
 '  Dim As hWnd hwnd = Cast(hwnd,IhWnd)

 if MessageBox(hWnd,"¿SEGURO FINALIZA?","RollMusic End ",4 or 64) =6 then
  cairo_destroy(c)
  '   cairo_surface_destroy( surface )
  '   cairo_font_face_destroy( cface )
  FT_Done_Face( ftface )

  Close
  End
 EndIf
EndIf
' repetir espacios con barra+ALTGRAF..luego la nota correspondiente
If MultiKey(SC_ALTGR) Then ' FIJA ESPACIOS REPETIDOS HASTA NUEVA PULSO
 fijarEspacio = 99
 ' fijar para muchso espacios
EndIf
'--

' ============== E S P A C I O ========
If MultiKey(SC_SPACE) And s7=0 Then 'barra espacio
 espacio = 1
 posicion= posicion + 1
 DUR=65
 s7=1
 Exit Do
EndIf
If MultiKey (SC_Q) Then ' con Q se deja de repetir espacios
 If fijarEspacio=99 Then
  fijarEspacio=0
 EndIf
EndIf
' ----------------------INGRESO NOTAS-------------------------
' MAYUSCULAS PARA SOSTENIDOS
' las duraciones iran del 1 al 8 ..cuadno se las pulsa le sumamos 12
' ya que la 128 se llega por 12*8 octavas = 96 + espcops intermedios....etc..
' ergo por hhora las durciones empien con 13 al 20
' 13 = O, 14=P, 15=I, 16=L,17=F,18=E,19=W,20=H

If MultiKey(SC_CONTROL) And MultiKey(SC_A)  Then ' A#
 nota= 2
 If espacio = 1 Then
  espacio=2
 EndIf   ' hasta aca logica nueva
 ' si estoy escribiendo espacio pero salto a otra nota, algo ilogico...
 ' para eso dejamos ALT_GLRAF que resetee el espacio a 0 manualmente,
 ' osi memuevo con flechas podrai enviar cero al espacio
 '    if nota<> notaold THEN ' stnd by nola usare
 '       espacio=0
 '    EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_C)   Then ' C#
 nota = 11
 If espacio = 1 Then
  espacio=11
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey(SC_CONTROL) And MultiKey(SC_D)  Then ' D#
 nota= 9
 If espacio = 1 Then
  espacio=9
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey(SC_CONTROL) And MultiKey(SC_F) Then ' F#
 nota= 6
 If espacio = 1 Then
  espacio=6
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If  MultiKey(SC_CONTROL) And MultiKey(SC_G)  Then ' G#
 nota= 4
 If espacio = 1 Then
  espacio=4
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_A) Then
 nota= 3
 If espacio = 1 Then
  espacio=3
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey (SC_B) Then
 nota = 1
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_C) Then
 nota = 12
 If espacio = 1 Then
  espacio=12
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_D) Then
 nota= 10
 If espacio = 1 Then
  espacio=10
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey (SC_E) Then
 nota = 8
 If espacio = 1 Then
  espacio=8
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey (SC_F) Then
 nota= 7
 If espacio = 1 Then
  espacio=7
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf
If MultiKey (SC_G) Then
 nota= 5
 If espacio = 1 Then
  espacio=5
 EndIf
 If cursorVert = 2  Then
  agregarNota=1
 EndIf
 Exit Do
EndIf
If MultiKey(SC_BACKSPACE) Then
 backspace=1

EndIf
' ----------------------FIN NOTAS-------------------------
' -----------REDIMENSIONAMIENTO DE OCTAVAS POR EL USUARIO
'  If octavas = 3 and DUR <> desde And DUR > 0 Then
'      Print #1, "OCTAVAS 3"; OCTAVAS,Timer
'       hasta=DUR
'       DUR=0
'        Print "DUR2 ", DUR
'        Print "DUR2 ", DUR
'        Print "DUR2 ", DUR'
'
'        octavas=0
'        NB => 1 + (desde-1) * 13   ' 27 para 3
'        NA => 12 + (hasta-1) * 13  ' 90 para  7
'        ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
'        ReDim (RollAux.trk) (NB To NA , 1 To CantTicks)
'        MaxPos = 1
'        posicion=1:posn=1
'        curpos=1
'        notacur=1
'   EndIf
'If octavas = 1 And DUR > 0 Then
'Print #1, "OCTAVAS 1 "; OCTAVAS,Timer
'      desde=DUR
'      Print "DUR1 ", DUR
'      Print "DUR1 ", DUR
'      Print "DUR1 ", DUR
'      DUR=0
'         OCTAVAS=2
'Print #1, "OCTAVAS 2 "; OCTAVAS,Timer
'EndIf

'------FIN OCTAVAS REDIM
' ----------INGRESO DE DURACIONES DE NOTAS -------------

If comEdit = TRUE Then
 If (menuNew = 2 Or menuNro=2) Then
  If MultiKey(SC_1) Then
   DUR = 1 :Exit Do
  EndIf
  If MultiKey(SC_2) Then
   DUR = 2:Exit Do
  EndIf
  If MultiKey(SC_3) Then
   DUR = 3:Exit Do
  EndIf
  If MultiKey(SC_4) Then
   DUR = 4:Exit Do
  EndIf
  If MultiKey(SC_5) Then
   DUR = 5:Exit Do
  EndIf
  If MultiKey(SC_6) Then
   DUR = 6:Exit Do
  EndIf
  If MultiKey(SC_7) Then
   DUR = 7:Exit Do
  EndIf
  If MultiKey(SC_8) Then
   DUR = 8:Exit Do
  EndIf
  If MultiKey(SC_9) Then
   DUR = 73:Exit Do
  EndIf

  If MultiKey(SC_0) Then ' FIN
   DUR = 74:Exit Do
  EndIf

  If MultiKey(SC_PERIOD) Then
   pun = 1  ' puntillo
   Exit Do
  EndIf
  If MultiKey(SC_S) Then
   sil = 1
   Exit Do  ' silencio
   ' indicadorde silencio solo para calculo de compas
  EndIf
 EndIf
 ' ojo ver q no habia  exit do antes !!!!!
EndIf
' ----HELP PRUEBA DE TEXT
If MultiKey(SC_F1) Then
 ' estopodemos hacer ayuda contextual
 '' Define character range
 '.DRAWASTRING CON FONT DEUSUARIO A VECES SEGUN COMO SE COMPILE HACE CERRAR LA APLICACION
 ' ELIMINADO NOUSAR ESE METODO, USAREMOS CAIRO...(para todo veo ....)
EndIf
'
If MultiKey(SC_R) Then ' recalculo de barras compas a veces no anda ¿? 
 ReDim compas(1 To CantTicks)
 ReCalCompas() ' jmg 01-04-21 
EndIf

If comEdit = FALSE Then '''???????????????? veremos para que usarlo
 ' para ubicrno enun octava dada

 If MultiKey(SC_1) Then

 EndIf
 If MultiKey(SC_2) Then

 EndIf
 If MultiKey(SC_3) Then

 EndIf
 If MultiKey(SC_4) Then

 EndIf
 If MultiKey(SC_5) Then

 EndIf
 If MultiKey(SC_6) Then

 EndIf
 If MultiKey(SC_7) Then

 EndIf
 If MultiKey(SC_8) Then

 EndIf
 If MultiKey(SC_PERIOD) Then

 EndIf
 If MultiKey(SC_S) Then

 EndIf
 ''''etc
EndIf


' --------------------------[NUCLEO]---------------------------

' <<<<<<<<<<<<  detector notas >>>>>>>>>>>>>>>>>>>>
' O P I L F E W (redonda, blanca,negra, corchea, semicorchea, Fusa, Semifusa).
' ser con las letras CDEFGAB, con sostenido las mismas pero con CTRL
' las duraciones seran los numeros 1,2,3,4,5,6,7 correpondientes
' a O P I L F E W, cuyas relaciones aon 1,2,4,8,16,32,64
' la sduraciones con puntillo se le agrega allado derecho un +, ej I+=negr conpuntillo
' el signo + tamien significara ligdura o sea I+L = negra ligda a corchea = I+
' untresillo simplemente se coloca un 3 junto a la figura 3III = tresillo de negra.
' ------------DETECCION DE OCTAVA-----NUCLEO CARGA ENTRADA---
''
' si no uso 1 + inicioDeLEctura y  uso posicion que pasa?
' el programa no sabe donde el usuario colocara una nota nueva en la pantalla
' pero si sabe en que posicion arranca la vista ergo ya no haria falta posn
' que revise desde el principio SINO DESDE posicion ficticimente hacia eso
' al usar iniciodeLectura, pero eso sí, con inicio congelaba el movimiento
' del roll ante una entrada de nota hasta el próximo incremento de pantalla
' SOLO SEUSAPARAINGRESO DE NOTAS NUEVAS ..VERIFICANDO JMG
If comEdit = TRUE  And nota> 0 And agregarNota=0 And cursorVert=0 And carga=0 Then
 'Print #1,"--------------------------------------------------------------"
 'Print #1,">>>START NUCLEO-COMPAS VECTOR posn: "; posn; "suma:";acumulado
 'Print #1,">>>START NUCLEO-COMPAS PROCESANDU DUR: " ; DUR;_
 '   " nota: ";nota; " figura: ";figura(DUR)
 posn=1 + InicioDeLectura
 If DUR=0 Then
  Exit Do
 EndIf
 If controlEdit=0 Then
   controlEdit=1 
   octavaEdicion=estoyEnOctava
 EndIf
 
 
 '   Print #1,"estoyEnOctava ";estoyEnOctava
 If estoyEnOctava <> 99 And octavaEdicion = estoyEnOctava Then ' estoy en una octava
  '  If indice <= 0 Then
  '      indice = 1
  '  EndIf
  '  If indice >= 128 Then
  '      indice = 128
  '  EndIf
  If nota > 0 And estoyEnOctava < 99 and estoyEnOctava >=1 Then

   ' ====>  Control PAgindo Horizontal <=======
   '      kNroCol= Int(posicion/60)
   '       Print #1, "A:Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota ", _
   '       Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota
   ' PARA USAR ESTO CON ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
   ' y hacer nota=semiotono 1 a 11 con el mouse...el esto es automtico...
   Do
    If Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = 0 OR _
     Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = 74 Then
     ' corregido .mota=66 decia pasado a dur...al principio
     'elfin lo poniaen nota horaendur nose si ponerlo en todo
     ' talvezdeberiaponerlo en amboslados nota y dur
     '     Print #1, "D:Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota ", _
     '     Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota
     posicion=posn
     '      Print #1, "ingreso a NUCLEO POSICION=POSN", posicion
     Exit Do
    EndIf
    posn = posn + 1
    If (posn > NroCol + InicioDeLectura) Then
     InicioDeLectura=InicioDeLectura + NroCol
    EndIf
    '   Print #1, "ingreso a NUCLEO posn ",posn
   Loop
   ' ESTO ME UBICA EN QUE RENGLON DE LA OCTaVA ESTOY SN USAR EL MOUSE
   ' LUEGO haRE ALGO CON EL MOUSE POR AHORA TODO TECLADO

   Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = nota 'carga
   '' ojo ver'  If cursorVert = 0 and cursorHori = 0 Then
   ' no actua para modificaciones o agregado en lo existente
   ' 65 o FIN indica final de TODO es la MAXPOS (+1obvio),se usara
   ' para insertar y saber hasta donde se debe mover...esta solo
   'en dur no afecta a notas pero se debe insertar siempreenedicion
   ' con o sin cursor
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn+1).dur = 74
   if notaOld > 0 And notaOld <> nota then
    Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 73
    '''ojo probar todo inserciones x  etc    endif
   EndIf
   ' cargamos Roll entonces Duracion no lo mostrara como "./."
   ' solo conrolara la posicion, quedndo solo ese simbolo paralaa entrada
   ' especifica del usuario....
   ' llena todo de espcio por eso  aparece la duracion
   ' cargada al siruarme en lugaressin nota o sea crgr nota
   ' nuevdonde no habia paso siguinte crgral not hbilitarencursos
   ' la crg de roll
   For i= 1 To 12 ' gracias a esto anda acordes
    If i<> nota Then
     If Roll.trk((i +(estoyEnOctava -1) * 13),posn).nota = 0 Then
      Roll.trk((i +(estoyEnOctava-1) * 13), posn).nota = 73
     EndIf
    EndIf
   Next
   notaOld = nota
   ' un binario 000,001,010,011,100,101,111

   If pun  = 0 And sil=0 And mas=0 Then ' no hay puntillo ni silencio
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR 'era duracion
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
    incr=0
   EndIf
   If pun = 1 And sil=0 And mas=0 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 9 'era dur
    incr=0
   EndIf
   If pun=0 And sil=1 And mas=0 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 18 'era dur
    Print #1," NUCLEO GUARDO EN ROLL CON S DUR: ";DUR +16;" figura:";figura(DUR+16) 
    incr=16
   EndIf
   If  pun=1 And sil=1 And mas =0 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 27
    incr=16
   EndIf
   If pun=0 And sil=0 And mas=1 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 36
    incr=32
   EndIf
   If pun=1 And sil=0 And mas=1 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 45
    incr=32
   EndIf
   If pun=0 And sil=1 And mas=1 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 54
    incr=48
   EndIf
   If pun=1 And sil=1 And mas=1 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 63
    incr=48
   EndIf
  ' Print #1," NUCLEO GUARDO DUR EN ROLL ";DUR;" figura ";figura(DUR)
  ' Print #1," NUCLEOBUSC Y GREG EN POSIICON :" ;posn
 '  If DUR=64 Then
 '   Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 65
 '   DUR=0
 '  EndIf
   pun=0:sil=0:mas=0
   ' mayorDurEnUnaPosicion (posn) quedo <--defectuoso
   
     calcCompas(posn) 'lrepeticion no espor calcCompas
   
   '   rmerr = Err
   '  Print #1,"Nucleo Error "; rmerr

   nota = 0



  Else ' edicion de nota anterior retroceso, concosco la posicion la octava
   'pero no la nota 1ero debo recuperar la nota, cursor lo sabe tomar de ahi
   'no puedo usar notaOld porque eso seria solo en el caso de que estaba editando
   ' para esa nota 1ero deberia oder moverme arribay bjo con el cursor para
   ' posicionarmeen una nota....ctrl-felcahs verticales ubicaran en cursor
    nota=0
  EndIf
  'print " Roll ", Roll.trk(indice, posicion)
    nota=0
  ' mostrarla en la t del Roll en el indice correspondiente
  ' ocalculo elindice en cada t y meto la nota o saco en t las otas
  ' del vector Roll pra ello acaa la grabo enRoll
 EndIf
 '''   calcCompas(posn)
 Exit Do 'kkkk 30-01-21 probando
EndIf
'If comEdit=FALSE then
'  calcCompas(posn)
' EndIf


'-----------------------------SCREEN EVENT-------START -----------
' para detectar mouse sin usar sdl
If (ScreenEvent(@e)) Then
 Select Case As Const e.type
  Case EVENT_MOUSE_BUTTON_RELEASE ' obtengoPosicion
   If s5=2 Then
    ScreenControl GET_WINDOW_POS, x0, y0
    s5=0
   EndIf
   Exit Do
  Case EVENT_MOUSE_WHEEL      ' <<<=== MOUSE WHEEL
   ' new position & e.z

   posmouse = e.z
   If posmouse <> posmouseOld Then
    incWheel = posmouse - posmouseOld
    posmouseOld = posmouse
   EndIf
   If incWheel > 0 Then
    If s2=0 Then
     s2=1
     BordeSupRoll = BordeSupRoll + inc_Penta
    EndIf
    If BordeSupRoll >= AltoInicial * 0.5   Then
     BordeSupRoll =  AltoInicial * 0.5
    EndIf

    Exit Do
   Else
    If s1=0 Then
     s1=1
     BordeSupRoll = BordeSupRoll - inc_Penta
    EndIf
    If BordeSupRoll <= - AltoInicial * 2.8  Then
     BordeSupRoll =  - AltoInicial * 2.8
    EndIf
    Exit Do
   EndIf
  Case EVENT_KEY_PRESS    ' <======== KEY PRESS PULSO

   If e.scancode = 72  Then ' SC_UP sube por pulsos mas presicion
    If cursorVert= 0 Then
     If s2=0 Then
      s2=1
      BordeSupRoll = BordeSupRoll +   inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf

     Exit Do
    EndIf
    If cursorVert=1 Or cursorVert=2 Then
     notacur=notacur-1
     If notacur < 1 Then
      notacur=12
     EndIf
     Exit Do
    EndIf
   EndIf

   If e.scancode = &h41 Then ' <======= SC_F7
    If comEdit = FALSE Then
     ' MOVE VENTANA
     w=ANCHO:h=ALTO
     '
     ALTO = ALTO - inc_Penta
     If ALTO <= ALTO * 0.3 Then
      ALTO =  ALTO * 0.3
     EndIf
     '
     '    ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     '    Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , 0 , (0+h-ALTO)\2, ANCHO,ALTO, TRUE )
     altofp11 = ALTO
    EndIf
    Exit Do
   EndIf
   If e.scancode = &h42  Then ' <==== F8
    If comEdit = FALSE Then

     ' MOVE VENTANA
     Dim As Integer w,h
     w=ANCHO:h=ALTO
     '
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
      ALTO = altoInicial  - 1
     EndIf

     '  ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)

     '  Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , 10, (10+ALTO-h)\2, ANCHO,ALTO, TRUE )
     altofp11 = ALTO
    EndIf
    Exit Do
   EndIf

   If e.scancode = 67 Then ' <===== F9
    font = font - 1
    Exit Do
   EndIf
   '  If  e.scancode = 68 Then ' <====== F10 'no funciona no lodetecta !!
   '      font = font + 1
   '      Exit Do
   '  EndIf
   If e.scancode = 80 Then  ' <===== SC_DOWN pulso
    If cursorVert=1 Or cursorVert=2 Then
     notacur = notacur + 1
     If notacur > 12 Then
      notacur=1
     EndIf
    EndIf
    Exit Do
   EndIf
   If e.scancode = 83 Then '<====== SC_DELETE cambia a silencio o nada le suma 16+16 ver eso
    borrar = 1
    Exit Do
   EndIf
   If e.scancode = SC_X Then ' 81 <==== SC_X ...fix
    'corrige nota cambia duracion o agrega nota nueva, acorde melodia
    ' solo debe funcionar con CTRL-M
    If cursorVert=1 Then ' ver cursorVert2 archivonuevocon espacios....sirve??
     cambiadur = 1    ' usando 1 a 8 para silencio esta delete
    EndIf
    Exit Do
   EndIf
   ' para insertar en cada iten de Roll cda vez queingreso nota al final
   ' comunmente se agregara 66 para indicar fin de datos..y loindicaremos
   'con FIN por ahora para visualizarlo

   If e.scancode = SC_INSERT And insert=0  Then '34 <===== SC_INSERT
    ' solo valido con CTRL-M
    If cursorVert = 1 And  cursorHori = 1 And insert = 0 Then
     ' solo tiene sentido insertar en lo echo y en cursor libre
     insert=1 ' comienzo hbilitotel I para insertr nota por nota
     indaux=0
     print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert
     If indaux=0 Then ' no haria falta todas las demas inserciones se deben
      'hacer con I no volver a repetir SC_INSERT sino se pulso SC_END
      StartInsert = posicion + curpos  ' guardo sin modificar el comienzo xxx ok
     EndIf
     print #1,">>>SC_INSERT  despues ajuste STARTINSERT ", StartInsert
     Erase (RollAux.trk) ' borro lo que habia en el auxiliar
     Erase (notasInsertadas)
     notins=0
     Print #1, ">>>SC_INSERT insert indaux borro RollAux.trk: ",insert,indaux
     'sigue el proceso en RollSub->sub cursor
    EndIf
   EndIf
   If e.scancode = SC_I And insert=1 And cursorVert=1 Then
    insert= 2 ' habilito cursor para que ingrese
    Print #1, "-----SC_I -> insert,indaux : ",insert,indaux
   EndIf
   If e.scancode = SC_END Then ' mueve insercion, podria usarse para ELIMINAR Probar
    If backspace=1 Then
      Dim as Integer i, y
      For y=posishow To MaxPos
        For i=NB To NA 
        Roll.trk(i,y).nota=0
        Roll.trk(i,y).dur=0
        Next i
      Next y
      MaxPos=posishow+1 'jmg 03-04-2021
      posn=posishow
      Roll.trk(nR, MaxPos).dur=66
      Roll.trk(nR, MaxPos).nota=0
      ReCalCompas()
      backspace=0
      DUR=0
      nota=0
      Exit Do 
    EndIf
    If cursorVert=1 Then ' solo válido con Ctrl-M
     ' no mas reemplazos
     insert=3
     Print #1, "-----SC_END StartInsert,indaux,insert,nota: ",StartInsert,indaux,insert,nota
     Print #1,"indaux no deberia valer cero !!!! es global "
     moveresto (StartInsert,indaux, insert,nota)
     '  param : posicion comienzo (fijo), indice incremental para el aux
     ' ,insert comando habilitado = 1
     '  insert 3 fin reemplazos comienzo de move total
     insert=0:indaux=0
     calcCompas(posn) '' mayorDurEnUnaPosicion (posn)
    EndIf
   EndIf

   ' ------------------PULSAR MUCHO TIEMPO <====== REPEAT------
  Case EVENT_KEY_REPEAT
   If e.scancode = 72  Then ' <======= SC_UP
    If cursorVert = 0 Then
     If s2=0 Then
      s2=1
      BordeSupRoll = BordeSupRoll +   2 * inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf
     Exit Do
    EndIf
    If cursorVert = 1 Then
     notacur=notacur-1
     If notacur < 1 Then
      notacur=12
     EndIf
     Exit Do
    EndIf
   EndIf

   If e.scancode = 80 Then  ' <===== SC_DOWN repeat
    If cursorVert=1 Then
     notacur = notacur + 1
     If notacur > 12 Then
      notacur=1
     EndIf
    EndIf
    Exit Do
   EndIf
   ' If e.scancode = 75 Then ' <=====  LEFT repeat
   '       posicion= posicion -1
   '     Exit Do
   ' EndIf

   ' If e.scancode = 77 Then ' <======= RIGHT repeat
   '       posicion = posicion + 1
   '
   ' EndIf

   If e.scancode = &h41 Then ' <============ F7

    If comEdit = FALSE Then
     ' MOVE VENTANA
     w=ANCHO:h=ALTO
     '
     ALTO = ALTO - inc_Penta
     If ALTO <= ALTO * 0.3 Then
      ALTO =  ALTO * 0.3

     EndIf
     '
     '   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     '   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , 10 , (10+h-ALTO)\2, ANCHO,ALTO, TRUE )

     altofp11 = ALTO
    EndIf

    Exit Do
   EndIf

   If e.scancode = &h42  Then ' <============  F8
    If comEdit = FALSE Then

     ' MOVE VENTANA
     Dim As Integer w,h

     w=ANCHO:h=ALTO
     '
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
      ALTO = altoInicial  - 1
     EndIf

     '   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)

     '   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , 10, (10+ALTO-h)\2, ANCHO,ALTO, TRUE )

     altofp11 = ALTO

    EndIf

    Exit Do
   EndIf


 End Select
 '-------------------------------------END SCREENEVENT ----------

 GetMouse mouseX, mouseY, , MouseButtons   ' <=======  CLICK EVENTOS
 If (mouseY >= edity1 ) And (mouseY <= edity2) Then
  If (mouseX >= 36) And (mouseX <= 70) And (menuNew=2 Or menuNro=2)  Then
   'SI ADEMS SEUSA CTRL-M SEPUEDE modificar ,agregr acordes e insertar
   ' 1 o varias notas en forma horizontal siemrpe la misma nota
   ' para acorde usar modificar SC_X
   menuMouse = 0
   ' ------ 04-03-21 aca siempre es edicin nueva no modificcion, o solo lectura
   cursorVert = 0
   cursorHori = 0
   agregarNota = 0
   menuMouse = 0
   carga=0
   '-----fin
   If MouseButtons And 1 Then ' <========= EDICION SOLO INGRESO DE NOTAS NUEVAS
    If s3 = 0 Then
     comEdit = TRUE : s3 = 1
     '       Print #1, "INVESTIGO COMEDIT ENTRO X TRUE EN MAIN S3: ",S3
     font = 18
     curpos=0
     'mayorDurEnUnaPosicion (posn)
     '' calcCompas(pos)
     Exit Do
    Else
     comEdit = FALSE : s3 = 0 ' solo LECTURA
     '       Print #1, "INVESTIGO COMEDIT ENTRO X FALSE EN MAIN S3: ",S3
     'posicion= posicion + curPOS ' estaba mal no va 3-3-21 jmg
     curpos=0
     controlEdit=0
     Exit Do
    EndIf
   EndIf
  EndIf
  If (mouseX > 0) And (mouseX <= 20 ) Then
   If MouseButtons And 1 Then
    If s4 = 0 Then
     resize = TRUE : s4 = 1
     Exit Do
    Else
     resize = FALSE: s4 = 0
     Exit Do
    EndIf
   EndIf
   Exit Do  ' OK
  EndIf
 EndIf

 If mouseY < 50 And s5= 0 And mouseX > 70 And mousex < (ANCHO-70) Then
  x1=mouseX: y1=mouseY
  s5=1
 EndIf
 If MouseButtons And 1 And s5=1 And mouseX > 70 and menuNro= 1 And mousex < (ANCHO-50)Then
  x2=mouseX
  y2=mouseY
  x0=x0+x2-x1
  y0=y0+y2-y1
  ScreenControl SET_WINDOW_POS, x0, y0
  ' mientras mantengo presiondo el mouse pudo mover el mouse con la ventana
  ' la performance no es tan buena pero funciona
  Exit Do
 EndIf
 ''  If mouseY > 50 Then ' <=== MENU DEFAULT 0 POR AHORA NO ES MOLESTO
 ''         menuNew=menuNro
 ''  EndIf
 ''https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-movewindow
 '                           <====== [BOTONES] =======>
 If (mousex>=(ANCHO-40)) And (mousey <= 16) Then
  If  MouseButtons And 1 Then
   if MessageBox(hWnd,"¿SEGURO FINALIZA? (puede usar  Escape tambien)","RollMusic End ",4 or 64) =6 then
    cairo_destroy(c)
    '   cairo_surface_destroy( surface )
    '     cairo_font_face_destroy( cface )
    FT_Done_Face( ftface )

    Close

    End
   EndIf
  EndIf
 EndIf
' ======> F7  BOTON - 
 If (mousex>=(ANCHO-40)) And (mousey > 17) And (mousey < 32) Then
  If  MouseButtons And 1 Then
   ''      If comEdit = FALSE Then
   ' MOVE VENTANA
   w=ANCHO:h=ALTO
   '
   ALTO = ALTO - inc_Penta
   If ALTO <= ALTO * 0.3 Then
    ALTO =  ALTO * 0.3
   EndIf
   '
   '    ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   '    Dim As hWnd hwnd = Cast(hwnd,IhWnd)
   MoveWindow( hWnd , 0 , (0+h-ALTO)\2, ANCHO,ALTO, TRUE )
   altofp11 = ALTO
  EndIf
  Exit Do

  ''  EndIf
 EndIf
 ' BOTON + F8
 If (mousex>=(ANCHO-40)) And (mousey > 33) And (mousey < 50) Then
  If  MouseButtons And 1 Then
   ''    If comEdit = FALSE Then
   ' MOVE VENTANA
   Dim As Integer w,h
   w=ANCHO:h=ALTO
   ALTO = ALTO + inc_Penta
   If ALTO >= altoInicial - 1  Then
    ALTO = altoInicial  - 1
   EndIf
   MoveWindow( hWnd , 0, (0+ALTO-h)\2, ANCHO,ALTO, TRUE )
   altofp11 = ALTO
  EndIf
  Exit Do

  ''  EndIf
 EndIf
 '         <==== MENU BORRR INSERTAR MODIFICAR ================>
 ' savemousex=0 : savemousey=0 LO QUE HACE ES PERMITIR QUE EL MENU APARESCA EN OTRO LADO
 ' DONDE SE CLICKEA, Y SI SON >0 DEJA QUE PERMANESCA VISUALMNETE
 ' HASTA EL PROXIMO CLICK IZQUIERDO, POR ESO SE PASAN A CERO SOLO EN EL ULTIMO CLICK
 ' IZQUIERDO (2 o 1 segun la cantidad necesaria para ejecutar el comando)
 '  MOMENTO EN EL QUE SE EJECUTA EL COMANDO Y SE VE EL CAMBIO.
 '       ==== NOTAS O DURACIONES EXISTENTES ====
 If  comEdit=TRUE then
  If  MultiKey(SC_CONTROL) And (MouseButtons And 2)  Then
   ' trae un menu contextual solo con ctrl-m  previo <==== menu borrar insertar modificar
   ' ESTADO:CALL MENU COMANDO
   ayudaModif=TRUE
   ayudaNuevaNota=FALSE
   menuMouse = 0
   nroClick=1
   cursorVert = 1
   cursorHori = 1
 '  Print #1, "------------------------------------------------------------"
 '  Print  #1,"(1) MultiKey(SC_CONTROL) And (MouseButtons And 2) And comEdit=TRUE"
 '  Print #1, "sc_CONTROL + MB2 + CE=TRUE <= ESTADO:CALL MENU COMANDO"
 '  Print  #1, " ayudaModif=TRUE"
 '  Print  #1," ayudaNuevaNota=FALSE"
 '  Print  #1," menuMouse = 0"
 '  Print  #1," nroClick=1 "
 '  Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
 '  Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   Exit Do

  EndIf
 EndIf
 If  comEdit=TRUE And ayudaNuevaNota=FALSE And ayudaModif=TRUE Then 'ESTADO: seleccionar comando
  If (mouseButtons And 1 )  And nroClick = 1  Then

   ayudaModif=FALSE
   menumouse=0
   posinterna=0
   cursorVert = 1:cursorHori = 1
   '  Print #1, "------------------------------------------------------------"
   '  Print  #1,"(2) (mouseButtons And 1 ) And ayudaModif=TRUE And nroClick = 1 And comedit=TRUE "
   '  Print  #1,  "And ayudaNuevaNota=FALSE "
   '  Print  #1,"ESTADO: seleccionar comando "
   '  Print  #1," ayudaModif=FALSE"
   '  Print  #1," menumouse=0"
   '  Print  #1," posinterna=0"
   '  Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   ' Necesitamos mostrar el menu todavia  savemousex=0 : savemousey=0
   If nroClick = 1 Then 'seleccionamos verticalmente ,,,,
    nroClick=2
    If (mousey >= usamousey -120) and  (mousey <= usamousey -100) Then
     modifmouse=1 'borrar =1
     Exit Do
    EndIf
    If (mousey >= usamousey -100) and  (mousey <= usamousey -70) Then
     modifmouse=2 'INSERTAR
     Exit Do
    EndIf
    If (mousey >= usamousey -70) and  (mousey <= usamousey -40) Then
     modifmouse=3 'FIN INSERTAR
     Exit Do
    EndIf
    If (mousey >= usamousey -40) and  (mousey <= usamousey -10) Then
     modifmouse=4 'CAMBIADUR=1 modificar
     Exit DO
    EndIf

   EndIf
  EndIf
 EndIf
 ' <===========   MODIFICACIONES INSERCION
 ' por ahor ainsercion con mouse funciona igual que con keys
 ' perola duracion entrpro tecldonos e porqu enotoma l demouse.
 ' luegode la 1erinsercion con solo click izquierdo repito otra insercion
 ' en lineahorizontal siqueiro otraduracion debere elegirotracon teclado
 ' elcursor seguira al click izq por elcalculo de cursor en este comando
 ' finalmente se usalatecla END para finalizar todas las inserciones.
 ' debere agregr un nuevo comando END paraahcerlo con elmouse....
 If   comedit=TRUE And ayudaModif=FALSE Then
  If (mouseButtons And 1 )  And nroClick = 2  Then
   savemousex=0 : savemousey=0 ' JMG NUEVA
   ' ESTADO: PREPARA COMANDO
   'Print #1, "------------------------------------------------------------"
   'Print #1, "(3) (mouseButtons And 1 ) and ayudaModif=FALSE And nroClick = 2 And comedit=TRUE "
   'Print #1, " ESTADO: PREPARA COMANDO"
   notacur=nE
   curpos= Int((mousex - 81)/20)
   posishow= curpos  + 1 ' NO CAUSA EL +1 EN MODIF MOUSE 03-03-21-15:10
   'Print #1, " savemousex=0 : savemousey=0 ' JMG NUEVA"
   'Print #1, " notacur=nE"
   'Print #1,"curpos= Int((mousex - 81)/20)"
   'Print #1," posishow= curpos + 1"
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   If modifmouse=1 Then  ' anda ok 27 02 2021
    BORRAR=1
    ayudaNuevaNota=TRUE
    Exit Do
   EndIf
   If modifmouse=2 And insert= 0 Then
    INSERT=1
    indaux=0
    '           print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert
    If indaux=0 Then ' no haria falta todas las demas inserciones se deben
     'hacer con I no volver a repetir SC_INSERT sino se pulso SC_END
     StartInsert = posicion + curpos  ' guardo sin modificar el comienzo xxx ok
    EndIf
    '          print #1,">>>SC_INSERT  despues ajuste STARTINSERT ", StartInsert
    Erase (RollAux.trk) ' borro lo que habia en el auxiliar
    Erase (notasInsertadas)
    notins=0
    '          Print #1, ">>>SC_INSERT insert indaux borro RollAux.trk: ",insert,indaux
    'sigue el proceso en RollSub->sub cursor
    ' nroclick=0
    Exit Do
   EndIf
   If modifmouse=2 And insert=1 Then
    insert=2
   EndIf
   If modifmouse=3  Then
    If cursorVert=1 Then ' solo vlido con Ctrl-M
     insert=3
     moveresto (StartInsert,indaux, insert,nota)
     insert=0:indaux=0
    EndIf
    nroClick=0 ' no permite entrar mas por insercion
    modifmouse=0
    ayudaNuevaNota=TRUE
    ' acomoda los compases  <======= organiza Compases
    ReCalCompas() ' organizaCompases()
    ' fin compases
   EndIf
   If modifmouse=4 Then ' modificar
    cambiadur=1
    ayudaNuevaNota=TRUE
    Exit Do
   EndIf
  EndIf
 EndIf
 ''
 '                     <=== INICIO  O P I L F E W H
 If  comedit= TRUE Then
  If (MouseButtons And 2)  Then ''<=== menu de duraciones para seleccionar con click
   ' el resto del code en CrearPenta(), para todaedicion lasduraciones 1 a 8 en letras
   ayudaNuevaNota=TRUE 'ESTADO: CALL MENU DURACIONES O CTRL-M
   ayudaModif =FALSE
   savemousex=0 : savemousey=0 ''ACA NO ¿?
   vuelta=FALSE
   menuMouse = 0

   nroClick=1
   'Print #1, "------------------------------------------------------------"
   'Print #1,"(MouseButtons And 2) and comedit= TRUE"
   ' Print  #1,"(4) ESTADO: CALL MENU DURACIONES O CTRL-M"
   'Print  #1," ayudaNuevaNota=TRUE "
   'Print  #1," ayudaModif =FALSE"
   'Print  #1," savemousex=0 : savemousey=0 "
   'Print  #1,"  vuelta=FALSE"
   'Print  #1,"  menuMouse = 0"
   'Print  #1,"  DUR=0"
   'Print  #1,"  nroClick=1"
   'Print  #1,"comEdit=TRUE "
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   Exit Do
  EndIf
 EndIf
 ' YA VUELVE OK
 if comedit= TRUE And ayudaModif=FALSE Then
  If (mouseButtons And 1 ) And cursorVert = 1  And modifmouse <> 3 Then ' ESTADO: SELECCIONA VUELTA CTRL-P
   'Print #1, "------------------------------------------------------------"
   'Print  #1,"(5) MB1  And AModif=FALSE And CE=TRUE  And CVert = 1 "
   'Print  #1,"5->ESTADO: SELECCIONA VUELTA CTRL-P"
   'Print  #1,"  vuelta=TRUE"
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   vuelta=TRUE
   If mousey <= usamousey -40  And mousey >= usamousey -60 Then
    If mousex >= usamousex -64 And mousex<= usamousex +86 Then
     cursorVert = 0: cursorHori = 0: agregarNota=0:  menuMouse = 0
     ayudaModif=FALSE
     savemousex=0 : savemousey=0
     nroClick=0
     modifmouse=0
     nota=0 ' jmg 03-03-21 22:28 sino le sumaba 1 a Posicion
     ' y trataba de insertarla.....como nota nueva
     'Print  #1,"5-> ctrl-p=> cursorVert = 0: cursorHori = 0: agregarNota=0:  menuMouse = 0"
     'Print  #1,"5-> ayudaModif=FALSE    savemousex=0 : savemousey=0  nroClick=0"

     Exit Do
    EndIf
   EndIf
  endif
 EndIf
 If comedit=TRUE And ayudaModif=FALSE And ayudaNuevaNota=TRUE Then ' ESTADO : SELECCIONA DURACION O CTRL-M
  If (mouseButtons And 1 )  And nroClick = 1  Then

   ayudaNuevaNota=FALSE
   savemousex=0 : savemousey=0
   menuMouse = 0
   'Print #1, "------------------------------------------------------------"
   'Print  #1,"(6) (mouseButtons And 1 ) And ayudaNuevaNota=TRUE And nroClick = 1 And comedit=TRUE "
   'print   #1," 6->And ayudaModif=FALSE Then ' ESTADO : SELECCIONA DURACION O CTRL-M"
   'Print  #1,"(6)MB1  +d ANN=TRUE + NClick = 1 + CE=TRUE + ayudaModif=FALSE"
   'Print  #1,"(6)ESTADO : SELECCIONA DURACION O CTRL-M , nroClick ", nroClick
   'Print  #1," 6 out-> ayudaNuevaNota=FALSE"
   'Print  #1,"  savemousex=0 : savemousey=0"
   'print  #1,"  menuMouse = 0"
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   ' ACA SERA LA ENTRADA POR MOUSE, DUR SALDRÁ DE LA ELECCION DEL MENU DE DURACION
   ' QUE APARECE CON CLICK DERCHO UBICAMOS LA POSICION RELATIVA Y OBTENEMOS LA DURACION
   ' EN EL 1ER CLICK IZQUIERDO, EN EL 2DO CLICK IZQUIERDO ENVIAMOS NOTA Y DURACION
   If  nroClick =1  Then ' determinmos uracion clickeada o seleccionada graficamente
    If mousey >= usamousey +30  And mousey <= usamousey + 44 Then
     If mousex >= usamousex -55 And mousex<= usamousex +102 Then
      ' ESTADO:SELECCION  CTRL-M
      cursorVert = 1: cursorHori = 1: agregarNota=0:  menuMouse = 0
      ''  ayudaModif=TRUE jmg elmenu no debe aparecer hasta dar ctrl-click derecho
      'Print #1,"6 ctrl-M ->cursorVert = 1: cursorHori = 1: agregarNota=0:  menuMouse = 0 "
      'Print #1,"6-> ayudaModif=TRUE"

      Exit Do
     EndIf
    EndIf

    If mousex >= usamousex -100 And  mousex <= usamousex -80 Then ' O en -90
     DUR=1
    EndIf
    If mousex >= usamousex -70 And  mousex <= usamousex -50 Then ' P en -60
     DUR=2
    EndIf
    If mousex >= usamousex -40 And  mousex <= usamousex -20 Then ' I en -30
     DUR=3
    EndIf
    If mousex >= usamousex -10 And  mousex <= usamousex +10 Then ' L en 0
     DUR=4
    EndIf
    If mousex >= usamousex +20 And  mousex <= usamousex +40 Then ' F en +30
     DUR=5
    EndIf
    If mousex >= usamousex +50 And  mousex <= usamousex +70 Then ' E en +60
     DUR=6
    EndIf
    If mousex >= usamousex +80 And  mousex <= usamousex +100 Then ' W en +90
     DUR=7
    EndIf
    If mousex >= usamousex +110 And  mousex <= usamousex +130 Then ' H en +120
     DUR=8
    EndIf

   EndIf
  EndIf
 EndIf

 if comEdit=TRUE And ayudaNuevaNota=FALSE And ayudaModif=FALSE Then
  If DUR > 0 And nE > 0 And nroClick = 1 _
  And modifmouse<> 3 Then ' ESTADO INGRESA O MODIFICA 1ER NOTA
  nota=nE   ''<== 1er nota ingresada para la duracion y nota elegida
  nroClick=0
  'Print #1, "------------------------------------------------------------"
  'Print  #1," DUR > 0 And nE > 0 And nroClick = 1 And ayudaNuevaNota=FALSE and comEdit=TRUE "
  'Print  #1," And ayudaModif=FALSE "
  'Print  #1," (7) ESTADO INGRESA O MODIFICA 1ER NOTA"
  ''Print  #1," 7-><== 1er nota ingresada para la duracion y nota elegida"
  'Print  #1," 7->nota=nE   ", nE
  'Print  #1," 7-> nroClick=0"
  'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

  EndIf
 EndIf
 If comEdit=TRUE And ayudaModif=FALSE And ayudaNuevaNota=FALSE _
   And octavaEdicion = estoyEnOctava Then
  If (mouseButtons And 1) And (DUR > 0) And (nE > 0) And modifmouse<> 3 Then
   'Print #1, "------------------------------------------------------------"
   'Print  #1,"(8) (mouseButtons And 1) And (DUR > 0) And (nE > 0) And ayudaNuevaNota=FALSE "
   'Print  #1,"(8)  And comEdit=TRUE And ayudaModif=FALSE"
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
   nota=nE ' <=== ingresamos varias notas por mouse del mismo valor
   ' hasta que si vuelva a dar click derecho y aparesca de nuevo el menu de duraciones.
   '   Print  #1," nota=nE ", nE
   Exit Do
  EndIf
 EndIf
 ''

  If MouseButtons And 1  Then
     old_btn_press_time = new_btn_press_time
     new_btn_press_time = timer
    If ((new_btn_press_time - old_btn_press_time) < dbl_click_time) Then
      dobleclick=TRUE
    Else
      dobleclick=FALSE
    EndIf
 EndIf


 '                     <===  FIN    O P I L F E W H


 If resize = TRUE Then    ' <===== MOVER Y REDIMENSIONAR LA PANTALLA NO TAN FACIL
  'CLICKEAR CERCA DEL CENTRO Y DRAGAR DERECHA IZQUIERDA ARRIBA ABAJO
  m.res = GetMouse( m.x, m.y, m.wheel, m.buttons, m.clip )
  If m.buttons = 1 And (m.x > 5 ) And (m.y > 5 ) Then
   'dim as integer desktopwidth,desktopheight
   desktopwidth = GetSystemMetrics(SM_CXSCREEN)
   desktopheight =GetSystemMetrics(SM_CYSCREEN)

   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
   ' m.x The new position of the left side of the window.
   ' m.y The new position of the top of the window.
   MoveWindow( hWnd , m.x/2 , m.y/2 , desktopwidth - mxold, desktopheight - myold, TRUE )
   mxold=m.x
   myold=m.y
  EndIf
  Exit Do
 Else
  '  m.res = GetMouse( m.x, m.y, m.wheel, m.buttons, m.clip )
  '  indice = Int(((m.y - bordesuproll)/inc_Penta)) + 1
  ' DEL INDICE DEPENDE TODO EL CONTROL DE OCTAVAY DE NOTAS mentira ni se usa
  /'
     If m.buttons = 2 Then
       Select Case indice
        Case 1
     '       Roll.trk(indice,1) = entraNota
        Case 2
        Case 3
        Case 4
        Case 5
        Case 6
        Case 7
        Case 8
        Case 9
        Case 10
        Case 11
        Case 12

       End Select
    EndIf
  '/


 EndIf
 '  Exit do
 ''  Loop While (m.buttons = 1  )
 If mouseY > 50 Then
  s5=2        'se resetea en EVENT_MOUSE_BUTTON_RELEASE ' obtengoPosicion
  '' ES ACA PROHIBIDO PONER EXIT DO ! NO FUNCION DETECTOR DE OCTAVAS
  '' DEBE SEGUIR EJECUTNDO HACIA ABAJO Y CALCULARINDICE VAMOS A MOVER
  ''ESTEIF AL FINAL
  If MenuNew <> 2 Then
     MenuNew=0
  EndIf
 EndIf
 Exit Do

EndIf ' end  (ScreenEvent(@e)) EVENTOS DE E Y MULTIKEY VAROS ESTAN AHI
' PODRIA SACARSE LOS MULTIKEY DE SCREEN EVENT PERO NO SE SI ANDAN MEJOR DEBO VERIFICAR

If s5<> 1 Then' acelerar mover ventana con el mouse
 Sleep 1 '1000 / 1000 frames = 1 milisegundos
End If
Loop

While Inkey <> "": Wend

If s5<> 1 Then
 Sleep 1 '1000 / 1000 frames = 1 milisegundos
EndIf
' total 2msg, lo mas rapido posible para que no interfiera
' en la entrada y salida de midi, midi reltime hasta 10 ms
' o sea tengo 8msg para procesar se?ales dibujar notas y moverlas...

Loop


#Include "ROLLSUB.BAS"
errorhandler:
Dim as Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error detected ", er, posicion, MaxPos
Print #1,Erl, Erfn,Ermn,Err

Print #1,"------------------------------------"
ErrorNumber = ERR
ErrorLine = ERL
DIM As String ProgError(0 To 17)

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


PRINT #1,"ERROR = ";ProgError(ErrorNumber); " on line ";ErrorLine
Print #1,"Error Function: "; *Erfn()
Dim ers As Integer = nota +(estoyEnOctava -1) * 13
Print #1, "nota +(estoyEnOctava -1) * 13) "; ers
CLOSE

