' VERSION 5.7.2 INSERTA NOTAS EN UNA MISMA LINEA CULQUIER NOTA PERO SIEMRPE
' HORIZONTAL, NO INSERTA BIEN ACORDES TODAVIA
'---------------debug log

Open "midebug.txt" for Output As #1
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
#Include "windows.bi" ' en winuser.bi esta el mouse o usamos sdl
#Include Once "win/mmsystem.bi" '' FUNCIONES MIDI!!!!
#If __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf

'' Example showing cairo being used to draw into the FB graphics window
#Include Once "cairo/cairo.bi"
#Include "ROLLDEC.BI"
#Include "NOTAS.bi"
Type dat
 nota As ubyte
 dur As UByte  ' duracion
 vol As UByte  ' volumen
 pan As UByte  ' paneo
 pb  As UByte  ' pitch bend
 inst As UByte ' instrumento para cada nota podra ser distinto 
End Type

Dim Shared As dat Roll  (1 To 128 , 1 To 12000) '  ' 9,216 MBytes, 32 tracks serian 295 Mbytes 
Dim Shared As dat RollAux (1 To 128 , 1 To 12000)
'''Dim Shared As ubyte Insercion (1 To 128, 1 To 12000)
' si inserto en un track o canal debo desplazar a todos!!!
'  de modoque tener una insercion para c7useracrisimo...
' solo resta usar copia y Redim preserve

 ' indice notas, fin compas, nota con puntillo o ligadura,
' se almcena en el heap porque es shared
' 12k posiciones de 96 notas o 128 con help y espacios ...8.388.608 bytes 8 megas 

Dim Shared As Integer  ANCHO 
Dim Shared As Integer   ALTO 
Dim Shared As Double BordeSupRoll, inc_Penta
Dim Shared As Integer  AnchoInicial,AltoInicial, font
Dim Shared q As String * 1
Dim As UByte s1, s2, s3, s4, s5,s6 '', s7,s8,s9
Dim escala As float = 1.0
Dim translado As float = 1.0 
''https://www.freebasic.net/forum/viewtopic.php?t=15127
ANCHO = GetSystemMetrics(SM_CXSCREEN)
ALTO = GetSystemMetrics(SM_CYSCREEN)
ANCHO = ANCHO 
ALTO = ALTO  
AnchoInicial=ANCHO
AltoInicial=ALTO


ScreenControl 103,"GDI" ' le da foco a la aplicacion
'''''ScreenControl POLL_EVENTS '200
'ScreenControl SET_WINDOW_POS ' (100) 'Sets the current program window position, in desktop coordinates.

' https://www.freebasic.net/forum/viewtopic.php?f=6&t=27555
Dim As String driver

 
ScreenRes ANCHO, ALTO, 32,2, GFX_NO_FRAME,GFX_HIGH_PRIORITY
 ScreenControl GET_WINDOW_POS, x0, y0
''ScreenControl SET_WINDOW_POS, 10,10

' CAIRO NO SOPORTA LA �!!! ESO ERA TODO!!!!
Dim As Integer i, octava, posmouse, posmouseOld,incWheel, altofp11,edity1,edity2
altofp11=ALTO
posmouseOld = 0
posmouse = 0
Dim Shared As BOOLEAN comEdit, resize
comEdit = FALSE
resize = FALSE 
Dim po As Integer Ptr
po = @octava
*po = 8
s1=0:s2=0:s3=0:s4=0:s5=0:s6=0 '':s7=0:s8=0:s9=0
font=18
Dim e As EVENT
Dim Shared as Integer StartInsert,ind 
ind=0
' -----------------------------------------------------------------------
' notas redonda (O),blanca(P),negra(I),corchea(C),semicorchea(S), Fusa(F),Semifusa(U)
' O P I L F E # (listo todas mis notas!!!)
' puntillo O+ P+  C+ elsigno  +
' -puntillo resta en vez de sumar -O -P -C ...-U
' O P I L F E # (redonda, blanca,corchea, semicorchea, Fusa,Semifusa) 
' no meconviene escribir encima de las lineas debo cmbiar todo para
' escibirsolo sobre espacios hgmos ROLL4
' -------------------------------------------------------------------------  
BordeSupRoll = Int((ALTO ) /18) ' (1400 )/18 integer = 77
inc_Penta = Int((ALTO - BordeSupRoll) /(40)) ' 26 double 1330/66 para 1400 resolu
BordeSupRoll = BordeSupRoll -  66* inc_Penta ' de inicio muestro octava 4 la central C3
' inc_Penta=separacion de lineas
'---------------------
Dim As Integer mxold,myold, w,h

GetMouse mxold,myold, , MouseButtons 


Dim  c As cairo_t  Ptr
Dim  cm As cairo_t  Ptr
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
'MENU POPUP INCONTROLABLE....SE DISPSRACON CLICK DERECHO,
' DE DESTRUYE CON CLICK IZQUIERDO PERO LUEGO SIEMREPFUNCIONA CON IZQUIERDO
' USARE SOLO MENUES GRAFICOS !!
'Dim Shared hmenu As HMENU 
'Dim Shared As HWND hpopup  
'Dim Shared as zstring * 100 buffer
'Dim Shared exelist As HMENU
'dim Shared as integer x,y
'dim Shared as Point p

''https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexw
''SYTEMcLAS #32768 = https://docs.microsoft.com/en-us/windows/win32/winmsg/about-window-classes#system-classes

         
'exelist = CreatepopupMenu()
'AppendMenu(exelist, MF_STRING,1,"Uno")
'AppendMenu(exelist, MF_STRING,2,"Dos")



'' ---------------  LOOP 1 ---------------
Do 

edity1 = 15 ' botton Edit bordeSup
edity2 = 30 ' botton Edit bordeInf
 
ScreenLock()

 
'' Create a cairo drawing context, using the FB screen as surface.
'' l originalestba mal sizeof(integer ) es mu chico debe ser 4
stride =cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


Var surface = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, ALTO, stride)
Var c = cairo_create(surface)

 
' https://www.cairographics.org/tutorial/

  

If comEdit = TRUE Then
 ' cairo_set_source_rgba(c, 0.6, 0.5, 0.6, 1)
  cairo_set_source_rgba(c, 0.6, 0.6, 0.7, 1)  
Else
  cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
EndIf
cairo_paint(c)
cairo_stroke(c)
cairo_set_line_cap(c, CAIRO_LINE_CAP_ROUND)
cairo_set_line_width(c, 3)


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


'llena la surface con nro_penta 
nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)
'Print nro_penta

 
'  cairo_save (c)
'  cairo_scale (c, escala, 1)
  cairo_set_antialias (c, CAIRO_ANTIALIAS_DEFAULT)
 ' usemos 8 octavas y una para pie de pagina  
   For i = 1 To 9 ' nro_penta
     creaPenta (c, i, po,InicioDeLectura )
     If *po = 99 Then
        *po = 8
      Exit For
     EndIf
     
   Next 
    
''' cairo_restore (c) esto es solo para translado
 
cairo_stroke(c) ' Escribe desde la fuente source a la mask ...(peden ser varias fuentes)

Var surf2 = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, 50, stride)
Var cm = cairo_create(surf2)

' menuNro= 1 resize, edit y move ventana
menu(cm, posicion,menuNro)
cairo_stroke(cm)   
If menuaccion=1111 Then ' no sirve las aciones perforan
 cairo_move_to(c, 9*(ANCHO/10),50)
 Var surf3 = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO/10, ALTO, stride)
 Var c3 = cairo_create(surf3)
 menuAcc (c3)
 cairo_stroke(c3)

EndIf

ScreenUnLock()



'' ---------------  LOOP 2 ---------------
Do ''While InKey =""

If MultiKey(SC_CONTROL) And MultiKey(SC_M)  Then 'MOVER cursor MEJOR CON MOUSECREO
 cursorVert = 1
 cursorHori = 1
EndIf
If MultiKey(SC_CONTROL) And MultiKey(SC_P)  Then 'PARAR cursor MEJOR CON MOUSE ?
 cursorVert = 0
 cursorHori = 0
EndIf

If MultiKey(SC_DOWN)  Then
  If cursorVert=0 Then  
   If s1=0 Then
    s1=1 
   BordeSupRoll = BordeSupRoll -  inc_Penta
   EndIf

   If BordeSupRoll <= - AltoInicial * 2  Then
      BordeSupRoll =  - AltoInicial * 2
   EndIf
   Exit Do
  EndIf 
EndIf

If MultiKey(SC_PAGEDOWN) Then
   
   If s1=0 Then
    s1=1 
   BordeSupRoll = BordeSupRoll - inc_Penta * 6
   EndIf
   If BordeSupRoll <= - AltoInicial * 2 Then
      BordeSupRoll =  - AltoInicial * 2
   EndIf


   Exit Do
EndIf

If MultiKey(SC_PAGEUP ) Then
     'beep
   If s2=0 Then
    s2=1
    BordeSupRoll = BordeSupRoll + inc_Penta * 6
   EndIf
   If BordeSupRoll >= AltoInicial * 0.25  Then
      BordeSupRoll =  AltoInicial * 0.25
   EndIf
   
   Exit Do

EndIf


If MultiKey(SC_PLUS) Or MultiKey(SC_KEYPADPLUS) Then

  
    ALTO = ALTO + 2 'inc_Penta 
    'Print "ALTO+ ", ALTO
'    If altofp11 > 0 Then
'      If ALTO >= altofp11 Then
'         ALTO =  altofp11
'      EndIf
'    Else
      If ALTO >= AltoInicial Then
         ALTO =  Altoinicial
      EndIf
     
'    EndIf 
    
   cairo_set_source_surface (c, surface, ANCHO, ALTO)

   Sleep 10
    Exit Do  
EndIf

If MultiKey(SC_MINUS) Then
    cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1 'evita fondo negro y flick
    cairo_paint(c)
'    cairo_stroke(c)
'   cairo_destroy(c)

   ALTO = AltoInicial/1.5  
'   If ALTO <= AltoInicial/3 Then
'     ALTO= AltoInicial/3
'   EndIf
'    cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1 'evita fondo negro y flick
   cairo_set_source_surface (c, surface, ANCHO, ALTO ) ' ALTO)
'  cairo_stroke(c)
'   Sleep 10
'    cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1 'evita fondo negro y flick
'    cairo_paint(c)
'    cairo_stroke(c)
   Exit Do  
' LA ZONA QUE DEJA ESTA POSICION EN LA PARTE INFERIOR SE USARA PARA
' HACER AJUSTES DE VELOCIDAD CON CURVAS O MOSTRAR CONTROLES ADICIONALES     
EndIf


'   escala = escala - 0.1
If MultiKey(SC_RIGHT)  Then ' <======== RIGHT
   If  mouseY < 50  Then ' seleccion de menu, mouse sobre cinta + teclas 
      menuNro=menuNro+1
      If menuNro > 2 Then
        menuNro=2
      EndIf
      While Inkey <> "": Wend
      Sleep 350
    Exit Do    
   Else
     'k60 cantidadscroll de 60
     k60= Int(posicion/60)
     If  (k60 > 0) And (posicion > 60 * k60) And (posicion < posn)Then
      posicion = posicion + 60
      iniciodelectura = iniciodelectura + 60
     Else
      posicion = posicion + 1
     EndIf
      Sleep 100
     If posicion > posn Then
      posicion = posn
     EndIf
     Exit Do 
   EndIf 
EndIf
'   escala = escala + 0.1
If MultiKey(SC_LEFT)  Then '  <========== LEFT
  If  mouseY < 50  Then  ' seleccion de menu
      menuNro=menuNro - 1
      If menuNro < 0 Then
        menuNro=0
      EndIf
      While Inkey <> "": Wend
      Sleep 350
    Exit Do    
   Else
      'MOVER PENTAGRAMA IZQUIERDA NO CURSOR
     Dim k60 As Integer ' cntidad de scroll de 60
     k60= Int(iniciodelectura/60)
     If  k60 > 0 And posicion = 60*k60 Then
      posicion = posicion - 60
      iniciodelectura = iniciodelectura - 60
     Else
      posicion = posicion - 1
     EndIf
     Sleep 100 
     If posicion < 1 Then
      posicion = 1
     EndIf
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
'
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
        ALTO = altoInicial  - 1
     EndIf
      
'      ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
      
'      Dim As hWnd hwnd = Cast(hwnd,IhWnd)
      MoveWindow( hWnd , 10, (10+ALTO-h)\2, ANCHO,ALTO, TRUE )
      
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
if Multikey (SC_F12) then
 dim as integer i1, i2
 ' testeo solo en la 1er octva por ahora
 print #1,
 for i1 = 1 to 12
     for i2= 1 to posicion
         print #1, Roll(i1, i2).nota;"-";
     next i2
     print #1, 
     for i2= 1 to posicion
        print #1, Roll(i1, i2).dur;"-";
      next i2
     print #1, 
     print #1,"------------------------"
 next i1 
 While Inkey <> "": Wend
 sleep 150
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
  
  if MessageBox(hWnd,"ARE YOU SURE YOU WANT TO QUIT?","RollMusic End ",4 or 64) =6 then 
     cairo_destroy(c)
     Close 1
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
 If MultiKey(SC_SPACE) Then 'barra espacio  
     espacio = 1
     posicion= posicion + 1
     DUR=45
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
       Exit Do
 EndIf
 
 If MultiKey(SC_CONTROL) And MultiKey(SC_C)   Then ' C#      
      nota = 11 
      If espacio = 1 Then
          espacio=11 
      EndIf   
    Exit Do  
 EndIf
 If MultiKey(SC_CONTROL) And MultiKey(SC_D)  Then ' D# 
      nota= 9
   If espacio = 1 Then
       espacio=9 
   EndIf   
  Exit Do 
 EndIf
 If MultiKey(SC_CONTROL) And MultiKey(SC_F) Then ' F#
      nota= 6
    If espacio = 1 Then
       espacio=6 
    EndIf   
    Exit Do
 EndIf
 If  MultiKey(SC_CONTROL) And MultiKey(SC_G)  Then ' G#
      nota= 4
     If espacio = 1 Then
        espacio=4 
     EndIf 
     Exit Do  
 EndIf

 If MultiKey (SC_A) Then
       nota= 3
       If espacio = 1 Then
          espacio=3 
       EndIf
       Exit Do
 EndIf
 If MultiKey (SC_B) Then
       nota = 1
       Exit Do
  EndIf       
 
  If MultiKey (SC_C) Then
       nota = 12
       If espacio = 1 Then
          espacio=12 
       EndIf
       Exit Do
 EndIf       

 If MultiKey (SC_D) Then
     nota= 10 
      If espacio = 1 Then
          espacio=10 
      EndIf
      Exit Do
 EndIf
 If MultiKey (SC_E) Then
     nota = 8 
     If espacio = 1 Then
          espacio=8 
     EndIf
     Exit Do
 EndIf
 If MultiKey (SC_F) Then
        nota= 7
     If espacio = 1 Then
          espacio=7 
     EndIf
     Exit Do
 EndIf
If MultiKey (SC_G) Then
       nota= 5
     If espacio = 1 Then
          espacio=5
     EndIf
     Exit Do
EndIf
If MultiKey(SC_BACKSPACE) Then 
    backspace=1 
 EndIf
' ----------------------FIN NOTAS-------------------------
' ----------INGRESO DE DURACIONES DE NOTAS -------------
If comEdit = TRUE Then 
  If MultiKey(SC_1) Then 
    DUR = 13 :Exit Do
  EndIf
  If MultiKey(SC_2) Then 
    DUR = 14:Exit Do
  EndIf 
  If MultiKey(SC_3) Then 
    DUR = 15:Exit Do
  EndIf 
  If MultiKey(SC_4) Then 
    DUR = 16:Exit Do
  EndIf 
  If MultiKey(SC_5) Then 
    DUR = 17:Exit Do
  EndIf 
  If MultiKey(SC_6) Then 
    DUR = 18:Exit Do
  EndIf 
  If MultiKey(SC_7) Then 
    DUR = 19:Exit Do
  EndIf 
  If MultiKey(SC_8) Then 
    DUR = 20:Exit Do
  EndIf 
  If MultiKey(SC_PERIOD) Then 
    pun = 1:Exit Do  ' puntillo
  EndIf 
  If MultiKey(SC_S) Then 
    sil = 1:Exit Do  ' silencio 
  EndIf 
  ' ojo ver q no habia  exit do antes !!!!!
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
 If comEdit = TRUE  And nota> 0 Then 
    posn=1 + InicioDeLectura
    If estoyEnOctava <> 99 Then ' estoy en una octava 
     '  If indice <= 0 Then 
     '      indice = 1 
     '  EndIf
     '  If indice >= 128 Then 
     '      indice = 128 
     '  EndIf
     If nota > 0 And estoyEnOctava < 99 Then 
       ' ====>  Control PAgindo Horizontal <=======
 '      k60= Int(posicion/60)

       Do 
         If Roll((nota +(estoyEnOctava -1) * 13),posn).nota = 0 OR _
          Roll((nota +(estoyEnOctava -1) * 13),posn).nota = 46  Then
           posicion=posn 
           Exit Do
         EndIf
         posn = posn + 1
         If (posn > 60 + InicioDeLectura) Then 
            InicioDeLectura=InicioDeLectura+60  
         EndIf
        
       Loop
       ' ESTO ME UBICA EN QUE RENGLON DE LA OCTVA ESTOY SN USAR EL MOUSE
       ' LUEGO haRE ALGO CON EL MOUSE POR AHORA TODO TECLADO
        Roll((nota +(estoyEnOctava -1) * 13),posn).nota = nota 'carga
        If cursorVert = 0 and cursorHori = 0 Then
         ' no actua para modificaciones o agregado en lo existente
         ' 46 o FIN indica final de TODO es la MAXPOS (+1obvio),se usara
         ' para insertar y saber hasta donde se debe mover...esta solo 
         'en dur no afecta a notas
          Roll((nota +(estoyEnOctava -1) * 13),posn+1).dur = 46
          if notaOld > 0 then
           Roll((notaOld +(estoyEnOctava -1) * 13),posn).dur = 45
          endif 
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
            If Roll((i +(estoyEnOctava -1) * 13),posn).nota = 0 Then
               Roll((i +(estoyEnOctava-1) * 13), posn).nota = 45  
            EndIf 
         EndIf
        Next

        notaOld = nota
        If pun  = 0 And sil=0 Then ' no hay puntillo ni silencio
          Roll((nota +(estoyEnOctava -1) * 13),posn).dur = DUR 'era duracion
          'DUR nunca se ahce cero solo para espacio ergo si pulso
          ' la misma u otra nota sigue con la misma duracion
        Else 
          If pun = 1 And sil=0 Then
           Roll((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 8 'era dur
           pun = 0
          Else 
            If sil = 1 And pun=0 Then
              Roll((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 16 'era dur
               sil=0
            Else ' sil=1 and pun=1
              Roll((nota +(estoyEnOctava -1) * 13),posn).dur = DUR + 24
              sil=0:pun=0
            EndIf
            If DUR=45 Then
               Roll((notaOld +(estoyEnOctava -1) * 13),posn).dur = 45
               DUR=0
            EndIf
          EndIf
        EndIf
        
        nota = 0 
     Else ' edicion de nota anterior retroceso, concosco la posicion la octava 
      'pero no la nota 1ero debo recuperar la nota, cursor lo sabe tomar de ahi
      'no puedo usar notaOld porque eso seria solo en el caso de que estaba editando
      ' para esa nota 1ero deberia oder moverme arribay bjo con el cursor para
      ' posicionarmeen una nota....ctrl-felcahs verticales ubicaran en cursor
      
     EndIf
    'print " Roll ", Roll (indice, posicion)   
    
  ' mostrarla en la t del Roll en el indice correspondiente 
  ' ocalculo elindice en cada t y meto la nota o saco en t las otas
  ' del vector Roll pra ello acaa la grabo enRoll
    EndIf
   Exit Do 'kkkk 30-01-21 probando 
 EndIf
    
 
  
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
          If BordeSupRoll >= AltoInicial * 0.25   Then
             BordeSupRoll =  AltoInicial * 0.25
          EndIf

          Exit Do
      Else 
         If s1=0 Then
            s1=1 
            BordeSupRoll = BordeSupRoll - inc_Penta 
         EndIf
         If BordeSupRoll <= - AltoInicial * 2  Then
            BordeSupRoll =  - AltoInicial * 2
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
        If BordeSupRoll >= AltoInicial * 0.25  Then
           BordeSupRoll =  AltoInicial * 0.25
        EndIf

        Exit Do
      EndIf 
      If cursorVert=1 Then   
         notacur=notacur-1
         If notacur < 1 Then
           notacur=12
         EndIf
         Exit Do
      EndIf 
    EndIf
    
   If e.scancode = &h41 Then ' <======= F7
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
        MoveWindow( hWnd , 10 , (10+h-ALTO)\2, ANCHO,ALTO, TRUE )
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
  If  e.scancode = 68 Then ' <====== F10 'no funciona no lodetecta !!
      font = font + 1
      Exit Do
  EndIf
  If e.scancode = 80 Then  ' <===== SC_DOWN pulso
     If cursorVert=1 Then
        notacur = notacur + 1
        If notacur > 12 Then
           notacur=1
        EndIf
     EndIf
     Exit Do
  EndIf
  If e.scancode = 83 Then '<====== delete cambia a silencio o nada le suma 16+16 ver eso
     borrar = 1 
     Exit Do
  EndIf
  If e.scancode = SC_X Then ' 45 <==== SC_X ...fix 
   'corrige nota cambia duracion o agrega nota nueva, acorde melodia
     cambiadur = 1    ' usando 1 a 8 para silencio esta delete
     Exit Do
  EndIf
' para insertar en cada iten de Roll cda vez queingreso nota al final
' comunmente se agregara 46 para indicar fin de datos..y loindicaremos 
'con FIN por ahora para visualizarlo  

  If e.scancode = SC_INSERT And insert=0  Then '82 <===== SC_INSERT
   If cursorVert = 1 And  cursorHori = 1 And insert = 0 Then
    ' solo tiene sentido insertar en lo echo y en cursor libre
     insert=1 ' comienzo hbilitotel I para insertr nota por nota 
     ind=0
    print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert 
    StartInsert = posicion ' guardo sin modificar el comienzo
    print #1,">>>SC_INSERT  despues ajuste STARTINSERT ", StartInsert 
    Erase (Rollaux) ' borro lo que habia en el auxiliar
    Erase (notasInsertadas) 
    notins=0
    Print #1, ">>>SC_INSERT insert ind borro RollAux: ",insert,ind
    'sigue el proceso en RollSub->sub cursor 
   EndIf
  EndIf
  If e.scancode = SC_I And insert=1 Then
      insert= 2 ' hbilito cursor para queingrese 
     Print #1, "-----SC_I insert,ind : ",insert,ind
  EndIf
  If e.scancode = SC_END Then 
     ' no mas reemplazos
     insert=3
     Print #1, "-----SC_END StartInsert,ind,insert,nota: ",StartInsert,ind,insert,nota 
     Print #1,"ind no deberia valer cero !!!! es global "
     moveresto (StartInsert,ind, insert,nota) 
 ' param : posicion comienzo (fijo), indice incremental para el aux 
 ' ,insert comando habilitado = 1
 ' insert 3 fin reemplazos comienzo de move total
    insert=0:ind=0
  EndIf
' ------------------PULSAR MUCHO TIEMPO <====== REPEAT------
   Case EVENT_KEY_REPEAT
     If e.scancode = 72  Then ' <======= SC_UP
        If cursorVert = 0 Then    
          If s2=0 Then
            s2=1
            BordeSupRoll = BordeSupRoll +   2 * inc_Penta
          EndIf
          If BordeSupRoll >= AltoInicial * 0.25  Then
             BordeSupRoll =  AltoInicial * 0.25
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
  
  GetMouse mouseX, mouseY, , MouseButtons
  If (mouseY >= edity1 ) And (mouseY <= edity2) Then
     If (mouseX >= 36) And (mouseX <= 70)  Then
        If MouseButtons And 1 Then
           If s3 = 0 Then
            comEdit = TRUE : s3 = 1
            font = 18
            Exit Do
           Else
            comEdit = FALSE : s3 = 0
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
  
  If mouseY < 50 And s5= 0 And mouseX > 70 Then 
    x1=mouseX: y1=mouseY 
    s5=1
  EndIf
  If MouseButtons And 1 And s5=1 And mouseX > 70 Then
    x2=mouseX 
    y2=mouseY 
    x0=x0+x2-x1
    y0=y0+y2-y1
    ScreenControl SET_WINDOW_POS, x0, y0
   ' mientras mantengo presiondo el mouse pudo mover el mouse con la ventana
   ' la performance no es tan buena pero funciona 
   Exit Do ' probando ac creo es el poblem mmmm and ok !
  EndIf  
  
 'https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-movewindow
 If resize = TRUE Then    
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
'   indice = Int(((m.y - bordesuproll)/inc_Penta)) + 1
   ' DEL INDICE DEPENDE TODO EL CONTROL DE OCTAVAY DE NOTAS mentira ni se usa
  /'  
     If m.buttons = 2 Then
       Select Case indice 
        Case 1
     '       Roll(indice,1) = entraNota 
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
  EndIf

  Exit Do 

EndIf ' end  (ScreenEvent(@e))

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


#Include "ROLLSUB.BI"
errorhandler:
Dim er As Integer 
er = Err
Print "Error detected ", er
Print Erl, Erfn,Ermn,Err
 