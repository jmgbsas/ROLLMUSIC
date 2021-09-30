#Include Once "freetype2/freetype.bi"
'' The FB headers don't contain this prototype
'extern "c"
'Declare function usleep( as ulong) as long '=0 when working
'end Extern

' code from
'https://freebasic.net/forum/viewtopic.php?t=23680
'Declare sub RtlInitUnicodeString alias "RtlInitUnicodeString"(as PUNICODE_STRING,as PCWSTR)
'Declare function NtDisplayString alias "NtDisplayString"(as PUNICODE_STRING)as NTSTATUS
Declare function NtDelayExecution alias "NtDelayExecution"(as BOOLEAN,as PLARGE_INTEGER)as NTSTATUS
Declare function NtTerminateProcess alias "NtTerminateProcess"(as HANDLE,as NTSTATUS)as NTSTATUS
'#define NtCurrentProcess() cast(HANDLE,-1)
'our entry point
'sub startup cdecl() 'NtProcessStartup... any name is ok
'    dim as UNICODE_STRING us
'    dim as LARGE_INTEGER delay
'    delay.QuadPart = -10000 '1 m seconds
'  ' RtlInitUnicodeString(@us,@wstr("FreeBasic Native Application"))
'  '  NtDisplayString(@us) 'show the message
'    NtDelayExecution(FALSE,@delay) 'so we can see the message
'    NtTerminateProcess(cast(HANDLE,-1),0)
'end Sub


extern "C"
declare function _
cairo_ft_font_face_create_for_ft_face ( as FT_Face, as long ) as cairo_font_face_t ptr
end extern

'' Convenience
type FreeType
 public:
 declare constructor()
 Declare destructor()

 declare operator Cast() as FT_Library

 private:
 as FT_Library _library
end type

constructor FreeType()
FT_Init_FreeType( @_library )
end constructor

destructor FreeType()
FT_Done_FreeType( _library )
end destructor

operator FreeType.cast() as FT_Library
 return( _library )
end Operator

'Declare Sub RollLoop (c As cairo_t Ptr, Roll As inst)
'Declare Sub creaPenta (c AS cairo_t Ptr, Roll As inst  )

Declare Sub Tracks (nroTrack As UByte, nroCanal As UByte)
'Declare Sub menu (c0 AS cairo_t Ptr,c AS cairo_t Ptr,n As Integer,menuNro As Integer)
declare Sub botones(hWnd As HWND,c As cairo_t Ptr,cm As cairo_t Ptr,x As Integer,y As integer)
Declare Sub menuAcc (c AS cairo_t Ptr)
Declare Sub moveresto (ByVal StartInsert As integer,ByRef indaux as Integer, Byref insert As Integer,Byref nota As integer)
Declare Sub movedato (ByVal StartInsert As Integer, ByRef indaux As Integer, Byref insert As Integer,Byref nota As integer)
'Declare sub calcCompas(ByRef j As Integer)
Declare Sub mayorDurEnUnaPosicion (ByRef posn as integer)
Declare Sub organizaCompases()
Declare Function KeyPress(Key As Integer) As Integer
' 128 track,16 canales o sea cada canal con 8 voces
'Declare Sub cursor(c AS cairo_t Ptr, ByRef n As Integer, ByRef nro As Integer)
'Declare Function print_text(ByVal x As Integer, ByVal y As Integer, ByRef text As String, _
'        ByVal font As FT_Face, ByVal size As Integer, ByVal col As UInteger ) As Integer
Declare Sub separarDur(ByRef j As Integer,ByRef DUR As Integer,  partes() As Integer,  ByRef cantPartes As Integer, ByRef falta As Integer )

Declare Sub menugrafico(c0 AS cairo_t Ptr,c As cairo_t Ptr, Cantitems As Integer, items() As String )
'Declare Sub ReCalCompas()
'Declare Sub CargaArchivo()
Declare Sub CambiarDim(redi As integer)
' para IUP textbox 
Declare Sub dialogoText(cadena As string)
Declare Sub velocidades(j as integer)
Declare Sub playthread()
Declare Sub GrabarArchivo ()
Declare Sub GrabarTrack   (ntk As integer)
Declare Sub GrabarRollaTrack (cambianombre As Integer, enCancion As integer)
Declare Sub CargarPistasEnCancion ()
' gtk
Declare Sub GtkListBox()
Declare Sub sigh_button_event Cdecl ( Byval gtklist As GtkWidget Ptr, ByVal event As GdkEventButton Ptr, Byval frame As GtkWidget Ptr )
Declare Sub sigh_print_selection Cdecl ( Byval gtklist As GtkWidget Ptr, Byval func_data As gpointer )
' end gtk
Declare sub selport(ByVal tipo As any ptr) 
Declare Sub ComboBox()
Declare Sub grabaprueba()
Declare Sub borrarColumnasMarcadas()
Declare Function SumarnR (notaPiano As Integer) As Integer


Dim shared As Integer cnota, cdur, cvel, cinst,cnEOld,CnE, veoPent=1 
Dim Shared As Integer ev1,ev2,ev3,ev4,ev5,ev6
Dim Shared RTA As String 
Dim Shared As Integer acumulado=0, controlEdit =0,octavaEdicion=0, pasoZona1=0, pasoZona2=0, pasoNota=0
Dim Shared As Integer vnotaOld=0, vdur=0 ,vnota =0, playloop=0, SelGrupoNota =0,moverZona=0, copiarZona=0
Dim Shared as ubyte  notasInsertadas (1 to 1500)
Dim Shared As String t
Dim Shared As Integer mouseX,mouseY, MouseButtons, notaOld,cursorVert,notacur, cursormove
Dim shared As integer Penta_y, indice, posicion=1,posicionOld=1, nota=0,espacio,posn=1,cursorHori
Dim shared as integer estoyEnOctava=1, estoyEnOctavaOld=1,backspace,fijarEspacio, offset,borrar,cambiadur
Dim Shared As Integer x2,y2,x1, y1, x0, y0,InicioDeLectura,kNroCol, agregarNota,posishow=1
Dim Shared As Integer octavas,  jc,ic,id,listaMIDI=0,ShowNroCol=0,nro,trak=1,trackold=1
dim Shared as string cromatica(1 to 12)
dim shared As Integer MaxPos=2,ntk=0,CPlay=0, guardopos=0, cargaCancion=0
Dim Shared As String ProgError(0 To 17)
Dim Shared ers As Integer  
Dim Shared As Integer DUR, menuAccion,insert,notins,haycompas,nroCompas=0,copiar=0,copi=0
Dim Shared NotasGuia (0 To 11) As String * 2 => _
{"B_","A#","A_","G#","G_","F#","F_","E_","D#","D_","C#","C_"}
' 5.7.6.00 tresillos
espacio=0:notacur=1 '16-05-2021 jmg
Dim  Shared figura (1 To 182) As ZString  * 4 => _
{" O "," P "," I "," L "," F "," E "," X "," H "," W ",_  '  1  9
 " O4"," P4"," I4"," L4"," F4"," E4"," X4"," H4"," W4",_  ' 10 18
 " O*"," P*"," I*"," L*"," F*"," E*"," X*"," H*"," W*",_  ' 19 27
 " O:"," P:"," I:"," L:"," F:"," E:"," X:"," H:"," W:",_  ' 28 36
 "3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45 
 "sO ","sP ","sI ","sL ","sF ","sE ","sX ","sH ","sW ",_  ' 46 54 
 "SO.","SP.","SI.","SL.","SF.","SE.","SX.","SH.","SW.",_  ' 55 63
 "sO*","sP*","sI*","sL*","sF*","sE*","sX*","sH*","sW*",_  ' 64 72 
 "SO:","SP:","SI:","SL:","SF:","SE:","SX:","SH:","SW:",_  ' 73 81
 "s3O","s3P","s3I","s3L","s3F","s3E","s3X","s3H","s3W", _ ' 82 90 
 " O+"," P+"," I+"," L+"," F+"," E+"," X+"," H+"," W+",_  ' 91 99 
 "O4+","P4+","I4+","L4+","F4+","E4+","X4+","H4+","W4+",_  ' 100 108
 "O*+","P*+","I*+","L*+","F*+","E*+","X*+","H*+","W*+",_  ' 109 117 
 "O:+","P:+","I:+","L:+","F:+","E:+","X:+","H:+","W:+",_  ' 118 126
 "3O+","3P+","3I+","3L+","3F+","3E+","3X+","3H+","3W+", _ ' 127 135 
 "sO+","sP+","sI+","sL+","sF+","sE+","sX+","sH+","SW+",_  ' 136 144 
 "SO4+","SP4+","SI4+","SL4+","SF4+","SE4+","SX4+","SH4+","SW4+",_  '145 153
 "sO*+","sP*+","sI*+","sL*+","sF*+","sE*+","sX*+","sH*+","sW*+", _ '154 162
 "SO:+","SP:+","SI:+","SL:+","SF:+","SE:+","SX:+","SH:+","SW:+",_  '163 171 
 "s3O+","s3P+","s3I+","s3L+","s3F+","s3E+","s3X+","s3H+","s3W+", _ '172 180
 "   ","|| " } ' 181 182 - 109 110, son 108 (old 73 74 son 72)
   
 'los puntillos de tresillos coinciden con elvalor superior
'ej (3I)*=I=1.. 1/3 tresillo de negra con puntillo = negra
' (3L)* = L ..1/3 tresillo de corchea con puntillo = corchea
' ergo no hace falta simbolos adicionales para tresillos con puntillo
' elprogram debe reemplazar si se entra tresillo con puntillo por
'la figura sin puntillo...
' 4/3 4 fig en espaciode 3 ya lo tenemos por ejemplo 
' L*L*L*L*=III 
' 73 ESPACIO 74 FIN....181 espacio 182 FIN 
'cambiar 107 por 108, 109 por 110,  109 por 110.
' ligaduras de notas con + que va a + o - creo qu emenso seria mejor
' parecido a una pila polos opuestos se atraen je...
Dim Shared As Integer comienzo1, fin1, comienzo2,fin2,PianoNota, trasponer=0, _
loopDeThreads=0 ,sefini=0
'comienzo1=28:fin1=54
'comienzo2=82:fin2=180

Dim Shared as any ptr thread1, thread2,threadPenta,thread3,pubi,threadloop,p1,threadMenu
Dim Shared As Any Ptr p2,tl1,tl2,tl3,tl4,tl5,tl6,tl7,tl8,tl9,tl10,tl11,tl12 

Dim Shared pun As Integer = 0 'entrada con punto = 1 con puntillo
Dim Shared sil As Integer = 0 'silencio entrada con s = 1 con silencio
Dim Shared mas As Integer = 0 ' + continuacion o ligadura
Dim Shared  As Integer incr=0 , entroActrlM =0, salgoDeCtrlM=0
Dim Shared tres As Integer = 0
Dim shared doblepun As Integer =0
Dim Shared cuart As Integer=0 'nro q indica sumarle 1/4 de su valor 
'' futuro=> Dim notmus(13 To 44) As String = {} bravura fonts?? pero son glyphs
''los deberia cargar cairo...experimentar...tarea futura
' 32 tipo de duraciones basicas encendidas o apagadas y lo mismo con puntillo
' I** negra con 2 puntilos seguidos tiene sentido?
' es I+L+L+F=I+I+F .' entre las notas entradas...
' la tecla para entrar puntillo el punto (.), para entrar silencio la s
' ej tecleo 1+.+A=O* en linea A dela octava que sea
' ej tecleo s+1+.+A =sO* en A..
' tambien en vez de estos simbolos usare los de un pentagrama comun,,,,futuro
Dim Shared As BOOLEAN ayuda, ayudaNuevaNota, ayudaModif, vuelta,dobleclick
haycompas=0
dobleclick=>FALSE

Dim Shared d7 As Integer => 10000000 ' PROBAREMOS CON 12/8 O 6 NEGRAS,,,15 MILL

' comienza el compas , se calculara dinamicamente,,,según posicion  y la historia anterior
' del mismo array (acumulado, nro Compas, posicion)

espacio=0:cursorVert=0:borrar=0:cambiadur=0:InicioDeLectura=0
menuAccion=0:insert=0:agregarNota=0:octavas=0
' Const LEFTBUTTON = 1 
' Const MIDDLEBUTTON = 4 
' Const RIGHTBUTTON = 2 
' Const SHOWMOUSE = 1
' Const HIDEMOUSE = 0 
Dim Shared As Integer MousePress = 0, MouseMove=0 '02-07-2021
Type mouse
 As Integer res
 As Integer x, y, wheel, clip
 Union
 buttons As Integer
 Type
  Left:1 As Integer
  Right:1 As Integer
  middle:1 As Integer
 End Type
 End Union
End Type
Dim shared As mouse m
Dim Shared As Double old_btn_press_time, new_btn_press_time,dbl_click_time,redimensionar=0
dbl_click_time = 0.18
Common shared As Integer menuNro, menuNew, desde , hasta, rango,RollDur,RollNota,compasX

''Declare Function MessageBox Alias "MessageBoxW"(n1 As Integer,s1 As Wstring,s2 As Wstring,MB As Integer) As Integer:Sleep 1
Dim Shared As Integer x, y, buttons, res,xscreen,anchofig,gap1,gap2,gap3
Dim Shared as Integer StartInsert,indaux,NroCol, carga, curpos, curposold, resultado, indicePos
Dim Shared As Integer pesoDur (0 To 182)  => {0, _ 'era 108
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '1  9
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '10 18
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '19 27
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '28 36
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '37 45
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '46 54 
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '55 63
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '64 72
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '73 81
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '82 90
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '91 99
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '100 108
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '109 117
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '118 126
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '127 135
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '136 144
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '145 153
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '154 162
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '163 171
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042,0,0 }  '172 182

Dim Shared As String nombreArchivo, TCompas,nombre, menuOldStr
nombre=""
Dim Shared As UByte Vfuerte=70,Vsemifuerte=60,Vdebil=50
TCompas ="4/4"
'Dim Shared As Integer MaxPosTrack(0 To 32) ' cada track tendra su max pos
'Dim Shared As Integer posnTrack(0 To 32) ' cada track tendra su posn
Dim Shared As Integer fueradefoco=0, tope=0
Type rangoOct
 As Integer desde
 As Integer hasta
 As Integer NB
 As Integer NA
 As Integer MaxPos
 As Integer posn
 As UByte   notaold
 As Integer Ticks
End type
Dim Shared As rangoOct pmTk (0 To 32)
Dim Shared As String titulos(0 To 32)  
Dim Shared As Integer ContadorError=0  
' Si revasa el ultimo,8 16,24,32,40,48,56,64, divido por 8
' tengo elindice del revsado 1,2,3,4,5,6,7,8....
' comprobar qellimite este entre 1 a 8 

' se repiten 3 veces el patron 1 a 16 por comodidad lo repetimos.
' equivalencias L+F=4 notas en 5=4:5, 4 figuras en 5 negras.
' 5I=12500000,5I/4=3125000=I+F o sea I+F I+F I+F I+F = 4 en 5
' podria ponerlo asi IF IF IF IF. IF=3125000 NUEVO VALOR A AGREGAR?
' VALORES LIGADOS EN UNA SOLA FIGURA FACIL PARA CARGAARLO PONGO
' I-F O SEA PULSO 3 LUEGO PULSO - Y LUEGO PULSO F = 3-5 = IF
'  ASI PUEDO LIGAR LAS NOTAS QUE QUIERO...LAS NOTAS LIGADAS
'LA SPONDRE AL FINAL DEL VECTOR...! CUANTAS TENGO?
'
' - 4 figuras en 3 negras. si tomamos como base el 4/4
' 3 negras=7500000 / 4 = 1875000=L*
' o sea 4 L* =4/3=>L*L*L*L*=III (4/3 lo tengo)
' DOBLE PUNTILLO : ó ** formula x,x+1,x+2 unidos
'Ctrl nro 1,2,3,4,5,6,7,8 9 notiene sentido
' 123 234 345 456 567 678 789 son7 doblepuntilo simple

Dim Shared As Integer menuMouse, savemousex, savemousey, usamousex, usamousey, _
modifmouse, EnOctava,nE,nR,lugarOld, nroClick, posinterna, indCompas

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

Dim Shared As Long CONTROL1 = 0
Dim Shared As Integer NB , NA, CantTicks, tempo, CantMin,CantCompas
Dim Shared As inst Roll ' para visualizar y tocar
Dim Shared Track  (0 To 64) As sec ' tracks para guardar,.. y tocar 

Dim Shared ix As Integer=0
Dim Shared titu As String
Dim Shared As Integer instru, instruOld
Dim Shared As Integer ubirtk, ubiroll
Dim Shared As Integer desdevector
Dim Shared As Integer hastavector
' memorias de Tracks..por ahora igual que Roll de trabajo luego veremos si achicamos
' mas parecido a midi.,,,,8 tracks ocupasn 0,544 giga 32 1,8 gigas de memoria virtual
'Dim Shared As Integer lim2=12, ctres=CantTicks
Dim Shared As Integer lim2=12
 
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
Dim Shared As inst RollAux
Dim Shared As Integer  ANCHO
Dim Shared As Integer  ALTO, deltaip=0
Dim Shared As Double   BordeSupRoll, inc_Penta
Dim Shared As Integer  AnchoInicial,AltoInicial
Dim Shared As FLOAT font, deltaipf=0, lockip=0 
Dim Shared q As String * 1
Dim Shared As UByte s1, s2, s3, s4, s5,s6, s7 ,s8 ',s9
Dim Shared escala As float = 1.0
Dim Shared translado As Integer = 1
' CAIRO NO SOPORTA LA ñ!!! ESO ERA TODO!!!!
Dim shared As Integer i,  posmouse, posmouseOld,incWheel, edity1,edity2,octavaloop
Dim Shared As Integer octaroll
Dim Shared As BOOLEAN comEdit, resize
Dim Shared po As Integer Ptr
Dim Shared e As EVENT
Dim Shared rmerr As Integer
Dim Shared As Integer mxold,myold, w,h
Dim Shared c As cairo_t  Ptr
Dim Shared cm As cairo_t  Ptr
Dim Shared c3 As cairo_t  Ptr
Dim Shared As Integer stride, nro_penta,IhWnd,Mhwnd,Style,desktopwidth,desktopheight
Dim Shared surface As Any Ptr
Dim Shared comienzo As Integer = 0
'ScreenControl  SET_DRIVER_NAME,"GDI"
Dim Shared As hWnd hwnd,hwndMenu 
'hwnd = Cast(hwnd,IhWnd)
'hwndmENU = Cast(hwnd,MhWnd)
Dim Shared As UINT codsalida=0
Dim shared As Any Ptr lpExitCode
'---------------------
ANCHO = GetSystemMetrics(SM_CXSCREEN)
ALTO = GetSystemMetrics(SM_CYSCREEN)
ANCHO = ANCHO *11/12
ALTO = (ALTO -25)*11/12
AnchoInicial=ANCHO
AltoInicial=ALTO
anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "

'--------------------
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
COMMON Shared As Long eventc
Common Shared As hwnd hwndListBox
