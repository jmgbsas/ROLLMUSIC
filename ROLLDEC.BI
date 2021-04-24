'miscroll_declare
Declare Sub creaPenta (c AS cairo_t Ptr, ByRef nro As Integer, ByRef po as Integer Ptr, InicioDeLectura As integer)
Declare Sub Tracks (nroTrack As UByte, nroCanal As UByte)
Declare Sub menu (c0 AS cairo_t Ptr,c AS cairo_t Ptr,n As Integer,menuNro As Integer)
declare Sub botones(hWnd As HWND,c As cairo_t Ptr,cm As cairo_t Ptr,x As Integer,y As integer)
Declare Sub menuAcc (c AS cairo_t Ptr)
Declare Sub moveresto (ByVal StartInsert As integer,ByRef indaux as Integer, Byref insert As Integer,Byref nota As integer)
Declare Sub movedato (ByVal StartInsert As Integer, ByRef indaux As Integer, Byref insert As Integer,Byref nota As integer)
Declare sub calcCompas(ByRef j As Integer)
Declare Sub mayorDurEnUnaPosicion (ByRef posn as integer)
Declare Sub organizaCompases()
Declare Function KeyPress(Key As Integer) As Integer
' 128 track,16 canales o sea cada canal con 8 voces
Declare Sub cursor(c AS cairo_t Ptr, ByRef n As Integer, ByRef nro As Integer)
'Declare Function print_text(ByVal x As Integer, ByVal y As Integer, ByRef text As String, _
'        ByVal font As FT_Face, ByVal size As Integer, ByVal col As UInteger ) As Integer
Declare Sub separarDur(ByRef j As Integer,ByRef DUR As Integer,  partes() As Integer,  ByRef cantPartes As Integer, ByRef falta As Integer )

Declare Sub menugrafico(c0 AS cairo_t Ptr,c AS cairo_t Ptr, Cantitems As Integer, items() As String )
Declare Sub ReCalCompas()
Declare Sub CargaArchivo()
Declare Sub CambiarDim()
' para IUP textbox 
Declare Sub dialogoText(cadena As string)
'Declare Sub getfiles(Byref File As OpenFileName,flag As String)



Dim Shared RTA As String 
Dim Shared As Integer nro=0, acumulado=0, controlEdit =0,octavaEdicion=0
Dim Shared As Integer vroll=0, vdur=0 ' borrar
Dim Shared as ubyte  notasInsertadas (1 to 1500)
Dim Shared As String t
Dim Shared As Integer mouseX,mouseY, MouseButtons, notaOld,cursorVert,notacur, cursormove
Dim shared As integer Penta_y, indice, posicion,posicionOld, nota,espacio,posn,cursorHori
Dim shared as integer estoyEnOctava=1, estoyEnOctavaOld=1,backspace,fijarEspacio, offset,borrar,cambiadur
Dim Shared As Integer x2,y2,x1, y1, x0, y0,InicioDeLectura,kNroCol, agregarNota,posishow
Dim Shared As Integer octavas,  jc,ic,id
dim Shared as string cromatica(1 to 12)
dim shared MaxPos as integer ' la nota o posicon mayor alcanzada
Dim Shared As Integer DUR, menuAccion,insert,notins,haycompas,nroCompas=0
Dim Shared NotasGuia (1 To 12) As String * 2 => _
{"B_","A#","A_","G#","G_","F#","F_","E_","D#","D_","C#","C_"}
' 5.7.6.00 tresillos
Dim  Shared figura (1 To 110) As String  * 4 => _
{" O "," P "," I "," L "," F "," E "," X "," H "," W ",_ '  1  9
 " O*"," P*"," I*"," L*"," F*"," E*"," X*"," H*"," W*",_  ' 10 18
 "3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 19 27 
 "sO ","sP ","sI ","sL ","sF ","sE ","sX ","sH ","sW ",_  ' 28 36 
 "sO*","sP*","sI*","sL*","sF*","sE*","sX*","sH*","sW*",_  ' 37 45 
 "s3O","s3P","s3I","s3L","s3F","s3E","s3X","s3H","s3W", _ ' 46 54 
 " O+"," P+"," I+"," L+"," F+"," E+"," X+"," H+"," W+",_  ' 55 63 
 "O*+","P*+","I*+","L*+","F*+","E*+","X*+","H*+","W*+",_  ' 64 72 
 "3O+","3P+","3I+","3L+","3F+","3E+","3X+","3H+","3W+", _ ' 73 81 
 "sO+","sP+","sI+","sL+","sF+","sE+","sX+","sH+","SW+",_  ' 82 90 
 "sO*+","sP*+","sI*+","sL*+","sF*+","sE*+","sX*+","sH*+","sW*+", _ '91 99 
 "s3O+","s3P+","s3I+","s3L+","s3F+","s3E+","s3X+","s3H+","s3W+", _ ' 100 108
 "   ","|| " } '109 110, son 108 (old 73 74 son 72) lospuntillos de tresillos coinciden con elvalor superior
'ej 3I*=I=1.. 
' 73 ESPACIO 74 FIN....109 espacio 110 FIN 
'cambiar 107 por 108, 109 por 110,  109 por 110.
' ligaduras de notas con + que va a + o - creo qu emenso seria mejor
' parecido a una pila polos opuestos se atraen je...
Dim Shared pun As Integer = 0 'entrada con punto = 1 con puntillo
Dim Shared sil As Integer = 0 'silencio entrada con s = 1 con silencio
Dim Shared mas As Integer = 0 ' + continuacion o ligadura
Dim Shared incr As Integer =0
Dim Shared tres As Integer = 0
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
dbl_click_time = 0.5

Dim Shared As Integer menuNro, menuNew, desde , hasta, rango
''Declare Function MessageBox Alias "MessageBoxW"(n1 As Integer,s1 As Wstring,s2 As Wstring,MB As Integer) As Integer:Sleep 1
Dim Shared As Integer x, y, buttons, res,xscreen,anchofig
Dim Shared as Integer StartInsert,indaux,NroCol, carga, curpos, resultado
Dim Shared As Integer pesoDur (1 To 108)  => { _
10000000,5000000,2500000,1250000,625000,312500,156250, 78125,39063, _ '1  9
15000000,7500000,3750000,1875000,937500,468750,234375,117187,58594, _ '10 18
 6666666,3333333,1666666, 833333,416666,208333,104166, 52083,26042, _ '19 27
10000000,5000000,2500000,1250000,625000,312500,156250, 78125,39063, _ '28 36 
15000000,7500000,3750000,1875000,937500,468750,234375,117187,58594, _ '37 45
 6666666,3333333,1666666, 833333,416666,208333,104166, 52083,26042, _ '46 54
10000000,5000000,2500000,1250000,625000,312500,156250, 78125,39063, _ '55 63
15000000,7500000,3750000,1875000,937500,468750,234375,117187,58594, _ '64 72
 6666666,3333333,1666666, 833333,416666,208333,104166, 52083,26042, _ '73 81
10000000,5000000,2500000,1250000,625000,312500,156250, 78125,39063, _ '82 90
15000000,7500000,3750000,1875000,937500,468750,234375,117187,58594, _ '91 99
 6666666,3333333,1666666, 833333,416666,208333,104166, 52083,26042 }  '100 108 

Dim Shared nombreArchivo As String 
' Si revasa el ultimo,8 16,24,32,40,48,56,64, divido po r 8
' tengo elindice del revsado 1,2,3,4,5,6,7,8....
' comprobar qellimite este entre 1 a 8 

' se repiten 3 veces el patron 1 a 16 por comodidad lo repetimos.

' free type
#Include once "freetype2/freetype.bi"
'' The FB headers don't contain this prototype
extern "C"
declare function _
cairo_ft_font_face_create_for_ft_face ( as FT_Face, as long ) as cairo_font_face_t ptr
end extern

'' Convenience
type FreeType
 public:
 declare constructor()
 declare destructor()

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
Dim Shared As Integer menuMouse, savemousex, savemousey, usamousex, usamousey, _
modifmouse, EnOctava,nE,nR,lugarOld, nroClick, posinterna, indCompas