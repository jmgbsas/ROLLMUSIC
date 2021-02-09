'miscroll_declare
Declare Sub creaPenta (c AS cairo_t Ptr, nro As Integer, po as Integer Ptr, InicioDeLectura As integer)
Declare Sub Tracks (nroTrack As UByte, nroCanal As UByte)
Declare Sub menu (c AS cairo_t Ptr,n As Integer,menuNro As Integer)
Declare Sub menuAcc (c AS cairo_t Ptr) 
Declare Sub moveresto (ByVal StartInsert As integer,ByRef ind as Integer, Byref insert As Integer,Byref nota As integer)
Declare Sub movedato (ByVal StartInsert As Integer, ByRef ind As Integer, Byref insert As Integer,Byref nota As integer)
' 128 track,16 canales o sea cada canal con 8 voces
Declare Sub cursor(c AS cairo_t Ptr, n As Integer,nro As Integer)
 
Dim Shared nro As Integer '''borrar despues jmg ja quedo 
Dim Shared As Integer vroll, vdur ' borrar
Dim  shared as ubyte  notasInsertadas (1 to 1500) 
CONST LEFTBUTTON   = 1
CONST MIDDLEBUTTON = 2
CONST RIGHTBUTTON  = 4
CONST SHOWMOUSE    = 0
CONST HIDEMOUSE    = 1
Dim Shared As Integer mouseX,mouseY, MouseButtons, notaOld,cursorVert,notacur, cursormove
Dim shared As integer Penta_y, indice, posicion, nota,espacio,posn,cursorHori  
Dim shared as integer estoyEnOctava, backspace,fijarEspacio, offset,borrar,cambiadur
Dim Shared As Integer x2,y2,x1, y1, x0, y0,InicioDeLectura,k60 
dim shared as string cromatica(1 to 12) 
dim shared MaxPos as integer ' la nota o posicon mayor alcanzada
Dim Shared As Integer DUR, menuAccion,insert,notins 
DIM  Shared figura (13 To 46) As String * 3 = {" O "," P "," I "," L "," F "," E "," W "," H ",_
" O*"," P*"," I*"," L*"," F*"," E*"," W*"," H*","SO ","SP ","SI ","SL ","SF ","SE ","SW ","SH ",_
"SO*","SP*","SI*","SL*","SF*","SE*","SW*","SH*","  ","FIN"}
Dim Shared pun As Integer = 0 'entrada con punto = 1 con puntillo
Dim Shared sil As Integer = 0 'silencio entrada con s = 1 con silencio
'' futuro=> Dim notmus(13 To 44) As String = {} bravura fonts?? pero son glyphs
''los deberia cargar cairo...experimentar...tarea futura
' 32 tipo de duraciones basicas encendidas o apagadas y lo mismo con puntillo 
' I** negra con 2 puntilos seguidos tiene sentido?
' es I+L+L+F=I+I+F .' entre las notas entradas...
' la tecla para entrar puntillo el punto (.), para entrar silencio la s
' ej tecleo 1+.+A=O* en linea A dela octava que sea
' ej tecleo s+1+.+A =sO* en A..
' tambien en vez de estos simbolos usare los de un pentagrama comun,,,,futuro

espacio=0:cursorVert=0:borrar=0:cambiadur=0:InicioDeLectura=0
menuAccion=0:insert=0
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
Dim Shared menuNro As Integer=0