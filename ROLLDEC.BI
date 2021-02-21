'miscroll_declare
Declare Sub creaPenta (c AS cairo_t Ptr, nro As Integer, po as Integer Ptr, InicioDeLectura As integer)
Declare Sub Tracks (nroTrack As UByte, nroCanal As UByte)
Declare Sub menu (c AS cairo_t Ptr,n As Integer,menuNro As Integer)
declare Sub botones(hWnd As HWND,c As cairo_t Ptr,cm As cairo_t Ptr,x As Integer,y As integer) 
Declare Sub menuAcc (c AS cairo_t Ptr) 
Declare Sub moveresto (ByVal StartInsert As integer,ByRef indaux as Integer, Byref insert As Integer,Byref nota As integer)
Declare Sub movedato (ByVal StartInsert As Integer, ByRef indaux As Integer, Byref insert As Integer,Byref nota As integer)
' 128 track,16 canales o sea cada canal con 8 voces
Declare Sub cursor(c AS cairo_t Ptr, n As Integer,nro As Integer)
'Declare Function print_text(ByVal x As Integer, ByVal y As Integer, ByRef text As String, _
'        ByVal font As FT_Face, ByVal size As Integer, ByVal col As UInteger ) As Integer 
Dim Shared nro As Integer '''borrar despues jmg ja quedo 
Dim Shared As Integer vroll, vdur ' borrar
Dim  shared as ubyte  notasInsertadas (1 to 1500) 

Dim Shared As Integer mouseX,mouseY, MouseButtons, notaOld,cursorVert,notacur, cursormove
Dim shared As integer Penta_y, indice, posicion,posicionOld, nota,espacio,posn,cursorHori  
Dim shared as integer estoyEnOctava, backspace,fijarEspacio, offset,borrar,cambiadur
Dim Shared As Integer x2,y2,x1, y1, x0, y0,InicioDeLectura,kNroCol, agregarNota,posishow 
dim Shared as string cromatica(1 to 12) 
dim shared MaxPos as integer ' la nota o posicon mayor alcanzada
Dim Shared As Integer DUR, menuAccion,insert,notins
Dim Shared NotasGuia (1 To 12) As String * 2 = {"B_","A#","A_","G#","G_","F#","F_","E_","D#","D_","C#","C_"} 
Dim  Shared figura (1 To 66) As String * 3 = {" O "," P "," I "," L "," F "," E "," W "," H ",_
" O*"," P*"," I*"," L*"," F*"," E*"," W*"," H*","sO ","sP ","sI ","sL ","sF ","sE ","sW ","sH ",_
"sO*","sP*","sI*","sL*","sF*","sE*","sW*","sH*","   "," ||",_
" O+"," P+"," I+"," L+"," F+"," E+"," W+"," H+",_
"+O ","+P ","+I ","+L ","+F ","+E ","+W ","+H ",_
"sO+","sP+","sI+","sL+","sF+","sE+","sW+","sH+",_
"+sO","+sP","+sI","+sL","+sF","+sE","+sW","+sH"}
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
Dim Shared As BOOLEAN ayuda

espacio=0:cursorVert=0:borrar=0:cambiadur=0:InicioDeLectura=0
menuAccion=0:insert=0:agregarNota=0
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
Dim Shared As Integer menuNro, menuNew
''Declare Function MessageBox Alias "MessageBoxW"(n1 As Integer,s1 As Wstring,s2 As Wstring,MB As Integer) As Integer:Sleep 1
Dim Shared As Integer x, y, buttons, res,xscreen 
Dim Shared as Integer StartInsert,indaux,NroCol, carga, curpos, resultado
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
Dim Shared As Integer menuMouse, savemousex, savemousey, usamousex, usamousey,notamouse
