Sub creaPenta (c AS cairo_t Ptr,nro As Integer, octava As Integer Ptr,InicioDeLectura As Integer)

Dim AS cairo_font_extents_t fe   '     font data
Dim AS cairo_text_extents_t te  '      text size
dim as integer i,n
' VERSION 3 DEBO FORMAR LA OCTAVACOMPLET 12 SONIDOS 
' v4 ponemos maslineas de separacion par q toda duricon este en espacios 
' hay que usar el font Goergia o culqueir con utf8 o seguir lo que dice 
'en usge http://www.non-gnu.org/guile-cairo/docs/html/Text.html
' sino ami me pasoque al poner una � las lineas desaparecen!
'5.6-ok-encolumnado en vez de poner lso numero de las nots debo colocr las durciones
' ya me ubique en unaline ahoradebo recibirlso pulsos 1,2,3,4,5,6,7
' directmente o almcenarlos en otro vector.... 
'if indice <= 0 then indice = 1 
'if indice >= 128 then indice = 128

cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (c, font)

' ======== NOTAS  =========
' cuando hago click en Edit el fondo cambia en main para edicion
cairo_set_source_rgba(c, 0, 0, 0, 1)  

 Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1)    
cairo_move_to(c, 0, Penta_y )
cairo_line_to(c, ANCHO - 1, Penta_y )

''var  t = "B_" + Str(*octava) + "       " + Str(AltoYenRoll (1 + (nro -1) *13)) + " " + Str(indice)
var  t = "B_" +  Str(*octava) + "_["
cairo_move_to(c, 0, Penta_y + inc_Penta- 6)
cairo_show_text(c, t)

 n = 1 
 ' esto se debe cmbiar todo para duraciones..!!! cambiando...
 do 
   if Roll (1 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _
      Roll (1 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 1 and n=posicion  Then 
        Roll (1 + (nro-1) * 13, n+ InicioDeLectura).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      EndIf
        cairo_move_to(c, 70 + (n-1)*20 , Penta_y + inc_Penta- 6)
        t= figura((Roll (1 + (nro-1) * 13, n+ InicioDeLectura).dur))
        cairo_show_text(c, t)
   Else      
      Exit do 
   EndIf
   n= n + 1 
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf

cairo_move_to(c, 0, Penta_y + inc_Penta )
cairo_line_to(c, ANCHO - 1, Penta_y + inc_Penta)


 t = "A#" + Str(*octava) +  "_["
 cairo_move_to(c, 0, Penta_y +  2 * inc_Penta - 6)
 cairo_show_text(c, t)
'------------------------------------------------------
 n = 1  
 do 
   if Roll (2 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (2 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 2 And n=posicion  Then 
        Roll (2 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 2 * inc_Penta- 6)
      t = figura(Roll (2 + (nro-1) * 13, n+ InicioDeLectura ).dur)
      cairo_show_text(c, t)
   else 
    Exit do 
   endif
   n= n + 1 
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n  + InicioDeLectura   
 EndIf
 
cairo_move_to(c, 0, Penta_y + 2 * inc_Penta  )
cairo_line_to(c, ANCHO - 1, Penta_y + 2* inc_Penta )

'-----------------------------------------------------------
 t = "A_" + Str(*octava) + "_["
 cairo_move_to(c, 0, Penta_y + 3 * inc_Penta -6)
 cairo_show_text(c, t)

 n= 1 
 do 
   if Roll (3 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (3 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 3 And n=posicion  Then 
        Roll (3 + (nro-1) * 13, n + InicioDeLectura).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 3 * inc_Penta- 6)
      t = figura(Roll (3 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   else     
    Exit Do  
   endif
   n= n + 1
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n  + InicioDeLectura   
 EndIf

 
cairo_move_to(c, 0, Penta_y + 3 * inc_Penta   )
cairo_line_to(c, ANCHO - 1, Penta_y + 3 * inc_Penta )
'--------------------------------------------------------
 t = "G#" + Str(*octava) +  "_["
 cairo_move_to(c, 0, Penta_y + 4 * inc_Penta - 6 )
 cairo_show_text(c, t)

 n = 1 
 do 
   if Roll (4 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (4 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 4 And n=posicion  Then 
        Roll (4 + (nro-1) * 13, n + InicioDeLectura -1).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 4 * inc_Penta- 6)
      t = figura(Roll (4 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   ELSE
    Exit do  
   endif
   n = n + 1
 loop 
  if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf

 
cairo_move_to(c, 0, Penta_y + 4 * inc_Penta  )
cairo_line_to(c, ANCHO - 1, Penta_y + 4 * inc_Penta )

'----------------------------------------
 t = "G_"  + Str(*octava) +  "_["
 cairo_move_to(c, 0,  Penta_y + 5 * inc_Penta - 6)
 cairo_show_text(c, t)

 n = 1 
 do 
   if Roll (5 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (5 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 5 And n=posicion  Then 
        Roll (5 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 5 * inc_Penta- 6)
      t = figura(Roll(5 + (nro-1) * 13, n+ InicioDeLectura ).dur)
      cairo_show_text(c, t)
   ELSE
    Exit do 
   endif
   n= n + 1
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf


cairo_move_to(c, 0, Penta_y + 5 * inc_Penta   )
cairo_line_to(c, ANCHO - 1, Penta_y + 5 * inc_Penta )
'----------------------------------------------------------
 t = "F#" + Str(*octava) +  "_["
 cairo_move_to(c, 0,  Penta_y + 6 * inc_Penta - 6)
 cairo_show_text(c, t)

 n = 1 
 do 
   if Roll (6 + (nro-1) * 13, n+ InicioDeLectura ).nota > 0 Or _ 
      Roll (6 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 6 And n=posicion  Then 
        Roll (6 + (nro-1) * 13, n+ InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 6 * inc_Penta- 6)
      t = figura(Roll(6 + (nro-1) * 13, n+ InicioDeLectura ).dur)
      cairo_show_text(c, t)
   ELSE
    Exit Do  
   endif
   n = n + 1
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf

 
cairo_move_to(c, 0, Penta_y + 6 * inc_Penta   )
cairo_line_to(c, ANCHO - 1, Penta_y + 6 * inc_Penta )
'-----------------------------------------------------------

 t = "F_" + Str(*octava) + "_["
 cairo_move_to(c, 0, Penta_y + 7 * inc_Penta - 6)
 cairo_show_text(c, t)

 n=1 
 do 
   if Roll (7 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (7 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 7 and n=posicion  Then 
        Roll (7 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      endif
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 7 * inc_Penta- 6)
      t = figura(Roll(7 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   ELSE
    Exit Do 
   endif
   n = n + 1
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf

  
cairo_move_to(c, 0, Penta_y + 7 * inc_Penta  )
cairo_line_to(c, ANCHO - 1, Penta_y + 7 * inc_Penta )
'------------------------------------------------------------
 t = "E_" + Str(*octava) + "_["
 cairo_move_to(c, 0, Penta_y + 8 * inc_Penta - 6)
 cairo_show_text(c, t)

 n= 1 
 do 
   if Roll (8 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (8 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
     if espacio = 8 and n=posicion  Then 
       Roll (8 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
     endif
     cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 8 * inc_Penta- 6)
     t = figura(Roll(8 + (nro-1) * 13, n + InicioDeLectura ).dur)
     cairo_show_text(c, t)
   else 
    Exit do 
   endif
   n = n +1
 loop 
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf


cairo_move_to(c, 0, Penta_y + 8 * inc_Penta   )
cairo_line_to(c, ANCHO - 1, Penta_y + 8 * inc_Penta)
'----------------------------------------------------------
 t = "D#" + Str(*octava) + "_["
 cairo_move_to(c, 0, Penta_y + 9 * inc_Penta - 6)
 cairo_show_text(c, t)

 n = 1 
 do 
   if Roll (9 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (9 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
     if espacio = 9 And n=posicion  Then 
       Roll (9 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
     endif
     cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 9 * inc_Penta- 6)
     t = figura(Roll(9 + (nro-1) * 13, n + InicioDeLectura ).dur)
     cairo_show_text(c, t)
   else 
    Exit Do 
   endif
   n = n + 1
 Loop
 if n + InicioDeLectura > MAXPos then 
     MaxPos = n + InicioDeLectura    
 EndIf

  
cairo_move_to(c, 0, Penta_y + 9 * inc_Penta )
cairo_line_to(c, ANCHO - 1, Penta_y + 9 * inc_Penta )
'-----------------------------------------------------

 t = "D_" + Str(*octava) + "_["
cairo_move_to(c, 0, Penta_y + 10 * inc_Penta - 6)
cairo_show_text(c, t)

 n=1 
  do
   if Roll (10 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (10 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 10 And n=posicion  Then 
        Roll (10 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      EndIf
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 10 * inc_Penta- 6)
      t = figura(Roll(10 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   ELSE
     exit do 
   endif
   n = n + 1
  loop 
  
   if n + InicioDeLectura > MAXPos then 
     MaxPos = n   + InicioDeLectura  
   EndIf
Dim td As String = t

  
cairo_move_to(c, 0, Penta_y + 10 * inc_Penta )
cairo_line_to(c, ANCHO - 1, Penta_y + 10 * inc_Penta)
'--------------------------------------------------------

 t = "C#" + Str(*octava) + "_["
 cairo_move_to(c, 0, Penta_y + 11 * inc_Penta - 6)
 cairo_show_text(c, t)

 n= 1 
  do 
   if Roll (11 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (11 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 11 And n=posicion  Then 
        Roll (11 + (nro-1) * 13, n + InicioDeLectura).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      EndIf
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 11 * inc_Penta- 6)
      t = figura(Roll(11 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   Else 
     Exit do 
   endif
   n = n + 1
  Loop
  if n + InicioDeLectura > MAXPos then 
     MaxPos = n   + InicioDeLectura  
  EndIf


cairo_move_to(c, 0, Penta_y + 11 * inc_Penta )
cairo_line_to(c, ANCHO - 1, Penta_y + 11 * inc_Penta )

' t = "C_" + Str(*octava) +"       " + Str(AltoYenRoll (12 + (nro-1) * 13)) + " " + Str(indice) + " td:" + td 
'----------------------------------------------------------------

t = "C_" + Str(*octava) + "_["
cairo_move_to(c, 0, Penta_y + 12 * inc_Penta - 6)
cairo_show_text(c, t)

 n= 1 
  do 
   if Roll (12 + (nro-1) * 13, n + InicioDeLectura ).nota > 0 Or _ 
      Roll (12 + (nro-1) * 13, n + InicioDeLectura ).dur > 0 Then
      if espacio = 12 and n=posicion  Then 
        Roll (12 + (nro-1) * 13, n + InicioDeLectura ).nota = 45
        if fijarEspacio=0 then 
            espacio=0
        EndIf
      EndIf
      cairo_move_to(c, 70 + (n-1)*20 , Penta_y + 12 * inc_Penta- 6)
      t = figura(Roll(12 + (nro-1) * 13, n + InicioDeLectura ).dur)
      cairo_show_text(c, t)
   else 
     exit Do 
   endif
   n = n + 1
  Loop
  if n + InicioDeLectura > MAXPos then 
     MaxPos = n  + InicioDeLectura   
  EndIf

  
cairo_move_to(c, 0, Penta_y + 12 * inc_Penta  )
cairo_line_to(c, ANCHO - 1, Penta_y + 12 * inc_Penta )

'Dim As Integer n, sinLinea ' en cada octava nose dibuja linea ergo hay menso separacion
' sinLinea=0 
' For n = 1 + (nro-1)*13 To 12 + (nro -1 )*13
'    
'    If n = indice Then
'      t= "       " + Str(AltoYenRoll (n))
'      cairo_move_to(c, 0,AltoYenRoll (n) + sinLinea)
'      cairo_show_text(c,t) 
'    EndIf
'    If (n Mod 13) = 0 Then
'      sinLinea = sinLinea - 1700
'    EndIf  
' Next

If nro = 9 Then 
 cairo_set_font_size (c, 20)
'cairo_select_font_face (c, "Georgia",CAIRO_FONT_SLANT_NORMAL , CAIRO_FONT_WEIGHT_BOLD)
 t= "Flecha Abajo/Arriba scroll de las octavas en la ventana "
cairo_move_to(c, 0, Penta_y + inc_Penta * 14  )
cairo_show_text(c, t)

 t = "F9/F10 achica/agranda el  font de las notas guia "
cairo_move_to(c, 0, Penta_y + inc_Penta * 15 )
cairo_show_text(c, t)

 t = "PARA EDITAR CLICK EN EDIT EN EL BORDE SUPERIOR, ESC TERMINA LA APP"
cairo_move_to(c, 0, Penta_y + inc_Penta * 16 )
cairo_show_text(c, t)

 t = "-/+ achica/agranda el area y altura de las octavas dentro de la ventana "
cairo_move_to(c, 0, Penta_y + inc_Penta * 17 )
cairo_show_text(c, t)

 t = "AvPag/RePag scroll de las octavas mas rapido "
cairo_move_to(c, 0, Penta_y + inc_Penta * 18 )
cairo_show_text(c, t)
End If
' si estoy en esta octava ...edicionsolo para esa octava segun posicion del mouse 
'automaticamete
if ( Penta_y <= m.y) and ((Penta_y + 12 * inc_Penta) >= m.y)  Then
 ' estoy en una octava    
  estoyEnOctava = nro

' CURSOR 
 cairo_stroke(c)
 cursor(c,posicion,nro)
 cairo_stroke(c)
 cairo_set_source_rgba c, 0, 0, 0, 1
 cairo_set_line_width(c, 3)
EndIf
*octava = *octava -1
If *octava < 0 Then
   *octava = 99
   Exit Sub
EndIf

End Sub 
''========================= C U R S O R ============
Sub cursor(c AS cairo_t Ptr,n As Integer, nro As Integer)
 cairo_set_source_rgba c, 1, 1, 0.2, 1
 cairo_set_line_width(c, 1)
'EL CURSOR PARA UNA linea dada o nota se coloca inc_Penta mas abajo
' para B 38.5 27 notaold=1  <- actualizar please
' para C 38.5 25 notold=12
 
  n=n -InicioDeLectura

If cursorVert=1 Then  ' vertical 
    
  cairo_move_to(c, 70 + (n-1)*20 ,  Penta_y + (notacur-1) * inc_Penta  )
  cairo_rectangle(c,70 + (n-1)*20, Penta_y + (notacur-1) * inc_Penta  , inc_Penta,inc_Penta)
  cairo_move_to(c, 70 + (n-1)*20 + inc_Penta, Penta_y  )
  cairo_line_to(c, 70 + (n-1)*20 + inc_Penta, Penta_y + (notacur-1) * inc_Penta  +inc_Penta ) 

EndIf
If cursorVert= 0 And cursorHori=0 then
  If notacur=1 Then
     notaOld=notacur
     notacur=0
  EndIf
 cairo_move_to(c, 70 + (n-1)*20 ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,70 + (n-1)*20, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If cursorHori=1 Then
 cairo_move_to(c, 70 + (posicion-1)*20 ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,70 + (posicion-1)*20, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If (cursorHori=1 Or cursorVert = 1) And borrar= 1 Then
 cairo_set_source_rgba c, 1, 0, 0.2, 1
 Dim dursaved As Integer
   If notacur=0 Then 
      notacur=notaOld
   EndIf
   dursaved = Roll(notacur + (nro-1) * 13, n).dur
' debo cambiar esa nota por su equivalente silencio
  
  Roll(notacur + (nro-1) * 13, n).dur = dursaved + 16 
  If Roll(notacur + (nro-1) * 13, n).dur > 45 Then
     Roll(notacur + (nro-1) * 13, n).dur = 45
  EndIf
  borrar = 0
      
EndIf 
' << CAMBIADUR TAMBIEN INGRESA NOTA NUEVA Y DURACION DONDE NO HABIA >>
' << SOBREESCRIBE >>  
If (cursorHori=1 Or cursorVert = 1) And cambiadur= 1 Then
' debo cambiar esa nota por su equivalente silencio
  '1ero se pulsa la nueva duracion y luego la X porque aca se anula luego elflag cambiadur
  'y la dUR debe tener un valor antes de esto 
   If notacur=0 Then  '<- aca ingreso nota aunqne no haya?
      notacur=notaOld
   EndIf
  Roll(notacur + (nro-1) * 13, n).dur = DUR 
  vdur=Roll(notacur + (nro-1) * 13, n).dur
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll(notacur + (nro-1) * 13, n).nota = notacur  ' 5.7.2 
  vroll= Roll(notacur + (nro-1) * 13, n).nota
  cambiadur = 0    
EndIf 
' PROCESO INSERT 
If (cursorHori=1 Or cursorVert = 1) And insert= 2 Then
 
' insertar  nota 
  'ESTANDO EN UNA NOTA ELEGIDA POR EL CURSOR 
  '1ero se pulsa la nueva duracion y luego la INSERT  
  '
  'y la dUR debe tener un valor antes de esto 
   If notacur=0 Then  '<- aca ingreso nota aunqne no haya..
      notacur=notaOld  'cual toma ? es donde esta el cursor ?
   EndIf
 Print #1, "Entra a Cursor con insert 2 notacur: ", notacur
 ' StartInsert esglobal,posicion de inicio de la insercion cuando  pulsamos
 ' la tecla insert se guarda la posicion de inicio desde ahi habra reemplzos
 ' usando la posicion comun n
 ' hasta que se pulse FINose alcance el FIN (46)   
  Print #1, "call a movedato con ind ",ind 
  ' n es StartInsert fijo no se modifica mas
   movedato (n ,ind, insert,notacur + (nro-1) * 13 ) 'antes de reemplazarlo 
  Print #1, "regreso de movedato con ind ",ind
 ' param : posicion comienzo (fijo), indice incremental para el aux 
 ' ,insert comando habilitado = 1
  'luego reemplazo 
  Print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (nro-1) * 13),posicion
  Roll(notacur + (nro-1) * 13, n).dur = DUR 
  vdur=Roll(notacur + (nro-1) * 13, n).dur
 ' SI LA NOTA  NO EXITIA LA agrega
Print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (nro-1) * 13)
  
  Roll(notacur + (nro-1) * 13, n).nota = notacur ' 5.7.3 
  vroll= Roll(notacur + (nro-1) * 13, n).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es 
  ' que se crgala nota en roll
  insert=1  ' habilita de nuevo la tecla I deingreso de insercion en un lugar
  'elegido con cursor
  Print #1, "insert 1 ",insert
   
EndIf 

' VEMOS QUE HACE ESTA FUNCION SI SIRVE
''cairo_surface_get_device_offset
End Sub
'----------------------------------------
Sub Tracks (nroTrack As UByte, nroCanal As UByte)
 ' sizeof array de ubytepr almcenar el pentagrama roll
'Notas ( 1 To 65536, 1 To 128) as UByte

End Sub
'-------------------------------MENU----------
Sub menu (c AS cairo_t Ptr,n As Integer,menuNro As Integer )
 Dim AS cairo_font_extents_t fe   '     font data
 Dim AS cairo_text_extents_t te  '      text size
 Dim  As Double Ptr xo,yo
 Dim As Double nxo,nyo
 Dim cd As cairo_device_t Ptr
 'Dim cbool as cairo_bool_t
 ' pinta celeste totodo surface 2
 ''cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
  cairo_set_source_rgba(c, 0.6, 0.5, 0.6, 1) ' morado
  cairo_paint(c)
 
cairo_set_source_rgba(c, 0, 0, 0, 1)

cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (c, font)

'---encendido de edit
Var t = ""
cairo_font_extents (c, @fe)
cairo_text_extents (c, t, @te)

cairo_move_to(c, 0, 30 ) ' aca esta la cosa la ubicacion del font!!!
' debo ubicarlo caracter por caracter o posicion no en string!!!
 
Dim oclog As Integer 
oclog = 8 - (estoyEnOctava-1) 
If comEdit = TRUE Then
  cairo_set_source_rgba c, 0, 1, 0, 1
Else
  cairo_set_source_rgba c, 0, 0, 0, 1
EndIf   
Select Case menuNro
  Case 0
  t=  " /F4-F5 MENU/<VENTANA>:/MOVER,DRAGAR CINTA/POSICON NORMAL F7-F8 (SUBE/BAJA BORDE INFERIOR) " 
  Case 1
  t = "      EDIT" + " " + Str(mouseX) + " " + Str(mouseY) + " Octava: " + Str(oclog) + " UltimaPos: " + Str(Maxpos-1) +" Pos: " + Str(posicion)+ "posn: "+Str(posn)+" Font: " +str(font) + "dur "+ Str(vDur) +"Roll "+ Str(vRoll)
  case 2 
  t = "<ARCHIVO>:/GRABAR/GRABAR COMO/CERRAR/SALIR <REPRODUCIR>:/INICIO/AHORA/ <VOL>/ + / - / <VEL>:/ + / - /"
  
End Select


cairo_show_text(c, t)
If menuNro=1 Then 
cairo_move_to(c, 0, 30 )
If resize = TRUE Then
  cairo_set_source_rgba c, 1, 0, 0, 1
   t = "<>" 
  
Else
  cairo_set_source_rgba c, 0, 0, 0, 1 
   t = "<>" 

EndIf

 
cairo_show_text(c, t)
EndIf
cairo_stroke(c)

End Sub
'----------------------------------------------------------------------
Sub menuAcc(c As cairo_t Ptr)
 ' IMPLEMENTAR NO SE USA 
 Dim AS cairo_font_extents_t fe   '     font data
 Dim AS cairo_text_extents_t te  '      text size
 Dim  As Double Ptr xo,yo
 Dim As Double nxo,nyo
 Dim cd As cairo_device_t Ptr
  cairo_set_source_rgba(c, 0.6, 0.5, 0.6, 1) ' morado
  cairo_paint(c)
 
 cairo_set_source_rgba(c, 0, 0, 0, 1)

cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (c, font)

'---encendido de edit
Var t = "menuaccion"
cairo_font_extents (c, @fe)
cairo_text_extents (c, t, @te)

cairo_move_to(c, ANCHO/10, 30 ) ' aca esta la cosa la ubicacion del font!!!
' debo ubicarlo caracterpor caracter no en string!!!
 
'Dim oclog As Integer 
'oclog = 8 - (estoyEnOctava-1) 
'If comEdit = TRUE Then
'  cairo_set_source_rgba c, 0, 1, 0, 1
'Else
'  cairo_set_source_rgba c, 0, 0, 0, 1
'EndIf   
'   t = "      EDIT" + " " + Str(mouseX) + " " + Str(mouseY) + " Octava: " + Str(oclog) + " UltimaPos: " + Str(Maxpos-1) +" Pos: " + Str(posicion)+ "posn: "+Str(posn)+" Font: " +str(font)


'cairo_show_text(c, t)

'cairo_move_to(c, ANCHO/10, 30 )
'If resize = TRUE Then
'  cairo_set_source_rgba c, 1, 0, 0, 1
'   t = "<>" 
  
'Else
'  cairo_set_source_rgba c, 0, 0, 0, 1 
'   t = "<>" 

'EndIf

 
cairo_show_text(c, t)
cairo_stroke(c)
 
 
End Sub
'--------------------------
Sub movedato (ByVal n As Integer, ByRef ind As Integer,ByRef  insert As Integer, ByRef nota As integer)
 

' ind parte de cero es el ind para el auxxilair que se ha borrado
' y por ende empieza por 1 no se guarda esun auxiliar...
' 
 ind = ind + 1
 Print #1, "----EN MOVEDATO ind, nota: ",ind , nota
 'notas gurdadas para luego usar en proceso CORRECCION
 notins = notins + 1
 notasInsertadas (notins) = nota 
 'muevo datos remplazados al auxiliar 
  Dim as integer i1,i2 ' nomovemos solo elindice <nota> sino todas las notas!
  for i1 = 1 to 128
    Rollaux(i1,ind)= Roll(i1,n)
  next i1

  Print #1, "EN MOVEDATO carga rollaux con valores viejos de roll"
  Print #1,"Rollaux(nota,ind) nota,ind " ,nota,ind
  Print #1,"Roll(nota,n) nota,n " ,nota,n
  Print #1,"durcion cargda Rollaux(nota,ind).dur: ", Rollaux(nota,ind).dur
  Print #1,"nota  cargda Rollaux(nota,ind).nota : ", Rollaux(nota,ind).nota
'vamos reemplazando los eventos y sobreescribiendo con los nuevos pero
'  los viejos se van guardando para desplazarlos a la derecha. 
End Sub

'--------------------------
Sub moveresto (ByVal StartInsert As Integer, ByRef ind As Integer, byref insert As Integer, ByRef nota As integer)
 Dim As Integer indpos,lastPosRollReemplzada,s,t,k,i
 lastPosRollReemplzada=posicion 
 indpos=posicion 'indice posicion partimos de ultima posicion global reemplazada
   Print #1, "EN MOVERESTO, ind,StartInsert: ",ind ,StartInsert
' ultima posicion movida a aux  es ind   
Dim As Integer lastIndAuxporInsercion = ind 
Print #1, "moveresto inicio: ind,indpos,posicion :", ind,indpos,posicion
Print #1, "entra Loop, ind,StartInsert,indpos: ",ind,StartInsert ,indpos
dim as integer cantInserciones = ind ' el ind cont� en movedata las inserciones nuevas 
' durante lascuales se movio los valores viejso a aux pero quedaron 
' losnuevos en Roll. que no debo opisar
 Do 
   
  
   indpos=indpos+1 ' empezamos de la siguiente no reemplazada y seguimos al final
   ''suponemos que posicion=indpos era la posicion de ultima reemplazada 
   ind=ind+1 'nosubicamos en 1er posicon libre en aux para recibir el resto
Print #1,"antes del FOR Roll,RollAux.. indpos,ind ", indpos,ind
' debemos barres solo la octava? no porque puede haber otras octavas cargadas
   For s= 1 To 128 'barremos todas las notas verticalmente para una misma posicion
     If Roll(s,indpos).dur = 46 Then ' fin de datos de Roll
        Rollaux(s,ind) = Roll(s,indpos)
        insert= 4 ' fin de llenado de almacenamiento auxiliar
        Print #1,"sale moveresto encontro 46 fin, ind :" ,ind
        Exit Do 
     EndIf
     Rollaux(s,ind) = Roll(s,indpos)
     if s=1 then 
     print #1, "luego de la puta copia a AUX del resto de mierda"
     endif 
     if (Roll(s,indpos).dur <> 45 and Roll(s,indpos).nota <> 45) _
        and (Roll(s,indpos).dur > 0 or Roll(s,indpos).nota > 0)    Then 
      print #1, "Roll(";s,indpos;").nota ",Roll(s,indpos).nota
      print #1, "Roll(";s,indpos; ").dur ",Roll(s,indpos).dur
     endif 
     if (Rollaux(s,ind).dur <> 45 and Rollaux(s,ind).nota <> 45) _
        and (Rollaux(s,ind).dur > 0 or Rollaux(s,ind).nota > 0)    Then 
      print #1, "Rollaux(";s,ind; ").nota ",Rollaux(s,ind).nota
      print #1, "Rollaux(";s,ind; ").dur ",Rollaux(s,ind).dur
     endif
     
   Next s
   
   'posicion siempre tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultipo reemplazo o insercion
 ' el 46 lo muevo??? sino debo hcer ind = ind -1
 ' ind=ind - 1 no le resto 1 copio el 46...el fin se corre 
 
 Print #1, "StartInsert: " ,StartInsert
 If insert = 4 Then 'movemos de nuevo todo el aux completo a Roll desde StartInsert
  Print #1, "insert 4 movemos Aux a ROLL ind maximos elementos en Aux: ",ind
   t = StartInsert + cantInserciones
   print #1, "posicion, posn "; posicion, posn
   For k = 1 To ind ' todo el auxiliar
         print #1, "k auxiliar "; k 
        For s=1 To 128
          Roll(s,t) = RollAux(s,k)
          if s=1 then 
            print #1, "luego de la puta copia A ROLL de mierda"
          EndIf
         if (Roll(s,t).dur <> 45 and Roll(s,t).nota <> 45) _
           and (Roll(s,t).dur > 0 or Roll(s,t).nota > 0)    Then 
           Print #1, "I) Roll(";s,t;")nota,s,t : ", Roll(s,t).nota, s, t
           Print #1, "I) Roll(";s,t;")dur, s,t : ", Roll(s,t).dur,s,t
          endif 
         if (RollAux(s,k).dur <> 45 and RollAux(s,k).nota <> 45) _
            and (RollAux(s,k).dur > 0 or RollAux(s,k).nota > 0)    Then 
            Print #1, "II) RollAux(";s,k;").dur ", RollAux(s,k).dur
            Print #1, "II) RollAux(";s,k;").nota ", RollAux(s,k).nota
          EndIf
        Next s
        t=t+1
    next k 
    print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
    posicion = MaxPos 
    posn = MaxPos  
    print #1, "usamos MxPos pra posicion, posn "; posicion, posn
   '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
   Print #1 ,"final ciclo moveresto a AUX"
   
   for s = 1 to 128
      For k=  StartInsert To MaxPos ' gracias a esto anda acordes
        If Roll(s ,k).nota = 0 Then
           Roll(s , k).nota = 45  
        EndIf 
      Next k
   next s 
 ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR 
 ' nota en 45 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
 ' estos son los valores  StartInsert y cantInserciones.
 ' StartInsert es de Roll antesdela insercion 
   print #1, "==> valor de nota antes de CORRECCION: ",nota 
   

   for k= StartInsert to StartInsert + cantInserciones -1
     for s= 1 to 128 
       for i=1 to cantInserciones
           if s <> notasInsertadas (i) then
              Roll(s,k).nota = 45
              Roll(s,k).dur  = 0      
           endif
       next i    
       
       
     next s 
   next k 
 EndIf
insert=0 
End Sub
