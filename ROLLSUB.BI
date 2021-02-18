Sub creaPenta (c AS cairo_t Ptr,nro As Integer, octava As Integer Ptr,InicioDeLectura As Integer)

Dim AS cairo_font_extents_t fe   '     font data
Dim AS cairo_text_extents_t te  '      text size
dim as integer semitono,n, ic
' VERSION 3 DEBO FORMAR LA OCTAVACOMPLET 12 SONIDOS 
' v4 ponemos maslineas de separacion par q toda duricon este en espacios 
' hay que usar el font Goergia o culqueir con utf8 o seguir lo que dice 
'en usge http://www.non-gnu.org/guile-cairo/docs/html/Text.html
' sino ami me pasoque al poner una ñ las lineas desaparecen!
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

' posicion es el indice del vector Roll
' curpos es la posicion del cursor de 1 a NroCol que suma a posicion
' porque posicion esta congelada pero hcemos q recorremos Roll con elcursor 
' cuando estoy en lectura el scroll avanza o retrocede con flechas izq der...
' en ctrl-M modo cursor loq semueve es el cursor el vectorqueda quieto
' la posicion del vector en donde se modifica seria en el rango 
' posicion NroCol sumandolde eloffset del cursor de 1 a NroCol
' o sea indice vEctor en Ctrl-M = posicion + curpos pero eso sucede
' internanmente para mostrar congelamos posiiocn y comenzamos desde ahi
' todo proceso de modificacion deberatener encuenta a curpos, pero no
' la vista por pantalla
'If comEdit= TRUE Then
'Dim As Integer delta

'delta=NroCol
If cursorVert=0 And comEdit=TRUE  Then
  posishow=  1 + inicioDelectura ' incAuto es negativo incrementa en  -1 
Else
  posishow=posicion
  
EndIf
' 
Var t = " "
For semitono = 1 To 12 
  t = NotasGuia(semitono) + Str(*octava) + "_["
  cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6)
  cairo_show_text(c, t)
  ic=0 'indice cursor
  For n = posishow to posishow + NroCol 
    
    If Roll (semitono + (nro-1) * 13, n  ).nota > 0 Or _
       Roll (semitono + (nro-1) * 13, n  ).dur > 0 Then
          if espacio = semitono and ((n - inicioDeLectura)=curpos)  Then 
             Roll (semitono + (nro-1) * 13, n ).nota = 33
             if fijarEspacio=0 then 
                espacio=0
             EndIf
             s7=0
          EndIf
       cairo_move_to(c, 81 + ic * 20 , Penta_y + semitono * inc_Penta - 6)
       t= figura((Roll (semitono + (nro-1) * 13, n).dur))
       cairo_show_text(c, t)
       ic += 1
    Else      
      Exit For 
    EndIf
    
  Next n
  if n  > MAXPos then 
     MaxPos = n     
  EndIf

  cairo_move_to(c, 0, Penta_y + semitono * inc_Penta )
  cairo_line_to(c, ANCHO - 1, Penta_y + semitono * inc_Penta)

Next semitono
  

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
' si estoy en esta octava ...edicion solo para esa octava segun posicion 
' del mouse automaticamete
if ( Penta_y <= mousey) and ((Penta_y + 12 * inc_Penta) >= mousey)  Then
 ' estoy en una octava    
  estoyEnOctava = nro '<========== DETERMINACION DE OCTAVA DE TRABAJO

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
'n esla posicion al cargar datos deberia llamar a curor creo,,,probando  
 

If cursorVert=1 Or cursorVert= 2 Then  ' vertical comEdit=TRUE
    
cairo_move_to  (c, 81 + (curpos)*20, Penta_y + (notacur-1) * inc_Penta  )
cairo_rectangle(c, 81 + (curpos)*20, Penta_y + (notacur-1) * inc_Penta  , inc_Penta,inc_Penta)
cairo_move_to  (c, 81 + (curpos)*20 + inc_Penta, Penta_y  )
cairo_line_to  (c, 81 + (curpos)*20 + inc_Penta, Penta_y + (notacur-1) * inc_Penta  +inc_Penta ) 

EndIf
If cursorVert= 0 And cursorHori=0 then
  If notacur=1 Then
     notaOld=notacur
     notacur=0
  EndIf
 cairo_move_to  (c,81 + (curpos)*20 ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,81 + (curpos)*20, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If cursorHori=1 Or cursorHori = 2 Then
 cairo_move_to  (c,81 + (curpos)*20 ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,81 + (curpos)*20, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If (cursorHori=1 Or cursorVert = 1) And borrar= 1 Then
 cairo_set_source_rgba c, 1, 0, 0.2, 1
 Dim dursaved As Integer
   If notacur=0 Then 
      notacur=notaOld
   EndIf
   dursaved = Roll(notacur + (nro-1) * 13, n + curpos).dur
' debo cambiar esa nota por su equivalente silencio
  
  Roll(notacur + (nro-1) * 13, n + curpos).dur = dursaved + 16 
  If Roll(notacur + (nro-1) * 13, n +curpos).dur > 33 Then
     Roll(notacur + (nro-1) * 13, n +curpos).dur = 33
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
  Roll(notacur + (nro-1) * 13, n + curpos).dur = DUR 
  vdur=Roll(notacur + (nro-1) * 13, n +curpos).dur
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
  vroll= Roll(notacur + (nro-1) * 13, n + curpos).nota
  cambiadur = 0    
EndIf 
' PROCESO INSERT 
If (cursorHori=1 Or cursorVert = 1) And insert= 2 Then
 
' insertar  nota 
  'ESTANDO EN UNA NOTA ELEGIDA POR EL CURSOR 
  '1ero se pulsa la nueva duracion y luego la Tecla INSERT  
  ' al final de todo la tecla I (ubicrse en lnota luego 3 teclas 
  ' Dur + insert + I) 
  'y la dUR debe tener un valor antes de esto 
   If notacur=0 Then  '<- aca ingreso nota aunqne no haya..
      notacur=notaOld  'cual toma ? es donde esta el cursor ?
   EndIf
 Print #1, "Entra a Cursor con insert 2 notacur: ", notacur
 ' StartInsert esglobal,posicion de inicio de la insercion cuando  pulsamos
 ' la tecla insert se guarda la posicion de inicio desde ahi habra reemplzos
 ' usando la posicion comun n
 ' hasta que se pulse FINose alcance el FIN (34)   
  Print #1, "call a movedato con ind ",ind 
  ' n es StartInsert fijo no se modifica mas
   movedato (n + curpos,ind, insert,notacur + (nro-1) * 13 ) 'antes de reemplazarlo 
  Print #1, "regreso de movedato con ind ",ind
 ' param : posicion comienzo (fijo), indice incremental para el aux 
 ' ,insert comando habilitado = 1
  'luego reemplazo 
  Print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (nro-1) * 13),n
  Roll(notacur + (nro-1) * 13, n +curpos).dur = DUR 
  vdur=Roll(notacur + (nro-1) * 13, n+curpos).dur
 ' SI LA NOTA  NO EXITIA LA agrega
Print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (nro-1) * 13)
  
  Roll(notacur + (nro-1) * 13, n+curpos).nota = notacur ' 5.7.3 
  vroll= Roll(notacur + (nro-1) * 13, n + curpos).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es 
  ' que se crgala nota en roll
  insert=1  ' habilita de nuevo la tecla I deingreso de insercion en un lugar
  'elegido con cursor
  Print #1, "insert 1 ",insert
   
EndIf 
'proceso modificar Nuevo secunci limpiaterminda en NroCol ,con
' nombre deletra pulsada en vez de X
If (cursorHori=2 Or cursorVert = 2) And agregarNota=1  And nota> 0 Then

  Roll(nota + (nro-1) * 13, n + curpos).dur = DUR 
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll(nota + (nro-1) * 13, n + curpos).nota = nota  ' 5.7.3.4
  nota=0     
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
   
cairo_move_to(c, 0, 30 ) 
If menuNew <> menuNro Then
  menuNro=menuNew
EndIf
' aca hay un comportamiento raro. por mas q menunro lo ajuste a 3
' para el caso 0, siempre queda en 0 , debí usar menunew una seguna variable
' y asi funciona ..(en ningun lado se ajusta a cero salvo al inicio fuera del
' loop.....
Select Case menuNro
  Case 0
   If mousex > 8 And mousex< 87 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
      menuNew = 3  ''<--- caso raro ¿?
   Else 
     cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t = " [Archivo]":cairo_show_text(c, t)

   If mousex > 104 And mousex< 180 And mousey < 50 Then
     cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
     cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t=  " [Edicion]":cairo_show_text(c, t)

   If mousex > 195 And mousex< 233 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf 
   t=  " [Ver]":cairo_show_text(c, t)

   If mousex > 249 And mousex< 299 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t=  " [Pista]":cairo_show_text(c, t)
   
   If mousex > 318 And mousex< 423 And mousey < 50 Then
      menuNew = 4
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t=  " [Reproducir]":cairo_show_text(c, t)

   If mousex > 443 And mousex< 532 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t=  " [Opciones]":cairo_show_text(c, t)

   If mousex > 552 And mousex< 613 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 0, 0, 0, 1
   EndIf
   t=  " [Ayuda]":cairo_show_text(c, t)

   cairo_stroke(c)
  Case 1
   t=  " <MENU=(<=|=>)>/<VENTANA=(MOVER=DRAGAR CINTA)(POSICON NORMAL,SUBE/BAJA BORDE INFERIOR=F7-F8)> "
 
  Case 2
   Dim posishow  As Integer 
   If cursorHori=1 And CursorVert=1 Then
       posishow= posicion + curpos
   Else 
       posishow = posicion
   EndIf
   t = "      EDIT" + " " + Str(mousex) + " " + Str(mousey) + " Octava: " + Str(oclog) + _
   " UltimaPos: " + Str(Maxpos) +" Posi: " + Str(posishow)+ " posn: "+Str(posn)+ _
   " Font: " +str(font) + "dur "+ Str(vDur) +"Roll "+ Str(vRoll) + _
   " InicioDELectura: " + Str(InicioDelectura)

  case 3 
   t = " [NUEVO] [ABRIR] [GRABAR] [GRABAR COMO] [EXPORTAR] [CERRAR] [SALIR] "
   If mousex > 12 And mousex < 80  and mousey < 50 Then ' nuevo
      Erase (Roll)
      MaxPos = NroCol
      posn=NroCol
      Roll(1,NroCol).dur = 34
      Dim As Integer mi,p
      For p=1 To NroCol -1
      For mi=1 To 128
        Roll(mi,p).nota=0
        Roll(mi,p).dur =33
      Next mi
      Next p
      curpos=0
      notacur=0
      
   EndIf
   If mousex > 104 And mousex < 160  and mousey < 50 Then ' abrir leer grabaroll.roll
     Erase Roll
      Dim z (1,1 )  As dat
      Dim As String x,x1,x2,x3,x4
     open "Trabajo.roll" for binary access Read as #2
       
       Get #2, , z(1,1)
       x1=Bin(z(1,1).nota,4)
       x2=Bin(z(1,1).dur,4)
       x3=Bin(z(1,1).vol,4)
       x4=Bin(z(1,1).pan,4)
       x=x1+x2+x3+x4
       Print #1,"reconstruccion x pos bin ", x
       MaxPos=CInt("&B"+x)
       posicion = 1
       Print #1,"reconstruccion x pos int ", Posicion
       notaold= z(1,1).pb
       nota=0 '''notaOld
       inicioDeLectura=0' Int(Maxpos/NroCol) 
       posn=Maxpos- 1
' cargamos trabajo datos       
   Dim Trabajo (1 To 128,1 To MaxPos) As dat   
       Get #2, , Trabajo()
  ' movemos los datos a Roll      
 ' -------------------------              
       Dim As Integer i,j 
       For i= 1 To 128
          For j = 1 To MaxPos
          Roll (i,j).nota  = Trabajo (i,j).nota
          Roll (i,j).dur  = Trabajo (i,j).dur
          Roll (i,j).vol  = Trabajo (i,j).vol
          Roll (i,j).pan  = Trabajo (i,j).pan
          Roll (i,j).pb   = Trabajo (i,j).pb
          Roll (i,j).inst = Trabajo (i,j).inst                 

          Next j
       Next i 
    Close 2
    curpos=0
    
    carga=1 ' <======= control de Carga
    Print #1,"VEMOS QUE QUEDA ENROLL nota, dur hasta 40 debug" 
    Dim P As Integer 
    Print #1, "B_--------->"
    For P = 260 To MaxPos
     Print #1, Roll(12,p).nota;".";
    Next
       Print #1,
    For P = 260 To MaxPos
     Print #1, Roll(12,p).dur;".";
     
    Next
    Print #1,
'      Print #1, "A#--------->"
'    For P = 67 To 77
'     Print #1, Roll(13,p).nota;".";
'    Next
'       Print #1,
'    For P = 67 To 77
'     Print #1, Roll(13,p).dur;".";
'     
'    Next
'      Print #1,
                
   Else 
     cairo_set_source_rgba c, 0, 0, 0, 1  
   EndIf
  Case 4
   t=  "[DESDE INICIO][DESDE ESTA POSICION] [VOLUMEN / + / - /]" 
End Select
' Si nodoy Stroke los show text van apendendo al final como un print;
cairo_show_text(c, t)
cairo_stroke(c)

If menuNro=2 Then ' cambio de tamaño de ventana
   cairo_move_to(c, 0, 30 )
   If resize = TRUE Then
     cairo_set_source_rgba c, 1, 0, 0, 1
      t = "<>" 
  
   Else
     cairo_set_source_rgba c, 0, 0, 0, 1 
      t = "<>" 
   EndIf
EndIf
cairo_show_text(c, t)
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
    Rollaux(i1,ind)= Roll(i1,n + curpos)
  next i1

  Print #1, "EN MOVEDATO carga rollaux con valores viejos de roll"
  Print #1,"Rollaux(nota,ind) nota,ind " ,nota,ind
  Print #1,"Roll(nota,n) nota,n " ,nota,n +curpos
  Print #1,"durcion cargda Rollaux(nota,ind).dur: ", Rollaux(nota,ind).dur
  Print #1,"nota  cargda Rollaux(nota,ind).nota : ", Rollaux(nota,ind).nota
'vamos reemplazando los eventos y sobreescribiendo con los nuevos pero
'  los viejos se van guardando para desplazarlos a la derecha. 
End Sub

'--------------------------
Sub moveresto (ByVal StartInsert As Integer, ByRef ind As Integer, byref insert As Integer, ByRef nota As integer)
 Dim As Integer indpos,lastPosRollReemplzada,s,t,k,i
 lastPosRollReemplzada=posicion + curpos
 indpos=posicion + curpos'indice posicion partimos de ultima posicion global reemplazada
   Print #1, "EN MOVERESTO, ind,StartInsert: ",ind ,StartInsert
' ultima posicion movida a aux  es ind   
Dim As Integer lastIndAuxporInsercion = ind 
Print #1, "moveresto inicio: ind,indpos,posicion :", ind,indpos,posicion
Print #1, "entra Loop, ind,StartInsert,indpos: ",ind,StartInsert ,indpos
dim as integer cantInserciones = ind ' el ind contï¿½ en movedata las inserciones nuevas 
' durante lascuales se movio los valores viejso a aux pero quedaron 
' losnuevos en Roll. que no debo opisar
 Do 
   
  
   indpos=indpos+1 ' empezamos de la siguiente no reemplazada y seguimos al final
   ''suponemos que posicion=indpos era la posicion de ultima reemplazada 
   ind=ind+1 'nosubicamos en 1er posicon libre en aux para recibir el resto
Print #1,"antes del FOR Roll,RollAux.. indpos,ind ", indpos,ind
' debemos barres solo la octava? no porque puede haber otras octavas cargadas
   For s= 1 To 128 'barremos todas las notas verticalmente para una misma posicion
     If Roll(s,indpos).dur = 34 Then ' fin de datos de Roll
        Rollaux(s,ind) = Roll(s,indpos)
        insert= 4 ' fin de llenado de almacenamiento auxiliar
        Print #1,"sale moveresto encontro 34 fin, ind :" ,ind
        Exit Do 
     EndIf
     Rollaux(s,ind) = Roll(s,indpos)
     if s=1 then 
     print #1, "luego de la puta copia a AUX del resto de mierda"
     endif 
     if (Roll(s,indpos).dur <> 33 and Roll(s,indpos).nota <> 33) _
        and (Roll(s,indpos).dur > 0 or Roll(s,indpos).nota > 0)    Then 
      print #1, "Roll(";s,indpos;").nota ",Roll(s,indpos).nota
      print #1, "Roll(";s,indpos; ").dur ",Roll(s,indpos).dur
     endif 
     if (Rollaux(s,ind).dur <> 33 and Rollaux(s,ind).nota <> 33) _
        and (Rollaux(s,ind).dur > 0 or Rollaux(s,ind).nota > 0)    Then 
      print #1, "Rollaux(";s,ind; ").nota ",Rollaux(s,ind).nota
      print #1, "Rollaux(";s,ind; ").dur ",Rollaux(s,ind).dur
     endif
     
   Next s
   
   'posicion siempre tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultipo reemplazo o insercion
 ' el 34 lo muevo??? sino debo hcer ind = ind -1
 ' ind=ind - 1 no le resto 1 copio el 34...el fin se corre 
 
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
         if (Roll(s,t).dur <> 33 and Roll(s,t).nota <> 33) _
           and (Roll(s,t).dur > 0 or Roll(s,t).nota > 0)    Then 
           Print #1, "I) Roll(";s,t;")nota,s,t : ", Roll(s,t).nota, s, t
           Print #1, "I) Roll(";s,t;")dur, s,t : ", Roll(s,t).dur,s,t
          endif 
         if (RollAux(s,k).dur <> 33 and RollAux(s,k).nota <> 33) _
            and (RollAux(s,k).dur > 0 or RollAux(s,k).nota > 0)    Then 
            Print #1, "II) RollAux(";s,k;").dur ", RollAux(s,k).dur
            Print #1, "II) RollAux(";s,k;").nota ", RollAux(s,k).nota
          EndIf
        Next s
        t=t+1
    next k 
    print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
   '' posicion = MaxPos con posn es suficiente,sino se va al tramo final 
    posn = MaxPos  
    print #1, "usamos MxPos pra posicion, posn "; posicion, posn
   '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
   Print #1 ,"final ciclo moveresto a AUX"
   
   for s = 1 to 128
      For k=  StartInsert To MaxPos ' gracias a esto anda acordes
        If Roll(s ,k).nota = 0 Then
           Roll(s , k).nota = 33  
        EndIf 
      Next k
   next s 
 ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR 
 ' nota en 33 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
 ' estos son los valores  StartInsert y cantInserciones.
 ' StartInsert es de Roll antesdela insercion 
   print #1, "==> valor de nota antes de CORRECCION: ",nota 
   

   for k= StartInsert to StartInsert + cantInserciones -1
     for s= 1 to 128 
       for i=1 to cantInserciones
           if s <> notasInsertadas (i) then
              Roll(s,k).nota = 33
              Roll(s,k).dur  = 0      
           endif
       next i    
       
       
     next s 
   next k 
 EndIf
insert=0 
End Sub
'------------------------------------
Sub botones(hWnd As HWND, c As cairo_t Ptr, cm As cairo_t Ptr, x As Integer,y As integer)
' RECTANGULO DE 40 X 40, FONDO ROJO
Dim AS cairo_font_extents_t fe   '     font data
Dim AS cairo_text_extents_t te  '      text size

cairo_move_to  (cm, ANCHO-40, 0 )
cairo_rectangle(cm, ANCHO-40, 0 , ANCHO,16)
cairo_set_source_rgba cm, 0.8, 0, 0.2, 1
cairo_move_to  (cm, ANCHO-10, 10 )
cairo_fill (cm)
cairo_set_source_rgba cm, 0.9, 0.9, 0.9, 1
cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (cm, 14)
cairo_move_to (cm,ANCHO-25,14)
cairo_font_extents (cm, @fe)
Var t= "X"
cairo_text_extents (cm, t, @te)

cairo_show_text(cm,t)
cairo_stroke(cm)
'---------
cairo_move_to  (cm, ANCHO-40, 17 )
cairo_rectangle(cm, ANCHO-40, 17 , ANCHO,17)
cairo_set_source_rgba cm, 0.8, 0.8, 0.2, 1
cairo_move_to  (cm, ANCHO-10, 26 )
cairo_fill (cm)
cairo_set_source_rgba cm, 0, 0, 0, 1
cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (cm, 14)
cairo_move_to (cm,ANCHO-25,31)
cairo_font_extents (cm, @fe)
 t= "-"
cairo_text_extents (cm, t, @te)

cairo_show_text(cm,t)
cairo_stroke(cm)
'---------
cairo_move_to  (cm, ANCHO-40, 33 )
cairo_rectangle(cm, ANCHO-40, 33 , ANCHO,33)
cairo_set_source_rgba cm, 0, 1, 0, 1
cairo_move_to  (cm, ANCHO-10, 30 )
cairo_fill (cm)
cairo_set_source_rgba cm, 0, 0, 0, 1
cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
cairo_set_font_size (cm, 14)
cairo_move_to (cm,ANCHO-25,45)
cairo_font_extents (cm, @fe)
 t= "+"
cairo_text_extents (cm, t, @te)

cairo_show_text(cm,t)
cairo_stroke(cm)

  
End Sub
'------------------------------------
' https://www.freebasic.net/forum/viewtopic.php?t=10398
Function KeyPress(Key As Integer) As Integer
Static LastKey(255) As Integer

   If MultiKey(Key) = -1 Then
      If Key = LastKey(Key) Then
         Return (0)
      Else
         LastKey(Key)=Key
         Return (-1)
      EndIf
   Else
      LastKey(Key)=0
      Return (0)
   End If

End Function