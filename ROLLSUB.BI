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
If cursorVert=0 And comEdit=TRUE Then
  If posicion < 60  Then  
    posishow=  1 
  Else
    posishow = posicion - 40
  EndIf    
Else
    posishow = posicion
EndIf  

Dim As Integer lugar=0, sitio
lugarOld=Penta_y
 
Var t = " "
For semitono = 1 To 12 

  t = NotasGuia(semitono) + Str(*octava) + "_["
  cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6)
  cairo_show_text(c, t)
  ic=0 'indice cursor 'donde se dibujara la duracion
  For n = posishow to posishow + NroCol 
    
    If Roll.trk (semitono + (nro-1) * 13, n  ).nota > 0 Or _
       Roll.trk (semitono + (nro-1) * 13, n  ).dur > 0 Then
          if espacio = semitono and ((n - inicioDeLectura)=curpos)  Then 
             Roll.trk (semitono + (nro-1) * 13, n ).nota = 33
             if fijarEspacio=0 then 
                espacio=0
             EndIf
             s7=0
          EndIf
       cairo_move_to(c, 81 + ic * anchofig , Penta_y + semitono * inc_Penta - 4)
       t= figura((Roll.trk (semitono + (nro-1) * 13, n).dur))
       cairo_show_text(c, t)
' LINEAS DE COMPAS
       'sitio=nposishow+ ic
       If compas(n).suma >= d7 Then ' ahi hy un corte un nuevo compas
           cairo_move_to(c,81 + (ic ) *anchofig +anchofig , Penta_y)
           cairo_line_to(c,81 + (ic ) *anchofig +anchofig, Penta_y + 12 * inc_Penta - 6)
       EndIf
       cairo_move_to(c, 81 + ic * anchofig , Penta_y + semitono * inc_Penta - 6) 
       ic += 1
    Else      
      Exit For 
    EndIf
    'con ic * 40 es + 32 osea ic * 40 + 32
  Next n
  if n  > MAXPos then 
     MaxPos = n     
  EndIf
'--------TRAZADI DE LINEAS VERTICALES GUIA DE COMPAS 1ERA VERSION 
   
'------------------------
' ENTRADAD DE NOTA NUEVA CONMOUSE: SE ELIGE LA DURACION CON LASTECLS 1 A 8
'Y SE SELECCIONA CON EL MOUSE CLICKI ZQUIERDO,LA NOTA DESEADA.  
lugar=Penta_y + semitono * inc_Penta
  cairo_move_to(c, 0, lugar )
  cairo_line_to(c, ANCHO - 1, lugar)
  cairo_stroke(c)
  If (mousey <= lugar) And (mousey >= lugarOld ) Then
     nE=semitono  ' semitono 1 a 11 usadopor entrada de tecladoy ahroa mouse
     nR=semitono + (nro-1) * 13  ' indice de la nota en Roll , en algo será util.
  EndIf 
lugarOld=lugar

Next semitono
' PARA ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
   ' y hacer nota=semiotono 1 a 11 con el mouse...el resto es automtico...  

If nro = hasta Then ' es la 10 usamos 0 tmbien ose 9 octavas+ ayuda
 cairo_set_font_size (c, font)
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
 EnOctava=1
' CURSOR 
'''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
 cursor(c,posicion,nro)
'''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - + 
'PERO PIERDO EL COLOR MARILLO DEL CURSOR
 cairo_set_source_rgba c, 0, 0, 0, 1
 'cairo_set_line_width(c, 3)
EndIf
If ((Penta_y + 12 * inc_Penta) <= mousey) And ((Penta_y + 14 * inc_Penta) >= mousey) Then 
 EnOctava = 0
 estoyEnOctava=90
EndIf
*octava = *octava -1
If *octava < desde -1 Then
   *octava = 99
   Exit Sub
EndIf
'           ================  MENUES CONTEXTUALES PARA MOUSE ==================
If ayudaModif=TRUE  And comEdit=TRUE And (cursorVert = 1 or  cursorHori = 1 ) Then
'Print #1,".............SUBRUTINA........................................."
'Print #1,"(9) ESTADO MUESTRA MENU COMANDOS"
'Print #1,"ayudaModif=TRUE  And comEdit=TRUE And (cursorVert = 1 or  cursorHori = 1 )"
'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
  If savemousex > 0 Or savemousey > 0 Then
   usamousex=savemousex
   usamousey=savemousey
  Else
   usamousex=mousex
   usamousey=mousey
  EndIf
  
    Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )

    dim as string  text0 => "BLANQUEAR" 
    Dim As String  text1 => "INSERTAR" 
    Dim As String  text2 => "FIN INSERTAR"
    Dim As String  text3 => "MODIFICAR"      
    Dim As cairo_text_extents_t extents


    If mousey > 50 Then 
    cairo_set_font_face( c, cface )
    cairo_set_font_size( c, 30 )
    cairo_set_source_rgba( c, 1, 1, 1, 1 )
         
 
       If menuMouse = 0 Then
         cairo_move_to( c, usamousex, usamousey -110 )
         cairo_text_extents( c, text0, @extents )
         cairo_show_text( c, text0 )
         menuMouse = 1
       '  Print #1,"BLANQUEAR BORRAR SIN ELIMINAR"
         Exit Sub
       EndIf

       If menuMouse = 1 Then   
          cairo_move_to( c, usamousex, usamousey -80 )
          cairo_text_extents( c, text1, @extents )
          cairo_show_text( c, text1 )
          menuMouse = 2
       '   Print #1,"INSERTAR"
          Exit Sub
       EndIf
       If menuMouse = 2 Then  
          cairo_move_to( c, usamousex, usamousey -50 ) 
          cairo_text_extents( c, text2, @extents )
          cairo_show_text( c, text2 )
          menuMouse = 3
        '  Print #1,"FIN INSERTAR"
       EndIf
       If menuMouse = 3 Then  
          cairo_move_to( c, usamousex, usamousey -20 ) 
          cairo_text_extents( c, text3, @extents )
          cairo_show_text( c, text3 )
          menuMouse = 0
        '  Print #1,"MODIFICAR"
       EndIf

    EndIf
   If savemousex=0 Or savemousey=0 Then
      savemousex=mousex
      savemousey=mousey
   EndIf

EndIf

If ayudaNuevaNota=TRUE  And comEdit=TRUE And vuelta=FALSE Then
' SI SE SELECCIONA INSERTAR O MODIFICAR Y DUR=0 THEN IMPRIMIR
' "ELIJA ANTES UNA DURACION"
'Print #1,".............SUBRUTINA........................................."
'Print #1,"(10) ESTADO SELECCIONAR DURACION O CTRL-P/cTRL-p"
'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
  
  If savemousex > 0 Or savemousey > 0 Then
   usamousex=savemousex
   usamousey=savemousey
  Else
   usamousex=mousex
   usamousey=mousey
  EndIf
  
    Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )
    Dim As String text(1 To 10) =>{"O","P","I","L","F","E","W","H", _    
           "CURSOR (ctrl-m)", "VOLVER (ctrl-p)"}
        
    Dim As cairo_text_extents_t extents


    If mousey > 50 Then 
    cairo_set_font_face( c, cface )
    cairo_set_font_size( c, 30 )
    cairo_set_source_rgba( c, 1, 1, 1, 1 )
    Dim i As UByte

      For i= 1 To 8
          cairo_move_to( c, usamousex -90 + (i-1)*30, usamousey +10 ) 
          cairo_text_extents( c, text(i), @extents )
          cairo_show_text( c, text(i) )
      Next i

' <= control-m O P
          If cursorVert=0 Or cursorHori=0 then
             cairo_move_to( c, usamousex -60 , usamousey +50 ) 
             cairo_text_extents( c, text(9), @extents )
             cairo_show_text( c, text(9) )
          EndIf
          If cursorVert=1 Or cursorHori=1 then
             cairo_move_to( c, usamousex -60 , usamousey -40 ) 
             cairo_text_extents( c, text(10), @extents )
             cairo_show_text( c, text(10) )
          EndIf

     '     menumouse = 0
     '  EndIf
    
   EndIf 
   If savemousex=0 Or savemousey=0 Then
      savemousex=mousex
      savemousey=mousey
   EndIf
EndIf

End Sub 
''========================= C U R S O R ============
Sub cursor(c AS cairo_t Ptr,n As Integer, nro As Integer)
 cairo_set_source_rgba c, 1, 1, 0.2, 1
 
 'cairo_set_line_width(c, 1)
'EL CURSOR PARA UNA linea dada o nota se coloca inc_Penta mas abajo
' para B 38.5 27 notaold=1  <- actualizar please
' para C 38.5 25 notold=12
'n esla posicion al cargar datos deberia llamar a curor creo,,,probando  
 

If cursorVert=1 Or cursorVert= 2 Then  ' vertical comEdit=TRUE
    
cairo_move_to  (c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
cairo_rectangle(c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  , inc_Penta,inc_Penta)
cairo_move_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y  )
cairo_line_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y + (notacur-1) * inc_Penta  +inc_Penta ) 

EndIf
If cursorVert= 0 And cursorHori=0 then
  If notacur=1 Then
     notaOld=notacur
     notacur=0
  EndIf
 cairo_move_to  (c,81 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,81 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If cursorHori=1 Or cursorHori = 2 Then
 cairo_move_to  (c,81 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
 cairo_rectangle(c,81 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , inc_Penta,inc_Penta)

EndIf
If (cursorHori=1 Or cursorVert = 1) And borrar= 1 Then
 cairo_set_source_rgba c, 1, 0, 0.2, 1
 Dim dursaved As Integer
   If notacur=0 Then 
      notacur=notaOld
   EndIf
   dursaved = Roll.trk(notacur + (nro-1) * 13, n + curpos).dur
' debo cambiar esa nota por su equivalente silencio
  
  Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = dursaved + 16 
  If Roll.trk(notacur + (nro-1) * 13, n +curpos).dur > 33 Then
     Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = 33
  EndIf
  borrar = 0
  organizaCompases()    
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
'  Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR 
'  vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
'  ' SI LA NOTA  NO EXITIA LA agrega
'  Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
'  vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
'  cambiadur = 0   
' ------------------------------------  
' nueva verion con puntillo o silencio
' ------------------------------------
        If pun  = 0 And sil=0 Then ' no hay puntillo ni silencio
           Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR 
           vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
           Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
           vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
           cambiadur = 0   

        Else 
          If pun = 1 And sil=0 Then
           Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 8 
           vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
           Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
           vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
           cambiadur = 0   
           pun=0
          Else 
            If sil = 1 And pun=0 Then
           Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 16 
           vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
           Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
           vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
           cambiadur = 0   
           sil=0
            Else ' sil=1 and pun=1
              Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR +24 
              vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
              Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2 
              vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
              cambiadur = 0   
              sil=0:pun=0

            EndIf
            If DUR=33 Then
              Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 33 
              vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
              cambiadur = 0   
              DUR=0
            EndIf
          EndIf
        EndIf
        
        nota = 0 

' fin nuevaversion   
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
  Print #1, "call a movedato con indaux ",indaux 
  ' n es StartInsert fijo no se modifica mas
   movedato (n + curpos,indaux, insert,notacur + (nro-1) * 13 ) 'antes de reemplazarlo xxx
  Print #1, "regreso de movedato con indaux ",indaux
 ' param : posicion comienzo (fijo), indice incremental para el aux 
 ' ,insert comando habilitado = 1
  'luego reemplazo 
  Print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (nro-1) * 13),n
  Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = DUR ' reemplazo con lo nuevo
  vdur=Roll.trk(notacur + (nro-1) * 13, n+curpos).dur
 ' SI LA NOTA  NO EXITIA LA agrega...osea sirve para acordes tambien
Print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (nro-1) * 13)
  
  Roll.trk(notacur + (nro-1) * 13, n+curpos).nota = notacur ' 5.7.3 
  vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es 
  ' que se cargala nota en roll,<- pero 1ero debo guardar la nota de Roll en aux
  ' antes de reemplazarla
  insert=1  ' habilita de nuevo la tecla I deingreso de insercion en un lugar
  'elegido con cursor
  Print #1, "insert 1 ",insert
   
EndIf 
'proceso modificar Nuevo secunci limpiaterminda en NroCol ,con
' nombre deletra pulsada en vez de X
If (cursorHori=2 Or cursorVert = 2) And agregarNota=1  And nota> 0 Then

  Roll.trk(nota + (nro-1) * 13, n + curpos).dur = DUR 
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll.trk(nota + (nro-1) * 13, n + curpos).nota = nota  ' 5.7.3.4
  nota=0     
EndIf
' VEMOS QUE HACE ESTA FUNCION SI SIRVE
''cairo_surface_get_device_offset
cairo_stroke(c) ' aca el stroke da el amarillo y no jode a teclas - + con saltos! 
End Sub
'----------------------------------------
Sub Tracks (nroTrack As UByte, nroCanal As UByte)
 ' sizeof array de ubytepr almcenar el pentagrama roll
'Notas ( 1 To 65536, 1 To 128) as UByte

End Sub
'-------------------------------MENU----------
Sub menu (c0 AS cairo_t Ptr, c AS cairo_t Ptr,n As Integer,menuNro As Integer )
 Dim AS cairo_font_extents_t fe   '     font data
 Dim AS cairo_text_extents_t te  '      text size
 Dim  As Double Ptr xo,yo
 Dim As Double nxo,nyo
 Dim cd As cairo_device_t Ptr
 'Dim cbool as cairo_bool_t
 ' pinta celeste totodo surface 2
 ''cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
 ' cairo_set_source_rgba(c, 0.6, 0.5, 0.6, 1) ' morado
 cairo_set_source_rgba(c, 0.3, 0.3, 0.3, 1) 'fondo gris de la cinta
  cairo_paint(c)
 
cairo_set_source_rgba(c, 1, 1, 1, 1) ' blanco letras

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
'If comEdit = TRUE Then
'  cairo_set_source_rgba c, 0, 1, 0, 1
'  menuNew=2
'Else
'  cairo_set_source_rgba c, 1, 1, 1, 1
'EndIf
   
'cairo_move_to(c, 0, 30 ) 
If menuNew <> menuNro Then
  menuNro=menuNew
EndIf
' aca hay un comportamiento raro. por mas q menunro lo ajuste a 3
' para el caso 0, siempre queda en 0 , debí usar menunew una seguna variable
' y asi funciona ..(en ningun lado se ajusta a cero salvo al inicio fuera del
' loop.....
t=" "
Select Case menuNro
  Case 0
   If mousex > 38 And mousex< 117 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
      font=18 ' el menu esta diseñqdo para este tamaño
      If MouseButtons And 1 Then
        menuNew = 3  ''<--- caso raro ¿?
      EndIf
   Else 
     cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   carga=0
   t = "       [Archivo]":cairo_show_text(c, t)

   If mousex > 134 And mousex< 210 And mousey < 50 Then
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
         menuNew=2
     EndIf 
   Else 
     cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
  ' Print #1, "INVESTIGO COMEDIT ENTRO X CLICK EN SUB S3, menuNro: ",S3, menuNro 
   t=  " [Edicion]":cairo_show_text(c, t)

   If mousex > 225 And mousex< 263 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf 
   t=  " [Ver]":cairo_show_text(c, t)

   If mousex > 279 And mousex< 329 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Pista]":cairo_show_text(c, t)
' ------------------------------------------   
   If mousex > 348 And mousex< 453 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
        menuNew = 4
      EndIf
   Else 
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Reproducir]":cairo_show_text(c, t)
' --------------------------
   If mousex > 473 And mousex< 562 And mousey < 50 Then  'OPCIONES
      cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menuNew=5
         octavas=0
      EndIf 
   Else 
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Opciones]":cairo_show_text(c, t)

   If mousex > 582 And mousex< 643 And mousey < 50 Then
      cairo_set_source_rgba c, 0, 1, 0, 1
   Else 
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Ayuda] solo archivo y edicion habilitados":cairo_show_text(c, t)

   cairo_stroke(c)
  Case 1
   t=  "      <MENU=(<=|=>)>/<VENTANA=(MOVER=DRAGAR CINTA)(POSICON NORMAL,SUBE/BAJA BORDE INFERIOR=F7-F8)> "
 
  Case 2 ' para posicionar  en pantalla NO BORRAR. <= EDIT MENU  
   menuNew=2 
 '  If cursorHori=1 And CursorVert=1 Then
 '      posishow= posicion + curpos
 '  Else 
 '      posishow = posicion
 '  EndIf
   cairo_move_to(c, 0, 36 )
   If comEdit= TRUE   Then
      cairo_set_source_rgba c,0,1,0,1
   Else
      cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf   
 Dim y1 As Integer => 1 + (desde-1) * 13
 Dim y2 As Integer => 1 + (hasta-1) * 13
   
   t = "      EDIT" + " " +  " UltimaPos: " + _
   Str(Maxpos) +" Posi: " + Str(posishow)+ " Posn: "+Str(posn)+ _
   " Font: " +str(font) + " DUR:"+ Str(Dur) +" Nota:"+ Str(nota)+ _
   " EnOct:" + Str(EnOctava) + " nE:"+Str(nE) + " nR:"+ Str(nR) + _
   " Mx:" + Str(mousex) + " My:" + Str(mousey) + "Desde:" + Str(y1) + "Hasta:" +Str(y2)   
/' " InicioDELectura: " + Str(InicioDelectura) + " BordeSupRoll: " + Str(BordeSupRoll) +
'/ 
'''   " modifmouse:" + Str(modifmouse) + " usamousey:" + Str(usamousey) + " usamousex:"+ Str(usamousex) + _

 cairo_show_text(c,t)

 cairo_stroke(c)
  
 menuNew=2

  If mousex > 500 and mousey < 50 And mousex < ANCHO-50 Then
     If MouseButtons And 1 Then 
         MenuNew=0
     EndIf   
  EndIf

 
  case 3 
'   t = " [NUEVO] [ABRIR] [GRABAR] [GRABAR COMO] [EXPORTAR] [CERRAR] [SALIR] "
' 3.1 [NUEVO]    
   If mousex > 42 And mousex < 110  and mousey < 50 Then ' <= NUEVO
      cairo_set_source_rgba c, 0, 1, 0, 1
    '  If MouseButtons And 1 Then
      If dobleclick Then 
       ' Erase (Roll.trk)
        ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
        MaxPos = 1 'NroCol
        posicion=1:posn=1
        curpos=1
        notacur=1
      EndIf
   Else 
     cairo_set_source_rgba c, 1, 1, 1, 1   
   EndIf
   t = "      [NUEVO]":cairo_show_text(c,t)
' 3.2 [ABRIR]   
   If mousex > 134 And mousex < 190  and mousey < 50 Then ' <= ABRIR lee grabaroll.roll
      cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 And carga = 0 Then 
         ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
         Dim z (1,1 )  As dat
         Dim As String x,x1,x2,x3,x4
         open "Trabajo_01.roll" for binary access Read as #2
       
         Get #2, , z(1,1)
         x1=Bin(z(1,1).nota,4)
         x2=Bin(z(1,1).dur,4)
         x3=Bin(z(1,1).vol,4)
         x4=Bin(z(1,1).pan,4)
         x=x1+x2+x3+x4
      '   Print #1,"reconstruccion x pos bin ", x
         MaxPos=CInt("&B"+x)
         posicion = 1
       '  Print #1,"reconstruccion x pos int ", Posicion
         notaold= z(1,1).pb
         nota=0 '''notaOld
         inicioDeLectura=0' Int(Maxpos/NroCol) 
         posn=Maxpos- 1
' cargamos trabajo datos       
         Dim Trabajo (NB To NA,1 To MaxPos) As dat   
         Get #2, , Trabajo()
  ' movemos los datos a Roll      
 ' -------------------------              
         Dim As Integer i,j , mayor
          mayor=99
           For j = 1 To MaxPos
             For i= NB To NA
              Roll.TRK(i,j)  => Trabajo (i,j) '
              DUR => Roll.trk(i,j).dur
              If i=1 And DUR >= 1 And DUR <=8 Then 
                mayor=DUR
              EndIf
              'Print #1, "PUTO DUR mayor";DUR, mayor
              If DUR < mayor And DUR >= 1 And DUR <= 8 Then ' las duraciones cuando mas
                 mayor=DUR
            ' Print #1, "PUTO DUR MAYOR";DUR,mayor
              EndIf
              If i=NA Then
                DUR=mayor 
                Print #1,"          J, DUR MAYOR: "; J, DUR
                calcCompas(j)
                DUR=0
                mayor=99
              EndIf
             Next i
          Next j 
        Close 2
        DUR => 0
        curpos =>0
    
        carga=1 ' <======= control de Carga ¿? VER
     '   Print #1,"VEMOS QUE QUEDA ENROLL nota, dur hasta 40 debug" 
     '   Dim P As Integer 
     '   Print #1, "B_--------->"
     '   For P = 260 To MaxPos
    '     Print #1, Roll.trk(12,p).nota;".";
     '   Next
     '   Print #1,
     '   For P = 260 To MaxPos
     '     Print #1, Roll.trk(12,p).dur;".";
     
      '  Next
      '  Print #1,
      EndIf         
   Else 
     cairo_set_source_rgba c, 1, 1, 1, 1  
   EndIf
   t = " [ABRIR]":cairo_show_text(c,t)
' 3.3[GRABAR] SC_F11 COPIA
   If mousex > 223 And mousex < 296  and mousey < 50 Then ' <=
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
'       Print #1, "Grabando a disco Roll GRABAR "
       Open "Trabajo_01.roll" For Binary access write As #2
   
       Dim Trabajo (NB To NA, 1 To Maxpos) As dat
       Dim grabaPos (1,1)  As dat
       Dim as integer i1, i2
       Dim As Integer y1,y2,y3,y4 
       Dim As String a1,a2,a3,a4 ,x
       x= Bin(MaxPos,16)
   '    Print #1,"Posicion ",Posicion
 'Print "string representando ", x
       a1=Mid(x,1,4)
       a2=Mid(x,5,4)
       a3=Mid(x,9,4)
       a4=Mid(x,13,4)
 '    Print #1,"a1 a2 a3 a4 ",a1, a2 ,a3, a4
 
       y1= CInt("&B"+a1)
       y2= CInt("&B"+a2)
       y3= CInt("&B"+a3)
       y4= CInt("&B"+a4)
 '    Print #1, "y1,y2,y3,y4", y1,y2,y3,y4  
' grabamos maxpos en 4 ubyte   
       grabaPos(1,1).nota = y1
       grabaPos(1,1).dur  = y2
       grabaPos(1,1).vol  = y3
       grabaPos(1,1).pan  = y4
' ------------------------------------
' hacemoslo mismo para l ultim not que se grabo que teng el 34
' esa es lapapa recorremos todas las dur de las notas en donde este el 34
' endur tomamosla nota y esala grabamos en pb o ins
' cuadno la cargamos lo ahcemos en notaold !!!! y Vuala!!!
' -------------------------------------
       Dim As Integer i,j,notafinal 
       For i = NB To NA
         If Roll.trk(i,posicion+1).dur = 34 Then
            notafinal= Roll.trk(i,posicion).nota
    '        Print #1, "ENCONTRO NOTA FINAL ", notafinal
    '        Print "ENCONTRO NOTA FINAL ", notafinal
         ' esten la posiciond ela ultimanotapero grasoerror
         ' o seaque no tiene duracion solo el34
         ' esta mal ...
            Exit For
         EndIf 
      Next i
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
   Else 
     cairo_set_source_rgba c,1,1,1,1
   EndIf  
  t = " [GRABAR]": cairo_show_text(c,t) :t= "" 
'   
  If mousex > 350 and mousey < 50 Then
    If MouseButtons And 1 Then 
       MenuNew=0
    EndIf   
  EndIf 
  Case 4  ' <======= COMO REPRODUCUIR 
     cairo_set_source_rgba c,1,1,1,1
   t=  "      [DESDE INICIO][DESDE ESTA POSICION] [VOLUMEN / + / - /] No habilitado"
   If mousex > 330 and mousey < 50 And mousex < ANCHO-50 Then
    If MouseButtons And 1 Then 
       MenuNew=0
    EndIf   
   EndIf
   Case 5 'OPCIONES => OCTAVAS
    
  If mousex > 38 And mousex< 123 And mousey < 50 Then ' OCTAVAS LABEL
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1  Then
        octavas=0
        Dim Cantitems As Integer => 11
        Dim As String text (1 To 11) => {"Click En Desde antes de cada click en - / + ", _
        "Click En Hasta antes de cada click en -/+", _
        "Esto redimensionara la capcidad de Octavas", _
        "Cuando grave esa capcidad ira en el archivo",_
        "Antes de cargar debe ajustar la capacidad a la misma que grabo",_
        "en un futuro proximo se grabra ese dato", _
        "y sera automatico","o sea ","se cargaran loa datos ", _
        "y la capacidad de Octavas", _
        "a mas Octavas mas memoria usada"}
         
        menugrafico  c0, c, CantItems, text()  
    
     EndIf
     
  EndIf 
  
  If mousex > 144 And mousex< 196 And mousey < 50 Then ' DESDE LABEL
     cairo_set_source_rgba c, 0, 1, 0, 1
     octavas=0
   
  EndIf  

  If mousex >= 215 And mousex< 229 And mousey < 50  Then ' - LBEL
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas=0 Then
        desde -= 1
        If desde < 1 Then 'desdevector Then
           desde = 1 'desdevector
        EndIf
        octavas=1
     EndIf
  EndIf
    
  If mousex >= 241 And mousex< 261 And mousey < 50   Then ' + LBEL
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas=0 Then
        desde = desde + 1
        If desde > hastavector Then
           desde = hastavector 
        EndIf
        octavas =2
     EndIf
  
  EndIf
  
 
   t = "      [OCTAVAS DESDE   - / + ] " + Str(desde-1): cairo_show_text(c,t)
' ===================
  If mousex > 308 And mousex< 363 And mousey < 50 Then ' HASTA
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
        octavas=0
     EndIf 
    
  Else 
     cairo_set_source_rgba c, 1, 1, 1, 1
     If mousex >= 374 And mousex< 394 And mousey < 50  Then '-
        cairo_set_source_rgba c, 0, 1, 0, 1
         If MouseButtons And 1 And octavas = 0 Then
         hasta -= 1
           If hasta < desdevector Then
              hasta = desdevector
           EndIf
         octavas=1
         EndIf 
     EndIf
    
     If mousex >= 403 And mousex< 423 And mousey < 50   Then ' +
        cairo_set_source_rgba c, 0, 1, 0, 1
        If MouseButtons And 1 and octavas = 0 Then
           hasta += 1
           If hasta > 9 Then
              hasta = 9
           EndIf
         octavas=2
       EndIf
     EndIf
    
EndIf  
 
   t = "  [HASTA  - / + ] " + Str(hasta-1): cairo_show_text(c,t)

   If mousex >= 473 And mousex< 634 And mousey < 50   Then ' LABEL REDIMENSIONAR
        ''cairo_set_source_rgba c, 0, 1, 0, 1
        If dobleclick Then
           cairo_set_source_rgba c, 1, 0, 0, 1
           NB => 1 + (desde) * 13   ' 27 para 3
           NA => 12 + (hasta) * 13  ' 90 para  7
           ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
           ReDim (RollAux.trk) (NB To NA , 1 To CantTicks)
        EndIf
    Else
           cairo_set_source_rgba c, 1, 1, 1, 1
    EndIf
   t = "  [REDIMENSIONAR] " 
     
End Select
' Si nodoy Stroke los show text van apendendo al final como un print;

cairo_show_text(c, t)
cairo_stroke(c)

cairo_move_to(c, 0, 30 )
  If resize = TRUE Then
     cairo_set_source_rgba c, 1, 0, 0, 1
      t = "<>" 
  Else
     cairo_set_source_rgba c, 1, 1, 1, 1 
      t = "<>" 
  EndIf

 cairo_show_text(c, t)
 cairo_stroke(c)

' ayuda
If ayuda = TRUE Then
    Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )
    dim as string  text => "HOLA SOY TU AYUDA!"
    
    dim As cairo_text_extents_t extents

    If mousey < 50 Then 
    cairo_set_font_face( c, cface )
    cairo_set_font_size( c, 36 )
    cairo_text_extents( c, text, @extents )
    cairo_move_to( c, mousex, mousey )
    cairo_set_source_rgba( c, 1, 1, 1, 1.0 )
    cairo_show_text( c, text )
    EndIf
EndIf
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
Sub movedato (ByVal n As Integer, ByRef indaux As Integer,ByRef  insert As Integer, ByRef nota As integer)
 
' OJO n ya entra conla suma de curpos
' indaux parte de cero es el indaux para el auxiliar que se ha borrado
' y por ende empieza por 1 no se guarda esun auxiliar...
' 
 indaux = indaux + 1 ' indaux auxiliar SC_INSERT INICIALIZA EN 0
 Print #1, "----EN MOVEDATO indaux, nota: ",indaux , nota
 'notas gurdadas para luego usar en proceso CORRECCION
 notins = notins + 1
 notasInsertadas (notins) = nota  ' ENTRADA POR TECLADO ANTES DE PULSAR INSERT
 'LA DURACION NO HACE FALTA PARA CONTROL,QUEDARA EN ROLL
 'muevo datos QUE SERAN remplazados al auxiliar PARA ESA POSICION 
  Dim as integer i1,i2 ' no movemos solo el indice <nota> sino todas las notas!
  ' INDAUX PARTE DE 1 Y SE Va INCREMENTANDO CON CADA PULSO SOLO DE  I...
  
  for i1 = NB to NA
    RollAux.trk(i1,indaux)= Roll.trk(i1,n ) 'xxx  n ya contiene curpos
  next i1
' movio la nota queestaba antes de pulsar insert
  Print #1, "EN MOVEDATO carga RollAux.trk con valores viejos de roll"
  Print #1,"RollAux.trk(nota,indaux) nota,indaux " ,nota,indaux
  Print #1,"Roll.trk(nota,n) nota,n, curpos" ,nota,n, curpos
  Print #1,"duracion cargda RollAux.trk(nota,indaux).dur: ", RollAux.trk(nota,indaux).dur
  Print #1,"nota  cargda RollAux.trk(nota,indaux).nota : ", RollAux.trk(nota,indaux).nota
'vamos reemplazando los eventos y sobreescribiendo con los nuevos pero
'  los viejos se van guardando para desplazarlos a la derecha.
' RollAux.trk tendra lo viejo, notas insertdas lo nuevo 
End Sub

'--------------------------
Sub moveresto (ByVal StartInsert As Integer, ByRef indaux As Integer, byref insert As Integer, ByRef nota As integer)
 Dim As Integer indpos,lastPosRollReemplzada,s,t,k,i
 lastPosRollReemplzada=posicion + curpos
 indpos=posicion + curpos'indice posicion partimos de ultima posicion global reemplazada
   Print #1, "EN MOVERESTO, indaux,StartInsert: ",indaux ,StartInsert
' ultima posicion movida a aux  es indaux (indaux POSICION DE AUX) JMG  
Dim As Integer lastIndAuxporInsercion = indaux 
Print #1, "moveresto inicio: indaux,indpos,posicion :", indaux,indpos,posicion
Print #1, "entra Loop, indaux,StartInsert,indpos: ",indaux,StartInsert ,indpos
dim as integer cantInserciones = indaux ' o notins el indaux continua en movedata las inserciones nuevas 
' durante lascuales se movio los valores viejso a aux pero quedaron 
' losnuevos en Roll. que no debo opisar
'1) COPIAMOS A AUX EL RESTO DE ROLL
 Do 
   
'indpos ya se le sumo curpos... COPIAMOS A AUX EL RESTO DE ROLL NO MODIFICADO COMPLETA 
   indpos=indpos+1 ' empezamos de la siguiente no reemplazada y seguimos al final
   ''suponemos que posicion=indpos era la posicion de ultima reemplazada 
   indaux=indaux+1 'nosubicamos en 1er posicon libre en aux para recibir el resto
Print #1,"antes del FOR Roll,RollAux.trk.. indpos,indaux ", indpos,indaux
' debemos barrer solo la octava? no porque puede haber otras octavas cargadas
   For s= NB To NA 'barremos todas las notas verticalmente para una misma posicion
     If Roll.trk(s,indpos).dur = 34 Then ' fin de datos de Roll
        RollAux.trk(s,indaux) = Roll.trk(s,indpos)
        insert= 4 ' fin de llenado de almacenamiento auxiliar
        Print #1,"sale moveresto encontro 34 fin, indaux :" ,indaux
        Exit Do 
     EndIf
     RollAux.trk(s,indaux) = Roll.trk(s,indpos)
     if s=1 then 
     print #1, "luego de la copia a AUX del resto "
     endif 
     if (Roll.trk(s,indpos).dur <> 33 and Roll.trk(s,indpos).nota <> 33) _
        and (Roll.trk(s,indpos).dur > 0 or Roll.trk(s,indpos).nota > 0)    Then 
      print #1, "Roll.trk(";s,indpos;").nota ",Roll.trk(s,indpos).nota
      print #1, "Roll.trk(";s,indpos; ").dur ",Roll.trk(s,indpos).dur
     endif 
     if (RollAux.trk(s,indaux).dur <> 33 and RollAux.trk(s,indaux).nota <> 33) _
        and (RollAux.trk(s,indaux).dur > 0 or RollAux.trk(s,indaux).nota > 0)    Then 
      print #1, "RollAux.trk(";s,indaux; ").nota ",RollAux.trk(s,indaux).nota
      print #1, "RollAux.trk(";s,indaux; ").dur ",RollAux.trk(s,indaux).dur
     endif
     
   Next s
   
   'posicion + curpos tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultimo reemplazo o insercion
 ' el 34 lo muevo??? sino debo hcer indaux = indaux -1
 ' indaux=indaux - 1 no le resto 1 copio el 34...el fin se corre 
 
 Print #1, "StartInsert: " ,StartInsert '  ya se le sumo curpos
 If insert = 4 Then 'movemos de nuevo todo el aux completo a Roll desde 
 ' laultimainsercion o sea Startindice + 
  Print #1, "insert 4 movemos Aux a ROLL indaux maximos elementos en Aux: ",indaux
   t = StartInsert + cantInserciones ' xxx  OK
   
   Print #1, "posicion, posn "; posicion, posn
   For k = 1 To indaux ' todo el auxiliar SUMA ROLL REEMPLAZADO + NOREEMPLAZADO
         print #1, "k auxiliar "; k 
        For s=NB To NA  ' todos los semitonos 96
          Roll.trk(s,t) = RollAux.trk(s,k)
          if s=1 then 
            print #1, "luego de la  copia A ROLL "
          EndIf
         if (Roll.trk(s,t).dur <> 33 and Roll.trk(s,t).nota <> 33) _
           and (Roll.trk(s,t).dur > 0 or Roll.trk(s,t).nota > 0)    Then 
           Print #1, "I) Roll.trk(";s,t;")nota,s,t : ", Roll.trk(s,t).nota, s, t
           Print #1, "I) Roll.trk(";s,t;")dur, s,t : ", Roll.trk(s,t).dur,s,t
          endif 
         if (RollAux.trk(s,k).dur <> 33 and RollAux.trk(s,k).nota <> 33) _
            and (RollAux.trk(s,k).dur > 0 or RollAux.trk(s,k).nota > 0)    Then 
            Print #1, "II) RollAux.trk(";s,k;").dur ", RollAux.trk(s,k).dur
            Print #1, "II) RollAux.trk(";s,k;").nota ", RollAux.trk(s,k).nota
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
   
   for s = NB to NA
      For k=  StartInsert To MaxPos ' gracias a esto anda acordes
        If Roll.trk(s ,k).nota = 0 Then
           Roll.trk(s , k).nota = 33  
        EndIf 
      Next k
   next s 
 ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR 
 ' nota en 33 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
 ' estos son los valores  StartInsert y cantInserciones.
 ' StartInsert es de Roll antesdela insercion 
   print #1, "==> valor de nota antes de CORRECCION: ",nota 
   
' aca revisamos el Roll solo la parte insertada , encada insercion en Roll
' esa posiicon se corresponde al de notas insertadas biunivocmente,,
' BORRAMOS LO QUE ESTABA ANTES PUES YA SEMOVIO ALFINAL
' EN CADA POSICION SOLO DEBE QUEDAR UNA SOLA NOTA, LA INSERTADA,
' SOLO SE PERMITE INSERTR UNA SOLA NOTA VERTICALMENTE O SEA PARA UNA MISMA POSICION
' PERO PARA DISTINTAS POSICIONES HABRA MAS NOTAS INSERTADAS UNICAS SI SE DESEA
' LUEGO CON PROBRSI LA NO TA EN ROLL ES IGUAL O DISTINA A LA INSERTADA SE PUEDEN
' BORRAR LAS DISTINTAS..Y EL ORDEN ES EL MISMO...SEGUN POSICION..
   Dim i As Integer      
   for k= StartInsert to StartInsert + cantInserciones -1 ' OK PARA 1 INSERCION ES SOLO EN STRTINSERT
     i = i + 1  
     for s= NB to NA 
        If s <> notasInsertadas (i) then
           Roll.trk(s,k).nota = 33
           Roll.trk(s,k).dur  = 0      
        endif
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
'
function calcCompas( j As integer) As Integer   


If j=1 Then
indCompas=0
haycompas=FALSE
EndIf
 ' 1 acumulado, 2 nro compas, 3 posicion
If haycompas=FALSE Then
   If j=1 Then
     COMPAS(j).suma = pesoDur(DUR)
   Else 
     COMPAS(j).suma = pesoDur(DUR) + COMPAS(j -1).suma
   EndIf
Else
COMPAS(j).suma = pesoDur(DUR) 
haycompas=FALSE
EndIf

 Print #1,"           indCompas: ";indCompas; " acumulado: ";compas(j).suma
 Dim As Integer acumulado , diferencia, k, diferencia2
 dur2=0
 acumulado  =compas(j).suma 
 Dim difnota As Integer 
 difnota=pesoDur(DUR) ' por ejemplo redonda despues de1 negra, O BLANCA

 If acumulado > d7 Then
    diferencia = acumulado  -d7 ' aca me da 1 negra EN AMBOS CASOS O ó P 2500000
    acumulado= acumulado-diferencia ' EL COMPAS EXACTO 10000000
    diferencia2 = difnota - diferencia ' P* o I  7500000
    For k= 1 To 16 ' hasta 15 la ultima no tiene sentido 
  Print #1,"==> diferencia pesodur "; diferencia,pesoDur(k)
      '  If k=8 Then
      '    continue For 
      '  EndIf   
        If diferencia2 = pesoDur(k) Then ' P* o I
          Print #1,"encontro diferencia"; diferencia2
           DUR2=k  'P* o I  10
           Exit For
        EndIf
    Next
    
 EndIf
Print #1, "calcCompas acumulado DUR DUR2 ";acumulado,DUR,DUR2
 
 If acumulado = d7 Then 'compas exacto 10000000
     compas(j).nro = indCompas ' compas 1,2,3,4,5...  etc
' asi lo diferencio de lacumulacion...
   Print #1, "Compas Nro: ";compas(j).nro; " posicion: "; compas(j).nro
   indCompas=indCompas + 1  
   haycompas=TRUE
   'dur1=DUR 
   If pesoDur(DUR2) >0  And pesoDur(dur)  = 2* pesoDur(dur2)  Then 'P I
      Print #1, "igualo negra con blnanca DUR, DUR2, DUR2 +34,figura ";DUR, DUR2, DUR2+34,figura(DUR2+34)   
      DUR=DUR2 +34
    '  For k =1 To 82
    '     Print #1, "k "; k ; " figura "; figura(k)
    '  Next
   Else   
     If pesoDur(dur) = pesoDur(dur2)*4/3  Then ' O P*
        dur=dur+67
     EndIf
   EndIf 
EndIf
 For k = 1 To 16
    If diferencia = PesoDur(k) Then
       DUR2 = k ' es elresto en ambos de estos 2 casos es la negra
       'debo de alguna forma introduciro automaticamente sin que el usuario
       'intervenga o sea no resetear la nota y cambiar a esta DUR---
    EndIf
 Next 
 Print #1, "calcCompas diferencia ";diferencia
 Print #1, "calcCompas acumulado-diferencia DUR DUR2 ";acumulado,DUR,DUR2
Return DUR2
End function
Sub organizaCompases()
Dim As Integer i,j , mayor
          mayor=99
           For j = 1 To MaxPos
             For i= NB To NA
              DUR => Roll.trk(i,j).dur
              If i=1 And DUR >= 1 And DUR <=16 Then 
                mayor=DUR
              EndIf
              'Print #1, "PUTO DUR mayor";DUR, mayor
              If DUR < mayor And DUR >= 1 And DUR <= 16 Then ' las duraciones cuando mas
                 mayor=DUR
            ' Print #1, "PUTO DUR MAYOR";DUR,mayor
              EndIf
              If i=NA Then
                DUR=mayor 
                Print #1,"          J, DUR MAYOR: "; J, DUR
              calcCompas(j)
                DUR=0
                mayor=99
              EndIf
             Next i
          Next j  
' fin compases             

End Sub
sub menugrafico(c0 As cairo_t Ptr,c AS cairo_t Ptr ,Cantitems As Integer, text() as String  )
  If savemousex > 0 Or savemousey > 0 Then
   usamousex=savemousex
   usamousey=savemousey
  Else
   usamousex=mousex
   usamousey=mousey
  EndIf
  
    Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )
        
    Dim As cairo_text_extents_t extents

 '   cairo_move_to (c0 , usamousex, usamousey + 250)
  '  SetMouse 100,200
'    If mousey > 50 Then 
    cairo_set_font_face( c0, cface )
    cairo_set_font_size( c0, 30 )
    cairo_set_source_rgba( c0, 1, 1, 1, 1 )
    Dim i As integer
    usamousex = usamousex + 200 
    usamousex = usamousey + 100 
    
      For i= 1 To CantItems
         cairo_move_to( c0, 100  , 100 + (i-1)*30 ) 
         cairo_text_extents( c0, text(i), @extents )
         cairo_show_text( c0, text(i) )
      Next i

' <= control-m O P
'          If cursorVert=0 Or cursorHori=0 then
'             cairo_move_to( c, usamousex -60 , usamousey +50 ) 
'             cairo_text_extents( c, text(9), @extents )
'             cairo_show_text( c, text(9) )
'          EndIf
'          If cursorVert=1 Or cursorHori=1 then
'             cairo_move_to( c, usamousex -60 , usamousey -40 ) 
'             cairo_text_extents( c, text(10), @extents )
'             cairo_show_text( c, text(10) )
'          EndIf

     '     menumouse = 0
     '  EndIf
    
 ' EndIf 
   If savemousex=0 Or savemousey=0 Then
      savemousex=mousex
      savemousey=mousey
   EndIf

End Sub

'------------------------------------
' https://www.freebasic.net/forum/viewtopic.php?t=10398
' Many times in my games I want to act on a single key press. 
' Multikey returns -1 constantly if a key is down and that isn't always what I want 
' so I created this function which uses multikey but will only return -1 once 
' for a particular key. The user must release and press the key again for it to fire.
' Call it just like Multikey....
'
' If KeyPress(SC_ESC)=-1 then .....!!! USARLO O USAR E.EVENT CREO ES SIMILAR O NO???
'
Function KeyPress(Key As Integer) As Integer
Static LastKey(255) As Integer
' DETECTA UNA APRETDA DEL USUARIO Y SOLO UNA CREO PROBAR
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
' -------------------------------------------------------------
