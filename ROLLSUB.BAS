Sub creaPenta (c As cairo_t Ptr, ByRef nro As Integer, ByRef octava As Integer Ptr, InicioDeLectura As Integer)

 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
 Dim As Integer semitono,n, ic,indf
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

 cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
 cairo_set_font_size (c, font)
 

 ' ======== NOTAS  =========
 ' cuando hago click en Edit el fondo cambia en main para edicion
 ' movidol loopde semitonos 11-05-2021
'If comEdit = TRUE  and estoyEnOctava = nro  Then
' If octavaEdicion=estoyEnOctava Then 
'    cairo_set_source_rgba(c, 0, 1, 0.5, 1)
' Else 
'    cairo_set_source_rgba(c, 1, 1, 1, 1)   
' EndIf   
'Else
' 
' cairo_set_source_rgba(c, 1, 1, 1, 1)
'EndIf



 Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1)
 cairo_move_to(c, 0, Penta_y )
 cairo_line_to(c, ANCHO - 1, Penta_y )

 ' posicion es el indice del vector Roll
 ' curpos es la posicion del cursor de 1 a NroCol que suma a posicion
 ' porque posicion esta congelada pero hacemos q recorremos Roll con el cursor,
 ' cuando estoy en lectura el scroll avanza o retrocede con flechas izq der...
 ' en ctrl-M modo cursor lo q se mueve es el cursor solo, el vector queda quieto
 ' la posicion del vector en donde se modifica seria en el rango
 ' posicion NroCol sumandole el offset del cursor de 1 a NroCol
 ' o sea indice vEctor en Ctrl-M = posicion + curpos pero eso sucede
 ' internanmente para mostrar congelamos posicion y comenzamos desde ahi
 ' todo proceso de modificacion debera tener en cuenta a curpos, pero no
 ' la vista por pantalla
 'If comEdit= TRUE Then
 'Dim As Integer delta

 'delta=NroCol
 If cursorVert=0 And comEdit=TRUE  Then
  If posicion < 30  Then
   posishow=  1
  
  Else
   posishow = posicion - 20
  EndIf
 Else
  posishow = posicion
 EndIf

 Dim As Integer lugar=0, sitio
 lugarOld=Penta_y

 ''' Var t = " " reempladapor sharedaprapoder poner if
 For semitono = 1 To 12
   If comEdit = TRUE  and estoyEnOctava = nro  Then
     If octavaEdicion=estoyEnOctava Then 
        cairo_set_source_rgba(c, 0, 1, 0.5, 1)
     Else 
        cairo_set_source_rgba(c, 1, 1, 1, 1)   
     EndIf   
   Else
 
       cairo_set_source_rgba(c, 1, 1, 1, 1)
   EndIf
   
  t = NotasGuia(semitono) + Str(*octava) + "_["
  cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6)
  cairo_show_text(c, t)
  t= ""
  ic=0 'indice cursor 'donde se dibujara la duracion
  n=0:indf=0


  For n = posishow To posishow + NroCol

   If Roll.trk (semitono + (nro-1) * 13, n  ).nota > 0 Or _
      Roll.trk (semitono + (nro-1) * 13, n  ).dur > 0 Then
      If comEdit=TRUE Then 
         If cursorVert=0 Then  
            If espacio = semitono Then
               Roll.trk (semitono + (nro-1) * 13, n ).dur = 181
               If fijarEspacio=0 Then
                 espacio=0
               EndIf
            EndIf
         EndIf   
' un buen forma de borrar facil y rpido seria usando
' ((n - inicioDeLectura)=curpos) y moviendoelcursor derecha izquierda
' en ctrl-m borra todo de una!! implementarlo...        
         If cursorVert=1 Then  
            If espacio = semitono And ((n - inicioDeLectura)=curpos)  Then
               Roll.trk (semitono + (nro-1) * 13, n ).dur = 181
               If fijarEspacio=0 Then
                  espacio=0
               EndIf
            EndIf   
         EndIf
     ' BORRADO LIBRE NO MARCA SOLO BLANCO habilita para usar nota=0    
    '     If cursorVert=1 And Borrar=1 Then  
    '        If ((n - inicioDeLectura)=curpos)  Then
    '           Roll.trk (semitono + (nro-1) * 13, n ).dur = 0
    '           Roll.trk (semitono + (nro-1) * 13, n ).nota = 0
    '           If fijarEspacio=0 Then
    '              Borrar=0
    '           EndIf
    '        EndIf   
    '     EndIf
      EndIf    
   
        
    cairo_move_to(c, 81 + ic * anchofig , Penta_y + semitono * inc_Penta - 4)
    Dim As Integer indf
    indf= Roll.trk (semitono + (nro-1) * 13, n).dur
    If (indf >= 1 And indf <= 182)  Then ' 13-05-2021 11
    Else
       indf=181
    EndIf ' t no puede quedar en un scope dsitinto se hace shared    
     t= figura(indf)
' ////////dar color al font en una determinada posicion     
    If n=jply Then 
       cairo_set_source_rgba(c,1,0,1,1)
    EndIf
    cairo_show_text(c, t)
 ' ' jmg 11-05-2021 1839 start
    If  n=jply And ( play =1 Or playb=1 ) Then
        
      ShowNroCol= Int(n/posishow)
      If ShowNroCol = 0 Then
         curpos= n  - 1 
      Else
         curpos= n  - posishow
      EndIf   
   '   If (kNroCol > 0) And (posicion = NroCol * kNroCol) And (posicion < MaxPos) Then
   '      curpos=curpos - kNroCol * NroCol
   '   EndIf

      cursor(c,n,nro)
      cairo_set_source_rgba(c, 1, 1, 1, 1)
    EndIf
    
' jmg 11-05-2021 1839 end 
    ' LINEAS DE COMPAS
    'If indf> 0 And indf< 181 Then
    '  Print #1," CREAPENTA t  DUR: ";indf;" figura ";figura(indf); " semitono: "; semitono
    'EndIf
    If n >0 And n <= CantTicks Then
     If Compas(n).Posi = n  Then ' ahi hay un corte un nuevo compas
      cairo_move_to(c,81 + (ic ) *anchofig +anchofig , Penta_y)
      cairo_line_to(c,81 + (ic ) *anchofig +anchofig, Penta_y + 12 * inc_Penta )
      '    Print #1, "|";
      cairo_move_to(c,74 + (ic ) *anchofig +anchofig, Penta_y + 12.5 * inc_Penta )
      t=Str(Compas(n).nro)
      cairo_show_text(c,t)
     EndIf
     If n = pasozona1 Or n =pasoZona2 Then '26-06-2021
        cairo_move_to(c,42 + (ic ) *anchofig +anchofig, Penta_y + 13.5 * inc_Penta )
         t="("+ Str(n) + ")"
         cairo_show_text(c,t)
     EndIf

    EndIf
    cairo_move_to(c, 81 + ic * anchofig , Penta_y + semitono * inc_Penta - 6)
    ic += 1
   Else
    Exit For
   EndIf
   'con ic * 40 es + 32 osea ic * 40 + 32
  Next n
  'Else
  'Print #1, " n= ";n; " posn=";posn;" MaxPos=";MaxPos;" Posishow=";Posishow
  'EndIf

  If n  > MAXPos Then
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
   nR=(13-semitono) + (hasta-nro) * 13  ' indice de la nota en Roll , en algo será util.
   PianoNota= nR 
   PianoNota= PianoNota - restar (PianoNota)

  EndIf
  lugarOld=lugar

 Next semitono
 ' PARA ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
 ' y hacer nota=semiotono 1 a 11 con el mouse...el resto es automtico...

If nro = hasta Then ' es la 10 usamos 0 tmbien ose 9 octavas+ ayuda
  cairo_set_font_size (c, font)
  'cairo_select_font_face (c, "Georgia",CAIRO_FONT_SLANT_NORMAL , CAIRO_FONT_WEIGHT_BOLD)
  t= "Flecha Abajo/Arriba o ruedita del mouse, scroll de las octavas en la ventana "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 14  )
  cairo_show_text(c, t)

  t = "F9/F10 achica/agranda el  font de las notas guia "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 15 )
  cairo_show_text(c, t)

  t = "Entrar Notas Click en [Edicion] en Menu, otro Click en Edit para modificar, ESC TERMINA LA APP"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 16 )
  cairo_show_text(c, t)

  t = "En modificacion: CTRL-M o CTRL-N pasa al MODO CURSOR para reemplazar insertar borrar notas"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 17 )
  cairo_show_text(c, t)

  t = "-/+ achica/agranda el area y altura de las octavas dentro de la ventana "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 18 )
  cairo_show_text(c, t)

  t = "AvPag/RePag scroll de las octavas mas rapido "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 19 )
  cairo_show_text(c, t)
  
  t = "Ctrl-Click en [Reproducir] Play con scroll e iluminacion de notas"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 20 )
  cairo_show_text(c, t)

  t = "Barra Espacio: Play para Debug sin scroll, luego sera igual que ctrl-clik reproducir"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 21 )
  cairo_show_text(c, t)

  t = "Pulsar F1 Para Un Notepad Con Ayuda Preliminar mas detallada, puede estar incompleta y no ser lo ultimo"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 22 )
  cairo_show_text(c, t)

  t = "En el menu algunso funcionan con Ctrl-clik otros con click solamente"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 23 )
  cairo_show_text(c, t)

  t = "En modificacion se pasa de una octava a otra para editarla, deslizando el mouse hasta el extremo izquierdo de la octava deseada, eso iluminara "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 24 )
  cairo_show_text(c, t)

  t = "la octava en edicion. (No hay vocales acentuadas, Cairo, la libreria grafica usada, no las maneja con font simples)"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 25 )
  cairo_show_text(c, t)
    
 End If

 ' si estoy en esta octava ...edicion solo para esa octava segun posicion
 ' del mouse automaticamete
 If ( Penta_y <= mousey) And ((Penta_y + 12 * inc_Penta) >= mousey)  Then
  ' estoy en una octava
  estoyEnOctava = nro '<========== DETERMINACION DE OCTAVA DE TRABAJO
  EnOctava=1
  ' CURSOR
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  If cursorHori=1 And cursorVert=1 Or play=1 Or playb=1 Then
      cursor(c,posicion,nro) 
  EndIf
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  'PERO PIERDO EL COLOR MARILLO DEL CURSOR
  cairo_set_source_rgba c, 0, 0, 0, 1
  'cairo_set_line_width(c, 3)
 EndIf
 If ((Penta_y + 12 * inc_Penta) <= mousey) And ((Penta_y + 14 * inc_Penta) >= mousey) Then
  EnOctava = 0
  estoyEnOctava=90 'praque esto ????jmg
 EndIf
 *octava = *octava -1
 If *octava < desde -1 Then
  *octava = 99
  Exit Sub
 EndIf
 '           ================  MENUES CONTEXTUALES PARA MOUSE ==================
 If ayudaModif=TRUE  And comEdit=TRUE Then
  If  (cursorVert = 1 Or  cursorHori = 1 ) Then
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

   Dim As String  text0 => "BLANQUEAR"
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
  Dim As String text(1 To 10) =>{"O","P","I","L","F","E","X","H", _
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
   If cursorVert=0 Or cursorHori=0 Then
    cairo_move_to( c, usamousex -60 , usamousey +50 )
    cairo_text_extents( c, text(9), @extents )
    cairo_show_text( c, text(9) )
   EndIf
   If cursorVert=1 Or cursorHori=1 Then
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
Sub cursor(c As cairo_t Ptr,ByRef n As Integer, ByRef nro As Integer)
 'Print #1,"1-cursor "
 cairo_set_source_rgba c, 1, 1, 0.2, 1

 'cairo_set_line_width(c, 1)
 'EL CURSOR PARA UNA linea dada o nota se coloca inc_Penta mas abajo
 ' para B 38.5 27 notaold=1  <- actualizar please
 ' para C 38.5 25 notold=12
 'n esla posicion al cargar datos deberia llamar a curor creo,,,probando

 'If s8=1 Then
 '   s8=0
 '   'curpos=(mousex- 81 )/35 ' 01-07-2021
 '   curpos=curposold
 'EndIf
 'curposold=curpos
 If cursorVert=1 Or cursorVert= 2 Then  ' vertical comEdit=TRUE

  cairo_move_to  (c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
  cairo_rectangle(c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  , inc_Penta,inc_Penta)
  cairo_move_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y  )
  cairo_line_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y + (notacur-1) * inc_Penta  +inc_Penta )
 
  RollDur=Roll.trk(notacur + (nro-1) * 13, n + curpos).dur
  
  RollNota=Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
 EndIf
 If cursorVert= 0 And cursorHori=0 Then
  If notacur=1 Then
   'Print #1,"notaold en cursor ";notaold
   notaOld=notacur
   'Print #1,"notaold en cursor laigualo a 1 ";notaold
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
' HABILITAR NOTA AL PONER EN CERO SE PUEDE USAR EN EDIT COMUN
''' If  borrar= 1 Then ' 
  cairo_set_source_rgba c, 1, 0, 0.2, 1
  Dim dursaved As Integer
  If notacur=0 Then
   notacur=notaOld 
  EndIf
  'dursaved = Roll.trk(notacur + (nro-1) * 13, n + curpos).dur
  ' debo cambiar esa nota por (equivalente en silencio no va mas
  'para eso entra un silencio) se pone nota cero y dur 0
  
  Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 0
  Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = 0
  'Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = dursaved + 16
 ' If Roll.trk(notacur + (nro-1) * 13, n +curpos).dur > 181 Then
 '  Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = 181
 ' EndIf
  borrar = 0
  ''calcCompas(n) ''mayorDurEnUnaPosicion (n)
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
  'si en una columna todas las notas son 0,181 y sihay un 254 en dur
  ' y nota esa columna se elimina,,,,,implementar 12-05-2021
  ' la forma facil de borrar seria grabar a disco cuando se está
  ' en posicion con 254 saltar esa posicion y restar 1 a la posicion final.


  
 If DUR=181 Then
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 0
     Roll.trk(notacur + (nro-1) * 13, n + curpos).nota =181   ' 5.7.2
     cambiadur = 0
     DUR=0
 ElseIf DUR=182 Then ' borrado 
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 182
     Roll.trk(notacur + (nro-1) * 13, n + curpos).nota =181   ' 5.7.2
  '   Print #1,"FINDE SECUENCIA !!"
     cambiadur = 0
     DUR=0
 ElseIf DUR=0 Then ' borrado
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 190
     Roll.trk(notacur + (nro-1) * 13, n + curpos).nota =190   ' 5.7.2
     cambiadur = 0
     DUR=0
 Else
'desde aca lasnotas se colocan con notacur siemrpe tienen valorde 1 a 12
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR
  EndIf
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 9
  EndIf
 
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 18
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 27
  EndIf 
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 36
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 45
  EndIf
   
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 54
  EndIf
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 63
  EndIf
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 72
  EndIf
   
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=0 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 81
  EndIf   
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 90
  EndIf   
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 99
  EndIf 

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 108
  EndIf

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 117
  EndIf

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 126
  EndIf
'----- fin 3er bloque   
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 135
  EndIf   

  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 144
  EndIf   

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 153
  EndIf   

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 162
  EndIf   

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=1 Then 
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 171
  EndIf   
  Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2    
 EndIf

 '   vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
  cambiadur = 0
  cuart=0:pun=0:doblepun=0:tres=0:sil=0:mas=0

  ReCalCompas() 'organizaCompases()
  nota = 0

  ' fin nuevaversion
 EndIf
 ' ===> PROCESO INSERT
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
  ' Print #1, "Entra a Cursor con insert 2 notacur: ", notacur
  ' StartInsert esglobal,posicion de inicio de la insercion cuando  pulsamos
  ' la tecla insert se guarda la posicion de inicio desde ahi habra reemplzos
  ' usando la posicion comun n
  ' hasta que se pulse FINose alcance el FIN (66)
  ' Print #1, "call a movedato con indaux ",indaux
  ' n es StartInsert fijo no se modifica mas
  movedato (n + curpos,indaux, insert,notacur + (nro-1) * 13 ) 'antes de reemplazarlo xxx
 '  Print #1, "regreso de movedato con indaux ",indaux
  ' param : posicion comienzo (fijo), indice incremental para el aux
  ' ,insert comando habilitado = 1
  'luego reemplazo
  ' Print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (nro-1) * 13),n
  Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = DUR ' reemplazo con lo nuevo
  'vdur=Roll.trk(notacur + (nro-1) * 13, n+curpos).dur
  ' SI LA NOTA  NO EXITIA LA agrega...osea sirve para acordes tambien
  'Print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (nro-1) * 13)

  Roll.trk(notacur + (nro-1) * 13, n+curpos).nota = notacur ' 5.7.3
  'vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es
  ' que se cargala nota en roll,<- pero 1ero debo guardar la nota de Roll en aux
  ' antes de reemplazarla
  insert=1  ' habilita de nuevo la tecla I deingreso de insercion en un lugar
  'elegido con cursor
 ' Print #1, "insert 1 ",insert

 EndIf
 'proceso modificar Nuevo secunci limpia terminda en NroCol ,con
 ' nombre de letra pulsada en vez de X CTRL-N
 ' ====> MODIFICAR con ctrl-n
 If (cursorHori=2 Or cursorVert = 2) And agregarNota=1  And nota> 0 Then
 
  Roll.trk(nota + (nro-1) * 13, n + curpos).dur = DUR
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll.trk(nota + (nro-1) * 13, n + curpos).nota = nota  ' 5.7.3.4
   ReCalCompas() 
   nota=0
 EndIf
 ' VEMOS QUE HACE ESTA FUNCION SI SIRVE
 ''cairo_surface_get_device_offset
 cairo_stroke(c) ' aca el stroke da el amarillo y no jode a teclas - + con saltos!

'If Roll.trk(notacur + (nro-1) * 13, n+curpos).dur >= 1 And _ 
'   Roll.trk(notacur + (nro-1) * 13, n+curpos).dur <= 181 Then
   If cursorHori=1 And vdur=0 And vnota=0 And copiar=1 Then
      vnota=notacur 
      vdur =Roll.trk(notacur + (nro-1) * 13, n+curpos).dur  ' 5.7.3
       copiar=2
   EndIf
'EndIf

'If Roll.trk(notacur + (nro-1) * 13, n+curpos).dur  < 1  And _  
'   Roll.trk(notacur + (nro-1) * 13, n+curpos).dur   > 180 Then
If cursorHori=1 And vdur>0 And copiar=3 Then
   If notacur<> vnota And MultiKey (SC_ENTER) Then 
      Roll.trk(notacur + (nro-1) * 13, n+curpos).nota = notacur 
      Roll.trk(notacur + (nro-1) * 13, n+curpos).dur = vdur
      copiar=0
      vdur=0:vnota=0
      ReCalCompas()
   EndIf 
EndIf
If nro=0 Then ' error al subir mucho mira despues del and el estupido 
nro=1 'jmg 16-05-2021 notcur nodeberi empezar desde 0
EndIf
If notacur=0 Then
  notacur=1
EndIf

If copiar=3 And Roll.trk(notacur + (nro-1) * 13, n+curpos).dur = vdur _
   And notacur=vnota Then
   copiar=0:vdur=0:vnota=0  

EndIf
  
End Sub
'----------------------------------------
Sub Tracks (nroTrack As UByte, nroCanal As UByte)
 ' sizeof array de ubytepr almcenar el pentagrama roll
 'Notas ( 1 To 65536, 1 To 128) as UByte

End Sub
'-------------------------------MENU----------
Sub menu (c0 As cairo_t Ptr, c As cairo_t Ptr,n As Integer,menuNro As Integer )
 ''Print #1,"1-menu "
 Dim As Integer fontOld = font
 font=18
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
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
  ' 0.1 ==> [Archivo]
   If mousex > 38 And mousex< 117 And mousey < 50  Then
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

'  0.2 ===> [Edicion]
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

' 0.3 ===> [Ver]
   If mousex > 225 And mousex< 263 And mousey < 50 Then
    cairo_set_source_rgba c, 0, 1, 0, 1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Ver]":cairo_show_text(c, t)
' 0.4 ====> [Pista]
   If mousex > 279 And mousex< 329 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Pista]":cairo_show_text(c, t)
   ' ------------------------------------------
' 0.5 ===> [Reproducir]
   If MultiKey(sc_Control) AND (mousex > 348 And mousex< 453 And mousey < 50   And play=0) Then
    cairo_set_source_rgba c, 0, 1, 0, 1
      
     While InKey<>"" :Wend
   '   Print #1,"--->entra a reproducir thread1 "
              play=1        
'    If (MouseButtons And 1) Then
        menuNew =0
        If  MaxPos > 1 Then
         thread1 = ThreadCreate(@PlayAll)
        EndIf 
   '     Print #1,"--->paso llamada a thread1 "
    ''    menuNew = 4 por hora sin elecciones
 '   EndIf

   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Reproducir]":cairo_show_text(c, t)
   ' --------------------------
 '0.7  ===> [Opciones]  
   If MultiKey(sc_Control) And mousex > 473 And mousex< 562 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=5
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Opciones]":cairo_show_text(c, t)

' 0.8 ===> [Ayuda]
   If MultiKey(sc_Control) And mousex > 582 And mousex< 643 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
      Shell ("start notepad ayuda.txt")
    End If
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   Dim muestra As String=nombre
   If Len(muestra) > 60  Then
     muestra=" C:\..\"+Mid(muestra,30)
   EndIf
   muestra=Str(PianoNota) + muestra   

   t=  " [Ayuda] "+ muestra :cairo_show_text(c, t)



   cairo_stroke(c)
  Case 1
  '1.1
   t=  "      <MENU=(<=|=>)>/<VENTANA=(MOVER=DRAGAR CINTA)(POSICON NORMAL,SUBE/BAJA BORDE INFERIOR=F7-F8)> "

  Case 2 ' para posicionar  en pantalla NO BORRAR. <= EDIT MENU
 
   menuNew=2
   '2.1 ===>      EDIT
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
   
'   If COMEDIT=TRUE Then ' jmg 02-06-2021 ver porque difieren
'     nR=nR-3
'   End If
indiceNota=(mousex- 81 )/35 + posishow
Dim As Integer veonota, veocopi 
If indiceNota > 0 Then
veonota=Roll.trk(117-nR,indiceNota).nota 
EndIf
If copiar >0 Then
   veocopi=copiar
EndIf   
If copi >0 Then
   veocopi=copi
EndIf   

   t = "      EDIT" + " " +  " UltimaPos:" + _
   Str(Maxpos) +" Posi:" + Str(posishow)+ " Posn:"+Str(posn)+ _
   " Font:" +Str(font) + " EnOct:" + Str(estoyEnOctava) + " nE:"+Str(veonota) + _ 
   " nR:"+ Str(PianoNota) + " Curpos:" +Str(curpos) + _
   " RollDur :"+ Str(RollDur) + " Rollnota:" + Str(RollNota) + _ 
   " Copiar:" + Str(veocopi) +" Col:" +Str(indicenota) + " DUR:"+ Str(Dur) + _ 
    " Nota:"+ Str(nota) 
   '''' " Mx:" + Str(mousex) + " My:" + Str(mousey) 
  ' + "Desde:" + Str(y1) + "Hasta:" +Str(y2)
   /' " InicioDELectura: " + Str(InicioDelectura) + " BordeSupRoll: " + Str(BordeSupRoll) +
'/
   '''   " modifmouse:" + Str(modifmouse) + " usamousey:" + Str(usamousey) + " usamousex:"+ Str(usamousex) + _

   cairo_show_text(c,t)

   cairo_stroke(c)

   menuNew=2
  ' ============================================================= 
  ' BORRAR HACIA Derecha y Ajustar MaxPos a la posicion actual en Edit Lectura
  '====================================================== 
  If mousex > 91 And mousex < 174  And mousey < 50 Then ' <= MaxPos ajuste a la posicion dada
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MultiKey(SC_CONTROL ) Then 
         If dobleclick Then
            MaxPos=posishow +1
            posn=posishow        
            Dim As Integer r1,i
            For i=NB To NA 
              For r1= posn+1 To CantTicks
                 Roll.trk(i,r1).nota=0
                 Roll.trk(i,r1).dur=0 
              Next r1 
            Next i 
        EndIf
      EndIf
  Else
    cairo_set_source_rgba c, 1, 1, 1, 1    
  EndIf  
 
  If mousex > 500 And mousey < 50 And mousex < ANCHO-50 Then
     If MouseButtons And 1 Then
      MenuNew=0
     EndIf
  EndIf


  Case 3
   '   t = " [NUEVO] [ABRIR] [GRABAR] [GRABAR COMO] [EXPORTAR] [CERRAR] [SALIR] "

   '===> 3.1 [NUEVO]
   If MultiKey(sc_Control) And mousex > 42 And mousex < 110  And mousey < 50  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
    
      If MultiKey(sc_control) Then
       If dobleclick Then
          ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)
          ReDim compas(1 To CantTicks)
          ReDim (RollAux.trk) (NB To NA , 1 To CantTicks)
          MaxPos = 1 'NroCol
          posicion=1:posn=1
          curpos=1
          notacur=1
          nroCompas=0
          tres=0:pun=0:sil=0:mas=0:doblepun=0:cuart=0
          tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
          SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
          nombre = ""
          Dim As Integer r1,i
          For i=NB To NA 
            For r1= posn+1 To CantTicks
                Roll.trk(i,r1).nota=0
                Roll.trk(i,r1).dur=0 
             Next r1 
         Next i           
       EndIf
      EndIf 
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = "      [NUEVO]":cairo_show_text(c,t)

   '===> 3.2 [ABRIR]

   If mousex > 134 And mousex < 190  And mousey < 50 And carga=0  Then ' <= ABRIR lee grabaroll.roll
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And carga = 0 Then
     CargaArchivo()
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
    carga=0  ' <======= control de Carga
   EndIf
   t = " [ABRIR]":cairo_show_text(c,t)
   
  '===> 3.3[GRABAR] SC_F11 COPIA
   If MultiKey(sc_Control) And mousex > 223 And mousex < 296  And mousey < 50  Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
      ''      Print #1, "Grabando a disco Roll GRABAR "
      '''' dialogoText("Grabar Archivo")
       Dim As String nombreg
       If nombre = "" Then
          getfiles(file,myfilter,"save")
          nombreg=*file.lpstrFile
          If nombreg = "" Then
             Exit Sub
          Else
             nombre=nombreg   
          EndIf
       EndIf
''''       grabaprueba()
       GrabarArchivo (nombre)
    EndIf
   Else
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = " [GRABAR]": cairo_show_text(c,t) :t= ""
   '
  '===> 3.3[GRABAR COMO] SC_F11 COPIA 
   If MultiKey(sc_Control) And mousex > 320 And mousex < 440  And mousey < 50  Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
     '       Print #1, "Grabando a disco Roll GRABAR "
       '''''no usar dialogoText("Grabar Archivo")
       Dim As String nombreg
       getfiles(file,myfilter,"save")
       nombreg=*file.lpstrFile
       If nombreg = "" Then
          Exit Sub
       Else
         nombre=nombreg   
       EndIf
     ''Open "Trabajo_03.roll" For Binary Access write As #2
     ''Open nombreArchivo +".roll" For Binary Access write As #2
       GrabarArchivo(nombre)
    EndIf
   Else
    cairo_set_source_rgba c,1,1,1,1
   EndIf


   t = " [GRABAR COMO]" : cairo_show_text(c,t) :t= ""
   If mousex > 350 And mousey < 50 Then
    If MouseButtons And 1 Then
     MenuNew=0
    EndIf
   EndIf
  Case 4  ' <======= COMO REPRODUCUIR
   cairo_set_source_rgba c,1,1,1,1
   t=  "      [DESDE INICIO][DESDE ESTA POSICION] [VOLUMEN / + / - /] No habilitado"
'----
   If mousex > 330 And mousey < 50 And mousex < ANCHO-50 Then
    If MouseButtons And 1 Then
       MenuNew=0
    EndIf
   EndIf
  Case 5 ' OPCIONES MENU, [COMPAS] [OCTAVAS] [TEMPO] [CONFIGURACION]
   
  If mousex > 38 And mousex < 120 And mousey < 50  Then  'COMPAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew= 7
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [COMPAS]":cairo_show_text(c, t)
  If mousex > 130 And mousex < 235 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=6
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [OCTAVAS]":cairo_show_text(c, t)
  If mousex > 245 And mousex < 330 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=8
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [TEMPO]":cairo_show_text(c, t)
 '
  If MultiKey(sc_Control) And mousex > 354 And mousex < 514 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=9
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf

   t=  " [CONFIGURACION]": cairo_show_text(c, t)
 cairo_stroke(c)
 
  Case 6 'OPCIONES => OCTAVAS
   
' ===> 6.1 [OCTAVAS DESDE   - / + ] 
   If mousex > 38 And mousex< 123 And mousey < 50  Then ' OCTAVAS LABEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1  Then
     octavas=0
     Dim Cantitems As Integer => 12
     Dim As String text (1 To 12) => { _
     "Click En 'Desde' antes de cada click en - / + ", _
     "Click En 'Hasta' antes de cada click en -/+", _
     "Esto redimensionara la capacidad de Octavas", _
     "Cuando grave, esa capacidad se graba en el archivo",_
     "y es automatico, o sea, se cargaran los datos ", _
     "y la capacidad de Octavas. A mas Octavas mas memoria usada", _
     "El numero de octavas puede achicarse respecto del inicio",_
     "pero agrandar puede destruir la memoria, si la cantidad de",_
     "octavas al inicio es menor al proyectado, por ello para incrementar",_
     "las octavas conviene iniciar siempre con el maximo de octavas y luego",_
     "cargar el archivo de octavas reducido, si en la misma sesion se desea",_
     "incrementar se lo podra hacer sin fallos"}

     menugrafico  c0, c, CantItems, text()

    EndIf

   EndIf
'  ----> DESDE
   If mousex > 144 And mousex< 196 And mousey < 50 And MultiKey(sc_Control) Then ' DESDE LABEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    octavas=0
    redimensionar=0
   EndIf
' ---->  -
   If mousex >= 215 And mousex< 229 And mousey < 50   Then ' - LBEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And octavas=0 Then
     desde -= 1
     If desde < 1 Then 'desdevector Then
      desde = 1 'desdevector
     EndIf
     octavas=1
    RTA = "SOLO REDIMENSION VISUAL "
    EndIf
   EndIf
' ----> +
   If mousex >= 241 And mousex< 261 And mousey < 50   Then ' + LBEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And octavas=0 Then
     desde = desde + 1
     If desde > hastavector Then
      desde = hastavector
     EndIf
     octavas =2
    RTA = "SOLO REDIMENSION VISUAL "
    EndIf
   EndIf
   t = "      [OCTAVAS DESDE   - / + ] " + Str(desde-1): cairo_show_text(c,t)
   ' --------------------------------
' ===> 6.2 [HASTA  - / + ]
   If mousex > 308 And mousex< 363 And mousey < 50  Then ' HASTA
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     octavas=0
     redimensionar=0
    EndIf

   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
' ----> -    
    If mousex >= 374 And mousex< 394 And mousey < 50  Then '-
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas = 0 Then
      hasta -= 1
      If hasta < desdevector Then
       hasta = desdevector
      EndIf
      octavas=1
      RTA = "SOLO REDIMENSION VISUAL "
     EndIf
    EndIf
' -----> +
    If mousex >= 403 And mousex< 423 And mousey < 50   Then ' +
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas = 0 Then
      hasta += 1
      If hasta > 9 Then
       hasta = 9
      EndIf
      octavas=2
      RTA = "SOLO REDIMENSION VISUAL "
     EndIf
    EndIf
   EndIf

   t = "  [HASTA  - / + ] " + Str(hasta-1): cairo_show_text(c,t)

' ===> 5.3 [REDIMENSIONAR]
 
   If mousex >= 473 And mousex< 634 And mousey < 50   Then ' LABEL REDIMENSIONAR
    cairo_set_source_rgba c, 0, 1, 0, 1
' cargar archivo anda bien redimensiona,¿porque no puedo redimensionr
' aca aunque no modifique los limites?  
    If dobleclick And redimensionar=0 Then
     cairo_set_source_rgba c, 1, 0, 0, 1
     NB => 1 + (desde) * 13   ' 27 para 3
     NA => 12 + (hasta) * 13  ' 90 para  7
  '   Print #1,"NB,NA,DESDE,HASTA ";NB,NA,desde,hasta
     CambiarDim()
     MaxPos = 1 'NroCol
     posicion=1:posn=1
     curpos=1
     notacur=1
     nota=0 
     notaOld=0
     inicioDeLectura=0

     redimensionar=1
     RTA="REDIMENSION VISUAL Y FISICA OK"
   '  Print #1, RTA

    EndIf
   Else
  '  cairo_set_source_rgba c, 1, 1, 1, 1
   
   EndIf
   t = "  [REDIMENSIONAR]<-Borra Datos " + RTA
   Case 7
   If mousex > 38 And mousex < 90 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=10000000
     menuNew= 7
     ReCalCompas()
     TCompas="4/4"
    
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [4/4]":cairo_show_text(c, t)
  If mousex > 100 And mousex < 150 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=15000000
     menuNew=7
     TCompas="12/8"
     ReCalCompas()
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [12/8]":cairo_show_text(c, t)
  If mousex > 160 And mousex < 210 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=5000000
     menuNew=7
     ReCalCompas()
     TCompas="2/4"
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [2/4]":cairo_show_text(c, t)
 
 cairo_stroke(c)
  Case 8
 ' USAR LA ENTRADA DE TEXTO DE IUP
    dialogoText("AJUSTE TEMPO")  
    tiempoPatron= Cdbl(nombreArchivo)
    If tiempoPatron = 0 Then
       tiempoPatron=60
    EndIf
    menunew=0
  Case 9
  ' [MIDI] 
    If mousex > 42 And mousex < 90  And mousey < 50 And listaMIDI = 0  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menunew=10 
         
      EndIf ' mousebuttons
 '     
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
    EndIf
    t = "      [MIDI]":cairo_show_text(c,t):t=""
 Case 10

    If mousex > 90 And mousex < 190  And mousey < 50 And listaMIDI = 0 Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
        ''thread2 = ThreadCreate(@ComboBox)
       Dim miport As Integer=1
       thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))  '' 'GtkListBox()

      EndIf ' mousebuttons
 'listaMIDI=1     
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
    EndIf

    t = "             [MIDI-OUT]":cairo_show_text(c,t):t=""
    If mousex > 200 And mousex < 300  And mousey < 50 And listaMIDI = 0 Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
        ''thread2 = ThreadCreate(@ComboBox)
       Dim miport As Integer=2
       thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))  '' 'GtkListBox()

      EndIf ' mousebuttons
 'listaMIDI=1     
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
    EndIf

    t = " [MIDI-IN]":cairo_show_text(c,t):t=""
      
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
  Dim As String  text => "HOLA SOY TU AYUDA!"

  Dim As cairo_text_extents_t extents

  If mousey < 50 Then
   cairo_set_font_face( c, cface )
   cairo_set_font_size( c, 36 )
   cairo_text_extents( c, text, @extents )
   cairo_move_to( c, mousex, mousey )
   cairo_set_source_rgba( c, 1, 1, 1, 1.0 )
   cairo_show_text( c, text )
  EndIf
 EndIf
 font=fontOld
End Sub
'----------------------------------------------------------------------
Sub menuAcc(c As cairo_t Ptr)
 ' IMPLEMENTAR NO SE USA
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
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
Sub movedato (ByVal n As Integer, ByRef indaux As Integer,ByRef  insert As Integer, ByRef nota As Integer)

 ' OJO n ya entra conla suma de curpos
 ' indaux parte de cero es el indaux para el auxiliar que se ha borrado
 ' y por ende empieza por 1 no se guarda esun auxiliar...
 '
 indaux = indaux + 1 ' indaux auxiliar SC_INSERT INICIALIZA EN 0
 ' Print #1, "----EN MOVEDATO indaux, nota: ",indaux , nota
 'notas gurdadas para luego usar en proceso CORRECCION
 notins = notins + 1
 notasInsertadas (notins) = nota  ' ENTRADA POR TECLADO ANTES DE PULSAR INSERT
 'LA DURACION NO HACE FALTA PARA CONTROL,QUEDARA EN ROLL
 'muevo datos QUE SERAN remplazados al auxiliar PARA ESA POSICION
 Dim As Integer i1,i2 ' no movemos solo el indice <nota> sino todas las notas!
 ' INDAUX PARTE DE 1 Y SE Va INCREMENTANDO CON CADA PULSO SOLO DE  I...

 For i1 = NB To NA
  RollAux.trk(i1,indaux)= Roll.trk(i1,n ) 'xxx  n ya contiene curpos
 Next i1
 ' movio la nota queestaba antes de pulsar insert
 'Print #1, "EN MOVEDATO carga RollAux.trk con valores viejos de roll"
 'Print #1,"RollAux.trk(nota,indaux) nota,indaux " ,nota,indaux
 'Print #1,"Roll.trk(nota,n) nota,n, curpos" ,nota,n, curpos
 'Print #1,"duracion cargda RollAux.trk(nota,indaux).dur: ", RollAux.trk(nota,indaux).dur
 'Print #1,"nota  cargda RollAux.trk(nota,indaux).nota : ", RollAux.trk(nota,indaux).nota
 'vamos reemplazando los eventos y sobreescribiendo con los nuevos pero
 '  los viejos se van guardando para desplazarlos a la derecha.
 ' RollAux.trk tendra lo viejo, notas insertdas lo nuevo
End Sub

'--------------------------
Sub moveresto (ByVal StartInsert As Integer, ByRef indaux As Integer, ByRef insert As Integer, ByRef nota As Integer)
 Dim As Integer indpos,lastPosRollReemplzada,s,t,k,i
 lastPosRollReemplzada=posicion + curpos
 indpos=posicion + curpos'indice posicion partimos de ultima posicion global reemplazada
 '   Print #1, "EN MOVERESTO, indaux,StartInsert: ",indaux ,StartInsert
 ' ultima posicion movida a aux  es indaux (indaux POSICION DE AUX) JMG
 Dim As Integer lastIndAuxporInsercion = indaux
 'Print #1, "moveresto inicio: indaux,indpos,posicion :", indaux,indpos,posicion
 'Print #1, "entra Loop, indaux,StartInsert,indpos: ",indaux,StartInsert ,indpos
 Dim As Integer cantInserciones = indaux ' o notins el indaux continua en movedata las inserciones nuevas
 ' durante lascuales se movio los valores viejso a aux pero quedaron
 ' losnuevos en Roll. que no debo opisar
 '1) COPIAMOS A AUX EL RESTO DE ROLL
 Do

  'indpos ya se le sumo curpos... COPIAMOS A AUX EL RESTO DE ROLL NO MODIFICADO COMPLETA
  indpos=indpos+1 ' empezamos de la siguiente no reemplazada y seguimos al final
  ''suponemos que posicion=indpos era la posicion de ultima reemplazada
  indaux=indaux+1 'nosubicamos en 1er posicon libre en aux para recibir el resto
  'Print #1,"antes del FOR Roll,RollAux.trk.. indpos,indaux ", indpos,indaux
  ' debemos barrer solo la octava? no porque puede haber otras octavas cargadas
  For s= NB To NA 'barremos todas las notas verticalmente para una misma posicion
   If Roll.trk(s,indpos).dur = 182 Or indpos=MaxPos Then ' fin de datos de Roll
    RollAux.trk(s,indaux) = Roll.trk(s,indpos)
    insert= 4 ' fin de llenado de almacenamiento auxiliar
  '      Print #1,"sale moveresto encontro 182 fin, indaux :" ,indaux
    Exit Do
   EndIf
   RollAux.trk(s,indaux) = Roll.trk(s,indpos)
   If s=1 Then
  '    print #1, "luego de la copia a AUX del resto "
   EndIf
   If (Roll.trk(s,indpos).dur <>181  And Roll.trk(s,indpos).nota <> 181) _
    And (Roll.trk(s,indpos).dur > 0 Or Roll.trk(s,indpos).nota > 0)    Then
   '    print #1, "Roll.trk(";s,indpos;").nota ",Roll.trk(s,indpos).nota
   '    print #1, "Roll.trk(";s,indpos; ").dur ",Roll.trk(s,indpos).dur
   EndIf
   If (RollAux.trk(s,indaux).dur <> 181 And RollAux.trk(s,indaux).nota <> 181) _
    And (RollAux.trk(s,indaux).dur > 0 Or RollAux.trk(s,indaux).nota > 0)    Then
   '    print #1, "RollAux.trk(";s,indaux; ").nota ",RollAux.trk(s,indaux).nota
   '    print #1, "RollAux.trk(";s,indaux; ").dur ",RollAux.trk(s,indaux).dur
   EndIf

  Next s

  'posicion + curpos tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultimo reemplazo o insercion
 ' el 66 lo muevo??? sino debo hcer indaux = indaux -1
 ' indaux=indaux - 1 no le resto 1 copio el 66...el fin se corre

' Print #1, "StartInsert: " ,StartInsert '  ya se le sumo curpos
 If insert = 4 Then 'movemos de nuevo todo el aux completo a Roll desde
  ' laultimainsercion o sea Startindice +
 ' Print #1, "insert 4 movemos Aux a ROLL indaux maximos elementos en Aux: ",indaux
  t = StartInsert + cantInserciones ' xxx  OK

 ' Print #1, "posicion, posn "; posicion, posn
  For k = 1 To indaux ' todo el auxiliar SUMA ROLL REEMPLAZADO + NOREEMPLAZADO
 '  Print #1, "k auxiliar "; k
   For s=NB To NA  ' todos los semitonos 96
    Roll.trk(s,t) = RollAux.trk(s,k)
    If s=1 Then
 '    Print #1, "luego de la  copia A ROLL "
    EndIf
    If (Roll.trk(s,t).dur <> 181 And Roll.trk(s,t).nota <> 181) _
     And (Roll.trk(s,t).dur > 0 Or Roll.trk(s,t).nota > 0)    Then
   '    Print #1, "I) Roll.trk(";s,t;")nota,s,t : ", Roll.trk(s,t).nota, s, t
   '    Print #1, "I) Roll.trk(";s,t;")dur, s,t : ", Roll.trk(s,t).dur,s,t
    EndIf
    If (RollAux.trk(s,k).dur <> 181 And RollAux.trk(s,k).nota <> 181) _
     And (RollAux.trk(s,k).dur > 0 Or RollAux.trk(s,k).nota > 0)    Then
   '    Print #1, "II) RollAux.trk(";s,k;").dur ", RollAux.trk(s,k).dur
   '    Print #1, "II) RollAux.trk(";s,k;").nota ", RollAux.trk(s,k).nota
    EndIf
   Next s
   t=t+1
  Next k
 ' Print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
  '' posicion = MaxPos con posn es suficiente,sino se va al tramo final
  posn = MaxPos
'  Print #1, "usamos MxPos pra posicion, posn "; posicion, posn
  '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
 ' Print #1 ,"final ciclo moveresto a AUX"

  For s = NB To NA
   For k=  StartInsert To MaxPos ' gracias a esto anda acordes
    If Roll.trk(s ,k).nota = 0 Then
     Roll.trk(s , k).nota = 181
    EndIf
   Next k
  Next s
  ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR
  ' nota en 181 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
  ' estos son los valores  StartInsert y cantInserciones.
  ' StartInsert es de Roll antesdela insercion
'  Print #1, "==> valor de nota antes de CORRECCION: ",nota

  ' aca revisamos el Roll solo la parte insertada , encada insercion en Roll
  ' esa posiicon se corresponde al de notas insertadas biunivocmente,,
  ' BORRAMOS LO QUE ESTABA ANTES PUES YA SEMOVIO ALFINAL
  ' EN CADA POSICION SOLO DEBE QUEDAR UNA SOLA NOTA, LA INSERTADA,
  ' SOLO SE PERMITE INSERTR UNA SOLA NOTA VERTICALMENTE O SEA PARA UNA MISMA POSICION
  ' PERO PARA DISTINTAS POSICIONES HABRA MAS NOTAS INSERTADAS UNICAS SI SE DESEA
  ' LUEGO CON PROBRSI LA NO TA EN ROLL ES IGUAL O DISTINA A LA INSERTADA SE PUEDEN
  ' BORRAR LAS DISTINTAS..Y EL ORDEN ES EL MISMO...SEGUN POSICION..
  Dim i As Integer
  For k= StartInsert To StartInsert + cantInserciones -1 ' OK PARA 1 INSERCION ES SOLO EN STRTINSERT
   i = i + 1
   For s= NB To NA
    If s <> notasInsertadas (i) Then
     Roll.trk(s,k).nota = 181
     Roll.trk(s,k).dur  = 0
    EndIf
   Next s
  Next k
 EndIf
 insert=0
 ReCalCompas() ' 09-05-2021
End Sub
'------------------------------------
Sub botones(hWnd As HWND, c As cairo_t Ptr, cm As cairo_t Ptr, x As Integer,y As Integer)
 ' RECTANGULO DE 40 X 40, FONDO ROJO
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size

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
Sub calcCompas( ByRef j As Integer)
 ' nota:fijarse que pasa con el grupo..1 to 4 de las 64 duraciones
 ' x print #1,"========================================================="
 ' x print #1,"1-calcCompas j o posn RECIBIDA: "; j
 Dim As Integer k, i,cantFalta,CantSobra, tengo, falta,sobra, noct
 
 If DUR=0  Then
  Compas(j).Posi=0
  Compas(j).nro = 0

  ' x print #1,"CALCOMPAS-0 DUR=0 COMPAS(J)=0 "
  Exit Sub
 EndIf
 'If DUR=181 Then
 ' Compas(j).Posi=0
 ' Compas(j).nro = 0

 ' ' x print #1,"CALCOMPAS-0 DUR=181 COMPAS(J)=0 "
 ' Exit Sub
 'EndIf

' estonunca seejecuta porque j es 4 nouncaentr con 1!!
 If j= 1 Then ' 128 es el maximo de ticks de un compas se agregan amspara terminared analizar elcompas
  'Erase (buffCompas_1) no usados pro ahor no son necesarios....
  'Erase (buffCompas_2)
  acumulado=0
  ''erase (compas) 'cada item es la posicion en donde
  ' x print #1,"J=1 comienzo calcCompas "
  jc=j
 EndIf
 ' x print #1,"1-calcCompas 2"
 If jc > 128 Then
  id=1
  ' procesar el resto que sobra luego de 129 en el buffer 2
 EndIf
 Dim As Integer partes_falta (1 To 100), partes_sobra(1 To 100) 
 ' x print #1,"1-calcCompas 3, DUR: ";DUR; " j: "; j; " figura: ";figura(DUR)
  
 For k=1 To 180
  If DUR=k   Then
   acumulado =acumulado +pesoDur(k)
   ' x print #1,"acumulado / k / PEsoDur "; acumulado, k , pesoDur(k);" figura ";figura(DUR)
   If abs(d7 -acumulado) < 10 Then
      acumulado = d7
   EndIf
   If acumulado = d7 Then
    Compas(j).Posi = j
    nroCompas = nroCompas+1
    Compas(j).nro = nroCompas
    ' x print #1,"acumulado ";acumulado
    ' x print #1,"Compas(j) , j: ";Compas(j).Posi;" "; j

    acumulado=0
    jc=1
    ' x print #1,"1-calcCompas 3.1 exit sub acumulado=0 ";acumulado
    Exit Sub
   EndIf

   If acumulado < d7  And j > 0 Then
    ' x print #1, "j que hace cancelar ";j
    Compas(j).Posi = 0   
    Compas(j).nro = 0 ' reseteo
    velocidades(j)
    ' x print #1,"1-calcCompas 3.2 acumulado menor sigue exit sub, carga: ";carga
    Exit Sub
   EndIf
   ' cuando cargamos desde disco aca nunca es necesario pasar pues
   ' ya esta todo fraccionado para tener compases completos sin notas
   '  que sobren o falten
   If carga=1 Then ' se supone que nunca carga=1 se usara en el resto
    'posn=j
    Exit sub
   EndIf

   Dim z1 As Integer
   ' x print #1,"1-calcCompas acumulado 3.3 >d7  ";acumulado; " carga: ";carga
   If acumulado > d7 And carga =0 Then
    ' x print #1,"acumulado > d7 :";acumulado
    tengo= acumulado - pesoDUR(DUR)
    ' x print #1,"tengo "; tengo
    falta=d7 - tengo
    ' x print #1,"falta ";falta
    sobra=acumulado -d7
    ' x print #1,"sobra "; sobra
    '       Dim ultimafigura As Integer=0
    separarDur(j,DUR, partes_falta(),cantFalta,falta)
    ' x print #1,"||||| CantFalta ";CantFalta
    If CantFalta > 100 Then
     ' x print #1, "ERROR CANTFALTA > 100 ";CantFalta
     EXIT Sub
    EndIf
    For z1 = 1 To CantFalta
     ' x print #1, " ";partes_falta(z1);" ";figura(partes_falta(z1));
    Next z1
    ' x print #1,
    separarDur(j,DUR, partes_sobra(),cantSobra,sobra)
    ' x print #1,"||||| CantSobra ";CantSobra
    If CantSobra > 100 Then
     ' x print #1, "ERROR CANTSOBRA > 100 ";CantSobra
     EXIT Sub
    EndIf

    For z1 = 1 To CantSobra
     ' x print #1, " ";partes_sobra(z1);" "; figura(partes_sobra(z1));
    Next z1
    ' x print #1,
    ' x print #1,"HAGO  DUR=0 BUENO.pero el sobra va a acum.."
    'preparo el acumulado de Falta ntes de reemplazar
  '  acumulado=pesoDur(partes_sobra(CantSobra))  'jmg 30-3-21
  '  ' x print #1,"acumulado sobra ";acumulado 
   EndIf
   ' x print #1,"1-calcCompas 3.4"
   '  EndIf
   Exit for
  EndIf
 Next k
 ' x print #1,"1-calcCompas 4  ingreso a REEMPLAZO EN ROLL de las durciones y notas"

 acumulado=tengo
 ' la ultima nota a cortar se ha cargado es la quese reemplaza de este modo
 ' el codigo nucleo no se toca para nada solo se copia las partes necesarias
 ' para imitar su comportamiento...el programa introduce auomaticamente las
 ' figuras de reemplazo y haria el corte ....
 ' valido en la carga manula pero no desde disco
 ' en disco esta ya todo procesado solo debo sumarduraciones
 ' aradeterminar compas. En Nucleo se busc siemrep la not que este en 0
 ' ergo parareemplazardebo borrar el contenido o reemplazarlo
 If CantFalta > 0 And carga= 0 Then  ' empezamos el reemplazo en el vector desde posn
  ' x print #1,"Recordar posn empieza en 0 "
  ' x print #1,"REEMPLAZO Cantfalta ";CantFalta
  For k=1 To CantFalta '
   If partes_falta(k)>= 1 And partes_falta(k) <=90 Then
    partes_falta(k)=partes_falta(k) + 90 ' comienzan los + ligados
   EndIf
   ' x print #1,"k,partes_falta(k), FIGURA ", k, partes_falta(k), figura(partes_falta(k))
   ' x print #1,"POSICION reemplazada: ";posn
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = partes_falta(k)
   
   If carga=0 Then ''no hce nada en la carga
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = notaold
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn+1).dur = 182
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 181
   EndIf
'------zzz
   For noct = desde To hasta
     For i= 1 To 12 ' gracias a esto anda acordes¿?
       If i= nota And noct = estoyEnOctava Then
         Continue For
       Else    
       
         If Roll.trk((i +(noct -1) * 13),posn).nota = 0 Then
         '   ' x print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk((i +(noct -1) * 13),posn).nota = 181
            Roll.trk((i +(noct -1) * 13),posn).dur  = 0
         EndIf
       EndIf
     Next i
   Next noct
'-------   
   acumulado=acumulado+pesoDur(partes_falta(k))
   If Abs(d7 -acumulado) <= 10 Then
       acumulado = d7
   EndIf
   If acumulado = d7 Then
      compas(posn).Posi = posn
      nroCompas=nroCompas+1
      Compas(posn).nro = nroCompas

      ' x print #1, "Hay compas en posn ";posn
      acumulado=0
   posn=Posn+1
   posicion = posn
   MaxPos= Posicion
   '   If k= CantFalta Then
   '  EndIf

   ' x print #1,"acumulado falta: ";acumulado; " posn: ";posn
   
   ' x print #1,"d7 ";d7

      Exit For 
   Else
      compas(posn).Posi= 0
      Compas(posn).nro = -2
   
      ' x print #1,"No Hay compas todavia en posn ";posn
   EndIf

   posn=Posn+1
   posicion = posn
   MaxPos= Posicion
   '   If k= CantFalta Then
   '  EndIf

   ' x print #1,"acumulado falta: ";acumulado; " posn: ";posn
   ' x print #1,"d7 ";d7


  Next k
  
 ' If j=posn Then
 '  ' x print #1," j y posn son iguales "; j, posn
 ' Else
 '  ' x print #1," j y posn no son iguales, k "; j, posn,k
 '  If posn = cantFAlta + j  Then
    ''''  If carga=0 Then
 '   Compas(posn-1) = posn-1 ' hay compas ' ok pero da 2
 '   ' x print #1, "HAY COMPAS calcompas ultima figura ";partes_falta(k);figura(partes_falta(k))
 '   ' x print #1, "ACUMULADO = ";acumulado
 '    acumulado=0 
 '   ' x print #1,"acumulado cero";acumulado
 '   cantFalta=0 'jmg 30-03-21
    '  EndIf
 '  EndIf
 ' EndIf
  '  If carga=0 Then
  'Compas(posn-1) = posn-1 ' hay compas ' ok pero da 2 jmg 30-03-21
 ' ' x print #1, "HAY COMPAS calcompas ultima figura ";partes_falta(k);figura(partes_falta(k))
  
  '  EndIf
 End If

CantFalta=0
acumulado=0 ' se supone termino el compas sino habra que forzarlo.... 
 ' x print #1,"1-calcCompas 5 carga: ";carga
 If cantSobra > 0 And carga=0 Then
  ' x print #1,"REEMPLAZO CantSobra ";CantSobra
  For k=1 To CantSobra
   If k < CantSobra Then ' le agrega >, continuacion
      ' x print #1,"k ,partes_sobra(k),figura ";k;" ";partes_sobra(k),figura(partes_sobra(k))
      ' x print #1,"posicion reemplazada: ";posn
      If partes_sobra(k)>= 1 And partes_sobra(k) <=90 Then
         partes_sobra(k)=partes_sobra(k) + 90 ' empieza ligados +
      EndIf
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = partes_sobra(k)
   If carga=0 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = notaold
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn+1).dur = 182
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 181

   EndIf
'------zzz

   For noct = desde To hasta
     For i= 1 To 12 ' gracias a esto anda acordes¿?
       If i= nota And noct = estoyEnOctava Then
         Continue For
       Else    
       
         If Roll.trk((i +(noct -1) * 13),posn).nota = 0 Then
         '   ' x print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk((i +(noct -1) * 13),posn).nota = 181
            Roll.trk((i +(noct -1) * 13),posn).dur  = 0
         EndIf
       EndIf
     Next i
   Next noct
'-------   
   
   
   ' x print #1,"ERROR==> ACUMULADO: " ;acumulado
   ' x print #1, "vlor buscdo de k: ";k
   ' x print #1,"partes_sobra(k) ";partes_sobra(k)
   acumulado=acumulado + pesoDur(partes_sobra(k)) ' para el siguiente compas..¿?.
   ' x print #1,"ERROR==> ACUMULADO: " ;acumulado
   If k= CantSobra Then
    CantSobra=0
    Exit For
   EndIf
   posn=Posn+1 'jmg 30-03-21
   posicion = posn
   MaxPos= Posicion

  Next k
  ' x print #1,"1-calcCompas 6"
  
 EndIf
 cantSobra=0
 
 Erase partes_falta
 Erase partes_sobra
  ' fin compases
 ' x print #1,"1-calcCompas 7 carga: ";carga
End Sub
' ======================================================================
Sub separarDur(ByRef j As Integer, ByRef DUR As Integer,partes() As Integer, ByRef cantPArtes As Integer, ByRef falta As Integer)

 ' S print #1,"1-Separadur "
 Dim As Integer lb,lu,k,tengo,sobra, falta1,acumx
 ' S print #1,"EN SEPARADUR-------------------------------------------------"
 lb=LBound(partes)
 lu=UBound(partes)
 'dim integer ultimafigura= partes(CantPartes)

 falta1=falta
 ' S print #1,"FALTA RECIBIDO: ";FALTA1
 ' S print #1,;" DUR recibido: ";DUR;
 Dim As Integer desder, hastar

 ' S print #1,"pun: ";pun;" sil: ";sil;" mas: ";mas; " tres: ";tres
 ' S print #1,"incr: ";incr

 ' cuando hago la carga los k son de 1 a 64 no son mas de 1 a 8

 desder =  1 + incr
 hastar = 45 + incr

 For k=desder To hastar
  If (falta >= pesoDur(K) - 10) And  (falta <= pesoDur(K) +10)    Then
   partes(lb)=k
   Compas(j).Posi=j ' hay compas ESTE PRODUCI DOBLES !!!! JMG SI
   nroCompas = nroCompas+1
   Compas(j).nro = nroCompas

   ' S print #1,"falta encontrado DUR ";k;" figura:";figura(k); " parte:";lb
   CantPartes =lb
   acumulado=0
   falta=0
   falta1=0
   Exit Sub
  Else
   Compas(j).nro = 0 ' reseteo
   velocidades(j)
  EndIf
 Next k
 ' si sigue es porque falló no hay ninguna figura con esa duracion
 lb=lb-1
 acumx=0
 ' S print #1,"SEGUIMOS NO SE ENCONTRO FALTA  POSN J "; J
 ' S print #1,"absoluto de falta1 original "; falta1
 falta1=Abs(falta1)
 
 'If falta1 >= 15000000 Then

 ' ' S print #1,"Caso de falla falta1 > 15000000 revisar codigo"
 ' Exit Sub

 'Else
 'CAMBIO DE ORDEN TENIA 10 A 18, 19 A 27, 28 A 36, 1 A 9
Dim CUENTA As INTEGER 
 If k<=180 And lb=0  Then ' no se encontro
  Do
  CUENTA += 1
   For k=28+incr To 36+incr 
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k

   For k=19+incr To 27+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k


   For k=10+incr To 18+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k

  
   
   For k=1+incr To 9+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >1A8 ";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If

    If (falta1 >= pesoDur(k) - 10) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO = 1a8";lb;" figura:";figura(k); " parte:";lb
     Exit For
    EndIf
   Next k
   
   For k=37+incr To 45+incr ' tresillos
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >19a27 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =19a27 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k
   
   If acumx=falta Or lb > lu Then
    ' S print #1,"FALTA1 FIN EN ";lb; " figura "; figura(K) ;" ACUMX";acumx
    Exit Do
   EndIf
   If falta1 < 40000 Then
    ' S print #1,">> falta1 es H/2..NO DEBERIA DAR ESTO AHORA ";falta1
    Exit Do
   EndIf
   If CUENTA = 100 Then
      ' S print "FALTA: ";falta;" Falta1: "; falta1
      Close
      Exit Do
   EndIf   
  Loop
  ' S print #1,"ACUMX SEPARADUR ";acumx
 EndIf

 'EndIf

 CantPartes =lb

 ' S print #1, "CANT PARTES LB, CantPartes "; LB;" "; CantPartes
 ' S print #1,"ACUMX  ";ACUMX

End Sub

Sub mayorDurEnUnaPosicion (ByRef posn as integer)
 Print #1,"1-MayorDurEnUnaPosicion  "
 Dim As Integer i, j,mayor,ia ,valdur
 mayor=1
 j = posn
 For i= NB To NA
  valdur => Roll.trk(i,j).dur
  If  valdur >= 1 And valdur <=181 Then
   'Print #1, "DUR mayor";DUR, mayor
   If pesoDUR(valdur) > mayor  Then ' las duraciones cuando mas
    mayor=pesoDur(valdur)
    ' Print #1, " DUR MAYOR";DUR,mayor
   EndIf
   If i=NA Then

    For ia=1 To 180
     If mayor=pesoDur(ia) Then
      valdur=ia
     EndIf
    Next ia
   ' Print #1,"          J, valdur Mayor: "; J, valdur
    DUR=valdur
    calcCompas(j)
'    Print #1,"ABRIR compas(j) ";compas(j).Posi; " j :"; j

   EndIf
  EndIf
 Next i
 ''  DUR=0

 ' fin compases

End Sub
Sub organizaCompases()
' Print #1,"1-organiza Compases "
 Dim j As Integer
 For j = 1 To MAxPos
  MayorDurEnUnaPosicion (j)
 Next j
End Sub

Sub menugrafico(c0 As cairo_t Ptr,c As cairo_t Ptr ,Cantitems As Integer, text() As String  )
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
 cairo_set_font_size( c0, 24 )
 cairo_set_source_rgba( c0, 1, 1, 1, 1 )
 Dim i As Integer
 usamousex = usamousex + 200
 usamousex = usamousey + 100

 For i= 1 To CantItems
  cairo_move_to( c0, 100  , 100 + (i-1)*24 )
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
Sub ReCalCompas()
   nroCompas = 0

     carga=1 ' no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi,i2
     cont=0
     'Print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
     Dim As Integer lim1, lim2, grupo, mayorgrupo, noct
     For j = 1 To MaxPos -2
      'Print #2,"POSICION :";J
      'Print #3,"POSICION :";J
      cont=0
      For i= NB To NA
       valdur => Roll.trk(i,j).dur ' de 1 a 180
       ' al cargr tenemos 4 grupos de 16 de duraciones identicas que se repiten
       ' 1-16,17-32,33-48.49-64 , el indicemeindicaa cual grupo pertenece, y
       'en que rango buscar
  '     mayorgrupo=0
''DE LO QUE SIGUE DEBO EXTRCTAR MAYOR ENUNA POSCION Y COPIARLO
'' A ESA SUBRUTINA Y PONERLA EN NUCLEO
       semi => Roll.trk(i,j).nota  
      If valdur > 0 And valdur < 181 And semi >=1 And semi <=12 Then
       ' Print #1, "LECTURA DUR: ";valdur;" EN LA CARGA j: ";j;" i: "; i;" figura ";figura(valdur)
       ' Print #3, "nota: "; i; " posi: "; j; " dur: ";valdur
        Select Case valdur
         Case  1   To 45
          lim1=1:lim2=45:grupo=1
         Case  46  To  90
          lim1=46:lim2=90:grupo=2
         Case  91  To  135
          lim1=91:lim2=135:grupo=3
         Case  136  To  180
          lim1=136:lim2=180:grupo=4
'         Case 181
'          compas(j).Posi= 0
'          Compas(j).nro = 0
 
        '  lim1=0:lim2=0
        '  valdur=180
        '  mayorgrupo=0
        '  mayor=180:grupo=0
          Continue For,For
        End Select

        If  valdur >= lim1 And valdur <=lim2 And lim1 > 0 And lim2> 0 Then
        ' Print #1, "Valdur i ";valdur ; i ; " grupo ";grupo;" figura ";figura(valdur)
         ' Print #1,"i NB ";i,NB
         If  cont=0   Then 'mira duraciones
          mayor=pesoDur(valdur):mayorgrupo=grupo
          '       Print #1,"1er mayor "; mayor; " mayorgrupo";mayorgrupo
          cont=1
         EndIf
         'Print #1, "DUR mayor";DUR, mayor
         If pesoDUR(valdur) > mayor  Then ' las duraciones cuando mas
          mayor=pesoDur(valdur):mayorgrupo=grupo
          '        Print #1,"Siguientes mayor "; mayor
          ' Print #1, " DUR MAYOR";DUR,mayor
         EndIf
         '     Print #1,"> i= ";i; " NA= ";NA
        EndIf
      EndIf 'jmg OJO OJO
      Dim As Integer iades =(mayorgrupo -1)*45 + 1
      If mayorgrupo=0 Then '05-07-2021
        iades=0
      EndIf
      Dim As Integer iahas = mayorgrupo * 45 
      
       If i=NA  Then

        'Print #1,"i=NA i: ";i;" NA: ";NA;" Mayor ";mayor;" grupo:"; mayorgrupo
        For ia= iades To iahas ' tresillos es grupo 
         If mayor=pesoDur(ia) Then
          valdur=ia
        '     Print #1, "peso de mayor encontrado valdur ";valdur; "dur ";ia; " grupo ";mayorgrupo
          Exit For   
         EndIf
        Next ia
        DUR=valdur
      '  If DUR >180  Then ' <- esto nunca se ejecuta creo se filtra antes...
      '    Print #1,"ERROR NO SE PUEDE PASAR DUR"
      '    compas(j).Posi=0
      '    Compas(j).nro = 0
'
 '       Else
        Dim durm As Integer
         If DUR >181 or DUR=0 Then
            durm=181
         Else
            durm=DUR   
         EndIf
         
         if DUR<=180 Then
         ' Print #1,"ABRIR IN:  J: "; J; " DUR: "; valdur;" GRUPO: "; mayorgrupo;" figura ";figura(durm)
          calcCompas(j)
         If carga=1 Then
 '------zzz
            For noct = desde To hasta
              For i2= 1 To 12 ' gracias a esto anda acordes¿?
                 If Roll.trk((i2 +(noct -1) * 13),j).nota = 0 Then
                    Roll.trk((i2 +(noct -1) * 13),j).nota = 181
                    Roll.trk((i2 +(noct -1) * 13),j).dur  = 0
                 EndIf
              Next i2
            Next noct
'-------   
         EndIf   
          
          'Print #1,"ABRIR OUT: J: "; J; " compas(j): " ;compas(j).Posi;" figura ";figura(durm)
          valdur=0
          DUR=0
          mayor=0
         EndIf
        'Print #3,"==============fin posicion nro: "; j
       EndIf
       valdur=0:DUR=0
    
      Next i
      
     Next j
     DUR => 0
    '' curpos =>0
     carga=0  ' <======= control de Carga

End Sub

Sub CargaArchivo()
   
     Dim z (1,1 )  As dat
     Dim zLim (1,1) As dat
     
     Dim As String x,x1,x2,x3,x4,nombrea
     ''' no usar mas  dialogoText("Cargar Archivo")
      getfiles(file,myfilter,"open")
      nombrea=*file.lpstrFile
'  nombrea="DESAFIO-LIGA-ACORDES-EN-1-NOTA.ROLL" ' para debug sino no funciona
'  nombrea="test-acordes-iguales01.roll"
'  nombrea="DESAFIO-LIGA-ACORDE-3-O.roll"
       If nombrea = "" Then
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
   
     'Open "Trabajo_03.roll" For Binary Access Read As #2
    ' Open nombreArchivo +".roll" For Binary Access Read As #2
      Open nombre  For Binary Access Read As #2
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
     ' crgamos limites Roll de octavas
     Get #2, , zLim(1,1)
     desde=zLim(1,1).nota
     hasta=zLim(1,1).dur
'     NB => 1 + (desde-1) * 13   ' 27 para 3
'     NA => 12 + (hasta-1) * 13  ' 90 para  7
     CambiarDim()
     
     ' cargamos trabajo datos
     Dim Trabajo (NB To NA,1 To MaxPos) As dat
     Get #2, , Trabajo()
     ' movemos los datos a Roll
     ' -------------------------
     ReDim compas(1 To CantTicks)
     carga=1 ' no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
 '    Print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
     Dim As Integer lim1, lim2, grupo, mayorgrupo
     For j = 1 To MaxPos   -2 '11-07-2021
'      Print #2,"POSICION :";J
      'Print #3,"POSICION :";J
      For i= NB To NA
/'      
       If Trabajo(i,j).nota=255 Then
          Trabajo(i,j).nota=181
          Trabajo(i,j).dur=0
       EndIf
       If Trabajo(i,j).dur=255 Then
          Trabajo(i,j).dur=0
          Trabajo(i,j).nota=181
       EndIf   
       If Trabajo(i,j).nota=254 Then
          Trabajo(i,j).nota=181
          Trabajo(i,j).dur=0
       EndIf
       If Trabajo(i,j).dur=254 Then
          Trabajo(i,j).dur=0
          Trabajo(i,j).nota=181
       EndIf   
       If Trabajo(i,j).nota=253 Then
          Trabajo(i,j).nota=181
          Trabajo(i,j).dur=0
       EndIf
       If Trabajo(i,j).dur=253 Then
          Trabajo(i,j).dur=0
          Trabajo(i,j).nota=181
       EndIf   
'/   
       Roll.TRK(i,j)  => Trabajo (i,j) '
      Next i

     Next j
     Close 2

     DUR => 0
     curpos =>0
     carga=0
     ReCalCompas()
 '    Dim cmax As Integer
 '    For j = 1 To MaxPos -2
 '      nroCompas = compas(j).nro
 '      If nroCompas > cmax Then
 '         cmax=nroCompas
 '      EndIf   
 '    Next j
 '    nroCompas=cmax     
End Sub

Sub CambiarDim()
     NB => 1 + (desde-1) * 13   ' 27 para 3
     NA => 12 + (hasta-1) * 13  ' 90 para  7

     ReDim (Roll.trk ) (NB To NA, 1 To CantTicks)     
'no se puede reemplaar en el main del programa por cuestion de scope
' o sea la definicion debe hcerse en el scope del main para que funcione
' Esta redefinicion funciona y es necesaria  donde se use cairo
' sino parece que hay un choque entre cairo y redim y hace crash
' la redimension, sacandola del scope de cairo funciona 

End Sub

Function on_click_ok cdecl(byval handler as Ihandle ptr) as Long
	var dialog = IupGetHandle("dialog")
	var textbox = IupGetHandle("textbox")
	var label = IupGetHandle("label")

	nombreArchivo = *IupGetAttribute(textbox, IUP_VALUE)

'	Print "OK clicked! content of textbox: <" + nombreArchivo + ">"

	'IupSetStrAttribute(label, IUP_TITLE, _
'		"Thanks. You entered <" + nombreArchivo + ">." + NEWLINE + _
'		"val() result: " & val(nombreArchivo) & ". " + NEWLINE + _
'		"IupGetInt() result: " & IupGetInt(textbox, IUP_VALUE) & "." + NEWLINE + _
'		"IupGetFloat() result: " & IupGetFloat(textbox, IUP_VALUE) & "." _
'	)

	'' Recalculate the window layout. Since we changed the label text, it
	'' may require more space. This may require us to even increase the
	'' entire dialog's size to make room for the bigger label. According to
	'' IUP documentation doing this requires setting the SIZE attribute of
	'' the dialog, then call IupRefresh() on it. I'm not 100% sure whether
	'' this is the right/best way to do it.
	IupSetAttribute(dialog, IUP_SIZE, NULL)
	IupRefresh(dialog)
 Function = IUP_DEFAULT
''''movido
 IupClose() '<== movido aca asi al dar ok cierra el dialago
''''
end function
Sub dialogoText(cadena As string)

IupOpen(NULL, NULL)

'' Create a window with a text box, ok button, and a label
var textbox = IupText(NULL)
var okbutton = IupButton("OK", "on_click_ok")
var label = IupLabel("Tempo: Negras en 1 minuto.")
var vbox = IupVbox(textbox, okbutton, label, NULL)
var dialog = IupDialog(vbox)

'' Window title
IupSetAttribute(dialog, IUP_TITLE, cadena )

'' Make text box accept numbers only (instead of arbitrary text)
'' According to the IUP documentation, the IupText() control accepts a FILTER
'' attribute on Windows, which can have the values LOWERCASE, UPPERCASE or NUMBER.
'' Thus FLOAT does not seem to be valid!?
'IupSetAttribute(textbox, IUP_FILTER, "FLOAT")
'' However, NUMBER does not disallow letters to be entered on Windows XP either...
'IupSetAttribute(textbox, IUP_FILTER, "NUMBER")

'' Adjust layout a little bit
IupSetAttribute(textbox, IUP_EXPAND, "YES")
IupSetAttribute(okbutton, IUP_EXPAND, "YES")
IupSetAttribute(label, IUP_EXPAND, "YES")
IupSetAttribute(vbox, IUP_GAP, "4")
IupSetAttribute(vbox, IUP_ALIGNMENT, "ACENTER")
IupSetAttribute(vbox, IUP_MARGIN, "4x4")

'' Set callbacks and handles
IupSetFunction("on_click_ok", @on_click_ok)
IupSetHandle("dialog", dialog)
IupSetHandle("textbox", textbox)
IupSetHandle("label", label)

'' Show window and run main loop
IupShow(dialog)
' agregue pra dar el foco una funcion de win api
SetForegroundWindow(FindWindow(NULL, cadena))

IupMainLoop()


IupClose()


End Sub

Sub velocidades(j As integer)
    If TCompas="4/4" Then
      If acumulado <= 2500000 Then 'nollego a negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 2500000 And acumulado <= 5000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 5000000 And acumulado <= 7500000 Then 'nollego a 2 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 7500000 And acumulado <= 10000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      
    EndIf
    If TCompas="12/8" Then
      If acumulado <= 3750000 Then 'nollego a I*
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 3750000 And acumulado <= 7500000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 7500000 And acumulado <= 11250000 Then 'nollego a 2 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 11250000 And acumulado <= 15000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' semifuerte
      EndIf
     
    EndIf

End Sub 
Sub GrabarArchivo (nombre As String)
     ''Open "Trabajo_03.roll" For Binary Access write As #2
     ''Open nombreArchivo +".roll" For Binary Access write As #2
    Open nombre  For Binary Access write As #2
  ''''  Open "test-AAAAA.TXT" For Output    As #2
     Dim Trabajo (NB To NA, 1 To Maxpos) As dat
     Dim grabaPos (1,1)  As dat
     Dim grabaLim (1,1)  As dat 
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0,final=0
'eliminar columnas marcadas al grabar disco, 0 + X
'Print #1, "inicio MaxPos "; MaxPos
 For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i1,i2 ).nota=190 And Roll.trk(i1,i2 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i1,i2 ).dur=182 Then ' 26-06-2021 copiar final archivo
          final= 1 'Atrapa el final
       EndIf

       If Roll.trk(i1,i2 ).nota >=1 And Roll.trk(i1,i2 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA And haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar
       Else
          If borrocol = 1 Then
             res=1  
          EndIf  
       EndIf            
    Next i1
    i1=0  
    If borrocol = 0 Or res=0 Or final=1 Then 'copio columna no borro
       k=k+1
      For i1 = NB To NA
          Trabajo(i1,k)=Roll.trk(i1,i2 )
      Next i1
    EndIf
      
    borrocol=0:res=0:haynota=0:final=0
 Next i2
' MaxPos = k JMG 16-05-2021 TODAVIA NO SE COMO SE ACHICA LA SECUENCIA 
' AL GRABAR NO SOLO CON MAXPOS PARECE,,,ANDA BIEN PERO SI CONSERVO MAXPOS
' DEL MISMO VALOR EN VEZ DE K     


posn=k        

Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    Trabajo(i1,r1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    Trabajo(i1,r1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  




'Print #1, "final MaxPos "; MaxPos
'Print #1,"---------------------------------------- K= "; K
'  For i1 = NB To NA
'  Print #1,i1;"|";
'    For i2 = 1 To MaxPos
'       Print #1,Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
'    Next i2
'    Print #1,
'   Print #1,"------------------------------------------" 
'  Next i1
  
 '''Shell "NOTEPAD test-AAAAA.TXT" 
''''Exit sub
     Dim As Integer y1,y2,y3,y4, y5,y6 ' y5 e y6 son los limites NB y NA
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
     '-----------------------
     grabaLim(1,1).nota = desde
     grabaLim(1,1).dur  = hasta
     
     
     
     ' ------------------------------------
     ' para seguir poniendo notas nuevas
     ' hacemoslo mismo para l ultima not que se grabo que teng el 182
     ' esa es lapapa recorremos todas las dur de las notas en donde este el 182
     ' en dur tomamosla nota y esala grabamos en pb o ins
     ' cuadno la cargamos lo ahcemos en notaold !!!! y Vuala!!!
     ' -------------------------------------
'    Dim As Integer i,j,notafinal
'    For j = 1 To MaxPos
'     For i = NB To NA
'      If Roll.trk(i,posicion+1).dur = 182 Then
'       notafinal= Roll.trk(i,posicion).nota
       '        Print #1, "ENCONTRO NOTA FINAL ", notafinal
       '        Print "ENCONTRO NOTA FINAL ", notafinal
       ' esten la posiciond ela ultimanotapero grasoerror
       ' o seaque no tiene duracion solo el 182
       ' esta mal ...
'       Exit For
'      EndIf
'     Next i
'    Next j
'     grabaPos(1,1).pb = notafinal
 '    Print #1,"****))) nota final y dur final "; notafinal,Roll.trk(i,posicion+1).dur
     '****))) nota final y dur final  0   0
     ' no existe mas 182 no se graba enningun lado q raro
     ' Grabacion de Trabajo
     ' ------------------------------------

     '  Print #1,"MaxPos grabada en Trabajo ",MaxPos

     Put #2, ,grabaPos(1,1)
     Put #2, ,grabaLim(1,1)
     Put #2, ,Trabajo()
     Close 2
     While InKey <> "": Wend
     Sleep 150

End Sub

Sub GtkListBox()
' ==============================================
' Main GTK Sub
' ==============================================

	Dim As GtkWidget Ptr separator
	Dim As GtkWidget Ptr win
	Dim As GtkWidget Ptr vbox
	Dim As GtkWidget Ptr scrolled_window
	Dim As GtkWidget Ptr frame
	Dim As GtkWidget Ptr gtklist
	Dim As GtkWidget Ptr button
	Dim As GtkWidget Ptr list_item
	Dim As GList Ptr dlist 
	Dim As Integer i
	Dim As Zstring * 64 buffer
	
	' Initialize GTK (and subsequently GDK) 
	
	gtk_init (NULL, NULL)
	
	' Create a window to put all the widgets in
	' connect gtk_main_quit() to the "destroy" event of
	' the window to handle window manager close-window-events
	
	win = gtk_window_new (GTK_WINDOW_POPUP) '(GTK_WINDOW_TOPLEVEL)
	gtk_window_set_title (GTK_WINDOW (win), "GtkList Example")
	g_signal_connect (G_OBJECT (win), "destroy", G_CALLBACK (@gtk_main_quit), NULL)
	
	' Inside the window we need a box to arrange the widgets vertically 
	vbox=gtk_vbox_new (FALSE, 5)
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 5)
	gtk_container_add (GTK_CONTAINER (win), vbox)
	gtk_widget_show (vbox)
	
	' This is the scrolled window to put the List widget inside 
	scrolled_window = gtk_scrolled_window_new (NULL, NULL)
	gtk_widget_set_size_request (scrolled_window, 250, 150)
	gtk_container_add (GTK_CONTAINER (vbox), scrolled_window)
	gtk_widget_show (scrolled_window)
	
	' Create thekList widget.
	' Connect the sigh_print_selection() signal handler
	' function to the "selection_changed" signal of the List
	' to print out the selected items each time the selection
	' has changed */
	gtklist=gtk_list_new ()
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), gtklist)
	gtk_widget_show (gtklist)
	g_signal_connect (G_OBJECT (gtklist), "selection_changed", G_CALLBACK (@sigh_print_selection), NULL)
	
	' We create a "Prison" to put a list item in ;) 
	frame=gtk_frame_new ("Prison")
	gtk_widget_set_size_request (frame, 200, 50)
	gtk_container_set_border_width (GTK_CONTAINER (frame), 5)
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_OUT)
	gtk_container_add (GTK_CONTAINER (vbox), frame)
	gtk_widget_show (frame)
	
	' Connect the sigh_button_event() signal handler to the List
	' which will handle the "arresting" of list items
	g_signal_connect (G_OBJECT (gtklist), "button_release_event", G_CALLBACK (@sigh_button_event), frame)
	
	' Create a separator 
	separator=gtk_hseparator_new ()
	gtk_container_add (GTK_CONTAINER (vbox), separator)
	gtk_widget_show (separator)
	
	' Finally create a button and connect its "clicked" signal
	' to the destruction of the window 
	button=gtk_button_new_with_label ("Close")
	gtk_container_add (GTK_CONTAINER (vbox), button)
	gtk_widget_show (button)
	g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (@gtk_widget_destroy), win)
	
	'* Now we create 5 list items, each having its own
	' label and add them to the List using gtk_container_add()
	' Also we query the text string from the label and
	' associate it with the list_item_data_key for each list item
	Dim As Zstring Ptr tmp
	For i = 0 To 4
		Dim As GtkWidget Ptr label
		Dim As Zstring Ptr m_string
	
		sprintf(buffer, "ListItemContainer with Label #%d", i)
		label=gtk_label_new (buffer)
		list_item=gtk_list_item_new ()
		gtk_container_add (GTK_CONTAINER (list_item), label)
		gtk_widget_show (label)
		gtk_container_add (GTK_CONTAINER (gtklist), list_item)
		gtk_widget_show (list_item)
		gtk_label_get (GTK_LABEL (label), @m_string)
		g_object_set_data (G_OBJECT (list_item), @list_item_data_key, m_string)
	Next
	
	' Here, we are creating another 5 labels, this time
	' we use gtk_list_item_new_with_label() for the creation
	' we can't query the text string from the label because
	' we don't have the labels pointer and therefore
	' we just associate the list_item_data_key of each
	' list item with the same text string.
	' For adding of the list items we put them all into a doubly
	' linked list (GList), and then add them by a single call to
	' gtk_list_append_items().
	' Because we use g_list_prepend() to put the items into the
	' doubly linked list, their order will be descending (instead
	' of ascending when using g_list_append())
	
	dlist = NULL
	For  i = 5 To 9
		sprintf(buffer, "List Item with Label %d", i)
		list_item = gtk_list_item_new_with_label (buffer)
		dlist = g_list_prepend (dlist, list_item)
		gtk_widget_show (list_item)
		tmp = @"ListItem with integrated Label"
		g_object_set_data (G_OBJECT (list_item), @list_item_data_key, tmp)
	Next
	gtk_list_append_items (GTK_LIST (gtklist), dlist)
	
	' Finally we want to see the window, don't we? ;) 
	''gtk_window_set_position(window as GtkWindow Ptr, GTK_WIN_POS_CENTER_ON_PARENT ) 'byval position as GtkWindowPosition)
	gtk_window_set_position (win, GTK_WIN_POS_CENTER)
	gtk_widget_show (win)
	
	' Fire up the main event loop of gtk 
	gtk_main ()
	
	' We get here after gtk_main_quit() has been called which
	' happens if the main window gets destroyed

End Sub

' This is the signal handler that got connected to button
' press/release events of the List

Sub sigh_button_event Cdecl ( Byval gtklist As GtkWidget Ptr, Byval event As GdkEventButton Ptr, Byval frame As GtkWidget Ptr )

	' We only do something if the third (rightmost mouse button was released

	If event->Type = GDK_BUTTON_RELEASE And event->button = 3 Then
		Dim As GList Ptr dlist, free_list
		Dim As GtkWidget Ptr new_prisoner

		' Fetch the currently selected list item which
		' will be our next prisoner ;)
		dlist = GTK_LIST (gtklist)->selection
		If dlist Then
			new_prisoner = GTK_WIDGET (dlist->Data)
		Else
			new_prisoner = NULL
		End If

		' Look for already imprisoned list items, we
		' will put them back into the list.
		' Remember to free the doubly linked list that
		' gtk_container_children() returns
		dlist = gtk_container_children (GTK_CONTAINER (frame))
		free_list = dlist
		Do While (dlist) 
			Dim As GtkWidget Ptr list_item

			list_item = dlist->Data

			gtk_widget_reparent (list_item, gtklist)

			dlist = dlist->Next
		Loop
	
		g_list_free (free_list)
	
		' If we have a new prisoner, remove him from the
		' List and put him into the frame "Prison".
		' We need to unselect the item first.
		If (new_prisoner) Then
			Dim As GList static_dlist
	    
			static_dlist.data = new_prisoner
			static_dlist.next = NULL
			static_dlist.prev = NULL
	    
			gtk_list_unselect_child (GTK_LIST (gtklist), new_prisoner)
			gtk_widget_reparent (new_prisoner, frame)
		End If
	End If
End Sub


' This is the signal handler that gets called if List
' emits the "selection_changed" signal

Sub sigh_print_selection Cdecl ( Byval gtklist As GtkWidget Ptr, Byval func_data As gpointer )

	Dim As GList Ptr dlist

	' Fetch the doubly linked list of selected items
	' of the List, remember to treat this as read-only!

	dlist = GTK_LIST (gtklist)->selection
	
	' If there are no selected items there is nothing more
	' to do than just telling the user so

	If  dlist = NULL Then
		g_print (!"Selection cleared\n")
		Return
	End If
	
	' Ok, we got a selection and so we print it
	g_print ("The selection is a ")
    
	' Get the list item from the doubly linked list
	' and then query the data associated with list_item_data_key.
	' We then just print it */
	Do While (dlist) 
		Dim As Zstring Ptr  item_data_string

		item_data_string = g_object_get_data (G_OBJECT (dlist->Data), list_item_data_key)
		g_print("%s ", *item_data_string)
 	
		dlist = dlist->Next
	Loop
	g_print (!"\n")
	
End Sub
Sub  selport (ByVal tipo As Any Ptr)  ' list de ruso
Dim As hwnd haw,hwl
Dim mitipo As Integer 
mitipo=  CInt(tipo)
'' => desde acaecho con tool del ruso no anda muy bien
' como mierda selecciono???
     haw=OpenWindow("MIDI",100,50,400,400,WS_VISIBLE, WS_EX_TOPMOST )
'       WindowBounds(haw,100,50,300,400) para no redimensioonar
 '    CenterWindow(haw) 
'         ''UpdateInfoXserver()
''Var style = LVS_EX_FULLROWSELECT   
     hwl=  ListViewGadget(1,10,10,300,400,,,,32,LVS_SINGLESEL  )
     listports()

     If mitipo = 1 Then ' out  
       AddListViewColumn(1, "Salidas  MIDI",0,0,250)
       For aa As Integer=0 To UBound (listout) 
           AddListViewItem(1, listout(aa) +Str(aa),0,aa,0)
       Next

     EndIf
     If mitipo = 2 Then ' in entradas
       AddListViewColumn(1, "Entradas MIDI",0,0,250)
       For aa As Integer=0 To UBound (listin) 
           AddListViewItem(1, listin(aa) +Str(aa),0,aa,0)
       Next

     EndIf


       ButtonGadget(2,330,30,50,40," OK ")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
  '''''    SetForegroundWindow(FindWindow(NULL, "MIDI"))
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         'SetSelectListViewItem(1,3)
         menunew=0
         Do
'         MoveWindow(haw,100,50,300,400,TRUE)

         Var event= waitEvent

          If event=eventgadget Then
            If eventnumber()=2 Then
              menunew=0
             ''  Print GetItemListView()
              MessBox("","This is a TextGadget " + Str( GetItemListView()))
              portout = GetItemListView()
              Close_Window(haw)
              Exit Do 
'            Case 2
'               MessBox("","This is a TextGadget")
            End If
          EndIf 
          Sleep 5  
          
         Loop 
' SendMessage(GadgetID(1), EM_UNDO, 0, 0) 'undo the last edit

'SendMessage(GadgetID(1), EM_REDO, 0, 0) 'redo

'SendMessage(GadgetID(1), EM_GETSEL, @Min,@Max) 'COPY

'SendMessage(GadgetID(1), EM_SETSEL, @Min,@Max ) 'select

'SendMessage(GadgetID(1), EM_SETSEL, 0,-1) 'select all

'SendMessage(GadgetID(1), EM_REPLACESEL, 0,@TEXT) 'PASTE

  
'' fin ruso

End Sub

Sub ComboBox()

#Ifdef __FB_WIN32__
Var h = 80
#Else
Var h = 30
#EndIf
Dim As hwnd haw,hwl
haw= OpenWindow("",10,10,400,150, WS_VISIBLE,WS_EX_TOPMOST)
hwl=ComboBoxGadget(1,10,10,100,h)
AddComboBoxItem(1,"Hola cero",-1)
AddComboBoxItem(1,"Hola uno ",-1)
AddComboBoxItem(1,"Hola dos",-1)
ButtonGadget(2,150,10,220,20,"Select an item and click here")
 SetForegroundWindow(haw)
menunew=0
Do
   var event=WaitEvent()
   If event=eventclose Then End
   If event=eventgadget Then
      If eventnumber()=2 Then
         Print GetItemComboBox(1)
         MessBox("","This is a TextGadget " + Str( GetItemComboBox(1)))
         Close_Window(haw)
         Exit Do
      EndIf
   EndIf
   Sleep 5
Loop


End Sub
 
Sub grabaprueba()
Print #1,"MAxPos inicio ", MAxPos
     Dim Trabajo (NB To NA, 1 To Maxpos) As dat
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0
'eliminar columnas marcadas al grabar disco
 For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i1,i2 ).nota=254 And Roll.trk(i1,i2 ).dur=254 Then
          borrocol= 1 'Atrapa al menso un caso
          
       EndIf
       If Roll.trk(i1,i2 ).nota >=1 And Roll.trk(i1,i2 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
          Print #1, Roll.trk(i1,i2 ).nota;"*";Roll.trk(i1,i2 ).dur;"|";
       EndIf
       
       If i1= NA And haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar IMPRIME
       Else
          If borrocol = 1 Then
             res=1  
          EndIf  
       EndIf            
    Next i1
      
      If borrocol = 0 Or res=0 Then 'copio columna no borro
        k=k+1
        For i1 = NB To NA
          Trabajo(i1,k)=Roll.trk(i1,i2 )
          If Trabajo(i1,k).nota >=1 And Trabajo(i1,k).nota<=12 Then
          Print #1,"C:"; Trabajo(i1,k).nota;"*";Trabajo(i1,k).dur;"|";
          EndIf  
        Next i1
      
      Else
       MaxPos=MaxPos -1  
      EndIf
      
      borrocol=0:res=0:haynota=0
      Print #1,
 Next i2
'-------------------------
 Print #1,"MAxPos Final ", MAxPos
Print #1,"------------------------------------------"
For i1 = NB To NA
    For i2 = 1 To MaxPos
     If Trabajo(i1,i2).nota = 181 Then 
        Trabajo(i1,i2).nota = 0
     EndIf
     If Trabajo(i1,i2).dur = 181 Then 
        Trabajo(i1,i2).dur = 0
     EndIf
     If Trabajo(i1,i2).nota = 254 Then 
        Trabajo(i1,i2).nota = 0
     EndIf
     If Trabajo(i1,i2).dur = 254 Then 
        Trabajo(i1,i2).dur = 0
     EndIf
'     If Trabajo(i1,i2).nota >=1 And Trabajo(i1,i2).nota<=12 Then  
'       Print #1, Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
'     EndIf  
     Print #1, Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
    Next i2
    Print #1,
   Print #1, "------------------------------------------" 
Next i1
'shell"notepad midebug.txt"
Exit Sub 
'-------------------------

End Sub
Sub borrarColumnasMarcadas()
' toda columna en una posicion que tengasolo 190 en su nota y durcion
' seraeliminada y todaslas demas desplazadas a izquierda,.
Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0
    For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i1,i2 ).nota=190 And Roll.trk(i1,i2 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i1,i2 ).nota >=1 And Roll.trk(i1,i2 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA And haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar
       Else
          If borrocol = 1 Then
             res=1  
          EndIf  
       EndIf            
    Next i1
    i1=0  
    If borrocol = 0 Or res=0 Then 'copio columna no borro
       k=k+1
      For i1 = NB To NA
          Roll.trk(i1,k)=Roll.trk(i1,i2 )
      Next i1
    EndIf
      
    borrocol=0:res=0:haynota=0
 Next i2
' Dim k1 As Integer
' dejo el mismo largo o achico??? --achico
' For i2 = k+1 To MaxPos
'     For i1 = NB To NA
'     Roll.trk(i1,i2 ).nota=0
'     Roll.trk(i1,i2 ).dur=0
'     Roll.trk(i1,i2 ).vol=0
'     Roll.trk(i1,i2 ).pan=0
'     Roll.trk(i1,i2 ).inst=0
'     Next i1
' Next i2
 
'MaxPos=k+1
        
'Dim As Integer r1,i
'For i=NB To NA 
'  For r1= k+1 To MaxPos
'    Roll.trk(i,r1).nota=0
'    Roll.trk(i,r1).dur=0 
'  Next r1 
'Next i  
 
End SUB



