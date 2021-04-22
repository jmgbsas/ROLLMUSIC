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

 cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
 cairo_set_font_size (c, font)

 ' ======== NOTAS  =========
 ' cuando hago click en Edit el fondo cambia en main para edicion
If comEdit = TRUE  and estoyEnOctava = nro  Then
 If octavaEdicion=estoyEnOctava Then 
    cairo_set_source_rgba(c, 0, 1, 0.5, 1)
 Else 
    cairo_set_source_rgba(c, 1, 1, 1, 1)   
 EndIf   
Else
 
 cairo_set_source_rgba(c, 1, 1, 1, 1)
EndIf



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
 If cursorVert=0 And comEdit=TRUE Then
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
               Roll.trk (semitono + (nro-1) * 13, n ).dur = 109
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
               Roll.trk (semitono + (nro-1) * 13, n ).dur = 109
               If fijarEspacio=0 Then
                  espacio=0
               EndIf
            EndIf   
         EndIf
         If cursorVert=1 And Borrar=1 Then  ' BORRADO LIBRE
            If ((n - inicioDeLectura)=curpos)  Then
               Roll.trk (semitono + (nro-1) * 13, n ).dur = 109
               If fijarEspacio=0 Then
                  Borrar=0
               EndIf
            EndIf   
         EndIf
      EndIf    
   
        
    cairo_move_to(c, 81 + ic * anchofig , Penta_y + semitono * inc_Penta - 4)
    Dim As Integer indf
    indf= Roll.trk (semitono + (nro-1) * 13, n).dur
    If (indf >= 1 And indf <= 110)  Then
    Else
       indf=109
    EndIf ' t no puede quedar en un scope dsitinto se hce shared    

     t= figura(indf)

    cairo_show_text(c, t)
    ' LINEAS DE COMPAS
    'If indf> 0 And indf< 109 Then
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
 If ( Penta_y <= mousey) And ((Penta_y + 12 * inc_Penta) >= mousey)  Then
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


 If cursorVert=1 Or cursorVert= 2 Then  ' vertical comEdit=TRUE

  cairo_move_to  (c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
  cairo_rectangle(c, 81 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  , inc_Penta,inc_Penta)
  cairo_move_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y  )
  cairo_line_to  (c, 81 + (curpos)*anchofig + inc_Penta, Penta_y + (notacur-1) * inc_Penta  +inc_Penta )

 EndIf
 If cursorVert= 0 And cursorHori=0 Then
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
' If (cursorHori=1 Or cursorVert = 1) And borrar= 1 Then
 If  borrar= 1 Then
  cairo_set_source_rgba c, 1, 0, 0.2, 1
  Dim dursaved As Integer
  If notacur=0 Then
   notacur=notaOld
  EndIf
  'dursaved = Roll.trk(notacur + (nro-1) * 13, n + curpos).dur
  ' debo cambiar esa nota por (equivalente en silencio no va mas
  'para eso entra un silencio) se pone nota cero y dur 109
  Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 0
  Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = 0
  'Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = dursaved + 16
 ' If Roll.trk(notacur + (nro-1) * 13, n +curpos).dur > 109 Then
 '  Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = 109
 ' EndIf
  borrar = 0
 ''pro ahroa saco calcCompas(n) ''mayorDurEnUnaPosicion (n)
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
   '   vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
   Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2
   '   vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
   cambiadur = 0

  Else
   If pun = 1 And sil=0 Then
    Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 9
    '   vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
    Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2
    '   vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
    cambiadur = 0
    pun=0
   Else
    If sil = 1 And pun=0 Then
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR + 18
     '   vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
     Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2
     '   vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
     cambiadur = 0
     sil=0
    Else ' sil=1 and pun=1
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = DUR +27
     vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
     Roll.trk(notacur + (nro-1) * 13, n + curpos).nota = notacur  ' 5.7.2
     '      vroll= Roll.trk(notacur + (nro-1) * 13, n + curpos).nota
     cambiadur = 0
     sil=0:pun=0

    EndIf
    If DUR=109 Then
     Roll.trk(notacur + (nro-1) * 13, n + curpos).dur = 109
     vdur=Roll.trk(notacur + (nro-1) * 13, n +curpos).dur
     cambiadur = 0
     DUR=0
    EndIf
   EndIf
  EndIf
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
  ' Print #1, "regreso de movedato con indaux ",indaux
  ' param : posicion comienzo (fijo), indice incremental para el aux
  ' ,insert comando habilitado = 1
  'luego reemplazo
  ' Print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (nro-1) * 13),n
  Roll.trk(notacur + (nro-1) * 13, n +curpos).dur = DUR ' reemplazo con lo nuevo
  vdur=Roll.trk(notacur + (nro-1) * 13, n+curpos).dur
  ' SI LA NOTA  NO EXITIA LA agrega...osea sirve para acordes tambien
  'Print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (nro-1) * 13)

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
 ' ====> MODIFICAR
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
   If mousex > 279 And mousex< 329 And mousey < 50 Then
    cairo_set_source_rgba c, 0, 1, 0, 1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Pista]":cairo_show_text(c, t)
   ' ------------------------------------------
' 0.5 ===> [Reproducir]
   If mousex > 348 And mousex< 453 And mousey < 50  And play=0 Then
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
        tiempoPatron=60 'negras por minuto
          PlayRoll()
          play=1
        menuNew =0 
        
    ''    menuNew = 4 por hora sin elecciones
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Reproducir]":cairo_show_text(c, t)
   ' --------------------------
 '0.7  ===> [Opciones]  
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

' 0.8 ===> [Ayuda]
   If mousex > 582 And mousex< 643 And mousey < 50 Then
    cairo_set_source_rgba c, 0, 1, 0, 1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Ayuda] archivo, ayuda, edicion y Reproducir habilitados,version alpha":cairo_show_text(c, t)

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

   t = "      EDIT" + " " +  " UltimaPos: " + _
   Str(Maxpos) +" Posi: " + Str(posishow)+ " Posn: "+Str(posn)+ _
   " Font: " +Str(font) + " DUR:"+ Str(Dur) +" Nota:"+ Str(nota)+ _
   " EnOct:" + Str(estoyEnOctava) + " nE:"+Str(nE) + " nR:"+ Str(nR) + "Curpos: " +Str(curpos) + _
   "RollDur :" +Str(Roll.trk(nR,posishow+curpos).dur)+ "Rollnota: " + Str(Roll.trk(nR,posishow+curpos).nota) + _ 
   " Mx:" + Str(mousex) + " My:" + Str(mousey) '+ "Desde:" + Str(y1) + "Hasta:" +Str(y2)
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
   If mousex > 42 And mousex < 110  And mousey < 50 Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
    '  If MouseButtons And 1 Then
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
          tres=0:pun=0:sil=0:mas=0
       EndIf
      EndIf 
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = "      [NUEVO]":cairo_show_text(c,t)

   '===> 3.2 [ABRIR]

   If mousex > 134 And mousex < 190  And mousey < 50 And carga=0 Then ' <= ABRIR lee grabaroll.roll
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
   If mousex > 223 And mousex < 296  And mousey < 50 Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     '       Print #1, "Grabando a disco Roll GRABAR "
     Open "Trabajo_03.roll" For Binary Access write As #2

     Dim Trabajo (NB To NA, 1 To Maxpos) As dat
     Dim grabaPos (1,1)  As dat
     Dim grabaLim (1,1)  As dat 
     Dim As Integer i1, i2
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
     ' hacemoslo mismo para l ultim not que se grabo que teng el 66
     ' esa es lapapa recorremos todas las dur de las notas en donde este el 109
     ' endur tomamosla nota y esala grabamos en pb o ins
     ' cuadno la cargamos lo ahcemos en notaold !!!! y Vuala!!!
     ' -------------------------------------
     Dim As Integer i,j,notafinal
     For i = NB To NA
      If Roll.trk(i,posicion+1).dur = 110 Then
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
     Put #2, ,grabaLim(1,1)
     Put #2, ,Trabajo()
     Close 2
     While InKey <> "": Wend
     Sleep 150
    EndIf
   Else
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = " [GRABAR]": cairo_show_text(c,t) :t= ""
   '
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
  Case 5 'OPCIONES => OCTAVAS
   
' ===> 5.1 [OCTAVAS DESDE   - / + ] 
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
'  ----> DESDE
   If mousex > 144 And mousex< 196 And mousey < 50 Then ' DESDE LABEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    octavas=0
    redimensionar=0
   EndIf
' ---->  -
   If mousex >= 215 And mousex< 229 And mousey < 50  Then ' - LBEL
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
' ===> 5.2 [HASTA  - / + ]
   If mousex > 308 And mousex< 363 And mousey < 50 Then ' HASTA
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
     Print #1,"NB,NA,DESDE,HASTA ";NB,NA,desde,hasta
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
     Print #1, RTA

    EndIf
   Else
  '  cairo_set_source_rgba c, 1, 1, 1, 1
   
   EndIf
   t = "  [REDIMENSIONAR]<-Borra Datos " + RTA

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
   If Roll.trk(s,indpos).dur = 110 Then ' fin de datos de Roll
    RollAux.trk(s,indaux) = Roll.trk(s,indpos)
    insert= 4 ' fin de llenado de almacenamiento auxiliar
    '    Print #1,"sale moveresto encontro 110 fin, indaux :" ,indaux
    Exit Do
   EndIf
   RollAux.trk(s,indaux) = Roll.trk(s,indpos)
   If s=1 Then
    '  print #1, "luego de la copia a AUX del resto "
   EndIf
   If (Roll.trk(s,indpos).dur <>109  And Roll.trk(s,indpos).nota <> 109) _
    And (Roll.trk(s,indpos).dur > 0 Or Roll.trk(s,indpos).nota > 0)    Then
    '   print #1, "Roll.trk(";s,indpos;").nota ",Roll.trk(s,indpos).nota
    '   print #1, "Roll.trk(";s,indpos; ").dur ",Roll.trk(s,indpos).dur
   EndIf
   If (RollAux.trk(s,indaux).dur <> 109 And RollAux.trk(s,indaux).nota <> 109) _
    And (RollAux.trk(s,indaux).dur > 0 Or RollAux.trk(s,indaux).nota > 0)    Then
    '   print #1, "RollAux.trk(";s,indaux; ").nota ",RollAux.trk(s,indaux).nota
    '   print #1, "RollAux.trk(";s,indaux; ").dur ",RollAux.trk(s,indaux).dur
   EndIf

  Next s

  'posicion + curpos tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultimo reemplazo o insercion
 ' el 66 lo muevo??? sino debo hcer indaux = indaux -1
 ' indaux=indaux - 1 no le resto 1 copio el 66...el fin se corre

 Print #1, "StartInsert: " ,StartInsert '  ya se le sumo curpos
 If insert = 4 Then 'movemos de nuevo todo el aux completo a Roll desde
  ' laultimainsercion o sea Startindice +
  Print #1, "insert 4 movemos Aux a ROLL indaux maximos elementos en Aux: ",indaux
  t = StartInsert + cantInserciones ' xxx  OK

  Print #1, "posicion, posn "; posicion, posn
  For k = 1 To indaux ' todo el auxiliar SUMA ROLL REEMPLAZADO + NOREEMPLAZADO
   Print #1, "k auxiliar "; k
   For s=NB To NA  ' todos los semitonos 96
    Roll.trk(s,t) = RollAux.trk(s,k)
    If s=1 Then
     Print #1, "luego de la  copia A ROLL "
    EndIf
    If (Roll.trk(s,t).dur <> 109 And Roll.trk(s,t).nota <> 109) _
     And (Roll.trk(s,t).dur > 0 Or Roll.trk(s,t).nota > 0)    Then
     '  Print #1, "I) Roll.trk(";s,t;")nota,s,t : ", Roll.trk(s,t).nota, s, t
     '  Print #1, "I) Roll.trk(";s,t;")dur, s,t : ", Roll.trk(s,t).dur,s,t
    EndIf
    If (RollAux.trk(s,k).dur <> 109 And RollAux.trk(s,k).nota <> 109) _
     And (RollAux.trk(s,k).dur > 0 Or RollAux.trk(s,k).nota > 0)    Then
     '  Print #1, "II) RollAux.trk(";s,k;").dur ", RollAux.trk(s,k).dur
     '  Print #1, "II) RollAux.trk(";s,k;").nota ", RollAux.trk(s,k).nota
    EndIf
   Next s
   t=t+1
  Next k
  Print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
  '' posicion = MaxPos con posn es suficiente,sino se va al tramo final
  posn = MaxPos
  Print #1, "usamos MxPos pra posicion, posn "; posicion, posn
  '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
  Print #1 ,"final ciclo moveresto a AUX"

  For s = NB To NA
   For k=  StartInsert To MaxPos ' gracias a esto anda acordes
    If Roll.trk(s ,k).nota = 0 Then
     Roll.trk(s , k).nota = 109
    EndIf
   Next k
  Next s
  ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR
  ' nota en 109 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
  ' estos son los valores  StartInsert y cantInserciones.
  ' StartInsert es de Roll antesdela insercion
  Print #1, "==> valor de nota antes de CORRECCION: ",nota

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
     Roll.trk(s,k).nota = 109
     Roll.trk(s,k).dur  = 0
    EndIf
   Next s
  Next k
 EndIf
 insert=0
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
 Print #1,"========================================================="
 Print #1,"1-calcCompas j o posn RECIBIDA: "; j
 Dim As Integer k, i,cantFalta,CantSobra, tengo, falta,sobra
 
 If DUR=0 Then
  Compas(j).Posi=0
  Compas(j).nro = 0

  Print #1,"CALCOMPAS-0 DUR=0 COMPAS(J)=0 "
  Exit Sub
 EndIf
 If DUR=109 Then
  Compas(j).Posi=0
  Compas(j).nro = 0

  Print #1,"CALCOMPAS-0 DUR=109 COMPAS(J)=0 "
  Exit Sub
 EndIf

' estonunca seejecuta porque j es 4 nouncaentr con 1!!
 If j= 1 Then ' 128 es el maximo de ticks de un compas se agregan amspara terminared analizar elcompas
  'Erase (buffCompas_1) no usados pro ahor no son necesarios....
  'Erase (buffCompas_2)
  acumulado=0
  ''erase (compas) 'cada item es la posicion en donde
  Print #1,"J=1 comienzo calcCompas "
  jc=j
 EndIf
 Print #1,"1-calcCompas 2"
 If jc > 128 Then
  id=1
  ' procesar el resto que sobra luego de 129 en el buffer 2
 EndIf
 Dim As Integer partes_falta (1 To 100), partes_sobra(1 To 100)
 Print #1,"1-calcCompas 3, DUR: ";DUR; " j: "; j; " figura: ";figura(DUR)
  
 For k=1 To 108
  If DUR=k   Then
   acumulado =acumulado +pesoDur(k)
   Print #1,"acumulado / k / PEsoDur "; acumulado, k , pesoDur(k);" figura ";figura(DUR)
   If abs(d7 -acumulado) < 10 Then
      acumulado = d7
   EndIf
   If acumulado = d7 Then
    Compas(j).Posi = j
    nroCompas = nroCompas+1
    Compas(j).nro = nroCompas
    Print #1,"acumulado ";acumulado
    Print #1,"Compas(j) , j: ";Compas(j).Posi;" "; j

    acumulado=0
    jc=1
    Print #1,"1-calcCompas 3.1 exit sub acumulado=0 ";acumulado
    Exit Sub
   EndIf

   If acumulado < d7  And j > 0 Then
    Print #1, "j que hace cancelar ";j
    Compas(j).Posi =0
    Compas(j).nro = 0

    Print #1,"1-calcCompas 3.2 acumulado menor sigue exit sub, carga: ";carga
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
   Print #1,"1-calcCompas acumulado 3.3 >d7  ";acumulado; " carga: ";carga
   If acumulado > d7 And carga =0 Then
    Print #1,"acumulado > d7 :";acumulado
    tengo= acumulado - pesoDUR(DUR)
    Print #1,"tengo "; tengo
    falta=d7 - tengo
    Print #1,"falta ";falta
    sobra=acumulado -d7
    Print #1,"sobra "; sobra
    '       Dim ultimafigura As Integer=0
    separarDur(j,DUR, partes_falta(),cantFalta,falta)
    Print #1,"||||| CantFalta ";CantFalta
    If CantFalta > 100 Then
     Print #1, "ERROR CANTFALTA > 100 ";CantFalta
     EXIT Sub
    EndIf
    For z1 = 1 To CantFalta
     Print #1, " ";partes_falta(z1);" ";figura(partes_falta(z1));
    Next z1
    Print #1,
    separarDur(j,DUR, partes_sobra(),cantSobra,sobra)
    Print #1,"||||| CantSobra ";CantSobra
    If CantSobra > 100 Then
     Print #1, "ERROR CANTSOBRA > 100 ";CantSobra
     EXIT Sub
    EndIf

    For z1 = 1 To CantSobra
     Print #1, " ";partes_sobra(z1);" "; figura(partes_sobra(z1));
    Next z1
    Print #1,
    Print #1,"HAGO  DUR=0 BUENO.pero el sobra va a acum.."
    'preparo el acumulado de Falta ntes de reemplazar
  '  acumulado=pesoDur(partes_sobra(CantSobra))  'jmg 30-3-21
  '  Print #1,"acumulado sobra ";acumulado 
   EndIf
   Print #1,"1-calcCompas 3.4"
   '  EndIf
   Exit for
  EndIf
 Next k
 Print #1,"1-calcCompas 4  ingreso a REEMPLAZO EN ROLL de las durciones y notas"

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
  Print #1,"Recordar posn empieza en 0 "
  Print #1,"REEMPLAZO Cantfalta ";CantFalta
  For k=1 To CantFalta '
   If partes_falta(k)>= 1 And partes_falta(k) <=108 Then
    partes_falta(k)=partes_falta(k) + 54
   EndIf
   Print #1,"k,partes_falta(k), FIGURA ", k, partes_falta(k), figura(partes_falta(k))
   Print #1,"POSICION reemplazada: ";posn
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = partes_falta(k)
   
   If carga=0 Then ''no hce nada en la carga
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = notaold
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn+1).dur = 110
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 109
   EndIf
   For i= 1 To 12 ' gracias a esto anda acordes
    If i<> nota Then
     If Roll.trk((i +(estoyEnOctava -1) * 13),posn).nota = 0 Then
      Roll.trk((i +(estoyEnOctava-1) * 13), posn).nota = 109
     EndIf
    EndIf
   Next i
   acumulado=acumulado+pesoDur(partes_falta(k))
   If Abs(d7 -acumulado) <= 10 Then
       acumulado = d7
   EndIf
   If acumulado = d7 Then
      compas(posn).Posi = posn
      nroCompas=nroCompas+1
      Compas(posn).nro = nroCompas

      Print #1, "Hay compas en posn ";posn
      acumulado=0
   posn=Posn+1
   posicion = posn
   MaxPos= Posicion
   '   If k= CantFalta Then
   '  EndIf

   Print #1,"acumulado falta: ";acumulado; " posn: ";posn
   
   Print #1,"d7 ";d7

      Exit For 
   Else
      compas(posn).Posi=0
      Compas(posn).nro = 0
   
      Print #1,"No Hay compas todavia en posn ";posn
   EndIf

   posn=Posn+1
   posicion = posn
   MaxPos= Posicion
   '   If k= CantFalta Then
   '  EndIf

   Print #1,"acumulado falta: ";acumulado; " posn: ";posn
   Print #1,"d7 ";d7


  Next k
  
 ' If j=posn Then
 '  Print #1," j y posn son iguales "; j, posn
 ' Else
 '  Print #1," j y posn no son iguales, k "; j, posn,k
 '  If posn = cantFAlta + j  Then
    ''''  If carga=0 Then
 '   Compas(posn-1) = posn-1 ' hay compas ' ok pero da 2
 '   Print #1, "HAY COMPAS calcompas ultima figura ";partes_falta(k);figura(partes_falta(k))
 '   Print #1, "ACUMULADO = ";acumulado
 '    acumulado=0 
 '   Print #1,"acumulado cero";acumulado
 '   cantFalta=0 'jmg 30-03-21
    '  EndIf
 '  EndIf
 ' EndIf
  '  If carga=0 Then
  'Compas(posn-1) = posn-1 ' hay compas ' ok pero da 2 jmg 30-03-21
 ' Print #1, "HAY COMPAS calcompas ultima figura ";partes_falta(k);figura(partes_falta(k))
  
  '  EndIf
 End If

CantFalta=0
acumulado=0 ' se supone termino el compas sino habra que forzarlo.... 
 Print #1,"1-calcCompas 5 carga: ";carga
 If cantSobra > 0 And carga=0 Then
  Print #1,"REEMPLAZO CantSobra ";CantSobra
  For k=1 To CantSobra
   If k < CantSobra Then ' le agrega >, continuacion
      Print #1,"k ,partes_sobra(k),figura ";k;" ";partes_sobra(k),figura(partes_sobra(k))
      Print #1,"posicion reemplazada: ";posn
      If partes_sobra(k)>= 1 And partes_sobra(k) <=108 Then
         partes_sobra(k)=partes_sobra(k) + 54
      EndIf
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn).dur = partes_sobra(k)
   If carga=0 Then
    Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota = notaold
   EndIf
   Roll.trk((nota +(estoyEnOctava -1) * 13),posn+1).dur = 110
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 109

   EndIf
   For i= 1 To 12 ' gracias a esto anda acordes
    If i<> nota Then
     If Roll.trk((i +(estoyEnOctava -1) * 13),posn).nota = 0 Then
      Roll.trk((i +(estoyEnOctava-1) * 13), posn).nota = 109
     EndIf
    EndIf
   Next
   Print #1,"ERROR==> ACUMULADO: " ;acumulado
   Print #1, "vlor buscdo de k: ";k
   Print #1,"partes_sobra(k) ";partes_sobra(k)
   acumulado=acumulado + pesoDur(partes_sobra(k)) ' para el siguiente compas..¿?.
   Print #1,"ERROR==> ACUMULADO: " ;acumulado
   If k= CantSobra Then
    CantSobra=0
    Exit For
   EndIf
   posn=Posn+1 'jmg 30-03-21
   posicion = posn
   MaxPos= Posicion

  Next k
  Print #1,"1-calcCompas 6"
  
 EndIf
 cantSobra=0
 
 Erase partes_falta
 Erase partes_sobra
  ' fin compases
 Print #1,"1-calcCompas 7 carga: ";carga
End Sub
' ======================================================================
Sub separarDur(ByRef j As Integer, ByRef DUR As Integer,partes() As Integer, ByRef cantPArtes As Integer, ByRef falta As Integer)

 Print #1,"1-Separadur "
 Dim As Integer lb,lu,k,tengo,sobra, falta1,acumx
 Print #1,"EN SEPARADUR-------------------------------------------------"
 lb=LBound(partes)
 lu=UBound(partes)
 'dim integer ultimafigura= partes(CantPartes)

 falta1=falta
 Print #1,"FALTA RECIBIDO: ";FALTA1
 Print #1,;" DUR recibido: ";DUR;
 Dim As Integer desde, hasta

 Print #1,"pun: ";pun;" sil: ";sil;" mas: ";mas; " tres: ";tres
 Print #1,"incr: ";incr

 ' cuando hago la carga los k son de 1 a 64 no son mas de 1 a 8

 desde =  1 + incr
 hasta = 27 + incr

 For k=desde To hasta
  If (falta >= pesoDur(K) - 10) And  (falta <= pesoDur(K) +10)    Then
   partes(lb)=k
   Compas(j).Posi=j ' hay compas ESTE PRODUCI DOBLES !!!! JMG SI
   nroCompas = nroCompas+1
   Compas(j).nro = nroCompas

   Print #1,"falta encontrado DUR ";k;" figura:";figura(k); " parte:";lb
   CantPartes =lb
   acumulado=0
   falta=0
   falta1=0
   Exit Sub
  EndIf
 Next k
 ' si sigue es porque falló no hay ninguna figura con esa duracion
 lb=lb-1
 acumx=0
 Print #1,"SEGUIMOS NO SE ENCONTRO FALTA  POSN J "; J
 Print #1,"absoluto de falta1 original "; falta1
 falta1=Abs(falta1)
 
 'If falta1 >= 15000000 Then

 ' Print #1,"Caso de falla falta1 > 15000000 revisar codigo"
 ' Exit Sub

 'Else
Dim CUENTA As INTEGER 
 If k<=108 And lb=0  Then ' no se encontro
  Do
  CUENTA += 1
   For k=10+incr To 18+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     Print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     Print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
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
     Print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
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
     Print #1,"FALTA1 ENCONTRO PEDAZO >1A8 ";lb; " figura:";figura(k); " parte:";lb
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
     Print #1,"FALTA1 ENCONTRO PEDAZO = 1a8";lb;" figura:";figura(k); " parte:";lb
     Exit For
    EndIf
   Next k
   For k=19+incr To 27+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     Print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     Print #1,"FALTA1 ENCONTRO PEDAZO >19a27 PArte;";lb; " figura:";figura(k); " parte:";lb
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
     Print #1,"FALTA1 ENCONTRO PEDAZO =19a27 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k
   
   If acumx=falta Or lb > lu Then
    Print #1,"FALTA1 FIN EN ";lb; " figura "; figura(K) ;" ACUMX";acumx
    Exit Do
   EndIf
   If falta1 < 40000 Then
    Print #1,">> falta1 es H/2..NO DEBERIA DAR ESTO AHORA ";falta1
    Exit Do
   EndIf
   If CUENTA = 100 Then
      Print "FALTA: ";falta;" Falta1: "; falta1
      Close
      Exit Do
   EndIf   
  Loop
  Print #1,"ACUMX SEPARADUR ";acumx
 EndIf

 'EndIf

 CantPartes =lb

 Print #1, "CANT PARTES LB, CantPartes "; LB;" "; CantPartes
 Print #1,"ACUMX  ";ACUMX

End Sub

Sub mayorDurEnUnaPosicion (ByRef posn as integer)
 Print #1,"1-MayorDurEnUnaPosicion  "
 Dim As Integer i, j,mayor,ia ,valdur
 mayor=1
 j = posn
 For i= NB To NA
  valdur => Roll.trk(i,j).dur
  If  valdur >= 1 And valdur <=109 Then
   'Print #1, "DUR mayor";DUR, mayor
   If pesoDUR(valdur) > mayor  Then ' las duraciones cuando mas
    mayor=pesoDur(valdur)
    ' Print #1, " DUR MAYOR";DUR,mayor
   EndIf
   If i=NA Then

    For ia=1 To 109
     If mayor=pesoDur(ia) Then
      valdur=ia
     EndIf
    Next ia
    Print #1,"          J, valdur Mayor: "; J, valdur
    DUR=valdur
    calcCompas(j)
    Print #1,"ABRIR compas(j) ";compas(j).Posi; " j :"; j

   EndIf
  EndIf
 Next i
 ''  DUR=0

 ' fin compases

End Sub
Sub organizaCompases()
 Print #1,"1-organiza Compases "
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
 cairo_set_font_size( c0, 30 )
 cairo_set_source_rgba( c0, 1, 1, 1, 1 )
 Dim i As Integer
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
Sub ReCalCompas()
   nroCompas = 0

     carga=1 ' no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
     Print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
     Dim As Integer lim1, lim2, grupo, mayorgrupo
     For j = 1 To MaxPos -2
      Print #2,"POSICION :";J
      Print #3,"POSICION :";J
      For i= NB To NA
       valdur => Roll.trk(i,j).dur
       ' al cargr tenemos 4 grupos de 16 de duraciones identicas que se repiten
       ' 1-16,17-32,33-48.49-64 , el indicemeindicaa cual grupo pertenece, y
       'en que rango buscar
  '     mayorgrupo=0
''DE LO QUE SIGUE DEBO EXTRCTAR MAYOR ENUNA POSCION Y COPIARLO
'' A ESA SUBRUTINA Y PONERLA EN NUCLEO
       semi => Roll.trk(i,j).nota  
       If valdur > 0 And valdur < 110 And semi >=1 And semi <=12 Then
        Print #1, "LECTURA DUR: ";valdur;" EN LA CARGA j: ";j;" i: "; i;" figura ";figura(valdur)
        Print #3, "nota: "; i; " posi: "; j; " dur: ";valdur
        Select Case valdur
         Case  1   To 27
          lim1=1:lim2=27:grupo=1
         Case  28  To  54
          lim1=28:lim2=54:grupo=2
         Case  55  To  81
          lim1=55:lim2=81:grupo=3
         Case  82  To  108
          lim1=82:lim2=108:grupo=4
         Case 109
          compas(j).Posi= 0
          Compas(j).nro = 0
 
        '  lim1=0:lim2=0
        '  valdur=108
        '  mayorgrupo=0
        '  mayor=108:grupo=0
          Continue For,For
        End Select

        If  valdur >= lim1 And valdur <=lim2 And lim1 > 0 And lim2> 0 Then
         Print #1, "Valdur i ";valdur ; i ; " grupo ";grupo;" figura ";figura(valdur)
         ' Print #1,"i NB ";i,NB
         If  cont=0   Then
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
      EndIf
      Dim As Integer iades =(mayorgrupo -1)*27 + 1
      Dim As Integer iahas = mayorgrupo * 27 
       If i=NA Then

        Print #1,"i=NA i: ";i;" NA: ";NA;" Mayor ";mayor;" grupo:"; mayorgrupo
        For ia= iades To iahas ' tresillos es grupo 36
         If mayor=pesoDur(ia) Then
          valdur=ia
             Print #1, "peso de mayor encontrado valdur ";valdur; "dur ";ia; " grupo ";mayorgrupo
          Exit For   
         EndIf
        Next ia
        DUR=valdur
        If DUR >108  Then ' <- esto nunca se ejecuta creo se filtra antes...
          Print #1,"ERROR NO SE PUEDE PASAR DUR"
          compas(j).Posi=0
          Compas(j).nro = 0

        Else
          Print #1,"ABRIR IN:  J: "; J; " DUR: "; valdur;" GRUPO: "; mayorgrupo;" figura ";figura(DUR)
          calcCompas(j)
          Print #1,"ABRIR OUT: J: "; J; " compas(j): " ;compas(j).Posi;" figura ";figura(DUR)
          valdur=0
          cont=0
          DUR=0
        EndIf
        Print #3,"==============fin posicion nro: "; j
       EndIf
       valdur=0:DUR=0:cont=0
       
      Next i

     Next j
     DUR => 0
     curpos =>0
     carga=0  ' <======= control de Carga

End Sub

Sub CargaArchivo()
   
     Dim z (1,1 )  As dat
     Dim zLim (1,1) As dat
     
     Dim As String x,x1,x2,x3,x4
     Open "Trabajo_03.roll" For Binary Access Read As #2
 
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
     Print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
     Dim As Integer lim1, lim2, grupo, mayorgrupo
     For j = 1 To MaxPos -2
      Print #2,"POSICION :";J
      Print #3,"POSICION :";J
      For i= NB To NA
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
