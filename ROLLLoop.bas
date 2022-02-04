#Include Once "crt/stdio.bi"

On  Error GoTo errorloopbas
''(c As cairo_t Ptr, ByRef nro As Integer, ByRef octava As Integer Ptr, InicioDeLectura As Integer)
Sub creaPenta (c As cairo_t Ptr, Roll as inst  )
'Dim octava As Integer Ptr
'*po va desde hasta -1 haci aabajo 
indEscala=1 ' inicializamos la guiade escalas a la 1era 
Dim As String t2="",t3="",t4=""

 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
 Dim As Integer semitono,n, ic,indf,indfa,indfb
 Dim As Integer verticalEnOctavaVacia
 Dim As Integer notac, aconro, grado
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
'If COMEDIT = TRUE  and estoyEnOctava = nro  Then
' If octavaEdicion=estoyEnOctava Then 
'    cairo_set_source_rgba(c, 0, 1, 0.5, 1)
' Else 
'    cairo_set_source_rgba(c, 1, 1, 1, 1)   
' EndIf   
'Else
' 
' cairo_set_source_rgba(c, 1, 1, 1, 1)
'EndIf
' --preubas de encabezado donde pondremos mas informaicon y tal vez menues botoes etc

 Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1)
'--------------------------
 t=" ESCALA: "+ UCase(tipoescala_inicial) + " [" +cadenaes_inicial +"] I="+Str(tiempoPatron) + "Factor "+ Str(FactortiempoPatron) + " Compas=" +TCompas  

  cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - inc_Penta)
  cairo_show_text(c, t)
  t= ""

  
' t=" BordeSupRoll="+ Str(BordeSupRoll)  
'  cairo_move_to(c, 0, BordeSupRoll )
'  cairo_show_text(c, t)
'  t= ""
'--------------------------

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
 'If COMEDIT= TRUE Then
 'Dim As Integer delta
 'delta=NroCol
 If cursorVert=0 And COMEDIT=TRUE  Then
    If posicion < 30  Then
      posishow=  1 ''curpos ' decia 1
  ' valla tatlmente al inicio veremos si es aca jmgjmg
    Else
        posishow = posicion - 20
    EndIf
 Else
   If posicion=0 Then ' para TAB de track nuevo  
      posishow=1 
   Else
      posishow = posicion
   EndIf  
 EndIf

 Dim As Integer lugar=0, sitio
 lugarOld=Penta_y



 For semitono = 0 To 11
   If COMEDIT = TRUE  and estoyEnOctava = *po  Then ''+ 1  Then
     If octavaEdicion=estoyEnOctava Then
        cairo_set_source_rgba(c, 0, 1, 0.5, 1)
     Else 
        cairo_set_source_rgba(c, 1, 1, 1, 1)   
     EndIf   
   Else
 
       cairo_set_source_rgba(c, 1, 1, 1, 1)
   EndIf
  font= font - 2 ' achicamos notas giuas
''  t = NotasGuia(semitono) + Str(*octava) + "_[" 
'' cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6)
  If alteracion="bem" Then
     t = NotasGuia2(semitono) + Str(*po -1) + "_["
  Else 
    t = NotasGuia(semitono) + Str(*po -1) + "_["
  EndIf
  cairo_move_to(c, 0, Penta_y + (semitono+1) * inc_Penta- 6)
  cairo_show_text(c, t)
  t= ""
  ic=0 'indice cursor 'donde se dibujara la duracion
  n=0:indf=0:indfa=0:indfb=0
  font= font + 2
  
  For n = posishow To posishow + NroCol

' =======> deteccion escalas auxiliares y acordes
    indfb = Roll.trk (n, 12 + (*po-1) * 13).dur
    indfa= Roll.trk (n, 12 + (*po-1) * 13).pb ' 26-01-2022
    
    t="": t2="":t3="":t4=""
    
' <====== CIFRADO ACORDE---
    If indfa=201 And nVerCifradoAcordes=3 Then  
       cairo_set_source_rgba(c, 1, 1, 1, 1)
       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y)

        verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde
        notac=CInt(Roll.trk(n,verticalEnOctavaVacia ).nota) 'Rollnota
     
       aconro=CInt(Roll.trk(n,verticalEnOctavaVacia ).dur) 'acordenro
       If aconro >=1 And aconro <=43 Then  ' por ahroa tenemo 43
         t4=RTrim(ClaseAcorde(aconro).clase)
       EndIf  
       If 13-notac >=1 And 13-notac <=12 Then
         
         t3=RTrim(NotasEscala(13-notac)) ' C/ 
           grado = BuscarGrado(t3) ' 4 en escala G
         t3=t3+t4  
          ' Print #1,"grado ",grado
         If ClaseAcorde(aconro).tipo >1  Then
           If ClaseAcorde(aconro).tipo -1 + grado > 0 Then
            t3=t3+notas_esc_inicial(ClaseAcorde(aconro).tipo -1 + grado)
           ' Print #1,"t3=t3+notas_esc_inicial(ClaseAcorde(aconro).tipo -1 + grado) ",t3
           EndIf 
          '  Print #1,"ClaseAcorde(aconro).tipo ",ClaseAcorde(aconro).tipo
            
          '  Print #1,"ClaseAcorde(aconro).tipo+grado-1 ",ClaseAcorde(aconro).tipo+grado-1
          '  Print #1,"notas_esc_inicial(ClaseAcorde(aconro).tipo+grado-1) ",notas_esc_inicial(ClaseAcorde(aconro).tipo+grado-1) 
         EndIf
       EndIf
       t4=""
       indfa=0
    Else
      t3="":t4=""
      indfa=0
    EndIf
    
  '  If t3 >"" Then
      cairo_show_text(c, t3)
   '   cairo_stroke(c)
      t3=""
'    EndIf
   ' cairo_stroke(c)
    'indf=0:indfa=0


    If indfb = 200 And nVerEscalasAuxiliares=3 Then ' escalas auxiliares o alternativas
   ''' Print #1,"ENTROA VER ESCAL AAUXILIAR"
       cairo_set_source_rgba(c, 0, 1, 0, 1) 
       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y)
       cairo_line_to(c,gap1 + (ic ) *anchofig , Penta_y + 13.5 * inc_Penta )
       notaescala=CInt( Roll.trk(n, 12  + (*po -1) * 13).vol)
       If Roll.trk(n, 12  + (*po -1) * 13).pan =3 Then
          t2= NotasEscala(CInt( notaescala ))
       EndIf
       If  Roll.trk(n, 12  + (*po -1) * 13).pan =2 Then
          t2= NotasEscala2(CInt( notaescala ))
       EndIf
'           Print #1, "NOTAESCALA ",t2
           ' 11-01-2022
       tipoescala=CInt(Roll.trk(n, 12  + (*po -1) * 13).inst)
'           Print #1," tipoescala ",tipoescala
       armarescala cadenaes,tipoescala, notaescala, alteracion,0

           ' fin 11-01-2022
       t2=t2+" "+ escala(tipoescala).nombre + " "+cadenaes

       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y + 13 * inc_Penta ) '26-01
       indfb=0
       
    Else
      t2=""
    indfb=0
    EndIf

  
' t no puede quedar en un scope dsitinto se hace shared    
       ' apenas usas t2 o t3 hay que borrarlas sino se pudre todo raro
  '  If t2 >"" Then
      cairo_show_text(c, t2)
     ' cairo_stroke(c)
      t2=""
'    EndIf
   
'      cairo_show_text(c, t)
'      cairo_stroke(c)
      cairo_set_source_rgba(c, 1, 1, 1, 1)

' <=========fin escalas y acordes      

  
   If Roll.trk (n,11- semitono  + (*po -1) * 13 ).nota > 0 Or _
      Roll.trk (n,11- semitono +  (*po -1) * 13 ).dur > 0 Then
     ' print #1,"lugar ",11
      If COMEDIT=TRUE Then 
         If cursorVert=0 Then  
            If espacio = (semitono +1) Then
               Roll.trk (n, 11-semitono + (hasta -nro) * 13 ).dur = 181
               Print #1,"esta metiendo espacios?"
               If fijarEspacio=0 Then
                 espacio=0
               EndIf
            EndIf
         EndIf   
' un buen forma de borrar facil y rpido seria usando
' ((n - inicioDeLectura)=curpos) y moviendoelcursor derecha izquierda
' en ctrl-m borra todo de una!! implementarlo...        
         If cursorVert=1 Then  
            If (espacio = semitono +1 ) And ((n - inicioDeLectura)=curpos)  Then
               Roll.trk (n,11-semitono + (*po-1) * 13 ).dur = 181
               If fijarEspacio=0 Then
                  espacio=0
               EndIf
            EndIf   
         EndIf
     ' BORRADO LIBRE NO MARCA SOLO BLANCO habilita para usar nota=0    
    '     If cursorVert=1 And Borrar=1 Then  
    '        If ((n - inicioDeLectura)=curpos)  Then
    '           Roll.trk (semitono + (*po) * 13, n ).dur = 0
    '           Roll.trk (semitono + (*po) * 13, n ).nota = 0
    '           If fijarEspacio=0 Then
    '              Borrar=0
    '           EndIf
    '        EndIf   
    '     EndIf
      EndIf    
  
      cairo_move_to(c, gap1 + ic * anchofig , Penta_y + (semitono+1 ) * inc_Penta - 4)
    
  '  print #1,"lugar ",12
     indf= Roll.trk (n, 11- semitono + (*po-1) * 13).dur
  '  print #1,"lugar ",13
    If (indf >= 1 And indf <= 182)  Then ' 13-05-2021 11
    Else
       indf=181
    EndIf ' t no puede quedar en un scope dsitinto se hace shared    
     t= figura(indf)
'     cairo_show_text(c, t)
'     cairo_stroke(c)
'     cairo_set_source_rgba(c, 1, 1, 1, 1)
  

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

      cursor(c,n,nro,Roll)
      cairo_set_source_rgba(c, 1, 1, 1, 1)
    EndIf
    
' jmg 11-05-2021 1839 end 
    ' LINEAS DE COMPAS
    'If indf> 0 And indf< 181 Then
    '  print #1," CREAPENTA t  DUR: ";indf;" figura ";figura(indf); " semitono: "; semitono
    'EndIf
    ' puede ser aca CantTicks pro MaxPos
    If n >0 And n <= MaxPos  Then '''''CantTicks Then +1 para ver mas el fin
    'print #1,"lugar ",14
       If Compas(n).Posi = n  Then ' ahi hay un corte un nuevo compas
    ' print #1,"lugar ",15
          cairo_move_to(c,gap1 + (ic ) *anchofig +anchofig , Penta_y)
          cairo_line_to(c,gap1 + (ic ) *anchofig +anchofig, Penta_y + 12 * inc_Penta )
      '    print #1, "|";
          cairo_move_to(c,gap2 + (ic ) *anchofig +anchofig, Penta_y + 12.5 * inc_Penta )
          t=Str(Compas(n).nro)
          cairo_show_text(c,t)
      EndIf
     'print #1,"lugar ",16
      If n = pasozona1 Or n =pasoZona2 Then '26-06-2021
     'print #1,"lugar ",17
         cairo_move_to(c,gap3 + (ic ) *anchofig +anchofig, Penta_y + 13.5 * inc_Penta )
         t="("+ Str(n) + ")"
         cairo_show_text(c,t)
      EndIf


    EndIf

    cairo_move_to(c, gap1 + ic * anchofig , Penta_y + (semitono +1)* inc_Penta - 6)
    ic += 1
   Else ' revisar es extraño que funcione porque n esta dentro del tramo de arriba  Maxpos-1 < Maxpos zas
      If n= MaxPos-1 Then ' indicador de final de secuencia
         cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y)
         cairo_line_to(c,gap1 + (ic ) *anchofig , Penta_y + 12 * inc_Penta )
         t=Str("FIN")
         cairo_show_text(c,t)
         cairo_move_to(c,gap1 + (ic ) *anchofig +anchofig , Penta_y)
         cairo_move_to(c,gap2 + (ic ) *anchofig +anchofig, Penta_y + 12 * inc_Penta )
        
      EndIf  


  
'''    Exit For ' sale para saltear las nota=0, dur=0 ¿?, estaba mal creo debo mostrar todo
   EndIf
   'con ic * 40 es + 32 osea ic * 40 + 32
   
  Next n








  'Else
  'print #1, " n= ";n; " posn=";posn;" MaxPos=";MaxPos;" Posishow=";Posishow
  'EndIf
' incremento aca ? raro lo deberia incrementar al cargar notas
  'If n  > MaxPos Then 
  '  MaxPos = n  
' 
'  EndIf

  '--------TRAZADOI DE LINEAS VERTICALES GUIA DE COMPAS 1ERA VERSION

  '------------------------
  ' ENTRADAD DE NOTA NUEVA CONMOUSE: SE ELIGE LA DURACION CON LASTECLS 1 A 8
  'Y SE SELECCIONA CON EL MOUSE CLICKI ZQUIERDO,LA NOTA DESEADA.
  ' LAS OCTAVAS Y LA SNOTAS TODAS SE INVIERTE PORQUE EL VECTOR SE DIBUJA DE ARRIBA 
  ' HACIA ABAJO Y COMO SE DESEA QUE LAS MAS AGUDAS ESTEN ARRIBA Y LAS MAS GRAVES ABAJO
  ' TENEMOS QUE LA OCTAVA 1 ES LA 1ERA DE ARRIBA HACIA ABAJO PERO AL INVERTIRLA
  ' ES LA OCTAVA 9 , PEOR PARA LOS CALCULOS CONTAMOS DE ARRIBA HACI AABAJO DE 1 A 9
  ' ASI LA OCATVA 3 (-1,0,1) O SEA LA 1 EN PANTALLA B1,C1 EN RELIDAD ES LA OCTAVA
  ' 7..Y B1 TIENE UN VALOR nR=(13-0-2) + (9-7)*13=37 y la PianoNota es 35 
  ' restandole restar(37). Semitono va de arriba hacia abajo de 0 a 11 y
  ' nE es la nota de carga va de 1 a 12.
  lugar=Penta_y + (semitono +1) * inc_Penta
  cairo_move_to(c, 0, lugar )
  cairo_line_to(c, ANCHO - 1, lugar)
  cairo_stroke(c)
  If (mousey <= lugar) And (mousey >= lugarOld ) Then
   nE=semitono + 1 'semitono ahora va desde 0 a 11 usadopor entrada de tecladoy ahroa mouse
   nR=(11-semitono) + (*po -1 ) * 13 ''+ (desde -1)*13 ' indice de la nota en Roll , en algo será util.
   If *po=1 Then
      Print #8, "nR octava 1 ",nR
   EndIf
   PianoNota= nR - restar (nR)
''' desèjando nE = 11 -nR   +  (*po -1 ) * 13 + 1
  EndIf
  lugarOld=lugar
 ' ahora quiero que salga son semitono=12 asi lee las esclas y acordes
 ' o lo ponemos directamente en 12 salir del loop debe ser mas rapido? 
   If semitono =11 Then ' asi no suma 1 a semitono y no pasa a ser 12
     Exit For
   EndIf 
 Next semitono

 
' ----------------------------------------------------------- 
 ' PARA ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
 ' y hacer nota=semiotono 1 a 11 con el mouse...el resto es automtico...
' nro=hasta significa que ya dibujo la octava 9, luego puede seguir dibujando
' hacia abajo, la ayuda 
If *po = desde Then ' termino 9 octavas o la NA y ahora  + ayuda...
  cairo_set_font_size (c, font)
  'cairo_select_font_face (c, "Georgia",CAIRO_FONT_SLANT_NORMAL , CAIRO_FONT_WEIGHT_BOLD)
  t= "Flecha Abajo/Arriba o ruedita del mouse, scroll de las octavas en la ventana "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 15  )
  cairo_show_text(c, t)

  t = "F9/F10 achica/agranda el  font de las notas guia "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 16 )
  cairo_show_text(c, t)

  t = "Entrar Notas Click en [Edicion] en Menu, otro Click en Edit para modificar, otro Click en Edit deja de editar,ESC TERMINA LA APP"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 17 )
  cairo_show_text(c, t)

  t = "En modificacion o Edit: CTRL-M o CTRL-N pasa al MODO CURSOR para reemplazar insertar borrar notas, para volver a Edit Ctrl-P"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 18 )
  cairo_show_text(c, t)

  t = "-/+ achica/agranda el area y altura de las octavas dentro de la ventana "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 19 )
  cairo_show_text(c, t)

  t = "AvPag/RePag scroll de las octavas mas rapido "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 20 )
  cairo_show_text(c, t)
  
  t = "Ctrl-Click en [Reproducir] Play con scroll e iluminacion de notas"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 21 )
  cairo_show_text(c, t)

  t = "Barra Espacio: Play para Debug sin scroll, luego sera igual que ctrl-clik reproducir"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 22 )
  cairo_show_text(c, t)

  t = "Pulsar F1 Para Un Notepad Con Ayuda Preliminar mas detallada, puede estar incompleta y no ser lo ultimo"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 23 )
  cairo_show_text(c, t)

  t = "En el menu algunso funcionan con Ctrl-clik otros con click solamente"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 24 )
  cairo_show_text(c, t)

  t = "En modificacion o Edit, se pasa de una octava a otra para editarla, deslizando el mouse hasta el extremo izquierdo de la octava deseada, eso iluminara las lines de verde"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 25 )
  cairo_show_text(c, t)

  t = "la octava en edicion. (No hay vocales acentuadas, Cairo, la libreria grafica usada, no las maneja con font simples)"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 26 )
  cairo_show_text(c, t)

  t = "F2-F3 comprime - expande horizontalmente la secuencia, se puede editar tambien"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 27 )
  cairo_show_text(c, t)

  t = "En Edit entrar notas: 1) duracion 1 a 9, luego el nombre CDEFGAB o Ctrl+ nombre para sostenidos "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 28 )
  cairo_show_text(c, t)

  t = "En Lectura navegar a derecha o izquierda con Flecha horizontales, arriba abajo con Flechas Verticales"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 29 )
  cairo_show_text(c, t)

  t = "En Lectura seleccionar zona entre 2 posiciones,con Ctrl+Click en cualquier parte de la posicion deseada de la octava deseada, repetir hacia derecha para la 2da posicion "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 30 )
  cairo_show_text(c, t)

    
 End If

 ' si estoy en esta octava ...edicion solo para esa octava segun posicion
 ' del mouse automaticamete iluminar
 If ( Penta_y <= mousey) And ((Penta_y + 12 * inc_Penta) >= mousey)  Then
  ' estoy en una octava
  estoyEnOctava = *po  '<========== DETERMINACION DE OCTAVA DE TRABAJO
  EnOctava=1
  ' CURSOR
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  ' Or (cursorHori=2 And cursorVert=2 paa hbilitar ctrl-N
  If cursorHori=1 And cursorVert=1 Or play=1 Or playb=1  Then
      cursor(c,posicion,nro,Roll) ' posicion por n 26-10-2021 se arreglo curpos 
      ' se ilumina en posicion 0 
  EndIf
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  'PERO PIERDO EL COLOR MARILLO DEL CURSOR
  cairo_set_source_rgba c, 0, 0, 0, 1
  'cairo_set_line_width(c, 3)
 EndIf
 If ((Penta_y + 12 * inc_Penta) <= mousey) And ((Penta_y + 14 * inc_Penta) >= mousey) Then
  EnOctava = 0
  estoyEnOctava=90 
 EndIf
 *po = *po -1
 If *po = desde -1 Then  ' 22-09-2021 < estaba mal es = ej 4 a 8 -> 4-1=3 ya ejecuto la 3 me voy 
  *po = 99
  Exit Sub
 EndIf
 '           ================  MENUES CONTEXTUALES GRAFICOS PARA MOUSE ==================
 If ayudaModif=TRUE  And COMEDIT=TRUE Then
  If  (cursorVert = 1 Or  cursorHori = 1 ) Then
   'print #1,".............SUBRUTINA........................................."
   'print #1,"(9) ESTADO MUESTRA MENU COMANDOS"
   'print #1,"ayudaModif=TRUE  And COMEDIT=TRUE And (cursorVert = 1 or  cursorHori = 1 )"
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
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
     '  print #1,"BLANQUEAR BORRAR SIN ELIMINAR"
     Exit Sub
    EndIf

    If menuMouse = 1 Then
     cairo_move_to( c, usamousex, usamousey -80 )
     cairo_text_extents( c, text1, @extents )
     cairo_show_text( c, text1 )
     menuMouse = 2
     '   print #1,"INSERTAR"
     Exit Sub
    EndIf
    If menuMouse = 2 Then
     cairo_move_to( c, usamousex, usamousey -50 )
     cairo_text_extents( c, text2, @extents )
     cairo_show_text( c, text2 )
     menuMouse = 3
     '  print #1,"FIN INSERTAR"
    EndIf
    If menuMouse = 3 Then
     cairo_move_to( c, usamousex, usamousey -20 )
     cairo_text_extents( c, text3, @extents )
     cairo_show_text( c, text3 )
     menuMouse = 0
     '  print #1,"MODIFICAR"
    EndIf

   EndIf
   If savemousex=0 Or savemousey=0 Then
    savemousex=mousex
    savemousey=mousey
   EndIf

  EndIf
 EndIf
 If ayudaNuevaNota=TRUE  And COMEDIT=TRUE And vuelta=FALSE Then
  ' SI SE SELECCIONA INSERTAR O MODIFICAR Y DUR=0 THEN IMPRIMIR
  ' "ELIJA ANTES UNA DURACION"
  'print #1,".............SUBRUTINA........................................."
  'print #1,"(10) ESTADO SELECCIONAR DURACION O CTRL-P/cTRL-p"
  'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

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



Sub barrePenta (c As cairo_t Ptr, Roll as inst  )
'------------------


  For i = desde To hasta 
    nro = i 
  ' si ahce falta ejecutar mas de un Penta podremos usar threads
  ' asi funciona mejor o no? igual debe esperar a que termine el thread
  ScreenSync  
   creaPenta (c, Roll )
  If *po = 99 Then
     *po = hasta -1 ' 9 po ejemplo
     Exit For
  EndIf
  cairo_stroke(c)
 
  Next
  

  
End Sub


'Roll Main Loop ACA NO APARECE EL VECTOR DE ROLL 


sub  RollLoop (ByRef param As pasa) ' (c As cairo_t Ptr, Roll As inst)
Dim As Integer ubiroll,ubirtk,encancion
 c=param.c
 Roll=param.Roll
 ubiroll=param.ubiroll 
 ubirtk=param.ubirtk
 encancion=param.encancion
 abrirRoll=2
 ALTO=param.alto
 ANCHO=param.ancho
 

 print #1,"ubirtk ",ubirtk
 print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
 print #1,"posicion ", posicion
 Print #1,"ancho, alto", ANCHO, ALTO
 Print #1, "EN ROLLLOOP cargaCancion DEBE SER 1 EN INICIO ",cargaCancion
 '    If hwnd =0 Then   ,GFX_WINDOWED
     ScreenControl  SET_DRIVER_NAME, "GDI"
     If usarmarco= 3 then
        ScreenRes ANCHO, ALTO , 32,1 , GFX_HIGH_PRIORITY
     Else
        ScreenRes ANCHO, ALTO, 32,1 , GFX_NO_FRAME Or GFX_HIGH_PRIORITY
     EndIf
     print #1,"param.titulo ",param.titulo
     WindowTitle param.titulo
     ScreenControl GET_WINDOW_POS, x0, y0
     ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     hwnd = Cast(hwnd,IhWnd)
  ' datos recibidos   
  print #1,"datos recibidos en rooloop nombre ", nombre
  print #1,"datos recibidos en rooloop desde,hasta ", desde, hasta
  print #1,"datos recibidos en rooloop *po ", *po 
  print #1, "ubound roll.trk 2 ",ubound(roll.trk,2)
  '  End If
'Dim Roll As inst
' @Roll(1) = *pRoll  
Dim As Integer pid = GetCurrentProcessId()' , pid_parent = 0
print #1 ,"pid", pid
Var surface = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, ALTO, stride)
 c = cairo_create(surface)
Var surf2 = cairo_image_surface_create_for_data(ScreenPtr(), CAIRO_FORMAT_ARGB32, ANCHO, 50, stride)
 cm = cairo_create(surf2)
/' AL CARGAR OCTAVAS MENOS QUE 1 9 SEGUIMOS EL DESDE E INCREMENTAMOS O DECEMENTAMON INC_pENTA
1 A 2  64 UP
2 A 3 54 UP
3 A 4 44 UP
4 A 5 30 UP
5 A 6 15 UP
6 A 7  0 UP
7 A 8  6  DOWN
8 A 9  9 DOWN
'/

inc_Penta = Int((ALTO -1) /40) - deltaip

'llena la surface con nro_penta
'nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)

'Print nro_penta
Select Case desde
    Case 1
      BordeSupRoll = BordeSupRoll + 64 * inc_Penta
    Case 2
      BordeSupRoll = BordeSupRoll + 54 * inc_Penta    
    Case 3
      BordeSupRoll = BordeSupRoll + 44 * inc_Penta
    Case 4
      BordeSupRoll = BordeSupRoll + 30 * inc_Penta
    Case 5
      BordeSupRoll = BordeSupRoll + 15 * inc_Penta
    Case 6
    Case 7
      BordeSupRoll = BordeSupRoll - 15 * inc_Penta    
    Case 8
      BordeSupRoll = BordeSupRoll - 30 * inc_Penta
    Case 9
      BordeSupRoll = BordeSupRoll - 44 * inc_Penta ' 16-09-2021
     
End Select
'print #1,"BordeSupRoll ",BordeSupRoll
'no se usa en ningun lado nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)
print #1,"INSTANCIA ", instancia

Print #1,"call roolloop, tipoescala",tipoescala_inicial
Print #1,"call roolloop, notaescala",notaescala_inicial    
Print #1,"4 ROLLLOOP ENTRA A CARGAR PISTAS 1ERA VEZ cargaCancion ES 1 SI O SI ",cargaCancion
If ubiroll > 0 Then
   Print #1,"cargo archivo desde rollLoop
   nombre = titulos(0)
   Print #1,"nombre",nombre
   Print #1,"titulo(0) ",titulos(0)
    cargaArchivo (Roll,ubiroll)
   ROLLCARGADO=TRUE
   MenuNew=0
   ubiroll=0
  param.ubiroll=0
EndIf

 If ubirtk > 0 Then ' ya tengo el nommbre en linea de comando
    print #1,"carga track desde linea de comando,  nombre antes   ",titulos(0)
    nombre = titulos(0)
    CargarTrack (Track() , 0, ubirtk ) ' ntk=0
    If nombre > ""  Then '16-01-2022 crach si se cancela la carga
      print #1,"carga track veo nombre despues ", titulos(0)
      TrackaRoll (Track() , 0 , Roll) ' ntk=0
      print #1,"TrackaRollcarga rtk veo nombre ", titulos(0)
      RecalCompas (Roll)
      TRACKCARGADO=TRUE
      ubirtk=0
    Else
      TRACKCARGADO=FALSE
    EndIf

    print #1,"despues RecalCompas veo nombre ", titulos(0)
    MenuNew=0
    ubirtk=0
param.ubirtk=0
 EndIf

'If ubiroll > 0 Then
 '   CargaArchivo(Roll,ubiroll)
 '   ROLLCARGADO=TRUE
 '   MenuNew=0
 '   ubiroll=0
 'EndIf
 
' -----------------
Do
arranquedo1=Timer

edity1 = 10 ' botton Edit bordeSup
edity2 = 40 ' botton Edit bordeInf

'' Create a cairo drawing context, using the FB screen as surface.
'' l originalestba mal sizeof(integer ) es mu chico debe ser 4
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)

' ---------------------
If  cargaCancion=1 Then
' esta cargando cancion 
   'Locate 5,10
   'Print "CARGANDO ...PISTA Nro ", ntk
   'Sleep 100
 s5=2  
Else   

'--------------

ScreenLock()

'' Measure the text, used with SDL_RenderCopy() to draw it without scaling

' https://www.cairographics.org/tutorial/

''cairo_translate(c, 100, 100)

'If COMEDIT = TRUE  and octavaEdicion = estoyEnOctava Then
' cairo_set_source_rgba(c, 0.6, 0.6, 0.7, 1)
'Else
' cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
 cairo_set_source_rgba c, 0, 0, 0, 1
'EndIf
cairo_paint(c)
'cairo_set_line_cap(c, CAIRO_LINE_CAP_ROUND)
cairo_set_line_width(c, 1)
'cairo_set_source_rgba(c, 0, 0, 0, 1)


If s1 = 1 Then
 s1= 0
EndIf
If s2 = 1 Then
 s2=0
EndIf
If s6 = 1 Then
 s6=0
EndIf


inc_Penta = Int((ALTO -1) /40) - deltaip
'llena la surface con nro_penta no se usa
'nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)
'Print nro_penta

''''''''estos 3 comadnos con los de abajo son para scale o translate ---------

'  cairo_save (c) ' comentado
'  cairo_scale (c, escala, escala) ' comentado
'   cairo_translate (c , 0, translado)
' ----------------------------------------------------------------------------
cairo_set_antialias (c, CAIRO_ANTIALIAS_DEFAULT) 'hace mas lental cosa pero nomeafecta
' usemos 8 octavas y una para pie de pagina
' podemos reducir !!! y dejar ciertas octavas por instrumento
' cada isntrumetnotendriaundefinicion distintde roll con redim?
' no sepeude salvo dentro de un Type?
'
' desde=3 hasta=7
'  rango= hasta
' si viene de 7 a 9, nro va de 1 a 3  en relidad es de
'
' Creapenta no hace una nueva vuelta para la ayuda, la escribe directamente abajo
' de la ultima octava o vuelta o sea la 9.- solo que las octabas estn invertidas
' van de abajo hacia arriba 1 a 9 o 9 a 1 desde arriba a bajo por eo se usa
' *po para contener el control de la octava invertida
'
' toda la columna tendra la informacion de escala por ahora o sea a nivel nota puedo saber la escala desde
' esa posicion solo debo consultar o cambiar la posicion y con la misma coordenada vertical de la nota en cuestion
' sabre la escala actual, podriamso cambiar par aponer mas inforamcion en vez de repetir ...veremos
' falta tomar al informacion segun la posicion actual y aplicarla armando la escala
'---------------------------------------------------------------
'---------<======= ESCALAS AUXILIARES INSERCION ================>
' --------------------------------------------------------------
' insercion en ROLL de la escala en un aposicion dada 
If cambioescala=1 And pasoZona1 > 0 Then ' creamos una posicion con cambio de escala en pasoZona1
' la escala queda vigente hasta el proximo cambio, esto recuerda las notas que se deben usar o para crear acordes
Print #1,"cAMBIO ESCALA Na,NB, tipoescala, notaescala ", NA,NB,tipoescala_num,notaescala_num
' DEBO CARGAR EN LAS ZONAS DONDE NO HAY NOTAS O SEA ENTRE OCTAVAS TENGO LUGARES SIN USAR ERGO NO HACE FALTA
' BORRAR LOS DATOS PUEDO TENER UN CAMBIO DE ESCALA EN DATOS PREVIOS
' CUALE SSON ESAS POSICIONES SIN NOTAS..LAS QUE SON MULTIPLO DE 13 SUPONGO
 tipoescala=tipoescala_num
 notaescala=notaescala_num

Dim As Integer k,vacio
For K=desde To hasta -1 ' queda entre 2 octavas ,corregido  26-01-2022
 Print #1,"CARGO FOR !!! NOTA 30 DUR 200, k, pasozona1, NA ", "K=";K, pasoZona1,NA
   vacio= 12 +(k -1) * 13
   Print #1,"vacio,tipoescala ",vacio, tipoescala
   Print #1,"vacio,notaescala ",vacio, notaescala
     Roll.trk(pasozona1, vacio).inst=CUByte(tipoescala)
     Roll.trk(pasozona1, vacio).vol= CUByte(notaescala)

     Roll.trk(pasozona1,vacio ).nota = 30
     Roll.trk(pasozona1,vacio ).dur  = 200
'     Print #1,"Roll.trk(pasozona1,k ).nota ",Roll.trk(pasozona1,k ).nota, k
'     Print #1,"Roll.trk(pasozona1,k ).dur ",Roll.trk(pasozona1,k ).dur,k
     If  alteracion="sos" Then
         Roll.trk(pasozona1, vacio).pan = 3
     EndIf 
     If   alteracion="bem" Then
          Roll.trk(pasozona1, vacio).pan = 2 
     EndIf
 Next K  
' nota=30 , dur=200 indicara cambio de escala ÇÇÇ
   guiaEscala(indEscala).posicion=posicion
   cambioescala=0
   pasoZona1=0

' sigue en crea_penta donde al barer el roll va leyendo las escalas auxiliares
EndIf


 
  Dim ta As Any Ptr = ThreadCall barrePenta (c, Roll )
    ThreadWait ta


pubi=0

menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)

botones(hWnd, c ,cm, ANCHO,ALTO) ' este despues sinocrash
cairo_stroke(c)
cairo_stroke(cm) ' cm despues de c sino crash



ScreenUnLock()

EndIf


'' ---------------  LOOP 2 ---------------
Do 


'---------
'simulamos TAB para cargaCancion=1 cuadno recien se carga la cancion
If MultiKey(SC_TAB) And instancia=0 And CANCIONCARGADA Or cargaCancion=1 Then
   cargaCancion=0 ' para que no entre mas luego de cargada la cancion
   
   Erase mel_undo, undo_acorde, undo_kant_intervalos
   mel_undo_k=0: ig=0:cnt_acor=0
   ROLLCARGADO = FALSE
   print #1,"--TAB "
   nota=0
   dur=0
   print #1,"1- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos
   ntk = ntk + 1
   print #1,"2- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos  
   If ntk > 32 Then
     ntk=1 
     print #1,">32- 1- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos     
   EndIf
   nombre= titulos(ntk)
   If nombre> "" Then
     print #1,"--------------------------"
     print #1,"3-NTK nombre", ntk,nombre
     print #1,"3-NTK MAXPOS pmtk(ntk).maxpos  ", maxpos,pmTK(ntk).maxpos
     print #1,"--------------------------"
   EndIf  
' evita leer track vacios   
   If nombre=""  Then ' evita revisar track vacios
     Do While nombre=""
        ntk=ntk+1
        If ntk>32 Then
           ntk=1
           nombre= titulos(ntk)
  print #1,"4 - NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos    
           Exit Do
        EndIf
 
        nombre= titulos(ntk)
    Loop
     MaxPos=pmTk(ntk).MaxPos
     posn=pmTk(ntk).posn
     desde=pmTk(ntk).desde
     hasta=pmTk(ntk).hasta
     NB=pmTk(ntk).NB
     NA=pmTk(ntk).NA
     notaold = CInt(pmTk(ntk).notaold)
     CantTicks=pmTk(ntk).Ticks
' ajusto escala principal durante la conmutacion para cada track visualizado con TAB     
     notaescala_num_ini=CInt(pmTk(ntk).notaescala) '13-01-2022
     tipoescala_num_ini= CInt(pmTk(ntk).tipoescala) '13-01-2022
     cadenaes_inicial="" '13-01-2022
     armarescala cadenaes_inicial,tipoescala_num_ini,notaescala_num_ini,alteracion,1 '13-01-2022
' todavia no probado, escala principal para TAB en cada track testeat 13-01-2022     
' no he grabado las escalas auxiliares en lso Trackc todavia !! 13-01-2022 jjj     
  print #1,"5- MAXPOS final TAB " ,maxpos
EndIf   
'

  print #1, "6-NTK nombre", ntk,nombre  
  print #1, "6-NTK ntk,MAXPOS, pmtk(ntk).maxpos  ", ntk, maxpos,pmTK(ntk).maxpos
' copia track a Roll en memoria  
  Tracks (ntk , 1,Roll) ' track , nro,  Canal
  Sleep 100
 print #1,"7- instancia, maspos ",instancia, maxpos
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_M)  Then ' modificar con X o insertar con Insert y I
 cursorVert = 1
 cursorHori = 1
 agregarNota=0
 menuMouse = 0
 nota=0
 DUR=0

EndIf
If MultiKey(SC_CONTROL) And MultiKey(SC_N)  Then 'modificar con nombre de nota
 nota=0
 cursorVert = 2
 cursorHori = 2
 agregarNota= 1
 DUR=0
 
EndIf


If MultiKey(SC_CONTROL) And MultiKey(SC_P)   Then 'PARAR cursor MEJOR CON MOUSE ?
 cursorVert = 0
 cursorHori = 0
 agregarNota = 0
 menuMouse = 0
 '' notadur=0
EndIf

If MultiKey (sc_P) And (play=1 Or playb=1 )Then
  CONTROL1=1 ' DETIENE EL PLAY VEREMOS
  playloop=0
EndIf

If MultiKey(SC_PLUS) Then  '13 , ligadura
 mas=1
 Exit Do
EndIf

If  MultiKey(SC_KEYPADPLUS) Then '78

   Dim As Integer w,h
   h=ALTO
   ALTO = ALTO + inc_Penta
   If ALTO >= altoInicial - 1  Then
    ALTO = altoInicial  - 1
   EndIf
   MoveWindow( hWnd , X0, (Y0+ALTO-h)\2, ANCHO - mxold,ALTO, TRUE )

 Exit Do

EndIf

If  MultiKey(SC_MINUS)  Then
   h=ALTO
   ScreenControl GET_WINDOW_POS, x0, y0
   ALTO = ALTO - inc_Penta
   If ALTO <= ALTO * 0.3 Then
    ALTO =  ALTO * 0.3
   EndIf
   '    ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   '    Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0 , (Y0+h-ALTO)\2, ANCHO - mxold,ALTO, TRUE )

    Exit Do


 Exit Do

EndIf

If MultiKey(SC_CONTROL) Then 
 If MultiKey (SC_RIGHT) Then
    posicion=posicion + Maxpos/5
    If posicion > MaxPos Then
      posicion = MaxPos
    EndIf
    posishow=posicion
    
 EndIf    
EndIf

If MultiKey(SC_CONTROL) Then 
  If  MultiKey (SC_LEFT) Then
   posicion=posicion - Maxpos/5
   If posicion < 1 Then
      posicion = 1
   EndIf
   posishow=posicion
   
  EndIf
EndIf

' 03-02-2022 screen event me pone con su 80 trasponer=1 hace una asignacion !!!
If MultiKey(SC_DOWN) Then  ' el screenevent me pone trasponer en 1 la puta e.scancode = 80 Then  ' <===== SC_DOWN pulso
     If trasponer=1 And SelGrupoNota=0 Then
        print #1,"0 pulso down screenevent TRASPONER con multikey!"
       trasponerRoll ( -1,Roll,encancion)
       Exit Do
     EndIf 
     If trasponer=1 And SelGrupoNota=1 Then
       print #1,"1 pulso down screenevent TRASPONER"
       trasponerGrupo ( -1,Roll,encancion)
       Exit Do
     EndIf 
     
    If cursorVert=1 Or cursorVert=2 Then
     notacur = notacur + 1
     If notacur > 12 Then
      notacur=1
     EndIf
      Exit Do
    EndIf
    If cursorVert=0 Then 
       If s1=0 Then
          s1=1
        print #1,"pulso down screenevent"
        BordeSupRoll = BordeSupRoll -  inc_Penta
       EndIf
      If BordeSupRoll <= - AltoInicial * 2.8  Then
         BordeSupRoll =  - AltoInicial * 2.8
      EndIf
      Exit Do
    EndIf
    

EndIf

'   escala = escala - 0.1

 If  mouseY < 50  And MultiKey(SC_RIGHT)   Then ' <======== RIGHT
 ' seleccion de menu, mouse sobre cinta + teclas
      menuNro=menuNro+1
     If menuNro > 10 Then ' 28-08-2021 
       menuNro=0
       menuNew=0
     EndIf
     menuNew=menuNro
     Sleep 100
     Exit Do
 EndIf

  'kNroCol cantidad scroll de NrocOL)
 If  mouseY > 50 And MultiKey(SC_RIGHT)   Then ' <======== RIGHT
 
     If COMEDIT = FALSE Then
        posicion = posicion + 1
        kNroCol= Int(posicion/NroCol)
        If  (kNroCol > 0) And (posicion = NroCol * kNroCol) And (posicion < MaxPos)Then
           iniciodelectura = iniciodelectura +  NroCol
           If inicioDeLEctura > MaxPos Then
              inicioDeLEctura = inicioDeLEctura -NroCol
           EndIf
        EndIf
        If posicion > MaxPos -1  Then
           posicion = MaxPos -1
        EndIf
     Else
        curpos= curpos + 1 ' mueve cursor cuando Roll se detiene (posicion)
        If curpos > NroCol  Then
          curpos = NroCol
        EndIf
    '    If posicion + curpos > MaxPos -1  Then 'ojo esto jmg
    '       curpos = MaxPos -1 - posicion 
    '    EndIf

     EndIf
     Sleep 100
    Exit Do
    
 EndIf
'   escala = escala + 0.1
                '  <========== LEFT
 If  mouseY < 50 And MultiKey(SC_LEFT) Then  ' seleccion de menu
  menuNro=menuNro - 1
  menuNew=menuNro
  If menuNro < 0 Then
   menuNro=10  ' 28-08-2021
   menuNew=10
  EndIf
  Exit Do
 EndIf
 If  MultiKey(SC_LEFT) And mouseY > 50   Then

  'MOVER ROLL IZQUIERDA NO CURSOR
  If COMEDIT = FALSE Then
     Dim kNroCol As Integer ' cntidad de scroll de 66
     posicion = posicion - 1
     kNroCol= Int(posicion/NroCol)
     If  kNroCol > 0 And (posicion = NroCol*kNroCol)  Then
         iniciodelectura = iniciodelectura - NroCol
     EndIf
     If iniciodelectura < 0 Then
        iniciodelectura = 0
     EndIf
     If posicion < 1 Then
        posicion = 1
     EndIf
  Else
     curpos = curpos - 1 ' <=== MOVER CURSOR IZQ
     If curpos < 0 Then
        curpos = 0
     EndIf
  EndIf
   Sleep 100
    Exit Do
 EndIf


If MultiKey (SC_F2)  Then
' escala = escala - 0.01
   anchofig=anchofig - 5
   gap1= anchofig* 2315/1000
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) - 4 
   If  anchofig < 1 Then
       anchofig = 1
   EndIf    
   nanchofig=anchofig
   font=font -1
  Exit Do
EndIf

If MultiKey (SC_F3)  Then
   anchofig=anchofig + 5
   gap1= anchofig* 2315/1000 '81 default
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) - 4
   If  anchofig > 175 Then
       anchofig = 175
   EndIf    
   font=font+1  
   nanchofig=anchofig
 Exit Do
EndIf
'----------
' SIZE ANCHO F5
' F5 SCANCODE 63 , F6 64
If  MultiKey (SC_F5)   Then
 If COMEDIT = FALSE Then
'  escala = escala - 0.01
'  translado = translado - 100
 EndIf

 Exit Do

EndIf

If  MultiKey (SC_F6)  Then
 If COMEDIT = FALSE Then

 'escala = escala + 0.01
' translado = translado + 100
 EndIf

 Exit Do
EndIf



' PRUEBAS DE GRABACION DEL VECTOR ROLL es sencillo porque grabo todo
' o cargo todo,,
' https://www.freebasic.net/forum/viewtopic.php?f=2&t=26636&p=246435&hilit=array+load+save#p246435
' ===================================
If MultiKey (SC_F11) Then '  <========= Grabar  Roll Disco  F11
 'cada posicion tendre 48bits osea 6bytes..
 ' luego 12000 posiicones si estuviera todo completo serian 9216000 bytes
 ' y grabo..9mbytes, seria 1 Track,,295 mbytes para 32 tracks
 '  print #1, "Grabando a disco Roll F11 "
   Dim As String nombreg
   If nombre = "" Then
      getfiles(file,myfilter,"save")
      nombreg=*file.lpstrFile
      If nombreg = "" Then
         Exit Do
      Else
         nombre=nombreg   
      EndIf
   EndIf
   GrabarArchivo(0)
   
EndIf
' cargar Roll y MaxPos de disco

If MultiKey(SC_CONTROL) and MultiKey(SC_L)  Then ' <======== load Roll
  If carga=0 Then
   CargaArchivo(Roll,0)
  EndIf 
 
EndIf

If MultiKey(SC_ALT) and MultiKey(SC_L)  Then ' <======== playloop
  playloop=1 
EndIf
'---UNDO ACORDE - O BORRAR ACORDES, debemos borrar cifrado tambien
If MultiKey(SC_ALT)  And MultiKey(SC_BACKSPACE) And scan_alt=0  Then '<=== undo acorde
  'undo de acorde y/o melodia
  ' esto funciona en Roll hay qu ever que pasa con los tracks....pendiente jjj
  Dim As Integer ik=0,ij=0,im,pn=1,noti

' undo de acordes hasta 500 acordes de 12 notas c/u
   ig=cnt_acor
   If ig<>0 And undo_kant_intervalos(ig) > 0 And ig <= cnt_acor Then   
      Print #1,"ig= ",ig
      Print #1," undo_kant_intervalos(ig) ", undo_kant_intervalos(ig)
      Print #1,"cnt_acor",cnt_acor
' borrado de la informacion del cifrado de acorde en la octava mas alta no usada
      Dim As Integer n0,pnr      
      pnr=undo_acorde(ig,0).pn ' es el indice de Roll nR
      
      n0=restar(pnr)+1 ' seria la octava en donde se trabajó
' ergo con ese nro de octava entro a la ultima octava de arriba que no se usa
' y en la linea 90 + en nro de octava esta la info del acorde a borrar..
      Dim As Integer verticalEnOctavaVacia '  6-4 =2
' 90,91,92,93,95 la default tomara la posicion vertical 
' 2) => verticalEnOctavaVacia= vacio + estoyEnOctava - desde   
' en un solo paso, linea de la octava libre que contendra la info dela corde:
verticalEnOctavaVacia= 12 + (hasta-2)*13 + n0 - desde ' 90 + 6 - 4=92 
 Roll.trk(undo_acorde(ig,0).posn,verticalEnOctavaVacia ).nota = 0
 Roll.trk(undo_acorde(ig,0).posn,verticalEnOctavaVacia ).dur  = 0

   
' ----fin borrado informacion de acorde en la octava mas alta no usada....        

      For ik=1 To undo_kant_intervalos(ig)
          Roll.trk(undo_acorde(ig,ik).posn,undo_acorde(ig,ik).pn).dur=undo_acorde(ig,ik).dur
          Roll.trk(undo_acorde(ig,ik).posn,undo_acorde(ig,ik).pn).nota =undo_acorde(ig,ik).nota
         ' Track(ntk).trk(indicePos,1+ik).nota=0
         ' Track(ntk).trk(indicePos,1+ik).dur=0
      Next ik
    
      ''undo_acorde(cnt_acor,ik).pn =0
      
      cnt_acor=cnt_acor-1
      If cnt_acor=0 Then
         Print "se anulo indices info de undo"
         ig=0
      EndIf
      scan_alt=1
      Sleep 100
   EndIf
   While InKey <> "": Wend
   scan_alt=1
Else
   While InKey <> "": Wend
   scan_alt=0
   
EndIf   
' melodia  
If MultiKey(SC_ALT)  And MultiKey(SC_U) And scan_alt=0  Then '<=== undo melodia
Dim As Integer ik=0,ij=0,im  
   If mel_undo_k > 0  Then ' borra de a uno desde final  a adelante
     ik=mel_undo_k
      ' no hace falta grabar y reponer se supone era nuevo ergo 0,0
      '   Roll.trk( cmel_undo(ik).posn, mel_undo(ik).columna.pn).dur =mel_undo(ik).columna.dur
      '   Roll.trk( mel_undo(ik).posn, mel_undo(ik).columna.pn).nota =mel_undo(ik).columna.nota
' volver a ceros el resto de notas
' tola la melodia en track esta en posicion vertical 1 las otras son de acorde 
' LO QUE HACE ES EN CADA POSN BORRA TODA LA COLUMNA, TAMBIEN SERVIRIA 
' COMO UNDO GENERAL YA QUE AL BARRER TODA LA COLUMNA BARRE ACORDES TAMBIEN
' O SEA SIEMPRE QUE LSO ACORDES INGRESADOS FORMENPARTE DE LA MELODIA ENTRADA
' BORRARA TODO.
         For ij=NB To NA -13' evitamos borrado decontroles de acorde 01-02-2022
            Roll.trk(mel_undo(ik).posn, ij).nota =0
            Roll.trk(mel_undo(ik).posn, ij).dur  =0
           ' Track(ntk).trk(mel_undo(ik).posn,1).nota=0
           ' Track(ntk).trk(mel_undo(ik).posn,1).dur=0
         Next ij 
     
     MaxPos=MaxPos-1
     mel_undo_k=mel_undo_k -1
     Sleep 100
   EndIf
   
   While InKey <> "": Wend
   scan_alt=1
Else
   While InKey <> "": Wend
   scan_alt=0
   
EndIf 



If MultiKey (SC_F12) And abierto=0 Then
'''archivo test-AAAAA.TXT

abierto=1
 Dim As Integer i1, i2
 Dim As String result 
 ' testeo solo en la 1er octva por ahora
  Print #5,
 Dim As Integer oct1, oct2
 oct1= 0 + (EstoyEnOctava-1) * 13 
 oct2 = 11 + (EstoyEnOctava-1)*13
 Print #5,"vuelco de octava ";EstoyEnOctava; " desde ";oct1;" a ";oct2
 For i1 = oct2 To oct1 Step -1
  For i2= 1 To Maxpos
   result = Format (Roll.trk(i2, i1).nota,"00")
   Print #5,  result;"-";
  Next i2
  Print #5,
  For i2= 1 To Maxpos
   result = Format (Roll.trk(i2, i1).dur,"00")
   Print #5, result;"-";
  Next i2
  Print #5,
  Print #5,"------------------------"
 Next i1
 While Inkey <> "": Wend
 Sleep 150
 Close 5
 Print #5,"fin >>>>>>>>>>> "
 
EndIf

If MultiKey (SC_F10) Then
 font = font + 1
 If font > 24 Then
  font=24 ' pisa la 1er duracion mas de ahi....
 EndIf
 Exit Do
EndIf

If MultiKey(SC_ESCAPE) Then
   'ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   'Dim As hWnd hwnd = Cast(hwnd,IhWnd)

  If MessageBox(hWnd,"¿Fin RollMusic? " ,param.titulo ,4 Or 64) =6 Then
    cairo_destroy(c)
    cairo_surface_destroy( surface )
    FT_Done_Face( ftface )
   
    If play=1 Or playb=1 Then
      alloff (1)
      ThreadDetach(thread1)
    EndIf
    close_port(midiout)
    out_free(midiout)
    Dim ffile As Integer
    ffile=FreeFile
    Open "./RollMusic.ini" For output As #ffile

    If nmxold = 0 Then
       nmxold=mxold
       nmyold=myold
    EndIf   
    If nancho=0 Then
       nancho=ANCHO
       nalto =ALTO
    EndIf    
    If ndeltaip=0 Then
       ndeltaip=inc_Penta
    EndIf
    nanchofig=anchofig
    Print #ffile,font , " font"
    Print #ffile,nmxold, " mxold "
    Print #ffile,nmyold, " myold"
    Print #ffile,nANCHO, " ANCHO"
    Print #ffile,nALTO, " ALTO"
    Print #ffile,ndeltaip, " inc_Penta"
    Print #ffile,nVerEscalasAuxiliares, "nVerEscalasAuxiliares"
    Print #ffile,nanchofig, "nanchofig"
    Print #ffile,nVerCifradoAcordes, "nVerCifradoAcordes"    

    cerrar ffile
      Sleep 100
    cerrar 0
    End 0

  EndIf  
EndIf
' AYUDA =============ESPACIOS MANEJO ===================================
' repetir espacios con barra+ALTGRAF..luego la nota correspondiente
'================================================================
If MultiKey(SC_ALTGR) Then ' FIJA ESPACIOS REPETIDOS HASTA NUEVA PULSO
 fijarEspacio = 99
 ' fijar para muchso espacios
EndIf
'--

' ============== E S P A C I O ========
If MultiKey(SC_SPACE)  Then 'barra espacio
 If COMEDIT=TRUE Then
    espacio = 1
    DUR=0
    nota=notacur ''nE 10-05-2021 00:06 probar de nuevo 
    If cursorVert =2 Then
      agregarNota = 1
    EndIf

 Else
   If playb = 0 And MaxPos > 1 Then
      playb=1
      print #1,"SPACE call play"
        If  MaxPos > 1 Then 
         '''Dim tlock As Any Ptr = MutexCreate()
         If CANCIONCARGADA Then
             Print #1,"USANDO PLAYCANCION"
            thread1 = ThreadCall  playCancion(Track())
         Else
            thread1 = ThreadCall  playAll(Roll)
            Print #1,"USANDO PLAYALL"
            '' playAll(Roll)
         EndIf
         '''MutexDestroy tlock
         '''playAll(Roll)
        EndIf   
      
      menunew=0
   EndIf
 EndIf  
 Exit Do
EndIf

If MultiKey (SC_Q) Then ' con Q se deja de repetir espacios tmbien resetea todo ls banderas de notas
 If fijarEspacio=99 Then
  fijarEspacio=0
 EndIf
If pasoZona1 > 0 Or pasoZona2 >0 Or pasoNota > 0 Or trasponer=1 Then ' hubo una trasposicion
   correcciondeNotas(Roll) ' para moverZona no se corrige creo por ahora...
EndIf 
pun=0:sil=0:tres=0:mas=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
deltaip=0:incWheel=0:lockip=0
'anchofig=35
'gap1= (anchofig* 2315)/1000  ' 81 default
'gap2= (914 * gap1) /1000 ' 74 default
'gap3= (519 * gap1) /1000 ' 42 default
'font=18
'NroCol =  (ANCHO / anchofig ) - 4
 cursorVert = 0
 cursorHori = 0
 agregarNota=0
 menuMouse = 0
 nota=0
 DUR=0
 alloff( 1 )

EndIf
' ----------------------INGRESO NOTAS-------------------------
' MAYUSCULAS PARA SOSTENIDOS
' Ahora en nota se guarda el semitono 1 a 12...,  DUR guarda la duracion
If COMEDIT = TRUE Then ' ingreso de notas   

If MultiKey(SC_CONTROL) And MultiKey(SC_A)  Then ' A#
 nota= 2
 If espacio > 0 Then
  espacio=2
 EndIf   ' hasta aca logica nueva
 ' si estoy escribiendo espacio pero salto a otra nota, algo ilogico...
 ' para eso dejamos ALT_GLRAF que resetee el espacio a 0 manualmente,
 ' osi memuevo con flechas podrai enviar cero al espacio
 '    if nota<> notaold THEN ' stnd by nola usare
 '       espacio=0
 '    EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_C)   Then ' C#
 nota = 11
 If espacio > 0 Then
  espacio=11
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_D)  Then ' D#
 nota= 9
 If espacio > 0 Then
  espacio=9
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_F) Then ' F#
 nota= 6
 If espacio > 0 Then
  espacio=6
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If  MultiKey(SC_CONTROL) And MultiKey(SC_G)  Then ' G#
 nota= 4
 If espacio > 0 Then
  espacio=4
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_A) Then
 nota= 3
 If espacio > 0 Then
  espacio=3
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_B) Then
 nota = 1
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_C) Then
 nota = 12
 If espacio > 0  Then
  espacio=12
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_D) Then
 nota= 10
 If espacio > 0 Then
  espacio=10
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do 
EndIf

If MultiKey (SC_E) Then
 nota = 8
 If espacio > 0 Then
  espacio=8
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_F) Then
 nota= 7
 If espacio >  0 Then
  espacio=7
 EndIf
 If cursorVert =2 Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_G) Then
 nota= 5
 If espacio >  0 Then
  espacio=5
 EndIf
 If cursorVert = 2  Then
  agregarNota=1
 EndIf
 Exit Do
EndIf

EndIf  ' COMEDIT =true para ingreso de notas 


If MultiKey(SC_BACKSPACE) Then ' sin uso por ahora. 
''podria ser otra forma de colocar 190,190 en lectura ¿? y
' ir borrando columnas dejandolas vacias con solo 190,190 al ser eliminada al grabar.
' y cargar. 
 backspace=1

EndIf
' ----------------------FIN NOTAS-------------------------
' ----------INGRESO DE DURACIONES DE NOTAS -------------

If COMEDIT = TRUE Then  
 If (menuNew = 2 Or menuNro=2) Then  
  If MultiKey(SC_1) Then
   DUR = 1 :Exit Do
  EndIf
  If MultiKey(SC_2) Then
   DUR = 2:Exit Do
  EndIf
  If MultiKey(SC_3) Then
   DUR = 3:Exit Do
  EndIf
  If MultiKey(SC_4) Then
   DUR = 4:Exit Do
  EndIf
  If MultiKey(SC_5) Then
   DUR = 5
   print #1,"DUR ",DUR
   Exit Do
  EndIf
  If MultiKey(SC_6) Then
   DUR = 6:Exit Do
  EndIf
  If MultiKey(SC_7) Then
   DUR = 7:Exit Do
  EndIf
  If MultiKey(SC_8) Then
   DUR = 8:Exit Do
  EndIf
  
' fin duracion de notas para entrada normal
' CURSOR MODIFICCION DE NOTAS ergo usamos nota=0 para evitar entrada normal   
  If MultiKey(SC_9) Then 'espacio en edit sin cursor
     DUR = 181:Exit Do
     nota=0
  EndIf
  If MultiKey(SC_CONTROL) And MultiKey(SC_0) Then ' fin seq en edit sin cursor
     DUR = 182
'     print #1,"DUR=182 !"
     nota=0
     Exit Do
  EndIf
  If MultiKey(SC_0) Then ' fin seq en edit sin cursor
     nota=0
     DUR = 0:Exit Do
  EndIf


  If MultiKey(SC_PERIOD) Then
     pun = 1  ' puntillo      
  EndIf
''  If MultiKey(SC_MULTIPLY) Then ' KEYPAD *
''     cuart=1
''   Exit Do
''  EndIf

  If MultiKey(SC_S) Then
   sil = 1
   Exit Do  ' silencio
   ' indicadorde silencio solo para calculo de compas
  EndIf
  If MultiKey(SC_t) Then
   tres = 1
   Exit Do  ' tresillo
   ' indicadorde tresillo solo para calculo de compas
  EndIf

  If MultiKey(SC_CONTROL) And MultiKey(SC_DELETE) Then
  ' BORRAR COLUMNA UTOMATICO LUEGO DE BORRAR NOTAS CON 0 Y X
  ' no anda bien 12-12-2021 se cambia a marcar por zona columna o columnas
     borrarColumnasMarcadas()
  EndIf

 EndIf 
 ' ojo ver q no haYa  exit do antes !!!!!
EndIf 


' ----HELP PRUEBA DE TEXT
If MultiKey(SC_F1) Then
' por ahora solo traeremos un texto, luego usaremos llamar
' a un archivo de help chm..
 Shell ("start notepad ayuda.txt")
 ' estopodemos hacer ayuda contextual
 '' Define character range
 '.DRAWASTRING CON FONT DEUSUARIO A VECES SEGUN COMO SE COMPILE HACE CERRAR LA APLICACION
 ' ELIMINADO NOUSAR ESE METODO, USAREMOS CAIRO...(para todo veo ....)
EndIf
'
If MultiKey(SC_R) Then ' recalculo de barras compas a veces no anda ¿? 
 ReDim compas(1 To CantTicks)
 ReCalCompas(Roll) ' jmg 01-04-21 
EndIf

If COMEDIT = FALSE Then ' construir cifras para copiar Nveces por ejemplo
 ' 

 If MultiKey(SC_1) Then
    cifra = "1"
    DUR=1
    Exit Do
 EndIf
 If MultiKey(SC_2) Then
    cifra = "2"
    DUR=2
     Exit Do
 EndIf
 If MultiKey(SC_3) Then
    cifra = "3"
    DUR=3
    Exit Do
 EndIf
 If MultiKey(SC_4) Then
    cifra = "4"
    DUR=4
    Exit Do
 EndIf
 If MultiKey(SC_5) Then
   cifra = "5"
   DUR=5
    Exit Do   
 EndIf
 If MultiKey(SC_6) Then
    cifra = "6"
    DUR=6
    Exit Do
 EndIf
 If MultiKey(SC_7) Then
    cifra = "7"
    DUR=7
     Exit Do
 EndIf
 If MultiKey(SC_8) Then
    cifra = "8"
    DUR=8
     Exit Do
 EndIf
 If MultiKey(SC_9) Then
    cifra = "9"
    DUR = 9
     Exit Do
 EndIf
 If MultiKey(SC_0) Then
    cifra = "0"
    Exit Do
 EndIf
' v23 agregado  inicio 
  If MultiKey(SC_PERIOD) Then
     pun = 1  ' puntillo      
  EndIf
''  If MultiKey(SC_MULTIPLY) Then ' KEYPAD *
''     cuart=1
''   Exit Do
''  EndIf

  If MultiKey(SC_S) Then
   sil = 1
   Exit Do  ' silencio
   ' indicadorde silencio solo para calculo de compas
  EndIf
  If MultiKey(SC_t) Then
   tres = 1
   Exit Do  ' tresillo
   ' indicadorde tresillo solo para calculo de compas
  EndIf
' v23 agregado fin 
 
 If pasoZona1 > 0 And pasoZona2 > 0 And copiarZona=0 And cifra<>""  Then
        digito= digito + cifra
        numero = cint(digito)
'    print #1,"numero ", numero
        copi=numero
        cifra=""
        Exit Do  
 EndIf
 
 If MultiKey(SC_HOME) Then
     posicion=1
     posishow=posicion
 EndIf

 If MultiKey(SC_END) Then
    If MaxPos > 45 then
       posicion=MaxPos - 30
    EndIf
    posishow=posicion
 EndIf
      
' ESTE LLAMADO ACTUA EN COMEDIT=FALSE Y CON pasoZona 1 y/o 2 
  If MultiKey(SC_CONTROL) And MultiKey(SC_DELETE) Then
  ' BORRAR COLUMNA UTOMATICO LUEGO DE BORRAR NOTAS CON 0 Y X
  ' no anda bien 12-12-2021 se cambia a marcar por zona columna o columnas
     borrarColumnasMarcadas()
  EndIf


 
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
''
' si no uso 1 + inicioDeLEctura y  uso posicion que pasa?
' el programa no sabe donde el usuario colocara una nota nueva en la pantalla
' pero si sabe en que posicion arranca la vista ergo ya no haria falta posn
' que revise desde el principio SINO DESDE posicion ficticimente hacia eso
' al usar iniciodeLectura, pero eso sí, con inicio congelaba el movimiento
' del roll ante una entrada de nota hasta el próximo incremento de pantalla
' SOLO SEUSAPARAINGRESO DE NOTAS NUEVAS ..VERIFICANDO JMG

If COMEDIT = TRUE  And nota> 0 And agregarNota=0 And cursorVert=0 And carga=0 And nota <=182 Then ' 182 entra el fin de archivo 
 'print #1,"--------------------------------------------------------------"
 'print #1,">>>START NUCLEO-COMPAS VECTOR posn: "; posn; "suma:";acumulado
 'print #1,">>>START NUCLEO-COMPAS PROCESANDU DUR: " ; DUR;_
 '   " nota: ";nota; " figura: ";figura(DUR)
Print #1,"entro nota ",nota 
 
 posn=1+InicioDeLectura
 'If DUR=0 Then
 ' nota=0
 ' 
 'EndIf
 
 If controlEdit=0 Then
   controlEdit=1 
   octavaEdicion=estoyEnOctava
 EndIf
  '   print #1,"estoyEnOctava ";estoyEnOctava
 ' And octavaEdicion = estoyEnOctava
 If estoyEnOctava <> 99  And octavaEdicion = estoyEnOctava Then ' estoy en una octava
  '  If indice <= 0 Then
  '      indice = 1
  '  EndIf
  '  If indice >= 128 Then
  '      indice = 128
  '  EndIf
  If nota > 0 And estoyEnOctava < 99 And estoyEnOctava >=1 Then

   ' ====>  Control PAgindo Horizontal <=======
   '      kNroCol= Int(posicion/60)
   '       print #1, "A:Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota ", _
   '       Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota
   ' PARA USAR ESTO CON ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
   ' y hacer nota=semiotono 1 a 11 con el mouse...el esto es automtico...
   Do ' nota es semitono ahora va de 0 a 11 deborestr 1 a nota
'  Print #1,"Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota ",Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota
    If Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = 0 Or Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = 182 Then
     posicion=posn
     '182 el fin de archivo lo puedo pisar para seguir la secuencia
     '      print #1, "ingreso a NUCLEO POSICION=POSN", posicion
     Exit Do
    EndIf
   

    posn = posn + 1
'---control barrido de pantalla columna
    If (posn > NroCol + InicioDeLectura) Then
     InicioDeLectura=InicioDeLectura + NroCol
    EndIf
  '   print #1, "ingreso a NUCLEO posn ",posn
  Loop 

    MaxPos=Posn +1 
    Print #1,"posn, Maxpos,nota ",posn,Maxpos,nota
    pmTk(ntk).posn=posn
    pmTk(ntk).MaxPos=MaxPos


'--- AUMENTO DE CAPACIDAD DEL VECTOR EN 1000 POSICIONES 
    If CantTicks - MaxPos < 120 Then
       print #1,"hace backup....." ' si no hay nombre usa fecha"
       GrabarArchivo(1) ''hacer un backup !!! 
      CantTicks=CantTicks + 1000 ' incremento el tamaño en 1000 posiciones =1 min
      ReDim Preserve (Roll.trk ) (1 To CantTicks,NB To NA)
      ReDim Preserve compas(1 To CantTicks)
      ReDim Preserve (RollAux.trk) (1 To CantTicks, NB To NA)
      ' ¿a que ntk apunta al 0 o al de cancion ?
      print #1,"REDIM EN NUCLEO DE TRACK NTK= ", ntk
      ReDim Preserve (Track(ntk).trk)(1 To CantTicks,1 To lim3)
    EndIf
    pmTk(ntk).Ticks=CantTicks


   ' ESTO ME UBICA EN QUE RENGLON DE LA OCTaVA ESTOY SN USAR EL MOUSE
   ' CON EL MOUSE tambien se hizo
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = nota 'carga visualizacion
   Print #1,"NUCLEO: Posn,nota,EstoyEnOctava ",posn,nota,EstoyEnOctava
   nR=(12-nota) + (estoyEnOctava -1 ) * 13 
   PianoNota= nR - restar (nR)
   print #1,"' cargo TRACK nro, PianoNota ",ntk,PianoNota 
   Track(ntk).trk(posn,1).nota= PianoNota ' de done la saca? creapenta ya se ejecuto?
   
   Roll.trk(posn+1,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
   
   'cargo TRACK
   Track(ntk).trk(posn,1).dur= 182

   If notaOld > 0 And notaOld <> nota  Then
  '  print #1,"Roll.trk((notaOld +(estoyEnOctava    -1) * 13),posn).nota"; _
   '           Roll.trk((notaOld +(estoyEnOctava    -1) * 13),posn).nota
    Roll.trk(posn,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).dur = 0 
    Roll.trk(posn,(12-notaOld  +(estoyEnOctava    -1) * 13)).dur = 0 
    Roll.trk(posn,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota = 181
    Roll.trk(posn,(12-notaOld  +(estoyEnOctava    -1) * 13)).nota = 181
   
    
 '   print #1,"(notaOld +(estoyEnOctava  -1) * 13)"; notaOld +(estoyEnOctava  -1) * 13
 '   print #1,"posn ";posn
 '   print #1,"notaold";notaold
 '   print #1,"nota";nota
    '''ojo probar todo inserciones x  etc    endif
   EndIf
   ' cargamos Roll entonces Duracion no lo mostrara como "./."
   ' solo conrolara la posicion, quedndo solo ese simbolo paralaa entrada
   ' especifica del usuario....
   ' llena todo de espcio por eso  aparece la duracion
   ' cargada al siruarme en lugaressin nota o sea crgr nota
   ' nuevdonde no habia paso siguinte crgral not hbilitarencursos
   ' la crg de roll
   'print #1,"-------------------------------------------"
   'print #1," estoyEnOctavaOld=estoyEnOctava OLD:";estoyEnOctavaOld
   'print #1," estoyEnOctavaOld=estoyEnOctava NEW:";estoyEnOctava
   'print #1," NOTAOLD ";notaold
   'print #1," NOTA    ";nota
   'print #1," dur "; DUR
   'print #1," pesoDur "; pesoDur(DUR)
   'print #1," figura "; figura(DUR)
   'print #1,"-------------------------------------------"

   Dim As Integer noct ''oclog = 8 - (estoyEnOctava-1)
   ' barre solo vertical no la posicion 
   For noct = desde To hasta
     For i= 0 To 11 ' gracias a esto anda acordes¿?
       If i= 12 - nota  And noct = estoyEnOctava  Then 
         Continue For
       Else    
       ' semitono ahroa va de 0 a 11 para Roll -> i-1 21-07-2021 jmg
       ' si estoy en fin de sec 182-> nota=0 dur=182
         If Roll.trk(posn,(i +(noct -1) * 13)).nota = 0 And Roll.trk(posn,(i +(noct -1) * 13)).nota < 182  Then ' no borra 182
         '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk(posn,(i +(noct -1) * 13)).nota = 181
            Roll.trk(posn,(i +(noct -1) * 13)).dur  = 0

         EndIf
        ' If Roll.trk((i +(noct -1) * 13),posn).nota = 182 And posn<>MaxPos Then
        ' print #1,"^^^^ cambia 182 en i";i ; "octava "; noct
        '    Roll.trk((i +(noct -1) * 13),posn).nota = 181
        ' EndIf
       EndIf
     Next i
   Next noct
   ' para track permitir acordes
   For i=1 To lim2
         If Track(ntk).trk(posn,i).nota = 0 And Track(ntk).trk(posn,i).dur <182    Then
         '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Track(ntk).trk(posn,i).nota = 181
            Track(ntk).trk(posn,i).dur  = 0

         EndIf
   Next i

   notaOld=nota
   pmTk(ntk).notaold=CUByte(notaold)
   'print #1,"carga notaold";notaold
   estoyEnOctavaOld=estoyEnOctava
   
   ' 
' los bloques de durcion se repiten de a 3, el incr es por bloque
' si voy al 2do bloque de 3 el incrdeja de ser 0 y pasa a 27 respectro de la
' 1er linea l cargo TRAck como 2da linea
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR 'era duracion
   Track(ntk).trk(posn,1).dur = DUR
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
   incr=0
EndIf
' CUART   
If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 9 'era duracion
    Track(ntk).trk(posn,1).dur = DUR +9
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
    incr=0
EndIf
' PUNTILLO   
' 3I* = I = 1 , el puntillo a un 3 suma dando la figura de la q proviene.   
If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 18 'era dur
    Track(ntk).trk(posn,1).dur = DUR + 18
    incr=0
EndIf
'DOBLE PUNTILLO   
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 27
   Track(ntk).trk(posn,1).dur = DUR + 27
   incr=0
EndIf
' SEGUIR  JMG
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 36
   Track(ntk).trk(posn,1).dur = DUR + 36
   incr=0
EndIf   
' -----fin 1er bloque ------------------------

If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   Roll.trk(posn,(12 -nota +(estoyEnOctava -1) * 13)).dur = DUR + 45 'era dur
   Track(ntk).trk(posn,1).dur = DUR + 45
'   print #1," NUCLEO GUARDO EN ROLL CON S DUR: ";DUR +27;" figura:";figura(DUR+27) 
   incr=45
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 54
   Track(ntk).trk(posn,1).dur = DUR + 54
   incr=45
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 63
   Track(ntk).trk(posn,1).dur = DUR + 63
   incr=45
EndIf
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 72
   Track(ntk).trk(posn,1).dur = DUR + 72
   incr=45
EndIf

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 81
   Track(ntk).trk(posn,1).dur = DUR + 81
   incr=45
EndIf

' -- fin 2do bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 90
    Track(ntk).trk(posn,1).dur = DUR + 90
    incr=90
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 99
    Track(ntk).trk(posn,1).dur = DUR + 99
    incr=90
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 108
    Track(ntk).trk(posn,1).dur = DUR + 108
    incr=90
EndIf

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 117
   Track(ntk).trk(posn,1).dur = DUR + 117
   incr=90
EndIf
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 126
   Track(ntk).trk(posn,1).dur = DUR + 126
   incr=90
EndIf
'----- fin 3er bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 135
   Track(ntk).trk(posn,1).dur = DUR + 135
   incr=135
EndIf   

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 144
   Track(ntk).trk(posn,1).dur = DUR + 144
   incr=135
EndIf   

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 153
   Track(ntk).trk(posn,1).dur = DUR + 153
   incr=135
EndIf   

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=1 Then 
   Roll.trk(posn, (12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 162
   Track(ntk).trk(posn,1).dur = DUR + 162
   incr=135
EndIf   

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 171
   Track(ntk).trk(posn,1).dur = DUR + 171
   incr=135
EndIf   
mel_undo_k = mel_undo_k + 1

mel_undo(mel_undo_k).trk = ntk
mel_undo(mel_undo_k).posn = posn
'mel_undo(mel_undo_k).columna.pn = nR
'mel_undo(mel_undo_k).columna.dur = DUR
'mel_undo(mel_undo_k).columna.nota = nota


' ---fin 4to bloque -
   
  ' print #1," NUCLEO GUARDO DUR EN ROLL ";DUR;" figura ";figura(DUR)
  ' print #1," NUCLEOBUSC Y GREG EN POSIICON :" ;posn
 '  If DUR=64 Then
 '   Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 65
 '   DUR=0
 '  EndIf
   cuart=0:pun=0:doblepun=0:tres=0:sil=0:mas=0
   ' mayorDurEnUnaPosicion (posn) quedo <--defectuoso
     'If DUR >=1 And DUR <= 72 Then
      DUR = Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur

      calcCompas(posn,Roll) 'lrepeticion no espor calcCompas
     'EndIf
   '   rmerr = Err
   '  print #1,"Nucleo Error "; rmerr

   nota = 0
   If DUR= 181 Then
      DUR=0
   EndIf

  Else ' edicion de nota anterior retroceso, concosco la posicion la octava
   'pero no la nota 1ero debo recuperar la nota, cursor lo sabe tomar de ahi
   'no puedo usar notaOld porque eso seria solo en el caso de que estaba editando
   ' para esa nota 1ero deberia oder moverme arribay bjo con el cursor para
   ' posicionarmeen una nota....ctrl-felcahs verticales ubicaran en cursor
    nota=0
  EndIf
  nota = 0 '18-11-2021
  'print " Roll ", Roll.trk(indice, posicion)
    
  ' mostrarla en la t del Roll en el indice correspondiente
  ' ocalculo elindice en cada t y meto la nota o saco en t las otas
  ' del vector Roll pra ello acaa la grabo enRoll
 EndIf
 '''   calcCompas(posn)
 ' correccion de loop octava jmg09-06-2021 , por ahora hasta encontar 
 ' la causa real preparado el corrector,pero encontre una causa veremos
 'If estoyEnOctava <>octavaEdicion Then
 '   octavaloop=octavaloop +1
 'EndIf 
 'If octavaloop > 1000 Then
 '   octavaEdicion=estoyEnOctava
 'EndIf
' ============================================== 
' CREACION DE SECUENCIA PARA TOCAR CON POCA CPU
'============================================== 
' notan vale 1 para toda la melodia cargada en Edit.
' al cargar acorde notan cambiara de 2 a 12
  If  MaxPos > 1 Then
   ' crearsecuencia(track(ntk).trk(), posn,ntk) habilitar cuadno todo ande bie
   ' en pausa secuecnia seria un pasopara crear un archivo midi de 1 track
   ' o crear un [[[play mas sencillo]] y rapido
  EndIf 
  nota=0 '18-11-20201
 ''''Exit Do 'kkkk 09-01-22 probando no debe sali rdebe seguir chequeando
EndIf
' fin correccion loop

'If COMEDIT=FALSE then
'  calcCompas(posn)
' EndIf

'' 24-06-2021 espaciado de lineas (1)
'If MultiKey(SC_CONTROL) And lockip=0  Then
'    deltaip=0
'    incWheel=0
'    lockip=1
'    Exit Do    '
'
'EndIf 
'If COMEDIT=FALSE Then '''HARIA FALTA COMEDIT FALSE ? CREO QUE NOORRERA EN TRUE?
' '' 24-06-2021 espaciado de lineas (1)
'  If MultiKey(SC_CONTROL) And lockip=0  Then
'    deltaip=0
'    incWheel=0
'    lockip=1
'    Exit Do    
'
'  EndIf 
'EndIf
'-----------------------------SCREEN EVENT-------START -----------
' para detectar mouse sin usar sdl

If (ScreenEvent(@e)) Then
 Select Case As Const e.type
 ' 23-08-21 CPU consumo fuera de foco
  Case EVENT_MOUSE_EXIT ' mouse fuera de pantalla  
       If play = 1 Or playb=1 Then ' fuera pero en play
           fueradefoco=0
       Else     ' fuera de y sin play reducimos consumo CPU
           '' Sleep 20
           fueradefoco=1
       EndIf
  Case EVENT_MOUSE_BUTTON_PRESS
' el help de freebasic esta mal dice que button left es 0 y es 1 !!!! joder pero anda mejor 
      ' If e.button = 0 Then
          MousePress = 1
      ' EndIf
      ' If e.button = 2 Then  ' queda pro probar
      '    MousePress = 2
      ' EndIf

		Case	EVENT_MOUSE_MOVE
		     MouseMove=1			
  Case EVENT_MOUSE_BUTTON_RELEASE ' obtengoPosicion
   MousePress = 0
   If mousey < 50 And s5=2 Then
      s5=0
      Exit Do ' 07-10-2021
   EndIf
   If mousey < 50 And s3=2 Then ' 20-01-2022
      s3=0
      Exit Do       
   EndIf
  
  
 
  Case EVENT_MOUSE_WHEEL      ' <<<=== MOUSE WHEEL
   ' new position & e.z
    If cargaCancion=1 Then ' 10-10-2021 durante al carga de cancion deshabilitamos
       Exit Do
    EndIf   
   posmouse = e.z
   If posmouse <> posmouseOld Then
    incWheel = posmouse - posmouseOld
    posmouseOld = posmouse
   EndIf
   If lockip=0 Or lockip=2 Then 
     If incWheel > 0 Then
        If s2=0  Then
           s2=1
           BordeSupRoll = BordeSupRoll + inc_Penta
        EndIf
        If BordeSupRoll >= AltoInicial * 0.5   Then
           BordeSupRoll =  AltoInicial * 0.5
        EndIf
      
     Else
       If s1=0  Then
         s1=1
         BordeSupRoll = BordeSupRoll - inc_Penta
       EndIf
       If BordeSupRoll <= - AltoInicial * 2.8  Then
         BordeSupRoll =  - AltoInicial * 2.8
       EndIf
     EndIf
   EndIf

  Case EVENT_KEY_PRESS    ' <======== KEY PRESS PULSO
   If e.scancode = SC_P And Play=1 then ' 25 anda mejor q con multikey
      CONTROL1=1
      playloop=0
      alloff( 1 )
   EndIf
   If e.scancode = 72  Then '<<<==== SC_UP sube por pulsos mas presicion

    If trasponer=1 And SelGrupoNota=0 Then
              Print #1,"3 TRASPONER !!!!!!!!!!!!!!", trasponer
     trasponerRoll ( 1,Roll,encancion)
     Exit Do
    EndIf
    If trasponer = 1 And SelGrupoNota=1 Then
              Print #1,"4 TRASPONER !!!!!!!!!!!!!!",trasponer
     TrasponerGrupo ( 1, Roll,encancion)

     Exit Do 
    EndIf 

    If cursorVert= 0 Then
     If s2=0 Then
      s2=1
         print #1,"pulso UP r 1 inc_penta"
      BordeSupRoll = BordeSupRoll +   inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf

     Exit Do
    EndIf
    If cursorVert=1 Or cursorVert=2 Then
     notacur=notacur-1
     If notacur < 1 Then
      notacur=12
     EndIf
     Exit Do
    EndIf
   EndIf

   If e.scancode = &h41 Then ' <======= SC_F7
    If COMEDIT = FALSE Then
     ' MOVE VENTANA
     h=ALTO
     ScreenControl GET_WINDOW_POS, x0, y0
     ALTO = ALTO - inc_Penta
     If ALTO <= ALTO * 0.3 Then
      ALTO =  ALTO * 0.3
     EndIf
     '
     '    ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     '    Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0 , (Y0+h-ALTO)\2, ANCHO - mxold,ALTO, TRUE )

    EndIf
    Exit Do
   EndIf
   If e.scancode = &h42  Then ' <==== F8
    If COMEDIT = FALSE Then
     ' MOVE VENTANA
     Dim As Integer w,h
     h=ALTO
     ScreenControl GET_WINDOW_POS, x0, y0
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
      ALTO = altoInicial  - 1
     EndIf

     '  ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)

     '  Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0, (Y0+ALTO-h)\2, ANCHO - mxold,ALTO, TRUE )

    EndIf
    Exit Do
   EndIf

   If e.scancode = 67 Then ' <===== SC_F9
    font = font - 1
    Exit Do
   EndIf
   '  If  e.scancode = 68 Then ' <====== F10 'no funciona no lodetecta !!
   '      font = font + 1
   '      Exit Do
   '  EndIf
' -------------------------------
 
   If e.scancode = 83 Then '<====== SC_DELETE cambia a silencio o nada le suma 16+16 ver eso
   
        If borrar=1 Then
          borrar=0
          Exit Do
       EndIf   
       If borrar=0 Then
          borrar=1
          Exit Do
       EndIf  
   EndIf 
'-------------
If e.scancode = SC_PAGEDOWN Then  ' PAGEDOWN 81
 'If s1=0 Then
 ' s1=1
  BordeSupRoll = BordeSupRoll - inc_Penta * 11
 'EndIf
 If BordeSupRoll <= - AltoInicial * 2.8 Then
  BordeSupRoll =  - AltoInicial * 2.8
 EndIf
 Exit Do
EndIf

If e.scancode= SC_PAGEUP Then  'PAGEUP
 

  BordeSupRoll = BordeSupRoll + inc_Penta * 11

 If BordeSupRoll >= AltoInicial * 0.5  Then
  BordeSupRoll =  AltoInicial * 0.5
 EndIf

 Exit Do

EndIf



'-----
   If e.scancode = SC_X Then ' 81 <==== SC_X ...fix
    'corrige nota cambia duracion o agrega nota nueva, acorde melodia
    ' solo debe funcionar con CTRL-M
    If cursorVert=1 Then ' ver cursorVert2 archivonuevocon espacios....sirve??
     cambiadur = 1    ' usando 1 a 8 para silencio esta delete
    EndIf
    Exit Do
   EndIf
   ' para insertar en cada iten de Roll cda vez queingreso nota al final
   ' comunmente se agregara 182 ? 66 para indicar fin de datos..y loindicaremos
   'con FIN por ahora para visualizarlo

   If e.scancode = SC_INSERT And insert=0  Then '34 <===== SC_INSERT
    ' solo valido con CTRL-M
    If cursorVert = 1 And  cursorHori = 1 And insert = 0 Then
     ' solo tiene sentido insertar en lo echo y en cursor libre
     insert=1 ' comienzo hbilitotel I para insertr nota por nota
     indaux=0
   '  print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert
     If indaux=0 Then ' no haria falta todas las demas inserciones se deben
      'hacer con I no volver a repetir SC_INSERT sino se pulso SC_END
      StartInsert = posicion + curpos  ' guardo sin modificar el comienzo xxx ok
     EndIf
   '  print #1,">>>SC_INSERT  despues ajuste STARTINSERT ", StartInsert
     '''Erase (RollAux.trk) ' borro lo que habia en el auxiliar
     ReDim (RollAux.trk) ( 1 To CantTicks, NB To NA )
     '''Erase (notasInsertadas)
     ReDim notasInsertadas (1 To 1500)
     notins=0
   '  print #1, ">>>SC_INSERT insert indaux borro RollAux.trk: ",insert,indaux
     'sigue el proceso en RollSub->sub cursor
    EndIf
   EndIf
   If e.scancode = SC_I And insert=1 And cursorVert=1 Then
    insert= 2 ' habilito cursor para que ingrese
  '  print #1, "-----SC_I -> insert,indaux : ",insert,indaux
   EndIf
   If e.scancode = SC_END Then ' mueve insercion, podria usarse para ELIMINAR Probar
    If backspace=1 Then
      Dim As Integer i, y
      For y=posishow To MaxPos
        For i=NB To NA 
        Roll.trk(y,i).nota=0
        Roll.trk(y,i).dur=0
        Next i
      Next y
      MaxPos=posishow+1 'jmg 03-04-2021
      posn=posishow
      Roll.trk(MaxPos,nR).dur=182 '  jmg no sera not1=182 ?
      Roll.trk(MaxPos,nR).nota=0
      ReCalCompas(Roll)
      backspace=0
      DUR=0
      nota=0
      Exit Do 
    EndIf
  '01-07-2021 hubo una cancelacion en ctrl-m al pulsar FIN en el teclado con 
  ' con And insert=2 se soluicona pero no se si es funcional para el insert verificar    
    If cursorVert=1  Then ' solo válido con Ctrl-M 30-06-2021 
     ' no mas reemplazos
     insert=3
   '  print #1, "-----SC_END StartInsert,indaux,insert,nota: ",StartInsert,indaux,insert,nota
   '  print #1,"indaux no deberia valer cero !!!! es global "
     moveresto (StartInsert,indaux, insert,nota)
     '  param : posicion comienzo (fijo), indice incremental para el aux
     ' ,insert comando habilitado = 1
     '  insert 3 fin reemplazos comienzo de move total
     insert=0:indaux=0
     ReCalCompas(Roll) '''''calcCompas(posn) '' mayorDurEnUnaPosicion (posn)
    EndIf
   EndIf

'--------   
'   If e.scancode = SC_RIGHT Then
'        If moverZona =1 Then
'        moverZonaRoll(1)
'        Exit Do
'        EndIf
'   EndIf
'   If e.scancode = SC_LEFT Then
''        If moverZona =1 Then
'        moverZonaRoll(-1)
'        Exit Do
'        EndIf
'   EndIf

  

   ' ------------------PULSAR MUCHO TIEMPO <====== REPEAT------
  Case EVENT_KEY_REPEAT
   If e.scancode = 72  Then ' <======= SC_UP

      If trasponer=1 And SelGrupoNota=0 Then
         trasponerRoll ( 1,Roll,encancion)
         Exit Do
      EndIf
      If trasponer=1 And SelGrupoNota=1 Then
        TrasponerGrupo ( 1, Roll,encancion)
        Exit Do 
      EndIf 
    If cursorVert = 0 Then
     If s2=0 Then
      s2=1
         print #1,"pulso UP screenevent 2 inc_Penta"
      BordeSupRoll = BordeSupRoll +   2 * inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
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

      If trasponer=1 And SelGrupoNota=0 Then
        Print #1,"0 TRASPONER !!!!!!!!!!!!!!",trasponer
         trasponerRoll ( -1,Roll,encancion)
         Exit Do
      EndIf
     If trasponer=1 And SelGrupoNota=1 Then
         TrasponerGrupo ( -1, Roll,encancion)
        Exit Do 
     EndIf 
    If cursorVert=1 Then
     notacur = notacur + 1
     If notacur > 12 Then
      notacur=1
     EndIf
    EndIf
    If cursorVert=0 Then
       If s1=0 Then
         s1=1
       print #1,"pulso down screeevent"
         BordeSupRoll = BordeSupRoll - 2 * inc_Penta
       EndIf
       If BordeSupRoll <= - AltoInicial * 2.8  Then
          BordeSupRoll =  - AltoInicial * 2.8
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

   If e.scancode = &h41 Then ' <============ SC_F7

    If COMEDIT = FALSE Then
     ' MOVE VENTANA
     h=ALTO
     ScreenControl GET_WINDOW_POS, x0, y0
     ALTO = ALTO - inc_Penta
     If ALTO <= ALTO * 0.3 Then
      ALTO =  ALTO * 0.3

     EndIf
     '
     '   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     '   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0 , (Y0+h-ALTO)\2, ANCHO - mxold,ALTO, TRUE )

    EndIf

    Exit Do
   EndIf

   If e.scancode = &h42  Then ' <============  F8
    If COMEDIT = FALSE Then

     ' MOVE VENTANA
     Dim As Integer w,h

     h=ALTO
     ScreenControl GET_WINDOW_POS, x0, y0
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
      ALTO = altoInicial  - 1
     EndIf

     '   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)

     '   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0, (Y0+ALTO-h)\2, ANCHO - mxold,ALTO, TRUE )

    EndIf

    Exit Do
   EndIf
'If e.scancode = 77  Then ' <======== RIGHT REPEAT
' If  mouseY < 50  Then ' seleccion de menu, mouse sobre cinta + teclas
'  menuNro=menuNro+1
'  If menuNro > 3 Then
'   menuNro=0
'   menuNew=0
'  EndIf
'  menuNew=menuNro
'  Exit Do
' Else
' 'kNroCol cantidad scroll de NrocOL)
'  If COMEDIT = FALSE Then
'   posicion = posicion + 1
'   kNroCol= Int(posicion/NroCol)
'   If  (kNroCol > 0) And (posicion = NroCol * kNroCol) And (posicion < MaxPos)Then
'    iniciodelectura = iniciodelectura +  NroCol
'    If inicioDeLEctura > MaxPos Then
'     inicioDeLEctura = inicioDeLEctura -NroCol
'    EndIf
'   EndIf
'   If posicion > MaxPos -1  Then
'    posicion = MaxPos -1
'   EndIf
'  Else
'   curpos= curpos + 1 ' mueve cursor cuando Roll se detiene (posicion)
'   If curpos > NroCol  Then
'    curpos = NroCol
'   EndIf
'
'  EndIf
'
'  Exit Do
' EndIf
'EndIf
'   escala = escala + 0.1
'If e.scancode = 75  Then '  <========== LEFT REPEAT
' If  mouseY < 50  Then  ' seleccion de menu
'  menuNro=menuNro - 1
'  menuNew=menuNro
'  If menuNro < 0 Then
'   menuNro=3
'   menuNew=3
'  EndIf
'  While Inkey <> "": Wend
' 
'  Exit Do
' Else
'  'MOVER ROLL IZQUIERDA NO CURSOR
'  If COMEDIT = FALSE Then
'   Dim kNroCol As Integer ' cntidad de scroll de 66
'   posicion = posicion - 1
'   kNroCol= Int(posicion/NroCol)
'   If  kNroCol > 0 And (posicion = NroCol*kNroCol)  Then
'    iniciodelectura = iniciodelectura - NroCol
'   EndIf
'   If iniciodelectura < 0 Then
'    iniciodelectura = 0
'   EndIf
'   If posicion < 1 Then
'    posicion = 1
'   EndIf
'
'  Else
'   curpos = curpos - 1 ' <=== MOVER CURSOR IZQ
'   If curpos < 0 Then
'    curpos = 0
'   EndIf
'
'  EndIf
' 
'  Exit Do
' EndIf
'EndIf
  Case EVENT_KEY_RELEASE 
' 24-06-2021 espaciado de lineas (3)  
       lockip=0

' -----------
  ' arriba de todo ponemos deteccion de teclas sin exit do para que siga.. 
       scan_alt=1

'---------------------------------------



	
       
 End Select
EndIf ' <= ScreenEvent(@e) END EVENTOS DE E Y MULTIKEY VAROS ESTAN AHI 
''If COMEDIT=FALSE Then ''HACE FALTA? 
' NO HACE FALTA SINO DEBERIAMSO AJUSTAR EL INTERLINEADO AL DEFAULT Y DEBERIAMOS REPETOR 
' EL SELECT CASE INICIAL DE ROOLLOOP O SEA ESTO FUNCIONE EN CONTROL-M TAMBIEN POR COMODIDAD EL USUARIO
' EL CAMBIO DE FONT PARA CONTROL-M JODE SACAMOS TAMBIEN
 ' --------------
 ' 24-06-2021 espaciado de lineas (2)
' If MultiKey(SC_CONTROL) And lockip=1 And cargacancion=0 Then
'    If incWheel < 0 Then
'       deltaipf=deltaipf + 1
'    EndIf
'    If Incwheel > 0 Then
'       deltaipf=deltaipf - 1
'    EndIf
'      deltaip=deltaipf
'      incWheel=0
'   ' ojo con los Exit Do si por defautl entra al if y hace exit do 
'   ' nunca ejecuta GetMouse y no anda el mouseButtons and 1 o sea el click
'    
' EndIf 
' If MultiKey(SC_CONTROL) And  MultiKey(SC_T) And trasponer=0  Then
'  ' trasponer notas 24-06-2021 - por teclado para todas las notas cargadas
'  ' si subo con flecha arriba sube 1 semitono
'  ' si bajo con flecha bajo un semitono
'         trasponer= 1
'   '      
'   ' ojo con los Exit Do si por defautl entra al if y hace exit do 
'   ' nunca ejecuta GetMouse y no anda el mouseButtons and 1 o sea el click''
'
' EndIf
 
 
 '-------------------------------------END SCREENEVENT ----------
 If cargacancion=0 Then ' evita flickr por carga de cancion
   GetMouse mouseX, mouseY, , MouseButtons   ' <=======  CLICK EVENTOS
 EndIf
 If (mouseY >= edity1 ) And (mouseY <= edity2) Then
  If (mouseX >= 36) And (mouseX <= 70) And (menuNew=2 Or menuNro=2)  Then
  ' =====> EDIT <===
   'SI ADEMS SEUSA CTRL-M SEPUEDE modificar ,agregr acordes e insertar
   ' 1 o varias notas en forma horizontal siemrpe la misma nota
   ' para acorde usar modificar SC_X
   menuMouse = 0
   ' ------ 04-03-21 aca siempre es edicin nueva no modificcion, o solo lectura
   cursorVert = 0
   cursorHori = 0
   agregarNota = 0
   menuMouse = 0
   carga=0
   '-----fin 2609 referencia de busqueda
   '''If MouseButtons And 1 Then ' <========= EDICION SOLO INGRESO DE NOTAS NUEVAS
   If MousePress = 1 Then ' no se si funciona mejor lo dejaremos un tiempo
    If s3 = 0  Then
     COMEDIT = TRUE 
     '       print #1, "INVESTIGO COMEDIT ENTRO X TRUE EN MAIN S3: ",S3
     ''' font = 18 SACAMOS AHORA FUNCIONA PROPORCIONALMENTE 
     curpos=0
     If s3=0 Then
       guardopos=posicion
     EndIf
     Print #1,"1) s3, guardopos ",s3, guardopos
     ''mayorDurEnUnaPosicion (posn)
     '' calcCompas(pos)
     s3 = 1
     controlEdit=0
     posishow=posicion
     Exit Do
    Else
     If s3=1 Then  
        COMEDIT = FALSE '': s3 = 0 ' solo LECTURA 06-12-2021
     '       print #1, "INVESTIGO COMEDIT ENTRO X FALSE EN MAIN S3: ",S3
     'posicion= posicion + curPOS ' estaba mal no va 3-3-21 jmg
        If play=0 Then
           Print #1,"2) s3, guardopos ",s3, guardopos
           posicion=guardopos
       '''posicion = posicion + curpos ' 15-09-2021 jmgjmg ok mejoró 
           curpos=0
           controlEdit=0 'jmg 09-06-2021
           nota=0
       ''posicion=posicion - NroCol/2

           If posicion < 1 Then
              posicion = 0 ' 20-01-2022 jmg a testear pase a1 y dibuja mal 
           EndIf
           posishow=posicion
        EndIf
        s3=2
        Exit Do
     EndIf
    EndIf
   EndIf
  EndIf
 ' RESIZE OCULTADO TAL VEZ SEA UNA OPCION EN OTRA VERSION O USAR SIN FRAME
 ' O CON FRAME SEGUN EL GUSTO DEL USU AUNQUE NO ES PEFECTO EL RESIZE TIENE
 ' SUS VENTAJAS...USAR UN MARCO O NO  EN LA VENTANA DE EDICION ?
  If usarmarco<>usarmarcoOld  Then
     reiniciar=1
     cairo_destroy(c)
     cairo_surface_destroy( surface )
     'FT_Done_Face( ftface )
     Sleep 1
     Exit Sub 
  EndIf 
  If (mouseX > 0) And (mouseX <= 20 ) And usarmarco=0 Then ' <=== RESIZE
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
' 12-07-2021 mousex > 70  
 If  s5= 0 And mouseX > 350 And mousex < (ANCHO-70) And  usarmarco=0 and mousey < 50 Then
     x1=mouseX: y1=mouseY
     s5=1
     Exit Do
  EndIf
 ' =========> MOVER VENTANA DRAGAR LA CINTA SUPERIOR con el mouse
 ' And menuNro= 1  '''348  (2* ANCHO/3)
 If MouseButtons And 1 And S5=1 And mouseX > ANCHO*7041/d4  And mousex < (ANCHO-70-mxold) And _
    usarmarco = 0 AND mousey < 50 Then
   
   x2=mouseX
   y2=mouseY
 
   x0=x0+x2-x1
   y0=y0+y2-y1
   ScreenControl SET_WINDOW_POS, x0, y0
  ' mientras mantengo presiondo el mouse pudo mover el mouse con la ventana
  ' la performance no es tan buena pero funciona
   Exit Do
   s5=0
  Else
     s5=0
  EndIf
 ''https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-movewindow
 '                           <====== [BOTONES] =======>
 ' 07-08-2021 lugar para test tamaño 10x8
 
 If (mousex>=(ANCHO-40-mxold)) And (mousey <= 16) Then
  If  MouseButtons And 1 Then
   If MessageBox(hWnd,"¿SEGURO FINALIZA? (puede usar  Escape tambien)","Fin RollMusic",4 Or 64) =6 Then
    cairo_destroy(c)
    cairo_surface_destroy( surface )
    FT_Done_Face( ftface )
   
    If play=1 Or playb=1 Then
      alloff (1)
      ThreadDetach(thread1)
    EndIf
    close_port(midiout)
    out_free(midiout)
    Dim ffile As Integer
    ffile=FreeFile
    Open "./RollMusic.ini" For output As #ffile

    If nmxold = 0 Then
       nmxold=mxold
       nmyold=myold
    EndIf   
    If nancho=0 Then
       nancho=ANCHO
       nalto =ALTO
    EndIf    
    If ndeltaip=0 Then
       ndeltaip=inc_Penta
    EndIf
    nanchofig=anchofig
    Print #ffile,font , " font"
    Print #ffile,nmxold, " mxold "
    Print #ffile,nmyold, " myold"
    Print #ffile,nANCHO, " ANCHO"
    Print #ffile,nALTO, " ALTO"
    Print #ffile,ndeltaip, " inc_Penta"
    Print #ffile,nVerEscalasAuxiliares, "nVerEscalasAuxiliares"
    Print #ffile,nanchofig, "nanchofig"
    Print #ffile,nVerCifradoAcordes, "nVerCifradoAcordes"
    
    cerrar ffile
      Sleep 100
    cerrar 0
    End 0
    
   EndIf
  EndIf
 EndIf
' ======> F7  BOTON - 
 If (mousex>=(ANCHO-40-mxold)) And (mousey > 17) And (mousey < 32) Then
  If  MouseButtons And 1 Then
   ''      If COMEDIT = FALSE Then
   ' MOVE VENTANA
   h=ALTO
   ScreenControl GET_WINDOW_POS, x0, y0
   ALTO = ALTO - inc_Penta
   If ALTO <= ALTO * 0.3 Then
    ALTO =  ALTO * 0.3
   EndIf
   '    ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   '    Dim As hWnd hwnd = Cast(hwnd,IhWnd)
     MoveWindow( hWnd , X0 , (Y0+h-ALTO)\2, ANCHO - mxold,ALTO, TRUE )

  EndIf
  Exit Do

  ''  EndIf
 EndIf
 ' BOTON + F8
 If (mousex>=(ANCHO-40-mxold)) And (mousey > 33) And (mousey < 50) Then
  If  MouseButtons And 1 Then
   ''    If COMEDIT = FALSE Then
   ' MOVE VENTANA
   Dim As Integer w,h
   h=ALTO
   ALTO = ALTO + inc_Penta
   If ALTO >= altoInicial - 1  Then
    ALTO = altoInicial  - 1
   EndIf
   MoveWindow( hWnd , X0, (Y0+ALTO-h)\2, ANCHO - mxold,ALTO, TRUE )

  EndIf
  Exit Do ' ACA ESTA BIEN QUE NO PROCESE LO QUE SIGUE

  ''  EndIf
 EndIf



If  mouseY > 50 Then '<=== delimitacion de area de trabajo
  s5=2  ' 04-01-2021
  If s3 = 2 Then  ''06-12-2021 jmg
     s3=0
  EndIf   
  If CANCIONCARGADA Then
     S3=0
  EndIf
  ''s3 = 2 ''20-01-2021 ' otro estado mas?
 ' <==== MENU CONTEXTUAL INSERCION DE ACORDES CON CTRL+ CLICK DERECHO EN LECTURA ================>
 ' 2 casos 
 ' 1) en la posicion elegida ya existe una nota de una melodia o secuencia
 ' 2) no hay nada. En el 1er caso se tomara por omision a la nota como la tónica
 ' y se armara un acorde con la misma duración a la nota elegida...
 ' en el caso 2) se necitara 1ero la entrada de una duracióN o tomar la que ya estaba cargada
 ' en cuyo caso se arma igual que antes el acorde con las selecciones del siguiente menu.
 ' usaremos la denominacion 1,3,5,7,9 ..4,6..11 etc para las posiciones de las notas del acorde.
 ' todo se armara segun la escala elegida previamente...el cual pasmos a desarrollar...16-12-2021
 
 If COMEDIT=FALSE Then

' para ingreser automatico acordes a partir de una TONICA futuro--01-12-2021  

    If MultiKey(SC_CONTROL) And MouseButtons And 2 Then 'yyy


    pasoZona1=0:pasoZona2=0
     Dim As HMENU hpopup1, cancelar,notas3,notas4,notas5,Noinversion,inversion1, inversion2,May6,Sus2
     Dim As HMENU Mayor,Menor,Dis,Mayor7,Menor7,Dom7,Dis7,Mayor9,Menor9,Dis9, notabase,Aum,Menor7b5,Dom75a     
     Dim As HMENU bVII,bVIIMaj7
     Dim As Integer event,Posx,Posy 
    ScreenControl GET_WINDOW_POS, x0, y0
    Print #1,"x0 ,y0 en menu contextual " ,x0,y0
    Print #1,"mousex ,mousey en menu contextual ", mousex,mousey
    Print #1,"Posx ,Posy en menu contextual ", Posx ,Posy
    Print #1,"ANCHO ,ALTO en menu contextual ", ANCHO ,ALTO
    Print #1,"mxold, myold ", mxold,myold
    Print #1,"nmxold, nmyold ", nmxold,nmyold
'
          ' You'd have to get the thread id of the graphics window (GetWindowThreadProcessId), 
' set a WH_GETMESSAGE hook with that thread id and then process the command messages 
' in your hook function. chino basico je                                                                  'WS_THICKFRAME
Dim As hwnd haco
Posx=x0 +mousex -anchofig
Posy=y0 +mousey -40
If mousex -anchofig > (ANCHO-mxold)* 3/5 Then
  Posx=x0+(ANCHO-mxold)* 3/5
EndIf
If mousey -40 > (ALTO-myold) *3/5 Then
  Posy=y0+(ALTO-myold)*3/5
EndIf  
' determinacion de la posicion y duracion en el click del mouse...igual que en Sc_Z
    indicePos=(mousex- gap1 )/anchofig + posishow 
    Print #1,"ACORDES: indicePos ",indicePos
    Rolldur = CInt(Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur)
    Rollnota= CInt(Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).nota)
    If Rollnota = 0 Or Rollnota=181 Or Rolldur=0 or Rolldur=181 Then ' construimos acorde donde no haya nada
       Rollnota = nE 
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).nota = CUByte(nE)
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Rolldur=DUR
       Vaciodur= TRUE
    EndIf   
    If DUR <> Rolldur And DUR > 0 Then
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Vaciodur= TRUE ' cambio la duracion se necesita un RecalCompas
       Rolldur=DUR
       DUR=0
    EndIf
    Print #1,"ACORDES: Rolldur ",Rolldur
    Print #1,"ACORDES: nE ",nE
    Print #1,"ACORDES: nR ",nR
    Print #1,"ACORDES: PianoNota del piano ",PianoNota
    Print #1,"ACORDES: vovlemos a nR  ",PianoNota + SumarnR(PianoNota)
' nE,nR y PianoNota se calculan en creaPenta..solo depende de mousey
' aunque de click derecho no importa el mouse y no depende del click 
' PianoNota=(12-nE +(estoyEnOctava -1) * 13) ' es nR ya lo tengo
' indice de la nota en el vector = nR,vertical como NA NB
' determinacion de la notapiano...
' calcualdo en CreaPenta -> PianoNota= nR - restar (nR) esto para el teclado real pero en roll es nR
' trabajo con Pianonota y luego convietrto a nR de nuevo...,con Pianonota
' para reconvertir volver debo usar SumaNr 
'---------------------------------------------------
' PARA GRABAR el acorde en Roll necesito guadar nE (nota) ,el nro octava,
' y acordeNro son 3 numeros...con ellos recosntruyo el acorde y lo escribo arriba 
' en la octava y en la posuicon gurdada ej CMayor, Dm7, G7 etc 


    ' haco = OpenWindow("Acordes", x0+Posx ,y0+Posy,40,40,WS_VISIBLE Or WS_THICKFRAME , WS_EX_TOPMOST ) 'Or WS_EX_TRANSPARENT  )
' VALOR POR OMISION  NOTA ORIGEN O DE COMIENZO -> 1 tonica
  
' --------------- 24-01-2022 VOY A GRABAR EL ACORDE EN ROLL PARA MOSTRAR SU NOMBRE
' ARRIBA DE LA OCTAVA DONDE SE USA ..CON UN FONT MAS CHICO TAL VEZ ,,,,
' VARIABLE NUEVA acordeNro tipo integer shared 
' la existencia de un acorde la ponemos en .inst con el nro 201 (solo indica buscar acorde)
' de una octava en ocurrencia 12 como las escalas me indica que hay un acorde
' o sea pueden subsistir la informacion de un acorde y una escala auxiliar
' el resto de la informacion del acorde son nE y acordeNro la poneos en la octava
' que no se usa la mas alta que sera diferente segun el tamaño de octavas elegido
' que será la octava hasta+1 ahi tenemos 12 posiciones verticales sin usar
' usamos la correspondiente al nro de octava contando como nE nE=1 octava 1
' hasta nE=8 tenemos 8 octavas..
' CALCULO DE SITIO DONDE GUARDO LA INFO DE ACORDE, EL RANGO ES
' COMIENZO 11 + (hasta-2)*13+1   FINAL 11+ (hasta -1)*13  
' la octava actual es=estoyEnoctava y debo encontrar en que sitio lo grabare.
' la octava inferior 'desde' va siemrpe en el 1er lugar libre en el caso de maximo
' en 103, en default el 1ero seria 90. de ahi en masa sumo la cantidad de octavas
' de 1 a 9 son 9-1 =8 (0,1,2,3,4,5,6,7), de 4 a 8 son 8-4=4 (3,4,5,6)
' 'estoyEnOctva' me da el nro de octava respecto al maximo (0,1,2,3,4,5,6,7)
' en el caso de default dira la 1er octava es 4 pues comienza desde 0 se vera
' en el grafico la octava 3 la 1era que es al 4 del rango total empezando de 
' la octava 0.Luego el lugar se cuenta desde la octava 'desde' en adelante 
' nR=desde
' calculo nR con nE=0 inicial en default = 12-nE+ (estoyEnOctava -1)*13 =12+39=51
' nR=51, indice de roll donde pongo la info d ela existencia de acorde en esa octava
' ocatva 4 default, es la 3 de la capacidad total. Si quiero poner en la siguietne octava
' 12 + 4*13=12+52=64
' lugares nR-> 12-24-36-48-60-72-84 en Notapiano seria
' o sea NB=0 to NA=115  step 12 salta 12 o sea cae en la 13 que es 12 ya que
' Na empieza en 0,(0,1,2,3,4,5,6,7,8,9,10,11) son 12 la proxima es 12 y hubo 
' un salto de 12 y es al 13 si cuento desde 1 en vez de 0
' la ocatava siguietne empieza en 12 +13=25 
' 0,12,25,38,51,64,77,90,103, como vemos salta en 12 
'----------------  
     haco = OpenWindow("",Posx ,Posy,anchofig*2,60,WS_VISIBLE Or WS_THICKFRAME  , WS_EX_TOPMOST Or WS_EX_TOOLWINDOW )''Or WS_EX_TRANSPARENT  )
     UpdateInfoXserver()    
     WindowStartDraw(haco)
       fillrectdraw(2,2,&hffffff)
       TextDraw(2,2,"[X]",&hffffff)
     StopDraw
     hpopup1 =CreatePopMenu()
  
     notas3   =OpenSubMenu(hpopup1,"3 Notas") 'triadas
     notas4   =OpenSubMenu(hpopup1,"4 Notas") ' septimas
     notas5   =OpenSubMenu(hpopup1,"5 Notas") ' novenas ...once 13 
     notabase =OpenSubMenu(hpopup1,"Esta Nota Base...") ' si la nota elegida será Tonica 3era 5ta etc
     cancelar =OpenSubMenu(hpopup1,"<-Cancelar->")
          
     Mayor=OpenSubMenu(notas3,"Mayor, ")
     Menor=OpenSubMenu(notas3,"Menor, m")
     Dis  =OpenSubMenu(notas3,"Dis, º")
     Aum  =OpenSubMenu(notas3,"Aum, +")
     Sus2 =OpenSubMenu(notas3,"Sus2  ")
     bVII =OpenSubMenu(notas3,"bVII = Mayor bajar base 1 semitono ")
     
     Mayor7   =OpenSubMenu(notas4,"May7")
     Menor7   =OpenSubMenu(notas4,"Menor7, m7,-7")
     Menor7b5 =OpenSubMenu(notas4,"Menor7b5, m7(b5),-7(b5)")
     Dom7     =OpenSubMenu(notas4,"Dominante 7, 7")
     Dom75a   =OpenSubMenu(notas4,"Dominante +7, 7#5, 7+5")
     Dis7     =OpenSubMenu(notas4,"Dis 7,o º7")
     May6     =OpenSubMenu(notas4,"May 6,o (6)")
     
     Mayor9=OpenSubMenu(notas5,"Mayor 9, 9")
     Menor9=OpenSubMenu(notas5,"Menor 9, -9")
     Dis9  =OpenSubMenu(notas5,"Dis 9, º9")



     MenuItem (1001,Mayor,"No inv") 'triada
     Menuitem (1002,Mayor,"1era inv o 6") 'triada
     Menuitem (1003,Mayor,"2da inv")  ' triada

     MenuItem (1004,Menor,"No inv")  ' triada
     Menuitem (1005,Menor,"1era inv o 6")  ' triada
     Menuitem (1006,Menor,"2da inv")  ' triada
     
     MenuItem (1007,Dis,"No inv")  ' triada
     Menuitem (1008,Dis,"1era inv o 6")  ' triada
     Menuitem (1009,Dis,"2da inv")  ' triada
     
     MenuItem (1010,Aum,"No inv")  ' triada
     Menuitem (1011,Aum,"1era inv o 6")  ' triada
     Menuitem (1012,Aum,"2da inv")  ' triada


' ------------------------------------------------------
     MenuItem (1013,Mayor7,"No inv ")
     Menuitem (1014,Mayor7,"1era inv o 6")
     Menuitem (1015,Mayor7,"2da inv")
     Menuitem (1016,Mayor7,"3era inv")
     
     MenuItem (1017,Menor7,"No inv ")
     Menuitem (1018,Menor7,"1era inv o 6")
     Menuitem (1019,Menor7,"2da inv")
     Menuitem (1020,Menor7,"3era inv")
     
     MenuItem (1021,Menor7b5,"No inv ")
     Menuitem (1022,Menor7b5,"1era inv o 6")
     Menuitem (1023,Menor7b5,"2da inv")
     Menuitem (1024,Menor7b5,"3era inv")
     
     MenuItem (1025,Dom7,"No inv")   ' domianante 7 o M7
     Menuitem (1026,Dom7,"1era inv o 6")
     Menuitem (1027,Dom7,"2da inv")
     Menuitem (1028,Dom7,"3era inv")

     MenuItem (1029,Dom75a,"No inv")   ' domianante 7 5 aumentda
     Menuitem (1030,Dom75a,"1era inv o 6")
     Menuitem (1031,Dom75a,"2da inv")
     Menuitem (1032,Dom75a,"3era inv")

     MenuItem (1033,Dis7,"No inv") ' disminuida
     Menuitem (1034,Dis7,"1era inv o 6")
     Menuitem (1035,Dis7,"2da inv")
     Menuitem (1036,Dis7,"3era inv")

     MenuItem (1037,May6,"No inv   6") ' Mayor 6ta
     Menuitem (1038,May6,"1era inv o 6/3ERA")
     Menuitem (1039,May6,"2da inv  6/5ta" )
     Menuitem (1040,May6,"3era inv 6/6ta ") ' C6/A =Am7 
'----sus2 triada     
     MenuItem (1041,Sus2,"No inv")  ' triada
     Menuitem (1042,Sus2,"1era inv o 6")  ' triada
     Menuitem (1043,Sus2,"2da inv")  ' triada
' ---fin sus2 triada
'---bVII triada y cuatriada 
     MenuItem (1044,bVII,"1era inv o 6")  ' triada
     Menuitem (1045,bVIIMaj7,"1era inv o 6")  ' cuatriada
'     Menuitem (1046,bVII,"2da inv")  ' triada

' .--fin bVII 
'--------------------------------------------------     
 '    MenuItem (1037,Mayor9,"No inv")
 '    Menuitem (1038,Mayor9,"1era inv o 6")
 '    Menuitem (1039,Mayor9,"2da inv")

 '    MenuItem (1040,Menor9,"No inv")
 '    Menuitem (1041,Menor9,"1era inv o 6")
 '    Menuitem (1042,Menor9,"2da inv")
     
'     MenuItem (1043,Dis9,"No inv")
 '    Menuitem (1044,Dis9,"1era inv o 6")
 '    Menuitem (1045,Dis9,"2da inv")

 '    MenuItem (1046,notabase,"Es Tonica")
' aca puedo decir que la base tonica es la nota del click, Notapiano
' nuevo campp grado 1,2,3,4,5,6,7.8.9.10,11,12
             
     Menuitem (1047,notabase,"Es 3era ")
        
     Menuitem (1048,notabase,"Es 5ta ")
        
     Menuitem (1049,notabase,"Es 7ma ")
        
' es 4ta, 6ta, 9na, 11a  ¿? podriamo agregar
     Menuitem (1050,notabase,"Es 4ta o 11")
        
     Menuitem (1051,notabase,"Es 6ta ")
        
     Menuitem (1052,notabase,"Es 9ma ")
        
     Menuitem (1053,notabase,"Es 11va o 4ta")
        

     MenuItem(1100,cancelar,"Salir")
 
     
     Do
      event=WaitEvent
    mouse_event MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0
    sleep 10
    mouse_event MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0
      
      If event=EventMenu then
       Select case EventNumber
' TRIADAS   
         Case 1001
       'NO INVERSION Mayor   C E G
      armarAcorde(grado ,4, 7, 0) ' mayor 4, 7
      acordeNro=1
         Case 1002      
        '1ERA INVERSION MAYOR  E G C
      armarAcorde(grado ,-5, -8, 0)
      acordeNro=2
      
         Case 1003
        ' 2DA INVERSION MAYOR  G C E
      armarAcorde(grado ,4, -5, 0)
      acordeNro=3
' -----------------
         Case 1004 ' Menores <------------No inversion
      armarAcorde(grado ,3, 7, 0) ' menor 3,7 C, Eb, G
      acordeNro=4  
         Case 1005
      armarAcorde(grado ,-5, -9, 0) ' Eb, G ,C ' menor 1era inversion
      acordeNro=5 
         Case 1006
      armarAcorde(grado ,3, -5, 0) ' G ,C , Eb' menor 2da inversion

' -----------disminuida
         Case 1007
      armarAcorde(grado ,3, 6, 0) ' disminuida 3,6 C,Eb,Gb
      acordeNro=7
         Case 1008 ' 
      armarAcorde(grado ,-4, -9, 0) '  Eb, Gb, C dism 1era inv
      acordeNro=8
         Case 1009
      armarAcorde(grado ,3, -4, 0) '  Gb, C , Eb dism 2da inv
      acordeNro=9
' ------------aumentada
         Case 1010
       'NO INVERSION Mayor   C E G#
      armarAcorde(grado ,4, 8, 0) ' mayor 4, 7
      acordeNro=10

         Case 1011
        '1ERA INVERSION MAYOR  E G C ..-8 -4 0
      armarAcorde(grado ,-4, -8, 0)
      acordeNro=11       

         Case 1012
        ' 2DA INVERSION MAYOR  G C E  -4 0 4
      armarAcorde(grado ,4, -4, 0)
      acordeNro=12
     
      
' -------------FIN TRIADAS --mas abajo estasn las sus2 son triadas tambien
' Cuaternario 
         Case 1013 ' mayor 7 no inversion
      armarAcorde(grado ,4, 7, 11) ' mayor 4, 7,11 C,E,G,B
      acordeNro=13   
         Case 1014 ' mayor 7 1er inversion E,G,B,C
      armarAcorde(grado , -8, -5, -1)       
      acordeNro=14
         Case 1015 ' MAYOR 7 2da inv G,B,C,E 
      armarAcorde(grado , -5, -1, 4)
      acordeNro=15     
         Case 1016 'mayor 7 3era inversion B,C,E,G
      armarAcorde(grado , -1, 4, 7)
      acordeNro=16
' --Menor 7 o m7--------------      
         Case 1017
      armarAcorde grado ,3, 7, 10 ' menor 3,7,10  ej D:  D, F, A, C
      acordeNro=17           
         Case 1018
      armarAcorde grado , -9, -5, -2 ' F, A ,C, D ' menor 1era inversion
      acordeNro=18     
         Case 1019
      armarAcorde grado ,-5, -2, 3 ' A ,C , D, F  ' menor 2da inversion
      acordeNro=19           
         Case 1020
      armarAcorde grado , -2, 3, 7 ' C , D, F, A ' menor 3era inversion
      acordeNro=20      
'---Menor7 b5  m7b5 o 
         Case 1021
      armarAcorde grado ,3, 6, 10 ' menor 3,7,10  ej D:  D, F, Ab, C
      acordeNro=21           
         Case 1022
      armarAcorde grado , -9, -6, -2 ' F, Ab ,C, D ' menor 1era inversion
      acordeNro=22     
         Case 1023
      armarAcorde grado ,-6, -2, 3 ' Ab ,C , D, F  ' menor 2da inversion
      acordeNro=23           
         Case 1024
      armarAcorde grado , -2, 3, 6 ' C , D, F, Ab ' menor 3era inversion
      acordeNro=24      

' ----Dom7 o 7 ---------------           
         Case 1025              
      armarAcorde  grado, 4, 7, 10 ' 7   C(0),E(4),G(7),Bb(10)  0,4,7,10  sin inversion
      acordeNro=25 
         Case 1026              
      armarAcorde  grado,-8,-5,-2 ' E(-8),G(-5),Bb(-2),C(0)   1era inversion
      acordeNro=26   
         Case 1027
      armarAcorde  grado,-5,-2, 4           'G(-5) Bb(-2) C E(4) 2da inversion
      acordeNro=27      
         Case 1028
      armarAcorde  grado,-2, 4, 7         'Bb(-2) C E(4) G(7) 3ERA INVERSION
      acordeNro=28
' ----Dom7a o 7#5 7+5 ---------------           
         Case 1029              
      armarAcorde  grado, 4, 8, 10 ' 7   C(0),E(4),Ab(8),Bb(10)  0,4,7,10  sin inversion
      acordeNro=29 
         Case 1030              
      armarAcorde  grado,-8,-4,-2 ' E(-8),Ab(-4),Bb(-2),C(0)   1era inversion
      acordeNro=30   
         Case 1031
      armarAcorde  grado,-4,-2, 4   'Ab(-4) Bb(-2) C E(4) 2da inversion
      acordeNro=31      
         Case 1032
      armarAcorde  grado,-2, 4, 8   'Bb(-2) C E(4) Ab(8) 3ERA INVERSION
      acordeNro=32


'---Dis7-------------------------
           
         Case 1033
      armarAcorde grado ,3, 6, 9 ' DIS 3,6,9  ej D:  C, Eb, Gb, A
      acordeNro=33           
         Case 1034
      armarAcorde grado , -9, -6, -3 ' Eb, Gb ,A, C ' DIS 1era inversion
      acordeNro=34     
         Case 1035
      armarAcorde grado ,-6, -3, 3 ' Gb ,A , C, Eb  ' Dis 2da inversion
      acordeNro=35           
         Case 1036
      armarAcorde grado , -3, 3, 6 ' A , C, Eb, Gb ' Dis 3era inversion
      acordeNro=36      
'---------------------Mayor 6ta ' CEGA , C-A INTERVALO DE 6TA 
'https://javi29clases.blogspot.com/2012/07/acordes-mayores-con-sexta-6-en-piano.html
'Posición Fundamental: Formados por Tónica, su 3ra Mayor, su 5ta justa y su 6ta
'inversión desde 3ra:  3ra Mayor , 5ta justa, 6ta y Tónica(1ra)
'inversión desde su 5ta: 5ta justa, 6ta,  Tónica y su 3ra Mayor
'inversión desde su 6ta: 6ta,  Tónica,   su 3ra Mayor y su 5ta justa        
         Case 1037 ' 
      armarAcorde grado ,4,7,9   ' C,E,G,A ....C6
      acordeNro=37   
         Case 1038 ' 1ERA INVERSION  
      armarAcorde grado ,-3,-5,-8   ' E,G,A,C ...C6/E E-C INTERVALO DE 6TA
      acordeNro=38   

         Case 1039 ' 2DA INV 
      armarAcorde grado ,-3,-5,4   ' G,A,C,E ...C6/G G-E INT 6TA
      acordeNro=39   

         Case 1040 '3ERA INV
'https://javi29clases.blogspot.com/2012/07/acordes-mayores-con-sexta-6-en-guitarra.html          
      armarAcorde grado ,-3,4,7   ' A,C,E,G ...C6/A =Am7, Am7/C=C6  
      acordeNro=40   
'--------------------------------
' -----------------------Sus2 triada
         Case 1041   'Sus2,"No inv")  ' triada
      armarAcorde(grado ,2, 7, 0) ' CSus2
      acordeNro=41
         
         Case 1042  ' Sus2,"1era inv o 6")  ' triada
      armarAcorde(grado ,-5, -10, 0) 'Csus2/D
      acordeNro=42

         Case 1043 'Sus2,"2da inv")  ' triada
      armarAcorde(grado ,-5, 2, 0) 'Csus2/G
      acordeNro=43
'--------------fin sus2 triada
' Acodes No Diatonicos https://guitarmonia.es/los-acordes-bvii-y-bviimaj7/
' bVII bemol septimo grado triada
         Case 1044 
      armarAcorde(grado ,-5, -8, 0)
      acordeNro=44

         Case 1045  
      armarAcorde(grado ,-5, -8, -1) ' 1era inversion  Bb y A juntos...¿? o -13
      acordeNro=45
           
         Case 1046 

         Case 1047  

         Case 1048  
            grado=5
         Case 1049  
            grado=7
         Case 1050  
            grado=9
         Case 1051  
            grado=11
         Case 1052  
            grado=13
         Case 1100 ' es Salir
         
       End Select
' grabacion en Roll y track
Dim As INTEGER vacio
vacio= 12 +(estoyEnOctava-1)*13
' marcamos en el interespacio con 201 en pb para indicar que hay acorde
' la info restante la ponemos arriab de todo en la octava que no se usa..
' o sea estoy indicando que en esta octava y en esta posicion hay un acorde
' y solo puede ahber uno de modo que la relacion es 1:1
' estoyEOCtava y acordeNro y la RollNota  dan el acorde compelto
'Dim NotaAcorde As String
'NotaAcorde=NotasGuia(nE-1) ' c,c#,d,d#..etc
' 26-01-2022 por choque con escalas cambio inst a pb
 Roll.trk(indicePos, vacio).pb=201 


' CALCULO DE POSICION DE LA INFORMACION DE ACORDES:
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default ->11+ 6*13 +1=90  ==> 11 + 7*13=102
' PARA EL MAXIMO SERIA
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default 9-2 ->11+ 7*13 +1=103  ==> 11 + 8*13=115
' O sea maximo desde 103 a 115 son las posiciones libres...
' vamos a reservar en una posicion dada para la info de acordes por ejemplo en la 
' maxima 103 para octava0,104 octava1,105 octava2, 106 oct3,107 oc4,108 oct5
' 109 oct6, 110 oct 7...ergo quedan libres 111,112,113,114,115
' 1) => vacio=11 + (hasta-2)*13+1 ' 90 es para la 1er octava , defULT 3,4,5,6 APARECEN
' EN GRAFICO, PERO SON LAS OCTAVAS 4,5,6 Y7 4 ES LA 1ER OCTAVA DE LO QUE SE MUESTRA
' 5 LA 2DA Y ASI SUCECIVAMENTE, ERGO la posicion es en vez de 90
Dim As Integer verticalEnOctavaVacia '  6-4 =2
' 90,91,92,93,95 la default tomara la posicion vertical 
' 2) => verticalEnOctavaVacia= vacio + estoyEnOctava - desde   
' en un solo paso, linea de la octava libre que contendra la info dela corde:
verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde ' 90 + 6 - 4=92 

 Roll.trk(indicePos,verticalEnOctavaVacia ).nota = CUByte(RollNota)
 Roll.trk(indicePos,verticalEnOctavaVacia ).dur  = CUByte(acordeNro)
 Roll.trk(indicePos,verticalEnOctavaVacia ).vol  = CUByte(estoyEnOctava) ' para pasar a Track
 Roll.trk(indicePos,verticalEnOctavaVacia ).pb   = 202
' con esta info reconstruyo el acorde que luego muestro solo en la octava
' la octava la se por donde esta el 201 no hace falta guardarla, pweo para pasarla a track si
' hace falta!! 27-01-2022 -. 
' en uso. 
/'
If nota3era <> 0 Then 
        nota3era= lugarNota(deltanota(aconro))
        
EndIf
If nota5ta <> 0 Then 
        nota5ta= lugarNota(nota5ta)
EndIf
If nota7ma <> 0 Then 
        nota7ma= lugarNota(nota7ma)
EndIf
'/
           Delete_Menu (hpopup1)            
           Close_Window(hpopup1)
           Close_Window(haco)
           Exit Do 
       
      ElseIf event=eventrbdown Then
          DisplayPopupMenu(hpopup1, GlobalMouseX,GlobalMouseY)
      EndIf
 
     Loop  
        
    EndIf
'=========================================================================    
' [[[[18-01-2022 menu alternativo con las 58 formas de acorde jjj
'  cambiar todo los popup menus por solamente un scroll ...futuro]]] 
    If MultiKey(SC_LSHIFT) And MouseButtons And 2 Then 'yyy


    pasoZona1=0:pasoZona2=0
     Dim As Integer event,Posx,Posy 
    ScreenControl GET_WINDOW_POS, x0, y0
    Print #1,"x0 ,y0 en menu contextual " ,x0,y0
    Print #1,"mousex ,mousey en menu contextual ", mousex,mousey
    Print #1,"Posx ,Posy en menu contextual ", Posx ,Posy
    Print #1,"ANCHO ,ALTO en menu contextual ", ANCHO ,ALTO
    Print #1,"mxold, myold ", mxold,myold
    Print #1,"nmxold, nmyold ", nmxold,nmyold
'
          ' You'd have to get the thread id of the graphics window (GetWindowThreadProcessId), 
' set a WH_GETMESSAGE hook with that thread id and then process the command messages 
' in your hook function. chino basico je                                                                  'WS_THICKFRAME
Dim As hwnd haco
Posx=x0 +mousex -anchofig
Posy=y0 +mousey -40
If mousex -anchofig > (ANCHO-mxold)* 3/5 Then
  Posx=x0+(ANCHO-mxold)* 3/5
EndIf
If mousey -40 > (ALTO-myold) *3/5 Then
  Posy=y0+(ALTO-myold)*3/5
EndIf  
' determinacion de la posicion y duracion en el click del mouse...igual que en Sc_Z
    indicePos=(mousex- gap1 )/anchofig + posishow 
    Print #1,"ACORDES: indicePos ",indicePos
    Rolldur = CInt(Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur)
    Rollnota= CInt(Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).nota)
    If Rollnota = 0 Or Rollnota=181 Or Rolldur=0 or Rolldur=181 Then ' construimos acorde donde no haya nada
       Rollnota = nE 
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).nota = CUByte(nE)
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Rolldur=DUR
       Vaciodur= TRUE
    EndIf   
    If DUR <> Rolldur And DUR > 0 Then
       Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Vaciodur= TRUE ' cambio la duracion se necesita un RecalCompas
       Rolldur=DUR
       DUR=0
    EndIf
    Print #1,"ACORDES: Rolldur ",Rolldur
    Print #1,"ACORDES: nE ",nE
    Print #1,"ACORDES: nR ",nR
    Print #1,"ACORDES: PianoNota del piano ",PianoNota
    Print #1,"ACORDES: vovlemos a nR  ",PianoNota + SumarnR(PianoNota)
' nE,nR y PianoNota se calculan en creaPenta..solo depende de mousey
' aunque de click derecho no importa el mouse y no depende del click 
'PianoNota=(12-nE +(estoyEnOctava -1) * 13) ' es nR ya lo tengo
' indice de la nota en el vector = nR,vertical como NA NB
' determinacion de la notapiano...
' calcualdo en CreaPenta -> PianoNota= nR - restar (nR) esto para el teclado real pero en roll es nR
' trabajo con Pianonota y luego convietrto a nR de nuevo...,con Pianonota
' para reconvertir volver debo usar SumaNr 


    ' haco = OpenWindow("Acordes", x0+Posx ,y0+Posy,40,40,WS_VISIBLE Or WS_THICKFRAME , WS_EX_TOPMOST ) 'Or WS_EX_TRANSPARENT  )
' VALOR POR OMISION  NOTA ORIGEN O DE COMIENZO -> 1 tonica
  
' --------------- 18-01-2022
 

'----------------  
     haco = OpenWindow("Acordes",Posx ,Posy,anchofig*6,720, , WS_EX_TOPMOST  )''Or WS_EX_TRANSPARENT  )
     hwndListBox= ListBoxGadget(4,10,20,240,650,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )
     UpdateInfoXserver()    
     WindowStartDraw(haco)
       fillrectdraw(2,2,&hffffff)
       TextDraw(2,2,"[Lista Acordes]",&hffffff)
     StopDraw
 
 
ButtonGadget(2,530,30,50,40," OK ")
'       ButtonGadget(3,530,90,50,40,"+Pag")
         #Ifdef __FB_WIN64__
           SetFocus (hwndListBox) 
           SetForegroundWindow(haco)
          #Else
           gtk_widget_grab_focus(GadgetID(4))
         #EndIf
         Do

         Var eventC= waitEvent

          If eventC=eventgadget Then
          
            If eventnumber()=2 Then
               'Instru = GetItemListView()
              ' print #1,"in = ",in 
             ' If instru=0 Then  instru=1 EndIf 
               
              ' If in >= 1 And in <=127 Then
              '   instru = IndiceInstAlfa(instru)
              ' EndIf
'''               instru=instru + 1
            ''   If instru > 1 Then
                  Close_Window(haco)
                  Exit Do
           ''    EndIf
            End If

          EndIf 
          Sleep 5  
          
         Loop
         
     
    EndIf     
    
   s2=0 :s1= 0 ' 10-12-2021 wheel no se movia ** ES SUFICIENTE??? CUANDO NO SE MOVIA?? 
   '                      RECORDDAR TEST CASE
 ' lockip=0   ' 10-12-2021 wheel no se movia ***JMG OJO JODE INTERLINEADO VER MAS COMENTADO

' ========================================================  
' SELECCION DE ZONA PARA TRASPONER, VOLUMEN, INSTRUMENTO, ETC ETC
' SOLO SELECCIONO PASO DESDE HASTA y/o NOTA. 
' usaremos tambien (en desarrollo futuro) para borrar un intervalo ya sea de 
' la octava elegida o todas las octavas o desde una nota hasta otra ....¿?
' clave DEVF (desarrollo futuro)
' esto funciona solo en modo lectura asi que lomovere ahi

  If MultiKey(SC_CONTROL) And MousePress= 1  Then
     Dim As Integer pasox, pasoy, pasonR
     pasox=(mousex- gap1 )/anchofig  + posishow  
     pasoy=nE
     print #1,"pasoy nE=",pasoy
     If trasponer=1 Then '03-02-2022
       correcciondeNotas(Roll)
     EndIf
     If pasoZona1 = 0 Then  ' selecion 1er posicion de la zona
        pasoZona1=  pasox ' pos de la 1er ventana
        pasoNota=0 
        print #1,"pasoZona1=",pasoZona1;" pasoNota=";pasoNota
        Exit Do
     EndIf

     If pasoZona1 > 0 And pasoZona1 <> pasox Then ' posicion 2 de la zona
        pasoZona2= pasox
        pasoNota=0
    '    print #1,"pasoZona2=",pasoZona2;" pasoNota=";pasoNota
        Exit Do
     EndIf
     If pasoZona1=pasoZona2 And pasoNota<>pasoy Then 
        pasoNota=nE
     '   print #1,"pasoNota=",pasoNota
        Exit Do
     Else
        pasoNota=0   
     EndIf

     If pasoZona1 > 0  And pasoZona1 = pasox Then ' la zona es solo  1 sola columna
        pasoZona2= pasox
        pasoNota=0 ' 28-06-2021 mueve acorde si existe , sino meuve nota 
     '   print #1,"pasoZona1 iguales pasoZona2=",pasoZona2;" pasoNota=";pasoNota
     EndIf

  EndIf 




' FIN SELECION ZONA
' cambio de lugar ...
' 24-06-2021 espaciado de lineas (1)
'If MultiKey(SC_CONTROL) And lockip=0  Then
'    deltaip=0
'    incWheel=0
'    lockip=1
'    Exit Do 
'
'EndIf 

'            <===== RESIZE ===================>
 If resize = TRUE And usarmarco=0 Then ' <=====  MOVER Y REDIMENSIONAR LA PANTALLA NO TAN FACIL
  'CLICKEAR CERCA DEL CENTRO Y DRAGAR DERECHA IZQUIERDA ARRIBA ABAJO
  m.res = GetMouse( m.x, m.y, m.wheel, m.buttons, m.clip )
  If m.buttons = 1 And (m.x > 5 ) And (m.y > 5 ) Then
   'dim as integer desktopwidth,desktopheight

   ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
   Dim As hWnd hwnd = Cast(hwnd,IhWnd)
   ' m.x The new position of the left side of the window.
   ' m.y The new position of the top of the window.
   ' 07-08-2021 cambien m.x/2 y m.y/2  por 1 y 1
   
   MoveWindow( hWnd , 1, 1 , ANCHO - mxold, ALTO - myold, TRUE )
   mxold=m.x
   myold=m.y
  EndIf
  
  
  Exit Do

 EndIf
 '' 26-01-2022 espaciado de lineas (1) movido desde 2190 afecta a acordes
 If MultiKey(SC_CONTROL) And lockip=0   Then '''' ççç REVISAR !!!!! 30-01-2022
    deltaip=0
    incWheel=0
    lockip=1
    'Exit Do    
 EndIf 
 ' 26-01-2022 espaciado de lineas (2) movido desde 2713 afecta a acordes

 If MultiKey(SC_CONTROL) And lockip=1 And cargacancion=0  Then
    If incWheel < 0 Then
       deltaipf=deltaipf + 1
    EndIf
    If Incwheel > 0 Then
       deltaipf=deltaipf - 1
    EndIf
      deltaip=deltaipf
      incWheel=0
   ' ojo con los Exit Do si por defautl entra al if y hace exit do 
   ' nunca ejecuta GetMouse y no anda el mouseButtons and 1 o sea el click

 EndIf 
 If  MultiKey(SC_ALT) And (SC_T)Then

      If trasponer=0  Then
         trasponer= 1
       EndIf  
   ' ojo con los Exit Do si por defautl entra al if y hace exit do 
   ' nunca ejecuta GetMouse y no anda el mouseButtons and 1 o sea el click'
 EndIf


 If MultiKey(SC_ALT) And MouseButtons And 1  Then 'posiciona el cursor
    ' habilito trasposicion de una sola nota, ejecuta solo con Ctrl-T previo y
    ' las flechas up/down, habilitare dragado tambien 02-07-2021
'    pasoNota=nE
    Print #1,"MARCAR CON ALT Y 13 UNA NOTA "
    indicePos=(mousex- gap1 )/anchofig + posishow
    Print #1,"MARCAR CON ALT Y 13 UNA NOTA ,INDICEPOS",indicePos 
' grupo de notas seleccionadas poniendo un 13 en nota
   Roll.trk(indicePos,nR ).nota = 13 ' marcamos para mover 
   Print #1,"MARCAR CON ALT Y 13   nR ", nR
   SelGrupoNota =1
   ' el valor correcot lo repone la sub correcionnotas
   ' luego puedo mover 1 sola nota o todas las marcadas con 13  
        
 EndIf 


 '-------------------- ERROR DE 10-12-2021
 ''!!! NUNCA PONER UN EXIT DO POR UN ELSE O AL FIANL PORQUE NO SE PROCESA NADA DE LO QUE SIGUE!!!!
 '------DEJO DE ANDAR SC_z POR QUE QUI HABIA UN EXIT DO  
 EndIf 
'' >>>========FIN COMEDIT=FALSE
'' >>>========FIN COMEDIT=FALSE 
 '         <==== MENU BORRAR INSERTAR MODIFICAR ================>
 ' savemousex=0 : savemousey=0 LO QUE HACE ES PERMITIR QUE EL MENU APARESCA EN OTRO LADO
 ' DONDE SE CLICKEA, Y SI SON >0 DEJA QUE PERMANESCA VISUALMNETE
 ' HASTA EL PROXIMO CLICK IZQUIERDO, POR ESO SE PASAN A CERO SOLO EN EL ULTIMO CLICK
 ' IZQUIERDO (2 o 1 segun la cantidad necesaria para ejecutar el comando)
 '  MOMENTO EN EL QUE SE EJECUTA EL COMANDO Y SE VE EL CAMBIO.
 '       ==== NOTAS O DURACIONES EXISTENTES ====

 If  COMEDIT=TRUE  Then
    If  MultiKey(SC_CONTROL) And (MouseButtons And 2)  Then
   ' trae un menu contextual solo con ctrl-m  previo <==== menu borrar insertar modificar
   ' ESTADO:CALL MENU COMANDO
     ayudaModif=TRUE
     ayudaNuevaNota=FALSE
     menuMouse = 0
     nroClick=1
     cursorVert = 1
     cursorHori = 1

 '  print #1, "------------------------------------------------------------"
 '  Print  #1,"(1) MultiKey(SC_CONTROL) And (MouseButtons And 2) And COMEDIT=TRUE"
 '  print #1, "sc_CONTROL + MB2 + CE=TRUE <= ESTADO:CALL MENU COMANDO"
 '  Print  #1, " ayudaModif=TRUE"
 '  Print  #1," ayudaNuevaNota=FALSE"
 '  Print  #1," menuMouse = 0"
 '  Print  #1," nroClick=1 "
 '  print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
 '  print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

     Exit Do
    EndIf
 
    If  ayudaNuevaNota=FALSE And ayudaModif=TRUE Then 'ESTADO: seleccionar comando
     If (mouseButtons And 1 )  And nroClick = 1  Then
        ayudaModif=FALSE
        menumouse=0
        posinterna=0
        cursorVert = 1:cursorHori = 1
   '  print #1, "------------------------------------------------------------"
   '  Print  #1,"(2) (mouseButtons And 1 ) And ayudaModif=TRUE And nroClick = 1 And COMEDIT=TRUE "
   '  Print  #1,  "And ayudaNuevaNota=FALSE "
   '  Print  #1,"ESTADO: seleccionar comando "
   '  Print  #1," ayudaModif=FALSE"
   '  Print  #1," menumouse=0"
   '  Print  #1," posinterna=0"
   '  print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   ' Necesitamos mostrar el menu todavia  savemousex=0 : savemousey=0
       If nroClick = 1 Then 'seleccionamos verticalmente ,,,,
          nroClick=2
          If (mousey >= usamousey -120) And  (mousey <= usamousey -100) Then
            modifmouse=1 'borrar =1
            notacur=nE
            curpos=(mousex- gap1 )/anchofig
            Exit Do
          EndIf
          If (mousey >= usamousey -100) And  (mousey <= usamousey -70) Then
            modifmouse=2 'INSERTAR
            Exit Do
          EndIf
          If (mousey >= usamousey -70) And  (mousey <= usamousey -40) Then
            modifmouse=3 'FIN INSERTAR
            Exit Do
          EndIf
         If (mousey >= usamousey -40) And  (mousey <= usamousey -10) Then
            modifmouse=4 'CAMBIADUR=1 modificar
            notacur=nE
            curpos=(mousex- gap1 )/anchofig
            Exit Do
         EndIf

       EndIf
     EndIf
    EndIf
 ' <===========   MODIFICACIONES INSERCION
 ' por ahor ainsercion con mouse funciona igual que con keys
 ' perola duracion entrpro tecldonos e porqu enotoma l demouse.
 ' luegode la 1erinsercion con solo click izquierdo repito otra insercion
 ' en lineahorizontal siqueiro otraduracion debere elegirotracon teclado
 ' elcursor seguira al click izq por elcalculo de cursor en este comando
 ' finalmente se usalatecla END para finalizar todas las inserciones.
 ' debere agregr un nuevo comando END paraahcerlo con elmouse....
   If   ayudaModif=FALSE Then
     If (mouseButtons And 1 )  And nroClick = 2  Then
       savemousex=0 : savemousey=0 ' 
   ' ESTADO: PREPARA COMANDO
   'print #1, "------------------------------------------------------------"
   'print #1, "(3) (mouseButtons And 1 ) and ayudaModif=FALSE And nroClick = 2 And COMEDIT=TRUE "
   'print #1, " ESTADO: PREPARA COMANDO"
       notacur=nE
       curpos= Int((mousex - gap1)/anchofig)
       posishow= curpos  + 1 ' NO CAUSA EL +1 EN MODIF MOUSE 03-03-21-15:10
   'print #1, " savemousex=0 : savemousey=0 ' JMG NUEVA"
   'print #1, " notacur=nE"
   'print #1,"curpos= Int((mousex - 81)/20)"
   'print #1," posishow= curpos + 1"
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

       If modifmouse=1 Then  ' anda ok 27 02 2021
          BORRAR=1
          ayudaNuevaNota=TRUE
          Exit Do
       EndIf
       If modifmouse=2 And insert= 0 Then
          INSERT=1
          indaux=0
    '           print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert
         If indaux=0 Then ' no haria falta todas las demas inserciones se deben
     'hacer con I no volver a repetir SC_INSERT sino se pulso SC_END
           StartInsert = posicion + curpos  ' guardo sin modificar el comienzo xxx ok
         EndIf
    '          print #1,">>>SC_INSERT  despues ajuste STARTINSERT ", StartInsert
    ''''Erase (RollAux.trk) ' borro lo que habia en el auxiliar
         ReDim (RollAux.trk) (1 To CantTicks, NB To NA )
    '''Erase (notasInsertadas)
         ReDim notasInsertadas (1 To 1500)
         notins=0
    '          print #1, ">>>SC_INSERT insert indaux borro RollAux.trk: ",insert,indaux
    'sigue el proceso en RollSub->sub cursor
    ' nroclick=0
         Exit Do
       EndIf
       If modifmouse=2 And insert=1 Then
          insert=2
       EndIf
       If modifmouse=3  Then
          If cursorVert=1 Then ' solo vlido con Ctrl-M
             insert=3
             moveresto (StartInsert,indaux, insert,nota)
             insert=0:indaux=0
          EndIf
          nroClick=0 ' no permite entrar mas por insercion
          modifmouse=0
          ayudaNuevaNota=TRUE
    ' acomoda los compases  <======= organiza Compases
          ReCalCompas(Roll) ' organizaCompases()
    ' fin compases
       EndIf
       If modifmouse=4 Then ' modificar
         cambiadur=1
         curpos=(mousex- gap1 )/anchofig
         notacur=nE
         ayudaNuevaNota=TRUE
         Exit Do
       EndIf
     EndIf
   EndIf
 ''
 '                     <=== INICIO  O P I L F E W H

   If (MouseButtons And 2)  Then ''<=== menu de duraciones para seleccionar con click
   ' el resto del code en CreaPenta(), para todaedicion lasduraciones 1 a 8 en letras
     ayudaNuevaNota=TRUE 'ESTADO: CALL MENU DURACIONES O CTRL-M
     ayudaModif =FALSE
     savemousex=0 : savemousey=0 ''ACA NO ¿?
     vuelta=FALSE
     menuMouse = 0

     nroClick=1
   'print #1, "------------------------------------------------------------"
   'print #1,"(MouseButtons And 2) and COMEDIT= TRUE"
   ' Print  #1,"(4) ESTADO: CALL MENU DURACIONES O CTRL-M"
   'Print  #1," ayudaNuevaNota=TRUE "
   'Print  #1," ayudaModif =FALSE"
   'Print  #1," savemousex=0 : savemousey=0 "
   'Print  #1,"  vuelta=FALSE"
   'Print  #1,"  menuMouse = 0"
   'Print  #1,"  DUR=0"
   'Print  #1,"  nroClick=1"
   'Print  #1,"COMEDIT=TRUE "
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
         curpos=(mousex- gap1 )/anchofig '01-07-2021
         notacur=nE
     ' 15-09-2021 no se puede dar ctrl-m mas alla de Maxpos, no sepuede modifar algo
     ' que no existe o si deberia???    
     ' esto es necesario si no agrego las 500 posiciones al cargar el archivo
     ' peo como le agregue posiciones el vector es mas grande que lo cargado
     ' ergo esto no es necesario fue un error sacar las posicioens agregadas
     ' en al carga siemrep tender mas en las edicion 
     'If curpos > Maxpos -2 Or posicion +  curpos > MAxPos -2 Then
     '   nroClick=0
     '   cursorVert = 0   ' JMG ERROR 24-09-2021
     '   cursorHori = 0
     '   mousex=(Maxpos -2 )*anchofig + gap1
     ''   print #1,"mousex ",mousex
     '   print #1,"LINEA 2784 TE CAMBIEN MOUSEX ",mousex
     '   '''savemousex=mousex '''ESTO NO SE PORQU ELO PUSE SACANDO
     '   ayudaNuevaNota=FALSE
     'EndIf   
       
    Exit Do
   EndIf
 
 ' YA VUELVE OK
   If  ayudaModif=FALSE Then
  If (mouseButtons And 1 ) And cursorVert = 1  And modifmouse <> 3 Then ' ESTADO: SELECCIONA VUELTA CTRL-P
   'print #1, "------------------------------------------------------------"
   'Print  #1,"(5) MB1  And AModif=FALSE And CE=TRUE  And CVert = 1 "
   'Print  #1,"5->ESTADO: SELECCIONA VUELTA CTRL-P"
   'Print  #1,"  vuelta=TRUE"
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   vuelta=TRUE
   If mousey <= usamousey -40  And mousey >= usamousey -60 Then
    If mousex >= usamousex -64 And mousex<= usamousex +86 Then
     cursorVert = 0: cursorHori = 0: agregarNota=0:  menuMouse = 0
     ayudaModif=FALSE
     savemousex=0 : savemousey=0
     nroClick=0
     modifmouse=0
     nota=0 ' jmg 03-03-21 22:28 sino le sumaba 1 a Posicion
     ' y trataba de insertarla.....como nota nueva
     'Print  #1,"5-> ctrl-p=> cursorVert = 0: cursorHori = 0: agregarNota=0:  menuMouse = 0"
     'Print  #1,"5-> ayudaModif=FALSE    savemousex=0 : savemousey=0  nroClick=0"

     Exit Do
    EndIf
   EndIf
  EndIf
   EndIf
   If ayudaModif=FALSE And ayudaNuevaNota=TRUE Then ' ESTADO : SELECCIONA DURACION O CTRL-M
  If (mouseButtons And 1 )  And nroClick = 1  Then

   ayudaNuevaNota=FALSE
   savemousex=0 : savemousey=0
   menuMouse = 0
   'print #1, "------------------------------------------------------------"
   'Print  #1,"(6) (mouseButtons And 1 ) And ayudaNuevaNota=TRUE And nroClick = 1 And COMEDIT=TRUE "
   'print   #1," 6->And ayudaModif=FALSE Then ' ESTADO : SELECCIONA DURACION O CTRL-M"
   'Print  #1,"(6)MB1  +d ANN=TRUE + NClick = 1 + CE=TRUE + ayudaModif=FALSE"
   'Print  #1,"(6)ESTADO : SELECCIONA DURACION O CTRL-M , nroClick ", nroClick
   'Print  #1," 6 out-> ayudaNuevaNota=FALSE"
   'Print  #1,"  savemousex=0 : savemousey=0"
   'print  #1,"  menuMouse = 0"
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   ' ACA SERA LA ENTRADA POR MOUSE, DUR SALDRÁ DE LA ELECCION DEL MENU DE DURACION
   ' QUE APARECE CON CLICK DERCHO UBICAMOS LA POSICION RELATIVA Y OBTENEMOS LA DURACION
   ' EN EL 1ER CLICK IZQUIERDO, EN EL 2DO CLICK IZQUIERDO ENVIAMOS NOTA Y DURACION
   If  nroClick =1  Then ' determinmos uracion clickeada o seleccionada graficamente
    If mousey >= usamousey +30  And mousey <= usamousey + 44 Then
     If mousex >= usamousex -55 And mousex<= usamousex +102 Then
      ' ESTADO:SELECCION  CTRL-M
      cursorVert = 1: cursorHori = 1: agregarNota=0:  menuMouse = 0
      ''  ayudaModif=TRUE jmg elmenu no debe aparecer hasta dar ctrl-click derecho
      'print #1,"6 ctrl-M ->cursorVert = 1: cursorHori = 1: agregarNota=0:  menuMouse = 0 "
      'print #1,"6-> ayudaModif=TRUE"

      Exit Do
     EndIf
    EndIf

    If mousex >= usamousex -100 And  mousex <= usamousex -80 Then ' O en -90
     DUR=1
    EndIf
    If mousex >= usamousex -70 And  mousex <= usamousex -50 Then ' P en -60
     DUR=2
    EndIf
    If mousex >= usamousex -40 And  mousex <= usamousex -20 Then ' I en -30
     DUR=3
    EndIf
    If mousex >= usamousex -10 And  mousex <= usamousex +10 Then ' L en 0
     DUR=4
    EndIf
    If mousex >= usamousex +20 And  mousex <= usamousex +40 Then ' F en +30
     DUR=5
    EndIf
    If mousex >= usamousex +50 And  mousex <= usamousex +70 Then ' E en +60
     DUR=6
    EndIf
    If mousex >= usamousex +80 And  mousex <= usamousex +100 Then ' W en +90
     DUR=7
    EndIf
    If mousex >= usamousex +110 And  mousex <= usamousex +130 Then ' H en +120
     DUR=8
    EndIf

   EndIf
  EndIf
   EndIf

   If ayudaNuevaNota=FALSE And ayudaModif=FALSE Then
      If DUR > 0 And nE > 0 And nroClick = 1 And modifmouse<> 3 Then ' ESTADO INGRESA O MODIFICA 1ER NOTA
         nota=nE   ''<== 1er nota ingresada para la duracion y nota elegida
         nroClick=0
 ' print #1, "------------------------------------------------------------"
 ' Print  #1," DUR > 0 And nE > 0 And nroClick = 1 And ayudaNuevaNota=FALSE and COMEDIT=TRUE "
 ' Print  #1," And ayudaModif=FALSE "
 ' Print  #1," (7) ESTADO INGRESA O MODIFICA 1ER NOTA"
 ' Print  #1," 7-><== 1er nota ingresada para la duracion y nota elegida"
 ' Print  #1," 7->nota=nE   ", nE
 ' Print  #1," 7-> nroClick=0"
 ' print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

     EndIf
   EndIf
   If ayudaModif=FALSE And ayudaNuevaNota=FALSE And octavaEdicion = estoyEnOctava Then
     '11-12-2021 uso Ctrl + click para ingresar notas con el mouse sino se ingresa por error
     ' facilmente durante el pasaje de edit a ctrl-m
     If (mouseButtons And 1) And (DUR > 0) And (nE > 0) And modifmouse<> 3 And MultiKey(SC_CONTROL) Then
   'print #1, "------------------------------------------------------------"
   'Print  #1,"(8) (mouseButtons And 1) And (DUR > 0) And (nE > 0) And ayudaNuevaNota=FALSE "
   'Print  #1,"(8)  And COMEDIT=TRUE And ayudaModif=FALSE"
   'print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
     Dim As Integer posdur= (mousex- gap1 )/anchofig + posishow '01-07-2021
     If posdur >= Maxpos - 1 Then  ' no permite entrar notas con click ,ucho antes de maxpos
       nota=nE ' <=== ingresamos varias notas por mouse del mismo valor
     EndIf
   ' hasta que si vuelva a dar click derecho y aparesca de nuevo el menu de duraciones.
   '   Print  #1," nota=nE ", nE
     Exit Do
  EndIf
   EndIf

   If MultiKey (SC_ENTER) And copiar=0 Then
      copiar=1
   EndIf 

   If MultiKey (SC_ENTER) And copiar=2 Then
      copiar=3
   EndIf 

 EndIf ' FIN <========COMEDIT=TRUE
 ' menu contextual
 
' You'd have to get the thread id of the graphics window (GetWindowThreadProcessId), 
' set a WH_GETMESSAGE hook with that thread id and then process the command messages 
' in your hook function. chino basico je


  
 
 ' habilitar una octava para edicion con el mouse
 If  mousex > ANCHO3div4  Then ' 09-06-2021 para que nochoque con boton EDIT
       octavaEdicion=estoyEnOctava
 EndIf




' A PARTIR DE ACA FUNCIONA PARA AMBAS CONDICIONES? PERO LO ESTOY USANDO EN FALSE
' REVISAR...24-01-2022


' If MultiKey(SC_ALT) And MouseButtons And 1 Then 'posiciona el cursor
'    ' habilito trasposicion de una sola nota, ejecuta solo con Ctrl-T previo y
'    ' las flechas up/down, habilitare dragado tambien 02-07-2021
''    pasoNota=nE
    
'    indicePos=(mousex- gap1 )/anchofig + posishow
     
'' grupo de notas seleccionadas poniendo un 13 en nota
'   Roll.trk(indicePos,NA-nR ).nota = 13 ' marcamos para mover 
'   
'   SelGrupoNota =1
'   ' el valor correcot lo repone la sub correcionnotas
'   ' luego puedo mover 1 sola nota o todas las marcadas con 13  
'        
' EndIf 
 
 If MultiKey(SC_M) And MouseButtons  And 1 And moverZona=0 Then  'mover Zona 
' usamos la seleccion de Zona y luego movemos la zona a una posicion dada
    indicePos=(mousex- gap1 )/anchofig + posishow
    moverZona=1 ' solo mueve 1 vez hasta el proximo pulsado de Q evita borrado
    If pasozona1 =0 Then pasozona1=1 EndIf
    moverZonaRoll(indicePos,Roll,pasozona1)
   ''' curpos=posishow-1 ' 26-10-2021 jmg
    Exit Do
 EndIf 

 If copiarZona=0 And MouseButtons  And 1 And MultiKey(SC_C)   Then  'mover Zona 
' usamos la seleccion de Zona y luego movemos la zona a una posicion dada
   print #1,"entra a copiar "
    nota=0
    indicePos=(mousex- gap1 )/anchofig + posishow
    
    Print #1,"VA A COPIAR DESDE POSICION INDICEPOS ",indicePos
    Print #1,"VA A COPIAR DESDE POSICION gap1 ",gap1
    Print #1,"VA A COPIAR DESDE POSICION anchofig ",anchofig
    Print #1,"VA A COPIAR DESDE POSICION POSishow ",posishow
    Print #1,"VA A COPIAR DESDE POSICION mousex ",mousex
    copiarZona=1 ' solo mueve 1 vez hasta el proximo pulsado de Q evita borrado
    If numero=0 Then
    print #1,"entra a copiar numero 0"
       moverZonaRoll(indicePos,Roll,pasozona1)
    Else
     print #1,"entra por Else numero > 0"
       Dim As short lz=0,delta
       delta = pasoZona2 - pasoZona1 + 1
' si la secuencia es chica debo agrandarla jmg   
'--- AUMENTO DE CAPACIDAD DEL VECTOR EN POSICIONES NECESARIAS
   print #1,"DELTA ",delta
    Dim As Integer nuevaspos
    nuevaspos= indicePos + delta * numero
    print #1,"NuevaPos,,", nuevaspos
      
    If CantTicks - MaxPos < nuevaspos  Then
     '  GrabarArchivo(1) ''hacer un backup !!! 
      CantTicks= nuevaspos + 2 ' incremento el tamaño en 1000 posiciones =1 min
      print #1,"incremento final de CantTick ", CantTicks 
      ReDim Preserve (Roll.trk ) (1 To CantTicks,NB To NA)
      ReDim Preserve compas(1 To CantTicks)
      ReDim Preserve (RollAux.trk) (1 To CantTicks, NB To NA)
      ReDim Preserve (Track(ntk).trk)(1 To CantTicks,1 To lim3)
      print #1,"Redim exitoso"
    EndIf
 print #1,"va a copiar FOR lz,numero de veces ",numero
'---
    
       For lz = 1 To numero
          moverZonaRoll(indicePos,Roll,pasozona1)
          indicePos=indicePos + delta
          
       Next lz 
    EndIf
    Exit Do
 EndIf
 

 '  ========================================================================= 
 ' <========= ABRIR NOTAS O FRACCIONAR EN DURACIONES MAS PEQUEÑAS ============>
 '  =========================================================================
 If MultiKey(SC_Z)  And MouseButtons And 1 Then ''''MousePress = 1 Then
    indicePos=(mousex- gap1 )/anchofig + posishow 
    Rolldur=CInt(Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur)
    pasozona1=0: pasoZona2=0 
  ' ARMADUR USA MOVE EL CUAL SETEA LAS PASOZONA NECESARIAS
  ' ya funciona ok con DUR 
    Print #1,"SC_Z indicePos ,Rolldur ",indicePos,Rolldur
    ArmarDurFrac()
Print #1," TipoFrac ",TipoFrac
    Select Case TipoFrac
       Case "igualdur" ' dando click en la nota a cambiar y lo hara en las similares
          Print #1," entra por igualdur" 
          FraccionarDur  Track(),Roll,indicePos, Rolldur,nR,ntk '1er verison  
       Case "tododur" ' dando click en la nota a cambiar y lo hara en todas agregando silencios
        Print #1," entra por tododur"
          FracTodoDur  Track(),Roll,indicePos, Rolldur,nR,ntk  ' 2da version
       Case "autodur" ' automatico toma la entrada si no la hay toma la menor y mayor fracciona todas las notas
    Print #1," entra por autodur"
    Print #1,"indicePos, Rolldur, nR, ntk ",indicePos, Rolldur, nR, ntk 
          AutoFracTodoDur Track(),Roll,indicePos, Rolldur,nR,ntk  ' 3er version
    End Select    
    MousePress=0  
 EndIf
 If MultiKey(SC_LSHIFT)  Then ' :
      cuart=1 
      Exit Do
 EndIf
 If MultiKey(SC_RSHIFT)  Then ' :
     doblepun = 1  ' doble puntillo
     Exit Do
 EndIf
 
EndIf    '  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
 
' ------------------------------------------------------------------
If MouseButtons And 1  And cargacancion=0 Then
   old_btn_press_time = new_btn_press_time
   new_btn_press_time = Timer
   If ((new_btn_press_time - old_btn_press_time) < dbl_click_time) Then
      dobleclick=TRUE
   Else
      dobleclick=FALSE
   EndIf
EndIf

'                     <===  FIN    O P I L F E W H

 If mouseY > 50 And MouseButtons  And 1 And cargacancion=0 Then
  '''s5=2        'se resetea en EVENT_MOUSE_BUTTON_RELEASE ' obtengoPosicion
  '' ES ACA PROHIBIDO PONER EXIT DO ! NO FUNCION DETECTOR DE OCTAVAS
  '' DEBE SEGUIR EJECUTNDO HACIA ABAJO Y CALCULARINDICE VAMOS A MOVER
  ''ESTEIF AL FINAL
    If MenuNew <> 2 Then
     MenuNew=0
    End If 
 EndIf
 Exit Do

'''EndIf ' <= ScreenEvent(@e) END EVENTOS DE E Y MULTIKEY VAROS ESTAN AHI
' PODRIA SACARSE LOS MULTIKEY DE SCREEN EVENT PERO NO SE SI ANDAN MEJOR DEBO VERIFICAR
' ------------IPC sensado de comando fifo..
'If fueradefoco=1  And (play = 0) and (playb=0) Then
''   Print #1,"fueradefoco=1"
'   Sleep 1
'EndIf

'If s5=2 Then ''se elimino el retardo de 1 mseg frena mucho el scroll ahora 29-12-2021
' Sleep 1
'EndIf
 
Loop

While InKey <> "": Wend

If fueradefoco=1 And (play = 0) and (playb=0) Then
'  Print #1,"2 -fueradefoco=1"
   Sleep 1
EndIf

If s5=2 Then
 Sleep 1
EndIf
'arranquedo1=Timer - arranquedo1
'If  arranquedo1 <= 0.07 Then
'  Print #1,"arranquedo1 ", arranquedo1
'  Sleep 20
'EndIf
Loop




End sub
'


'
' error
errorloopbas:
  
'Dim As Integer er1, ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 0 And ContadorError < 101 Then
Print #1,"------------------------------------"
  ContadorError=ContadorError+1
  Print #1,"ErrorLoop ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
  Print #1,"Error Function: "; *Erfn()
  Print #1, "n ",posicion;" posishow "; posishow; " NroCol ";NroCol
  Print #1, "semitono "; nE; " *po "; *po 
  Print #1, "valor1 ",posicion; " valor2 "; 12- nE  + (*po -1)* 13
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
  Print #1, "lbound 2 de Roll.trk ", lBound(Roll.trk, 2)   
  Print #1, "Roll.trk (n,11- nE  + (*po-1) * 13 ).nota", Asc(Str(Roll.trk (posicion,12- nE  + (*po -1)* 13 ).nota))
  Print #1,"Roll.trk (n,12- nE  + (*po -1)* 13 ).dur ",  Asc(Str(Roll.trk (posicion,12- nE  + (*po -1) * 13 ).dur))
  Print #1,"gap1 + ic * anchofig ",gap1 + ic * anchofig
  Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po
  Print #1, "nE ";nE; " nR ";nR; " PianoNota ";PianoNota
  Print #1, "mensaje, Ermn ", *Ermn, Ermn
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2) 
  print #1,"------------------------------------"

EndIf

