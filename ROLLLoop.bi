#Include Once "crt/stdio.bi"

On  Error GoTo errorloopbas
''(c As cairo_t Ptr, ByRef nro As Integer, ByRef octava As Integer Ptr, InicioDeLectura As Integer)

Sub creaPenta (c As cairo_t Ptr, Roll as inst )


On Local Error Goto fail
If  Parar_De_Dibujar=SI  Or terminar=TERMINAR_POR_ESCAPE  Then
    Exit Sub
End If

''Dim octava As Integer Ptr
''*po va desde hasta -1 hacia abajo
'''Print #1,"creaPenta; *po , desde "; *po, desde 
indEscala=1 ' inicializamos la guiade escalas a la 1erwa 
Dim As String t2="",t3="",t4=""
 
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
 Dim As Integer semitono,indf,indfa,indfb,k,indnota,verticalEnOctavaVacia,k1
verticalEnOctavaVacia=12 + (hasta-2)*13 + estoyEnOctava - desde

 Dim As UByte code,repe
 Dim As Integer n,notac, aconro, grado,repeind
 repeind=12+(hasta-2)*13+hasta 
'Print #1,"creaPenta ajustado repeind ",repeind  98 para 4 a 8
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
' nro numero octava
 Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1) 'nro esta en el for de barrepenta
'--------------------------
 t=" ESCALA: "+ UCase(tipoescala_inicial) + " [" +cadenaes_inicial +"] "
  cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - inc_Penta)
  cairo_show_text(c, t)
  t= ""
     
 ' CAMBIOS FUTUROS FACILES: ubicar bien las posiciones de cada parametro con cairo_move tomar su argumento
 ' y colocar mas para cada parametro y luego si apreto click izquierdo aumento el valor
 ' si apreto click derecho lo disminuyo ,,o con la ruedita...Hace rlo mismo con el Pan
 ' de sonido para ubicar el sonido entre la derecha o izquierda,,,o
 ' PARA SABER SOBRE QUE CONTROL ESTOY HACE RUN CURSOSR PARA ESTA PARTE DE ARRIBA QUE PUEDA NAVEGAR
 ' ENTRE CONTROLES MODIFICABLES Y ESTAANDO UBICADO USAR RUEDITA O FLECHAS PARA CAMBIAR...
  
 cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - 2* inc_Penta)
  t=" CANAL MIDI: "+Str(canalx+1) ' para mostrar le sumo 1 pero se usa asi como viene 0 a 15
  cairo_show_text(c, t)
  t= ""
 cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - 3* inc_Penta)
 t=" I="+Str(tiempoPatron) 
  cairo_show_text(c, t)
  t= ""
 cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - 4* inc_Penta)
 t= " Factor: "+ Str(FactortiempoPatron)
  cairo_show_text(c, t)
  t= ""
 cairo_move_to(c, 0, BordeSupRoll - (hasta-9)*20* inc_Penta - 5* inc_Penta)
  t= " Compas=" +TCompas
  cairo_show_text(c, t)
  t= ""

  
' t=" BordeSupRoll="+ Str(BordeSupRoll)  
'  cairo_move_to(c, 0, BordeSupRoll )
'  cairo_show_text(c, t)
'  t= ""
'--------------------------
''ES LA 1ER LINEA DE CADA OCTAVA
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

' posishow es el inicio fijo de la posicion en el vector desde donde se leen los datos para 
' mostrarlos en pantalla y es fijo para cada barrido
' ic es para el barrido automatico de pantalla para mostrar el contenido del intervalo elegido
' para mostrar.
' curpos es para el usuario para moverse sobre el intervalo

 If  COMEDIT=ENTRADA_NOTAS  Then 'cursorVert=0 and comedit=true  
    If posicion < NroCol*3/4  Then '04-02-2022
      posishow=  1 ''curpos ' decia 1
  ' valla tatlmente al inicio veremos si es aca jmgjmg
    Else
        posishow = posicion - NroCol/2 '04-02-2022 
    EndIf
 Else
   If posicion=0 Then ' para TAB de track nuevo  
      posishow=1
      posicion=1  ' 14-03-2022
   Else
      posishow = posicion 
   EndIf  
 EndIf


 lockhoriz=0 ' teclas der izquierda controles 
 lockfont=0
 Dim As Integer lugar=0, sitio
 lugarOld=Penta_y
' |||||||||||| --------- FOR SEMITONO -------------------
 For semitono = 0 To 11
   If COMEDIT <> LECTURA  and estoyEnOctava = *po  Then ''+ 1  Then
 'COLORES DISTINTOS PARA CTRL-M CTRL-N
     If  OCTAVAFIJA=octavaEdicion Or OctavaEdicion=EstoyenOctava Then 'octavaEdicion=estoyEnOctava
        if COMEDIT=ENTRADA_NOTAS Then ' edit normal ingreso nots nuevas
           cairo_set_source_rgba(c, 0, 1, 0.5, 1) ' verde brillante
        EndIf
        if COMEDIT=MODIFICACION_INSERCION Then 'ctrl-m modificacion con X O MOUSE
           cairo_set_source_rgba(c, 1, 0.8, 0.5, 1) 'rojo ?
        EndIf
        if COMEDIT=SOLO_MODIFICACION Then ' ctrl-n modificacion con nombre nota
           cairo_set_source_rgba(c, 0.5, 0.8, 1, 1) 'celeste
        EndIf
     Else 
        cairo_set_source_rgba(c, 1, 1, 1, 1)   
     EndIf   
   Else
 
       cairo_set_source_rgba(c, 1, 1, 1, 1)
   EndIf

   If notaGuia=0 Then '5 feb 2025 
     font= font - 2 ' achicamos notas guias
     cairo_set_font_size (c, font)
''  t = NotasGuia(semitono) + Str(*octava) + "_[" 
'' cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6)
     If alteracion="bem" Then
       t = NotasGuia2(semitono) + Str(*po -1) + "_["
     Else 
       t = NotasGuia(semitono) + Str(*po -1) + "_["
     EndIf
     cairo_move_to(c, 0, Penta_y + (semitono+1) * inc_Penta- 6)
  
     cairo_show_text(c, t)
     font= font + 2  'vuelve al valor anterior...
     cairo_set_font_size (c, font)

   EndIf
   t= ""
   ic=0 'indice para  dibujar las duraciones y notas en pantalla automatico
   
' no depende de las fle4chas es el barrido automatico de CrearPenta para mostrar
' lo que hay en este pedazo de secuencia, otra cosa es cursor que es voluntario
' para que el usuario se ubique en esos datos en el vector
   n=0:indf=0:indfa=0:indfb=0
  
' ||||||||=============== FOR TICKS ========================>>>>>>>>>>>>>>>>
  For n = posishow To posishow + NroCol 
     If posishow + NroCol > CantTicks Then
        Exit For
     EndIf
' =======> deteccion escalas auxiliares y acordes
'' Print #1, " 12 + (*po-1) * 13 " ; 12 + (*po-1) * 13
'' Print #1, " *po , n "; *po, n

     If  NADACARGADO=TRUE Then
        indfa=0
        indfb=0
''indnota=0
     Else

    indfb = CInt(Roll.trk (n, 12 + (*po-1) * 13).dur) ' 103 --dur
    indfa = CInt(Roll.trk (n, 12 + (*po-1) * 13).pb) ' 26-01-2022 103 pb
''    indnota =CInt(Roll.trk (n,11- semitono  + (*po -1) * 13 ).nota) 
     EndIf    
     t="": t2="":t3="":t4=""
    
' <====== CIFRADO ACORDE---

    If indfa=201 And nVerCifradoAcordes=3 Then  
       cairo_set_source_rgba(c, 1, 1, 1, 1)
       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y)

        verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde
      If verticalEnOctavaVacia <= NA  Then '09-03
         notac=CInt(Roll.trk(n,verticalEnOctavaVacia ).nota) 'Rollnota
         aconro=CInt(Roll.trk(n,verticalEnOctavaVacia ).dur) 'acordenro
         If aconro >=1 And aconro <=43 Then  ' por ahroa tenemo 43
           t4=RTrim(ClaseAcorde(aconro).clase)
         EndIf  
         If 13 - notac >= 1 And 13 - notac <=12 Then
            t3=RTrim(NotasEscala(13-notac)) ' C/ 
            grado = BuscarGrado(t3) ' 4 en escala G
            t3=t3+t4  
   '        Print #1,"grado ",grado
            If ClaseAcorde(aconro).tipo >1  Then
               If ClaseAcorde(aconro).tipo -1 + grado > 0 Then
                  t3=t3+notas_esc_inicial(ClaseAcorde(aconro).tipo -1 + grado)
             ' Print #1,"t3=t3+notas_esc_inicial(ClaseAcorde(aconro).tipo -1 + grado) ",t3
               EndIf 
            EndIf
        EndIf
        t4=""
        indfa=0
      EndIf   
    Else
      t3="":t4=""
      indfa=0
    EndIf
' =======> fin cifrado acordes----    
    If t3 >"" Then
      cairo_show_text(c, t3)
      t3=""
    EndIf


    If indfb = 200 And nVerEscalasAuxiliares=3 Then ' escalas auxiliares o alternativas
    'Print #1,"ENTROA VER ESCAL AAUXILIAR"
       cairo_set_source_rgba(c, 0, 1, 0, 1) 
       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y)
       cairo_line_to(c,gap1 + (ic ) *anchofig , Penta_y + 13.5 * inc_Penta )
       notaescala=CInt( Roll.trk(n, 12  + (*po -1) * 13).vol)
       If notaescala=0 Then
          notaescala=1
       EndIf
       If Roll.trk(n, 12  + (*po -1) * 13).pan =3 Then
          t2= NotasEscala(CInt( notaescala ))
       EndIf
       If  Roll.trk(n, 12  + (*po -1) * 13).pan =2 Then
          t2= NotasEscala2(CInt( notaescala ))
       EndIf
     '      Print #1, "NOTAESCALA ",t2
           ' 11-01-2022
       tipoescala=CInt(Roll.trk(si, 12  + (*po -1) * 13).inst)
       If tipoescala=0 Then
          tipoescala=1
       EndIf
      '     Print #1," tipoescala ",tipoescala
       armarescala cadenaes,tipoescala, notaescala, alteracion,0

           ' fin 11-01-2022
       t2=t2+" "+ escala(tipoescala).nombre + " "+cadenaes

       cairo_move_to(c,gap1 + (ic ) *anchofig , Penta_y + 13 * inc_Penta ) '26-01
       indfb=0
       
    Else
      t2=""
    indfb=0
    EndIf

  
' t no puede quedar en un scope distinto se hace shared    
       ' apenas usas t2 o t3 hay que borrarlas sino se pudre todo raro
    If t2 >"" Then
      cairo_show_text(c, t2)
      t2=""
    EndIf
' NO PISAR COLOR DE OCTAVA SELECCIONADA
     If octavaEdicion<>estoyEnOctava Then ' 28-05-2025 REPOSICION COLORES DE LINEA COMPLETOS
      cairo_set_source_rgba(c, 1, 1, 1, 1)
     EndIf 
' <=========fin escalas y acordes      

 '' ver si peudo borrar esto 10-04-2022 ����
 ' Print #1,"n semitono *po  "; n; " ";semitono; " "; *po; " ARGU: ";(11- semitono  + (*po -1) * 13) 
  
 ' *po llega a 3 y cancela porque baja a 3?
'  ESCRITURA DE NOTAS: ------->

 If Roll.trk (n,11- semitono  + (*po -1) * 13 ).nota > 0  Or Roll.trk (n,11- semitono +  (*po -1) * 13 ).dur > 0 Or Roll.trk (n,11- semitono +  (*po -1) * 13 ).onoff > 0 Then
     ' print #1,"lugar ",11
'  10-04-2022 verificar si esto sigue funcionando ���� colocar esapcios
      If COMEDIT<>LECTURA Then 
         If COMEDIT=ENTRADA_NOTAS Then   ' edicion manual
            If espacio = (semitono +1) Then
               Roll.trk (n, 11-semitono + (hasta -nro) * 13 ).dur = 181
               Roll.trk (n, 11-semitono + (hasta -nro) * 13 ).nota = 0
               Roll.trk (n, 11-semitono + (hasta -nro) * 13 ).onoff = 0
               ''Print #1,"esta metiendo espacios?"
               If fijarEspacio=0 Then
                 espacio=0
               EndIf
            EndIf
         EndIf   
' un buen forma de borrar facil y rpido seria usando
' ((n - inicioDeLectura)=curpos) y moviendoelcursor derecha izquierda
' en ctrl-m borra todo de una!! implementarlo...        
         If COMEDIT=MODIFICACION_INSERCION  Then  ' ctrl m
            If (espacio =semitono +1  ) And ((n - inicioDeLectura)=curpos)  Then 'semitono +1
               Roll.trk (n,11-semitono + (*po-1) * 13 ).dur = 181
               Roll.trk (n,11-semitono + (*po-1) * 13 ).nota = 0
               Roll.trk (n,11-semitono + (*po-1) * 13 ).onoff = 0
               If fijarEspacio=0 Then
                  espacio=0
               EndIf
            EndIf   
         EndIf
' un buen forma de borrar facil y rpido seria usando
' ((n - inicioDeLectura)=curpos) y moviendoelcursor derecha izquierda
' en ctrl-m borra todo de una!! implementarlo...        
         If COMEDIT=MODIFICACION_INSERCION  Then  
            If (espacio = semitono +1 ) And ((n - inicioDeLectura)=curpos)  Then
               Roll.trk (n,11-semitono + (*po-1) * 13 ).dur = 181
               Roll.trk (n,11-semitono + (*po-1) * 13 ).nota = 0
               Roll.trk (n,11-semitono + (*po-1) * 13 ).onoff = 0
               If fijarEspacio=0 Then
                  espacio=0
               EndIf
            EndIf   
         EndIf

     ' BORRADO LIBRE NO MARCA SOLO BLANCO habilita para usar nota=0  ,,,??? 
         If COMEDIT=MODIFICACION_INSERCION  And Borrar=1 Then  
            If ((n - inicioDeLectura)=curpos)  Then
               Roll.trk (n,11-semitono + (*po) * 13 ).dur = 181
               Roll.trk (n,11-semitono + (*po) * 13 ).nota = 0
               Roll.trk (n,11- semitono + (*po) * 13 ).onoff = 0
               If fijarEspacio=0 Then
                  Borrar=0
               EndIf
            EndIf   
         EndIf
      EndIf    
' FIGURA ESCRITURA DE NOTAS, CAIRO SE MUEVE EN X CON ic...  anchofig*2 domingo
' y solo pinta cada loop de izq a derecha partiendo siempre de 1 
'-------------------------------------------------------------------
      cairo_move_to(c, gap1 + ic * anchofig , Penta_y + (semitono+1 ) * inc_Penta - 4)
  
  '  print #1,"lugar ",12
     indf= CInt(Roll.trk (n, 11- semitono + (*po-1) * 13).dur)

  '  print #1,"lugar ",13
    If (indf >= 1 And indf <= 185) Or indf=190  Then ' 12-03-2025 185=N roll sin duraciones
    Else
        indf=181
    EndIf ' t no puede quedar en un scope dsitinto se hace shared    
    ''If Roll.trk (n, 11- semitono + (*po-1) * 13).dur=183 Then '26-feb2025
     ' Print #1,"UN 183 DETECTADO EN CREAPENTA, n ";n 
     '''  cairo_move_to(c, gap1 + ic * anchofig , Penta_y + (semitono+1 ) * inc_Penta - 4) 
    ''EndIf
     If indf=181 And resumen=1 Then
        If intervalo > 0 Then
           separaenuno=separaenuno +1
           If separaenuno=intervalo Then ' ESTO ACHICA CON INTERVALO 4 O 5 YA SE APILA TODO AL PRINCIPIO
              separaenuno=0 
           Else
              Continue For ' ESTO ACHICA 
           EndIf
        Else
           
        EndIf
     Else
       If indf = 181 Then
        t= "    "
       Else
        t= figura(indf)
       EndIf
     EndIf
 ' ////////dar color al font en una determinada posicion durante el play
    If n<=jply + 2 And n<=jply - 2 And Parar_De_Dibujar=NO Then 
       cairo_set_source_rgba(c,1,0,1,1)
    EndIf
    If indf <> 181 Then ' esto acelera un monton 181 es vacio espacio no hay nada para mostrar
      cairo_show_text(c, t)
    EndIf
' se elimina el cursos con las notas coloreadas durante el play es suficiente
 ' ' jmg 11-05-2021 1839 start
    If  n<=jply + 2 And n<=jply - 2 And ( play =SI Or playb=SI Or Cplay=SI ) And Parar_De_Dibujar=NO Then ' Parar_De_Dibujar 17-06-2022
        
      ShowNroCol= Int(n/posishow) 
      If ShowNroCol = 0 Then
         curpos= n  - 1 
      Else
         curpos= n  - posishow ' posishow es el lugar x en el vector desde donde se leen los datos
' se supone que n esta moviendose barrieno en x
      EndIf

     '''  cursor(c,n,nro,Roll)
      cairo_set_source_rgba(c, 1, 1, 1, 1)
    EndIf

' jmg 11-05-2021 1839 end  
'===== LINEAS DE COMPAS  ==================================
     If n >0 And n < MaxPos  Then 
 '   Print #1,"lugar ",14
      If Compas(n).Posi = n  Then
        ' ic=ic+1 
' aca tambien tendremos las repeticiones si estan las leemos y la usamos en el play
' seri aun loop como hasta ahora pero N veces no infinitas, o sea aca solo
' grabo las N veces y comienzo de loop y en el final grabare fin loop 
    ' print #1,"lugar ",15
          cairo_move_to(c,gap1 + (ic ) *anchofig +anchofig , Penta_y)
          cairo_line_to(c,gap1 + (ic ) *anchofig +anchofig, Penta_y + 12 * inc_Penta )
      '    print #1, "|";
          cairo_move_to(c,gap2 + (ic ) *anchofig +anchofig, Penta_y + 12.5 * inc_Penta )
          t=Str(Compas(n).nro)
          cairo_show_text(c,t)
      EndIf
  '   Print #1,"lugar ",16
      If n = pasozona1 Or n =pasoZona2 Then '26-06-2021
   '  Print #1,"lugar ",17
        ' ic=ic+1
         cairo_move_to(c,gap3 + (ic ) *anchofig +anchofig, Penta_y + 13.5 * inc_Penta )
         t="("+ Str(n) + ")"
         cairo_show_text(c,t)
      EndIf
      
     ' If code > NA Then
     ' Print #1,"mayor que NA,code ", NA,code
     ' EndIf
 
      code=Roll.trk (n, repeind).nota
      repe=Roll.trk (n, repeind).vol
      If code =210 Or code=211  Then
         cairo_move_to(c,gap3 + (ic ) *anchofig +anchofig, Penta_y + 13.5 * inc_Penta )
         Select Case code   
            Case 210
               t="[:"
  '          Print #1,"code 210, *po= ", *po, t  
            Case 211
              t= Str(repe)+":]"
   '         Print #1,"code 211, *po= ", *po, t
         End Select         
            cairo_show_text(c,t)
      End If

     EndIf

    cairo_move_to(c, gap1 + ic * anchofig , Penta_y + (semitono +1)* inc_Penta - 6)
    ic =ic+1 ' adelanta una posicion ???
    
  
'''    Exit For ' sale para saltear las nota=0, dur=0 �?, estaba mal creo debo mostrar todo
 Else
   ic=ic+1
   
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
  ' nsE es la nota de carga va de 1 a 12.
  lugar=Penta_y + (semitono +1) * inc_Penta
  cairo_move_to(c, 0, lugar )
'' TRAZADO DE LINEAS HORIZONTALES DE LA OCTAVA 
  cairo_line_to(c, ANCHO - 1, lugar)
  cairo_stroke(c) ' aca da exception al GrabarMidiIn con cancion cargada ..uff

If GrabarPenta=0 Then '���� NO ESTABA
  If (mousey <= lugar) And (mousey >= lugarOld ) Then
   nsE=semitono + 1 'semitono ahora va desde 0 a 11 usado por entrada de teclado y ahroa mouse
   nR=(11-semitono) + (*po -1 ) * 13 ''+ (desde -1)*13 ' indice de la nota en Roll , en algo ser� util.
 '  If *po=1 Then
   '   Print #8, "nR octava 1 ",nR
 '  EndIf
   PianoNota= nR - restar (nR)
''' des�jando nsE = 11 -nR   +  (*po -1 ) * 13 + 1
  EndIf
  lugarOld=lugar
Else '���� ELSE NO ESTABA
   nsE=semitono + 1 'semitono ahora va desde 0 a 11 usadopor entrada de tecladoy ahroa mouse
   nR=(11-semitono) + (*po -1 ) * 13 ''+ (desde -1)*13 ' indice de la nota en Roll , en algo ser� util.
   PianoNota= nR - restar (nR)
   
EndIf
 ' ahora quiero que salga son semitono=12 asi lee las esclas y acordes
 ' o lo ponemos directamente en 12 salir del loop debe ser mas rapido? 
   If semitono =11 Then ' asi no suma 1 a semitono y no pasa a ser 12
     Exit For
   EndIf

 
 Next semitono
 'Next semitono

 
' ----------------------------------------------------------- 
 ' PARA ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
 ' y hacer nota=semiotono 1 a 11 con el mouse...el resto es automtico...
' nro=hasta significa que ya dibujo la octava 9, luego puede seguir dibujando
' hacia abajo, la ayuda 
/'
 If *po = desde Then ' termino 9 octavas o la NA y ahora  + ayuda...
  cairo_set_font_size (c, font)
  'cairo_select_font_face (c, "Georgia",CAIRO_FONT_SLANT_NORMAL , CAIRO_FONT_WEIGHT_BOLD)
  t= "Flecha Abajo/Arriba o ruedita del mouse, scroll de las octavas en la ventana. F1 Ayuda Notepad "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 15  )
  cairo_show_text(c, t)

  t = "F9/F10 achica/agranda el  font de las notas guia,F2/F3 proporcion "
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
  
  t = "Ctrl-Click en [Reproducir] Play con scroll e iluminacion de notas, o barra espaciadora"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 21 )
  cairo_show_text(c, t)

  t = "Configuracion, Tempo. Port Salida MIDI-OUT,Canales"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 22 )
  cairo_show_text(c, t)

  t = "Pulsar F1 Para Un Notepad Con Ayuda Preliminar mas detallada, puede estar incompleta y no ser lo ultimo"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 23 )
  cairo_show_text(c, t)

  t = "En el menu algunos funcionan con Ctrl-clik otros con click solamente o ALT"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 24 )
  cairo_show_text(c, t)

  t = "En modificacion o Edit, se pasa de una octava a otra para editarla, deslizando el mouse hasta el extremo derecho de la octava deseada, eso iluminara las lines de verde"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 25 )
  cairo_show_text(c, t)

  t = "la octava en edicion. (No hay vocales acentuadas, Cairo, la libreria grafica usada, no las maneja con font simples)"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 26 )
  cairo_show_text(c, t)

  t = "F2/F3 comprime - expande horizontalmente la secuencia proporcionalmente, se puede editar tambien"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 27 )
  cairo_show_text(c, t)

  t = "En Edit entrar notas: 1) duracion 1 a 9, luego el nombre CDEFGAB o Ctrl+ nombre para sostenidos "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 28 )
  cairo_show_text(c, t)

  t = "En Lectura navegar a derecha o izquierda con Flecha horizontales O CTRL IZQ,O CTRL DER MAS RAPIDO, arriba abajo con Flechas Verticales O RePAg AvPAg"
  cairo_move_to(c, 0, Penta_y + inc_Penta * 29 )
  cairo_show_text(c, t)

  t = "En Lectura seleccionar zona entre 2 posiciones,con Ctrl+Click en cualquier parte de la posicion deseada de la octava deseada, repetir hacia derecha para la 2da posicion "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 30 )
  cairo_show_text(c, t)

  t = "Para ver el Final de la pista pulsar Tecla FIN, principio INICIO,  "
  cairo_move_to(c, 0, Penta_y + inc_Penta * 31 )
  cairo_show_text(c, t)
    
 End If
'/
 ' si estoy en esta octava ...edicion solo para esa octava segun posicion
 ' del mouse automaticamete iluminar
If GrabarPenta =0 Then
 If ( Penta_y <= mousey) And ((Penta_y + 12 * inc_Penta) >= mousey)  Then
  ' estoy en una octava
  If MouseButtons And 1 Or MouseButtons And 2 Then
     estoyEnOctava = *po  '<========== DETERMINACION DE OCTAVA DE TRABAJO
  EndIf
  EnOctava=1
  ' CURSOR
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  ' Or (cursorHori=2 And cursorVert=2 paa hbilitar ctrl-N
  If COMEDIT=MODIFICACION_INSERCION  Or COMEDIT=SOLO_MODIFICACION   Or play=SI Or playb=SI   Then ' ctrl-m y ctrl-n ch
      cursor(c,posishow,nro,Roll) ' posicion por n 26-10-2021 se arreglo curpos
''      cursor(c,posicion,nro,Roll) ' posicion por n 26-10-2021 se arreglo curpos 
      ' se ilumina en posicion 0 
  EndIf
  '''' cairo_stroke(c) ESTOS STROKE HACEN QUE SALTE LA PANTALLACON - +
  'PERO PIERDO EL COLOR MARILLO DEL CURSOR
  
  cairo_set_source_rgba c, 0, 0, 0, 1
  
  'cairo_set_line_width(c, 3)
 EndIf
else
  
  If COMEDIT=MODIFICACION_INSERCION  Or play=SI Or playb=SI  Then ' solo ctrl-m ?
      cursor(c,posishow,nro,Roll) ' posicion por n 26-10-2021 se arreglo curpos 
      '''cursor(c,posicion,nro,Roll) ' posicion por n 26-10-2021 se arreglo curpos
  EndIf
  
   cairo_set_source_rgba c, 0, 0, 0, 1
   
EndIf 


'---------
If GrabarPenta=0 Then
  If ((Penta_y + 12 * inc_Penta) <= mousey) And ((Penta_y + 14 * inc_Penta) >= mousey)  Then
   EnOctava = 0
   estoyEnOctava=90 ' esto dio error corregido 
  EndIf
EndIf

 *po = *po -1 ' 4 -1 = 3
 If *po = desde -1 Then  ' 22-09-2021 < estaba mal es = ej 4 a 8 -> 4-1=3 ya ejecuto la 3 me voy
  *po = 99
   Exit Sub
 EndIf
 '           ================  MENUES CONTEXTUALES GRAFICOS PARA MOUSE ==================
 If ayudaModif=TRUE  And COMEDIT<>LECTURA Then
  If  COMEDIT=MODIFICACION_INSERCION  Then 'solo ctrl-m
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
 If ayudaNuevaNota=TRUE  And COMEDIT<>LECTURA And vuelta=FALSE Then
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
   If COMEDIT=ENTRADA_NOTAS Then
    cairo_move_to( c, usamousex -60 , usamousey +50 )
    cairo_text_extents( c, text(9), @extents )
    cairo_show_text( c, text(9) )
   EndIf
   If COMEDIT=MODIFICACION_INSERCION  Then
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

Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
Print #1,"Roll.trk (n,11- semitono  + (*po -1) * 13 ).nota > 0 Or       Roll.trk (n,11- semitono +  (*po -1) * 13 ).dur > 0 Then"
Print #1, "n ,semitono, *po ",n, semitono, *po
Print #1, "11- semitono ", 11-semitono
Print #1," (*po -1) * 13 "; (*po -1) * 13
Print #1, "11- semitono  + (*po -1) * 13 "; 11- semitono  + (*po -1) * 13


End If


End Sub



Sub barrePenta (c As cairo_t Ptr, Roll as inst  )
'------------------
 '''NroCol=(ANCHO / anchofig ) - 4 07-03 lo puse pero no se si ahce cancelar
'' no no es esto...lo que hace cancelar...
  Dim i As Integer
  
  For i = desde To hasta ' 4 a 8 por omision
    nro = i 
  ' si ahce falta ejecutar mas de un Penta podremos usar threads
  ' asi funciona mejor o no? igual debe esperar a que termine el thread
 ' NOOOO ScreenSync  no usar nunca sync desfasa el barrido de cairo y salta
 ' las lineas
     ScreenSync ' a ver si aca es mejor....
     creaPenta (c, Roll )
     

    If *po = 99 Or *po=3 Then
       *po = hasta -1 ' 8 por ejemplo => *po=7
       
       Exit For
    EndIf
     
  Next
  
  
End Sub

'Roll Main Loop ACA NO APARECE EL VECTOR DE ROLL 


sub  RollLoop (ByRef param As pasa) ' (c As cairo_t Ptr, Roll As inst)
On Local Error Goto fail
' 15-02-2026


Dim As Integer ubiroll,ubirtk,encancion
' PORACA CANCELA '''' JMGDEBUG
abrirRollCargaMidi=2 ' no permite cargar Roll ya esta cargado
' si  levantamos un plano de midi despues de cargar roll
Dim midionof As Integer 
'NO BORRAR VECTOR ROLL SI TIENE DATOS:
If MaxPos <=6 Then ' no borra Roll cuando cierro y abro el grafico pero tengo datos
 c=param.c
 Roll=param.Roll
 ubiroll=param.ubiroll 
 ubirtk=param.ubirtk
 encancion=param.encancion
EndIf

If NombreCancion > "" Then
 abrirRoll=REABRIR_ROLL_CON_DATOS_CARGADOS
EndIf
 ALTO=param.alto
 ANCHO=param.ancho
 ANCHO3div4 = ANCHO *3 / 4
 midionof=param.midionof


 If midionof = 4 Then  ' volcado de midi
    MIDIFILEONOFF=HABILITAR  
 End If 

'  print #1,"rollLoop ubirtk ",ubirtk
'  print #1,"rollLoop ubiroll ",ubiroll

' print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
' print #1,"posicion ", posicion
' Print #1,"ancho, alto", ANCHO, ALTO
' Print #1, "EN ROLLLOOP cargaCancion DEBE SER 1 EN INICIO ",cargaCancion
 '    If hwnd =0 Then   ,GFX_WINDOWED
' 07-02-2025 CAMBIAMOS DE 32 BITS A 24 PARECE ANDA ALGO MAS RAPIDO�?
' anda con los 3 drivers OpenGL DirectX y GDI ....
Dim resultado As Long
     ScreenControl  SET_DRIVER_NAME, "GDI" '''"Direct2D" '''
' LOS PIXELS ESTABAN EN 24 LO LLEVE A 16 VEMOS SI ANDA MAS RAPDIO Y CONSERVA TODO 
' SAQUE 2 PAGINAS A 1 NO USO MAS QUE UNA PAGINA PARA DIBUJAR
 'si usara dos paginas podria ser mas rapido?
     If usarmarco= 3 then
        resultado = ScreenRes ( ANCHO, ALTO ,24 ,1 , GFX_HIGH_PRIORITY)
     Else
        resultado = ScreenRes (ANCHO, ALTO, 24 ,1, GFX_NO_FRAME Or GFX_HIGH_PRIORITY)
     EndIf


 '    print #1,"param.titulo ",param.titulo
     WindowTitle param.titulo
     ScreenControl GET_WINDOW_POS, x0, y0
     ScreenControl(fb.GET_WINDOW_HANDLE,IhWnd)
     hwnd = Cast(hwnd,IhWnd)
     
  ' datos recibidos   
'  print #1,"datos recibidos en rooloop nombre ", nombre
'  print #1,"datos recibidos en rooloop desde,hasta ", desde, hasta
'  print #1,"datos recibidos en rooloop *po ", *po 
'  print #1, "ubound roll.trk 2 ",ubound(roll.trk,2)
  '  End If
'Dim Roll As inst
' @Roll(1) = *pRoll  
'Dim As Integer pid = GetCurrentProcessId()' , pid_parent = 0
   abrirSecuencia(20)
' print #1 ,"pid", pid 
' EXISTE CAIRO_FORMAT_RGB24 TAMBIEN PARA ELLOS DEBO CREAR SCREENRES CON 24, NO SE CONQUE ANDA MEJOR
' 24 USA MENSO RECURSOS??? ES MAS RAPISO NO SE , ES MAS RAPIDO OPENGL O GDI? DEJE DE USAR GDI VEREMOS
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

' Print nro_penta

                BordeSupRoll = Int((ALTO ) /18) 
                inc_Penta = Int((ALTO - BordeSupRoll) /(40))
                BordeSupRoll = BordeSupRoll -  66* inc_Penta ' de inicio muestro octava 4 la central C3

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

' print #1,"BordeSupRoll ",BordeSupRoll
'no se usa en ningun lado nro_penta = ((ALTO - 1)- BordeSupRoll)/(inc_Penta * 4)
' print #1,"INSTANCIA ", instancia

' Print #1,"call roolloop, tipoescala",tipoescala_inicial
' Print #1,"call roolloop, notaescala",notaescala_inicial  
'If cargacancion=CARGAR_NO_PUEDE_DIBUJAR Then SOLO PARA DREBUG
   
 ' Print #1,"4 ROLLLOOP ENTRA A CARGAR PISTAS 1ERA VEZ cargaCancion ES 1 SI O SI ",cargaCancion
'EndIf

If ubiroll > 0 Then  ' CARGA DE ARCHIVOS POR LINEA DE COMANDO DE ROLLMUSIC
 '  Print #1,"cargo archivo desde rollLoop"
   nombre = titulosTk(0)
'   Print #1,"nombre",nombre
'   Print #1,"tituloTk(0) ",titulosTk(0)
    CargaArchivo (Roll,ubiroll) ' aca ajusta ubiroll a 2
   s5=2
   ROLLCARGADO=TRUE
   MenuNew=MENU_INICIAL
  param.ubiroll=ubiroll ' vale 2
  portout=CInt(pmTk(0).portout)
   
' abrir ports si no estan abiertos 
   GrabarPenta=0 ' para que en PlayAll abra los ports

EndIf

 If ubirtk > 0 Then ' ya tengo el nommbre en linea de comando
  '  print #1,"carga track desde linea de comando,  nombre antes   ",titulosTk(0)
    nombre = titulosTk(0)
    CargarTrack (Track() , 0, ubirtk ) ' ntk=0
    If nombre > ""  Then '16-01-2022 crach si se cancela la carga
  '    print #1,"carga track veo nombre despues ", titulosTk(0)
      TrackaRoll (Track(),0,Roll) ' ntk=0
'Print #1," desde 892 ";desde
  '    print #1,"TrackaRollcarga rtk veo nombre ", titulosTk(0)
      RecalCompas (Roll)
      TRACKCARGADO=TRUE
      ubirtk=2
    Else
      TRACKCARGADO=FALSE
    EndIf

  '  print #1,"despues RecalCompas veo nombre ", titulosTk(0)
    MenuNew=MENU_INICIAL
    ubirtk=0
   param.ubirtk=0
' abrir ports si no estan abiertos 
   GrabarPenta=0 ' para que en PlayAll abra los ports
   portout=CInt(pmTk(0).portout)
 EndIf


If instancia = ARG7_NOMBRECANCION  Then ' 04-03-2024 LOGRE LEVANTAR CANCION EN UN ROLL EXTERNO
' CANCELA LA REPRODUCCION DEBERIA HABILITARLA VEREMOS
 If NombreCancion > ""  Then
 '  Print #1, "rollloop instancia ", instancia
 '  Print #1, "rollloop NombreCancion ", NombreCancion
    
'' carga de cancion porlinea de comandos
    cargacancion=CARGAR_NO_PUEDE_DIBUJAR 
    CargarPistasEnCancion ()
    cargariniciotxt(NombreCancion, CANCION)
    instancia=ARG107_FICTICIO  ' ficticio para que entre al if de TAB pero  que no entre en el resto ni aca
    param.encancion=CON_CANCION
    
      
 EndIf
EndIf
 

Dim  As UByte   huboerror  
Dim  As integer contid=0 ' 1 to 24 maximo son 12 on y 12 off en acorde
Dim  As Integer nroPartesNota,nnn=0
' -----------------

edity1 = 1 ' botton Edit bordeSup
edity2 = 50 ' botton Edit bordeInf

''''stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)
Do
''arranquedo1=Timer

'' Create a cairo drawing context, using the FB screen as surface.
'' l originalestba mal sizeof(integer ) es mu chico debe ser 4
'' esto solo ejecutar si hay cambio de tama�o!!

stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)

' ------para reducir el consumo de recursos por ahora al tocarpistas MIDI
' o grabar pistas midi con una cancion o track o roll cargado no escribimos nada en pantalla grafica
' queda congelado en los 1eros compasaes mostrados al terminar esos procesos mencionados
' se se libera la escrituta al grafico...vermeos si sirve para seguirgrabando pistas y reproduciendo 
' en mejores condiciones,,,
If  cargaCancion=CARGAR_NO_PUEDE_DIBUJAR  Or Parar_De_Dibujar=SI Or  GrabarEjec=GrabarPistaEjecucion Then
' esta cargando cancion 
   'Locate 5,10
   'Print "CARGANDO ...PISTA Nro ", ntk
   'Sleep 100
 s5=2 ' el loop principal necesita menos cpu  
Else   

'--------------
   '''' If  terminar=0 And GrabarEjec=0 Then  '16-06-2022
        
        
      ScreenLock()
          cairo_set_source_rgba c, 0, 0, 0, 1
          cairo_paint(c)
          cairo_set_line_width(c, 1)
          If s1 = 1 Then   s1=0 EndIf
          If s2 = 1 Then   s2=0 EndIf
          If s6 = 1 Then   s6=0 EndIf
          If s7 = 1 Then   s7=0 EndIf
          If s8 = 1 Then   s8=0 EndIf
          If s9 = 1 Then   s9=0 EndIf

          inc_Penta = Int((ALTO -1) /40) - deltaip
' ----------------------------------------------------------------------------
          cairo_set_antialias (c, CAIRO_ANTIALIAS_DEFAULT) 'hace mas lental cosa pero nomeafecta
'---------------------------------------------------------------
'---------<======= ESCALAS AUXILIARES INSERCION ================>
' --------------------------------------------------------------
           
         If cambioescala=1 And pasoZona1 > 0 Then ' creamos una posicion con cambio de escala en pasoZona1
             tipoescala=tipoescala_num
             notaescala=notaescala_num

             Dim As Integer k,vacio
             For K=desde To hasta -1 ' queda entre 2 octavas ,corregido  26-01-2022
             ' Print #1,"CARGO FOR !!! NOTA 30 DUR 200, k, pasozona1, NA ", "K=";K, pasoZona1,NA
                vacio= 12 +(k -1) * 13 'K=4 -> 51
             '   Print #1,"vacio,tipoescala ",vacio, tipoescala
             '   Print #1,"vacio,notaescala ",vacio, notaescala
                  Roll.trk(pasozona1, vacio).inst=CUByte(tipoescala)
                  Roll.trk(pasozona1, vacio).vol= CUByte(notaescala)
             
                  Roll.trk(pasozona1,vacio ).nota = 30  ' 30 INDICA CAMBIO ESCALA AUXILIAR
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
  
          ' nota=30 , dur=200 indicara cambio de escala 
             guiaEscala(indEscala).posicion=posicion
             cambioescala=0
             pasoZona1=0

'            sigue en crea_penta donde al barer el roll va leyendo las escalas auxiliares
         EndIf
     '05-02-2022 usamos threarPenta ya definida global
     ' se supone que la direccion es unica y se reusa no se la crea muchas veces
     ' es mejor no �? zas je
' cairo_text en creaPenta hace cancelar la salida desde Ventana Ctrl
' evitamos escribir a grafico si ya estamso saliendo ,,,
    
  
      EndIf
        If (terminar=NO_TERMINAR_BARRE_PANTALLA Or Parar_De_Dibujar=NO)  Then 
           threadPenta = ThreadCall barrePenta (c, Roll )
           ThreadWait threadPenta
            pubi=0
           If VerMenu=1 Then
             GetMouse mouseX, mouseY, , MouseButtons  
             menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
             cairo_stroke(c)
             botones(hWnd, cm, ANCHO,ALTO) ' este despues sinocrash
             cairo_stroke(cm) ' cm despues de c sino crash
           EndIf

        EndIf
    
      

      ScreenUnLock() 
   
    


'' ---------------  LOOP 2 ---------------
Do
Do 
'Print #1,"1051 do 2, ROOLLOOP DESDE "; desde

'---------
'simulamos TAB para cargaCancion=CARGAR_NO_PUEDE_DIBUJAR cuando recien se carga la cancion
' no permitimos cambio de track durante el play aveces trae problemas
' por la apertura de ports hay que analizar, no deberia el play de cancion
' solo usar los tracks y lo visual es solo Roll 
' lo habilitamos de nuevo para observar si ocurren erroes de nuevo
' 23-02-2022, lo q debo hacer es no permitir entrar si se esta usando
'  playAll  play=0 y lo mismo en PlayAll no usar PlayCancion si esta
' ejecutando PlayAll de unsa sola pista...
'Print #1,"1062  DO2 ROOLLOOP DESDE GrabarPenta "; desde,GrabarPenta


If MultiKey(SC_TAB) And (instancia=ARG0_EN_LINEA Or instancia= ARG107_FICTICIO) And CANCIONCARGADA And play=NO Or cargaCancion=CARGAR_NO_PUEDE_DIBUJAR Or clickpista=1   Then
   If GrabarPenta=1 Then     'sale sin procesar
   Else   

   cargaCancion=NO_CARGAR_PUEDE_DIBUJAR ' para que no entre mas luego de cargada la cancion
   s5=0  '11-06-2022
   Erase mel_undo, undo_acorde, undo_kant_intervalos
   mel_undo_k=0: ig=0:cnt_acor=0
   ROLLCARGADO = FALSE
  ' print #1,"--TAB "
   nota=0
   dur=0
 '  print #1,"TAB 1- NTK,MAXPOS, pmtk(ntk).maxpos  clickpista ", ntk,maxpos,pmTK(ntk).maxpos, clickpista
   If clickpista=1 Then
 '    Print #1,"no incrementea ntk"
     clickpista=0
   Else
     ntk = ntk + 1
  '   Print #1,"Incrementea ntk"
   EndIf
 '  print #1,"TAB 2- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos  
   If ntk > 32 Or ntk > tope Then
     ntk=1 
  '   print #1,">TAB 2A- 1- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos     
   EndIf
   nombre= titulosTk(ntk)
 '  If nombre > "" Then
 '    print #1,"--------------------------"
 '    print #1,"TAB 3-NTK nombre", ntk,nombre
 '    print #1,"TAB 3-NTK MAXPOS pmtk(ntk).maxpos  ", maxpos,pmTK(ntk).maxpos
 '    print #1,"--------------------------"
 '  EndIf  
' evita leer track vacios   
   If nombre=""  Then ' evita revisar track vacios
     Do While nombre=""
        ntk=ntk+1
        If ntk>32 Or ntk > tope Then
           ntk=1
           nombre= titulosTk(ntk)
 ' print #1,"TAB 4 - NTK, pmtk(ntk).maxpos  ", ntk,pmTK(ntk).maxpos    
           Exit Do
        EndIf
 
        nombre= titulosTk(ntk)
     Loop
  EndIf
     posicion=0 ' 14.-03-2022
     MaxPos=pmTk(ntk).MaxPos
     posn=pmTk(ntk).posn
     desde=pmTk(ntk).desde
     hasta=pmTk(ntk).hasta
     NB=pmTk(ntk).NB
     NA=pmTk(ntk).NA
     portout=pmTk(ntk).portout 'solo debe servir para play de pista
     notaold = CInt(pmTk(ntk).notaold)
     CantTicks=pmTk(ntk).Ticks
     patchsal=Track(ntk).trk(1,1).nnn
' ajusto escala principal durante la conmutacion para cada track visualizado con TAB     
     notaescala_num_ini=CInt(pmTk(ntk).notaescala) '13-01-2022
     tipoescala_num_ini= CInt(pmTk(ntk).tipoescala) '13-01-2022
     cadenaes_inicial="" '13-01-2022
     armarescala cadenaes_inicial,tipoescala_num_ini,notaescala_num_ini,alteracion,1 '13-01-2022
' todavia no probado, escala principal para TAB en cada track testeat 13-01-2022     
' no he grabado las escalas auxiliares en lso Trackc todavia !! 13-01-2022 jjj     
  '   print #1,"TAB 5- MAXPOS final TAB " ,maxpos
     
      
'

 '  print #1, "TAB 6-NTK nombre", ntk,nombre  
 '  print #1, "TAB 6-NTK ntk,MAXPOS, pmtk(ntk).maxpos  ", ntk, MaxPos,pmTK(ntk).maxpos
' copia track a Roll en memoria  
' el segundo parametro es canal no se usa...lo saco o lo dejo?
   Tracks (ntk , 1,Roll) ' track , nro,  Canal, copia track a Roll en memoria
'Print #1,"14-sleep ";Timer
   Sleep 100
 '  Print #1,"TAB 7- instancia, maspos ",instancia, maxpos
   EndIf
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_M)  Then ' modificar con X o insertar con Insert y I
 If COMEDIT<>LECTURA Then
    COMEDIT=MODIFICACION_INSERCION
 EndIf

  
 agregarNota=0
 menuMouse = 0 ' INICIA EL MENU CONTEXTUAL PARA IMPRIMIR LOS LABELS DEL MENU
 nota=0
 DUR=0
 trasponer=0
 curpos= Int((mousex - gap1)/anchofig)
 notacur=nsE
 parametros=0
 lockip=1
EndIf

If MultiKey(SC_ALT) And MultiKey(SC_M)  Then ' menu Roll inicial
  menunew=0
  menunro=0
  COMEDIT=LECTURA
  agregarNota=0
  menuMouse = 0
  nota=0
  DUR=0
  trasponer=0
  VerMenu=SI
  parametros=0
  cierroedit= 0
EndIf

If MultiKey(SC_LSHIFT) And MultiKey(SC_M)  Then ' ver  menu durante play por eje para cambiar patch
  If s9=0 Then
     s9=1
     If VerMenu=SI Then
        VerMenu=NO
     Else
        VerMenu=SI
     EndIf
     Sleep 100 
  EndIf
EndIf

If MultiKey(SC_ALT) And MultiKey(SC_E) Then ' edicion Roll
  menunew=12
  menunro=12
  trasponer=0
 If COMEDIT<>LECTURA Then
    COMEDIT=ENTRADA_NOTAS
 EndIf
 agregarNota = 0
 menuMouse = 0
 nota=0
 DUR=0
 lockip=2


EndIf


If MultiKey(SC_CONTROL) And MultiKey(SC_N)  Then 'modificar con nombre de nota
 If COMEDIT<>LECTURA Then
    COMEDIT=SOLO_MODIFICACION
 EndIf

 nota=0

 agregarNota= 1
 menuMouse = 0
 DUR=0
 trasponer=0
 curpos= Int((mousex - gap1)/anchofig)
 parametros=0
 lockip=1
EndIf


If MultiKey(SC_CONTROL) And MultiKey(SC_P)   Then 'PARAR cursor MEJOR CON MOUSE ?
'vuelve a COMEDIT=ENTRADA_NOTAS PUES ES UN SOLO PASO ATRAS
 cierroedit= 0
 cierroedit= 0
 If COMEDIT<>LECTURA Then
    COMEDIT=ENTRADA_NOTAS
 EndIf
 agregarNota = 0
 menuMouse = 0
 trasponer=0
 nota=0
 DUR=0
 lockip=2
 cierroedit= 0
 cierroedit= 0
EndIf

If MultiKey(SC_LSHIFT) And MultiKey(SC_V)   Then ' ver parametros roll durente play
'ver parametros durante play
  If play=SI Or playb=SI Or Cplay=SI Then
     VerMenu=SI
     menuNro= PARAMETROS_ROLL
     menuNew = menuNro ' PARA EVITAR QUE CAMBIE DE MENU
  EndIf 
  Exit Do
EndIf

If MultiKey (SC_P) Then  
   If COMEDIT=LECTURA   Then
      PARAR_PLAY_MANUAL=SI ' DETIENE EL PLAY VEREMOS
      PARAR_PLAY_EJEC=SI
      playloop=NO:playloop2=NO
      s5=2 ' el loop necesita menos cpu se libera
      trasponer=0
      If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Or instancia < ARG3_TITU Then 
      Else
      SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_LIBERADO)
      EndIf
   EndIf
  Exit Do
EndIf

If  MultiKey (SC_N) Then  

   If COMEDIT<>LECTURA  Then
    If parametros=1 Then
       If s9=0  Then
          s9=1
          parametros=0
       EndIf
       Exit Do
    EndIf
    If parametros = 0  Then
       If s9=0  Then
          s9=1 
          parametros=1
       EndIf  
       Exit Do 
    EndIf 

   EndIf 

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

If MultiKey(SC_ALT)   Then '' mover pantalla a derecha en cualquier modo edicion aun CTRL-M 
 If MultiKey (SC_RIGHT) Then
    posicion=posicion + NroCol/100
    If posicion > MaxPos Then
      posicion = MaxPos
    EndIf
    posishow=posicion
 EndIf
EndIf

If MultiKey(SC_ALT)   Then '' mover pantalla a derecha en cualquier modo edicion aun CTRL-M 
 If MultiKey (SC_LEFT) Then
    posicion=posicion - NroCol/100
    If posicion < 1 Then
      posicion = 1
    EndIf
    posishow=posicion
 EndIf
EndIf


' UNA NEGRA SON 96, CORCHEA 48, SEMICORCHEA 24, FUSA 12, NOS MOVEMOS CON FUSA EN TICKS00
If MultiKey(SC_CONTROL) And lockhoriz=0 Then 
 If MultiKey (SC_RIGHT) Then
   If COMEDIT=LECTURA Then
     posicion=posicion + 24 ' UNA corchea ''NroCol*2
     If posicion > MaxPos Then
       posicion = MaxPos
     EndIf
     posishow=posicion
     lockhoriz=1
     Exit Do
   Else
     curpos= curpos + 24 ' mueve cursor cuando Roll se detiene (posicion)
      If curpos > NroCol  Then
        curpos = NroCol
      EndIf
    EndIf
    Sleep 100
 EndIf    
EndIf

If MultiKey(SC_LSHIFT) And MultiKey(SC_RIGHT) Then
' AVANZAR DE ONOFF=2 A OTRO POSTERIOR o un onoff=1  o un 190
'Print #1,"AVANZA O NO AVANZA AL ONOFF2 ?"
Dim As Integer  x1, y1, xdesde, xllegada
' salta en onoff=2 onoff=1 o dur=190 para ver los errores escondidos
If S8=0 Then
   S8=1 
     If COMEDIT=LECTURA Then
        xdesde=posicion
     Else
        xdesde=curpos +1
     EndIf

   For x1= xdesde +1 To MaxPos
      For y1 = NB To NA-13
         If Roll.trk(x1, y1).onoff = 2 Or Roll.trk(x1, y1).onoff = 1 Or Roll.trk(x1, y1).dur = 190 Then
            xllegada=x1
            onoff=2   ' parametro a mostrar
''Print #1,"AVANZAO AL ONOFF2 "
            Exit For,For
         Else
            onoff=0
         EndIf        
      Next y1 
   Next x1
     If COMEDIT=LECTURA Then
        posicion=xllegada
     Else 
        curpos=xllegada - 1
        If curpos > Maxpos Then
           curpos = MaxPos
        EndIf
     EndIf
  GetMouse mouseX, mouseY, , MouseButtons
   menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
EndIf
 Exit Do
EndIf


If MultiKey(SC_CONTROL) And lockhoriz=0 Then 
  If  MultiKey (SC_LEFT) Then
    If COMEDIT=LECTURA Then
      posicion=posicion - 24' UNA corchea ''NroCol*2
      If posicion < 1 Then
      posicion = 1
      EndIf
      posishow=posicion
      lockhoriz=1
      Exit Do
   Else
      curpos= curpos - 24 ' mueve cursor cuando Roll se detiene (posicion)
      If curpos > NroCol  Then
         curpos = NroCol
      EndIf

   EndIf
   Sleep 100   
   EndIf
EndIf
'''RECETA el codigo de LAS TECLAS COMPUESTAS de Multikey DEBEN ESTAR ANTES DEL codigo de las SIMPLES!!!!
'hagamos esto para cursor tambien...
If MultiKey(SC_LSHIFT) And MultiKey(SC_LEFT) Then
' RETROCEDER A IZQ DE ONOFF=2 A OTRO ONOFF=2 ANTERIOR !!! 
  Dim As Integer  x1, y1, xdesde, xllegada
  If S8=0 Then
     S8=1 
     If COMEDIT=LECTURA Then
        xdesde=posicion
     Else 
        xdesde=curpos
     EndIf

     For x1= xdesde-1 To 1 Step -1
       For y1 = NB To NA-13
         If Roll.trk(x1, y1).onoff = 2 Or Roll.trk(x1, y1).onoff = 1 Or Roll.trk(x1, y1).dur = 190 Then
            onoff=2 ' parametro a mostrar
            xllegada=x1 
            Exit For,For
         Else
            onoff=0 
         EndIf        
       Next y1 
     Next x1
     If COMEDIT=LECTURA Then
        posicion=xllegada
     Else
        curpos=xllegada -1
        If curpos < 0 Then
           curpos = 0
        EndIf
     EndIf
    GetMouse mouseX, mouseY, , MouseButtons
     menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
  EndIf
    Exit Do    
EndIf



If MultiKey(SC_ALT) And MultiKey(SC_F12) Then

EndIf
If MultiKey(SC_ALT) And MultiKey(SC_F11) Then

EndIf


' 03-02-2022 screen event me pone con su 80 trasponer=1 hace una asignacion !!!
If MultiKey(SC_DOWN) Then  ' el screenevent me pone trasponer en 1 la puta e.scancode = 80 Then  ' <===== SC_DOWN pulso
     If trasponer =1 And SelGrupoNota=0  Then
        'print #1,"0 pulso down screenevent TRASPONER con multikey!"
       If s6=0  Then
          s6=1   
         trasponerRoll ( -1,Roll,encancion)
       EndIf   

       Exit Do
     EndIf 
     If trasponer =1 And SelGrupoNota=1 Then
       'print #1,"1 pulso down screenevent TRASPONER"
       If s6=0  Then
         s6=1 
       trasponerGrupo ( -1,Roll,encancion,0,0)
       EndIf  
       Exit Do
     EndIf 
     
    If COMEDIT=MODIFICACION_INSERCION  Or COMEDIT=MODIFICACION_COLUMNA And trasponer= 0 Then 'ctrl-m o ctrl-o, ctrl-n no
     notacur = notacur + 1
     If notacur > 12 Then
      notacur=1
     EndIf
     cambiadur=0 
      Sleep 100
      Exit Do
    EndIf
    If COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS  And trasponer=0 Then 
       If s1=0 Then
          s1=1
        'print #1,"pulso down screenevent"
        BordeSupRoll = BordeSupRoll -  inc_Penta
       EndIf
      If BordeSupRoll <= - AltoInicial * 2.8  Then
         BordeSupRoll =  - AltoInicial * 2.8
      EndIf

      Exit Do
    EndIf
    

EndIf

If MultiKey (SC_UP) Then
    If trasponer=1 And SelGrupoNota=0 Then
      If s6=0  Then
         s6=1
        trasponerRoll ( 1,Roll,encancion)
      EndIf
While InKey <> "": Wend
      Exit Do
    EndIf
    If trasponer = 1 And SelGrupoNota=1 Then
       If s6=0  Then
          s6=1 
         trasponerGrupo ( 1, Roll,encancion,0,0)
       EndIf  
While InKey <> "": Wend
       Exit Do 
    EndIf 

    If (COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS)  And trasponer=0 Then
     If s2=0 Then
      s2=1
         'print #1,"pulso UP r 1 inc_penta"
      BordeSupRoll = BordeSupRoll +   inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf
While InKey <> "": Wend     
     Exit Do
    EndIf
    If COMEDIT=MODIFICACION_INSERCION Or COMEDIT=MODIFICACION_COLUMNA And trasponer= 0 Then ' no se usa con ctrl-n,pero si en ctrl-m 1 y ctrl-o 3 futuro columna
     notacur=notacur-1
     If notacur < 1 Then
      notacur=12
     EndIf
     cambiadur=0  
      Sleep 50
While InKey <> "": Wend
     Exit Do
    EndIf

EndIf
'   escala = escala - 0.1

 'If  mouseY < 50  And MultiKey(SC_RIGHT)   Then ' <======== RIGHT
 ' seleccion de menu, mouse sobre cinta + teclas
 '     menuNro=menuNro+1
 '    If menuNro > 11 Then ' 28-08-2021 
 '      menuNro=0
 '      menuNew=0
 '    EndIf
 '    menuNew=menuNro
 '    Sleep 500
 '    Exit Do
 'EndIf

  'kNroCol cantidad scroll de NrocOL)
'   escala = escala + 0.1
                '  <========== LEFT
 'If  mouseY < 50 And MultiKey(SC_LEFT) Then  ' seleccion de menu
 ' menuNro=menuNro - 1
 ' menuNew=menuNro
 ' If menuNro < 0 Then
 '  menuNro=11  ' 28-08-2021
 '  menuNew=11
 ' EndIf
 ' Sleep 500
 ' Exit Do
 'EndIf



If MultiKey (SC_F2)  And lockfont=0 Then
' escala = escala - 0.01
   
   anchofig=anchofig - 1
   gap1= anchofig * 6  ' porque tanto era 20
   gap1 = gap1 + 8 
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

   NroCol =  (ANCHO / anchofig ) + 4

 
'   If NroCol > MaxPos Then
'      NroCol=MaxPos
'      anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
'   EndIf

 '  If MaxPos > 100 Then
 '   ' NroCol =  (MaxPos/ anchofig ) - 4
 '  EndIf
   'If NroCol < 800 Then
   '   NroCol = 800
   'endif 
   If  anchofig < 1 Then
       anchofig = 1
   EndIf    
   nanchofig=anchofig
   font=font - 0.5
   Sleep 50
   If font < 5 And font >0  Then
    font=18 *3/5
    DUR => 0
   curpos =>1
   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
   gap1= anchofig* 6 ' porque era tanto 20
   NroCol =  (ANCHO / anchofig ) + 4 '6
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

   EndIf
   
'   dim as integer figancho 
'   for t1 as Integer = 1 to 30  
'   figancho=mispx(t1, 2)
'   if figancho=anchofig then
'      font=t1
'   endif
'   next t1
   ANCHO3div4 = ANCHO *3 / 4
   lockfont=1
  Exit Do
EndIf

If MultiKey (SC_F3)  And lockfont=0 Then
   anchofig=anchofig + 1
   gap1= anchofig * 6 ' era porque tanto 20 '81 default
   gap1=gap1 - 1
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

   NroCol =  (ANCHO / anchofig ) + 4

 '  If NroCol > MaxPos Then
 '     NroCol=MaxPos
 '     anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
 '  EndIf
  ' If MaxPos > 100 Then
    ' NroCol =  (MaxPos/ anchofig ) - 4
  ' EndIf

'   If NroCol < 800 Then
'      NroCol = 800
'   endif 

'->

'   nanchofig=anchofig

'   Dim As Integer figancho 
'   For t1 As Integer = 1 To 30  
'   figancho=mispx(t1, 2)
'   If figancho=anchofig Then
'      font=t1
'   EndIf
'   Next t1


'   If  anchofig > 175 Then
'       anchofig = 175
'   EndIf    
   font=font+0.5  
   If font > 50 Then
      font=50
   EndIf 
   lockfont=1
   nanchofig=anchofig
   ANCHO3div4 = ANCHO *3 / 4
 Exit Do
EndIf
'----------
' SIZE ANCHO F5
' F5 SCANCODE 63 , F6 64
' CAIRO TIENE UNA FUNCION DE ESCALA QUE LA DESCARTE VOLVER A VER ESO
If  MultiKey (SC_F5)   Then
 
 If COMEDIT=LECTURA Then
'  escala = escala - 0.01
'  translado = translado - 100
 EndIf

 Exit Do

EndIf

If  MultiKey (SC_F6)  Then
 If COMEDIT=LECTURA Then

 'escala = escala + 0.01
' translado = translado + 100
 EndIf

 Exit Do
EndIf

 If MultiKey(SC_H) And notaGuia=0 Then
' apaga las notas Guia al costado izquierdo de Roll Grafico
' para encender pulsar Q
      notaGuia=1
      Exit Do
 EndIf
 If MultiKey(SC_K)  Then
     notaguia=0 'vuelve las notas guia
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
      nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0) +"*.roll;*.rtk"+Chr(0), OFN_CREATEPROMPT)
      Sleep 100
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
s5=2 '11-06-2022
  EndIf 
 
EndIf

If MultiKey(SC_ALT) and MultiKey(SC_L)  Then ' <======== playloop
  s5=0  ' un play con grafico necesitamas cpu
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
    '  Print #1,"ig= ",ig
    '  Print #1," undo_kant_intervalos(ig) ", undo_kant_intervalos(ig)
    '  Print #1,"cnt_acor",cnt_acor
' borrado de la informacion del cifrado de acorde en la octava mas alta no usada
      Dim As Integer n0,pnr      
      pnr=undo_acorde(ig,0).pn ' es el indice de Roll nR
      
      n0=restar(pnr)+1 ' seria la octava en donde se trabaj�
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
      '   Print "se anulo indices info de undo"
         ig=0
      EndIf
      scan_alt=1

      Sleep 100
   EndIf
   While InKey <> "": Wend
   scan_alt=1
   Exit Do
Else
   While InKey <> "": Wend
   scan_alt=0

EndIf   
' ////////////////// UNDO ACORDE O NOTAS ENTRADAS MELODIA ///////////////
' DEBEMOS VER CUANDO BORRAR LOS VALORES GURADADOS, O AL GRABAR, CREO QUE NUNCA DURANTE LA SESION
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
            Roll.trk(mel_undo(ik).posn, ij).vol  =0
            Roll.trk(mel_undo(ik).posn, ij).pan  =0
            Roll.trk(mel_undo(ik).posn, ij).pb  =0
            Roll.trk(mel_undo(ik).posn, ij).inst =0
            Roll.Trk(mel_undo(ik).posn, ij).onoff=0

         Next ij 
     
     MaxPos=MaxPos-1
     mel_undo_k=mel_undo_k -1

     Sleep 100
   EndIf
   
   While InKey <> "": Wend
   scan_alt=1
   Exit Do
Else
   While InKey <> "": Wend
   scan_alt=0
   
EndIf 



If MultiKey (SC_F12) And abierto=0 Then
'''archivo AAAAA-test.TXT

abierto=1
 Dim As Integer i1, i2
 Dim As String result 
 ' testeo solo en la 1er octva por ahora
 ' Print #5,
 Dim As Integer oct1, oct2
 oct1= 0 + (EstoyEnOctava-1) * 13 
 oct2 = 11 + (EstoyEnOctava-1)*13
oct1=NB
oct2=NA

  Print #fk,"vuelco de ARCHIVO COMPELTO ";EstoyEnOctava; " desde ";oct1;" a ";oct2
 For i1 = 50 To 80 ''Step -1
  'For i2= 1 To Maxpos
   For i2= 200 To 300 
   result = Format (Roll.trk(i2, i1).nota,"000")
   Print #fk,  result;"-";
  Next i2
   Print #fk,
  For i2= 200 To 300
   result = Format (Roll.trk(i2, i1).dur,"000")
   Print #fk, result;"-";
  Next i2
  Print #fk,
  For i2= 200 To 300
   result = Format (Roll.trk(i2, i1).vol,"000")
   Print #fk, result;"-";
  Next i2
Print #fk,
  For i2= 200 To 300
   result = Format (Roll.trk(i2, i1).onoff,"000")
   Print #fk, result;"-";
  Next i2


  Print #fk,
  Print #fk, String (8,"*")
 Next i1
 While Inkey <> "": Wend

 Sleep 150
  Print #fk,"fin >>>>>>>>>>> "
 cerrar fk
EndIf

If MultiKey (SC_F9) And lockfont=0 Then
 font = font - 0.5
 gap1=gap1 + 3
 If font < 5 And font >0  Then
   font=18
   DUR => 0
   curpos =>1
   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
   gap1= anchofig* 6 ' porque era tanto 20
   NroCol =  (ANCHO / anchofig ) + 4 '6
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

 EndIf
 lockfont=1
 Exit Do
EndIf

If MultiKey (SC_F10) And lockfont=0 Then
 font = font + 0.5
 gap1 = gap1 + 3
 If font > 50 Then
  font=50 ' pisa la 1er duracion mas de ahi....
 EndIf
 lockfont=1
 Exit Do
EndIf
'6pt   	8px 	0.5em 	50%
'7pt 	   9px 	0.55em 	55%
'7.5pt 	10px 	0.625em 	62.5%
'8pt   	11px 	0.7em 	70%
'9pt 	  12px 	0.75em 	75%
'10pt 	13px 	0.8em 	80%
'10.5pt 	14px 	0.875em 	87.5%
'11pt 	15px 	0.95em 	95%
'12pt 	16px 	1em 	100%
'13pt 	17px 	1.05em 	105%
'13.5pt 	18px 	1.125em 	112.5%
'14pt 	19px 	1.2em 	120%
'14.5pt 	20px 	1.25em 	125%
'15pt 	21px 	1.3em 	130%
'16pt 	22px 	1.4em 	140%
'17pt 	23px 	1.45em 	145%
'18pt 	24px 	1.5em 	150%
'20pt 	26px 	1.6em 	160%
'22pt 	29px 	1.8em 	180%
'24pt 	32px 	2em 	200%
'26pt 	35px 	2.2em 	220%
'27pt 	36px 	2.25em 	225%
'28pt 	37px 	2.3em 	230%
'29pt 	38px 	2.35em 	235%
'30pt 	40px 	2.45em 	245%
'32pt 	42px 	2.55em 	255%
'34pt 	45px 	2.75em 	275%
'36pt 	48px  3em 	   300% 	
'px	1/96 of 1 inch (96px = 1 inch)	font-size: 12px
'pt	1/72 of 1 inch (72pt = 1 inch)	font-size: 12pt;

If MultiKey(SC_CONTROL) And MultiKey(SC_F4)  Then
''ACA LOGRAMOS CERRAR LA PANTALLA DE ROLL GRAFICO!!!
'' SIN TERMINAR EL PROGRAMA  LO QUE HACE POSIBLE ESTO, es screen 0 en realidad
'' NO DESTRUIR NADA Y SEPUEDE VOLVER A USAR
'' CERRAMOS EL GRAFICO, PERO EL GRAFICO ES UNICO,,,,

     If play=SI Or playb=SI Or Cplay=SI Then 'detenemos los play
      ' MessBox("","(2)Detenga el play primero ")
      ' SetForegroundWindow(hwnd)
      '   Terminar=0
      '   Exit Do 
        PARAR_PLAY_MANUAL=SI
        PARAR_PLAY_EJEC=SI       
     EndIf


Dim As Integer i3

  If MessageBox(hWnd,"�CERRAR GRAFICO ? " ,param.titulo ,4 Or 64) =6  Then
     eventM=eventrbdown ' por si selecciono algo en lista pistas y quedo el loop de menu popup
      ' va a usar sc_p para para r el play y vuelve 
     
    terminar=NO_TERMINAR_CON_DATOS_CARGADOS
    If teclado=1 Then ' detenemos los midi in
      cancel_callback(midiin(pmTk(ntk).portin ))
      Dim k1 As Integer
      k1=pmTk(ntk).portout
      alloff( pmTk(ntk).canalsalida,k1 )  
      listoutAbierto(k1)=0
      close_port midiout(k1)
      out_free(midiout(k1))

    End If 

FileFlush (-1)
    
   SCREEN 0 ''', , ,  GFX_SCREEN_EXIT '' &h80000000 
 ''https://www.freebasic.net/forum/viewtopic.php?t=26963
   If ubirtk =2 Or ubiroll = 2 Then
       salir()
       Kill "procesos.txt"
       Close
       End 0 ''31-03-25 si entro por linea de comando es 2 
   EndIf
   If MaxPos > 2 Then
     abrirRoll=REABRIR_ROLL_CON_DATOS_CARGADOS 
     Terminar=NO_TERMINAR_CON_DATOS_CARGADOS
   Else
     abrirRoll=NO_CARGAR
     reiniciar=1
   EndIf
  Exit Sub

  Else
  Terminar=NO_TERMINAR_BARRE_PANTALLA
   Exit Do ' 06-05-2024
  EndIf  
EndIf

'--------------------------------------
If MultiKey(SC_ESCAPE) Or  Terminar=TERMINAR_POR_ESCAPE Then
    Sleep 5

     If terminar=NO_TERMINAR_BARRE_PANTALLA And (play=SI Or playb=SI Or Cplay=SI)   Then 'detenemos los play
       MessBox("","Deteniendo play pulse escape de nuevo ")
        SetForegroundWindow(hwnd)
        PARAR_PLAY_MANUAL=SI 
        PARAR_PLAY_EJEC=SI
        TERMINAR=NO_TERMINAR_CON_DATOS_CARGADOS '3 
         Exit Do  ' REINICIO DETENIENDO LOS PLAY Y AL PULSAR OTRO ESCAPE ENTRA EL DIALOGO
     EndIf
  If terminar=TERMINAR_POR_ESCAPE Then
        salir()
       Kill "procesos.txt"
       Close
       End 0
  Else   
    If  MessageBox(hWnd,"�TERMINA PROGRAMA ROLLMUSIC ? " ,param.titulo ,4 Or 64) =6 Then
       salir()
       Kill "procesos.txt"
       Close
       End 0
    Else
       Terminar=NO_TERMINAR_BARRE_PANTALLA
       Exit Do ' 06-05-2024
    EndIf
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
 If COMEDIT<>LECTURA Then
    espacio = 1
    DUR=0
    nota=notacur ''nsE 10-05-2021 00:06 probar de nuevo 
    If COMEDIT=SOLO_MODIFICACION Then  ' ctrl-n
      agregarNota = 1
    EndIf

 Else
   If playb = NO And play=NO And Cplay=NO And MaxPos > 1 Then ' 23-02-22 ningun play
      GrabarPenta=0
      naco=0:naco2=0
      If INSTANCIA = ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Or instancia < ARG3_TITU Then 
      Else  
     ' SetGadgetstate(BTN_ROLL_GRABAR_MIDI,0) ' 10-04-2022 DE  VENTANA CTROL
       SetGadgetstate(15,0) ' 20-02-2025 
      EndIf
   '   print #1,"SPACE call play"
        If  MaxPos > 1 Then 
         
         If CANCIONCARGADA = TRUE Then
         '    Print #1,"USANDO PLAYCANCION"
             playb=SI : s5=NO 'Necesita mas tiempo de cpu
             threadDetach(thread1)
             threadDetach(thread2)
             
             Sleep 100    
             thread1 = ThreadCall  PlayCancion(Track())
             grabariniciotxt(NombreCancion, CANCION)
             FileFlush (-1)

         Else
           If  MaxPos > 1 Then
      '        print #1,"llama a playall"
              Play=SI:s5=NO
             threadDetach(thread1)
             threadDetach(thread2)
             Sleep 100
             thread2 = ThreadCall  playAll(Roll)
           EndIf 

         EndIf


        EndIf   
      
      menunew=MENU_INICIAL
   EndIf
   
 EndIf  
 Exit Do
EndIf

If MultiKey (SC_Q) Then ' con Q se deja de repetir espacios tmbien resetea todo ls banderas de notas
'aca puedo hacer unselect case de lo que quiero resettear
''  Select Case Reset 
 If fijarEspacio=99 Then
  fijarEspacio=0
 EndIf
If pasoZona1 > 0 Or pasoZona2 >0 Or pasoNota > 0 Or trasponer=1 Then ' hubo una trasposicion
   correcciondeNotas(Roll) ' para moverZona no se corrige creo por ahora...
EndIf 
pun=0:silen=0:tres=0:mas=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
deltaip=0:incWheel=0:lockip=0:playloop=0
'anchofig=35
'gap1= (anchofig* 2315)/1000  ' 81 default
'gap2= (914 * gap1) /1000 ' 74 default
'gap3= (519 * gap1) /1000 ' 42 default
'font=18
 NroCol =  (ANCHO / anchofig ) + 4
 '''cursorVert = 0 EL COMEDIT DEBE SEGUIR EL MISMO SE MODIFICA CON CTRL-P O ALT-M O TECLAS
 '''cursorHori = 0
 agregarNota=0
 menuMouse = 0
 nota=0
 DUR=0
'''''' alloff( 1 ) no ahce falta aca para eso esta P
' terminar version reducida de la secuencia
 resumen=0 ' quita separacion de notas
 
  
EndIf
' ----------------------INGRESO NOTAS-------------------------

' MAYUSCULAS PARA SOSTENIDOS
' Ahora en nota se guarda el semitono 1 a 12...,  DUR guarda la duracion
If COMEDIT<>LECTURA Then ' ingreso de notas   

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
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_C)   Then ' C#
 nota = 11
 If espacio > 0 Then
  espacio=11
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_D)  Then ' D#
 nota= 9
 If espacio > 0 Then
  espacio=9
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey(SC_CONTROL) And MultiKey(SC_F) Then ' F#
 nota= 6
 If espacio > 0 Then
  espacio=6
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If  MultiKey(SC_CONTROL) And MultiKey(SC_G)  Then ' G#
 nota= 4
 If espacio > 0 Then
  espacio=4
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_A) Then
 nota= 3
 If espacio > 0 Then
  espacio=3
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_B) Then
 nota = 1
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_C) Then
 nota = 12
 If espacio > 0  Then
  espacio=12
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_D) Then
 nota= 10
 If espacio > 0 Then
  espacio=10
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do 
EndIf

If MultiKey (SC_E) Then
 nota = 8
 If espacio > 0 Then
  espacio=8
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_F) Then
 nota= 7
 If espacio >  0 Then
  espacio=7
 EndIf
 If COMEDIT=SOLO_MODIFICACION Then
  agregarNota = 1
 EndIf
 Exit Do
EndIf

If MultiKey (SC_G) Then
 nota= 5
 If espacio >  0 Then
  espacio=5
 EndIf
 If COMEDIT=SOLO_MODIFICACION  Then
  agregarNota=1
 EndIf
 Exit Do
EndIf


'----teclado
' nueva escritura de teclado midi sobre roll, en esta nueva forma no es on line uno por uno,
' mas bien cuando todos los on obtienen su correspondiente off se libera la informacion y 
' se procesa como un todo, pdoriamos hacerlo parcialmente t alvez a medida que cierran en off
'-----------------

  Dim  As UByte huboerror
  Dim i1 As Integer
' aca entra todaslas veces con cada ON Y OFF si estamos en grabarPenta=1
If GrabarPenta=1 Then
  clickpista=0 ' para que no simule TAB
 'Print #1,"DURK DURKOLD ";DURK, DURKOLD
'  If DURK=0  Then
'     EscribirMidiSobreRoll (notamidi(), durafig(), contid)
'    DURKOLD=DURK
'  EndIf
'  If DURK<>DURKOLD  Then
'    EscribirMidiSobreRoll (notamidi(), durafig(), contid)
'    DURKOLD=DURK
'  EndIf

           
' duras () ahora seran 3 como antes pero * contid duraciones,, y silencios (no desarrollado)
' seran durason y durasoff, flor de kilombito jjaja
' OPCION NO MOSTRAR SILENCIOS,.,,,IF SILENCIO=0 ...
 ' filtro1=128 :filtro2=144
' ACA DEBER ENTRAR SOLO CUANDO 144=128 O SEA LA CANTIDAD DE ON=OFF, QUE CREO SE DARA
' CUADNO huboerror=0 .los resultados deber ir en un vector
' NOTA PARA UNA PRIMER PRUEBA PONEMOS VELMIDI=0 PARA TODOS LOS OFF
' AUNQUE DATO3 NO SEA CERO , LALOGICA DE ROLLMUSIC NECESITA UN 0 PARA OFF
' PORQUE NO SE SI LUEGO CONSTRUYE BIEN LAS LIGADURAS...(REVISAR ALGUN DIA)
  If contid > 0  And (huboerror =0 Or huboerror=2 ) Then 
   ' Print #1, "entro a for contid ,huboerror ";contid,huboerror
    For i1=1 To contid
   ' Print #1, "durafig(i1) ";durafig(i1)
   ' Print #1, "notamidi(i1).durk " ;notamidi(i1).durk
      numfloat=durafig(i1)/((60/tiempoPatron) / FactortiempoPatron)  
      ' la duracion debe mantenerse constante por mas que el tempo cambie 
     ' asi las figuras seran las mismas pero el tempo las tocara mas rapido nada mas
     ' si toco una negra a 120 no sera una corchea, sera una negra,,,
    '''If  numfloat  >= 0.01041666   Then
      
      notaMidiDurk=durafig(i1)
      
      If  numfloat  >= 0.005208325    Then
        'Print #1,"NUMFLOAT nota ";numfloat; " ";notamidi(i1).nota
    'If numfloat  > 0.02 And numfloat <= 7 Then
					For ii=1 To 44
					 If numfloat > durcla(ii,1) And numfloat < durcla(ii+1,1) And ii< 45 Then
					 '   Print #1, "1) esta entre ",durcla(ii,1), " y ",durcla(ii+1,1), numfloat
					    numfloat=numfloat-durcla(ii,1)
                   numdurasmidi(i1)=1
                   Select Case notamidi(contid).dato1 ' 144 o 128
                     Case 144  'on
     					     duras(contid,1).dura=ii
                       duras(contid,1).nota=notamidi(contid).nota
                       duras(contid,1).onoff = 1  ' on
                       duras(contid,1).velmidi=notamidi(contid).vel 
                     Case 128 'off
                       duras(contid,1).dura=ii + 45 'silencios
                       duras(contid,1).nota=notamidi(contid).nota
                       duras(contid,1).onoff=2 'off
                       duras(contid,1).velmidi=0
                  End Select 
	       	     
					    Exit For
					 EndIf
			
               Next ii
'			     Print #1,"---------teclado 1-------------------" 
						For ii=1 To 44 
						   If numfloat > durcla(ii,1) And numfloat < durcla(ii+1,1) And ii< 45 Then
					'	     Print #1,"2) esta entre ",durcla(ii,1), " y ",durcla(ii+1,1),numfloat
						     numfloat=numfloat-durcla(ii,1)
                       numdurasmidi(i1)=2
                     Select Case notamidi(i1).dato1
                        Case 144  'on
         					     duras(contid,2).dura=ii
                             duras(contid,2).nota=notamidi(contid).nota
                             duras(contid,2).onoff=1
                             duras(contid,2).velmidi=notamidi(contid).vel 
                             duras(contid,1).dura=duras(contid,1).dura+90

' si tenemos una duracion 2, la anterior debe ser ligada => +90 se cambia 
                        Case 128 ' off
                             duras(contid,2).dura=ii  
                             duras(contid,2).nota=notamidi(contid).nota
                             duras(contid,2).onoff=2
                             duras(contid,2).velmidi=notamidi(contid).vel 
         					     duras(contid,1).dura=duras(contid,1).dura +90

                     End Select 
  	                  
						     Exit For
						   End If
						Next ii
'						Print #1,"--------teclado 2 --------------------"
						For ii=1 To 44
						   If numfloat > durcla(ii,1) And numfloat < durcla(ii+1,1) And ii< 45 Then
			 		'	     Print #1,"3) esta entre ",durcla(ii,1), " y ",durcla(ii+1,1),numfloat
						      numfloat=numfloat-durcla(ii,1)
                        numdurasmidi(i1)=3
                      Select Case notamidi(contid).dato1
                        Case 144
         					     duras(contid,3).dura=ii
                             duras(contid,3).nota=notamidi(contid).nota
                             duras(contid,3).onoff=1
                             duras(contid,3).velmidi=notamidi(contid).vel 
                             duras(contid,2).dura=duras(contid,2).dura+90
                        Case 128
         					     duras(contid,3).dura=ii + 45
                             duras(contid,3).nota=notamidi(contid).nota
                             duras(contid,3).onoff=2
                             duras(contid,3).velmidi=0
                             duras(contid,2).dura=duras(contid,2).dura+90                            
                      End Select 

						     Exit For
						   EndIf
						Next ii
             	'	Print #1," NO HAY NUMDURASmidi 3 "
         numfloat=0

    

       Tiemposilencio=Timer
       
      EndIf 
    Next i1
  Else
    Tiempodelta = Tiemposilencio -Timer
  EndIf
  
EndIf


' ------------fin teclado"

If huboerror =2 Then
 Print #1,"huboerro2 =2 contid > 24 notas! "
EndIf


EndIf  ' COMEDIT<>LECTURA para ingreso de notas 


If MultiKey(SC_BACKSPACE) Then ' sin uso por ahora. 
''podria ser otra forma de colocar 190,190 en lectura �? y
' ir borrando columnas dejandolas vacias con solo 190,190 al ser eliminada al grabar.
' y cargar. 
 backspace=1

EndIf
' ----------------------FIN NOTAS-------------------------

' ----------INGRESO DE DURACIONES DE NOTAS -------------
If COMEDIT<>LECTURA Then  'TERMINA EN 2628
' PORQUE LA CONDICION PARAMETROS_ROLL? PARA ESTAR EN COMMEDIT TRUE ES CONDICION ESTAR
' EN PARAMETROS_ROOL ES REDUNDATE CREO.....PUEDO ESTAR EL PARAMETROS_ROLL Y EN COMEDIT FALSE 
 If (menuNew = PARAMETROS_ROLL Or menuNro=PARAMETROS_ROLL) Then  
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
   'print #1,"DUR ",DUR
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
  
'----------------------------------------
' CURSOR MODIFICCION DE NOTAS ergo usamos nota=0 para evitar entrada normal 
' REVISAR REVISAR REVISAR JMG JMG JMG WWWWWWWWWWWWWWWWWWW  
  If MultiKey(SC_CONTROL) And MultiKey(SC_9) Then 'espacio en edit sin cursor
     DUR = 190:Exit Do
     nota=0
  EndIf

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
  If MultiKey(SC_0) Then 
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
   silen = 1
   Exit Do  ' silencio
   ' indicadorde silencio solo para calculo de compas
  EndIf
  If MultiKey(SC_t) Then
   tres = 1
   Exit Do  ' tresillo
   ' indicadorde tresillo solo para calculo de compas
  EndIf

  If MultiKey(SC_CONTROL) And MultiKey(SC_DELETE) Then
  ' BORRAR COLUMNA AUTOMATICO LUEGO DE BORRAR NOTAS CON 0 Y X
  ' no anda bien 12-12-2021 se cambia a marcar por zona columna o columnas
  ' lo marcad ocon espacio y x se marca con 190,190
     borrarColumnasMarcadas()
  EndIf

 EndIf 
 ' este delete esta fuera porque podra ser usado con cualquier comedit 
' y en la ventana de control o sea la del menu inicial  no el grafico.
 If multikey(SC_DELETE) Then ''cambia a silencio o nada le suma 16+16 ver eso!!!!!!!
      If s7=0 Then
         If borrar=1 Then
            borrar=0
            s7=1
            Exit Do
         EndIf  
      EndIf
      If s7=0 Then
        If borrar=0 Then
           borrar=1
           s7=1 
           Exit Do
        EndIf 
      EndIf
     Sleep 50 
 EndIf
' EL SALTO POR OMISION SERA EL  QUE ELIJA EL USUARIO CON LA DURACION DE 1 A 8 O 0
' PARA VOLVER A 1  
 'preparamos las teclas para saltar desde un onoff=2 al proximo


 ' ojo ver q no haYa  exit do antes !!!!! ?????
EndIf ' COMMEDIT TRUE CTRL-M


' ----HELP PRUEBA DE TEXT
If MultiKey(SC_F1) Then
' por ahora solo traeremos un texto, luego usaremos llamar
' a un archivo de help chm..
   Shell ("start notepad " + pathinicio + "\ayuda.txt")
 ' estopodemos hacer ayuda contextual
 '' Define character range
 '.DRAWASTRING CON FONT DEUSUARIO A VECES SEGUN COMO SE COMPILE HACE CERRAR LA APLICACION
 ' ELIMINADO NOUSAR ESE METODO, USAREMOS CAIRO...(para todo veo ....)
EndIf
'
If MultiKey(SC_R) Then ' recalculo de barras compas a veces no anda �? 
 ReDim compas(1 To CantTicks)
 ReCalCompas(Roll) ' jmg 01-04-21 
EndIf

If COMEDIT=LECTURA Then ' construir cifras para copiar Nveces por ejemplo
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
   silen = 1
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
     Exit Do
 EndIf
 If MultiKey(SC_CONTROL) And MultiKey(SC_HOME) Then
  'TODA LA SECUENCIA ENTRA EN UN PANTALLA
   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
' If font >=5 And font <= 34 Then
'   anchofig= mispx(font-4,2)
'Else
'   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
'EndIf

   NroCol =  (ANCHO / anchofig ) + 4
   gap1= anchofig* 6 ' porque era tanto 20
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

   Exit Do

 EndIf

 If MultiKey(SC_J) And resumen=0 Then
    resumen=1
    Exit Do
 EndIf

 If MultiKey(SC_END)  Then
     posicion=MaxPos - NroCol*3/4
  
    posishow=posicion
    Exit Do
 EndIf
      
' ESTE LLAMADO ACTUA EN COMEDIT=FALSE Y CON pasoZona 1 y/o 2 
  If MultiKey(SC_CONTROL) And MultiKey(SC_DELETE) Then
  ' BORRAR COLUMNA UTOMATICO LUEGO DE BORRAR NOTAS CON ctrl-9 Y X
  ' no anda bien 12-12-2021 se cambia a marcar por zona columna o columnas
     borrarColumnasMarcadas()
     Exit Do
  EndIf


 
EndIf ''' fin sc_END en lectura

 
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
' al usar iniciodeLectura, pero eso s�, con inicio congelaba el movimiento
' del roll ante una entrada de nota hasta el pr�ximo incremento de pantalla
' SOLO SEUSAPARAINGRESO DE NOTAS NUEVAS ..VERIFICANDO JMG
'Print #1,"llega a Nucleo numdurasmidi, nota,nRk  ",numdurasmidi,RelnRnE(nRk), nRk
If numdurasmidi(1)=0 Then numdurasmidi(1)=1 End If

' fuerza escritura en roll de las notas entradas por midi sobre el nucleo de teclas de roll.
Dim As UByte ij=1,ija=1
Dim As Integer posnarranca
Dim As Integer verticalEnOctavaVacia
If GrabarPenta=0 Then 
  contid=1
  numdurasmidi(1)=0
EndIf

'''For ija =1 To contid 

For ij= 1 To numdurasmidi(ija)
 'Print #1,"GrabarPenta duras(ija,ij) "; GrabarPenta; " "; duras(ija,ij).dura
''Print #1,"numdurasmidi(ija), ija, ij "; numdurasmidi(ija), ija, ij
 If GrabarPenta=1 And duras(ija,ij).dura > 0 And HabilitarMIDIIN > 0  Then
  DUR=DUR+duras(ija,ij).dura
 ' nRk es dato2 la notapian oreal que entra por midi 
 '  Print #1,"NUCLEO DUR ",DUR
' la siguiente nota es semitono para rollmusic, sale de notapiano midi
  If ij=1 Then
     nota=relnRnE (duras(ija,ij).nota) ' de 0 132 a 1 a 12 notapiano a semitono
 'Print #1,"NUCLEO nota ",nota
     estoyEnOctava = (duras(ija,ij).nota +nota +1)/13 ' la 1er nota es notapiano 
  EndIf
 'controlEdit=0
 ''octavaEdicion=estoyEnOctava
 EndIf
Next ij
ij= numdurasmidi(ija)
If Tiempodelta > 0 And GrabarPenta = 1 Then
   DUR=TiempoDelta
EndIf

If COMEDIT=ENTRADA_NOTAS  And nota > 0 And agregarNota=0  And carga=0 And nota <=182   Then ' 182 entra el fin de archivo 
' agregue cambiadur para que entre las modificaciones de cursor en ticks
 Print #1,"--------------------------------------------------------------"
' Print #1,">>>START NUCLEO-COMPAS VECTOR posn: "; posn; "suma:";acumulado
' Print #1,">>>START NUCLEO-COMPAS PROCESANDU DUR: " ; DUR;_
'    " nota: ";nota; " figura: ";figura(DUR)
' Print #1,"entro nota ",  nota;"=";  NotasGuia (nota-1) 'nota 1 a 12 o 0 11 inversa?
' ACA PODRIA SER EL PROBLEMA PPPPPPPPP
 If nota >=1 And nota <=12  And DUR=0 Then 'falla no se acepta la entrada 28 feb 25
    nota=0
     Exit Do
 EndIf
  
 Print #1,"empieza posn, posnOff ";posn; " ";posnOff
  If posn=0 Then ''+posnOff ''InicioDeLectura
     posn=2
  EndIf
 Print #1,"posn "; posn 
 
 If controlEdit=0  Then
   controlEdit=1 
   octavaEdicion=estoyEnOctava
 EndIf
  '   print #1,"estoyEnOctava ";estoyEnOctava
 ' And octavaEdicion = estoyEnOctava
 If estoyEnOctava <> 99  And octavaEdicion = estoyEnOctava Or (GrabarPenta=1 And DUR >0 )Then ' estoy en una octava
  '  If indice <= 0 Then
  '      indice = 1
  '  EndIf
  '  If indice >= 128 Then
  '      indice = 128
  '  EndIf
  If nota > 0 And estoyEnOctava < 99 And estoyEnOctava >=1  Or (GrabarPenta=1 And DUR >0 ) Then

   ' ====>  Control PAgindo Horizontal <=======
   '      kNroCol= Int(posicion/60)
   '       print #1, "A:Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota ", _
   '       Roll.trk((nota +(estoyEnOctava -1) * 13),posn).nota
   ' PARA USAR ESTO CON ENTRADA POR MOUSE SOLO DEBO DETERMINAR EL SEMITONO...
   ' y hacer nota=semiotono 1 a 11 con el mouse...el esto es automtico...
   Do ' nota es semitono ahora va de 0 a 11 deborestr 1 a nota
  ' Print #1,"posn, nota, estoyEnOctava ",posn,nota,estoyEnOctava 

'    Print #1,"Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota ",Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota
'    Print #1,"Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur ",Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur
' deberia agregar aca nota=181???

    If estoyEnOctava=90 Then ' entre dos octavas en 654 de RollLoop GrabarPenta=0 se ajusta a 90
       Exit Do,Do
    Else
      If Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = 0 Or Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = 182   Then
         posicion=posn
     '182 el fin de archivo lo puedo pisar para seguir la secuencia O EL 183 fin del evento ojo!!!
         print #1, "ingreso a NUCLEO POSICION=POSN", posicion
        Exit Do
      EndIf
    EndIf
     If HabilitarMIDIIN =0 And  GrabarPenta=0  Then
      posn = posn + 1
      Print #1,"LOOP DO posn = posn + 1 ";posn 
     EndIf

     If HabilitarMIDIIN > 0  And GrabarPenta=1  Then 
        If DUR <= 0.03  Then
        'seria un acorde se esciben en la misma posicion 
        Else
          posn = posn + 1
        EndIf
     EndIf
     
     
'---control barrido de pantalla columna
    If (posn > NroCol + InicioDeLectura) Then
     InicioDeLectura=InicioDeLectura + NroCol
 '    Print #1,"NUCLEO::CONTROL DE PANTALLEO  NroCol,InicioDeLEctura ", NroCol,InicioDeLEctura
    EndIf
  '   print #1, "ingreso a NUCLEO posn ",posn
   Loop 

    ''' se ajsuta despues MaxPos=Posn +1 
 '   Print #1,"posn, Maxpos,nota ",posn,Maxpos,nota
  '  pmTk(ntk).posn=posn
  '  pmTk(ntk).MaxPos=MaxPos


'--- AUMENTO DE CAPACIDAD DEL VECTOR EN 18000 POSICIONES  3 min 
    If CantTicks - MaxPos < 2000 Then
       print #1,"hace backup....." ' si no hay nombre usa fecha"
       '''GrabarArchivo(1) ''hacer un backup !!! 
      CantTicks=CantTicks + 18000 ' incremento el tama�o en 18000 posiciones =3 min
      ReDim Preserve (Roll.trk ) (1 To CantTicks,NB To NA)
      ReDim Preserve compas(1 To CantTicks)
      ReDim Preserve (RollAux.trk) (1 To CantTicks, NB To NA)
      ' �a que ntk apunta al 0 o al de cancion ?
      print #1,"REDIM EN NUCLEO DE TRACK NTK= ", ntk
      ReDim Preserve (Track(ntk).trk)(1 To CantTicks,1 To lim3)
    EndIf
   


   '' ESTO ME UBICA EN QUE RENGLON DE LA OCTaVA ESTOY SN USAR EL MOUSE
   ' CON EL MOUSE tambien se hizo
  ' >>>>>>CARGA ROLL NOTA Y EL ON :---------------------------------

   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = nota 'carga visualizacion
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).onoff = 2 'on de la nota
' para indicar  que la columna almacena datos de una nota, ej 4 a 8
' 12 + (8-2)*13 + 8 - 4= 90 +8 -4 = 94

  If  GrabarPenta=1 And  HabilitarMIDIIN > 0  Then
   'Print #1,"IJA IJ duras(ija,1).velmidi "; ija, ij,duras(ija,1).velmidi 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).vol = _
         duras(ija,1).velmidi ' tomamos la 1 e lloop ya termino aca no tiene sentido
' estoy fuera del loop ija => ij sera 4 se paso...pero el vol con el  1 es suficiente
  End If

 '  Print #1,"NUCLEO: Posn,nota,EstoyEnOctava ",posn,nota,EstoyEnOctava
   nR=(12-nota) + (estoyEnOctava -1 ) * 13 
   PianoNota= nR - restar (nR)
 '  print #1,"' cargo TRACK nro, PianoNota ",ntk,PianoNota 
   Track(ntk).trk(posn,1).nota= PianoNota ' 
   Track(ntk).trk(posn,1).onoff = 2
   ''' despuesd de la duracion Roll.trk(posn+1,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
   
   'cargo TRACK
   ' despues de la duracion Track(ntk).trk(posn+1,1).dur= 182

   If notaOld > 0 And notaOld <> nota   Then
  '  print #1,"Roll.trk((notaOld +(estoyEnOctava    -1) * 13),posn).nota"; _
   '           Roll.trk((notaOld +(estoyEnOctava    -1) * 13),posn).nota
    Roll.trk(posn,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).dur = 0 '''' deberia ser 181 blanco como mierda uso esto?
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

   Dim As Integer noct,i ''oclog = 8 - (estoyEnOctava-1)
   ' barre solo vertical no la posicion 
   For noct = desde To hasta
     For i= 0 To 11 ' gracias a esto anda acordes�?
       If i= 12 - nota  And noct = estoyEnOctava  Then 
         Continue For
       Else    
       ' semitono ahroa va de 0 a 11 para Roll -> i-1 21-07-2021 jmg
       ' si estoy en fin de sec 182-> nota=0 dur=182
         If Roll.trk(posn,(i +(noct -1) * 13)).nota = 0 And Roll.trk(posn,(i +(noct -1) * 13)).nota < 182  Then ' no borra 182
         '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk(posn,(i +(noct -1) * 13)).nota = 181
            Roll.trk(posn,(i +(noct -1) * 13)).dur  = 0
            Roll.trk(posn,(i +(noct -1) * 13)).onoff  = 0
         EndIf
        ' If Roll.trk((i +(noct -1) * 13),posn).nota = 182 And posn<>MaxPos Then
        ' print #1,"^^^^ cambia 182 en i";i ; "octava "; noct
        '    Roll.trk((i +(noct -1) * 13),posn).nota = 181
        ' EndIf
       EndIf
     Next i
   Next noct
  ' para track permitir acordes no tiene sentido!!
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
   
Print #1,"DUR Q ENTRA  ANTES DE ANALIZAR LO QUE SIGUE + PUN ETC ";DUR
' los bloques de durcion se repiten de a 3, el incr es por bloque
' si voy al 2do bloque de 3 el incrdeja de ser 0 y pasa a 27 respectro de la
' 1er linea l cargo TRAck como 2da linea
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR 'era duracion
   Track(ntk).trk(posn,1).dur = DUR
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
   incr=0
EndIf
' CUART   
If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 9 'era duracion
    Track(ntk).trk(posn,1).dur = DUR +9
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
    incr=0
EndIf
' PUNTILLO   
' 3I* = I = 1 , el puntillo a un 3 suma dando la figura de la q proviene.   
If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 18 'era dur
    Track(ntk).trk(posn,1).dur = DUR + 18
    incr=0
EndIf
'DOBLE PUNTILLO   
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 27
   Track(ntk).trk(posn,1).dur = DUR + 27
   incr=0
EndIf
' SEGUIR  JMG
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 36
   Track(ntk).trk(posn,1).dur = DUR + 36
   incr=0
EndIf   
' -----fin 1er bloque ------------------------

If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   Roll.trk(posn,(12 -nota +(estoyEnOctava -1) * 13)).dur = DUR + 45 'era dur
   Track(ntk).trk(posn,1).dur = DUR + 45
'   print #1," NUCLEO GUARDO EN ROLL CON S DUR: ";DUR +27;" figura:";figura(DUR+27) 
   incr=45
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 54
   Track(ntk).trk(posn,1).dur = DUR + 54
   incr=45
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 63
   Track(ntk).trk(posn,1).dur = DUR + 63
   incr=45
EndIf
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 72
   Track(ntk).trk(posn,1).dur = DUR + 72
   incr=45
EndIf

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=0 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 81
   Track(ntk).trk(posn,1).dur = DUR + 81
   incr=45
EndIf

' -- fin 2do bloque  
 
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 90
    Track(ntk).trk(posn,1).dur = DUR + 90
    incr=90
Print #1,"ACA ESTA >= 90 LOS LIGADOS VIENE CON DUR+ "; Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur
EndIf


If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 99
    Track(ntk).trk(posn,1).dur = DUR + 99
    incr=90
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 108
    Track(ntk).trk(posn,1).dur = DUR + 108
    incr=90
EndIf

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 117
   Track(ntk).trk(posn,1).dur = DUR + 117
   incr=90
EndIf
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 126
   Track(ntk).trk(posn,1).dur = DUR + 126
   incr=90
EndIf
'----- fin 3er bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 135
   Track(ntk).trk(posn,1).dur = DUR + 135
   incr=135
EndIf   

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 144
   Track(ntk).trk(posn,1).dur = DUR + 144
   incr=135
EndIf   

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 153
   Track(ntk).trk(posn,1).dur = DUR + 153
   incr=135
EndIf   

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=1 Then 
   Roll.trk(posn, (12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 162
   Track(ntk).trk(posn,1).dur = DUR + 162
   incr=135
EndIf   

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=1 Then 
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = DUR + 171
   Track(ntk).trk(posn,1).dur = DUR + 171
   incr=135
EndIf   
' SI EL USUARIO CARGO UNA DUR ENTRE 1 Y 90 LA DURACION TERMINA AHI NO HAY
' LIGADURA ERGO SE puede COLOCAR EL OFF DIVIDO LA DURACION POR EL TICK
' Y OBTENGO EL NRO DE POSICIONES ADELANTE DONDE OCURRE EL OFF DE ESA NOTA
' PARA ESTO EN VEZ DE CALCULAR SIEMPRE HARE UNA TABLA DE DUR VS NRO TICKS
'  DurXTick SE LLAMARA
' registramos el off :
' sila duracion no es ligada el off temina en la duracion de esa sola nota
' buscamos la cantidad de posiciones segun esa dur
'  si la dur es menor = 90 no es ligada, la posicion sera donde estoy
' mas la duracion o sea posn+ DurXtick(DUR)
''' la nota ya fue cargada antes en Roll ver mas arriba como  tambien el onoff=2
'' >>>>>>>> CONTINUAMOS CARGA ----------------
'' EN LAS NOTAS LIGADAS PONEMOS TODO 183 >>, ONOFF 1, 182 ||, AUNQUE SE QUE NO VAN AYUDA A SABER QUE 
'' EL ENGANCHE ES CORRECTO LIGADO Y SI EL USU NO PONE LA SIGUIENTE NOTA QUEDA
'' COMO UNA NOTA SIMPLE SIN LIGAR AUNQUE ESTE MAL PERO HABRA UN OFF PARA EL SONIDO
Dim As Integer DURAUX=Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur
  If DURAUX <= 90 Then 'caso 1) no ligado, I, L, W etc, viene algo que termina
    Print #1,"1) DUR <= 90 "
    If posnOffOld > 0 Then ' OK!! caso 4) previo llego un caso 2) ligado y luego ahora un 1) no ligado 
       Print #1,"OK! 1A) DUR <= 90 posnOffOld > 0 "
       posnarranca=posn
       Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).onoff = 0 'borro el on 2 actual ya toco la anterior       
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).onoff=0 'era 1 O 0
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).nota = 181 'borro el 183 anterior >>
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).dur = 0 'borro el 183 >>
       Roll.trk(posnOffOld+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 0 'borro el 182 anterior fin secuencia
       Roll.trk(posnOffOld+6,(12-nota +(estoyEnOctava -1) * 13)).nota = 181 'borro el 182 anterior fin secuencia


       Track(ntk).trk(posnOffOld,1).onoff = 0 'sigue la duracion
       Track(ntk).trk(posn,1).nota = PianoNota
       Track(ntk).trk(posn,1).onoff = 2
       posnOff=posn + DurXTick(DURAUX) -1'nuevo off porque dura mas
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).onoff=1
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).nota=183
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).dur=183

       Print #1,"posn, DUR, DurXTick(DUR), posnoff "; posn, " ";DURAUX;" " ;DurXTick(DUR); " ";posnoff
       Track(ntk).trk(posnOff,1).nota = 183
       Track(ntk).trk(posnOff,1).dur = 183
       Track(ntk).trk(posnOff,1).onoff = 1
       posn=posnOff +1
       Print #1,"posn  "; posn 
       If (posn > NroCol + InicioDeLectura) Then
        InicioDeLectura=InicioDeLectura + NroCol
       EndIf

       Roll.trk(posnOff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
       Track(ntk).trk(posnOff+6,1).dur= 182

       MaxPos=posnoff +6 ' la figura tiene 5 posiciones si pongo 1 o 2  se pisa
       pmTk(ntk).posn=posn
       pmTk(ntk).MaxPos=MaxPos
       notaOld = nota
       posnOffOld=0 
    Else ' OK! 1) viene unan nota on y termina DUR <= 90 solita 
       Print #1,"OK! 1B ) CASO DUR <= 90 posnOffOld = 0 "
       posnarranca=posn 
       posnOff=posn + DurXTick(DURAUX)  -1 '' caso 1) nada entes nada despues
       Print #1," 1B1) posn, DUR, DurXTick(DUR), posnOff "; posn, " ";DURAUX;" " ;DurXTick(DUR);" ";posnOff
       posnOffOld=0
       ''For j As Integer=posn+6 To posnoff-6
       ''  Roll.trk(j,(12-nota +(estoyEnOctava -1) * 13)).dur=184
       ''Next j
        
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).onoff=1
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).nota=183
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).dur=183

       Print #1," 1B2) puso nota 183 en posnOff ";posnOff  '97
       Track(ntk).trk(posn,1).nota = PianoNota
       Track(ntk).trk(posn,1).onoff = 2
       Track(ntk).trk(posnOff,1).nota = 183
       Track(ntk).trk(posnOff,1).dur = 183
       Track(ntk).trk(posnOff,1).onoff = 1

       Roll.trk(posnOff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
       Track(ntk).trk(posnOff+6,1).dur= 182

       posn=posnOff+1  ' por omision se va al fina de la duracion el usuario puede retroceder
    ''   If (posn > NroCol + InicioDeLectura) Then
    ''    InicioDeLectura=InicioDeLectura + NroCol
    ''   EndIf
       Print #1,"1B3) posn, posnOff "; posn; " "; posnOff
       MaxPos=posnOff + 6 
       pmTk(ntk).posn=posn
       pmTk(ntk).MaxPos=MaxPos
       notaOld = nota
       posnOffOld=0
    EndIf
      nroPartesNota=DurXTick(DUR)
  Else 'DUR > 90  ... caso 2) la nota ligada n+ 
    Print #1,"2 ) DUR > 90 "
    If posnOffOld > 0 Then 'caso 4 o 5 vino una n+ yluego n+ otra ligada 
       Print #1,"OK! 2A) DUR > 90 posnOffOld > 0 "
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).onoff=0 ' se borra el off 1 anterior
       Roll.trk(posn,      (12-nota +(estoyEnOctava -1) * 13)).onoff=0 ' se borra el off 2 actual
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).nota = 181 'borro el 183 anterior >>
       Roll.trk(posnOffOld,(12-nota +(estoyEnOctava -1) * 13)).dur = 0 'borro el 183 >>
       Roll.trk(posnOffOld+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 0 'borro el 182 anterior fin secuencia
       Roll.trk(posnOffOld+6,(12-nota +(estoyEnOctava -1) * 13)).nota = 181 'borro el 182 anterior fin secuencia



       Track(ntk).trk(posnOffOld,1).onoff = 0 'sigue la duracion se borra el 1
       posnOff=posnOffOld + DurXTick(DURAUX) -1
       
       ' ponemos el onoff 1 por si no se carga el siguiente pero en la carga de lsiguiente se
       ' borrar el onoff 1
       posnarranca=posn
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).onoff=1
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).nota=183
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).dur=183
       Track(ntk).trk(posnOff,1).onoff = 1 
       Track(ntk).trk(posnOff,1).nota = 183 
       Track(ntk).trk(posnOff,1).dur = 183
       
       posn=posnOff +1
       posnOffOld=posnOff
       If (posn > NroCol + InicioDeLectura) Then
        InicioDeLectura=InicioDeLectura + NroCol
       EndIf
       MaxPos=posnOff +6 
       pmTk(ntk).posn=posn
       pmTk(ntk).MaxPos=MaxPos
       notaOld = nota
       Roll.trk(posnOff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182 'FIN DE SECUENCIA +6 POSICIONES
       Track(ntk).trk(posnOff+6,1).dur= 182
      ' continua en otra nota n+ o termina en otra nota sin ligar,,
       
    Else
       Print #1," OK! CASO 2B) nota ligada sin historia DUR > 90 posnOffOld = 0 "
       posnOff=posn + DurXTick(DURAUX) -1 ' caso 1) nada entes ligada despues
       posnOffOld=0
       posnarranca=posn
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).onoff=1
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).nota=183
       Roll.trk(posnOff,(12-nota +(estoyEnOctava -1) * 13)).dur=183


       Track(ntk).trk(posn,1).nota = PianoNota
       Track(ntk).trk(posn,1).onoff = 2
       Track(ntk).trk(posnOff,1).nota = 183
       Track(ntk).trk(posnOff,1).dur = 183

       Track(ntk).trk(posnOff,1).onoff = 1
       posn=posnOff +1
       If (posn > NroCol + InicioDeLectura) Then
        InicioDeLectura=InicioDeLectura + NroCol
       EndIf
       MaxPos=posnOff +6 
       pmTk(ntk).posn=posn
       pmTk(ntk).MaxPos=MaxPos
       Roll.trk(posnOff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
       Track(ntk).trk(posnOff+6,1).dur= 182
      ' la sihiiente terminaa o seguira la nota,,,
       posnOffOld=posnoff
       notaOld = nota
    EndIf
     nroPartesNota=DurXTick(DURAUX)
  EndIf
  
  If nroPartesNota > 0 Then 'limpia entre el nacimiento y el muere de la duracion de la nota..eje kk 
     For kk As Integer = posnarranca+1 To posnarranca + nroPartesNota -1
       If notaOld > 0 And notaOld <> nota   Then
        Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).dur = 0 
        Roll.trk(kk,(12-notaOld  +(estoyEnOctava    -1) * 13)).dur = 0 
'   Print #1,"A) Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota ";Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota 
'   Print #1,"A) Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota ";Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota 
        Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota = 181
        Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).onoff = 0
'   Print #1,"B)Roll.trk(kk,(12-notaOld  +(estoyEnOctava -1) * 13)).nota ";Roll.trk(kk,(12-notaOld  +(estoyEnOctava -1) * 13)).nota 
        Roll.trk(kk,(12-notaOld  +(estoyEnOctava    -1) * 13)).nota = 181
        Roll.trk(kk,(12-notaOld  +(estoyEnOctava    -1) * 13)).onoff = 0
       EndIf
  Next kk

   Dim As Integer noct,i ''oclog = 8 - (estoyEnOctava-1)
   ' barre solo vertical no la posicion 
   For noct = desde To hasta
     For i= 0 To 11 ' gracias a esto anda acordes�?
       If i= 12 - nota  And noct = estoyEnOctava  Then 
         Continue For
       Else    
       ' semitono ahroa va de 0 a 11 para Roll -> i-1 21-07-2021 jmg
       ' si estoy en fin de sec 182-> nota=0 dur=182
        For ll As Integer = posnarranca+1 To posnarranca + nroPArtesNota -1  
         If Roll.trk(ll,(i +(noct -1) * 13)).nota >= 0 And Roll.trk(ll,(i +(noct -1) * 13)).nota < 182  Then ' no borra 182
         '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct
 'Print #1,"C)  Roll.trk(ll,(i +(noct -1) * 13)).nota ";  Roll.trk(ll,(i +(noct -1) * 13)).nota 
     
            Roll.trk(ll,(i +(noct -1) * 13)).nota = 181
            Roll.trk(ll,(i +(noct -1) * 13)).dur  = 0

         EndIf
         Next ll
       EndIf
     Next i
   Next noct
  ' para track permitir acordes no tiene sentido!! ???????????????
   For i=1 To lim2
     For ll As Integer = posnarranca+1 To posnarranca + nroPArtesNota -1
         If Track(ntk).trk(ll,i).nota >= 0 And Track(ntk).trk(ll,i).dur <182    Then
         '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Track(ntk).trk(ll,i).nota = 181
            Track(ntk).trk(ll,i).dur  = 0

         EndIf
        Next ll  
   Next i


     nroPartesNota=0
  EndIf
 
' si es ligada se le sumara la nota que sigue ...> SEGUIR JMG
' SE BORRA LO ANTERIOR Y SE AGREGA OFF 1 EN EL NRO TOTAL DE POSICIONES
' DE LA LIGAA CON LA SIGUIENTE,,11 O 2 O 3 LAS QUE HALLA
'------------------
mel_undo_k = mel_undo_k + 1

mel_undo(mel_undo_k).trk = ntk
mel_undo(mel_undo_k).posn = posn
'mel_undo(mel_undo_k).columna.pn = nR
'mel_undo(mel_undo_k).columna.dur = DUR
'mel_undo(mel_undo_k).columna.nota = nota


' ---fin 4to bloque -
If GrabarPenta=1 And HabilitarMIDIIN > 0 Then   
 print #1," NUCLEO GUARDO DUR EN ROLL,NOTA, FIG ";DUR;" Pianonota ";Pianonota
 Print #1, "contid ";contid
EndIf
  ' print #1," NUCLEOBUSC Y GREG EN POSIICON :" ;posn
 '  If DUR=64 Then
 '   Roll.trk((notaOld +(estoyEnOctava -1) * 13),posn).dur = 65
 '   DUR=0
 '  EndIf
   cuart=0:pun=0:doblepun=0:tres=0:silen=0:mas=0
   ' mayorDurEnUnaPosicion (posn) quedo <--defectuoso
     'If DUR >=1 And DUR <= 72 Then
   '-----   DUR = Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur
      
   '-----   calcCompas(posn,Roll) 'lrepeticion no espor calcCompas
     'EndIf
   '   rmerr = Err
   '  print #1,"Nucleo Error "; rmerr

   nota = 0

 '  If DUR= 181 Then ' PARA QUE SIRE ?? ME JODE LA ENTRADA
 '     DUR=0
 '  EndIf

  Else ' edicion de nota anterior retroceso, concosco la posicion la octava
   'pero no la nota 1ero debo recuperar la nota, cursor lo sabe tomar de ahi
   'no puedo usar notaOld porque eso seria solo en el caso de que estaba editando
   ' para esa nota 1ero deberia oder moverme arribay bjo con el cursor para
   ' posicionarmeen una nota....ctrl-felcahs verticales ubicaran en cursor
    nota=0
  EndIf   ' comentar el endif para teclado
  nota = 0 '18-11-2021
  'print " Roll ", Roll.trk(indice, posicion)
 
  ' mostrarla en la t del Roll en el indice correspondiente
  ' ocalculo elindice en cada t y meto la nota o saco en t las otas
  ' del vector Roll pra ello acaa la grabo enRoll
 EndIf ''comentar el endif para teclado
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
   ''' crearsecuencia(track(ntk).trk(), posn,ntk) habilitar cuadno todo ande bie
   ' en pausa secuecnia seria un pasopara crear un archivo midi de 1 track
   ' o crear un [[[play mas sencillo]] y rapido
  EndIf 
  nota=0 '18-11-20201

 ''''Exit Do ' 09-01-22 probando no debe sali rdebe seguir chequeando
  
EndIf

If GrabarPenta=1 And HabilitarMIDIIN > 0 Then 
   DUR=0
   Dim As Integer ih1,ih2 
   ih1 = contid
     For ih2= 1 To 3
      duras(ih1,ih2).nota=0
      duras(ih1,ih2).dura=0
      duras(ih1,ih2).onoff=0
      duras(ih1,ih2).velmidi=0
      duras(ih1,1).nRk=0
     Next ih2
     numdurasmidi(ih1)=0
     
EndIf


''''Next ija

If GrabarPenta=1 And HabilitarMIDIIN > 0 Then 
   Dim As Integer ih1,ih2 
   For ih1 = 1 To contid
     For ih2= 1 To 3
      duras(ih1,ih2).nota=0
      duras(ih1,ih2).dura=0
      duras(ih1,ih2).onoff=0
      duras(ih1,ih2).velmidi=0
      duras(ih1,1).nRk=0
     Next ih2
     numdurasmidi(ih1)=0
     notamidi(ih1).nota=0
     notamidi(ih1).dato1=0
     notamidi(ih1).vel=0
     notamidi(ih1).durk=0

   Next ih1
   contid=0
   DUR=0
 
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

' ///////////////////////////////////////////////////////////////////////////////////
'-----------------------------SCREEN EVENT-------START -----------
' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
' para detectar mouse sin usar sdl.. parece que screen event es mas preciso que multikey
' multikey es mas veloz pero cuesta ser preciso.....

If (ScreenEvent(@e)) Then

 Select Case As Const e.type
' ********************************************************************************
'===========================EVENT_WINDOW_GOT_FOCUS================================
' ********************************************************************************
  Case EVENT_WINDOW_GOT_FOCUS
       fueradefoco=0

' ********************************************************************************
'===========================EVENT_WINDOW_LOST_FOCUS=====================================
' ********************************************************************************
  Case EVENT_WINDOW_LOST_FOCUS
       If play = SI Or playb=SI Then ' fuera pero en play
           fueradefoco=NO
       Else     ' fuera de y sin play reducimos consumo CPU
           'Print #1,"1-sleep "; timer
         '  Sleep 20 '12-06-2022 lo puse denuevo
           fueradefoco=1
       EndIf

' ********************************************************************************
'===========================EVENT_MOUSE_Exit=====================================
' ********************************************************************************
 ' 23-08-21 CPU consumo fuera de foco
  Case EVENT_MOUSE_EXIT ' mouse fuera de pantalla del programa  
       If play = SI Or playb=SI Then ' fuera pero en play
           fueradefoco=NO
       Else     ' fuera de y sin play reducimos consumo CPU
        '  Print #1,"2-sleep ";timer
        '   Sleep 20 '12-06-2022 lo puse denuevo
            fueradefoco=SI
       EndIf
' ********************************************************************************
'===========================EVENT_MOUSE_BUTTON_PRESS==============================
' ********************************************************************************

  Case EVENT_MOUSE_BUTTON_PRESS
' el help de freebasic esta mal dice que button left es 0 y es 1 !!!! joder pero anda mejor 
      ' If e.button = 0 Then
      '    MousePress = 1 ???
      ' EndIf
      ' If e.button = 2 Then  ' queda pro probar
      '    MousePress = 2
      ' EndIf
' ********************************************************************************
'============================EVENT_MOUSE_MOVE=====================================
' ********************************************************************************
		Case	EVENT_MOUSE_MOVE
		     MouseMove=1
	
 ' ********************************************************************************
 ' ====================EVENT_MOUSE_BUTTON_RELEASE =========================== 
 ' ********************************************************************************
  Case EVENT_MOUSE_BUTTON_RELEASE ' obtengoPosicion
  '' MousePress = 0
   If mousey < 50 And s5=2 Then
      s5=0  ' necesita mas tiempo de cpu
      Exit Do ' 07-10-2021
   EndIf
   If mousey < 50 And s3=2 Then ' 20-01-2022
      s3=0
      Exit Do       
   EndIf
   If mousey > 50 And SelGrupoNota=2 And( MouseButtons And 1) Then
     'vemos si se clickeo un lugar vacio lo que indicara el fin del grupo y el
     'lugar donde se transportara las notas
    indicePos=(mousex- gap1 )/anchofig + posishow
    mouseyOld=mousey
    estoyEnOctava  =1 + (PianoNota -12 + nsE)/13 
    curpos=(mousex -gap1)/anchofig '''19-07-2025
    notacur=nsE
    resultado= BuscarNota (1,curpos, notacur)
  
   If resultado = 1 Then
      Print #1,"ES UN LUGAR VACION FIN DE GRUPO  ",curpos,notacur
      SelGrupoNota=3
    EndIf


    
    EndIf
 ' ********************************************************************************
 ' ============================== MOUSE WHEEL =========================== 
 ' ********************************************************************************
  Case EVENT_MOUSE_WHEEL      ' <<<=== MOUSE WHEEL
   ' new position & e.z
      
    If cargaCancion=CARGAR_NO_PUEDE_DIBUJAR Then ' 10-10-2021 durante al carga de cancion deshabilitamos
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
' ********************************************************************************
  Case EVENT_KEY_PRESS    ' <======== KEY PRESS PULSO =====================================================
' ********************************************************************************       
   If e.scancode = SC_P   Then ' 25 anda mejor q con multikey
      PARAR_PLAY_MANUAL=SI
      PARAR_PLAY_EJEC=SI
      playloop=NO:playloop2=NO
      s5=2 'necesita menos tiempo de procesamiento    
    EndIf
'-------------------------------------------------------------------
   If e.scancode = 72  Then '<<<==== SC_UP sube por pulsos mas presicion'
  '
  '  If trasponer=1 And SelGrupoNota=0 Then
   '          ' Print #1,"3 TRASPONER !!!!!!!!!!!!!!", trasponer
   '  trasponerRoll ( 1,Roll,encancion)
    ' Exit Do
  '  EndIf
  '  If trasponer = 1 And SelGrupoNota=1 Then
  '           ' Print #1,"4 TRASPONER !!!!!!!!!!!!!!",trasponer
  '   trasponerGrupo ( 1, Roll,encancion)''
'
 '    Exit Do 
  '  EndIf 
    If (COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS)  And trasponer=0 Then ' incluye ctrl-n que puede cambiar en todas las octavas
     If s2=0 Then
      s2=1
         'print #1,"pulso UP r 1 inc_penta"
      BordeSupRoll = BordeSupRoll +   inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf

     Exit Do
    EndIf
'    If cursorVert=1 Or cursorVert=2 Then
'     notacur=notacur-1
'     If notacur < 1 Then
'      notacur=12
'     EndIf
'     Exit Do
'    EndIf
   EndIf
'------------------------------------------------
   If e.scancode = &h41 Then ' <======= SC_F7
    If COMEDIT=LECTURA Then
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
'---------------------------------------------------
   If e.scancode = &h42  Then ' <==== F8
    If COMEDIT=LECTURA Then
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

   '' If e.scancode = 67 Then ' <===== SC_F9
   ''  font = font - 1
   ''  Exit Do
   '' EndIf
   ''  If  e.scancode = 68 Then ' <====== F10 'no funciona no lodetecta !!
   ''      font = font + 1
   ''      Exit Do
   ''  EndIf
' -------------------------------
 
'   If e.scancode = 83 Then '<====== SC_DELETE cambia a silencio o nada le suma 16+16 ver eso
   
'        If borrar=1 Then
'          borrar=0
'          Exit Do
'       EndIf   
'       If borrar=0 Then
'          borrar=1
'          Exit Do
'       EndIf  
'   EndIf 
'------------------------------------------------------
If e.scancode = SC_PAGEDOWN Then  ' PAGEDOWN 81 'FUNCIONARIA EN TODOS LOS COMEDIT
  BordeSupRoll = BordeSupRoll - inc_Penta * 11
  If BordeSupRoll <= - AltoInicial * 2.8 Then
     BordeSupRoll =  - AltoInicial * 2.8
  EndIf
  Exit Do
EndIf
'---------------------------------------------------
If e.scancode= SC_PAGEUP Then  'PAGEUP 
  BordeSupRoll = BordeSupRoll + inc_Penta * 11

 If BordeSupRoll >= AltoInicial * 0.5  Then
  BordeSupRoll =  AltoInicial * 0.5
 EndIf
 Exit Do

EndIf

'-----------------------------------------------------------------------
   If e.scancode = SC_X Then ' 81 <==== SC_X ...fix
    'corrige nota cambia duracion o agrega nota nueva, acorde melodia
    ' solo debe funcionar con CTRL-M
    If COMEDIT=MODIFICACION_INSERCION Then ' ver cursorVert2 archivo nuevo con espacios....sirve??
     cambiadur = 1    ' usando 1 a 8,, para silencios esta delete verificarlo!! 03-05-2025
    EndIf
    Exit Do
   EndIf

'-----------------------------------------------------------------------
   If e.scancode = SC_INSERT And insert=0  Then '34 <===== SC_INSERT con comedit=true
    ' solo valido con CTRL-M
    If COMEDIT=MODIFICACION_INSERCION Then
     ' solo tiene sentido insertar en lo echo y en cursor libre
     insert=1 ' comienzo hbilito el I para insertr nota por nota
     indaux=0
   '  print #1,">>>SC_INSERT ajust STARTINSERT ", StartInsert
   '  If indaux=0 Then ' no haria falta todas las demas inserciones se deben
      'hacer con I no volver a repetir SC_INSERT sino se pulso SC_END
   '   StartInsert = posicion + curpos  ' guardo sin modificar el comienzo xxx ok
   '  EndIf
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
'------------------------------------------------------------
   If e.scancode = SC_I And insert=1 And COMEDIT=MODIFICACION_INSERCION Then
    insert=2 ' habilito cursor para que ingrese
  '  print #1, "-----SC_I -> insert,indaux : ",insert,indaux
   EndIf
'------------------------------------------------------------------
   If e.scancode = SC_END Then ' mueve insercion, podria usarse para ELIMINAR Probar
    If backspace=1 Then ' BORRA HASTA EL FINAL
      Dim As Integer i, y
      For y=posishow To MaxPos
        For i=NB To NA 
          Roll.trk(y,i).nota=0
          Roll.trk(y,i).dur=0
          Roll.trk(y,i).onoff=0
          Roll.trk(y,i).pan=0
          Roll.trk(y,i).vol=0
          Roll.trk(y,i).pb=0
          Roll.trk(y,i).inst=0
        Next i
      Next y
      MaxPos=posishow+1 'jmg 03-04-2021
      posn=posishow
      Roll.trk(MaxPos,nR).dur=182 '  jmg 
      Roll.trk(MaxPos,nR).nota=0
      ReCalCompas(Roll)
      backspace=0
      DUR=0
      nota=0
      Exit Do 
    EndIf
  '01-07-2021 hubo una cancelacion en ctrl-m al pulsar FIN en el teclado con 
  ' con And insert=2 se soluicona pero no se si es funcional para el insert verificar    
    If COMEDIT=MODIFICACION_INSERCION And insert=2 Then ' solo v�lido con Ctrl-M 30-06-2021 
     ' no mas reemplazos
     insert=3 
     'la duracion DUR no hace falta guardarla va cambiando pero almacenando em
    'Cantinserciones con sus ticks mas adelante en el proceso DurXTick(DUR)
     print #1, "-----SC_END StartInsert,indaux,insert,nota: ",StartInsert,indaux,insert,nota
     print #1,"indaux no deberia valer cero !!!! es global "
     
     moveresto ()
     '  param : posicion comienzo (fijo), indice incremental para el aux
     ' ,insert comando habilitado = 1
     '  insert 3 fin reemplazos comienzo de move total
     insert=0:indaux=0
     ReCalCompas(Roll) '''''calcCompas(posn) '' mayorDurEnUnaPosicion (posn)
    EndIf
   EndIf
'-------------------------------------------------------------------------------------
 If e.scancode = SC_LEFT Then
    If DUR > 0 And DUR <= 180 Then 
       deltax=  DurXTick(DUR) 
    Else
       deltax=1
    EndIf
  'MOVER ROLL IZQUIERDA NO CURSOR
    If COMEDIT=LECTURA Then
      Dim kNroCol As Integer ' cntidad de scroll de 66
      posicion = posicion - deltax ' UNA semi FUSA
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
     ''mueveHorizontalmayor50=1
     '' menuNew=2
  Else

     curpos = curpos - deltax ' <=== MOVER CURSOR IZQ
     If curpos < 0 Then
        curpos = 0
     EndIf
     
  EndIf
 GetMouse mouseX, mouseY, , MouseButtons
  menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
    Exit Do

 EndIf

'----------------------------------------------------------------
If e.scancode = SC_RIGHT Then
    ''Print #1,"ENTRA POR ACA SOLO ???"
      If DUR > 0 And DUR <= 180 Then 
         deltax=  DurXTick(DUR) ' 1 a 9 solamente
      Else
         deltax = 1 'si dur=0 => delta = 1 se mueve de a un tick!
      EndIf

     If COMEDIT=LECTURA Then
        posicion = posicion + deltax ' UN Tick  
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
' esta curpos en el nuevo criterio funcionara para ctrl-N  y ctrl-M pero no para ctrl-O
' 
        curpos= curpos + deltax ' mueve cursor cuando Roll se detiene (posicion)
        If curpos > NroCol  Then
          curpos = NroCol
        EndIf
        
     EndIf
   GetMouse mouseX, mouseY, , MouseButtons
   menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
    Exit Do
EndIf  
' ********************************************************************************
'=============PULSAR MUCHO TIEMPO ======= REPEAT ==================================================
' **********************************************************************************
  Case EVENT_KEY_REPEAT
   If e.scancode = 72  Then ' <======= SC_UP
    If COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS  And trasponer=0 Then
     If s2=0 Then
      s2=1
    '     print #1,"pulso UP screenevent 2 inc_Penta"
      BordeSupRoll = BordeSupRoll +   2 * inc_Penta
     EndIf
     If BordeSupRoll >= AltoInicial * 0.5  Then
      BordeSupRoll =  AltoInicial * 0.5
     EndIf
     Exit Do
    EndIf
    If COMEDIT=MODIFICACION_INSERCION Or COMEDIT=MODIFICACION_COLUMNA And trasponer= 0 Then
     notacur=notacur-1  ' funcionara con ctrl-m y ctrl-o pero no para ctrl-N
     If notacur < 1 Then
      notacur=12
     EndIf
     Exit Do
    EndIf
   EndIf
'----------------------------------------------------------
   If e.scancode = 80 Then  ' <===== SC_DOWN repeat

    'If cursorVert=1 Then
    '   notacur = notacur + 1
    '   If notacur > 12 Then
    '     notacur=1
    '   EndIf
    'EndIf
    If COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS  And trasponer=0 Then
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
'----------------------------------------------------------
   If e.scancode = 75 Then ' <=====  SC_LEFT repeat
    If DUR > 0 And DUR <= 180 Then 
       deltax=  DurXTick(DUR) ' 1 a 9 solamente
    Else
       deltax=1
    EndIf
  'MOVER ROLL IZQUIERDA NO CURSOR
    If COMEDIT=LECTURA Then
      Dim kNroCol As Integer ' cntidad de scroll de 66
      posicion = posicion - deltax ' UNA semi FUSA
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
     ''mueveHorizontalmayor50=1
     '' menuNew=2
  Else
'    If s9=0 Then
'       s9=1 
        curpos = curpos - deltax ' <=== MOVER CURSOR IZQ
        If curpos < 0 Then
          curpos = 0
        EndIf
'    EndIf
  EndIf
  GetMouse mouseX, mouseY, , MouseButtons
  menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
    Exit Do


   EndIf

    If e.scancode = 77 Then ' <======= SC_RIGHT repeat
          If DUR > 0 And DUR <= 180 Then 
             deltax=  DurXTick(DUR) ' 1 a 9 solamente
          Else
             deltax = 1 'si dur=0 => delta = 1 se mueve de a un tick!
          EndIf

     If COMEDIT=LECTURA Then
        posicion = posicion + deltax ' UN Tick  
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
' esta curpos en el nuevo criterio funcionara para ctrl-N  y ctrl-M pero no para ctrl-O
' 
'       If s9=0 Then
'          s9=1
           curpos= curpos + deltax ' mueve cursor cuando Roll se detiene (posicion)
           If curpos > NroCol  Then
              curpos = NroCol
           EndIf
''      EndIf         
     EndIf
    GetMouse mouseX, mouseY, , MouseButtons   
     menu(c,cm, posicion,menuNro, Roll,ubiroll,ubirtk)
     Exit Do
 
   EndIf

   If e.scancode = &h41 Then ' <============ SC_F7

    If COMEDIT=LECTURA Then
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
    If COMEDIT=LECTURA Then

     ' MOVE VENTANA
     Dim As Integer w,h

     h=ALTO
     ScreenControl GET_WINDOW_POS, x0, y0
     ALTO = ALTO + inc_Penta
     If ALTO >= altoInicial - 1  Then
      ALTO = altoInicial  - 1
     EndIf

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
''       mueveHorizontalmayor50=0
''       menuNew=0
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
 
 
 '-------------------------------------END SCREENEVENT ----------
  
 ' ====> TRABAJO CON MOUSEX Y MOUSEY 
 '------------------------------------------------------------------- 
 If cargacancion=NO_CARGAR_PUEDE_DIBUJAR  Then ' evita flickr por carga de cancion
   GetMouse mouseX, mouseY, , MouseButtons   ' <=======  CLICK EVENTOS
 EndIf
' BASICO DE TIPOS DE EDICION O LECTURA A MODIFICAR SEGUN ESTE CRITERIO NUEVO 20-05-2025
' cursorVert = 0 +  cursorHori = 0 + COMEDIT=FALSE  LECTURA
' cursorVert = 0 +  cursorHori = 0 + COMEDIT=TRUE   ENTRADA DE NOTA MANUAL SIEMPRE AL FINAL DE LA SECUENCIA
' cursorVert = 1 +  cursorHori = 1 + COMEDIT=TRUE   CTRL-M MODIFICACION INSERCION CON X AL FINAL
' cursorVert = 0 +  cursorHori = 2 + COMEDIT=TRUE   CTRL-N MODIFICACION INSERCION SIN X CON NOTA CDEFGAB
' cursorVert = 3 +  cursorHori = 0 + COMEDIT=TRUE   CTRL-O MODIFICACION DE COLUMNAS O ACORDES 
' EL 1 Y 2 CAMBIAREMOS POR CONSTANTES ? veremos
' algo mas basico 
' en cursor ctrl-m con la mano movemos el cursor vertical y horizontal
' o sea cursorVert=1 y cursorHori=1, ctrl-m=1
' en ctrl-N el cursor manualmente se mueve en horizontal o sea cursorHori=2 ctrl-N=2
' pero cursorVert=0 no lo mueven las flechas solo la nota pulsada CDEFGAB o elegida de menu contextual

' si no muevo horizontal pero si vertical seria cursorHori=0 cursorVert=3 podria ser
' para una columna y la posicion seria dada por el proximo comienzo de nota pulsando
' SC_LSHIFT And SC_RIGHT o SC_LEFT   �? se llamaria ctrl-O
  


'******************************************************************************
' **********************<<<<< COMEDIT<>LECTURRA TRUE EDICION >>>>>>>> *******************
'******************************************************************************

'------- MENU CLICK EDIT PARA ENTRAR EN <<<<< COMEDIT = TRUE EDICION >>>>>>>>
' COMEDIT TRUE PARA INGRESO DE NOTAS NUEVAS TIENE cursorVert=0 Y cursorHori=0
 If (mouseY >= edity1 ) And (mouseY <= edity2) Then ' entre 1 y 50 de y
  If (mouseX >= 36) And (mouseX <= 70) And (menuNew=PARAMETROS_ROLL Or menuNro=PARAMETROS_ROLL)  Then
  ' =====> EDIT <=== de para metros color blanco solo para ver parametros
   'SI ADEMAS SE USA CTRL-M SE PUEDE modificar ,agregar acordes e insertar
   ' 1 o varias notas en forma horizontal siemrpe la misma nota
   ' para acorde usar modificar SC_X
   
   menuMouse = 0
   ' ------ 04-03-21 aca siempre es edicin nueva no modificcion, o solo lectura
   ' no hay ningun tipo de edicion todavia
   agregarNota = 0
   menuMouse = 0
   carga=0
   '-----fin 2609 referencia de busqueda
   '''If MouseButtons And 1 Then ' <========= EDICION SOLO INGRESO DE NOTAS NUEVAS
   If MouseButtons And 1 And cierroedit= 0 Then ' no se si funciona mejor lo dejaremos un tiempo
      cierroedit=1 ' no permite modificar mas que una vez
    If s3 = 0  Then
     COMEDIT=ENTRADA_NOTAS  ' EDIT SE PONE EN VERDE PARA ENTRAR NOTAS NUEVAS AL FINAL DE LA SECUENCIA
            
        'print #1, "INVESTIGO COMEDIT ENTRO X TRUE EN MAIN S3: ",S3
      ''font = 18 SACAMOS AHORA ESTO LO MANEJA MENU  
    ''' curpos=0 JMGJMG25FEB
     '''guardopos=posicion
     
     Print #1,"Pulso boton Edit 1) s3, guardopos ",s3, guardopos
     If  GrabarPenta=1 And metronomo_si=3 Then
        terminar_metronomo=0
        Dim As Integer im=0
        For im=1 To 4  
            noteon(60,60,1,0,1)
            noteoff(60,1,0,1)
            duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
        Next im
        threadmetronomo = ThreadCall metronomo()
         contcode =0 ' para detectar 1er nota de midiin porque no envia 144 la 1era vez solo 128!!
          
     EndIf
     ''mayorDurEnUnaPosicion (posn)
     '' calcCompas(pos)
     s3 = 1
     controlEdit=0
     posishow=posicion
     Exit Do
    Else
     If s3=1 Then ' VIENE DE COMEDIT=TRUE PASAMOA A FALSE  
        COMEDIT=LECTURA '': s3 = 0 ' solo LECTURA 06-12-2021
        print #1, "INVESTIGO COMEDIT ENTRO X FALSE EN MAIN S3: ",S3
     'posicion= posicion + curPOS ' estaba mal no va 3-3-21 jmg
          
        If  GrabarPenta=1 Or  metronomo_si=0 Then
         terminar_metronomo=1
        EndIf
        If play=NO Then
          ' Print #1,"2) s3, guardopos ",s3, guardopos
''           posicion=guardopos
       '''posicion = posicion + curpos ' 15-09-2021 jmgjmg ok mejor� 
'           curpos=0
           controlEdit=0 'jmg 09-06-2021 
           nota=0
       ''posicion=posicion - NroCol/2
           s5=2 ' se requiere menos tiempo de cpu
           If posicion < 1 Then
              posicion = 0 ' 20-01-2022 jmg a testear pase a1 y dibuja mal 
           EndIf
         '  posishow=posicion
        EndIf
        s3=2 '1) EDIT PARAMETROS,2) UNCLICK VERDE ENTRA NOTAS NUEVAS COMEDIT=TRUE,3) OTRO CLICK COMEDIT=FALSE S3=2 
        ' VOVLEMOS A MOSTRAR LOS PARAMETROS, PERO YA NO SE PUEDE VOLVER A COMEDIT=TRUE SOLO A MENU PRINCIPAL CON alt_m
        Exit Do
     EndIf
    EndIf
   EndIf 
  EndIf
 ' If S3=2 Then
   '   mouse_event MOUSEEVENTF_ABSOLUTE + MOUSEEVENTF_LEFTDOWN, 900, 25, 0, 0
   '   mouse_event MOUSEEVENTF_ABSOLUTE + MOUSEEVENTF_LEFTUP, 900, 25, 0, 0  
   '   menuNEW = 0
 ' EndIf
 ' RESIZE OCULTADO TAL VEZ SEA UNA OPCION EN OTRA VERSION O USAR SIN FRAME
 ' O CON FRAME SEGUN EL GUSTO DEL USU AUNQUE NO ES PEFECTO EL RESIZE TIENE
 ' SUS VENTAJAS...USAR UN MARCO O NO  EN LA VENTANA DE EDICION ?
  If usarmarco<>usarmarcoOld  Then
     reiniciar=1
    cairo_destroy(cm)
    cairo_surface_destroy( surf2 )
    cairo_destroy(c)
    cairo_surface_destroy( surface )
''''    FT_Done_Face( ftface )

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
 If  s5= 0 And mouseX > 450 And mousex < (ANCHO -70 - mxold) And  usarmarco=0 and mousey < 50 Then
     Sleep 5 '12-06-2022 15-03-2025 decia 20
    If  play=NO And playb=NO Then ' durante un play funciona mal esto => se bloquea su uso por ahora
     x1=mouseX: y1=mouseY
    EndIf
     s5=1  ' tiempo intermedio de cpu???
     Exit Do
 EndIf
 ' =========> MOVER VENTANA DRAGAR LA CINTA SUPERIOR con el mouse
 ' And menuNro= 1  '''348  (2* ANCHO/3)
 If MouseButtons And 1 And S5=1 And mouseX > 450  And mousex < (ANCHO -70 - mxold) And  usarmarco = 0 AND mousey < 50 Then
     Sleep 5  '12-06-2022 15-03-2025 decia 20
    If  play=NO And playb=NO Then ' durante un play funciona mal esto => se bloquea su uso por ahora   
       x2=mouseX
       y2=mouseY
       x0=x0+x2-x1 
       y0=y0+y2-y1
      
      ScreenControl SET_WINDOW_POS, x0, y0
   EndIf   
  ' mientras mantengo presiondo el mouse pudo mover el mouse con la ventana
  ' la performance no es tan buena pero funciona
   
    Exit Do
  
  Else
     s5=0 'mas tiempo de cpu
'   Print #1,"sleep s5 3867 ",timer
 
   
  EndIf
 ''https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-movewindow
 '                           <====== [BOTONES] =======>
 ' 07-08-2021 lugar para test tama�o 10x8
 ' salir de roll music  unificar jmg 02-03-2024
 If (mousex>=(ANCHO-40-mxold)) And (mousey <= 16)  Then 'LA X DE LA VENTANA GRAFICA
  If  MouseButtons And 1  Then
''ACA LOGRAMOS CERRAR LA PANTALLA DE ROLL GRAFICO!!!
'' SIN ERMINAR EL PROGRAMA screen 0 ES LO QUE HACE POSIBLE ESTO
'' NO DESTRUIR NADA Y SEPUEDE VOLVER A USAR
'' CERRAMOS EL GRAFICO, PERO EL GRAFICO ES UNICO,,,,

Dim As Integer i3
     If play=SI Or playb=SI Or Cplay=SI Then
       'MessBox( "","Detenga el play primero ")
       'SetForegroundWindow(hwnd)
       '  Terminar=2
       '  Exit Do
        PARAR_PLAY_MANUAL=SI
        PARAR_PLAY_EJEC=SI 
     EndIf
  If ubirtk > 0 or ubiroll > 0 Then ' valen 2
  Print #1," termina Roll tambien pues es un grafico independiente"
       salir()
       Kill "procesos.txt"
       Close
       End 0
     ''Exit Do 
  EndIf

  If MessageBox(hWnd,"�CERRAR GRAFICO ? " ,param.titulo ,4 Or 64) =6  Then
     eventM=eventrbdown ' por si selecciono algo en lista pistas y quedo el loop de menu popup
     ''''terminar=3
     If teclado=1 Then
        cancel_callback(midiin(pmTk(ntk).portin ))
        Dim k1 As Integer
        k1=pmTk(ntk).portout
        alloff( pmTk(ntk).canalsalida,k1 )  
        listoutAbierto(k1)=0
        close_port midiout(k1)
        out_free(midiout(k1))
     End If 

     FileFlush (-1)
     Screen 0 ''', , ,  GFX_SCREEN_EXIT '''&h80000000
     If Maxpos > 2 Then
      
        Terminar=NO_TERMINAR_CON_DATOS_CARGADOS '3  para que no barra la pantalla 
        abrirRoll=REABRIR_ROLL_CON_DATOS_CARGADOS 'con 4 puede reabrir la ventana con los datos cargados
     
     Else
        abrirRoll=NO_CARGAR
        reiniciar=1
     EndIf
   
     Exit Sub

  Else
     Terminar=NO_TERMINAR_BARRE_PANTALLA
     Exit Do ' 06-05-2024
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
 
   s5=2  ' me nos tiempo de cpu 15-03-2024
   
  '''Sleep  1  ' reemplazamos el de S5=2 de antes eso daba 1 mseg de retardo veremos si sirve aca
  If s3 = 2 Then  ''06-12-2021 jmg
     s3=0
  EndIf   
'  If CANCIONCARGADA Then 12-02-2022 quedaba en edicion siempre  
'     S3=0
'  EndIf
  ''s3 = 2 ''20-01-2021 ' otro estado mas?
 ' <==== MENU CONTEXTUAL INSERCION DE ACORDES CON CTRL+ CLICK DERECHO EN LECTURA ================>
 ' 2 casos 
 ' 1) en la posicion elegida ya existe una nota de una melodia o secuencia
 ' 2) no hay nada. En el 1er caso se tomara por omision a la nota como la t�nica
 ' y se armara un acorde con la misma duraci�n a la nota elegida...
 ' en el caso 2) se necitara 1ero la entrada de una duraci�N o tomar la que ya estaba cargada
 ' en cuyo caso se arma igual que antes el acorde con las selecciones del siguiente menu.
 ' usaremos la denominacion 1,3,5,7,9 ..4,6..11 etc para las posiciones de las notas del acorde.
 ' todo se armara segun la escala elegida previamente...el cual pasmos a desarrollar...16-12-2021
 
 If COMEDIT=LECTURA Then

    copiar=0

' para ingreser automatico acordes a partir de una TONICA futuro--01-12-2021  
' ---------- INGRESO ACORDES SIN EDICION CON MENU DE MOUSE
    If MultiKey(SC_CONTROL) And MouseButtons And 2 Then 'yyy
'////////////////////// MENU ACORDES ////////////////////////////////

       Print #1,"entro al menu acordes"


       pasoZona1=0:pasoZona2=0
       Dim As HMENU hpopup1, cancelar,notas3,notas4,notas5,Noinversion,inversion1, inversion2,May6,Sus2
       Dim As HMENU Mayor,Menor,Dis,Mayor7,Menor7,Dom7,Dis7,Mayor9,Menor9,Dis9, notabase,Aum,Menor7b5,Dom75a     
       Dim As HMENU bVII,bVIIMaj7
       Dim As Integer event,Posx,Posy
    ScreenControl GET_WINDOW_POS, x0, y0
   ' Print #1,"x0 ,y0 en menu contextual " ,x0,y0
   ' Print #1,"mousex ,mousey en menu contextual ", mousex,mousey
   ' Print #1,"Posx ,Posy en menu contextual ", Posx ,Posy
   ' Print #1,"ANCHO ,ALTO en menu contextual ", ANCHO ,ALTO
   ' Print #1,"mxold, myold ", mxold,myold
   ' Print #1,"nmxold, nmyold ", nmxold,nmyold
'
          ' You'd have to get the thread id of the graphics window (GetWindowThreadProcessId), 
' set a WH_GETMESSAGE hook with that thread id and then process the command messages 
' in your hook function. chino basico je                                                                  'WS_THICKFRAME
Dim As hwnd haco
Posx=mousex
Posy=mousey 
MousexNotaElegida=Posx
PianoNotaElegida=PianoNota
'Print #1, "mousex nota original ";mousex
'Print #1, "PianoNota original ";PianoNota
'Print #1, "nsE otiginal "; nsE
' determinacion de la posicion y duracion en el click del mouse...igual que en Sc_Z
    Print #1,"gap1 deberia ser 9 ", gap1
    indicePos=(mousex-gap1)/anchofig + posishow
'Print #1,"indicePos original ",indicePos
 
    ''''estoyEnOctava = *po '17-07-2025 
'' en lectura anda PianoNota
    estoyEnOctava  =1 + (PianoNota -12 + nsE)/13 
    Print #1,"ACORDES: indicePos,estoyEnOctava ",indicePos ,estoyEnOctava
' dur anterior en vector a modificar 
 'si se da click sobre una nota hay que buiscar su comienzo
   curpos=(mousex -gap1)/anchofig '''19-07-2025
  '' curpos=indicePos - posishow      
   Print #1, "1 curpos o Col "; curpos 
   notacur=nsE
   nsEelegida=nsE
   Print #1,"notacur "; notacur  

   resultado= BuscarNota (1,curpos, notacur)  
   If resultado = 1 Then
      Print #1,"BuscarNota sin resultados curpos notacur ",curpos,notacur
   Else
      indicePos=curpos + posishow 
      Print #1, "(I)indicePos, notacur, curpos,posishow ",indicePos, notacur,curpos, posishow
      Print #1,"encontro onoff 2 " ,Roll.trk(indicePos, PianoNota+SumarnR(PianoNota)).onoff 
                
   EndIf

'--------------------------------------------- 
    Rolldur = CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur)
    Rollnota= CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).nota)
   
    If Rollnota = 0 Or Rollnota=181 Or Rolldur=0 or Rolldur=181 Then ' construimos acorde donde no haya nada
       Rollnota = nsE 
       Rolldur = DUR
       Vaciodur= TRUE
    EndIf   
    If DUR <> Rolldur And DUR > 0  Then
       Vaciodur= TRUE ' cambio la duracion se necesita un RecalCompas
       Rolldur=DUR
    EndIf 
    If DUR=0 And RollDur > 0 Then
           DUR=RollDur    
    EndIf
    If DUR=0 And RollDur = 0 Then
          Exit Do     
    EndIf


 
   '''' DUR=0
 '   Print #1,"ACORDES: Rolldur ",Rolldur
 '   Print #1,"ACORDES: nsE ",nsE
 '   Print #1,"ACORDES: nR ",nR
 '   Print #1,"ACORDES: PianoNota del piano ",PianoNota
 '   Print #1,"ACORDES: vovlemos a nR  ",PianoNota + SumarnR(PianoNota)
' nsE,nR y PianoNota se calculan en creaPenta..solo depende de mousey
' aunque de click derecho no importa el mouse y no depende del click 
' PianoNota=(12-nsE +(estoyEnOctava -1) * 13) ' es nR ya lo tengo
' indice de la nota en el vector = nR,vertical como NA NB
' determinacion de la notapiano...
' calcualdo en CreaPenta -> PianoNota= nR - restar (nR) esto para el teclado real pero en roll es nR
' trabajo con Pianonota y luego convietrto a nR de nuevo...,con Pianonota
' para reconvertir volver debo usar SumaNr 
'---------------------------------------------------
' PARA GRABAR el acorde en Roll necesito guadar nsE (nota) ,el nro octava,
' y acordeNro son 3 numeros...con ellos recosntruyo el acorde y lo escribo arriba 
' en la octava y en la posicion gurdada ej CMayor, Dm7, G7 etc 


    ' haco = OpenWindow("Acordes", x0+Posx ,y0+Posy,40,40,WS_VISIBLE Or WS_THICKFRAME , WS_EX_TOPMOST ) 'Or WS_EX_TRANSPARENT  )
' VALOR POR OMISION  NOTA ORIGEN O DE COMIENZO -> 1 tonica
  
' --------------- 24-01-2022 VOY A GRABAR EL ACORDE EN ROLL PARA MOSTRAR SU NOMBRE
' ARRIBA DE LA OCTAVA DONDE SE USA ..CON UN FONT MAS CHICO TAL VEZ ,,,,
' VARIABLE NUEVA acordeNro tipo integer shared 
' la existencia de un acorde la ponemos en .inst con el nro 201 (solo indica buscar acorde)
' de una octava en ocurrencia 12 como las escalas me indica que hay un acorde
' o sea pueden subsistir la informacion de un acorde y una escala 77777
' el resto de la informacion del acorde son nsE y acordeNro la poneos en la octava
' que no se usa la mas alta que sera diferente segun el tama�o de octavas elegido
' que ser� la octava hasta+1 ahi tenemos 12 posiciones verticales sin usar
' usamos la correspondiente al nro de octava contando como nsE nsE=1 octava 1
' hasta nsE=8 tenemos 8 octavas..
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
' calculo nR con nsE=0 inicial en default = 12-nsE+ (estoyEnOctava -1)*13 =12+39=51
' nR=51, indice de roll donde pongo la info d ela existencia de acorde en esa octava
' ocatva 4 default, es la 3 de la capacidad total. Si quiero poner en la siguietne octava
' 12 + 4*13=12+52=64
' lugares nR-> 12-24-36-48-60-72-84 en Notapiano seria
' o sea NB=0 to NA=115  step 12 salta 12 o sea cae en la 13 que es 12 ya que
' Na empieza en 0,(0,1,2,3,4,5,6,7,8,9,10,11) son 12 la proxima es 12 y hubo 
' un salto de 12 y es al 13 si cuento desde 1 en vez de 0
' la ocatava siguietne empieza en 12 +13=25 
' 0,12,25,38,51,64,77,90,103, como vemos salta en 12
' LA NOTA SELECCIONADA SERA UNA TONICA, 3ERA,5TA, 6,7,9,11 ETC
'Print #1,"indicePos original ",indicePos

Dim As HWND hwndOpc
hwndOpc=OpenWindow("",Posx,Posy-100,200,240,WS_OVERLAPPEDWINDOW Or WS_VISIBLE)
'' faltaria en un futuro ver acordes cerrados y abertos pero eso es mas un trabajo del
' usuario podran mover las notas del acorde formado , verificar el movimiento de 
' notas individuales ,, MOVER UNA TRASPOSICION SEMITONO A SEMITONO HASTA LLEGAR A SU POSICION
' UNA OCTAVA MAS ARRIBA O MAS ABAJO, LA TECNICA PODRIA SER ESA PONER UNA OPCION DE SUBIR O BAJAR
' UNA OCTAVA CADA NOTA SELECCIONADA INDIVIDUALMENTE,

Dim As Integer res=0,i1, evento

 TextGadget  (0,10,10,180,30, "LA NOTA ELEGIDA ES..",SS_CENTER)

 CheckBoxGadget( 1,10 ,  40, 90, 20, "Tonica")',, hwndOpc) 
 CheckBoxGadget( 2,10 ,  60, 90, 20, "3era")',, hwndOpc)
 CheckBoxGadget( 3,10 ,  80, 90, 20, "4ta")',, hwndOpc) 'sus
 CheckBoxGadget( 4,10 , 100, 90, 20, "5ta")',, hwndOpc)
 CheckBoxGadget( 5,10 , 120, 90, 20, "6ta")',, hwndOpc)
 CheckBoxGadget( 6,10 , 140, 90, 20, "7ma")',, hwndOpc) 
 CheckBoxGadget( 7,10 , 160, 90, 20, "9na")',, hwndOpc) 
' EN UNA MELODIA GENERALMENTE SUS NOTAS SON SIEMPRE LA NOTA MAS ALTA DE UN ACORDE
' ERGO SE SUELE USAR LA 1ER INVERSION COMUNMENTE DONDE LA FUNDAMENTAL PASA AL TOPE. 
' PERO ACA GENERAMOS MAS POSIBILIDADES QUE LA NOTA DE LA SECUENCIA SEA UNA 3ERA 57A 4TA,7MA 9NA 
Do
  evento=WaitEvent()
   If evento= eventclose  Then
      Exit Do
   EndIf  
   If evento=eventgadget Then
      For i1=1 To 7
        if   EventNumber()=i1 Then
         Print #1,"seleccion de Funcion ", i1
         res=i1
         Exit For 
        EndIf
      
      Next i1 

     If i1 > 0 Then 
        Close_Window(hwndOpc)
        Exit Do  
     EndIf
  EndIf
Loop

   
'Print #1,"seleccionado res "; res
'Print #1, "nsEelegida "; nsEelegida
'Print #1,"1 MousexNotaElegida"; MousexNotaElegida
'Print #1, "PianoNotaElegida "; PianoNotaElegida

'---------------- 
''Print #1,"1 ) maxpos antes de ventana acordes ", MaxPos 
      
     haco = OpenWindow("",Posx ,Posy,anchofig*20,60,WS_VISIBLE Or WS_THICKFRAME  , WS_EX_TOPMOST Or WS_EX_TOOLWINDOW )''Or WS_EX_TRANSPARENT  )
     UpdateInfoXserver()    
     WindowStartDraw(haco)
       fillrectdraw(6,6,&hffffff) '  ERA 2,2 CON TICKS PASAMOS A 5 MNIMO TAMA�O
       TextDraw(6,6,"[X]",&hffffff)
     StopDraw
     hpopup1 =CreatePopMenu()


     notas3   =OpenSubMenu(hpopup1," Acorde 3 Notas") 'triadas
     notas4   =OpenSubMenu(hpopup1," Acorde 4 Notas") ' septimas
     notas5   =OpenSubMenu(hpopup1," Acorde 5 Notas") ' novenas ...once 13 COMPLETAS 
     notabase =OpenSubMenu(hpopup1," Esta Nota Base es ...") ' si la nota elegida ser� Tonica 3era 5ta etc
     cancelar =OpenSubMenu(hpopup1,"<-Cancelar->")

' 3 NOTAS -------------------------------->          
     Mayor=OpenSubMenu(notas3,"Mayor, ")
     Menor=OpenSubMenu(notas3,"Menor, m")
     Dis  =OpenSubMenu(notas3,"Dis, �")
     Aum  =OpenSubMenu(notas3,"Aum, +")
     Sus2 =OpenSubMenu(notas3,"Sus2  ")
     bVII =OpenSubMenu(notas3,"bVII = Mayor bajar base 1 semitono ")
' 4 NOTAS -------------------------------->     
     Mayor7   =OpenSubMenu(notas4,"May7")
     Menor7   =OpenSubMenu(notas4,"Menor7, m7,-7")
     Menor7b5 =OpenSubMenu(notas4,"Menor7b5, m7(b5),-7(b5)")
     Dom7     =OpenSubMenu(notas4,"Dominante 7, 7")
     Dom75a   =OpenSubMenu(notas4,"Dominante +7, 7#5, 7+5")
     Dis7     =OpenSubMenu(notas4,"Dis 7,o �7")
     May6     =OpenSubMenu(notas4,"May 6,o (6)")
'      
'5 NOTAS --------------------------------->
     Mayor9=OpenSubMenu(notas5,"Mayor 9, 9")
     Menor9=OpenSubMenu(notas5,"Menor 9, -9")
     Dis9  =OpenSubMenu(notas5,"Dis 9, �9")

' Deberia hacer un algoritmico para evitar tanto desarrollo de posibilidades?, datos que tengo para el algoritmo
' la nota elegida, si es mayor menor etc, si es acorde base, o 1era inv o 2da inv,,

     MenuItem (1001,Mayor,"No inv")       'triada -> 7 CASOS TONICA, 3ERA,4TA,5TA,67A, 7MA,9NA LISTO
     Menuitem (1002,Mayor,"1era inv o 6") 'triada -> 7 CASOS TONICA, 3ERA,4TA,5TA,67A, 7MA,9NA LISTO
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
' nuevo campp  1,2,3,4,5,6,7.8.9.10,11,12
             
     Menuitem (1047,notabase,"Es 3era ")
        
     Menuitem (1048,notabase,"Es 5ta ")
        
     Menuitem (1049,notabase,"Es 7ma ")
        
' es 4ta, 6ta, 9na, 11a  �? podriamo agregar
     Menuitem (1050,notabase,"Es 4ta o 11")
        
     Menuitem (1051,notabase,"Es 6ta ")
        
     Menuitem (1052,notabase,"Es 9ma ")
        
     Menuitem (1053,notabase,"Es 11va o 4ta")
        

     MenuItem(1100,cancelar,"Salir")
     
    DisplayPopupMenu(hpopup1, GlobalMouseX,GlobalMouseY)

 ' mouse _event win32,,,
      
Print #1,"indicePos por aca  ",indicePos
 Print #1,"2 MousexNotaElegida"; MousexNotaElegida
     Do
      event=WaitEvent
      If event=eventclose Then
         Delete_Menu (hpopup1)            
         Close_Window(hpopup1)
         Close_Window(haco)

          Exit Do
      EndIf   
      If WM_VKEYTOITEM And  EventKEY = VK_ESCAPE Then
         Delete_Menu (hpopup1)            
         Close_Window(hpopup1)
         Close_Window(haco)

          Exit Do
      EndIf
      If event=EventMenu then
       Select case EventNumber
' TRIADAS   
         Case 1001
       'NO INVERSION Mayor   C-> E-> G ok  apilando hacia arriba desde C, C es la nota mas baja
'Print #1,"3 MousexNotaElegida"; MousexNotaElegida
      Select Case res '''ACORDES MAYORES SIN INVERTIR  terminado
         Case 1  ' la nota es la tonica
           armarAcorde(res ,4, 7, 0) ' [C]->E->G mayor 4, 7 ,  (3era, 5ta,0) hacia arriba
         Case 2  ' es la 3era y no invertida 
           armarAcorde(res ,-4, 3, 0) ' C <- [E] -> G mayor -4, 3 ,  
         Case 3 '  es la 4ta sus4 suspendida remplaza la 3era y no invertida
           armarAcorde(res , -5, 2, 0) '  C <- [F] <- G mayor -5, 2 ,
         Case 4 '  es la 5ta y no invertida 
           armarAcorde(res , -3, -7, 0) ' C <- E <- [G] mayor -4, 3 ,  DE QUINTA BAJO A 3ERA Y LUEGO TONICA 
         Case 5 '  es la 6ta del acorde mayor
           armarAcorde(res , -5, -9, 0) ' C<-E<-[A]  DE 6TA BAJO A 3ERA Y LUEGO TONICA
         Case 6  ' es la 7ma
           armarAcorde(res , -5, -11, 0) ' C<-E<-[B] ' DE 7MA BAJO A 3ERA Y LUEGO TONICA
         Case 7  ' ES LA   NOVENA 9NA  
           armarAcorde(res , -10, -2, 0) ' C<-E<-[D] ' DE 9NA BAJO A 3ERA Y LUEGO TONIA
      End Select
      acordeNro=1
'Print #1,"2 ) maxpos despues case 1001 de ventana acordes ", Maxpos
 
         Case 1002   '''ACORDES MAYORES 1ERA INVERSION   
      Select Case res  
         Case 1 ' la nota es la tonica  E G [C] ok  C es la mas aguda tonica en este 
                'caso y para abajo viene 1ero la 5ta y luego la 3era
           armarAcorde(res ,-8, -5, 0) '' hacia abajo podria poner 3era -8, 5ta -5, 0 
         Case 2 ' la nota es la 3era en una invertida  [E] G C ok  E es la mas grave en este caso y 
                  'para arriba vienen 1ero la 5ta y luego la tonica
           armarAcorde(res ,3, 8, 0) '' hacia abajo podria poner 3era -8, 5ta -5, 0 
         Case 3 ' la nota es la 4ta en una invertida  [F] G C ok  F la 4ta es la mas grave en este 
                'caso y para arriba vienen 1ero la 5ta y luego la tonica
           armarAcorde(res ,7, 2, 0) '' hacia arriba podria poner tonica  7, 5ta 2, 0 
         Case 4 ' la nota es la 5ta  de una mayor invertida  E<- [G]-> C ok , G es la 5ta esta en el medio  
                  'para arriba viene 1ero la tonica y para abajo la tercera se pasa 3era, tonica,0
           armarAcorde(res ,-3, 5, 0)  
         Case 5 '  es la 6ta del acorde mayor invertida E [A] C
           armarAcorde(res , -5, 3, 0) ' E<-[A]->C   3ERA -5 Y LUEGO TONICA +3
         Case 6  ' es la 7ma en un acorde mayor invertido , latonica la mas aguda el C 
           armarAcorde(res , -7, 1, 0) ' E<-[B]->C ' DE 7MA BAJO A 3ERA -7, Y LUEGO subo a TONICA +1

         Case 7  ' ES LA   NOVENA 9NA de un mayor invertido  
           armarAcorde(res , -10, 10, 0) ' E<-[D]->C ' DE 9NA BAJO A 3ERA Y LUEGO TONIA , acorde muy abierto

      End Select 
      acordeNro=2

         Case 1003 ' TRIADA SEGUNDA INVERSION listo
        ' 2DA INVERSION MAYOR  G C E ok  5ta  -5 <- C -> 4  3era
      Select Case res  
         Case 1 ' la nota es la tonica en un  acorde en 2da inversion  G [C]  E ok  C es la del medio en este 
                'caso y para abajo viene 1ero la 5ta y luego para arriba la 3era
           armarAcorde(res ,4, -5, 0) '' ESTABA -- listo
         Case 2 ' la nota es la 3era en una 2da invertida   G C [E] ok  E es la mas aguda en este caso y 
                  'para abajo vienen 1ero la 5ta y luego la tonica
           armarAcorde(res ,-9, -4, 0) '' hacia abajo podria poner 3era -8, 5ta -5, 0 listo 
         Case 3 ' la nota es la 4ta en una 2da invertida   G C [F] ok  F la 4ta es la mas aguda en este 
                'caso y para abajo vienen 1ero la 5ta y luego la tonica
           armarAcorde(res ,-5 ,-10, 0) '' hacia abajo podria poner tonica  -5, 5ta -10, 0 

         Case 4 ' la nota es la 5ta  de una mayor 2da invertida   [G]-> C-> E ok , G es la 5ta esta la mas baja  
                  'para arriba viene 1ero la tonica y luego la tercera 
           armarAcorde(res ,9, 5, 0)  '' 9 para llegar a la 3era E. 5  para llegar a la tonica C listo
         Case 5 '  es la 6ta del acorde mayor 2da invercion [A] C E
           armarAcorde(res , 3, 7, 0) ' [A]->C->E  TONICA +3, 3era 7
         Case 6  ' es la 7ma en un acorde mayor 2da invercion, latonica la mas aguda el C 
           armarAcorde(res , 1, 5, 0) ' [B]->C-E ' DE 7MA arriba tonica 1, 3era  

         Case 7  ' ES LA   NOVENA 9NA de un mayor 2da inversion, o sea tomo una 2da inversion y de ella la 9na  
           armarAcorde(res , 10,14,0) ' [D]->C->E ' DE 9NA arriba TONICA , y luego 3era acorde muy abierto

      End Select 


      acordeNro=3
' -----------------
         Case 1004 ' Menores <------------No inversion LISTO
      Select Case res '''ACORDES MENORES SIN INVERTIR listo   
         Case 1  ' la nota es la tonica
          armarAcorde(res ,3, 7, 0) ' menor 3,7 ..C, Eb, G ok
         Case 2  ' es la 3era y no invertida 
           armarAcorde(res,-3, 4,0) ' C <- [Eb] -> G menor -3, 4  ok  
         Case 3 '  es la 4ta sus4 suspendida remplaza la 3era y no invertida 4ta justa 2.5 tonos o 5 semitonos
           armarAcorde(res ,-5 , 2, 0) '  C <- [F] <- G menor -5, 2 , ? Eb-> F seria 4ta
' es igual que para mayor es unica tanto para mayor como menor
         Case 4 '  es la 5ta y no invertida de una menor
           armarAcorde(res ,-4 ,-7 , 0) ' C <- Eb <- [G] mayor -4, 3 ,  DE QUINTA BAJO A 3ERA Y LUEGO TONICA 
         Case 5 '  es la 6ta del acorde menor
           armarAcorde(res ,-6 , -9, 0) ' C<-Eb<-[A]  DE 6TA BAJO A 3ERA Y LUEGO TONICA
         Case 6  ' es la 7ma de un acorde menor
           armarAcorde(res ,-8 ,-11 , 0) ' C<-Eb<-[B] ' DE 7MA BAJO A 3ERA Y LUEGO TONICA sin la 5ta
         Case 7  ' ES LA   NOVENA 9NA de una menor  
           armarAcorde(res ,-11 ,-2 , 0) ' C<-Eb<-[D] ' DE 9NA BAJO A 3ERA Y LUEGO TONIA
      End Select


      acordeNro=4  
'-----------------------------SEGUIR DESDE ACA -------------

         Case 1005  ' MENORES 1ERA INVERSION TRIADA
      armarAcorde(res ,-9, -5, 0) ' Eb, G ,C ' menor 1era inversion ok
      acordeNro=5 
         Case 1006 ' MENORES 2DA INVERSION TRIADA
      armarAcorde(res ,3, -5, 0) ' G ,C , Eb' menor 2da inversion ok

' -----------disminuida
         Case 1007
      armarAcorde(res ,3, 6, 0) ' disminuida 3,6 C,Eb,Gb
      acordeNro=7
         Case 1008 ' 
      armarAcorde(res ,-9, -4, 0) '  Eb, Gb, C dism 1era inv
      acordeNro=8
         Case 1009
      armarAcorde(res ,3, -4, 0) '  Gb, C , Eb dism 2da inv
      acordeNro=9
' ------------aumentada
         Case 1010
       'NO INVERSION Mayor   C E G#
      armarAcorde(res ,4, 8, 0) ' mayor 4, 7
      acordeNro=10

         Case 1011
        '1ERA INVERSION MAYOR  E G# C ..-8 -4 0
      armarAcorde(res ,-8, -4, 0)
      acordeNro=11       

         Case 1012
        ' 2DA INVERSION MAYOR  G# C E  -4 0 4
      armarAcorde(res ,4, -4, 0)
      acordeNro=12
     
      
' -------------FIN TRIADAS --mas abajo estasn las sus2 son triadas tambien
' Cuaternario 
         Case 1013 ' mayor 7 no inversion
      armarAcorde(res ,4, 7, 11) ' mayor 4, 7,11 C,E,G,B
      acordeNro=13   
         Case 1014 ' mayor 7 1er inversion E,G,B,C
      armarAcorde(res , -8, -5, -1)       
      acordeNro=14
         Case 1015 ' MAYOR 7 2da inv G,B,C,E 
      armarAcorde(res , -5, -1, 4)
      acordeNro=15     
         Case 1016 'mayor 7 3era inversion B,C,E,G
      armarAcorde(res , -1, 4, 7)
      acordeNro=16
' --Menor 7 o m7--------------      
         Case 1017
      armarAcorde res ,3, 7, 10 ' menor 3,7,10  ej D:  D, F, A, C
      acordeNro=17           
         Case 1018
      armarAcorde res , -9, -5, -2 ' F, A ,C, D ' menor 1era inversion
      acordeNro=18     
         Case 1019
      armarAcorde res ,-5, -2, 3 ' A ,C , D, F  ' menor 2da inversion
      acordeNro=19           
         Case 1020
      armarAcorde res , -2, 3, 7 ' C , D, F, A ' menor 3era inversion
      acordeNro=20      
'---Menor7 b5  m7b5 o 
         Case 1021
      armarAcorde res ,3, 6, 10 ' menor 3,7,10  ej D:  D, F, Ab, C
      acordeNro=21           
         Case 1022
      armarAcorde res , -9, -6, -2 ' F, Ab ,C, D ' menor 1era inversion
      acordeNro=22     
         Case 1023
      armarAcorde res ,-6, -2, 3 ' Ab ,C , D, F  ' menor 2da inversion
      acordeNro=23           
         Case 1024
      armarAcorde res , -2, 3, 6 ' C , D, F, Ab ' menor 3era inversion
      acordeNro=24      

' ----Dom7 o 7 ---------------           
         Case 1025              
      armarAcorde  res, 4, 7, 10 ' 7   C(0),E(4),G(7),Bb(10)  0,4,7,10  sin inversion
      acordeNro=25 
         Case 1026              
      armarAcorde  res,-8,-5,-2 ' E(-8),G(-5),Bb(-2),C(0)   1era inversion
      acordeNro=26   
         Case 1027
      armarAcorde  res,-5,-2, 4           'G(-5) Bb(-2) C E(4) 2da inversion
      acordeNro=27      
         Case 1028
      armarAcorde  res,-2, 4, 7         'Bb(-2) C E(4) G(7) 3ERA INVERSION
      acordeNro=28
' ----Dom7a o 7#5 7+5 ---------------           
         Case 1029              
      armarAcorde  res, 4, 8, 10 ' 7   C(0),E(4),Ab(8),Bb(10)  0,4,7,10  sin inversion
      acordeNro=29 
         Case 1030              
      armarAcorde  res,-8,-4,-2 ' E(-8),Ab(-4),Bb(-2),C(0)   1era inversion
      acordeNro=30   
         Case 1031
      armarAcorde  res,-4,-2, 4   'Ab(-4) Bb(-2) C E(4) 2da inversion
      acordeNro=31      
         Case 1032
      armarAcorde  res,-2, 4, 8   'Bb(-2) C E(4) Ab(8) 3ERA INVERSION
      acordeNro=32


'---Dis7-------------------------
           
         Case 1033
      armarAcorde res ,3, 6, 9 ' DIS 3,6,9  ej D:  C, Eb, Gb, A
      acordeNro=33           
         Case 1034
      armarAcorde res , -9, -6, -3 ' Eb, Gb ,A, C ' DIS 1era inversion
      acordeNro=34     
         Case 1035
      armarAcorde res ,-6, -3, 3 ' Gb ,A , C, Eb  ' Dis 2da inversion
      acordeNro=35           
         Case 1036
      armarAcorde res , -3, 3, 6 ' A , C, Eb, Gb ' Dis 3era inversion
      acordeNro=36      
'---------------------Mayor 6ta ' CEGA , C-A INTERVALO DE 6TA 
'https://javi29clases.blogspot.com/2012/07/acordes-mayores-con-sexta-6-en-piano.html
'Posici�n Fundamental: Formados por T�nica, su 3ra Mayor, su 5ta justa y su 6ta
'inversi�n desde 3ra:  3ra Mayor , 5ta justa, 6ta y T�nica(1ra)
'inversi�n desde su 5ta: 5ta justa, 6ta,  T�nica y su 3ra Mayor
'inversi�n desde su 6ta: 6ta,  T�nica,   su 3ra Mayor y su 5ta justa        
         Case 1037 ' 
      armarAcorde res ,4,7,9   ' C,E,G,A ....C6
      acordeNro=37   
         Case 1038 ' 1ERA INVERSION  
      armarAcorde res ,-3,-5,-8   ' E,G,A,C ...C6/E E-C INTERVALO DE 6TA
      acordeNro=38   

         Case 1039 ' 2DA INV 
      armarAcorde res ,-3,-5,4   ' G,A,C,E ...C6/G G-E INT 6TA
      acordeNro=39   

         Case 1040 '3ERA INV
'https://javi29clases.blogspot.com/2012/07/acordes-mayores-con-sexta-6-en-guitarra.html          
      armarAcorde res ,-3,4,7   ' A,C,E,G ...C6/A =Am7, Am7/C=C6  
      acordeNro=40   
'--------------------------------
' -----------------------Sus2 triada
         Case 1041   'Sus2,"No inv")  ' triada
      armarAcorde(res ,2, 7, 0) ' CSus2 C D G
      acordeNro=41
         
         Case 1042  ' Sus2,"1era inv o 6")  ' triada  D G C
      armarAcorde(res ,-10, -5, 0) 'Csus2/D
      acordeNro=42

         Case 1043 'Sus2,"2da inv")  ' triada G C D
      armarAcorde(res , 2, -5, 0) 'Csus2/G
      acordeNro=43
'--------------fin sus2 triada
' Acodes No Diatonicos https://guitarmonia.es/los-acordes-bvii-y-bviimaj7/
' bVII bemol septimo grado triada 1ERA INVERSION
         Case 1044 '' B, D ,F -> Bb, D, F -> D, F, Bb  (GRADO, 5TA, 3ERA) Bb semitono mas alto
      armarAcorde(res ,-8, -5, 0)                                    '  F  5 semi tono abajo 
      acordeNro=44                                                     '  D  8 semitono abajo

         Case 1045  
      armarAcorde(res ,-5, -8, -1) ' 1era inversion  Bb y A juntos...�? o -13
      acordeNro=45
           
         Case 1046 

         Case 1047  

         Case 1048  
            
         Case 1049  
            
         Case 1050  
            
         Case 1051  
            
         Case 1052  
            
         Case 1100 ' es Salir
         Delete_Menu (hpopup1)            
         Close_Window(hpopup1)
         Close_Window(haco)

       End Select
' grabacion en Roll y track
Dim As INTEGER vacio
' estoyEnOctava cuenta las octavas desde 1 a 9, pero por eo el 1er vacio ocurre en 12
' luego son: 25,38,52,64,77,90,103..es la ultima de 103 a 115 es una octava que nos usa
' para notas pero si para este control al notener notas no hay acorde ni tampoco vacio
' que seria el 116 qu enoexiste....
vacio= 12 +(estoyEnOctava-1)*13 ' vacio lim inferior d ela octava que sobra arriba
' OCTAVA 8->103 --- OCTAVA 6 -> 77
' marcamos en el interespacio con 201 en pb para indicar que hay acorde
' la info restante la ponemos arriab de todo en la octava que no se usa..
' o sea estoy indicando que en esta octava y en esta posicion hay un acorde
' y solo puede ahber uno de modo que la relacion es 1:1
' estoyEOCtava y acordeNro y la RollNota  dan el acorde compelto
'Dim NotaAcorde As String
'NotaAcorde=NotasGuia(nsE-1) ' c,c#,d,d#..etc
' 26-01-2022 por choque con escalas cambio inst a pb
 Roll.trk(indicePos, vacio).pb=201  ' codigo de lugar en octavas de roll


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
' (===> USARE LA POSIION 115 PARA INFORMAR SI LA COLUMNA TIENE AL MENOS UNA NOTA  SINO SERA CERO)
' 1) => vacio=11 + (hasta-2)*13+1 ' 90 es para la 1er octava , defULT 3,4,5,6 APARECEN
' EN GRAFICO, PERO SON LAS OCTAVAS 4,5,6 Y7 4 ES LA 1ER OCTAVA DE LO QUE SE MUESTRA
' 5 LA 2DA Y ASI SUCECIVAMENTE, ERGO la posicion es en vez de 90
Dim As Integer verticalEnOctavaVacia '  6-4 =2
' 90,91,92,93,95 la default tomara la posicion vertical 
' 2) => verticalEnOctavaVacia= vacio + estoyEnOctava - desde   
' en un solo paso, linea de la octava libre que contendra la info dela corde:
' hasta=8 ,desde=4 ... 90 + 
verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde ' 90 + 6 - 4=92 
' para la octava cero su info de acorde se escribe en 90+0-0 = 90
' par al� octava 1    su info de   "     "  "         90+1-1 = 90
' coinciden proque las 2 son la 1er octava y la inferior
' para el caso de 4 a 8 que se analiza, ej: la octava 5 estaria
' 90 + 5 -4 =91 , y es correcto porque es la 2da octava siendo al 1era del intervalo 
' la octava 4. 
 Roll.trk(indicePos,verticalEnOctavaVacia ).nota = CUByte(RollNota)
 Roll.trk(indicePos,verticalEnOctavaVacia ).dur  = CUByte(acordeNro)
 Roll.trk(indicePos,verticalEnOctavaVacia ).vol  = CUByte(estoyEnOctava) ' para pasar a Track
 Roll.trk(indicePos,verticalEnOctavaVacia ).pb   = 202 ' codigo de exsitencia de cifrado en el cabezado
 ' por cada 201 en el INTERESPACIO hay un 202 de una octava de roll , en track solo
 ' existe el 202 en el encabezado
' con esta info reconstruyo el acorde que luego muestro solo en la octava
' la octava la se por donde esta el 201 no hace falta guardarla, peRo para pasarla a track si
' hace falta!! 27-01-2022 -. YA INCORPORADO A TRACK
' Lo maximo que uso son 8 posiciones para las octavas de 0 a 7 o 1 a 8 , como tengo 13
' posiciones verticales en la ultiam octava me quedan 5 posiciones libres y tambien
' como la estructura dat tiene 6 campos me quedan 2 campos en las 8 primeras
' y todas las 6 en las 5 siguietnes = 46 sitios donde poner informaicon para una posicion dada
' las repeticiones las colocaremos en la 1era de la posicion libre o sea en este caso 
' la posicion 98 como si fuera una octava 9 ficticia...las repeticiones no dependen de la
' octava todas las pisas se repiten al unisono. solo hace falta inforamcion
' arriba en al octava que no se usa.
 
' en uso. 
/'If nota3era <> 0 Then 
        nota3era= lugarNota(deltanota(aconro))
        
EndIf
If nota5ta <> 0 Then 
        nota5ta= lugarNota(nota5ta)
EndIf
If nota7ma <> 0 Then 
        nota7ma= lugarNota(nota7ma)
EndIf
'/
    '''' DUR=0   
         Delete_Menu (hpopup1)            
         Close_Window(hpopup1)
         Close_Window(haco)
         Exit Do 
               
      EndIf
 
     Loop  
       Print #1,"EXIT LOOP 3 ) maxpos fin acordes  ", Maxpos 
    EndIf
'=========================================================================    
' [[[[18-01-2022 menu alternativo con las 58 formas de acorde jjj
'  cambiar todo los popup menus por solamente un scroll ...futuro]]] 
/'
    If MultiKey(SC_LSHIFT) And MouseButtons And 2 Then 'yyy


    pasoZona1=0:pasoZona2=0
     Dim As Integer event,Posx,Posy 
    ScreenControl GET_WINDOW_POS, x0, y0
  '  Print #1,"x0 ,y0 en menu contextual " ,x0,y0
  '  Print #1,"mousex ,mousey en menu contextual ", mousex,mousey
  '  Print #1,"Posx ,Posy en menu contextual ", Posx ,Posy
  '  Print #1,"ANCHO ,ALTO en menu contextual ", ANCHO ,ALTO
  '  Print #1,"mxold, myold ", mxold,myold
  '  Print #1,"nmxold, nmyold ", nmxold,nmyold
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
  '  Print #1,"ACORDES: indicePos ",indicePos
    Rolldur = CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur)
    Rollnota= CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).nota)
    If Rollnota = 0 Or Rollnota=181 Or Rolldur=0 or Rolldur=181 Then ' construimos acorde donde no haya nada
       Rollnota = nsE 
       Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).nota = CUByte(nsE)
       Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Rolldur=DUR
       Vaciodur= TRUE
    EndIf   
    If DUR <> Rolldur And DUR > 0 Then
       Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur = CUByte(DUR)
       Vaciodur= TRUE ' cambio la duracion se necesita un RecalCompas
       Rolldur=DUR
       DUR=0
    EndIf
  '  Print #1,"ACORDES: Rolldur ",Rolldur
  '  Print #1,"ACORDES: nsE ",nsE
  '  Print #1,"ACORDES: nR ",nR
  '  Print #1,"ACORDES: PianoNota del piano ",PianoNota
  '  Print #1,"ACORDES: vovlemos a nR  ",PianoNota + SumarnR(PianoNota)
' nsE,nR y PianoNota se calculan en creaPenta..solo depende de mousey
' aunque de click derecho no importa el mouse y no depende del click 
'PianoNota=(12-nsE +(estoyEnOctava -1) * 13) ' es nR ya lo tengo
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

         Var eventHaco= waitEvent

          If eventHaco=eventgadget Then
          
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

          Sleep 1  
          
         Loop
         
     
    EndIf     
'/    
   s2=0 :s1= 0 ' 10-12-2021 wheel no se movia ** ES SUFICIENTE??? CUANDO NO SE MOVIA?? 
   '                      RECORDDAR TEST CASE
 ' lockip=0   ' 10-12-2021 wheel no se movia ***JMG OJO JODE INTERLINEADO VER MAS COMENTADO

' ========================================================  
' SELECCION DE ZONA PARA TRASPONER NOTAS
' SOLO SELECCIONO PASO DESDE HASTA y/o NOTA. 
' usaremos tambien (en desarrollo futuro) para borrar un intervalo ya sea de 
' la octava elegida o todas las octavas o desde una nota hasta otra ....�?
' clave DEVF (desarrollo futuro)
' esto funciona solo en modo lectura asi que lo movere ahi

  If MultiKey(SC_CONTROL) And MouseButtons= 1  Then '24-07-2025 Mousepress detecta mouse encima sin hacer ckick no sirve aca
     Dim As Integer pasox, pasoy, pasonR
     pasox=(mousex- gap1 )/anchofig  + posishow  
     pasoy=nsE
     'print #1,"pasoy nsE=",pasoy
     If trasponer=1 Then '03-02-2022
       correcciondeNotas(Roll)
     EndIf
     If pasoZona1 = 0 Then  ' selecion 1er posicion de la zona
        pasoZona1=  pasox ' pos de la 1er ventana
        pasoNota=0 
       ' print #1,"pasoZona1=",pasoZona1;" pasoNota=";pasoNota
        Exit Do
     EndIf

     If pasoZona1 > 0 And pasoZona1 <> pasox Then ' posicion 2 de la zona
        pasoZona2= pasox
        pasoNota=0
    '    print #1,"pasoZona2=",pasoZona2;" pasoNota=";pasoNota
        Exit Do
     EndIf
     If pasoZona1=pasoZona2 And pasoNota<>pasoy Then 
        pasoNota=nsE
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
 If MultiKey(SC_CONTROL) And lockip=0   Then ''''  REVISAR !!!!! 30-01-2022
    deltaip=0
    incWheel=0
    lockip=1
    'Exit Do    
 EndIf 
 ' 26-01-2022 espaciado de lineas (2) movido desde 2713 afecta a acordes

 If MultiKey(SC_CONTROL) And lockip=1 And cargacancion=NO_CARGAR_PUEDE_DIBUJAR  Then
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

' TRASPOSICION DE UNA SOLA NOTA MARCANDOLA CON NOTA= nota +12
'========================================================= 

' tratremos de ahcerlo solo arrastrando el mouse!
 If MultiKey(SC_ALT) And MouseButtons And 1 And trasponer=1 Then 'posiciona el cursor 
    ' habilito trasposicion de una sola nota, ejecuta solo con Ctrl-T previo y
    ' las flechas up/down, habilitare dragado tambien 02-07-2021
'    pasoNota=nsE
   ' Print #1,"MARCAR CON ALT Y nota +12 en UNA NOTA "
    indicePos=(mousex- gap1 )/anchofig + posishow
    mouseyOld=mousey
'---------------------------para ticks nuevo codigo-----------------
   estoyEnOctava  =1 + (PianoNota -12 + nsE)/13 
    Print #1,"idicePos,estoyEnOctava ",indicePos ,estoyEnOctava
   curpos=(mousex -gap1)/anchofig '''19-07-2025
  '' curpos=indicePos - posishow      
   Print #1, "1 curpos o Col "; curpos 
   notacur=nsE
   ''nsEelegida=nsE
   Print #1,"notacur "; notacur  
 
   resultado= BuscarNota (1,curpos, notacur)
  
   If resultado = 1 Then
      Print #1,"BuscarNota sin resultados en mover una nota curpos notacur ",curpos,notacur
   Else
      indicePos=curpos + posishow 
      Print #1, "(I)indicePos, notacur, curpos,posishow ",indicePos, notacur,curpos, posishow
      Print #1,"encontro onoff 2 " ,Roll.trk(indicePos, PianoNota+SumarnR(PianoNota)).onoff 
'------------------------------------------------------------------    
   ' Print #1,"MARCAR CON ALT Y 13 UNA NOTA ,INDICEPOS",indicePos 
'    grupo de notas seleccionadas poniendo un 13 en nota
     RollNotaOld=RollNota
     nR=PianoNota + SumarnR(PianoNota)
     Print #1,"nota off2 encontrada Roll.trk(indicePos,nR ).nota ";Roll.trk(indicePos,nR ).nota
     Roll.trk(indicePos,nR ).nota = Roll.trk(indicePos,nR ).nota + 12 ' marcamos para mover 13 a 24
     Print #1,"nota off2 SUMADA 12  Roll.trk(indicePos,nR ).nota ";Roll.trk(indicePos,nR ).nota
     If SelGrupoNota=0  Then ' primer nota clickeada 
        nROld=nR
        indicePosOld=indicePos ' sera la primer nota del grupo el X1 desde
     Else
        indicePosUltimaGrupo=indicePos 
     EndIf
     RollDurOld=CInt(Roll.trk(indicePos,nR ).dur)
     onoff=CInt(Roll.trk(indicePos,nR ).onoff)
     trasponer=2 ' no deja entrar  de nuevo
     
''' tener en cuenta que nR=(12-nsE) + (estoyEnOctava -1 ) * 13
'entonces cone lmouse podria mover la nota grafica ponindo la dur en la nueva
'posion nR y borrandola de la nr Old lo mismo con el off1 off2 y el resto!!

  ' Print #1,"MARCAR CON ALT Y 13   nR ", nR
     SelGrupoNota =1
'( note As ubyte, vel As UByte, canal As UByte, portsal As UByte,i1 As Integer)
     abrirPortoutEjec(100)
     noteon(cubyte(PianoNota),60,1,0,1)
     noteoff(60,1,0,1)
     ' duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
      duracion(Timer, relDur(RollDurOld) ) 
   ' el valor correcto lo repone la sub correcionnotas
   ' luego puedo mover 1 sola nota o todas las marcadas con 13  
   EndIf     
 EndIf 

 If (SelGrupoNota=1 ) And  nR <> nROld Or  SelGrupoNota=3 Then
      Print #1, "nROld, nR, nROld-nR "; nROld, nR,nROld-nR 
     If  SelGrupoNota=3 Then 
         '' es X1= IndicePosOld=X1
         '' es X2 = indicePos  
         trasponerGrupo ( nR-nROld , Roll,encancion, indicePosOld, indicePosUltimaGrupo )
         SelGrupoNota=0
     Else
         SelGrupoNota=2 ' asi solo ejecuta una sola vez
     EndIf
     trasponer=1
 EndIf
''  lurgo al detectar RELEASE del click izquierdo 
''   If mousey > 50 And SelGrupoNota=2 And( MouseButtons And 1) Then
''      SelGrupoNota=3
''   EndIf


 '-------------------- ERROR DE 10-12-2021
 ''!!! NUNCA PONER UN EXIT DO POR UN ELSE O AL FIANL PORQUE NO SE PROCESA NADA DE LO QUE SIGUE!!!!
 '------DEJO DE ANDAR SC_z POR QUE QUI HABIA UN EXIT DO  
 EndIf 
'_____________________________________________________________
'' >>>========FIN COMEDIT=LECTURA 
'_____________________________________________________

 '         <==== MENU BORRAR INSERTAR MODIFICAR ================>
 ' savemousex=0 : savemousey=0 LO QUE HACE ES PERMITIR QUE EL MENU APARESCA EN OTRO LADO
 ' DONDE SE CLICKEA, Y SI SON >0 DEJA QUE PERMANESCA VISUALMNETE
 ' HASTA EL PROXIMO CLICK IZQUIERDO, POR ESO SE PASAN A CERO SOLO EN EL ULTIMO CLICK
 ' IZQUIERDO (2 o 1 segun la cantidad necesaria para ejecutar el comando)
 '  MOMENTO EN EL QUE SE EJECUTA EL COMANDO Y SE VE EL CAMBIO.
 '       ==== NOTAS O DURACIONES EXISTENTES ====
 '******************************************************
 ' MENU CONTEXTUAL O MOVER CURSOR A UNA POSICION
 '******************************************************
 If  COMEDIT<>LECTURA  Then
     If  (COMEDIT=MODIFICACION_INSERCION  Or COMEDIT=SOLO_MODIFICACION) then
        If MouseButtons And 1 Then 
            notacur=nsE
            curpos=Int((mousex- gap1 )/anchofig) ' no lo toma 27-07-2025
            indicePos = curpos + posishow '27-07-2025

             If curposOld=0 Then  '''   VER JMG ==>>>>  
                curposOld = curpos
             Else
             '''   curpos=curposOld ' '2.3 de ayuda corregido 27-07-2025
             EndIf
          '  Print #1," 0) Primer click izq curpos curposOld "; curpos, curposOld  
            If RollDur >0 And (COMEDIT=MODIFICACION_INSERCION  Or COMEDIT=SOLO_MODIFICACION) Then
               DUR=RollDur
            EndIf
            OCTAVAFIJA=octavaEdicion
        EndIf
    Else   
          octavaEdicion = EstoyEnOctava 
          
    EndIf
         cierroedit=0
'---- MENU INSERCIONES NOTAS CON MENU DE MOUSE
    If  MultiKey(SC_CONTROL) And (MouseButtons And 2)  Then
   ' trae un menu contextual solo con ctrl-m  previo <==== menu borrar insertar modificar
   ' ESTADO:CALL MENU COMANDO
   ' aparece en ctrl-n tambien
     ayudaModif=TRUE
     ayudaNuevaNota=FALSE
     menuMouse = 0
     nroClick=1
     curposOld=curpos ' GUARDO LA POSICION DEL CURSOR DE LA NOTA DONDE SE DESPLEGO EL MENU  
     COMEDIT=MODIFICACION_INSERCION
     GrabarPenta=0
     naco=0:naco2=0
 '  print #1, "------------------------------------------------------------"
 '  Print  #1,"(1) MultiKey(SC_CONTROL) And (MouseButtons And 2) And COMEDIT<>LECTURA"
 '  print #1, "sc_CONTROL + MB2 + CE=TRUE <= ESTADO:CALL MENU COMANDO"
 '  Print  #1, " ayudaModif=TRUE"
 '  Print  #1," ayudaNuevaNota=FALSE"
 '  Print  #1," menuMouse = 0"
 '  Print  #1," nroClick=1 "
   
 '  print #1,"0) inicio ,posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
 

     Exit Do
    EndIf
 
    If  ayudaNuevaNota=FALSE And ayudaModif=TRUE Then 'ESTADO: seleccionar comando
     If (mouseButtons And 1 )  And nroClick = 1  Then
        ayudaModif=FALSE
        menumouse=0
        posinterna=0
        COMEDIT=MODIFICACION_INSERCION 'solo cntrl-m el viejo
    ' print #1, "------------------------------------------------------------"
    ' Print  #1,"(2) (mouseButtons And 1 ) And ayudaModif=TRUE And nroClick = 1 And COMEDIT=TRUE "
    ' Print  #1,  "And ayudaNuevaNota=FALSE "
    ' Print  #1,"ESTADO: seleccionar comando "
    ' Print  #1," ayudaModif=FALSE"
    ' Print  #1," menumouse=0"
    ' Print  #1," posinterna=0"
    ' print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

   ' Necesitamos mostrar el menu todavia  savemousex=0 : savemousey=0
       If nroClick = 1 Then 'seleccionamos verticalmente ,,,,
          nroClick=2
          If (mousey >= usamousey -120) And  (mousey <= usamousey -100) Then
            modifmouse=1 'borrar =1
            notacur=nsE
            curpos=Int((mousex- gap1 )/anchofig)
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
            notacur=nsE
            curpos=Int((mousex- gap1 )/anchofig)
            Exit Do
         EndIf

       EndIf
     EndIf
    EndIf
 ' <===========   MODIFICACIONES INSERCION
 ' por ahora insercion con mouse funciona igual que con keys
 ' pero la duracion entra por teclado no se porque no toma la de mouse.
 ' luego de la 1er insercion con solo click izquierdo repito otra insercion
 ' en linea horizontal si quiero otra duracion debere elegir otra con teclado
 ' el cursor seguira al click izq por el calculo de cursor en este comando
 ' finalmente se usa la tecla END para finalizar todas las inserciones.
 ' debere agregr un nuevo comando END para hacerlo con el mouse....
   If   ayudaModif=FALSE Then
     If (mouseButtons And 1 )  And nroClick = 2  Then
       savemousex=0 : savemousey=0 ' 
   ' ESTADO: PREPARA COMANDO
  ' Print #1, "------------------------------------------------------------"
  ' Print #1, "(3) (mouseButtons And 1 ) and ayudaModif=FALSE And nroClick = 2 And COMEDIT=TRUE "
  ' Print #1, " ESTADO: PREPARA COMANDO"
       notacur=nsE
       curpos= Int((mousex - gap1)/anchofig)
       posishow= curpos  + 1 ' NO CAUSA EL +1 EN MODIF MOUSE 03-03-21-15:10
  ' Print #1, " savemousex=0 : savemousey=0 ' JMG NUEVA"
  ' Print #1, " notacur=nsE"
  ' Print #1,"curpos= Int((mousex - 81)/20)"
  ' Print #1," posishow= curpos + 1"
  ' Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn

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
          If COMEDIT=MODIFICACION_INSERCION Then ' solo vlido con Ctrl-M
             insert=3
             moveresto ()
             insert=0:indaux=0
          EndIf
          nroClick=0 ' no permite entrar mas por insercion
          modifmouse=0
          ayudaNuevaNota=TRUE
    ' acomoda los compases  <======= organiza Compases
        '''  ReCalCompas(Roll) ' organizaCompases()
    ' fin compases
       EndIf
       If modifmouse=4 Then ' modificar
         cambiadur=1
         curpos=Int((mousex- gap1 )/anchofig)
         notacur=nsE
         ayudaNuevaNota=TRUE
         Exit Do
       EndIf
     EndIf
   EndIf
 ''
 '                     <=== INICIO  O P I L F E W H
' aca funciona mal toma la posicion del click izquierdo al elegir una duracion 
' y mueve el cursor no lo deberia mover !!!!
   If (MouseButtons And 2)  Then ''<=== menu de duraciones para seleccionar con click
   ' el resto del code en CreaPenta(), para toda edicion las duraciones 1 a 8 en letras
     ayudaNuevaNota=TRUE 'ESTADO: CALL MENU DURACIONES O CTRL-M
     ayudaModif =FALSE
     savemousex=0 : savemousey=0 ''ACA NO �?
     vuelta=FALSE
     menuMouse = 0

     nroClick=1
'   Print #1, "------------------------------------------------------------"
'   Print #1,"(MouseButtons And 2) and COMEDIT<>LECTURA"
'    Print  #1,"(4) ESTADO: CALL MENU DURACIONES O CTRL-M"
'   Print  #1," ayudaNuevaNota=TRUE "
'   Print  #1," ayudaModif =FALSE"
'   Print  #1," savemousex=0 : savemousey=0 "
'   Print  #1,"  vuelta=FALSE"
'   Print  #1,"  menuMouse = 0"
'   Print  #1,"  DUR=0"
'   Print  #1,"  nroClick=1"
 '  Print  #1,"COMEDIT<>LECTURA "
 ''  Print #1,"4 posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
' esto mueve el cursor conde se abrira el menu grafico esta bien.,..
         curpos=Int((mousex- gap1 )/anchofig) '01-07-2021
         curposold = curpos
            Print #2," 2) 2do click izq curpos curposOld "; curpos, curposOld 
         notacur=nsE
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
 '******************************************************************************
'>>>>>>>>>>>>>>>> EJECUCION COMANDOS MENU GRAFICO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
'*******************************************************************************

 If  ayudaModif=FALSE Then
   If (mouseButtons And 1 ) And  COMEDIT=MODIFICACION_INSERCION  And modifmouse <> 3 Then ' ESTADO: SELECCIONA VUELTA CTRL-P
   'Print #1, "------------------------------------------------------------"
   'Print  #1,"(5) MB1  And AModif=FALSE And CE=TRUE  And CVert = 1 "
   'Print  #1,"5->ESTADO: SELECCIONA VUELTA CTRL-P"
   'Print  #1,"  vuelta=TRUE"
   'Print #1,"posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
' LA VUELTA ES UN SOLO PASO A ENTRADA_DATOS 
     vuelta=TRUE
     If mousey <= usamousey -40  And mousey >= usamousey -60 Then
       If mousex >= usamousex -64 And mousex<= usamousex +86 Then
          COMEDIT=ENTRADA_NOTAS: agregarNota=0:  menuMouse = 0
          ayudaModif=FALSE
          savemousex=0 : savemousey=0
          nroClick=0
          modifmouse=0
          nota=0 ' jmg 03-03-21 22:28 sino le sumaba 1 a Posicion
     ' y trataba de insertarla.....como nota nueva
    ' Print  #1,"5-> ctrl-p=> cursorVert = 0: cursorHori = 0: agregarNota=0:  menuMouse = 0"
    ' Print  #1,"5-> ayudaModif=FALSE    savemousex=0 : savemousey=0  nroClick=0"
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
  ' Print #1, "------------------------------------------------------------"
  ' Print  #1,"(6) (mouseButtons And 1 ) And ayudaNuevaNota=TRUE And nroClick = 1 And COMEDIT<>LECTURA "
  ' Print   #1," 6->And ayudaModif=FALSE Then ' ESTADO : SELECCIONA DURACION O CTRL-M"
  ' Print  #1,"(6)MB1  +d ANN=TRUE + NClick = 1 + CE=TRUE + ayudaModif=FALSE"
  ' Print  #1,"(6)ESTADO : SELECCIONA DURACION O CTRL-M , nroClick ", nroClick
  ' Print  #1," 6 out-> ayudaNuevaNota=FALSE"
  ' Print  #1,"  savemousex=0 : savemousey=0"
  ' Print  #1,"  menuMouse = 0"
       Print #1,"1 posicion curpos curposold MaxPos,posn ", posicion, curpos, curposold, MaxPos,posn

   ' ACA SERA LA ENTRADA POR MOUSE, DUR SALDR� DE LA ELECCION DEL MENU DE DURACION
   ' QUE APARECE CON CLICK DERCHO UBICAMOS LA POSICION RELATIVA Y OBTENEMOS LA DURACION
   ' EN EL 1ER CLICK IZQUIERDO, EN EL 2DO CLICK IZQUIERDO ENVIAMOS NOTA Y DURACION
     If  nroClick =1  Then ' determinmos duracion clickeada o seleccionada graficamente
       If mousey >= usamousey +30  And mousey <= usamousey + 44 Then
     If mousex >= usamousex -55 And mousex<= usamousex +102 Then
      ' ESTADO:SELECCION  CTRL-M
      COMEDIT=MODIFICACION_INSERCION : agregarNota=0:  menuMouse = 0
      ''  ayudaModif=TRUE jmg elmenu no debe aparecer hasta dar ctrl-click derecho
     ' Print #1,"6 ctrl-M ->COMEDIT=MODIFICACION_INSERCION : agregarNota=0:  menuMouse = 0 "
     ' Print #1,"6-> ayudaModif=TRUE"
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
       curpos=curposOld ' REPONEMOS EL CURPOS AL DE LA NOTA A MODIFICAR 
     EndIf
   EndIf
 EndIf

   If ayudaNuevaNota=FALSE And ayudaModif=FALSE Then
      If DUR > 0 And nsE > 0 And nroClick = 1 And modifmouse<> 3 Then ' ESTADO INGRESA O MODIFICA 1ER NOTA
         nota=nsE   ''<== 1er nota ingresada para la duracion y nota elegida
         nroClick=0
 ' print #1, "------------------------------------------------------------"
 ' Print  #1," DUR > 0 And nsE > 0 And nroClick = 1 And ayudaNuevaNota=FALSE and COMEDIT<>LECTURA "
 ' Print  #1," And ayudaModif=FALSE "
 ' Print  #1," (7) ESTADO INGRESA O MODIFICA 1ER NOTA"
 ' Print  #1," 7-><== 1er nota ingresada para la duracion y nota elegida"
 ' Print  #1," 7->nota=nsE   ", nsE
 ' Print  #1," 7-> nroClick=0"
         Print #1,"2 posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
      EndIf
   EndIf
   If ayudaModif=FALSE And ayudaNuevaNota=FALSE And octavaEdicion = estoyEnOctava Then
     '11-12-2021 uso Ctrl + click para ingresar notas con el mouse sino se ingresa por error
     ' facilmente durante el pasaje de edit a ctrl-m
     'LUGAR DONDE SE PODRIA REPETIR EL CAMBIODE NOTA CON UNA DURACION PREVIA
     If (mouseButtons And 1) And (DUR > 0) And (nsE > 0) And modifmouse<> 3 And MultiKey(SC_CONTROL) Then
  ' Print #1, "------------------------------------------------------------"
  ' Print  #1,"(8) (mouseButtons And 1) And (DUR > 0) And (nsE > 0) And ayudaNuevaNota=FALSE "
  ' Print  #1,"(8)  And COMEDIT<>LECTURA And ayudaModif=FALSE"
       Print #1,"3 posicion curpos MaxPos,posn ", posicion, curpos, MaxPos,posn
       Dim As Integer posdur= (mousex- gap1 )/anchofig + posishow '01-07-2021
       curpos =(mousex- gap1 )/anchofig ' 27-07-2025
       If posdur >= Maxpos - 1 Then  ' no permite entrar notas con click , antes de maxpos
        nota=nsE ' <=== ingresamos varias notas por mouse del mismo valor
       EndIf
   ' hasta que si vuelva a dar click derecho y aparesca de nuevo el menu de duraciones.
       Print  #1," nota=nsE ", nsE
       Exit Do
     EndIf
   EndIf

   If MultiKey (SC_ENTER) And copiar=0 Then
      copiar=1
   EndIf 

   If MultiKey (SC_ENTER) And copiar=2 Then
      copiar=3
   EndIf 

 EndIf ' FIN <========COMEDIT<>LECTURA
 ' menu contextual
 
' You'd have to get the thread id of the graphics window (GetWindowThreadProcessId), 
' set a WH_GETMESSAGE hook with that thread id and then process the command messages 
' in your hook function. 


  
 
' ====>>> HABILITAR UNA OCTAVA PARA EDICION CON EL MOUSE, ctrl-n no necesita habilitar 
' es para ingresar notas manualmente y ctrl-m , y tal  vez columna futuro
' NO SERA IGUAL EL COLOR DE EDIT PARA INGRESO MANUAL AL FINAL QUE MODIFICACION
' CON CTRL-M Y CON CTRL-N
If GrabarPenta=0 Then
''' If  mousex > ANCHO3div4 And COMEDIT<>LECTURA  Then ' 09-06-2021 para que nochoque con boton EDIT
' ************************************************************
' Ubicar el cursor y elegir una octava  ambas cosas 22-05-2025
' ************************************************************
 If  MouseButtons  And 1 And COMEDIT<>LECTURA  Then ' 09-06-2021 para que nochoque con boton EDIT
       octavaEdicion=estoyEnOctava
 EndIf
EndIf


' A PARTIR DE ACA FUNCIONA PARA AMBAS CONDICIONES? PERO LO ESTOY USANDO EN FALSE
' REVISAR...24-01-2022


' If MultiKey(SC_ALT) And MouseButtons And 1 Then 'posiciona el cursor
'    ' habilito trasposicion de una sola nota, ejecuta solo con Ctrl-T previo y
'    ' las flechas up/down, habilitare dragado tambien 02-07-2021
''    pasoNota=nsE
    
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
   ' print #1,"entra a copiar "
    nota=0
    indicePos=(mousex- gap1 )/anchofig + posishow
    
  '  Print #1,"VA A COPIAR DESDE POSICION INDICEPOS ",indicePos
  '  Print #1,"VA A COPIAR DESDE POSICION gap1 ",gap1
  '  Print #1,"VA A COPIAR DESDE POSICION anchofig ",anchofig
  '  Print #1,"VA A COPIAR DESDE POSICION POSishow ",posishow
  '  Print #1,"VA A COPIAR DESDE POSICION mousex ",mousex
    copiarZona=1 ' solo mueve 1 vez hasta el proximo pulsado de Q evita borrado
    If numero=0 Then
  '  print #1,"entra a copiar numero 0"
       moverZonaRoll(indicePos,Roll,pasozona1)
    Else
  '   print #1,"entra por Else numero > 0"
       Dim As short lz=0,delta
       delta = pasoZona2 - pasoZona1 + 1
' si la secuencia es chica debo agrandarla jmg   
'--- AUMENTO DE CAPACIDAD DEL VECTOR EN POSICIONES NECESARIAS
 '  print #1,"DELTA ",delta
    Dim As Integer nuevaspos
    nuevaspos= indicePos + delta * numero
 '   print #1,"NuevaPos,,", nuevaspos
      
    If CantTicks  < nuevaspos  Then
     '  GrabarArchivo(1) ''hacer un backup !!! 
      CantTicks= nuevaspos + 18000 ' incremento el tama�o en 1000 posiciones =1 min
'      print #1,"incremento final de CantTick ", CantTicks 
      ReDim Preserve (Roll.trk ) (1 To CantTicks,NB To NA)
      ReDim Preserve compas(1 To CantTicks)
      ReDim Preserve (RollAux.trk) (1 To CantTicks, NB To NA)
      ReDim Preserve (Track(ntk).trk)(1 To CantTicks,1 To lim3)
'      print #1,"Redim exitoso"
    EndIf
 'print #1,"va a copiar FOR lz,numero de veces ",numero
'---
    
       For lz = 1 To numero
          moverZonaRoll(indicePos,Roll,pasozona1)
          indicePos=indicePos + delta
          
       Next lz 
    EndIf
    Exit Do
 EndIf
 

 '  ========================================================================= 
 ' <========= ABRIR NOTAS O FRACCIONAR EN DURACIONES MAS PEQUE�AS ============>
 '  =========================================================================
 If MultiKey(SC_Z)  And MouseButtons And 1 Then ''''MousePress = 1 Then
    indicePos=(mousex- gap1 )/anchofig + posishow 
    Rolldur=CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur)
    pasozona1=0: pasoZona2=0 
  ' ARMADUR USA MOVE EL CUAL SETEA LAS PASOZONA NECESARIAS
  ' ya funciona ok con DUR 
'    Print #1,"SC_Z indicePos ,Rolldur ",indicePos,Rolldur
    ArmarDurFrac()
' Print #1," TipoFrac ",TipoFrac
    Select Case TipoFrac
       Case "igualdur" ' dando click en la nota a cambiar y lo hara en las similares
 '         Print #1," entra por igualdur" 
          FraccionarDur  Track(),Roll,indicePos, Rolldur,nR,ntk '1er verison  
       Case "tododur" ' dando click en la nota a cambiar y lo hara en todas agregando silencios
  '      Print #1," entra por tododur"
          FracTodoDur  Track(),Roll,indicePos, Rolldur,nR,ntk  ' 2da version
       Case "autodur" ' automatico toma la entrada si no la hay toma la menor y mayor fracciona todas las notas
 '   Print #1," entra por autodur"
 '   Print #1,"indicePos, Rolldur, nR, ntk ",indicePos, Rolldur, nR, ntk 
       nR=0  ' solo se usa para separar los casos ahora, antes no servia hubo que eliminarlo  
          AutoFracTodoDur Track(),Roll,indicePos, Rolldur,nR,ntk  ' 3er version
    End Select    
''    MousePress=0   
 EndIf
 If MultiKey(SC_LSHIFT)  Then ' :
      cuart=1 
      Exit Do
 EndIf
 'If MultiKey(SC_RSHIFT)  Then ' : da lo mismo que LSHIFT 
 '    doblepun = 1  ' doble puntillo
 '    Exit Do
 'EndIf
 If MultiKey(SC_COMMA)  Then   
     doblepun = 1  ' doble puntillo
     Exit Do
 EndIf
 
 EndIf    '  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
'  ' <=== fin if mouseY > 50, delimitacion de area o superficie
 
' ------------------------------------------------------------------
If MouseButtons And 1  And cargacancion=NO_CARGAR_PUEDE_DIBUJAR Then
   old_btn_press_time = new_btn_press_time
   new_btn_press_time = Timer
   If ((new_btn_press_time - old_btn_press_time) < dbl_click_time) Then
      dobleclick=TRUE
   Else
      dobleclick=FALSE
   EndIf
EndIf


'                     <===  FIN    O P I L F E W H

' ------------IPC sensado de comando fifo..




Exit Do ' este exit do hace que ande el menu!!!
' o sea nunca loopea solo una vez!!


 
Loop
nnn=nnn+1
If nnn=20 And MAXPOS < 800 Then ' que loopee mas en el lop mas interno solo salga menos al loop externo
   nnn=0
  Exit Do
EndIf
If nnn=100 And MAXPOS > 800 Then ' que loopee mas en el lop mas interno solo salga menos al loop externo
   nnn=0
  Exit Do
EndIf

If fueradefoco=SI  And (play = NO) and (playb=NO) And (Cplay=NO) Then

   Sleep 5 ' ESTO HACE QUE LA CINTA CORRA SUAVE
EndIf
While InKey <> "": Wend
'podria reemplazarse por REset(0) ???
Reset (0)

Loop
While InKey <> "": Wend
'podria reemplazarse por REset(0) ???
Reset (0)


Loop

Exit Sub 

fail:
 Dim errmsg As String
 Dim As Long er1 = Err()
If  er1 > 0 Then
print #1,"-----------------err ROLLLOOP-----------------"
  errmsg = "ROOLLLOOP FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(er1)
  Print #1, errmsg
  FileFlush (-1)
  Close
  End 0
End If




End sub
'-------------------------------------------------------------------
Sub  RefacturarPista() 'automatico
FileFlush (-1)
Print #1,"13-sleep ";Timer
Sleep 100
'Print #1, "====> RefacturarPista() MaxPos " ; MaxPos
FileFlush (-1)
Dim  As Integer  j1, i1 
  'indicePos=(mousex- gap1 )/anchofig + posishow 
For  j1 = 1 To  MaxPos  -2
FileFlush (-1)
   For  i1=NB To NA 
   ' Rolldur=CInt(Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur)
    Rolldur=CInt(Roll.trk(j1,i1).dur)
       If  Rolldur = 0 Then
           Continue  For
       Else
'Print #1," ===> Rolldur "; Rolldur
 
       EndIf
    pasozona1=0: pasoZona2=0 
  ' ARMADUR USA MOVE EL CUAL SETEA LAS PASOZONA NECESARIAS
  ' ya funciona ok con DUR 
'    Print #1,"SC_Z indicePos ,Rolldur ",indicePos,Rolldur
     
    ''''ArmarDurFrac()
''NumeroFrac=Rolldur
numeroFrac=0
'Print #1," NumeroFrac devuelto j1,i1  ", numeroFrac , j1,i1
'     "autodur" ' automatico toma la entrada si no la hay toma la menor y mayor fracciona todas las notas
'    Print #1," entra por autodur"
    Dim n1 As Integer 
    n1=1
'    Print #1,"j1, Rolldur, n1, ntk ", j1 , Rolldur, n1, ntk 
''nR no se usa para nada eliminarlo .....de la siguiente rutina...
' adaptar la siguiente rutina esta echa para una columna y
' define un pasozona o algo asi debo hacer que dependa de
' un for y vaya tomando columna por columna buscando 
' cada acorde desigual
FileFlush (-1)
     AutoFracTodoDur Track(),Roll,j1, Rolldur,n1,ntk  ' 3er version
     Rolldur=0 
  Next i1 
Next  j1   
   
End Sub 

'
' error
errorloopbas:
 
'Dim As Integer er1, ErrorNumber1, ErrorLine1
ErrorNumber1 = Err()
ErrorLine1 = Erl

If ErrorNumber1 > 1 And ContadorError < 101 Then

Print #1,"------------------------------------"
  ContadorError=ContadorError+1
  Print #1,"ErrorLoop ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
If ProgError(ErrorNumber1) ="" Then
  Print #1,"progerror En MAIN,  on line ";ErrorLine1
Else
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
EndIf
  Print #1,"Error Function: "; *Erfn()
  Print #1,"kply ",kply
/'
  Print #1, "n ",posicion;" posishow "; posishow; " NroCol ";NroCol
  Print #1, "semitono "; nsE; " *po "; *po 
  Print #1, "valor1 ",posicion; " valor2 "; 12- nsE  + (*po -1)* 13
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
  Print #1, "lbound 2 de Roll.trk ", lBound(Roll.trk, 2)   
  Print #1, "Roll.trk (n,11- nsE  + (*po-1) * 13 ).nota", Asc(Str(Roll.trk (posicion,12- nsE  + (*po -1)* 13 ).nota))
  Print #1,"Roll.trk (n,12- nsE  + (*po -1)* 13 ).dur ",  Asc(Str(Roll.trk (posicion,12- nsE  + (*po -1) * 13 ).dur))
  Print #1,"gap1 + ic * anchofig ",gap1 + ic * anchofig
  Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po
  Print #1, "nsE ";nsE; " nR ";nR; " PianoNota ";PianoNota
  Print #1, "mensaje, Ermn ", *Ermn, Ermn
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2) 
  print #1,"------------------------------------"
'/

 Print #1,"RollLoop MODULO error number: " ; ErrorNumber1 ; " at line: " + Str( Erl )
'
EndIf

