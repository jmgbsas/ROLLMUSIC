On Error Goto errorhandler
' include DE ROLLMUSIC.BAS, EVENTC, eventgadget
' tiene su scope
    '   SetForegroundWindow(hwndC)
      ' el codigo anterior que traia de disco esta en notas
' TODOS DICEN RUSO Y USA QUE VK_LBUTTON ES 1 PERO CON 1 NO ANDA
' SIN EMBARGO CON 3 ANDA A VECES..
Dim As Integer k=0
Static As Integer millave

       If eventnumber()=  PISTASROLL And GrabarPenta=0 And CANCIONCARGADA=TRUE Then 
         
         borrapos=0
' ME GUSTO FUNCIONA ASI: DAR CLICK EN UNA PISTA LUEGO CON flecha arriba
' y abajo CAMBIA DE PISTA EN ROLL, para habilitar el  CLICK DERECHO CONTEXTUAL
' DAR ENTER Y LUEGO CLICK DERECHO APARECE EL MENU CONTEXTIUAL, PARA VOLVER
' AL INICIO DAR CLICK EN OTRA PISTA Y TODO COMIENZA DE NUEVO... 
'  
             
          If MOUSEBUTTONS AND LEFTBUTTON  Then 
                
             ''' ROLLCARGADO=FALSE
             '' porque CANCIONCARGADA=TRUE
             Dim item As String
             Dim As Integer ubi1,ubi2
              
             item=GetListBoxText(3,GetItemListBox(3))
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf

             item=Trim(item)
             Print "item ",item
             If item > "" Then
             '  Dim nombre1 As String
             '   nombre1= NombreCancion + "\"+item +".rtk"
              ubirtk=3 ' ahora indice carga desde lista o memoria
             ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
             ' pero si quiero volver a disco solo debo resetear ubirtk=0
              ntk=sacarNtk(item) ' este ntk no sirve para boorar
 ' aca no copia track a Roll
              nombre= titulos(ntk)

                
              EndIf
              
          EndIf
  
'--------------------------------------------------------------

   clickpista=1 ' no incrementa el ntk que simula SC_TAB, el cual carga el track a Roll
 
'--------------------------------------------------------------

' /// // // / / /  menu contextual popup 
   
            
          If eventnumber()=  PISTASROLL And  WM_VKEYTOITEM And  EventKEY = VK_RETURN Then
           '  MOUSEBUTTONS  And RIGHTBUTTON Then 
              Dim As HMENU hMessages2
              Dim As Long eventM
              hMessages2=CreatePopMenu()
             
              MenuItem(4001,hMessages2,"1 Menu")
              MenuItem(4002,hMessages2,"2 Menu")
              Do
                   millave=millave +1  
                   If millave > 1000 Then
                      millave=0
                      Exit Do
                   EndIf
                 eventM= waitevent()
                    
                 If eventM=EventMenu then
                    Select case EventNumber
                       Case 4001
                          Exit Do
                       Case 4002
                         Exit Do
                    End Select
                 Else
                   If eventM=eventrbdown Then
                     If instancia =1  Then   
                        DisableGadget(PISTASROLL,1)
                     EndIf

                     DisplayPopupMenu(hMessages2,,)
                     If instancia =1  Then   
                        DisableGadget(PISTASROLL,0)
                     EndIf
                     Exit Do
                   EndIf
                 EndIf
             
              Loop 
          EndIf
'--------------------------------------------------------------
' este ntk sirve para identificar el ntk del archivo t del vector
' pero el ntk de la lista es otro vector y al borrar el indice cambia
' debo obtener el indice primero                
'' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT
            If WM_VKEYTOITEM Then '
                If EventKEY = VK_DELETE Then 
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
                    copiarATemp (titulos(ntk),pistas(ntk))
                    BorrarPista (titulos(ntk))
                    titulos(ntk)=""
                    pistas(ntk)=""
                    pmTk(ntk).desde=0
                    pmTk(ntk).hasta=0
                    pmTk(ntk).NB=0
                    pmTk(ntk).NA=0                  
                    pmTk(ntk).MaxPos=0
                    pmTk(ntk).posn=0
                    pmTk(ntk).notaold=0                  
                    pmTk(ntk).Ticks=0
                    pmTk(ntk).portout=portout
                    Sleep 1
                    'SetItemListBox(3,ind+1) no funca
                    'SetGadgetState(3,1) no funca 
                    borrar=0
                  EndIf
                EndIf 
                 
           ' aca no debe leer a disco solo conmutar de track en track
'------------------------------
                

            EndIf
                                  
       EndIf

       If eventnumber()=  PISTASROLL And GrabarPenta=1 Then
          PISTASROLLSELECCIONADA=1  
       EndIf

       If eventnumber()=  PISTASEJECUCIONES Then
             PISTASEJECSELECCIONADA=1
       '     If WM_VKEYTOITEM Then '
       '         If EventKEY = VK_DELETE Then 
       '           If tocatope > 0  Then
       '              DeleteListBoxItem(PISTASEJECUCIONES,GetItemListBox(PISTASEJECUCIONES))
       '             If ultimo_chequeado > 0 Then 
       '                copiarATemp (DirEjecSinBarra+"\"+ tocaparam(ultimo_chequeado).nombre,tocaparam(ultimo_chequeado).nombre)
       '             ' podria hacer usado titulos(ultimo_chequeado +32)

       '             BorrarPista (DirEjecSinBarra+"\"+tocaparam(ultimo_chequeado).nombre)
       '             comprimirListaEjecs()
       '             ntoca = ntoca - 1
       '             ntkp=ntoca
       '             tocatope=ntoca 
       '             EndIf 
       '             Sleep 10
       '           EndIf
    '            EndIf 
                 
           ' aca no debe leer a disco solo conmutar de track en track
'------------------------------
                

     '       EndIf
       'Else
       '     PISTASEJECSELECCIONADA=0
       EndIf


'  CUAL PISTA DE ROLL SE ESCUCHA SEGUN LO SELECCIONADO
' este es otro metodo para seleccionar todas las pistas cargadas
' y hacer que suenen o deseleccionarlas yque no suene ninguna
' aca no se usa un archivo de estado como en ejecuciones de pista
' no lo necesita... es todo o nada pero no recuerda que se escuchaba o no
' siemrpe que haya una cancion cargada .. es un boton de todo o nada
' solo si cancion esta cargada...BOTON "S"
       If eventnumber()=BOTON_PISTA_ROLL And CANCIONCARGADA=TRUE Then
        Dim i As integer
         If cntsuena =0 Then
            SuenaTodo=0
         EndIf

         If cntsuena =Tope And suenaTodo >= 1 Then
            SuenaTodo=1
            cntsuena=0
         EndIf
 
         Select Case  SuenaTodo
            Case 0
             SuenaTodo=1
           Case Is >= 1
             SuenaTodo=0
         End Select
         For i=1 To tope 
            CheckBox_SetCheck(cbxnum(i), SuenaTodo)
            cntsuena+=SuenaTodo
          Next i
         SuenaTodo=3 'ajustar algo que no sea cero ni uno
       EndIf
'------------------
' revisar CheckBox_GetCheck de las ejecuciones
'-------------------------------------------------------
'//////////////// BOTON ROJO COMIENZO GRABACION EJEC ////////////////// O PATRON
'---------------------------------------------------------------------
' YA funciona estamos comparando con 
'F:\IT64\AREAWORK\ROLLMUSIC-138-INPUT-OK\ que si funciona graba ejecuciones
' pero es distinto no tien menu dispositicos o ejecuciones
' llamar a un list  port y ajustar portout
' NO GRABA BIEN UNA SEGUNDA PISTA MIENTRAS ESCUCHO LA ANTERIOR
' CAD AUNO CON SU PATCH CORRESPONDIENTE ...NUEVO DESAFIO!!! 
'GRABA BIEN CADA INST CON SUPATCH SEESCUCHA VIEN AL REPRODUCIR
' PERO AL GRABAR UNA NUEVA PISTA USA EL PATCH DE LA PRIMERA PISTA 
     If eventnumber()= BTN_MIDI_GRABAR And GrabarEjec=HabilitaGrabar Then ' BOTON GRABAR ROJO
      ' EVENTO 10
         k=0
         jgrb=0:repro=0 'impide el play  o lo resetea

        '  If PISTASEJECSELECCIONADA=0 Then
        '      Exit Select 
        '  EndIf 

        ' If PISTASEJECSELECCIONADA=1 Then
        '    PISTASEJECSELECCIONADA=0
          For K=1 To 32
           If CheckBox_GetCheck( cbxgrab(k))= 1 Then 
              ntoca=k
              canalDeGrabacion = ntoca
              Exit For
           EndIf
          Next k     
            If k=0 Then 
              Exit Select
            EndIf
        ' EndIf  
             'ntoca se usa en my callback es la  pista ejec que se esta grabando
            ''calltoca=ntoca 'calltoca se usa en playTocall use otra variable , ver si conviene
       ' mil negras a I=60 son 192 * mil ticks (16 minutos a I=60)
       ' a i=240 todo *4---192*4*1000=768000(16min a I=240)
       'la idea es que el usuario grabe a I=60 o I=120 384000
       ' pero cada nota requiere 2 eventos on y off se multiplicaria por 2
        If ntoca > 0 then
         ChangeProgram ( tocaparam(ntoca).patch, tocaparam(ntoca).canal,tocaparam(ntoca).portout )
        EndIf

        SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
        GrabarEjec=GrabarPistaEjecucion
        arrancaPlay=0
        If  metronomo_si=3 Then
           terminar_metronomo=0
           Dim As Integer im=0
           For im=1 To 4
             If sonidopista_si=3 Then  
               noteon(60,100,tocaparam(k).canal,tocaparam(k).portout,1) '' NOTA VEL ,CANAL, PORTSAL
               noteoff(60, tocaparam(k).canal,tocaparam(k).portout,1)
             Else
               PlaySound(".\recur\INICIO.wav", 0, SND_FILENAME+SND_NODEFAULT + SND_ASYNC )
             EndIf
               duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
           Next im
           threadmetronomo = ThreadCall metronomo()
        EndIf 
        If tocatope > 1 Then ' toco pistas ejec mientras grabo
'la que se esta grabando debe estar deseleccionada en S 
           repro=1 
           CONTROL2=0 'HABILITA TOCAR EJECS
           CONTROL1=0 ' HABILITA TOCAS CANCION
           Dim p As Integer Ptr
           p=@tocatope
           threadG  = ThreadCreate (@PlayTocaAll, p)
        

        EndIf
' O SEA ASI COMOESTA SOLO NOS PREPARA PARA PARA TOCAR Y LUEGO SE CALLA
' DEBERIA SER OPCIONAL QUE SE CALLE O NO DESPUES DE LOS 4 PRIMEROS
' 

     EndIf ' end event 10 BTN_MIDI_GRABAR ROJO LISTA DERECHA

'//////////////// BOTON NEGRO STOP EJEC  , GRABA A DISCO //////////////////

' 
      If eventnumber()= BTN_MIDI_PARAR    Then ' BOTON STOP NEGRO DE MIDI-IN
         SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
         If pmTk(ntoca+32).MaxPos > 0 And (GrabarEjec=GrabarPistaEjecucion  Or GrabarEjec=GrabarPatronaDisco ) Then
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
            GrabarEjec=HabilitaGrabar
            repro=0
            arrancaPlay=0
' terminar cualquier metrono que este funcionando 
         terminar_metronomo=1
'detiene el play de cancion o roll
         If  play=1 Or playb=1 Then
           CONTROL1=1 ' DETIENE EL PLAY DE CANCION O ROLL
            play=0: playb=0 
           playloop=0:playloop2=0
           SetGadgetstate(BTN_ROLL_EJECUTAR,0)
           Sleep 2
         EndIf
         CONTROL2=1
         Sleep 2
' -------cargamos toca
         k=0 
         Dim As Integer i1=1, j =0, partes, pj
   ' tocaparam(ntoca).delta toma valor desde la 2dapista grabada  en PlayTocaAll
' este delta no se usa para nada creo 03-12-2024
         If  tocaparam(ntoca).delta > 0 And ntoca >1 Then
             partes=tocaparam(ntoca).delta/TickChico
             k=partes
             pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos+partes
'             Toca(ntoca).maxpos=pmTk(ntoca+32).MaxPos
             For pj=1 To partes 
               Toca(ntoca).trk(pj).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(pj).nota = 0
               Toca(ntoca).trk(pj).vel  = 0
              Next pj

         EndIf
          k=partes+1
         Do 
           if k=pmTk(ntoca+32).MaxPos+1  Then
              Exit Do
           EndIf  

            Select Case  CargaIn( i1).modo
               Case 144,128
            Toca(ntoca).trk(k).modo = CargaIn( i1).modo
            Toca(ntoca).trk(k).nota = CargaIn( i1).nota
            Toca(ntoca).trk(k).vel  = CargaIn( i1).vel
                 i1 = i1 +1    
              If i1=jgrb+1 Then
                 Exit Do 
              EndIf           
           End Select
           If CargaIn( i1).partes > 0 Then
              For j=1 To CargaIn( i1).partes 
                k=k+1
                
               Toca(ntoca).trk(k).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' un retardo de tick
               Toca(ntoca).trk(k).vel  = 0
            
              Next j
           Else
             k=k+1
               Toca(ntoca).trk(k).modo = 0 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' no hay retardo de tick
               Toca(ntoca).trk(k).vel  = 0
                 
           EndIf
          k=k+1
         Loop
             jgrb=0
'----------------------grabar archivo de pista 
         Dim As String nombreg,myfil
  ' una cosa es grabar una pista y otra todas las pistas
  ' aca estamos grabando  una  sola pista,la marca da con G alpulsar el Boton Rojo
' esta funcion deberia estar en STOP 
''        DirEjecSinBarra=Toca(ntoca).nombre
   'y el path dondeesta? cuadnograbopareceque no 
  ' al cargar si debo afinar eso....   
         ReDim (toc.trk)(1 To tocaparam(ntoca).maxpos)
         
 
    
         For j As Integer =1 To   tocaparam(ntoca).maxpos
              toc.trk(j).modo=Toca(ntoca).trk(j).modo
              toc.trk(j).nota=Toca(ntoca).trk(j).nota
              toc.trk(j).vel=Toca(ntoca).trk(j).vel
         Next j
         Dim tocap As ejecparam = tocaparam(ntoca)

         
             maxgrb=tocap.maxpos
         If maxcarga < tocap.maxpos Then
         Else
            maxcarga=maxgrb 
         EndIf
         SetGadgetText(21, Str(maxcarga)) 
' para una sola pista grabada el maxgrb es el maxpos de esa pista
         If  maxgrb > 0 And ntoca > 1 Then
           If  maxgrb <  tocap.maxpos Then ' para cada pista se define tocap de nuevo  
              maxgrb=tocap.maxpos
'------------adapto la grabacion anterior a mayo rlongitud              
               ReDim  Preserve (Toca(ntoca-1).trk) (1 To maxgrb)
''''           pmTk(ntoca-1 +32).MaxPos=maxgrb ' conservo la long original
' el asunto es que igualo las  longitudes para que no reviente el play, pero
' mantengo las longitudes para menor espacio en disco al grabar
' al cargar reconstruyo en memoria igualo todo de nuevo         
          EndIf
         EndIf 
' si es grabacion nueva tocatope va incrementando apuntando a 1,2,3,4 etc
' y graba 1 por vez,el ultimo corriente...o actual
' EN ROLLDEC 
'Type paramGrabamidi
'  As vivo toc
'  As Integer  tocatope
'  As ejecparam tocap'
'End Type

'EN ROLLDEC ...Dim  Shared  As paramGrabamidi pgmidi
        pgmidi.toc=toc 'pgmidi es global
'pgmidi.tocatope = tocatope
        pgmidi.tocap = tocap ' dentro esta orden  ubyte
'threadGrabamidi=@pgmidi
        GrabarMidiIn(pgmidi,ntoca) 'POR STOP aca se graba bien el orden
  'ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

         Else
           CONTROL2=1
            If play=1 Or playb=1 Then
               CONTROL1 =1
            EndIf   
            repro=0
            arrancaPlay=0
         EndIf
'----------------
      EndIf

'//////////////// BOTON VERDE PLAY EJEC //////////////////
' si hay una cancion de pistas trk el el grafico cargada, al dar play debera tocar
' lacancion y las pistas chequeadas en columna 'S' de las ejecuciones,deese modo
' sincronizaremos el arranque solamente. Esto se puede usar para escuchar o 
' al grabar una pista nueva de ejecuciones por uncontrolador midi.,(teclado midi por ej)
 
      If eventnumber()= BTN_MIDI_EJECUTAR And repro=0 Or  GrabarEjec =PatronDeEjecucionCompleto Then ' BOTON PLAY VERDE DE MIDI-IN
         SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
            repro=1
            CONTROL2=0
            CONTROL1=0
            Dim p As Integer Ptr
            p=@ntoca 'ntoca se ajusta en CargarPstasEjec tambien
            
' tocar cancion de trakc si esta cargada 
' son 2 threadas que se inician casi simultanemente pero sin control entre ellos
' por ahora
' ACA DEBERIA USAR MUTEX!!! ���???
'''Dim As Any Ptr sync =MutexCreate
        If  MaxPos > 2 Then  ''''' And GrabarEjec=1 And repro=1 Then 
            If CANCIONCARGADA = TRUE And playb=0 Then
               playb=1   
               thread1 = ThreadCall  playCancion(Track())
            Else
               If  MaxPos > 2 And  Play=0 Then
                   Play=1
                   thread2 = ThreadCall  playAll(Roll)
               EndIf 
            EndIf
        EndIf   

        threadG  = ThreadCreate (@PlayTocaAll, p)
        'ThreadWait (threadG) '22-04-2024  como andaba si hacia detach? ja
'        repro=0   
        'threadDetach(threadG)

         '   PlayTocaAll(p)
        grabariniciotxt(NombreCancion)
  
      EndIf
' test de retardos  de inicio en ejecucion de datos entre playCancion y PlayTocaAll
' CALCULO DE RETARDO DEL INICIO DE PLAY CANCION RESPECTO PLAYTOCAALL
'playTocaAll inicio datos:    9751.75934545541
'playcancion inicio datos:   9751.76207997309
' 9751,76208744816
' 9751,75934545541
'--------------------------
'       0,00274199275  seg= 2,7 mseg o sea la mitad del Tick (5 mseg)
'podemos decir que el inicio esta casi sincronizado solo un delta de medio Tick
'---- OJO ACA ESTAMOS GRABANDO PARANDO Y EJECUTANDO GRABACIONES
' DEL USUARIO PERO SOBRE ROLL SIN NECESIDAD DE ENTRADA MANULA!!!
'----------------------------------------------------------------------------------------------
'//////////////// BOTON ROJO GRABAR EN ROLL //////////////////

      If eventnumber()= BTN_ROLL_GRABAR_MIDI And GrabarPenta=0  Then 
'solo usado por ahora sin cancion cargada , si hay una cancion
' cargada solo hace falta GrabarPenta Roll grafico ya esta cargado
         SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
         SetGadgetstate(BTN_ROLL_PARAR , BTN_LIBERADO)
         jgrbRoll=0
         GrabarPenta=1
        IF abrirRollCargaMidi=0 Then 'EVITA CARGA ROLL DESDE ACA 2 VECES
' TAMBIEN USADO EN CARGA MIDI
          threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
          SetForegroundWindow(hwnd)
       ''RollLoop ( param)
           abrirRoll=0 'EVITA CARGA ROLL DE MENU PRONCIPAL
        EndIf  
' SI EL USUARIO ABRE ROLL MANUALMENTE ..Y LEDA UN CLICK A EDIT
           COMEDIT=TRUE
           SetForegroundWindow(hwnd) 
        If  metronomo_si=3 Then
           terminar_metronomo=0
           Dim As Integer im=0
           For im=1 To 4
             If sonidopista_si=3 Then  
               noteon(60,100,tocaparam(k).canal,tocaparam(k).portout,1) '' NOTA VEL ,CANAL, PORTSAL
               noteoff(60, tocaparam(k).canal,tocaparam(k).portout,1)
             Else
               PlaySound(".\recur\INICIO.wav", 0, SND_FILENAME+SND_NODEFAULT + SND_ASYNC )
             EndIf
               duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
           Next im
           threadmetronomo = ThreadCall metronomo()
        EndIf 
         GrabarPenta=1 ' redundante ,,, 
      EndIf 
'-------------------------------
      If eventnumber()= BTN_ROLL_PARAR And GrabarPenta=1 Then
         SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
         SetGadgetstate(BTN_ROLL_GRABAR_MIDI , BTN_LIBERADO)
         GrabarPenta=0
         metronomo_si=0
         terminar_metronomo=1
         COMEDIT=FALSE  
''      If NombreCancion > "" Then ' detiene todo pista aisalda o cancion 
            If play=1 Or playb=1 Or CPlay=1 Then
               CONTROL1=1 ' DETIENE EL PLAY 
               playloop=0:playloop2=0
               play=0 : playb=0:CPlay=0
               Sleep 20
            EndIf 
      ' EndIf
      EndIf
   
' ///////////////// BOTON VERDE PLAY CANCION ROLL ////////  28-02-2024 GUIA
      If eventnumber()= BTN_ROLL_EJECUTAR And COMEDIT=FALSE Then ' 13-02-2024 PROBAR BIEN
         SetGadgetstate(BTN_ROLL_PARAR, BTN_LIBERADO)
         SetGadgetstate(BTN_ROLL_GRABAR_MIDI , BTN_LIBERADO)
         terminar_metronomo=1
         If CPlay = 0 And MaxPos > 2 Then
            CPlay=1
            If NombreCancion > "" Then
               If play=1 Or playb=1 Then
                  CONTROL1=1 ' DETIENE EL PLAY 
                  playloop=0:playloop2=0
                  play=0 : playb=0
                  Sleep 20
               EndIf 
               thread1 = ThreadCall  PlayCancion(Track())
            Else
               thread1 = ThreadCall  PlayAll(Roll)
            EndIf
         EndIf   

      EndIf
' ---------------- BOTONES PORTSAL VOL PATCH CANAL A LA DERECHA Y ABAJO ...
'--------------------------------------------------------------------------------------------
' ////////////// PORT SAL EJEC ////////////////
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' osea el orden se crea al crear el nombre de la pista
' usaremos seleccion multiple tambien GetSelCountListBox 
' EN PROCESO...
      If  eventnumber()=BTN_EJEC_PORTSAL Then ' boton PortSal de track cbxnum o ejec cbxejec
          Dim As Integer miport =1, pis=0,num=0', cntpis
    '      If PISTASEJECSELECCIONADA=0 Then
    '          Exit Select 
    '      EndIf 
    '     If PISTASEJECSELECCIONADA=1 Then
    '        PISTASEJECSELECCIONADA=0
            'Dim vec(32) As Integer preparando seleccion multiple
            pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
            'cntpis=GetSelCountListBox(PISTASEJECUCIONES,@vec(0)) +1
            If pis=0 Then ' o cntpis=0
              Exit Select
            EndIf
'si es seleccion multiple cntpis sera el nro de pistas seleccionadas
' TOMAMOS DEL VECTOR LA 1ER PISTA Y hacemos pis=vec(1), procesamos todo
' para esa pista y luego ajustamos ese portout al resto  de las pistas
' solo en la primera seleccionamos y abrimos port en el resto no,,,
            If  pis >=1 Then ' o cntpis >=1
' en  sel multiple ahcemos una seleccion de port para una pista
' y luego la copiamos en las otras pistas seleccionadas, esto era mas facil
' con checkbox o es lo mismo no se siconviene
      ' Si la pista tiene un nombre y tiene datos de ejecucion
             If  tocaparam(pis).nombre  >""  And  tocaparam(pis).maxpos > 0 Then
                 miport=1   ' 1= VA A seleccion port Salida
                 ntkp=pis
               Dim As Integer k1 = pmTk(pis+32).portout
     ''''     thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))

               selportEjec(miport,ntkp)

    '  preparamos la grabacion SI HAY DATOS por cambio de portsal

                ReDim  toc.trk(1 To tocaparam(pis).maxpos)
              
                 For j As Integer =1 To   tocaparam(pis).maxpos
                        toc.trk(j).modo=Toca(pis).trk(j).modo
                        toc.trk(j).nota=Toca(pis).trk(j).nota
                        toc.trk(j).vel=Toca(pis).trk(j).vel
                 Next j
    Dim tocap As ejecparam = tocaparam(pis)
    
    ' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje

                pgmidi.toc=toc
'pgmidi.tocatope = tocatope
               pgmidi.tocap = tocap
               threadGrabamidi=@pgmidi
               GrabarMidiIn(pgmidi,pis) ' por PORSAL
  '''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))
             maxgrb= tocap.maxpos '27-11-2024
             Else ' no hay nombre y/o no hay datos
               miport=1   ' seleccion port Salida sin pista para tocar teclado
               ntkp=pis
   ''              thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))
               selportEjec(miport,ntkp)
 
             EndIf
             Dim k1 As Integer
' buscamos  elport de esta pista
             k1=CInt(tocaparam(pis).portout)
             If listOutAbierto(k1)=0 Then  'abrir port
                If listoutCreado( k1)=0 Then
                    midiout(k1) = rtmidi_out_create_default ( )
                    listoutCreado( k1)=1
                EndIf
                open_port midiout(k1),k1, nombreOut(k1)
                Dim As integer    porterror=Err 
                listoutAbierto( k1) = 1
                porterrorsub(porterror)
            EndIf
            EndIf
        '' EndIf
      EndIf
'--------------  
      If  eventnumber()=BTN_EJEC_VOL Then ' VOL futuro

      EndIf 
'--------------
      If  eventnumber()=BTN_EJEC_PAN Then 'PAN futuro

      EndIf 
'----------------
'////////////////// PATCH EJEC /////////////////////////////
      If  eventnumber()=BTN_EJEC_PATCH Then 'PATCH o insrumento de un Sinte,,,
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

          Dim As Integer instrum =0, pis=0,num=0

      '    If PISTASEJECSELECCIONADA=0 Then
      '       Exit Select 
      '    EndIf 

      '    If PISTASEJECSELECCIONADA=1 Then
      '       PISTASEJECSELECCIONADA=0
            pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
  
            If pis=0 Then 
              Exit Select
            EndIf

            patchsal=CInt(tocaparam(pis).patch)

           If  pis >=1 Then  
          '   If tocaparam(pis).nombre > ""  Then ''''And  tocaparam(pis).maxpos > 0  Then 
                   selInstORdenNum (instrum)
                    '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
                   tocaparam(pis).patch=CUByte (instrum)
                   pmTk(pis+32).patch=CUByte (instrum)
                   ''patchsal=instrum
                   ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
'--------------------------
' preparamos para grabar la pista por cambio de patch
              If tocaparam(pis).maxpos > 0 Then
    
                  ReDim  toc.trk(1 To tocaparam(pis).maxpos)
' si cargo dos pistas y grabo una tercera y grabo mas de longitud
' de lo que cargue, entonces cancela habria que hacer un redim preserve
' de las pistas anteriores si su maxpos es menor                
                   For j As Integer =1 To  tocaparam(pis).maxpos
                          toc.trk(j).modo = _ 
                          Toca(pis).trk(j).modo 'cancela
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                   Next j
                Dim tocap As ejecparam = tocaparam(pis)

' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
                pgmidi.toc=toc
'pgmidi.tocatope = tocatope
                pgmidi.tocap = tocap
                threadGrabamidi=@pgmidi
                GrabarMidiIn(pgmidi,pis) ' POR PATCH
  'ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

           '  Else
           '       patchsal=1
           '       instru=patchsal
           '       selInstORdenNum (instrum)
                   '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
           ''       tocaparam(pis).patch=CUByte (instrum)
            '      pmTk(pis+32).patch=CUByte (instrum)
            '      ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
           '  EndIf
             EndIf
           EndIf 
    ''   EndIf
      EndIf 


'////////////////// CANAL EJEC ///////////////// 
    If  eventnumber()=BTN_EJEC_CANAL Then ' CANAL de un synthe por ejemplo
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

        Dim As Integer canal =0, pis=0,num=0

      '  If PISTASEJECSELECCIONADA=0 Then
      '     Exit Select 
      '  EndIf 

      '  If PISTASEJECSELECCIONADA=1 Then
      '     PISTASEJECSELECCIONADA=0
           pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO

             
           If pis=0 Then 
             Exit Select
           EndIf
 
           If  pis >=1  Then  
             If tocaparam(pis).nombre > ""  And  tocaparam(pis).maxpos > 0  Then 
                 selcanalEjec (1,pis) ' 1 salida
                 'tocaparam(pis).canalent 
    '--------------------------
    ' preparamos para grabar la pista por cambio de patch
                ReDim  (toc.trk)(1 To tocaparam(pis).maxpos)
        ' PREPARAMOS PARA GRABAR A ARCHIVO
                For j As Integer =1 To   tocaparam(pis).maxpos
                     toc.trk(j).modo= _
                                Toca(pis).trk(j).modo
                     toc.trk(j).nota=Toca(pis).trk(j).nota
                     toc.trk(j).vel=Toca(pis).trk(j).vel
               Next j
   Dim tocap As ejecparam = tocaparam(pis)
   
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi,pis)  'POR CANAL
  ''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

             Else
                selcanalEjec (1,pis) ' 1 salida
                ''tocaparam(pis).canal =pmTk(pis+32).canalsalida
             EndIf
           EndIf
      '  EndIf
'-----------------------
  '       For k=1 To 32 ' pistastrack de cancion PARA QUE??
  '         If CheckBox_GetCheck( cbxnum(k))= 1  Then
  '            num=k
  '         EndIf
  '       Next k 
  '       If  num >=1 Then
  '           selcanal (1)  '''ESTO ES PARA ROLL SACAR CREO JMGJMG
  '          
  '       EndIf

    EndIf

''' en base alanterior terminar esta parte que es para pistas de cancion manual
'' mas adelante....cuando termine todo pistas ejec 
''para pistas de cancion manual futuro ???pero si ya hay para pistas manual??
'//////////////// SEL PORT DE ROLL O MANUALES O CANCION
     If  eventnumber()=BTN_ROLL_PORTSAL  And cierroport= 0 Then
         Dim As Integer miport =1, pis=0,num=0
         cierroport=1 ' asi entra una sola vez,,,

       '   If PISTASROLLSELECCIONADA=0 Then
       '       Exit Select 
       '   EndIf 

       '  If PISTASROLLSELECCIONADA=1 Then
       '     PISTASROLLSELECCIONADA=0
            pis=GetItemListBox(PISTASROLLSELECCIONADA) +1 ' DEVUELVE A PARTIR DE CERO
            ntk=pis
            If pis=0 Then
              num=0 
              Exit Select
            Else
              ntk=pis
              num=1   
            EndIf


' miport=1 estamos seleccionadno port de salida , de entrada es 2 midi in
            If  num=1 Then  ' se chequeop una pista no importa cual
             threadsel = ThreadCreate(@selport(), CPtr(Any Ptr, miport))
            EndIf
      '  EndIf    
     EndIf 
'-------------------
'////////////////// BOTON PATCH ROLL O CANCION O MANUAL /////////////////////////////
' futuro todas estos codigos de  case si son parecidos luego  algun dia 
' los convertiremos en rutinas,,,JMG RECORDAR...!
      If  eventnumber()=BTN_ROLL_PATCH Then 'PATCH o insrumento de un Sinte,,,
' //////// PATCH PARA CANCION PERO NO GRABA A DISCO,,    
         Dim  as Integer num = 0  , instrum =0,k=0
        If GrabarPenta=0 Then             
        '  If PISTASROLLSELECCIONADA=0 Then
        '      Exit Select 
        '  EndIf 

        ' If PISTASROLLSELECCIONADA=1 Then
        '    PISTASROLLSELECCIONADA=0
            k=GetItemListBox(PISTASROLL) +1 ' DEVUELVE A PARTIR DE CERO
  
            If k=0 Then 
              Exit Select
            EndIf

            num=k
'              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
' y si instrum es > 0 es un cambio
             
            instrum=CInt(Track(k).trk(1,1).nnn) 
            ntk=k 
              
        ' EndIf
         
        Else
           ntk=0:num=1
        EndIf  
         If  num >=1 Then
             selInstORdenAlfa (instrum)
             If CANCIONCARGADA =TRUE Then
             Else
               ntk=0
             EndIf
            pmTk(ntk).patch=CUByte(instrum)
            patchsal=pmTk(ntk).patch
            portsal=pmTk(ntk).portout
            If GrabarPenta=0 And playb=0 And play=0 Then
              Track(ntk).trk(1,1).nnn =CUByte(instrum)
            EndIf
            Roll.trk(1,NA).inst= CUByte(instrum)

            Dim As String nombreg
            If MaxPos > 2 Then 
              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Or NombreCancion > "" Then
                    GrabarRollaTrack(0)
              Else
                If  ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  
              carga=1 ' control de carga, anula calcompas durante la carga ,,etc
            EndIf
        EndIf

      End If

      If  eventnumber()=BTN_ROLL_CANAL Then
         Dim  as Integer num = 0  , instrum =0 ,k=0
       '   If PISTASROLLSELECCIONADA=0 Then
       '       Exit Select 
       '   EndIf 

       '  If PISTASROLLSELECCIONADA=1 Then
       '     PISTASROLLSELECCIONADA=0 
            k=GetItemListBox(PISTASROLL) +1 ' DEVUELVE A PARTIR DE CERO
  
            If K=0 Then 
              Exit Select
            EndIf
      '   EndIf    
            num=k
'              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
' y si instrum es > 0 es un cambio
           canalx=pmTk(k).canalsalida
           ntk=k 
         If  num >=1 Then
             selcanal (1) 'canal salida mitipo=!
             If CANCIONCARGADA =TRUE Then
              Else
               ntk=0
             EndIf
            Dim As String nombreg
            If MaxPos > 2 Then 
              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Or NombreCancion > "" Then
                    GrabarRollaTrack(0)
              Else
                If  ROLLCARGADO  Then
'                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
'                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  
              carga=1 ' control de carga, anula calcompas durante la carga ,,etc
            EndIf
         EndIf


      EndIf
     
      If eventnumber()= BOTON_SELECCION_EJECUCION  Then ' "S" 
         Dim As Integer pista
         Static As Integer cuantos
         If cuantos > 0 Then
            cargariniciotxt (DirEjecSinBarra)
            cuantos=0
         Else  
 
           For pista =1 To 32 
              If CheckBox_GetCheck (cbxejec(pista)) = 1 Then 
                 CheckBox_SetCheck (cbxejec(pista),0)
                 cuantos=cuantos+1
              EndIf
           Next pista
         EndIf
      EndIf 

