DisableGadget(PISTASROLL,0) 'habilita la pistaroll
DisableGadget(PISTASEJECUCIONES,0) 'habilita la pistasejecuciones
DisableGadget(BOTON_SELECCION_EJECUCION,0) 'habilita BOTON_SELECCION_EJECUCION

' include DE ROLLMUSIC.BAS, EVENTC, eventgadget
' tiene su scope

''' SetForegroundWindow(hwndC) 'kiki

' el codigo anterior que traia de disco esta en notas
' TODOS DICEN RUSO Y USA QUE VK_LBUTTON ES 1 PERO CON 1 NO ANDA
' SIN EMBARGO CON 3 ANDA A VECES..
''Print #1, "CTRL_EVENTGADGET DirEjecSinBarra ",DirEjecSinBarra
Dim As Integer k=0
Static As Integer millave

If eventnumber()=  PISTASROLL And GrabarPenta=0 And CANCIONCARGADA=TRUE Then
     
     ''        borrapos=0   ' no se usa
     ' ME GUSTO FUNCIONA ASI: DAR CLICK EN UNA PISTA LUEGO CON flecha arriba
     ' y abajo CAMBIA DE PISTA EN ROLL, para habilitar el  CLICK DERECHO CONTEXTUAL
     ' DAR ENTER Y LUEGO CLICK DERECHO APARECE EL MENU CONTEXTIUAL, PARA VOLVER
     ' AL INICIO DAR CLICK EN OTRA PISTA Y TODO COMIENZA DE NUEVO...
     '
     
     '--------------------------------------------------------------
     'SIMULA TAB AL DAR CLICK EN UN ITEM UNAPISTA DE ROLL EN LA LISTA
     clickpista=1 ' no incrementa el ntk que simula SC_TAB, el cual carga el track a Roll
     
     '--------------------------------------------------------------
     
     '  print #1,"CLICK lbutton EN LISTA WM==============="
     '  Print #1,"COORDENADAS X, Y ", GlobalMouseX,GlobalMouseY
     ''' ROLLCARGADO=FALSE
     '' porque CANCIONCARGADA=TRUE
     Dim item As String
     Dim As Integer ubi1,ubi2
     
     item=GetListBoxText(PISTASROLL,GetItemListBox(PISTASROLL))
     '  Print #1,"item 1580 ",item  ' 28-02-2024 esto aparece en debug sin roll
     If item > "" Then
          If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
          End If
          
          item=Trim(item)
          '  Dim nombre1 As String
          '   nombre1= NombreCancion + "\"+item +".rtk"
          '   print #1," NUEVO eventgadget click en lista nombre", nombre1
          ubirtk=3 ' ahora indice carga desde lista o memoria
          ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
          ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
          ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
          ' pero si quiero volver a disco solo debo resetear ubirtk=0
          ''' ntk=sacarNtk(item) ' este ntk no sirve para boorar
          ntk=GetItemListBox(PISTASROLL)+1  '''sacarNtk(item)
          Print #1,"LISTA PIANO ROLL CLICK EN PISTA DA NUMERO ",ntk
          ' aca no copia track a Roll
          Print #1,"ntk de item ", ntk
          nombre= titulosTk(ntk)
          '   Print #1,"ntk, nombre ",ntk, nombre
     End If
     
     
     ' ESTO NO VA ES DE MENU And eventNumber <> 1010 And EventNumber<> 1012
     
     
     ' /// // // / / /  menu contextual popup
     
     
     If eventnumber()=  PISTASROLL And  WM_VKEYTOITEM And  EventKEY = VK_RETURN  Then
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
               End If
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
                         If instancia =ARG1_1_TITULO  Then
                              DisableGadget(PISTASROLL,1)
                         End If
                         
                         DisplayPopupMenu(hMessages2,,)
                         If instancia =ARG1_1_TITULO  Then
                              DisableGadget(PISTASROLL,0)
                         End If
                         Exit Do
                    End If
               End If
               
          Loop
     End If
     '--------------------------------------------------------------
     ' este ntk sirve para identificar el ntk del archivo t del vector
     ' pero el ntk de la lista es otro vector y al borrar el indice cambia
     ' debo obtener el indice primero
     '' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT
     If WM_VKEYTOITEM Then '
          '           print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
          If EventKEY = VK_DELETE Then
               '          print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
               If NombreCancion > "" And ntk > 0  Then
                    borrar=2
                    DeleteListBoxItem(PISTASROLL,GetItemListBox(PISTASROLL))
                    '            print #1,"LISTABOX EventKeyDown borrar ntk",ntk
                    Print #1,"LISTBOX  titulosTk(ntk)= ",titulosTk(ntk)
                    copiarATemp (titulosTk(ntk),pistasTk(ntk))
                    BorrarPista (titulosTk(ntk))
                    titulosTk(ntk)=""
                    pistasTk(ntk)=""
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
                    Dim As integer i1,i2
                    For f1 As Integer=ntk To Tope-1
                         ReDim (Track(f1).trk ) (1 To MaxPosTope,1 To lim3)
                         titulosTk(f1)=titulosTk(f1+1)
                         Sleep 10
                         copiaTrackaTrack (Track(),f1,f1+1)
                         copiarPmtkaPmtk(f1 , f1+1)
                    Next f1
                    titulosTk(Tope)=""
                    Tope=Tope-1
                    CheckBox_SetCheck(cbxnum(Tope),0)
                    clickpista=1
                    borrar=0
               End If
          End If
          
          ' aca no debe leer a disco solo conmutar de track en track
          '------------------------------
          ' para que funciones lso eventos se debe carcar alguna cancion y
          ' dar click en la pista roll luego pulsar F1 o cualquier otra Tecla
          ' lo raro es que no funciones sin cargar cancion ¿??
     End If
     
     '       Print #1," CLICK EN LISTA FIN "
End If

If eventnumber()=  PISTASROLL Then '''PORQUE SOLO EN GRABAPENTA ??? And GrabarPenta=1 Then
     PISTASROLLSELECCIONADA=1
     PISTASEJECSELECCIONADA=0
End If

'-----------------------------------------------------
'''DisableGadget(PISTASROLL,1) 'deshabilita la pistaroll PUPU

If eventnumber()=  PISTASEJECUCIONES Then
     ''' ESTE MOUSEBUTTONS AND LEFTBUTTON NO FUNCIONA !!! LA PUTA MADRE
     '''''          If MOUSEBUTTONS AND LEFTBUTTON  Then
     PISTASEJECSELECCIONADA=1
     PISTASROLLSELECCIONADA=0
     Print #1,"CLICK lbutton EN LISTA EJECUCIONES ==========="
     Dim item As String
     Dim As Integer ubi1,ubi2
     
     item=GetListBoxText(PISTASEJECUCIONES,GetItemListBox(PISTASEJECUCIONES))
     If item > "" Then
          '  If Len (item) < 24 Then ' son 29 + 4 de un numero que lo sacare (32)
          '    item = item + String( 40-Len(item),32)
          '  EndIf
          '  Print #1,"ITEM pista ",item
          '  item=Trim(item)
          
          
          ntkp=GetItemListBox(PISTASEJECUCIONES)+1  '''sacarNtk(item)
          nombre= titulosEj(ntkp)
          ''    If tocaparam(ntkp).maxpos > 0 Then
          SetGadgetText(TEXT_TOPE, Str(tocaparam(ntkp).maxpos))
          ChangeProgram ( tocaparam(ntkp).patch  , tocaparam(ntkp).canal, tocaparam(ntkp).portout)
          ''    EndIf
          Print #1,"//////ntkp seleccionado ",ntkp, nombre,tocaparam(ntkp).maxpos,tocaparam(ntkp).patch
          
          
     End If
     ''''          EndIf
     ' ESTO NO VA ES DE MENU And eventNumber <> 1010 And EventNumber<> 1012
     
     '------------------------------
     
     
End If

'  CUAL PISTA DE ROLL SE ESCUCHA SEGUN LO SELECCIONADO
' este es otro metodo para seleccionar todas las pistas cargadas
' y hacer que suenen o deseleccionarlas yque no suene ninguna
' aca no se usa un archivo de estado como en ejecuciones de pista
' no lo necesita... es todo o nada pero no recuerda que se escuchaba o no
' siemrpe que haya una cancion cargada .. es un boton de todo o nada
' solo si cancion esta cargada...BOTON "S"
''DisableGadget(PISTASEJECUCIONES,1) 'deshabilita la pistasejecuciones
If eventnumber()=BOTON_SELECCION_PISTA_ROLL And CANCIONCARGADA=TRUE Then
     disablegadget(PISTASROLL,0)
     Dim i As integer
     If cntsuena =0 Then
          SuenaTodo=0
     End If
     
     If cntsuena =Tope And suenaTodo >= 1 Then
          SuenaTodo=1
          cntsuena=0
     End If
     
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
End If
'------------------
' revisar CheckBox_GetCheck de las ejecuciones
'-------------------------------------------------------
'//////////////// BOTON ROJO COMIENZO GRABACION MIDI EJEC ////////////////// O PATRON

'-----------------------------000000000000000000000000----------------------------------------

If eventnumber()= BTN_MIDI_GRABAR And GrabarEjec=HabilitaGrabar Then ' BOTON GRABAR ROJO
     SetGadgetstate(BTN_MIDI_GRABAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
     SetGadgetstate(BTN_MIDI_CARGAR,BTN_LIBERADO)
Print #1,"'//////////////// BOTON ROJO COMIENZO GRABACION MIDI EJEC ////////////////// "
' a cada pista al inicio debo darle canticks de longitiud asi cualquier pista puede seguir grabando y listo????

' VEO SI LA PISTA YA TENIA DATOS 
 nroTicksAntesDeGrabar =tocaparam(ntoca).maxpos 
startcall=0  ''para indicar cuando entra el primer pulso del keyboard midi en esta grabacion
kSilencio=1 '' cuenta silencios antes del primer on del teclado midi in
 


''MIENTRAS QUE ESPERAMOS EL COMIENZO DEL PRIMER On DEL USURIO HAY UN TIEMPO DE SILENCIOS
CTRL1092 () ''' OJO RESETEA JGRB A  CERO TAMBIEN , ABRE PORTIN Y  CIERRA LOS OTROS
''  ademas ojo cheque a grabar y ejecutar podrian ser distintos no el mismo por eso se repite
'' solo para grabar
   If ntoca=0 Then
      Exit Select
   EndIf

     ' EVENTO 10

Static  As Integer cntgrabar
cntgrabar =cntgrabar+1
'     Print #1,"Entro a btn_midi_grabar EJEC "
Dim  As Integer k3=0 
GrabarEjec =GrabarPistaEjecucion
     k3=0:  ntoca=0 ' usa la k de rollmusic.bas ...en fin parece no  afecta
Parar_De_Dibujar=NO 'impide el play  o lo resetea
     terminar_metronomo=2 '' PARAQUE FUNCIONE EL METRONOMO DE GRABAR Y NO EL DE ENTRENAR 

 For K=1 To 32
   If CheckBox_GetCheck( cbxgrab(k))= 1 Then ''solo para grabar
      ntoca=k
     '''''tocaparam(k).canal=k-1
      Exit For
   End If
  Next k
    
     pmEj(ntoca).vol=90
      tocaparam2(ntoca).vol=90 ' versionEJEC=2
      Globalvol=90
     pmEj(ntoca).pan=64
      tocaparam2(ntoca).pan=64 ' versionEJEC=2
      Globalpan=64
     tocaparam(ntoca).canal= ntoca
   If ntoca=0 Then
      Exit Select
   EndIf
Print #1, "[";cntgrabar;"]=======GRABANDO==EN PISTA ==========";ntoca
      Print #1,"CANTIDAD DE DATOS EN LA PISTA, GRABACION NRO  ";nroTicksAntesDeGrabar ;cntgrabar
' CADA GRABACION SE INICIA CON UN VECTOR VACIO PARA LA CARGA EN MYCALLBACK
      Redim  CargaIn (1 To CantTicks)
  jgrb=0 ' INDICADOR DE LA CANTIDAD DE NOTAS INGRESADAS EN MYCALLBACK
     'ntoca se usa en my callback es la  pista ejec que se esta grabando
     ' 15min son   60 negras * 15min * 96 ticks =  86400 ticks a I=60
     ' 15min son 120 negras * 15min * 96 ticks = 172800 ticks a I=240
     ' 15min son 240 negras * 15min * 96 ticks = 345600 ticks a I=240
'vector maximo 345600 posiciones para 15 min de duracion, no es para operas jaja
     'la idea es que el usuario grabe hasta I=240 maximo 345600 ticks
     ' cada nota requiere 2 eventos on y off , EL OFF esta en el ultimo tick de la duracion
     ' forma parte de la duraicon de la nota 
     'If ntoca > 0 then NO DEBERIA HA CER FALTA
     '     ChangeProgram ( tocaparam(ntoca).patch, tocaparam(ntoca).canal,tocaparam(ntoca).portout )
     'End If
     
     SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
      
     arrancaPlay=NO
     'Print #1,"Nro pista ejec en grabacion ntoca "; ntoca
     'Print #1,"metronomo de 4 pulsos para comenzar a grabar EJEC"
' >----------------------------METRONOMO 
     If  metronomo_si=3 Then
          'Print #1,"EJEC TOCAR 4 PULSOS DEL instrumento elegido y luego el metronomo, onidopista_si ";sonidopista_si
          terminar_metronomo=2 ' NO ENTRAR POR ENTRENAR
          Dim As Integer im=0
          For im=1 To 4
               If sonidopista_si=3 Then
                    noteon(60,100,tocaparam(k).canal,tocaparam(k).portout,1,1) '' NOTA VEL ,CANAL, PORTSAL
                    noteoff(60,40, tocaparam(k).canal,tocaparam(k).portout,1,1)
               Else
                    PlaySound(ROLLDIR+"recur\INICIO.wav", 0, SND_FILENAME+SND_NODEFAULT + SND_ASYNC )
               End If
               duracion(Timer, (60/(tiempoPatron)) / FactortiempoPatron)
          Next im
          threadmetronomo = ThreadCall metronomo()
          GrabarEjec=GrabarPistaEjecucion
     End If
' <------------- fin metronomo y play tocaall
'> CALCULO SILENCIOS SOLO EN PISTA 1 VACIA
'''     If ntoca=1 And tocaparam(ntoca).MaxPos = 0 Then '' es la primer pista y VACIA  se justifica los silencios  
         threadSilencio = threadcall SilenciosAntesDeTocar () '' tengo "kSilencio" la cantidad de ticks antes del primer on en el teclado midi
'''    EndIf
'>------------------PLAYTOCAALL SI YA HAY ALGO GRABADO EN UNA PISTA ANTERIOR O LA MISMA     
     If ntoca > 1 Or nroTicksAntesDeGrabar > 0  Then ''''And tocaparam(ntoca).maxpos > 0  Then ' toco pistas ejec mientras grabo
          'la que se esta grabando debe estar deseleccionada en S
          '''           Parar_De_Dibujar=SI kokito
'Print #1,"LLAMA A PLAY DURANTE LA GRABACION, cntgrabar ";cntgrabar
          arrancaPlay=SI
          PARAR_PLAY_EJEC=NO 'HABILITA TOCAR EJECS
          PARAR_PLAY_MANUAL=NO ' HABILITA TOCAS CANCION

          Dim p As Integer Ptr
          p=@tocatope
          topeEjec=tocatope
          If tocaparam(ntoca).maxpos > 0 Then  '' estoy grabando una pista con datos
             If tocatope=1 Then
                maxgrb=tocaparam(ntoca).maxpos 
             EndIf
             If tocatope >1 Then '' verificamos el maxgrb contra esta pista con datos per odespues podremos agregar mas datos
                If maxgrb < tocaparam(ntoca).maxpos Then ''vamos calculando el maximo
                   maxgrb= tocaparam(ntoca).maxpos         
                EndIf    
            EndIf
            pmEj(ntoca).MaxPos=maxgrb          

         EndIf
         Print #1, "[";cntgrabar;"]=====MAXGRB EN ==TOCAALL DURANTE GRABACION==EN PISTA =========="; maxgrb,ntoca
         
         threadG  = ThreadCreate (@PlayTocaAll, p)
         GrabarEjec=GrabarPistaEjecucion  
     End If
     ' O SEA ASI COMOESTA SOLO NOS PREPARA PARA PARA TOCAR Y LUEGO SE CALLA
     ' DEBERIA SER OPCIONAL QUE SE CALLE O NO DESPUES DE LOS 4 PRIMEROS
     '
     GrabarEjec=GrabarPistaEjecucion

Print #1,"EN FIN BOTON ROJO ntoca en BTN_MIDI_GRABAR "; ntoca
Print #1,"'//////////////// BOTON ROJO FIN //////////////////
EndIf ' end event 10 BTN_MIDI_GRABAR ROJO LISTA DERECHA

'//////////////// BOTON NEGRO STOP EJEC  , GRABA A DISCO //////////////////

'-----------------------000000000000000000000000000000-----------------------

' ESTADO 26-07-2026 LA SEGUNDA GRABACION EMPIEZA AL FINAL DESPUES DE UN GAP
' NO DONDE QUIERO
If eventnumber()= BTN_MIDI_PARAR  And ntoca > 0   Then ' BOTON STOP NEGRO DE MIDI-IN
     SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
     SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_LIBERADO)
     ''''''SetGadgetstate(BTN_MIDI_PARAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_MIDI_CARGAR,BTN_LIBERADO)
CheckBox_SetCheck( cbxgrab(ntoca),0)
'> CALCULO SILENCIOS 
         Print #1,"SILENCIOS " ;kSilencio
          
         If  kSilencio > 1 Then
             tocaparam(ntoca).Maxpos=kSilencio+tocaparam(ntoca).Maxpos
             pmEj(ntoca).MaxPos = tocaparam(ntoca).Maxpos
             If maxgrb < tocaparam(ntoca).Maxpos Then 
                maxgrb = tocaparam(ntoca).Maxpos
             EndIf
             tocaparam2(ntoca).tipoDelta=1       
            tocaparam(ntoca).delta =kSilencio
            pmEj(ntoca).tipoDelta=1
         Else
           tocaparam2(ntoca).tipoDelta=0    
           tocaparam(ntoca).delta =0
           pmEj(ntoca).tipoDelta=0
        EndIf

     If  tocaparam(ntoca).Maxpos=0 Then
         startcall=1
         GrabarEjec=HabilitaGrabar
        terminar_metronomo=SI
        ''''CheckBox_SetCheck( cbxgrab(ntoca),0)
       SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
          Exit Select
     End If

     Print #1,"'//////////////// BOTON NEGRO STOP EJEC  , GRABA A DISCO //////////////////"
  ''   Print #1,"ntoca en BTN_MIDI_PARAR "; ntoca
  ''   CheckBox_SetCheck( cbxgrab(ntoca),0)  ' sacamos el check de grabacion
' necesario porque  solo al pulsar en grabar en esa mis mapista u otra se arma de nuevo bien
' las condiciones para grabar,,asi obligamos a que  se pulse de  nuevo el check G

''ESTADO:_ GRABA UNA SEGUNDA VEZ EN LA MISMA PISTA PERO LEJOS DE LA PRIMERA GRABACION
'' LE METE UN GAP O INTERVALO QUE NO SE CUAL ES  
     ' If ubiejec=2 Then
     '    ubiejec=0
     '    Exit Select
     ' EndIf
''nroTicksAntesDeGrabar  SE CARGO INMEDIATAMENTE AL PULSAR GRABAR,ACA EN PARAR LOS USAMOS SI SON > 0

      CantTicks=CantMin*PPQN*tiempoPatron

     If GrabarEjec=GrabarPistaEjecucion Then ''''And PlayEj=NO Then
'' SI EN MYCALLBACK SE GRABARON DATOS tocaparam(ntoca).MaxPos > 0 y CargaIn()         
          If tocaparam(ntoca).MaxPos > 0 or GrabarEjec=GrabarPistaEjecucion  Or GrabarEjec=GrabarPatronaDisco  Then
               GrabarEjec=HabilitaGrabar '' desic ahabilitar=0
               Parar_De_Dibujar=NO
               arrancaPlay=NO
               terminar_metronomo=SI
              If nroTicksAntesDeGrabar  > 0 Then ' NO INDICA POSICION son NRO ticks que ya tenia esta pista antes que se grabe GRABARMAS
                  Print #1, "GRABAMAS -- K, tocaparam(ntoca).MaxPos ";k , tocaparam(ntoca).MaxPos  ''' SON LOS TICKS
                  GRABARMAS=TRUE
              Else
                  GRABARMAS=FALSE
              EndIf
              Dim As Integer I1=1, j =0, pj
              Print #1, "I1 , ntoca "; I1, ntoca
             ' kSilencio ya se cargo en GRABAR y en los tocoparam y maxgrb
             If kSilencio > 1 Then '' muevo k a kSilencio per otodavia no se cargo nada
                Print #1,"ARRANCO LA 1er GRABACION EN kSilencio= tocaparam(ntoca).delta, kply ";kSilencio, kply
                 k=kSilencio   ' el maxgrb y parametros se calcularon en GRABAR
             EndIf
          EndIf



''son los ticks acumulados de la pista que esta sonando no importa cual, Y CUANDO SE TOCA EL TECLADO MIDI
'' GRABACION EN UN TIEMPO DADO MAYOR AL INICIO DEL PLAY DE UNA PISTA CON O SIN DATOS PREVIOS 
          If  kSilencio  > 1  Then ' una segunda grabacion sobre la mism pista o en pista nueva con o sindatos
                   Print #1,"ARRANCO LA grabarmas EN kSilencio, jToca ";kSilencio , jToca
                   print #1,"STOP 03 Toca(ntoca).maxpos, ntoca ",tocaparam(ntoca).maxpos,ntoca
                 
                ' a) sin datos
                 If GRABARMAS=FALSE  Then    
                    'DATOS A GRABAR A PARTIR DE kSilencio le sumanos los grabado en mycallback
                    tocaparam(ntoca).maxpos = kSilencio +tocaparam(ntoca).maxpos
                    If maxgrb < tocaparam(ntoca).maxpos Then
                       maxgrb = tocaparam(ntoca).maxpos 
                    EndIf
                 EndIf
                'b) con datos
                 If GRABARMAS=TRUE   Then    ' hay datos de antes 
                    ''en esta pista regrabada la longitud es
                    Dim maxnew As Integer
                    maxnew =kSilencio + tocaparam(ntoca).maxpos '' donde + datos nuevos
                 ' pero como habia otras pistas sonando debemos comparar con el maxgrb de todas las pistas   
                    If nroTicksAntesDeGrabar < maxnew  Then 
                      maxgrb = maxnew 
                      tocaparam(ntoca).maxpos=maxnew '' lo grabado mas donde, supera el maxpos orig de la pista
                   Else
                      maxgrb=nroTicksAntesDeGrabar
                      tocaparam(ntoca).maxpos=nroTicksAntesDeGrabar '' lo grabado esta dentro del maxpos orig
                   EndIf   
                    
                 EndIf
                 pmEj(ntoca).MaxPos = tocaparam(ntoca).Maxpos
                 k=kSilencio 
              Print #1," kSilencio respecto de lo que suena "; kSilencio
''ESTOY EN PARAR ACA YA SE TOCO LA PISTA A PARTIR DE ADONDEGRABO,LUEGO
''  tocaparam(ntoca).Maxpos ES SOLO LO QUE SE TOCO HAY QUE SUMARLE EL ADONDE
             ''tocaparam(ntoca).Maxpos=partes+tocaparam(ntoca).Maxpos
'' ES EL MAXPOS FINAL DE ESTA PISTA
          EndIf

          If k > 1 Then '' el silencio
          Else
               k=1
           EndIf
         
 Dim j1 As Integer
 I1=1
  Print #1," ANTES DEL DO DE CARGA EN TOCA DESDE CARGAIN puede contener kSilencio  k=  ",k
          Do
                    'if  k>=pmEj(ntoca).MaxPos+1 Then
                    '     Print #1,"STOP 04  k=pmEj(ntoca).MaxPos+1, GrabaMidiIn "
                    '     Exit Do
                    'End If
                    '  Print #1,"CargaIn(i1).modo ",CargaIn(i1).modo
                      '''i1 = i1 +1
                    Select Case  CargaIn( i1).modo ''I1 TIENE OTRO VALOR EN LA 2DA GRABADA=JGRB
                    Case 144,128
                         Print #1, " ntoca , K ,I1 ", ntoca , K ,I1 ,"k deberia contener a kSilecio"
                          FILEFLUSH(-1)
              
                If  Toca(ntoca).trk(k).modo =144   Or  Toca(ntoca).trk(k).modo =128  Then
                       k=k+1   'evitar que cualquer nota que exista grabada  anterior se borre 
                EndIf      
                ' EL 144   NO TIENE PARTES SI ES EL 1ERO, su PARTES VIENE EN EL 2DO EL 128 U OTRO 144
                         Toca(ntoca).trk(k).modo = CargaIn( i1).modo
                         Toca(ntoca).trk(k).nota  = CargaIn( i1).nota
                         Toca(ntoca).trk(k).vel    = CargaIn( i1).vel
                         i1 = i1 +1
                    End Select
                    If CargaIn( i1).partes > 0 Then   ''partes otcicks del evento anterior
                            Print #1,"CargaIn(i1).partes ",CargaIn(i1).partes
                            Print #1,"ntoca,k ",ntoca,k

                         For j1=1 To CargaIn( i1).partes
                              k=k+1
          'DEJAMOS PASAR TIEMPO PERO NO MARCAMOS NADA 27-07-2026
                         
                         Next j1
''LAS  GRABACIONES LA DETENEMOS CUANDO SE LLEGA AL JGRB LA CANT DE NOTAS GRABADAS EN CARGAIN
                         If I1= jgrb   Then
' aca agrandamos la pista y al final le colocamos en velocidad 182 como en roll fin de pista
' COMO LA AGRANDAS SI MAXGRB Y  SE ALCANZO???
                           tocaparam(ntoca).maxpos=k '' EL ULTIMO K DE PARTES incluye kSilencio
                           Toca(ntoca).trk(k+6).vel =182 '' fin secuencia como en Roll
                           pmEj(ntoca).MaxPos=k
                           If  maxgrb < tocaparam(ntoca).maxpos Then
                               maxgrb = tocaparam(ntoca).maxpos
                           EndIf 

                            Exit Do                          
                         End If
                         
                    Else
                         k=k+1
                    End If
                    If K >=CantTicks - tocaparam(ntoca).maxpos -1001 Then
                       Print #1,"se supero la CantTicks - tocaparam(ntoca).maxpos -1000, k ",k
                       Exit Do
                    EndIf
          Loop

Print #1,"---TERMINO LOOP DO "
               '''jgrb=0 20-07-2026 PARA SEGUIR GRABANDO SI QUIERO EN LA MISMA SESION
               '----------------------grabar archivo de pista
               Dim As String nombreg,myfil
               ' una cosa es grabar una pista y otra todas las pistas
               ' aca estamos grabando  una  sola pista,la marca da con G alpulsar el Boton Rojo
               Print #1,"EN Grabar midi-in nombre ",tocaparam(ntoca).nombre
               ''        DirEjecSinBarra=Toca(ntoca).nombre
               'y el path dondeesta? cuadnograbopareceque no
               ' al cargar si debo afinar eso....   Shared  As vivo toc
             CantTicks=CantMin*PPQN*tiempoPatron ' 15*96*240=345600
'' COPIAR A TOC VECTOR PARA PLAYALL
Print #1," pmEj(ntoca).maxpos cantTicks "; pmEj(ntoca).maxpos, cantTicks
               ReDim (toc.trk)(1 To  CantTicks) ''21-07-2026 carga ticks
               
               
               Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",ntoca
               Print #1,"tocaparam(ntoca).maxpos),ntoca ",tocaparam(ntoca).maxpos, ntoca
               Print #1,"kSilencio,   tocaparam(ntoca).maxpos "; kSilencio,   tocaparam(ntoca).maxpos

''//// LLENADO DEL VECTOR PARA GRABAR A DISCO      
'' toca se cargo y se redimensiono preserve en cantTicks
'' pero al grabar lo achico a su maxpos , al cargar lo redimensiono de nuevo         
                ReDim  As vivo toc.trk(1 To tocaparam(ntoca).maxpos)
              For j As Integer =1  To   tocaparam(ntoca).maxpos ''que valor tiene tocaparam(ntoca).maxpos
                    toc.trk(j).modo=Toca(ntoca).trk(j).modo
                    toc.trk(j).nota  =Toca(ntoca).trk(j).nota
                    toc.trk(j).vel    =Toca(ntoca).trk(j).vel
                    '''''' VER DATOS            Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
              Next j
              Dim tocap As ejecparam = tocaparam(ntoca)
              Dim tocap2 As ejecparam2 = tocaparam2(ntoca)

               '          Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
               '          Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
               '          Print #1,"PARAMETROS EJEC orden ",tocap.orden
               '          Print #1,"PARAMETROS EJEC delta ",tocap.delta
               '          Print #1,"PARAMETROS EJEC portout ",tocap.portout
               '          Print #1,"PARAMETROS EJEC patch ",tocap.patch
               '          Print #1,"PARAMETROS EJEC canal ",tocap.canal
               
               
               maxcarga=maxgrb  '' para grabar en inicio.txt
               SetGadgetText(TEXT_TOPE, Str(maxcarga))
               '''   Sleep 5000 ' QUIERO VER SI LO LLENA
               ' para una sola pista grabada el maxgrb es el maxpos de esa pista
' esto no es necesario pues todas las pistas se crean con CantTicks son iguales
' en todo caso se deberia ajustar antes de Grabar
               If  maxgrb > 0 And ntoca > 1 Then
                    If  maxgrb <  tocaparam(ntoca).maxpos Then ' para cada pista se define tocap de nuevo
                         maxgrb=tocaparam(ntoca).maxpos
                     End If
               End If
               ' si es grabacion nueva tocatope va incrementando apuntando a 1,2,3,4 etc
               ' y graba 1 por vez,el ultimo corriente...o actual
               Print #1,"//STOP: llama a GrabarMidiIn,,,MAXGRB hasta ahora "; maxgrb
' cada vez grabo el doble del maxgrb ? cuando cargo cargo ese doble
' pero deberia ponerlo luego en cantTicks!!!
               ReDim pgmidi.toc.trk(1 To tocaparam(ntoca).maxpos) ' lo unico a redimensionar grabamos el tamaño real 
               pgmidi.toc=toc 'pgmidi es global
			pgmidi.tocap = tocap ' dentro esta orden  ubyte
			pgmidi.tocap2 = tocap2 ' dentro esta orden  ubyte
               GrabarMidiIn(pgmidi,ntoca) 'POR STOP aca se graba bien el orden
               If NombreCancion="" Then
                  NombreCancion=ROLLDIR 
               EndIf   
               grabariniciotxt(NombreCancion, EJECUCION)
               
          '----------------
     Else
          If  playEj=SI Then
               PARAR_PLAY_EJEC=SI ' DETIENE EL PLAY DE CANCION O ROLL
               playEj=NO
               playloop=NO:playloop2=NO:Cplay=NO
               SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_LIBERADO) ' si hay cancion
               SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
               SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_LIBERADO)
               SetGadgetstate(BTN_MIDI_PARAR,BTN_PRESIONADO)
               SetGadgetstate(BTN_MIDI_CARGAR,BTN_LIBERADO)
               For i3 As Integer  = 1 To TopeEjec
                    portsal=CInt(pmEj(i3).portout)
                    alloff(pmEj(i3).canalsalida,portsal)
                    allSoundoff( pmEj(i3).canalsalida, portsal )
               Next i3
               Parar_De_Dibujar=NO
               Exit Select
          Else
               If playEj=NO And GrabarEjec=DesHabilitaGrabar Then
                    Exit Select
               End If
          End If
          
     End If
     deltatime=0  '''reset del deltatimeBack que devuelve el mycallback
     For  i As Short =1 To 32
          If CheckBox_GetCheck( cbxgrab(i))= 1   Then
               cancel_callback(midiin(tocaparam(i).portin )) ' porque lso port fisicos empiezan desde cero
               listinAbierto( tocaparam(i).portin) = 0
               close_port (midiin(tocaparam(i).portin))
               teclado=0
               
          End If
     Next i
     GrabarEjec=HabilitaGrabar
     Print #1,"'//////////////// BOTON NEGRO FIN //////////////////
Else
     If ntoca=0 Then
     SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
     End If

End If
'//////////////// BOTON VERDE PLAY EJEC //////////////////
' si hay una cancion de pistas trk el el grafico cargada, al dar play debera tocar
' lacancion y las pistas chequeadas en columna 'S' de las ejecuciones,deese modo
' sincronizaremos el arranque solamente. Esto se puede usar para escuchar o
' al grabar una pista nueva de ejecuciones por uncontrolador midi.,(teclado midi por ej)

If eventnumber()= BTN_MIDI_EJECUTAR And Parar_De_Dibujar=NO Or  GrabarEjec =PatronDeEjecucionCompleto  Then ' BOTON PLAY VERDE DE MIDI-IN
     SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
     '''''SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
     SetGadgetstate(BTN_MIDI_CARGAR,BTN_LIBERADO)

     If ntoca=0 Then
         SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_LIBERADO)
         Exit Select 
     EndIf
     playEj=SI
     Parar_De_Dibujar=NO
     PARAR_PLAY_EJEC=NO
     PARAR_PLAY_MANUAL=NO
     Dim p As Integer Ptr
     p=@ntoca 'ntoca se ajusta en CargarPstasEjec tambien
     
     ' tocar cancion de trakc si esta cargada
     ' son 2 threadas que se inician casi simultanemente pero sin control entre ellos
     ' por ahora
     ' ACA DEBERIA USAR MUTEX!!! ï¿½ï¿½ï¿½???
     '''Dim As Any Ptr sync =MutexCreate
     Print #1,"MaxPos en play verde ejec deberia ser cero si no hay grafico ",maxgrb
     threadG = ThreadCall  PlayTocaAll (p)
     grabariniciotxt(NombreCancion, EJECUCION)
     
End If
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
'/////////////////BITON CELESTE CARGAR EJEC //////////////////////

If eventnumber()= BTN_MIDI_CARGAR Then
     SetGadgetstate(BTN_MIDI_CARGAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
     SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
     SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_LIBERADO)
     
     Dim lugar As string
     If ubiejec > 0 Then
          CTRL10165 (lugar, "BATCH")
          ubiejec=0
     Else
          CTRL10165 (lugar, "ONLINE")
     End If
     DirEjecSinBarra = lugar
     SetGadgetstate(BTN_MIDI_CARGAR,BTN_LIBERADO)
End If
'----------------------------------------------------------------------------------------------
'//////////////// BOTON ROJO GRABAR EN ROLL NO HABILITADO EN TICKS POR AHORA //////////////////
' APLICABLE=FALSE
If eventnumber()= BTN_ROLL_GRABAR_MIDI And GrabarPenta=0 And APLICABLE Then
     'solo usado por ahora sin cancion cargada , si hay una cancion
     ' cargada solo hace falta GrabarPenta Roll grafico ya esta cargado
     SetGadgetstate(BTN_ROLL_GRABAR_MIDI,BTN_PRESIONADO)
     SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_PARAR , BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_CARGAR,BTN_LIBERADO)

     Exit Select  ''NO HABILITADO

     jgrbRoll=0
     GrabarPenta=1
     If abrirRollCargaMidi=0 Then 'EVITA CARGA ROLL DESDE ACA 2 VECES
          ' TAMBIEN USADO EN CARGA MIDI
          Print #1,"cALL rOLLLOOP II) por grabar midi "
          
          threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
          ''    SetForegroundWindow(hwnd)
          ''RollLoop ( param)
          abrirRoll=NO_CARGAR 'EVITA CARGA ROLL DE MENU PRONCIPAL
     End If
     ' SI EL USUARIO ABRE ROLL MANUALMENTE ..Y LEDA UN CLICK A EDICION Y LUEGO A EDIT
     COMEDIT=ENTRADA_NOTAS
     ''   SetForegroundWindow(hwnd)
     If  metronomo_si=3 Then
          Print #1,"Va a TOCAR 4 PULSOS DEL instrumento elegido y luego el metronomo"
          terminar_metronomo=0
          Dim As Integer im=0
          For im=1 To 4
               If sonidopista_si=3 Then
                    noteon(60,100,tocaparam(k).canal,tocaparam(k).portout,1,1) '' NOTA VEL ,CANAL, PORTSAL
                    noteoff(60,40,tocaparam(k).canal,tocaparam(k).portout,1,1)
               Else
                    PlaySound(ROLLDIR+"recur\INICIO.wav", 0, SND_FILENAME+SND_NODEFAULT + SND_ASYNC )
               End If
               duracion(Timer, (60/(PPQN*tiempoPatron)) / FactortiempoPatron)
          Next im
          threadmetronomo = ThreadCall metronomo()
     End If
     GrabarPenta=1 ' redundante ,,,
End If
'-------------------------------
If eventnumber()= BTN_ROLL_PARAR And (GrabarPenta=SI Or Cplay=SI Or Playb=SI Or Play=SI ) Then
     SetGadgetstate(BTN_ROLL_PARAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_GRABAR_MIDI , BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_CARGAR,BTN_LIBERADO)
     GrabarPenta=0
     ''Print #1, "542 GrabarPenta=0"
     metronomo_si=0
     terminar_metronomo=1
     COMEDIT=LECTURA
     If CPlay=SI Or Playb=SI Or Play=SI Then
          CPlay=NO: Playb=NO: Play=NO
          PARAR_PLAY_MANUAL=SI
          playloop=NO:playloop2=NO
          Cplay=NO :Playb=NO:Play=NO
          ubionline=0 ' y estamos online reseteamos sino vuelve a ejecutar
          BatchGraficoOCtrl=CONTROL ' esto ya vino corregido en control
          ''para grabar  a disco enrollmusic.ini lo repetimos por las dudas
     End If
     For i3 As Integer  = 1 To Tope
          portsal=CInt(pmTk(i3).portout)
          alloff(pmTk(i3).canalsalida,portsal)
          allSoundoff( pmTk(i3).canalsalida, portsal )
     Next i3
     Parar_De_Dibujar=NO
     
End If

' ///////////////// BOTON VERDE PLAY CANCION ROLL ////////  28-02-2024 GUIA
If eventnumber()= BTN_ROLL_EJECUTAR And COMEDIT=LECTURA Then
     SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_ROLL_PARAR, BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_GRABAR_MIDI , BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_CARGAR,BTN_LIBERADO)
     If (BatchGraficoOCtrl=3 Or BatchGraficoOCtrl=4 Or BatchGraficoOCtrl=5 ) And ubiejec=0 Then
     Else
          mensajeEstado="NO USAR TAB DURANTE PLAY CON MEZCLA DE EJECUCIONES DE TECLADO CON MANUALES, SE CONGELARA LA SECUENCIA"
     End If
     terminar_metronomo=1
     If (playb = NO Or Cplay=NO )And (MaxPos> 2  Or Maxgrb > 2) Then
          GrabarPenta=0
          naco=0:naco2=0
          If INSTANCIA = ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Or instancia <= ARG4_INSTRU  Then '04-10-2025
          Else
               ' SetGadgetstate(BTN_ROLL_GRABAR_MIDI,0) ' 10-04-2022 DE  VENTANA CTROL
               SetGadgetstate(15,0) ' 20-02-2025
          End If
          If CANCIONCARGADA = TRUE  And CPlay=NO   Then
               Parar_De_Dibujar=NO
               Cplay=SI : s5=NO 'Necesita mas tiempo de cpu
               '      Sleep 100
               grabariniciotxt(NombreCancion, CANCION)
               FileFlush (-1)
               thread1 = ThreadCall  PlayCancion(Track())
               
               CPlay=SI
               
          ElseIf   playb=NO And  CANCIONCARGADA = FALSE Then
               ' ESTA OPCION NUCA PODRA EJECUTRSE EN PARALELO PORQUE IMPPLICA UN ROLL Y POR ENDE
               ' LLENARA EL ROLL GRAFICO QUE LA CANCION DE RTK ESTA USANDO
               Print #1,"llama a playall"
               Playb=SI:s5=NO
               '       Sleep 100
               thread2 = ThreadCall  playAll(Roll)
          End If
          
     End If
End If

If eventnumber()=BTN_ROLL_CARGAR And CANCIONCARGADA=FALSE Then
     SetGadgetstate(BTN_ROLL_CARGAR,BTN_PRESIONADO)
     SetGadgetstate(BTN_ROLL_PARAR,BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_GRABAR_MIDI,BTN_LIBERADO)
     SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_LIBERADO)
     
     If play=SI Or playb=SI Then
          PARAR_PLAY_MANUAL=SI ' DETIENE EL PLAY VEREMOS
          playloop=NO:playloop2=NO
          Sleep 2
     End If
     CargaArchivo(Roll, 0)
     cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
     cierroedit= 0
     carga=1  ' <======= control de Carga
     ROLLCARGADO=TRUE ' aunque este en cancion puedo cargar un roll
     SetGadgetText (TEXT_TOPE,Str(pmTk(0).Maxpos))
     abrirRoll=REABRIR_ROLL_CON_DATOS_CARGADOS
     Terminar=NO_TERMINAR_CON_DATOS_CARGADOS
End If

' ---------------- BOTONES PORTSAL VOL PATCH CANAL A LA DERECHA Y ABAJO ...
'--------------------------------------------------------------------------------------------
' ////////////// PORT SAL EJEC ////////////////
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' osea el orden se crea al crear el nombre de la pista
' usaremos seleccion multiple tambien GetSelCountListBox
' EN PROCESO...
If  eventnumber()=BTN_EJEC_PORTSAL Then ' boton PortSal de track cbxnum o ejec cbxejec
     Dim As Integer miport =1, pis=0,num=0', cntpis
     Print #1,"1 en BTN_EJEC_PORTSAL"
     pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
     Print #1,"en BTN_EJEC_PORTSAL pis ";pis
     ''           If pis=0 Then ' o cntpis=0
     ''             Exit Select
     ''           EndIf
     'si es seleccion multiple cntpis sera el nro de pistas seleccionadas
     ' TOMAMOS DEL VECTOR LA 1ER PISTA Y hacemos pis=vec(1), procesamos todo
     ' para esa pista y luego ajustamos ese portout al resto  de las pistas
     ' solo en la primera seleccionamos y abrimos port en el resto no,,,
     If  pis >=1 Then ' o cntpis >=1
          ' en  sel multiple ahcemos una seleccion de port para una pista
          ' y luego la copiamos en las otras pistas seleccionadas, esto era mas facil
          ' con checkbox o es lo mismo no se siconviene
          ' Si la pista tiene un nombre y tiene datos de ejecucion
          
          Print #1,"tocaparam(pis).nombre ",tocaparam(pis).nombre
          Print #1,"tocaparam(pis).maxpos ",tocaparam(pis).maxpos
          ' PRIMERO HAY QUE GRABAR ALGO ENTONCES MAXPOS > 0 MMM Y SI QUIERO TOCAR
          ' ESCUCHANDO Y UN PATCH DE CERO??
          If  tocaparam(pis).nombre  >""  And  tocaparam(pis).maxpos > 0 Then
               miport=1   ' 1= VA A seleccion port Salida
               ntkp=pis
               Dim As UByte k1 = pmEj(pis).portout
               Print #1,"antes del cambio k1, listOutAbierto(k1) ", k1, listOutAbierto(k1)
               Print #1,"tocaparam(pis).portout previo al cambio",tocaparam(pis).portout
               ''''     thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))
               
               selportEjec(miport,ntkp)
               Print #1,"tocaparam(pis).portout despues del cambio",tocaparam(pis).portout
               
               '  preparamos la grabacion SI HAY DATOS por cambio de portsal
               
               ReDim  toc.trk(1 To tocaparam(pis).maxpos)
               Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis
               Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
               
               For j As Integer =1 To   tocaparam(pis).maxpos
                    toc.trk(j).modo=Toca(pis).trk(j).modo
                    toc.trk(j).nota=Toca(pis).trk(j).nota
                    toc.trk(j).vel=Toca(pis).trk(j).vel
                    '    Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
               Next j
               Dim tocap As ejecparam = tocaparam(pis)
               Dim tocap2 As ejecparam2 = tocaparam2(pis)
               
               '         Print #1,"portsal PARAMETROS EJEC nombre ",tocap.nombre
               '         Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
               '         Print #1,"PARAMETROS EJEC orden ",tocap.orden
               '         Print #1,"PARAMETROS EJEC delta ",tocap.delta
               '         Print #1,"PARAMETROS EJEC portout ",tocap.portout
               '         Print #1,"PARAMETROS EJEC patch ",tocap.patch
               '         Print #1,"PARAMETROS EJEC canal ",tocap.canal
               
               ' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
               
               pgmidi.toc=toc  'datos secuencia
               'pgmidi.tocatope = tocatope
               pgmidi.tocap = tocap
               pgmidi.tocap2 = tocap2
               threadGrabamidi=@pgmidi
               GrabarMidiIn(pgmidi,pis) ' por PORSAL
               '''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))
               Print #1,"despues de GrabarMidiIn pgmidi maxpos ",tocap.maxpos
               maxgrb= tocap.maxpos '27-11-2024
          Else ' no hay nombre y/o no hay datos
               miport=1   ' seleccion port Salida sin pista para tocar teclado
               ntkp=pis
               Print #1,"tocaparam(pis).portout previo al cambio",tocaparam(pis).portout
               ''              thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))
               selportEjec(miport,ntkp)
               Print #1,"tocaparam(pis).portout despues del cambio",tocaparam(pis).portout
               
          End If
          Dim k1 As Integer
          ' buscamos  elport de esta pista
          Print #1,"tocaparam(pis).portout cambiado ",tocaparam(pis).portout
          k1=CInt(tocaparam(pis).portout)
          Print #1,"k1 portout, listOutAbierto(k1) ", k1, listOutAbierto(k1)
          If listOutAbierto(k1)=0 Then  'abrir port
               If listoutCreado( k1)=0 Then
                    midiout(k1) = rtmidi_out_create_default ( )
                    listoutCreado( k1)=1
               End If
               open_port midiout(k1),k1, nombreOut(k1)
               Dim As integer    porterror=Err
               listoutAbierto( k1) = 1
               Print #1,"abro ",*nombreOut(k1)
               porterrorsub(porterror)
          End If
     End If
     '' EndIf
End If
'--------------
If  eventnumber()=BTN_EJEC_VOL Then '
     Dim As Integer pis
     pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
     'cntpis=GetSelCountListBox(PISTASEJECUCIONES,@vec(0)) +1
     Print #1,"en BTN_EJEC_VOL pis ";pis
     Print #1,"en BTN_EJEC_VOL versionEjec ";versionEjec
     If pis >=1 Then
          menuOldStr="[VOLEJEC]"
          ntkp=pis
          'Print #1,"1111) CONTROLGADGET [VOLEJEC] versionEjec ", pmEj(ntkp).versionEJEC
          threadvol=threadCall EntrarTeclado()
          ThreadWait threadvol
          'Print #1,"2222) CONTROLGADGET [VOLEJEC] versionEjec ", pmEj(ntkp).versionEJEC
          menuOldStr=""
          'statusBarGadget NO PUEDE IR EN UN THREAD CANCELA !!!
     End If
End If
'--------------
If  eventnumber()=BTN_EJEC_PAN Then 'PAN futuro
     menuOldStr="[PANEJEC]"
     threadpan=threadCall EntrarTeclado()
     ThreadWait threadpan
     Print #1,"sel pan Globalpan, ntkp ",Globalpan,ntkp
     
End If
If  eventnumber()=BTN_EJEC_ECO Then 'PAN futuro
     menuOldStr="[ECOEJEC]"
     threadpan=threadCall EntrarTeclado()
     ThreadWait threadpan
     Print #1,"sel pan Globaleco, ntkp ",Globaleco,ntkp
     
End If
If  eventnumber()=BTN_EJEC_CORO Then 'PAN futuro
     menuOldStr="[COROEJEC]"
     threadpan=threadCall EntrarTeclado()
     ThreadWait threadpan
     Print #1,"sel pan Globalcoro, ntk ",Globalcoro,ntkp
     
End If

'----------------
'////////////////// PATCH EJEC /////////////////////////////
If  eventnumber()=BTN_EJEC_PATCH Then 'PATCH o insrumento de un Sinte,,,
     ' si todavia no grabe nada tocaparam tendra el nombre y el orden
     ' o sea el orden se crea al crear el nombre de la pista
     
     Dim As Integer instrum =0, pis=0,num=0
     
     pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
     
     If pis=0 Then
          Exit Select
     End If
     Print #1,"pis > 0 ",pis
     patchsal=CInt(tocaparam(pis).patch)
     instrum=patchsal
     Print #1,"patchsal ",patchsal
     If  pis >=1 Then
          '   If tocaparam(pis).nombre > ""  Then ''''And  tocaparam(pis).maxpos > 0  Then
          If instrum=0 Then instrum=1 EndIf ''por omision
          selInstORdenAlfa (instrum,"EJEC", pis)
          '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
          Print #1," pista ejec  nro ",pis
          tocaparam(pis).patch=CUByte (instrum)
          pmEj(pis).patch=CUByte (instrum)
          patchsal=instrum
          ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
          Print #1,"ejecucion patch elegido tocaparam(pis).patch ", tocaparam(pis).patch
          '--------------------------
          ' preparamos para grabar la pista por cambio de patch
          If tocaparam(pis).maxpos > 0 Then
               
               'Print #1, "VA A HACER EL REDIM TOC, pis,maxpos ";pis; " ";tocaparam(pis).maxpos
               ReDim  toc.trk(1 To tocaparam(pis).maxpos)
               'Print #1, "VINO DE HACER EL REDIM TOC"
               '               Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis
               '               Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
               ' si cargo dos pistas y grabo una tercera y grabo mas de longitud
               ' de lo que cargue, entonces cancela habria que hacer un redim preserve
               ' de las pistas anteriores si su maxpos es menor
               For j As Integer =1 To  tocaparam(pis).maxpos
                    toc.trk(j).modo =      Toca(pis).trk(j).modo 'cancela
                    toc.trk(j).nota=Toca(pis).trk(j).nota
                    toc.trk(j).vel=Toca(pis).trk(j).vel
                    '        Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
               Next j
               Dim tocap As ejecparam = tocaparam(pis)
               '       Print #1,"patch ejec PARAMETROS EJEC nombre ",tocap.nombre
               '       Print #1,"patch ejec PARAMETROS EJEC mapos ",tocap.maxpos
               '       Print #1,"patch ejec PARAMETROS EJEC orden ",tocap.orden
               '       Print #1,"PARAMETROS EJEC delta ",tocap.delta
               '       Print #1,"PARAMETROS EJEC portout ",tocap.portout
               '       Print #1,"PARAMETROS EJEC patch ",tocap.patch
               '       Print #1,"PARAMETROS EJEC canal ",tocap.canal
               
               ' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
               pgmidi.toc=toc
               'pgmidi.tocatope = tocatope
               pgmidi.tocap = tocap
               threadGrabamidi=@pgmidi
               GrabarMidiIn(pgmidi,pis) ' POR PATCH
          End If
     End If
End If


'////////////////// CANAL EJEC /////////////////
If  eventnumber()=BTN_EJEC_CANAL Then ' CANAL de un synthe por ejemplo
     ' si todavia no grabe nada tocaparam tendra el nombre y el orden
     ' o sea el orden se crea al crear el nombre de la pista
     
     Dim As Integer canal =0, pis=0,num=0
     
     If PISTASEJECSELECCIONADA=0 Then
          Exit Select
     End If
     pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
     
     
     If pis=0 Then
          Exit Select
     End If
     
     If  pis >=1  Then
          If tocaparam(pis).nombre > ""  And  tocaparam(pis).maxpos > 0  Then
               selcanalEjec (1,pis) ' 1 salida
               Print #1," pista ejec  nro ",pis
               'tocaparam(pis).canalent
               Print #1,"ejecucion canal elegido 0 a 15 tocaparam(pis).canal ", tocaparam(pis).canal
               '--------------------------
               ' preparamos para grabar la pista por cambio de patch
                ReDim  (toc.trk)(1 To CantTicks)

               Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis
               Print #1,"tocaparam(pis).maxpos),pis ",tocaparam(pis).maxpos, pis
               ' PREPARAMOS PARA GRABAR A ARCHIVO
               For j As Integer =1 To   tocaparam(pis).maxpos
                    '                Print #1,"pis "; pis;" j ";j
                    toc.trk(j).modo=      Toca(pis).trk(j).modo
                    toc.trk(j).nota=Toca(pis).trk(j).nota
                    toc.trk(j).vel=Toca(pis).trk(j).vel
                    '''  Print #1, toc.trk(j).modo;" ";toc.trk(j).nota;" ";toc.trk(j).vel
               Next j
               Dim tocap As ejecparam = tocaparam(pis)
               '             Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
               '             Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
               '             Print #1,"PARAMETROS EJEC orden ",tocap.orden
               '             Print #1,"PARAMETROS EJEC delta ",tocap.delta
               '             Print #1,"PARAMETROS EJEC portout ",tocap.portout
               '             Print #1,"PARAMETROS EJEC patch ",tocap.patch
               '             Print #1,"PARAMETROS EJEC canal ",tocap.canal
               
               pgmidi.toc=toc
               'pgmidi.tocatope = tocatope
               pgmidi.tocap = tocap
               threadGrabamidi=@pgmidi
               GrabarMidiIn(pgmidi,pis)  'POR CANAL
               ''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))
               
          Else
               selcanalEjec (1,pis) ' 1 salida
               Print #1," pista ejec  nro ",pis
               Print #1,"ejecucion canal 0 a 15 elegido ", pmEj(pis).canalsalida
          End If
     End If
     
End If

''' en base alanterior terminar esta parte que es para pistas de cancion manual
'' mas adelante....cuando termine todo pistas ejec
''para pistas de cancion manual futuro ???pero si ya hay para pistas manual??
'//////////////// SEL PORT DE ROLL O MANUALES O CANCION
If  eventnumber()=BTN_ROLL_PORTSAL  And cierroport= 0 Then ' este no se recupera de archivo
     Dim As Integer miport =1, pis=0,num=0
     cierroport=1 ' asi entra una sola vez,,,
     If PISTASROLLSELECCIONADA=1 Then
          pis=GetItemListBox(PISTASROLL) +1 ' DEVUELVE A PARTIR DE CERO
     End If
     
     ntk=pis
     ' miport=1 estamos seleccionadno port de salida , de entrada es 2 midi in
     '   If  num > 0 Then  ' se chequeop una pista no importa cual
     threadsel = ThreadCreate(@selport(), CPtr(Any Ptr, miport))
End If
'-------------------------------------------------------
If  eventnumber()=BTN_ROLL_PAN Then 'PAN  REPRODUCCION HACIA LOS LADOS DERECHA IZQUIERDA,,,
     menuOldStr="[PAN]"
     threadpan=threadCall EntrarTeclado()
     ThreadWait threadvol
     Print #1,"sel pan Globalpan, ntk ",Globalpan,ntk
End If
If  eventnumber()=BTN_ROLL_ECO Then 'ECO ,,
     menuOldStr="[ECO]"
     threadeco=threadCall EntrarTeclado()
     ''ThreadWait threadeco
     ''Print #1,"sel pan Globalpan, ntk ",Globalpan,ntk
End If
If  eventnumber()=BTN_ROLL_CORO Then 'CORO ,,
     menuOldStr="[CORO]"
     threadcoro=threadCall EntrarTeclado()
     ''ThreadWait threadcoro
     ''Print #1,"sel pan Globalpan, ntk ",Globalpan,ntk
End If
If  eventnumber()=BTN_ROLL_VOL Then ' VOL
     menuOldStr="[VOL]"
     threadvol=threadCall EntrarTeclado()
     ''ThreadWait threadvol
     
End If



'-------------------
'////////////////// BOTON PATCH ROLL O CANCION O MANUAL /////////////////////////////
' futuro todas estos codigos de  case si son parecidos luego  algun dia
' los convertiremos en rutinas,,,JMG RECORDAR...!
If  eventnumber()=BTN_ROLL_PATCH Then 'PATCH o insrumento de un Sinte,,,
     ' //////// PATCH PARA CANCION PERO NO GRABA A DISCO,,
     'Print #1,"EN BTN_ROLL_PATCH"
     Dim  as Integer num = 0  , instrum =0,k=0
     If GrabarPenta=0 Then
          If PISTASROLLSELECCIONADA=0 Then
               Exit Select
          End If
          
          k=GetItemListBox(PISTASROLL) +1 ' DEVUELVE A PARTIR DE CERO
          ' NO CAMBIA PISTA 0 DE ROLL SOLO A PARTIR DE LA 1 DE UNA CANCION
          If k=0 Then
               Exit Select
          End If
          
          num=k
          '              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
          ' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
          ' y si instrum es > 0 es un cambio
          If  ROLLCARGADO  Then
          Else
               instrum=pmTk(ntk).patch  'CInt(Track(k).trk(1,1).nnn)
               If instrum=0 Then
                    pmTk(ntk).patch= CUByte(instrum)
                    pmTk(0).patch= CUByte(instrum)
               End If
               Print #1,"k, instrumento en check ";k,instrum
               ntk=k
          End If
          
     Else
          ntk=0:num=1
     End If
     Print #1, "PATCH . num,instrum ", num, instrum
     If  num >=1 Then
          selInstORdenAlfa (instrum, "ROLL",ntk)
          Print #1, "patch instrum seleccionado ", instrum
          If CANCIONCARGADA =TRUE Then
          Else
               ntk=0
          End If
          pmTk(ntk).patch=CUByte(instrum)
          pmTk(0).patch= CUByte(instrum)
          patchsal=pmTk(ntk).patch
          portsal=pmTk(ntk).portout
          If GrabarPenta=NO And playb=NO And play=NO Then
               pmTk(ntk).patch=CUByte(instrum)
          End If
          Print #1, "patch portsal almacenado, instru ", portsal, instrum
          pmTk(ntk).patch= CUByte(instrum)
          pmTk(0).patch= CUByte(instrum)
          Dim As String nombreg
          If MaxPos > 2 Then
               If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Or NombreCancion > "" Then
                    GrabarRollaTrack(0,0,"grabartrkcancion")
               Else
                    If  ROLLCARGADO=TRUE Then
                         'aca graba el roll con patch
                         If intentos=0 Then
                              LLAMA_GRABAR_ROLL("",intentos)
                         End If
                         Sleep 1000,1
                         intentos=0
                    End If
               End If
               carga=1 ' control de carga, anula calcompas durante la carga ,,etc
          End If
     End If
     
End If

If  eventnumber()=BTN_ROLL_CANAL Then
     Dim  as Integer num = 0  , instrum =0 ,k=0
     If PISTASROLLSELECCIONADA=1 Then
          k=GetItemListBox(PISTASROLL) +1 ' DEVUELVE A PARTIR DE CERO O SEA K=1 SI SE SELECCIONO LA PISTA
     End If
     If K=0 Or CANCIONCARGADA=FALSE Then '' K=0 NO SE CARGO ROLL PERO PUEDE HABER SIDO CARGADO POR GRAFICO O POR MENU CTRL
          ntk=0
     End If
     If K=1 And CANCIONCARGADA=FALSE Then ' HAY UN ROLL CARGADO EN LA LISTA
          K=0:ntk=0
     End If
     If K >0 And CANCIONCARGADA=TRUE Then ' HAY UN TRACK CARGADO EN LA PISTA
          ntk=K
     End If
     
     'SI HAY NOMBRE EN  LA PISTA 1 Y CANCIONCARGADA_=TRUE , ES EL 1ER TRACK
     'SI HAY NOMBRE EN  LA PISTA 1 Y CANCIONCARGADA_=FALSE , ES UN  ROLL CARGADO
     'SI NO HAY NOMBRE EN  LA PISTA 1 Y ROLLCARGADO=TRUE ..SE CARGO DESDE GRAFICO
     
     
     num=k
     '              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
     ' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
     ' y si instrum es > 0 es un cambio
     canalx=pmTk(k).canalsalida
     Print #1,"k, canalsalida  ";k, canalx
     ntk=k
     '         Print #1, "PATCH . num,instrum ", num, instrum
     threadcanal = ThreadCall selcanal(1) 'canal salida mitipo=1
     '             Print #1, "patch instrum seleccionado ", instrum
     If CANCIONCARGADA =TRUE Then
     Else
          ntk=0
     End If
     pmTk(ntk).canalsalida=canalx
     Dim As String nombreg
     If MaxPos > 2 Then  '' ESTO DE GRABAR AUTOMATICO NO ME GUASTA MUHC
          If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Or NombreCancion > "" Then
               GrabarRollaTrack(0,0,"grabartrkcancion")
          Else
               If  ROLLCARGADO  Then
                    '                  'aca graba el roll con patch
                    If intentos=0 Then
                         LLAMA_GRABAR_ROLL("",intentos)
                    End If
                    Sleep 1000,1
                    intentos=0
                    ' no el undo dolo se debe borrar al ahcer nuevo creo
               End If
          End If
          carga=1 ' control de carga, anula calcompas durante la carga ,,etc
     End If
     
End If

If eventnumber()= BOTON_SELECCION_EJECUCION  Then ' "S"
     Dim As Integer pista
     Static As Integer cuantos
     If cuantos > 0 Then
          cargariniciotxt (DirEjecSinBarra, EJECUCION )
          RecalCompas(ritmo)
          cuantos=0
     Else
          
          For pista =1 To 32
               If CheckBox_GetCheck (cbxejec(pista)) = 1 Then
                    CheckBox_SetCheck (cbxejec(pista),0)
                    cuantos=cuantos+1
               End If
          Next pista
     End If
End If
DisableGadget(BOTON_SELECCION_EJECUCION,1) 'deshabilita BOTON_SELECCION_EJECUCION
' porque lo deshabilito y habilito escribir porque
''------> BOTON OK DE LINEA DE COMADNO ARRIBA EN VENTANA
If eventnumber()= OK   Then 'boton ok de linea de comando
     comando=GetGadgetText(LINEA_COMANDO)
     Dim  As Integer flag2=1
     If flag2 > 0 And comando= "ENTRE UN COMANDO PULSANDO INICIO " Then
          comando=""
     End If
     
     comando = InputBoxJmg("Entre un Comando ","",comando, ES_MULTILINE + ES_AUTOVSCROLL , flag2,hwndC  )
     comando=UCase(Trim(comando))
     SetGadgetText(LINEA_COMANDO,comando) ' lo muestra
     ' MENU DE COMANDOS por ahora no usare el thread usaremos multikey con comando
     
     ''threadCmd = ThreadCall  ejecutarComando (comando)
End If

If eventnumber()= BTN_METRONOMO And tic=0   Then
     'Print #1,"RESPONDE EL BOTON  M"
     terminar_metronomo=0
     If medio_metronomo_on=FALSE Then
          threadmetronomo = ThreadCall metronomo()
          SetGadgetText (TEXT_GADGET,Str(tiempoPatron))
          tic=1
     End If
 
   If  ParenthWnd > 0 Then ''se levanto un notepad para entrenar
       SetForegroundWindow(ParenthWnd)
   EndIf
Else
     If medio_metronomo_on=FALSE Then
          terminar_metronomo=1
          tic=0
      WindowStartDraw(hwndC,410,770,25,25,1) 
      ImageDraw(Load_image(ROLLDIR+"recur\fondometronomo.bmp"),0,0)
      StopDraw

     End If
End If
If eventnumber()= BTN_MAS Then
     tiempoPatron=tiempoPatron+1
     SetGadgetText (TEXT_GADGET,Str(tiempoPatron))
End If
If eventnumber()= BTN_MENOS Then
     tiempoPatron=tiempoPatron-1
     SetGadgetText (TEXT_GADGET,Str(tiempoPatron))
End If

If eventnumber()= BTN_MAS_RETARDO_M Then
     If CANCIONCARGADA=TRUE  Then
          retrasoMetronomoCan=retrasoMetronomoCan+1
          retrasoMetronomo=retrasoMetronomoCan
     End If
     If ROLLCARGADO=TRUE Or TRACKCARGADO=TRUE Or medio_metronomo_on=TRUE Then
          retrasoMetronomoRoll=retrasoMetronomoRoll+1
          retrasoMetronomo=retrasoMetronomoRoll
     End If
     SetGadgetText(TEXT_METRONOMO_RETARDO,"Retraso M "+Str(retrasoMetronomo))
End If

If eventnumber()= BTN_MENOS_RETARDO_M Then
     If CANCIONCARGADA=TRUE Then
          retrasoMetronomoCan=retrasoMetronomoCan-1
          retrasoMetronomo=retrasoMetronomoCan
     End If
     If ROLLCARGADO=TRUE Or TRACKCARGADO=TRUE Or medio_metronomo_on=TRUE Then
          retrasoMetronomoRoll=retrasoMetronomoRoll-1
          retrasoMetronomo=retrasoMetronomoRoll
     End If
     SetGadgetText(TEXT_METRONOMO_RETARDO,"Retraso M "+Str(retrasoMetronomo))
End If
'---------------------------------------------------------------
If eventnumber()= BTN_MAS_METRO_VOL_IZQ Then
     ' tic=1
     'terminar_metronomo=0
     velMetronomoIzq=velMetronomoIzq+5
     If velMetronomoIzq > 100 Then
          velMetronomoIzq=100
     End If
     '''VolIzq=convA5cifras(velMetronomoIzq)
     VolIzq100=Str(velMetronomoIzq)
     SetGadgetText(TEXT_METRO_VOL_IZQ,"VolM Izq "+ VolIzq100)
     volhizq =  velMetronomoIzq*65535/100
     volumenTotal = (CULng(volhDer) Shl 16) Or volhIzq
End If
If eventnumber()= BTN_MENOS_METRO_VOL_IZQ Then
     ' tic=1
     ' terminar_metronomo=0
     velMetronomoIzq=velMetronomoIzq-5
     If velMetronomoIzq < 0 Then
          velMetronomoIzq=0
     End If
     ''VolIzq=convA5cifras(velMetronomoIzq)
     VolIzq100=Str(velMetronomoIzq)
     SetGadgetText(TEXT_METRO_VOL_IZQ,"VolM Izq "+ VolIzq100)
     volhizq =  velMetronomoIzq*65535/100
     volumenTotal = (CULng(volhDer) Shl 16) Or volhIzq
End If
'------------------------------------
If eventnumber()= BTN_MAS_METRO_VOL_DER Then
     '  tic=1
     ' terminar_metronomo=0
     velMetronomoDer=velMetronomoDer+5
     If velMetronomoDer > 100 Then
          velMetronomoDer=100
     End If
     ''VolDer=convA5cifras(velMetronomoDer)
     VolDer100=Str(velMetronomoDer)
     SetGadgetText(TEXT_METRO_VOL_DER,"VolM Der "+ VolDer100)
     volhder =  velMetronomoDer*65535/100
     volumenTotal = (CULng(volhDer) Shl 16) Or volhIzq
     
End If
If eventnumber()= BTN_MENOS_METRO_VOL_DER Then
     '  tic=1
     '  terminar_metronomo=0
     velMetronomoDer=velMetronomoDer-5
     If velMetronomoDer < 0 Then ' si se baja menos de 10 se conmutan los volumenes y se pone fuerte este y el otro despacio glup bueno queda asi me cansó
          velMetronomoDer=0
     End If
     VolDer100=Str(velMetronomoDer)
     SetGadgetText(TEXT_METRO_VOL_DER,"VolM Der "+ VolDer100)
     volhder =  velMetronomoDer*65535/100
     volumenTotal = (CULng(volhDer) Shl 16) Or volhIzq
     
End If

If eventnumber()= BTN_MAS_VOL_EJEC Then
   datoEjec=datoEjec+5
   If datoEjec > 127 Then
      datoEjec=127
   EndIf
     VolEJEC127=Str(CInt(datoEjec))

     SetGadgetText(TEXT_VOL_EJEC,"Vol EJEC "+ VolEJEC127)
     
End If

If eventnumber()= BTN_MENOS_VOL_EJEC Then
   datoEjec=datoEjec-5
   If datoEjec < 0 Then
      datoEjec=0
   EndIf
     VolEJEC127=Str(CInt(datoEjec))
     SetGadgetText(TEXT_VOL_EJEC,"Vol EJEC "+ VolEJEC127)
     
End If

'-------------------------------------------------------
If EVENTNUMBER()= BTN_PANIC Then
startcall=1
GrabarEjec=HabilitaGrabar
 terminar_metronomo=SI
CTRL10901()
EndIf
