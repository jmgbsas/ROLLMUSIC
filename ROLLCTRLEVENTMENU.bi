            If NombreCancion > "" And S5=0 Then 
               SetForegroundWindow(hwndC)
            EndIf
         Select Case EventNumber
            'CON ROLL , SIN ROLL
           Case 1006, 10061   '<=========== CARGAR CANCION con roll, o sin Roll
             'cargamos todos los tracks
             ' ok anda bien, una vez cagados se permuta en memoria con TAB
             ' o haciedno click en la lista
            '' UseGadgetList(hwndC)
         
              CTRL100610061 (hMessages , Tope )
             
             If abrirRoll=0 And NombreCancion > ""  Then
                abrirRoll=1
                cargaCancion=1
                Print #1,"SALE A CARGAR ROLL POR 1ERA VEZ ABRIRROLL=1 EXIT DO"
                Exit Do                 
             EndIf
  '           Print #1,"termino 1006 va a abrir Roll"
          SetForegroundWindow(hwnd)


           Case 10062
' LO ABRE ACA PERO ES DEPENDIENTE DE LA VENTANA  DE CONTROL 
' ESTA NO SE PUEDE CERRAR EL USUARIO LO DEBE SABER
' CADA PISTA SE PODRA LEVANTAR UNA POR UNA CON LA OTRAOPCION
' SOLO DEBO PASAR LOS PARAMETROS...Y SI MODIFICO ALGO
' DEBO GRABAR A DISCO Y ENVIAR ORFEN DE RECARGA DE ESA PISTA EN LA CANCION
   Print #1," CASE 10062 abrirRoll=0 And NombreCancion > ", abrirRoll, NombreCancion

           CTRL1062 (hmessages )

           Case 10063 ' CARGAR CANCION EN UN ROLL SIN VENTANA DE CONTROL
' HAY QUE PASA EL NOMBRE DEL DIRECTORIO NADA MAS,,,Y EL PATH
' era la 1063 antigua
            CTRL1063 ()             
' ----------------------------------------------------------------------
           Case 1007 '<============ grabar cancion bosquejo
' 26-02-2022 desarrollo debo probar el codigo echo hace mucho 
' no se si anda bien          
'DESHABILITADO CREO ESTA HACIENDO LIO PROBAR Y SEGUIR
' MAS ADELANTE USANDO UNA COPIA DE CANCION PAR NO DESTRUIR LA ACTUAL
' voy a incorporar cargar un midi asi tengo una cancion para probar,,

           CTRL1007 ()

          SetForegroundWindow(hwnd)
           Case 10075 '<======== CARGAR UNA PISTA A ROLL PARA EXPORAR A MIDI
' DE ESTE MODO PODEMOS ENVIAR EL NOMBRE DE LA PISTA AL ROLL AISLADO
' Y CONVERTIR A MID CON EL NOMBRE REAL Y N OEL FANTASIA ARCHIVO.MID

          CTRL10075 ( ) 
          
        Shell (" start RollMusic.exe "+ nombre)
          SetForegroundWindow(hwnd)

           Case 1008 '<======= 3.1 Exportar Pista a midi 
 
               '' CTRL10075 ()
 ' getfiles cambiado gracias a Rusia! ja.. intentaremos hacerlo no tan manual 
              
'  con usarmarcoins=4 indicamos habilitar ESCRITURA MIDI EN EL PLAY
                
           ' nombre , hasta, titu, instru ,pid1, usarmarco, nombrecancion
            Print #1,"Nombre roll a midi ", nombre

               usarmarcoins=4            
            Shell (" start RollMusic.exe "+ Str(desde) +" "+ Str(hasta) +  _
            " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " + _ 
            Str(pid1) + " "+ Str(usarmarcoins) )
             SetStateMenu(hmessages,1009,0)
             SetStateMenu(hmessages,1008,1)
             SetStateMenu(hmessages,10081,1)
           Case 10081
               usarmarcoins=4
                CTRL1063 
             SetStateMenu(hmessages,1009,0)
             SetStateMenu(hmessages,1008,1)
             SetStateMenu(hmessages,10081,1)
            
' ---------------------------------------------------------------           
           Case 1009 '<======= 3.2 Exportar Cancion a midi

                SetStateMenu(hmessages,1008,0)
                SetStateMenu(hmessages,10081,0)
                SetStateMenu(hmessages,1009,1)
  
               Print #1, "nombres 1 y 2 "; "secuenciaPLAY.txt archivo.mid" 
               'Dim result As Integer
                result = midiconv("secuenciaPLAY.txt", "archivo.mid")

                If result = -1 Then

                   Print #1, "error conv a  archivo.mid"  
                Else
                   Print #1, "ok conv a  archivo.mid "  
                End If
               Print #1, "Exit code midiconv: "; result


           Case 1010 '<================ Cargar Pista externa a cancion

   '        Print #1,"entro a 1010 Cargar Pista externa a cancion"

           CTRL1010 (salida )
           If salida =1 Then 
              salida=0
              Exit Select
           End If   
          
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------            
           Case 1011 ' <======= Grabar una Pista de la Cancion con modificaciones, que son tracks
    '        print #1,"entro a 1011 esto lo hace menu de Roll tambien" '' jmg probar es nuevo...
 ' copiamos logica Rolla Track 
   '         print #1, "Click Grabando a disco pista modif con GrabarRollaTrack ",nombre
            Dim As String nombreg
            ROLLCARGADO=FALSE 
           If NombreCancion > ""  Then
              GrabarRollaTrack(0)
           EndIf
          MenuNew=0           
          carga=1
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1012 ' <====== Grabar Pista Como, Copia una pista a otra  nueva nueva
   '        print #1,"entro a 1012 Grabar Pista Como, Copia una pista a otra  nueva nueva"
             
             CTRL1012 (SALIDA)

             If SALIDA=1 Then
                 salida=0
                 Exit Select
             End If 
'-----------------------------------------------------------------------
           Case 1014  ' <============= TRACK A ROLL
           TrackaRoll (Track(), ntk , Roll ) ' no usa ubirtk
           GrabarArchivo(0)
           SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1015 '<========== Grabar MIDI-In aca sera para grabar 
 ' EN ejecuciones, CON CANCION CARGADA NO GRABA NADA, la grabacion se hace en STOP SIN CANCION
Print 1,"GRABA MIDI IN EN CASE 1015  "
'--------------------------
' preparamos para grabar la pista por cambio de patch
           CTRL1015 ()
'-----------------------------------------------------------------------
           Case 1016 '<============Cargar MIDI-IN
' deberia borrar la cancion cargada ??? creo que si totdavia no ejecuta
' ambas cosas a la vez,,,,,debo almenos poner dos opciones mas
' borrar cancion cargada y borrar ejecucion cargada
' LA GLOBAL HIJA DE PUTA nombreMidiIn SE BORRA AL REGRESAR DE CTRL1016
' FREEBASIC ES UNA MIERDA JAJAJAJA!!!!!!!!!!!!!!!!!!!!!!!
' DEBO USAR UNA VARIABLE AUXILIAR MAS AL PEDO NI MIERDA ES GLOBAL
           Dim lugar As string 
           CTRL1016 (lugar)
 nombreMidiIn = lugar

'-----------------------------------------------------------------------
           Case 1017 'renombrar pista ejecucion y borrado
           Dim As String nomPista   
           For i1=1 To 32
               If CheckBox_GetCheck (cbxejec(i1)) =1 Then
                  nomPista  = InputBox("Nombre de Pista " ,"Entre un nuevo Nombre ",nomPista)
                  tocaparam(i1).nombre=nompista
'aca falta que si nompista es "" borrar la pista y mover todo hacia arriba
' si la pista estaba en el medio,,,FALTA
                 If Len (nompista) > 0 Then
                  SetListBoxItemText(LISTA_DE_EJECUCIONES,nompista,i1-1)
                 End If

                 If Len (nompista) = 0 Then
                  'borrar pista y comprimir lista de ejecs si quedo un hueco..
DeleteListBoxItem(LISTA_DE_EJECUCIONES,GetItemListBox(LISTA_DE_EJECUCIONES))
                   comprimirListaEjecs() ' a implementar
                 EndIf  
                   Exit for    
               EndIf
           Next i1  
'-----------------------------------------------------------------------
           Case 1018 '  cargar archivo midi plano con fracturacion 
       If  abrirRollCargaMidi =0 Then
           CANCIONCARGADA=False
           ''cargaCancion=0  
           param.encancion=0 
           EstaBarriendoPenta=1
           threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
       End if
       ''RollLoop ( param)
       abrirRollCargaMidi=1 'solo una vez levanta roll grafico
       Sleep 100
         nombre="" 
         Dim As Integer confrac=1 ' con fracturacion
         cargarMidiPlano (confrac)
         repro=0  
        SetForegroundWindow(hwnd)
           
             Exit Select  
'-----------------------------------------------------------------------
           Case 10181 '  cargar archivo midi plano sin fracturacion 
       If  abrirRollCargaMidi =0 Then
           CANCIONCARGADA=False
           ''cargaCancion=0  
           param.encancion=0 
           EstaBarriendoPenta=1
           threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
       End if
       ''RollLoop ( param)
       abrirRollCargaMidi=1 'solo una vez levanta roll grafico
       Sleep 100
         nombre="" 
         Dim As Integer confrac=0 ' sin fracturacion
         cargarMidiPlano (confrac)
         repro=0  
        SetForegroundWindow(hwnd)
           
             Exit Select  
'-----------------------------------------------------------------------

           Case 1019  ''<============= SALIR TERMINA ROLL
             'eventM=eventrbdown
             eventM=eventClose
  ' no funciona si hay loop en listapista por popup menu ,,no hay caso
  ' ni poniendo end 0.. nada de nada ni enga�ando forzando eventC a close ni 
  ' eventM a EVENTRBdown nada de nada, no pasa nunca por aca solo obedece
  ' a un eventClose en EventC, el cual hay que pulsar dos veces para este caso
            terminar=1
            Exit Do ,Do    
'-----------------------------------------------------------------------
           Case 1020 ' <=========== Entrar Nombre o T�tulo de la Cancion     
               NombreCancion = ""
               pathdir=""
               EntrarNombreCancion(NombreCancion)
      '    SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1021 ' <=========== Entar Tempo 
             menuOldStr="[TEMPO]"
             nombreArchivo="0"
               thread3= ThreadCall EntrarTeclado()
      '        SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1025 ' <======== Crear un directorio de Cancion con Pistas separadas
               CrearDirCancion (NombreCancion)
               If NombreCancion > "" Then
                  param.encancion=1
               EndIf    
    '      SetForegroundWindow(hwnd)        
'-----------------------------------------------------------------------
           Case 1028 ' <========== seleccion octavas menores a 1 9 
               seloctava (desde, hasta)
               *po = hasta -1
                posn=1
                Nuevo (Roll,1 )
                param.ubiroll=ubiroll
                param.ubirtk=ubirtk

                posn=0
     '     SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1031 ' <========  SELECCION DE CANAL DE LA PISTA (10 DRUMS)
'-----------------------------------------------------------------------               
           Case 1040 ' <========== seleccion de instrumento por orden Alfabetico
     '' este da problemas lo tuve que traaer de nuevo ojo
             ''   CTRL1040
                 selInstORdenAlfa (instru)
                If CANCIONCARGADA =TRUE  Then
               Else
                  'midisal = midiout(portout)
                  ntk=0
                EndIf
                portsal=pmTk(ntk).portout
                pmTk(ntk).patch=CUByte(instru)
 ' los canales van de 0 a 15 (1 a 16) no se si en todos los dispositivos
 ' van de 0 a 15 o en alguno de 1 a 16 opto por 0 a 15                
             ''  If pmTk(ntk).canalsalida > 0 Then
            '      ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de neuvo 13-02-2022 �
             ''  EndIf
               If instru=0 Then instru=1 EndIf

                Roll.trk(1,NA).inst= CUByte(instru)
                Track(ntk).trk(1,1).nnn=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg
              If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
                 If (NombreCancion > ""  Or TRACKCARGADO =TRUE) And MAxPos > 2 Then
                   Print #1,"VOY A GrabarRollaTrack(0) DESDE CTRL1040"
                    GrabarRollaTrack(0)
                   Sleep 100 
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 Sleep 100
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  


              MenuNew=0           
              carga=1

        '     SetForegroundWindow(hwnd)    
'-----------------------------------------------------------------------
           Case 1050 ' <=========== seleccion de instrumento por orden Numerico

              ''  CTRL1050
               selInstORdenNum (instru)
               If CANCIONCARGADA =TRUE Then
               Else
                 ' midisal = midiout(portout)
                 ntk=0
               EndIf
             ' no se cuadno funciona esto  si midisal y canal tienen valores 
             ' la seleccion de instrumento se ahce tanto par auna pista aislada como no
               portsal=pmTk(ntk).portout
      '         If pmTk(ntk).canalsalida > 0 Then
          '         ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de nuevo
       '        EndIf

               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).nnn =CUByte(instru)
              ' grabar el track 
   '         print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg

              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos >2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  

              MenuNew=0           
              carga=1
  
        '      SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1060 ' <========== crea track y reemplaza al existente en la edicion
                CTRL1060 salida
                If salida = 1 Then 
                   salida=0
                   Exit Do
                End If
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1061 ' <====== crear pista en cancion con lo elegido

               CTRL1061 (SALIDA)
               If SALIDA = 1 Then
                  salida=0
                  Exit Select  
               EndIf
               
               Exit Do                 
               
' FALTA CREAR LA PISTA !!! jmg ERO PUEDO USAR UNA PISTA YA CREADA EN 1011
' la graba igual desde roll parece pero debe ser en orden
       '   SetForegroundWindow(hwnd)            
'-----------------------------------------------------------------------
           Case 1062 ' <======== crear instancia independiente sin control
 ' ponerle diferente color y/o tama�o para poder distinguirlo adma sde l nombre
 ' estudiar si puedo hacer IPC entre Menus de GUI pero son loop tambien no creo.
             '''Print #fa1,pd1  
     
             Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))
      '    SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
' aca debo tomar de la seleccion del usuario con ctrl+p por ejemplo sobre
' una pista y tomar los parametros de la cancion cargada de esa pista
' y enviarla a un grafico para que la vea o toda la cancion !!! 
'-----------------------------------------------------------------------
' //////////////////////  P_A_T_R_O_N_E_S   ////////////////
'-----------------------------------------------------------------------

           Case 1064 ' <========= Nombre del PatrOn
         Dim As String patronPorOmision
         patronPorOmision=nombrePatron 
         nombrePatron = InputBox("Nombre del Patron Nuevo" ,"Entre un Nombre ",patronPorOmision)
         
'-----------------------------------------------------------------------
           Case 1065 ' <========== numero de compases del patron
         Dim As String nroCompasesPatronOmision
         nroCompasesPatronOmision=Str(nroCompasesPatron) 
         nroCompasesPatron = CInt(InputBox("Numero de Compases del Patron" ,"Entre un Numero ",nroCompasesPatronOmision))

'-----------------------------------------------------------------------

           Case 1066 '<======== SECUENCIA DE PATRONES DE EJECUCION
           CrearSecuenciaPatrones()
'-----------------------------------------------------------------------
           Case 1068
               CTRL1068 (hmessages)

           Case 1070
               CTRL1070(hmessages)

              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1071
               CTRL1071(hmessages)

          SetForegroundWindow(hwnd)
            Case 1074 ''<== Parametros de Roll y Track(0) en memoria

'-----------------------------------------------------------------------
           Case 1080 ' tempo
              nombreArchivo="0"
              menuOldStr="[TEMPO]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1081 ' factor de multi tempo
              nombreArchivo="0"
              menuOldStr="[FACTOR]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
'-----------------------------------------------------------------------
           Case 1090 ' Reproducir cancion
SetGadgetstate(BTN_ROLL_PARAR, BTN_LIBERADO)
         If CPlay = 0 And MaxPos > 2 Then
            CPlay=1
            If NombreCancion > "" Then
               If play=1 Or playb=1 Then
                  CONTROL1=1 ' DETIENE EL PLAY 
                  playloop=0:playloop2=0
                  play=0 : playb=0
                  Sleep 20
               EndIf 
               thread1 = ThreadCall  playCancion(Track())
            EndIf
         EndIf   
          '    Dim As Any Ptr thplayC = ThreadCall  playCancion(track())
          '    CONTROL1 = 1
 
             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1091 ' <=========== Repeticiones de un nro de compases
             If pasoZona1 > 0 And pasoZona2 > 0  Then
                menuOldStr="[NROREP]"
                nombreArchivo="0"
                thread3= ThreadCall EntrarTeclado()
             Else
               MessBox ("Repeticiones", "Debe entrar una zona de campases, Ctrl-clik en comienzo y Ctrl-click final")
             EndIf
             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1092 ' abrir un midi-in ...con callback
' no depende del numero de pista de ejecucion,sino del portin solamente,,,
' es para tocar en un teclado midi y poder escuchar o grabar
'Reproducir MIDI-IN (teclado) por  MIDI-OUT. 
             CTRL1092 ()

'----------------------------------------------------
           Case 1093
'Detener Reproduccion MIDI-IN (teclado) por  MIDI-OUT. (test de Input) 
             abrirMIDIin=2
           For  i As Short =1 To 32
               If CheckBox_GetCheck( cbxgrab(i))= 1  Then
                   cancel_callback(midiin(pmTk(i+32).portin )) ' porque lso port fisicos empiezan desde cero
                   listinAbierto( pmTk(i+32).portin) = 0
                  teclado=0
              EndIf
           Next i 

'-----------------------------------------------------------------------
           Case 1100 '<======== usar o no, marco de ventana de Roll
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarco
             usarmarco=GetStateMenu (hMessages,1100)
             Select Case usarmarco
               Case 0             
                  SetStateMenu(hMessages,1100,3)
                  usarmarco=3
               Case 3
                  SetStateMenu(hMessages,1100,0)
                  usarmarco=0
             End Select
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1101 ' marco o no marco par ainstancias
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarcoins
             usarmarcoins=GetStateMenu (hMessages,1101)
             Select Case usarmarcoins
               Case 0             
                  SetStateMenu(hMessages,1101,3)
                  usarmarcoins=3
               Case 3
                  SetStateMenu(hMessages,1101,0)
                  usarmarcoins=0
             End Select
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1102
                 usarAcordesIguales=1
                 TipoFrac="igualdur" 
                 SetStateMenu(hmessages,1102,3)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1103
                 usarAcordesIguales=1
                 TipoFrac="tododur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,3)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1104
                 usarAcordesIguales=1
                 TipoFrac="autodur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,3)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1105
                 usarAcordesIguales=0
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,3)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1106 ' <======== escala de la secuencia, similar a la de instrumentos
               pasozona1=0
               selTipoEscala (tipoescala_num_ini)

' GRABADO EN grabaPos(1,1).inst = CUByte(tipoescala) ' 20-12-2021 - tipoescala en uso
' CUADNO QUEIRO UN CAMBIO PUEDO DEJAR UN ACOLUMNA VACIA Y PONER TODO ESTA INFO
' PERO DEBO INDICAR AL PROGRAM QUE SALTEE ESTA COLUMNA CREO CON TENER NOTA=181 Y DUR181
' PODRI AINDICAR ESO DEBO PROBARLO Y USAR LSO DEMAS CAMPOS PARA INTRODUCIR ALGUN  CAMBIO               
'''            Roll.trk(1,NA).vol= CUByte(tipoescala + 127) ' a partir de 128
''               Print #1,"Roll.trk(1,NA).vol ",Roll.trk(1,NA).vol
''               END
''               Track(ntk).trk(1,1).vol=CUByte(tipoescala + 127)
              ' grabar el track 
'' NOTA: LA VARIABLES DE ESCALA DE TODA LA SECUENCIA TIENEN SUBFIJOS _STR O _NUM
'' LAS QUE SON PARA USO DE ESCLAS EN POSICIONES NO LO TIENEN
      '      Print #1,"tipo de escala seleccionado ", tipoescala_num_ini
              
' -------cadena de escala, construye dsde C hay que hacer las otras esclas
    ' C,D,E,F,G,A,B,Bb,Ab,Gb ver las debo pedir escala y 1er nota desde donde empieza uff
      '        Print #1,"armarescla desde 1106"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
' --------------------------   
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1107 ' <======== usamos sostenidos o bemoles ???
              pasozona1=0
              selNotaEscala (notaescala_num_ini)
 
       '       Print #1, "seleccion de Nota de la escala num  ",notaescala_num
       '       Print #1,"armarescla desde 1107"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)

            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1108 '<======= alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="sos" ' grabado en grabaLim(1,1).pan  = CUByte(3)
              SetStateMenu(hmessages,1108,3)  
              SetStateMenu(hmessages,1109,0)
            ' si hay nombre de archivo grabar sino no   
      ''        GrabarArchivo()
       '       Print #1,"armarescla desde 1108"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
          
            SetForegroundWindow(hwnd)
' --------------------------   
           Case 1109 ' <======== alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="bem" ' grabado en grabaLim(1,1).pan  = CUByte(2)
              SetStateMenu(hmessages,1108,0)  
              SetStateMenu(hmessages,1109,3) 
       '       Print #1,"armarescla desde 1109"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
      
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1110
   
             MessBox ("", acercade)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1111 '<========== cambiode escala
              CTRL1111 ()
 
            SetForegroundWindow(hwnd)             
'-----------------------------------------------------------------------
           Case 1112 '<========= cambiode a escala Alternativa de la Principal
              CTRL1112() 
            SetForegroundWindow(hwnd)  

           Case 1113 ' usar metronomo
                
             metronomo_si=GetStateMenu(hmessages,1113)
              Select Case metronomo_si 
                     Case  1 
                    metronomo_si=0
                    SetStateMenu(hmessages,1113,0)
                     Case 0
                    metronomo_si=1
                    SetStateMenu(hmessages,1113,1)

              End Select
              SetForegroundWindow(hwnd)

           Case 1200 'Seleccionar  Puertos MIDI-IN SOLO PARA PORTS DE EJECUCION POR AHORA
' seleccion de portin , 2:portin. ntkp:salida
' ->  npi: numero port entrada DIFERENCIA PARA ABAJO
' portsin es la cantidad de ports que hay de entrada
' seleccionamos un port de entrada para cada track de ejecucion
' como pensamos por ahora que hay un solo usuario ejecutando ese port se usara para todos 
' los tracks pero probarmos usar distintos solo que no tengo 2 port sanos para probar en este momento 
' el port de la tarjeta no anda y estoy entrando por USB midi cable 
' portin  'GLOBAL ULTIMO PUERTO IN SELECCIONADO, por omision el cero
' si solo hay ejecuciones .....
           CTRL1200 ()
 
      
'           Case 1201 'Abrir      Puertos MIDI-IN
'              listinAbierto(npi)=1
'           Case 1202'Cerrar    Puertos MIDI-IN
           Case 1203 'DesTruir Puertos MIDI-IN

           Case 1204 'Seleccionar      Puertos MIDI-OUT PARA EJECUCIONES
' seleccion de portout , 1:portout. ntkp:salida
' ->  npo: numero port salida
' portsin es la cantidad de ports que hay de entrada
            CTRL1204 ()
 

           Case 1205'Abrir      Puertos MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES
  
           CTRL1205 ()
  

'-------------------------------

           Case 1206 'Cerrar    Puertos MIDI-OUT de ejecucion play por el usuario
               CTRL1206()

           Case 1207'DesTruir Puertos MIDI-OUT

           Case 2000
   
             MessBox ("", acercade)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 2001 ' cuadro ayuda tempo 
            Dim As integer eventvel
            Dim As HWND velimg

            velimg=OpenWindow("Cuadro de Tempos Clasicos ",400,100,800,400)
            ImageGadget(IMAGE_VEL,10,10,1100,600,Load_image(".\recur\velocidades.jpg"))
            Do  
             eventvel=WaitEvent()
              If eventvel=EventClose  Then
                 Close_Window(velimg)  
                   Exit Do
             EndIf 
            Loop  

          SetForegroundWindow(hwnd)
'---------------------------------------------------------------------
          Case 2002  'MUESTRA FIGURAS DISPONIBLES
          Dim As integer eventFig
           Dim As HWND Figimg

            Figimg=  OpenWindow("Duraciones de Figuras y sus Teclas",400,100,800,600  )
            ImageGadget(IMAGE_FIG,10,10,1100,800,Load_image(".\recur\FIGURAS.jpg"))
           
 
            Do  
             eventFig=WaitEvent()
              If eventFig=EventClose  Then
                  Close_Window(Figimg)  
                   Exit Do
              EndIf 
            Loop  

          SetForegroundWindow(hwnd)
                   
         End Select

' ---------------------------------------------
