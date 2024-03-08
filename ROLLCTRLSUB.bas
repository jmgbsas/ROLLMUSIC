Declare sub  RollLoop (ByRef param As pasa)


Sub CTRL100610061 (hMessages As hmenu , Tope As integer)

               If EventNumber = 10061 Then
   ' habiliatmos 2.1 y 2.2 
                   SetStateMenu(hmessages,10062,0)
                   SetStateMenu(hmessages,10063,0)
    'si uso dos veces 10061 no funciona la segunda no carga nada,,, 
    ' compruebo si roll dependiente esta funcionando 
                     If  EstaBarriendoPenta=0 Then
                         ' no funciona dejo habilitado 10061 carga de roll grafico
                         ' dependiente de control..sol ose puede cargar una vez
                     Else
      'funciona debo deshabilitar CARGA DE OTRO ROLL GRAFICO DEPENDIENTE
      ' SEGUIRA USANDO EL MISMO  
                         SetStateMenu(hmessages,10062,1) 
                     EndIf
               EndIf 
               nombre=""
              ROLLCARGADO=FALSE 'NINGUN ARCHIVO ROLL CARGADO  
             Sleep 20
' resetea todo para limpiar y cargar cancion
             If NombreCancion > "" And cargaCancion=0 Then 
                  NombreCancion = ""
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  cargarDirectorioCancion(NombreCancion)
                  CANCIONCARGADA=False
                  ntk=0
                  If Tope >0 Then ' tenia datos se supone q pudo abrir Roll y abrirRoll=0
                     CargarPistasEnCancion ()
                     If tope=0 Then  ' directorio fallido
                        NombreCancion = ""
                        cargacancion=0
                        param.encancion=0
                        abrirRoll=2 ' roll ya esta abierto abre mas abajo
                     Else
                        cargacancion=1
                        param.encancion=1
                        abrirRoll=3 ' para evitar que abra rolloop de nuevo
                     EndIf

                  Else
                  ' si Tope=0 no cargo nada ni abrio roll posiblemente
                     NombreCancion = "" ' serai como una carga inicial...  
                     cargaCancion=1
                     abrirRoll=0 ' para que abra rolloop nunca abrio tal vez
                  EndIf   

             EndIf
             If NombreCancion = "" Then
                nombre=""
                ntk=0
               ' pistacreada=0
                CANCIONCARGADA=FALSE
' toma solo el nombre y path de la cancion no carga las pistas todavia
                cargarDirectorioCancion(NombreCancion)
                param.encancion=1
               If abrirRoll=2 Then ' ver rollloop roll esta cargado vengo a cargar cancion de nuevo
               ' por ejemplo tenia solo un roll abierto
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  CargarPistasEnCancion ()
                  If tope=0 Then
                    NombreCancion = "" ' directorio fallido
                  EndIf
                  CANCIONCARGADA=True
                  param.encancion=1

               EndIf
             EndIf
             


End Sub

Sub CTRL1062 (hmessages As hmenu)

             If NombreCancion > ""  Then
                EstaBarriendoPenta=1 
                threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))   
                Print #1,"CARGO ROLL PARA cancion sin roll"
            ' ES TAN RAPIDO QUE PARECE EJECUTA DOS VECES EL 10062
            ' AL DEBUGUEAR NO LOA HACE ERGO PONEMOS UN RETARDO 0,1 SEG
                Sleep 100        
             EndIf
 'No se puede cargar otro roll dependiente ,,,
            SetStateMenu(hmessages,10062,1)
' pero deberia poder cargar otor directorio sin roll??
            SetStateMenu(hmessages,10061,0)

End Sub 

Sub CTRL1063() 

            If NombreCancion > "" Then
                 
               Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + _ 
                                     " Track_"+Str(desde)+"_"+Str(hasta) + " "+ _
              Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins) + " " + _ 
              Str (NombreCancion))   ' @JMG
                Print #1,"Cargo Cancion en un roll grafico externo" 
              
                 
            End If



End Sub

Sub CTRL1007()
       If CANCIONCARGADA =True  Then
           Dim As String nombreg
           ROLLCARGADO=False 
          If NombreCancion > ""  Then
             GrabarCancion()
          EndIf
          MenuNew=0           
          carga=1
              
       EndIf


End Sub

Sub CTRL1010(ByRef salida As INTEGER)
           ROLLCARGADO=False 
            Dim As String nombreg
            
            getfiles(file,myfilter,"save")
            nombreg=*file.lpstrFile
            If nombreg = "" Then
               Print #1,"exit select por nombreg vacio "
               salida=1 
               Exit Sub
            Else
               nombre=nombreg   
            EndIf
            If NombreCancion > ""  Then 
               ImportarPistaExterna() ' estoy en cancion importando  una pista rtk
            EndIf   
          MenuNew=0           
          carga=1

End Sub

Sub CTRL1012 (ByRef SALIDA As Integer)
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            If nombre = "" Then
               getfiles(file,myfilter,"save")
               nombreg=*file.lpstrFile
               If nombreg = "" Then
                  print #1,"exit select por nombreg vacio "
                  SALIDA=1 
                  Exit Sub
               Else
                  nombre=nombreg   
               EndIf
           EndIf
           If NombreCancion > ""  Then 
              GrabarCopiadePista() ' estoy en cancion copiando una pista desde otra pista
           EndIf   
          MenuNew=0           
          carga=1


End Sub

Sub CTRL1015 ()
' preparamos para grabar la pista por cambio de patch
           Dim As Integer  pis=0, K=0
           For k=1 To 32 ' pistas ejec de grabaciondesde teclado
             If CheckBox_GetCheck( cbxejec(k))= 1  Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
                Exit For
             EndIf
           Next k

                  reDim  toc.trk(1 To tocaparam(pis).maxpos)

                  Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                  Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                '        Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
                   Next j
 Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal


' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
'threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi)  ' POR 1015
'  ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))


End Sub

Sub CTRL1016 ()

      Dim As String nombrea,myfil
       print #1,"EN Cargar midi-in nombre ",nombreMidiIn
       ResetAllListBox(4)
      Dim As String lugar
       If  NombreCancion= "" Then
lugar= BrowseForFolder( NULL, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, "c:\" )
       Else
          lugar=NombreCancion
       EndIf
          If lugar = "" Then
          Else
''             nombreMidiIn=nombrea
             ''aca hay que hacerun loop
              Print #1,"lugar ";lugar
              'NTKP ES UNA SALIDA DE LA SUB
              CargarPistasEjec lugar, ntkp
  
              Dim j As integer
              For  j=1 To ntkp
                 If  tocaparam(j).nombre > "" Then
'nombre debe estar sin extension,las ejecuciones tienen un orden estricto
' vamos a tenerque igualar la cantidad de ticks en todas las pistas de modo
' que el ordende las pistas sea indistinto,elnumero de la pista ejec esta ensu archivo
'veremos si funciona cualqueira sea el orden en el disco alcargar se ordenara por
' ese numero Toca().orden,si funciona tal vez loaplicariamos a roll (mucho trabajo porahora queda asi) 
 
                   ntoca=j
                   pmTk(j+32).portout=tocaparam(j).portout
                   abrirPortoutEjec(j)
' en una carga abri los ports de salida pero todavia no los de entrada
' ergo el mycalback deberia tomar el patch si lo ajusto aca
'volver
'          ChangeProgram ( tocaparam(j).patch , tocaparam(j).canal, tocaparam(j).portout)
'       Print #1,"1016 tocaparam(j).patch ",tocaparam(j).patch
'       Print #1,"1016 tocaparam(j).canal ",tocaparam(j).canal
'       Print #1,"1016 tocaparam(j).portout ",tocaparam(j).portout
'
'-----------------06-06-2022-- abre portin siempre toca piano no toma el patch!!! no se como 
'      portin= CInt(tocaparam(j).portin)
'      If  listinAbierto( portin) = 0 Then
'              calltoca=j 'para el portout en mycallback
'              open_port (midiin(portin ), portin, *nombrein( portin ) )
'              set_callback midiin(portin ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
'  ignoreTypes(false, false, false);
 '             rtmidi_in_ignore_types  (midiin(portin ), 1, 2, 4)
 '             teclado=1 
 '             listinAbierto( portin) = 1
 '             jgrb=0
 '      End If
'-----------------06-06-2022 fin
               EndIf 
           Next j 

           tocatope=ntkp
      EndIf 


End Sub

Sub CTRL1040 () ' <========== seleccion de instrumento por orden Alfabetico

               selInstORdenAlfa (instru)
                If CANCIONCARGADA =TRUE  Then
               Else
                  'midisal = midiout(portout)
                  ntk=0
                EndIf
                portsal=pmTk(ntk).portout
                
 ' los canales van de 0 a 15 (1 a 16) no se si en todos los dispositivos
 ' van de 0 a 15 o en alguno de 1 a 16 opto por 0 a 15                
             ''  If pmTk(ntk).canalsalida > 0 Then
            '      ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de neuvo 13-02-2022 ï¿½
             ''  EndIf
               If instru=0 Then instru=1 EndIf
                Roll.trk(1,NA).inst= CUByte(instru)
                Track(ntk).trk(1,1).nnn=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg
              If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos > 2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo¿?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  


              MenuNew=0           
              carga=1


End Sub

Sub CTRL1050 () ' <=========== seleccion de instrumento por orden Numerico

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
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  

              MenuNew=0           
              carga=1


End Sub 

Sub CTRL1060 (ByRef SALIDA As INTEGER) ' <========== crea track y reemplaza al existente en la edicion
               'If ntk=0 Then  ' no se cargo ningun track
               '   *po = hasta -1
               '   posn=1
               '   Nuevo(Roll,1 )
               '   posn=0
                  instruOld=instru
                  Roll.trk(1,NA).inst= CUByte(instru)
                  Track(ntk).trk(1,1).nnn= CUByte(instru)
                  'ChangeProgram ( CUByte (instru) , 0)
               'EndIf   
               If abrirRoll=0 Then
Print #1,"1060 abrirRoll=0 entro"
                  abrirRoll=1
                  cargacancion=0
                  If reiniciar=1 Then
                     ThreadDetach(threadloop)
                     usarmarcoOld=usarmarco
                     reiniciar=1
                  EndIf
                  If reiniciar=0 Then
                     reiniciar=1
                     usarmarcoOld=usarmarco
                  EndIf   
                  Print #1,"sale de 1060 abrirrol,reiniciar, cargacancion ", abrirRoll, reiniciar, cargacancion
                  SALIDA =1
               EndIf


End Sub 

Sub CTRL1061 (ByRef SALIDA As INTEGER) ' <====== crear pista en cancion con lo elegido

               ntk = CountItemListBox(3)+ 1
   '            Print #1,"creando Pista nto ",ntk
               If ntk > 32 Then
                  print #1,"exit select ntk> 32"
                   SALIDA=1 ''Exit Select
                   Exit SUB
               EndIf 

               If instru=0 Then 
                  instru=1
               EndIf
   '            print #1,"instru en 1061 , toma el ultimo ",instru
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
   '            Print #1, "NombrePista en 1061 sin nro track ",NombrePista
   '            print #1,"porque se resetea? pathdir",pathdir
               If CANCIONCARGADA=true Or NombreCancion <> "" Then
                 ' armó el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
    '           print #1, "NombrePista en 1061",NombrePista
               AddListBoxItem(3, NombrePista)
               
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
    '           print #1,"nombre en 1061",nombre
               CantTicks=1000
    '           Print #1,"CantTicks ",CantTicks
               
               ''' para cuando las pistas esten juntas en un archivo ->ZGrabarTrack(ntk)
               If ntk=1 Then
                  ReDim (Roll.trk ) (1 To CantTicks,NB To NA)
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               Else
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               EndIf
               ' EL TRACK SE CREA AL GrabarRollaTrack y debe tener CantTicks
               NB => 0 + (desde-1) * 13   
               NA => 11 + (hasta-1) * 13  

               titulos(ntk)=nombre
               pmTk(ntk).desde=desde
               pmTk(ntk).hasta=hasta
               pmTk(ntk).NB=NB
               pmTk(ntk).NA=NA                  
               pmTk(ntk).MaxPos=2
               pmTk(ntk).posn=0
               pmTk(ntk).notaold=0                  
               pmTk(ntk).Ticks=4000
               ' usamos encancion=1 para grabar dentro de la cancion

               NombrePista="" 
               posicion=1
               posn=0
               MaxPos=2
               nota=0
               dur=0
               tope=ntk
               *po = hasta -1
               GrabarTrack(ntk)
               abrirRoll=0
               If ntk=1 Then 
                  abrirRoll=1
                  cargacancion=0
                  CANCIONCARGADA=TRUE
               EndIf
               If ntk>=2 Then
                 cargacancion=1
                 abrirRoll=0
                 CANCIONCARGADA=FALSE
               EndIf

 
End Sub
' --- FALTAN MAS
'''-///////////// FIN  RUTINAS DE VENTANA PRINCIPAL ROLLCONTROL ////////
Sub CTRL1068(hmessages As hmenu)

           HabilitarPatrones=GetStateMenu(hmessages,1068)
              Select Case HabilitarPatrones 
                     Case  3 
                    HabilitarPatrones=0
                    SetStateMenu(hmessages,1068,0)
                    SetStateMenu(hmessages,1064,1)
                    SetStateMenu(hmessages,1065,1)
                    SetStateMenu(hmessages,1066,1)
                    SetGadgetText(21,"        ")
                     Case 0
                    HabilitarPatrones=3
                    SetStateMenu(hmessages,1068,3)
                    SetStateMenu(hmessages,1064,0)
                    SetStateMenu(hmessages,1065,0)
                    SetStateMenu(hmessages,1066,0)

                    SetGadgetText(21,"PATRONES")
              End Select


End Sub

Sub ctrl1070 (hmessages As hmenu)
              nVerEscalasAuxiliares=GetStateMenu(hmessages,1070)
              Select Case nVerEscalasAuxiliares 
                     Case  3 
                    nVerEscalasAuxiliares=0
                    SetStateMenu(hmessages,1070,0)
                     Case 0
                    nVerEscalasAuxiliares=3
                    SetStateMenu(hmessages,1070,3)
              End Select

 
End Sub

Sub CTRL1071(hmessages As hmenu)
              nVerCifradoAcordes=GetStateMenu(hmessages,1071)
              Select Case nVerCifradoAcordes 
                     Case  3 
                    nVerCifradoAcordes=0
                    SetStateMenu(hmessages,1071,0)
                     Case 0
                    nVerCifradoAcordes=3
                    SetStateMenu(hmessages,1071,3)

              End Select


End Sub

Sub CTRL1090 ()
             If Cplay = 0 And MaxPos > 2 Then
                 GrabarPenta=0:naco=0:naco2=0
                 CPlay=1
                 If NombreCancion > "" Then
                    If play=1 Or playb=1 Then
                       CONTROL1=1 ' DETIENE EL PLAY VEREMOS
                       playloop=0:playloop2=0
                       play=0 : playb=0
                       Sleep 20
                    EndIf 
    '                Print #1,"USANDO PLAYCANCION"
                    thread1 = ThreadCall  playCancion(Track())
                 EndIf
              EndIf   

End Sub
Sub CTRL1092()
' abrir un midi-in ...con callback
' no depende del numero de pista de ejecucion,sino del portin solamente,,,
             abrirMIDIin=1
'If abrirMIDIin=1 Then ' grabacion desde teclado ...entr mal los valores por ahora
'   abrirMIDIin=0
'NTK??? vale cero al comienzo ergo es la ultima  ejecucion de trackas ...32+1 sera de ejec
'' çççç SEGUIRACA AJUSTE DEPORTOUT....05-05-2022
 ' deberiamos, 1) seleccionar el portIN de la pista ejec y luego abrir ejecutando esta accion
' se podra usar mycallback mas de 1 vez en distinto port ? supongo que si,.,,,
' SI ya esta abierto el portin no abrirlo por 2da vez...eso requiere controlar la apertura
' de los portin tambien JMGJMGJMG QUEDA PARA DESPUES AHORA VEREMOS APERTURA
' Y CIERRE EN DISPOSITIVOS....
 '----> seguir aca ÇÇÇÇÇÇ debo
   
       For  i As Short =1 To 32
             If  CheckBox_GetCheck( cbxgrab(i))= 1 Then
                portin  = CInt(tocaparam(i).portin) ' PREVIAMENTE SELECCIONADO
                portout= CInt(tocaparam(i).portout) ' PREVIAMENTE SELECCIONADO
                calltoca= i ' 04-06-2022
                ntoca=i
                portsal=portout

              ChangeProgram ( 1 , 1, 0)
                Exit For  ' termina con el 1er seleccionado solo se toma 1 sola accion
             EndIf
       Next i
/' lo bore  de mycallback
        For k As Short =1 To 32 
           If CheckBox_GetCheck( cbxejec(k))= 1  Then
              calltoca=k
Print #1,"DENTRO DE MYCALLBACK k ",k
Print #1,"tocaparam(k).patch ",tocaparam(k).patch
Print #1,"tocaparam(k).canal ",tocaparam(k).canal
Print #1,"tocaparam(k).portout ",tocaparam(k).portout
              ChangeProgram ( tocaparam(k).patch , tocaparam(k).canal, tocaparam(k).portout)
              Exit For
           EndIf
         Next k 
'/
Print #1,"listinAbierto( portin) ",listinAbierto( portin)
Print #1,"listInCreado(portin) ",listInCreado(portin) 
       If  listinAbierto( portin) = 0 Then
              If listInCreado(portin)  = 0 Then
                 midiin(portin) = rtmidi_in_create_default()
                 listInCreado(portin)  =1
             EndIf
Print #1,"abriendo portin y call back",*nombrein( portin )
              open_port (midiin(portin ), portin, *nombrein( portin ) )
              set_callback midiin(portin ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(portin ), 1, 2, 4)
              teclado=1 
              listinAbierto( portin) = 1
              jgrb=0
       End If

End Sub

Sub CTRL1111() '<========== cambiode escala
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                selTipoEscala (tipoescala_num)
                selNotaEscala (notaescala_num) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala_num
                guiaEscala(indEscala).notaescala=notaescala_num
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1111 TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
             EndIf


End Sub

Sub CTRL1112() '<========= cambiode a escala Alternativa de la Principal

  If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
     Dim As String notastr
     'tipoescala_num,notaescala_num se pisan de modo q no hace falta inicializar
     EscalaAlternativa (tipoescala_num,notaescala_num) 
     cambioescala=1
     indEscala=indEscala+1

     guiaEscala(indEscala).tipoescala=tipoescala_num
     guiaEscala(indEscala).notaescala=notaescala_num
     If alteracion="sos" Then
        guiaEscala(indEscala).alteracion=3
     EndIf 
     If alteracion="bem" Then
        guiaEscala(indEscala).alteracion=2
     EndIf 
   Print #1,"1112 Alternativa TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
  EndIf


End Sub

Sub CTRL1200()  ' <== Seleccionar  Puertos MIDI-IN SOLO PARA PORTS DE EJECUCION 
' POR AHORA
' seleccion de portin , 2:portin. ntkp:salida
' ->  npi: numero port entrada DIFERENCIA PARA ABAJO
' portsin es la cantidad de ports que hay de entrada
' seleccionamos un port de entrada para cada track de ejecucion
' como pensamos por ahora que hay un solo usuario ejecutando ese port se usara para todos 
' los tracks pero probarmos usar distintos solo que no tengo 2 port sanos para probar en este momento 
' el port de la tarjeta no anda y estoy entrando por USB midi cable 
' portin  'GLOBAL ULTIMO PUERTO IN SELECCIONADO, por omision el cero
' si solo hay ejecuciones .....
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 ntkp =i +32 
             EndIf
          Next i  
Print #1, "1] case 1200 portin, ntkp  ", portin, ntkp
         selportEjec (2,ntkp ) ' fix 13-03-23 enviamos el track es una seleccion para ese track
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
         Print #1, "2] case 1200 portin, ntkp  ", portin, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portin entrado
' podemos asignar muchos a la vez 
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 pmTk(i+32).portin=CUByte(portin) ' evitamos el cero
             EndIf
          Next i  



End Sub

Sub CTRL1204() 'Seleccionar      Puertos MIDI-OUT PARA EJECUCIONES
' seleccion de portout , 1:portout. ntkp:salida
' ->  npo: numero port salida
' portsin es la cantidad de ports que hay de entrada
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 ntkp =i +32 
             EndIf
          Next i  

         selportEjec (1,ntkp )
         Print #1, "portout, ntkp ",portout, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portin
' podemos asignar muchos a la vez 
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
                 pmTk(i+32).portout=CUByte(portout)
             EndIf
          Next i  


End Sub
Sub CTRL1205 () 'Abrir      Puertos MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES
Print #1,"abriendo port....si no se selecciona previamnete toma cero"

Dim k1 As Integer
 For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
        k1=CInt(pmTk(i+32).portout)
        Print #1,"midiout ",k1, *nombreOut(k1)
        If InStr(*nombreOut(k1),"Microsoft")>0 Then
           Print #1,"No se usa Microsoft"
        Else
           If listoutAbierto( k1) = 0 Then
              If listoutCreado( k1) = 0 Then
                 midiout(k1) = rtmidi_out_create_default ( )
                 listoutCreado( k1) =1
              EndIf
              open_port midiout(k1),k1, nombreOut(k1)
              Dim As integer    porterror=Err
               listoutAbierto( k1) = 1
              Print #1,"1205 abro MIDI-OUT ",*nombreOut(k1)
              porterrorsub(porterror) 
          Else
              Print #1,"PORT YA ABIERTO",*nombreOut(k1)
          EndIf
      EndIf 
      Print #1,"Port usando en Play teclado ",portout
      Print #1,"-------------------------------------"
' el portsal de rtmidi empieza desde cero luego le resto 1
    ChangeProgram ( tocaparam(ntoca).patch, tocaparam(ntoca).canal, tocaparam(ntoca).portout-1)
    
    EndIf
 Next i


End Sub

Sub CTRL1206() 'Cerrar    Puertos MIDI-OUT de ejecucion play por el usuario
For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Then
        Dim k1 As Integer
        k1=pmTk(i+32).portout 
        Print #1,"midiout ",k1, *nombreOut(k1)
        alloff( pmTk(i+32).canalsalida,k1 )  
        listoutAbierto(k1)=0
        close_port midiout(k1)
   EndIf
Next i 


End Sub
'' ///////////////////// GADGET //////////////////////////

Sub CTRL_EVENTGADGET ()
    '   SetForegroundWindow(hwndC)
      ' el codigo anterior que traia de disco esta en notas
' TODOS DICEN RUSO Y USA QUE VK_LBUTTON ES 1 PERO CON 1 NO ANDA
' SIN EMBARGO CON 3 ANDA A VECES..

Dim As Integer k=0
Static As Integer millave

       If eventnumber()=  LISTA_DE_PISTAS Then 
         
         borrapos=0
' MEGUSTO FUNCIONA ASI: DAR CLICK EN UNAPISTA LUEGO CON flecha arriba
' y abajo CAMBIA DE PISTA EN ROLL, parahabilitar el  CLICK DERECHO CONTEXTUAL
' DAR ENTER Y LUEGO CLICK DERECHO APARECE EL MENU CONTEXTIUAL, PARA VOLVER
' AL INICIO DAR CLICK EN OTRAPISTA Y TODO COMIENZ DE NUEVO... 
'  
             
           If MOUSEBUTTONS AND LEFTBUTTON Then 
                
             print #1,"CLICK lbutton EN LISTA WM==============="
             Print #1,"COORDENADAS X, Y ", GlobalMouseX,GlobalMouseY 
             ROLLCARGADO=FALSE
             CANCIONCARGADA=TRUE
             Dim item As String
             Dim As Integer ubi1,ubi2
              
             item=GetListBoxText(3,GetItemListBox(3))
             Print #1,"item 1580 ",item  ' 28-02-2024 esto aparece en debug sin roll
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf

             item=Trim(item)
             Print "item ",item
             If item > "" Then
             '  Dim nombre1 As String
             '   nombre1= NombreCancion + "\"+item +".rtk"
             '   print #1," NUEVO eventgadget click en lista nombre", nombre1
              ubirtk=3 ' ahora indice carga desde lista o memoria
             ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
             ' pero si quiero volver a disco solo debo resetear ubirtk=0
              ntk=sacarNtk(item) ' este ntk no sirve para boorar
 ' aca no copia track a Roll
              Print #1,"ntk de item ", ntk
              nombre= titulos(ntk)

         Print #1,"ntk, nombre ",ntk, nombre
                
              EndIf
              
          EndIf
  
'--------------------------------------------------------------

   clickpista=1 ' no incrementa el ntk que simula SC_TAB, el cual carga el track a Roll
 
'--------------------------------------------------------------

' /// // // / / /  menu contestual popup 
   
            
          If eventnumber()=  LISTA_DE_PISTAS And _
             WM_VKEYTOITEM And  EventKEY = VK_RETURN Then
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
                     If ix < 3 Then   
                        DisableGadget(LISTA_DE_PISTAS,1)
                     EndIf

                     DisplayPopupMenu(hMessages2,,)
                     If ix < 3 Then   
                        DisableGadget(LISTA_DE_PISTAS,0)
                     EndIf
                     Exit Do
                   EndIf
                 EndIf
             
              Loop 
            EndIf
'--------------------------------------------------------------
' este ntk sirve para identificar el ntk del arcchivo t dle vector
' pero el ntk de la lista es otro vector y al borrar el indice cambia
' debo obtener el indice primero                
'' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT
            If WM_VKEYTOITEM Then '
       '           print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
                If EventKEY = VK_DELETE Then 
       '          print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
        '            print #1,"LISTABOX EventKeyDown borrar ntk",ntk
         '           print #1,"LISTBOX  titulos(ntk)= ",titulos(ntk)
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
                                  
         '       Print #1," CLICK EN LISTA FIN "
                  
       EndIf
  


'  CUAL PISTA DE ROLL SE ESCUCHA SEGUN LO SELECCIONADO
       If eventnumber()=CHECK_PISTA_ROLL Then
       
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
            CheckBox_SetCheck(ByVal cbxnum(i), SuenaTodo)
            cntsuena+=SuenaTodo
          Next i
         SuenaTodo=3
       EndIf
'------------------
' revisar CheckBox_GetCheck de las ejecuciones

'//////////////// BOTON ROJO COMIENZO GRABACION EJEC ////////////////// O PATRON
' llamar a un list  port y ajustar portout  
     If eventnumber()= BTN_MIDI_GRABAR And GrabarEjec=NoGrabar Then ' BOTON GRABAR ROJO
         k=0
         jgrb=0:repro=0
         For k=1 To 32 
           If CheckBox_GetCheck( cbxgrab(k))= 1 Then 
              ntoca=k 'ntoca es la  pista ejec que se esta grabando
              calltoca=ntoca 'calltoca se usa en mycallback use otra variable , ver si conviene
              Exit For
           EndIf
         Next k

        
' mil negras a I=60 son 192 * mil ticks (16 minutos a I=60)
' a i=240 todo *4---192*4*1000=768000(16min a I=240)
'la idea es que el usuario grabe a I=60 o I=120 384000
' pero cada nota requiere 2 eventos on y off se multiplicaria por 2

         SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
         GrabarEjec=GrabarPistaEjecucion
         arrancaPlay=0

'metronomo de 4 pulsos para comenzar a grabar
 '    If  GrabarEjec=1 And metronomo_si=1 Then
 '       terminar_metronomo=0
 '       Dim As Integer im=0
 '       For im=1 To 4  
 '           noteon(60,60,1,0)
 '           noteoff(60,1,0)
 '           duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
 '       Next im
 '       threadmetronomo = ThreadCall metronomo()
             '    EndIf



     EndIf ' end event 10

'//////////////// BOTON NEGRO STOP EJEC  , GRABA A DISCO //////////////////

' 
      If eventnumber()= BTN_MIDI_PARAR   Or  GrabarEjec=GrabarPatronaDisco Then ' BOTON STOP NEGRO DE MIDI-IN
         SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
         If GrabarEjec=GrabarPistaEjecucion  Or GrabarEjec=GrabarPatronaDisco Then
            Print #1,"STOP:pmTk(ntoca+32).MaxPos ",pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).orden=CUByte(ntoca)
   '         Print #1,"stop MaxPos ",pmTk(ntoca).MaxPos
            GrabarEjec=NoGrabar
            repro=0
            arrancaPlay=0
' terminar cualquier metrono que este funcionando 
         terminar_metronomo=1
'detiene el play de cancion o roll
If  play=1 Or playb=1 Then
  CONTROL1=1 ' DETIENE EL PLAY DE CANCION O ROLL
   play=0: playb=0 
  playloop=0:playloop2=0
  SetGadgetstate(12,0)
  Sleep 2
EndIf
CONTROL2=1
Sleep 2
' -------cargamos toca
       k=0 
         Dim As Integer i1=1, j =0, partes, pj
   ' tocaparam(ntoca).delta toma valor desde la 2dapista grabada  en PlayTocaAll
         If  tocaparam(ntoca).delta > 0 And ntoca >1 Then
             partes=tocaparam(ntoca).delta/TickChico
             Print #1,"STOP: numero de partes de retardo ",partes 
             k=partes
             pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos+partes
'             Toca(ntoca).maxpos=pmTk(ntoca+32).MaxPos
'             print #1,"STOP Toca(ntoca).maxpos, ntoca ",Toca(ntoca).maxpos,ntoca
             For pj=1 To partes 
               Toca(ntoca).trk(pj).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(pj).nota = 0
               Toca(ntoca).trk(pj).vel  = 0
              Next pj

         EndIf
          k=partes+1
         Do 
           if k=pmTk(ntoca+32).MaxPos+1  Then
              Print #1," k=pmTk(ntoca+32).MaxPos+1, GrabaMidiIn "
              Exit Do
           EndIf  
     '  Print #1,"CargaIn(i1).modo ",CargaIn(i1).modo

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
           '   Print #1,"CargaIn(i1).partes ",CargaIn(i1).partes
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
      print #1,"EN Grabar midi-in nombre ",tocaparam(ntoca).nombre
''        nombreMidiIn=Toca(ntoca).nombre
   'y el path dondeesta? cuadnograbopareceque no 
  ' al cargar si debo afinar eso....   
   ReDim (toc.trk)(1 To tocaparam(ntoca).maxpos)

 
      Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",ntoca   
      Print #1,"tocaparam(ntoca).maxpos),ntoca ",tocaparam(ntoca).maxpos, ntoca
    
       For j As Integer =1 To   tocaparam(ntoca).maxpos
              toc.trk(j).modo=Toca(ntoca).trk(j).modo
              toc.trk(j).nota=Toca(ntoca).trk(j).nota
              toc.trk(j).vel=Toca(ntoca).trk(j).vel
'''''' VER DATOS            Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
       Next j
   Dim tocap As ejecparam = tocaparam(ntoca)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal

 
      maxgrb=tocap.maxpos
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
Print #1,"STOP: llama a GrabarMidiIn,,,porque  no esta grabando ahora? "   
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
GrabarMidiIn(pgmidi) 'POR STOP aca se graba bien el orden
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
            repro=1
            CONTROL2=0
            CONTROL1=0
            Dim p As Integer Ptr
            p=@ntoca 'ntoca se ajusta en CargarPstasEjec tambien
            t1play=Timer
' tocar cancion de trakc si esta cargada 
' son 2 threadas que se inician casi simultanemente pero sin control entre ellos
' por ahora
' ACA DEBERIA USAR MUTEX!!! ï¿½ï¿½ï¿½???
'''Dim As Any Ptr sync =MutexCreate
Print #1,"MaxPos en play verde ejec deberia ser cero si no hay grafico ",MaxPos
        If  MaxPos > 2 Then  ''''' And GrabarEjec=1 And repro=1 Then 
            If CANCIONCARGADA = TRUE And playb=0 Then
               Print #1,"USANDO PLAYCANCION"
               playb=1   
               thread1 = ThreadCall  playCancion(Track())
            Else
               If  MaxPos > 2 And  Play=0 Then
          '        print #1,"llama a playall"
                   Play=1
 Print #1,"Va Play All ????,maxpos  ", MaxPos
                   thread1 = ThreadCall  playAll(Roll)
               EndIf 
            EndIf
       EndIf   
            threadG  = ThreadCreate (@PlayTocaAll, p)
            threadDetach(threadG)
        '  Print #1,"llama a  PlayTocaAll(p)"

         '   PlayTocaAll(p)
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

      If eventnumber()= BTN_ROLL_GRABAR_MIDI Then 
         GrabarPenta=1
      EndIf 
'-------------------------------
      If eventnumber()= BTN_ROLL_PARAR  Then
         SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
         GrabarPenta=0
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
      If eventnumber()= BTN_ROLL_EJECUTAR Then ' 13-02-2024 PROBAR BIEN
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

      EndIf
' ---------------- BOTONES PORTSAL VOL PATCH CANAL A LA DERECHA Y ARRIBA ...
'--------------------------------------------------------------------------------------------
' ////////////// PORT SAL EJEC ////////////////
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' osea el orden se crea al crear el nombre de la pista
      If  eventnumber()=BTN_EJEC_PORTSAL Then ' boton PortSal de track cbxnum o ejec cbxejec
          Dim As Integer miport =1, pis=0,num=0
          For k=1 To 32 
              If CheckBox_GetCheck( cbxejec(k))= 1 Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
             EndIf
          Next k
         If  pis >=1 Then 
      ' Si la pista tiene unnombre y tiene datos de ejecucion
             If  tocaparam(pis).nombre  >""  And  tocaparam(pis).maxpos > 0 Then
                 miport=1   ' 1= VA A seleccion port Salida
                 ntkp=pis
Dim As Integer k1 = pmTk(pis+32).portout
Print #1,"antes del cambio k1, listOutAbierto(k1) ", k1, listOutAbierto(k1)
Print #1,"pmTk(pis+32).portout previo al cambio",pmTk(pis+32).portout
     ''''     thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))

         selportEjec(miport,ntkp)
Print #1,"pmTk(pis+32).portout despues del cambio",pmTk(pis+32).portout

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
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal
    
    ' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje

pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi) ' por PORSAL
  '''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))


             Else ' no hay nombre y/o no hay datos
               miport=1   ' seleccion port Salida sin pista para tocar teclado
               ntkp=pis
               Print #1,"pmTk(pis+32).portout previo al cambio",pmTk(pis+32).portout
   ''              thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))
               selportEjec(miport,ntkp)
               Print #1,"pmTk(pis+32).portout despues del cambio",pmTk(pis+32).portout
 
             EndIf
             Dim k1 As Integer
' buscamos  elport de esta pista
             Print #1,"pmTk(pis).portout cambiado ",pmTk(pis+32).portout
             k1=CInt(pmTk(pis+32).portout)
             Print #1,"k1 portout, listOutAbierto(k1) ", k1, listOutAbierto(k1)
             If listOutAbierto(k1)=0 Then  'abrir port
                If listoutCreado( k1)=0 Then
                    midiout(k1) = rtmidi_out_create_default ( )
                    listoutCreado( k1)=1
                EndIf
                open_port midiout(k1),k1, nombreOut(k1)
                Dim As integer    porterror=Err 
                listoutAbierto( k1) = 1
               Print #1,"abro ",*nombreOut(k1)
                porterrorsub(porterror)
            EndIf
        EndIf
      EndIf
'--------------  
      If  eventnumber()=BTN_EJEC_VOL Then ' VOL

      EndIf 
'--------------
      If  eventnumber()=BTN_EJEC_PAN Then 'PAN

      EndIf 
'----------------
'////////////////// PATCH EJEC /////////////////////////////
      If  eventnumber()=BTN_EJEC_PATCH Then 'PATCH o insrumento de un Sinte,,,
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

           Dim As Integer instrum =0, pis=0,num=0
           For k=1 To 32 ' pistas ejec de grabaciondesde teclado
             If CheckBox_GetCheck( cbxejec(k))= 1  Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
                patchsal=CInt(tocaparam(pis).patch)
             EndIf
           Next k
           If  pis >=1 Then  
                If tocaparam(pis).nombre > ""  Then ''''And  tocaparam(pis).maxpos > 0  Then 
                   selInstORdenNum (instrum)
                    '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
                   Print #1," pista ejec  nro ",pis
                   tocaparam(pis).patch=CUByte (instrum)
                   pmTk(pis+32).patch=CUByte (instrum)
                   patchsal=instrum
                   ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
                   Print #1,"ejecucion patch elegido tocaparam(pis).patch ", tocaparam(pis).patch
'--------------------------
' preparamos para grabar la pista por cambio de patch

                  ReDim  toc.trk(1 To tocaparam(pis).maxpos)

                  Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                  Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                '        Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
                   Next j
    Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal

' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi) ' POR PATCH
  'ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

             Else
                  patchsal=1
                  instru=patchsal
                  selInstORdenNum (instrum)
                   '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
                  Print #1," pista ejec  nro ",pis
                  tocaparam(pis).patch=CUByte (instrum)
                  pmTk(pis+32).patch=CUByte (instrum)
                  ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
                  Print #1,"ejecucion patch elegido tocaparam(pis).patch ", tocaparam(pis).patch
             EndIf
         EndIf
      EndIf 


'////////////////// CANAL EJEC ///////////////// 
    If  eventnumber()=BTN_EJEC_CANAL Then ' CANAL de un synthe por ejemplo
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

          Dim As Integer canal =0, pis=0,num=0
         For k=1 To 32 ' pistas ejec de grabaciondesde teclado
           If CheckBox_GetCheck( cbxejec(k))= 1 Or CheckBox_GetCheck( cbxgrab(k))= 1  Then
              pis=k
           EndIf
         Next k

         If  pis >=1  Then  
             If tocaparam(pis).nombre > ""  And  tocaparam(pis).maxpos > 0  Then 
                 selcanalEjec (1,pis) ' 1 salida
                 Print #1," pista ejec  nro ",pis
                 tocaparam(pis).canal =pmTk(pis+32).canalsalida
                 Print #1,"ejecucion canal elegido tocaparam(pis).canal ", tocaparam(pis).canal
    '--------------------------
    ' preparamos para grabar la pista por cambio de patch
                ReDim  (toc.trk)(1 To tocaparam(pis).maxpos)
                Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                Print #1,"tocaparam(pis).maxpos),pis ",tocaparam(pis).maxpos, pis
        ' PREPARAMOS PARA GRABAR A ARCHIVO
                For j As Integer =1 To   tocaparam(pis).maxpos
                     toc.trk(j).modo=Toca(pis).trk(j).modo
                     toc.trk(j).nota=Toca(pis).trk(j).nota
                     toc.trk(j).vel=Toca(pis).trk(j).vel
               '    Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
               Next j
   Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal
   
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi)  'POR CANAL
  ''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

            Else
                selcanalEjec (1,pis) ' 1 salida
                Print #1," pista ejec  nro ",pis
                tocaparam(pis).canal =pmTk(pis+32).canalsalida
                Print #1,"ejecucion canal elegido tocaparam(pis).canal ", tocaparam(pis).canal
            EndIf
         EndIf
'-----------------------
         For k=1 To 32 ' pistastrack de cancion
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=k
           EndIf
         Next k 
         If  num >=1 Then
             selcanal (1)
            
         EndIf

    EndIf

''' en base alanterior terminar esta parte que es para pistas de cancion manual
'' mas adelante....cuando termine todo pistas ejec 
''para pistas de cancion manual futuro ???pero si ya hay para pistas manual??
'//////////////// SEL PORT DE ROLL O MANUALES O CANCION
      If  eventnumber()=BTN_ROLL_PORTSAL  And cierroport= 0 Then
         Dim As Integer miport =1, pis=0,num=0
         cierroport=1 ' asi entra una sola vez,,,
         For k=1 To 32 
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=1
           EndIf
         Next k
' miport=1 estamos seleccionadno port de salida , de entrada es 2 midi in
        If  num=1 Then  ' se chequeop una pista no importa cual
         thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))
        EndIf
            
     EndIf 
'-------------------
'////////////////// BOTON PATCH ROLL O CANCION O MANUAL /////////////////////////////
' futuro todas estos codigos de  case si son parecidos luego  algun dia 
' los convertiremos en rutinas,,,JMG RECORDAR...!
      If  eventnumber()=BTN_ROLL_PATCH Then 'PATCH o insrumento de un Sinte,,,
' //////// PATCH PARA CANCION PERO NO GRABA A DISCO,,    
'Print #1,"EN BTN_ROLL_PATCH" 
         Dim  as Integer num = 0  , instrum =0             
         For k=1 To 32 ' pistastrack de cancion
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=k
              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
' y si instrum es > 0 es un cambio
              Exit For
           EndIf
         Next k 
'         Print #1, "PATCH . num,instrum ", num, instrum
         If  num >=1 Then
             selInstORdenNum (instrum)
              '''thread2 = ThreadCreate(@selInstORdenNum(), CPtr(Any Ptr, instrum))
 '             Print #1, "patch instrum seleccionado ", instrum
             pmTk(num).patch=CUByte(instrum)
             If CANCIONCARGADA =TRUE Then
             Else
                      ntk=0
             EndIf
            portsal=pmTk(ntk).portout
 '           Print #1, "patch portsal almacenado, instru ", portsal, instrum
            Roll.trk(1,NA).inst= CUByte(instrum)
            Track(ntk).trk(1,1).nnn =CUByte(instrum)
            Dim As String nombreg

              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos >2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  
              carga=1 ' control de carga, anula calcompas durante la carga ,,etc

        EndIf

      End If


End Sub 