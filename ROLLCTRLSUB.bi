
On Error Goto errorhandler
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
                  For i As Integer = 1 To Tope
                    CheckBox_SetCheck(cbxnum(i),0)
                  next i 
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
Print #1, "entro por CTRL1062 NOMBRECANCION TITULOS(0) ", NombreCancion, titulos(0)
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
' esta rutina no se usa mas, ahora se entra por menu archivo
  Print #1, "entro por CTRL1063 NOMBRECANCION TITULOS(0) ", NombreCancion, titulos(0)
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

Sub CTRL10075 ()

           ROLLCARGADO=False 
            Dim As String nombreg
nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0))            
' viva rusia lo hace sencillo....al win api..
            
            If nombreg = "" Then
               Exit Sub
            Else
               nombre=nombreg   
            EndIf



End Sub

Sub CTRL1010(ByRef salida As INTEGER)
           ROLLCARGADO=False 
           TRACKCARGADO=FALSE
            Dim As String nombreg
nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0))
            
            If nombreg = "" Then
               Print #1,"exit select por nombreg vacio "
               salida=1 
               Exit Sub
            Else
               nombre=nombreg   
            EndIf
            If NombreCancion > ""  Then 
               ImportarPistaExterna(nombre) ' estoy en cancion importando  una pista rtk
            EndIf   
          'MenuNew=0           
          'carga=1

End Sub

Sub CTRL1012 (ByRef SALIDA As Integer)
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            If nombre = "" Then
nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0), OFN_CREATEPROMPT)
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
             If CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k  
                Exit For
             EndIf
           Next k
           If pis=0 Then
              Exit Sub
           EndIf

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

                Print #1,"PARAMETROS EJEC nombre ",pgmidi.tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",pgmidi.tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",pgmidi.tocap.orden
                Print #1,"PARAMETROS EJEC delta ",pgmidi.tocap.delta
                Print #1,"PARAMETROS EJEC portout ",pgmidi.tocap.portout
                Print #1,"PARAMETROS EJEC patch ",pgmidi.tocap.patch
                Print #1,"PARAMETROS EJEC canal ",pgmidi.tocap.canal
 ntkp=pis 

' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
'threadGrabamidi=@pgmidi

 grabariniciotxt (NombreCancion)
GrabarMidiIn(pgmidi,pis)  ' POR 1015
'  ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))


End Sub

Sub cargariniciotxt(lugar As String)
'carga estado de sonido, mudo no, o ejecutando si, de los archivos de ejecucion
' no los puse en los par�metros me olvid�, no pienso cambiar todo...
' ademas esta bueno poder configurar desde afuera como texto
' tal vez lo agregue a la cancion, pero ya tiene todo dentro del archivo

Dim As Integer arch
Dim As String  estado  
Var ini=16
Print #1,lugar
 If  Open (lugar+"\inicio.txt" For Input As #ini) <> 0 Then 
     Print #1,"No se puede leer inicio.txt"
    Exit Sub
End If

 
Do while Not Eof(ini)
   Input #ini, arch, estado
   Print #1, arch, estado 
    If LCase(estado) = "si"  Then
      CheckBox_SetCheck (cbxejec(arch),1)
    EndIf
    If LCase(estado) = "no" Then
      CheckBox_SetCheck (cbxejec(arch),0)
    EndIf
    If LCase(estado) = "tiempopatronejec" then
        tiempoPatronEjec=arch
    EndIf 
 Loop


Close #ini

End Sub
'------------
Sub grabariniciotxt(lugar As String)
'carga estado de sonido, mudo no, o ejecutando si, de los archivos de ejecucion
' no los puse en los par�metros me olvid�, no pienso cambiar todo...
' ademas esta bueno poder configurar desde afuera como texto
' tal vez lo agregue a la cancion, pero ya tiene todo dentro del archivo
' guarda la ultima seleccion S de las pistas al dar Play con boton verde de ejecuciones
Dim As Integer arch,i1
Dim As String  estado  
Var ini=17
Print #1,"grabariniciotxt ", lugar
 If Open (lugar+"inicio.txt" For Output As #ini ) <> 0 Then
    Print #1,"No se puede escribir en inicio.txt "
    Exit Sub 
 EndIf

For i1=1 To tocatope
 
    if   CheckBox_getCheck (cbxejec(i1)) = 1 Then
         estado="si"
    EndIf
    if   CheckBox_getCheck (cbxejec(i1)) = 0 Then
         estado="no"
    EndIf
       
     Print #ini, i1;",";estado 
Next i1 
Print #ini, tiempoPatronEjec; ","; "tiempoPatronEjec"

Close #ini

End Sub 

'--------------------------

 
Sub CTRL1016 (ByRef lugar As String) 'CARGAR MIDI-IN PISTAS DE EJECUCION
Print #1,"1 ctrl1016 lugar DirEjecSinBarra ",lugar, DirEjecSinBarra
 
      Dim As String nombrea,myfil,nameCurDir
      ' print #1,"EN Cargar midi-in nombre ",DirEjecSinBarra
       ResetAllListBox(PISTASEJECUCIONES)

       If  CANCIONCARGADA=FALSE Then ' And lugar = "" And DirEjecSinBarra= "" Then  '23-04-2024
           nameCurDir = CurDir() 
        '   Dim PB As Integer
        '   PB=InStr(nameCurDir, ":\")
        '   Dim drive As String
        '   drive=Mid(nameCurDir,1,PB+1)
' estas 2 funciones siguietes hacen lo mismo
''lugar= BrowseForFolder( hwndC, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, drive )

' esta es de window9 del ruso, los flags estan bien documentados
'' PROBAR FileListBoxItem !!!
         lugar=ShellFolder("SELECCION DE CARPETA", nameCurDir, BIF_RETURNONLYFSDIRS Or BIF_USENEWUI)
         DirEjecSinBarra=lugar
     '    Print #1," 2 ctrl1016 lugar DirEjecSinBarra ",DirEjecSinBarra
       Else
         If CANCIONCARGADA=TRUE Then
          lugar=NombreCancion
         End If
       EndIf
       If lugar = "" And DirEjecSinBarra <> "" Then
          lugar=DirEjecSinBarra 'volver  a cargar ejecs sin  cancion por borrado de alguna pista 
       EndIf
       If lugar = "" Then
          Exit Sub
       EndIf         
              'NTKP ES UNA SALIDA DE LA SUB
Print #1,"3 ctrl1016 lugar DirEjecSinBarra ",lugar, DirEjecSinBarra
       CargarPistasEjec lugar, ntkp
'' EJECCARGADA quedo en true en la sub CargarPistasEjec  
       Dim j As integer
           For  j=1 To ntkp
             If tocaparam(j).nombre > "" Then
'nombre debe estar sin extension,las ejecuciones tienen un orden estricto
' vamos a tenerque igualar la cantidad de ticks en todas las pistas de modo
' que el ordende las pistas sea indistinto,elnumero de la pista ejec esta ensu archivo
'veremos si funciona cualqueira sea el orden en el disco alcargar se ordenara por
' ese numero Toca().orden,si funciona tal vez loaplicariamos a roll (mucho trabajo porahora queda asi) 
 
                ntoca=j
                pmTk(j+32).portout=tocaparam(j).portout
                pmTk(j+32).MaxPos=tocaparam(j).maxpos
                pmTk(j+32).portin=tocaparam(j).portin
                pmTk(j+32).patch=tocaparam(j).patch
                pmTk(j+32).canalsalida=tocaparam(j).canal
                pmTk(j+32).canalentrada=tocaparam(j).canalent

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
      ntoca=ntkp
      tocatope=ntkp
      GrabarEjec=0
      cargariniciotxt(lugar) 'para guardar que pista ejec se escucha y cual no
   
Print #1,"4 ctrl1016 lugar DirEjecSinBarra ",lugar, DirEjecSinBarra
 
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
            '      ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de neuvo 13-02-2022 �
             ''  EndIf
               If instru=0 Then instru=1 EndIf
                Roll.trk(1,NA).inst= CUByte(instru)
                Track(ntk).trk(1,1).nnn=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "CTRL1040 Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg
              If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos > 2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo�?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  


              MenuNew=0           
              carga=1


End Sub

Sub CTRL1050 () ' <=========== seleccion de instrumento por orden Numerico

/'
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
'/

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

               ntk = CountItemListBox(PISTASROLL)+ 1
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
                 ' arm� el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista,hwndC )
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
    '           print #1, "NombrePista en 1061",NombrePista
               AddListBoxItem(PISTASROLL, NombrePista)
               
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

Sub CTRL1074() '' Parametros de Roll y Track(0) en memoria

'Type dat Field=1
' nota As UByte =0 ' 1 a 12, en un futuro contendra nota, octava, canal etc 
' dur As  UByte =0 ' duracion 1 a 180, tambien tendra rasguidos distintos programables por usuario o fijos
' vol As  UByte =0 ' volumen hasta 127 es volumen desde ahi es escala 128 a 255 =127 escalas
' pan As  UByte =0 ' paneo + o -
' pb  As  UByte =0 ' pitch bend + o -
' inst As UByte =0 ' instrumento para cada nota podra ser distinto 1 to 128
' ' Nota de escala son 12 ..bemol o sostenido son 2
' ' entonces en 14 numero stengo la info
' ' 129 -> c,130->c#,131->d...140->B--, 141-sos,142,bemol
' ''t   As Ulong   '  ticks por ahroa no 
'End Type
'' dentro del vol pondremso las escalas
'' chords http://www.looknohands.com/chordhouse/piano/ ahi hay 168 escalas..!!
'' en vol tengo desde 129 a 255 para numerar escalas. si faltan puedo usar pan o pb
'' l aidea es poner en que escala esta cada nota o compas y asi poder tener cambios de escla
'' y construir los acordes que se quieran construir es esa escala de esea nota o del compas o 
'' la escala del ultimo cambio...por default la escala sera C mayor.. 
'Type inst
' As dat trk(Any, Any)
'End Type
'Dim Shared As inst Roll ' para visualizar y tocar
' Type inst
' As dat trk(Any, Any)
'End Type

'Type poli Field=1 ' para guardar la secuencia
' dur As  UByte =0   ' duracion 
' dur2 As UByte =0   ' SONIDO ON/OFF 
' dur3 As UByte =0   '  
' dur4 As UByte =0   '  
' dur5 As UByte =0   '  
' dur6 As UByte =0   '  
' dur7 As UByte =0   '  
' dur8 As UByte =0   '  
'
' nota As UByte =0 ' en un futuro contendra nota, octava, canal etc 
' vol As  UByte =0 ' volumen
' pan As  UByte =0 ' paneo
' pb  As  UByte =0 ' pitch bend
' nnn As UByte =0' se usa para escala canal etc 
' tick As ubyte =0' 128 tiene la redonda *1,75 segun pesoDur, 1 la cuartifusa o garrapatea todavia no la uso
' acorde  As ubyte =0 ' 1 a 12 , son el se hara el sort    
'End Type
' posn As Integer =0' de roll todavia no lo uso para generar secuencia
' comentarios Para Futuro:
' tick y posn seria para tener una relacion entre ticks los 128, y la posicin 
' de roll, o sea  en que posicion o columna esta la nota en Roll
' acorde, no se si seria necesario quiere indicar si hay o no un acorde
' una forma de disminuir el algoritmo de lectura posterior....a verlo....
'Type sec
' As poli trk(Any, any)
'End Type

 
'ReDim Shared Track  (0 To 32) As sec ' tracks para guardar,.. y tocar 
'    Roll.Trk(1,i1).nota ' As UByte =0 ' 1 a 12, en un futuro contendra nota, octava, canal etc 
'    Roll.Trk(1,i1).dur  ' As UByte =0 ' duracion 1 a 180, tambien tendra rasguidos distintos programables por usuario o fijos
'    Roll.Trk(1,i1).vol  ' As UByte =0 ' volumen hasta 127 es volumen desde ahi es escala 128 a 255 =127 escalas
'    Roll.Trk(1,i1).pan  ' As UByte =0 ' paneo + o -
'    Roll.Trk(1,i1).pb   ' As UByte =0 ' pitch bend + o -
'    Roll.Trk(1,i1).inst ' As UByte =0 ' instrumento para cada nota podra ser distinto 1 to 128
' PUERTO DE SALIDA O DISPOSITIVO
  '''===>>   pmTk(0).portout      ' ubyte  a ubyte
' CANA LDE SALIDA EN ESE DISPOSITIVO
  '''===>   pmTk(0).canalsalida ' ubyte a ubyte



End Sub

Sub CTRL1092()

' abrir un midi-in ...con callback
' tmbie ndeberia abrir el port out para que suene los que entra por midi-in!
' eso se hace en abrir midi out
' no depende del numero de pista de ejecucion,sino del portin solamente,,,
 '            abrirMIDIin=1
'If abrirMIDIin=1 Then ' grabacion desde teclado ...entr mal los valores por ahora
'   abrirMIDIin=0
'NTK??? vale cero al comienzo ergo es la ultima  ejecucion de trackas ...32+1 sera de ejec
'' ���� SEGUIRACA AJUSTE DEPORTOUT....05-05-2022
 ' deberiamos, 1) seleccionar el portIN de la pista ejec y luego abrir ejecutando esta accion
' se podra usar mycallback mas de 1 vez en distinto port ? supongo que si,.,,,
' SI ya esta abierto el portin no abrirlo por 2da vez...eso requiere controlar la apertura
' de los portin tambien JMGJMGJMG QUEDA PARA DESPUES AHORA VEREMOS APERTURA
' Y CIERRE EN DISPOSITIVOS....
 '----> seguir aca ������ debo
   ' portin y proout se ajustan antes en seleccion midi-in midi-out 
 ' y tambien se seleciona antes el instrumento
' EL PORTOUT ES SOLO PARA EL TECLADO QUE ESTA TOCANDO EN ESA PISTA
' JODE A LA EJECUCION PLAYTOCA ALL??? cbxgrab(i) es de ejecuciones
' pero al abrir un portin lo abro para todo el progrma
' una cosa es abrir un IN y otra ajustar un IN a una pista
' la columna G tiene incidencia en ambas listas la moveremos al centro 
Dim As UByte portin1092, portout1092
       For  i As Short =1 To 32
             If  CheckBox_GetCheck( cbxgrab(i))= 1  Then
                portin1092  = tocaparam(i).portin ' PREVIAMENTE SELECCIONADO
                portout1092= tocaparam(i).portout ' PREVIAMENTE SELECCIONADO
                '''calltoca= i ' 04-06-2022 20-12-2024
                ntoca=i
                portsal=portout1092
 
             'instru, canal, portsal abre ahora distinto para cada pista
' el portin y portout podrian ser los mismos pero igual hay que seleccionarlos antes
' debo cambiar por toaparam ??? es lo mismo ambos se cargaen el la seleccion
' con el mismo valor pmTk(i+32).xxx=tocaparam(i).xxx
              ChangeProgram ( tocaparam(i).patch  , tocaparam(i).canal, portout1092)
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
Print #1,"listinAbierto( portin1092) ",listinAbierto( portin1092)
Print #1,"listInCreado(portin1092) ",listInCreado(portin1092) 
       If  listinAbierto( portin1092) = 0 Then
              If listInCreado(portin1092)  = 0 Then
                 midiin(portin1092) = rtmidi_in_create_default()
                 listInCreado(portin1092)  =1
             EndIf
Print #1,"abriendo portin y call back",*nombrein( portin1092 )
              open_port (midiin(portin1092 ), portin1092, *nombrein( portin1092 ) )
              set_callback midiin(portin1092 ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(portin1092 ), 1, 2, 4)
              teclado=1 
              listinAbierto( portin1092) = 1
              jgrb=0
       End If
     HabilitarMIDIIN=1 ' tengo seleccionado portin

End Sub

Sub CTRL1111() '<========== cambiode escala MUSICAL
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

Sub CTRL1112() '<========= cambiode a escala MUSICAL Alternativa de la Principal

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

Sub CTRL1200(hmessages As hmenu) ' PORTIN EJEC
'1200 seleccionar midiin PORTIN SELECCION INDIVIDUAL
' SACAMOS LA MULTIPLE SERIA ENGORROSA CON GETCHECK
' HAREMOS SELECCION MULTIPLE CON LA LISTA EN UN FUTURO MEDIATO!!!
  ' <== Seleccionar  Puertos MIDI-IN  
' POR AHORA tanto de grabacion como ejecucion grab y ejec
' seleccion de portin , 2:portin. ntkp:salida
' ->  npi: numero port entrada DIFERENCIA PARA ABAJO
' portsin es la cantidad de ports que hay de entrada
' seleccionamos un port de entrada para cada track de ejecucion
' como pensamos por ahora que hay un solo usuario ejecutando ese port se usara para todos 
' los tracks pero probarmos usar distintos solo que no tengo 2 port sanos para probar en este momento 
' el port de la tarjeta no anda y estoy entrando por USB midi cable 
' portin  'GLOBAL ULTIMO PUERTO IN SELECCIONADO, por omision el cero
' si solo hay ejecuciones .....
' 25-09-2024 no monitoreaba port de grabacion cbxgrab

Dim As Integer miport =2, pis=0,num=0,k
If PISTASEJECSELECCIONADA=1 Then
  For k=1 To 32 
    If CheckBox_GetCheck( cbxgrab(k))= 1  Then
       ntoca=k
       pis=k
       Exit For
    EndIf
  Next k
  
  If pis=0 Then 
   Exit Sub
  EndIf

  Print #1, "1] case 1200 portin, ntkp  ", portin, ntkp
  selportEjec (miport,pis ) ' fix 13-03-23 enviamos el track es una seleccion para ese track
  Print #1, "2] case 1200 portin, ntkp  ", portin, ntkp
                   
  pmTk(pis+32).portin=CUByte(portin) ' evitamos el cero
  tocaparam(pis).portin=pmTk(pis+32).portin 
  ntoca=pis '20-12-2024
EndIf



If PISTASROLLSELECCIONADA=1 Then
   For k=1 To 32 
     If  CheckBox_GetCheck( cbxnum(k))= 1  Then
       ntoca=k
       pis=k
       Exit For
     EndIf
   Next k


   If pis=0 Then 
    Exit Sub
   EndIf
   selportEjec (miport,pis )
   pmTk(pis).portin=CUByte(portin) ' evitamos el cero
   Print #1,"PIS, pmTk(pis).portin ";PIS, pmTk(pis).portin
   ntoca=pis
EndIf

  HabilitarMIDIIN = HabilitarMIDIIN + 1
  If HabilitarMIDIIN=2 Then
 ' habilito abrir portin 1092
     HabilitarMIDIIN = 0
     SetStateMenu(hmessages,1092,0)
     SetStateMenu(hmessages,1093,0)

  EndIf


End Sub

Sub CTRL1204(hmessages As hmenu) 'Seleccionar      Puertos MIDI-OUT PARA EJECUCIONES
' seleccion de portout , 1:portout. ntkp:salida
' ->  npo: numero port salida
' portsin es la cantidad de ports que hay de entrada
          If PISTASEJECSELECCIONADA=0 Then
             Exit Sub 
          EndIf
Dim pis As Integer 
 pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
                 ntkp =pis


If ntkp=0 Then 
  Exit Sub
EndIf

         selportEjec (1,ntkp )
         Print #1, "portout, ntkp ",portout, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portout
' podemos asignar muchos a la vez (lalista de la izquiera va de 1 a 32
' lalista de la derecha de 33 a 64 para tener todo en un solo  vector
' pmtk )
                 pmTk(pis+32).portout=CUByte(portout)
                 tocaparam(pis).portout=CUByte(portout)
          HabilitarMIDIIN = HabilitarMIDIIN + 1
      If HabilitarMIDIIN=2 Then
 ' habilito abrir portin 1092
         HabilitarMIDIIN=0
         SetStateMenu(hmessages,1092,0)
         SetStateMenu(hmessages,1093,0)
      EndIf

End Sub
Sub CTRL1205 () 'Abrir      Puerto MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES de la lista 
' DERECHA E IZQUIERDA O SEA EJECUCION ES POR MIDI-IN O POR SECUECNIA
' ROLL O TRK. NO SE ABREN PORTS  DE UN ARCHIVO DE EJEC CARGADO
' PARA ELLO SE USAN LOS BOTONES DE ABAJO POR SI SE QEIREN AGREGAR,,
' AR ALLUEGO GRABARLOS COMO NUEVOS EN EL ARCHIVO DE EJECUCIONES
Print #1,"abriendo port....si no se selecciona previamnete toma cero"
Dim As Integer k1=0,k2=0,pis=0
' CBXEJEC Y CBXGRAB PERTENECEN A LA MISMA LISTA DE LA DERECHA EJECUCIONES
' EJECUTAR O GRABAR ya  sea para play o teclado midi in el port out
' de la pista debe abrirse
' FALTARIA MOSTRAR QUE OUT ESTAN ABIERTOS A DOS COLUMNAS DANDO QUE
' PISTAS LO USAN, SI NINGUNA PISTA USA UN PORT OUT PODRIAMOS CERRARLO
' AUTOMATICAMENTE?? PULSANDO UN BOTON VEIFICACION PORT OUT
        If PISTASEJECSELECCIONADA=0 Then
           Exit Sub 
        EndIf
 PISTASEJECSELECCIONADA=0
 pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
 If pis =0 Then
  Exit Sub
 EndIf
               ''  ntkp =pis

        k1=CInt(tocaparam(pis).portout)
        k2=CInt(pmTk(pis+32).portout)
If k1 <> k2 Then ' algo anda mal
  Print #1, "inconsistencias k1<>k2 en port out no se cierra nada"
  Exit Sub
EndIf
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
              Print #1,"1 PORT YA ABIERTO",*nombreOut(k1)
          EndIf
        EndIf 
        Print #1,"Port usando en Play teclado ",portout
        Print #1,"-------------------------------------"
' el portsal de rtmidi empieza desde cero luego le resto 1
'ntoca es la  pista ejec que se esta grabando global entera
' en este caso recorremos el vector o sea ntoca=i
' aca estamos cargando un archivo ntoca o lo estamos construyendo ???
' si se esta cargando debe estar en el archivo la informacion 
' y la recuperamos ,,   por cargamidiin tenemos que
'     pmTk(ntkp+32).portout= tocaparam(ntkp).portout
'      pmTk(ntkp+32).portin= tocaparam(ntkp).portin
'      pmTk(ntkp+32).patch = tocaparam(ntkp).patch
'      pmTk(ntkp+32).canalsalida=tocaparam(ntkp).canal
'      pmTk(ntkp+32).MaxPos=tocaparam(ntkp).maxpos
' si ntkp > 1 puede ser que se haya cargado sino se esta creando
' NO ABRE PUERTOS NI AJUSTA PARA METROS SI ES ALGO CARGADO
' ESO SE HARA EN LA PARTE INFERIOS PORTSAL PATCH CANAL ETC 
     '  If (i <= ntkp Or i <= ntoca) And  EJECCARGADA=FALSE  Then '30-09-2024
     '   ChangeProgram ( tocaparam(i).patch, tocaparam(i).canal, tocaparam(i).portout-1)
     '  EndIf 
 
 If k1=0 Then
   Print #1,"NO SE ABRE NINGUN PORT",*nombreOut(k1)
 EndIf

End Sub

Sub CTRL1206()
' OJO CIERRA PARA TODAS LAS PISTAS 
'Cerrar  Puerto MIDI-OUT en una pista de ejecucion play por el usuario
'este cierre es para finalizar el uso de un dispositivo en una pista dada
' pero  como el dispo  es unico si lo cierro, lo estoy cerrando para todas
' las pistas otra cosa seria asignar otro midi-out a la pista sin cerrar
' por si lo usa otra pista

     '   If PISTASEJECSELECCIONADA=0 Then
     '      Exit Sub 
     '   EndIf
'PISTASEJECSELECCIONADA=0
Dim pis As Integer
 pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
 If pis =0 Then
  Exit Sub
 EndIf


        Dim k1 As Integer
        k1=pmTk(pis+32).portout 
        Print #1,"midiout ",k1, *nombreOut(k1)
        alloff( pmTk(pis+32).canalsalida,k1 )  
        listoutAbierto(k1)=0
        close_port midiout(k1)


End Sub

Sub CTRL1207() 'NUEVA PARA TICKS
On Local Error Goto fail

Dim  As Integer pis 
pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
 
If pis =0 Then
  Exit Sub
EndIf

Dim As String  nombreTrack
Dim As Integer c144,c128
Dim As Integer i1,i2,i3,j1,on1, off1,cuentatick,maxposTrack, n1,n1old

Dim As Double  Tick5mseg = 0.005208325 'seg 5 miliseg.. para I=240
Dim As UByte   dato1,dato2, dato3
Dim As Double  mitimer=0,deltatime=0
Dim As UByte   monitornota

'Dim As mididat flujo (1 To tocaparam(pis).maxpos)

nombreTrack=tocaparam(pis).nombre
maxposTrack=tocaparam(pis).maxpos
Print #1,"nombreTrack ";nombreTrack
Print #1,"maxposTrack ";maxposTrack
'deberia definir maxposTrack+6 pero ya pongo as capacidad por si se quiere cargar mas notas
ReDim (Track(0).trk ) (1 To maxposTrack*2 ,1 To lim3)



i1=0
Do 
   i1=i1+1
  dato1=Toca(pis).trk(i1).modo
  dato2=Toca(pis).trk(i1).nota
  dato3=Toca(pis).trk(i1).vel

  Select Case  dato1
        Case 1
      n1=n1+1
 '     deltatime=deltatime +Tick5mseg ' milisegundos PARA USAR DurXtick
 '     cuentatick=cuentatick+1  'es i1 para moverme en la pantalla esa cantidad de ticks

      Track(0).trk(i1,1).onoff=0
      Track(0).trk(i1,1).nota=0 'es PianoNota
      Track(0).trk(i1,1).vol=0
      Track(0).trk(i1,1).dur=0
   

'           flujo(i1).estado=0
'           flujo(i1).nota=0
'           flujo(i1).vel=0
'           flujo(i1).indiceX=i1

       Case 144 ' on
         If n1=n1old Then 
            i2=i2+1
         Else
            i2=1 
         EndIf
      Track(0).trk(i1,i2).onoff=2
      Track(0).trk(i1,i2).nota=dato2 'es PianoNota
      Track(0).trk(i1,i2).vol=dato3  'velocidad
      Track(0).trk(i1,i2).dur=185 ' " N " no conosco todavia la duracion
'           flujo(i1).estado=2
'           flujo(i1).nota=dato2
'           flujo(i1).vel=dato3
'           flujo(i1).indiceX =i1
'           flujo(i1).indiceY =i2
       n1old=n1
         Case 128 'off
           If n1=n1old Then 
              i2=i2+1
           Else
              i2=1 
           EndIf

           Track(0).trk(i1,i2).onoff=1
           Track(0).trk(i1,i2).nota=dato2 'es PianoNota
           Track(0).trk(i1,i2).vol=dato3
           Track(0).trk(i1,i2).dur=183 
           n1old=n1
'           flujo(i1).estado=1
'           flujo(i1).nota=dato2
'           flujo(i1).vel=dato3
'           flujo(i1).indice=i1 
'           flujo(i1).deltatime=deltatime
'           deltatime=0
           
'           ' duracion aproximada
'           Dim pasos As Integer
'           For k1 As Integer =1 To 180
'             pasos = DurXTick(k1)
'             If pasos <= cuentatick*1.01 and pasos >= cuentatick * 0.90 Then
'              Track(0).trk(i2,1).dur=pasos ''hay q ubicar i2
' vale para melodia falta para acorde  
'             EndIf 
'           Next k1
'           cuentatick=0 
     End Select
     
 ' casillas de columna sin dato se resetean   
  For j1=1 To lim2 ' decia lim2 porque,,
    If Track(0).trk(i1,j1).nota = 0 And Track(0).trk(i1,j1).dur <182    Then
       Track(0).trk(i1,j1).nota = 181
       Track(0).trk(i1,j1).dur  = 0
      ' Track(0).trk(i1,j1).canal = 0
      ' Track(0).trk(i1,j1).vol  = 0

    EndIf
  Next j1

  If  i1=maxposTrack Then
     For i1=maxposTrack To maxposTrack +6
        For j1=1 To lim2 ' decia lim2 porque,,
          If Track(0).trk(i1,j1).nota = 0 And Track(0).trk(i1,j1).dur <182    Then
           Track(0).trk(i1,j1).nota = 181
           Track(0).trk(i1,j1).dur  = 0
       '    Track(0).trk(i1,j1).canal = 0
       '    Track(0).trk(i1,j1).vol  = 0
          EndIf
        Next j1
     Next i1

  i1=i1-1  ' +6 sino quedaria en +7
      Track(0).trk(i1,1).onoff=0
      Track(0).trk(i1,1).nota=0 'es PianoNota
      Track(0).trk(i1,1).vol=0
      Track(0).trk(i1,1).dur=182

     Exit Do
  EndIf

Loop

titulos(0)=nombreTrack
pmTk(0).MaxPos = maxposTrack +6 '''tocaparam(pis).maxpos
pmTk(0).desde = 4
pmTk(0).hasta = 8
pmTk(0).posn=1
pmTk(ntk).canalsalida=tocaparam(pis).canal
pmTk(ntk).portout=tocaparam(pis).portout
pmTk(ntk).patch=tocaparam(pis).patch
Track(ntk).trk(1,1).nnn =tocaparam(pis).patch

TrackaRoll (Track(), 0 , Roll)
ROLLCARGADO=TRUE



Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error CTRL1207 " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
End If   

End Sub

Sub CTRL1207OLD()
'CONVIERTE UNA PISTA DE EJECUCION SELECCIONADA EN PISTA TRK SIN TICKS
' y luego la idea es solo tocarla con playTocaAll o sea, verlo en rollgrafico 
' pero el play lo hace playTocaAll para mostrar usa trk pero para tocar Tocar()
' usa siempre el track(0)
' 14-01-2024 agregar silencios
On Local Error Goto fail
 
Dim  As Integer pis ,i1,on1, off1,tot1
 pis=GetItemListBox(PISTASEJECUCIONES) +1 ' DEVUELVE A PARTIR DE CERO
 
 If pis =0 Then
  Exit Sub
 EndIf

Dim As String  nombreTrack
Dim As Integer maxposTrack, candado, arranque,delta
Dim As Double  Tick5mseg = 0.005208325 'seg 5 miliseg.. para I=240
Dim As UByte   dato1,dato2, dato3
Dim As Double  mitimer=0,deltatime=0
Dim As UByte monitornota


Dim As mididat flujo (1 To tocaparam(pis).maxpos)

nombreTrack=tocaparam(pis).nombre
maxposTrack=tocaparam(pis).maxpos
'ReDim (Track(0).trk ) (1 To tocaparam(pis).maxpos,1 To lim3)
'ReDim Roll.trk (1 To tocaparam(pis).maxpos, NB To NA) As dat

'pasar notas y duraciones a partir de las pistas de ejecucion
' donde estan los datos? buscamos en PlayTocaAll dos metodos
' pasar con PlayTocaAll a archivo midi y luego levantarlo
' pero levantarlo no anda bien, pasaremos directo a ver q sale

' hacemos un play de una sola pista, si toca bien la trataremos 
' de pasar a track
' YA HACE PLAY OK, LUEGO AHORA DEBO PASAR A DURACIONES DE ROLL

'Dim  As long j=0,k=0,partes,cuenta=0
 
'--------------play TOCA
tocaparam(pis).delta=0

'ChangeProgram ( 1, ntoca, 0)
' todas las pistas empiezan en el mismo Timer


portsal=0 ' por ahora ???

Print #1,"playtoca maxpos ", tocaparam(pis).maxpos

'estado ' on = 2, off=1
'''TickPlay=TickChico*tiempoPatronEjec/240
' no hay caso no cambia nada el play deberia modificar los datos ???
' los tiempos son fijos la unica forma seria cambiar sus valores
' lo que debo hcer es restar una cantidad de ticks fijos a cada
' timex(kply)  por ejemplo timex(kply) - xcant * tickchico
' timex(kply) +  x3H * Tickchico
 
 'usaremos tocaparam(pis).delta para tratr de ver la duracion de cada nota
' pasarlo a duracion de Track y guardarla...

ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
portsal=tocaparam(pis).portout

mitimer=Timer
'flujo(tot1).indice=tot1  me dara el indice de los eventos 144 y 128
'flujo se pasara a trk y  se grabara no se si siempre o solo temporal
' debo poner la info de TOca resumida en TRK y para tocar debo deducir
' toca o reconstruirla a  partir de trk
For i1=1 To maxposTrack 



    dato1=Toca(pis).trk(i1).modo
    dato2=Toca(pis).trk(i1).nota
    dato3=Toca(pis).trk(i1).vel
     

     If  dato1=1 Then ' marcamos con 1 un delta de tick en modo
       '  duracion (mitimer ,Tick5mseg) ' si es cero no 1 no hay duracion es acorde
       '  mitimer = mitimer  + Tick5mseg
         deltatime=deltatime +Tick5mseg ' milisegundos
 
     EndIf

     Select Case  dato1 
         Case 144 ' on
         'noteon dato2,dato3,tocaparam(pis).canal, tocaparam(pis).portout, 1 'message(3) ' noter vel canal
         tot1=tot1+1
          on1=on1+1
         flujo(tot1).indiceX=tot1   
         flujo(tot1).estado=2
         flujo(tot1).nota=dato2
         flujo(tot1).vel=dato3
         delta= deltatime*1000
         Print #1, "ON "; delta;" "; "nota: "; dato2 ; " vel "; dato3           
         flujo(tot1).deltatime=deltatime
         deltatime=0

         Case 128 'off
         'noteoff dato2,tocaparam(pis).canal ,tocaparam(pis).portout,1 'message(2)'
         tot1=tot1+1
         off1=off1+1 
         flujo(tot1).estado=1
         flujo(tot1).nota=dato2
         flujo(tot1).vel=dato3
         delta= deltatime*1000
         flujo(tot1).indiceX=tot1 
        Print #1, "OFF ";delta; "nota "; dato2 ; " vel "; dato3
         flujo(tot1).deltatime=deltatime
         deltatime=0
     End Select
 
Next i1

Print #1,"cantidad de estados on + off ";tot1
Print #1,"cantidad de estados on       ";on1
Print #1,"cantidad de estados      off ";off1
Dim As Integer  ciclo=0, pasociclo=0

For i1=1 To tot1
 If i1=1 Then ' el delta que venga es silencio
   flujo(i1).vel=0
 EndIf
 delta=flujo(i1).deltatime*1000    
 
'ciclo de cierre de todos los on con su off
         Select Case flujo(i1).estado
              Case 2 
                ciclo=ciclo+1
                pasociclo=ciclo
              Case 1 
                ciclo=ciclo-1
  
         End Select

' estado 2 nota 64  5375 1
' estado 2 nota 62  734  2
' estado 1 nota 64  130  1
' estado 2 nota 60  333  2
' estado 1 nota 62  120  1
' estado 2 nota 59  365  2
' estado 1 nota 60  62   1
' estado 2 nota 57  365  2
' estado 1 nota 59  62   1
' estado 1 nota 57  464  0-------------(1)-
' estado 2 nota 79  2042 1
' estado 2 nota 77  458  2
' estado 1 nota 79  99   1
' estado 2 nota 76  437  2
' estado 1 nota 77  130  1
' estado 2 nota 74  365  2
' estado 1 nota 76  57   1
' estado 1 nota 74  604  0--------------(1)
' estado 2 nota 83  1130 1  <-vel=0
' estado 1 nota 83  52   0 ..............
' estado 2 nota 83  193  1  <-vel>0  (2)
' estado 1 nota 83  385  0 .............
' estado 2 nota 83  120  1
' estado 1 nota 83  406  0 .............
' estado 2 nota 83  130  1
' estado 1 nota 83  359  0
' estado 2 nota 83  156  1
' estado 1 nota 83  1021 0
' estado 2 nota 76  1370 1
' estado 1 nota 76  583  0
' estado 2 nota 76  125  1
' estado 1 nota 76  802  0
' estado 2 nota 74  812  1
' estado 2 nota 76  448  2
' estado 1 nota 74  94   1
' estado 2 nota 77  375  2
' estado 1 nota 76  104  1
' estado 2 nota 76  260  2
' estado 1 nota 77  109  1
' estado 2 nota 74  307  2
' estado 1 nota 76  125  1

'(1)  comienzo despues de un ciclo (0), puede haber un vel=0

 If ciclo=0 And (i1+1 <= maxposTrack ) And (i1+2 <= maxposTrack ) And flujo(i1+1).estado=2 _ 
    And flujo(i1).nota <> flujo(i1+1).nota And flujo(i1+2).estado=1 Then 
    flujo(i1+1).vel=0
 EndIf

' If ciclo=0 And flujo(i1+1).estado=2 And flujo(i1+2).estado=2 _ '' Then ' todos los on tienen su correspondiente off y lo que tengo es un silencio
'      And (i1+1 <= maxposTrack ) And (i1+2 <= maxposTrack ) Then
 ' tiene velocidad      
 '   flujo(i1+1).vel=flujo(i1).vel
' EndIf  
 Print #1, " estado ";flujo(i1).estado; " nota ";flujo(i1).nota;" ";delta ;  " vel "; flujo(i1).vel  

Next i1

'proceso posterior

' ----------------------
Print #1," -----------CALCULO NOTA DUR ----ANDA OK----------"
Dim As Integer k1=0, knotaoff,topmax=0, indices(1 To 5000) ' 5000 notas
Dim As UByte notaoff,notaon
Dim As Double notadur=0
' miro los estados 1 off y de ahi retrocedo hasta el estado 2 de la misma nota
Do While k1 < tot1

  k1=k1+1
'  Print #1, "k1 ";k1
  If flujo(k1).estado = 1 Then 'encontro un on,busco su on hacia atras y acumulo deltatime 
     knotaoff=k1 
     notaoff=flujo(k1).nota
     notadur=flujo(k1).deltatime
     Do 
      k1=k1-1
      If flujo(k1).nota=notaoff And flujo(k1).estado=2 Then ' encontro el on
         flujo(k1).duron=notadur  
         Print #1,"estado ";flujo(k1).estado;  " nota ";flujo(k1).nota;" duron ";flujo(k1).duron; " deltatime "; flujo(k1).deltatime  
         topmax=topmax+1
         indices(topmax)=k1 
         k1=knotaoff 
         notadur=0 
         Exit Do
      EndIf 
      If flujo(k1).nota<>notaoff  Then
         notadur=notadur+flujo(k1).deltatime
      EndIf 
     Loop 
  EndIf 
Loop
' tengo el indice de flujo que ser el orden de las notas y su on off
' tengo las dur en mseg
'PERO ESTE DUR EN MSEG DEBE CONERTIRSE AL DUR DE ROLL QUE VA DE 0 A 182
' VECTOR RELDUR O DURCLA... una vez que tengo esa dur y la nota puedo 
' empezar a cargar track(0), las notas se apilaran acorde, si la distancia entre
' notas es menor o igual a 5  mseg...luego el vector flujo tendra marcada
' cada nota on con 144 y terminara en 128 en roll grafico pondremos los 144
' uno al lado del otro con la figura correspondiente a la duracion o sumas
' de figuras ligadas para representar la duracion total de esa nota,
'luego par escuchar tengo el playtocaall y para ver el rollgrafico y ambos se
' sincronizaran con los eventos 144 que sera cada columna del Roll, al flujo
' ya le puse un indice y con ese se puede sincronizar mejor..
' y al fin veremos un roll exacto a como se escucha el vector flujo.
' luego vendra el desafio de modificar o entrar datos estos debe nreflejarse
' tanto en el Roll o track como en el Toca. dato1 son los 5msg el tick
' si quiero achicar el valor dato1=1 que indica un tck de retardo se 
'podra eliminar tantos como reduccion de tama�o. PAr aaumentar el tama�o
' habra que insertar datos con dato1=1 y mover todo hacia adelante del resto
' y renumerar si se insertan notas adicionales en ese cado se coloca un nuevo
' 144 muchos datos1=1 para representar la duracion y el evento 128 para finalizar
' de modo que lo toque bien en el play. 
' CARGA DE TRK(0) YA CREADA

ReDim (Track(0).trk ) (1 To topmax*4 ,1 To lim3)
ReDim Roll.trk (1 To topmax*4, NB To NA) As dat


Dim As UByte durbyte=0
Dim As Integer cntN=0, z=1,k2
k1=0 
k2=0

 Do 
  k1=k1+1 'el de indices
 
' acorde detectamos deltatime < 0.005 y apilamos las notas en track

  If k1 >= 2 Then
     If flujo(indices(k1)).deltatime < 0.01041666 And flujo(indices(k1-1)).nota > 0 And _
        flujo(indices(k1)).nota>0 and flujo(indices(k1)).estado=2  And flujo(indices(k1-1)).estado=2  Then  ' suponemos acorde
        Print #1,"acorde  de notas en k1 ";k1; " notas ";flujo(indices(k1-1)).nota;" -> ";flujo(indices(k1)).nota
        z=z+1
        Track(0).trk(cntN,z).nota= flujo(indices(k1)).nota ' es la Pianonota 
        Track(0).trk(cntN,z).vol = flujo(indices(k1)).vel
      ' la dur debe caer en la misma k2 no avanzar   
     Else
        z=1
        k2=k2+1 ' para el track puede tener por cada k1 varios k2 al abrir la duracion
        Print #1, "====>  contador k2 "; k2
        Track(0).trk(k2,1).nota= flujo(indices(k1)).nota ' es la Pianonota
        Track(0).trk(k2,1).vol = flujo(indices(k1)).vel
        cntN=k2
     EndIf 
  Else
     z=1
     k2=k2+1 
     Print #1, "====>  contador k2 "; k2
     Track(0).trk(k2,1).nota= flujo(indices(k1)).nota ' es la Pianonota
     Track(0).trk(k2,1).vol = flujo(indices(k1)).vel
     cntN=k2
  EndIf 

  
    

''' NO SIRVE => durbyte = FiguraEquivalente(flujo(indices(k1)).duron)
Dim  As Integer aa=1,bb=0
' a los lencios menores a I les doy sonido pero debo ligarlos al siguiete
For aa  = 1 To 2 
    ''aa=1 SILENCIO
   If aa=1 And flujo(indices(k1)).deltatime  > 0.01041666  And flujo(indices(k1)).estado=2  Then ' silencio
     Print #1,"PASO POR AA=1 bb ";bb
     Erase midi
     midi(k1).dur = flujo(indices(k1)).deltatime
     midi(k1).nota = flujo(indices(k1)).nota
     midi(k1).volum = flujo(indices(k1)).vel 'mas q silencio un retardo  con audio
     bb = 1
     
   EndIf 
   If aa=2 Then

     Print #1,"PASO POR AA=2 bb ";bb
      If bb=1 Then   
        k2=k2+1
        cntN=k2
     Track(0).trk(k2,1).nota= flujo(indices(k1)).nota ' es la Pianonota
     Track(0).trk(k2,1).vol = flujo(indices(k1)).vel

        bb=0 
      EndIf
     Erase midi
     midi(k1).dur = flujo(indices(k1)).duron
     midi(k1).nota = flujo(indices(k1)).nota
     midi(k1).volum =  flujo(indices(k1)).vel
   EndIf
Print #1 , "midi(k1).dur "; midi(k1).dur
Print #1 , "midi(k1).nota "; midi(k1).nota
Print #1 , "midi(k1).volum "; midi(k1).volum

Erase duramidi
      duraciones midi(),k1,k1 
   If aa=1 Then '''And flujo(indices(k1)).deltatime  <= 1  Then  'SILENCIOS LE DAMOS AUDIO Y LO LIGAMOS A LA NOTA ON REAL
' pero se podria pasar esto vale para silencion chicos <= F
      duramidi(1)=duramidi(1)+90
   EndIf
 
'' aa=1 silencios audibles y aa=2 notas audibles
   If duramidi(2) >0 And aa =2 Then
      duramidi(1)=duramidi(1)+90
   EndIf
   If duramidi(3) >0 And aa =2 Then
      duramidi(2)=duramidi(2)+90
   EndIf

'	If duramidi(1) >0 Then
'        Print #1, "veo ";duramidi(1); " "; figura(duramidi(1))
'	EndIf
'	If duramidi(2) >0 Then
'       Print #1, "veo ";duramidi(2);" "; figura(duramidi(2))
'	EndIf
'	If duramidi(3) >0 Then
'      Print #1, "veo ";duramidi(3); " "; figura(duramidi(3))
'	EndIf
   
Print #1,"----------carga track en k2 ";k2      

 If duramidi(1) > 0 Then ' encontro una figura equivalente
   If z > 1 Then 
     Track(0).trk(cntN,z).dur  = duramidi(1)
   Else 
     Track(0).trk(k2,1).dur  = duramidi(1)
   EndIf
    Print #1,"FIG 1 EQUIV k2 ";k2; " "; figura(duramidi(1))
 EndIf

 If duramidi(2) > 0 Then ' encontro una figura equivalente
   k2=k2+1  
   If z > 1 Then
     Track(0).trk(cntN+1,z).dur  = duramidi(2)
     Track(0).trk(cntN+1,z).nota = Track(0).trk(cntN,z).nota 
     Track(0).trk(cntN+1,z).vol  = Track(0).trk(cntN,z).vol
   Else
     Track(0).trk(k2,1).dur  = duramidi(2)
     Track(0).trk(k2,1).nota  = Track(0).trk(k2-1,1).nota
     Track(0).trk(k2,1).vol  = Track(0).trk(k2-1,1).vol 
   EndIf 
    Print #1,"FIG 2 EQUIV k2 ";k2; " ";figura(duramidi(2))
 EndIf

 If duramidi(3) > 0 Then ' encontro una figura equivalente
   k2=k2+1  
   If z=2 Then 
     Track(0).trk(cntN+2,z).dur  = duramidi(3)
     Track(0).trk(cntN+2,z).nota =Track(0).trk(cntN,z).nota
     Track(0).trk(cntN+2,z).vol  =Track(0).trk(cntN,z).vol
   Else
     Track(0).trk(k2,1).dur  = duramidi(3)
     Track(0).trk(k2,1).nota =Track(0).trk(k2-1,1).nota
     Track(0).trk(k2,1).vol  =Track(0).trk(k2-1,1).vol
   EndIf
    Print #1,"FIG EQUIV k2 "; k2; " ";figura(duramidi(3))
 EndIf
  Track(0).trk(k2+1,1).dur= 182

  For i1=1 To lim2 ' decia lim2 porque,,
    If Track(0).trk(k2,i1).nota = 0 And Track(0).trk(k2,i1).dur <182    Then
       Track(0).trk(k2,i1).nota = 181
       Track(0).trk(k2,i1).dur  = 0
       Track(0).trk(k2,i1).canal = 0
       Track(0).trk(k2,i1).vol  = 0

    EndIf
  Next i1
Next aa



 If k1 = topmax  Then
    Exit Do
 EndIf
Loop 
     

titulos(0)=nombreTrack
pmTk(0).MaxPos = k2 +2 '''tocaparam(pis).maxpos
pmTk(0).desde = 4
pmTk(0).hasta = 8
pmTk(0).posn=1
pmTk(ntk).canalsalida=tocaparam(pis).canal
pmTk(ntk).portout=tocaparam(pis).portout
pmTk(ntk).patch=tocaparam(pis).patch
Track(ntk).trk(1,1).nnn =tocaparam(pis).patch

TrackaRoll (Track(), 0 , Roll)
ROLLCARGADO=TRUE

Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error CTRL1207" & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
End If   

End Sub 

Sub CTRL2500()
' abrir un midi-in ...con callback para Roll
' usamos los ports con SelPort los de Roll
' SINO SE SELECCIONO PORTIN Y PORTOUT EN EL MENU EJECUCIONES NO SE DEBE EJECUTAR ESTA RUTINA
' SI NO SE SELECIONO NADA TOMARA VALORES CERO PARA PORTIN Y PORTOUT
' ............................................................
' para cambiar al nuevo metodo de pista selecionada,esto deberia tener pistas con nombre
' para poder seleccionarlas por ahora no se cambia,,porque si tengo una camcion cargada
' habra nombres de pistas, pero si no tengo cancion cargada no habara nombres
' solucion si hay cancion seleccionar la pista de la cancion a usar, si no lo hay
' crear una fictica!!! 11 dic 24 IMPLEMETARLA!!!!

Dim As UByte portin2500, portout2500
       For  i As Short =1 To 32
             If  CheckBox_GetCheck( cbxnum(i))= 1  Then
                portin2500  = pmTk(i).portin ' PREVIAMENTE SELECCIONADO
                portout2500 = pmTk(i).portout ' PREVIAMENTE SELECCIONADO
                ntk=i   ' ver esto
                portsal=portout2500
 
              ChangeProgram ( pmTk(i).patch  , pmTk(i).canalsalida, portout2500)
                Exit For  ' termina con el 1er seleccionado solo se toma 1 sola accion
             EndIf
       Next i
Print #1,"listinAbierto( portin2500) ",listinAbierto( portin2500)
Print #1,"listInCreado(portin2500) ",listInCreado(portin2500) 
       If  listinAbierto( portin2500) = 0 Then
              If listInCreado(portin2500)  = 0 Then
                 midiin(portin2500) = rtmidi_in_create_default()
                 listInCreado(portin2500)  =1
             EndIf
Print #1,"abriendo portin y call back",*nombrein( portin2500 )
              open_port (midiin(portin2500 ), portin2500, *nombrein( portin2500 ) )
              set_callback midiin(portin2500 ), @RollCallback, pp
       ' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(portin2500 ), 1, 2, 4)
              teclado=1 
              listinAbierto( portin2500) = 1
              jgrb=0
       End If
     HabilitarMIDIINROLL = HabilitarMIDIINROLL + 1
     If HabilitarMIDIINROLL=3 THEN ' tengo seleccionado portin para ROLL
        Print #1,"PORT IN ABIERTO PARA ROLL"
     EndIf

End Sub

Sub CTRL2502(hmessages As hmenu) ' PORTIN ROLL
'25020 seleccionar midiin PORTIN ROLL
Dim As Integer miport =2 
  If PISTASROLLSELECCIONADA=0 Then
     Exit Sub
  EndIf 

  If PISTASROLLSELECCIONADA=1 Then
     PISTASROLLSELECCIONADA=0
     ntk=GetItemListBox(PISTASROLLSELECCIONADA) +1 ' DEVUELVE A PARTIR DE CERO
     Print #1,"2502 NTK "; ntk
  EndIf
  If ntk=0 Then 
    Exit Sub
  EndIf

Print #1, "1] case 2502 portin, ntk  ", portin, ntk
' selportejec con opcoin 2 selecciona los port de entrada con 1 salida
         selport (miport ) 
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
         Print #1, "2] case 2502portin, ntk  ", portin, ntk
                   
' control de portin ,si hay un track de roll seleccionado le asignamos este portin entrado
' podemos asignar muchos a la vez , eliminamos estemetodo reemplaaremos por seleccion multiple
'          For i As Short =1 To 32
'             If CheckBox_GetCheck( cbxnum(i))= 1  Then
                  pmTk(ntk).portin=CUByte(portin) ' evitamos el cero
'                  
'                 Exit For  
'             EndIf
'          Next i  


           HabilitarMIDIINROLL = HabilitarMIDIINROLL + 1
          If HabilitarMIDIINROLL=2 Then
 ' habilito abrir portin 2500
             HabilitarMIDIINROLL = 0
             SetStateMenu(hmessages,2500,0)
             SetStateMenu(hmessages,2501,0)

          EndIf


End Sub

Sub CTRL2504() 'Seleccionar midi-out roll      
'25020 seleccionar  PORTout ROLL
Dim As Integer miport =1 
  If PISTASROLLSELECCIONADA=0 Then
     Exit Sub
  EndIf 

  If PISTASROLLSELECCIONADA=1 Then
     PISTASROLLSELECCIONADA=0
     ntk=GetItemListBox(PISTASROLLSELECCIONADA) +1 ' DEVUELVE A PARTIR DE CERO
  EndIf
  If ntk=0 Then 
    Exit Sub
  EndIf


Print #1, "1] case 2504 portout, ntk  ", portout, ntk
' selportejec con opcoin 2 selecciona los port de entrada con 1 salida
         selport (miport ) 
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
         Print #1, "2] case 2504 portout ntk  ", portout, ntk
                   
' control de portin ,si hay un track de roll seleccionado le asignamos este portin entrado
' podemos asignar muchos a la vez 
'          For i As Short =1 To 32
'             If CheckBox_GetCheck( cbxnum(i))= 1  Then
'                 pmTk(i).portout=CUByte(portout) ' evitamos el cero
'                  
'                 Exit For  
'             EndIf
'          Next i  


           HabilitarMIDIINROLL = HabilitarMIDIINROLL + 1
          If HabilitarMIDIINROLL=2 Then
 ' habilito abrir portin 2500
             HabilitarMIDIINROLL = 0
             SetStateMenu(hmessages,2500,0)
             SetStateMenu(hmessages,2501,0)

          EndIf

End Sub
'-------------------------------------------------
Sub CTRL2505 () 'Abrir      Puertos MIDI-OUT roll
'------------ABRIR PORT DE SALIDA ---<<<<< ROLL de la lista 
Print #1,"abriendo port....si no se selecciona previamnete toma cero"
Dim As Integer k1,k2
  If PISTASROLLSELECCIONADA=0 Then
     Exit Sub
  EndIf 

  If PISTASROLLSELECCIONADA=1 Then
     PISTASROLLSELECCIONADA=0
     k2=GetItemListBox(PISTASROLLSELECCIONADA) +1 ' DEVUELVE A PARTIR DE CERO
  EndIf
  If ntk=0 Then 
    Exit Sub
  EndIf

  k1=CInt(pmTk(k2).portout)
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
       Print #1,"2505 abro MIDI-OUT ",*nombreOut(k1)
       porterrorsub(porterror)
               
    Else
       Print #1,"1 PORT YA ABIERTO",*nombreOut(k1)
    EndIf
  EndIf 
  Print #1,"Port usando en Play teclado ",portout
  Print #1,"-------------------------------------"
    
    
 
 If k1=0 Then
   Print #1,"NO SE ABRE NINGUN PORT",*nombreOut(k1)
 Else 
   HabilitarMIDIINROLL = HabilitarMIDIINROLL + 1 ' ACA DEBERIA SER 4
'LAS 2 SELECCIONES IN OUT Y LAS 2 APERTURAS IN OUT
 EndIf
End Sub
'-----------------------------------------
Sub CTRL2506() 'Cerrar    Puertos MIDI-OUT de roll
Dim k1 As Integer

For  i As Short =1 To 32
   If  titulos(i) > "" Then
        k1=pmTk(i).portout 
        Print #1,"midiout ",k1, *nombreOut(k1)
        alloff( pmTk(i).canalsalida,k1 )  
        listoutAbierto(k1)=0
        close_port midiout(k1)
   EndIf
Next i 


End Sub



'' ///////////////////// GADGET //////////////////////////


Sub comprimirListaEjecs(nroPista As integer)
' lalista comprime perse solo falta rebombrar
' sitengo 1,2,3 y borro 2 (nroPista) queda 1,3 ordenadas y apiladas contiguas autoamticamemte
' por el gadget sin hacer nada. Deberia renombrar la 3 como 2 eso es todo y borrar 
' la 2 de disco, pero tambien cambiar el nro de orden en el archivo 3 renombrado como 2

Dim as Integer i1,k1,pis
Dim As String nombrepis
Dim Tocaborra  as vivo 'datos
Dim tocaparborra  As ejecparam ' TODO EN CERO USAMOS PARA BORRAR, parametros


    For i1=1 To 32
    If i1 = nroPista Then 'LA Q SE BORRO
      Print #1," i1, tocatope,tocaparam(i1).nombre "; i1,tocatope,tocaparam(i1).nombre 
      
       For k1= nroPista+1 To tocatope ' en los vectores no en pantalla
          If tocaparam(k1).nombre >"" Then
            Print #1,"tocaparam(k1).nombre ,k1 ", tocaparam(k1).nombre, k1  
             Toca (k1-1) = Toca(k1)
             Toca (k1) = Tocaborra
             tocaparam (k1-1) = tocaparam(k1)
             tocaparam(k1-1).orden = CUByte(k1-1)
             tocaparam (k1) = Tocaparborra ' BORRADO DE TOCAPARAM(K1)
             nombrepis=tocaparam(k1-1).nombre  
          EndIf
       Next k1
        tocatope=tocatope -1
       Print #1,"tocatope ", tocatope
       Exit For
    EndIf
    Next i1


 

For pis = 1 To tocatope 
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

                Print #1,"comprimir PARAMETROS EJEC nombre ",pgmidi.tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",pgmidi.tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",pgmidi.tocap.orden
                Print #1,"PARAMETROS EJEC delta ",pgmidi.tocap.delta
                Print #1,"PARAMETROS EJEC portout ",pgmidi.tocap.portout
                Print #1,"PARAMETROS EJEC patch ",pgmidi.tocap.patch
                Print #1,"PARAMETROS EJEC canal ",pgmidi.tocap.canal
 ntkp=pis 

' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
'threadGrabamidi=@pgmidi

 grabariniciotxt (NombreCancion)
GrabarMidiIn(pgmidi,pis)  ' POR 1015 comprimir listas


Next pis
Dim comando As String
comando="del " + DirEjecSinBarra + "\"+ "(" + doscifras(ntoca) + ")"+tocaparam(ntoca).nombre
Print #1,"comando borrar archivo ";comando
Shell comando
Sleep 100

' cargar de nuevo
    Dim As Integer barra = InStrRev(tocaparam(ntoca).nombre,"\")
    Dim  lugar As String   
    lugar=DirEjecSinBarra 'Mid(tocaparam(ntoca).nombre, 1, barra-1)
    Print #1,"path de carpeta ejec  ", lugar

CTRL1016 (lugar)

End Sub

Function bmp_load( ByRef filename As Const String ) As Any Ptr

    Dim As Long filenum, bmpwidth, bmpheight
    Dim As Any Ptr img

    '' open BMP file
    filenum = FreeFile()
    If Open( filename For Binary Access Read As #filenum ) <> 0 Then Return NULL

        '' retrieve BMP dimensions
        Get #filenum, 19, bmpwidth
        Get #filenum, 23, bmpheight

    Close #filenum

    '' create image with BMP dimensions
    img = ImageCreate( bmpwidth, Abs(bmpheight) )

    If img = NULL Then Return NULL

    '' load BMP file into image buffer
    If BLoad( filename, img ) <> 0 Then ImageDestroy( img ): Return NULL

    Return img

End Function

