
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
Print #1, "entro por CTRL1062 NOMBRECANCION TITOLOS(0) ", NombreCancion, titulos(0)
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
GrabarMidiIn(pgmidi)  ' POR 1015
'  ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))


End Sub

Sub cargariniciotxt(lugar As String)
'carga estado de sonido, mudo no, o ejecutando si, de los archivos de ejecucion
' no los puse en los parámetros me olvidé, no pienso cambiar todo...
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
' no los puse en los parámetros me olvidé, no pienso cambiar todo...
' ademas esta bueno poder configurar desde afuera como texto
' tal vez lo agregue a la cancion, pero ya tiene todo dentro del archivo

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



 
Sub CTRL1016 (ByRef lugar As String) 'CARGAR MIDI-IN PISTAS DE EJECUCION
Print #1,"1 ctrl1016 lugar nombreMidiIn ",lugar, nombreMidiIn
 
      Dim As String nombrea,myfil
       print #1,"EN Cargar midi-in nombre ",nombreMidiIn
       ResetAllListBox(LISTA_DE_EJECUCIONES)

      
       If  CANCIONCARGADA=FALSE And lugar = "" And nombreMidiIn= "" Then  '23-04-2024
lugar= BrowseForFolder( NULL, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, "c:\" )
nombreMidiIn=lugar
Print #1," 2 ctrl1016 lugar nombreMidiIn ",nombreMidiIn
       Else
         If CANCIONCARGADA=TRUE Then
          lugar=NombreCancion
         End If
       EndIf
       If lugar = "" And nombreMidiIn <> "" Then
          lugar=nombreMidiIn 'volver  a cargar ejecs sin  cancion por borrado de alguna pista 
       EndIf
                
              'NTKP ES UNA SALIDA DE LA SUB
Print #1,"3 ctrl1016 lugar nombreMidiIn ",lugar, nombreMidiIn
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
       

      cargariniciotxt(lugar) 'para guardar que pista ejec se escucha y cual no
   
Print #1,"4 ctrl1016 lugar nombreMidiIn ",lugar, nombreMidiIn
 
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
            print #1, "CTRL1040 Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
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
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
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
               
                  EntrarNombrePista(NombrePista,hwndC )
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
'' çççç SEGUIRACA AJUSTE DEPORTOUT....05-05-2022
 ' deberiamos, 1) seleccionar el portIN de la pista ejec y luego abrir ejecutando esta accion
' se podra usar mycallback mas de 1 vez en distinto port ? supongo que si,.,,,
' SI ya esta abierto el portin no abrirlo por 2da vez...eso requiere controlar la apertura
' de los portin tambien JMGJMGJMG QUEDA PARA DESPUES AHORA VEREMOS APERTURA
' Y CIERRE EN DISPOSITIVOS....
 '----> seguir aca ÇÇÇÇÇÇ debo
   ' portin y proout se ajustan antes en seleccion midi-in midi-out 
 ' y tambien se seleciona antes el instrumento
' EL PORTOUT ES SOLO PARA EL TECLADO QUE ESTA TOCANDO EN ESA PISTA
' JODE A LA EJECUCION PLAYTOCA ALL???
Dim As UByte portin1092, portout1092
       For  i As Short =1 To 32
             If  CheckBox_GetCheck( cbxgrab(i))= 1  Then
                portin1092  = tocaparam(i).portin ' PREVIAMENTE SELECCIONADO
                portout1092= tocaparam(i).portout ' PREVIAMENTE SELECCIONADO
                calltoca= i ' 04-06-2022
                ntoca=i
                portsal=portout
 
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
Dim pp As Any Ptr
 ''pp=@Toca(1)
              set_callback midiin(portin1092 ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(portin1092 ), 1, 2, 4)
              teclado=1 
              listinAbierto( portin1092) = 1
              jgrb=0
       End If
     HabilitarMIDIIN=1 ' tengo seleccionado portin

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

Sub CTRL1200(hmessages As hmenu) ' PORTIN EJEC
'1200 seleccionar midiin PORTIN
  ' <== Seleccionar  Puertos MIDI-IN SOLO PARA PORTS DE EJECUCION 
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
Dim As Integer miport =2, pis=0,num=0
          For i As Short =1 To 32
' CheckBox_GetCheck( cbxejec(i))= 1 Or
             If CheckBox_GetCheck( cbxgrab(i))= 1  Then
                 pis =i  'ntkp global
                 Exit For  
             EndIf
          Next i  
If pis=0 Then 
  Exit Sub
EndIf

Print #1, "1] case 1200 portin, ntkp  ", portin, ntkp
' selportejec con opcoin 2 selecciona los port de entrada con 1 salida
         selportEjec (miport,pis ) ' fix 13-03-23 enviamos el track es una seleccion para ese track
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
         Print #1, "2] case 1200 portin, ntkp  ", portin, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portin entrado
' podemos asignar muchos a la vez 
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxgrab(i))= 1  Then
                 pmTk(i+32).portin=CUByte(portin) ' evitamos el cero
                 tocaparam(i).portin=pmTk(i+32).portin 
                 Exit For  
             EndIf
          Next i  


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
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 ntkp =i  
             EndIf
          Next i  
If ntkp=0 Then 
  Exit Sub
EndIf

         selportEjec (1,ntkp )
         Print #1, "portout, ntkp ",portout, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portout
' podemos asignar muchos a la vez (lalista de la izquiera va de 1 a 32
' lalista de la derecha de 33 a 64 para tener todo en un solo  vector
' pmtk )
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1  Then
                 pmTk(i+32).portout=CUByte(portout)
             EndIf
          Next i  
          HabilitarMIDIIN = HabilitarMIDIIN + 1
      If HabilitarMIDIIN=2 Then
 ' habilito abrir portin 1092
         HabilitarMIDIIN=0
         SetStateMenu(hmessages,1092,0)
         SetStateMenu(hmessages,1093,0)
      EndIf

End Sub
Sub CTRL1205 () 'Abrir      Puertos MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES de la lista 
' DERECHA E IZQUIERDA O SEA EJECUCION ES POR MIDI-IN O POR SECUECNIA
' ROLL O TRK. NO SE AABREN PORTS  DE UN ARCHIVO DE EJEC CARGADO
' PARA ELLO SE USAN LOS BOTONES DE ABAJO POR SI SE QEIREN AGREGAR,,
' AR ALLUEGO GRABARLOS COMO NUEVOS EN EL ARCHIVO DE EJECUCIONES
Print #1,"abriendo port....si no se selecciona previamnete toma cero"
Dim As Integer k1,k2
' CBXEJEC Y CBXGRAB PERTENECEN A LA MISMA LISTA DE LA DERECHA EJECUCIONES
' EJECUTAR O GRABAR ya  sea para play o teclado midi in el port out
' de la pista debe abrirse
 For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
        k1=CInt(tocaparam(i).portout)
        k2=CInt(pmTk(i+32).portout)
'podria pasar que cambio el portout de midi-in y por ende del out de la pista tambien
        If k1<>k2 Then
          If k1 > k2 Then
          Else
             k1=k2  
          EndIf
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
    
    EndIf
 Next i
 If k1=0 Then
   Print #1,"NO SE ABRE NINGUN PORT",*nombreOut(k1)
 EndIf
End Sub

Sub CTRL1206() 'Cerrar    Puertos MIDI-OUT de ejecucion play por el usuario
For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
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


Sub comprimirListaEjecs()
Dim as Integer i1,k1,pis
Dim Tocaborra (1) as vivo
Dim tocaparborra (1) As ejecparam ' TODO EN CERO USAMOS PARA BORRAR


    For i1=1 To 32
    If tocaparam(i1).nombre="" And tocatope >= i1 Then
      Print #1," i1, tocatope,tocaparam(i1).nombre "; i1,tocatope,tocaparam(i1).nombre 
      If i1 <= 32 Then
       For k1= i1+1 To tocatope 
          If tocaparam(k1).nombre >"" Then
         Print #1,"tocaparam(k1).nombre ,k1 ", tocaparam(k1).nombre, k1  
             Toca (k1-1) = Toca(k1)
             Toca (k1) = Tocaborra(1)
             Tocaparam (k1-1) = Tocaparam(k1)
             Tocaparam (k1) = Tocaparborra(1) ' BORRADO DE TOCAPARAM(K1)
          EndIf
       Next k1
       tocatope=tocatope -1
       Print #1,"tocatope ", tocatope
      EndIf
       Exit For
    EndIf
    Next i1

' grabamos las pistas que quedaron 

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
GrabarMidiIn(pgmidi)  ' POR 1015


Next pis

' cargar de nuevo
    Dim As Integer barra = InStrRev(tocaparam(ntoca).nombre,"\")
    Dim  lugar As String   
    lugar=Mid(tocaparam(ntoca).nombre, 1, barra-1)
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

