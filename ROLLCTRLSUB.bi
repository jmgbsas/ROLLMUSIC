

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
             If NombreCancion > ""  Then
                EstaBarriendoPenta=1 
                threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))   
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
            If NombreCancion > "" Then
                 
               Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + _ 
                                     " Track_"+Str(desde)+"_"+Str(hasta) + " "+ _
              Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins) + " " + _ 
              Str (NombreCancion))   ' @JMG
  
                 
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

                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                   Next j
 Dim tocap As ejecparam = tocaparam(pis)

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
' no los puse en los par�metros me olvid�, no pienso cambiar todo...
' ademas esta bueno poder configurar desde afuera como texto
' tal vez lo agregue a la cancion, pero ya tiene todo dentro del archivo

Dim As Integer arch
Dim As String  estado  
Var ini=16
 If  Open (lugar+"\inicio.txt" For Input As #ini) <> 0 Then 
    Exit Sub
End If

 
Do while Not Eof(ini)
   Input #ini, arch, estado
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

Dim As Integer arch,i1
Dim As String  estado  
Var ini=17
 If Open (lugar+"inicio.txt" For Output As #ini ) <> 0 Then
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



 
Sub CTRL1016 (ByRef lugar As String)
 
      Dim As String nombrea,myfil
       ResetAllListBox(LISTA_DE_EJECUCIONES)

      
       If  CANCIONCARGADA=FALSE And lugar = "" And nombreMidiIn= "" Then  '23-04-2024
lugar= BrowseForFolder( NULL, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, "c:\" )
nombreMidiIn=lugar
       Else
         If CANCIONCARGADA=TRUE Then
          lugar=NombreCancion
         End If
       EndIf
       If lugar = "" And nombreMidiIn <> "" Then
          lugar=nombreMidiIn 'volver  a cargar ejecs sin  cancion por borrado de alguna pista 
       EndIf
                
              'NTKP ES UNA SALIDA DE LA SUB
       CargarPistasEjec lugar, ntkp
  
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
                  SALIDA =1
               EndIf


End Sub 

Sub CTRL1061 (ByRef SALIDA As INTEGER) ' <====== crear pista en cancion con lo elegido

               ntk = CountItemListBox(3)+ 1
               If ntk > 32 Then
                   SALIDA=1 ''Exit Select
                   Exit SUB
               EndIf 

               If instru=0 Then 
                  instru=1
               EndIf
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
               If CANCIONCARGADA=true Or NombreCancion <> "" Then
                 ' arm� el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
               AddListBoxItem(3, NombrePista)
               
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
               CantTicks=1000
               
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
' no depende del numero de pista de ejecucion,sino del portin solamente,,,
             abrirMIDIin=1
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
              ChangeProgram ( tocaparam(k).patch , tocaparam(k).canal, tocaparam(k).portout)
              Exit For
           EndIf
         Next k 
'/
       If  listinAbierto( portin) = 0 Then
              If listInCreado(portin)  = 0 Then
                 midiin(portin) = rtmidi_in_create_default()
                 listInCreado(portin)  =1
             EndIf
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
         selportEjec (2,ntkp ) ' fix 13-03-23 enviamos el track es una seleccion para ese track
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
                   
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

Dim k1 As Integer
 For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
        k1=CInt(pmTk(i+32).portout)
        If InStr(*nombreOut(k1),"Microsoft")>0 Then
        Else
           If listoutAbierto( k1) = 0 Then
              If listoutCreado( k1) = 0 Then
                 midiout(k1) = rtmidi_out_create_default ( )
                 listoutCreado( k1) =1
              EndIf
              open_port midiout(k1),k1, nombreOut(k1)
              Dim As integer    porterror=Err
               listoutAbierto( k1) = 1
              porterrorsub(porterror) 
          Else
          EndIf
      EndIf 
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
Dim tocaparborra (1) As ejecparam


    For i1=1 To 32
    If tocaparam(i1).nombre="" And tocatope >= i1 Then
      If i1 <= 32 Then
       For k1= i1+1 To tocatope 
          If tocaparam(k1).nombre >"" Then
             Toca (k1-1) = Toca(k1)
             Toca (k1) = Tocaborra(1)
             Tocaparam (k1-1) = Tocaparam(k1)
             Tocaparam (k1) = Tocaparborra(1)
          EndIf
       Next k1
       tocatope=tocatope -1
      EndIf
       Exit For
    EndIf
    Next i1

' grabamos las pistas que quedaron 

For pis = 1 To tocatope 
                  reDim  toc.trk(1 To tocaparam(pis).maxpos)

                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                   Next j
 Dim tocap As ejecparam = tocaparam(pis)

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

