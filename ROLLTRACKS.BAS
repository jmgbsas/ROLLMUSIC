
Sub CargarTrack(Track() As sec, ByRef ntk As Integer , ByRef ubirtk As Integer)
' solo carga track no lo pasa a Vector de Visualizacion. ntk debe venir informado
' para cargar desde disco un track se usa esta sub y luego TrackaRoll para verlo
' y editarlo
'abrirRoll=0  
cargacancion=0

  print #1,"----------------------------------------------"
  print #1,"1) CargarTrack nombre, ntk= ", nombre , ntk  
  Print #1,"1) UBIRTK QUE LLEGA ", ubirtk
 
     Dim grabaPos (1,1 )  As poli
     Dim grabaLim (1,1) As poli
     Dim graba3   (1,1)  As poli ' 04-02-2022 se agregan 48 bytes para info futura 
     Dim graba4   (1,1)  As poli 
     Dim graba5   (1,1)  As poli 
     Dim graba6   (1,1)  As poli 
     Dim graba7   (1,1)  As poli 
     Dim graba8   (1,1)  As poli 
     Dim graba9   (1,1)  As poli 
     Dim graba10  (1,1)  As poli 
   'Print #1,"termino dimension de grabas "  
     Dim As Integer ubi1,ubi2 
     Dim As String x,x1,x2,x3,x4,x5,nombrea
  '1) cargar pista desde disco y desde Roll puro   
     If ubirtk = 0   Then ' no tengo nombre debo explorar
           myfilter  = "Track Files"+Chr(0)  +"*.rtk"+Chr(0)
           getfiles(file,myfilter,"open")
           nombrea=*file.lpstrFile
           ubi1 = InStrrev(nombrea,"[")
           ubi2 =InStrRev (nombrea,"]")
           ntk=CInt(Mid(nombrea,ubi1+1,ubi2-ubi1-1))
      '
     Else
  '2)  carga *.rtk de linea de comando doble clik o de cancion   
       If ubirtk > 0 Then 
        '  print #1,"ubirtk > 0 carga de disco o cancion ",ubirtk
          nombrea=titulos(ntk) ' ya venia el nombre
          ubirtk=0
       EndIf   
     EndIf
 'Print #1,"paso 1era parte ,nombrea ",nombrea    
ubirtk=0
       If nombrea = "" Then
      ' print #1, "cargaTrack exit sub"
       nombre=""
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
    Dim ct As Integer
    ct=FreeFile
    Dim miroerr As Integer
'    Print #1,"nombre track ",nombre
 '   print #1,"NTK Y nombre que llego a open en CargaTrack ",ntk ,nombre
    titulos(ntk)=nombre
    
    miroerr= ( Open (nombre  For Binary Access Read As #ct ))
     If miroerr <> 0 Then
  '     print #1,"arch track  abrio con error 1307 CargarTrack miroerr, nombre",miroerr, nombre
       Exit sub
     EndIf
        
     Get #ct, , grabaPos(1,1)
     x1=Bin(grabaPos(1,1).nota,4)
     x2=Bin(grabaPos(1,1).dur,4)
     x3=Bin(grabaPos(1,1).vol,4)
     x4=Bin(grabaPos(1,1).pan,4)
     x5=Bin(grabaPos(1,1).pb,4)
     tipoescala_num_ini =CInt( grabaPos(1,1).nnn ) ' 20-12-2021 - tipoescala en uso
     If tipoescala_num_ini=0 Then
        tipoescala_num_ini=1
     EndIf
     pmTk(ntk).tipoescala=grabaPos(1,1).nnn
  '   Print #1,"Carga Track tipoescala_num_ini ",tipoescala_num_ini
     x=x1+x2+x3+x4+x5
   '     print #1,"reconstruccion x pos bin ", x
     'toda carga de track se guarda en pmTk sea ntk=0 u otro valor   
     pmTk(ntk).MaxPos=CInt("&B"+x)
     MaxPos=pmTk(ntk).MaxPos   
   '  print #1,"pmTk(ntk).MaxPos ", pmtk(ntk).MaxPos
     '|--> LLEVAR A TRACK A ROLL posicion = 1
     '|--> LLEVAR A TRACK A rOLL nota=0 '''notaOld
     '|--> LLEVAR A TRACK A rOLL    inicioDeLectura=0' Int(Maxpos/NroCol)
' ==> aca no lo esta cargadno a roll visual solo a un track ntk
     pmTk(ntk).posn=pmTk(ntk).MaxPos - 2
     If pmTk(ntk).posn < 0 Then pmTk(ntk).posn=0 EndIf
     pmTk(ntk).Ticks = 4000 - pmTk(ntk).MaxPos 
     If pmTk(ntk).Ticks < 0 Then 
        pmTk(ntk).Ticks = pmTk(ntk).MaxPos+4000
     EndIf   
     CantTicks=pmTk(ntk).Ticks
     'es un get trabajo debe ser exactamente MAxPos
   '       Print #1,"llego a 2 antes de redim "

     ReDim Trabajo  (1 To CantTicks,1 To lim3) As poli 
   If NombreCancion > "" Then
     If grabaPos(1,1).dur2 = 1 Then ' sonido on/off 16-03-2022
        print #1,"grabaPos(1,1).dur2",grabaPos(1,1).dur2
        CheckBox_SetCheck(cbxnum(ntk),1) 
     Else
        CheckBox_SetCheck(cbxnum(ntk),0) 
     EndIf
   EndIf
     ' crgamos limites Roll de octavas
     Get #ct, , grabaLim(1,1)
     pmTk(ntk).desde  = CInt(grabaLim(1,1).nota)
     pmTk(ntk).hasta  = CInt(grabaLim(1,1).dur) '01-03 cint
     Print #1,"ntk hasta",ntk, hasta
     pmTk(ntk).notaold= CInt(grabaLim(1,1).pb)
' la nota esca y la esca es la misma `para todos lso tracks despues lo debo cambiar
     
     notaescala_num_ini =CInt(grabaLim(1,1).vol) ' notadeescala 20-12-2021
     If notaescala_num_ini=0 Then
        notaescala_num_ini=1
     EndIf
     Print #1,"Carga Track notaescala_num_ini ",notaescala_num_ini
     pmTk(ntk).notaescala=grabaLim(1,1).vol
     If grabaLim(1,1).pan = 3 Then
        alteracion="sos"
     EndIf
     If grabaLim(1,1).pan = 2 Then
        alteracion="bem"
     EndIf
     If grabaLim(1,1).pan = 0 Then
        alteracion="sos"
     EndIf
     
 '    Print #1,"alteracion ",alteracion
 '    print #1,"desde ",pmTk(ntk).desde
 '    print #1,"hasta ",pmTk(ntk).hasta
     tiempoPatron=CInt(grabaLim(1,1).nnn)
     If tiempoPatron = 0 Then 
        tiempoPatron = 60
     EndIf
'     print #1,"en la carga de track desde hasta", pmTk(ntk).desde,pmTk(ntk).hasta
     pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13   ' 27 para 3 SI CARGO CANCION NO VA
     pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13  ' 90 para  7 06-09-2021 decia 12 -> es 11
 '    print #1,"CargarTrack  NB Na", pmTk(ntk).NB, pmTk(ntk).NA
     
     Get #ct, ,graba3  (1,1)
     pmTk(ntk).canalsalida = graba3(1,1).nnn
     Print #1,"cargatrack pmTk(ntk).canalsalida, ntk ",pmTk(ntk).canalsalida,ntk
     canalx=graba3(1,1).nnn    
     pmTk(ntk).portout= graba3(1,1).dur
     portout = CInt(graba3(1,1).dur)
     
 '    print #1,"cargaCancion ",cargacancion

     Get #ct, ,graba4  (1,1)
     Get #ct, ,graba5  (1,1)
     Get #ct, ,graba6  (1,1)
     Get #ct, ,graba7  (1,1)
     Get #ct, ,graba8  (1,1)
     Get #ct, ,graba9  (1,1)
     Get #ct, ,graba10 (1,1)


     ' 1) con cancion cargada puedo cargar cualqueir pista de cancion existente
     ' en Roll Visual, modificarlo y al pasar de RollaTrack AL grabarlo SE PONDRA 
     ' en cancion automaticamente con un numero nuevo de pista, eso seria una copia
     ' del track con datos y hacer un nuevo track
' Proc: con los datos de otro track, tomo los valores del track x cargado de disco
     ' los dejo como actuales(o sea esta en Roll y Trac(0)) para cargarlo a Roll visual
     ' al grabar con rollatrack estando en cancion, copio en memoria en nuevo ntk 
     ' distinto de 0,buscando el proximo nro de track y grabo a disco el track 
     ' con el nuevo numero de track en el dir de cancion, roll queda con esos datos
 ' y Track 0 tambien, solo que ahora apuntan a un archivo en cancion como track nuevo.
 ' sademas logico hay qye agregarlo como item a la listbox y el nombre a titulo
 ' y todos los parametros nuevos al vector pmTk
 ' 2) Sin cancion cargada puedo cargar un roll y grabarlo como track, siemrpe se
 ' grabara a Track(0), los tracks 1 a 32 son exclusivos de Cancion.
 ' SI la cancion ya está cargada o no hay cancion ajusto al ambiente los parametros
 ' cargados de ese track puntual, puede ser 0 u otro track
     If cargaCancion=0  Then ' termino la carga de cancion es otro evento despues carga de un track aislado¿?
        NB=pmTk(ntk).NB  ' por ejemplo cargar un track clickeado en lista
        NA=pmTk(ntk).NA
        desde=pmTk(ntk).desde
        hasta=pmTk(ntk).hasta
        MaxPos=pmTk(ntk).MaxPos
        notaOld=pmTk(ntk).notaold
        posn=MaxPos -2
        If posn< 0 Then 
          posn=0 
        EndIf
        posicion=1
        curpos=0
        canalx=pmTk(ntk).canalsalida
        portout=pmTk(ntk).portout
     EndIf



     
     ' configuro el track receptor
     ReDim (Track(ntk).trk) (1 To CantTicks, 1 To lim3)
 '    print #1,"*PO = hasta -1 ",  pmTk(ntk).hasta -1
 '    print #1,"Get TrK POLI "
     Dim errget As Integer
     errget= Get( #ct, , Trabajo()) 
     If errget <> 0 Then
 '       print #1,"error en Geteo "
     Else
  '      print #1,"Geteo Trabajo poli ok "
     EndIf
     ' movemos los datos a Track
     ' -------------------------
     'ReDim compas(1 To CantTicks) es solo para visualizar
     'carga=1 ' para visualizar no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
  '   print #1,"pmtk(ntk).MaxPos, ntk ",pmTk(ntk).MaxPos , ntk
  '   print #1,"ABRIR MAXPOS ,NB,NA "; pmTk(ntk).MaxPos, pmTk(ntk).NB,pmTk(ntk).NA
     Dim As Integer  grupo, mayorgrupo

     For j = 1 To pmTk(ntk).MaxPos  '11-07-2021 02-03-2022 ÇÇÇ
    '   print #1,"POSICION :",J
    '  print #1,"lim3 :", lim3
      For i= 1 To lim3
      'print #1,"comienaa FOR i ",i
    '  print #1,"carga i acorde ",i
      Track(ntk).trk(j,i)  => Trabajo(j,i)  ' <-- aca va copiando tambien acorde
    '  print #1,"luego de carga i acorde ",i  
' cargar en visualizacion con otra sub si es necesario
      Next i

     Next j
     cerrar (ct)

     DUR => 0
     curpos =>1

    patchsal=Track(ntk).trk(1,1).nnn
    If patchsal=0 Then
       patchsal=1
       Track(ntk).trk(1,1).nnn=1
    EndIf
 '------------

 
    cadenaes_inicial=""
 '   Print #1,"armarescala desde cargar Track"

    armarescala (cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini, alteracion,1 )
'Print #1,"cargaTrack cadena_inicial ",cadenaes_inicial
    tipoescala_inicial=escala(tipoescala_num_ini).nombre ' 13-01-2022 faltaba ini
'Print #1,"cargaTrack tipoescala_inicial ",tipoescala_inicial    
' carga de escala en guia de escala
    guiaEscala(1).tipoescala=tipoescala_num_ini '13-01-2022 faltaba ini
    guiaEscala(1).notaescala=notaescala_num_ini
    guiaEscala(1).alteracion =CInt(grabaLim(1,1).pan)
    guiaEscala(1).posicion=1
' cuando cargo un track desde disco sin cancion, aislado, es similar a Roll
' ahora durante la carga de cancion y el uso de tAB esto debe ir cambiadno para
' cada track cambiado en el grafico 
'----------
Print #1," patch ntk canal ",	Track(ntk).trk(1,1).nnn, ntk,pmTk(ntk).canalsalida
      print #1,"CargarTrack  fin"  
      print #1,"----------------------------------------------" 
ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0
mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0
mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0
 Sleep 100 ' retardo para que se ubiquen lso datos en memoria ¿? parece necesario
End Sub



'
Sub RollTempaTrackTemp (TrkTemp() As poli,RollTemp() As dat)
'--------CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA
Dim As Integer i4,i3=0,i2=0,i1=0,verticalEnOctavaVacia, octavaDeAcorde,copiado=0
' 
' copia en TrkTemp RollTemp donde está Roll modificado
' se copia toda la secuencia y sus controles, la parte inicial de bloques de los archivos
' solo se envia a dichos bloques en los comandos de grabacion a archivo
 
For i2 = 1 To MaxPos
   'print #1,"i2",i2
   i3=0
   For i1=NB To NA -13
    ' print #1,"i1",i1
      If  RollTemp(i2,i1 ).nota >= 1 and RollTemp(i2,i1 ).nota <=12 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         'print #1,"copia Tabajo a Temp en GrabaRollaTrack"
         i3=i3+1
         TrkTemp(i2,i3).dur  =CInt(RollTemp(i2,i1 ).dur)
         TrkTemp(i2,i3).nota =RollTemp(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =RollTemp(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =RollTemp(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =RollTemp(i2,i1 ).pb
         trkTemp(i2,i3).nnn =RollTemp(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         TrkTemp(i2,i3).nota=CUByte(PianoNota)
   '      print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            print #1,"llego a  12 elementeos de un acorde sale "
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf

      If  Roll.trk(i2,i1 ).dur= 200 And copiado=0 Then ' solo en 13 copia 1 soal vez 
        'OK 
         TrkTemp(i2,13).nnn = RollTemp(i2, i1).inst ''=CUByte(tipoescala)
         TrkTemp(i2,13).vol = RollTemp(i2, i1).vol ''= CUByte(notaescala)

         TrkTemp(i2,13).nota = RollTemp(i2,i1 ).nota ' = 30
         TrkTemp(i2,13).dur  = RollTemp(i2,i1 ).dur  ' = 200
         TrkTemp(i2,13).pan = RollTemp(i2, i1).pan
         Print #1,"Actualizar RollaTrack copia var de control inst ",TrkTemp(i2,13).nnn
         Print #1,"Actualizar RollaTrack copia var de control vol ",TrkTemp(i2,13).vol
         copiado=1
      EndIf
' pero con acordes debo porner para cada octava la info de Roll en cada linea segun
' su octava y agregar especificamente la octava ya que esa info se pierde 
' en cada octava no puedo poner el 201,,el 201 ira una sola vez arriba 
' cada linea correspondiente a una octava tendra la info de lso acordes para esa
' octava eso sigue igual solo que esta info se debe leer antes de todo el resto
' de foram que se sepa que cifrados de acorde van en cada octava...      
      If Roll.trk(i2,i1 ).pb = 201  Then ' info acorde en una octava
      '
         Print #1,"RolltempaTrack hay marca 201, i2, i1",i2, i1
         octavaDeAcorde=1+ (i1-12)/13 ' formula de vacio lineas entre octavas
         Print #1,"i1, octavaDeAcorde,vacio deberia ser 6!! ",i1, octavaDeAcorde
         i4=13 + octavaDeAcorde 
         Print #1,"guardado en i4 ",i4
         Print #1," desde, hasta ",desde, hasta
         verticalEnOctavaVacia= 12 + (hasta-2)*13 + octavaDeAcorde - desde ' 90 + 6 - 4=92
         Print #1,"lugar en roll guardado verticalEnOctavaVacia ",verticalEnOctavaVacia
         Print #1,"Roll.trk(i2,verticalEnOctavaVacia).vol ",Roll.trk(i2,verticalEnOctavaVacia).vol
Print #1,"Roll.trk(i2,verticalEnOctavaVacia).nota ",Roll.trk(i2,verticalEnOctavaVacia).nota
Print #1,"Roll.trk(i2,verticalEnOctavaVacia).dur ",Roll.trk(i2,verticalEnOctavaVacia).dur         
         TrkTemp(i2,i4).vol  = Roll.trk(i2,verticalEnOctavaVacia).vol ' octava
         TrkTemp(i2,i4).nota = Roll.trk(i2,verticalEnOctavaVacia ).nota ' Rollnota
         TrkTemp(i2,i4).dur  = Roll.trk(i2,verticalEnOctavaVacia ).dur  ' acordeNro
         TrkTemp(i2,i4).pb  = 202 
      EndIf
      If i2=MaxPos  Then ' fin sec
         i3=1
         TrkTemp(i2,i3).dur  =182 
         TrkTemp(i2,i3).nota =RollTemp(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =RollTemp(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =RollTemp(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =RollTemp(i2,i1 ).pb
         trkTemp(i2,i3).nnn =RollTemp(i2,i1 ).inst     
      EndIf 
      
   Next i1
   
   If i3 >=2 Then
    'print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
TrkTemp(1,1).nnn=RollTemp(1,NA).inst  ' instrumento o PATCH
print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
'--FIN--CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA Y VICEVERSA
posicion =1
  
  
End Sub
'
Sub ActualizarRollyGrabarPista ()
' nombre es global ,,,por ahora...con un Roll único nombre puede ser global
' ya que es único y siempre es parte de Roll grafico...
   Dim grt As Integer
   grt = FreeFile
   Maxpos=pmTk(ntk).MaxPos
   NB => 0 + (desde-1) * 13   ' 06-03
   NA => 11 + (hasta-1) * 13  ' 
   pmTk(ntk).NB= NB
   pmTk(ntk).NA= NA   
   print #1,"Maxpos ",MaxPos  
   ' DEFINO UN ROLL DONDE MANDO EL ROLL CARGADO LE SACO LOS DELETE SI HUBO
   CantTicks=MaxPos+4000
   ReDim RollTemp (1 To CantTicks, NB To NA) As dat 
' copia en RollTemp el Roll ....que tiene las modificaciones ultimas
   Dim As Integer i1, i2, i3, semitono, borrocol=0, haynota=0,res=0,k=0,final=0
  'eliminar columnas marcadas al grabar disco, 0 + X
  ' VERIFICAR QU EEL MAXPOS SEA DEL ROLL CREO QUE SI LOGICO
   print #1, "inicio MaxPos "; MaxPos
   For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=190 And Roll.trk(i2,i1 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i2,i1 ).dur=182 Then ' 26-06-2021 copiar final archivo
          final= 1 'Atrapa el final
       EndIf

       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA And haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar
       Else
          If borrocol = 1 Then
             res=1  
          EndIf  
       EndIf            
    Next i1
    i1=0  
    If borrocol = 0 Or res=0 Or final=1 Then 'copio columna no borro
       k=k+1
      For i1 = NB To NA
          RollTemp(k, i1) =Roll.trk(i2,i1 )
      Next i1
    EndIf
      
    borrocol=0:res=0:haynota=0:final=0
   Next i2
 print #1,"termina con los borrados, posn ",posn

posn=k        

print #1,"posn=k y  ",posn
' track empieza desde k..no es eso
Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    RollTemp(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    If RollTemp(r1, i1).dur=182 Then
       RollTemp(r1-1, i1).dur=182
    EndIf
    RollTemp(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  
' AHORA MAXPOS SE ACHICO
Print #1,"MaxPos, posn ",MaxPos,posn 
MaxPos=posn
pmTk(ntk).MaxPos=MaxPos
posn=Maxpos-2
pmTk(ntk).posn=posn
Print #1,"MaxPos, posn ",MaxPos,posn
posicion =1
print #1,"MaxPos despues de acchicar",MaxPos
' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir
print #1,"Termina con achicar secuencia"
 Dim grabaPos (1,1)  As poli
 Dim grabaLim (1,1)  As poli
 Dim graba3   (1,1)  As poli ' 04-02-2022 se agregan 48 bytes para info futura 
 Dim graba4   (1,1)  As poli 
 Dim graba5   (1,1)  As poli 
 Dim graba6   (1,1)  As poli 
 Dim graba7   (1,1)  As poli 
 Dim graba8   (1,1)  As poli 
 Dim graba9   (1,1)  As poli 
 Dim graba10  (1,1)  As poli 

 
' datos para track....en temp  
 Dim TrkTemp (1 To CantTicks,1 To lim3) As poli 'definimos un Track temporario
' no usamos RollaTrack porque son distintas estructuras RollaTrack usa sec
' vector de tracks a solo usamos un track as poli, de modo que dejaremos 
' un codigo casi duplicado condensado seri amas engorroso creo
' esta llamada copia el patch o instrumento que se usa en Changeprogram al TrackTemp 
RollTempaTrackTemp (TrkTemp() ,RollTemp())

/' 
'--------CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA
i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
Dim As Integer copiado =0 
For i2 = 1 To MaxPos
   'print #1,"i2",i2
   i3=0
   For i1=NB To NA 
    ' print #1,"i1",i1
      If RollTemp(i2,i1 ).nota >= 1 and RollTemp(i2,i1 ).nota <=12 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         'print #1,"copia Tabajo a Temp en GrabaRollaTrack"
         i3=i3+1
         TrkTemp(i2,i3).dur  =CInt(RollTemp(i2,i1 ).dur)
         TrkTemp(i2,i3).nota =RollTemp(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =RollTemp(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =RollTemp(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =RollTemp(i2,i1 ).pb
         trkTemp(i2,i3).inst =RollTemp(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         TrkTemp(i2,i3).nota=CUByte(PianoNota)
   '      print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            print #1,"llego a  12 elementeos de un acorde sale "
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf

      If Roll.trk(i2,i1 ).dur= 200 And copiado= 0 Then ' solo en 13 copia 1 soal vez 
         
         TrkTemp(i2,13).inst = RollTemp(i2, i1).inst ''=CUByte(tipoescala)
         TrkTemp(i2,13).vol = RollTemp(i2, i1).vol ''= CUByte(notaescala)

         TrkTemp(i2,13).nota = RollTemp(i2,i1 ).nota ' = 30
         TrkTemp(i2,13).dur  = RollTemp(i2,i1 ).dur  ' = 200
         TrkTemp(i2,13).pan = RollTemp(i2, i1).pan
         Print #1,"Actualizar RollaTrack copia var de control inst ",Track(ntk).trk(i2,13).inst
         Print #1,"Actualizar RollaTrack copia var de control vol ",Track(ntk).trk(i2,13).vol
         copiado=1 
      EndIf

      
      
   Next i1
   
   If i3 >=2 Then
    'print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
TrkTemp(1,1).inst=RollTemp(1,NA).inst  
print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
'--FIN--CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA Y VICEVERSA
'/

'''ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3) 30-01-2022 porque cantTicks?
'  ojo espero no haga cancelaciones CantTicks siemrp es mayor a MAxPos o en 
' nucleo se agranda ? fijarse,,,,si en nucleo se redimensiona Track y Roll si
' hace falta sin borrar datos..
'[[[ SE CREA REALMENTE EL TRACK ]]]] 
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)


' copiamaos a ntk que sera 0 al lado de roll, o ntk o ntk +1 segun sea el caso
' en el path de cancion

' ----> de TrkTemp a TRack 
For i1=1 To MAxPos ' de Roll que deberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim3
     Track(ntk).trk(i1,i2)=TrkTemp(i1,i2) ' 
    Next i2
Next i1

'ÇÇÇ
' nombr es global se resuelve en la subrutina que llama a esta
If Open (nombre  For Binary Access write As #grt ) <> 0 Then
     print #1,"Error open Rollsub 2727, nombre ",nombre
     Exit Sub
End If
 print #1, "nombre tiene el path ",nombre
 print #1, "NB , NA,MAXPOS ",NB, NA, MaxPos  
 Print #1,"paso copia a vector"

     Dim As ubyte y1,y2,y3,y4, y5 
     Dim As String a1,a2,a3,a4,a5 ,x
     
     print #1,"etapa1"
     x= Bin(MaxPos,20)
         print #1,"Posicion ",Posicion
     print #1,"string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     a5=Mid(x,17,4)
         print #1,"a1 a2 a3 a4,a5 ",a1, a2 ,a3, a4,a5

     y1= Cubyte("&B"+a1)
     y2= CUByte("&B"+a2)
     y3= CUByte("&B"+a3)
     y4= CUByte("&B"+a4)
     y5= CUByte("&B"+a5)
         print #1, "y1,y2,y3,y4,y5", y1,y2,y3,y4,y5
     ' grabamos maxpos en 5 ubyte
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     grabaPos(1,1).pb   = y5
     If tipoescala_num_ini = 0 Then
        tipoescala_num_ini =1
     EndIf
     grabaPos(1,1).nnn = CUByte(tipoescala_num_ini) ' 15-01-2022 - tipoescala en uso
   If NombreCancion > "" THEN
     If CheckBox_GetCheck( cbxnum(ntk))= 1 Then ' sonido on/off 16-03-2022
         grabaPos(1,1).dur2=1
     Else
         graba3(1,1).dur2=0
     EndIf
   EndIf
     '-----------------------
     grabaLim(1,1).nota = CUByte(desde)
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
     If notaescala_num_ini =0 Then
        notaescala_num_ini =1
     EndIf
     grabaLim(1,1).vol  = CUByte(notaescala_num_ini) ' notadeescala 15-01-2022 
     Select Case alteracion
       Case "sos" 
         grabaLim(1,1).pan  = CUByte(3)
       Case "bem"
         grabaLim(1,1).pan  = CUByte(2)
       Case Else 
         grabaLim(1,1).pan  = CUByte(3) 
     End Select
     grabaLim(1,1).nnn = CUByte(tiempoPatron)
' cargado un Roll desde archivo canalx toma el valor del canal midi de salida del archivo

     graba3(1,1).nnn=pmTk(ntk).canalsalida
     graba3(1,1).dur=pmTk(ntk).portout 
     '-----------------------------
     print #1,"etapa final puts"
     Put #grt, ,grabaPos(1,1)
     Put #grt, ,grabaLim(1,1)
     Put #grt, ,graba3  (1,1)
     Put #grt, ,graba4  (1,1)
     Put #grt, ,graba5  (1,1)
     Put #grt, ,graba6  (1,1)
     Put #grt, ,graba7  (1,1)
     Put #grt, ,graba8  (1,1)
     Put #grt, ,graba9  (1,1)
     Put #grt, ,graba10 (1,1)
     
     Put #grt, ,TrkTemp()
     cerrar (grt)
     While InKey <> "": Wend
     Sleep 150
Print #1,"fin ActualizarRollyGrabarPista"

End Sub
' ---------------------------
Sub ImportarPistaExterna()
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia ImportarPistaExterna " 
Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
Dim midsal As  RtMidiOutPtr
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva  
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    print #1,"extension de la pista importada",ext
    print #1,"nom nombre sin extension ni path ",nom
    print #1,"numero de pista tope =",tope
   tope=tope+1
   If tope <= 32 Then
      ntk=tope
   Else    
      Exit Sub   
   EndIf
   print #1,"pista nueva importada será ntk=",ntk   

  
Select Case ext
 Case ".rtk"
' sacar corchetes si lso tiene del nombre y armar de nuevo el nombre
' toma el ntk globalmente,,,
  titulos(ntk)=nombre
  CargarTrack(Track(),ntk,1)
  TrackaRoll (Track(),ntk,Roll) 'carga a roll el track cargado anteriormente
  ' cambiamos el nombre segun el ntk
    Dim haycorchete As Integer
    haycorchete = InStr(nom,"]")
    If haycorchete> 0 Then
       nom = Mid(nom, haycorchete+1)  ' sacamos el [x] si existe
    EndIf
     
    nombre= NombreCancion + "\[" + doscifras(ntk) + "]" + nom +".rtk"
    Dim cadena As String = "[" + doscifras(ntk) + "]" + nom
    AddListBoxItem(3, cadena)
    print #1,"GRABANDO PISTA EN CANCION EN ",nombre
      'si es vacio tomo la ultima
    titulos(ntk)=nombre ' cambiamos el nombre de la pista 
' grabar en el directorio de cancion
    GrabarRollaTrack(0) ' graba el roll del rtk a directorio cancion 
      
 Case ".roll"
    titulos(0)=nombre ' nombre de un roll externo fuera de cancion
    CargaArchivo(Roll,1) ' sobreescribe ntk global con 0, carga a Roll y a track(0)
    ntk=Tope  ' recupero ntk 
   ' todos los valores quedaron en ntk=0 

    nombre= NombreCancion + "\[" + doscifras(ntk) + "]" + nom +".rtk"
    print #1,"GRABANDO PISTA EN CANCION EN ",nombre
    
'esta en Roll y track (0) debo grabarlo a rtk nuevo ntk en cancion
    titulos(ntk)=nombre ' cambiamos el nombre de la pista en el ntk nuevo 
' grabar en el directorio de cancion
    Tope=Tope-1 'retrocedo el tope porque GrabarCopiaPista incrementa 
    'de nuevo el tope, debo estar posicionado en roll y pista 0
    ntk=0 ' asi apunto o simulo que estoy en ntk=0, será el ntkold  
    GrabarCopiadePista() ' de 0 a ntk 
            
End Select 
    


End Sub
sub GrabarCopiadePista()
' variables globales nombre, ntk
' recorro la list atomo el ntk proximo o simplemente sumo 1 a tope e incremento tope
' siempre que sea menor igual a 32, agrego a la lista decontrol y grabo a disco 
' en cancoin el nuevo track copia, renombrar pitas faltaria esa funcionalidad
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia GrabarCopiaPista " 
Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva  
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    print #1,"extension ",ext
    print #1,"nom nombre sin extension ni path ",nom
    print #1,"pista origen ntk=",ntk
'-> copiamos una pista de cancion en otra pista nueva de cancion
' para invocar esto se necesit ael menu de Control 
   tope=tope+1
   If tope <= 32 Then
      ntk=tope
   Else    
      Exit Sub   
   EndIf
   print #1,"pista nueva ntk=",ntk   
' a) tomar el roll de la pista origen sacarle lso deletes, en un roll temporario
' b) copiar el roll temporario al track nuevo
' a) en este caso como es una copia todos los parametros del Roll origen son
' iguales (dsde hasta maxpos notaold posn etc)
' desde aca es igual
     If CANCIONCARGADA =TRUE  Then ' copia track en otro track
     print #1,"copia track a otro track"
        pmTk(ntk).desde=pmTk(ntkold).desde
        pmTk(ntk).hasta=pmTk(ntkold).hasta
        pmTk(ntk).NB=pmTk(ntkold).NB
        pmTk(ntk).NA=pmTk(ntkold).NA
        pmTk(ntk).MaxPos=pmTk(ntkold).MaxPos
        pmTk(ntk).posn=pmTk(ntkold).posn
        pmTk(ntk).notaold=pmTk(ntkold).notaold
        pmTk(ntk).Ticks=pmTk(ntkold).Ticks
        pmTk(ntk).portout=portout
        ReDim (Track(ntk).trk) (1 To pmTk(ntkold).Ticks, 1 To lim3)
        print #1,"hizo copia de parametros de trackold en new"
        print #1," *po seria hasta -1 ", pmTk(ntk).hasta -1
     EndIf
     If nom<> "" Then ' graba roll en cancion
        print #1,"preparo nombre cancion nuevo ntk ", ntk
        Dim haycorchete As Integer
        haycorchete = InStr(nom,"]")
        If haycorchete> 0 Then
          nom = Mid(nom, haycorchete+1)  ' sacamos el [x] si existe
        EndIf
     
        nombre= NombreCancion + "\[" + doscifras(ntk) + "]" + nom +".rtk"
        Dim cadena As String = "[" + doscifras(ntk) + "]" + nom
        AddListBoxItem(3, cadena)
        print #1,"GRABANDO PISTA EN CANCION EN ",nombre
      'si es vacio tomo la ultima
        titulos(ntk)=nombre
     EndIf
   ActualizarRollyGrabarPista ()
   
print #1,"FIN GrabarCopiaPista nueva ",ntk
print #1,"FIN GrabarCopiaPista ,maxpos,posn ",maxpos,posn

print #1,"---------------------------------------------------------------------------------"


  
End Sub
' ----------------------------------
Sub GrabarRollaTrack ( cambiaext As Integer ) ' SE USA PARA TODO 
' las 3 1eras funciones pueden hacerce en roll sin problemas..roll lo deduce
' de la entrada y las variables globales. la 4ta no debe llamarse desde Control
'1) Convierte y Graba un Roll cargado de disco, como Track [00] fuera de cancion. conversion
' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia 
' cambiaext=1 porque pasa de .roll a .rtk  
'2) convierte un Roll a trk y graba en cancion, igual que antes pero 
' lo pone en cancion con el mismo nro de track, sobreescribe
' ACA NO ELIMINAMOS LA POSIBILIDAD DE UNDO YA QUE NO GRABAMOS ROLL
' ESTAMSO GRABANDO A TRACK EXPORTANDO PERO EL ROLL NO SE GRABO Y PODRIA AHCER UNDO
' EXPORTARLO DE NUEVO O NO EXPORTARLO Y HACER UNDO DE ROLL PERO TRACK QUEDA COMO BACKUP
' DE LO NUEVO , SOLO AL GRABAR ROLL SE ELIMINA EL UNDO
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia GrabarRollaTrack, cambiaext ",cambiaext 
   Dim As String path, nom,ext
   Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de rool a track, i tengo una cancion en edcion
' y queiro un roll nuevo no conviene hacerlo aca si hay cancion no se usa
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    print #1,"extension ",ext
    print #1,"nom nombre sin extension ni path ",nom
 '1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' por eso cambia extension a rtk
If  nombre > "" Then
   ' graba roll a rtk en cancion 0 o 1 
   If cambiaext > 0   And ext =".roll" Then ' convertir
      ntk=0 
      nombre=path +"[00]"+nom +".rtk" 'path + 0 + rtk por default si no hay cancion
      print #1,"armado de nombre roll a trk[00]",nombre
        ' guardo los valores de Roll cargado en el track nuevo [00] 
        If CANCIONCARGADA=TRUE  And ROLLCARGADO=TRUE Then 
           ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3) 
           pmTk(ntk).desde=desde
           pmTk(ntk).hasta=hasta
           pmTk(ntk).NB=NB
           pmTk(ntk).NA=NA
           print #1,"NB y NA de RollaTrack cargando un Roll en cancion ", NB, NA 
           pmTk(ntk).MaxPos=MaxPos
           pmTk(ntk).posn=posn
           pmTk(ntk).notaold=notaold
           pmTk(ntk).Ticks=CantTicks
           pmTk(ntk).canalsalida=CUByte(canalx) ' SOLO VALIDO PRA UAN PISTA AISLADA
           pmTk(ntk).portout=portout 
        EndIf
      
   EndIf
' NOTA: pra cambiar un canal de salida midi se debera editar la pista en el editor de Roll
' cambiar al canal MIDI deseado y Grabar el archivo como roll y como rtk asi queda el valor
' en ambos archivos o si se cargo como track se grabara como track..
' si estoy en cancion al usar TAB para pasar de pista el valor del canal mido quedara ademas
' en al estructura pmTk en memoeria,,,
   
 '2) Actualizacion de un trk de cancion, se modifica desde roll igual que antes 
 'pero sobreescribe el track del roll correspondiente en cancion
 ' lso parametros no se tocan es la misma pista 
   If cambiaext=0 And nombre >""  And ext = ".rtk" Then 
     print #1,"update ntk, Nombre Cancion", NombreCancion ' es el path del directorio de cancion
     print #1,"update de NumPista ntk ", ntk
     print #1,"update,nombre del track con  path no se toca ",nombre
   EndIf
Else
' no crea solo convierte roll a rtk y graba, o solo graba de rtk a rtk
' el archivo debe existir y estar cargado por eso nombre debe existir sino sale sin hacer nada
   Exit sub
EndIf

   If ext = ".rtk" Then 
     print #1,"  ntk ", ntk
     print #1,"GRABANDO UPDATE DE PISTA EN CANCION EN ",nombre
   ' veo valore sde track  
    print #1,"desde ", desde,pmTk(ntk).desde
    print #1,"hasta ", hasta,pmTk(ntk).hasta
    print #1,"Maxpos ",MaxPos,pmTk(ntk).MaxPos
    print #1,"NB ",NB,pmTk(ntk).NB
    print #1,"NA ",NA,pmTk(ntk).NA
   EndIf   

' edsde aca es igual.. 
  ActualizarRollyGrabarPista () 

print #1,"FIN GrabarRollaTrack ,cambiaext ",cambiaext
print #1,"FIN GrabarRollaTrack ,maxpos,posn ",maxpos,posn

print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   
 
End Sub
'---------
Sub RollaTrack (Track() As sec, ntk As Integer,Roll As inst)
' PROCESO EN MEMORIA , desde 15-01 hemos puesto subrutina comun con GrabarRollaTrack
' con la penalidad de que se copai 2 veces los datos si demora mucho los profesos de 
' AutoFracTodoDur y FraccionarDur
' volverlo a la version sin copia....  
' 15-01-2022 copio campos control de Roll a Track en lim3=13  
' 02-12-2021
' condicion Roll debe estar cargado en el editor entonces copiamos Roll a Track
' si es ntk=0 estamos copiando el gemelo de Roll en Track...
' MaxPos es global y lim3 tambien no s eporque -2 debo copiar todo
Dim As Integer j,i3,i2,i1
'------------------------------------------
' basada en ActualizarRollyGrabarPista pero en un solo paso, dejare una aola ? veremos
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
   Erase mel_undo, undo_acorde,undo_kant_intervalos  
   mel_undo_k=0: ig=0:cnt_acor=0
' `PODRIA USAR UN TEMP Y LUEGO COPIAR COMO SE HACE EN ACTUALIZARROLLATRACKYGRABAR
' Y ASI UNIFICARIAMOS
 ReDim RollTemp (1 To Maxpos, NB To NA) As dat
'1) 1ER COPIA PENALIDAD OJO DE ROLL A ROLLtEMP  
 For i2 = 1 To MaxPos
   For i1 = NB To NA
      RollTemp(i2, i1) = Roll.trk(i2,i1 )
   Next i1
 Next i2

 ReDim TrkTemp (1 To MaxPos,1 To lim3) As poli 'definimos un Track temporario
' no usamos RollaTrack porque son distintas estructuras RollaTrack usa sec
' vector de tracks a solo usamos un track as poli, de modo que dejaremos 
' un codigo casi duplicado condensado seri amas engorroso creo
'1) 2DA COPIA PENALIDAD OJO DE ROLLTEMP A TRKTEMP
 RollTempaTrackTemp (TrkTemp() ,RollTemp())
For i1=1 To MAxPos ' de Roll que deberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim3
     Track(ntk).trk(i1,i2)=TrkTemp(i1,i2) ' 
    Next i2
Next i1
' son 2 copias no se si sera algo lento y ma suso de memoria veremos sino lo volveremos 
' a copia dulpicada por diferente estructuras no se borra lo anterio por backup

/' ANTES SE HACIA 1 SOLA COPIA ........LA MITAD DE TIEMPO OJO 
'--------CODIGO SIMILAR A ACTUALIZARROLLYGRABAR SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA
i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
Dim As Integer copiado =0 
For i2 = 1 To MaxPos
   'print #1,"i2",i2
   i3=0
' l aultima octava no se usa en Roll o sea desde NA-12 hasta NA por eso se la usa
' para guardar valores de control y se copiaran a lim3=13 en track
' escalas auxiliares esta en...yyy   
   For i1=NB To NA  '15-01-2022 
    ' print #1,"i1",i1
      If Roll.trk(i2,i1 ).nota >= 1 and Roll.trk(i2,i1 ).nota <=12 Then
         i3=i3+1
         Track(ntk).trk(i2,i3).dur  =CInt(Roll.trk(i2,i1 ).dur)
         Track(ntk).trk(i2,i3).nota =Roll.trk(i2,i1 ).nota
         Track(ntk).trk(i2,i3).vol  =Roll.trk(i2,i1 ).vol
         Track(ntk).trk(i2,i3).pan  =Roll.trk(i2,i1 ).pan
         Track(ntk).trk(i2,i3).pb   =Roll.trk(i2,i1 ).pb
         track(ntk).trk(i2,i3).inst =Roll.trk(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         Track(ntk).trk(i2,i3).nota=CUByte(PianoNota)
   '      print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            print #1,"Fin de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf

      If Roll.trk(i2,i1 ).dur= 200 And copiado= 0 Then ' solo en 13 copia 1 soal vez 
         
         Track(ntk).trk(i2,13).inst = Roll.trk(i2, i1).inst ''=CUByte(tipoescala)
         Track(ntk).trk(i2,13).vol = Roll.trk(i2, i1).vol ''= CUByte(notaescala)

         Track(ntk).trk(i2,13).nota = Roll.trk(i2,i1 ).nota ' = 30
         Track(ntk).trk(i2,13).dur  = Roll.trk(i2,i1 ).dur  ' = 200
         Track(ntk).trk(i2,13).pan = Roll.trk(i2, i1).pan
         Print #1,"RollaTrack copia var de control inst ",Track(ntk).trk(i2,13).inst
         Print #1,"RollaTrack copia var de control vol ",Track(ntk).trk(i2,13).vol
         copiado=1 
      EndIf

   Next i1
   
   If i3 >=2 Then
    print #1,"copia acorde ",i3," en ",i2
    Track(ntk).trk(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 


  
   
Next i2   
Track(ntk).trk(1,1).inst=Roll.trk(1,NA).inst  
Print #1,"Fin copia de Roll a Track, RollaTrack"
'---FIN--CODIGO SIMILAR A ACTUALIZARROLLYGRABAR SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA
'/

 
End Sub
' ESTE ROLL A TRACK ES EL ORIGINAL LA MITAD DE TIEMPO PARA AHCER TAB
' DEBO PROBAR CON 2 TRACKS MUY PESADOS 
Sub RollaTrack_ORIGINAL(Track() As sec, ntk As Integer,Roll As inst)
' PROCESO EN MEMORIA (DEBERIA SER LLAMADO POR GRABARROLLATRACK ASI SIMPLIFICO ALGO)  
' 15-01-2022 copio campos control de Roll a Track en lim3=25  
' 02-12-2021
' condicion Roll debe estar cargado en el editor entonces copiamos Roll a Track
' si es ntk=0 estamos copiando el gemelo de Roll en Track...
' MaxPos es global y lim3 tambien no s eporque -2 debo copiar todo
Dim As Integer j,i3,i2,i1,verticalEnOctavaVacia, octavaDeAcorde
'------------------------------------------
' basada en ActualizarRollyGrabarPista pero en un solo paso, dejare una aola ? veremos
' esto debe hacerse solo al pasar por memoria no al grabar
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
   Erase mel_undo, undo_acorde,undo_kant_intervalos  
   mel_undo_k=0: ig=0: cnt_acor=0

'----------------
i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
 
For i2 = 1 To MaxPos
   'print #1,"i2",i2
   i3=0
' l aultima octava no se usa en Roll o sea desde NA-12 hasta NA por eso se la usa
' para guardar valores de control y se copiaran a lim3=13 en track
' escalas auxiliares esta en...yyy   
   For i1=NB To NA  '15-01-2022 
    ' print #1,"i1",i1
      If Roll.trk(i2,i1 ).nota >= 1 and Roll.trk(i2,i1 ).nota <=12 Then
         i3=i3+1
         Track(ntk).trk(i2,i3).dur  =CInt(Roll.trk(i2,i1 ).dur)
         Track(ntk).trk(i2,i3).nota =Roll.trk(i2,i1 ).nota
         Track(ntk).trk(i2,i3).vol  =Roll.trk(i2,i1 ).vol
         Track(ntk).trk(i2,i3).pan  =Roll.trk(i2,i1 ).pan
         Track(ntk).trk(i2,i3).pb   =Roll.trk(i2,i1 ).pb
         track(ntk).trk(i2,i3).nnn =Roll.trk(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         Track(ntk).trk(i2,i3).nota=CUByte(PianoNota)
   '      print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            print #1,"Fin de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf
' con esaclas esta bien, con una sola vez que copie se repita para todas las octavas
' 
      If Roll.trk(i2,i1 ).dur= 200  Then ' solo en 13 copia 1 sola vez ok sigue igual
         
         Track(ntk).trk(i2,13).nnn = Roll.trk(i2, i1).inst ''=CUByte(tipoescala)
         Track(ntk).trk(i2,13).vol = Roll.trk(i2, i1).vol ''= CUByte(notaescala)

         Track(ntk).trk(i2,13).nota = Roll.trk(i2,i1 ).nota ' = 30
         Track(ntk).trk(i2,13).dur  = Roll.trk(i2,i1 ).dur  ' = 200
         Track(ntk).trk(i2,13).pan = Roll.trk(i2, i1).pan
         Print #1,"RollaTrack copia var de control inst ",Track(ntk).trk(i2,13).nnn
         Print #1,"RollaTrack copia var de control vol ",Track(ntk).trk(i2,13).vol
          
      EndIf
' pero con acordes debo porner para cada octava la info de Roll en cada linea segun
' su octava y agregar especificamente la octava ya que esa info se pierde 
' en cada octava no puedo poner el 201,,el 201 ira una sola vez arriba 
' cada linea correspondiente a una octava tendra la info de lso acordes para esa
' octava eso sigue igual solo que esta info se debe leer antes de todo el resto
' de foram que se sepa que cifrados de acorde van en cada octava...      
      If Roll.trk(i2,i1 ).pb = 201  Then ' info acorde en una octava
      '
         octavaDeAcorde=restar(i1)+1
         i3=13 + octavaDeAcorde 
         verticalEnOctavaVacia= 12 + (hasta-2)*13 + octavaDeAcorde - desde ' 90 + 6 - 4=92
         Track(ntk).trk(i2,i3).vol  = Roll.trk(i2,verticalEnOctavaVacia).vol ' octava
         Track(ntk).trk(i2,i3).nota = Roll.trk(i2,verticalEnOctavaVacia ).nota ' Rollnota
         Track(ntk).trk(i2,i3).dur  = Roll.trk(i2,verticalEnOctavaVacia ).dur  ' acordeNro
          
      EndIf
   Next i1
   
   If i3 >=2 Then
    print #1,"copia acorde ",i3," en ",i2
    Track(ntk).trk(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 


  
   
Next i2   
Track(ntk).trk(1,1).nnn=Roll.trk(1,NA).inst  
Print #1,"Fin copia de Roll a Track, RollaTrack"

 
End Sub




' ---------------------
Sub TrackaRoll (Track() As sec, ByVal ntk As Integer, Roll As inst)
'[[[[[ VER MAS ABAJO JMG 15-01-2022  DEBO COPIAR EL CAMPO DE CONTROL DE LIM3 EL 13 !!!! ]]]]]  
' como es track a Roll se supone que el track ya esta cargado en memoria
' y esta sub pasa de Track a Roll Visual y track 0 
' o sea copia track a Roll en memoria
Dim As Integer i1,i2,i3,Maxposicion,octavaDeAcorde,verticalEnOctavaVacia
 ' print #1,"TrackaRoll 1"   
''If ubirtk=3 Then ' estoy conmutando de track durante la edicion
' si no estoy en cancion el nto va a ntk 0, siemrep uso ntk y el vector de pnTk
print #1,"-------------ARRANCA TRACKAROLL---------------------------------"
print #1,"NTK Y nombre que llego a TrackaRoll ",ntk ,titulos(ntk)
print #1,"(ntk).maxpos ", pmTk(ntk).MaxPos
nombre=titulos(ntk)
'0) Si se pula delete o Supr estando en Roll Visual, se iran borando de memoria
' los datos de ese trrack y de la lista de Control, pero los archivo sen disco
' seguiran iguales, primera etapa,luego al Grabar Cancion se borrara definitivamente
' los track no usados, lso nombres de los tracks permaneceran iguales.
'  luego se hara que si se borro y se agregan nuevos tracks estos tomaran los lugares
' de lso tracks borados y sus numeros no usados [x].
nota=0:dur=0
'copia a variables de Roll desde track
   desde  = pmTk(ntk).desde
   hasta  = pmTk(ntk).hasta
   Print #1,"hasta 1054 TracjaRoll ",hasta 
   NB     = pmTk(ntk).NB
   NA     = pmTk(ntk).NA
   MaxPos = pmTk(ntk).MaxPos
   posn   = pmTk(ntk).posn
   notaOld= CInt(pmTk(ntk).notaold)
   canalx= CInt(pmTk(ntk).canalsalida)
   portout=CInt(pmTk(ntk).portout)
   patchsal=Track(ntk).trk(1,1).nnn 'ubyte ambos
   
   print #1,"TrackaRoll ntk, desde, hasta, MaxPos ", ntk, desde,hasta,MaxPos
   desdevector=desde
   hastavector=hasta
   estoyEnOctava =desde
   estoyEnOctavaOld =desde
   CantTicks = MaxPos + 1000
   print #1,"TrackaRoll, NB, NA, CantTricks", NB,NA, CantTicks
' redim de ROLL de Visualizacion       
   ReDim (Roll.trk ) (1 To CantTicks, NB To NA ) ' 27-02 ÇÇÇ
   ReDim compas(1 To CantTicks)
   ReDim (RollAux.trk) (1 To CantTicks, NB To NA)
   ' redimensiono track (0)! que estara a la par de roll cargandose
   ' mientras cargo cancion podria ahcer redim a track 0 porque cargo a roll y track(0)
   ' pero como la carga de track es continua en la carga inicial no hace falta
   ' es un esfuerzo innecesario, por eso en carga cancoin no se usa TrackaRoll.
   ' Solo cargo track(0) cuadno visualizo Roll despues de la carga de cancion.
   ' si cargo Roll sin cancion se carga track(0), al grabar se grabaa en track(0)
   ' y si cargo de disco Track(0) no debo borrar track(0) al hacer track a Roll
   ' si cargo cancion no deberia cargar Roll si no al final el track 1
   ' o luego haciedno click
   '
   If CANCIONCARGADA=TRUE Then ' 01 -03
     Print #1,"CANCION CARGADA EN TRACKAROLL ntk,lim3",ntk,lim3
   Else
     Print #1,"CANCION NO CARGADA EN TRACKAROLL"
   EndIf
   If ROLLCARGADO=FALSE Then 
     Print #1,"ROLL NO CARGADO EN TRACKAROLL, maxpos",pmTk(ntk).MaxPos
   Else
       Print #1,"ROLL CARGADO EN TRACKAROLL"
   EndIf  
   If CANCIONCARGADA=TRUE And ROLLCARGADO=FALSE Then ' cargarcancion NO SE USA VIVE muy poco solo durante lA carga  
       print #1,"REDIM DE TRACK 0" ' NO SE HACE SIN CANCION O CON ROLLCARGADO"
       ' SE PREPARA TRACK 0 PARA RECIBIR DATOS DE TRACK X,PUES PARA IR A ROLL
       ' SE LLENA TRACK 0 SIEMRPE.,,,
       ReDim (Track(00).trk ) (1 To CantTicks, 1 To lim3)
   EndIf
   nota=0:dur=0
   inicioDeLEctura=0
   *po=pmTk(ntk).hasta -1
   
print #1,"TrackaRoll desde, hasta ", desde , hasta
print #1,"TrackaRoll NB, NA ,*po, MaxPos ", NB , NA, *po,MaxPos
Print #1,"1106 en trackaRoll numero de track ntk ",ntk
print #1, "ubound (Track(ntk).trk,2) ", ubound (Track(ntk).trk,2)
print #1, "lbound (Track(ntk).trk,2) ", LBound (Track(ntk).trk,2)
print #1," va a ejecutar for de copia de TrackaRoll"
Print #1," pmTk(ntk).MaxPos ",pmTk(ntk).MaxPos

' 1) carga de Roll desde track ntk
For i2 = 1 To pmTk(ntk).MaxPos 
   For i1=1 To lim2
      If Track(ntk).trk (i2,i1).nota > 0 and Track(ntk).trk(i2,i1 ).nota <=NA -13 Then '14-03-2022 na-13
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         PianoNota=Track(ntk).trk(i2,i1 ).nota
         PianoNota= PianoNota + SumarnR (PianoNota) ' para 60 es 65
         'i3= 115 -PianoNota  ' 06-09-2021 jmg para 1 a 9 octava anda ok menso no
         i3= PianoNota  ' JMG 4.3  65

      '   print #1,"indice i3 el vertical ", i3
         'i3=semitono + (*po) * 13
         ' nueva formula veremos de B=1 a C=12 bajando B->C  Ej:71 ->60
         ' para 71 PianoNota=71 + 5=76.(12-(76 - (int(76/13)*13))= 1 ..ok
         ' es ok si las notas van de 1 a 12 bajando....  
         Roll.trk(i2,i3).nota = CUByte(12 -( i3 - (Int(i3/13))*13) )
     '    Print #1,"Roll.trk(i2,i3).nota ",Roll.trk(i2,i3).nota
     '    print #1,"Roll.trk(i2,i3).nota ",Roll.trk(i2,i3).nota
         'ers=11- semitono  + *po * 13
         '    11 - semitono +(*po) * 13)  103 notacur  12
         '  11 - 12 + 9*13 = -1+ 117 = 116
         Roll.trk(i2,i3).dur  = Track(ntk).trk(i2,i1).dur
    '     Print #1,"Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
         Roll.trk(i2,i3).vol  = Track(ntk).trk(i2,i1).vol
         Roll.trk(i2,i3).pan  = Track(ntk).trk(i2,i1).pan
         Roll.trk(i2,i3).pb   = Track(ntk).trk(i2,i1).pb
         Roll.trk(i2,i3).inst = Track(ntk).trk(i2,i1).nnn
                
'''         print #1,"VEO CARGA DE ROLL Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
      EndIf
   Next i1
   If i2=pmTk(ntk).MaxPos -1 And Track(ntk).trk (i2,i1).nota > 0 Then
    i3 =Track(ntk).trk(posn,1).nota  
    i3 = i3 + SumarnR(i3)    
       Roll.trk(i2,i3+1).dur  = 182
       Roll.trk(i2,i3+1).nota =0 
       Roll.trk(i2,i3+1).vol  = 0
       Roll.trk(i2,i3+1).pan  = 0
       Roll.trk(i2,i3+1).pb   = 0
       Roll.trk(i2,i3+1).inst = 0
       
   EndIf
Next i2
Sleep 5
' [[[[[ JMG 15-01-2022 ACA DEBO COPIAR EL CAMPO DE CONTROL DE LIM3 EL 13 !!!! ]]]]] 
Dim  As Integer k,vacio
For i2=1 To pmTk(ntk).MaxPos-1
  If Track(ntk).trk(i2,13).dur = 200 Then
    For K=desde To hasta -1 
    vacio= 12 +(k -1) * 13
    Print #1,"COPIA CONTROLES A ROLL DESDE TRACK INST",Track(ntk).trk(i2,13).nnn
       Roll.trk(i2, vacio).inst = Track(ntk).trk(i2,13).nnn
       Roll.trk(i2, vacio).vol  = Track(ntk).trk(i2,13).vol
       Roll.trk(i2, vacio).nota = Track(ntk).trk(i2,13).nota ' 30
       Roll.trk(i2, vacio).dur  = Track(ntk).trk(i2,13).dur ' 200
       Roll.trk(i2, vacio).pan  = Track(ntk).trk(i2,13).pan ' alteracion
    Next K
  EndIf  
Next i2
'
Sleep 5

'copiar info de acordes de track a Roll ,.,,,
For i2=1 To pmTk(ntk).MaxPos-1
  For i1 = lim2 To lim3
      If Track(ntk).trk(i2,i1 ).pb =202  Then ' info acorde en una octava
      ' 
       Print #1,"TrackARoll hay octava de acorde a copiar a roll",Track(ntk).trk(i2,i1 ).vol
         verticalEnOctavaVacia= 12 + (hasta-2)*13 + Track(ntk).trk(i2,i1 ).vol - desde ' 90 + 6 - 4=92
         Print #1,"verticalEnOctavaVacia ",verticalEnOctavaVacia
         Roll.trk(i2,verticalEnOctavaVacia).vol   = Track(ntk).trk(i2,i1).vol ' octava
         Roll.trk(i2,verticalEnOctavaVacia ).nota = Track(ntk).trk(i2,i1).nota ' Rollnota
         Roll.trk(i2,verticalEnOctavaVacia ).dur  = Track(ntk).trk(i2,i1).dur  ' acordeNro
         Roll.trk(i2,verticalEnOctavaVacia ).pb  = 202
         vacio= 12 +(Track(ntk).trk(i2,i1).vol -1) * 13
         Roll.trk(i2,vacio).pb  =  201
      EndIf
  Next i1    
Next i2
Sleep 5
' ajuste de notas no usadas con 0, si uso 181 no se puede cargar notas
' el 181 es durante la carga para evitar retrocesos en la carga de notas
' y poder entrar acordes
' ¿y si tiene datos ? debrai pregunta por MAxpos si es mayor a 2,no hizo falta
' DUDA REVISAR JMG JJJJ NO AFECTA AL VISUALISACION DE PISTA 1 QUE NO SE PUEDE VER EN LA 
' CARG ANTES ANDABA ....SEGUIR BUSCANDO
' al traer de track a Roll Roll no tiene ninguan infoarmacion de
' nota=181 y por ess puede retroceder al cargar notas
' esto es necesario si o si al pasar de Track a Roll 05-03 ÇÇÇ
If Maxpos > 2 Then ''14-03-2022 esto ahceia empezar a grabra en posicion 2
For i2 = 1 To MaxPos -1  
  For i1=NB To NA
   If Roll.trk(i2,i1).nota =0 And Roll.trk(i2,i1).dur <>182  Then
      Roll.trk(i2,i1).nota=181
      Roll.trk(i2,i1).dur=0
   Else
'   ''print #1,i2,Roll.trk(i2,i1).nota,Roll.trk(i2,i1).dur
     Continue For   
   EndIf
  Next i1
Next i2     
EndIf
' 2)  pasar lso datos del Track(x) a Track(0)!! si es cancion
' el redim esta arriba
Print #1,"cargaCancion DEBE SER 1 ",cargaCancion 
If cargaCancion=1 Then ' solo mientras cargo cancion? no ??¿¿ ÇÇÇ 
 For i1=1 To pmTk(ntk).MaxPos 
  For i2 = 1 To lim3
  Track(0).trk(i1,i2) = Track(ntk).trk(i1,i2)
  Next i2
 Next i1
EndIf  
Sleep 5  
Roll.trk(1,NA).inst = Track(ntk).trk(1,1).nnn


'--------------------------------Track(0) fin
curpos=0
notacur=1
nroCompas=0
tres=0:pun=0:sil=0:mas=0:doblepun=0:cuart=0
tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
calcCompas(MaxPos,Roll) ' 05-03 ÇÇÇ

  print #1,"TrackaRoll Fin,,maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
print #1,"----------------------------------------------"
Sleep 10

End Sub

Sub Resetear ( pmTk() As rangoOct)
Dim i As Integer
 For i =1 To 32
   pmTk(i).desde=0
   pmTk(i).hasta=0
   pmTk(i).NB =0 
   pmTk(i).NA=0
   pmTk(i).MaxPos=0
   pmTk(i).posn=0
   pmTk(i).notaold=0
   pmTk(i).Ticks=0
   pmTk(i).portout=0
   titulos(i)=""          
 next i
 ntk=0
End Sub 
Function doscifras (NTK As Integer) As String
  Dim cifra As String 
  cifra= Str(ntk)
  cifra =Trim(cifra)
  If Len(cifra)=1 Then
     cifra="0"+cifra
  EndIf
  doscifras=cifra
End Function
' ------------------------------

Sub PlayCancion(Track() As sec)
'------------apertura e ports
Dim As Long porterror,nousar


Dim i1 As integer
' creoa todos los defaults siempre
 

' los nombres ya fueron cargados al inicio

Print #1,"abriendo ports...."

Dim k1 As Integer
For i=1 To tope
  
   k1=CInt(pmTk(i).portout)
    
   Print #1,"midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
     Print #1,"No se usa Microsoft"
   Else
     If listoutAbierto( k1) = 0 Then
        midiout(k1) = rtmidi_out_create_default ( )
        open_port midiout(k1),k1, nombreOut(k1)
            porterror=Err 
        listoutAbierto( k1) = 1
        Print #1,"abro ",*nombreOut(k1)
    Select Case porterror
      Case RTMIDI_ERROR_WARNING ' esto da cero ¿? y es eror¿?
        Print #1, "RTMIDI_ERROR_WARNING",porterror
      Case RTMIDI_ERROR_DEBUG_WARNING
        Print #1, "RTMIDI_ERROR_DEBUG_WARNING"
        Close
        End

      Case RTMIDI_ERROR_UNSPECIFIED
        Print #1,"RTMIDI_ERROR_UNSPECIFIED"
        Close
        End

      Case RTMIDI_ERROR_NO_DEVICES_FOUND
        Print #1,"RTMIDI_ERROR_NO_DEVICES_FOUND"
        Close
        End

      Case RTMIDI_ERROR_INVALID_DEVICE
        Print #1,"RTMIDI_ERROR_INVALID_DEVICE"
        Close
        End

      Case RTMIDI_ERROR_MEMORY_ERROR
        Print #1,"RTMIDI_ERROR_MEMORY_ERROR"
        Close
        End

      Case RTMIDI_ERROR_INVALID_PARAMETER
        Print #1,"RTMIDI_ERROR_INVALID_PARAMETER"
        Close
        End

      Case RTMIDI_ERROR_INVALID_USE
        Print #1,"RTMIDI_ERROR_INVALID_USE"
        Close
        End

      Case RTMIDI_ERROR_DRIVER_ERROR
        Print #1,"RTMIDI_ERROR_DRIVER_ERROR!"
        Close
        End

      Case RTMIDI_ERROR_SYSTEM_ERROR
        Print #1,"RTMIDI_ERROR_SYSTEM_ERROR"
        Close
        End

      Case RTMIDI_ERROR_THREAD_ERROR
        Print #1,"RTMIDI_ERROR_THREAD_ERROR"
        Close
        End
    End Select

     Else
        Print #1,"ya abierto ",*nombreOut(k1)
        porterror=0
     EndIf
   EndIf

  

Next i

Dim As Double tiempoDUR
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron'60 seg/ cuantas negras enun minuto

Dim As Integer i2,K, mayor,i0,xmouse,ymouse
Dim As UByte i3=0, pis=0, pisnota=0

Dim As Integer comienzo=1, final=MaxPos, vel=100,velpos =0
' canal 0 es el 1 van de 0 a 15
' pasoCol debe ser de 2 dimensiones 1 para c/pista, 115 el amxio de octavas
' ojo si cambioaamos por mas octavas debo cambiar, igual el nro de tracks 32 
''Dim pasoCol (0 To 384) As vec  ' entrada de durciones a medida que barro una columna
'------------determinamos el MAxPos de toda la cancion o sea la pista de mayor longitud
mayor=pmTk(portout).MaxPos
For i0=1 To Tope 
 If mayor < pmTk(i0).MaxPos Then
    mayor=pmTk(i0).MaxPos
 EndIf   
Next i0

final=mayor

xmouse=mousex
ymouse=mousey
'-------------
Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,Notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim As Integer result,limsup

' tope es la maxima capacidad de tracks usada , lso tracks son contiguos
' si se borra un track queda sin usar eso hay que verlo...JMG ..
' por ahora solo proceso los que tengan lim2 o si llegamos a tope 
  jply=0:curpos=0
  mousex=0
' print #1,                    "-----------------------------------------"
  comienzo=posicion
  cntold=0
  If pasoZona1 > 0 Then
    comienzo=pasoZona1 
  EndIf

  If pasoZona2 > 0 Then
    final=pasoZona2 +2
  EndIf




  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
' Esto es play de pistas el limite de barrido es 1 a 12 siempre en c/pista  
' debo hcer un for para cada pista, en cada psita ver si esta mute o play
' ajustar el instrumento de la pista cada vez que cambie la pista.

 Print #1,"[[[[ PCA CANCION MAXPOS AL COMENZAR ,final]]]",Maxpos,final

 For pis=1 To tope
      Print #1,"ON patch ntk canal ",	Track(ntk).trk(1,1).nnn, ntk,pmTk(pis).canalsalida
      ChangeProgram ( Track(pis).trk(1,1).nnn, pmTk(pis).canalsalida, pmTk(pis).portout)	

      If CheckBox_GetCheck( cbxnum(pis))= 1 Then
         Print #1,"+++++++pista on ",pis
         sonidoPista(pis)=1
      Else  
         Print #1,"+++++++pista off ",pis        
         sonidoPista(pis)=0
      EndIf


 Next pis





For jply=comienzo To final
  print #1," ---------------000000000000000000000-----------------"
  print #1," [[[PCA 0:]]]---START--PASO:[";jply;"] ----------------"
  print #1," ---------------000000000000000000000-----------------"
  
' cambio de inst para la pista, podria poner mas de un instrumento por pista
' o por cada nota.. 
' VER DE PONER LOS INSTRUMENTOS EN TRACK  
kNroCol= Int(jply/NroCol)
   If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < final)Then
     posicion=jply
     curpos=0
     SetMouse xmouse, ymouse
   EndIf

   mousex=jply
   If CONTROL1 = 1 Then
      For i3 = 1 To tope
       portsal=CInt(pmTk(i3).portout) 
       alloff(pmTk(i3).canalsalida,portsal) 
      Next i3
      CONTROL1=0
      Exit For
   EndIf  
' puede pasar que el maxpos sea menro al final de la secuencia porque se agrego espacio
 
   If Compas(jply).nro = -1 Then
     velpos=vfuerte
   EndIf
   If Compas(jply).nro = -2 Then
      velpos=vdebil
   EndIf
   If Compas(jply).nro = -3 Then
      velpos=vsemifuerte
   EndIf
   If Compas(jply).nro = -4 Then
      velpos=vdebil
   EndIf
   If Compas(jply).nro > 0 Then ' marca del numero de compas 1 2 3 4 es el ultimo tiempo del compas
      velpos=vdebil
   EndIf
   
   cnt=0
   iguales=0
   distintos=0
   duraold=0 ' 04-11-2021
  
 For pis =1 To tope ' loop de pistas

   print #1,"--loop de pistas---pista NRO :";pis;" --------------------------------"
   print #1,"  De esta pista MAXPOS ,final",pmTk(pis).MaxPos, final
    limsup=UBound (Track(pis).trk,2)
    If limsup < lim2 Then
          Exit For 
    EndIf  
    If pis > tope Then
       Exit For
    EndIf

'If Track(pis).trk(1,1).inst > 0 Then
'  Print #1,"CAMBIO PATCH PISTA ",pis,"instru ",Track(pis).trk(1,1).inst
'  ChangeProgram ( Track(pis).trk(1,1).inst, pmTk(ntk).canalsalida,pmTk(ntk).portout)
'EndIf  

' ojo con silencios ligados !!!

' limites para cada pista NB,NA,son limites para las Notapiano
' si se grabo con un limite de octavas las Notapiano tambien estan limitadas  
   NB = 0  + (pmTk(pis).desde -1 ) * 13 
   NA = 11 + (pmTk(pis).hasta -1 ) * 13 
'   print #1,"Play Cancion NB ,NA, pis",NB,NA,pis
   
   Dim As ubyte NBpiano= CUByte(NB) - CUByte(restar(NB))
   Dim As ubyte NApiano= CUByte(NA) - CUByte(restar(NA))
'   print #1,"limite inferior NBpiano ",NBpiano
'   print #1,"limite Superior NApiano ",NApiano
' en realida seria bueno no tner limites para los tracks, cada track
' tiene polifonia de 12 o sea un acorde de 12 notas, ergo
' la cancion podria tener 12*32tracks = 384 de polifonia, no se si rtmidi lo soporta
' info del inst en 1 no en lim2   

 Print #1,"FOR -- RECORRIDO DE NOTAS DE PISTA", pis
   For i1=1 To lim2   ' coo voy de 1 a lim2 necesito que la info del int este en 1

     If (Track(pis).trk(jply,i1).nota >= NBpiano) And (Track(pis).trk(jply,i1).nota <= NA) And (Track(pis).trk(jply,i1).dur >=1) And (Track(pis).trk(jply,i1).dur <= 180) Then ' es semitono
         Notapiano = CInt(Track(pis).trk(jply,i1).nota)
'         print #1,"Notapiano ",Notapiano
'         print #1,"i1,NBpiano,NApiano ",i1,NBpiano,NApiano
'       print #1,"VEO LO CORRECTO DE NOTAPIANO "; Notapiano
         dura=Track(pis).trk(jply,i1).dur ' el dur de track esta en integer
'      print #1,"jply ";jply; "dura ";dura
         cnt=cnt+1
   '   print #1,"paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
        If cnt=1 Then 
           duraOld=dura
        EndIf
       ' usamos reldur() para comparar dura y con cnt>1 tiene sentido
       If reldur(duraOld)=reldur(dura) And cnt > 1  Then
          iguales=1
'          print #1,"cnt ";cnt;" iguales ";iguales
       EndIf
       If reldur(duraOld)<>reldur(dura) And cnt > 1 Then
          distintos=1
 '         print #1,"cnt ";cnt;" distintos ";distintos
       EndIf         

'        print #1,"-> cnt"; cnt 
         pasoCol(cnt).DUR =dura
'         pasoCol(cnt).DURold =dura
'         print #1,"pasoCol(cnt).DUR ", pasoCol(cnt).DUR 
         If pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <=180 Then
'            print #1,"nota con + es una liga"
            pasoCol(cnt).liga    =  1
         Else   
            pasoCol(cnt).liga  = 0
         EndIf
        ' DURACIOENS SILENCIO O NO sF o sF+
        If (pasoCol(cnt).DUR >= 1 And pasoCol(cnt).DUR <=45) Or (pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <= 135) Then
          '  print #1,"PALL 9a:PALL 0: nota tiene audio"
            pasoCol(cnt).audio =  1 ' tiene audio
        Else
            pasoCol(cnt).audio =  2 ' no tiene audio, 0 valor no ajustado no se nada  
        EndIf

         pasoCol(cnt).notapiano=Notapiano
 ' >>>>>>>>>>>>>>>CANAL MIDI y POR SALIDA >>>>>>>>>>>>>>>>>>>>>>>>        
         pasoCol(cnt).canal=pmTk(pis).canalsalida ' 12-02-2022 canal en pasoCol
         Print #1,"pasocol guarda canal, pista --> ",pasoCol(cnt).canal," estoy en pista ",pis
         pasoCol(cnt).port=pmTk(pis).portout
         Print #1,"pasocol guarda port , pista --> ",pasoCol(cnt).port," estoy en pista ", pis  
         pasoCol(cnt).pista=pis       
         Print #1,"pasocol guarda pista  --> ",pasoCol(cnt).pista
 ' >>>>>>>>>>>>>>>CANAL MIDI >>>>>>>>>>>>>>>>>>>>>>>>         
         
         pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * d11
         pasoCol(cnt).i1 = i1 'posicion vertical en el vector real
  '       pasoCol(cnt).i1Old = i1 'posicion vertical en el vector real
         If sonidoPista(pis)=1 Then
            pasoCol(cnt).vol=velpos
         Else
            pasoCol(cnt).vol=0
         EndIf   
      ' 20-06-2021 eliminado duraold=dura repetido 
     '' ??VER SI SAO DE ACA INST Y VEL 
        ' If sonidoPista(pis)=1 Then
  
         ''vel=CUByte(vol( dura, velpos))
     EndIf
' llegamos al final de la Columna
   
     If i1=lim2  And (pis = tope Or pis=32) Then
        ''''  reponer mouse_event MOUSEEVENTF_MOVE, 1, 0, 0, 0
        If cnt > 1 Then' Acorde
          print #1,"i1=";i1 ; " ACORDE cnt= ";cnt
        Else    
           print #1,"i1=";i1 ; " SIMPLE cnt= ";cnt
        EndIf  
  
        
        Select Case cnt
          Case 1 
           ' con y sin liga
           ' INCLUYE LIGADOS O NO LIGADOS
' print #1, "SIMPLE COM ACORDES IGUALES cntold, vel, tiempodur",  cntold, vel, tiempoDur
  ' cnt por cntold 04-11-2021           
  ''    noteSimple  pasoCol(), cnt, vel, canal,tiempoDur,velpos
         TipoAcorde=1 ' simple   
         AcordeIguales pasoCol(), cnt, cntold, vel,tiempoDur,Roll,velpos,pisnota,portsal
         pasoCol(cnt).notapianoOld    = Notapiano             
                     
          Case Is > 1

            If iguales=1 And distintos=0  Then
                TipoAcorde=2 ' iguales
                print #1,"cnt ";cnt;" Acordeiguales "
                
                AcordeIguales pasoCol(),cnt,cntold,vel,tiempoDur,Roll,velpos,pisnota,portsal
                
            EndIf
            If  distintos=1 Then
               print #1,"cnt ";cnt;" AcordeDistintos"
                TipoAcorde=3 ' distintos               
                AcordeDistintos pasoCol(),cnt, cntold,vel,tiempoDur,Roll,velpos,pisnota,portsal
                
            EndIf
            
         End Select  

        cntold = cnt
        print #1,"cnt,cntold"; cnt;" ";cntold
         
     EndIf
     
   Next i1
' usando variblkes staticas no hace falta usar esta trampita   
   ''''  reponer mouse_event MOUSEEVENTF_MOVE, 1, 0, 0, 0
  print #1,"---DOWN -----PISTA:"; pis;" --------------------------------" 
    
 If playloop=1 And jply= final Then
    jply=comienzo
    'posicion=comienzo
 EndIf
 tiempoDUR=(60/tiempoPatron) / FactortiempoPatron '13-07-2021 cambiamos velocidad durante el play!!!

  Next pis   
  print #1,"---FIN -----paso:"; jply;" --------------------------------"
Next jply


posicion=comienzo
'posishow=posicion + 20
'posishow=posicion - 20
'posicion=posicion -20
 
jply=0:curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
 
playb=0
Cplay=0 ' ontrol de play cancion si fue desde control
mousey=100 'otra mas para evitar rentrar a play en menu
finplay=1

'''  reponer mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
''fueradefoco=0
 SuenaTodo=0 ' habilita boton cotrol sonido de peistas
 SetMouse xmouse, ymouse 
Sleep 10,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
/'
close_port(midiout)
out_free(midiout)
'/ 

For i=1 To tope
   k1=pmTk(i).portout
   Print #1,"midiout ",k1, *nombreOut(k1)
   alloff( pmTk(i).canalsalida,k1 )  
   'out_free   midiout(k1)
   ''Print #1,"desmarco ",*nombreOut(k1)
   listoutAbierto(k1)=0
   close_port midiout(k1)
'   out_free   midiout(k1)
   listoutAbierto( k1) = 0
Next i
ThreadDetach(thread1)



End Sub 

Sub FraccionarDur (Track() As sec,Roll As inst, indicePos As Integer, dura As ubyte,nR As Integer, ntk As integer)
' npo= notapiano, posi =posicion
' DADA una Notapiano selecionada con cursor o click
' entrar argumento de division con las figiuras 2,3,4,5,6,7
' o sea blanca,negra,corchea,semicorchea, fusa , semifusa
' luego reemplazar la Nota elegida por N+ desplazar todo el vector hacia la derecha
' desde la posicion +1 desde la nota en la cantidad de posiocine snecesarias para
' completar al duracion deseada. ej I=F+F+F+F O I=L+L, P=I+I,P=L+L+L+L
' ETC. DE ESE MODO DEJO INGRESAR NOTAS MAS CHICAS EN UN PASO DADO SIN MALGASTAR
' POSICIONES. podriamso ahcer un vector tridimensional o sea en esa columna
'1) determinar la MAxPos de c/pista (moverpista lo hara autoamticamente)
'2) determinar la columna donde esta la nota a expandirse en n fracciones
'  res la entrada parametro posi
'3) determinar n segun entrada de usuario fracciones de P,I,L,F,E,W
' en RollLoop coocar ctrl+alt+x un if con la cifra que se entra,,,
'4) DETERMIANR EN CUANTAS PARTES SE FRACCIONA LA NOTA NP (NPARTES)
' deducirla con al duracion de la nota exsitente en la pista o roll y la 
'  cifra introducida por el usuario ej I (2),,,usuario:5 -> I fraccionada como F
' seran 4 F hace una I son 4 partes 1 la origial pas de I a F+ y las otras 3
' 2 F+ y una final F osea F+F+F+F  
' 5) RESTAR 1 A NP, NP-1.. (4 -1)
' 6) MOVER EN CADA PISTA NP-1 (3) POSICIONES A LA DERECHA DE LOS DATOS.
' 7) DETERMINAR LA NUEVA MAXPOS DE C/PISTA SUMANDOLE NP-1 (3)
' FIN
' USO: COMADNO CTRL+Z ANTES DE PULSAR EL COMANDO SE ESPERA UN NUMERO DE
' FIGURA A USAR 2,3,4,5,6,7,8=P,I,L,F,E,W,H, O SEA FRACCIONAR EN BALNACAS, NEGRAS
' CORCHEAS, SEMI CORCHEAS, FUSA, SEMIFUSA,H, LSO TRESILLOS OCUPAN 2 FIGURAS MAS CHICAS
' ESO SE MANEJA CON ASL NOTAS INTRIDUCIDAS PERO LAS POSICIOENS SIEMRPE SON
' LAS ENUMERADAS...EN ELLAS SE PODRA PONER DURACIONES TIPO TRESILLOS,,,PARA USARLAS

' tengo dur a cambiar para partirla debera ser mas chica
' si el usuario puso 5 es una F semicorchea, siempre se mueve a derecha del vector
' en rolldec o hacia arriba en indice,,,y cada salto se multiplica en 2 la figura
' si es 3 una negra para llegar a 5 tengo 5-3= 2 saltos de 2 c/u =>
' tengo 4 figuras de 5 FFFF, pero las 3 1eras deben estar ligadas para obtener las ligadas
' le sumo 90 a la posicion de la figura..5+90=95=F+
' ls posicion de ,ovimiento final total es 4 -1 , ergo muevo 3 posiciones todo el 
' resto de la secuenci aasi libero 3 posiciones la actual se reemplaza por F+
' 2 mas con F+ y un aultima con F. Osea a posi le sumo 3 en este caso
' delta*2 -1 
Dim As Integer posivieja,durfig,posinueva,i,j,pis=0,delta=0, nroPartes=0, nroligadas=0,indiceligada,indicenoligada

print #1,"Rolldur ",Rolldur

Print #1,"numeroFrac DUR", numeroFrac, DUR
If numeroFrac >=1 And numeroFrac <= 180 Then
Else
  Exit Sub
     
EndIf
' numeroFrac es la DUR real con tresillo silencio puntillo etc SALE DE ArmarDurFrac()
durfig=pesoDur(numeroFrac) ' la sdiferencia se calculas con el peso real 
DUR = 0 ' reseteo para la proxima entrada.
cifra=""
digito=""
    
If durfig=0 Then
  Exit Sub
EndIf
' se supone que durfig es la menor con la cual dividir a RollDur ergo siempre RollDur > durfig 
delta= pesoDur(Rolldur) -durfig

print #1,"DUR de usuario, delta ",durfig,delta
' nropartes es integer ergo redondea, el cociente da el nro de partes

nroPartes=pesoDur(Rolldur) /durfig ' tengo la cantidad de figuras=nroPArtes
posivieja=indicePos
posinueva=indicePos + nroPartes ' POSICION DONDE CONTINUA LA SECUENCIA 

print #1,"nropartes =",nropartes
print #1,"muevo  a ",posinueva
' nroPartes -1
nroligadas=nroPArtes -1 
print #1,"nroligadas ",nroligadas
indiceligada= numeroFrac + 90 ' indice de la ligada correpsondiente
print #1,"indiceligada ",indiceligada
indicenoligada=numeroFrac 
'print #1,"FIGURA A CAMBIAR ",figura(DUR)
print #1,"FIGURA de REEMPLAZO LIGADA ",figura(INDICELIGADA)
print #1,"FIGURA de REEMPLAZO NO LIGADA ",figura(indicenoligada)
' entonces tengo nroligadas columnas de figura indiceligada
' y una ultima columna de indice de  figura (numusuario)
If CANCIONCARGADA =TRUE Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION?
' si vamoa a fracionar toda la cancion al fraccionar una pista, podemos
' detectar si maxpos de la pista a fraccionar es mayor a la posicion fraccionada,
' si lo es, fraccionamos, y si no lo es, se saltea....ahorramos trabajo
' 
' muevo N tracks y Roll en edicion
   For pis= 1 To tope
 '     moverPista pis
 ' aca deberemos fraccionar Tracks en vez de Rolls
   Next pis
 '     moverRoll 
Else ' FRACCIONAR LA PISTA CARGADA
' muevo 1 track y Roll de edicion
 '--  moverPista  0 ' solo mueve pista cero
 '--  moverRoll 
   ' posi es la not a partir luego se reemplaza por otra figura peo se mueve
   ' a derecha desde posi+1
' ind de moverZonaRoll es donde empezara los datos movidos
' en este caso sera n divisiones -1 + posicion


'''Rolldur=Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur

'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  moverZonaRoll(posinueva, Roll,posivieja) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nE usar
 ' nE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
      If Roll.trk(posivieja,j).dur = Rolldur Then
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
        Next i
     EndIf 
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
ntk=0 
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

EndIf
 
End Sub
''
Sub FracTodoDur (Track() As sec,Roll As inst, indicePos As Integer, dura As ubyte,nR As Integer, ntk As integer)
' Generalizacion de FraccionarDur.Esto servira para ambos casos al estar en lectura y quiero
' abrir como antes o desde cursor al ingresar notas. En ambo scasos lo hará en mas de 2 notas
' recibira la menro y mayor oco antes determinado por la sub ya echa menoryMayorEncolumna
' lugo determino partes con la mayor, fracciono. Y toa nota menor a la mayor y mayor ala menro 
' o sea intermedia se contrastara con la menor y asi determinamos cuantas menores van en esa
' nota media se llenara con ellas y lo que falta ocmpletar de esa media y hasta las partes de
' la mayor se llenaran con silencios de la menor en cuestion 

Dim As Integer posivieja,durfig,posinueva,i,j,pis=0,delta=0, nroPartes=0, nroligadas=0,indiceligada,indicenoligada
Dim As Integer otronroligadas=0,otroindiceligada,otroindicenoligada
' dura es la mayor o la nota sobre la cual me poso con el mouse y le doy click para fraccionar
print #1,"dura ",dura

Print #1,"numeroFrac DUR", numeroFrac, DUR 
If numeroFrac >=1 And numeroFrac <= 180 Then ' entrada de usuario que podra ser menor auomatico
Else
  Exit Sub
     
EndIf
' numeroFrac es la DUR real con tresillo silencio puntillo etc SALE DE ArmarDurFrac()
durfig=pesoDur(numeroFrac) ' peso de la mneor con la que fraccionare,la diferencia se calculas con el peso real 
DUR = 0 ' reseteo para la proxima entrada.
cifra=""
digito=""
    
If durfig=0 Then
  Exit Sub
EndIf
' se supone que durfig es la menor con la cual dividir a RollDur ergo siempre RollDur > durfig 
delta= pesoDur(dura) -durfig

print #1,"DUR de usuario, delta ",durfig,delta
' nropartes es integer ergo redondea, el cociente da el nro de partes

nroPartes=pesoDur(dura) /durfig ' tengo la cantidad de figuras=nroPArtes
posivieja=indicePos
posinueva=indicePos + nroPartes ' POSICION DONDE CONTINUA LA SECUENCIA 

print #1,"nropartes =",nropartes
print #1,"posinueva ",posinueva
' nroPartes -1
nroligadas=nroPArtes -1 
print #1,"nroligadas ",nroligadas
If numeroFrac <= 90 Then
indiceligada= numeroFrac + 90 ' indice de la menro o du rde usuario ligada correpsondiente
Else
indiceligada= numeroFrac
EndIf
print #1,"indiceligada ",indiceligada
If numeroFrac <= 90 Then
indicenoligada=numeroFrac
Else
indicenoligada=numeroFrac -90
EndIf 

'print #1,"FIGURA A CAMBIAR ",figura(DUR)
print #1,"FIGURA de REEMPLAZO LIGADA ",figura(INDICELIGADA)
print #1,"FIGURA de REEMPLAZO NO LIGADA ",figura(indicenoligada)
' entonces tengo nroligadas columnas de figura indiceligada
' y una ultima columna de indice de  figura (numusuario)
If CANCIONCARGADA =TRUE Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION?
' si vamoa a fracionar toda la cancion al fraccionar una pista, podemos
' detectar si maxpos de la pista a fraccionar es mayor a la posicion fraccionada,
' si lo es, fraccionamos, y si no lo es, se saltea....ahorramos trabajo
' 
' muevo N tracks y Roll en edicion
''>>>   For pis= 1 To tope
 '     moverPista pis
 ' aca deberemos fraccionar Tracks en vez de Rolls
'>>>>   Next pis
' -=-=-=----------fracciono el roll y luego roll a track
Dim As Integer otroPesoDur, otronroPartes, indiceLigadasilencio, indiceNoLigadasilencio
'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  moverZonaRoll(posinueva, Roll,posivieja) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nE usar
 ' nE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
        Next i
        Case Else

           otroPesoDur    = pesoDur(Roll.trk(posivieja,j).dur)
           otronroPartes  = otroPesoDur /durfig ' I/L --2
           otronroligadas = nroligadas ' igual que antes solo aque alguans son silencio y otras no 
           
           indiceLigadasilencio   = numeroFrac + 45  + 90
           indiceNoLigadasilencio = numeroFrac + 45 
           
           ' indiceligada se usa el mismo 
       ' tengo el nro de partes que se parte esa otra dur del medio ni menorni mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nE)
            
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
ntk=0 
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

ReCalCompas (Roll)
   
'-----------------------------------------   
 '     moverRoll 
Else ' FRACCIONAR LA PISTA CARGADA
' muevo 1 track y Roll de edicion
 '--  moverPista  0 ' solo mueve pista cero
 '--  moverRoll 
   ' posi es la not a partir luego se reemplaza por otra figura peo se mueve
   ' a derecha desde posi+1
' ind de moverZonaRoll es donde empezara los datos movidos
' en este caso sera n divisiones -1 + posicion


'''Rolldur=Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur
dim As Integer otroPesoDur, otronroPartes, indiceLigadasilencio, indiceNoLigadasilencio
'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  moverZonaRoll(posinueva, Roll,posivieja) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nE usar
 ' nE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
        Next i
        Case Else

           otroPesoDur    = pesoDur(Roll.trk(posivieja,j).dur)
           otronroPartes  = otroPesoDur /durfig ' I/L --2
           otronroligadas = nroligadas ' igual que antes solo aque alguans son silencio y otras no 
           
           indiceLigadasilencio   = numeroFrac + 45  + 90
           indiceNoLigadasilencio = numeroFrac + 45 
           
           ' indiceligada se usa el mismo 
       ' tengo el nro de partes que se parte esa otra dur del medio ni menorni mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nE)
            
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
ntk=0 
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

ReCalCompas (Roll)

EndIf
 
End Sub

Sub AutoFracTodoDur (Track() As sec,Roll As inst, indicePos As Integer, dura As ubyte,nR As Integer, ntk As integer)
' Automaticamnete tomara la mayor y menor duracion y fraccionara por la menor, sea la menro existente
' o la menor incluida la entrada de usuario.
' Auto matizacion de FracTodoDur.Esto servira para ambos casos al estar en lectura y quiero
' abrir como antes o desde cursor al ingresar notas. En ambo scasos lo hará en mas de 2 notas
' recibira la menro y mayor oco antes determinado por la sub ya echa menoryMayorEncolumna
' lugo determino partes con la mayor, fracciono. Y toa nota menor a la mayor y mayor ala menro 
' o sea intermedia se contrastara con la menor y asi determinamos cuantas menores van en esa
' nota media se llenara con ellas y lo que falta ocmpletar de esa media y hasta las partes de
' la mayor se llenaran con silencios de la menor en cuestion
'----------
Print #1,">>>> START AutoFracTodoDur "
   Dim As UByte menor, mayor
   Dim As Integer i1men,i1may,posdur
' en modo lectura la posicion de una nota en el vector es posdur, la 1er parte es en la pantalla
' la 2da posishow es en el vector hasta el inicio de la pantalla 
   posdur= (mousex- gap1 )/anchofig + posishow
   Print #1,"posdur ",posdur
   If posdur < 1 Then ' 08-03-2022 servira?
      Exit sub
   EndIf
   menoryMayorEnColumna (Roll, posdur,menor,mayor,i1men,i1may)
   Print #1,"menoryMayorEnColumna, posdur,menor,mayor,i1men,i1may ", posdur,menor,mayor,i1men,i1may
   If menor=mayor Then  ' 06-12-2021
    Print #1,"menor=mayor  no se procesa Use otra opcion"
      Exit Sub 
   EndIf
' saco la info para usar Fraccionador...pero ojo fraccionador solo reemplaza a notas
' de igual duracion no de distintas, debere hacer un nuevo fraccionador ? o generalizo
' el existente y lo parametrizo   
   Print #1,"AutoFracTodoDur: menor,mayor,i1men,i1may ",menor,mayor,i1men,i1may
 
'-------
Dim As Integer posivieja,durfig,posinueva,i,j,pis=0,delta=0, nroPartes=0, nroligadas=0,indiceligada,indicenoligada
Dim As Integer otronroligadas=0,otroindiceligada,otroindicenoligada
' dura es la mayor o la nota sobre la cual me poso con el mouse y le doy click para fraccionar
print #1,"dura ",dura
' si el usuario dio click a una nota en pantalla que no es la mayor corregimos 
If pesoDur(dura) < pesoDur(mayor)  Then ' error corregimos no puede ser mayor a menor
   dura=mayor
EndIf
Print #1,"dura ",dura

' numeroFrac es lo q entra el usuario por teclado , sino entro nada se toma la menor

Print #1,"numeroFrac DUR", numeroFrac, DUR 
If numeroFrac= 0 Then
   numeroFrac=menor
EndIf

If numeroFrac >=1 And numeroFrac <= 180 Then ' entrada de usuario que podra ser menor auomatico
Else
 Print #1,"se sale numeroFrac no compatible 1 a 180 ",numeroFrac
  Exit Sub
     
EndIf
' numeroFrac es la DUR real con tresillo silencio puntillo etc SALE DE ArmarDurFrac()
durfig=pesoDur(numeroFrac) ' peso de la mneor con la que fraccionare,la diferencia se calculas con el peso real 
DUR = 0 ' reseteo para la proxima entrada.
cifra=""
digito=""
    
If durfig=0 Then
  Exit Sub
EndIf
' se supone que durfig es la menor con la cual dividir a RollDur ergo siempre RollDur > durfig 
delta= pesoDur(dura) -durfig

print #1,"DUR de usuario, delta ",durfig,delta
' nropartes es integer ergo redondea, el cociente da el nro de partes

nroPartes=pesoDur(dura) /durfig ' tengo la cantidad de figuras=nroPArtes
posivieja=indicePos
posinueva=indicePos + nroPartes ' POSICION DONDE CONTINUA LA SECUENCIA 

print #1,"nropartes =",nropartes
print #1,"posinueva ",posinueva
' nroPartes -1
nroligadas=nroPArtes -1 
print #1,"nroligadas ",nroligadas
If numeroFrac <= 90 Then
indiceligada= numeroFrac + 90 ' indice de la menro o du rde usuario ligada correpsondiente
Else
indiceligada= numeroFrac
EndIf
print #1,"indiceligada ",indiceligada
If numeroFrac <= 90 Then
indicenoligada=numeroFrac
Else
indicenoligada=numeroFrac -90
EndIf 

'print #1,"FIGURA A CAMBIAR ",figura(DUR)
print #1,"FIGURA de REEMPLAZO LIGADA ",figura(INDICELIGADA)
print #1,"FIGURA de REEMPLAZO NO LIGADA ",figura(indicenoligada)
' entonces tengo nroligadas columnas de figura indiceligada
' y una ultima columna de indice de  figura (numusuario)
If CANCIONCARGADA=TRUE  Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION? PENDIENTE....
' si vamoa a fracionar toda la cancion al fraccionar una pista, podemos
' detectar si maxpos de la pista a fraccionar es mayor a la posicion fraccionada,
' si lo es, fraccionamos, y si no lo es, se saltea....ahorramos trabajo
' 
' muevo N tracks y Roll en edicion
   For pis= 1 To tope
 '     moverPista pis
 ' aca deberemos fraccionar Tracks en vez de Rolls
   Next pis
 '     moverRoll 
Else ' FRACCIONAR SOLO LA PISTA CARGADA
' muevo 1 track y Roll de edicion
 '--  moverPista  0 ' solo mueve pista cero
 '--  moverRoll 
   ' posi es la not a partir luego se reemplaza por otra figura peo se mueve
   ' a derecha desde posi+1
' ind de moverZonaRoll es donde empezara los datos movidos
' en este caso sera n divisiones -1 + posicion


'''Rolldur=Roll.trk(indicePos,(12-nE +(estoyEnOctava -1) * 13)).dur
dim As Integer otroPesoDur, otronroPartes, indiceLigadasilencio, indiceNoLigadasilencio
'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  moverZonaRoll(posinueva, Roll,posivieja) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nE usar
 ' nE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
        Next i
        Case Else

           otroPesoDur    = pesoDur(Roll.trk(posivieja,j).dur)
           otronroPartes  = otroPesoDur /durfig ' I/L --2
           otronroligadas = nroligadas ' igual que antes solo aque alguans son silencio y otras no 
           
           indiceLigadasilencio   = numeroFrac + 45  + 90
           indiceNoLigadasilencio = numeroFrac + 45 
           
           ' indiceligada se usa el mismo 
       ' tengo el nro de partes que se parte esa otra dur del medio ni menorni mayor
           nE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nE)
            
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            Else
 '           print #1,"DENTRO 2) i, j,nE ", i,j,nE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nE)
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
ntk=0 
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

ReCalCompas (Roll)

EndIf

End Sub