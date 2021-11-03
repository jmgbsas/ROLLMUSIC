
Sub CargarTrack(Track() As sec, ByRef ntk As Integer , ByRef ubirtk As Integer)
' solo carga track no lo pasa a Vector de Visualizacion. ntk debe venir informado
' para cargar desde disco un track se usa esta sub y luego TrackaRoll para verlo
' y editarlo
  print #1,"----------------------------------------------"
  Print #1,"1) ENTRA A CargarTrack nombre, ntk= ",nombre , ntk   
     Dim z (1,1 )  As poli
     Dim zLim (1,1) As poli
     Dim As Integer ubi1,ubi2 
     Dim As String x,x1,x2,x3,x4,x5,nombrea
  '1) cargar pista desde disco y desde Roll puro   
     If ubirtk=0  Then ' no tengo nombre debo explorar
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
          Print #1,"ubirtk > 0 carga de disco o cancion ",ubirtk
          nombrea=titulos(ntk) ' ya venia el nombre
          ubirtk=0
       EndIf   
     EndIf
       If nombrea = "" Then
       Print #1, "cargaTrack exit sub"
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
    Dim ct As Integer
    ct=FreeFile
    Dim miroerr As Integer
    Print #1,"NTK Y nombre que llego a open en CargaTrack ",ntk ,nombre
    titulos(ntk)=nombre
    
    miroerr= ( Open (nombre  For Binary Access Read As #ct ))
     If miroerr <> 0 Then
       Print #1,"arch track  abrio con error 1307 CargarTrack miroerr, nombre",miroerr, nombre
       Exit sub
     EndIf
        
     Get #ct, , z(1,1)
     x1=Bin(z(1,1).nota,4)
     x2=Bin(z(1,1).dur,4)
     x3=Bin(z(1,1).vol,4)
     x4=Bin(z(1,1).pan,4)
     x5=Bin(z(1,1).pb,4)
     x=x1+x2+x3+x4+x5
        Print #1,"reconstruccion x pos bin ", x
     'toda carga de track se guarda en pmTk sea ntk=0 u otro valor   
     pmTk(ntk).MaxPos=CInt("&B"+x)   
     Print #1,"pmTk(ntk).MaxPos ", pmtk(ntk).MaxPos
     '|--> LLEVAR A TRACK A ROLL posicion = 1
     '|--> LLEVAR A TRACK A rOLL nota=0 '''notaOld
     '|--> LLEVAR A TRACK A rOLL    inicioDeLectura=0' Int(Maxpos/NroCol)
' ==> aca no lo esta cargadno a roll visual solo a un track ntk
     pmTk(ntk).posn=pmTk(ntk).MaxPos - 2
     If pmTk(ntk).posn < 0 Then pmTk(ntk).posn=0 EndIf
     pmTk(ntk).Ticks = pmTk(ntk).MaxPos + 500
     'es un get trabajo debe ser exactamente MAxPos
     ReDim Trabajo  (1 To pmTk(ntk).MaxPos,1 To lim2) As poli 

     ' crgamos limites Roll de octavas
     Get #ct, , zLim(1,1)
     pmTk(ntk).desde  = CInt(zLim(1,1).nota)
     pmTk(ntk).hasta  = zLim(1,1).dur
     pmTk(ntk).notaold= CInt(zLim(1,1).pb)
     Print #1,"desde ",pmTk(ntk).desde
     Print #1,"hasta ",pmTk(ntk).hasta
     
     Print #1,"en la carga de track desde hasta", pmTk(ntk).desde,pmTk(ntk).hasta
     pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13   ' 27 para 3 SI CARGO CANCION NO VA
     pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13  ' 90 para  7 06-09-2021 decia 12 -> es 11
     Print #1,"CargarTrack  NB Na", pmTk(ntk).NB, pmTk(ntk).NA
     Print #1,"cargaCancion ",cargacancion
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
     If cargaCancion=0  Then ' termino la carga de cancion es otro evento despues
        NB=pmTk(ntk).NB  ' por ejemplo cargar un track clickeado en lista
        NA=pmTk(ntk).NA
        desde=pmTk(ntk).desde
        hasta=pmTk(ntk).hasta
        MaxPos=pmTk(ntk).MaxPos
        notaOld=pmTk(ntk).notaold
        posn=MaxPos -2
        If posn< 0 Then posn=0 EndIf
        posicion=0
        curpos=0
        CantTicks=pmTk(ntk).Ticks
     EndIf
     ' configuro el track receptor
     ReDim (Track(ntk).trk) (1 To pmTk(ntk).Ticks, 1 To lim2)
     Print #1,"*PO = hasta -1 ",  pmTk(ntk).hasta -1
     Print #1,"Get TrK POLI "
     Dim errget As Integer
     errget= Get( #ct, , Trabajo()) 
     If errget <> 0 Then
        Print #1,"error en Geteo "
     Else
        Print #1,"Geteo Trabajo poli ok "
     EndIf
     ' movemos los datos a Track
     ' -------------------------
     'ReDim compas(1 To CantTicks) es solo para visualizar
     'carga=1 ' para visualizar no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
     Print #1,"pmtk(ntk).MaxPos, ntk ",pmTk(ntk).MaxPos , ntk
     Print #1,"ABRIR MAXPOS ,NB,NA "; pmTk(ntk).MaxPos, pmTk(ntk).NB,pmTk(ntk).NA
     Dim As Integer  grupo, mayorgrupo

     For j = 1 To pmTk(ntk).MaxPos  -2 '11-07-2021
    '   Print #1,"POSICION :",J
    '  Print #1,"lim2 :", lim2
      For i= 1 To lim2
      'Print #1,"comienaa FOR i ",i
    '  Print #1,"carga i acorde ",i
      Track(ntk).trk(j,i)  => Trabajo(j,i)  ' <-- aca va copiando tambien acorde
    '  Print #1,"luego de carga i acorde ",i  
' cargar en visualizacion con otra sub si es necesario
      Next i

     Next j
     cerrar (ct)

     DUR => 0
     curpos =>1

     '''''ReCalCompas(Roll) '''No es vEctor de Visualizacion      

      Print #1,"CargarTrack  fin"  
      print #1,"----------------------------------------------" 
 Sleep 100 ' retardo para que se ubiquen lso datos en memoria ¿? parece necesario
End Sub
' backup
/' backup roll atrack 
Sub GrabarRollaTrack (cambiaNombre As Integer, enCancion As Integer, copia As boolean )
' las 3 1eras funciones pueden hacerce en roll sin problemas..roll lo deduce
' de la entrada y las variables globales. la 4ta no debe llamarse desde Control
'1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia   
'2) convierte un Roll a trk y graba en cancion, igual que antes pero 
 ' lo pone en cancion con un nro de track nuevo, esto esta solo en Roll
 ' por ahora no en Control..exclusivo de Roll (1,1,FALSE)
'3) si no paso por 1) ni 2)  y es un track el nombre es de un track logicamente
' no hace falt hacer nada el nombre esta completo por ejemplo [03]Guitarra.rtk
' sobreescribe el track (0,0,FALSE)  

Print #1,"---------------------------------------------------------------------------------"
Print #1,"inicia GrabarRollaTrack en Cancionfunciona como un Dir y contador de archivos ",cambianombre,enCancion 
   Dim As String no1, no2,ext
   Dim As Integer ubi1=0,ubi2=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de rool a track, i tengo una cancion en edcion
' y queiro un roll nuevo no conviene hacerlo aca si hay cancion no se usa
    ubi1=InStrRev(nombre,"\")
    ubi2=InStrRev(nombre,".")
    no1= Mid(nombre,1,ubi1) ' path
    no2= Mid(nombre,ubi1+1,ubi2-1-ubi1) ' nomre archivo sin extension
    ext= LCase(Mid(nombre,ubi2)) ' contiene el punto .rtk .roll
    Print #1,"extension ",ext
    Print #1,"no2 nombre sin extension ni path ",no2
    ' convierte rool a rtk sin cancion y lo graba a  track 0
    ' O SEA CANCIONCREADA=FALSE CANCIONCARGADA=FALSE
 '1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia
If  ROLLCARGADO=TRUE And copia=FALSE Then     
   If cambianombre > 0 And nombre > ""  And enCancion=0  Then  
      nombre=no1+"[00]"+no2 +".rtk" 'path + 0 + rtk por default si no hay cancion
      Print #1,"armado de nombre ",nombre
   EndIf
 '2) convierte un Roll a trk y graba en cancion, igual que antes pero 
 ' lo pone en cancion con un nro de track nuevo, esto esta solo en Roll
 ' por ahora no en Control..exclusivo de Roll (1,1,FALSE)
   If enCancion > 0 Then ' BUSCO EL NUMERO DE PISTA PROXIMO
     Dim As String filename, filenameold
     filename = Dir (NombreCancion+"\*")
     Print #1,"filename encancion > 0 ",filename
     If filename = "" Then ' no hay ningu archivo dentro del dir de cancion
        ntk=1
        CANCIONCREADA=TRUE
        Print #1,"dice que no hay archivos sera el 1ero"
     Else
 ' BUSCA NUMPISTA ntk ULTIMO Y LE SUMA 1       
       Do While Len(filename) > 0 ' If len(filename) is 0, exit the loop: no more filenames are left to be read.
        filenameold=filename
        Print #1, "trabajo con este filename...", filenameOld
        Dim cadena As String
        ubi1=InStrRev (filenameOld,".")
        cadena=Mid(filenameOld,1,ubi1-1)
       '' AddListBoxItem(3, cadena)
        ubi1=InStr(filenameold,"[")
        ubi2=InStr(filenameold,"]")
        no1 = Mid(filenameold,ubi1+1,ubi2-1-ubi1)
       
        ntk=CInt(no1)
  '      
        filename = Dir()
        
       Loop
      ' ver que nro de pista sigue ....
      ubi1=InStr(filenameold,"[")
      ubi2=InStr(filenameold,"]")
      no1= Mid(filenameold,ubi1+1,ubi2-1-ubi1)
      ntk=CInt(no1) +1
      Print #1, "Numpista PROXIMO ",ntk
     EndIf
     Print #1,"Nombre Cancion", NombreCancion
     Print #1,"NumPista ntk ", ntk
     Print #1,"no2 nombre del roll puro ",no2
     ' aca hay 2 casos 1) grabo un track de cancion con otro nombre en cancion o sea
     ' hago una copia, como diferenciarlo? con un indicador de si cargue un roll..
     ' 2) y el otro estoy en cancion , pero cargue un roll fuera de cancion y 
     ' la quiero convertir a track y poner en cancion.
     '1) estoy grabando un track en edicion con cancion, por lo tanto
     ' se crea un nuevo track copia con otro numero pero debe tener
     ' todos los parametros anteriores..luego debo ahcer una grabacion de
     ' track que haga sobreescritura no nuevo track eso se manejara dede
     ' control
     ' CANCION CARGADA TIENE TRACKS CANCION CREADA NO
    EndIf
EndIf     
     If CANCIONCARGADA And ROLLCARGADO=FALSE Or copia=TRUE Then ' copia track en otro track
     print #1,"copia track a otro track"
        pmTk(ntk).desde=pmTk(ntkold).desde
        pmTk(ntk).hasta=pmTk(ntkold).hasta
        pmTk(ntk).NB=pmTk(ntkold).NB
        pmTk(ntk).NA=pmTk(ntkold).NA
        pmTk(ntk).MaxPos=pmTk(ntkold).MaxPos
        pmTk(ntk).posn=pmTk(ntkold).posn
        pmTk(ntk).notaold=pmTk(ntkold).notaold
        pmTk(ntk).Ticks=pmTk(ntkold).Ticks
        ReDim (Track(ntk).trk) (1 To pmTk(ntkold).Ticks, 1 To lim2)
        Print #1,"hizo copia de parametros de trackold en new"
        Print #1," *po seria hasta -1 ", pmTk(ntk).hasta -1
     EndIf
      If no2<> "" Then ' graba roll en cancion
         print #1,"preparo nombre cancion nuevo ntk ", ntk
         Dim haycorchete As Integer
         haycorchete = InStr(no2,"]")
         If haycorchete> 0 Then
           no2 = Mid(no2, haycorchete+1)  ' sacamos el [x] si existe
         EndIf
        
         nombre= NombreCancion + "\[" + doscifras(ntk) + "]" + no2 +".rtk"
         Dim cadena As String = "[" + doscifras(ntk) + "]" + no2
         AddListBoxItem(3, cadena)
         Print #1,"GRABANDO PISTA EN CANCION EN ",nombre
      'si es vacio tomo la ultima
        titulos(ntk)=nombre
        ' guardo los valores de Roll cargado en el track nuevo 
        If CANCIONCARGADA And ROLLCARGADO=TRUE Then 
           ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2) 
           pmTk(ntk).desde=desde
           pmTk(ntk).hasta=hasta
           pmTk(ntk).NB=NB
           pmTk(ntk).NA=NA
           Print #1,"NB y NA de RollaTrack cargando un Roll en cancion ", NB, NA 
           pmTk(ntk).MaxPos=MaxPos
           pmTk(ntk).posn=posn
           pmTk(ntk).notaold=notaold
           pmTk(ntk).Ticks=CantTicks
        EndIf
      Else
         Print #1," no hay archivo nuevo se sale sin grabar nada"
         Exit Sub ' no hay archivo nuevo se sale sin grabar nada
      EndIf
   

   
   Dim grt As Integer
   grt = FreeFile
' aca al nombre siempre ya se le agrego trk 
'3) si no paso por 1) ni 2)  y es un track el nombre es de un track logicamente
' no hace falt hacer nada el nombre esta completo por ejemplo [03]Guitarra.rtk
' sobreescribe el track (0,0,FALSE)  

 ReDim Trabajo (1 To Maxpos, NB To NA) As dat ' definido como roll
' copia en Trabajo el Roll ....que tiene las modificaciones ultimas
 Dim As Integer i1, i2, i3, semitono, borrocol=0, haynota=0,res=0,k=0,final=0
'eliminar columnas marcadas al grabar disco, 0 + X
'Print #1, "inicio MaxPos "; MaxPos
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
          Trabajo(k, i1) =Roll.trk(i2,i1 )
      Next i1
    EndIf
      
    borrocol=0:res=0:haynota=0:final=0
 Next i2
Print #1,"termina con los borrados"

posn=k        
pmTk(ntk).posn=posn

' track nuevo empieza desde k..no es eso
Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    Trabajo(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    Trabajo(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  


' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir
Print #1,"Termina con achicar secuencia"
 Dim grabaPos (1,1)  As poli
 Dim grabaLim (1,1)  As poli
' datos para track....en temp  
 Dim TrkTemp (1 To MaxPos,1 To lim2) As poli 'definimos un Track temporario

i3=0
' copia en Temp Trabajo donde esta Roll
For i2 = 1 To MaxPos
   i3=0
   For i1=NB To NA 
      If Trabajo(i2,i1 ).nota >= 1 and Trabajo(i2,i1 ).nota <=12 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         Print #1,"copia Tabajo a Temp en GrabaRollaTrack"
         i3=i3+1
         TrkTemp(i2,i3).dur  =CInt(Trabajo(i2,i1 ).dur)
         TrkTemp(i2,i3).nota =Trabajo(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =Trabajo(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =Trabajo(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =Trabajo(i2,i1 ).pb
         trkTemp(i2,i3).inst =Trabajo(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         TrkTemp(i2,i3).nota=CUByte(PianoNota)
   '      Print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      Print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            Print #1,"Error mas de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf
   Next i1
   
   If i3 >=2 Then
    Print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
TrkTemp(1,1).inst=Trabajo(1,NA).inst  

Print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
' deberia andar igual en el caso de grabar track en cancion porque se pasa a
' temp y de ahi se copia de nuevo al track previamente borrado
' siemrpe que se maneje track individuales o una cancion y se haga modificacioens
' como borrados de notas se pasa por Temp y se vuelve a grabar 
' en disco y en el TRack.
' No se deberia permitir cargar un roll o mejor ajustar ROLLCARGADO=FALSE
' en Control al querer ahcer una copia.
If ROLLCARGADO=FALSE And cambiaNombre=0 And Copia= TRUE And encancion > 0 Then ' (0,1,TRUE)
' aca buscamos tope sumamos 1 copiamos a un track nuevo
'4) copiamos una pista de cancion en otra pista nueva de cancion
' para invocar esto se necesit ael menu de Control 
   tope=tope+1
     If tope <= 32 Then
        ntk=tope
     EndIf
   
Else 
'3) sobreescribe la pista con las modificaciones del Roll   
   Print #1,"continua 3) Se está grabando modificaciones de un track ntk de cancion", ntk
   
EndIf

' borro datos de track ntk ,0 o ntk o ntk+1   
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2)

' copiamaos a ntk que sera 0 al lado de roll, o ntk o ntk +1 segun sea el caso
' en el path de cancion

For i1=1 To MAxPos ' de Roll qu edeberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim2
     Track(ntk).trk(i1,i2)=TrkTemp(i1,i2) ' 
    Next i2
Next i1

' sino 
If Open (nombre  For Binary Access write As #grt ) <> 0 Then
     Print #1,"Error open Rollsub 2727, nombre ",nombre
     Exit Sub
End If
 Print #1, "nombre tiene el path ",nombre
 Print #1, "NB , NA,MAXPOS ",NB,NA,MaxPos  
     
Print #1,"pasó copia a vector"
     Dim As ubyte y1,y2,y3,y4, y5 
     Dim As String a1,a2,a3,a4,a5 ,x
     
     Print #1,"etapa1"
     x= Bin(MaxPos,20)
         Print #1,"Posicion ",Posicion
     Print #1,"string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     a5=Mid(x,17,4)
         Print #1,"a1 a2 a3 a4,a5 ",a1, a2 ,a3, a4,a5

     y1= Cubyte("&B"+a1)
     y2= CUByte("&B"+a2)
     y3= CUByte("&B"+a3)
     y4= CUByte("&B"+a4)
     y5= CUByte("&B"+a5)
         Print #1, "y1,y2,y3,y4,y5", y1,y2,y3,y4,y5
     ' grabamos maxpos en 5 ubyte
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     grabaPos(1,1).pb   = y5
     '-----------------------
     grabaLim(1,1).nota = CUByte(desde)
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
     '-----------------------------
     Print #1,"etapa final puts"
     Put #grt, ,grabaPos(1,1)
     Put #grt, ,grabaLim(1,1)
     Put #grt, ,TrkTemp()
     cerrar (grt)
     While InKey <> "": Wend
     Sleep 150
Print #1,"FIN GrabarRollaTrack ,cambianombre,enCancion ",cambianombre,enCancion
Print #1,"FIN GrabarRollaTrack ,maxpos,posn, enCancion ",maxpos,posn

Print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   
 
End Sub
'/
Sub ActualizarRollyGrabarPista ()
' nombre es global ,,,por ahora...con un Roll único nombre puede ser global
' ya que es único y siempre es parte de Roll grafico...
   Dim grt As Integer
   grt = FreeFile
   ' DEFINO UN ROLL DONDE MANDO EL ROLL CARGADO LE SACO LOS DELETE SI HUBO
   ReDim RollTemp (1 To Maxpos, NB To NA) As dat 
' copia en RollTemp el Roll ....que tiene las modificaciones ultimas
   Dim As Integer i1, i2, i3, semitono, borrocol=0, haynota=0,res=0,k=0,final=0
  'eliminar columnas marcadas al grabar disco, 0 + X
  ' VERIFICAR QU EEL MAXPOS SEA DEL ROLL CREO QUE SI LOGICO
   Print #1, "inicio MaxPos "; MaxPos
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
 Print #1,"termina con los borrados"

posn=k        
pmTk(ntk).posn=posn -1
Print #1,"posn ",posn
' track empieza desde k..no es eso
Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    RollTemp(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    RollTemp(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  
' AHORA MAXPOS SE ACHICO 
MaxPos=posn
pmTk(ntk).MaxPos=MaxPos
posn=Maxpos-2
Print #1,"MaxPos despues de acchicar",MaxPos
' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir
Print #1,"Termina con achicar secuencia"
 Dim grabaPos (1,1)  As poli
 Dim grabaLim (1,1)  As poli
' datos para track....en temp  
 Dim TrkTemp (1 To MaxPos,1 To lim2) As poli 'definimos un Track temporario

i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
For i2 = 1 To MaxPos
   'Print #1,"i2",i2
   i3=0
   For i1=NB To NA 
    ' Print #1,"i1",i1
      If RollTemp(i2,i1 ).nota >= 1 and RollTemp(i2,i1 ).nota <=12 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         'Print #1,"copia Tabajo a Temp en GrabaRollaTrack"
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
   '      Print #1,"Temp(i2,i3).nota ",Temp(i2,i3).nota
   '      Print #1,"Temp(i2,i3).dur ",Temp(i2,i3).dur 
' acorde          
         If i3=12 Then ' track solo guarda 12 notas en acorde el resto se desrpecia
            Print #1,"Error mas de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf
   Next i1
   
   If i3 >=2 Then
    Print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
TrkTemp(1,1).inst=RollTemp(1,NA).inst  
Print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2)

' copiamaos a ntk que sera 0 al lado de roll, o ntk o ntk +1 segun sea el caso
' en el path de cancion

For i1=1 To MAxPos ' de Roll que deberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim2
     Track(ntk).trk(i1,i2)=TrkTemp(i1,i2) ' 
    Next i2
Next i1

' nombr es global se resuelve en la subrutina que llama a esta
If Open (nombre  For Binary Access write As #grt ) <> 0 Then
     Print #1,"Error open Rollsub 2727, nombre ",nombre
     Exit Sub
End If
 Print #1, "nombre tiene el path ",nombre
 Print #1, "NB , NA,MAXPOS ",NB,NA,MaxPos  
     
Print #1,"pasó copia a vector"
     Dim As ubyte y1,y2,y3,y4, y5 
     Dim As String a1,a2,a3,a4,a5 ,x
     
     Print #1,"etapa1"
     x= Bin(MaxPos,20)
         Print #1,"Posicion ",Posicion
     Print #1,"string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     a5=Mid(x,17,4)
         Print #1,"a1 a2 a3 a4,a5 ",a1, a2 ,a3, a4,a5

     y1= Cubyte("&B"+a1)
     y2= CUByte("&B"+a2)
     y3= CUByte("&B"+a3)
     y4= CUByte("&B"+a4)
     y5= CUByte("&B"+a5)
         Print #1, "y1,y2,y3,y4,y5", y1,y2,y3,y4,y5
     ' grabamos maxpos en 5 ubyte
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     grabaPos(1,1).pb   = y5
     '-----------------------
     grabaLim(1,1).nota = CUByte(desde)
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
     '-----------------------------
     Print #1,"etapa final puts"
     Put #grt, ,grabaPos(1,1)
     Put #grt, ,grabaLim(1,1)
     Put #grt, ,TrkTemp()
     cerrar (grt)
     While InKey <> "": Wend
     Sleep 150

End Sub
' ---------------------------
Sub ImportarPistaExterna()
Print #1,"---------------------------------------------------------------------------------"
Print #1,"inicia ImportarPistaExterna " 
Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva  
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    Print #1,"extension de la pista importada",ext
    Print #1,"nom nombre sin extension ni path ",nom
    Print #1,"numero de pista tope =",tope
   tope=tope+1
   If tope <= 32 Then
      ntk=tope
   Else    
      Exit Sub   
   EndIf
   Print #1,"pista nueva importada será ntk=",ntk   

  
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
    Print #1,"GRABANDO PISTA EN CANCION EN ",nombre
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
    Print #1,"GRABANDO PISTA EN CANCION EN ",nombre
    
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
Print #1,"---------------------------------------------------------------------------------"
Print #1,"inicia GrabarCopiaPista " 
Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva  
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    Print #1,"extension ",ext
    Print #1,"nom nombre sin extension ni path ",nom
    Print #1,"pista origen ntk=",ntk
'-> copiamos una pista de cancion en otra pista nueva de cancion
' para invocar esto se necesit ael menu de Control 
   tope=tope+1
   If tope <= 32 Then
      ntk=tope
   Else    
      Exit Sub   
   EndIf
   Print #1,"pista nueva ntk=",ntk   
' a) tomar el roll de la pista origen sacarle lso deletes, en un roll temporario
' b) copiar el roll temporario al track nuevo
' a) en este caso como es una copia todos los parametros del Roll origen son
' iguales (dsde hasta maxpos notaold posn etc)
' desde aca es igual
     If CANCIONCARGADA  Then ' copia track en otro track
     print #1,"copia track a otro track"
        pmTk(ntk).desde=pmTk(ntkold).desde
        pmTk(ntk).hasta=pmTk(ntkold).hasta
        pmTk(ntk).NB=pmTk(ntkold).NB
        pmTk(ntk).NA=pmTk(ntkold).NA
        pmTk(ntk).MaxPos=pmTk(ntkold).MaxPos
        pmTk(ntk).posn=pmTk(ntkold).posn
        pmTk(ntk).notaold=pmTk(ntkold).notaold
        pmTk(ntk).Ticks=pmTk(ntkold).Ticks
        ReDim (Track(ntk).trk) (1 To pmTk(ntkold).Ticks, 1 To lim2)
        Print #1,"hizo copia de parametros de trackold en new"
        Print #1," *po seria hasta -1 ", pmTk(ntk).hasta -1
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
        Print #1,"GRABANDO PISTA EN CANCION EN ",nombre
      'si es vacio tomo la ultima
        titulos(ntk)=nombre
     EndIf
   ActualizarRollyGrabarPista ()
   
Print #1,"FIN GrabarCopiaPista nueva ",ntk
Print #1,"FIN GrabarCopiaPista ,maxpos,posn ",maxpos,posn

Print #1,"---------------------------------------------------------------------------------"


  
End Sub
' ----------------------------------
Sub GrabarRollaTrack ( cambiaext As Integer )
' las 3 1eras funciones pueden hacerce en roll sin problemas..roll lo deduce
' de la entrada y las variables globales. la 4ta no debe llamarse desde Control
'1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia 
 ' cambiaext=1 porqu epasa de .roll a .rtk  
'2) convierte un Roll a trk y graba en cancion, igual que antes pero 
 ' lo pone en cancion con le mismo nro de track sobreescribe

Print #1,"---------------------------------------------------------------------------------"
Print #1,"inicia GrabarRollaTrack, cambiaext ",cambiaext 
   Dim As String path, nom,ext
   Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de rool a track, i tengo una cancion en edcion
' y queiro un roll nuevo no conviene hacerlo aca si hay cancion no se usa
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll
    
    Print #1,"extension ",ext
    Print #1,"nom nombre sin extension ni path ",nom
 '1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' por eso cambia extension a rtk
If  nombre > "" Then
   ' graba roll a rtk encancion 0 o 1
   If cambiaext > 0   And ext =".roll" Then 
      ntk=0 
      nombre=path +"[00]"+nom +".rtk" 'path + 0 + rtk por default si no hay cancion
      Print #1,"armado de nombre roll a trk[00]",nombre
        ' guardo los valores de Roll cargado en el track nuevo [00] 
        If CANCIONCARGADA And ROLLCARGADO=TRUE Then 
           ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2) 
           pmTk(ntk).desde=desde
           pmTk(ntk).hasta=hasta
           pmTk(ntk).NB=NB
           pmTk(ntk).NA=NA
           Print #1,"NB y NA de RollaTrack cargando un Roll en cancion ", NB, NA 
           pmTk(ntk).MaxPos=MaxPos
           pmTk(ntk).posn=posn
           pmTk(ntk).notaold=notaold
           pmTk(ntk).Ticks=CantTicks
        EndIf
      
   EndIf
 '2) Actualizacion de un trk de cancion, se modifica desde roll igual que antes 
 'pero sobreescribe el track del roll correspondiente en cancion
 ' lso parametros no se tocan es la misma pista 
   If cambiaext=0 And nombre >""  And ext = ".rtk" Then 
     Print #1,"update ntk, Nombre Cancion", NombreCancion ' es el path del directorio de cancion
     Print #1,"update de NumPista ntk ", ntk
     Print #1,"update,nombre del track con  path no se toca ",nombre
   EndIf
Else
' no crea solo convierte roll a rtk y graba, o solo graba de rtk a rtk
' el archivo debe existir y estar cargado
   Exit sub
EndIf
   If ext = ".rtk" Then 
     print #1,"  ntk ", ntk
     Print #1,"GRABANDO UPDATE DE PISTA EN CANCION EN ",nombre
   ' veo valore sde track  
    Print #1,"desde ", desde,pmTk(ntk).desde
    Print #1,"hasta ", hasta,pmTk(ntk).hasta
    Print #1,"Maxpos ",MaxPos,pmTk(ntk).MaxPos
    Print #1,"NB ",NB,pmTk(ntk).NB
    Print #1,"NA ",NA,pmTk(ntk).NA
   EndIf   

' edsde aca es igual.. 
  ActualizarRollyGrabarPista () 
' hasta aca es igual     
Print #1,"FIN GrabarRollaTrack ,cambiaext ",cambiaext
Print #1,"FIN GrabarRollaTrack ,maxpos,posn ",maxpos,posn

Print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   
 
End Sub
' ---------------------
Sub TrackaRoll (Track() As sec, byref ntk As Integer, Roll As inst)
' como es track a Roll se supone que el track ya esta cargado en memoria
' y esta sub pasa de Track a Roll Visual y track 0 
Dim As Integer i1,i2,i3,Maxposicion
 ' Print #1,"TrackaRoll 1"   
''If ubirtk=3 Then ' estoy conmutando de track durante la edicion
' si no estoy en cancion el nto va a ntk 0, siemrep uso ntk y el vector de pnTk
print #1,"-------------ARRANCA TRACKAROLL---------------------------------"
Print #1,"NTK Y nombre que llego a TrackaRoll ",ntk ,titulos(ntk)
Print #1,"maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
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
   NB     = pmTk(ntk).NB
   NA     = pmTk(ntk).NA
   MaxPos = pmTk(ntk).MaxPos
   posn   = pmTk(ntk).posn
   notaOld= CInt(pmTk(ntk).notaold)
   
   Print #1,"TrackaRoll ntk, desde, hasta, MaxPos ", ntk, desde,hasta,MaxPos
   desdevector=desde
   hastavector=hasta
   estoyEnOctava =desde
   estoyEnOctavaOld =desde
   CantTicks = pmTk(ntk).Ticks '
   Print #1,"TrackaRoll, NB, NA, CantTricks", NB,NA, CantTicks
' redim de ROLL de Visualizacion       
   ReDim (Roll.trk ) (1 To CantTicks, NB To NA )
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
   If CANCIONCARGADA=TRUE And ROLLCARGADO=FALSE Then ' cargarcancion NO SE USA VIVE muy poco solo durante lA carga  
       Print #1,"REDIM DE TRACK 0, NO SE HACE SIN CANCION O CON ROLLCARGADO"
       ' SE PREPARA TRACK 0 PARA RECIBIR DATOS DE TRACK X,PUES PARA IR A ROLL
       ' SE LLENA TRACK 0 SIEMRPE.,,,
       ReDim (Track(0).trk ) (1 To CantTicks,1 To lim2)
   EndIf
   nota=0:dur=0
   inicioDeLEctura=0
   *po=pmTk(ntk).hasta -1
   
Print #1,"TrackaRoll desde, hasta ", desde , hasta
Print #1,"TrackaRoll NB, NA ,*po, MaxPos ", NB , NA, *po,MaxPos
Print #1, "ubound (Track(ntk).trk,2) ", ubound (Track(ntk).trk,2)
Print #1, "lbound (Track(ntk).trk,2) ", LBound (Track(ntk).trk,2)
Print #1," va a ejecutar 510 de Track aRoll"

' 1) carga de Roll desde track ntk
For i2 = 1 To MaxPos 
   For i1=1 To lim2
      If Track(ntk).trk (i2,i1).nota > 0 and Track(ntk).trk(i2,i1 ).nota <=NA Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         PianoNota=Track(ntk).trk(i2,i1 ).nota
         PianoNota= PianoNota + SumarnR (PianoNota) ' para 60 es 65
         'i3= 115 -PianoNota  ' 06-09-2021 jmg para 1 a 9 octava anda ok menso no
         i3= PianoNota  ' JMG 4.3  65
      '   Print #1,"indice i3 el vertical ", i3
         'i3=semitono + (*po) * 13
         ' nueva formula veremos de B=1 a C=12 bajando B->C  Ej:71 ->60
         ' para 71 PianoNota=71 + 5=76.(12-(76 - (int(76/13)*13))= 1 ..ok
         ' es ok si las notas van de 1 a 12 bajando....  
         Roll.trk(i2,i3).nota = CUByte(12 -( i3 - (Int(i3/13))*13) )
     '    Print #1,"Roll.trk(i2,i3).nota ",Roll.trk(i2,i3).nota
         'ers=11- semitono  + *po * 13
         '    11 - semitono +(*po) * 13)  103 notacur  12
         '  11 - 12 + 9*13 = -1+ 117 = 116
         Roll.trk(i2,i3).dur  = Track(ntk).trk(i2,i1).dur
         Roll.trk(i2,i3).vol  = Track(ntk).trk(i2,i1).vol
         Roll.trk(i2,i3).pan  = Track(ntk).trk(i2,i1).pan
         Roll.trk(i2,i3).pb   = Track(ntk).trk(i2,i1).pb
         Roll.trk(i2,i3).inst = Track(ntk).trk(i2,i1).inst
                
     '    Print #1,"Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
      Else
 ''        Print #1,"nada nota,dur ",Roll.trk(i2,i3).nota,Roll.trk(i2,i3).dur
                
      EndIf
   Next i1
Next i2     
' ajuste de notas no usadas con 181 para evitar retrocesos en la carga de notas
' y poder entrar acordes
For i2 = 1 To MaxPos 
  For i1=NB To NA
   If Roll.trk(i2,i1).nota =0 And Roll.trk(i2,i1).dur <>182  Then
      Roll.trk(i2,i1).nota=181
      Roll.trk(i2,i1).dur=0
   Else
   ''Print #1,i2,Roll.trk(i2,i1).nota,Roll.trk(i2,i1).dur
    '' Continue For   
   EndIf
  Next i1
Next i2     
' 2)  pasar lso datos del Track(x) a Track(0)!! si es cancion
' el redim esta arriba
If cargaCancion=1 Then ' solo mientras cargo cancion? no  
 For i1=1 To MaxPos 
  For i2 = 1 To lim2
  Track(0).trk(i1,i2) = Track(ntk).trk(i1,i2)
  Next i2
 Next i1
EndIf  
  
Roll.trk(1,NA).inst = Track(ntk).trk(1,1).inst


'--------------------------------Track(0) fin
curpos=0
notacur=1
nroCompas=0
tres=0:pun=0:sil=0:mas=0:doblepun=0:cuart=0
tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0

  Print #1,"TrackaRoll Fin,,maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
print #1,"----------------------------------------------"
Sleep 100

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
   titulos(i)=""          
 next i
 
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

fueradefoco=1
Dim As Double tiempoDUR
tiempoDUR=60/tiempoPatron '60 seg/ cuantas negras enun minuto
Dim nombre As ZString ptr
Dim As Integer i1,i2,pis,K
Dim As Integer comienzo=1, final=MaxPos,  canal=0,vel=100,velpos =0
' canal 0 es el 1 van de 0 a 15
' pasoCol debe ser de 2 dimensiones 1 para c/pista, 115 el amxio de octavas
' ojo si cambioaamos por mas octavas debo cambiar, igual el nro de tracks 32 
Dim pasoCol (0 To 384) As vec  ' entrada de durciones a medida que barro una columna

Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim As Integer result,limsup

' tope es la maxima capacidad de tracks usada , lso tracks son contiguos
' si se borra un track queda sin usar eso hay que verlo...
' por ahora solo proceso los que tengan lim2 o si llegamos a tope 
  jply=0:curpos=0
  mousex=0
' Print #1,                    "-----------------------------------------"
  comienzo=posicion
  cntold=0
  If pasoZona1 > 0 Then
    comienzo=pasoZona1
  EndIf

  If pasoZona2 > 0 Then
    final=pasoZona2
  EndIf




 ' Print #1,"---START-----paso:";jply;" --------------------------------"
  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
' Esto es play de pistas el limite de barrido es 1 a 12 siempre en c/pista  
' debo hcer un for para cada pista, en cada psita ver si esta mute o play
' ajustar el instrumento de la pista cada vez que cambie la pista.



For jply=comienzo To final
' cambio de inst para la pista, podria poner mas de un instrumento por pista
' o por cada nota.. 
' VER DE PONER LOS INSTRUMENTOS EN TRACK  
kNroCol= Int(jply/NroCol)
   If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < pmTk(pis).MaxPos)Then
     posicion=jply
     curpos=0
   EndIf

   mousex=jply
   If CONTROL1 = 1 Then
      alloff( 1 )
      CONTROL1=0
      Exit For
   EndIf  

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

 For pis =1 To 32 ' loop de pistas
    limsup=UBound (Track(pis).trk,2)
    If limsup < lim2 Then
          Exit For 
    EndIf  
    If pis > tope Then
       Exit For
    EndIf
  '''final=pmTk(pis).MaxPos

      
' ojo con silencios ligados !!!

' limites para cada pista NB,NA 
   NB = 0  + (pmTk(pis).desde -1 ) * 13 
   NA = 11 + (pmTk(pis).hasta -1 ) * 13 
   Print #1,"Play Cancion NB ,NA, pis",NB,NA,pis
   
   Dim As ubyte NBpiano= CUByte(NB) - CUByte(restar(NB))
   Print #1,"limite inferior NBpiano ",NBpiano
' info del inst en 1 no en lim2   
   For i1=1 To lim2   ' coo voy de 1 a lim2 necesito que la info del int este en 1

     If (Track(pis).trk(jply,i1).nota >= NBpiano) And (Track(pis).trk(jply,i1).nota <= NA) And (Track(pis).trk(jply,i1).dur >=1) And (Track(pis).trk(jply,i1).dur <= 180) Then ' es semitono
         Notapiano = Track(pis).trk(jply,i1).nota
         Print #1,"Notapiano ",Notapiano
         Print #1,"i1,NB,NA ",i1,NB,NA
'       Print #1,"VEO LO CORRECTO DE NOTAPIANO "; Notapiano
         dura=Track(pis).trk(jply,i1).dur ' el dur de track esta en integer
'      Print #1,"jply ";jply; "dura ";dura
         cnt=cnt+1
   '   Print #1,"paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
        If cnt=1 Then 
           duraOld=dura
        
          If ligaglobal=1 Then
            For i2=1 To cntold
                pasoCol(i2).DUR =181
'                pasoCol(i2).liga=0
                pasoCol(i2).tiempoFigura =0
                pasoCol(i2).inst=Track(pis).trk(1,1).inst
'                pasoCol(i2).old_time=0
            Next i2
         EndIf
       EndIf
       If duraOld=dura  Then
          iguales=1
    '     Print #1,"cnt ";cnt;" iguales ";iguales
       Else
          distintos=1
    '     Print #1,"cnt ";cnt;" distintos ";distintos
       EndIf         
      If ligaglobal =0 Then 
     '    Print #1,"ligaglobal es cero ****"   
      '  Print #1,"-> cnt"; cnt 
         pasoCol(cnt).DUR =dura
       '  Print #1,"pasoCol(cnt).DUR ", pasoCol(cnt).DUR 
         pasoCol(cnt).notapiano=Notapiano 
         pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * 100000000000
         pasoCol(cnt).i1 = i1 'posicion vertical en el vector real
      ' 20-06-2021 eliminado duraold=dura repetido  
         pasoCol(cnt).inst=Track(pis).trk(1,1).inst  
         vel= vol( dura, velpos)
      Else
       '  Print #1,"ligaglobal es uno *****"
         If dura >= 91 And dura <=180 Then
            For i2=1 To cntold
                If Notapiano  = pasoCol(i2).notapiano Then
        '           Print #1,"Notapiano de liga igual "; Notapiano
                   pasoCol(i2).DUR =dura
         '          Print #1,"pasoCol(i2).DUR "; pasoCol(cnt).DUR 
                   pasoCol(i2).notapiano=Notapiano 
                ' tiempofigur aya calculado
        '           Print #1,"tiempofigura anterior ";pasoCol(i2).tiempoFiguraOld
                   pasoCol(i2).tiempoFigura=pasoCol(i2).tiempoFiguraOld       
        '           Print #1,"pasoCol(i2).i1 ";pasoCol(i2).i1
        '           Print #1," i1 debe ser igual al anterior:"; i1       
                   pasoCol(i2).i1 = i1 'posicion vertical en el vector real es igual
                   pasoCol(i2).inst=Track(pis).trk(1,1).inst
                   vel= vol( dura, velpos)
               EndIf    
            Next i2
         Else
                   pasoCol(cnt).DUR =dura
         '          Print #1,"pasoCol(cnt).DUR "; pasoCol(cnt).DUR 
                   pasoCol(cnt).notapiano=Notapiano 
                ' tiempofigur aya calculado
                   pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * 100000000000       
                   pasoCol(cnt).i1 = i1 'posicion vertical en el vector real es igual
                   pasoCol(cnt).inst=Track(pis).trk(1,1).inst
                   vel= vol( dura, velpos)
         EndIf
      EndIf
' llegamos al final de la Columna

     EndIf
   
     If i1=lim2  And (pis = tope Or pis=32) Then
 
        If cnt > 1 Then' Acorde
          Print #1,"i1=NB=";i1 ; " ACORDE cnt= ";cnt
        Else    
           Print #1,"i1=NB=";i1 ; " SIMPLE cnt= ";cnt
        EndIf  
        Select Case cnt
          Case 1 
           ' con y sin liga
           ' INCLUYE LIGADOS O NO LIGADOS
 Print #1, "CALLL A NOTESIMPLE cntold, vel, canal, tiempodur",  cntold, vel, canal,tiempoDur
             
            noteSimple  pasoCol(), cntold, vel, canal,tiempoDur,velpos
                     
          Case Is > 1

            If iguales=1 And distintos=0  Then
               For i2=1 To cnt
                 pasoCol(i2).tiempoFiguraOld=0
               Next i2

                Print #1,"cnt ";cnt;" Acordeiguales "
                
                AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll,velpos,pis
                
            EndIf
            If  distintos=1 Then
               Print #1,"cnt ";cnt;" AcordeDistintos"
                
                AcordeDistintos pasoCol(),cnt, cntold,vel,canal,tiempoDur,Roll,velpos,pis
                
            EndIf
            
         End Select  

        cntold = cnt
        Print #1,"cnt,cntold"; cnt;" ";cntold
  
     EndIf 
   Next i1
  Print #1,"---FIN -----paso:"; jply;" --------------------------------" 
  Print #1,"COMIENZA OTRA  POSICION O J ======"; jply
 mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
 If playloop=1 And jply= final Then
    jply=comienzo -1
    'posicion=comienzo
 EndIf
 tiempoDUR=60/tiempoPatron '13-07-2021 cambiamos velocidad durante el play!!!

  Next pis   

Next jply


posicion=comienzo
'posishow=posicion + 20
'posishow=posicion - 20
'posicion=posicion -20
 
jply=0:curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
play=0 
playb=0
mousey=100 'otra mas para evitar rentrar a play en menu
finplay=1

mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
fueradefoco=0

Sleep 100,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
/'
close_port(midiout)
out_free(midiout)
'/ 



ThreadDetach(thread1) 'JMG REPONER !!!!
End Sub 
