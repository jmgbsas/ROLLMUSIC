
Sub CargarTrack(Track() As sec, ByRef ntk As Integer , ByRef ubirtk As Integer)
' solo carga track no lo pasa a Vector de Visualizacion. ntk debe venir informado
' para cargar desde disco un track se usa esta sub y luego TrackaRoll para verlo
' y editarlo
  print #1,"----------------------------------------------"
  print #1,"1) ENTRA A CargarTrack nombre, ntk= ",nombre , ntk   
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
          print #1,"ubirtk > 0 carga de disco o cancion ",ubirtk
          nombrea=titulos(ntk) ' ya venia el nombre
          ubirtk=0
       EndIf   
     EndIf
       If nombrea = "" Then
       print #1, "cargaTrack exit sub"
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
    Dim ct As Integer
    ct=FreeFile
    Dim miroerr As Integer
    print #1,"NTK Y nombre que llego a open en CargaTrack ",ntk ,nombre
    titulos(ntk)=nombre
    
    miroerr= ( Open (nombre  For Binary Access Read As #ct ))
     If miroerr <> 0 Then
       print #1,"arch track  abrio con error 1307 CargarTrack miroerr, nombre",miroerr, nombre
       Exit sub
     EndIf
        
     Get #ct, , z(1,1)
     x1=Bin(z(1,1).nota,4)
     x2=Bin(z(1,1).dur,4)
     x3=Bin(z(1,1).vol,4)
     x4=Bin(z(1,1).pan,4)
     x5=Bin(z(1,1).pb,4)
     x=x1+x2+x3+x4+x5
        print #1,"reconstruccion x pos bin ", x
     'toda carga de track se guarda en pmTk sea ntk=0 u otro valor   
     pmTk(ntk).MaxPos=CInt("&B"+x)   
     print #1,"pmTk(ntk).MaxPos ", pmtk(ntk).MaxPos
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
     print #1,"desde ",pmTk(ntk).desde
     print #1,"hasta ",pmTk(ntk).hasta
     
     print #1,"en la carga de track desde hasta", pmTk(ntk).desde,pmTk(ntk).hasta
     pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13   ' 27 para 3 SI CARGO CANCION NO VA
     pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13  ' 90 para  7 06-09-2021 decia 12 -> es 11
     print #1,"CargarTrack  NB Na", pmTk(ntk).NB, pmTk(ntk).NA
     print #1,"cargaCancion ",cargacancion
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
     print #1,"*PO = hasta -1 ",  pmTk(ntk).hasta -1
     print #1,"Get TrK POLI "
     Dim errget As Integer
     errget= Get( #ct, , Trabajo()) 
     If errget <> 0 Then
        print #1,"error en Geteo "
     Else
        print #1,"Geteo Trabajo poli ok "
     EndIf
     ' movemos los datos a Track
     ' -------------------------
     'ReDim compas(1 To CantTicks) es solo para visualizar
     'carga=1 ' para visualizar no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
     print #1,"pmtk(ntk).MaxPos, ntk ",pmTk(ntk).MaxPos , ntk
     print #1,"ABRIR MAXPOS ,NB,NA "; pmTk(ntk).MaxPos, pmTk(ntk).NB,pmTk(ntk).NA
     Dim As Integer  grupo, mayorgrupo

     For j = 1 To pmTk(ntk).MaxPos  -2 '11-07-2021
    '   print #1,"POSICION :",J
    '  print #1,"lim2 :", lim2
      For i= 1 To lim2
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

     '''''ReCalCompas(Roll) '''No es vEctor de Visualizacion      

      print #1,"CargarTrack  fin"  
      print #1,"----------------------------------------------" 
 Sleep 100 ' retardo para que se ubiquen lso datos en memoria ¿? parece necesario
End Sub
'
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
 print #1,"termina con los borrados"

posn=k        
pmTk(ntk).posn=posn -1
print #1,"posn ",posn
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
print #1,"MaxPos despues de acchicar",MaxPos
' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir
print #1,"Termina con achicar secuencia"
 Dim grabaPos (1,1)  As poli
 Dim grabaLim (1,1)  As poli
' datos para track....en temp  
 Dim TrkTemp (1 To MaxPos,1 To lim2) As poli 'definimos un Track temporario

i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
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
            print #1,"Error mas de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf
   Next i1
   
   If i3 >=2 Then
    print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
TrkTemp(1,1).inst=RollTemp(1,NA).inst  
print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
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
     print #1,"Error open Rollsub 2727, nombre ",nombre
     Exit Sub
End If
 print #1, "nombre tiene el path ",nombre
 print #1, "NB , NA,MAXPOS ",NB,NA,MaxPos  
     
print #1,"pasó copia a vector"
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
     '-----------------------
     grabaLim(1,1).nota = CUByte(desde)
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
     '-----------------------------
     print #1,"etapa final puts"
     Put #grt, ,grabaPos(1,1)
     Put #grt, ,grabaLim(1,1)
     Put #grt, ,TrkTemp()
     cerrar (grt)
     While InKey <> "": Wend
     Sleep 150

End Sub
' ---------------------------
Sub ImportarPistaExterna()
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia ImportarPistaExterna " 
Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
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
Sub GrabarRollaTrack ( cambiaext As Integer )
' las 3 1eras funciones pueden hacerce en roll sin problemas..roll lo deduce
' de la entrada y las variables globales. la 4ta no debe llamarse desde Control
'1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia 
 ' cambiaext=1 porqu epasa de .roll a .rtk  
'2) convierte un Roll a trk y graba en cancion, igual que antes pero 
 ' lo pone en cancion con le mismo nro de track sobreescribe

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
   ' graba roll a rtk encancion 0 o 1
   If cambiaext > 0   And ext =".roll" Then 
      ntk=0 
      nombre=path +"[00]"+nom +".rtk" 'path + 0 + rtk por default si no hay cancion
      print #1,"armado de nombre roll a trk[00]",nombre
        ' guardo los valores de Roll cargado en el track nuevo [00] 
        If CANCIONCARGADA And ROLLCARGADO=TRUE Then 
           ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2) 
           pmTk(ntk).desde=desde
           pmTk(ntk).hasta=hasta
           pmTk(ntk).NB=NB
           pmTk(ntk).NA=NA
           print #1,"NB y NA de RollaTrack cargando un Roll en cancion ", NB, NA 
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
     print #1,"update ntk, Nombre Cancion", NombreCancion ' es el path del directorio de cancion
     print #1,"update de NumPista ntk ", ntk
     print #1,"update,nombre del track con  path no se toca ",nombre
   EndIf
Else
' no crea solo convierte roll a rtk y graba, o solo graba de rtk a rtk
' el archivo debe existir y estar cargado
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
' hasta aca es igual     
print #1,"FIN GrabarRollaTrack ,cambiaext ",cambiaext
print #1,"FIN GrabarRollaTrack ,maxpos,posn ",maxpos,posn

print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   
 
End Sub
'---------
Sub RollaTrack(Track() As sec, ntk As Integer,Roll As inst)
' 02-12-2021
' condicion Roll debe estar cargado en el editor entonces copiamos Roll a Track
' si es ntk=0 estamos copiando el gemelo de Roll en Track...
' MaxPos es global y lim2 tambien no s eporque -2 debo copiar todo
Dim As Integer j,i3,i2,i1
'------------------------------------------
' basada en ActualizarRollyGrabarPista pero en un solo paso, dejare una aola ? veremos
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim2)


i3=0
' copia en TrkTemp RollTemp donde esta Roll modificado
For i2 = 1 To MaxPos
   'print #1,"i2",i2
   i3=0
   For i1=NB To NA 
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
            print #1,"Error mas de 12 elementeos de un acorde"
            Exit For '13-09-2021 tenia 2 for salia del todo ja  
         End If
      EndIf
   Next i1
   
   If i3 >=2 Then
    print #1,"copia acorde ",i3," en ",i2
    Track(ntk).trk(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
Track(ntk).trk(1,1).inst=Roll.trk(1,NA).inst  
Print #1,"Fin copia de Roll a Track, RollaTrack"

 
End Sub
' ---------------------
Sub TrackaRoll (Track() As sec, byref ntk As Integer, Roll As inst)
' como es track a Roll se supone que el track ya esta cargado en memoria
' y esta sub pasa de Track a Roll Visual y track 0 
Dim As Integer i1,i2,i3,Maxposicion
 ' print #1,"TrackaRoll 1"   
''If ubirtk=3 Then ' estoy conmutando de track durante la edicion
' si no estoy en cancion el nto va a ntk 0, siemrep uso ntk y el vector de pnTk
print #1,"-------------ARRANCA TRACKAROLL---------------------------------"
print #1,"NTK Y nombre que llego a TrackaRoll ",ntk ,titulos(ntk)
print #1,"maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
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
   
   print #1,"TrackaRoll ntk, desde, hasta, MaxPos ", ntk, desde,hasta,MaxPos
   desdevector=desde
   hastavector=hasta
   estoyEnOctava =desde
   estoyEnOctavaOld =desde
   CantTicks = pmTk(ntk).Ticks '
   print #1,"TrackaRoll, NB, NA, CantTricks", NB,NA, CantTicks
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
       print #1,"REDIM DE TRACK 0, NO SE HACE SIN CANCION O CON ROLLCARGADO"
       ' SE PREPARA TRACK 0 PARA RECIBIR DATOS DE TRACK X,PUES PARA IR A ROLL
       ' SE LLENA TRACK 0 SIEMRPE.,,,
       ReDim (Track(0).trk ) (1 To CantTicks,1 To lim2)
   EndIf
   nota=0:dur=0
   inicioDeLEctura=0
   *po=pmTk(ntk).hasta -1
   
print #1,"TrackaRoll desde, hasta ", desde , hasta
print #1,"TrackaRoll NB, NA ,*po, MaxPos ", NB , NA, *po,MaxPos
print #1, "ubound (Track(ntk).trk,2) ", ubound (Track(ntk).trk,2)
print #1, "lbound (Track(ntk).trk,2) ", LBound (Track(ntk).trk,2)
print #1," va a ejecutar 510 de Track aRoll"

' 1) carga de Roll desde track ntk
For i2 = 1 To MaxPos 
   For i1=1 To lim2
      If Track(ntk).trk (i2,i1).nota > 0 and Track(ntk).trk(i2,i1 ).nota <=NA Then
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
     '    print #1,"Roll.trk(i2,i3).nota ",Roll.trk(i2,i3).nota
         'ers=11- semitono  + *po * 13
         '    11 - semitono +(*po) * 13)  103 notacur  12
         '  11 - 12 + 9*13 = -1+ 117 = 116
         Roll.trk(i2,i3).dur  = Track(ntk).trk(i2,i1).dur
         Roll.trk(i2,i3).vol  = Track(ntk).trk(i2,i1).vol
         Roll.trk(i2,i3).pan  = Track(ntk).trk(i2,i1).pan
         Roll.trk(i2,i3).pb   = Track(ntk).trk(i2,i1).pb
         Roll.trk(i2,i3).inst = Track(ntk).trk(i2,i1).inst
                
     '    print #1,"Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
      Else
 ''        print #1,"nada nota,dur ",Roll.trk(i2,i3).nota,Roll.trk(i2,i3).dur
                
      EndIf
   Next i1
Next i2     
' ajuste de notas no usadas con 0, si uso 181 no se puede cargar notas
' el 181 es durante la carga para evitar retrocesos en la carga de notas
' y poder entrar acordes
' ¿y si tiene datos ? debrai pregunta por MAxpos si es mayor a 2,no hizo falta
For i2 = 1 To MaxPos 
  For i1=NB To NA
   If Roll.trk(i2,i1).nota =0 And Roll.trk(i2,i1).dur <>182  Then
      Roll.trk(i2,i1).nota=0
      Roll.trk(i2,i1).dur=0
   Else
   ''print #1,i2,Roll.trk(i2,i1).nota,Roll.trk(i2,i1).dur
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

  print #1,"TrackaRoll Fin,,maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
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
''Dim pasoCol (0 To 384) As vec  ' entrada de durciones a medida que barro una columna

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
' print #1,                    "-----------------------------------------"
  comienzo=posicion
  cntold=0
  If pasoZona1 > 0 Then
    comienzo=pasoZona1
  EndIf

  If pasoZona2 > 0 Then
    final=pasoZona2
  EndIf





  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
' Esto es play de pistas el limite de barrido es 1 a 12 siempre en c/pista  
' debo hcer un for para cada pista, en cada psita ver si esta mute o play
' ajustar el instrumento de la pista cada vez que cambie la pista.

print #1,"[[[[ PCA CANCION MAXPOS AL COMENZAR TODO,final]]]",Maxpos,final

For jply=comienzo To final
  print #1," ---------------000000000000000000000-----------------"
  print #1," [[[PCA 0:]]]---START--PASO:[";jply;"] ----------------"
  print #1," ---------------000000000000000000000-----------------"
  
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
   duraold=0 ' 04-11-2021

 For pis =1 To tope ' loop de pistas
   print #1,"---UP-----pista:";pis;" --------------------------------"
   print #1,"de esta pista MAXPOS ,final",pmTk(pis).MaxPos, final
    limsup=UBound (Track(pis).trk,2)
    If limsup < lim2 Then
          Exit For 
    EndIf  
    If pis > tope Then
       Exit For
    EndIf
  '''final=pmTk(pis).MaxPos

      
' ojo con silencios ligados !!!

' limites para cada pista NB,NA,son limites para las Notapiano
' si se grabo con un limite de octavas las Notapiano tambien estan limitadas  
   NB = 0  + (pmTk(pis).desde -1 ) * 13 
   NA = 11 + (pmTk(pis).hasta -1 ) * 13 
   print #1,"Play Cancion NB ,NA, pis",NB,NA,pis
   
   Dim As ubyte NBpiano= CUByte(NB) - CUByte(restar(NB))
   Dim As ubyte NApiano= CUByte(NA) - CUByte(restar(NA))
   print #1,"limite inferior NBpiano ",NBpiano
   print #1,"limite Superior NApiano ",NApiano
' en realida seria bueno no tner limites para los tracks, cada track
' tiene polifonia de 12 o sea un acorde de 12 notas, ergo
' la cancion podria tener 12*32tracks = 384 de polifonia, no se si rtmidi lo soporta
' 
' info del inst en 1 no en lim2   
   For i1=1 To lim2   ' coo voy de 1 a lim2 necesito que la info del int este en 1

     If (Track(pis).trk(jply,i1).nota >= NBpiano) And (Track(pis).trk(jply,i1).nota <= NA) And (Track(pis).trk(jply,i1).dur >=1) And (Track(pis).trk(jply,i1).dur <= 180) Then ' es semitono
         Notapiano = Track(pis).trk(jply,i1).nota
         print #1,"Notapiano ",Notapiano
         print #1,"i1,NBpiano,NApiano ",i1,NBpiano,NApiano
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
          print #1,"cnt ";cnt;" iguales ";iguales
       EndIf
       If reldur(duraOld)<>reldur(dura) And cnt > 1 Then
          distintos=1
          print #1,"cnt ";cnt;" distintos ";distintos
       EndIf         

        print #1,"-> cnt"; cnt 
         pasoCol(cnt).DUR =dura
'         pasoCol(cnt).DURold =dura
         print #1,"pasoCol(cnt).DUR ", pasoCol(cnt).DUR 
         If pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <=180 Then
            print #1,"nota con + es una liga"
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
 '        pasoCol(cnt).notapianoOld=Notapiano 
         
         pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * d11
         pasoCol(cnt).i1 = i1 'posicion vertical en el vector real
  '       pasoCol(cnt).i1Old = i1 'posicion vertical en el vector real

      ' 20-06-2021 eliminado duraold=dura repetido 
     '' ??VER SI SAO DE ACA INST Y VEL 
         pasoCol(cnt).inst=Track(pis).trk(1,1).inst  
         vel= vol( dura, velpos)
   EndIf
' llegamos al final de la Columna
   
     If i1=lim2  And (pis = tope Or pis=32) Then
 
        If cnt > 1 Then' Acorde
          print #1,"i1=";i1 ; " ACORDE cnt= ";cnt
        Else    
           print #1,"i1=";i1 ; " SIMPLE cnt= ";cnt
        EndIf  

        Select Case cnt
          Case 1 
           ' con y sin liga
           ' INCLUYE LIGADOS O NO LIGADOS
 print #1, "SIMPLE COM ACORDES IGUALES cntold, vel, canal, tiempodur",  cntold, vel, canal,tiempoDur
  ' cnt por cntold 04-11-2021           
  ''    noteSimple  pasoCol(), cnt, vel, canal,tiempoDur,velpos
         TipoAcorde=1 ' simple   
         AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll,velpos,pis
         pasoCol(cnt).notapianoOld    = Notapiano             
                     
          Case Is > 1

            If iguales=1 And distintos=0  Then
                TipoAcorde=2 ' iguales
                print #1,"cnt ";cnt;" Acordeiguales "
                
                AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll,velpos,pis
                
            EndIf
            If  distintos=1 Then
               print #1,"cnt ";cnt;" AcordeDistintos"
                TipoAcorde=3 ' distintos               
                AcordeDistintos pasoCol(),cnt, cntold,vel,canal,tiempoDur,Roll,velpos,pis
                
            EndIf
            
         End Select  

        cntold = cnt
        print #1,"cnt,cntold"; cnt;" ";cntold
  
     EndIf 
   Next i1
  print #1,"---DOWN -----PISTA:"; pis;" --------------------------------" 
 mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
 If playloop=1 And jply= final Then
    jply=comienzo -1
    'posicion=comienzo
 EndIf
 tiempoDUR=60/tiempoPatron '13-07-2021 cambiamos velocidad durante el play!!!

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
play=0 
playb=0
mousey=100 'otra mas para evitar rentrar a play en menu
finplay=1

mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
fueradefoco=0
 alloff( 1 )

Sleep 100,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
/'
close_port(midiout)
out_free(midiout)
'/ 

ThreadDetach(thread1) 'JMG REPONER !!!!
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
If CANCIONCARGADA Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION?
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
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim2)
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
If CANCIONCARGADA Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION?
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
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim2)
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
   Dim As UByte menor, mayor
   Dim As Integer i1men,i1may,posdur
' en modo lectura la posicion de una nota en el vector es posdur, la 1er parte es en la pantalla
' la 2da posishow es en el vector hasta el inicio de la pantalla 
   posdur= (mousex- gap1 )/anchofig + posishow
   menoryMayorEnColumna (Roll, posdur,menor,mayor,i1men,i1may)
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

' numeroFrac es lo q entra el usuario por teclado , sino entro nada se toma la menor

Print #1,"numeroFrac DUR", numeroFrac, DUR 
If numeroFrac= 0 Then
   numeroFrac=menor
EndIf

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
If CANCIONCARGADA Then ' ¿FRACCIONAR TODAS LAS PISTAS DE UNA CANCION?
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
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim2)
RollaTrack Track(), ntk,Roll

ReCalCompas (Roll)

EndIf

End Sub