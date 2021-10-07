On Error GoTo errorTrack

Sub CargarTrack(Track() As sec, ByRef ntk As Integer , ByRef ubirtk As integer)
' solo carga track no lo pasa a Vector de Visualizacion. ntk debe venir informado
' para cargar desde disco un track se usa esta sub y luego TrackaRoll para verlo
' y editarlo
  print #1,"----------------------------------------------"
  Print #1,"1) ENTRA A CargarTrack nombre, ntk= ",nombre , ntk   
     Dim z (1,1 )  As poli
     Dim zLim (1,1) As poli
     Dim As Integer ubi1,ubi2 
     Dim As String x,x1,x2,x3,x4,nombrea
  '1) cargar pista desde disco y desde Roll puro   
     If ubirtk=0  Then 
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
     x=x1+x2+x3+x4
        Print #1,"reconstruccion x pos bin ", x
     'toda carga de track se guarda en pmTk sea ntk=0 u otro valor   
     pmTk(ntk).MaxPos=CInt("&B"+x)   
     Print #1,"pmTk(ntk).MaxPos ", pmtk(ntk).MaxPos
     '|--> LLEVAR A TRACK A ROLL posicion = 1
     '|--> LLEVAR A TRACK A rOLL nota=0 '''notaOld
     '|--> LLEVAR A TRACK A rOLL    inicioDeLectura=0' Int(Maxpos/NroCol)
' ==> aca no lo esta cargadno a roll visual solo a un track ntk
     pmTk(ntk).posn=pmTk(ntk).MaxPos - 2
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
        posicion=1
        curpos=1
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
     curpos =>0

     '''''ReCalCompas(Roll) '''No es vEctor de Visualizacion      

      Print #1,"CargarTrack  fin"  
      print #1,"----------------------------------------------" 
      Sleep 100
End Sub
' ----------------------------------
Sub GrabarRollaTrack (cambiaNombre As Integer, enCancion As Integer)
' GRABO ROLL EN EDICION COMO PISTA PARA ELLO DEBE ESTAR LIBRE en disco
' LA PISTA 0,como nombre de archivo.[0]-..,, esto sirve para archivos tipo Roll 
' al cargarlos solo queda en Roll?? ahora no cuanDo cargo Roll se carga en track(0)
' tambien. Tambien se carga en TRack a medida que cargo las notas
' pero si modifico el Roll, todavia las estoy pasando los cambios a trcak..
' una forma por ahora es ahcer un Roll modificarlo todo lo qeu necesita 
' y al terminarlo grabarlo como track ..ROLLATRACK EN EL MENU.(GrabarRollaTRack())
' En edicion de Roll en track al ingresar nota por nota es cuadno se carga en Roll
' y en track...pero las modificacione sno van a track por ahora..
' cuando cargue una pista desde archivo se pasa a Roll
' por default siemrpe usa [0] podremos cambiarlo agregando mas codigo SI SE DESEA
Print #1,"---------------------------------------------------------------------------------"
Print #1,"inicia GrabarRollaTrack en Cancionfunciona como un Dir y contador de archivos ",cambianombre,enCancion 
   Dim As String no1, no2
   Dim As Integer ubi1=0,ubi2=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de rool a track, i tengo una cancion en edcion
' y queiro un roll nuevo no conviene hacerlo aca si hay cancion no se usa
    ubi1=InStrRev(nombre,"\")
    ubi2=InStrRev(nombre,".")
    no1= Mid(nombre,1,ubi1) ' path
    no2= Mid(nombre,ubi1+1,ubi2-1-ubi1) ' nomre archivo sin extension
    Print #1,"no2 nombre sin extension ni path ",no2
    ' convierte rool a rtk sin cancion y lo graba a  track 0
    ' O SEA CANCIONCREADA=FALSE CANCIONCARGADA=FALSE
   If cambianombre > 0 And nombre > ""  And enCancion=0 Then  
      nombre=no1+"[00]"+no2 +".rtk" 'path + 0 + rtk por default si no hay cancion
      Print #1,"armado de nombre ",nombre
   EndIf
   ' convierte roll a trk y graba en cancion 
   If enCancion > 0 Then ' BUSCO EL NUMERO DE PISAT PROXIMO
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
     ' hago una copia, comodiferenciarlo? con un indicador de si cargu eun roll..
     ' 2) y el otro estoy en cancion , pero cargue un roll fuera de cancion y 
     ' la quiero convertir a track y poner en cancion.
     '1) estoy grabando un track en edicion con cancion, por lo tanto
     ' se crea un nuevo track copia con otro numero pero debe tener
     ' todos los parametros anteriores..luego debo ahcer una grabacion de
     ' track que haga sobreescritura no nuevo track eso se manejara dede
     ' control
     ' CANCION CARGADA TIENE TRACKS CANCION CREADA NO
     If CANCIONCARGADA And ROLLCARGADO=FALSE Then ' copia track en otro track
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
   EndIf
   Dim grt As Integer
   grt = FreeFile
  
   If Open (nombre  For Binary Access write As #grt ) <> 0 Then
     Print #1,"Error open Rollsub 2727, nombre ",nombre
     Exit Sub
   End If
   Print #1, "nombre tiene el path ",nombre
   Print #1, "NB , NA,MAXPOS ",NB,NA,MaxPos  

 ReDim Trabajo (1 To Maxpos, NB To NA) As dat

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

Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    Trabajo(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    Trabajo(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  
' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir

 Dim grabaPos (1,1)  As poli
 Dim grabaLim (1,1)  As poli
  
 Dim Temp (1 To MaxPos,1 To lim2) As poli

i3=0

For i2 = 1 To MaxPos
   i3=0
   For i1=NB To NA 
      If Trabajo(i2,i1 ).nota >= 1 and Trabajo(i2,i1 ).nota <=12 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         Print #1,"copia Tabajo a Temp en GrabaRollaTrack"
         i3=i3+1
         Temp(i2,i3).dur  =CInt(Trabajo(i2,i1 ).dur)
         Temp(i2,i3).nota =Trabajo(i2,i1 ).nota
         Temp(i2,i3).vol  =Trabajo(i2,i1 ).vol
         Temp(i2,i3).pan  =Trabajo(i2,i1 ).pan
         Temp(i2,i3).pb   =Trabajo(i2,i1 ).pb
         Temp(i2,i3).inst =Trabajo(i2,i1 ).inst
         
         PianoNota= i1 ' nR=i1 es el indice de Roll 06-09-2021 N!=115
         ' cuanta al reves desde ocatva mas aguda a la mas grave,,,
         ' no lo cambiare o podri aahcer lo veremos 
         ' convertimos a PianoNota
         PianoNota= PianoNota - restar (PianoNota)
          
         ' track tendra directamente el valor del piano para tocar con rtmidi
         Temp(i2,i3).nota=CUByte(PianoNota)
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
    Temp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2     

Print #1,"termino compia a temp"

If ROLLCARGADO=FALSE Then
   
Print #1,"copia a ntk vector nuevo, se creo a partir de otro y se debe guardar en track ntk"
     For i1=1 To MAxPos
      For i2= 1 To lim2
      Track(ntk).trk(i1,i2)=Temp(i1,i2)
      Next i2
     next i1
Else
' SE ESTA CARGANDO ROLL NUEVO Y GRAVANDO A UN TRACK NUEVO     
EndIf     
     Dim As Integer y1,y2,y3,y4, y5,y6 ' y5 e y6 son los limites NB y NA
     Dim As String a1,a2,a3,a4 ,x
     
     
     x= Bin(MaxPos,16)
     '    Print #1,"Posicion ",Posicion
     'Print "string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     '    Print #1,"a1 a2 a3 a4 ",a1, a2 ,a3, a4

     y1= CInt("&B"+a1)
     y2= CInt("&B"+a2)
     y3= CInt("&B"+a3)
     y4= CInt("&B"+a4)
     '    Print #1, "y1,y2,y3,y4", y1,y2,y3,y4
     ' grabamos maxpos en 4 ubyte
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     '-----------------------
     grabaLim(1,1).nota = CUByte(desde)
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
     '-----------------------------
     Put #grt, ,grabaPos(1,1)
     Put #grt, ,grabaLim(1,1)
     Put #grt, ,Temp()
     cerrar (grt)
     While InKey <> "": Wend
     Sleep 150
Print #1,"FIN GrabarRollaTrack ",cambianombre,enCancion
Print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   

End Sub
' -------------------
Sub TrackaRoll (Track() As sec, byval ntk As Integer, Roll As inst)
' FALTA LLENAR .ACORDE DE TRACK ...
' como es track a Roll se supone que el track ya esta cargado en memoria
' y esta sub pasa de Track a Roll Visual y track 0 
Dim As Integer i1,i2,i3,Maxposicion
 ' Print #1,"TrackaRoll 1"   
''If ubirtk=3 Then ' estoy conmutando de track durante la edicion
' si no estoy en cancion el nto va a ntk 0, siemrep uso ntk y el vector de pnTk
print #1,"-------------ARRANCA TRACKAROLL---------------------------------"
Print #1,"NTK Y nombre que llego a TrackaRoll ",ntk ,titulos(ntk)

   desde  = pmTk(ntk).desde
   hasta  = pmTk(ntk).hasta
   NB     = pmTk(ntk).NB
   NA     = pmTk(ntk).NA
   MaxPos = pmTk(ntk).MaxPos
   posn   = pmTk(ntk).posn
   notaOld= pmTk(ntk).notaold
  
   Print #1,"TrackaRoll ntk, desde, hasta, MaxPos ", ntk, desde,hasta,MaxPos
   desdevector=desde
   hastavector=hasta

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
   If CANCIONCARGADA=TRUE Then ' cargarcancion NO SE USA VIVE muy poco solo durante lA carga  
       Print #1,"REDIM DE TRACK 0, NO SE HACE SIN CANCION"
       ' SE PREPARA TRACK 0 PARA RECIBIR DATOS DE TRACK X,PUES PARA IR A ROLL
       ' SE LLENA TRACK 0 SIEMRPE.,,,
       ReDim (Track(0).trk ) (1 To CantTicks,1 To lim2)
   EndIf
   posicion=1
   nota=0
   inicioDeLEctura=0
   *po=pmTk(ntk).hasta -1
   
Print #1,"TrackaRoll desde, hasta ", desde , hasta
Print #1,"TrackaRoll NB, NA ,*po, MaxPos ", NB , NA, *po,MaxPos
Print #1, "ubound (Track(ntk).trk,2) ", ubound (Track(ntk).trk,2)
Print #1, "lbound (Track(ntk).trk,2) ", LBound (Track(ntk).trk,2)
Print #1," va a ejecutar 1534 de Track aRoll"

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
     '    Print #1,"Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
      Else
 ''        Print #1,"nada nota,dur ",Roll.trk(i2,i3).nota,Roll.trk(i2,i3).dur
                
      EndIf
   Next i1
Next i2     
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
' aca me falta pasar lso datos del Track(x) a Track(0)!! si es cancion
' el redim esta arriba
If cargaCancion=1 Then ' solo mientras cargo cancion? no  
 For i1=1 To MaxPos 
  For i2 = 1 To lim2
  Track(0).trk(i1,i2) = Track(ntk).trk(i1,i2)
  Next i2
 Next i1
EndIf  
  



'--------------------------------Track(0) fin
'MaxPos=MaxPosTrack(ntk)
posicion=1:posn=MaxPos-2
curpos=1
notacur=1
nroCompas=0
tres=0:pun=0:sil=0:mas=0:doblepun=0:cuart=0
tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0

  Print #1,"TrackaRoll Fin"
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

errorTrack:

'Dim As Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error Modulo ROLLTRACKS ", er, posicion, MaxPos
Print #1,Erl, Erfn,Ermn,Err

Print #1,"------------------------------------"
ErrorNumber = Err
ErrorLine = Erl


Print #1,"ERROR = ";ProgError(ErrorNumber); " on line ";ErrorLine
Print #1,"Error Function: "; *Erfn()
ers = nota +(*po -1) * 13
Print #1, "nota +(estoyEnOctava -1) * 13) "; ers
Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)


 