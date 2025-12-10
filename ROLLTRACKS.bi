

Sub ConversorTocaATecla(pistaToca() As ejec, pistaCargarIn()  As midicod)
' conesteconversor puedo tomar un Toca llevarlo a formto CargarIn y los datos usados es menor
' y con ello grabar a archivo...elvector CagarIn()
 
End Sub

'------------

Sub CargarMidiIn(fileMidiIn As String,   ByVal ntkp As Integer, byval version As Integer)
'[ carga un archivo en cada llamada ]
' carga un archivo por cada llamada, ntkp el nro de trak o pista de ejecucion
' la entrada DirEjecSinBarra recibido enCargarMidiIn CONTIENE EL PATCH NO SERIA 
' NTKP ES POR VALOR PASA ELNUMERO DE ARCHIVO DEL LOOP DE CARGARPISTASJECS

      Dim As Integer  j 
      ' nombre tiene el path y extension esta completo
     
  '    pmEj(ntkp).MaxPos=N
 Print #1,"fileMidiIn recibido enCargarMidiIn ",fileMidiIn ' TIENE PATH
 Print #1,"NombreCancion  ",NombreCancion
      Dim As Integer ubi1 ',ubi2 
      ubi1 = InStrrev(fileMidiIn,"\")
 '     ubi2 =InStrRev (DirEjecSinBarra,")")
 '     ntkp=CInt(Mid(DirEjecSinBarra,ubi1+1,ubi2-ubi1-1)) ' el orden o nro de la pista sale del nombre
' pero tambien esta grabado en tocaparam en el archivo
' si hay numero en la string aunque no halla parentesis al comienxo y  el numero este
' al fina lo cualquier parte tomara el numero igual.,,cint descarta las letras,,,
  '    If ubi1=0 Or ubi2=0 And ntkp=0 Then
  '       ntkp=1 'corregimos despues
  '    EndIf  
'''NombreCancion CONTENDRA EL PATH CON LA BARRA AL FINAL
      If  NombreCancion="" And ubi1 > 0 Then
          NombreCancion=Mid(fileMidiIn,1,ubi1)
      EndIf


      If Fileexists(fileMidiIn)=0 Then 
          Print #1,fileMidiIn;"  not found"
          Return
      EndIf 
      
 Print #1,"maxgrb en CARGAmidiin ", maxgrb
      Var  f=12
     If  Open (fileMidiIn For Binary Access Read As #f ) <> 0 Then 
          Print #1,"No se puede leer  "; fileMidiIn
          Exit Sub
     EndIf  
         ''   Dim tocaparamaux As ejecparam
         ''   Dim tocaparamaux2 As ejecparam2 
         ''   Get #f, ,tocaparamaux 'parametros de una ejecucion
     If version= 2 Then 
         Get #f, ,tocaparamCabeza(ntkp) ' DEERMIONA SI ES VERSION VIEJA O NUEVA
         Print #1,"' VERSION 2 NUEVA leo lo siguiente"
         Get #f, ,tocaparam(ntkp)  ' EN VER2 HABRA UNA CABEZA Y LUEGO TOCAPARAM Y TOCAPARAM2
         Get #f, ,tocaparam2(ntkp)
'---------------------         
     Else '''versiosn vieja  
         Get #f, ,tocaparam(ntkp)
     EndIf
' tocaparam  YA SE CARGO ANTES falta tocaparam2 lo neuvo 

      If version=2  Then
' movemos lo nuevo y viejo
Print #1,"ESTAMOS EN VERION 2 !!!!"
         pmEj(ntkp).sonido       = tocaparam2(ntkp).sonido        
         pmEj(ntkp).pan          = tocaparam2(ntkp).pan           
         pmEj(ntkp).Eco          = tocaparam2(ntkp).Eco           
         pmEj(ntkp).coro         = tocaparam2(ntkp).coro
Print #1,"QUE CORO DE MIERDA LEYO ? ", tocaparam2(ntkp).coro          
         pmEj(ntkp).vibrato      = tocaparam2(ntkp).vibrato
         tocaparam2(ntkp).ejec   = 1 
         pmEj(ntkp).ejec         = tocaparam2(ntkp).ejec          
         pmEj(ntkp).canalx       = tocaparam2(ntkp).canalx        
         pmEj(ntkp).canalsalida  = tocaparam2(ntkp).canalsalida   
         pmEj(ntkp).vol          = tocaparam2(ntkp).vol 
Print #1,"//tocaparam2(ntkp).vol cargado ",tocaparam2(ntkp).vol          
         If pmEj(ntkp).vol=0 Then
            pmEj(ntkp).vol=127
Print #1,"---ajusto a 127 tocaparam2(ntkp).vol !!"
         EndIf  
         pmEj(ntkp).TipoCompas    = tocaparam2(ntkp).TipoCompas

         Dim mit As aUshort
         mit.tp1 = tocaparam2(ntkp).tiempoPatron1
         mit.tp2  = tocaparam2(ntkp).tiempoPatron2
         tiempoPatron=mit.ST
         If tiempoPatron = 0 Then 
            tiempoPatron = 60
         EndIf
         pmEj(ntkp).tiempoPatron=tiempoPatron    
         pmEj(ntkp).pitchbend     = tocaparam2(ntkp).pitchbend     
  ELSE             
  Print #1,"ESTAMOS EN VERION 1 !!!!"
'EN VERSION 1 NO LEE LA PARTE 2 DE ENCABEZADO ERGO FALTARIA VOLUMEN Y TIEMPOPATRON EJEC
      If pmEj(ntkp).vol=0 Then
         pmEj(ntkp).vol=127
      EndIf  
      If tiempoPatron = 0 Then 
         tiempoPatron = 60
      EndIf
 EndIf
      pmEj(ntkp).ejec =1 'SIEMPRE PORQUE ES EJECUCION VIVA
'------------------------------------------
      Print #1,"Open DirEjecSinBarra: tocaparam(ntkp).maxpos ",tocaparam(ntkp).maxpos
  ''maxpos es de cancion mxgrp de ejecucion aca maxpos no se debe usar nunca
  ''''    maxpos=tocaparam(ntkp).maxpos ' 28-11-2024 ,,ACA LAPISA ESTA MAL
      nombre=tocaparam(ntkp).nombre
      maxpos=0
    ' maxgrp se calcula en CargarPistasEjec  
Print #1,"////////redimendsiona a 2*maxgrb ",2*maxgrb
' se redimensionas todos iguales a 2*maxgrb         
      ReDim (Toca(ntkp).trk ) (1 To 2*maxgrb) ''
'----------nuevo orden dinamico
      tocaparam(ntkp).orden=ntkp 
       Get #f, ,Toca(ntkp).trk()

'Print #1,"ubound ",UBound (Toca(ntkp).trk)
''For j As Integer=1 To tocaparam(ntkp).maxpos
''Print #1, Toca(ntkp).trk(j).modo;" ";Toca(ntkp).trk(j).nota;" ";Toca(ntkp).trk(j).vel
''Next j 
      ' cerrar f
       Close 12
          
'      pmEj(ntkp).MaxPos=Toca(ntkp).maxpos
      ntoca=tocaparam(ntkp).orden
    

'      Print #1,"CargarIn maxpos,np  ",tocaparam(ntkp).maxpos , ntkp
'      Print #1,"Cargar delta ",tocaparam(ntkp).delta
'      Print #1,"Cargar nombre ",tocaparam(ntkp).nombre
'      Print #1,"Cargar orden ",tocaparam(ntkp).orden 
'      Print #1,"Cargar portout ",tocaparam(ntkp).portout
'      Print #1,"Cargar portin ",tocaparam(ntkp).portin
'      Print #1,"Cargar patch ",tocaparam(ntkp).patch
'      Print #1,"Cargar canal ",tocaparam(ntkp).canal
' el ultimo valor de ntkp queda en calltoca... 
   '  calltoca=ntkp no se usa

   '  viv.trk()=Toc()

End Sub

Sub CargarPistasEjec (lugar As String, ByRef ntkp As Integer)
' devuelve ntkp topeejec!!
' lugar es el path de  la carpeta seleccionada 
' cada vez que cargo borro la info de fechas de pistas anterior
' para usar esta carga hay que crear un Dir de cancion y abrirlo
'podra haber pistas trk o ejec o ambas,veremos si se pueden sincronizar   
' devuelve el numero de pistas...cargadas
' y fija la maxgrb o la maxpos de todas las pistas ejec
'NTKP ES EL TOPE al terkminar el procedimiento ntkp=nf
' no deberia grabar mas el nombre  de la pista,el nombre sera la del archivo
'asi el ussuario puede renombrarlo a su gusto...y no hay q ue hacer lio de renombrar
' dentro ni usar rename que anda para la mierda ,,,vsmos a guardar siempre el nombre
' del archivo en disco a la estructura para consultar durante el proceso 
' o sea si luego de cargar y procesar queremos grabar usaremos el nombre de la parm
' o el que esta en la lista visual. Lo que conviene HACER ES CARGAR DE DISCO
' EN VECTOR PARAM GUARDARLO SIN PATH PERO CON LA EXTENSION *.EJEC
' EN LA LISTA COLOCARLO SIN LA EXTENSION. Y AL GRABAR AGREGARLE EL PATCH AL NOMBRE 
' EN EL PARAM...LOS NOMBRES DE LA LISTA NO SE USAN,,,
ROLLCARGADO=FALSE
Print #1,"LUGAR RECIBIDO, ntkp maximo ";lugar ,ntkp
TopeEjec=ntkp
GrabarEjec =HabilitaGrabar: ntoca=0 : arrancaPlay=NO
Dim As Integer version
''''         ntkp=1 '<================= inicia en 1 y va a devolver ntkp!!
' ahi estaba el error empieza en 1 no en cero veremos! 
s5=2 '11-06-2022 control de pantalla
''ReDim As Double fechasPistas(1 To 32)
print #1,"-------------------------------------------------------"
print #1,"inicia CargaPistasEjec ejecuta 1 sola vez los loops son internos devuelve ntkp "
  Dim As Integer ubi1=0,ubi2=0 
     Dim As String filenameold
     ' el Dir me trae lso nombres sin el path de cancion, EL PATH POSTA COMPLETO ES NOMBRECANCION

     If  lugar ="" Then
         DirEjecSinBarra = Dir ("*.ejec")
     Else
         DirEjecSinBarra = Dir (lugar+"\*.ejec")
     EndIf
' el dir da el orden DE CARGA POR ALFABETO PARA ELPLAY NO IMPORTA EL ORDEN DE CARGA YA  SE HA PROBADO
'
' 1) DETERMINA EL MAXGRB DE LA CANCION MIRANDO EN TODOS LOS ARCHIVOS
'     PARA AL CARGAR DIMENSIONAR TODOS LOS VECTORES DE CARGA AL MISMO
'     VALOR MAXIMO 
     print #1,"DirEjecSinBarra encancion > 0 ",DirEjecSinBarra
     If DirEjecSinBarra = "" Then ' no hay ningu archivo dentro del dir de cancion
        ntkp=1
        print #1,"dice que no hay archivos "
        EJECCARGADA=FALSE
        ' NO HAY NADA QUE CARGAR ESTO NODEPENDE DE ROLL
        Exit Sub
     Else
       ' carga Todos los tracks de cancion en un Loop
      'busco maxgrb o sea el maxpos de todas las pistas para evitar el redim preserve
       Dim  fileMidiIn As String
       If  lugar > "" Then 
          fileMidiIn=lugar+"\"+DirEjecSinBarra
       Else 
          fileMidiIn=DirEjecSinBarra
       EndIf 



       Dim  As Integer nf=0
' OBTIENE EL MAXGRB PARA REDIMENSIONAR LOS VECTORES A CARGAR  
       Do While Len(DirEjecSinBarra) > 0
           Print #1,"DirEjecSinBarra maxgrb ",DirEjecSinBarra
           Print #1,"fileMidiIn  maxgrb ",fileMidiIn

           nf =nf+ 1
           Var  f=13
           If Open (fileMidiIn For Binary Access Read As #f ) <> 0 Then 
              Print #1,"No se puede leer fileMidiIn CargarPistasEjec "; fileMidiIn
              Exit Sub
           End If
           Get #f, ,tocaparamCabeza(nf) ' DETERMIONA SI ES VERSION VIEJA O NUEVA
           Dim fecha1 As String 
           fecha1 =Format (tocaparamCabeza(nf).fecha ,"ddddd")
           Print #1, "fecha1 ",fecha1
           Print #1, "tocaparamCabeza(nf).fecha",tocaparamCabeza(nf).fecha
 'cantidad de dias desde 30 dic 1899,1900 a 2025 son 125 años
 ' 125 * 365 = 45625
          If IsDate(fecha1) <> 0 And  tocaparamCabeza(nf).fecha > 45000 Then
             Print #1,"cargando archivo ejec version 2"  '' corre ok
             ' VERSION 2 NUEVA leo lo siguiente
             version=2     
             Get #f, ,tocaparam(nf)  ' EN VER2 HABRA UNA CABEZA Y LUEGO TOCAPARAM Y TOCAPARAM2
          Else
             Close f
             Sleep 50
             Open fileMidiIn For Binary Access Read As #f
             Print #1,"cargando archivo ejec version 1"
             version=1   
             Get #f, ,tocaparam(nf)
          EndIf
' CON EJEC2 tocaparam0 tendra otros datos a definir y recien empieza
' lso datos de antes mas nuevos en tocaparam y tocaparam2 e l 2 aca no hace falta solo quereemos maxpos
' para obtener el maxgrb de todas las pistas   
'---------------------------
' solo interesa la 1er parte del ejecparam porque tine el maxpos
           Print #1,"tocaparam(nf).maxpos ",tocaparam(nf).maxpos

           If tocaparam(nf).maxpos > maxgrb Then
              maxgrb = tocaparam(nf).maxpos
           EndIf
           Close f
         Print #1," ORDEN DE LECTURA EJEC nf ", nf
         Print #1,"PARA FILEMIDIIN  ",FILEMIDIIN
          tocaparam(nf).orden=nf

'------------------------------------------------------------------------------------------------
         pmEj(nf).portout= tocaparam(nf).portout
         pmEj(nf).portin = tocaparam(nf).portin
         pmEj(nf).patch  = tocaparam(nf).patch
         pmEj(nf).canalsalida =tocaparam(nf).canal
         pmEj(nf).MaxPos      =tocaparam(nf).maxpos
         pmEj(nf).canalentrada=tocaparam(nf).canalent
         pmEj(nf).orden      =tocaparam(nf).orden
         Dim k1 As Integer
         k1=InStr(DirEjecSinBarra,".")
         Dim As String nomSinExt
         nomSinExt= Mid(DirEjecSinBarra,1,k1-1)         
         tocaparam(nf).nombre=nomSinExt  ' SOLO EL NOMBRE DEL ARCHIVO MAXIMA LONGITUD 29, almacena
         titulosEj(nf)=fileMidiIn ' temporario completo con extension y path
         pistasEj(nf) =fileMidiIn
  
           Sleep  100
           DirEjecSinBarra = Dir() 'trae el siguiente nombre de archivo en el directorio

           If  lugar > "" Then 
             fileMidiIn=lugar+"\"+DirEjecSinBarra
           Else 
             fileMidiIn=DirEjecSinBarra
           EndIf 


       Loop
   
      tocatope=nf
      TopeEjec=nf   
      ntoca=tocatope

      Print #1,"MAXIMA LONG DE PISTA maxgrb y cant de pistas  nf ",maxgrb,nf
      maxcarga=maxgrb 
      SetGadgetText(TEXT_TOPE, Str(maxcarga))
'termino con  todoslos archivos si llamo de nuevo con argumento
' empieza de nuevo  
'*********************************************************************     
'-2)----------------CARGA LOS DATOS DE CADA PISTA
'*******************************************************************
       Dim  As Integer mayor=1
       If  lugar ="" Then
           DirEjecSinBarra = Dir ("*.ejec")
      Else
          DirEjecSinBarra = Dir (lugar+"\*.ejec")
      EndIf
       Dim np As Long=0
  ' va cargando los track internos, tomando el nro de trackp 
  ' del nombre del archivo solamente , ergo el usuario puede cambiar el orden
  ' o poner un trakcp de otra cancion con un numero que no exista [x]
  '      
        
       Do While Len(DirEjecSinBarra) > 0 ' If len(filename) is 0, exit the loop: no more filenames are left to be read.
        filenameold=DirEjecSinBarra
        print #1, "trabajo con este filename...", filenameOld '[1]AAA.EJEC por ejemplo
        np=np+1
       ' Var N=Filelen(DirEjecSinBarra)\Sizeof(ejec)
       'Print #1,"CargarMidiIn N LEIDOS",N
        fileMidiIn=lugar+"\"+filenameOld
        Print #1,"CargarMidiIn  fileMidiIn ",fileMidiIn

        
        CargarMidiIn (fileMidiIn,  np,version)  
                
        Dim cadena As String
        ''''cadena= sacarExtension(filenameOld) ' [1]AAA
        cadena = tocaparam(np).nombre ' este ntkp sale del nombre del archivo el nro ej: (03) 
' pero la carga usa np que deberia coincidir....o no si borro una pista sinrenombrar 01,03,04 ... borre la (02) 
         AddListBoxItem(PISTASEJECUCIONES, cadena,np-1)
          Sleep 1                         
        '''''ntkp=CInt(np) no hace falta
      Print #1,"tocaparam(ntkp).maxpos ",tocaparam(np).maxpos
        pmEj(np).MaxPos=tocaparam(np).maxpos
        
         Print #1,"pmEj(ntkp).MaxPos ",pmEj(np).MaxPos

        print #1,"nombre en CargarPistasEjec ,Ntkp ",DirEjecSinBarra, np
        Sleep 100
        DirEjecSinBarra = Dir()
       If  lugar > "" Then 
          fileMidiIn=lugar+"\"+DirEjecSinBarra
       Else 
          fileMidiIn=DirEjecSinBarra
       EndIf 

        Print #1,"siguiente ", DirEjecSinBarra         
       Loop



       print #1,"CARGO PISTAS MAXIMA CANTIDAD TOPETOCA=",tocatope
       Print #1,"UBOUND TOCA ",UBound(Toca(np).trk)
       EJECCARGADA=TRUE
       
    EndIf

''''cargaCancion=CARGAR_NO_PUEDE_DIBUJAR '12-02-2022 mientras carg las pista el 1 indica cargando pistas    
    print #1,"FIN CargaPistasEjec"
    print #1,"-------------------------------------------------------"
'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0
'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 
      Parar_De_Dibujar=NO ' habilitar play
      
End Sub



Sub CargarTrack(Track() As sec, ByRef ntk As Integer , ByRef ubirtk As Integer) '', ByRef check As UByte)
On Error GoTo saledeaca
' solo carga track no lo pasa a Vector de Visualizacion. ntk debe venir informado
' para cargar desde disco un track se usa esta sub y luego TrackaRoll para verlo
' y editarlo, ubirtk es si al carga se hace por argv desde el explorer en disco
'abrirRoll=0  
cargacancion=NO_CARGAR_PUEDE_DIBUJAR 'PUEDE DIBUJAR PORQUE NO HAY REDIM  DE ROLL
' NO CARGAR CANCION PORQUE SON TRACKS...Y ACA CARGAMOS UN SOLO TRACK..
''TRACKCARGADO =FALSE ' CONTROL DE TRACK(0)
' el track puede haber sido echo  desde una ejecucion real ejec=1 o manual ejec=0.
  print #1,"----------------------------------------------"
  print #1,"1) CargarTrack nombre, ntk= ", nombre , ntk  
  Print #1,"1) UBIRTK QUE LLEGA ", ubirtk
 
     Dim grabaPos   As poli
     Dim grabaLim   As poli
     Dim graba3     As poli ' 04-02-2022 se agregan 48 bytes para info futura 
     Dim graba4     As poli 
     Dim graba5     As poli 
     Dim graba6     As poli 
     Dim graba7     As poli 
     Dim graba8     As poli 
     Dim graba9     As poli 
     Dim graba10    As poli 
   'Print #1,"termino dimension de grabas "  
     Dim As Integer ubi1,ubi2 
     Dim As String x,x1,x2,x3,x4,x5,nombrea
  '1) cargar pista desde disco y desde Roll puro
   
     If ubirtk = 0   Then ' no tengo nombre debo explorar ACA SI BUSCA SOLO LOS RTK NO LOS *.SOLO
           myfilter  = "Track files (*.rtk)"+Chr(0)+"*.rtk"+Chr(0)
           nombrea = OpenFileRequester("","", myfilter, OFN_CREATEPROMPT)
           Sleep 100

           ubi1 = InStrrev(nombrea,"[")
           ubi2 =InStrRev (nombrea,"]")
           If ubi1=0 Then
              ubi1 = InStrrev(nombrea,"(")
           EndIf
           If ubi2=0 Then
              ubi2 = InStrrev(nombrea,")")
           EndIf
           If NombreCancion = "" Then 
              ntk=0
           Else 
              ntk=CInt(Mid(nombrea,ubi1+1,ubi2-ubi1-1))
           EndIf      '
     Else
  '2)  carga *.rtk de linea de comando doble clik o de cancion   
       If ubirtk > 0 Then ' ENTRAN TAMBIEN TODAVIA LOS *.SOLO
        '  print #1,"ubirtk > 0 carga de disco o cancion ",ubirtk
          nombrea=titulosTk(ntk) ' ya venia el nombre
          ubirtk=2 '31-0325
       EndIf   
     EndIf
 'Print #1,"paso 1era parte ,nombrea ",nombrea    
 ''''''' ubirtk=0 '''???? 31-03-2025
       If nombrea = "" Then
      ' print #1, "cargaTrack exit sub"
       nombre=""
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
  

    Dim miroerr As Integer
    Print #1,"nombre track ",nombre
    print #1,"NTK Y nombre que llego a open en CargaTrack ",ntk ,nombre
    titulosTk(ntk)=nombre

    
    ct=14
    miroerr= ( Open (nombre  For Binary Access Read As #ct ))
     If miroerr <> 0 Then
        print #1,"arch track  abrio con error 1307 CargarTrack miroerr, nombre",miroerr, nombre
       Exit sub
     EndIf
Print #1,"MaxPos ntk ",pmTk(ntk).MaxPos,ntk

     Get #ct, , grabaPos
     x1=Bin(grabaPos.nota,4)
     x2=Bin(grabaPos.dur,4)
     x3=Bin(grabaPos.vol,4)
     x4=Bin(grabaPos.pan,4)
     x5=Bin(grabaPos.pb,4)

     x=x1+x2+x3+x4+x5
   '     print #1,"reconstruccion x pos bin ", x
     'toda carga de track se guarda en pmTk sea ntk=0 u otro valor   
     pmTk(ntk).MaxPos=CInt("&B"+x)


    '   If ntk >0 Then
    '      maxposTope=84600 '''pmTk(ntk).MaxPos le pone algo fijo ?? NO VA 5-12-2025
    '   EndIf

' AL CONVERTIR UN EJEC A ROLL Y LUEGO A NTK QUEDA EL NTK=1
' TOMA EL NRO DE EJEC Y SI SON SIMPLES SIN CANCION DEBE SER NTK=00 SIEMPRE
' ENTONCES AL CONVERTIR ROLL A TRACK SI ES UN EJEC AJUSTAR NTK=0 
     ''MaxPos=pmTk(ntk).MaxPos 5-12-2025
  '   If ntk >0 And maxposTope < MaxPos Then  MAL 5-12-2025
  '      maxposTope=84600 ''pmTk(ntk).MaxPos 
  '   EndIf
   
   '  print #1,"pmTk(ntk).MaxPos ", pmtk(ntk).MaxPos
     '|--> LLEVAR A TRACK A ROLL posicion = 1
     '|--> LLEVAR A TRACK A rOLL nota=0 '''notaOld
     '|--> LLEVAR A TRACK A rOLL    inicioDeLectura=0' Int(Maxpos/NroCol)
' ==> aca no lo esta cargadno a roll visual solo a un track ntk
     'pmTk(ntk).posn=pmTk(ntk).MaxPos - 2
    ' If pmTk(ntk).posn < 0 Then pmTk(ntk).posn=0 EndIf
     pmTk(ntk).Ticks = pmTk(ntk).MaxPos ''86400 '''1000 - pmTk(ntk).MaxPos 5-12-2025 
   '  If pmTk(ntk).Ticks < 1000 Then 
   '     pmTk(ntk).Ticks = pmTk(ntk).MaxPos+1000
   '  EndIf   
     Dim As Integer ubisolo=InStr(LCase(nombre),".solo")
     CantTicks=CantMin * 96 *tiempoPatron '''pmTk(ntk).Ticks
' NO TOMAMOS LOS SOLO PARA EL CALCULO DE LA MAXPOS
     If CantTicks < pmTk(ntk).MaxPos And ubisolo=0 Then
        CantTicks = pmTk(ntk).MaxPos
     EndIf     
     If ntk >=2 Then ' es una cancion y ahi tiene sentido
 ' para un track solo ntk=0 ,,ntk-1 = -1 daria error  
         If CantTicks < pmTk(ntk-1).MaxPos  And ubisolo=0 Then
           CantTicks = pmTk(ntk-1).MaxPos 'vamos comparando los maxpos
         EndIf     
     EndIf 
  Print #1,"CantTicks "; CantTicks
  ''MaxPosTope=CantTicks  '4-12-2025
     'es un get trabajo debe ser exactamente MAxPos
   '       Print #1,"llego a 2 antes de redim "
  '   If  check = 1 Then 
  '       check = 0
  '       cerrar (ct)
  '       Exit Sub
  '   End If



     tipoescala_num_ini =CInt( grabaPos.nnn ) ' 20-12-2021 - tipoescala en uso
     If tipoescala_num_ini=0 Then
        tipoescala_num_ini=1
     EndIf
     pmTk(ntk).tipoescala=grabaPos.nnn
     pmTk(ntk).ejec=grabaPos.ejec
     
 
  '   Print #1,"Carga Track tipoescala_num_ini ",tipoescala_num_ini

     ReDim Trabajo  (1 To CantTicks,1 To lim3) As poli
Print #1,"NombreCancion,nomobre, CantTicks ";NombreCancion,nombre, CantTicks 
   If NombreCancion > "" Then
     If grabaPos.sonido = 1 Then ' sonido on/off 16-03-2022
        print #1,"grabaPos.sonido",grabaPos.sonido
        CheckBox_SetCheck(cbxnum(ntk),1) 
     Else
        CheckBox_SetCheck(cbxnum(ntk),0) 
     EndIf
   EndIf

   If ubisolo > 0 Then
      CheckBox_SetCheck(cbxsolo(ntk),1) ' kokon 
   EndIf
     ' crgamos limites Roll de octavas

     Get #ct, , grabaLim
     pmTk(ntk).desde  = CInt(grabaLim.nota)
     pmTk(ntk).hasta  = CInt(grabaLim.dur) '01-03 cint
     desde=pmTk(ntk).desde
     hasta=pmTk(ntk).hasta
     If desde=0 Then 
       desde=4
pmTk(ntk).desde=desde 'kuku
       messbox("CARGA TRACK DESDE","esta en cero el  track "+Str(ntk))   
     EndIf
     If hasta=0 Then 
        hasta=8
pmTk(ntk).hasta=hasta 'kuku
messbox("CARGA TRACK HASTA","esta en cero el  track "+Str(ntk)) 
     EndIf

     Print #1,"ntk desde, hasta",ntk, desde, hasta
     pmTk(ntk).notaold= CInt(grabaLim.pb)
' la nota esca y la esca es la misma `para todos lso tracks despues lo debo cambiar
     
     notaescala_num_ini =CInt(grabaLim.vol) ' notadeescala 20-12-2021
     If notaescala_num_ini=0 Then
        notaescala_num_ini=1
     EndIf
     Print #1,"Carga Track notaescala_num_ini ",notaescala_num_ini
     pmTk(ntk).notaescala=grabaLim.vol
     If grabaLim.pan = 3 Then
        alteracion="sos"
     EndIf
     If grabaLim.pan = 2 Then
        alteracion="bem"
     EndIf
     If grabaLim.pan = 0 Then
        alteracion="sos"
     EndIf
     
 '    Print #1,"alteracion ",alteracion
 '    print #1,"desde ",pmTk(ntk).desde
 '    print #1,"hasta ",pmTk(ntk).hasta
     ''' no le da tiempoPatron=CInt(grabaLim(1,1).nnn)

'     print #1,"en la carga de track desde hasta", pmTk(ntk).desde,pmTk(ntk).hasta
     pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13   ' 27 para 3 SI CARGO CANCION NO VA
     pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13  ' 90 para  7 06-09-2021 decia 12 -> es 11
     NB=pmTk(ntk).NB
     NA=pmTk(ntk).NA


 '    print #1,"CargarTrack  NB Na", pmTk(ntk).NB, pmTk(ntk).NA
     
     Get #ct, ,graba3  

     pmTk(ntk).canalsalida = graba3.nnn  ' as poli es trck
     Print #1,"cargatrack pmTk(ntk).canalsalida, ntk ",pmTk(ntk).canalsalida,ntk
     canalx=graba3.nnn    
     pmTk(ntk).portout= graba3.dur
Print #1,"PORTOUT, NTK "; pmTk(ntk).portout, ntk 
     portout = CInt(graba3.dur)
     pmTk(ntk).patch=graba3.patch
Print #1,"EN CARGATRACK PATCH ",pmTk(ntk).patch
     patchsal=pmTk(ntk).patch
     instru=CInt(patchsal)
     If patchsal=0 Then
        patchsal=1
        pmTk(ntk).patch=1 
     EndIf

     pmTk(ntk).eco=graba3.eco
     Globaleco= graba3.eco
     pmTk(ntk).pan=graba3.pan
     Globalpan= graba3.pan

     pmTk(ntk).coro=graba3.canal
     Globalcoro= graba3.canal

     pmTk(ntk).ejec=graba3.ejec

     If graba3.nanchofig > 0 Then
       nanchofig  = cdbl(graba3.nanchofig)/10
     EndIf 
     pmTk(ntk).vol = graba3.vol

    If portsout <= portout Then
 'portsout cantidad de port fisicos o dispositivos empieza por 1 2 3 etc"
  '  portout parte de 0 o sea 0 1 2 3 '
  ' un portout 2 significa 2 dispositivos 1 2 
  ' luego si portsout = 2  then portout maximo es 1
    MessageBox( null, "MIDI-OUT "+ Str(portout)+ " INEXISTENTE CAMBIELO", "Se cambia a 1", MB_OK )
    portout = 0
    pmTk(ntk).portout=0
   EndIf

Print #1, "2 instru en cargar track ";instru
     TipoCompas = graba3.pb  ' 26-04-2024
     TCompas=Mid(tempoString(TipoCompas),1,4) 
 '    print #1,"cargaCancion ",cargacancion

     Get #ct, ,graba4  
     Get #ct, ,graba5  
     Get #ct, ,graba6  
     Get #ct, ,graba7  
     Get #ct, ,graba8  
     Get #ct, ,graba9  
     Get #ct, ,graba10 


Dim mit As aUshort
mit.tp1 = graba4.pan
mit.tp2  = graba4.pb
tiempoPatron=mit.ST
pmTk(ntk).tiempopatron=tiempoPatron
     If tiempoPatron = 0 Then 
        tiempoPatron = 60
     EndIf


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
     If cargaCancion=NO_CARGAR_PUEDE_DIBUJAR  Then ' termino la carga de cancion es otro evento despues carga de un track aislado¿?
        NB=pmTk(ntk).NB  ' por ejemplo cargar un track clickeado en lista
        NA=pmTk(ntk).NA
        desde=pmTk(ntk).desde
        hasta=pmTk(ntk).hasta
        If NA=0 Or NB=0   Then
     pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13  
     pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13
     NB=pmTk(ntk).NB
     NA=pmTk(ntk).NA
 
        EndIf
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
     'ReDim compas(1 To CantTicks,1 to Spis) es solo para visualizar
     'carga=1 ' para visualizar no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi
     cont=0
  '   print #1,"pmtk(ntk).MaxPos, ntk ",pmTk(ntk).MaxPos , ntk
  '   print #1,"ABRIR MAXPOS ,NB,NA "; pmTk(ntk).MaxPos, pmTk(ntk).NB,pmTk(ntk).NA
     Dim As Integer  grupo, mayorgrupo
print #1,"cargartrack maxpos, ntk  :", pmTk(ntk).MaxPos, ntk
     For j = 1 To pmTk(ntk).MaxPos  '11-07-2021 02-03-2022 ÇÇÇ
     '  print #1,"cargartrack POSICION :",J
    '  print #1,"lim3 :", lim3
      For i= 1 To lim3
      'print #1,"comienaa FOR i ",i
    '  print #1,"carga i acorde ",i
      Track(ntk).trk(j,i)  => Trabajo(j,i)  ' <-- aca va copiando tambien acorde
'If Track(ntk).trk(j,i).dur > 0 And Track(ntk).trk(j,i).nota > 0 Then
'   Print #1, Track(ntk).trk(j,i).dur;" ";Track(ntk).trk(j,i).nota;" "; 
'EndIf
    '  print #1,"luego de carga i acorde ",i  
' cargar en visualizacion con otra sub si es necesario
      Next i
'Print #1, "-------------------------------"
     Next j
  ''   cLOSE ct
Close 14
Print #1,"CLOSE ct CERRO TRK NRO ct ",ntk, ct

''' Track(ntk).trk(1,1).ejec=pmTk(ntk).ejec

     DUR => 0
     curpos =>1

    

     If pmTk(ntk).ejec = 0 And Track(ntk).trk(1,1).ejec=1 Then 
        pmTk(ntk).ejec=1
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
    guiaEscala(1).alteracion =CInt(grabaLim.pan)
    guiaEscala(1).posicion=1
' cuando cargo un track desde disco sin cancion, aislado, es similar a Roll
' ahora durante la carga de cancion y el uso de tAB esto debe ir cambiadno para
' cada track cambiado en el grafico 
'----------
''no debe haber RecalCompas(ntk) pues en una cancion hacefalta saber 1er oel maxpoTope
'' en carga de un rtk solo si colocarlo cada vez que se carga un solo track
Print #1," patch ntk canal ",	Track(ntk).trk(1,1).nnn, ntk,pmTk(ntk).canalsalida
      print #1,"CargarTrack  fin"  
      print #1,"----------------------------------------------" 
'If ntk=0 Then 
'  TRACKCARGADO =TRUE ' SIN CANCION SE CARGO EL TRACK(0)
'Print #1,"TRACKCARGADO = TRUE"
'Else 
'  TRACKCARGADO =FALSE
'EndIf 
ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0:ubirtk=0
'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0
'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0
 Sleep 100 ' retardo para que se ubiquen lso datos en memoria ¿? parece necesario
Print #1,"CARGATRACK ntk,VEO EJEC pmTk(ntk).ejec ";ntk, pmTk(ntk).ejec
Print #1,"CARGATRACK ntk,VEO EJEC Track(ntk).trk(1,1).ejec ";ntk, Track(ntk).trk(1,1).ejec

Exit Sub

saledeaca:
 Dim errmsg As String
If  Err > 0 Then
  Close (14)
  Sleep 1000,1  

  errmsg = "FAIL Error CargarTrack " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
  
  
End If

End Sub



'
Sub RollTempaTrackTemp (TrkTemp() As poli,RollTemp() As datsec)
'--------CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA
Dim As Integer i4,i3=0,i2=0,i1=0,verticalEnOctavaVacia, octavaDeAcorde,copiado=0,vertical
' ///////====>>>14-10-2025 agregamos onoff1 no se como andaba o si andará mejor!! de paso arreglamos
' //////que el off1 no tiene nota es ahora 183 yaca se corrige ahora y le mandamos la notapiano
' copia en TrkTemp RollTemp donde está Roll modificado
' se copia toda la secuencia y sus controles, la parte inicial de bloques de los archivos
' solo se envia a dichos bloques en los comandos de grabacion a archivo
If ntkcarga=0 Then ntkcarga=ntk
vertical=12+(hasta-2)*13+hasta ' "[NROREP]" de EntrarTeclado 
For i2 = 1 To MaxPos ' maxpos ojo es posnoff+6 
   'print #1,"i2",i2
   i3=0
   For i1=NB To NA -13
    ' print #1,"i1",i1
      If  RollTemp(i2,i1 ).nota >= 1 and RollTemp(i2,i1 ).nota <=12 Or RollTemp(i2,i1 ).onoff > 0 Then
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         'print #1,"copia Tabajo a Temp en GrabaRollaTrack"
         i3=i3+1
         TrkTemp(i2,i3).dur  =CInt(RollTemp(i2,i1 ).dur)
         ''''TrkTemp(i2,i3).nota =RollTemp(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =RollTemp(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =RollTemp(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =RollTemp(i2,i1 ).pb
         TrkTemp(i2,i3).nnn =RollTemp(i2,i1 ).inst
         TrkTemp(i2,i3).onoff =RollTemp(i2,i1 ).onoff

         
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
' pero con acordes debo poner para cada octava la info de Roll en cada linea segun
' su octava y agregar especificamente la octava ya que esa info se pierde 
' en cada octava no puedo poner el 201,,el 201 ira una sola vez arriba 
' cada linea correspondiente a una octava tendra la info de lso acordes para esa
' octava eso sigue igual solo que esta info se debe leer antes de todo el resto
' de forma que se sepa que cifrados de acorde van en cada octava...      
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

      If Roll.trk(i2,vertical ).nota = 210 Or Roll.trk(i2,vertical ).nota = 211 Then ' repeticiones [:  n:]
      ' vacio= 12 +(estoyEnOctava-1)*13 son las que no se ven 
      '       i1= 12 + (octavaDeAcorde -1)*13 ergo
         'i4=12+8+1=21  

         TrkTemp(i2,21).nota   = Roll.trk(i2,vertical ).nota '
         TrkTemp(i2,21).vol    = Roll.trk(i2,vertical ).vol '
      EndIf


' repeticion de Roll a Track ....
      If i2=MaxPos  Then ' fin sec
         i3=1
         TrkTemp(i2,i3).dur  =182 
         TrkTemp(i2,i3).nota =RollTemp(i2,i1 ).nota
         TrkTemp(i2,i3).vol  =RollTemp(i2,i1 ).vol
         TrkTemp(i2,i3).pan  =RollTemp(i2,i1 ).pan
         TrkTemp(i2,i3).pb   =RollTemp(i2,i1 ).pb
         TrkTemp(i2,i3).nnn =RollTemp(i2,i1 ).inst     
      EndIf 
      
   Next i1
   
   If i3 >=2 Then
    'print #1,"copia acorde ",i3," en ",i2
    TrkTemp(i2,i3).acorde=CUByte(i3)   ' Grabamos la cantidad de elem del acorde
   EndIf 
Next i2   
'TrkTemp(1,1).nnn=RollTemp(1,NA).inst  ' instrumento o PATCH
'TrkTemp(1,1).ejec=RollTemp(1,NA).onoff  ' marca ejec 

print #1,"termino copia a Trktemp, maxpos, posn ", maxpos, posn
'--FIN--CODIGO SIMILAR A ROLLATRACK SI SE CAMBIA ACA SE DEBE CAMBIAR HALLA Y VICEVERSA
posicion =1
  
  
End Sub
'
Sub ActualizarRollyGrabarPistaTrk (funcion As String, nroejec As Integer)
' nombre es global ,,,por ahora...con un Roll único nombre puede ser global
' ya que es único y siempre es parte de Roll grafico...
'  OJO GRABAR TIEMPOPATRON ACA EN TRACK A DISCO 30-03-2025 ok
' Y EN DOS LADOS MAS EN LA COPIA EN MEMORIA NO SE PUEDE NO ESTAN LOS ENCABEZADOS
' EN GRABARTRACK QUE USA CANCION TAMBIEN ok
On Local Error GoTo actu
Print #1,"//////////////////////////////////////////////////////////////////////////"
Print #1, "Entra A ActualizarRollyGrabarPistaTrkTrack ntk q llega ";ntk
Print #1,"//////////////////////////////////////////////////////////////////////////"

If funcion ="copiapista" And ntkcarga > 0 Then 'aca esta el problema se sobreescribe ntk!!! 
   ntk=Tope
Print #1, " ntk definitivo copiapista ";ntk
Print #1 , "nombre ", titulosTk(ntk)
EndIf
If funcion ="grabartrkcancion" Then
  Print #1, " ntk definitivo grabar trk cancion ";ntk
  Print #1 , "nombre ", titulosTk(ntk)
EndIf
   Maxpos=pmTk(ntk).MaxPos
   NB => 0 + (desde-1) * 13   ' 06-03
   NA => 11 + (hasta-1) * 13  ' 
   pmTk(ntk).NA= NA   

   print #1,"Maxpos , NB, NA, DEsde , hasta ",MaxPos, NB, NA, desde, hasta  
   ' DEFINO UN ROLL DONDE MANDO EL ROLL CARGADO LE SACO LOS DELETE SI HUBO

    
   Print #1,"ActualizarRollyGrabarPistaTrk CantTicks REdim ";CantTicks
   ReDim RollTemp (1 To CantTicks, NB To NA) As datsec 
' copia en RollTemp el Roll ....que tiene las modificaciones ultimas
   Dim As Integer i1, i2, i3, semitono, borrocol=0, haynota=0,res=0,k=0,final=0
  'eliminar columnas marcadas al grabar disco, 0 + X
  ' VERIFICAR QU EEL MAXPOS SEA DEL ROLL CREO QUE SI LOGICO
   print #1, "inicio MaxPos "; MaxPos
Print #1, " NTK, UBOUND ", ntk,  UBound(Roll.trk, 1)
If Maxpos > UBound(Roll.trk, 1)  Then
   pmTk(ntk).MaxPos=UBound(Roll.trk, 1)
   MaxPos=pmTk(ntk).MaxPos 
EndIf
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
          RollTemp(k, i1) =Roll.trk(i2,i1 ) ' <=== COPIA ROLL A ROLLTEMP
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
MaxPos=posn +2
pmTk(ntk).MaxPos=MaxPos
pmTk(ntk).eco=Globaleco
pmTk(ntk).pan=Globalpan
pmTk(ntk).coro=Globalcoro
pmTk(ntk).vol =Globalvol
''.ejec?
pmTk(ntk).posn=posn
Print #1,"MaxPos, posn ",MaxPos,posn
posicion =1
print #1,"MaxPos despues de acchicar",MaxPos
' tengo a Roll listo para convertir a track sin marcas de borrado de Col
' convertir
'//////////lo que sigue es GrabarTrack repetido ver de no repetir!!! kuku
print #1,"Termina con achicar secuencia"
 Dim grabaPos   As poli
 Dim grabaLim   As poli
 Dim graba3     As poli ' 04-02-2022 se agregan 48 bytes para info futura 
 Dim graba4     As poli 
 Dim graba5     As poli 
 Dim graba6     As poli 
 Dim graba7     As poli 
 Dim graba8     As poli 
 Dim graba9     As poli 
 Dim graba10    As poli 

 
' datos para track....en temp  
 Dim TrkTemp (1 To MaxPos,1 To lim3) As poli 'definimos un Track temporario
' no usamos RollaTrack porque son distintas estructuras RollaTrack usa sec
' vector de tracks a solo usamos un track as poli, de modo que dejaremos 
' un codigo casi duplicado condensado seri amas engorroso creo
' esta llamada copia el patch o instrumento que se usa en Changeprogram al TrackTemp 
RollTempaTrackTemp (TrkTemp() ,RollTemp())


'''ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3) 30-01-2022 porque cantTicks?
'  ojo espero no haga cancelaciones CantTicks siemrp es mayor a MAxPos o en 
' nucleo se agranda ? fijarse,,,,si en nucleo se redimensiona Track y Roll si
' hace falta sin borrar datos..
'[[[ SE CREA REALMENTE EL TRACK ]]]] , al hacer redim borramos en memoria los datos
' que tenia si era un track de cancion pero ojo debo conservar el ntk que pase a roll
' a track (0) (nt=0) y este ntk debe ser el del track original vermeos como viene
' si estoy navegando con TAB el ntk debe ser el correcto... bororo sus datos:
ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)

'If ntk > 0 Then ' falta copiar a ntk=0 tambien ,17-04-2024 ,borro y redimensiono Track(0)
'  ReDim (Track(0).trk ) (1 To CantTicks,1 To lim3)
'EndIf

' copiamaos a ntk que sera 0 al lado de roll, o ntk o ntk +1 segun sea el caso
' en el path de cancion.. hay que copiarlo a ambos lados en TRack(0) y Track(ntk)
' en memoria y a disco sobreescribiendo en caso de cancion,,,3 copias!

' ----> de TrkTemp a TRack 
For i1=1 To MAxPos ' de Roll que deberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim3
     Track(ntk).trk(i1,i2)=TrkTemp(i1,i2) ' 
    Next i2
Next i1


'-------------------------
'ÇÇÇ
' 
nombre=titulosTk(ntk) 
'corregir el nombre esto andaba nose que pasa...seperdio todo???
   Dim As String path, nom,ext
   Dim As Integer barra=0,punto=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de roll a track, si tengo una cancion en edcion
' y quiero un roll nuevo no conviene hacerlo aca si hay cancion no se usa
   Print #1,"nombre que llega a ActualizarRollyGrabarPistaTrk "; nombre
  Dim As Integer ubi1=0,ubi2=0,ubi3=0,ubi4=0, ubi5=0 
       Dim As String no1,no2
       ubi1=InStr(nombre,"[")
       ubi2=InStr(nombre,"]")
       ubi3=InStr(nombre,".rtk")
       ubi4=InStr(nombre,".roll")
       ubi5=InStr(nombre,".solo")

    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll .ejec
    
    print #1,"extension ",ext
    print #1,"nom nombre sin extension ni path ",nom
  If ubi1 >0 And ubi2 > 0 Or ubi3 > 0 Or ubi5 > 0 Then
     If ubi3> 0 Then
     nombre=path +nom +".rtk"
     EndIf
     If ubi5> 0 Then
     nombre=path +nom +".solo"
     EndIf
  
  Else
    If nroejec= 0 Then
     nombre=path +"[00]"+nom +".rtk"
    Else
     If DirEjecSinBarra > "" Then
          path=DirEjecSinBarra+"\"
     EndIf 
     nombre=path +"["+doscifras(nroejec)+"]"+nom +".rtk" 'path + 0 + rtk por default si no hay cancion
    EndIf
  EndIf



Print #1,"===>>>>> nombre del Track que vamos a grabar 0 o sobrescribir el de cancion "; nombre

   grt = 11

If Open (nombre  For Binary Access write As #grt ) <> 0 Then
     print #1,"Error lectura en ActualizarRollyGrabarPistaTrk, nombre "; nombre
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
     grabaPos.nota = y1
     grabaPos.dur  = y2
     grabaPos.vol  = y3
     grabaPos.pan  = y4
     grabaPos.pb   = y5
     If tipoescala_num_ini = 0 Then
        tipoescala_num_ini =1
     EndIf
     grabaPos.nnn = CUByte(tipoescala_num_ini) ' 15-01-2022 - tipoescala en uso
   If NombreCancion > "" And ntk > 0 Then
     If CheckBox_GetCheck( cbxnum(ntk))= 1 Then ' sonido on/off 16-03-2022
         graba3.sonido=1
     Else
         graba3.sonido=0
     EndIf
   EndIf
     '------------ los parametros de Roll o Track(0) estan en las globals tambien
     ' pues se llenan al cargar un Roll....
    
     grabaLim.nota = CUByte(desde)
     grabaLim.dur  = CUByte(hasta)
     grabaLim.pb   = CUByte(notaold)

     If notaescala_num_ini =0 Then
        notaescala_num_ini =1
     EndIf
     grabaLim.vol  = CUByte(notaescala_num_ini) ' notadeescala 15-01-2022 
     Select Case alteracion
       Case "sos" 
         grabaLim.pan  = CUByte(3)
       Case "bem"
         grabaLim.pan  = CUByte(2)
       Case Else 
         grabaLim.pan  = CUByte(3) 
     End Select
' grabaLim.nnn no le da el tamaño solo llega a 256
 ''''    grabaLim.nnn = CUByte(tiempoPatron)
' cargado un Roll desde archivo canalx toma el valor del canal midi de salida del archivo

     graba3.nnn=pmTk(ntk).canalsalida ' es un track as poli
     graba3.dur=pmTk(ntk).portout 
     graba3.eco =pmTk(ntk).eco
     graba3.pan =pmTk(ntk).pan
     graba3.canal =pmTk(ntk).coro
     graba3.ejec=pmTk(ntk).ejec
     graba3.patch=pmTk(ntk).patch
     graba3.nanchofig=CUByte(nanchofig*10)
     graba3.canal=pmTk(ntk).coro
     graba3.vol = pmTk(ntk).vol
Print #1,"graba3.patch ActualizarRollyGrabarPistaTrk ",graba3.patch
' en graba4 ponemos tiempoPatron en los mismos campos que en Roll asi
' aunque nada que ver es compatible poli tiene mas campos sisusar pero bueno
' seguir algoorganizado dentro de este despelote esta bien jajaj
Dim mit As aUshort
mit.ST = tiempoPatron
graba4.pan= mit.tp1
graba4.pb = mit.tp2
 FileFlush (grt)
     '-----------------------------
     print #1,"etapa final puts"
     Put #grt, ,grabaPos
     Put #grt, ,grabaLim
     Put #grt, ,graba3  
     Put #grt, ,graba4  
     Put #grt, ,graba5  
     Put #grt, ,graba6  
     Put #grt, ,graba7  
     Put #grt, ,graba8  
     Put #grt, ,graba9  
     Put #grt, ,graba10 
     
     Put #grt, ,TrkTemp()
     cLOSE grt
FileFlush (grt)
     While InKey <> "": Wend
     Sleep 150
Print #1,"fin ActualizarRollyGrabarPistaTrk"
Print #1, "VER SI GRABO A DISCO "
Print #1, "/////////////////////////////////////////////////"

Exit Sub

actu:
 Dim errmsg As String
 Dim As Long er1 = Err()
If  er1 > 0 Then
print #1,"-----------------err ActualizarRollyGrabarPistaTrk-----------------"
print #1,
print #1,"ActualizarRollyGrabarPistaTrk FAIL Error "  
print #1, Str(er1) + " in function " & *Erfn  
print #1," on line " & Erl & " " & ProgError(er1)
  Print #1, errmsg
  FileFlush (-1)
print #1," MaxPos ", Maxpos
print #1," i2 i1 ", i2,i1 
End If


End Sub

' ---------------------------
Sub ImportarPistaExterna(nombre As String)
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia ImportarPistaExterna " 

Dim As String path, nom,ext
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion
Dim midsal As  RtMidiOutPtr
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva  
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path del archivo a importar
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll

    Dim haycorchete As Integer
    haycorchete = InStr(nom,"]")
    If haycorchete> 0 Then
       nom = Mid(nom, haycorchete+1)  ' sacamos el [x] si existe
    EndIf

    
    print #1,"extension de la pista importada",ext
    print #1,"nom nombre sin extension ni path ni [] ",nom
    print #1,"numero de pista tope =",tope

   tope = tope + 1

   If tope <= 32 Then
      ntk=tope
   Else    
      Exit Sub   
   EndIf
   print #1,"pista nueva importada será ntk=",ntk   
'si importamos de un directorio de ejecuciones donde se grabó por omision
' los roll o trk entonces existira el archivo inicio.txt
cargariniciotxt(NombreCancion, CANCION)
SetGadgetText(TEXT_TOPE, Str(maxpos))
RecalCompas()

  '
Select Case ext
 Case ".rtk" ' anda bien seguir probando
' hay que probar ahora modificaciones de los tracks en la cancion
' si modifico alguno a veces cancela  en el play aunque 
' la modificacion como notas nuevas lo ahce pero en play cancela
' se cierrra el progrma lo levant ode nuevo cargo lacancion
' y las modificaciones quedaron,,
' sacar corchetes si lso tiene del nombre y armar de nuevo el nombre
' toma el ntk globalmente,,,
  titulosTk(ntk)=nombre
  CargarTrack(Track(),ntk,1) ' lo carga en ntk nuevo
' no haria falta cargarlo a roll solo copiar el archivo rtk a cancion
' aumentar el tope y cargar la lista, pero bueno lo dejamos asi,,, 
' y track a roll tampoco haria falta eso loa hace al dar TAB...

  TrackaRoll (Track(),ntk,Roll,"ImportarPistaExterna" ) 'carga a roll el track cargado anteriormente
  ' cambiamos el nombre segun la ext
     
    nombre= NombreCancion + "\[" + doscifras(ntk) + "]" + nom +".rtk"
    Dim cadena As String = "[" + doscifras(ntk) + "]" + nom
    AddListBoxItem(PISTASROLL, cadena)
    Sleep 1                         
    print #1,"GRABANDO PISTA EN CANCION EN ",nombre
      'si es vacio tomo la ultima
    titulosTk(ntk)=nombre ' cambiamos el nombre de la pista 
' grabar en el directorio de cancion
    GrabarRollaTrack(0,0) ' graba el roll del rtk a directorio cancion 
      
 Case ".roll" ' corregido 20-04-2024
    titulosTk(0)=nombre  ''nombre de un roll externo fuera de cancion
    CargaArchivo(Roll,1) ' sobreescribe ntk global con 0, carga a Roll y a track(0)
    
   ' todos los valores quedaron en ntk=0 
s5=0 '11-06-2022
   ''ntk=Tope
   nombre= NombreCancion + "\[" + doscifras(Tope) + "]" + nom +".rtk"
   Dim cadena As String = "[" + doscifras(Tope) + "]" + nom
    AddListBoxItem(PISTASROLL, cadena)
    Sleep 1                          
   print #1,"GRABANDO PISTA EN CANCION EN ",nombre
    
'esta en Roll y track (0) debo grabarlo a rtk nuevo ntk en cancion
    titulosTk(Tope)=nombre ' cambiamos el nombre de la pista en el ntk nuevo 
' grabar en el directorio de cancion
    'de nuevo el tope, debo estar posicionado en roll y pista 0
  
    'necesito copiar el Track(0) al Track(Tope) en memoria esto lo hago ya
  '''copiar Track 0  A Track Tope  no  convien hace sub creo osi...
Dim As Integer i1,i2

ReDim  (Track(Tope).trk ) (1 To CantTicks, 1 To lim3)

For i1=1 To pmTk(0).MaxPos 
  For i2 = 1 To lim3
  Track(Tope).trk(i1,i2) = Track(0).trk(i1,i2)
  Next i2
Next i1

copiarPmtkaPmtk(Tope, 0 )
' observar que ntk vale 0 no se toco   

Sleep 5  
' copio de track(0) a track(ntk)
'fallta grabar a disco el track que se obtuvo a partir de un roll en disco
' grabar track en memoria a Track en disco ,o 
' grabar Roll en memoria a Track en disco esto es TrackaRoll igual que antes
' llamo y ntk esta en Tope

GrabarRollaTrack(0,0)

cargariniciotxt(NombreCancion,CANCION)
SetGadgetText(TEXT_TOPE, Str(maxpos))
RecalCompas()       

    ' y a disco con su nuevo [xx]  
           
End Select 
    


End Sub

Sub copiaTrackaTrack(Track() As sec ,ntkrecibe As integer,ntkda As Integer)
Dim As Integer i1,i2
For i1=1 To pmTk(ntkda).MaxPos ' de Roll que deberia ser el PmTk(ntk).maxpos....verificar
    For i2= 1 To lim3
     Track(ntkrecibe).trk(i1,i2)=Track(ntkda).trk(i1,i2) 
    Next i2
Next i1
End Sub
'--------------------------------------------------
sub GrabarCopiadePista()
' variables globales nombre, ntk
' recorro la list atomo el ntk proximo o simplemente sumo 1 a tope e incremento tope
' siempre que sea menor igual a 32, agrego a la lista decontrol y grabo a disco 
' en cancoin el nuevo track copia, renombrar pitas faltaria esa funcionalidad
cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
Print #1,"//////////////////////////////////////////////////////////////////////////"
print #1,"            inicia GrabarCopiaPista "
Print #1,"//////////////////////////////////////////////////////////////////////////"

FILEFLUSH(-1) 
Dim As String path, nom,ext, pistanueva
Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntkTAB ' el ntk que esta en edicion
' al seleccionar en lista el ntk no se selecciona automaticamente sacando ntk del nombre
 '  quiero una pista nueva
EntrarNombrePista (pistanueva, hwndC)
''aca esta viendo una pista que esta en este momento cargada en roll en edicion 
    print #1,"pista origen ntkTAB=",ntkTAB
'-> copiamos una pista de cancion en otra pista nueva de cancion
' para invocar esto se necesit ael menu de Control 
   tope=tope+1
   If tope <= 32 Then
      ntkcarga=tope ' LA NUEVA
   Else    
      Exit Sub   
   EndIf
   print #1,"pista nueva ntk=",tope  
   ntkcarga=tope 
' a) tomar el roll de la pista origen sacarle lso deletes, en un roll temporario
' b) copiar el roll temporario al track nuevo
' a) en este caso como es una copia todos los parametros del Roll origen son
' iguales (dsde hasta maxpos notaold posn etc)
' desde aca es igual
CANCIONCARGADA =TRUE
     If CANCIONCARGADA =TRUE  Then ' copia track en otro track
     print #1,"copia PmtkaPmtk NTKTAB a NTKCARGA ", ntkTAB,ntkcarga
     copiarPmtkaPmtk(ntkcarga, ntkold) 'ya se cargo maxpos!!!
   print #1,"hizo copia de parametros de trackold en new"

Print #1,"//redim pmTk(ntkTAB).MaxPos origen NTKOLD ", pmTk(ntkTAB).MaxPos
FILEFLUSH(-1)
        ReDim (Track(ntkcarga).trk) (1 To pmTk(ntkcarga).MaxPos, 1 To lim3)

  print #1,"FIN reDIM.. *po seria hasta -1 ", pmTk(ntkcarga).hasta -1
FILEFLUSH(-1)

     EndIf
Print #1,"//////////////////////////////////////////////////////////////////////////"
print #1,"     pistanueva " ,pistanueva, "NTKCARGA ",ntkcarga     
Print #1,"//////////////////////////////////////////////////////////////////////////"
FILEFLUSH(-1)


     If pistanueva<> "" Then ' graba roll en cancion
        print #1,"preparo nombre pista nueva ntkCARGA ", ntkcarga
FILEFLUSH(-1)
     
        nombre= NombreCancion + "\[" + doscifras(ntkcarga) + "]" + pistanueva +".rtk"
        Dim cadena As String = "[" + doscifras(ntkcarga) + "]" + pistanueva
        AddListBoxItem(PISTASROLL, cadena)
        Sleep 1                          
        print #1,"==>> GRABANDO PISTA EN CANCION EN ntkCARGA nombre ",ntkcarga, nombre
      'si es vacio tomo la ultima
        titulosTk(ntkcarga)=nombre
     EndIf
   ' primero debo mover el  track nuevo a roll
   cargaCancion=NO_CARGAR_PUEDE_DIBUJAR 
''   cargaCancion=CARGAR_NO_PUEDE_DIBUJAR ' CON ESTA ENTRA A TAB AUTOMATICO NO QUEREMOS ESO 03-11-2025
 'falta copiar de track origen a track nuevo!!
Print #1,"//////////////////////////////////////////////////////////////////////////"
Print #1,"/////copiar track ntkTAB a track ntkcarga o nueva ", ntkTAB,ntkcarga 
 copiaTrackaTrack(Track(),ntkcarga ,ntkTAB)
Print #1,"///// ojo las variables globales apuntan a ntkold ///" 
FILEFLUSH(-1)


   '''TrackaRoll (Track(),ntkcarga,Roll)
Print #1,"//////////////////////////////////////////////////////////////////////////"
Print #1,"/////copiar track ntkcarga o nueva A Roll", ntkcarga 
clickpista=0
FILEFLUSH(-1)

  Tracks (ntkcarga , 1,Roll) ' track , nro,  Canal, copia track a Roll en memoria
' luego si pasamos este roll nuevo a pista
      ntk=ntkcarga
Print #1,"//////////////////////////////////////////////////////////////////////////"
Print #1,"///// actualizarRollyGrabarPista ntk ntkcarga debe nser iguales aca ", ntk, ntkcarga 
Print #1, "///las variables de ntkTAB siguen en las globales todavia!"
FILEFLUSH(-1)
' funcion copiapista
   ActualizarRollyGrabarPistaTrk ("copiapista", 0)
   cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
terminar=NO_TERMINAR_BARRE_PANTALLA 
Parar_De_Dibujar=NO
   ntkcarga=0

print #1,"FIN GrabarCopiaPista nueva ",ntk
print #1,"FIN GrabarCopiaPista ,maxpos,posn ",maxpos,posn

print #1,"---------------------------------------------------------------------------------"


End Sub
' ----------------------------------
Sub GrabarRollaTrack ( cambiaext As Integer, nroejec As integer  ) ' SE USA PARA TODO 
' las 3 1eras funciones pueden hacerce en roll sin problemas..roll lo deduce
' de la entrada y las variables globales. la 4ta no, debe llamarse desde Control
'1) Convierte y Graba un Roll cargado de disco, como Track [00] fuera de cancion. conversion
' exclusivo de Roll no de Control (1,0,FALSE), es conversion no copia 
' cambiaext=1 porque pasa de .roll a .rtk  
'2) convierte un Roll a trk y graba en cancion, igual que antes pero 
' lo pone en cancion con el mismo nro de track, sobreescribe el archivo---
' para el caso de sin cancion: ACA NO ELIMINAMOS LA POSIBILIDAD DE UNDO YA QUE NO GRABAMOS ROLL
' ESTAMSO GRABANDO A TRACK EXPORTANDO PERO EL ROLL NO SE GRABO SI HICIMOS MODIFICACIONES EN ROLL,
' Y no se sobreescribio EL roll DE DISCO ANTES DE CONVERTIR ROLL A TRACK 
' Y PODRIA AHCER UNDO cargadno de nuevo el roll de disco.,y
' EXPORTARLO DE NUEVO O NO EXPORTARLO Y HACER UNDO DE ROLL PERO TRACK QUEDA COMO BACKUP
' DE LO NUEVO , SOLO AL GRABAR ROLL SE ELIMINA EL UNDO..
' CUANDO CARGO UN ROLL SE CARGA SUS VALORES EN PMTK(0) Y EN LAS GLOBALES (VERIFICANDO...)
' -->PORTSAL: al cambiar en BTN_ROLL_PORTSAL USA ESTA RUTINA(0) PARA TRK DE CANCION
print #1,"---------------------------------------------------------------------------------"
print #1,"inicia GrabarRollaTrack, cambiaext ",cambiaext 
   Dim As String path, nom,ext
   Dim As Integer barra=0,punto=0,ubi3=0,ubi4=0,ntkold=ntk ' el ntk que esta en edicion 
'' cambia de nombre de roll a track, si tengo una cancion en edcion
' y quiero un roll nuevo no conviene hacerlo aca si hay cancion no se usa
   Print #1,"nombre que llega a GrabarRollaTrack "; nombre
    barra=InStrRev(nombre,"\")
    punto=InStrRev(nombre,".")
    path= Mid(nombre,1,barra) ' path
    nom= Mid(nombre,barra+1,punto - 1 -barra) ' nombre archivo sin extension
    ext= LCase(Mid(nombre,punto)) ' contiene el punto .rtk .roll .ejec
    
    print #1,"extension ",ext
    print #1,"nom nombre sin extension ni path ",nom
 '1) Convierte y Graba un Roll cargado de disco, como Track[00] fuera de cancion. conversion
 ' por eso cambia extension a rtk
If  nombre > "" Then
   ' graba roll a rtk en cancion 0 o 1 
'el ejec viene de cargar un ejec en roll pero queda con su nombre terminado en ejec no se lo cambia
   If cambiaext > 0   And (ext =".roll" Or ext=".ejec") Then ' convertir
      If NombreCancion > "" Then
      Else
        ntk=0 ' con .ejec  se guimos con ntk 0 asi funcionaba bien
      EndIf 

      
      If ext=".ejec" Then
         Dim As Integer abrecor, cierracor
         cierracor=InStr(nom,")")
         Mid(nom,1,1)="["
         Mid(nom,cierracor,1)="]" 
         path=DirEjecSinBarra+"\"
         nombre=path + nom +".rtk" 'path + 0 + rtk por default si no hay cancion
      ' cancion,,
      Else
         If ext=".rtk" Then
            nombre=path +nom +".rtk" 'path + 0 + rtk por default si no hay cancion
         Else 
            nombre=path +"[00]"+nom +".rtk" 'path + 0 + rtk por default si no hay cancion
         EndIf
      EndIf
      print #1,"armado de nombre roll a trk[00]",nombre
   
        ' guardo los valores de Roll cargado en el track nuevo [00]
   ' hay una cancion cargada pero cargue (cargue o estoy por cargar? )un roll de disco en Roll Grafico
   ' estando posicionado en la pista ntk.. 
      If CANCIONCARGADA=TRUE  And ROLLCARGADO=TRUE Or ROLLCARGADO=TRUE Then
         'borro la pista ntk=0 en memoria y la redimensiono 
         ReDim (Track(ntk).trk ) (1 To MaxPos,1 To lim3) ' 23-03-2022 ÇÇÇ
         ' tomo los valores del roll cargado de disco verificar en cargarArchivo
         ' pero esto ya esta cargado en ntk=0 no hace falta solo la maxpos
         'pmTk(ntk).desde=desde
         'pmTk(ntk).hasta=hasta
         'pmTk(ntk).NB=NB
         'pmTk(ntk).NA=NA
         If NB=0 Or NA=0 Then
           pmTk(ntk).NB => 0 + (pmTk(ntk).desde-1) * 13   
           pmTk(ntk).NA => 11 + (pmTk(ntk).hasta-1) * 13
         EndIf 
         print #1,"NB y NA de RollaTrack cargando un Roll en cancion ", NB, NA 
         pmTk(ntk).MaxPos=MaxPos
If posn=0 Then
   posn=pmTk(ntk).MaxPos -2
EndIf
         pmTk(ntk).posn=posn
         pmTk(ntk).notaold=notaold
         pmTk(ntk).Ticks=CantTicks
         'pmTk(ntk).canalsalida=CUByte(canalx) ' SOLO VALIDO PARA UNA PISTA AISLADA
         'pmTk(ntk).portout=portout
         'pmTk(ntk).eco=Globaleco
         'pmTk(ntk).pan=Globalpan
         'pmTk(ntk).coro=Globalcoro ''o podriamos tomar la de roll pmTk(0)
        If Roll.trk(1,NA).inst > 0 Then  'prioridad de conversion
            pmTk(ntk).patch= Roll.trk(1,NA).inst 'es la vieja forma
            Roll.trk(1,NA).inst=0  'es la vieja forma
            patchsal=pmTk(ntk).patch
            instru=CInt(patchsal)
        Else
           If pmTk(ntk).patch > 0 Then
              patchsal=pmTk(ntk).patch
             instru=CInt(patchsal)
           EndIf 
        EndIf
' ejec solo puede ser por ahora 0 o 1, 1 viene de una ejecucion por teclado
' tendra N como figuras de notas
   ' If Roll.trk(1,NA).onoff =1 Then  
   '     pmTk(ntk).ejec = 1  ' lo viejo
   '     Roll.trk(1,NA).onoff=0 'ver el resto del codigo debe usar dat.ejec ahora
   ' EndIf
   Dim xp As aUshort
   Dim As Integer tiempoPatronNA ' por compatibilidad hacia atas
      xp.tp1=Roll.trk(1,NA).pan   
      xp.tp2=Roll.trk(1,NA).pb   
      tiempoPatronNA=xp.ST
   If  tiempoPatron > 0 Then
      pmTk(ntk).tiempopatron = tiempoPatron
   EndIf
   If tiempoPatron =0 And tiempoPatronNA > 0 Then
      tiempoPatron=tiempoPatronNA
      pmTk(ntk).tiempopatron = tiempoPatron
      Roll.trk(1,NA).pan=0
      Roll.trk(1,NA).pb=0
   EndIf 
     
        'los datos del rtk de disco estan ahora en memoria en Roll Visual
        ' y en el Track 0 , luego 
   EndIf
   End If
   
' NOTA: para cambiar un canal de salida midi se debera editar la pista en el editor de Roll
' cambiar al canal MIDI deseado y Grabar el archivo como roll y como rtk asi queda el valor
' en ambos archivos o si se cargo como track se grabara como track..
' si estoy en cancion al usar TAB para pasar de pista el valor del canal midi quedara ademas
' en al estructura pmTk en memoeria,,,
   
 '2) Actualizacion de un trk de cancion, se modifica desde roll igual que antes 
 'pero sobreescribe el track del roll correspondiente en cancion
 ' lso parametros no se tocan es la misma pista 
   If cambiaext=0 And nombre >"" And (ext =".roll" Or ext =".ejec") Then 
     print #1,"update ntk, Nombre Cancion", NombreCancion ' es el path del directorio de cancion
     print #1,"update de NumPista ntk ", ntk
     print #1,"update,nombre del track con  path no se toca ",nombre
   EndIf
Else
' no crea solo convierte roll a rtk y graba, o solo graba de rtk a rtk
' el archivo debe existir y estar cargado por eso nombre debe existir sino sale sin hacer nada
   Exit sub
EndIf

   If ext = ".rtk" Or ext= ".solo" Then 
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
' funcion grabartrkcancion
  ActualizarRollyGrabarPistaTrk ("grabartrkcancion", nroejec) 

print #1,"FIN GrabarRollaTrack ,cambiaext ",cambiaext
print #1,"FIN GrabarRollaTrack ,maxpos,posn ",maxpos,posn

print #1,"---------------------------------------------------------------------------------"

If ROLLCARGADO=TRUE Then
   ROLLCARGADO=FALSE 
EndIf   
 
End Sub
'---------
Sub RollaTrack (Track() As sec, ntk As Integer,Roll As inst)
': se usaba en AutoFracTodoDur y FraccionarDur
'-------------------
' PROCESO EN MEMORIA , desde 15-01 hemos puesto subrutina comun con GrabarRollaTrack
' con la penalidad de que se copia 2 veces los datos, si demora mucho los procesos de 
' AutoFracTodoDur y FraccionarDur
' volverlo a la version sin copia....  
' 15-01-2022 copio campos control de Roll a Track en lim3=13  
' 02-12-2021
' condicion Roll debe estar cargado en el editor entonces copiamos Roll a Track
' si es ntk=0 estamos copiando el gemelo de Roll en Track...
' MaxPos es global y lim3 tambien no s eporque -2 debo copiar todo
Dim As Integer j,i3,i2,i1
'------------------------------------------
' basada en ActualizarRollyGrabarPistaTrk pero en un solo paso, dejare una aola ? veremos
ReDim (Track(ntk).trk ) (1 To MaxPos,1 To lim3)
   Erase mel_undo, undo_acorde,undo_kant_intervalos  
   mel_undo_k=0: ig=0:cnt_acor=0
' `PODRIA USAR UN TEMP Y LUEGO COPIAR COMO SE HACE EN ACTUALIZARROLLATRACKYGRABAR
' Y ASI UNIFICARIAMOS
 ReDim RollTemp (1 To Maxpos, NB To NA) As datsec
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
pmTk(ntk).MaxPos=MaxPos
'copio a track ntk,los parametros en Roll 0
' recive, entrega
copiarPmtkaPmtk(ntk,0)

 '  Print #1,"hasta 1054 TracjaRoll ",hasta 
If pmTk(0).patch > 0 Then
   pmTk(ntk).patch=pmTk(0).patch
EndIf
' como Roll edita se deben llenar las Globales
' para tomar las ultimas modificaciones en ROLL
' cuando modifico se guarda en la Global y en plTk no haria falta!
pmTk(ntk).eco=Globaleco
pmTk(ntk).pan=Globalpan
pmTk(ntk).coro=Globalcoro
pmTk(ntk).vol=Globalvol

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

Sub copiarPmtkaPmtk(r as Integer, e As integer)

pmTk(r).desde=pmTk(e).desde
pmTk(r).hasta=pmTk(e).hasta
NB=pmTk(e).NB
NA=pmTk(e).NA
If NB=0 Or NA=0 Then
  pmTk(r).NB => 0 + (pmTk(e).desde-1) * 13   
  pmTk(r).NA => 11 + (pmTk(e).hasta-1) * 13
  NB=pmTk(r).NB
  NA=pmTk(r).NA
  pmTk(e).NB=NB
  pmTk(e).NA=NA
EndIf 


pmTk(r).MaxPos=pmTk(e).MaxPos
If pmTk(e).posn = 0 Then
   pmTk(e).posn=pmTk(e).MaxPos -2
EndIf
pmTk(r).posn=pmTk(e).posn
pmTk(r).notaold=pmTk(e).notaold
pmTk(r).Ticks=pmTk(e).Ticks
pmTk(r).patch=pmTk(e).patch
pmTk(r).notaescala=pmTk(e).notaescala
pmTk(r).tipoescala =pmTk(e).tipoescala
pmTk(r).alteracion=pmTk(e).alteracion
pmTk(r).canalsalida=pmTk(e).canalsalida
pmTk(r).canalentrada=pmTk(e).canalentrada
pmTk(r).portout=pmTk(e).portout
pmTk(r).zona1=pmTk(e).zona1
pmTk(r).zona2=pmTk(e).zona2
pmTk(r).nroRep=pmTk(e).nroRep
pmTk(r).portin=pmTk(e).portin
pmTk(r).tiempopatron=pmTk(e).tiempopatron
pmTk(r).TipoCompas=pmTk(e).TipoCompas
pmTk(r).ejec=pmTk(e).ejec
pmTk(r).vol=pmTk(e).vol
pmTk(r).pan=pmTk(e).pan
pmTk(r).eco=pmTk(e).eco
pmTk(r).coro=pmTk(e).coro
End Sub


' ---------------------
Sub TrackaRoll (Track() As sec, ByVal ntk As Integer, Roll As inst, funcion As String)
On Local Error Goto fail
clickpista=0
' EN TRACKS SE GRABA PIANONOTA TANTO EN EL ON=ONOFF2 COMO EN EL OFF=ONOFF1
' ENM ROLL MANUAL HAY QUE CONSTRUIR EL OFF1
' agregamops tiempoPatron
'[[[[[ VER MAS ABAJO JMG 15-01-2022  DEBO COPIAR EL CAMPO DE CONTROL DE LIM3 EL 13 !!!! ]]]]]
' al final de todo se los hace  
' como es track a Roll se supone que el track ya esta cargado en memoria
' y esta sub pasa de Track ntk, a Roll Visual y track 0 
' o sea copia track ntk a Roll en memoria
Dim As Integer i1,i2,i3,Maxposicion,octavaDAcorde,verticalEnOctavaVacia,vertical,ejec
 ' print #1,"TrackaRoll 1"   
''If ubirtk=3 Then ' estoy conmutando de track durante la edicion
' si no estoy en cancion el nto va a ntk 0, siemrep uso ntk y el vector de pnTk
If CANCIONCARGADA=FALSE And  NADACARGADO=TRUE Then
   ntk=0
EndIf
cargaCancion=CARGAR_NO_PUEDE_DIBUJAR
terminar=NO_TERMINAR_CON_DATOS_CARGADOS

print #1,"-------------ARRANCA TRACKAROLL---------------------------------"
print #1,"NTK Y nombre que llego a TrackaRoll ",ntk ,titulosTk(ntk)
print #1,"(ntk).maxpos ", pmTk(ntk).MaxPos
nombre=titulosTk(ntk)


'0) Si se pulsa delete o Supr estando en Roll Visual, se iran borando de memoria
' los datos de ese trrack y de la lista de Control, pero los archivos en disco
' seguiran iguales, primera etapa,luego al Grabar Cancion se borrara definitivamente
' los track no usados, lso nombres de los tracks permaneceran iguales.
'  luego se hara que si se borro y se agregan nuevos tracks estos tomaran los lugares
' de lso tracks borados y sus numeros no usados [x].

nota=0:dur=0
' ESTA COPIA DE PARAMETROS DESDE TRACK A ROLL LA REEMPLAZAREMOS POR COPIARPMTKA PMTK
' CHEQUEAMOS SI ES ASI.....
'copia a variables de Roll desde track
copiarPmtkaPmtk(0,ntk)
desde  = pmTk(ntk).desde
hasta  = pmTk(ntk).hasta
 '  Print #1,"hasta 1054 TracjaRoll ",hasta 
MaxPos = pmTk(ntk).MaxPos
posn  = pmTk(ntk).posn
notaOld= CInt(pmTk(ntk).notaold)
canalx= CInt(pmTk(ntk).canalsalida)
portout=CInt(pmTk(ntk).portout)
If pmTk(ntk).patch > 0 Then
   patchsal=pmTk(ntk).patch
   pmTk(0).patch=pmTk(ntk).patch
   instru=CInt(patchsal)
EndIf
   Print #1,"instru en TrackaRoll ",instru
If pmTk(ntk).ejec > 0 And pmTk(ntk).ejec=0 Then
   pmTk(0).ejec=pmTk(ntk).ejec   
EndIf
' como Roll edita se deben llenar las Globales
Globaleco=pmTk(ntk).eco
Globalpan=pmTk(ntk).pan
Globalcoro=pmTk(ntk).coro
Globalvol=pmTk(ntk).vol
' a partir de esta carga todo parametro ira a pmTk.. y toda la logica no
' debe usar mas los campos que no sean pmTk...
   tiempoPatron =   pmTk(ntk).tiempopatron
  

  ' print #1,"TrackaRoll ntk, desde, hasta, MaxPos ", ntk, desde,hasta,MaxPos
  ' Print #1,"TrackaRoll ntk patchsal canalx portout: "; ntk, patchsal, canalx, portout
   desdevector=desde
   hastavector=hasta
   estoyEnOctava =desde
   estoyEnOctavaOld =desde
' si se carga roll o trk que fueron convertidos desde un ejec
' maxgrb y maxcarga deben ser iguales y >0
'26-03-2025 todos los roll deben ser de igual maxpos!! 
' ese puede ser el problema antes de inlcuir una nueva pista a una cancion igualremos maxpos
' para todas las pistas??? pero la cancion actual con pistas ejec no tiene igual duracion....¿y?
   If maxgrb>0 And maxgrb=maxcarga Then '26-03-2025 todos los roll deben ser de igual maxpos!!
     If CantTicks < maxgrb Then
        CantTicks = maxgrb * 2 ' pongo mas por siquiere grabar mas datos
     EndIf
   EndIf
   'print #1,"TrackaRoll, NB, NA, CantTricks", NB,NA, CantTicks
' redim de ROLL de Visualizacion , para ello  
   ''  Parar_De_Dibujar=SI kokito
     Sleep 10 ' para que surja efecto la detencion ,,,   
  'If  ntk= 0 Then
 ' Else
   Erase Roll.trk , compas
Print #1, "// trckaroll redim Roll MaxPos ",MaxPos
   ReDim (Roll.trk ) (1 To CantTicks, NB To NA ) ' 27-02 ÇÇÇ
  ''' ReDim compas(1 To MaxPos)
 ' End If  
   'Print #1,"paso el redim de roll"
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
       print #1,"/////REDIM DE TRACK 0 MaxPos ",MaxPos 
' NO SE HACE SIN CANCION O CON ROLLCARGADO"
       ' SE PREPARA TRACK 0 PARA RECIBIR DATOS DE TRACK X,PUES PARA IR A ROLL
       ' SE LLENA TRACK 0 SIEMRPE.,,,
         
       ReDim (Track(00).trk ) (1 To CantTicks, 1 To lim3)
       Print #1,"paso el redim de track 0" 
   EndIf
   nota=0:dur=0
   inicioDeLEctura=0
   *po=pmTk(ntk).hasta -1
   
print #1,"TrackaRoll desde, hasta ", desde , hasta
print #1,"TrackaRoll NB, NA ,*po, MaxPos ", NB , NA, *po,MaxPos
Print #1,"1106 en trackaRoll numero de track ntk ",ntk
print #1, "ubound (Track(ntk).trk,2) ", ubound (Track(ntk).trk,2)
print #1, "ACA VIENE EL ERROR ? "
print #1, "lbound (Track(ntk).trk,2) ", LBound (Track(ntk).trk,2)
print #1," va a ejecutar for de copia de TrackaRoll"
Print #1," pmTk(ntk).MaxPos ",pmTk(ntk).MaxPos
Dim As Integer ultimaPianonota
' 1) carga de Roll desde track ntk
vertical=12+(hasta-2)*13+hasta ' "[NROREP]" de EntrarTeclado
'*********************************************************************************
' OJO EN .nota NO SE GUARDA LOS SEMITONOS DE ROLL , SE  GUARDA LA PIANONOTA!!!
'**********************************************************************************
' en las notas estaran los on y off, ¿que pasa con  el final 182 en la sub se trata de ponerlo mas abajo parece?
' aca solo copia las notas! y los off=2 que estan junto a nota y el onoff=1 donde lo copia o lo regenra??
For i2 = 1 To pmTk(ntk).MaxPos 
   For i1=1 To lim2 ' porque lim2 y no lim3??? porque solo las notas 
      If Track(ntk).trk (i2,i1).nota > 0 and Track(ntk).trk(i2,i1 ).nota <=NA-13   Then '14-03-2022 na-13
      ' copio a track 1 temporario. el usuairo debera renombrarlo por ahora
         PianoNota=Track(ntk).trk(i2,i1 ).nota
''lo llevo al indice del vector
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
         Roll.trk(i2,i3).vol  = Track(ntk).trk(i2,i1).vol 'acava la vel origina de ejec
         Roll.trk(i2,i3).pan  = Track(ntk).trk(i2,i1).pan
         Roll.trk(i2,i3).pb   = Track(ntk).trk(i2,i1).pb
         Roll.trk(i2,i3).inst = Track(ntk).trk(i2,i1).nnn
         Roll.trk(i2,i3).onoff = Track(ntk).trk(i2,i1).onoff
'If Roll.trk(i2,i3).onoff = 2 Or Roll.trk(i2,i3).onoff =1 Then
'  Print #1,"TrackARoll Roll.trk(i2,i3).vol, onoff ";Roll.trk(i2,i3).vol, Roll.trk(i2,i3).onoff
'EndIf
   If Track(ntk).trk (i2,i1).nota > 0 And Track(ntk).trk (i2,i1).onoff =2 Then 
      ultimaPianonota=i3 
   EndIf            
         ''print #1,"VEO CARGA DE ROLL Roll.trk(i2,i3).dur ",Roll.trk(i2,i3).dur
      EndIf
   Next i1

   
   If i2=pmTk(ntk).MaxPos  then 
    Print #1," ///maxpos-1, nota  ", i2, ultimaPianonota
      i3=ultimaPianonota
      Roll.trk(i2,i3).dur  = 182
       Roll.trk(i2,i3).nota =0 
       Roll.trk(i2,i3).vol  = 0
       Roll.trk(i2,i3).pan  = 0
       Roll.trk(i2,i3).pb   = 0
       Roll.trk(i2,i3).inst = 0
       
   EndIf
Next i2
 Print #1," ///maxpos ", pmTk(ntk).MaxPos 

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
      If Track(ntk).trk(i2,21).nota = 210 Or Track(ntk).trk(i2,21).nota = 211 Then ' repeticiones
  '       Print #1,"))))))) TRACKAROLL 210 O 211"
  '       Print #1,"))))))) TRACKAROLL VERTICAL ",vertical
  '       Print #1,"))) Track(ntk).trk(i2,i1).nota ",Track(ntk).trk(i2,21).nota
  '       Print #1,"))) Track(ntk).trk(i2,i1+1).vol ",Track(ntk).trk(i2,21).vol
         Roll.trk(i2,vertical ).nota   = Track(ntk).trk(i2,21).nota
         Roll.trk(i2,vertical ).vol = Track(ntk).trk(i2,21).vol   
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
cargaCancion=CARGAR_NO_PUEDE_DIBUJAR
terminar=NO_TERMINAR_CON_DATOS_CARGADOS
''Parar_De_Dibujar=SI kokito
Print #1,"cargaCancion DEBE SER 1 ",cargaCancion
Print #1,"NTK del copiado aca esta el lio, Y MAXPOS EN REDIM ",ntk, pmTk(ntk).MaxPos
ReDim (Track(0).trk ) (1 To  pmTk(ntk).MaxPos +1, 1 To lim3) '////HOY


If cargaCancion=CARGAR_NO_PUEDE_DIBUJAR Or CANCIONCARGADA=TRUE Then ' MIENTRAS HAYA MAS DE UN TRACK O SEA UNA CANCION
Print #1,"CARGA DE  TRACK(0)"  
copiaTrackaTrack(Track(), 0 , ntk )
' For i1=1 To pmTk(ntk).MaxPos 
'  For i2 = 1 To lim3
'  Track(0).trk(i1,i2) = Track(ntk).trk(i1,i2)
'  Next i2
' Next i1
EndIf  
Print #1,"FIN CARGA DE  TRACK(0)"

''Sleep 5  

'patch

' redefinir tiempoPatron
'Union aUshort ' es unredefine de cobol
' ST As UShort
'   Type
'    pan As UByte
'    pb As UByte
'   End Type
'End Union
'Print #1,"DIM  MIT START "
Dim mit As aUshort 'redefine de cobol
'Print #1,"DIM  MIT FIN "
mit.st = CUShort(pmTk(ntk).tiempoPatron)
'Print #1,"MIT  TIEMPOPATRON"


'con estos dos campos puedo reconstruir tiempoPatron
Track(0).trk(1,1).pan = mit.tp1
Track(0).trk(1,1).pb  = mit.tp2
'Print #1,"CARGO TIEMPOPATRON"
'FILEFLUSH(-1)
' Roll va a tocar bien a tempo si sacamos el tempo de ahi
' fralta ver como lo ponemos en track, en el encabezado supongo
'------------

''Print #1,"EJEC Roll.trk(1,NA).onoff ";Roll.trk(1,NA).onoff


''Print #1, "TrackaRoll Fin ntk, patch " ; ntk, Roll.trk(1,NA).inst
terminar=NO_TERMINAR_BARRE_PANTALLA 
Parar_De_Dibujar=NO ' volvemos a dibujar en pantalla...18-04-2024
cargacancion=NO_CARGAR_PUEDE_DIBUJAR ' " " 


'hemos copiado el track ntk en el Track(0) que corresponde al Track asociado a Roll.
'--------------------------------Track(0) fin
'''curpos=0
notacur=1
nroCompas=0
tres=0:pun=0:silen=0:mas=0:doblepun=0:cuart=0
tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
'''calcCompas(MaxPos,Roll) ' 05-03
'-------------------------de cargarroll
If funcion<> "saltoTAB" then 
 DUR => 0
 curpos =>1
 anchofig=anchofig + 0.1
   gap1= anchofig * 6 ' era porque tanto 20 '81 default
   gap1=gap1 - 1
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   
  '' NroCol =  (ANCHO / anchofig ) + 4

   lockfont=1
   nanchofig=anchofig
   ANCHO3div4 = ANCHO *3 / 4
EndIf
  print #1,"TrackaRoll Fin,,maxpos y (ntk).maxpos ", maxpos,pmTk(ntk).MaxPos
print #1,"-----------------Fin TrackaRoll-----------------"
Sleep 100

Exit Sub 

fail:


 Dim errmsg As String
 Dim As Long er1 = Err()
If  er1 > 0 Then
print #1,"-----------------err TrackaRoll-----------------"
  errmsg = "TrackaRoll FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(er1)
  Print #1, errmsg
End If
FileFlush (-1)
End Sub

'-------------------------------------------------

Sub Resetear ( )
Dim i As Integer
 For i =1 To 32  ' agregamos los 32 de ejecuciones 07-06-2022
   pmTk(i).desde=0
   pmTk(i).hasta=0
   pmTk(i).NB =0 
   pmTk(i).NA=0
   pmTk(i).MaxPos=0
   pmTk(i).posn=0
   pmTk(i).notaold=0
   pmTk(i).Ticks=0
   pmTk(i).portout=0
   titulosTk(i)=""
   pmTk(i).patch=0
   pmTk(i).notaescala=0
   pmTk(i).tipoescala=0
   pmTk(i).alteracion=0  ' sos 3, bem 2
   pmTk(i).fechasPistas=0
   pmTk(i).canalsalida=0
   pmTk(i).canalentrada=0
   pmTk(i).zona1=0
   pmTk(i).zona2=0
   pmTk(i).nroRep=0
   pmTk(i).portin=0 
   pmTk(i).tipoCompas=0
   pmTk(i).ejec=0
   pmTk(i).vol=0
   pmTk(i).tiempopatron=0 ' 240 60 etc
   pmTk(i).pan=64
   pmTk(i).Eco=0
   pmTk(i).Coro=0


   pmEj(i).desde=0
   pmEj(i).hasta=0
   pmEj(i).NB =0 
   pmEj(i).NA=0
   pmEj(i).MaxPos=0
   pmEj(i).posn=0
   pmEj(i).notaold=0
   pmEj(i).Ticks=0
   pmEj(i).portout=0
   titulosEj(i)=""
   pmEj(i).patch=0
   pmEj(i).notaescala=0
   pmEj(i).tipoescala=0
   pmEj(i).alteracion=0  ' sos 3, bem 2
   pmEj(i).fechasPistas=0
   pmEj(i).canalsalida=0
   pmEj(i).canalentrada=0
   pmEj(i).zona1=0
   pmEj(i).zona2=0
   pmEj(i).nroRep=0
   pmEj(i).portin=0 
   pmEj(i).tipoCompas=0
   pmEj(i).ejec=0
   pmEj(i).vol=0
   pmEj(i).tiempopatron=0 ' 240 60 etc
   pmEj(i).pan=64
   pmEj(i).Eco=0
   pmEj(i).Coro=0

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
' ----------------------------------------
'               P L A Y   C A N C I O N 
'------------------------------------------
Sub PlayCancion(Track() As sec)
Print #1,"' ----------------------------------------"
Print #1,"'               P L A Y   C A N C I O N" 
Print #1,"'------------------------------------------"
CPlay = SI
On Local Error GoTo PlayCancionError
Dim  As Integer i1,NroEvento

ReDim  MidiDatos (1 To tope) As miditxtsalida 
ReDim  NroEventoPista(1 To tope )


If MIDIFILEONOFF = HABILITAR  Then 
   MICROSEGUNDOS_POR_NEGRA = 60000000/tiempoPatron ' 60 MILL /BPM
   '' SE AJUSTO A 2000 PARA ESCUCHAR LO MISMO A 60 DSRG POR NEGRA... 500 
   '' NO ESTA CLARO EL  TEMPO EN BPM SON 60 EN TIEMPO 1000000 ,NI 500 NI 1000 NI 2000
   '' FALTRA ENTENDER MAS 
'' el formato 1 tiene la cabeza como track 1 el resto son los siguientes tracks
'' aca saldra en formato 0 todo mezclado..luego se nescitaria
'' sumar los canales diferentes ch=1, ch=2 la suma da el nro de tracks -1
'' luego volcar en otro archivo separado la cabeza y luego los Mtrk para cada canal
'' y asi obtendre el formato 1. o sea luego de generar secuenciaPlay.txt e3n formato 0
'' debo reconstruir en otro txt el formato 1 y ese archivo se convertira a *.mid
'' haremos una cosa secuenciaplay.txt sera el formato 1 necesito otro archivo intermedio
 
   indicenotas=0
   'Dim As String NombreTrack
   midiplano=20
   'NombreTrack= sacarpath(titulos(ntk)) 
   i1=InStrRev(NombreCancion,"\")
   dim  As String nc, tiempo 
   nc=Mid(NombreCancion,i1+1)
  Dim numc As Integer = CInt(pmTk(0).canalsalida) + 1
' suponemos que en la cancion todos los tracks tienen el mismo tempo
' luego el tempo  del Roll sera igual al resto usamoes el de roll
' igual que el plaAll , TipoCompas y tempo son dos cosas distintas 

      tiempo = tempoString(TipoCompas)
 
   Print #midiplano, "MFile 1 "+Str(tope+1)+" " + Str (1000)
   Print #midiplano, "MTrk"
   Print #midiplano, "0 Meta SeqName "; Chr(34);nc;Chr(34)
   Print #midiplano, "0 Meta Text "; chr(34);"Creado por RollMusic"; chr(34)
   Print #midiplano, "0 Tempo " + Str (MICROSEGUNDOS_POR_NEGRA)
   Print #midiplano, "0 TimeSig " + tiempo 
   Print #midiplano, "0 KeySig 0 Major"
   Print #midiplano, "0 Meta TrkEnd"
   Print #midiplano, "TrkEnd"
'   Print #midiplano, "0 Meta TrkName "; Chr(34); nc ;Chr(34)
'''' patchs
' For i1=1 To tope
'   Print #midiplano, "0 PrCh  ch=";numc;" "; "p=";pmTk(i1).patch
  ''veremos como se pone el portout si se puede pmTk(pis).portout)	
' Next i1
'i1=0
'preparo vector de datos txt a grabar

  Dim As Integer p1
  For p1 =1 To tope
        MidiDatos(p1).pista=p1        
  Next p1

EndIf


'------------apertura de ports
Dim As Long porterror,nousar
' idea para controlar cancion con repeticiones podria usar el track 00 solo para eso control
' creoa todos los defaults siempre
' 1-----------de playall--------
PARAR_PLAY_MANUAL=NO
PARAR_PLAY_EJEC=NO    
playloop=NO:playloop2=NO
' 1------------de playall fin---------

' los nombres ya fueron cargados al inicio
Print #1,"TOPE ", tope
Print #1,"abriendo ports....play cancion "

Dim As Integer k1,i
For i=1 To tope
  
   k1=CInt(pmTk(i).portout)
    
'   Print #1,"midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
    Print #1,"No se usa Microsoft"
    Exit sub
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( ) '' es como un new RtMidiOut()
           listoutCreado( k1)=1 
        EndIf
        open_port midiout(k1),k1, nombreOut(k1)
            porterror=Err 
        listoutAbierto( k1) = 1
        Print #1,"abro ",*nombreOut(k1)
        porterrorsub(porterror)

     Else
        Print #1,"ya abierto ",*nombreOut(k1)
        porterror=0
     EndIf
   EndIf

  

Next i

Dim As Double tiempoDUR
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron'60 seg/ cuantas negras enun minuto

Dim As Integer i2,K, mayor,i0,xmouse,ymouse,finfin=0,finalloop=0,comienzoloop=0,Smayor
Dim As UByte i3=0, pis=0, pisnota=0

Dim As Integer comienzo=1, final=0, vel=100,velpos =0,cntrepe=0,final2=0,comienzo2=0

' canal 0 es el 1 van de 0 a 15
' pasoCol debe ser de 2 dimensiones 1 para c/pista, 115 el amxio de octavas
' ojo si cambioaamos por mas octavas debo cambiar, igual el nro de tracks 32 
''Dim pasoCol (0 To 384) As vec  ' entrada de durciones a medida que barro una columna
'------------determinamos el MAxPos de toda la cancion o sea la pista de mayor longitud
mayor=pmTk(1).MaxPos
For i0=1 To Tope 
Print #1,"CANCION ntk MAXPOS "; pmTk(i0).MaxPos
  If InStr(LCase(TitulosTk(i0)),".solo") = 0 Then
     Continue For
  EndIf   
fileflush(-1)
 If mayor < pmTk(i0).MaxPos Then 
    mayor=pmTk(i0).MaxPos
 EndIf  
 
Next i0
RecalCompas()
 Print #1,"CANCION Tope MAXPOS "; Tope, mayor  

Print #1,"cancion tiempoPatron ";tiempoPatron
final=mayor 
maxposTope=mayor 

Print #1,"maxposTope ===> ", maxposTope
fileflush(-1)
Maxpos=mayor

Print #1,"UBound(compas, 1) ",UBound(compas, 1)

'-------------
Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,Notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim As Integer result,limsup,vertical=12+(hasta-2)*13+hasta 'vertical en Roll donde estan las repeticiones
Dim As UByte canal=0 
' tope es la maxima capacidad de tracks usada , lso tracks son contiguos
' si se borra un track queda sin usar eso hay que verlo...JMG ..
' por ahora solo proceso los que tengan lim2 o si llegamos a tope 
  jply=1:curpos=1
  mousex=0
' print #1,                    "-----------------------------------------"
  comienzo=posicion
  cntold=0
  If pasoZona1 > 0 Then
    comienzo=pasoZona1 
    comienzoloop=comienzo
  EndIf

  If pasoZona2 > 0 Then
    final=pasoZona2
    finalloop=final
  EndIf

If comienzo = 0 Then  '01-03-2024 play sin roll
  comienzo= 1 
End If

Dim As Double  tickUsuario=60/(tiempoPatron*96) ''''tickUsuario=0.01041666 * 240/tiempoPatron
' SI TEMPOPATRON O VELOCIDAD ES 240 LA SEMIFUSA VALE ESO 0.01041666
' SI TIEMPOPATRON VALE 60 LA SEMIFUSA VALE X 4= 0,0416666
Print #1,"TickUsuario "; tickUsuario

tickUsuario=60/(tiempoPatron*96)

  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
' Esto es play de pistas el limite de barrido es 1 a 12 siempre en c/pista  
' debo hcer un for para cada pista, en cada psita ver si esta mute o play
' ajustar el instrumento de la pista cada vez que cambie la pista.

 Print #1," PLAYCANCION MAXPOS AL COMENZAR ,final tope ",Maxpos,final,tope
Dim As float ajuste=1.0
''//////////////// PISTA //////////////
Dim As Integer cntRtk,cntSolo,cnt_pistas_cancion_suenan
' <=========CHEQUEOS PREVIOS DE LAS PISTAS ========>
 For pis=1 To tope
' escribimos salidamidi
    If MIDIFILEONOFF = HABILITAR  Then 
       MidiDatos(pis).datos(1)="MTrk"
       Dim ii As Integer   
       ii=InStrRev(titulosTk(pis),"\")
       dim  As String ncii, tiempo 
       ncii=Mid(titulosTk(pis),ii+1)
       MidiDatos(pis).datos(2)= "0 Meta TrkName "+ Chr(34) + ncii + Chr(34) 
'''' patchs
       MidiDatos(pis).datos(3)= "0 PrCh  ch=" + Str(CInt(pmTk(pis).canalsalida) + 1)+ " p="+Str(pmTk(pis).patch)
  ''veremos como se pone el portout si se puede pmTk(pis).portout)	
       NroEventoPista(pis)=3
    EndIf
 '     Print #1,"ON patch ntk canal ",	Track(ntk).trk(1,1).nnn, ntk,pmTk(pis).canalsalida
 ' PARA TODOS RTK Y SOLO
      Paneo (pmTk(pis).pan, pmTk(pis).canalsalida,pmTk(pis).portout)
      Eco   (pmTk(pis).eco,  pmTk(pis).canalsalida,pmTk(pis).portout) 
      Chorus(pmTk(pis).coro,  pmTk(pis).canalsalida,pmTk(pis).portout)
      ChangeProgram ( pmTk(pis).patch, pmTk(pis).canalsalida, pmTk(pis).portout)	
' reveeer esto de sonido ,,,,,
    If InStr(LCase(TitulosTk(pis)),".solo") > 1 Then
       cntSolo=cntSolo +1
       CheckBox_SetCheck( cbxsolo(pis), 1)
    EndIf
    If InStr(LCase(TitulosTk(pis)),".rtk") > 1 Then
       cntRtk=cntRtk +1
       If CheckBox_GetCheck( cbxnum(pis)) = 1 Then
          cnt_pistas_cancion_suenan=cnt_pistas_cancion_suenan +1
       EndIf 
 
    EndIf


      If instancia=ARG7_NOMBRECANCION Or instancia=ARG107_FICTICIO  Then 'batch grafico solo
          sonidoPista(pis)=1
      Else 
      
          If CheckBox_GetCheck( cbxnum(pis))= 1 Then
 '        Print #1,"+++++++pista on ",pis
             sonidoPista(pis)=1
          Else  
 '        Print #1,"+++++++pista off ",pis
         alloff( pmTk(pis).canalsalida,CInt(pmTk(pis).portout) )
         allSoundoff( pmTk(pis).canalsalida, CInt(pmTk(pis).portout) ) 
             sonidoPista(pis)=0
          EndIf
      EndIf

 Next pis
 If cnt_pistas_cancion_suenan = 0 And cntSolo > 0 Then
' SIEMPRE DEBE EJECUTARSE UNA PISTA DE CANCION QUE ES LA GUIA, SI QUEREMOS ESCUCHAR UN SOLO 
' DEBERIA ESTAR CHEQUEADO UNA  PISTA DE CANCION Y EL VOL=0. SINO MEJOR DESDE UN DOBLE CLICK
' AL ARCHIVO SOLO 
    For K As Integer = 1 To Tope
     If CheckBox_GetCheck(cbxsolo(k))= 0 And InStr(LCase(TitulosTK(k)),".rtk") = 1  Then ' la primer pista que no este chequeda para solo 
       ' se chequea para cancion
        CheckBox_SetCheck(cbxnum (k),1)
     EndIf 
    Next k
 EndIf
'' <=========FIN CHEQUEOS PREVIOS DE LAS PISTAS ========>

STARTMIDI=Timer
old_time_on=STARTMIDI
Print #1,"old_time_on PLAYCANCION "; old_time_on
Sleep 11 ' para ESPERAR a  inicio de playunosolo
Print #1,"ENTRA PLAYCANCION  "; Timer


' SI TEMPOPATRON O VELOCIDAD ES 240 LA SEMIFUSA VALE ESO 0.01041666
' SI TIEMPOPATRON VALE 60 LA SEMIFUSA VALE X 4= 0,0416666
Print #1,"TickUsuario "; tickUsuario
''" ---------------000000000000000000000-----------------"
''//////////////// L O O P E O  JPLY HORIZONTAL ////////////////////////////////////////////////
''" ---------------000000000000000000000-----------------"
For jply=comienzo To final
'  print #1," ---------------000000000000000000000-----------------"
'  print #1," [[[PCA 0:]]]---START--PASO:[";jply;"] ----------------"
'  print #1," ---------------000000000000000000000-----------------"
  
' cambio de inst para la pista, podria poner mas de un instrumento por pista
' o por cada nota.. 
' VER DE PONER LOS INSTRUMENTOS EN TRACK
   If PARAR_PLAY_MANUAL = SI Then
      For i3 = 1 To tope
       portsal=CInt(pmTk(i3).portout) 
       alloff(pmTk(i3).canalsalida,portsal) 
       allSoundoff( pmTk(i3).canalsalida, portsal ) 
      Next i3
      Sleep 5
      PARAR_PLAY_MANUAL=NO
      Parar_De_Dibujar=NO
      Exit For
   EndIf  
  
kNroCol= Int(jply/NroCol)
   If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < final)Then
     posicion=jply
     curpos=0
   EndIf


 '  Print #1," cancon jply velpos "; jply, velpos
''' /////////////////////// L O O P DE P I S T A S /////////////////////////////////// 
'''/// BARRE VERTICALMENTE LAS PISTAS PARA CADA POSICION HORIZONTAL medio rebuscado al dope creo..////
''// un metodo mejroe seria no barrer nada solo el archivo de eventos en fin 
  For pis =1 To tope ' loop de pistas rtk
    If CheckBox_GetCheck( cbxsolo(pis))= 1 Then
         Continue For ' saltea no tocar pero la toca playall 
    EndIf       
    If CheckBox_GetCheck( cbxnum(pis))= 1 Then
       ' tocar
         sonidoPista(pis)=1
    Else
      If MIDIFILEONOFF = HABILITAR Then
      Else
    '   mandar off a todas las notas de la pista silenciada 
        alloff( pmTk(pis).canalsalida,CInt(pmTk(pis).portout) )
        sonidoPista(pis)=0       
        Continue For ' saltear no tocar
      EndIf 
    EndIf 
    ajuste=pmTk(pis).vol/127
''''Print #1,"PISTA AJUSTE ",pis, ajuste
    If Track(pis).trk(1,1).ejec = 1 Or pmTk(pis).ejec = 1 Then ' VIENE DE UNA EJEC
        
    Else

      If Compas(jply).nro = -1 Then
        velpos=vfuerte * ajuste
      EndIf
      If Compas(jply).nro = -2 Then
         velpos=vdebil * ajuste
      EndIf
      If Compas(jply).nro = -3 Then
         velpos=vsemifuerte * ajuste
      EndIf
      If Compas(jply).nro = -4 Then
         velpos=vdebil * ajuste
      EndIf
      If Compas(jply).nro > 0 Then ' marca del numero de compas 1 2 3 4 es el ultimo tiempo del compas
         velpos=vdebil * ajuste
      EndIf
      If Compas(jply).nro = 0 Then 
        velpos=vsemifuerte * ajuste ' para midipolano divisones por partes veremso si se soluciona el sonido
   ' en la rutina vol , depende de la dur ajusta vol=0 o vol = velpos... no hay problema con los silencios
      EndIf
    EndIf

 '  print #1,"--loop de pistas---pista NRO :";pis;" --------------------------------"
 '  print #1,"  De esta pista MAXPOS ,final",pmTk(pis).MaxPos, final
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

 'Print #1,"FOR -- RECORRIDO DE NOTAS DE PISTA", pis

   If jply <= pmTk(pis).MaxPos Then ' tocamos una pista mientras que tenga datos
     If CheckBox_GetCheck( cbxsolo(pis))= 1 Then
       Continue For ' saltea no tocar pero la toca playUNOSOLO 
     EndIf 
     For i1=1 To lim3   'lim3 decia coo voy de 1 a lim2 necesito que la info del int este en 1
       If i1<= lim2  And (pis <= tope Or pis<=32) Then
         If (Track(pis).trk(jply,i1).nota >= NBpiano) And (Track(pis).trk(jply,i1).nota <= NA) And (Track(pis).trk(jply,i1).dur >=1) And (Track(pis).trk(jply,i1).dur <= 180) Or Track(pis).trk(jply, i1).dur <= 183 Or Track(pis).trk(jply, i1).dur <= 185 Then ' es semitono
            Notapiano = CInt(Track(pis).trk(jply,i1).nota)' solo en los onoff2 es notapiano o no?
            portsal=pmTk(pis).portout
            canal=pmTk(pis).canalsalida
            If sonidoPista(pis)=1 Then
               If Track(ntk).trk(1,1).ejec = 1 Or pmTk(pis).ejec=1 Then
         ' Print #1,"playAll Roll.trk(jply, i1).onoff ,vol ";Roll.trk(jply, i1).onoff, Roll.trk(jply, i1).vol
                  vel=Track(pis).trk(jply,i1).vol * ajuste
               EndIf
             ' la duracion me da si suena o no

               Select CASE Track(pis).trk(jply,i1).dur
                  Case 46 To  90  'silencios
                      vel=0  
                  Case 138 To 180  'silencios
                     vel=0
                  Case Else
                     vel=Track(pis).trk(jply,i1).vol
                     If vel=0 Then
                       vel=velpos
                     EndIf   
                 End select 
  
            Else
               alloff( pmTk(pis).canalsalida,CInt(pmTk(pis).portout) )
               vel=0
            EndIf
         EndIf
' llegamos al final de la Columna
         If Track(pis).trk(jply,i1).onoff =2 Then
  '          NroEventoPista(pis) = NroEventoPista(pis) + 1 por ahora sacamos es pa volcar midi a disco
  '          NroEvento=NroEventoPista(pis)
            noteon CUByte(Notapiano),vel,canal,portsal,pis,NroEvento
         EndIf
         If Track(pis).trk(jply,i1).onoff= 1 Then
  '          NroEventoPista(pis)=NroEventoPista(pis) +1
  '          NroEvento=NroEventoPista(pis)
            noteoff CUByte(Notapiano),canal,portsal,pis,NroEvento
         EndIf    
       EndIf

'--------
     If i1 > lim2  Then
       If Track(pis).trk(jply,i1).nota = 210 Then
  '  Print #1,"210 leido jply",jply
         playloop2=SI
         comienzo2=jply
     EndIf

     If Track(pis).trk(jply,i1).nota = 211 Then
     '  Print #1,"211 leido jply",jply
           final2=jply
'---------- reset de las notas que no llegan a su off por un corte antes del off -- ok 
' deberia hacer el corte tambien si hay ejecuciones 
         For i3 = 1 To tope
            portsal=CInt(pmTk(i3).portout) 
            alloff(pmTk(i3).canalsalida,portsal) 
            allSoundoff( pmTk(i3).canalsalida, portsal ) 
         Next i3
         If TopeEjec > 0 Then
            For i3 = 1 To topeEjec
               portsal=CInt(pmEj(i3).portout) 
               alloff(pmEj(i3).canalsalida,portsal) 
               allSoundoff( pmEj(i3).canalsalida, portsal ) 
            Next i3
         EndIf
'---------------------------------------------
        If cntrepe > 0 Then
           cntrepe -= 1
        Else
           cntrepe=Track(pis).trk(jply,i1).vol ' nro repeticiones en vertical +1
        EndIf
        If cntrepe =0 Then
           final2=Mayor 
           If finalloop > 0 Then
              final2=finalloop
           EndIf
        EndIf 
     EndIf
   EndIf

 
'---- 

     Next i1
   EndIf

  Next pis

  duracion (old_time_on ,tickUsuario) ' si es cero no 1 no hay duracion es acorde
  old_time_on=old_time_on + tickUsuario 'jmgtiempo


'  print #1,"---FIN -----paso:"; jply;" --------------------------------"
'  Print #1,"---FIN---playloop PLAYLOOP2 ",playloop, playloop2
  If playloop=SI And jply= finalloop  Then
    jply=comienzoloop -1

  EndIf
  If playloop2=SI And jply= final2    Then
    if finfin=0  Or playloop=SI Then
       jply=comienzo2 -1
    EndIf
    If final2=finalloop Then 
       If playloop=SI Then
         jply=comienzoloop -1
       Else
         final=Mayor 
         final2=Mayor 
         jply=final2 
       EndIf
    EndIf
  EndIf
  Sleep 1,1 ' para que corranmas de un thread

Next jply

If MIDIFILEONOFF = HABILITAR Then 
    Dim As Double TiempoAcumNew
     Dim As Integer T1
     TiempoAcumNew= Timer - STARTMIDI
     T1 =  TiempoAcumNew *  1000 * tiempoPatron/60
     'las terminaciones de cada pista 
     For pis=1 To tope
       NroEventoPista(pis) = NroEventoPista(pis) + 1
       NroEvento=NroEventoPista(pis) 
       MidiDatos(pis).datos(NroEvento)= Str(T1) + " Meta TrkEnd"
       NroEventoPista(pis) = NroEventoPista(pis) + 1
       NroEvento=NroEventoPista(pis) 
       MidiDatos(pis).datos(NroEvento)="TrkEnd"
     Next pis
     MIDIFILEONOFF = DESHABILITAR
      'volcar misDatos a SecuenciaPlay.txt
       Dim k4 As Integer
       For pis=1 To tope
          For k4=1 To NroEventoPista(pis)
              Print #midiplano,MidiDatos(pis).datos(k4)
          Next k4
       Next pis
      
       Close 20 ' cierra SecuemciaPlay.txt FIX ERROR 07-10-2025
EndIf  
'
posicion=comienzo
'posishow=posicion + 20
'posishow=posicion - 20
'posicion=posicion -20
 
jply=0:curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
 

Cplay=NO ' Control de play cancion si fue desde control
trabaspace=0

'''  reponer mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0

 SuenaTodo=0 ' habilita boton cotrol sonido de pistas
 
Sleep 10,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
/'
close_port(midiout)
out_free(midiout)
'/ 
If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Then 
Else
 SetGadgetstate(BTN_ROLL_EJECUTAR,0)
EndIf

Dim As Integer checkejec=0
For  iz As Short =1 To 32
      If CheckBox_GetCheck( cbxejec(iz))= 1  Or CheckBox_GetCheck( cbxgrab(iz))= 1 Then
          checkejec=1
      End If
      Exit For
Next iz
if GrabarPenta=0 and GrabarEjec=HabilitaGrabar and Parar_De_Dibujar=NO And checkejec=0 Then 
   For i=1 To tope
      k1=pmTk(i).portout
   '   Print #1,"midiout ",k1, *nombreOut(k1)
      alloff( pmTk(i).canalsalida,k1 )  
      allSoundoff( pmTk(i).canalsalida,k1 )  

      ''Print #1,"desmarco ",*nombreOut(k1)
      listoutAbierto(k1)=0
      close_port midiout(k1)
   '   out_free   midiout(k1)
      listoutAbierto( k1) = 0
   Next i
EndIf 
  Parar_De_Dibujar=NO ' 05-03-2024
  Cplay=NO
STARTMIDI=0
  For i3 = 1 To tope
       portsal=CInt(pmTk(i3).portout) 
       alloff(pmTk(i3).canalsalida,portsal) 
       allSoundoff( pmTk(i3).canalsalida, portsal ) 
  Next i3

Sleep 20,1

Exit Sub

PlayCancionError:
 Dim errmsg As String

If  Err > 0 Then
  If i1=26 Then i1=25 EndIf

  errmsg = "FAIL PlayCANCION Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg ,"jply ", jply, "i1 ";i1
  Print #1, "ubound(Compas,1) ",ubound(Compas,1)
Print #1, "Track(pis).trk(jply,i1).nota ", Track(pis).trk(jply,i1).nota
Print #1, "Track(pis).trk(jply,i1).dur ", Track(pis).trk(jply,i1).dur
Print #1, "maxposTope  MaxPos  jply  ",maxposTope, MaxPos, jply  

  FileFlush (-1)

'''''ThreadDetach(thread1)
  Parar_De_Dibujar=NO ' 05-03-2024
 
EndIf

End Sub 

'-----------------------------------------
Sub PlayUnoSolo(SolX As sec, Spis As Integer)
''PORQUE PIERDE EL PATCH EN LA SEGUNDA EJECUCION ??????????????
Print #1,"' ----------------------------------------",Spis
Print #1,"'               P L A Y  U N O  S O L O"   ,Spis 
Print #1,"'------------------------------------------",Spis
''CPlay = SI
'ABRE PRTS PERO NO LOS CIERA
On Local Error GoTo PlaySoloError
Dim  As Integer i1,NroEvento,Smayor
Dim JSOLO As Integer '''LOCAL
ReDim  MidiDatos (1 ) As miditxtsalida 
ReDim  NroEventoPista(1  )
Sleep 10

If MIDIFILEONOFF = HABILITAR  Then 
   MICROSEGUNDOS_POR_NEGRA = 60000000/tiempoPatron ' 60 MILL /BPM
   '' SE AJUSTO A 2000 PARA ESCUCHAR LO MISMO A 60 DSRG POR NEGRA... 500 
   '' NO ESTA CLARO EL  TEMPO EN BPM SON 60 EN TIEMPO 1000000 ,NI 500 NI 1000 NI 2000
   '' FALTRA ENTENDER MAS 
'' el formato 1 tiene la cabeza como track 1 el resto son los siguientes tracks
'' aca saldra en formato 0 todo mezclado..luego se nescitaria
'' sumar los canales diferentes ch=1, ch=2 la suma da el nro de tracks -1
'' luego volcar en otro archivo separado la cabeza y luego los Mtrk para cada canal
'' y asi obtendre el formato 1. o sea luego de generar secuenciaPlay.txt e3n formato 0
'' debo reconstruir en otro txt el formato 1 y ese archivo se convertira a *.mid
'' haremos una cosa secuenciaplay.txt sera el formato 1 necesito otro archivo intermedio
 
   indicenotas=0
   'Dim As String NombreTrack
   midiplano=20
   'NombreTrack= sacarpath(titulos(ntk)) 
   i1=InStrRev(NombreCancion,"\")
   dim  As String nc, tiempo 
   nc=Mid(NombreCancion,i1+1)
  Dim numc As Integer = CInt(pmTk(0).canalsalida) + 1
' suponemos que en la cancion todos los tracks tienen el mismo tempo
' luego el tempo  del Roll sera igual al resto usamoes el de roll
' igual que el plaAll , TipoCompas y tempo son dos cosas distintas 

      tiempo = tempoString(TipoCompas)
 
   Print #midiplano, "MFile 1 "+Str(tope+1)+" " + Str (1000)
   Print #midiplano, "MTrk"
   Print #midiplano, "0 Meta SeqName "; Chr(34);nc;Chr(34)
   Print #midiplano, "0 Meta Text "; chr(34);"Creado por RollMusic"; chr(34)
   Print #midiplano, "0 Tempo " + Str (MICROSEGUNDOS_POR_NEGRA)
   Print #midiplano, "0 TimeSig " + tiempo 
   Print #midiplano, "0 KeySig 0 Major"
   Print #midiplano, "0 Meta TrkEnd"
   Print #midiplano, "TrkEnd"
'   Print #midiplano, "0 Meta TrkName "; Chr(34); nc ;Chr(34)
'''' patchs
' For i1=1 To tope
'   Print #midiplano, "0 PrCh  ch=";numc;" "; "p=";pmTk(i1).patch
  ''veremos como se pone el portout si se puede pmTk(pis).portout)	
' Next i1
'i1=0
'preparo vector de datos txt a grabar

  Dim As Integer p1

   MidiDatos(1).pista=1        


EndIf


'------------apertura de ports
Dim As Long porterror,nousar
' idea para controlar cancion con repeticiones podria usar el track 00 solo para eso control
' creoa todos los defaults siempre
' 1-----------de playall--------
PARAR_PLAY_MANUAL=NO
PARAR_PLAY_EJEC=NO    
Dim As Integer Splayloop,Splayloop2
Splayloop=NO:Splayloop2=NO
' 1------------de playall fin---------

' los nombres ya fueron cargados al inicio
Print #1,"abriendo ports....play solos "
sonidoPista(Spis)=1
Dim As Integer k1,i
 i=Spis 
  
   k1=CInt(pmTk(i).portout)
    
'   Print #1,"midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
    Print #1,"No se usa Microsoft"
    Exit sub
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( ) '' es como un new RtMidiOut()
           listoutCreado( k1)=1 
        EndIf
        open_port midiout(k1),k1, nombreOut(k1)
            porterror=Err 
        listoutAbierto( k1) = 1
        Print #1,"abro ",*nombreOut(k1)
        porterrorsub(porterror)

     Else
        Print #1,"ya abierto ",*nombreOut(k1)
        porterror=0
     EndIf
   EndIf

  



Dim As Double tiempoDUR
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron'60 seg/ cuantas negras enun minuto

Dim As Integer i2,K, i0,xmouse,ymouse,Sfinfin=0,Sfinalloop=0,Scomienzoloop=0
Dim As UByte i3=0, pisnota=0

Dim As Integer Scomienzo=1, Sfinal=0, vel=100,velpos =0,Scntrepe=0,Sfinal2=0,Scomienzo2=0, Sntk

' canal 0 es el 1 van de 0 a 15
' pasoCol debe ser de 2 dimensiones 1 para c/pista, 115 el amxio de octavas
' ojo si cambioaamos por mas octavas debo cambiar, igual el nro de tracks 32 
''Dim pasoCol (0 To 384) As vec  ' entrada de durciones a medida que barro una columna
'------------
Dim As Integer cntpis=0,Stope=1 
 
''' Maxpos es de la cancion ya calculado por playCancion aca no se usa no va mas halla dela lonfg de l apista
Smayor=pmTk(Spis).MaxPos ' usamos solo la de la pista

Print #1,"solo Spis MAXPOS "; Spis, Smayor  
Print #1,"PlayUnoSolo pmTk(pis).patch,spis ",CInt(pmTk(Spis).patch), Spis
Print #1,"solo tiempoPatron ";tiempoPatron
Sfinal=Smayor



'fileflush(-1)
''''RecalCompas()
'Print #1,"UBound(compas, 1) ",UBound(compas, 1)
'Sleep 10
'-------------
Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,Notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim As Integer result,limsup,vertical=12+(hasta-2)*13+hasta 'vertical en Roll donde estan las repeticiones
Dim As UByte Scanal=0,Sportsal=0 
' tope es la maxima capacidad de tracks usada , lso tracks son contiguos
' si se borra un track queda sin usar eso hay que verlo...JMG ..
' por ahora solo proceso los que tengan lim2 o si llegamos a tope 
  JSOLO=1:'''Scurpos=1
  mousex=0
' print #1,                    "-----------------------------------------"
  Scomienzo=1 ''''posicion
  cntold=0
  If pasoZona1 > 0 Then
    Scomienzo=pasoZona1 
    Scomienzoloop=Scomienzo
  EndIf

  If pasoZona2 > 0 Then
    Sfinal=pasoZona2
    Sfinalloop=Sfinal
  EndIf

If Scomienzo = 0 Then  '01-03-2024 play sin roll
  Scomienzo= 1 
End If

'''STARTMIDI=Timer
Dim As Double Sold_time_on
Sold_time_on=STARTMIDI
Print #1,"Sold_time_on "; Sold_time_on
Dim As Double  tickUsuario=60/(tiempoPatron*96) ''''tickUsuario=0.01041666 * 240/tiempoPatron
' SI TEMPOPATRON O VELOCIDAD ES 240 LA SEMIFUSA VALE ESO 0.01041666
' SI TIEMPOPATRON VALE 60 LA SEMIFUSA VALE X 4= 0,0416666
Print #1,"TickUsuario "; tickUsuario

tickUsuario=60/(tiempoPatron*96)

  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
' Esto es play de pistas el limite de barrido es 1 a 12 siempre en c/pista  
' debo hcer un for para cada pista, en cada psita ver si esta mute o play
' ajustar el instrumento de la pista cada vez que cambie la pista.

 Print #1," PlayUnoSolo MAXPOS AL COMENZAR ,final tope ",Smayor,Sfinal,Stope
Dim As float ajuste=1.0
''//////////////// PISTA //////////////

' escribimos salidamidi
    If MIDIFILEONOFF = HABILITAR  Then 
       MidiDatos(Spis).datos(1)="MTrk"
       Dim ii As Integer   
       ii=InStrRev(titulosTk(Spis),"\")
       dim  As String ncii, tiempo 
       ncii=Mid(titulosTk(Spis),ii+1)
       MidiDatos(Spis).datos(2)= "0 Meta TrkName "+ Chr(34) + ncii + Chr(34) 
'''' patchs
       MidiDatos(Spis).datos(3)= "0 PrCh  ch=" + Str(CInt(pmTk(Spis).canalsalida) + 1)+ " p="+Str(pmTk(Spis).patch)
  ''veremos como se pone el portout si se puede pmTk(pis).portout)	
       NroEventoPista(Spis)=3
    EndIf
  Print #1,"ON patch pis canal ",	pmTk(Spis).patch, Spis ,pmTk(Spis).canalsalida

      Paneo (pmTk(Spis).pan, pmTk(Spis).canalsalida,pmTk(Spis).portout)
      Eco   (pmTk(Spis).eco,  pmTk(Spis).canalsalida,pmTk(Spis).portout) 
      Chorus(pmTk(Spis).coro,  pmTk(Spis).canalsalida,pmTk(Spis).portout)
      ChangeProgram ( pmTk(Spis).patch, pmTk(Spis).canalsalida, pmTk(spis).portout)	
Print #1,"Cambio Prog pis patch, portout, canal ", Spis, pmTk(Spis).patch, pmTk(spis).portout,pmTk(Spis).canalsalida
    fileflush(-1)
' reveeer esto de sonido ,,,,,
      If instancia=ARG7_NOMBRECANCION Or instancia=ARG107_FICTICIO  Then 'batch grafico solo
          sonidoPista(Spis)=1
      Else 
      
          If  CheckBox_GetCheck( cbxsolo(Spis))= 1 Then
 '        Print #1,"+++++++pista on ",pis
             sonidoPista(Spis)=1
          Else  
 '        Print #1,"+++++++pista off ",pis
         alloff( pmTk(Spis).canalsalida,CInt(pmTk(Spis).portout) )
         allSoundoff( pmTk(Spis).canalsalida, CInt(pmTk(Spis).portout) ) 
             sonidoPista(Spis)=0
          EndIf
      EndIf



''STARTMIDI=Timer
Sold_time_on=STARTMIDI
Print #1,"old_time_on "; Sold_time_on
Print #1,"ENTRA UNOSOLO ",Timer
' SI TEMPOPATRON O VELOCIDAD ES 240 LA SEMIFUSA VALE ESO 0.01041666
' SI TIEMPOPATRON VALE 60 LA SEMIFUSA VALE X 4= 0,0416666
Print #1,"TickUsuario, comienzo, final "; tickUsuario, Scomienzo, Sfinal
''//////////////// L O O P E O  JSOLO HORIZONTAL ////////////////////////////////////////////////
For JSOLO=Scomienzo To Sfinal
'  print #1," ---------------000000000000000000000-----------------"
'  print #1," [[[PCA 0:]]]---START--PASO:[";JSOLO;"] ----------------"
'  print #1," ---------------000000000000000000000-----------------"
  
' cambio de inst para la pista, podria poner mas de un instrumento por pista
' o por cada nota.. 
' VER DE PONER LOS INSTRUMENTOS EN TRACK
   If PARAR_PLAY_MANUAL = SI Then

       Sportsal=CInt(pmTk(Spis).portout) 
       alloff(pmTk(Spis).canalsalida,Sportsal) 
       allSoundoff( pmTk(Spis).canalsalida, Sportsal ) 
      Sleep 5
      PARAR_PLAY_MANUAL=NO
      Parar_De_Dibujar=NO
      Exit For
   EndIf  
  
'kNroCol= Int(JSOLO/NroCol)
'   If (kNroCol > 0) And (JSOLO = NroCol * kNroCol) And (JSOLO < final)Then
'     posicion=JSOLO
'     curpos=0
'   EndIf


 '  Print #1," cancon JSOLO velpos "; JSOLO, velpos
''' /////////////////////// L O O P DE P I S T A S /////////////////////////////////// 
'''/// BARRE VERTICALMENTE LAS PISTAS PARA CADA POSICION HORIZONTAL medio rebuscado al dope creo..////
''// un metodo mejroe seria no barrer nada solo el archivo de eventos en fin 

    If CheckBox_GetCheck( cbxsolo(Spis))= 1 Then
       ' tocar
         sonidoPista(Spis)=1
    Else
      If MIDIFILEONOFF = HABILITAR Then
      Else
    '   mandar off a todas las notas de la pista silenciada 
        alloff( pmTk(Spis).canalsalida,CInt(pmTk(Spis).portout) )
        sonidoPista(Spis)=0       
        Continue For ' saltear no tocar
      EndIf 
    EndIf 
    ajuste=pmTk(Spis).vol/127
''    Print #1,"PISTA AJUSTE ",pis, ajuste
    If SolX.trk(1,1).ejec = 1 Or pmTk(Spis).ejec = 1 Then ' VIENE DE UNA EJEC
        
    Else

      If Compas(JSOLO).nro = -1 Then
        velpos=vfuerte * ajuste
      EndIf
      If Compas(JSOLO).nro = -2 Then
         velpos=vdebil * ajuste
      EndIf
      If Compas(JSOLO).nro = -3 Then
         velpos=vsemifuerte * ajuste
      EndIf
      If Compas(JSOLO).nro = -4 Then
         velpos=vdebil * ajuste
      EndIf
      If Compas(JSOLO).nro > 0 Then ' marca del numero de compas 1 2 3 4 es el ultimo tiempo del compas
         velpos=vdebil * ajuste
      EndIf
      If Compas(JSOLO).nro = 0 Then 
        velpos=vsemifuerte * ajuste ' para midipolano divisones por partes veremso si se soluciona el sonido
   ' en la rutina vol , depende de la dur ajusta vol=0 o vol = velpos... no hay problema con los silencios
      EndIf
    EndIf
velpos=vsemifuerte
 '  print #1,"--loop de pistas---pista NRO :";pis;" --------------------------------"
 '  print #1,"  De esta pista MAXPOS ,final",pmTk(pis).MaxPos, final
    limsup=UBound (SolX.trk,2)
    If limsup < lim2 Then
          Exit For 
    EndIf  

'If SolX.trk(1,1).inst > 0 Then
'  Print #1,"CAMBIO PATCH PISTA ",pis,"instru ",Track(pis).trk(1,1).inst
'  ChangeProgram ( Track(pis).trk(1,1).inst, pmTk(ntk).canalsalida,pmTk(ntk).portout)
'EndIf  

' ojo con silencios ligados !!!

' limites para cada pista NB,NA,son limites para las Notapiano
' si se grabo con un limite de octavas las Notapiano tambien estan limitadas  
   NB = 0  + (pmTk(Spis).desde -1 ) * 13 
   NA = 11 + (pmTk(Spis).hasta -1 ) * 13 
'   print #1,"Play Cancion NB ,NA, pis",NB,NA,pis
   
   Dim As ubyte NBpiano= CUByte(NB) - CUByte(restar(NB))
   Dim As ubyte NApiano= CUByte(NA) - CUByte(restar(NA))
'   print #1,"limite inferior NBpiano ",NBpiano
'   print #1,"limite Superior NApiano ",NApiano
' en realida seria bueno no tner limites para los tracks, cada track
' tiene polifonia de 12 o sea un acorde de 12 notas, ergo
' la cancion podria tener 12*32tracks = 384 de polifonia, no se si rtmidi lo soporta
' info del inst en 1 no en lim2   

 'Print #1,"FOR -- RECORRIDO DE NOTAS DE PISTA", pis
   If JSOLO <= pmTk(Spis).MaxPos Then ' tocamos una pista mientras que tenga datos 
     For i1=1 To lim3   'lim3 decia coo voy de 1 a lim2 necesito que la info del int este en 1
       If i1<= lim2  And Spis<=32 Then
         If (SolX.trk(JSOLO,i1).nota >= NBpiano)  And _
            (SolX.trk(JSOLO,i1).nota <= NA)      And _
            (SolX.trk(JSOLO,i1).dur >=1)         And _
            (SolX.trk(JSOLO,i1).dur <= 180)       Or _ 
            (SolX.trk(JSOLO, i1).dur <= 183)      Or _
            (SolX.trk(JSOLO, i1).dur <= 185)    Then ' es semitono
            Notapiano = CInt(SolX.trk(JSOLO,i1).nota)' solo en los onoff2 es notapiano o no?
            Sportsal=pmTk(Spis).portout
            Scanal=pmTk(Spis).canalsalida
            If sonidoPista(Spis)=1 Then
               If SolX.trk(1,1).ejec = 1 Or pmTk(Spis).ejec=1 Then
                   If SolX.trk(JSOLO, i1).vol > 0 Then
    '      Print #1,"playUnoSolo SolX.trk(JSOLO, i1).onoff ,vol ";SolX.trk(JSOLO, i1).onoff, SolX.trk(JSOLO, i1).vol
                    vel=SolX.trk(JSOLO,i1).vol * ajuste
                   Else
                     vel=80
                   EndIf
               Else
                  vel=VelPos
               EndIf
               ' la duracion me da si suena o no
               Select CASE SolX.trk(JSOLO, i1).dur
                 Case 46 To  90  'silencios
                      vel=0  
                 Case 138 To 180  'silencios
                     vel=0
               End select 
  
            Else
               alloff( pmTk(Spis).canalsalida,CInt(pmTk(Spis).portout) )
               vel=0
            EndIf
         EndIf
' llegamos al final de la Columna
         If SolX.trk(JSOLO,i1).onoff =2 Then
        '    NroEventoPista(pis) = NroEventoPista(pis) + 1
         '   NroEvento=NroEventoPista(pis)
            noteon CUByte(Notapiano),vel,Scanal,Sportsal,Spis,NroEvento
         EndIf
         If SolX.trk(JSOLO,i1).onoff= 1 Then
        '    NroEventoPista(pis)=NroEventoPista(pis) +1
         '   NroEvento=NroEventoPista(pis)
            noteoff CUByte(Notapiano),Scanal,Sportsal,Spis,NroEvento
         EndIf    
       EndIf

'--------
     If i1 > lim2  Then
       If SolX.trk(JSOLO,i1).nota = 210 Then
  '  Print #1,"210 leido JSOLO",JSOLO
         Splayloop2=SI
         Scomienzo2=JSOLO
     EndIf

     If SolX.trk(JSOLO,i1).nota = 211 Then
     '  Print #1,"211 leido JSOLO",JSOLO
           Sfinal2=JSOLO
'---------- reset de las notas que no llegan a su off por un corte antes del off -- ok 
' deberia hacer el corte tambien si hay ejecuciones 

            Sportsal=CInt(pmTk(Spis).portout) 
            alloff(pmTk(Spis).canalsalida,Sportsal) 
            allSoundoff( pmTk(Spis).canalsalida, Sportsal ) 

'---------------------------------------------
        If Scntrepe > 0 Then
           Scntrepe -= 1
        Else
           Scntrepe=SolX.trk(JSOLO,i1).vol ' nro repeticiones en vertical +1
        EndIf
        If Scntrepe =0 Then
           Sfinal2=SMayor 
           If Sfinalloop > 0 Then
              Sfinal2=Sfinalloop
           EndIf
        EndIf 
     EndIf
   EndIf

 
'---- 

     Next i1
   EndIf



  duracion (Sold_time_on ,tickUsuario) ' si es cero no 1 no hay duracion es acorde
  Sold_time_on=Sold_time_on + tickUsuario 'jmgtiempo


'  print #1,"---FIN -----paso:"; JSOLO;" --------------------------------"
'  Print #1,"---FIN---playloop PLAYLOOP2 ",playloop, playloop2
  If Splayloop=SI And JSOLO= Sfinalloop  Then
    JSOLO=Scomienzoloop -1

  EndIf
  If Splayloop2=SI And JSOLO= Sfinal2    Then
    if Sfinfin=0  Or Splayloop=SI Then
       JSOLO=Scomienzo2 -1
    EndIf
    If Sfinal2=Sfinalloop Then 
       If Splayloop=SI Then
         JSOLO=Scomienzoloop -1
       Else
         Sfinal=SMayor 
         Sfinal2=SMayor 
         JSOLO=Sfinal2 
       EndIf
    EndIf
  EndIf
  Sleep 1,1 ' para que corranmas de un thread

Next JSOLO

If MIDIFILEONOFF = HABILITAR Then 
    Dim As Double TiempoAcumNew
     Dim As Integer T1
     TiempoAcumNew= Timer - STARTMIDI
     T1 =  TiempoAcumNew *  1000 * tiempoPatron/60
     'las terminaciones de cada pista 

       NroEventoPista(Spis) = NroEventoPista(Spis) + 1
       NroEvento=NroEventoPista(Spis) 
       MidiDatos(Spis).datos(NroEvento)= Str(T1) + " Meta TrkEnd"
       NroEventoPista(Spis) = NroEventoPista(Spis) + 1
       NroEvento=NroEventoPista(Spis) 
       MidiDatos(Spis).datos(NroEvento)="TrkEnd"

     MIDIFILEONOFF = DESHABILITAR
      'volcar misDatos a SecuenciaPlay.txt
       Dim k4 As Integer
          For k4=1 To NroEventoPista(Spis)
              Print #midiplano,MidiDatos(Spis).datos(k4)
          Next k4

      
       Close 20 ' cierra SecuemciaPlay.txt FIX ERROR 07-10-2025
EndIf  
'
'---------no va posicion=comienzo
'posishow=posicion + 20
'posishow=posicion - 20
'posicion=posicion -20
 
JSOLO=0:'---curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
 

'' Cplay=NO ' Control de play cancion si fue desde control


'''  reponer mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0

 SuenaTodo=0 ' habilita boton cotrol sonido de pistas
 
Sleep 10,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
/'
close_port(midiout)
out_free(midiout)
'/ 
If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Then 
Else
 SetGadgetstate(BTN_ROLL_EJECUTAR,0)
EndIf

Dim As Integer checkejec=0
For  iz As Short =1 To 32
      If CheckBox_GetCheck( cbxejec(iz))= 1  Or CheckBox_GetCheck( cbxgrab(iz))= 1 Then
          checkejec=1
      End If
      Exit For
Next iz
'if GrabarPenta=0 and GrabarEjec=HabilitaGrabar and Parar_De_Dibujar=NO And checkejec=0 Then 

      k1=pmTk(Spis).portout
'   '   Print #1,"midiout ",k1, *nombreOut(k1)
      alloff( pmTk(Spis).canalsalida,k1 )  
      allSoundoff( pmTk(Spis).canalsalida,k1 )  
'
'      ''Print #1,"desmarco ",*nombreOut(k1)
'      listoutAbierto(k1)=0
'      close_port midiout(k1)
'   '   out_free   midiout(k1)
'      listoutAbierto( k1) = 0
   
'EndIf 
  Parar_De_Dibujar=NO ' 05-03-2024
'  Cplay=NO
 
'       Sportsal=CInt(pmTk(Spis).portout) 
'       alloff(pmTk(Spis).canalsalida,Sportsal) 
'       allSoundoff( pmTk(Spis).canalsalida, Sportsal ) 
  

Sleep 20,1

Exit Sub

PlaySoloError:
 Dim errmsg As String

If  Err > 0 Then
  If i1=26 Then i1=25 EndIf

  errmsg = "FAIL PLAYUNOSOLO Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg ,"JSOLO ", JSOLO, "i1 ";i1

Print #1, "SolX.trk(JSOLO,i1).nota ", SolX.trk(JSOLO,i1).nota
Print #1, "SolX.trk(JSOLO,i1).dur ", SolX.trk(JSOLO,i1).dur
Print #1, " MaxPos  JSOLO  ", Smayor, JSOLO  

  FileFlush (-1)

'''''ThreadDetach(thread1)
  'Parar_De_Dibujar=NO ' 05-03-2024
 
EndIf

End Sub 


'-----------------------------------------
Sub FraccionarDur (Track() As sec,Roll As inst, indicePos As Integer, dura As ubyte,nR As Integer, ntk As integer)
' npo= notapiano, posi =posicion
' DADA una Notapiano selecionada con cursor o click
' entrar argumento de division con las figuras 2,3,4,5,6,7
' o sea blanca,negra,corchea,semicorchea, fusa , semifusa
' luego reemplazar la Nota elegida por N+ desplazar todo el vector hacia la derecha
' desde la posicion +1 desde la nota en la cantidad de posiocines necesarias para
' completar al duracion deseada. ej I=F+F+F+F O I=L+L, P=I+I,P=L+L+L+L
' ETC. DE ESE MODO DEJO INGRESAR NOTAS MAS CHICAS EN UN PASO DADO SIN MALGASTAR
' POSICIONES. podriamso hacer un vector tridimensional o sea en esa columna
'1) determinar la MAxPos de c/pista (moverpista lo hara automaticamente)
'2) determinar la columna donde esta la nota a expandirse en n fracciones
'  res la entrada parametro posi
'3) determinar n segun entrada de usuario fracciones de P,I,L,F,E,W
' en RollLoop coocar ctrl+alt+x un if con la cifra que se entra,,,
'4) DETERMIANR EN CUANTAS PARTES SE FRACCIONA LA NOTA NP (NPARTES)
' deducirla con lal duracion de la nota exsistente en la pista o roll y la 
'  cifra introducida por el usuario ej I (2),,,usuario:5 -> I fraccionada como F
' seran 4 F hace una I son 4 partes 1 la origial pas de I a F+ y las otras 3
' 2 F+ y una final F osea F+F+F+F  
' 5) RESTAR 1 A NP, NP-1.. (4 -1)
' 6) MOVER EN CADA PISTA NP-1 (3) POSICIONES A LA DERECHA DE LOS DATOS.
' 7) DETERMINAR LA NUEVA MAXPOS DE C/PISTA SUMANDOLE NP-1 (3)
' FIN
' USO: COMANDO CTRL+Z ANTES DE PULSAR EL COMANDO SE ESPERA UN NUMERO DE
' FIGURA A USAR 2,3,4,5,6,7,8=P,I,L,F,E,W,H, O SEA FRACCIONAR EN BLANCAS NEGRAS
' CORCHEAS, SEMI CORCHEAS, FUSA, SEMIFUSA,H, LOS TRESILLOS OCUPAN 2 FIGURAS MAS CHICAS
' ESO SE MANEJA CON LAS NOTAS INTRIDUCIDAS PERO LAS POSICIOENS SIEMRPE SON
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


'''Rolldur=Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur

'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  Dim D1 As Integer 
  moverZonaRoll(posinueva, Roll,posivieja, D1) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nsE usar
 ' nsE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
      If Roll.trk(posivieja,j).dur = Rolldur Then
           nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nsE)
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nsE)
            EndIf
        Next i
     EndIf 
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
If NombreCancion > "" Then
Else
 ntk=0
EndIf 
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
  Dim D1 As Integer
  moverZonaRoll(posinueva, Roll,posivieja,D1) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nsE usar
 ' nsE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nsE)
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nsE)
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
           nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nsE)
            
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
If NombreCancion> "" Then
Else
 ntk=0
EndIf 
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

RecalCompas ()
   
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


'''Rolldur=Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur
dim As Integer otroPesoDur, otronroPartes, indiceLigadasilencio, indiceNoLigadasilencio
'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  Dim D1 As Integer
  moverZonaRoll(posinueva, Roll,posivieja,D1) '16-11-2021
  print #1,"NB, NA", NB,NA
 ' si no anda nsE usar
 ' nsE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
  For j=NB To NA
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nsE)
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nsE)
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
           nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nsE)
            
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
If NombreCancion > "" Then
Else
ntk=0 
EndIf
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll

RecalCompas ()

EndIf
 
End Sub

Sub AutoFracTodoDur (Track() As sec,Roll As inst, indicePos As Integer, dura As ubyte,n1 As Integer, ntk As integer)
' Automaticamnete tomara la mayor y menor duracion y fraccionara por la menor, sea la menro existente
' o la menor incluida la entrada de usuario.
' Auto matizacion de FracTodoDur.Esto servira para ambos casos al estar en lectura y quiero
' abrir como antes o desde cursor al ingresar notas. En ambo scasos lo hará en mas de 2 notas
' recibira la menro y mayor oco antes determinado por la sub ya echa menoryMayorEncolumna
' lugo determino partes con la mayor, fracciono. Y toa nota menor a la mayor y mayor ala menro 
' o sea intermedia se contrastara con la menor y asi determinamos cuantas menores van en esa
' nota media se llenara con ellas y lo que falta ocmpletar de esa media y hasta las partes de
' la mayor se llenaran con silencios de la menor en cuestion
'---------- para poder usar en la rutina para pista completa sin pantalla usaremos nR para separar
' los casos,,,
Print #1,">>>> START AutoFracTodoDur "
   Dim As UByte menor, mayor
   Dim As Integer i1men,i1may,posdur
' en modo lectura la posicion de una nota en el vector es posdur, la 1er parte es en la pantalla
' la 2da posishow es en el vector hasta el inicio de la pantalla 

  If  n1 = 0 Then 
       posdur= (mousex- gap1 )/anchofig + posishow
       Print #1,"posdur ",posdur
       If posdur < 1 Then ' 08-03-2022 servira?
          Exit sub
       EndIf
   Else
Print #1,"--> posishow ", posishow
    If indicePos < NroCol*3/4  Then '04-02-2022
      posishow=  1 ''curpos ' decia 1
  ' valla tatlmente al inicio veremos si es aca jmgjmg
    Else
        posishow = indicePos - NroCol/2 '04-02-2022 
    EndIf
      posdur=indicePos -1 + posishow ' para midiplano 
   EndIf
 FileFlush (-1)
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
   indiceligada= numeroFrac + 90 ' indice de la menor o dur de usuario ligada correpsondiente
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


'''Rolldur=Roll.trk(indicePos,(12-nsE +(estoyEnOctava -1) * 13)).dur
dim As Integer otroPesoDur, otronroPartes, indiceLigadasilencio, indiceNoLigadasilencio
'---posi= posi + ndiv -1   
print #1,"MUEVO DESDE POSI ";posiVIEJA;" A POSInueva ";posinueva  
  pasoZona1=indicePos+1
  moverzona=1
  Dim D1 As Integer
  moverZonaRoll(posinueva, Roll,posivieja, D1) '16-11-2021

  print #1,"NB, NA", NB,NA
 ' si no anda nsE usar
 ' nsE = 11 -nR   +  (EstoyEnOctava -1 ) * 13 + 1 
 Dim As Integer conge=0, divi,Notapiano
 'conge=(EstoyEnOctava -1 ) * 13 + 1 
' -----------------------------------------------
' CASO DE MIDI PLANO EL FLAG ES n1=1 ...
' If  n1 = 1 Then 
'i1Mayor  indice en roll de la mayor debo p asarlo a Notapiano
'   Notapiano= i1May
'   Notapiano= Notapiano - restar (Notapiano)
'   EstoyEnOctava=SumarnR(Notapiano) +1  ' para la mayor  
'   Print #1, "Estoy En Octava midi plano ", EstoyEnOctava 
' End If
' EstoyEnOctava para el caso manual creo estaria mal tambien porque es solo una octava
' fija donde esta el mouse y en pantalla le hago barrer toda la columna luego 
' Estoy enOctava debe reemplazarse siemrpe  por la formula nueva que 
' dedudsca la octava mienras barre la columna !!!!
'--------------------------------------------
' y que pasa con los otros parametros de Roll, vel etc???
  For j=NB To NA    ' j es el vector de Roll para obtener Notapiano esta la formula 
     If Roll.trk(posivieja,j).dur >=1 And Roll.trk(posivieja,j).dur <=180 Then
        Select Case Roll.trk(posivieja,j).dur
        Case  dura ' la mayor
           ' If n1=0 Then
           '    nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1
           ' Else
               nsE = 11 -j +  SumarnR (j - restar(j) )  * 13 + 1  ' midiplano perfecto muesta el semitono
           ' EndIf
         For i= posivieja To posinueva -1
            If i < posinueva-1 Then
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceligada)
               Roll.trk(i,j).nota= CUByte(nsE)
               Roll.trk(i,j).vol = Roll.trk(posivieja,j).vol
               Roll.trk(i,j).pan = Roll.trk(posivieja,j).pan
               Roll.trk(i,j).pb = Roll.trk(posivieja,j).pb
               Roll.trk(i,j).inst = Roll.trk(posivieja,j).inst         
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indicenoligada)
               Roll.trk(i,j).nota= CUByte(nsE)
               Roll.trk(i,j).vol = Roll.trk(posivieja,j).vol
               Roll.trk(i,j).pan = Roll.trk(posivieja,j).pan
               Roll.trk(i,j).pb = Roll.trk(posivieja,j).pb
               Roll.trk(i,j).inst = Roll.trk(posivieja,j).inst         

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
        '   If n1=0  Then 
         '     nsE = 11 -j +  (EstoyEnOctava -1 ) * 13 + 1  ' estara bien esto???
         '  Else 
              nsE = 11 -j +  SumarnR (j - restar(j) ) * 13 + 1 ' midi plano perfecto da el semitono
         '  EndIf    
          Dim As Integer limparte = posivieja + otronroPartes -1
         For i= posivieja To  limparte ' son todos ligados porque despue ssigu eligado con silencio
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigada)
               Roll.trk(i,j).nota= CUByte(nsE)
              Roll.trk(i,j).vol = Roll.trk(posivieja,j).vol
               Roll.trk(i,j).pan = Roll.trk(posivieja,j).pan
               Roll.trk(i,j).pb = Roll.trk(posivieja,j).pb
               Roll.trk(i,j).inst = Roll.trk(posivieja,j).inst               
         Next i
         
         For i= limparte+1   To posinueva -1
            If i < posinueva -1 Then ' silencios ligados
'            print #1,"DENTRO 1) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte(indiceLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
              Roll.trk(i,j).vol = Roll.trk(posivieja,j).vol
               Roll.trk(i,j).pan = Roll.trk(posivieja,j).pan
               Roll.trk(i,j).pb = Roll.trk(posivieja,j).pb
               Roll.trk(i,j).inst = Roll.trk(posivieja,j).inst   
            Else
 '           print #1,"DENTRO 2) i, j,nsE ", i,j,nsE
               Roll.trk(i,j).dur = CUByte( indiceNoLigadasilencio)
               Roll.trk(i,j).nota= CUByte(nsE)
              Roll.trk(i,j).vol = Roll.trk(posivieja,j).vol
               Roll.trk(i,j).pan = Roll.trk(posivieja,j).pan
               Roll.trk(i,j).pb = Roll.trk(posivieja,j).pb
               Roll.trk(i,j).inst = Roll.trk(posivieja,j).inst   
            EndIf
            
        Next i
           
       End Select   
    EndIf 
     
  Next j
' tenemos Roll fraccionado ahroa debemos borrar Track(0) y copiar Roll en Track(0)
' o sea RollaTrack...al reves
If NombreCancion > "" Then
Else
ntk=0
EndIf 
cargaCancion=CARGAR_NO_PUEDE_DIBUJAR
ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
RollaTrack Track(), ntk,Roll
cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
ReCalCompas ()

EndIf

End Sub

Sub CargarSinRoll ()
 cargaCancion=NO_CARGAR_PUEDE_DIBUJAR ' para que no entre mas luego de cargada la cancion
   s5=2  '11-06-2022
   Erase mel_undo, undo_acorde, undo_kant_intervalos
   mel_undo_k=0: ig=0:cnt_acor=0
   ROLLCARGADO = FALSE
   
  ' print #1,"--TAB "
   nota=0
   dur=0
  ' print #1,"TAB 1- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos
   If clickpista=1 Then
  '   Print #1,"no incrementea ntk"
     clickpista=0
   Else
     ntk = ntk + 1
   EndIf
 '  print #1,"TAB 2- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos  
   If ntk > 32 Or ntk > tope Then
     ntk=1 
  '   print #1,">TAB 2A- 1- NTK,MAXPOS, pmtk(ntk).maxpos  ", ntk,maxpos,pmTK(ntk).maxpos     
   EndIf
   nombre= titulosTk(ntk)
   If nombre> "" Then
 '    print #1,"--------------------------"
 '    print #1,"TAB 3-NTK nombre", ntk,nombre
 '    print #1,"TAB 3-NTK MAXPOS pmtk(ntk).maxpos  ", maxpos,pmTK(ntk).maxpos
 '    print #1,"--------------------------"
   EndIf  
' evita leer track vacios   
   If nombre=""  Then ' evita revisar track vacios
     Do While nombre=""
        ntk=ntk+1
        If ntk>32 Or ntk > tope Then
           ntk=1
           nombre= titulosTk(ntk)
 ' print #1,"TAB 4 - NTK, pmtk(ntk).maxpos  ", ntk,pmTK(ntk).maxpos    
           Exit Do
        EndIf
 
        nombre= titulosTk(ntk)
     Loop
  EndIf
     posicion=0 ' 14.-03-2022
     MaxPos=pmTk(ntk).MaxPos
     posn=pmTk(ntk).posn
If posn=0 Then
   posn=pmTk(ntk).MaxPos -2
   pmTk(ntk).posn=posn    
EndIf

     desde=pmTk(ntk).desde
     hasta=pmTk(ntk).hasta
     NB=pmTk(ntk).NB
     NA=pmTk(ntk).NA
     If NA=0 Or NB= 0 Then
       NB => 0 + (pmTk(ntk).desde-1) * 13   
       NA => 11 + (pmTk(ntk).hasta-1) * 13
       NB=pmTk(ntk).NB
       NA=pmTk(ntk).NA
     EndIf
     portout=pmTk(ntk).portout 'solo debe servir para play de pista
     notaold = CInt(pmTk(ntk).notaold)
     CantTicks=pmTk(ntk).Ticks
     patchsal=pmTk(ntk).patch
' ajusto escala principal durante la conmutacion para cada track visualizado con TAB     
     notaescala_num_ini=CInt(pmTk(ntk).notaescala) '13-01-2022
     tipoescala_num_ini= CInt(pmTk(ntk).tipoescala) '13-01-2022
     cadenaes_inicial="" '13-01-2022
     armarescala cadenaes_inicial,tipoescala_num_ini,notaescala_num_ini,alteracion,1 '13-01-2022
' todavia no probado, escala principal para TAB en cada track testeat 13-01-2022     
' no he grabado las escalas auxiliares en lso Trackc todavia !! 13-01-2022 jjj     
  '   print #1,"TAB 5- MAXPOS final TAB " ,maxpos
     
      
'

 '  print #1, "TAB 6-NTK nombre", ntk,nombre  
 '  print #1, "TAB 6-NTK ntk,MAXPOS, pmtk(ntk).maxpos  ", ntk, MaxPos,pmTK(ntk).maxpos
' copia track a Roll en memoria  
' el segundo parametro es canal no se usa...lo saco o lo dejo?
   Tracks (ntk , 1,Roll) ' track , nro,  Canal, copia track a Roll en memoria
   Sleep 100

End Sub 