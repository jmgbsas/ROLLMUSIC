 On Error Goto errorsub
'#Include "foro/window9.bi" no hace falta parece

'extern "c++"
'#include once "fltk/fl_window.bi"
'#include once "fltk/fl_button.bi"
'#include once "fltk/fl_check_button.bi"
'#include once "fltk/fl_input.bi"
'#include once "fltk/Fl_Multiline_Output.bi"
'#include once "fltk/Fl_Radio_Button.bi"
' #include once "fltk/Fl_Menu_Window.bi"
'''#include once "fltk/Fl_Table.bi" ' no funca
' '#include once "fltk/fl_Tree_Item.bi" no funca
''#include once "fltk/Fl_Table_Row.bi" ' no funca
'''#include once "fltk/Fl_Choice.bi"  ' no funca
'''#Include once "fltk/fl_Input_choice.bi"   '' NO FUNCA   

'end extern

'''On  Error GoTo errorSub
#Include Once "rollutil.bi"

Sub ArmarDurFrac ()
' los bloques de durcion se repiten de a 3, el incr es por bloque
' si voy al 2do bloque de 3 el incrdeja de ser 0 y pasa a 27 respectro de la
' 1er linea l cargo TRAck como 2da linea
Print #1,"armardurFrac procesa numeroFrac"
FileFlush (-1)
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
   numeroFrac = DUR 'era duracion
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
EndIf
' CUART   
If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    numeroFrac = DUR + 9 'era duracion
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
EndIf
' PUNTILLO   
' 3I* = I = 1 , el puntillo a un 3 suma dando la figura de la q proviene.   
If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    numeroFrac = DUR + 18 'era dur
EndIf
'DOBLE PUNTILLO   
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=0 Then 
   numeroFrac = DUR + 27
EndIf
' SEGUIR  JMG
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=0 Then 
   numeroFrac = DUR + 36
EndIf   
' -----fin 1er bloque ------------------------

If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   numeroFrac = DUR + 45 'era dur
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   numeroFrac = DUR + 54
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=0 Then 
   numeroFrac = DUR + 63
EndIf
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=0 Then 
   numeroFrac = DUR + 72
EndIf

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=0 Then 
   numeroFrac = DUR + 81
EndIf

' -- fin 2do bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    numeroFrac = DUR + 90
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    numeroFrac = DUR + 99
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    numeroFrac = DUR + 108
EndIf

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=1 Then 
   numeroFrac = DUR + 117
EndIf
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=1 Then 
   numeroFrac = DUR + 126
EndIf
'----- fin 3er bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   numeroFrac = DUR + 135
EndIf   

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   numeroFrac = DUR + 144
EndIf   

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
   numeroFrac = DUR + 153
EndIf   

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=1 Then 
   numeroFrac = DUR + 162
EndIf   

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=1 Then 
   numeroFrac = DUR + 171
EndIf   
' ---fin 4to bloque -
   cuart=0:pun=0:doblepun=0:tres=0:silen=0:mas=0

Print #1,"armardurFrac  numeroFrac Resultante ", numeroFrac
FileFlush (-1)
End Sub


Sub RecalCompas() ' Nuevo para ticks
 On Local Error GoTo errorCalc 
' velocidades para 4/4 reposicion par ticks
' lso tiempos de una comaps son 96, 192, 288, 384
clickpista=0
 Dim As Integer i,j,k,negra=96, multi,kj,kp, cntcompas,cntnegra,com, compas4cuartos= 4*negra,MAXI

 If CANCIONCARGADA=TRUE Then
    MAXI=MaxPosTope
 Else
    MAXI=Maxpos 
 EndIf
Print #1,"En Recalcompas MAXI ",MAXI
fileflush(-1)
Sleep 50
 ReDim compas(1 To 2*MAXI)
Sleep 50
MaxPos=MAXI
If CantTicks < MAXI Then
  CantTicks=MAXI
EndIf
  compas(1).Posi=0
  compas(1).nro = 0
For j=1 To MAXI-2
  kj=kj+1
  Select Case kj
    Case   1 To  96
   compas(j).nro = -1
   compas(j).Posi=j
    Case  97 To  192 
   compas(j).nro = -2
   compas(j).Posi=j
    Case 193  To 288 
   compas(j).nro = -3
   compas(j).Posi=j
    Case 289 To 383
   compas(j).nro = -4
   compas(j).Posi=j
    Case compas4cuartos '384
     cntcompas=cntcompas+1
  compas(j).Posi=j
  compas(j).nro = cntcompas
      KJ=0
  End Select

 ' kp=kp+1
 ' Select Case kp
 '   Case negra
 '    cntnegra=cntnegra+1
 ' compas(j).Posi=j
 ' compas(j).nro = cntnegra
 '     kp=0
 ' End Select


Next j

Print #1,"fin RecalCompas ok "
fileflush(-1)
Sleep 5

Exit Sub 
errorCalc:
Dim As long er1
er1 = Err()  
If er1 > 0 Then
Print #1,"Error ReCalcompas detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po

Print #1,"---------------err calccompas---------------------"

Print #1,"ERROR = ";ProgError(er1); " on line ";Erl
Print #1,"Error Function: "; *Erfn()
fileflush(-1)

EndIf

 
 
End Sub
' ======================================================================

Sub calcCompasOLD( ByRef j As Integer, Roll As inst)
 ' nota:fijarse que pasa con el grupo..1 to 4 de las 64 duraciones
 ' x print #1,"========================================================="
 ' x print #1,"1-calcCompas j o posn RECIBIDA: "; j
' 10-02-2025 MARCAMOS LAS POSICIONES QUE NO TIENEN NOTAS PARA NO VIUSALIZARLAS SI SE DESEA
' 06-03-2025 ticks esbozo de modificacion ver y probar,,,falta muchas pruebsa y
' modificaciones...
On Local Error GoTo errorCalc 

 Dim As Integer k, i,cantFalta,CantSobra, tengo, falta,sobra, noct, posnoff



 If DUR=0  Then ''And j >0 Then '23-03-2022 ï¿½ï¿½ï¿½
  compas(j).Posi=0
  compas(j).nro = 0

  ' x print #1,"CALCOMPAS-0 DUR=0 COMPAS(J)=0 "
  Exit Sub
 EndIf
 'If DUR=181 Then
 ' Compas(j).Posi=0
 ' Compas(j).nro = 0

 ' ' x print #1,"CALCOMPAS-0 DUR=181 COMPAS(J)=0 "
 ' Exit Sub
 'EndIf

' esto nunca se ejecuta porque j es 4 no un caentr con 1!!
 If j= 1 Then ' 128 es el maximo de ticks de un compas se agregan amspara terminared analizar elcompas
  'Erase (buffCompas_1) no usados pro ahor no son necesarios....
  'Erase (buffCompas_2)
  acumulado=0
  ''erase (compas) 'cada item es la posicion en donde
  ' x print #1,"J=1 comienzo calcCompas "
  jc=j
 EndIf
 ' x print #1,"1-calcCompas 2"
 If jc > 128 Then
  id=1
  ' procesar el resto que sobra luego de 129 en el buffer 2
 EndIf
 ' ---------------------------------------------
 '11-12-2021 redusco la camtidad de partes a 20
 '---------------------------------------------
 Dim As Integer partes_falta (1 To 20), partes_sobra(1 To 20) 
 ' x print #1,"1-calcCompas 3, DUR: ";DUR; " j: "; j; " figura: ";figura(DUR)
  
 For k=1 To 180
  If DUR=k   Then
   acumulado =acumulado +pesoDur(k)
   ' x print #1,"acumulado / k / PEsoDur "; acumulado, k , pesoDur(k);" figura ";figura(DUR)
   If Abs(d7 -acumulado) < 10 Then
      acumulado = d7
   EndIf
   If acumulado = d7 Then
    Compas(j).Posi = j
    nroCompas = nroCompas+1
    Compas(j).nro = nroCompas
    ' x print #1,"acumulado ";acumulado
    ' x print #1,"Compas(j) , j: ";Compas(j).Posi;" "; j

    acumulado=0
    jc=1
    ' x print #1,"1-calcCompas 3.1 exit sub acumulado=0 ";acumulado
    Exit Sub
   EndIf

   If acumulado < d7  And j > 0 Then
    ' x print #1, "j que hace cancelar ";j
    Compas(j).Posi = 0   
    Compas(j).nro = 0 ' reseteo
    velocidades(j)
'Print #1,"calcompas j Compas(j).nro velpos "; j , Compas(j).nro  
    ' x print #1,"1-calcCompas 3.2 acumulado menor sigue exit sub, carga: ";carga
    Exit Sub
   EndIf
   ' cuando cargamos desde disco aca nunca es necesario pasar pues
   ' ya esta todo fraccionado para tener compases completos sin notas
   '  que sobren o falten
   If carga=1 Then ' se supone que nunca carga=1 se usara en el resto
    'posn=j
    Exit Sub
   EndIf

   Dim z1 As Integer
   ' x print #1,"1-calcCompas acumulado 3.3 >d7  ";acumulado; " carga: ";carga
   If acumulado > d7 And carga =0 Then
    ' x print #1,"acumulado > d7 :";acumulado
    tengo= acumulado - pesoDUR(DUR)
    ' x print #1,"tengo "; tengo
    falta=d7 - tengo
    ' x print #1,"falta ";falta
    sobra=acumulado -d7
    ' x print #1,"sobra "; sobra
    '       Dim ultimafigura As Integer=0
    separarDur(j,DUR, partes_falta(),cantFalta,falta)
    ' x print #1,"||||| CantFalta ";CantFalta
    If CantFalta > 100 Then
     ' x print #1, "ERROR CANTFALTA > 100 ";CantFalta
     Exit Sub
    EndIf
    For z1 = 1 To CantFalta
     ' x print #1, " ";partes_falta(z1);" ";figura(partes_falta(z1));
    Next z1
    ' x print #1,
    separarDur(j,DUR, partes_sobra(),cantSobra,sobra)
    ' x print #1,"||||| CantSobra ";CantSobra
    If CantSobra > 100 Then
     ' x print #1, "ERROR CANTSOBRA > 100 ";CantSobra
     Exit Sub
    EndIf

    For z1 = 1 To CantSobra
     ' x print #1, " ";partes_sobra(z1);" "; figura(partes_sobra(z1));
    Next z1
    ' x print #1,
    ' x print #1,"HAGO  DUR=0 BUENO.pero el sobra va a acum.."
    'preparo el acumulado de Falta ntes de reemplazar
  '  acumulado=pesoDur(partes_sobra(CantSobra))  'jmg 30-3-21
  '  ' x print #1,"acumulado sobra ";acumulado 
   EndIf
   ' x print #1,"1-calcCompas 3.4"
   '  EndIf
   Exit For
  EndIf
 Next k
 ' x print #1,"1-calcCompas 4  ingreso a REEMPLAZO EN ROLL de las durciones y notas"

 acumulado=tengo
 ' la ultima nota a cortar se ha cargado es la quese reemplaza de este modo
 ' el codigo nucleo no se toca para nada solo se copia las partes necesarias
 ' para imitar su comportamiento...el programa introduce auomaticamente las
 ' figuras de reemplazo y haria el corte ....
 ' valido en la carga manula pero no desde disco
 ' en disco esta ya todo procesado solo debo sumarduraciones
 ' aradeterminar compas. En Nucleo se busc siemrep la not que este en 0
 ' ergo parareemplazardebo borrar el contenido o reemplazarlo
 If CantFalta > 0 And carga= 0 Then  ' empezamos el reemplazo en el vector desde posn
  ' x print #1,"Recordar posn empieza en 0 "
  ' x print #1,"REEMPLAZO Cantfalta ";CantFalta
  For k=1 To CantFalta '
   If partes_falta(k)>= 1 And partes_falta(k) <=90 Then
    partes_falta(k)=partes_falta(k) + 90 ' comienzan los + ligados
   EndIf


   ' x print #1,"k,partes_falta(k), FIGURA ", k, partes_falta(k), figura(partes_falta(k))
   ' x print #1,"POSICION reemplazada: ";posn
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = partes_falta(k)
      
   If carga=0 Then ''no hce nada en la carga
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = notaold
   EndIf

   posnoff=posn+ DurXTick(partes_falta(k)) -1 '06-03-2025 ticks
'raro aca debo poner laprimera con onoff=2 y ligada y el resto ligada hasta el final
' unasimple sin ligar que puede estar antes odespues de la separacion del compas,,,uff
   If k=1 Then
     Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).dur = 0 ' esto seria para la ultima parte 
     Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).onoff = 0 ' esto seria para la ultima parte

     Roll.trk(posnoff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 181 ' esto seria para la ultima parte

   EndIf
   If k=CantFalta Then
     Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).dur = 183 ' esto seria para la ultima parte 
     Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).onoff = 1 ' esto seria para la ultima parte

     Roll.trk(posnoff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182 ' esto seria para la ultima parte
   EndIf 
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk(posn,(notaOld-1 +(estoyEnOctava -1) * 13)).dur = 181
   EndIf
'------zzz
   For noct = desde To hasta -1 '01-02-2022
     For i= 0 To 11 ' gracias a esto anda acordesï¿½?
       If i= 12 -nota And noct = estoyEnOctava Then
         Continue For
       Else    
       
         If Roll.trk(posn,(i +(noct -1) * 13)).nota = 0 Then
         '   ' x print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk(posn,(i +(noct -1) * 13)).nota = 181
            Roll.trk(posn,(i +(noct -1) * 13)).dur  = 0
         EndIf
       EndIf
     Next i
   Next noct
'-------   
   acumulado=acumulado+pesoDur(partes_falta(k))
   If Abs(d7 -acumulado) <= 10 Then
       acumulado = d7
   EndIf
   If acumulado = d7 Then
      compas(posn).Posi = posn
      nroCompas=nroCompas+1
      Compas(posn).nro = nroCompas

      ' x print #1, "Hay compas en posn ";posn
      acumulado=0
   posn=posnoff+1
   posicion = posn
   MaxPos= Posicion
 
   ' x print #1,"acumulado falta: ";acumulado; " posn: ";posn
   
   ' x print #1,"d7 ";d7

      Exit For 
   Else
      compas(posn).Posi= 0
      Compas(posn).nro = -2
   
      ' x print #1,"No Hay compas todavia en posn ";posn
   EndIf

   posn=posnoff+1
   posicion = posn
   MaxPos= Posicion 

   ' x print #1,"acumulado falta: ";acumulado; " posn: ";posn
   ' x print #1,"d7 ";d7


  Next k
  
  End If

CantFalta=0
acumulado=0 ' se supone termino el compas sino habra que forzarlo.... 
 ' x print #1,"1-calcCompas 5 carga: ";carga
 If cantSobra > 0 And carga=0 Then
  ' x print #1,"REEMPLAZO CantSobra ";CantSobra
  For k=1 To CantSobra
   If k < CantSobra Then ' le agrega >, continuacion
      ' x print #1,"k ,partes_sobra(k),figura ";k;" ";partes_sobra(k),figura(partes_sobra(k))
      ' x print #1,"posicion reemplazada: ";posn
      If partes_sobra(k)>= 1 And partes_sobra(k) <=90 Then
         partes_sobra(k)=partes_sobra(k) + 90 ' empieza ligados +
      EndIf
   EndIf
   Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).dur = partes_sobra(k)
   posnoff=posn+DurXTick(partes_sobra(k)) -1
   If carga=0 Then
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = notaold
   EndIf
   Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).onoff = 0
   Roll.trk(posnoff,(12-nota +(estoyEnOctava -1) * 13)).dur = 183
   Roll.trk(posnoff+6,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk(posn,(notaOld-1 +(estoyEnOctava -1) * 13)).dur = 181

   EndIf
'------zzz

   For noct = desde To hasta -1 ' 01-02-2022
     For i= 0 To 11 ' gracias a esto anda acordesï¿½?
       If i= 12-nota And noct = estoyEnOctava Then
         Continue For
       Else    
       
         If Roll.trk(posn,(i +(noct -1) * 13)).nota = 0 Then
         '   ' x print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
            Roll.trk(posn,(i +(noct -1) * 13)).nota = 181
            Roll.trk(posn,(i +(noct -1) * 13)).dur  = 0
         EndIf
       EndIf
     Next i
   Next noct
'-------   
   
   
   ' x print #1,"ERROR==> ACUMULADO: " ;acumulado
   ' x print #1, "vlor buscdo de k: ";k
   ' x print #1,"partes_sobra(k) ";partes_sobra(k)
   acumulado=acumulado + pesoDur(partes_sobra(k)) ' para el siguiente compas..ï¿½?.
   ' x print #1,"ERROR==> ACUMULADO: " ;acumulado
   If k= CantSobra Then
    CantSobra=0
    Exit For
   EndIf
   posn=Posn+1 'jmg 30-03-21
   posicion = posn
   MaxPos= Posicion

  Next k
  ' x print #1,"1-calcCompas 6"
  
 EndIf
 cantSobra=0
 
 Erase partes_falta
 Erase partes_sobra
  ' fin compases
 ' x print #1,"1-calcCompas 7 carga: ";carga
' error
Exit Sub 
errorCalc:
Dim As long er1
er1 = Err()  
Print #1, "er1 sos boludo o te hafces freebasic "; er1 
If er1 > 0 Then
Print #1,"Error Calc detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po

'Print #1,"*po ";*po;" semitono ";i
'Print #1,Erl, Erfn,Ermn,Err
Print #1,"---------------err calccompas---------------------"
'ErrorNumber1 = Err
'ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


Print #1,"ERROR = ";ProgError(er1); " on line ";Erl
Print #1,"Error Function: "; *Erfn()
'ers= 11 -i  +(*po) * 13
'Print #1, "11 - semitono  +(*po) * 13)  "; ers; "semitono  ";i
'Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf

 
 
End Sub
' ======================================================================
Sub separarDur(ByRef j As Integer, ByRef DUR As UByte,partes() As Integer, ByRef cantPArtes As Integer, ByRef falta As Integer)

On Local Error GoTo errorSepara

 ' S print #1,"1-Separadur "
 Dim As Integer lb,lu,k,tengo,sobra, falta1,acumx
 ' S print #1,"EN SEPARADUR-------------------------------------------------"
 lb=LBound(partes)
 lu=UBound(partes)
 'dim integer ultimafigura= partes(CantPartes)

 falta1=falta
 ' S print #1,"FALTA RECIBIDO: ";FALTA1
 ' S print #1,;" DUR recibido: ";DUR;
 Dim As Integer desder, hastar

 ' S print #1,"pun: ";pun;" silen: ";silen;" mas: ";mas; " tres: ";tres
 ' S print #1,"incr: ";incr

 ' cuando hago la carga los k son de 1 a 64 no son mas de 1 a 8
        Select Case DUR
         Case  1   To 45
          incr=0
         Case  46  To  90
          incr=45
         Case  91  To  135
          incr=90
         Case  136  To  180
          incr=135
        End Select

 desder =  1 + incr
 hastar = 45 + incr

 For k=desder To hastar
  If (falta >= pesoDur(K) - 10) And  (falta <= pesoDur(K) +10)    Then
   partes(lb)=k
   Compas(j).Posi=j ' hay compas ESTE PRODUCI DOBLES !!!! JMG SI
   nroCompas = nroCompas+1
   Compas(j).nro = nroCompas

   ' S print #1,"falta encontrado DUR ";k;" figura:";figura(k); " parte:";lb
   CantPartes =lb
   acumulado=0
   falta=0
   falta1=0
   Exit Sub
  Else
   Compas(j).nro = 0 ' reseteo
   velocidades(j)
  EndIf
 Next k
 ' si sigue es porque fallï¿½ no hay ninguna figura con esa duracion
 lb=lb-1
 acumx=0
 ' S print #1,"SEGUIMOS NO SE ENCONTRO FALTA  POSN J "; J
 ' S print #1,"absoluto de falta1 original "; falta1
 falta1=Abs(falta1)
 
 'If falta1 >= 15000000 Then

 ' ' S print #1,"Caso de falla falta1 > 15000000 revisar codigo"
 ' Exit Sub

 'Else
 'CAMBIO DE ORDEN TENIA 10 A 18, 19 A 27, 28 A 36, 1 A 9
Dim CUENTA As Integer 
 If k<=180 And lb=0  Then ' no se encontro
  Do
  CUENTA += 1
   For k=28+incr To 36+incr 
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k

   For k=19+incr To 27+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k


   For k=10+incr To 18+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >9a16 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =9a16 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k

  
   
   For k=1+incr To 9+incr
    If  falta1 > pesoDur(k)  Then
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >1A8 ";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If

    If (falta1 >= pesoDur(k) - 10) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO = 1a8";lb;" figura:";figura(k); " parte:";lb
     Exit For
    EndIf
   Next k
   
   For k=37+incr To 45+incr ' tresillos
    If  falta1 > pesoDur(k)  Then
     lb += 1
     ' S print #1,"dur k incr";k,incr
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb)=k
     ' S print #1,"FALTA1 ENCONTRO PEDAZO >19a27 PArte;";lb; " figura:";figura(k); " parte:";lb
     falta1=falta1-pesoDur(k)
     acumx=pesoDur(k)
     Exit For
    End If
    If (falta1 >= pesoDur(k) -10 ) And (falta1 <= pesoDur(k) + 10) Then ' si elproximo es la mitad buscodeneuvo
     lb += 1
     If lb > lu Then
      Exit Do
     EndIf
     partes(lb) = k
     acumx = acumx + pesoDur(k)
     ' S print #1,"FALTA1 ENCONTRO PEDAZO =19a27 ";lb; " figura:";figura(k); " parte:";lb
     Exit For
    EndIf

   Next k
   
   If acumx=falta Or lb > lu Then
    ' S print #1,"FALTA1 FIN EN ";lb; " figura "; figura(K) ;" ACUMX";acumx
    Exit Do
   EndIf
   If falta1 < 40000 Then
    ' S print #1,">> falta1 es H/2..NO DEBERIA DAR ESTO AHORA ";falta1
    Exit Do
   EndIf
   If CUENTA = 100 Then
      ' S print "FALTA: ";falta;" Falta1: "; falta1
      Close 0
      Exit Do
   EndIf   
  Loop
  ' S print #1,"ACUMX SEPARADUR ";acumx
 EndIf

 'EndIf

 CantPartes =lb

 ' S print #1, "CANT PARTES LB, CantPartes "; LB;" "; CantPartes
 ' S print #1,"ACUMX  ";ACUMX
'' error local
Exit Sub 

errorSepara:
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
er1 = Err
If er1 > 0 Then
Print #1,"Error Separa detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po-1 ";*po-1;" *po+1 ";*po+1
Dim As Integer valor1, valor2


Print #1,"*po ";*po 
Print #1,Erl, Erfn,Ermn,Err
Print #1,"------------------------------------"
ErrorNumber1 = Err()
ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


Print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
Print #1,"Error Function: "; *Erfn()
'ers= 11 -semitono +(*po) * 13


Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf


End Sub

Sub mayorDurEnUnaPosicion (ByRef posn As Integer)
 Print #1,"1-MayorDurEnUnaPosicion  "
 Dim As Integer i, j,mayor,ia ,valdur
 mayor=1
 j = posn
 For i= NB To NA -13
  valdur => Roll.trk(j,i).dur
  If  valdur >= 1 And valdur <=181 Then
    Print #1, "DUR mayor ";DUR, mayor
   If pesoDUR(valdur) > mayor  Then ' las duraciones cuando mas
    mayor=pesoDur(valdur)
    ' print #1, " DUR MAYOR";DUR,mayor
   EndIf
   If i=NA -13 Then

    For ia=1 To 180
     If mayor=pesoDur(ia) Then
      valdur=ia
     EndIf
    Next ia
   ' print #1,"          J, valdur Mayor: "; J, valdur
    DUR=valdur
'    calcCompas(j, Roll)
'    print #1,"ABRIR compas(j) ";compas(j).Posi; " j :"; j

   EndIf
  EndIf
 Next i
 
 ' fin compases

End Sub

'-----------
' ------
Sub menoryMayorEnColumna (Roll As inst, ejex As Integer, ByRef menor As UByte, ByRef mayor As UByte,ByRef i1men As Integer, ByRef i1may As Integer)
' estoy en Roll el cual es ï¿½nico tengo entonces definido las globales NB y NA por default
' recorro la columna
Print #1,"ENTRA menoryMayorEnColumna ejex NB NA "; ejex ; " " ; NB;" "; NA
If  ejex = 0 Then
  Print #1,"ejex es cero no se procesa" 
    Exit Sub
Else
  Print #1,"ejex  posdur "; ejex
End If 
Dim As Integer i1,j1=0, DurMen,DurMay

  For i1 = NB To NA -13 ' 01-02-2022
FileFlush (-1)
    If Roll.trk(ejex,i1).nota >=1 And Roll.trk(ejex,i1).nota <=12 Then 
     If Roll.trk(ejex,i1).dur >=1 And Roll.trk(ejex,i1).dur <= 180 Then
        Print #1,"=> VEO DATOS i1,nota,dur ", i1, Roll.trk(ejex,i1).nota,Roll.trk(ejex,i1).dur
        j1=j1+1
        If j1=1 Then
           menor=Roll.trk(ejex,i1).dur
           Print #1,"menor inicial ", menor
           DurMen = pesoDur(menor)
           Print #1,"DurMen inicial ", DurMen
           mayor=Roll.trk(ejex,i1).dur
           Print #1,"mayor inicial ", mayor
           DurMay = pesoDur(mayor)
           Print #1,"DurMay inicial ", DurMay
           i1may=i1
           i1men=i1
           
        EndIf   
        If j1 > 1 Then
           If DurMen > pesoDur(Roll.trk(ejex,i1).dur) Then
              DurMen = pesoDur(Roll.trk(ejex,i1).dur)
              Print #1,"DurMen sig ", DurMen
              menor=Roll.trk(ejex,i1).dur
              Print #1,"menor sig ", menor
              i1men=i1
           EndIf 
           If DurMay < pesoDur(Roll.trk(ejex,i1).dur) Then
              DurMay = pesoDur(Roll.trk(ejex,i1).dur)
              Print #1,"DurMmay sig ", DurMay              
              mayor=Roll.trk(ejex,i1).dur
              Print #1,"mayor sig ", mayor
             i1may=i1
           EndIf
          
        EndIf
    EndIf  
    EndIf      
  Next i1

Print #1,"encontro menor y mayor ", menor, mayor, "En columna ",ejex
Dim As Integer notapMay, notapMen
notapMay=i1may -restar(i1may)
notapMen=i1men -restar(i1men)
Print #1,"Notapiano menor,mayor ", notapMen, notapMay
i1may=notapMay
i1men=notapMen 
' ejex es fijo es al columna en analis y origen del  fraccionamiento
  FileFlush (-1)
End Sub

Function BuscarNota (ctrl As Integer, ByRef cur1 As Integer,ncur As Integer) As Integer ' en cursor cambiadur u otras el programa se posicionara en la nota mas cercana
'SOLO SIRVE PARA APLICAR EN CURSOR COMEDIT=TRUE, HBRIA QUE HACER OTRA PARA LECTURA
' Y FUERA DE CURSOR SI HACE FALTA POR AHORA NO
' hace falta para acordes en lectura no le doy pie con bola todavia!!!! 19-07-2025
' decidiremos cuanotos ticks de acercamiento en un principio 10
' posn1=n+curpos  es x , notacur es y, barreemos un cuadrado ¡ 1 1 semitono y + - 10 curpos
' BUSCA OFF 1 O 2 y 190
Print #1,"ENTRA A BISCANOTA CUR1 "; cur1; " ncur "; ncur
If ctrl= 0 Then ' ctrl es cambiadur solo procesa con 1 para evitar otras modificaciones se ajusta en cursor()
  Exit Function
EndIf
Dim As Integer x,x1, x2, y, y1, y2,posn1

'Dim As String s1,s2,s3,s4,s5,s6,s7,s8,s10

x1=cur1 - 20
x2=cur1 + 20

y1=ncur - 1
y2=ncur + 1
If y1 < 1 Then y1=1 EndIf
If y2 > 12 Then y2=12 EndIf
If x1 < 1 Then x1=1 EndIf
If x2 > NroCol Then x2=NroCol EndIf

'Print #1," x1 x2 "; x1, x2
'Print #1," y1 y2 "; y1, y2
For x= x1 To x2  'curpos
 For y=y1 To y2  ' notacur
    posn1 = posicion+ x 'este deberia ser siempre ¿?
'Print #1,"POSN1 ";posn1
'Print #1,"POSISHOW  ";posishow
'Print #1,"POSIcion  ";posicion
'Print #1,"curpos  ";curpos
'Print #1,"----------------------------------------"
'     Print #1," x posn1 "; x, posn1; " estoyEnOctava ";estoyEnOctava; " y ";y
         
      ''' Roll.trk(posn1,(12-y +(estoyEnOctava -1) * 13)).onoff = 1 Or _    
    If Roll.trk(posn1,(12-y +(estoyEnOctava -1) * 13)).onoff = 2  Or _
        Roll.trk(posn1,(12-y +(estoyEnOctava -1) * 13)).dur = 190   Then
' encontro ubicacion de nota ,o final sec o un 190 borrar
'        Print #1,"encontro onoff posn1 "; Roll.trk(posn1,(12-y +(estoyEnOctava -1) * 13)).onoff, posn1
     cur1=posn1-posicion
     ncur=y
    

 'Print #1,"ENCONTRAMOS ON 2 EN CURPOS, NOTACUR "; cur1, ncur
    BuscarNota=0 ' ok
    Exit Function 
    EndIf
 Next y
Next x
 'Print #1,"NO ENCONTRAMOS UNA MIERDA "

 'FileFlush(-1)
 BuscarNota=1 ' error
 trasponer=0
End Function

''========================= C U R S O R ============
Sub cursor(c As cairo_t Ptr,ByRef n As Integer, ByRef nro As Integer, Roll As inst)
'trasponer=0 ' NO SEPORQUE MIERDA SE PONE EN 1
' n deberia ser posishow 
 'print #1,"1-cursor "
trasponer=0
 cairo_set_source_rgba c, 1, 1, 0.2, 1
 Dim Wc As Integer
On Local Error GoTo errorhand 
Dim As Integer posnarranca
Dim  As Integer nroPartesNota,verticalEnOctavaVacia
    
 'If play=1 Or playb=1 Then
     wc=anchofig '31-01-2022 23-02 tapaba la figura
 'Else
 '    wc=inc_penta
 'EndIf 
 'cairo_set_line_width(c, 1) 'descomentado en la 02
 'EL CURSOR PARA UNA linea dada o nota se coloca inc_Penta mas abajo
 ' para B 38.5 27 notaold=1  <- actualizar please
 ' para C 38.5 25 notold=12
 'n esla posicion al cargar datos deberia llamar a curor creo,,,probando
 'curpos es la posicion relativa horizontal del cursor cuando el barrido esta
 ' detenido, es una foto del vector Roll inmovilizado horizontalmente, pero
 ' puedo navegar con cursor.,
 'If s8=1 Then
 '   s8=0
 '   'curpos=(mousex- gap1 )/35 ' 01-07-2021
 '   curpos=curposold
 'EndIf
 'curposold=curpos 
 '''''''curpos= posishow +algo -posishow
'---------------CTRL-N ..................
'ctrl-n: usaremos el mismo codigo de ctrl-m solo que la nota se entra por nombre la X que es cambiadur=1 se
' establece desde el principio no hace falta pulsarla
 If  COMEDIT=SOLO_MODIFICACION  And agregarNota=1  And nota > 0 Then 'ctrl-n ch cambiado hoy
  If MouseButtons And 1 Then '''YA PROBADO
     If curposClickDerecho >0 Then
        curpos=curposClickDerecho
     Else  
        curpos= Int((mousex - gap1)/anchofig) '27-07-2025
     EndIf
     nota=nsE 
  EndIf

  notacur=nota 
  octavaEdicion=estoyEnOctava ' modifico en cualquier octava
  cambiadur=1

 EndIf

 If COMEDIT=SOLO_MODIFICACION Or COMEDIT=MODIFICACION_INSERCION Then  ' vertical ctrl-m o Ctrl-N ch
  trasponer=0
    If MouseButtons And 1 Then
      curpos= Int((mousex - gap1)/anchofig) '27-07-2025
      notacur=nsE
    EndIf
  cairo_move_to  (c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
  cairo_rectangle(c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  ,wc*6 ,inc_Penta) '13-07-2021 inc_penta/2 por inc_penta
  cairo_move_to  (c, gap1 + (curpos)*anchofig , Penta_y  )
  cairo_line_to  (c, gap1 + (curpos)*anchofig , Penta_y + (notacur-1) * inc_Penta  +inc_Penta )
 
 
  RollDur=Roll.trk(n + curpos,12 - notacur + (*po-1) * 13 ).dur
  
  RollNota=Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota
'' para ver el estado del onoff en la posicion elegida notacur
     onoff=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff)
   ''' verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde
   ''  acordeNro=CInt(Roll.trk(n,verticalEnOctavaVacia ).dur) 'acordenro
' este pb es de la nota en si , el otro pb esta en verticalEnOctava el 201 y 202
' pero puedo usar este o unificar
    acordeNro=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).pb) 'acordenro
    notaold=notacur 

 EndIf
 If COMEDIT=LECTURA Or COMEDIT=ENTRADA_NOTAS  Then ' lectura y comedit=true
    If MouseButtons And 1 Then
      curpos= Int((mousex - gap1)/anchofig) '27-07-2025
      notacur=nsE
    EndIf
  
  If notacur=1 Then
   notaOld=notacur
  EndIf
  cairo_move_to  (c,gap1 + (curpos)*anchofig , Penta_y + (notaOld-1) * inc_Penta  )
  cairo_rectangle(c,gap1 + (curpos)*anchofig , Penta_y + (notaOld-1) * inc_Penta  , wc/4,inc_Penta)

 EndIf
 If COMEDIT=MODIFICACION_INSERCION Or COMEDIT=SOLO_MODIFICACION Then 'ctrl-m y ctrl-n
     If MouseButtons And 1 Then 
       curpos= Int((mousex - gap1)/anchofig) '27-07-2025
       notacur=nsE
       notaOld=nsE
     EndIf
  cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
  cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)

 EndIf
'-----------------------
' BORRAR CON DELETE: (EN VEZ DE 0)
'------------------------
 If (COMEDIT=SOLO_MODIFICACION Or COMEDIT=MODIFICACION_INSERCION) And borrar= 1  Then 'ctrl-M selecion  de nota con flechas y posicion
' HABILITAR NOTA AL PONER EN CERO SE PUEDE USAR EN EDIT COMUN
' ch lo habilitamos para control-n tambien
' OJO NO BORRA EL ONOFF=1!!! PONERLO
trasponer=0
  cairo_set_source_rgba c, 1, 0, 0.2, 1
  
  Dim dursaved As Integer
  If notacur=1 Then ' 1 POR 0 JMG
   notacur=notaOld 
  EndIf
' buscanota onoff=2 o fin secuencia 182 o borrar futuro al grabar 190
    If BuscarNota (borrar, curpos ,notacur ) = 1 Then
       borrar=0
       Exit sub
    EndIf

'  Print #1,"n + curpos, 12 - notacur + (*po-1) * 13 ", n+curpos, 12 - notacur + (*po-1) * 13
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13 ).dur = 0
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13 ).nota = 181
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13 ).onoff = 0
  borrar = 0
'------------------------
     Print #1," DESPUES DE BUSCONOTA curpos notacur "; curpos, notacur
     borrar=0
     cairo_move_to  (c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
     cairo_rectangle(c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  ,wc*6 ,inc_Penta) '13-07-2021 inc_penta/2 por inc_penta
     cairo_move_to  (c, gap1 + (curpos)*anchofig , Penta_y  )
     cairo_line_to  (c, gap1 + (curpos)*anchofig , Penta_y + (notacur-1) * inc_Penta  +inc_Penta )
     RollDur=Roll.trk(n + curpos,12 - notacur + (*po-1) * 13 ).dur
     RollNota=Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota
'' para ver el estado del onoff en la posicion elegida notacur
     onoff=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff)
     acordeNro=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).pb)
     '''verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde
     '''acordeNro=CInt(Roll.trk(n,verticalEnOctavaVacia ).dur) 'acordenro
     notaold=notacur  ' hará falta ??? puede traer problemas,,,en cambiadur 

     cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
     cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)
'-----------------------
  Sleep 100
 ''' calcCompas(n) ''mayorDurEnUnaPosicion (n)
 EndIf
 ' ==========================================================================
 ' << CAMBIADUR TAMBIEN INGRESA NOTA NUEVA Y DURACION DONDE NO HABIA >>
 ' << SOBREESCRIBE >> 
   '=============================================================================
' octavaEdicion=estoyEnOctava  para que solo modifique en la octava seleccionada 
' 03-05-2025 ok aorafalta borrar la nota y cambiadur=0 si cambio de octava con el mouse
' pero no la seleccione con el mouse hasta el final 

 If cambiadur= 1  Then ''Ctrl-M y pulso X O CTRL-N MAS NOTA TERMINA 28-09-2025 EN 1467
     cambiadur=0
     trasponer=0
     curposClickDErecho=0 ' ya noquiero que vuelva a la posicion donde se dio click derecho
' sino n odeja cambiar otras notas en un ucrpos nuevo OK!
   If COMEDIT=MODIFICACION_INSERCION   And octavaEdicion<>estoyEnOctava Then ' PARA ctrl-M SOLO CAMBIA UNA OCTAVA POR VEZ
       Exit Sub  'cursorVert = 1 es solo ctrl-m 
   EndIf
   If  COMEDIT=SOLO_MODIFICACION And nota = 0 Then ' PARA ctrl-N cambia en todas las octavas libremente
       Exit Sub  ' SALE DE  ctrl-n, CTRL-M LA NOTA SALE DE NSE EL CLICK DEL CURSOR O FLECHAS
   EndIf

Print #1,"CAMBIADUR 1 ENTRA  "
Print #1,"0000 ENTRA EN ESTA  ONOFF "; Roll.trk(n + curpos, (12-notacur +(estoyEnOctava -1) * 13)).onoff
  If Roll.trk(n + curpos, (12-notacur +(estoyEnOctava -1) * 13)).onoff <>  2 Then
     Print #1,"ENTRO A LLAMAR A BUCONOTA "
     cambiadur=1
     Print #1," ANTES BUCONOTA curpos notacur "; curpos, notacur

     If BuscarNota (cambiadur, curpos ,notacur )=1  Then
        If COMEDIT=SOLO_MODIFICACION  Then
           cambiadur=0 ' si busconota da error 1 deberia salir sin hacer nada, para ctrl-M no va salir   
           Exit Sub ' sale para ctrl-N proque solo modifico notas existentes
        EndIf
     EndIf 
     Print #1," DESPUES BUCONOTA curpos notacur "; curpos, notacur
     cambiadur=0
     cairo_move_to  (c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
     cairo_rectangle(c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  ,wc*6 ,inc_Penta) '13-07-2021 inc_penta/2 por inc_penta
     cairo_move_to  (c, gap1 + (curpos)*anchofig , Penta_y  )
     cairo_line_to  (c, gap1 + (curpos)*anchofig , Penta_y + (notacur-1) * inc_Penta  +inc_Penta )
     RollDur=Roll.trk(n + curpos,12 - notacur + (*po-1) * 13 ).dur
     RollNota=Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota
'' para ver el estado del onoff en la posicion elegida notacur
     onoff=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff)
     acordeNro=CInt(Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).pb)
'''     verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde
'''    acordeNro=CInt(Roll.trk(n,verticalEnOctavaVacia ).dur) 'acordenro
     notaold=notacur 

     cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
     cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)

  EndIf  

  ' debo cambiar esa nota por su equivalente silencio
  '1ero se pulsa la nueva duracion y luego la X porque aca se anula luego el flag cambiadur
  'y la dUR debe tener un valor antes de esto
  '  Roll.trk(notacur + (*po) * 13, n + curpos).dur = DUR
  '  vdur=Roll.trk(notacur + (*po) * 13, n +curpos).dur
  '  ' SI LA NOTA  NO EXITIA LA agrega
  '  Roll.trk(notacur + (*po) * 13, n + curpos).nota = notacur  ' 5.7.2
  '  vroll= Roll.trk(notacur + (*po) * 13, n + curpos).nota
  '  cambiadur = 0
  ' ------------------------------------
  ' nueva verion con puntillo o silencio
  ' ------------------------------------
  'si en una columna todas las notas son 0,181 y si hay un 190 en dur
  ' y nota esa columna se elimina,,,,,implementar 12-05-2021 implementado se uso 190
  ' la forma facil de borrar seria grabar a disco cuando se esta
  ' en posicion con 190 saltar esa posicion y restar 1 a la posicion final.
  ' se ahce ya de ambasformas usando delete tambiense borra online
  If notaold > 0 Then 
     notacur=notaold
    '' notaold=0
  EndIf
'-------------------------------
  notaold=notacur 
  cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
  cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)
'-------------------------------
' DEBO RECREAR TODO LO DE NUCLEO ACA PARA TICKS 
  Dim posn1 As Integer
  Dim As Integer durant,posoffdurant=0
  durant=Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur
  Print #1,"CURSOR BORRADO DE OFF 1 DE LA NOTA "
  Print #1,"durant anterior a cambiar, n ";"dur= ";durant," n= ";n
  Print #1,"Curpos ";curpos
  posoffdurant=n + curpos + DurXTick(durant) -1
  Print #1,"posoffdurant anterior ";posoffdurant
' BORRADO --nuevo 28 abril si no anda usar este 
  Roll.trk(posoffdurant, 12 - notacur + (*po-1) * 13).dur=181
  Roll.trk(posoffdurant, 12 - notacur + (*po-1) * 13).nota=0
  Roll.trk(posoffdurant, 12 - notacur + (*po-1) * 13).onoff=0
  
'----------------------

  trasponer=0
 If DUR=181 Then
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = 181
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota =0   ' 5.7.2
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff =0
     cambiadur = 0
     DUR=0
 ElseIf DUR=182 Then ' borrado 
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = 182
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota =181   ' 5.7.2
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff =0

  '   print #1,"FINDE SECUENCIA !!" debo calcular el maxpos
     Maxpos=n+curpos ' en el caso que partimos de una secuencia vacia sin datos
' quedaria rellenar los espacios vacios con nota=181 y dur=0 creo
     cambiadur = 0
     DUR=0
 ElseIf DUR=0 Then ' borrado
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur =181
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota=0
    cambiadur = 0
    DUR=0
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff =0
 ElseIf DUR=190  Then ' marcamos para borrar al grabar CTRL-9
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur =190
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota=190
    cambiadur = 0
    DUR=0
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff =0

 Else
'desde aca las notas se colocan con notacur siemrpe tienen valorde 1 a 12
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR
    incr=0
  EndIf
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 9
   incr=0
  EndIf
 
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 18
   incr=0
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 27
   incr=0
  EndIf 
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 36
   incr=0    
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 45
    incr=45
  EndIf
   
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 54
    incr=45
  EndIf
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 63
    incr=45
  EndIf
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 72
    incr=45
  EndIf
   
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 81
    incr=45
  EndIf   
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 90
    incr=90
  EndIf   
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 99
    incr=90
  EndIf 

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 108
    incr=90
  EndIf

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 117
    incr=90
  EndIf

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 126
    incr=90
  EndIf
'----- fin 3er bloque   
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 135
   incr=135
  EndIf   

  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 144
   incr=135
  EndIf   

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And silen=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 153
   incr=135
  EndIf   

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And silen=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 162
   incr=135
  EndIf   

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And silen=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 171
   incr=135
  EndIf   
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur  ' 5.7.2  

 EndIf
Dim As UBYTE DURX = Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur
 
''''' =>>> agregado ticks aca no hay fin de secuencia se pone 181 en vez de 182
Print #1,"LLEGANOTA y DUR ",notacur;"  DUR "; DUR
trasponer=0
  If notacur > 0 And DURX > 0 Then
'Print #1,"LLEGA NOTA ",notacur
     posn1=n+curpos ' posishow + curpos
     
     If DURX <= 90 Then 'caso 1) no ligado, I, L, W etc, viene algo que termina
   '     Print #1,"DUR <= 90 "
        If posnOffOld > 0 Then ' caso 4) llego un caso 2) ligado y luego un 1) no ligado
    '       Print #1,"DUR <= 90 posnOffOld > 0 "
           Roll.trk(posnOffOld,(12-notacur +(estoyEnOctava -1) * 13)).onoff=0 'sigue
           Track(ntk).trk(posnOffOld,1).onoff = 0 'sigue la duracion
           Track(ntk).trk(posn1,1).nota = notacur
           Track(ntk).trk(posn1,1).onoff = 0
           posnOff=posn1 + DurXTick(DURX) -1 'nuevo off porque dura mas -1 estaba mal
     '      Print #1,"posn1, DUR, DurXTick(DUR) "; posn1, " ";DUR;" " ;DurXTick(DUR)
           Track(ntk).trk(posnOff,1).nota = PianoNota 'notacur
           Track(ntk).trk(posnOff,1).onoff = 1 '
           Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).onoff=1 'termina
           Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).nota=183 'notacur 'termina

           Roll.trk(posnOff+1,(12-notacur +(estoyEnOctava -1) * 13)).dur = 181
           Track(ntk).trk(posnOff+1,1).dur= 181


           posnOffOld=0 
        Else '==> PROBADO  TESTEADO 1a) viene una nota on y termina 
' si modifico el valor de algo existente debo eliminar el off  de la duracion anterior!
      '      Print #1,"DUR <= 90 posnOffOld = 0 "
            posnarranca=posn1 
            posnOff=posn1 + DurXTick(DURX) -1' caso 1) nada entes nada despues
 ' vemos si sobreescribimos una nota on          
           If  Roll.trk(posn1,(12-notacur +(estoyEnOctava -1) * 13)).onoff=2 Then
' buscar su terminacion
          ' Print #1,"habia nota a anterior"   
          ' Print #1,"encontro el fin anterior y lo borro" 
          ' borrando...NO BORRA 
         '  Roll.trk(posoffdurant,(12-notacur +(estoyEnOctava -1) * 13)).dur=181
         '  Roll.trk(posoffdurant,(12-notacur +(estoyEnOctava -1) * 13)).nota=0
         '  Roll.trk(posoffdurant,(12-notacur +(estoyEnOctava -1) * 13)).onoff=0
           EndIf
      '     Print #1,"caso 1a) posn1, DUR, DurXTick(DUR), posnOff "; posn1, " ";DUR;" " ;DurXTick(DUR);" ";posnOff
           posnOffOld=0
       ''For j As Integer=posn+6 To posnoff-6
       ''  Roll.trk(j,(12-notacur +(estoyEnOctava -1) * 13)).dur=184
       ''Next j

          Roll.trk(posn1,(12-notacur +(estoyEnOctava -1) * 13)).onoff=2
          Roll.trk(posn1,(12-notacur +(estoyEnOctava -1) * 13)).nota=notacur
          Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).onoff=1
          Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).nota=183 ''notacur
          Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).dur=183
      '    Print #1," puso nota 183 en posnOff ";posnOff  '97
          Track(ntk).trk(posn1,1).nota = notacur
          Track(ntk).trk(posn1,1).onoff = 2
          Track(ntk).trk(posnOff,1).nota = PianoNota ''notacur
          Track(ntk).trk(posnOff,1).dur = 183
          Track(ntk).trk(posnOff,1).onoff = 1  

          Roll.trk(posnOff+1,(12-notacur +(estoyEnOctava -1) * 13)).dur = 181
          Track(ntk).trk(posnOff+1,1).dur= 181

    
        EndIf
        nroPartesNota=DurXTick(DURX)
     Else ' caso 2) la nota ligada n+ 
       ' Print #1,"DUR > 90 "
     If posnOffOld > 0 Then 'caso 4 o 5 vino una n+ yluego n+ otra ligada o no
     '  Print #1,"DUR > 90 posnOffOld > 0 "
       Roll.trk(posnOffOld,(12-notacur +(estoyEnOctava -1) * 13)).onoff=0
       Track(ntk).trk(posnOffOld,1).onoff = 0 'sigue la duracion
       posnOff=posnOffOld + DurXTick(DURX) -1
     ' ' posn=posnOff
      ' If (posn > NroCol + InicioDeLectura) Then
      '  InicioDeLectura=InicioDeLectura + NroCol
      ' EndIf
      '' MaxPos=Posn +1 
      '' pmTk(ntk).posn=posn
      '' pmTk(ntk).MaxPos=MaxPos

       posnOffOld=0
      
     Else
      ' Print #1,"DUR > 90 posnOffOld = 0 "
       posnOff=posn1 + DurXTick(DURX) -1 ' caso 1) nada entes nada despues
       posnOffOld=0
       Roll.trk(posn1,(12-notacur +(estoyEnOctava -1) * 13)).onoff=0
       Roll.trk(posn1,(12-notacur +(estoyEnOctava -1) * 13)).nota=notacur
       Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).onoff=1
       Roll.trk(posnOff,(12-notacur +(estoyEnOctava -1) * 13)).nota=183 ''notacur
       Track(ntk).trk(posn1,1).nota = notacur
       Track(ntk).trk(posn1,1).onoff = 0
       Track(ntk).trk(posnOff,1).nota = PianoNota ''notacur
       Track(ntk).trk(posnOff,1).onoff = 1
 
       posnOffOld=0
     
    EndIf
     nroPartesNota=DurXTick(DURX)
      
     EndIf
' ajuste de valores para lugares vacios sin notas en todo el intervalo x con cambio de octava
' esto tenia sentido para pisar el 182 fin de secuencia, si habia una nota nueva y que no esta en la misma
' octava  que la nota anterior, pero aca no hay carga  secuencial esto es modificacion,,,
' creo que esto lo podemos eliminar pero si la modificacion es elmininar una nota podria tener sentido.
      If nroPartesNota > 0 Then
      For kk As Integer = posnarranca To posnarranca + nroPartesNota-1 
        If notaOld > 0 And notaOld <> notacur   Then
         Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).dur = 0 
         Roll.trk(kk,(12-notaOld  +(estoyEnOctava    -1) * 13)).dur = 0 
         Roll.trk(kk,(12-notaOld  +(estoyEnOctavaOld -1) * 13)).nota = 181
         Roll.trk(kk,(12-notaOld  +(estoyEnOctava    -1) * 13)).nota = 181
        EndIf
      Next kk
' ajuste de valores para lugares vacios sin notas en todas las columnas x
      Dim As Integer noct,i ''oclog = 8 - (estoyEnOctava-1)
   ' barre solo vertical no la posicion 
      For noct = desde To hasta
      For i= 0 To 11 ' gracias a esto anda acordes..
        If i= 12 - nota  And noct = estoyEnOctava  Then 
          Continue For
        Else    
        ' semitono ahora va de 0 a 11 para Roll -> i-1 21-07-2021 jmg
        ' si estoy en fin de sec 182-> nota=0 dur=182 pero aca no se inserta se modifica se pone 181
        ' no hay fin nuevo
         For ll As Integer = posnarranca To posnarranca + nroPArtesNota  -1
          If Roll.trk(ll,(i +(noct -1) * 13)).nota = 0 And Roll.trk(ll,(i +(noct -1) * 13)).nota < 182  Then ' no borra 182
          '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
             Roll.trk(ll,(i +(noct -1) * 13)).nota = 181
             Roll.trk(ll,(i +(noct -1) * 13)).dur  = 0''
             Roll.trk(ll,(i +(noct -1) * 13)).onoff  = 0''
 
          EndIf
          Next ll
        EndIf
      Next i
      Next noct
  ' para track permitir acordes no tiene sentido!!
      For i=1 To lim2
      For ll As Integer = posnarranca To posnarranca + nroPArtesNota -1
          If Track(ntk).trk(ll,i).nota = 0 And Track(ntk).trk(ll,i).dur <182    Then
          '   print #1,"^^^^ cambia 0 en i";i; "octava "; noct 
             Track(ntk).trk(ll,i).nota = 181
             Track(ntk).trk(ll,i).dur  = 0
             Track(ntk).trk(ll,i).onoff  = 0
          EndIf
         Next ll  
      Next i


     nroPartesNota=0
     EndIf
      notaold=12-notacur +(estoyEnOctava -1) * 13
'''---- OJO ESTO NO ESTBA -----------      
      If COMEDIT=SOLO_MODIFICACION Then ' wwwwwwwwwwwwww crucial 
' necesario en ctrl-n sino borra la nota, el cursor salta arriba pero no importa 
' el cursor lo mueve verticalmente el nombre de la nota 
'PERO en ctrl-m Hori=1 no borra la nota pero no mantiene la ultima posicion del 
' cursor salta arriba  
      notacur=0 'volvemos a poner sino entra en nucleo o repite sin fin !!! podria ser esto
       notaOld=0 
      EndIf
   
  EndIf 
''' fin agregado ticks
  cambiadur = 0: nota=0
  cuart=0:pun=0:doblepun=0:tres=0:silen=0:mas=0

  Exit Sub
 EndIf ' fin ctrl-m  modificacion Y CTRL-N AGREGAR NOTA CON NOMBRE DE NOTA
 




' EN LA VERSION TICKS AHORA FALTA QUE COLOQUE ONOFF 2 O 1 Y HAGA TODO COMO NUCLEO DESDE ACA
' ORDENAMOS QUE NUCLEO SE EJECUTE UNA VEZ COMO SI HUBIEROM PUESTO LA NOTA MANUALMENTE
'-------------------------------------------------------------------------------------
 
' EN UNA COLUMNA BUSCO LA DURACION MENOR Y LA MAYOR incluido LA DE ENTRADA NUEVA.
' TOMO LA MENOR DE TODAS INCLUSO LA ENTRADA Y FRACCIONO TODAS LAS NOTAS DE LA COLUMNA
' En TANTAS PARTES TENGA CADA FIGURA DE ESA MENOR. PARA ELLO TOMARE LA MAYOR Y LA PARTIRE
' EN LAS PARTES CORRESPONDIENTES DE LA MENOR, LUEGO DE ESO SIGO FRACCIONANDO LAS OTRAS
' QUE SON MAYORES A LA MENOR PERO MENORES A LA MAYOR ,,,,

'  
 '-----------------------------------------------------------
 ' ADAPTAR BIEN PARA CURSOR POR AHORA NO SE USA PONEMOS 3
'If usarAcordesIguales = 3 Then ' empeiza Fraccionador..
'   Dim As UByte menor, mayor
'   Dim As Integer i1men,i1may,j
'   menoryMayorEnColumna (Roll, n+curpos,menor,mayor,i1men,i1may)
'   Print #1,"En Cursor: menor,mayor,i1men,i1may ",menor,mayor,i1men,i1may
'    numeroFrac = CInt(menor) ' simulamos la entrada por el usuario DE LA DURACION DUR
'    Print #1,"Cursor: numeroFrac ", numeroFrac
'   FracTodoDur Track(),Roll,indicePos, mayor,nR,ntk

'   RollaTrack Track(), 0,Roll 
'endif
 ' ===> PROCESO INSERT
 If COMEDIT=MODIFICACION_INSERCION  And insert=2 Then 'ctrl-m insert

  ' insertar  nota
  'ESTANDO EN UNA NOTA ELEGIDA POR EL CURSOR
  '1ero se pulsa la nueva duracion y luego la Tecla INSERT
  ' al final de todo la tecla I (ubicrse en lnota luego 3 teclas
  ' Dur + insert + I)
  'y la dUR debe tener un valor antes de esto
  If notacur=1 Then  ' 1 POR 0 JMG <- aca ingreso nota aunqne no haya..
   notacur=notaOld  'cual toma ? es donde esta el cursor ?
  EndIf
   print #1, "Entra a Cursor con insert 2 notacur: ", notacur
  ' StartInsert esglobal,posicion de inicio de la insercion cuando  pulsamos
  ' la tecla insert se guarda la posicion de inicio desde ahi habra reemplzos
  ' usando la posicion comun n
  ' hasta que se pulse FIN o se alcance el FIN (66)
indaux=0
   print #1, "call a movedato con indaux ",indaux
  ' n es StartInsert fijo no se modifica mas
nota=notacur + (*po-1) * 13
  
  ' param : posicion comienzo (fijo), indice incremental para el aux
  ' ,insert comando habilitado = 1
  'luego reemplazo
   print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (*po) * 13),n
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR ' reemplazo con lo nuevo
  'vdur=Roll.trk(notacur + (*po) * 13, n+curpos).dur
  ' SI LA NOTA  NO EXITIA LA agrega...osea sirve para acordes tambien
  'print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (*po) * 13)
  Dim As Integer posnOff=n + curpos + DurXTick(DUR) -1 
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur ' 5.7.3
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff = 2
  Roll.trk(posnOff, 12 - notacur + (*po-1) * 13).onoff = 1  
  Roll.trk(posnOff, 12 - notacur + (*po-1) * 13).DUR = 183
  'vroll= Roll.trk(notacur + (*po) * 13, n + curpos).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es
  ' que se cargala nota en roll,<- pero 1ero debo guardar la nota de Roll en aux
  ' antes de reemplazarla
  '''insert=1  ' habilita de nuevo la tecla I de ingreso de insercion en un lugar
  'elegido con cursor
 ' print #1, "insert 1 ",insert
  '' insert=0 ' solo inserto una nota por ahora
 EndIf
 'proceso modificar Nuevo secunci limpia terminda en NroCol ,con
 ' nombre de letra pulsada en vez de X CTRL-N
 ' ====> MODIFICAR con ctrl-n
' If (cursorHori=2 Or cursorVert = 2) And agregarNota=1  And nota> 0 Then
' ''COMPLETAR PARA TICKS !!! 29-04-2025
''' nunca consideró notas ligadas, opcion Ctrl-M es mas completa
'' solo agrega  notas sin ligar y tal vez en cualquier octava probrmos usando  (*po-1) en vez de (EstoyEnOctava -1)
'  Dim posn1 As Integer
'  posn1=n + curpos
'  posnOff=posn1 + DurXTick(DUR) -1
'  notacur=nota 


'  ' SI LA NOTA  NO EXITIA LA agrega (lo hacia al final de la secuencia pero no al medio??)
' Roll.trk(posn1,  (12-notacur +(*po-1) * 13)).dur=DUR
' Roll.trk(posn1,  (12-notacur +(*po-1) * 13)).nota=nota
' Roll.trk(posn1,  (12-notacur +(*po-1) * 13)).onoff=2
' Roll.trk(posnOff,(12-notacur +(*po-1) * 13)).onoff=1
' Roll.trk(posnOff,(12-notacur +(*po-1) * 13)).nota=183
' Roll.trk(posnOff,(12-notacur +(*po-1) * 13)).dur=183

 '  ReCalCompas() 
 '  nota=0
 'EndIf
 ' VEMOS QUE HACE ESTA FUNCION SI SIRVE
 ''cairo_surface_get_device_offset
 cairo_stroke(c) ' aca el stroke da el amarillo y no jode a teclas - + con saltos!

'If Roll.trk(notacur + (*po) * 13, n+curpos).dur >= 1 And _ 
'   Roll.trk(notacur + (*po) * 13, n+curpos).dur <= 181 Then
' ===> PROCESO COPIAR
   If COMEDIT=MODIFICACION_INSERCION And vdur=0 And vnota=0 And copiar=1 Then ' ctrl-m
    'Print #1,"copiar 1 timer ",timer
    If  BuscarNota (1, curpos ,notacur ) = 1 Then
        copiar=0 
        Exit Sub
    EndIf

      vnota=notacur 
      vdur =Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur  ' 5.7.3
       copiar=2
    'Print #1,"copiar 2 timer ",timer

   EndIf
'EndIf

'If Roll.trk(notacur + (*po) * 13, n+curpos).dur  < 1  And _  
'   Roll.trk(notacur + (*po) * 13, n+curpos).dur   > 180 Then
'Print #1,"==>VEO COMEDIT VDUR COPIA NOTACUR VNOTA "; comedit, vdur, copiar, notacur, vnota
If COMEDIT=MODIFICACION_INSERCION And vdur>0 And copiar=3 Then ' ctrl-N
    'Print #1,"copiar 3 timer ",timer

   posnOff=n + curpos + DurXTick(vdur) -1   
''   If notacur<> vnota  And MultiKey(SC_CONTROL) And MultiKey (SC_ENTER) Then 
   If MultiKey(SC_CONTROL) And MultiKey (SC_ENTER) Then
'Print #1,"INSERTA COPIA "
      Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur 
      Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = vdur
      Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).onoff = 2
      Roll.trk(posnOff, 12 - notacur + (*po-1) * 13).onoff = 1
      Roll.trk(posnOff, 12 - notacur + (*po-1) * 13).DUR = 183  
     ' copiar=0
     ' vdur=0:vnota=0
     '' ReCalCompas()
   EndIf 
EndIf
If nro=0 Then ' error al subir mucho mira despues del and el estupido 
nro=1 'jmg 16-05-2021 notcur no deberia empezar desde 0
EndIf
If notacur=0 Then
  notacur=1
EndIf
' la causa del error era que el limite mayor del ubound es 39 
' el ubound aunque tenga muchas mas posisiones 4000 , no puede ser
' se ahbra definido el vector de 39 ï¿½???? muy raro
' y n + curpos supera los 39 termina en 67 
' si hago curpos=0 funciona pero lso cuadraditos no avanzan
' ergo salto esto en play y listo deberi aestar condicionado oara edicion eso es
If COMEDIT=MODIFICACION_INSERCION  Then ' ctrl-N 17-09-2021 faltaba condicion de modificacion !! 
  If copiar=3 And (Roll.trk(n + curpos, 12 -notacur + (*po-1) * 13).dur = vdur) And notacur = vnota Then
     'Print #1,"termina copiar 3 lo pone a cero ",copiar,timer
     copiar=0:vdur=0:vnota=0
  EndIf
EndIf
' modelo para poner control de errores en subrutinas lo dejo de modelito


Exit Sub 
 errorhand:
  
Dim As long er1, ErrorNumber1, ErrorLine1
er1 = Err()
If er1 > 0 Then
 Print #1,"Error Cursor detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
 Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po-1 ";*po-1;" *po+1 ";*po+1
 Print #1, "NB "; NB; " NA ";NA; " n ="; n
 Print #1, "LBound (Roll.trk,1) ",LBound (Roll.trk,1)
 Print #1, "LBound (Roll.trk,2) ",LBound (Roll.trk,2)
 Print #1, "UBound (Roll.trk,1) ",UBound (Roll.trk,1)
 Print #1, "UBound (Roll.trk,2) ",UBound (Roll.trk,2)
 
 Dim As Integer valor1 , valor2
 valor2=12 - notacur + (*po) * 13
 valor1 = n + curpos
 Print #1,"*po ";*po;" notacur ";notacur; " indice1 ";valor1; " indice2 ";valor2
 Print #1,Erl, Erfn,Ermn,Err
 Print #1,"------------------------------------"
 ErrorNumber1 = Err
 ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


 Print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
 Print #1,"Error Function: "; *Erfn()
 ers= 12 -notacur +(*po) * 13
 Print #1, "12 - notacur +(*po) * 13) "; ers; "notacur ";notacur
 Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf

 
 
End Sub
'----------------------------------------
Sub Tracks ( ByRef ntk As Integer, ByRef nroCanal As UByte,Roll As inst)
  ''copia track a Roll en memoria , revision para Ticks 31-03-2025 ok queda igual
  ' nrocanal no se usa ,,,,borraremos mas adelante...
clickpista=0
Print #1, ">>>> TRACKS: MaxPos <=2 Or pmTK(ntk).maxpos=0 Then " , Maxpos, pmTK(ntk).maxpos 
If MaxPos <=2 Or pmTK(ntk).maxpos=0 Then
  Print #1, "Salimos de Tracks sin ahcer nada "
  Exit Sub
EndIf

nota=0:dur=0
Print #1,">>>> TRACKS MAXPOS (NTK).MAXPOS ", maxpos, pmTK(ntk).maxpos
 ' sizeof array de ubytepr almcenar el pentagrama roll
 'Notas ( 1 To 65536, 1 To 128) as UByte
' usaremso para cambianr de track en Roll y Track(0)
nombre= titulosTk(ntk)
' evita leer track vacios esto ya lo hace TAB otra vez no
/'   
If nombre=""  Then ' evita revisar track vacios EN UNA CANCION
 Do While nombre=""
 ntk=ntk+1
 If ntk>32 Then
    ntk=1
    Exit Do
 EndIf
 Loop
  nombre= titulosTk(ntk)
Else   
'/
' funcion saltoTAB indica que no calcule los tamaños ancho de figura 
TrackaRoll (Track(), ntk,Roll,"saltoTAB" ) ' no usa ubirtk
Print #1,">>>> Tracks  ntk cargado, nombre ",ntk, nombre

'''EndIf


End Sub
'------
Function valorFigura(TipoCompas As UByte) As Integer

Select Case TipoCompas
      Case Tcompas2_4
        valorFigura= 5000000  
      Case Tcompas3_4, Tcompas6_8
        valorFigura=7500000 
      Case Tcompas4_4
        valorFigura=10000000
      Case Tcompas12_8
        valorFigura=15000000
End Select

End Function
Sub sacanumeros( linea As String, cabeza() As Integer)
 Dim As Integer n=Len(linea), i1=0, num, k1=0,i2 
 Dim ch As String * 1 
 Dim res As String
 
 For i1 =1 To n
   ch = Mid(linea,i1,1)
   res=res + ch
   If  k1= 2 Then
       linea = Mid(linea, i1)
       k1=k1+1
       Print #1, "queda "; linea 
       cabeza(k1,1)=ValInt(linea)
       cabeza(k1,2)=i1     
   Else   
        If ch = " " Then
          num=ValInt(res) ''' ojo res es integer como global no afecta
          res=""
          If num> 0 Then
             k1=k1+1
             cabeza(k1,1)=num
             cabeza(k1,2)=i1 -i2
             i2=0  
          EndIf
        Else
         i2=i2+1  
        EndIf
  EndIf  
 Next i1 
For i1=1 To 3
 Print #1, "cabeza ";i1;" ";cabeza(i1,1);" ";cabeza(i1,2)
Next i1
End Sub

Sub buscarsuon (k1 As Integer,  midi() As Nproc, nota As UByte, ByRef h2 As Integer, primeron As integer) 
Print #31," entro a bo  midi(15).volum "; midi(15).volum
Dim h1 As Integer
 For  h1= k1-1  To primeron  Step  -1
     If midi(h1).nota = nota And  midi(h1).non > 0 And midi(h1).match=1 Then
        Print #31,"buscarsuon encontro match del on en off "
      Print #31,"nini del off en curso " ;midi(k1).nini
      Print #31,"nini del on  que machea  " ;midi(h1).nini
      
        midi(h1).dur= midi(k1).nini - midi(h1).nini  'duracion de ON absoluto
Print #31,"resta dur "; midi(h1).dur
        midi(h1).match=2 : midi(k1).match=2  'matchearon
        midi(k1).dur = midi(h1).dur
        Print #31,"encontro un dur h1 "; midi(h1).dur,h1
Print #31, "bo midi(h1).j1 ";midi(h1).j1
Print #31, "bo midi(h1).i1 "; midi(h1).i1
Print #31, " bo midi(h1).volum "; midi(h1).volum 
        Exit For
     EndIf 
 Next h1
h2=h1
Print #31," sale a bo  midi(15).volum "; midi(15).volum
End Sub

Sub cargarMidiPlano (externo As Integer)
On Local Error Goto fail

'1) reconstruimos las duraciones de las notas calculando la diferencia de los delta
'  entre los off y on de una misma nota.  listo ok
' usar la nueva sub o funcion duraciones() que tomara una duracion y
' devolvera la suma de duraciones estandar que componen ...
'2) necesitmos saber el retardo entre notas ON para construir las posiciones de cada nota 
'  ON respecto de la anterior. Aca se complica, si una nota ON dura mucho y antes que termine
' comienza el on de otra nota, debemos calcular la separacion entre notas on, tomar esa 
' separacion convertirla en una duracion ponersela a la primer nota usando una figura con
' ligadura (+) y el resto de su duracion continuara en otra figura que tendra una duracion
' igual a la duracion inicial (off - on) menos la duracion de la ligada calculada anteriormente.
' ese proceso podria ocurrir varias veces. (esto no es necesario dado que para eso tenemos los
' algoritmos de entrada por teclado, deberiamos usar o remedar la entrada por teclado )
' FUNDAMENTAL IMITAR LA ENTRADA POR TECLADO EN ESTA SUBRUTINA... 
'.Pero como usamos acordes iguales lo mejor
' seria convertir todo eso en acordes iguales. Pero sin tener en cuenta la conversion a acordes
' iguales , lo unico que necesitamos es la separacion entre notas ON, que podria convertirse
' wn silencios intermedios si esa separacion es mayor que la duracion de la primer nota ON.
' esos silencios son lo unico que importa deducir porque el ritmo lo dara el tempo.
' si es muy deforme la reconstruccion acordes distintos lo intentara reproducir y si es muy
' mala se debera reconstruir con acordes iguales manualmente por el eusuario o haremos
' una nueva funcion de transformar en acordes iguales una secuencia con acordes distintos
' esa funcion sera muy util y ya es necesaria queda por hacer con alta prioridad.
' Vamos  a usar duraciones con decimales y luego la funcion 
''' Function FiguraEquivalente(DURk As Double) As UByte 
' y esa figura   equivalente es la que se cargara como duracion. Para ello necesitamos
' que durp sea definida como double o float y que sea el argumento de la funcion
' FiguraEquivalente y esa duracion equivalente es la que cargaremos en Track o en un  Roll
' creo que en un track es mas facil que es lo que se implemento, luego lo pasaremos a Rol
' con TrackARoll ... y lo tendremos en pantalla ...
Dim CantEventos  As Integer
IgualaMidiPlano(externo, CantEventos)

fileflush(-1)

ReDim (Track(00).trk ) (1 To CantEventos, 1 To lim3)

If Open ( ROLLDIR+"CARGAPLANO.TXT" For Output As 31) <> 0 Then
   Print #1,"ERROR ABRIR CARGAPLANO.TXT "
   Exit Sub
EndIf 
Print #31,"en cargar midi plano "


If  Open( ROLLDIR+"igualado.txt" For Input As 30 ) <> 0 Then
'If Open( "secuencia-F-SF+SF-F-PLAY.txt" For Input As 30 ) <> 0 then
  Print  #31,"error archivo igualado  midi"
  Exit Sub 
Else
Print #31, "seguimos procesando  midi plano "

Dim As String linea
Dim As Integer j1 , i1 ,i2, acumnew,acumold=999, numTrk, trkend,h1,primeronEncontrado
Dim As Integer negra,incremento, principio, notacant, diferencia
Dim As UByte  mfile,esOn,esnota, esch, esvol, canal, nota, _
              r1,cantTrk, tipoMidi,inicioTrk, compas,trkname,seqname,durp
Dim As Double durx, razon
For j1=1 To  CantEventos 
  For  i1=1 To  lim3 
Track(00).trk (j1,i1 ).nota = 181
Track(00).trk (j1,i1 ).dur = 0
Track(00).trk (j1,i1 ).vol = 0
Track(00).trk (j1,i1 ).canal = 0
 
  Next i1
Next j1 
'consideramos un acorde de 8 notas maximo para pruebas

'LA CANTIDAD DE NOTAS SU ORDEN Y NUMEROS ESTA PERFECTO
' CORREGIR LAS DURACIONES , falta ahora convertir a duraciones de rollmusic
Dim As Integer cabeza (1 To 3, 1 To 2)  

'''Static As  NProc midi (1 To 8000)   ' 9 octavas serian 108 + 1 de relleno ctrls
Dim As  Integer k1=0
'Print #31,"antes del do"
  Do While Not EOF(30)
'Print  #1,"entro al  do"
    Line Input #30, linea

 
    k1=k1+1
    trkend= InStr(UCase(linea),"TRKEND")
    If trkend > 0  And trkend=1 Then
       Print #31, "trkend ";trkend  
      Erase midi 
    EndIf  
      
     midi(k1).Mfile  =InStr (UCase(linea), "MFILE")
    If midi(k1).Mfile > 0 Then
          Print #31, "INPUT "; linea 
          sacanumeros (Mid(linea,6), cabeza() )
          midi(k1).Tipo= cabeza(1,1)
          cantTrk=cabeza(2,1)
          midi(k1).valFig=cabeza(3,1) 'este valor es por una negra no importa que. 
          negra  = cabeza(3,1)
          'If midi(k1).Tipo=1 Then
          '   Print #31,"cantTrk ",cantTrk
          '   For r1=1 To cantTrk 
          '      ReDim (Track(r1).trk) (1 To CantTicks, 1 To lim3)
          '   Next r1  
          'EndIf
         ' Print  #31, "cabeza 1,2,3  :  "; cabeza(1), cabeza(2), cabeza(3)
    EndIf       

    tempo=InStr (UCase(linea), "TEMPO")
    If tempo > 0 Then 
       Print #31, "INPUT "; linea 
       tempo=ValInt(Mid(linea, tempo+6)) '' mas 6 ????
      ' Print #31,"tempo "; tempo 
    End If

    compas=InStr(UCase(linea), "TIMESIG")
    If compas > 0 Then
       Print #31, "INPUT "; linea 
       midi(k1).TimeSig = Mid(linea, compas+8, 4)
      ' Print #31, "TimeSig " ; midi(k1).TimeSig
    EndIf  
' una negra es un cuerto y es de un valor de cabeza
    trkname= InStr(UCase(linea), "TRKNAME")
    seqname= InStr(UCase(linea), "SEQNAME")
    If trkname > 0  Or seqname > 0 Then
         Print #31, "INPUT "; linea 
       '  Print #31, " entra para numTRk "
         numTrk=numTrk+1
         midi(k1).numTrk = numTrk
         Print #31, "k1 midi(k1).numTrk "; k1, midi(k1).numTrk
         notacant=0 ' cantidad de notas y controles
      '   Print #31,"TODO A CERO I1 J1 AFECTA ???"
         j1=0
         i1=0  
         inicioTrk=1
    EndIf 
    midi(k1).non=InStr(UCase(linea), " ON ")
    
    If midi(k1).non > 0 And midi(k1).match = 0 then  ''And iniciotrk=1 Then ' priemr linea de datos
          Print #31,"_______________________________________"
          Print #31, "INPUT "; linea 
          Print #31,"entra a ver los ON  k1 "; k1
          midi(k1).nini=ValInt(Mid(linea, 1,midi(k1).non -1))
          Print #31,"nota nini inicial "; midi(k1).nini
          notacant=notacant+1
          midi(k1).match = 1  ' hay on todavia no hay off 
          acumnew=midi(k1).nini 
         If  primeronEncontrado=0 Then
            primeronEncontrado=k1
            acumold=acumnew
           midi(k1).i1=1
           midi(k1).j1=1
           i1=1
            j1=1
         Else
            diferencia = Abs(acumold-acumnew)
            razon = diferencia/acumold 
 Print #31, "razon "; razon
               If acumnew=acumold Or  razon <=  0.01 Then
                     i1=i1+1
                     If i1 > 13 Then
                       i1=1:j1=j1+1 
                     End If '22-09-2024
                     midi(k1).i1=i1
                     midi(k1).j1=j1
                   Print #31, "i1 incremento vertical   "; i1
               Else
                  i1=1
                  midi(k1).i1=i1
                   j1=j1+1
                   midi(k1).j1=j1
                   Print #31," j1 incremento horizontal "; j1 
               EndIf
         EndIf
          Print #31, "midi(k1).j1 ";midi(k1).j1
          Print #31, "midi(k1).i1 "; midi(k1).i1
          acumold=acumnew

          esch = InStr(UCase(linea), "CH=")
          If esch > 0 Then  
            midi(k1).canal =CUByte (ValInt(Mid(linea,esch+3, 2)))
            Print #31, "hay canal de on "; midi(k1).canal
          EndIf
          esnota=InStr(UCase(linea), "N=")
          If esnota > 0 Then
            midi(k1).nota = CUByte( ValInt(Mid(linea,esnota+2, 2)))
            Print #31,"nota nro de on"; midi(k1).nota   
          EndIf
          esvol=InStr(UCase(linea), "V=")
          If esvol > 0 Then 
            midi(k1).volum=CUByte(ValInt(Mid(linea,esvol+2, 3)))
            Print #31,"vallint hay volumen de on ,k1 "; midi(k1).volum, k1
          EndIf
         midi(k1).numTrk = numTrk
         Print #31,"on midi(k1).numTrk ";midi(k1).numTrk
         Print #31,"on midi(k1).nota ";midi(k1).nota                 
         Print #31,"on midi(k1).canal ";midi(k1).canal
         Print #31,"on midi(k1).volum, k1     ";midi(k1).volum, k1
  ' If  numTrk > 0  And j1 > 0 And i1 > 0 Then
  '       Print #31,"Carga Track nota i1 j1 k1 "; midi(k1).nota, i1, j1, k1
  '     Track(numTrk).trk(j1,i1).nota =midi(k1).nota
  '     Track(numTrk).trk(j1,i1).canal  =midi(k1).canal
  '     Track(numTrk).trk(j1,i1).volum     = midi(k1).volum
  '     Track(numTrk).trk(j1,i1).dur    = midi(k1).dur
  ' EndIf 

    EndIf
    midi(k1).noff=InStr(UCase(linea), "OFF")
    midi(k1).nligado=InStr(linea, "+++")
    If midi(k1).nligado > 0 Then 
    Print #31,"SALELIGADO  midi(k1).nligado "; midi(k1).nligado
    EndIf
' LO PROCESAREMOS DESPUES POR AHORA SOLO USAREMOS ON OFF 
    If midi(k1).noff  > 0  And midi(k1).match = 0 Then   
         midi(k1).match = 1 ' de un off 
         Print #31,"__________________________________"
         Print #31, "INPUT "; linea 
         Print #31,"estamos en un off " 
          midi(k1).nini=ValInt(Mid(linea, 1,midi(k1).noff -1))
          Print #31, "midi(k1).nini ";midi(k1).nini
          esch = InStr(UCase(linea), "CH=")
          If esch > 0 Then 
             midi(k1).canal =CUByte (ValInt(Mid(linea,esch+3, 2)))
          EndIf
          esnota=InStr(UCase(linea), "N=")
          If esnota > 0 Then
             midi(k1).nota = CUByte( ValInt(Mid(linea,esnota+2, 2)))
          EndIf
          esvol=InStr(UCase(linea), "V=")
          If esvol > 0 Then 
             midi(k1).volum=CUByte(ValInt(Mid(linea,esvol+2, 3)))
           Print #31, "midi(k1).volum esvol "; midi(k1).volum
          EndIf
          Print #31," k1 ANTES DE BUSCAR SU ON ", k1
          buscarsuon ( k1, midi(), midi(k1).nota, h1,primeronEncontrado)
          Print #31, " h1 DESPUES DE BUSCAR ON ", h1
         Print #31, "midi(h1).j1 ";midi(h1).j1
         Print #31, "midi(h1).i1 "; midi(h1).i1
         Print #31, "midi(h1).nota ";   midi(h1).nota
         Print #31,"midi(h1).volum  ";midi(h1).volum
         If   midi(h1).j1 > 0 And  midi(h1).i1 > 0 And midi(h1).match=2 Then 
' pasamos relativo a la negra y con eso alimentamos a 
' duraciones 
             Print #31, "negra ===>, midi(h1).dur  "; negra, midi(h1).dur
             midi(h1).dur=midi(h1).dur/negra  ' negra es canbeza(3)
             Print #31, " k1 h1 midi(h1).dur "; k1,h1, midi(h1).dur 
   '''' durp=FiguraEquivalente(durplano)
'' aca usare duraciones (midi, k1) 
         If  midi(h1).dur < 0 Then ' 24 julio 2024
             midi(h1).dur= midi(h1).dur * -1
         EndIf   
            duraciones (midi(), h1,k1)
' y duraciones devuleve un vector duras con 3 partes de la nota
' podremos hacer mas veremos como funcionaba esto 
            Print #31, " j1, i1 " ; midi(h1).j1,midi(h1).i1
' para roll negra = 1 y las duraciones estan expresadas respecto de ella 
' las dur de roll son 1,2,3,4,5,6,7,8,9 ...a 182  O P I L F E X H W... etc
' debemos distinguir las que tienen volum 0 silencios de las que no.
' PARA VER UNA APROX ENVIAMOS ESTE VALOR 
' aca si me devuelve varias notas lso duras(1 to 3) debo insertar
' nuevas notas,
' si es unanota sin acorde se o con acorde siempre la apertura es horizontal
' ergo se expange j1 
' incremento hotizontal por la expansion
           Print #31,"Carga Track nota j1 i1  k1 "; midi(h1).nota, midi(h1).j1, midi(h1).i1, k1
           Print #31, "numdurasmidi nota j1 i1 "; numdurasmidi(1); " ";midi(h1).nota;" ";midi(h1).j1;" "; midi(h1).i1,
numTrk=0 

         For  i2=1 To numdurasmidi(1)
           If  duramidi(i2) > 0  Then ' 2 es off 
             Print #31, "duramidi("; i2 ;")"; duramidi(i2) ; figura (duramidi(i2))
             Print #31,  "h1 ";   h1; " i1 ";i1; " i2 "; i2 
             Print #31, "midi(h1).j1+principio+i2-1 ";midi(h1).j1+principio+i2-1
         
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).dur    = duramidi(i2)
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).vol    = midi(h1).volum
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).nota    = midi(h1).nota
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).canal   = midi(h1).canal
               incremento=i2-1
           Else
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).dur    = 0
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).vol    =  0
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).nota    = 181
               Track(numTrk).trk(midi(h1).j1+principio+i2-1,midi(h1).i1).canal   = 0

           EndIf
          Next i2
          principio=principio+incremento
          incremento=0
          Print #31,"nota principio "; midi(h1).nota; " "; principio
         EndIf
         numdurasmidi(1)=0
    EndIf
    SetGadgetText(TEXT_TOPE,"Proc "+ Str (k1))
  Loop
'
' listo lo grueso estaria veremos como funciona 1er aprox
  Print #31,"Finn Proceso "
ntk=0
  SetGadgetText(TEXT_TOPE,"FIN Proceso 1" )
  Dim As Integer  k2,k3
  Print #31, "j1 "; j1; " i1 ";i1
 ' For r1=1 To cantTrk 
       For  k2=1 To j1+principio
           SetGadgetText(TEXT_TOPE,"Proc j "+ Str (k2))
          For  k3 =1 To  lim3 
              If  Track(ntk).trk(k2,k3).nota > 0 And Track(ntk).trk(k2,k3).nota <  181 Then
 Print #31, "j1=";k2;" i1=";k3; " nota="; Track(ntk).trk(k2,k3).nota; " dur=" ;  Track(ntk).trk(k2,k3).dur; " vol=";Track(ntk).trk(k2,k3).vol;" canal=";Track(ntk).trk(k2,k3).canal  
              EndIf

          Next k3
        Next k2

 
 'Next r1  
 SetGadgetText(TEXT_TOPE,"FIN TOTAL" )

nombre="CARGAPLANO"

ntk=0
NADACARGADO=FALSE
Parar_De_Dibujar=SI
pmTk(ntk).desde = 2
pmTk(ntk).hasta = 8
pmTk(ntk).MaxPos = j1+principio +2
MaxPos = k2
pmTk(ntk).posn = 2
pmTk(ntk).patch=1
pmTk(ntk).portout=0
posn=2
titulosTk(ntk)=nombre
posicion=1
DUR => 0
curpos =>0
patchsal=1
pmTk(ntk).patch=1 
Track(ntk).trk(MaxPos,1).dur  =182 
cargacancion=CARGAR_NO_PUEDE_DIBUJAR
MaxPos=pmTk(ntk).MaxPos 
pmTk(ntk).posn=pmTk(ntk).MaxPos - 2
ROLLCARGADO=TRUE
numdurasmidi(1)=0
Erase duras

TrackaRoll (Track(),ntk,Roll,"cargarMidiPlano")
cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
Parar_De_Dibujar=NO
Sleep 5
ReCalCompas ()
          '  print #1,"despues RecalCompas veo nombre ",nombre
            MenuNew=MENU_INICIAL
            cierroedit= 0
            carga=1 ' <======= control de Carga 
            TRACKCARGADO=TRUE
            ROLLCARGADO=FALSE
           
EndIf
'--------------------------------------------------
' fraccionar acordes desiguales para convertir acordes desiguales
' en iguales ....

'--------------------------------------------------
 Print #31, "============> FIN PLANO MIDI <========================
FileFlush (30)
FileFlush (31)
Close 30
Close 31
Sleep 20
SetGadgetText(TEXT_TOPE,"EXIT SUB " )
Print #1, "============================="
Print #1," llama a REfacturarPista "
' REFACTURAR ACA FUNCIONA BIEN COMO EL ORIGINAL MANUAL 31-05-2024
' LO QUE PASA ES QUE LA FUNCION AUTOMATICA ANDA MAL INSERTA MUCHA COLUMNA 
' EN BLANCO O BORRA COLUMNAS AL INSERTAR... TAREA CORREGIR
' SE GRABO  CARGAPLANO.ROLL ORIGINAL DE LA CARGA TEST PLANO CANAL 2 PARA PRUEBAS
' ES UNDESAFIO DE REFACTURAR COMPROBAMOS QUE ANDA MAL EN ESTE CASO!!!
' cuando se corrija el refacturar automatico aca anda ra bien tambien , se supone ,,,,
' ELIMINAMOS LA OPCION CON O SIN REFACTURACION
'If confrac=1 Then  ' hace la fracturacion de cada columna 
'
' RefacturarPista()
 
'EndIf
'----------------------fin -------------
Exit Sub 
fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl
  Print #1, errmsg

End If

End Sub 

Sub buscarnotasdeacorde (expansoron() As string,expansoroff() As String ,cnton As Integer, cntoff As integer , stringnotas() As String, ByRef cntnewoff As Integer, expansorontime() As Integer,expansorofftime() As Integer)
' cntoff puede ser 1 o mas pero siempre menor a cnton pues faltarian offs
Dim As Integer i3,i2, i1, esn, eson, timeon,timeoff, timefig
Dim As String notaon, notaoff

' cntoff=1 es el caso mas sencillo porque podria venir el ON luego de 4 on y 2 off
' y en ese caso  faltaria completar 2 off 
'  sino lo mas sencillo hubiera sido 4 on y 1 off.. faltarian 3 off
' en el caso general habira que comparar todos los off que hay con todos los on
' y ver cuales on no tienen off... PERO ADEMAS DEBEMOS MIRAR  EN LAS DIFERENCIAS DE TIEMPO!
' generalizar un algoritmo para comparar todos los  casos,, uff 
' determinar los off que faltan , para ello comparamos los dos vectores expandor on y off
' 1) preguntamos si cntoff=1 , si lo es ya sabemos que los on solo tienen un off hay 
' q agregar el resto,esto es hacer un apareo rudimentario pero los off se leen reiteradamente.
 Dim as Integer matchon=0
 For i1=1 To cnton
    matchon=0 
    esn= InStr(expansoron(i1), " n=")
    notaon=Mid(expansoron(i1), esn,5) ' "n=54 " por ejemplo
 'si es un acorde todos los time son iguales
' el primer timeoff despues de los N on del acorde me da la figura 
' del acorde de la 1er columna o sea su duracion
    timefig = expansorofftime(i1)-expansorontime(i1)     
    timeoff = expansorofftime(i1) 'el primer off me da el time para el resto agregado  
    
    For i2=1 To cntoff 
      esn= InStr(expansoroff(cntoff), " n=")
      notaoff=Mid(expansoroff(cntoff), esn,5) ' "n=54 " por ejemplo
      If notaon=notaoff Then ' encuentro el on del ultimo off
         matchon=i2
      EndIf
    Next i2
    If matchon > 0 Then ' encontro el on ,nose hace nada  
    Else 
       i3=i3+1
       stringnotas(i3)= notaon
    EndIf
    matchon=0  
Next i1
' en stringnotas(i3) tengo las notas on sin off, hay que generar los off faltantes
' i3 es la cantidad de off que faltam
' si al cerar un time con off el gap de tiempo es mayor al que tiene
'  la columna se divide de nuevo uff
cntnewoff=i3

End Sub 

Sub  tiempoNegra(linea As String, ByRef tNegra As Integer, ByRef ubicacion As Integer)
Dim As Integer mfile 
'MFile 1 2 960 
Dim As Integer cabeza (1 To 3,1 To 2) 
mfile = InStr (UCase(linea), "MFILE")
If mfile > 0 Then ' tomo el valor de la negra
   sacanumeros (Mid(linea,6), cabeza() )
   tNegra  = cabeza(3,1)
Print #1, "tiemponegra  "; tnegra 
   ubicacion=cabeza(3,2) + 6 -2 ' en linea 
  Print #1, "tiemponegra ubicacion ";ubicacion
EndIf


End Sub
Function ConvertirAmil (ByRef L1 As string, ubicacion As Integer, dbg As integer) As String
L1 = Mid(L1, 1, ubicacion)
Print #dbg, "ubicacion de Time de negra ",ubicacion
ConvertirAmil = L1 + "1000"
Print #dbg, "ConvertirAmil = L1 + 1000";" ";L1 + "1000" 
End Function 

Function ConvertirAmilTimeEvento (L1 As String ,tNegra As Integer, dbg As integer) As string
' BUSCO DELTA ENRE L1 Y L2 DEVUELVO L2 QUE SERIA LR EN EL CODIGO QUE LLAMA
Dim As Integer esoff1, eson1, esch1, esmeta1 ,T1, TR
 
Print #dbg,"ini --------------------------------------"

esch1 =InStr(UCase(L1), " CH=")
'eson1=InStr(UCase(L1), " ON ")
'esoff1=InStr(UCase(L1), " OFF ")
'esmeta1=InStr(UCase(L1), " META ")

Dim As String st1, st2, st3


'T1=ValInt (Mid(L1,1, eson1))
'If T1 > 0 Then
'  st1 = Mid(L1, eson1) 
'  ''nuevotime = estimeon*1000/tNegra
'EndIf

'T2=ValInt (Mid(L2,1, esoff2))
'If T2 >0 Then
'  st2 = Mid(L2, esoff2) 
' ' nuevotime = estimeoff*1000/tNegra
'EndIf
Print #dbg, "CATE esch1 "; esch1
If esch1 > 0 Then 
   st1 = Mid(L1, 1,esch1-4) ' PARA SACAR NUMERODE TIME
 '  Print #DBG,"st1 "; st1
EndIf

T1=ValInt(st1)
Print #dbg,st1; " CATE T1 ", T1

Dim lx As Integer
lx=Len(Str(T1))


st3=Mid(L1,lx+1)
TR=T1*1000/tNegra
Print #DBG,"CATE st3 "; st3
ConvertirAmilTimeEvento = Str(TR) + " "+st3
Print #DBG,"CATE L1 MODIF "; Str(TR) + " "+st3
If T1 =0 Then
 ConvertirAmilTimeEvento=L1
EndIf

Print #dbg,"fin --------------------------------------"
End Function 
Sub IgualaMidiPlano(externo As Integer, ByRef CantEventos  As Integer)
' 23 SETIEMBRE 2024, ultima verison carga midi externos, pero
' con dificultad se asemeja poco a la secuencia original escuchandola
' NO SIRVE DE MUCHO, CARGA LAS NOTAS PERO HAY QUE CORREGIR MUCHO,,   
' Este es un algoritmo secuencial aproximado,que va mirando detectando acordes
' y en cada off seguido de un on se fija si hubo N off para N on del
' acorde y los off que falta  va agregando pares de on-off de modo de que 
' el acorde quede en columnas de igual duracion ligadas o no con sonido o no
' segun el caso.. eso casos tratere de probarlos...
' Este algoritmo podra fallar, uno nuevo que separe en pedazos
' la secuencia y componga con el criterio adicional de detectar la
' duracion de las notas de los acordes,cual es la duracion total del acorde
' y su fraccionamiento en partes iguales teniendo en cuenta esa duracion total de acorde 
'  como en el caso de estudio
' seria la solucion definitiva...por ahora no tengo ganas de hacerlo
' no es la meta cargar perfecto secuencias externas sino producirlas..
' DESPUES DE CARGAR EL ARCHIVO MIDI EN TEXTO AL PROGRAMA PRODUCE:
' F SL F   -> I 
' I
' I
' ES UN ACORDE DE DURACION I PERO LA 1ER NOTA F SL F ESTA COMPUESTA
' POR TRES PARTES Y LA RECONSTRUCCION AUNQUE VALIDA NO SE TOCARIA MUY
' BIEN PORQUE PARA ESO CONVIENE QUE LAS NOTAS ESTEN IGUALADAS EN COLUMNAS
' Y DURACIONES.. ASI DEBERIAMOS LLEGAR A ;
' F SL F   -> I 
' F+SL+F   -> I
' F+SL+F   -> I
' DONDE HAY 3 COLUMNAS CON DURACIONES IGUALES EN CADA COLUMNA F SL Y F
' EL ALGORITMO ACTUAL LO CONSIGUE.. PARA ESE CASO
' FALLO ALPROBAR CON AA-TEST-4L-ACORDE-SIL-SEPARADOS.roll
' FALLO ESTA PARTE O LA OTRA?
' otro caso
' L SL L SL
' L+ L+L +L
'ESTE ULTIMO CASOS DA LO SIG
' L SL L SL
' L+ L+ I
' NO ES PERFECTO PERO SUENA PARECIDO DEBERA CORREGIRSE 
'PROCESAR SI ENTRA UN ARCHIVO EXERNO CON MFILE 192 O VALOR
' MAS BAJO QUE 1000 CONVERTIR A 1000

On Local Error Goto fail
' para expandir acordes desiguales en iguales
' luego se llamara a cargarMidiplano a partir del archivo
' generado por esta rutina
' ESTA RUTINA SOLO FUNCIONA POR AHORA SI LA ENTRADA
' TIENE EVENTOS ON Y OFF PUES ALGUNOS ARCHIVOS MIDI
' PONEN SIEMPRE ON 
'SOLUCION PARA INTERPRETAR SIN NINGUNA REFACTURACION !!!
' o la hacemos con codigo en este mismo proceso ó, grabamos otro archivo
' con estos eventos agregados,,, yluego procesamos como antes..
' asi seria mas sencillo de hacer,,,

' 7015 on ch=2 n=48 v=100
' 7015 on ch=2 n=52 v=100
' 7016 on ch=2 n=57 v=100  -> contar los on
' 7266 off ch=2 n=57 v=0   -> al primer off terminar 57 y cortar las que no terminaron pero ligadas

' *7266 off ch=2 n=52 v=0   -> agregada
' *7266 off ch=2 n=48 v=0   -> agregada

' 7267 on ch= 2 n=57 v=0   -> nace otra 57 pero silencio

' *7267 on ch= 2 n=52 v=80   -> agregada nace otra 52 con audio
' *7267 on ch= 2 n=48 v=80   -> agregada nace otra 48 copn audio

' 7768 off ch=2 n=57 v=0   -> termina 57 silencia de largo L

' *7768 off ch=2 n=52 v=0   -> agregada termina 52 de largo L
' *7768 off ch=2 n=48 v=0   -> agregada termina 48 de largo L


' 7769 on ch=2 n=57 v=80   -> nace 57  
' 8020 off ch=2 n=57 v=0   -> terminan todas 57 F
' 8020 off ch=2 n=48 v=0   ->    " 48 F
' 8021 off ch=2 n=52 v=0   ->    " 52 F

'   7015  7266    7769  8020 
'57    251    s500   251  <- UNA F AUDIBLE,UN SILENCIO L, UNA AUDIBLE F
'52    251+    500+  251  <- UNA NOTA ENTERA I AUDIBLE
'48    251+    500+  251  <- UNA NOTA ENTERA I AUDIBLE
' debemos guardar la duracion mas corta la F en este caso y usarla para todo 251
' esto seria para analizar un acorde de 100 notas 
Dim As String  LOSon     (1 To 2000) ' hasta un acorde de 100, para muchas pistas seran mas?
Dim As String  LOSoff    (1 To 2000) ' hasta un acorde de 100, para muchas pistas seran mas?
Dim As Integer LOStimeon (1 To 2000)
Dim As Integer LOStimeoff(1 To 2000),igualado, dbgIgualado,AMIL 
Dim As String  LASnotas  (1 To 2000) ' string tipo "n=54 "
Dim As Integer LOSdelta  (1 To 2000) 'seria la duracion que da la figura F,L,I etc
' los delta se cargan en cada off 
Dim As lineaSecuencia secuencia (1 To 6000) 'almacena la secuencia
Dim As String final (1 To 1000) 
Dim As String comienzo (1 To 1000) 

igualado=34
dbgIgualado=33
AMIL=35 
' inicioAcorde     1
' finAcorde        2
' inicioSimple     3
' finSimple        4
Dim As Integer hayacorde=0

If Open (ROLLDIR+"DEBUG_IGUALADO.TXT" For Output As dbgIgualado) <> 0 Then
   Print dbgIgualado,"ERR en GUARDAE DEBUG_IGUALADO.TXT "
   Exit Sub
EndIf
Dim As String archivomidi, L1,L2,L3, DELTA, LR  
archivomidi = OpenFileRequester("","","secuencias  (*.txt)"+Chr(0) +"*.txt"+Chr(0) )
Sleep 100
'convertir archivomidi a base negra=1000
' lo que se debe hace es tomar una linea de eventos con la siguiente
' a la primera si es la 1 no hacer nada, en la segunda calcular el
' incremento respecto de la primera que estaria en base 192 por eje, 
'  y pasarla a negra=1000 por ejemplo si con base negra=192 y la pasamos a 1000
' si para 192 es 1000, para incremento x es x*1000/192
Dim As Integer tNegra, ubicacion,STOPCONTINUE
If externo = 1 Then 
 If Open (ROLLDIR+"AMIL.TXT" For Output As 35) <> 0 Then
     Print #1,"ERROR ABRIR PARA SALIDA DE AMIL.TXT "
     Exit Sub 
 EndIf 

 If  Open( ROLLDIR + archivomidi For Input As 32 ) <> 0 Then
   Print #dbgIgualado,"error archivo de entrada midi"
   Exit Sub  
 Else
     Do While Not Eof(32)
          
        Line Input #32,L1
        Print #dbgigualado,"L1 :";L1
        If InStr (UCase(L1), "MFILE") > 0 Then
           tiempoNegra(L1 , tNegra , ubicacion )
           L1 = ConvertirAmil (L1, ubicacion, dbgigualado)
           Print #dbgigualado, "L1 MFILE ";L1
           Print #35, L1 
        Else
          If InStr (UCase(L1), "TRKNAME") > 0 or InStr (UCase(L1), "META") > 0Then 
          ' COMIENZA LOOPERO SOBRE EVENTOS
             STOPCONTINUE=1
'Print #dbgIgualado,"COMIENZA CONVERSION "
             Print #35,L1
             Continue Do  
          Else 
            If STOPCONTINUE=0 Then
               Print #35,L1 
               Continue Do 
            EndIf
          EndIf
        EndIf
        If STOPCONTINUE=1  Then
           LR= ConvertirAmilTimeEvento (L1,tNegra,dbgIgualado)
           Print #dbgIgualado,"LR ";LR
          ' Print #35,L1
           Print #35,LR
        EndIf
     loop
 EndIf

  
     
  

 FILEFLUSH(-1)
 Close 35,32

 'Shell "rename " + archivomidi +" " + archivomidi+"old"
 'Shell "rename " + "AMIL.TXT" + " " + archivomidi
End If

'------------------------------------------------------
 
 If Open (ROLLDIR + "igualado.txt" For Output As igualado) <> 0 Then
     Print #1,"err salida en igualado.txt"
     Exit Sub
 EndIf
' mandamos todo a vector luego volcaremos a igualado.txt

  
  Dim As Integer eson, esoff, esch, esv, esv0,esn,cnton, cnts,cntoff, _
      cntnewoff ,i1,g1,g2, estimeon,estimeonOld,esiniacorde, _
      estimeoff,estimeoffOld,esfinacorde, cierre, cabeza,cntnewoffOld,valv, _
      tempo,MetaTrkEnd
  Dim As String notaon,notaoff 
If externo=1 Then
  archivomidi="AMIL.TXT"
EndIf
If  Open( ROLLDIR + archivomidi For Input As 32 ) <> 0 Then
  Print #dbgIgualado,"error archivo de entrada midi"
  Exit Sub 
Else
''  Open     "secuencia-F-SF+SF-F-PLAY.txt" For Input As 32
  'Print dbgIgualado, "seguimos procesando  midi plano "
' BUSCAMOS LOS EVENTOS DE LAS NOTAS
  Dim As String linea
  
  Do While Not EOF(32)
     Line Input #32,linea 

   tempo=InStr(UCase(linea),"TEMPO")
   'If externo=1  And tempo >0 Then
   '   linea=Mid(linea,1,tempo +5) +"500000"
   'End If 
   MetaTrkEnd = InStr(UCase(linea), "META TRKEND")
   If externo=1  And MetaTrkEnd > 0 Then
      linea=""
       Continue Do
   End If 

     eson =InStr(UCase(linea)," ON ")
     esoff=InStr(UCase(linea)," OFF ")
     'If esoff > 0 Then
     '    Print #dbgigualado, "PUTA MADRE EXISTE OFF ";linea
     'EndIf 
     esch =InStr(UCase(linea)," CH=")
     esn  =InStr(UCase(linea)," N=")
     esv  =InStr(UCase(linea)," V=")
     valv=ValInt(Mid(linea, esv+3,3))
     esv0 =InStr(UCase(linea)," V=0")
     'PRINT #dbgIgualado,"valv ",valv
   If eson=0 And esoff=0 And esch > 0 Then ''controles no interesan por ahora
       Print #dbgIgualado,linea 
       Print #dbgIgualado,"NO PROCESADO" 
         Continue Do
   EndIf   
   ' conversion de on a off para velocidad=0 
   If externo=1 And esv0 > 0 And eson > 0 Then
     ' cambiamos on por off si v=0 para archivos planos no generados
     ' por RollMusic
       Print #dbgigualado,"PASANDO A OFF ";linea
        Dim  As String st1, st2
        st1=Mid(linea,1, eson-1)
        st2=Mid(linea,eson+4)  
        linea=st1+" off " + st2
   EndIf
   ' 0) deteccion de acorde por tiempo de on casi igual
     If eson > 0 Then
        cnts=cnts+1
        estimeon=ValInt (Mid(linea,1, eson))
        If (estimeon - estimeonOld) < 10 Then
            esiniacorde=1
            secuencia(cnts).indicador =inicioAcorde
        Else
            esiniacorde=0
            secuencia(cnts).indicador=inicioSimple 
        EndIf
        estimeonOld=estimeon
        'Print dbgIgualado,"estime "; estime ok    
         
         secuencia(cnts).linea =linea
        
     EndIf 
     If esoff > 0 Then
        Print #dbgigualado,"ENTRA A ESOFF >0"
        cnts=cnts+1
        estimeoff=ValInt (Mid(linea,1, esoff))
        If (estimeoff - estimeoffOld) < 10 Then
            esfinacorde=1
            secuencia(cnts).indicador =finAcorde
        Else
            esfinacorde=0 
            secuencia(cnts).indicador =finSimple
        EndIf
        estimeoffOld=estimeoff
        'Print dbgIgualado,"estime "; estime ok
         secuencia(cnts).linea=linea
        Print #dbgigualado,secuencia(cnts).linea 
     EndIf 
     If cnts=0 Then
    '     Print #34,linea ; '<=== igualado
          cabeza=cabeza+1
          comienzo(cabeza)=linea
     EndIf  
   '1) detectamos eventos de notas
     If esoff > 0 Then 
       PRINT #dbgIgualado,"Es un evento de nota OFF "; linea
       Print #dbgigualado,"esch > 0 And esn >0 And esv > 0";" "; esch;" "; esn;" "; esv   
     EndIf 
   If (eson > 0 Or esoff > 0 ) And esch > 0 And esn >0 And esv > 0  Then 
     PRINT #dbgIgualado,"Es un evento de nota"; linea
      If cnton=1 Then
         cntoff=0
      EndIf
     esv  =InStr(linea," v=")
     valv=ValInt(Mid(linea, esv+3,3))
     Print #dbgigualado,"valv ";valv

   '4)  'vino un on y todavia faltan off, se agregan, ¿cuantos off vinieron antes?
     '2) solo nos interesa detectar el comienzo y final de un acorde
     ' para ello contamos los on, y guardamos la linea, lo mismo para los off
      If eson > 0 Then 
         cnton = cnton + 1
         Print #dbgIgualado,"cnton+1  ";cnton
   '      Print dbgIgualado,"linea cnton "; linea
         g1=InStr(linea," n=")  
         notaon=Mid(linea,g1,5)
         Print #dbgIgualado,"notaon "; notaon
         LOSon(cnton)= linea
    '     Print dbgIgualado,"LOSon(cnton) ";LOSon(cnton)
         LOStimeon(cnton)=estimeon  
     '    Print dbgIgualado,"LOStimeon(cnton) ";LOStimeon(cnton)  
      EndIf
      If esoff > 0 Then 
         cntoff = cntoff + 1
      '   Print dbgIgualado,"cntoff +1  "; cntoff
       '  Print dbgIgualado,"linea cntoff "; linea
         g2=InStr(linea," n=")  
         notaoff=Mid(linea,g2,5)
         Print #dbgIgualado,"notaoff ";  notaoff
         LOSoff(cntoff)= linea
       '  Print dbgIgualado,"LOSoff(cntoff) "; LOSoff(cntoff)
         LOStimeoff(cntoff)=estimeoff  
         Print #dbgIgualado,"LOStimeoff(cntoff) ";LOStimeoff(cntoff)  

      EndIf
' agregue And cnton > 1 23:14 2024-09-10
      If cnton=cntoff And notaon=notaoff  Then
        ' termino el acorde borro todo pero... ya se proceso??? 
       '' es lo mismo ponerlo aca o abajo al final
        ''''Erase LOSon, LOSoff, LASnotas
        cntnewoff=0 
        cnton=0:cntoff=0
        notaon=""
        notaoff=""
        esoff=0 :eson = 1
      'Print #34,""
      '  cnts=cnts+1
      '  secuencia(cnts).linea="====================================="
      EndIf
    ' 3)
      If cntoff = 1 And cnton > 1 And eson=0 And esoff > 0 And cntoff < cnton   Then
         ' es un fin de acorde 
          'Print #34, "  A=";cnton 'inndica la cantidad de on o nnotas del acorde para procesar despues
           ''cnts=cnts+1  
           secuencia(cnts).linea=linea + "  A=" + Str(cnton) 
     ' Else
     '      Print #34,""    
      End If 
Print #dbgIgualado,"cntoff, cnton, esoff, eson,valv "; cntoff, cnton, esoff, eson,valv

      If (cntoff >= 1 And cnton >= 1 And esoff=0 And eson > 0 And cntoff <= cnton ) Or _ 
         (cntoff=0 and cnton=0 And esoff=0 And eson  > 0 and  valv > 0 ) Or _ 
         (cntoff=0 and cnton=0 And esoff=0 And eson  = 1 and  valv =0 )  Then
           ' cnton=1 
         If (cntoff=0 and cnton=0 And esoff=0 And eson  > 0 and  valv > 0 ) Then
            g1=InStr(linea," n=")  
            notaon=Mid(linea,g1,5)
            Print #dbgIgualado,"notaon "; notaon
         EndIf
         Print #dbgIgualado,"ENTRO AL MASAJEO valv ";valv
         Print #dbgIgualado,"4 'vino un on y todavia faltan off, se agregan "
    'vino un on y todavia faltan off, se agregan 
         ' la cantidad a agragar es la diferencia emtre on y off
  'Tambien indica que estamos en un acorde incompleto pero acorde al fin y
 ' las notas ON identifican el acorde de nnumero de notas cnton
 ' ahora deberia marcar como fin de identificacion de inicio de acorde       
          cntnewoff = cnton - cntoff
          If cntnewoff=0  Then ' jmg nuevo
            cntnewoff=cntnewoffOld
          EndIf
          cntnewoffOld=cntnewoff
          Print #dbgIgualado,"CNTNEWOFF ", cntnewoff
          ' buscar de cual nota n= es el off y gregar los 
          ' off a las notas que no sean la anterior con el mismo
          ' time...
          Dim As Integer cntagregar=0  
          If cntoff=0 and cnton=0 And esoff=0 And eson  > 0 And esfinacorde=0 Then
          Else  
            Print #dbgIgualado,"llama a buscarnotasdeacorde"
            buscarnotasdeacorde LOSon(), LOSoff(), cnton, cntoff, LASnotas(), cntnewoff,LOStimeon(),LOStimeoff()
          EndIf
      'agregagamos los off
      ' armamos la linea de impresion de un off que falta, todos seran iguales solo
      ' sera distinto la nota que la tenemos en stringnotas()
          Dim As String nuevoOff,nuoff,nuon,nuevoOn, ch
      'tomamos el 1er off como modelo podra ser unico o que hala mas.,..no importa
          nuevoOff=LOSoff(1)
          nuevoOn=LOSon(1) ' por ahora tomamos cualquiera el 1 
          Print #dbgIgualado,"LOSoff(1) "; nuevoOff     
    '--- imprimimos los nuevos remplazando las nortas
          Print #dbgIgualado,"cntnewoff "; cntnewoff
          Dim As Integer f1,f2, fch
          f1=InStr(nuevoOff,"n=")
'cnts=cnts+1  
'secuencia(cnts).linea="------------------------------------"
' futuro SI FALLA CASOS:MODIFICAR TOMAR LA DIVISION MENOR POR EJEM F
' futuro AGREGAR EN LOS LARGOS OTROS ON OFF DE F SI VALE L
' por ahora ubicar la nota off y no tomarla
Print #dbgIgualado,"ENTRA EL FOR NUEVOS OFF Y ON"
          For i1=1 To cntnewoff
           Print #dbgigualado,"LASnotas(i1),notaon ",LASnotas(i1), notaon
           If (LASnotas(i1) <> notaon  And notaon > "    ") Or _
              (LASnotas(i1) <> notaon  And notaon = "    ") Then
'''saque And LASnotas(i1)> "    "
'construye el off dado que viene volcado como un on con v=0
            nuOff=Mid(nuevoOff,1,f1-1) + LASnotas(i1) + " v=0"
   'agrego un on ya que lo apague pero debe seguir
           Print #dbgIgualado,"ENTRO AL IF del FOR"   
            cntoff=cntoff+1
            f2=InStr(UCase(nuOff),"OFF ")
            fch=InStr(nuOff,"ch=")
            ch=Mid(nuOff,fch,4)            
            nuOn=Mid(nuOff,1,f2-1) + "on " + ch + " "+ LASnotas(i1) + " v=80"
            cnton=cnton+1 
            Print #dbgIgualado,"nuevoOff "; nuOff 
            'Print #34,nuOff + " +++" ' generar nota ligada
             cnts = cnts +1
             secuencia(cnts).linea=nuOff + " +++"   
            Print #dbgIgualado,"nuevoOn "; nuOn
             cnts = cnts +1 
             secuencia(cnts).linea=nuOn + " +++"    
            'Print #34,nuOn
           Else 
            Print #dbgIgualado,"ESTA EN EL FOR PERO NO PROCESA"   
           EndIf 
          Next i1      
' NUEVA RUTINA control de las figuras que sean iguales
' F L F
' F L*
' F L*  ESTARIA MAL DEBO PRIMERO HALLAR EL MINIMO DE CADA COLUMNA
' DE LA 1ER ES F Y TODOS ESTAN IGUALES CHEQUEAR
' DELA 2DA ES L Y HAY QUE CORREGIR L* PARA PRODUCIR L

          'reseteamos todo:
           'Erase LOSon, LOSoff, LASnotas
          ' cntnewoff=0 
           nuevoOff=""
           nuOff=""
           nuon=""
           esoff=0 ' jmg nuevo
           cnton=0:cntoff=0
'cnts=cnts+1  
'secuencia(cnts).linea="------------------------------------"
      Else
          Print #dbgIgualado,"NOOOO ENTRO AL MASAJEO"
      EndIf
   Else
      ' Print #34,""
       If cnts > 0 Then
         cierre=cierre+1  
         final(cierre)=linea
       EndIf    
      Continue Do
   EndIf 

  Loop
 Print #dbgIgualado, "Cantidad de eventos ",cnts
 For i1=1 To cnts
 Print #dbgIgualado, secuencia(i1).linea;" ";
       Select Case secuencia(i1).indicador
             Case 1
            Print #dbgIgualado, "inicioAcorde"
             Case 2
         Print #dbgIgualado, "finAcorde"
             Case 3
         Print #dbgIgualado, "inicioSimple"
             Case 4
          Print #dbgIgualado, "finSimple"
            Case Else
             Print #dbgIgualado, 
       End Select 
 Next i1
EndIf
' TODA LA SECUENCIA CORREGIDA ESTA EN EL VECTOR 
' ANTES DE VOLVARLA HAY QUE CORREGIR LOS ERRORES !!
' nueva subritina!
 'corregirSecuenciaMidi secuencia(i1),cnts

 CantEventos = cnts

For i1=1 To cabeza
 Print #igualado,comienzo(i1)
Next i1

For i1=1 To cnts
 Print #igualado,secuencia(i1).linea  
Next i1

For i1=1 To cierre
 Print #igualado, final(i1)
Next i1

FILEFLUSH(-1)
Close 32,33,34
Exit Sub
fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl
  Print #dbgIgualado, errmsg
End If

End Sub



'--------
Sub CargaArchivo(Roll As inst, ByRef ubiroll As Integer)
' PRINCIPALMENTE CARGA ROLL Y LO PASA SIEMPRE A TRACK (0)
' los datos se carga ntambien a pmTk(0) el cual es comun a Roll y Track(0)

On Local Error GoTo saledeaca 
ntk=0 ' Cargar en 3 vectores Roll pmTk(0) y Track(0). 
' la carga de tracks aislada o la carga de una cancion con sus tracks
' ira aumentando ntk, es otra rutina no esta..aca se carga a 
' pmTk(0) y a Track(0)
'  carga roll
' al cargar borro todo el undo 
ResetVariables=TRUE   
abrirRoll=CARGAR_MAS_PISTAS_O_CANCION ' habilita la carga de una cancion
   mel_undo_k=0 :ig=0:cnt_acor=0

   Erase mel_undo, undo_acorde,undo_kant_intervalos
' FIN BORRO UNDO
   
   
'   Print #1,"ubiroll en CA ",ubiroll
     
     Dim As String x,x1,x2,x3,x4,x5,nombrea
     ''' no usar mas  dialogoText("Cargar Archivo")
     Dim myFil As String
      If ubiroll=0 Then
   nombrea = OpenFileRequester("","","Roll files (*.roll)"+Chr(0)+"*.roll"+Chr(0))
Sleep 100
         titulosTk(0)=nombrea
      Else
         If ubiroll > 0 Then
'           *file.lpstrFile=titulosTk(0)
'            fclose(file.lpstrFile)
            nombrea=Trim(titulosTk(0))
         EndIf      
      EndIf
'  nombrea="DESAFIO-LIGA-ACORDES-EN-1-NOTA.ROLL" ' para debug sino no funciona
'  nombrea="test-acordes-iguales01.roll"
'  nombrea="DESAFIO-LIGA-ACORDE-3-O.roll"
       If nombrea = "" Then
          Exit Sub
       Else
          nombre=nombrea   
       EndIf
   Print #1,"CArgarArchivo ",nombre    

   ca=6   
 ' print #1,"open nombre en CargaArchivo ",nombre

      If  ubiroll =0 Then
          If   Open (nombre  For Binary Access Read As #ca ) <> 0 Then
              Print #1, "No se puede leer  "; nombre 
          EndIf
      Else
         Sleep 100
         If Open (nombre  For Binary Access Read Shared As #ca ) <> 0 Then
            Print #1,"Error open cargaArchivo , nombre ",nombre
            Exit Sub
         End If
         
     ''    Open "temp.roll"  For Binary Access Read As #ca
         ubiroll=2
      EndIf
'cada dat tiene 7 bytes son 70 bytes, vamos a generar un type
' encabezado de 70 bytes para que sea compatible con los archivos actuales
' rolldat 
     Dim dat As rolldat

'     Dim zLim As dat
'     Dim z3   As dat  
'     Dim z4   As dat 
'     Dim z5   As dat 
'     Dim z6   As dat 
'     Dim z7   As dat 
'     Dim z8   As dat 
'     Dim z9   As dat 
'     Dim z10  As dat 
     
     

     'Print #1,"posicion lectura en ca ", Seek(ca)  
'     Seek ca,1
     Get #ca, , dat
     x1=Bin(dat.x1,4) 
     x2=Bin(dat.x2,4) 
     x3=Bin(dat.x3,4) 
     x4=Bin(dat.x4,4) 
     x5=Bin(dat.x5,4)
 
     tipoescala_num_ini =CInt( dat.tipoescala_num_ini) ' 20-12-2021 - tipoescala en uso
     If tipoescala_num_ini =0 Then
        tipoescala_num_ini =1
     EndIf
     Print #1,"tipoescala_num_ini ",tipoescala_num_ini ' 
     
     x=x1+x2+x3+x4+x5
        Print #1,"reconstruccion x pos bin ", x
     MaxPos=CInt("&B"+x) ' EN GRABAR ARCHICO MaxPos incluye a la ultima posoff + 6 y ahi esta el 182
     posicion = 1
       Print #1,"reconstruccion x pos int ", MaxPos
     ''' ditrvr oNroCol=MaxPos ' 11-02-2025     
     
     nota=0 '''notaOld
     inicioDeLectura=0' Int(Maxpos/NroCol)
     posn=Maxpos- 6  ' no  seria -6 ? decia -2 ahora ticks es -6 es donde termina el ultimo off
     ' maspos es esa posn +6 =MaxPos
     
     
     
     If posn < 0 Then posn=0 EndIf
     ' crgamos limites Roll de octavas
'carga en la pista ntk, si ntk es cero coincide y se puede cargar solo en cero
     desde  = CInt(dat.desde) 'Variable global
     Print #1,"desde ",desde
     hasta  = CInt(dat.hasta) ' global
     Print #1,"hasta ",hasta
     notaold= CInt(dat.notaold) 'global

     Print #1,"notaold ",notaold
     If dat.nanchofig > 0 Then
        If nanchofig > 0 And nanchofig < dat.nanchofig Then
        Else   
        nanchofig  = cdbl(dat.nanchofig)/10
        EndIf   
     EndIf     
     pmTk(ntk).vol = dat.vol
     notaescala_num_ini =CInt(dat.notaescala_num_ini) ' global notadeescala 20-12-2021
     If notaescala_num_ini = 0 Then
        notaescala_num_ini =1
     EndIf
     Print #1,"notaescala_num_ini ",notaescala_num_ini     
     If  dat.alteracion = 3 Then
         alteracion="sos" 'global
     EndIf    
     If  dat.alteracion = 2 Then
         alteracion="bem"
     EndIf 
     Print #1,"alteracion ",alteracion 'Global
    pmTk(ntk).eco = dat.eco
    Globaleco=dat.eco  
    pmTk(ntk).ejec =  dat.ejec 

    pmTk(ntk).pan = dat.pan
    Globalpan=dat.pan
    pmTk(ntk).coro = dat.coro
    Globalcoro=dat.coro
          
Dim mit As aUshort
'con estos dos campos puedo reconstruir tiempoPatron
 mit.tp1 = dat.tiempoPatron1
 mit.tp2 = dat.tiempoPatron2
 tiempoPatron = mit.st

     
     pmTk(0).desde=desde    ' pmtk
     pmTk(0).hasta =hasta
     pmTk(0).MaxPos =MaxPos
     pmTk(0).posn =posn
     pmTk(0).notaold=dat.notaold
     pmTk(0).alteracion=dat.alteracion
     pmTk(0).notaescala=dat.notaescala_num_ini
     pmTk(0).tipoescala=dat.tipoescala_num_ini
     pmTk(0).tiempopatron=tiempoPatron
     pmTk(0).portout=dat.portout
     pmTk(0).canalsalida = dat.canalsalida ' ubyte a ubyte
     pmTk(0).tipoCompas = dat.tipoCompas

     
Print #1,"/////--Cargar Arch pan Globalpan,pmtk, ntk ",Globalpan,pmTk(ntk).pan,ntk
     canalx=CInt(dat.canalsalida)
    cadenaes_inicial=""
   Print #1,"armarescala desde cargarArchivo"

    armarescala (cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini, alteracion ,1)

    tipoescala_inicial=escala(tipoescala_num_ini).nombre ' 13-01-2022 faltaba ini
' carga de escala en guia de escala
    guiaEscala(1).tipoescala=CInt(tipoescala_num_ini) '13-01-2022 faltaba ini
    guiaEscala(1).notaescala=CInt(notaescala_num_ini)
    guiaEscala(1).alteracion =CInt(dat.alteracion) ' 3 sostenido , o 2 bemoles
    guiaEscala(1).posicion=1

'''    tiempoPatron=CInt(zLim(1,1).inst) no alcanzaba

     If tiempoPatron = 0 Then 
        tiempoPatron = 240
     EndIf
' NB Y NA SE DEDUCEN no hace fakta guardarlos
     NB= 0 + (desde-1) * 13
     NA= 11 + (hasta -1)*13
' hay valores que los ajusto en ntk y en 0.. si ntk fuese distinto de cero
' si cargo un roll aisaldo es todo 0 y ntk=0 pero en una cancion si cargo
' un roll en una pista cargo a roll pero sus valores irarn en un track ntk
' de la pista correspondiente poque ntk sera >  0si estoy en una pista cualqueira de
' una cancion y para ello cargo en ntk de la pista > 0 y en ntk=0 para el
' roll grafico...Cada vez que una pista de camcion de ve en roll visual
' todos los valores de ntk  de esa pista se copian a ntk=0 ademas de los 
' datos de notas de la secuencia (ver TAB). Entonces aca al cargar si estamos
' en un atrack de cancion lso valores siemrpe van a dos lados ntk=0 y nrk=pista 
     pmTk(0).notaold = dat.notaold ''notaold  'notaold es ubyte en pmTk
     pmTk(0).MaxPos = MaxPos
Print #1,"pmTk(0).MaxPos = MaxPos ";  MaxPos
     pmTk(0).posn = MaxPos -6
' cargo estos campos nb na pero se los podria eliminar o usar para otra cosa,,,,
     pmTk(0).NB=NB  
     pmTk(0).NA=NA
     pmTk(0).desde = desde
     pmTk(0).hasta = hasta
     pmTk(0).posn = MaxPos -6

     'NB => 0 + (desde-1) * 13   ' 27 para 3
     'NA => 11 + (hasta-1) * 13  ' 90 para  7
     ' los 2 vectores deben tener el mismo tamaï¿½o ergo si CantTicks
     ' lo agrando trabajo tambien ? mmm no trabajo debe tener el tamaï¿½o del disco!
     ' cargamos trabajo datos
     If CantTicks - MaxPos < 2000 Then
        CantTicks = CantTicks + 2000 ' jmg 24-09-2021
     EndIf  
     pmTk(0).Ticks=CantTicks
     ntk=0
'' Redim Roll y TRack ya tiene calculo NB y NA con CantTicks y carga de pmTk.NB y NA
     Parar_De_Dibujar=SI
     Sleep 5
     CambiarDim(0) 
     Parar_De_Dibujar=NO
  

     canalx  = CInt(dat.canalx) ' canal midi cargado en Global si es un track se pasara a info de trck
     Print #1,"canalx cargado desde archivo ",canalx
     portout = CInt(dat.portout) 
     pmTk(0).portout=dat.portout   ' ubyte  a ubyte
     Print #1,"portout cargado desde archivo ",portout
    If portsout <= portout Then
 'portsout cantidad de port fisicos o dispositivos empieza por 1 2 3 etc"
  '  portout parte de 0 o sea 0 1 2 3 '
  ' un portout 2 significa 2 dispositivos 1 2 
  ' luego si portsout = 2  then portout maximo es 1
MessageBox( null, "MIDI-OUT "+ Str(portout)+ " INEXISTENTE CAMBIELO", "Se cambia a 1", MB_OK )
    portout = 0
    pmTk(0).portout=0
    pmTk(ntk).portout=0
   EndIf


     
     pmTk(0).canalsalida = dat.canalsalida ' ubyte a ubyte
     pmTk(0).portout=portout
     pmTk(0).pan = dat.pan
'-----PATCH ----- 14-04-2024
     pmTk(0).patch=dat.patch  ' ubyte a ubyte
     patchsal=dat.patch ' ubyte a ubyte
     instru= CInt(patchsal) ' integer ubyte


     TipoCompas= dat.TipoCompas 
     TCompas=Mid(tempoString(TipoCompas),1,4) ' string
    
     pmTk(0).tipoCompas = TipoCompas 
     d7=valorFigura(TipoCompas) ''===> ojo d7 se usaba en ROllMusic sin Ticks para dividir
' todo eso servia para cambiar de compas y cortar y liga notas con ticks no creo tenga sentido
' porque la division de compases en CreaPenta es ahora por ticks y si una nota pasa de compas
' la division de compas no se ve afectada solo depende de la cantidad de ticks definida para un compas
' 
'     Get #ca, ,z4  
'     Get #ca, ,z5  
'     Get #ca, ,z6  
'     Get #ca, ,z7  
'     Get #ca, ,z8  
'     Get #ca, ,z9  
'     Get #ca, ,z10 

     ReDim compas(1 To CantTicks)
     Print #1,"En CargarArchivo NB NA ", NB,NA
     Dim Trabajo (1 To MaxPos, NB To NA) As datsec     ''' es un Roll
 
     Get #ca, , Trabajo() ' aca levanta tambien el instrumento que se grabo
 
     ' CARGA VECTOR ROLL movemos los datos a Roll Y a track 0
     ' --------------------------------------------
     ' Roll y Track tendran 500 posiciones mas que trabajo el archivo de disco
     ' la carga se limita a MaxPos no deberi apasar nada...
     carga=1 ' no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi,i3,verticalEnOctavaVacia,octavaDeAcorde,vertical
     cont=0
 '    print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
Dim As Integer copiado=0

 vertical=12+(hasta-2)*13+hasta ' "[NROREP]" de EntrarTeclado

     For j = 1 To MaxPos    '11-07-2021
'      Print #2,"POSICION :";J
      'Print #3,"POSICION :";J
      Dim As Integer ip=0,Notapiano
      For i= NB To NA 
       Roll.TRK(j,i)  => Trabajo (j,i)    
' 06-09-2021 carga track en ntk=0 
      
                                                        ' 12 a 13   
       If Trabajo (j,i).nota >= 1 And Trabajo (j,i).nota <=13 And  Trabajo (j,i).dur >=1 And Trabajo (j,i).dur <= 202  Then
         ip+=1
         If ip >13 Then 
  '            print #1,"error  ip > 12,ip, Tr,j,i ",ip, Trabajo (j,i).dur,j,i
   '           print #1,"saltamos linea vertical vamos a otra posicion"
              Exit For
         EndIf      
         Notapiano= i 
         Notapiano= Notapiano - restar (Notapiano)
         ev1=ntk:ev2=j:ev3=Notapiano:ev4=ip ' CONTROL ERRORES 
         Track(ntk).trk(j,ip).nota => CUByte(Notapiano)
         Track(ntk).trk(j,ip).dur  => CInt(Trabajo (j,i).dur)   
         Track(ntk).trk(j,ip).vol  => Trabajo (j,i).vol
         Track(ntk).trk(j,ip).pan  => Trabajo (j,i).pan       
         Track(ntk).trk(j,ip).pb   => Trabajo (j,i).pb
         Track(ntk).trk(j,ip).nnn => Trabajo (j,NA).inst 'inst en todos lados???
         'Print #fs,"notapiano,ntk", Track(ntk).trk(j,ip).nota,ntk,Track(ntk).trk(j,ip).dur
       EndIf
' copia datos de escala, esta repetido en cada octava pero bueno....con una sola
 ' copai estarai bien,,,,

      If Trabajo(j,i ).dur= 200  And copiado=0 Then ' solo en 13 copia 1 sola vez ok sigue igual
         
         Track(ntk).trk(j,13).nnn = Trabajo(j,i ).inst ''=CUByte(tipoescala)
         Track(ntk).trk(j,13).vol = Trabajo(j,i ).vol ''= CUByte(notaescala)

         Track(ntk).trk(j,13).nota = Trabajo(j,i ).nota ' = 30
         Track(ntk).trk(j,13).dur  = Trabajo(j,i ).dur  ' = 200
         Track(ntk).trk(j,13).pan = Trabajo(j,i ).pan
   '      Print #1,"RollaTrack copia var de control inst ",Track(ntk).trk(j,13).inst
   '      Print #1,"RollaTrack copia var de control vol ",Track(ntk).trk(j,13).vol
         copiado=1 
      EndIf
' copiar campso de acordes a Track(ntk)       

      If Trabajo(j,i ).pb = 201  Then ' info acorde en una octava es el 202 de Roll
      ' vacio= 12 +(estoyEnOctava-1)*13 son las que no se ven 
      '       i1= 12 + (octavaDeAcorde -1)*13 ergo
         octavaDeAcorde=1+ (i-12)/13 ' nro octava de roll empieza por  cero 0 
         i3=13 + octavaDeAcorde 
         verticalEnOctavaVacia= 12 + (hasta-2)*13 + octavaDeAcorde - desde ' 90 + 6 - 4=92
         Track(ntk).trk(j,i3).vol  = Trabajo(j,verticalEnOctavaVacia).vol ' octava
         Track(ntk).trk(j,i3).nota = Trabajo(j,verticalEnOctavaVacia ).nota ' Rollnota
         Track(ntk).trk(j,i3).dur  = Trabajo(j,verticalEnOctavaVacia ).dur  ' acordeNro
         Track(ntk).trk(j,i3).pb  = Trabajo(j,verticalEnOctavaVacia ).pb  ' hay acorde 202          
      EndIf

      If Trabajo(j,vertical ).nota = 210 Or Trabajo(j,vertical ).nota = 211 Then ' repeticiones [:  n:]
      ' vacio= 12 +(estoyEnOctava-1)*13 son las que no se ven 
      '       i1= 12 + (octavaDeAcorde -1)*13 ergo
         Track(ntk).trk(j,21).nota = Trabajo(j,vertical ).nota '
         Track(ntk).trk(j,21).vol = Trabajo(j,vertical ).vol ' 
      EndIf


      Next i
' SE CREA EL CAMPO ACORDE DE POLI DEL TRACK
      Track(ntk).trk(j,1).acorde=ip
     ' Print #fs, "acorde ",Track(ntk).trk(j,1).acorde
     '''  crearsecuencia(Track(), j, ntk )
     Next j

' falta ver que pasa con Track con el tempo....
If Roll.trk(1,NA).inst > 0 Then ' convertimos 
   patchsal=Roll.trk(1,NA).inst
   dat.patch=Roll.trk(1,NA).inst
   Roll.trk(1,NA).inst=0
   instru=CInt(dat.patch)
   pmTk(0).patch=patchsal

Else
  If dat.patch > 0 Then
    patchsal=dat.patch
    instru=CInt(dat.patch)
    pmTk(0).patch=patchsal
  EndIf
EndIf

'If Roll.trk(1,NA).onoff =1 Then
'   pmTk(ntk).ejec=1
'   pmTk(0).ejec=1
'   dat.ejec=1
'   Roll.trk(1,NA).onoff=0
'EndIf

Dim tip As aUshort
Dim As Integer tiempoPatronNA
'con estos dos campos puedo reconstruir tiempoPatron
 tip.tp1 = Roll.trk(1,NA).pan
 tip.tp2 = Roll.trk(1,NA).pb
 tiempoPatronNA = tip.st


If  tiempoPatronNA > 0  Then
  dat.tiempoPatron1=Roll.trk(1,NA).pan 
  dat.tiempoPatron2=Roll.trk(1,NA).pb 
  Roll.trk(1,NA).pan=0  ' borramos para convertir
  Roll.trk(1,NA).pb=0
  pmTk(ntk).tiempopatron=tiempoPatronNA
  pmTk(0).tiempopatron=tiempoPatronNA 
EndIf
 


If Close (6) <> 0 Then
  Print #1, "NO SE CERRO EL ARCHIVO CARGADO "; nombre
Else
  Print #1, "Bien CERRADO EL ARCHIVO CARGADO "; nombre
EndIf
Sleep 1000,1 
nroCompas=0
tres=0:pun=0:silen=0:mas=0:doblepun=0:cuart=0
tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
DUR => 0
curpos =>1
''' carga=0
   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
   gap1= anchofig* 6 ' porque era tanto 20
   NroCol =  (ANCHO / anchofig ) + 4 '6
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default

  RecalCompas()  
'--------------------------------
' correccion LLAMAMOS ROLL A TRACK PARA NTK = 0, 14 -04-2024
' pero no era necesario, total al grabar nunca grabo el track
' segraba roll a track por si fue modificado..
' lo importante era revisar que todos los pmTk(0) sean cargados
' desde Track y pasados al  programa porque el encabezado de roll
' y track en el archivo son diferentes y la forma de pasarlo es por 
' memoria usando como intermediario pmTk(0),,al modificar debo modificar 
' el pmTk(0)... y al grabar pasar de pmTk(0) a encabezado de Roll
''' NO HACE FALTA ->>> RollaTrack (Track(), 0,Roll)

' SI UNA CANCION ESTA CARGADA Y EDITO UN TRACK n, ESE TRACK ESTA 
' EN ROLL PERO NO EN TRACK(0) PORQUE NO LO CARGUE, AL GRABAR
' DEBO USAR ROLL A TRACK AJUSTANDO EL NRO DE TRACK, Y DEBE SER UNA
' SOBREESCRITURA DEL TRACK N

' 05-03-2024 estos toques virtuales los sacare por ahora  
           'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0  
          'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0
 'print #1, "CargaARchivo LBound (Roll.trk,1) ",LBound (Roll.trk,1)
 'print #1, "CargaARchivo LBound (Roll.trk,2) ",LBound (Roll.trk,2)
    
 'print #1, "CargaARchivo UBound (Roll.trk,1) ",UBound (Roll.trk,1)
 'print #1, "CargaARchivo UBound (Roll.trk,2) ",UBound (Roll.trk,2)
  Sleep 10
  ROLLCARGADO=TRUE
NADACARGADO=FALSE
nVerCifradoAcordes=3 'POR OMISION MUESTRA ACORDES AL DAR CLICK EN UNA OCTAVA

Exit Sub

saledeaca:
 Dim errmsg As String
If  Err > 0 Then
  Close (6)
  Sleep 1000,1  

  errmsg = "FAIL Error CargarArchivo " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
  
  
End If

End Sub
' ------------------------------------------

'
Sub Nuevo(Roll As inst,borraNombre As Integer)
CantTicks=86400 ' vuelvo al original default 15 min *60*96
' borro todo el undo
   mel_undo_k=0 :ig=0:cnt_acor=0
   Erase mel_undo, undo_acorde,undo_kant_intervalos
' FIN BORRO UNDO
nota=0
   dur=0
' se inicia el vector desde 4 hasta 8, se puede achicar las octavas ok.
' si ya se habian achicado y quiero agrandar, no se puede, debo borrar el vector.
' En la cantidad de posiciones no hay porblema en ambas direcciones. 
          NB => 0 + (desde-1) * 13   ' 39 para 4
          NA => 11 + (hasta-1) * 13  ' 102 para  8
       '   If desdevector > desde Or hastavector < hasta Then ' si se amplia el rango
               Print #1,"entro a redimiensionar vector por octavas"
               Erase  Roll.trk, Track(ntk).trk, RollAux.trk, mel_undo,undo_acorde,undo_kant_intervalos
                ig=0:cnt_acor=0  
               desdevector= desde
               hastavector= hasta
               Print #1,"Runtime desde hasta ",desde ,hasta
               Print #1,"Runtime NB NA ",NB ,NA
               ReDim (Roll.trk ) (1 To CantTicks, NB To NA )
               ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               ReDim compas(1 To CantTicks)
               ReDim (RollAux.trk) (1 To CantTicks, NB To NA)
               mel_undo_k=0:ig=0:cnt_acor=0
               Erase undo_kant_intervalos,undo_Acorde
               posn=0
     '     Else
'
'               ReDim Preserve (Roll.trk ) (1 To CantTicks, NB To NA )
'               ReDim Preserve compas(1 To CantTicks)
'               ReDim Preserve (RollAux.trk) (1 To CantTicks, NB To NA)
'               ReDim preserve (Track(ntk).trk ) (1 To CantTicks,1 To lim3)           
'          EndIf

          MaxPos = 2 'NroCol
          posicion=0
          curpos=0 
          notacur=0 ' 10-04-2022
          nroCompas=0
          InicioDeLectura=0
          tres=0:pun=0:silen=0:mas=0:doblepun=0:cuart=0
          tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
          SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
          
          If borraNombre > 0  And NombreCancion = "" Then 
             nombre = ""
             Dim i As Integer
             For i=1 To 32
               titulosTk(i)= "" ' borro toda la lista si habia de cancion anterior
               pistasTk(i)="" 
             Next i
             tope=0
          EndIf
          If borraNombre > 0  And NombreCancion > "" Then
            ' tope=tope +1
            ' ntk=tope
             ' sin importar de cancion queriendo grabar un roll en cancion
             ' jmg 23-09-2021 
             nombre = ""
             ntk=ntk+1
          EndIf
          Print #1,"posn,CantTicks ",posn,CantTicks
          Print #1,"NB, NA ",NB,NA
          
       '   Dim As Integer r1,i
       '   For i=NB To NA 
       '     For r1= posn To CantTicks ' POSN +1 TO POSN 24-09-2021 JMG
       '         Roll.trk(r1,i).nota=0
       '         Roll.trk(r1,i).dur=0 
       '      Next r1 
       '  Next i           
param.Roll = Roll ' por esto reventava al variar octavas se debecargar
Tracks (ntk , 1,Roll)
' de nuevo Roll que quedo en rolldec ahora 405
End Sub
' 
'
Sub menugrafico( c0 As cairo_t Ptr,Cantitems As Integer, text() As String  )
'c0 es la superfice de todo roll o c en rolloop
 If savemousex > 0 Or savemousey > 0 Then
  usamousex=savemousex
  usamousey=savemousey
 Else
  usamousex=mousex
  usamousey=mousey
 EndIf
 
 Var cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )

 Dim As cairo_text_extents_t extents

 '   cairo_move_to (c0 , usamousex, usamousey + 250)
 '  SetMouse 100,200
 '    If mousey > 50 Then
 cairo_set_font_face( c0, cface )
 cairo_set_font_size( c0, 24 )
 cairo_set_source_rgba( c0, 1, 1, 1, 1 )
 Dim i As Integer
 usamousex = usamousex + 200
 usamousex = usamousey + 100


 For i= 1 To CantItems
  cairo_move_to( c0, 100  , 100 + (i-1)*24 )
  cairo_text_extents( c0, text(i), @extents )
  cairo_show_text( c0, text(i) )
 Next i


 ' <= control-m O P
 '          If cursorVert=0 Or cursorHori=0 then
 '             cairo_move_to( c, usamousex -60 , usamousey +50 )
 '             cairo_text_extents( c, text(9), @extents )
 '             cairo_show_text( c, text(9) )
 '          EndIf
 '          If cursorVert=1 Or cursorHori=1 then
 '             cairo_move_to( c, usamousex -60 , usamousey -40 )
 '             cairo_text_extents( c, text(10), @extents )
 '             cairo_show_text( c, text(10) )
 '          EndIf

 '     menumouse = 0
 '  EndIf

 ' EndIf

 If savemousex=0 Or savemousey=0 Then
  savemousex=mousex
  savemousey=mousey
 EndIf
Sleep 100

End Sub

'-------------------------------MENU----------
Sub menu (c0 As cairo_t Ptr, c As cairo_t Ptr,n As Integer,menuNro As Integer, Roll As inst, ByRef ubiroll As Integer, ByRef ubirtk As Integer )

 On Local Error Goto fail

 Dim As float fonti
 fonti=18 ' ACA AHCE FALTA ESTE TAMAnO TODO ESTA CALCULADO EN PIXELS PARA QUE ANDE EL MENU
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size
 Dim  As Double Ptr xo,yo
 Dim As Double nxo,nyo 
 'Dim cd As cairo_device_t Ptr
 'Dim cbool as cairo_bool_t
 ' pinta celeste totodo surface 2
 ''cairo_set_source_rgba c, 0.6, 0.7, 0.8, 1
 ' cairo_set_source_rgba(c, 0.6, 0.5, 0.6, 1) ' morado
 If   ix > 2   Then ' instancia independiente
   cairo_set_source_rgba(c, 0.3, 0.3, 0.3, 1) 'fondo gris de la cinta independiente 
 Else 
   cairo_set_source_rgba(c, 0.3, 0.3, 0.6, 1) 'fondo azulada de la cinta esclava
 EndIf 
 cairo_paint(c)

 cairo_set_source_rgba(c, 1, 1, 1, 1) ' blanco letras

 cairo_select_font_face (c, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
 cairo_set_font_size (c, fonti)

 '---encendido de edit
 Var t = ""
 cairo_font_extents (c, @fe)
 cairo_text_extents (c, t, @te)

 cairo_move_to(c, 0, 30 ) ' aca esta la cosa la ubicacion del font!!!
 ' debo ubicarlo caracter por caracter o posicion no en string!!!

 Dim oclog As Integer
 oclog = 8 - (estoyEnOctava-1) ' jmg ojo hay un hardcodeo ï¿½?
 'If COMEDIT = TRUE Then
 '  cairo_set_source_rgba c, 0, 1, 0, 1
 '  menuNew=2
 'Else
 '  cairo_set_source_rgba c, 1, 1, 1, 1
 'EndIf

 'cairo_move_to(c, 0, 30 )
 'If ubiroll > 0 Then
 '   CargaArchivo(Roll,ubiroll)
 '   ROLLCARGADO=TRUE
 '   MenuNew=0
 '   ubiroll=0
 'EndIf
 
 
 
 If menuNew <> menuNro Then
  menuNro=menuNew
 EndIf
 ' aca hay un comportamiento raro. por mas q menunro lo ajuste a 3
 ' para el caso 0, siempre queda en 0 , debï¿½ usar menunew una seguna variable
 ' y asi funciona ..(en ningun lado se ajusta a cero salvo al inicio fuera del
 ' loop.....
 t=" "
   Dim As String muestra = " " ' nombre tambien se usa en cancion
   If NombreCancion > "" And titulosTk(ntk) > "" And ROLLCARGADO=FALSE Then
      nombre = titulosTk(ntk)
     ' Print #1,"en menu nombre ",nombre
   EndIf
   Dim ps As Integer
   If NombreCancion > "" Then
      ps=InStr(nombre,"[")
      muestra=Mid (nombre,ps) + " "
   Else
     ps=InStrRev (nombre,"\") 
     muestra =Mid(nombre,ps) +" "
     camino= Mid (nombre,1,ps-1) + " "
     SetWindowText(hwndC, "RollMusic Path archivo: " + camino)
   EndIf
   
   muestra=Str(PianoNota) + " "+ muestra   + "ANCHO "+ Str(ANCHO)+ " Mx:" + Str(mousex) + " My:" + Str(mousey) +   " NroCol "+Str(NroCol) 
'xxxx Str (BordeSupRoll)

' //////////////////////////////////////////////////////////////////
  If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf
 
 Select Case menuNro
' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  Case MENU_INICIAL '0
  ' 0.1 ==> [Archivo]
   If mousex > 46 And mousex< 111 And mousey < 50   Then
    cairo_set_source_rgba c, 0, 1, 0, 1 ' verde
     ''font=18 ' el menu esta diseï¿½ado para este tamaï¿½o 
    If MouseButtons And 1 Then
     menuNew = COMANDOS_ARCHIVO  ''
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1

   EndIf
   carga=0
   t = "       [Archivo]":cairo_show_text(c, t)

'  0.2 ===> [Edicion]
   If mousex > 141 And mousex< 203 And mousey < 50 Then
    cairo_set_source_rgba c, 0, 1, 0, 1
    '''If MouseButtons And 1 Then
    If MouseButtons And 1 Then  ' funciona mejor ? veremos...
     menuNew=PARAMETROS_ROLL
     cierroedit= 0 ' habilita EDIT de nuevo
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   ' print #1, "INVESTIGO COMEDIT ENTRO X CLICK EN SUB S3, menuNro: ",S3, menuNro
   t=  " [Edicion]":cairo_show_text(c, t)

' 0.3 ===> [Ver] pondremos aca ver una posicion dada de la secuecnia
' entrando el numero.
   If MultiKey(sc_Control) And (mousex > 232 And mousex< 258 And mousey < 50) Then
     cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menuOldStr="[Ver]"
        ' Print #1,"voy a menu 8 desde ver"
         menuNew=PARAMETROS_SECUENCIA '8
      EndIf 
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Ver]":cairo_show_text(c, t)
' 0.4 ====> [Pista]
   If mousex > 286 And mousex< 322 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Pista]":cairo_show_text(c, t)
   ' ------------------------------------------
' 0.5 ===> [Reproducir]
   If MultiKey(sc_Control) And (mousex > 355 And mousex< 451 And mousey < 50 And play=NO And playb=NO And Cplay=NO   ) Then
    cairo_set_source_rgba c, 0, 1, 0, 1
     While InKey<>"" :Wend
   '   print #1,"--->entra a reproducir thread1 "
        menuNew =MENU_INICIAL
        cierroedit= 0
        ' Print #1,"hizo click para llamar a playall, maxpos ",maxpos 
' 28-07-2021 probamos Mutex con ThreadCall , anduvo bien lo dejo veremos        
        If  MaxPos > 2 Then
               GrabarPenta=0:naco=0:naco2=0
               If instancia=ARG7_NOMBRECANCION Or instancia=ARG107_FICTICIO  Or instancia < ARG3_TITU  Then
               Else
               SetGadgetstate(15,0) ' 10-04-2022
               EndIf  
        ' Print #1,"llama a playall"
         thread2 = ThreadCall  playAll(Roll)
         ''''playAll(Roll)
         play=SI         
        EndIf 
   '     print #1,"--->paso llamada a thread1 "
    ''    menuNew = 4 por hora sin elecciones

   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
  ' If CPlay=1 Then
  '    If  MaxPos > 1 Then
  '       thread1 = ThreadCall  playAll(Roll)
  '       ''playAll(Roll)
  '    EndIf 
  '    CPlay=0
   'EndIf
   t=  " [Reproducir]":cairo_show_text(c, t)
   ' --------------------------
 '0.7  ===> [Opciones]  '25-07-2021
   If  MultiKey(SC_ALT) And mousex > 483 And mousex< 561 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
        Sleep 20
        menuNew=OPCIONES_MENU '5
        
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Opciones]":cairo_show_text(c, t)

' 0.8 ===> [Ayuda]
   If MultiKey(sc_Control) And mousex > 590 And mousex< 638 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
      Shell ("start notepad " + pathinicio + "\ayuda.txt")
    End If
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
   t=  " [Ayuda] "+ muestra :cairo_show_text(c, t)
   cairo_stroke(c)

  '' VUELVE menu grafico Rollmusic cinta AL INICIO....
  If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf



' *********************************************************************************
  Case 1
  '1.1 ELIMINADO SU USO 
   
   t=  "      <MENU=(<=|=>)>/<VENTANA=(MOVER=DRAGAR CINTA)(POSICON NORMAL,SUBE/BAJA BORDE INFERIOR=F7-F8)> "
' ****************************************************************************************   
  Case PARAMETROS_ROLL ''2 ' para posicionar  en pantalla NO BORRAR. <= EDIT MENU
 
   menuNew=PARAMETROS_ROLL
   
   cairo_move_to(c, 0, 36 )
   If COMEDIT<>LECTURA   Then
    cairo_set_source_rgba c,0,1,0,1
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   Dim y1 As Integer => 1 + (desde-1) * 13
   Dim y2 As Integer => 1 + (hasta-1) * 13
   
'   If COMEDIT=TRUE Then ' jmg 02-06-2021 ver porque difieren
'     nR=nR-3
'   End If
' indice nota es posicion 
indicePos=(mousex- gap1 )/anchofig + posishow 
Dim As Integer veonota, veocopi 
' indicePos < MaxPos fix 02-09-21
'If indicePos >gap1 And nR <=(NA -13) And indicePos < MaxPos Then
'   veonota=Roll.trk(indicePos, nR).nota ' NA-nr cambiado a nR jmg 
'EndIf
If copiar >0 Then
   veocopi=copiar
EndIf   
If copi >0 Then
   veocopi=copi
EndIf   

If parametros=0  Then
   t = "      EDIT" + " " +  " Maxp:" + _
   Str(Maxpos) +" Psi:" + Str(posicion)+ " Psn:"+Str(posn)+ _
   " Fnt:" +Str(font) + " Oct:" + Str(estoyEnOctava) + _ 
   " PN:"+ Str(PianoNota) + " nsE:"+Str(nsE) + " Curpos:" +Str(curpos) + _
   " RollDur :"+ Str(RollDur) + " Rollnota:" + Str(RollNota) + _ 
   " Copia:" + Str(veocopi) +" Col:" +Str(indicePos) + " DUR:"+ Str(Dur) + _ 
   " Nota:"+ Str(nota) + " ONOFF:" + Str(onoff) + " ACOR:" + Str(acordeNro)
EndIf
If parametros=1 Then
   t = "      EDIT" + " " + " Curpos:" +Str(curpos) + _
   " RollDur :"+ Str(RollDur) + " Rollnota:" + Str(RollNota) + _ 
   " modifmouse:" + Str(modifmouse) + " usamousey:" + Str(usamousey) + " usamousex:"+ Str(usamousex) + _ 
   " Mx:" + Str(mousex) + " My:" + Str(mousey) 

EndIf
'''" Mx:" + Str(mousex) + " My:" + Str(mousey) 
  ' + "Desde:" + Str(y1) + "Hasta:" +Str(y2)
   /' " InicioDELectura: " + Str(InicioDelectura) + " BordeSupRoll: " + Str(BordeSupRoll) +
'/
   '''   " modifmouse:" + Str(modifmouse) + " usamousey:" + Str(usamousey) + " usamousex:"+ Str(usamousex) + _
'" nA:"+Str(veonota)
   cairo_show_text(c,t)

   cairo_stroke(c)

   menuNew=PARAMETROS_ROLL
  ' ============================================================= 
  ' BORRAR HACIA Derecha y Ajustar MaxPos a la posicion actual en Edit Lectura
  '====================================================== 
  If mousex > 91 And mousex < 174  And mousey < 50 Then ' <= MaxPos ajuste a la posicion dada
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MultiKey(SC_ALT ) Then ''And  dobleclick Then
            MaxPos=posishow +1
            posn=posishow        
            Dim As Integer r1,i
            For i=NB To NA 
              For r1= posn+2 To CantTicks
                 Roll.trk(r1,i).nota=0
                 Roll.trk(r1,i).dur=0
                 Roll.trk(r1,i).vol=0
                 Roll.trk(r1,i).pan=0
                 Roll.trk(r1,i).inst=0 
              Next r1 
            Next i 
        
      EndIf
  Else
    cairo_set_source_rgba c, 1, 1, 1, 1    
  EndIf  
   '' VUELVE menu grafico Rollmusic cinta AL INICIO....
  If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf
  
'*******************************************************************
  Case COMANDOS_ARCHIVO '3  ==========================> caso 3
   '   t = " [NUEVO] [ABRIR] [GRABAR] [GRABAR COMO] [EXPORTAR] [CERRAR] [SALIR] "

   '===> 3.1 [NUEVO]
   If mousex > 42 And mousex < 110  And mousey < 50  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
    
        If MultiKey(SC_ALT) And MouseButtons=1 Then
         'If dobleclick Then
            Nuevo(Roll,1)
            Sleep 50
            MenuNew=MENU_INICIAL
            cierroedit= 0
         'EndIf
         
      EndIf 
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = "      [NUEVO]":cairo_show_text(c,t)

   '===> 3.2 <ABRIR>[*.ROLL] abre roll y automaticamente lo pone
   ' en trak 0 ntk=0
  '14-04-2024 NO LO ESTABA COPIANDO A TRCK 0 SOLO SUS PARAMETROS
' EN pmtk(0)...corregido en sub cargaarchivo
   If (mousex > 215 And mousex < 269  And mousey < 50 And carga=0 ) Then ' <= ABRIR lee grabaroll.roll
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And carga = 0  Then
      If play=SI Or playb=SI Then
       PARAR_PLAY_MANUAL=SI ' DETIENE EL PLAY VEREMOS
       playloop=NO:playloop2=NO
       Sleep 2
      EndIf 
       CargaArchivo(Roll, 0)
       s5=0 '11-06-2022
       cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
    '   print #1,"nombre cargado en Roll ",nombre
       MenuNew=MENU_INICIAL
       cierroedit= 0
       carga=1  ' <======= control de Carga
       ROLLCARGADO=TRUE ' aunque este en cancion puedo cargar un roll
    EndIf
   Else
   ' print #1,"else de *ROLL CARGA, mousex ", carga, mousex, mousey
    carga=0   
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = " ABRIR<[*.ROLL]":cairo_show_text(c,t)

   '===> 3.2.1 [*.RTK]


'  =====< CARGA UN TRACK EN NTK=0 Y LO PASA A ROLL PARA EDITAR >=======
   If (mousex > 300 And mousex <  345  And mousey < 50 And carga=0 )  Then ' <= ABRIR lee grabaroll.roll
     If NombreCancion > "" Then
     Else
      ntk=0  
     EndIf 
      cairo_set_source_rgba c, 0, 1, 0, 1
      If (MouseButtons And 1 And carga=0) Then
         If play=SI Or playb=SI Then
            PARAR_PLAY_MANUAL=SI ' DETIENE EL PLAY VEREMOS
            playloop=NO:playloop2=NO
            Sleep 2
         EndIf 

         'print #1,"carga track veo nombre antes   ",nombre
         CargarTrack (Track() , ntk, 0)
         s5=0 ' 11-06-2022
         
         If nombre > "" Then  ' evita crash al cancelar el dialogo 
         '   print #1,"carga track veo nombre despues ",nombre
            ROLLCARGADO=TRUE ' NO PERMITOQUE TRACK(0) SE BORRE
            TrackaRoll (Track() , ntk , Roll,"menuGraficoCargaTrack")
          '  print #1,"TrackaRollcarga rtk veo nombre ",nombre 
            RecalCompas ()
          '  print #1,"despues RecalCompas veo nombre ",nombre
            MenuNew=MENU_INICIAL
            cierroedit= 0
            carga=1 ' <======= control de Carga 
            TRACKCARGADO=TRUE
            ROLLCARGADO=FALSE
            cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
         EndIf

     Else
         TRACKCARGADO=FALSE    
     EndIf
   Else
    carga=0
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = " [*.RTK]":cairo_show_text(c,t)

   '===> 3.2.2 [CARPETA]
   ' para cargar cancion o tema habia sido proyectado, podria usarlo
   ' para que una instancia pueda cargar una cancion.....no porque no corre la otra logica
   If MultiKey(sc_Control) And mousex > 378 And mousex <  459  And mousey < 50 And carga=0  Then ' <= ABRIR lee grabaroll.roll
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And carga = 0 Then
      ' GRABAR A MIDI PLANO aca no funciona
'        Print #1,"1) A MIDI PLANO DESDE MENU ROLL"
        ''''abrirSecuencia(20)
 '       midionof = 4
 '       Print #1,"2) A MIDI PLANO DESDE MENU ROLL"
 '       MIDIFILEONOFF = HABILITAR 
 '       AMIDI=1
  '     Print #1,"3) A MIDI PLANO DESDE MENU ROLL" 
    EndIf
    MenuNew=MENU_INICIAL
    cierroedit= 0
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
    carga=0  ' <======= control de Carga
   EndIf
   t = " [ ----- ]>":cairo_show_text(c,t)

   
  '===> 3.3[GRABAR] 
' graba el roll a roll y el track a track..
   If (MultiKey(sc_Control) And mousex > 450 And mousex < 530  And mousey < 50) And carga=0 Then ' <=
    cairo_set_source_rgba c, 1, 0.5, 0, 1
     If MouseButtons And 1    Then
      cairo_set_source_rgba c,1,0,0,1
       Dim As String nombreg
       Print #1,"EN Grabar nombre ",nombre
       If nombre = "" And titulosTk(ntk) = "" Then 
 nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0)+"*.roll;*.rtk"+Chr(0), OFN_CREATEPROMPT)
 Sleep 100
          If nombreg = "" Then
             Exit Sub
          Else
             nombre=nombreg   
          EndIf
       EndIf

       Dim As Integer ubi1=0,ubi2=0,ubi3=0,ubi4=0 
       Dim As String no1,no2
       ubi1=InStr(nombre,"[")
       ubi2=InStr(nombre,"]")
       ubi3=InStr(nombre,".rtk")
       ubi4=InStr(nombre,".roll")

       If ubi1 >0 And ubi2 > 0 And ubi3 > 0 Then 'graba un track como track
       ' pero como las modificaciones estan en ROLL debo usar GrabarrollaTrack
  Print #1,"2190 menu pierde ntk ??? ",ntk
'SI CARGUE UN RTK DESDE ARCHIVO SIEMPRE USO ROLL A  TRACK PORQUE
' SE PUDO EDITAR EL ROLL VISUAL Y ROLL TENDRA LA ULTIMA MODIFICAION
' ahora si es una cancion debere grabar a disco con el nrode track correspondiente
' al track modificado porque el ntk=0 para roll. ademas debere actualizar el track ntk
' en memoria,,,son dos operaciones,,,
          GrabarRollaTrack(0) 
    ''kiki     cairo_set_source_rgba c,1,1,1,1
         cairo_set_source_rgba c, 0, 1, 0, 1
           Sleep 500,1           
       Else ' graba roll como roll, si se desea eso solo sacar [xx] del nombre
           ''''GrabarRoll ()
           LLAMA_GRABAR_ROLL()
        cairo_set_source_rgba c, 0, 1, 0, 1 
           Sleep 500,1 
       EndIf
         carga=1
         lockip=2 ' habilita mouse 28-08-2021
        MenuNew=COMANDOS_ARCHIVO  '  sera aca ? jjjj decia menunew=0
       
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = "[GRABAR]": cairo_show_text(c,t) :t= ""

'-----------------------------
  '===> 3.3.1 [ROLL=>RTK] SC_F11 COPIA 
 ' CONVIERTE DESDE UN ROLL CARGADO O DE DISCO DEBE ESTAR NOMBRE <> "" 
   If MultiKey(sc_Control) And mousex > 570 And mousex < 700  And mousey < 50  And carga=0 Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
    Print #1,"ENTRO A ROLL A TRACK"
     If MouseButtons And 1 Then
       Print #1, "Click Grabando a disco Roll a Track ",nombre
      '''' dialogoText("Grabar Archivo")
       Dim As String nombreg
       If nombre = "" Then
 nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0)+"*.roll;*.rtk"+Chr(0), OFN_CREATEPROMPT)
 Sleep 100
          If nombreg = "" Then
            Print #1,"NOMBREG VACIO "
             Exit Sub
          Else
             nombre=nombreg   
          EndIf
       EndIf
''''       grabaprueba()
       Dim As Integer esroll, esrtk , esejec 
       esroll=InStr(LCase(nombre),".roll")
       esrtk =InStr(LCase(nombre),".rtk") 
       esejec=InStr(LCase(nombre),".ejec") ' es unroll que viene de ejec solo en el nombre
' no es que termine en ejec, el nombre del rool contine ejec en el nombre       
'   GrabarRollaTRack aca graba con [00] adelante, pues no es comando de cancion
' la pista de cancion van de 1 a 32 la pista 00 es el track asociado a Roll sin cancion 
        If esrtk >0 Then ' update de un track con o sin cancion
           GrabarRollaTrack(0)
        EndIf  
        ' convierte roll a track comun le abtepone [00] y rtk al final
        If esroll > 0 Or esejec >0 Then ' con o sin cancion
           GrabarRollaTrack(1) ' el 0 no lo toma es indiferente
        EndIf
        MenuNew=MENU_INICIAL  ' por formato de TRack anteponiendo[0]
        cierroedit= 0
        carga=1
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf


   t = " [ROLL=>TRK(0)]" : cairo_show_text(c,t) :t= ""

  '===> 3.3[GRABAR COMO] SC_F11 COPIA 
   If MultiKey(sc_Control) And mousex > 720 And mousex < 850  And mousey < 50  And carga=0 Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
     '       print #1, "Grabando a disco Roll GRABAR "
       '''''no usar dialogoText("Grabar Archivo")
       Dim As String nombreg
nombreg = OpenFileRequester("","","Roll files (*.roll, *.rtk)"+Chr(0)+"*.roll;*.rtk"+Chr(0), OFN_CREATEPROMPT)
Sleep 100
       If nombreg = "" Then
          Exit Sub
       Else
         nombre=nombreg   
       EndIf
        
       ''GrabarRoll()
       LLAMA_GRABAR_ROLL()
       Sleep 1000,1   
          lockip=2 ' habilita mouse 28-08-2021
       MenuNew=MENU_INICIAL
       cierroedit= 0
       carga=1   
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = " [A OTRO ROLL]" : cairo_show_text(c,t) :t= ""


 '  If mousex > 941 And mousey < 50 Then
 '   If MouseButtons And 1 Then
 '    MenuNew=0
 '   EndIf
 '  EndIf
   '' VUELVE menu grafico Rollmusic cinta AL INICIO....
  If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf
  

  Case 4  ' <======= COMO REPRODUCUIR
   cairo_set_source_rgba c,1,1,1,1
   t=  "      [Desde Inicio][Desde Esta Posicion] [Volumen / + / - /] No habilitado"
'----
   If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
       MenuNew=MENU_INICIAL
       cierroedit= 0
   EndIf
   
'------------------------------------------------------
  Case OPCIONES_MENU '5  OPCIONES MENU, [COMPAS] [OCTAVAS] [TEMPO] [CONFIGURACION]

  If mousex > 38 And mousex < 120 And mousey < 50  Then  'COMPAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew= TIPO_DE_COMPAS
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [COMPAS]":cairo_show_text(c, t)
  If mousex > 130 And mousex < 235 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=OCTAVAS
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [OCTAVAS]":cairo_show_text(c, t)
  If mousex > 260 And mousex < 329 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuOldStr="[TEMPO]"
     
     menuNew=PARAMETROS_SECUENCIA
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [TEMPO]":cairo_show_text(c, t)
 '
  If MultiKey(sc_Control) And mousex > 354 And mousex < 514 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=CONFIGURACION ''9
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf

   t=  " [CONFIGURACION]": cairo_show_text(c, t)
 cairo_stroke(c)
 
'----------------------------------------------------------------------
  Case AJUSTE_OCTAVAS '6 OPCIONES => OCTAVAS
   
' ===> 6.1 [OCTAVAS DESDE   - / + ] 
   If mousex > 38 And mousex< 123 And mousey < 50  Then ' OCTAVAS LABEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1  Then
     octavas=0
     Dim Cantitems As Integer => 12
     Dim As String text (1 To 12) => { _
     "Click En 'Desde' antes de cada click en - / + ", _
     "Click En 'Hasta' antes de cada click en -/+", _
     "Esto redimensionara la capacidad de Octavas", _
     "Cuando grave, esa capacidad se graba en el archivo",_
     "y es automatico, o sea, se cargaran los datos ", _
     "y la capacidad de Octavas. A mas Octavas mas memoria usada", _
     "El numero de octavas puede achicarse respecto del inicio",_
     "o agrandar "}
  Print #1, "ANTES DE MENUGRAFICO"
      menugrafico  c0, CantItems, text()

    EndIf

   EndIf
   t="    ":cairo_show_text(c,t)
'  ----> DESDE
   If mousex > 144 And mousex< 196 And mousey < 50 And MultiKey(sc_Control) Then ' DESDE LABEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    octavas=0
    redimensionar=0
   EndIf
' ---->  -
   If mousex >= 215 And mousex< 229 And mousey < 50   Then ' - LBEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And octavas=0 Then
     desde -= 1
     If desde < 1 Then 'desdevector Then
      desde = 1 'desdevector
     EndIf
     octavas=1
    RTA = "SOLO REDIMENSION VISUAL "
    EndIf
   EndIf
' ----> +
   If mousex >= 241 And mousex< 261 And mousey < 50   Then ' + LBEL
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And octavas=0 Then
     desde = desde + 1
     If desde > hastavector Then
      desde = hastavector
     EndIf
     octavas =2
    RTA = "SOLO REDIMENSION VISUAL "
    EndIf
   EndIf
   t = "      [OCTAVAS DESDE   - / + ] " + Str(desde-1): cairo_show_text(c,t)
   ' --------------------------------
' ===> 6.2 [HASTA  - / + ]
   If mousex > 308 And mousex< 363 And mousey < 50  Then ' HASTA
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     octavas=0
     redimensionar=0
    EndIf

   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
' ----> -    
    If mousex >= 374 And mousex< 394 And mousey < 50  Then '-
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas = 0 Then
      hasta -= 1
      If hasta < desdevector Then
       hasta = desdevector
      EndIf
      octavas=1
      RTA = "SOLO REDIMENSION VISUAL "
     EndIf
    EndIf
' -----> +
    If mousex >= 403 And mousex< 423 And mousey < 50   Then ' +
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 And octavas = 0 Then
      hasta += 1
      If hasta > 9 Then '' jmg parametriza no va hardcodeo 
       hasta = 9
      EndIf
      octavas=2
      RTA = "SOLO REDIMENSION VISUAL "
     EndIf
    EndIf
   EndIf

   t = "  [HASTA  - / + ] " + Str(hasta-1): cairo_show_text(c,t)

' ===> 5.3 [REDIMENSIONAR]
 
   If mousex >= 473 And mousex< 634 And mousey < 50   Then ' LABEL REDIMENSIONAR
    cairo_set_source_rgba c, 0, 1, 0, 1
' cargar archivo anda bien redimensiona,ï¿½porque no puedo redimensionr
' aca aunque no modifique los limites?  
    If dobleclick And redimensionar=0 Then
     cairo_set_source_rgba c, 1, 0, 0, 1
     NB => 0 + (desde-1) * 13   ' 39 para 4
     NA => 11 + (hasta-1) * 13  ' 102 para  8
  '   print #1,"NB,NA,DESDE,HASTA ";NB,NA,desde,hasta
     CambiarDim(1)
     If MaxPos=0 Then
       MaxPos = 2 'NroCol
     EndIf
     posicion=1:posn=2
     curpos=0
     notacur=1
     nota=0 
     notaOld=0
     inicioDeLectura=0

     redimensionar=1
     RTA="REDIMENSION VISUAL y FISICA OK"
   '  print #1, RTA
      MenuNew=MENU_INICIAL
      cierroedit= 0
    EndIf
   Else
  '  cairo_set_source_rgba c, 1, 1, 1, 1
   
   EndIf
   t = "  [REDIMENSIONAR]<-NO BORRA DATOS" + RTA
'----------------------------------------------------------
   Case TIPO_DE_COMPAS ' 7

   If mousex > 38 And mousex < 90 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=10000000
     menuNew= TIPO_DE_COMPAS
     TCompas="4/4 "  ' para mostrar en pantalla  
     TipoCompas=Tcompas4_4  ' 8 para guardar en disco roll o rtk
       pmTk(0).tipocompas = TipoCompas 
     d7=valorFigura(TipoCompas)
     'If MaxpOS > 2 Then
     '''  ReCalCompas()
     'EndIf
    'MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [4/4]":cairo_show_text(c, t)
  If mousex > 100 And mousex < 150 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=15000000
     menuNew=TIPO_DE_COMPAS
     TCompas="12/8"
     TipoCompas=Tcompas12_8  ' 20  
     'If mAXpOS > 2 Then
     'ReCalCompas()
     'End If
     'MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [12/8]":cairo_show_text(c, t)
  If mousex > 160 And mousex < 210 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=5000000
     menuNew=TIPO_DE_COMPAS
     TCompas="2/4 "
     TipoCompas=Tcompas2_4  ' 6 

    ' If mAXpOS > 2 Then
    ' ReCalCompas()
    ' EndIf
     'MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [2/4]":cairo_show_text(c, t)

  If mousex > 220 And mousex < 270 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=7500000
     menuNew=TIPO_DE_COMPAS
     TCompas="3/4 "
     TipoCompas=Tcompas3_4  ' 7
     'If mAXpOS > 2 Then
     'ReCalCompas()
     'EndIf
     'MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [3/4]":cairo_show_text(c, t)

 If mousex > 280 And mousex < 310 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=7500000
     menuNew=TIPO_DE_COMPAS
     TCompas="6/8 "
     TipoCompas=Tcompas6_8  ' 7
  '   If mAXpOS > 2 Then
  '   ReCalCompas()
  '   EndIf
     'MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [6/8]":cairo_show_text(c, t)

 
 cairo_stroke(c)

'-----------------------------------------------------
  Case PARAMETROS_SECUENCIA ''8
 Print #1,"entro a menu 8 desde ver"

      Select Case  menuOldStr
          Case "[Ver]" 
          nombreArchivo="0"
         
          If pubi =0 Then
           Print #1,"voy a EntrarTEcla desde ver menu8"
            menunew=MENU_INICIAL
            cierroedit= 0
            thread3= ThreadCall EntrarTeclado()
             pubi=1
          EndIf
 '          
          Case "[TEMPO]" 
          nombreArchivo="0"
          
          Print #1,">>>>>>>>TIEMPO PATRON viejo>>>>>>>>>>,PUBI ", tiempoPatron,pubi
      
          If pubi =0 Then
             menunew=MENU_INICIAL
             cierroedit= 0  
             thread3= ThreadCall EntrarTeclado()
            pubi=1   
          EndIf
         Case "[PAN]","[CORO]","[ECO]"
          threadpan=threadCall EntrarTeclado() ''SelPan(Globalpan)
      End Select
    menunew=MENU_INICIAL
    cierroedit= 0
'-----------------------------------------------------
   Case CONFIGURACION ''9
  ' [MIDI] [CANALES]
    If mousex > 42 And mousex < 90  And mousey < 50  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menunew=SEL_MIDI '10 
      EndIf ' mousebuttons
           
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      
    EndIf
    t = "      [MIDI]":cairo_show_text(c,t)

   If mousex > 120 And mousex < 190  And mousey < 50 Then 
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
        menunew=NRO_CANAL '' 11  NRO CANALES 
        
     EndIf ' mousebuttons
   
   Else
     cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
    t = " [CANALES]":cairo_show_text(c,t)

   cairo_stroke(c)
'------------------------------------------------------
 Case SEL_MIDI ''' 10
  '[MIDI-OUT] [MIDI-IN]
    If mousex > 90 And mousex < 190  And mousey < 50  Then ' <= NUEVO
       cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 And cierroport = 0 Then
        cierroport=1 
       Dim miport As Integer=1
       MenuNew=SEL_MIDI '' 10
       '''' error noandaba threadsel = ThreadCreate(@selport(), CPtr(Any Ptr, miport))
       threadsel = ThreadCall selport(miport)  '' 'GtkListBox()
      EndIf ' mousebuttons
  
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      MenuNew=SEL_MIDI '' 10
    EndIf

    t = "             [MIDI-OUT]"
    cairo_show_text(c,t)
    If mousex > 200 And mousex < 300  And mousey < 50  Then ' <= NUEVO
     '''        Print #1,"ENTRO POR EL MIDIIN !!!!!!!"
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1   And cierroport = 0 Then
       cierroport=1 
       Dim miport As Integer=2
       MenuNew=SEL_MIDI ''10
       Menunro=SEL_MIDI
       ' create con argumentos
       '''threadsel = ThreadCreate(@selport(), CPtr(Any Ptr, miport)) '' 'GtkListBox()
       threadsel = ThreadCall selport(miport)  '' 'GtkListBox()
      EndIf ' mousebuttons
    
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
          MenuNew=SEL_MIDI ''10
    EndIf
    t = " [MIDI-IN]":cairo_show_text(c,t):t=""
'-------------------------------------------
  Case NRO_CANAL ''11


     If mousex > 40 And mousex < 190  And mousey < 50 Then 'And listaMIDI = 0 Then ' <= NUEVO
      cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
       Dim ti As Integer
       ti=1 ' seleccion de canales midi de entrada
       'threadcanal = ThreadCreate(@selcanal(), CPtr(Any Ptr, ti))
        threadcanal = ThreadCall selcanal(ti) 
       'Print #1,"canal elegido luego del thread2 en menu ",canalx
      EndIf ' mousebuttons
      MenuNew=CONFIGURACION '9

     Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      MenuNew=CONFIGURACION '9
     EndIf

     t = "      Seleccione un canal"
  '------------
  Case 12  ' edit por teclado
    cairo_set_source_rgba c, 0, 1, 0, 1
    '''If MouseButtons And 1 Then
     COMEDIT=ENTRADA_NOTAS
     menuNew=PARAMETROS_ROLL
     cierroedit= 0 ' habilita EDIT de nuevo
   ' print #1, "INVESTIGO COMEDIT ENTRO X CLICK EN SUB S3, menuNro: ",S3, menuNro
 '  t=  " [Edicion]":cairo_show_text(c, t)
     
 End Select
 ' Si nodoy Stroke los show text van apendendo al final como un print;

  '' VUELVE menu grafico Rollmusic cinta AL INICIO....
  If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf

  cairo_show_text(c, t)
  cairo_stroke(c)
  If usarmarco=0  Then 
   cairo_move_to(c, 0, 30 )
   If resize = TRUE Then
     cairo_set_source_rgba c, 1, 0, 0, 1
     t = "<>"
   Else
     cairo_set_source_rgba c, 1, 1, 1, 1
     t = "<>"
   EndIf

   cairo_show_text(c, t)
   cairo_stroke(c)
  EndIf
 ' ayuda NO SWE USA 
 'If ayuda = TRUE Then
 ' Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )
 ' Dim As String  text => "HOLA SOY TU AYUDA!"
'
 ' Dim As cairo_text_extents_t extents
'
' If mousey < 50 Then
'   cairo_set_font_face( c, cface )
'   cairo_set_font_size( c, 36 )
'   cairo_text_extents( c, text, @extents )
'   cairo_move_to( c, mousex, mousey )
'   cairo_set_source_rgba( c, 1, 1, 1, 1.0 )
'   cairo_show_text( c, text )
'  EndIf
' EndIf
 cairo_set_font_size (c, font)
   If mousex > ANCHO/2 And mousey < 50 And mousex < ANCHO-50 Then
      MenuNew=MENU_INICIAL
      cierroedit= 0
  EndIf

 'DeAllocate xo: DeAllocate yo
Exit Sub 
fail:
 Dim errmsg As String
 Dim er1 As Long
 er1 = Err()
If  er1 > 0 Then
  errmsg = "FAIL Error " & er1 & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(er1)
  Print #1, errmsg
End If

End Sub

'--------------------------
Sub moveresto ()
' parametro nota no se usa podriamos borrarlo.
' entra con insert=3....y pasa pòr  4
 Dim As Integer indpos,s,t,k,i, fininsert, moverDesde,moverDesdeOld
'StartInsert posicion + curpos'indice posicion partimos de ultima posicion global reemplazada
 ' indpos es la ultima cantidad de ticks hasta el comienzo de la ultima nota insertada
indaux=0
    print #1, "EN MOVERESTO, indaux,StartInsert: ",indaux ,StartInsert
 ' ultima posicion movida a aux  es indaux (indaux POSICION DE AUX) JMG
 Dim As Integer  cntoff1,cntoff2,corrimiento,insercion
 
 Print #1, "moveresto inicio: indaux,indpos,posicion :", indaux,indpos,posicion
 Print #1, "entra Loop StartInsert: ", StartInsert 
 Print #1, "duracion insertada DUR "; DUR
 ' o notins el indaux continua en movedata las inserciones nuevas
 
' antes cantInserciones=indaux porque el nro  de posiciones coincidia con la posicion de la nota
' ahora una nota ocupara muchas posiciones ticks!,, y cada nota tiene su propio nro de 
' ticks DurXTick(DUR), necesito la duracion de cada nota entrada
 ' durante lascuales se movio los valores viejso a aux pero quedaron
 ' losnuevos en Roll. que no debo opisar
 '1) COPIAMOS A AUX EL RESTO DE ROLL
  CantTicks=86400 'default 15 minutos * 60 * 96
  CantTicks=CantTicks+DurXTick(DUR)
  Print #1,"CantTicks=CantTicks+DurXTick(DUR) ", CantTicks  
  CambiarDim(1)  ' nuevo pipipipipi
' tickInsertados no indica el inicio correcto desde donde mover
' debo buscar el primer onoff=2 que no sea de las notas insertadas ydesde ahi sera el inicio
' ese onoff=2 deberá estar durante la duracion de la nota insertada o despues
'        I        >>         se mueve a derechas despues del fin de L 
'    L     >>                nota insertada no se mueve
'I      >>                   no se mueve
'--------la L se inserta al dar Fin END debe moverse  asi
'            I        >>   comienza despues del fin de L
'    L     >>              queda igual 
'I      >>                 queda igual
'---------- o sea solo si es un ON (onoff=2) de la nota se mueve
' los onoff=1 de la insertada y cualquier otro que este antes de  que termine la duracion
' de la nota insertada no se moverán.
StartInsert=posicion+ curpos
indpos=StartInsert
fininsert=StartInsert + DurXTick(DUR) -1
' 1) COPIA AL AUX LOS ON OFF=2 QUE ESTEN MAS ADELANTE QUE EL ONOFF=2 INSERTADO Y TODO LO QUE SIGUE
' 2) SI HAY UN ONOFF=1 CUYO ONOFF2 ESTA ANTES DEL ONOFF=2 INSERTADO NO SE MUEVE < startinsert
' 3) SI HAY UN ONOFF=2 QUE ESTA DENTRO DE LA DURACION DE LA NOTA INSERTADA SE MUEVE
moverDesde=0
moverDesdeOld=0
'1)
For s=NB To NA     
  For i= StartInsert +1 To fininsert
      If Roll.trk(i, s).onOff=2 Then
         If moverDesde =0  Then
            moverDesde=i  ' desde aca se mueve los datos SEGUN 1) primer onoff2
            insercion=moverdesde-StartInsert ' didstancia entre el insert y el porimer onoff=2
            corrimiento=insercion+DurXTick(DUR)
            Exit for
         EndIf
      EndIf
  Next i
Next s    

  indpos=moverdesde -1  ' porque despues incrementamos  en 1
' si hay un off=1 se lo debe copiar entonces copio desde ese off=1
' si le sumo a indpos los ticks de la ultima duracion tengo el final de la insericon
'--------------
  If indpos <=0 Then Exit Sub EndIf '30-09-2025
'------------------------- 
  Print #1, "FINAL insercion indpos + DurxTck(DUR) ", indpos
Dim notadeoff1 As UByte
Dim As Integer p1=0, p2=0 ,off1nosemueve=0
 Do
 
  'indpos ya se le sumo curpos y DurxTick... COPIAMOS A AUX EL RESTO DE ROLL NO MODIFICADO COMPLETA
  indpos=indpos + 1 ' empezamos de la siguiente no reemplazada y seguimos al final
  ''suponemos que posicion=indpos era la posicion de ultima reemplazada
  ''' old indaux=indaux +1
  indaux=indaux +1  'nos ubicamos en 1er posicon libre en aux para recibir el resto
  'Print #1,"antes del FOR Roll,RollAux.trk.. indpos,indaux ", indpos,indaux
  ' debemos barrer solo la octava? no porque puede haber otras octavas cargadas
  For s= NB To NA 'barremos todas las notas verticalmente para una misma posicion
   
   If Roll.trk(indpos, s).dur = 182 Or indpos=MaxPos Then ' fin de datos de Roll
    RollAux.trk(indaux, s) = Roll.trk(indpos, s)
    insert=4 ' fin de llenado de almacenamiento auxiliar
        print #1,"sale moveresto encontro 182 fin, indaux :" ,indaux
    Exit Do
   EndIf
    
'copia al Aux
' 2) SI HAY UN ONOFF=1 CUYO ONOFF ESTA ANTES DEL ONOFF=2 INSERTADO NO SE MUEVE
' indpos es moverdesde
off1nosemueve=0 ' se mueve
   If Roll.trk(indpos, s).ONOFF=1  And indpos > moverdesde Then ' se mueve
      ' la nota y el dur valen 183 deberia poner el valor de la nota para matchear??
      ' asi igualo notas , el off1 no tiene nota cargada en el vector aparentemente
      ' pero si la tiene es la vertical del vector,,como es unica en el barrido
      ' cualquier onoff=2 sera su origen no hace falta otra cosa
      For p2= indpos To  1 Step -1 'busco el onof=2
          If  Roll.trk(p2, s).onOff=2 Then ''encontre el onoff2
             Print #1," Roll.trk(p2, s).nota ",Roll.trk(p2, s).nota
             Print #1," onoff=2,p2  ",Roll.trk(p2, s).onoff, p2
             p1=p2
             If p1 < moverdesde Then 
                off1nosemueve=1  ' no se mueve el off1 porque  su off2 esta antes del sartinsert
                Print #1,"no se mueve ", Roll.trk(indpos, s).onoff
             Else ' >=
                off1nosemueve=0  'se mueve el off1 
             EndIf  
             Exit For 
          EndIf 
      Next p2  
         
   EndIf
   If off1nosemueve=0 Then ' se mueve
       RollAux.trk(indaux, s) = Roll.trk(indpos, s) ' INCLUYE A 3)
       Roll.trk(indpos, s).nota=0
       Roll.trk(indpos, s).dur=181
       Roll.trk(indpos, s).pb=0
       Roll.trk(indpos, s).vol=0
       Roll.trk(indpos, s).onOff=0
       Roll.trk(indpos, s).inst=0
       Roll.trk(indpos, s).pan=0
    EndIf
    
  Next s

  'posicion + curpos tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultimo reemplazo o insercion
 ' el 66 lo muevo??? sino debo hcer indaux = indaux -1
 ' indaux=indaux - 1 no le resto 1 copio el 66...el fin se corre

' print #1, "StartInsert: " ,StartInsert '  ya se le sumo curpos
 If insert=4 Then 'movemos de nuevo todo el aux completo a Roll desde
  ' la ultima insercion o sea Startindice +
  Print #1," CantTicks ", CantTicks
  print #1, "insert 4 movemos Aux a ROLL indaux maximos elementos en Aux: ",indaux
 TicksInsertados= DurXTick(DUR)
  t = fininsert + insercion ' + 1  xxx  OK
  Print #1,"t,StartInsert, TicksInsertados ", t, StartInsert,  TicksInsertados
  print #1, "posicion, posn "; posicion, posn
'copia al auxiliar
  For k = 1 To indaux ' todo el auxiliar SUMA ROLL REEMPLAZADO + NOREEMPLAZADO
   ''print #1, "k auxiliar "; k
   For s=NB To NA  ' todos los semitonos 96
    Roll.trk(t,s) = RollAux.trk(k,s)
   Next s
   t=t+1
  Next k
 ' print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
  '' posicion = MaxPos con posn es suficiente,sino se va al tramo final
  posn = MaxPos -6
'  print #1, "usamos MxPos pra posicion, posn "; posicion, posn
  '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
 ' print #1 ,"final ciclo moveresto a AUX"

  For s = NB To NA
   For k=  StartInsert To MaxPos ' gracias a esto anda acordes
    If Roll.trk(k, s).nota = 0 And Roll.trk(k, s).onOff = 0 And Roll.trk(k, s).dur <> 183 Then
     Roll.trk(k ,s ).nota = 181
    EndIf
   Next k
  Next s
  ' ACA DEBO CORREGIR TODAS LAS LINEAS CON NOTAS DIFERENTES A LA INSERTADA DEBEN QUEDAR
  ' nota en 181 y durciones en  0. bueno hor determinamos donde y cuanto se inserto.
  ' estos son los valores  StartInsert y cantInserciones.
  ' StartInsert es de Roll antesdela insercion
'  print #1, "==> valor de nota antes de CORRECCION: ",nota

  ' aca revisamos el Roll solo la parte insertada , encada insercion en Roll
  ' esa posiicon se corresponde al de notas insertadas biunivocmente,,
  ' BORRAMOS LO QUE ESTABA ANTES PUES YA SEMOVIO ALFINAL
  ' EN CADA POSICION SOLO DEBE QUEDAR UNA SOLA NOTA, LA INSERTADA,
  ' SOLO SE PERMITE INSERTR UNA SOLA NOTA VERTICALMENTE O SEA PARA UNA MISMA POSICION
  ' PERO PARA DISTINTAS POSICIONES HABRA MAS NOTAS INSERTADAS UNICAS SI SE DESEA
  ' LUEGO CON PROBRSI LA NO TA EN ROLL ES IGUAL O DISTINA A LA INSERTADA SE PUEDEN
  ' BORRAR LAS DISTINTAS..Y EL ORDEN ES EL MISMO...SEGUN POSICION..
'  Dim i As Integer
'  For k= StartInsert To StartInsert + TicksInsertados -1 ' OK PARA 1 INSERCION ES SOLO EN STRTINSERT
'   i = i + 1
'   For s= NB To NA
'    If s <> notasInsertadas (i) Or Roll.trk(k, s).onOff=0 Then
'     Roll.trk(k, s).nota = 181
'     Roll.trk(k, s).dur  = 0
'    EndIf
'   Next s
'  Next k
 EndIf
 insert=0
 indaux=0
 RecalCompas() ' 09-05-2021
 TicksInsertados=0
 StartInsert=0
 fininsert=0
End Sub
'------------------------------------
Sub botones(hWnd As HWND,  cm As cairo_t Ptr, x As Integer,y As Integer)
 ' RECTANGULO DE 40 X 40, FONDO ROJO
 Dim As cairo_font_extents_t fe   '     font data
 Dim As cairo_text_extents_t te  '      text size

' indicacion de sitio optimo par amover la ventana ancho*2/3
'Dim sitio As Integer=ANCHO*2/3
' cairo_move_to  (cm, sitio, 0 )
' cairo_rectangle(cm, sitio,  0 , sitio+10,50)
' cairo_set_source_rgba cm, 0.2, 0, 0.4, 1 
' cairo_move_to  (cm, sitio+5, 25 )
' cairo_fill (cm)
' cairo_stroke(cm)




 cairo_move_to  (cm, ANCHO-40-mxold, 0 )
 cairo_rectangle(cm, ANCHO-40-mxold, 0 , ANCHO,16)
 cairo_set_source_rgba cm, 0.8, 0, 0.2, 1
 cairo_move_to  (cm, ANCHO-10-mxold, 10 )
 cairo_fill (cm)

 cairo_set_source_rgba cm, 0.9, 0.9, 0.9, 1
 cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
 cairo_set_font_size (cm, 14)
 cairo_move_to (cm,ANCHO-25-mxold,14)
 cairo_font_extents (cm, @fe)
 Var t= "X"
 cairo_text_extents (cm, t, @te)

 cairo_show_text(cm,t)
 cairo_stroke(cm)
 '---------
 cairo_move_to  (cm, ANCHO-40-mxold, 17 )
 cairo_rectangle(cm, ANCHO-40-mxold, 17 , ANCHO,17)
 cairo_set_source_rgba cm, 0.8, 0.8, 0.2, 1
 cairo_move_to  (cm, ANCHO-10-mxold, 26 )
 cairo_fill (cm)
 cairo_set_source_rgba cm, 0, 0, 0, 1
 cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
 cairo_set_font_size (cm, 14)
 cairo_move_to (cm,ANCHO-25-mxold,31)
 cairo_font_extents (cm, @fe)
 t= "-"
 cairo_text_extents (cm, t, @te)

 cairo_show_text(cm,t)
 cairo_stroke(cm)
 '---------
 cairo_move_to  (cm, ANCHO-40-mxold, 33 )
 cairo_rectangle(cm, ANCHO-40-mxold, 33 , ANCHO,33)
 cairo_set_source_rgba cm, 0, 1, 0, 1
 cairo_move_to  (cm, ANCHO-10-mxold, 30 )
 cairo_fill (cm)
 cairo_set_source_rgba cm, 0, 0, 0, 1
 cairo_select_font_face (cm, "Georgia", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
 cairo_set_font_size (cm, 14)
 cairo_move_to (cm,ANCHO-25-mxold,45)
 cairo_font_extents (cm, @fe)
 t= "+"
 cairo_text_extents (cm, t, @te)

 cairo_show_text(cm,t)
 cairo_stroke(cm)

End Sub
'
Sub organizaCompases()
' print #1,"1-organiza Compases "
 Dim j As Integer
 For j = 1 To MAxPos
  mayorDurEnUnaPosicion (j)
 Next j
End Sub


'------------------------------------
' https://www.freebasic.net/forum/viewtopic.php?t=10398
' Many times in my games I want to act on a single key press.
' Multikey returns -1 constantly if a key is down and that isn't always what I want
' so I created this function which uses multikey but will only return -1 once
' for a particular key. The user must release and press the key again for it to fire.
' Call it just like Multikey....
'
' If KeyPress(SC_ESC)=-1 then .....!!! USARLO O USAR E.EVENT CREO ES SIMILAR O NO???
'
Function KeyPress(Key As Integer) As Integer
 Static LastKey(255) As Integer
 ' DETECTA UNA APRETDA DEL USUARIO Y SOLO UNA CREO PROBAR
 If MultiKey(Key) = -1 Then
  If Key = LastKey(Key) Then
   Return (0)
  Else
   LastKey(Key)=Key
   Return (-1)
  EndIf
 Else
  LastKey(Key)=0
  Return (0)
 End If

End Function
' -------------------------------------------------------------



Sub CambiarDim(redi As Integer)
     NB => 0 + (desde-1) * 13   ' 39 para 4
     NA => 11 + (hasta-1) * 13  ' 102 para  8
     pmTk(0).NB= NB
     pmTk(0).NA= NA     

' hacemos un redim preserve por default
     *po=hasta-1
     Print #1,"CambiarDim desde ";desde;" hasta ";hasta;" *po ";*po;" NB ";NB;" NA ";NA
     Print #1,"CantTicks ";CantTicks
     '''CantTicks=CantTicks +  DurXTick(DUR) -1
If redi=1 Then 
     ReDim  Preserve (Roll.trk ) ( 1 To CantTicks, NB To NA)
     ReDim  Preserve (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
     ReDim  Preserve compas(1 To CantTicks)

Else
     ReDim  (Roll.trk ) ( 1 To CantTicks, NB To NA)
     ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
     ReDim  compas(1 To CantTicks)

     Print #1,"FIN CAMBIARDIM  else" 
EndIf   
ReDim (RollAux.trk) ( 1 To CantTicks,NB To NA )
 desdevector=desde
 hastavector=hasta
 
Print #1,"FIN CAMBIARDIM "



     ''ReDim (Roll.trk ) ( NB To NA,1 To CantTicks) ' temporario     
'no se puede reemplaar en el main del programa por cuestion de scope
' o sea la definicion debe hcerse en el scope del main para que funcione
' Esta redefinicion funciona y es necesaria  donde se use cairo
' sino parece que hay un choque entre cairo y redim y hace crash
' la redimension, sacandola del scope de cairo funciona 

End Sub

Sub EntrarTeclado ()


Dim As String default, default2
 Dim  As Integer ni
 
Select Case  menuOldStr 
 Case  "[Ver]" 
            For ni=1 To MaxPos
               If Compas(ni).Posi = posicion Then
                  compasX = Compas(ni).nro
                  Exit For
               EndIf
            Next ni

default=Str(CompasX)
nombreArchivo=InputBoxJmg("IR A COMPAS" ,"Entre una posicion", default, ES_MULTILINE + ES_AUTOVSCROLL,0 )
  
            compasX=CInt(nombreArchivo)
            If compasX > NroCompas Then
               compasX=Nrocompas
            EndIf
            If compasX < 0 Then
               compasX=1
            EndIf
   '         If compasX = 0 Then
    '           Exit Sub
    '        EndIf
          If compasX >=1 Then       
            Dim ni As Integer
            Print #1,"NroCompas: ", NroCompas
            Print #1,"CompasX: ", CompasX
            For ni=1 To MaxPos
            Print #1,"ni: ,posi, nro Compas ", ni, Compas(ni).Posi, Compas(ni).nro
               If Compas(ni).nro = compasX Then
                  posicion = Compas(ni).Posi
                  Print #1,"encontro : ", CompasX
                  Exit For
               EndIf
            Next ni
          EndIf  

 Case "[TEMPO]"


'If  EJECCARGADA=FALSE Then
'    If tiempoPatron = 0  Then
'       tiempoPatron=60
'       default="60"
'   EndIf
'EndIf
'If  EJECCARGADA = TRUE  Then
   If tiempoPatronEjec=0 Then
      tiempoPatronEjec=240
      tiempoPatron=240
      default="240"
      default2="240"
  EndIf
''EndIf
    
' debo generar un default para manual 60 y oro para ejecs 240
   If  EJECCARGADA = TRUE And ROLLCARGADO=FALSE Then ' cargar lo que hhay aen el archivo
       default2 = Str(tiempoPatronEjec)
       ticksdefault = InputBoxJmg("Ajuste Tempo Ejec" ,"Entre un Tempo ",default2, ES_MULTILINE + ES_AUTOVSCROLL,0  )
       tiempoPatronEjec= CInt(ticksdefault) '08-01-2022 de doble a int
   Else
       'If  ticksdefault<>"" Then
       '    default=ticksdefault
       'Else
        default=Str(tiempoPatron)  ' manual
       'EndIf 
     ticksdefault = InputBoxJmg("Ajuste Tempo Manual" ,"Entre un Tempo ",default, ES_MULTILINE + ES_AUTOVSCROLL,0   )
      tiempoPatron= ValInt(ticksdefault) '08-01-2022 de doble a int
      pmTk(ntk).tiempopatron = tiempopatron
   EndIf
    Print #1,"tEMPO llego a TEMPO en entrar tecla",ticksdefault 
   
' o sea el default de tiempoPAtron para manual es 60, pero para ejecuciones es 240

 Case "[FACTOR]"
   default=Str("1.25")
    Print #1,"llego a TEMPO en entrar tecla",NombreArchivo 

    NombreArchivo = InputBoxJmg("Ajuste Factor Tempo " ,"Entre un Factor ",default, ES_MULTILINE + ES_AUTOVSCROLL,0  )
      
    Print #1,"nombreArchivo de click ok",nombreArchivo
    FactortiempoPatron= CDbl(nombreArchivo) '08-01-2022 de doble a int
    Print #1,">>>>>>>>Factor TIEMPO PATRON NUEVO>>>>>>>", FactortiempoPatron
 Case "[NROREP]"
  default=Str("2")
  NombreArchivo = InputBoxJmg("Repeticiones " ,"Cantidad entera ",default, ES_MULTILINE + ES_AUTOVSCROLL ,0 )
' en este momento inserto al repeticion 
' Lo maximo que uso son 8 posiciones para los acrodes de las octavas de 0 a 7 o 1 a 8,
' como tengo 13 posiciones verticales en la ultima octava me quedan 5 posiciones libres 
' y tambien como la estructura 'dat' tiene 6 campos me quedan 2 campos en las 8 primeras
' y todas las 6 en las 5 siguietnes = 46 sitios donde poner informaicon para una posicion
' dada. Las repeticiones las colocaremos en la 1era de la posicion libre o sea en este caso 
' octavas que van default de 4 a 8, la posicion 98 como si fuera una octava 9 ficticia...
' las repeticiones no dependen de la octava todas las pistas se repiten al unisono. 
' solo hace falta inforamcion arriba en la octava que no se usa.
' pa la infor de acordes usabamos
' verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde ' 90 + 6 - 4=92
' luego para repeticiones usaremos desde la 98del caso dicho o sea generalizando
'verticalEnOctavaVacia= 12 + (hasta-2)*13 + estoyEnOctava - desde ' 90 + 6 - 4=92
' par ala octava mas alta seria 9
' 12+7*13
  Dim As Integer vertical=0 ' elvalor minimo 
  vertical=12+(hasta-2)*13+hasta ' para 9 -> 112
  'Print #1,"ajustado repeind ",vertical  98
  Roll.trk(pasozona1,vertical).nota = 210 ' comienza repeticion en pasozona1
' ajusto comienzo= pasozona1...
' al llear pasozona2 en el mismo lugar pongo le nro de repeticiones
  Roll.trk(pasozona2,vertical).nota = 211' indica final de repeticion
  Roll.trk(pasozona2,vertical).vol = CUByte(NombreArchivo) ' nro de repeticiones
' ajusto final=pasozona2 
' creo un contador que vaya disminuyendo su valor con cada repeticion  
  pasozona1=0:pasozona2=0
' en crear penta coloco un [ ariba de laposiicon 

' luego al detectar 210 tomoel ascii de vertical+1 [ 
' 
' con esta info comienzo en play=pasozona1 y final=posicion (seria el pasozona2)
' en cada play de la repetiion le resto 1 al nro de repeticiones
' cuadno lleg aa 0 habra terminado el nro de repeticiones. 
' asi digo hay repeticion de 
' no se sipongo en pmTk eso solo sirve para una sola repeticion
' debo ponerlo en NA-13 o lim3 -lim2
' aca lo dejo solo como paso transitorio para luego colocarlo en la pista 
' tanto en Roll como en Track 
'  pmTk(ntk).zona1  = CUByte(pasozona1)
'  pmTk(ntk).zona2  = CUByte(pasozona2)
'  pmTk(ntk).nroRep = CUByte(nroRep)                
' Roll.trk()= 
' HACEMOS LO MISMO PARA EL TRACK ntk en edicion ya sea nto= en caso de Roll puro o pista ntk
' en edicion
' Track(ntk).trk=
 Case "[SEPARA]"
 Case "[PAN]","[PANEJEC]"
'' NO SE PUEDE USAR UN TRACKBAR DESCONFIGURA LA SELECION DE PISTASROLL Y SE QUEDA
'' CLAVADA EN UNA PISTA AZUL Y LA OTRA PASA A BLANCA UNA CHOTADA NO SE COMOMIERDA SE SOLUCIONA
'' PROBE DETODO HASTA LO INIMAGINABLE JAJAJ 09-11-2025
'' MI ENTRA TECLADO ES LA POSTA PARA ENTRAR DESDE TECLADO Y CON ENTER 
'' FALTARIA AGREGARLE CON MOUSE QUE FUNCIONE EL ACEPTAR listo funciona  con 2 enter y mouse ok
   default=Str(pmTk(ntk).pan)  
   intervaloTxt = InputBoxJmg("Ajuste Numerico de PAN 64 al medio " ,"ENTRE UN VALOR DE 1 IZQ, A 127 DER ",default, ES_MULTILINE + ES_AUTOVSCROLL,0   )
   valorpan= ValInt(intervaloTxt)  '19-03-2025
   If menuOldStr = "[PAN]" Then 
   pmTk(ntk).pan=CUByte(valorpan)
   pmTk(0).pan=CUByte(valorpan)
   EndIf
   If menuOldStr = "[PANEJEC]" Then
     If ntkp=0 Then ntkp=1 EndIf 
      pmEj(ntkp).pan=CUByte(valorpan) 
   EndIf

   Print #1,"PAN SELECCIONADO ->> "; valorpan
   Globalpan=CUByte(valorpan)
  SetGadgetText(3,Str(valorpan))
 Case "[CORO]","[COROEJEC]"
   default=Str(valorcoro)  
   intervaloTxt = InputBoxJmg("Ajuste Numerico de CORO/Chorus 64 al medio " ,"ENTRE UN VALOR DE 1 IZQ, A 127 DER ",default, ES_MULTILINE + ES_AUTOVSCROLL,0   )
   valorcoro= ValInt(intervaloTxt)  '19-03-2025
   If menuOldStr="[CORO]" Then  
     pmTk(ntk).coro=CUByte(valorcoro)
     pmTk(0).coro=CUByte(valorcoro)
   EndIf
   If menuOldStr="[COROEJEC]" Then
     If ntkp=0 Then ntkp=1 EndIf  
     pmEj(ntkp).coro=CUByte(valorcoro)
   EndIf

   Print #1,"PAN SELECCIONADO ->> "; valorcoro
   Globalcoro=CUByte(valorcoro)
  SetGadgetText(3,Str(valorcoro))

  Case "[ECO]","[ECOEJEC]"
   default=Str(pmTk(ntk).eco)  
   intervaloTxt = InputBoxJmg("Ajuste Numerico de ECO 64 al medio " ,"ENTRE UN VALOR DE 1 IZQ, A 127 DER ",default, ES_MULTILINE + ES_AUTOVSCROLL,0   )
   valoreco= ValInt(intervaloTxt)  '19-03-2025
   If menuOldStr="[ECO]" Then 
     pmTk(ntk).eco=CUByte(valoreco)
     pmTk(0).eco=CUByte(valoreco)
   EndIf
   If menuOldStr="[ECOEJEC]" Then 
     If ntkp=0 Then ntkp=1 endif   
     pmEj(ntkp).eco=CUByte(valoreco)
   EndIf

   Print #1,"ECO SELECCIONADO ->> "; valoreco
   Globaleco=CUByte(valoreco)
  SetGadgetText(3,Str(valoreco))
    Case "[VOL]","[VOLEJEC]"
   default=Str(pmTk(ntk).vol)  
   intervaloTxt = InputBoxJmg("Ajuste Numerico de VOL 64 al medio " ,"ENTRE UN VALOR DE 1 IZQ, A 127 DER ",default, ES_MULTILINE + ES_AUTOVSCROLL,0   )
   valorvol= ValInt(intervaloTxt)  '19-03-2025
   If menuOldStr="[VOL]" Then 
     pmTk(ntk).vol=CUByte(valorvol)
     pmTk(0).vol=CUByte(valorvol)
   EndIf
   If menuOldStr="[VOLEJEC]" Then 
     If ntkp=0 Then ntkp=1 endif   
     pmEj(ntkp).vol=CUByte(valorvol)
   EndIf

   Print #1," VOLUMEN AJUSTADOO ->> "; valorvol
   Globalvol=CUByte(valorvol)
  SetGadgetText(3,Str(valorvol))

End Select

End Sub


Sub velocidades(j As Integer)
 'Print #1,"velocidades Tcompas j "; TCompas, j 
    If TCompas="4/4 " Then ' dividido y subdividido
      If acumulado <= 1250000 Then ' hasta a corchea
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 1250000 And acumulado <= 2500000 Then 'hasta 1 negra
       compas(j).nro = -2 ' debil
      EndIf

      If acumulado > 2500000 And acumulado <= 3750000 Then 'hasta 1,5 negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 3750000 And acumulado <= 5000000 Then 'hasta 2 negra
       compas(j).nro = -2 ' debil
      EndIf

      If acumulado > 5000000 And acumulado <= 6250000 Then 'nollego a 2,5 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 6250000 And acumulado <= 7500000 Then 'nollego a 3 negra
       compas(j).nro = -2 ' debil
      EndIf

      If acumulado > 7500000 And acumulado <= 8750000 Then 'hasta 3,5 negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 8750000 And acumulado <= 10000000 Then 'hasta 4 negra
       compas(j).nro = -2 ' debil
      EndIf



    EndIf

    If TCompas="2/4 " Then 'dividido y subdividido. seria un 6/8 tmbien
      If acumulado <= 1250000 Then 'nollego a corchea
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 1250000 And acumulado <= 2500000 Then 'hasta negra
       compas(j).nro = -2 ' debil
      EndIf

      If acumulado > 2500000 And acumulado <= 3750000 Then 'hasta 1,5 negra
       compas(j).nro = -3 ' semi fuerte
      EndIf
      If acumulado > 3750000 And acumulado <= 5000000 Then 'hasta 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      Exit Sub
    EndIf




    If TCompas="3/4 " Then 'dividido y subdividido. seria un 6/8 tmbien
    '  Print #1 ,"TCompas=3/4 en velocidades acumulado "; acumulado
      If acumulado <= 1250000 Then 'nollego a corchea
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 1250000 And acumulado <= 2500000 Then 'hasta negra
       compas(j).nro = -2 ' debil
      EndIf

      If acumulado > 2500000 And acumulado <= 3750000 Then 'hasta 1,5 negra
       compas(j).nro = -3 ' semi fuerte
      EndIf
      If acumulado > 3750000 And acumulado <= 5000000 Then 'hasta 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 5000000 And acumulado <= 6250000 Then 'hasta 2,5 negra
       compas(j).nro = -3 ' semi fuerte
      EndIf
      If acumulado > 6250000 And acumulado <= 7500000 Then 'hasta 3 negra
       compas(j).nro = -2 ' debil
      EndIf
      Print #1, "velocidades 3/4 compas(j).nro "; compas(j).nro 
      Exit Sub 
    EndIf

    If TCompas="6/8 " Then ' dividido.. f d  y subdividido fdd,sfdd  ..?
      If acumulado <= 1250000 Then ' hasta corchea 
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 1250000 And acumulado <= 2500000 Then 'hasta 1 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 2500000 And acumulado <= 3750000 Then ' hasta 1,5 negra
       compas(j).nro = -2 ' debil
      EndIf


      If acumulado > 3750000 And acumulado <= 5000000 Then 'hasta 2 negra
       compas(j).nro = -3 ' semi fuerte
      EndIf
      If acumulado > 5000000 And acumulado <= 6250000 Then 'hasta 2,5 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 6250000 And acumulado <= 7500000 Then 'hasta 3 negra
       compas(j).nro = -2 ' debil
      EndIf

      Exit Sub
     
    EndIf


    If TCompas="12/8" Then ' dividido.. f d sf d y subdividido fdd,fdd,sfdd,fdd
      If acumulado <= 1250000 Then ' hasta corchea 
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 1250000 And acumulado <= 2500000 Then 'hasta 1 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 2500000 And acumulado <= 3750000 Then ' hasta 1,5 negra
       compas(j).nro = -2 ' debil
      EndIf


      If acumulado > 3750000 And acumulado <= 5000000 Then 'hasta 2 negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 5000000 And acumulado <= 6250000 Then 'hasta 2,5 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 6250000 And acumulado <= 7500000 Then 'hasta 3 negra
       compas(j).nro = -2 ' debil
      EndIf


      If acumulado > 7500000 And acumulado <= 8750000 Then 'hasta 3,5 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 8750000 And acumulado <= 10000000 Then 'hasta 4 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 10000000 And acumulado <= 11250000 Then 'hasta 4,5 negra
       compas(j).nro = -2 ' debil
      EndIf


      If acumulado > 11250000 And acumulado <= 12500000 Then 'hasta 5 negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 12500000 And acumulado <= 13750000 Then 'hasta 5,5 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 13750000 And acumulado <= 15000000 Then 'hasta 6 negra
       compas(j).nro = -2 ' debil
      EndIf
      Exit Sub
    EndIf



End Sub 
Sub grabaralteracion(ByRef pan As UByte)
       Select Case alteracion
       Case "sos" 
         pan  = CUByte(3)
       Case "bem"
         pan  = CUByte(2)
       Case Else 
         pan  = CUByte(3) 
     End Select

End Sub

Function  GrabarRoll ()  As Integer

On Local Error GoTo sale
' GRABA ARCHIVOS CON EXTENSION Y FORMATO *.ROLL
' el borrado de undo solo se deberia ahcer si se crea un archivo nuevo y se borra
' O SI EL USUARIO VACIA EL UNDO BOTON O MENU QUE HARA FALTA
' el que se esta editando o se carga un nuevo archivo
' para roll ntk es siemrpe cero
ntk=0
Print #1,"GRABAR ARCHIVO: nombre y nombreg al comienzo "; nombre
Dim  As Integer npl, length
     If nombre = ""  Then ' backup antes de aumentar el tamano del vector
        nombre = "Backup"+ Date + ".roll"
     Else
       npl= InStr(UCase(nombre),".RTK") 
       If npl > 0 Then
          nombre=Mid(nombre,1,npl-1)
''' NUNCA ENTRA POR ACA , SOLO SI SE USA ESTA RUTINA SIN
''' ANALIZAR EL NOMBRE 
          nombre = nombre +".roll"  
          Print #1,"NO, es un trk grabado como roll ",nombre ' sin sentido
          Exit Function    
       Else
        If InStr(UCase(nombre),".ROLL") = 0 Then
           'seria nombre="" pero se toma como backup odeberia preguntar por un nombre nuevo 
           nombre = nombre +".roll"  'un nombre sin extension no creo ocurra solo si es nuevo y no hay nombre 
        EndIf
       EndIf 
     EndIf   
     Print #1,"NOMBRE A GRABAR COMO",nombre
 
    ga=7 
    If  Open (nombre  For Binary Access write As #ga ) > 0 Then   
    
        Print #1,"No se puede escribir error "; nombre, GrabarRoll  
        Close ga
        Sleep 1000,1
        Exit Function
    Else
   Print #1,"Se puede escribir en "; nombre
  ''''  Open "test-AAAAA.TXT" For Output    As #2
FileFlush (ga)
     Dim Trabajo (1 To Maxpos, NB To NA) As datsec
     Dim dat   As rolldat  ''es el z de cargar archivo
' en bloque graba3 vamos a colocar nro de canal 1 a 16
'                                   port midi de salida
'                                   port midi de entrada
' etc, para el canal pondremos una seleccion de nrro de canal de salida
' en el menu de control , como tambien luego del puerto midi de entrada
' una vez qu etenga nro de canal vamos a manipualr un poco el canal 10 de
' percusion a ver si ponemos alguna ayuda de los instrumentos de percusion 
'en vez de nombres de notas....lo que creo practivo sera posar el mouse sobre las notas guias
' y mostrar el nombre del instrumento con letras popup al estilo de los menues graficos
' que hice para control-M o pondremos en forma fija al final de pantalla y sobre cada 
' linea el nombre del instrumento siemrpe estara en el extremo derecho y moviendo 
' las flechas podre ingresar notas mas a al izquierda de drums,,,                                        
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0,final=0
'eliminar columnas marcadas al grabar disco, 0 + X
'print #1, "inicio MaxPos "; MaxPos
 For i2 = 1 To MaxPos  'incluye a +6 es ultimo off +6
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=190 And Roll.trk(i2,i1 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i2,i1 ).dur=182 Then ' 26-06-2021 copiar final archivo
          final= 1 'Atrapa el final
       EndIf
' extendemos a semitono ficticio 13 donde esta acorde a ver si lo recupera
       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=13 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA And haynota=1 Then ' si hay una sola nota en la columna no borro
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
          Trabajo(k, i1)=Roll.trk(i2,i1 )
      Next i1
    Else
      Maxpos=MaxPos-1  
    EndIf
      
    borrocol=0:res=0:haynota=0:final=0
 Next i2
' MaxPos = k JMG 16-05-2021 TODAVIA NO SE COMO SE ACHICA LA SECUENCIA 
' AL GRABAR NO SOLO CON MAXPOS PARECE,,,ANDA BIEN PERO SI CONSERVO MAXPOS
' DEL MISMO VALOR EN VEZ DE K     


posn=k       

Dim As Integer r1
For i1=NB To NA 
  For r1= posn+1 To MaxPos
    Trabajo(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    Trabajo(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  

MaxPos=k+1


Print #1, "final MaxPos "; MaxPos
'print #1,"---------------------------------------- K= "; K
'  For i1 = NB To NA
'  print #1,i1;"|";
'    For i2 = 1 To MaxPos
'       print #1,Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
'    Next i2
'    print #1,
'   print #1,"------------------------------------------" 
'  Next i1
  
 '''Shell "NOTEPAD test-AAAAA.TXT" 
''''Exit sub
     Dim As UByte y1,y2,y3,y4, y5  
     Dim As String a1,a2,a3,a4,a5, x
    
     
     x= Bin(MaxPos,20)
     '    print #1,"Posicion ",Posicion
     'Print "string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     a5=Mid(x,17,4)
     '    print #1,"a1 a2 a3 a4 ",a1, a2 ,a3, a4

     y1= CUByte("&B"+a1)
     y2= CUByte("&B"+a2)
     y3= CUByte("&B"+a3)
     y4= CUByte("&B"+a4)
     y5= CUByte("&B"+a5)
     '    print #1, "y1,y2,y3,y4", y1,y2,y3,y4
     ' grabamos maxpos en 5 ubyte  (1 a 12 no notapiano)
'==============GRABAPOS
     dat.x1 = y1
     dat.x2 = y2
     dat.x3 = y3
     dat.x4 = y4
     dat.x5 = y5
Print #1,"grabando tipoescala_num_ini en el archivo ",tipoescala_num_ini 
     dat.tipoescala_num_ini = CUByte(tipoescala_num_ini) ' 20-12-2021 - tipoescala en uso
''''QUEDO LIBRE grabaPos.onoff
    '-----------------------
'==============GRABALIM
     dat.desde = CUByte(desde) 
     dat.hasta  = CUByte(hasta)
Print #1,"grabando notaescala_num_ini en el archivo ",notaescala_num_ini
     dat.notaescala_num_ini  = CUByte(notaescala_num_ini) ' notadeescala 20-12-2021
Print #1,"grabando alteracion en el archivo ",alteracion
     grabaralteracion (dat.alteracion)
     ' ------------------------------------
     ' para seguir poniendo notas nuevas:
     ' hacemos lo mismo para la ultima nota que se grabo que tenga el 182
     ' esa es la papa recorremos todas las dur de las notas en donde este el 182
     ' en dur tomamos la nota y esa la grabamos en pb o ins
     ' cuando la cargamos lo hacemos en notaold !!!! y Vuala!!!
     ' -------------------------------------
     dat.notaold   = CUByte(notaold)
     dat.nanchofig = CUByte(nanchofig*10)
     dat.vol = pmTk(ntk).vol
''no le da 
'''     grabaLim.inst = CUByte(tiempoPatron) '08-01-2022 hasta 256 sera suficienteï¿½?
' el tiempoPatron no alcanza 256 30-03-2025<--antes de poner onoff 
' el tiempoPatron al pasar de ejec a Roll lo dejo aca
''''===> QUEDADN LIBRES grabaLim.inst
''''===>               grabaLim.onoff este no estaba antes
'---------------------------

     If instru=0 Then 
        instru=1
     EndIf
     If instru > 0 And instru <> CInt(pmTk(ntk).patch) Then
       pmTk(ntk).patch=CUByte(instru)
     EndIf
     If CANCIONCARGADA =TRUE Then ' SELECCION DE ISNTRUMENTO CON CANCION CARGADA
       pmTk(ntk).patch=CUByte(instru)   
     EndIf
'=============GRABA3===============================================
'===>>graba3
     ' faltaba el patch!!! por eso habia incongruencias al modificar
     ' ver la carga tambien SEGUIR!!!!, TAMPOCO ESTABAN NA Y NB
     dat.patch= pmTk(ntk).patch ' 14-04-2024 o sea inst
     dat.portout = pmTk(ntk).portout ''EL DISPOSITIVO
     dat.eco = Globaleco
     pmTk(ntk).eco=Globaleco   
Print #1,"///----grabar ECO GlobalECO,pmtk, ntk ",Globaleco,pmTk(ntk).eco,ntk
     dat.ejec = pmTk(ntk).ejec ' xxxx nuevo  
''===> libres graba3.pan
     dat.pan =Globalpan
     pmTk(ntk).pan=Globalpan 
     dat.coro=Globalcoro
     pmTk(ntk).coro=Globalcoro 
  
Print #1,"///----GrabaRoll pan Globalpan,pmtk, ntk ",Globalpan,pmTk(ntk).pan,ntk
     dat.TipoCompas = TipoCompas ' 26-04-2024
     ' NB y NA se calcula a partir de desde y hasta no hace falta grabarlos  
     '  print #1,"MaxPos grabada en Trabajo ",MaxPos
     dat.canalx=canalx 'el canal global
     dat.canalsalida =pmTk(0).canalsalida ' el CANAL MIDI 1 A 16 QUE ACA VA DE 0 A 15
     ' EL CANAL DEL DISPOSITIVO canalsalida
     ' el canalx globa lcuando selecciono un canal se pasa a pmtk
     ' y deberian ser iguales salvo que se cargue un roll yel canalx tenia
     ' otro valor en ese caso hay que cambiar canalx
     If dat.canalx  <> dat.canalsalida Then
        Print #1,"canalx canalsalida distinto ",dat.canalx, dat.canalsalida
     EndIf 

' es un roll !!
'===> libre grabar3.onoff

'============> graba4
' usaremos el graba4 
'''===> libres 
' nota 
' dur  
' vol  
Dim mit As aUshort
mit.st = tiempoPatron
'con estos dos campos puedo reconstruir tiempoPatron y poner los aca? no
'Roll.trk(1,NA).pan = mit.pan
'Roll.trk(1,NA).pb  = mit.pb
' tratare de sacar todo lo que haya en 1,NA podria usarse algun dia como notas
  dat.tiempoPatron1 = mit.tp1
  dat.tiempoPatron2  = mit.tp2
'''===> libres el resto
' inst 
' onoff 
' por compatibilidad hacia atras dejo asi no muevo a grabaLim


Print #1,"grabando las partes del archivo "; nombre
FileFlush (7)
''' graba4   ' aca esta tiempoPatron
     Put #ga, ,dat
               
     Put #ga, ,Trabajo()
     
     Close ga
    End If  ' del open ga archivo
FileFlush (7)
     While InKey <> "": Wend
     Sleep 1000,1
     
If nombre = "Backup"+ Date + ".roll" Then
   nombre=""
EndIf
length = FileLen(nombre)
If length = 0 Then
  GrabarRoll=4 ' mi error por tamaño 
EndIf
Exit Function 

sale:
 Dim errmsg As String
If  Err > 0 Then
  Close ga
  Sleep 1000,1  
  GrabarRoll=Err
  errmsg = "FAIL Error GrabarRoll " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
End If
End Function
'
Sub  abrirSecuencia(nf As Integer) 

  If  Open (ROLLDIR +"secuenciaPLAY.txt" For Output Shared As nf ) <> 0 Then
    		Print #1, ROLLDIR +"secuenciaPLAY.txt no abre!"
  Else
      Print #1, ROLLDIR+"secuenciaPLAY.txt abierta!"
  EndIf
End  Sub 
'
Sub CargarPistasEnCancion ()
' cada vez que cargo borro la info de fechas de pistas anterior
ROLLCARGADO=FALSE

ntk=0
If instancia=ARG0_EN_LINEA Then
  DisableGadget(PISTASROLL,0) ' HABILITAMOS LISTA DE PISTAS
EndIf
ReDim As Double fechasPistas(1 To 32)
Print #1,"-------------------------------------------------------"
Print #1,"inicia CargaPistasEnCancion"
  Dim As String no1, no2
  Dim As Integer ubi1=0,ubi2=0 
     Dim As String filename, filenameold
     ' el Dir me trae lso nombres sin el path de cancion
     filename = Dir (NombreCancion+"\*.rtk")
     Print #1,"filename encancion > 0 ",filename
     If filename = "" Then ' no hay ningu archivo dentro del dir de cancion
        ntk=0
        Print #1,"dice que no hay archivos "
        CANCIONCARGADA=FALSE
        ' NO HAY NADA QUE CARGAR
        abrirRoll=CARGAR_MAS_PISTAS_O_CANCION ' habilita otro camino para cargar pistas
        Exit Sub
     Else
       ' carga Todos los tracks de cancion en un Loop
       Dim  As Integer mayor=1
       Do While Len(filename) > 0 ' If len(filename) is 0, exit the loop: no more filenames are left to be read.
        filenameold=filename
        Print #1, "trabajo con este filename...", filenameOld '[1]AAA.rtk por ejemplo
        Dim cadena As String
        cadena= sacarExtension(filenameOld) ' [1]AAA
       ''' If pistacreada=0 Then
          If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Then ' en batch no hay ventana control 
          Else   
          AddListBoxItem(PISTASROLL, cadena)
          End If  
       ''' EndIf
        ntk= sacarNtk(filenameold)  ' 1
       Print #1,"sacarNtk ntk "; ntk
        If mayor< ntk Then
           mayor=ntk
        EndIf
 
  ' va cargando los track internos, tomando el nro de track 
  ' del nombre del archivo solamente , ergo el usuario puede cambiar el orden
  ' o poner un trakck de otra cancion con un numero que no exista [x]
  '      
         ' cargamos un track simulamos como linea de cmd o cancion cargada
        nombre=NombreCancion+"\"+filename
        titulosTk(ntk)=nombre ' como agregue en titulos la opcion es 1
        ' como si viniera de lie ad ecomadno puedo usar cualquiera o 0
        pistasTk (ntk)=filename
        Print #1,"nombre en CargarPistasEnCancion ,Numpista",nombre, ntk
         'se carga track nada mas si dialogo simualmos un ubirtk>0 (1)
' chequeamos que todos los maxpos sean iguales sino se corrige
        
        CargarTrack  (Track() , ntk, 1 ) ' se cargo a track x nada mas

 'durante la cargaTrack el programa va a Rolloop se encuentra con SC_TAB
 ' y si cargacancion esta en 1 trata de cargar y no lo debe ahcer de modo
 ' que se ajusta a 0 cargacancion dentro de la rutina veremos 
'        
        Sleep 100
        filename = Dir()
                
       Loop
       Tope=mayor  ' el nro tope de la lista
       Print #1,"CARGO PISTAS MAXIMA CANTIDAD TOPE=",tope
       CANCIONCARGADA=TRUE
       NADACARGADO=FALSE
       ntk=0   ' tab le suma 1 pero si creo pistas estria  mal 05-03-2022 ï¿½ï¿½ï¿½
    EndIf
ntk=1
cargaCancion=CARGAR_NO_PUEDE_DIBUJAR '12-02-2022 mientras carg las pista el 1 indica cargando pistas    
    Print #1,"FIN CargaPistasEnCancion no debe calcuar *po ",*po
    Print #1,"-------------------------------------------------------"
clickpista=SI 'abre tab una sola vez y se posiciona en pista 1
'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0  05-mar-2024 veremos
'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 
      
End Sub

' ---------------------------------
Sub GrabarTrack (ntk As Integer)  'Error seguro aca nota=181 posicion1
' SIRVE  COMO ESTA PARA TICKS YA QUE POLI YA LE PUSE  ONOFF
' solo util para Grabar Cancion en sus track independietnes
' toma todos los valores de pmTk(ntk)
' Graba cancion en sus archivo sde tracks independientes...
' Otra forma serï¿½ DE MUCHOS TRACKS UN VECTOR, GRABA UNA SOLA OCURRENCIA 
' en donde todos los tracks cargado se grabaran a disco pero no como track 
' individual sino como el track de 2 dimensiones por lo tanto esta
' subruttina debe copiarse a otra y cambiarle el nombre diciendo <<GrabarCancion tipo carpeta>>
' como 1er paso deberia pasar de RollaTrack en memoria para que quede el Track
' en edicion con todo lo modificado en Roll. dEspues sï¿½, grabar el vector
' multiple con todos los tracks. Esa serai al forma compacta de grabar una
' cancion, OTra forma seria crear una carpeta y grabar todos los tracks o pistas 
' individualmente y ahi si usaria esta sub, teniendo en cuenta de que si el track 
' grabado esta en edicion en Roll 1ero pasar de rollatrack ..
' SE SUPONE QUE TODOS LOS VALORES O PARAMETRO SD TRACK EN ESTE MOMENTO 
' ESTAN EN LAS VARIABLES SIMPLES ADEMAS DEL VECTOR POR EOS NO HACE FALTA
' SACARLAS DESDE EL VACTOR ADEMAS EL TRACK PUDO HABER TENIDO MODIFICACIONES
' EL VECTOR ESTARA CON PARAM VIEJOS
 Dim gt As Integer
 Print #1,"---------------------------------------------------------------------------------"
 Dim ntknom As String 
 ntknom=titulosTk(ntk)
 Print #1,"  inicia GrabarTrack NUMERO= ",ntk
 gt=8
 Maxpos=pmTk(ntk).MaxPos
    If  Open (ntknom  For Binary Access write As #gt ) <> 0 Then
        Print #1,"No se puede Grabar Trk "; ntknom
        Exit Sub
    EndIf

     Dim Trabajo (1 To  MAxPos, 1 To lim3 ) As poli
' habiamos expandido poli debemos aprovecharlo 
'Type poli Field=1 ' para guardar la secuencia
' dur As  UByte =0   ' duracion 
' dur2 As UByte =0   ' SONIDO ON/OFF 
' dur3 As UByte =0   '  
' dur4 As UByte =0   '  
' dur5 As UByte =0   '  
' dur6 As UByte =0   '  
' dur7 As UByte =0   '  
' dur8 As UByte =0   '  

' nota As UByte =0 ' en un futuro contendra nota, octava, canal etc 
' vol As  UByte =0 ' volumen
' pan As  UByte =0 ' paneo
' pb  As  UByte =0 ' pitch bend
' nnn As UByte =0' se usa para escala canal etc 
' tick As ubyte =0' 128 tiene la redonda *1,75 segun pesoDur, 1 la cuartifusa o garrapatea todavia no la uso
' acorde  As ubyte =0 ' 1 a 12 , son el se hara el sort    
'End Type

   'r  Erase Trabajo ' todo a cero para un array fijo no dinamico
     Dim grabaPos   As poli ' 15 ubytes en cada poli 
     Dim grabaLim   As poli
     Dim graba3     As poli ' 04-02-2022 se agregan 48 bytes para info futura 
     Dim graba4     As poli 
     Dim graba5     As poli 
     Dim graba6     As poli 
     Dim graba7     As poli 
     Dim graba8     As poli 
     Dim graba9     As poli 
     Dim graba10    As poli 

   ' Total 150 ubytes en 10 poli 
   
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0,final=0
'eliminar columnas marcadas al grabar disco, 0 + X
'print #1, "inicio MaxPos "; MaxPos
 For i2 = 1 To MaxPos
     For i1 = 1 To lim2
       If Track(ntk).trk(i2,i1 ).nota=190 And Track(ntk).trk(i2,i1 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Track(ntk).trk(i2,i1 ).dur=182 Then ' 26-06-2021 copiar final archivo
          final= 1 'Atrapa el final
       EndIf

       If Track(ntk).trk(i2,i1 ).nota >=NB And Track(ntk).trk(i2,i1 ).nota<=NA-13 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= lim2 And haynota=1 Then ' sihay unasola notaen la columna no borro
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
      For i1 = 1 To lim3 ' aca copio controles tambien en lim3
          Trabajo(k, i1)=Track(ntk).trk(i2,i1 ) ' ACA IRIA LA COPIA DE ACORDE y control
      Next i1
    EndIf
      
    borrocol=0:res=0:haynota=0:final=0
 Next i2
' MaxPos = k JMG 16-05-2021 TODAVIA NO SE COMO SE ACHICA LA SECUENCIA 
' AL GRABAR NO SOLO CON MAXPOS PARECE,,,ANDA BIEN PERO SI CONSERVO MAXPOS
' DEL MISMO VALOR EN VEZ DE K     


posn=k        

Dim As Integer r1
For i1=1 To lim3 '  
  For r1= posn+1 To MaxPos
    Trabajo(r1, i1).nota=0 ' ok debo habiliatar desde ahi todas las columnas se juntaron
    Trabajo(r1, i1).dur=0  ' la secuecnia quedo mas corta
  Next r1 
Next i1  




'print #1, "final MaxPos "; MaxPos
'print #1,"---------------------------------------- K= "; K
'  For i1 = NB To NA
'  print #1,i1;"|";
'    For i2 = 1 To MaxPos
'       print #1,Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
'    Next i2
'    print #1,
'   print #1,"------------------------------------------" 
'  Next i1
  
 '''Shell "NOTEPAD test-AAAAA.TXT" 
''''Exit sub
     Dim As UByte y1,y2,y3,y4, y5 
     Dim As String a1,a2,a3,a4,a5 ,x
     
     
     x= Bin(MaxPos,20)
     '    print #1,"Posicion ",Posicion
     'Print "string representando ", x
     a1=Mid(x,1,4)
     a2=Mid(x,5,4)
     a3=Mid(x,9,4)
     a4=Mid(x,13,4)
     a5=Mid(x,17,4)
     '    print #1,"a1 a2 a3 a4 ",a1, a2 ,a3, a4

     y1= CUByte("&B"+a1)
     y2= CUByte("&B"+a2)
     y3= CUByte("&B"+a3)
     y4= CUByte("&B"+a4)
     y5= CUByte("&B"+a5)
     '    print #1, "y1,y2,y3,y4", y1,y2,y3,y4
     ' grabamos maxpos en 4 ubyte
     grabaPos.nota = y1
     grabaPos.dur  = y2
     grabaPos.vol  = y3
     grabaPos.pan  = y4
     grabaPos.pb   = y5
     grabaPos.nnn = pmTk(ntk).tipoescala
If  ntk >0 Then
     If CheckBox_GetCheck( cbxnum(ntk))= 1 Then ' sonido on/off 16-03-2022
         Print #1,"grabo grabaPos(1,1).dur2=1,, ntk", ntk
         grabaPos.sonido=1
     Else
         grabaPos.sonido=0
     EndIf
Else
         grabaPos.sonido=1

EndIf   
     '-----------------------
     grabaLim.nota = CUByte(pmTk(ntk).desde) 
     grabaLim.dur  = CUByte(pmTk(ntk).hasta)
     grabaLim.pb   = CUByte(pmTk(ntk).notaold) 
     grabaLim.vol  = CUByte(pmTk(ntk).notaescala) ''   CUByte(notaescala_num_ini) ' notadeescala 20-12-2021     


     grabaLim.pan  = pmTk(ntk).alteracion
   ''''  grabaLim.nnn = CUByte(tiempoPatron)
    
     graba3.dur = pmTk(ntk).portout
     graba3.nnn = pmTk(ntk).canalsalida ' as poli son tracks ojo 
   If NombreCancion > "" And ntk > 0 Then
     If CheckBox_GetCheck( cbxnum(ntk))= 1 Then ' sonido on/off 16-03-2022
         graba3.sonido=1
     Else
         graba3.sonido=0
     EndIf
   EndIf
     graba3.ejec =pmTk(ntk).ejec
     graba3.eco=pmTk(ntk).eco
     graba3.patch=pmTk(ntk).patch
     graba3.pan =pmTk(ntk).pan
     graba3.canal =pmTk(ntk).coro
     graba3.vol = pmTk(ntk).vol
     Print #1,"grabartrack graba3.vol, ntk ",graba3.vol , ntk

     graba3.pb = TipoCompas ' 26-04-2024
     graba3.nanchofig = CUByte(nanchofig*10)
       print #1,"MaxPos grabada en Trabajo ",MaxPos
Dim mit As aUshort
mit.ST = tiempoPatron
graba4.pan= mit.tp1
graba4.pb = mit.tp2
Print #1,"PASO GRABA4 tiempo patron elimino fileflush vermos"
  ''   FileFlush (gt)

     Put #gt, ,grabaPos
     Put #gt, ,grabaLim
     Put #gt, ,graba3  
     Put #gt, ,graba4  
     Put #gt, ,graba5  
     Put #gt, ,graba6  
     Put #gt, ,graba7  
     Put #gt, ,graba8  
     Put #gt, ,graba9  
     Put #gt, ,graba10 
     
     Put #gt, ,Trabajo() ' ACA DEBERIA QUEDAR EL CAMPO ACORDE !!
     Close gt
   FileFlush (gt)
     While InKey <> "": Wend
     Sleep 150
Print #1,"---------------------------------------------------------------------------------"
Print #1,"  Fin GrabarTrack "
End Sub

' ---------------------------------
'---------------------
Sub GrabarCancion() ' PENDIENTE GRABAR TODA LA CANCION EN UN SOLO COMANDO
 '1) recorro titulosTk(ntk) los titulos que quedan se graban, los otros se borran o
 ' en el momento de borrar se copia a backup y se borra del directorio de cancion
 ' eso si se borro alguna pista. (en vez de borrar mandamos a una carpeta de backup)
 '2) cada pista se graba con GrabarRollaTrack

' punto 2) 
Dim As Integer i,ntkold
       ntkold=ntk 

       Dim As Integer ubi1=0,ubi2=0,ubi3=0,ubi4=0 
       Dim As String no1,no2

  For i=0 To tope
        ntk=i 
       nombre=titulosTk(i)
       ubi1=InStr(nombre,"[")
       ubi2=InStr(nombre,"]")
       ubi3=InStr(nombre,".rtk")
''       ubi4=InStr(nombre,".roll")
       If ubi1 >0 And ubi2 > 0 Or ubi3 > 0 Then 'graba un track como track
       ' pero como las modificaciones estan en ROLL debo usar GrabarrollaTrack
  Print #1,"Grabar Cancion   rtk actual   ",ntk
'SI CARGUE UN RTK DESDE ARCHIVO SIEMPRE USO ROLL A  TRACK PORQUE
' SE PUDO EDITAR EL ROLL VISUAL Y ROLL TENDRA LA ULTIMA MODIFICAION
' eso solo para el track que esta siendo visto en pantalla pero
' los que no se ven ya fueron refrescados , o sea debo
' grabar Track n a disco y al final grabar rolla track solo para
'el visual
            GrabarRollaTrack (0) ' nuevo deberia ser asi sin cambiar estencion 0
 
        '''   GrabarTrack ntk  tenia esto cual va 
           Sleep 20           
       EndIf

  Next i

       ''  GrabarRollaTrack(0)
  'ntk=ntkold ''??

End Sub


Sub  selport (ByVal mitipo As integer)  ' list de ruso
' SELECCION DE CASO 1 PORT DE SALIDA OUT, O CASO 2 ENTRADA MIDI IN
' PARA EJEC usamos selportEjec

Dim As hwnd haw,hwl
Dim As Integer x0,y0,Posx,Posy 
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
Dim cad As String
 
''portsout = port_count (midisal)
' ESTOY ABRIENDO LSO PORT SEGUN LO ELEGIDO CADA VEZ QUE SE ELIJA UNO
' DEJA DE ESTAR DISPONIBLE PUES YA ESTA ABIERTO, PONDREMOS UN MENSAJE ABIERTO CREO ES MEJOR 
Print #1, "selport 1 "
    If NombreCancion > "" Then
    Else
       ntk=0
    EndIf
Print #1, "selport 2 "



'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("MIDI",500,Posy,500,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
Print #1, "selport despues open window "
Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
    
     hwl=  ListViewGadget(1,10,10,350,400, LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL  )
Print #1, "selport despuesde listviewgadget  "
     listports()
'lsitports y listout son para ports y eso es comun para roll rtk y ejec  
Print #1, "selport 3 ntk "; ntk
     If mitipo = 1 Then ' out
       AddListViewColumn(1, "Salidas  MIDI",0,0,250)
       For aa As Integer=0 To UBound (listout) 
          If pmTk(ntk).portout = aa  Then
             AddListViewItem(1, "[x] "+listout(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listout(aa),0,aa,0)
          EndIf   
           
       Next aa

     EndIf
     If mitipo = 2 Then ' in entradas
      
       AddListViewColumn(1, "Entradas MIDI",0,0,250)
       For aa As Integer=0 To UBound (listin) 
          If pmTk(ntk).portin = aa  Then
             AddListViewItem(1, "[x] "+listin(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listin(aa),0,aa,0)
          EndIf   

       Next

     EndIf

Print #1, "selport 4 "
       ButtonGadget(2,380,30,100,40,"CAMBIA")
' No sepuede poner otro boton porque la lista de pistas que da en blanco para siempre
         #Ifdef __FB_WIN32__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         'menunew=0
         Do

         Var event= waitEvent
         If event=EventLBDown Then ' 26-02-2022
            If EventNumberListView=1 Then
               If mitipo=1 Then 
                  portout=GetItemListView
               EndIf
               If mitipo=2 Then
                 portin=GetItemListView
               EndIf 
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)
               If  mitipo=1 Then           
                  If InStr(cad,"x") >0  Then
                     cad="[ ] " +listout(portout) 
                  Else  
                     cad="[x] " +listout(portout) 
                  EndIf
               EndIf

               If mitipo=2 Then 
                  If InStr(cad,"x") >0  Then
                    cad="[ ] " +listin(portin) 
                  Else  
                    cad="[x] " +listin(portin) 
                  EndIf 
               EndIf 

               ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If event=eventgadget Then
            If eventnumber()=2 And InStr(cad,"x")> 0 Then
              'menunew=0
              '''portout = GetItemListView()
' faltan tipos 3 y 4 para ejec..
              If mitipo=1 Then
              Print #1,"PORT SELECCIONADO en menu ",portout
' si editamos un track de una cancion modificamos en el track ntk
' y en el nt=0 de edicion o sea roll visual y track(0) , track(0) solo pertenece a Roll
              pmTk(ntk).portout=CUByte(portout)
              pmTk(0).portout=CUByte(portout)
' MODIFICACION EN MASA SI VARISAPISTAS SE PASAN A OTRO DISPOSITIVO
' SE LOS HARA EN LOS CHEQUEADOS
               For  i As Short =1 To 32
                     If CheckBox_GetCheck( cbxnum(i))= 1  Then
                      pmTk(i).portout=CUByte(portout)
                    EndIf
               Next i
              EndIf

              If mitipo=2 Then
              Print #1,"PORT SELECCIONADO en menu ",portin
                pmTk(ntk).portin =CUByte(portin)
                pmTk(0).portin =CUByte(portin)
              EndIf

'=> NO VAMOS A GRABAR AUTOMATICO AL CAMBIAR UN PARAMETRO ESO ES DESICION DEL USUARIO
' TAL VEZ PODRIA SER UNA OPCION, SI LA OPCION ES SI , GRABAREMOS, PERO EL CAMBIO DE
' PARAMETROS ES UNA PRUEBA HASTA DECIDIR QUE ESTA BIEN, ENTONCES EL USUARIO DEBERIA GRABAR
'              If CANCIONCARGADA=TRUE  Or TRACKCARGADO=TRUE  Then
'                 If NombreCancion > ""  And MAxPos > 1 Then
'                    GrabarRollaTrack(0)
'                 EndIf
'              Else
'                If MaxPos > 1  And ROLLCARGADO  Then
' NO VAMOS A GRABAR AUTOMATICO AL CAMBIAR UN PARAMETRO ESO ES DESICION DEL USUARIO  
'             LLAMA_GRABAR_ROLL()
                 '''GrabarRoll () ' graba roll en edicion, borro todo el undo??
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
'                 Sleep 1000,1 
'                EndIf  
'              EndIf  
              Close_Window(haw)
              Exit Do 
            End If

          EndIf 
          If event=EventClose Then
              Close_Window(haw)
               Exit Do 
          EndIf
          
         Loop 


  
'' fin ruso
Print #1, "selport fin  "
cierroport=0
End Sub

Sub  selportEjec (ByVal mitipo As Integer, ByVal pis As Integer )  ' list de ruso
' SELECCION DE CASO 1 PORT DE SALIDA OUT, O CASO 2 ENTRADA MIDI IN
Dim As hwnd haw,hwl
Dim As Integer x0,y0,Posx,Posy 
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
Dim cad As String
 
''portsout = port_count (midisal)
' ESTOY ABRIENDO LSO PORT SEGUN LO ELEGIDO CADA VEZ QUE SE ELIJA UNO
' DEJA DE ESTAR DISPONIBLE PUES YA ESTA ABIERTO, PONDREMOS UN MENSAJE ABIERTO CREO ES MEJOR 
Print #1, "selport 1 "
'    If NombreCancion > "" Then
'    Else
'       ntkp=0
'    EndIf
Print #1, "selport 2 "



'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("MIDI",500,Posy,500,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
Print #1, "selport despues open window "
Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
    
     hwl=  ListViewGadget(1,10,10,350,400, LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL  )
Print #1, "selport despuesde listviewgadget  "
     listports()
  
Print #1, "selport 3 ntkp global, pis parametro es lo mismo  "; ntkp, pis
     If mitipo = 1 Then ' OUT OUT OUT SALIDA
       AddListViewColumn(1, "Salidas  MIDI",0,0,250)
       For aa As Integer=0 To UBound (listout) 
          If pmEj(pis).portout = aa Then
             AddListViewItem(1, "[x] "+listout(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listout(aa),0,aa,0)
          EndIf   
           
       Next aa

     EndIf
     If mitipo = 2 Then ' in entradas
      
       AddListViewColumn(1, "Entradas MIDI",0,0,250)
       For aa As Integer=0 To UBound (listin) 
          If pmEj(pis).portin = aa Then
             AddListViewItem(1, "[x] "+listin(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listin(aa),0,aa,0)
          EndIf   

       Next

     EndIf

Print #1, "selport 4 "
       ButtonGadget(2,380,30,100,40,"CAMBIA")
' No sepuede poner otro boton porque la lista de pistas que da en blanco para siempre
         #Ifdef __FB_WIN32__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         'menunew=0
         Do

         Var event= waitEvent
         If event=EventLBDown Then ' 26-02-2022
            If EventNumberListView=1 Then
               If mitipo=1 Then 
                  portout=GetItemListView
               EndIf
               If mitipo=2 Then
                 portin=GetItemListView
               EndIf 
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)
               If  mitipo=1 Then           
                  If InStr(cad,"x") >0  Then
                     cad="[ ] " +listout(portout) 
                  Else  
                     cad="[x] " +listout(portout) 
                  EndIf
               EndIf

               If mitipo=2 Then 
                  If InStr(cad,"x") >0  Then
                    cad="[ ] " +listin(portin) 
                  Else  
                    cad="[x] " +listin(portin) 
                  EndIf 
               EndIf 

               ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If event=eventgadget Then
            If eventnumber()=2 And InStr(cad,"x")> 0 Then
              'menunew=0
              '''portout = GetItemListView()

              If mitipo=1 Then
              Print #1,"PORT SELECCIONADO en menu ",portout
              pmEj(pis).portout=CUByte(portout)
              tocaparam(pis).portout=CUByte(portout)
                  
              EndIf
              If mitipo=2 Then
              pmEj(pis).portin=CUByte(portin)
              tocaparam(pis).portin=CUByte(portin)

              EndIf

              Close_Window(haw)
              Exit Do 
            End If

          EndIf 
          If event=EventClose Then
              Close_Window(haw)
               Exit Do 
          EndIf
          
         Loop 


  
'' fin ruso
Print #1, "selport fin  "
cierroport=0


End Sub


' 
Sub  selcanal (ByVal mitipo As Integer )  ' list de ruso
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw, hwl
Dim As Integer x0,y0,Posx,Posy,cambio=0  

Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
'' => desde aca echo con tool del ruso no anda muy bien
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100

     haw=OpenWindow("CANALES MANUALES",500,Posy,500,400,WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )

     hwl=  ListViewGadget(1,10,10,300,300,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL  )

     
     Dim As String mensaje, cad 
     If mitipo = 1 Then ' out
      mensaje="Canales Salida"
     EndIf
   '  If mitipo = 2 Then ' in entradas   'CANALES DE ENTRADA NO TIENE SENTIDO POR AHORA
   '   mensaje="Canales Entrada"
   '  EndIf 
     
     AddListViewColumn(1, mensaje ,0,0,250)
     For aa As Integer=1 To 16 
         If  pmTk(ntk).canalsalida = aa -1 Then
               AddListViewItem(1, "[x] "+listCanal(aa),0,aa,0)
               Print #1,"pmTk(ntk).canalsalida ",pmTk(ntk).canalsalida
              '''' ntk=aa 
         Else
               AddListViewItem(1, "[ ] "+listCanal(aa),0,aa,0)
         EndIf 
         
     Next
   ''''GroupGadget(GRUPO_BTNS_OKCAN ,320,20,130,130,"")
        ButtonGadget(2,330,30,100,40,"CAMBIA")


         #Ifdef __FB_WIN32__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf


         Do

         Var event= waitEvent

         If event=EventLBDown Then
            If EventNumberListView=1  Then
               canalx=GetItemListView
Print #1, "SELCANAL canalx "; canalx
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)           
               If InStr(cad,"x")>0 Then
                 cad="[ ] " +Str(canalx+1)
               Else  
                 cad="[x] " +Str(canalx+1)
               EndIf

              ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If event=eventgadget Then
            If eventnumber()=2 And InStr(cad,"x") >0 Then
            '  canalx = GetItemListView() ' sirve para el roll actual en edicion
           ' get itme va de 0 a 15 no ahce falta convertir 
' esto sirve para cambia rdinamicamente el canal midi mientras tocamos, perp tambien
' si grabamos el archjivo se grabara y pisara la info de midi canal en el archio
' al cargar el archivo de nuevo se tomara ese canal.              

          
          ' carga de canal MIDI al vector parametros de tracks de cancion
          
               Print #1,"ntk en cambio de canal ntk= ",ntk
               pmTk(ntk).canalsalida=CUByte(canalx) ' sirve para el track cargao actual en cancion
               pmTk(0).canalsalida=CUByte(canalx) ' sirve para el track cargado actual Roll de edicion
            If  MAxPos > 1 Then
               pmTk(ntk).canalsalida=CUByte(canalx)
               pmTk(0).canalsalida=CUByte(canalx)
               If NombreCancion > ""  Or TRACKCARGADO =TRUE Then
                   ' sirve para el track cargao actual en cancion
                     GrabarRollaTrack(0)
               Else
                  If ROLLCARGADO=TRUE  Then
                    LLAMA_GRABAR_ROLL()
                   ''  GrabarRoll () ' graba roll en edicion, borro todo el undoï¿½?
                  ' no el undo dolo se debe borrar al ahcer nuevo creo
                   Sleep 1000,1 
                  EndIf  
               EndIf  
            End If  
               
               Close_Window(haw)
               Exit Do 
            End If
          EndIf
          If event=EventClose Then
                  Close_Window(haw)
               Exit Do 

           EndIf
            
         
         Loop 
  
'' fin ruso
/'
If EventNumberListView= 1 Then
   cad=GetTextItemListView(1,GetItemListView,0)           
   ReplaceTextItemListView(1,GetItemListView,0,cad + " [X]")
           
   ReplaceTextItemListView(1,canalx,0, Str(canalx)+" x")
EndIf
'/                 

End Sub
'----------------
Sub  selcanalEjec (ByVal mitipo As Integer, ByVal pis As Integer )  ' list de ruso
' 03-10-2024 todavia falla no muestra el canal seleccionado!!
' REVISAR REVISAR REVISAR JMG
Dim As hwnd haw,hwl
Dim As Integer  x0,y0,Posx,Posy,cambio=0  

'Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
'' => desde aca echo con tool del ruso no anda muy bien
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100

     haw=OpenWindow("CANALES EJECUCION",500,Posy,500,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
     hwl=  ListViewGadget(1,10,10,300,300,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL  )
     
     Dim As String mensaje, cad 
     If mitipo = 1 Then ' out
      mensaje="Canales Salida"
     EndIf
     If mitipo = 2 Then ' in entradas
      mensaje="Canales Entrada"
     EndIf 
     
     AddListViewColumn(1, mensaje ,0,0,250)
     For aa As Integer=1 To 16 
         If pmEj(pis).canalsalida = aa -1 Then
               AddListViewItem(1, "[x] "+listCanal(aa),0,aa,0)
               Print #1,"pmEj(pis).canalsalida ",pmEj(pis).canalsalida
                
         Else
               AddListViewItem(1, "[ ] "+listCanal(aa),0,aa,0)
         EndIf 
         
     Next
 
       ButtonGadget(2,330,30,100,40,"CAMBIA")
       '''ButtonGadget(3,330,80,100,40,"CANCELA")
           SetForegroundWindow(haw)        
           SetFocus (hwl) 

        '  #Else
        '   gtk_widget_grab_focus(GadgetID(1))
        ' #EndIf
         Do

         Var event= waitEvent

         If event=EventLBDown Then
            If EventNumberListView=1  Then
               canalx=GetItemListView
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)           
               If InStr(cad,"x")>0 Then
                 cad="[ ] " +Str(canalx+1)
               Else  
                 cad="[x] " +Str(canalx+1)
               EndIf

              ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If event=eventgadget Then
            If eventnumber()=2 And InStr(cad,"x") >0 Then
            '  canalx = GetItemListView() ' sirve para el roll actual en edicion
           ' get itme va de 0 a 15 no ahce falta convertir 
' esto sirve para cambia rdinamicamente el canal midi mientras tocamos, perp tambien
' si grabamos el archjivo se grabara y pisara la info de midi canal en el archio
' al cargar el archivo de nuevo se tomara ese canal.              

          
          ' carga de canal MIDI al vector parametros de tracks de cancion
          ' canales van de 0 a 15 que sonenelmenu 1 a 16
              If mitipo=1 Then 'salida
                   Print #1,"CANAL SELECCIONADO en menu ",canalx
                   For  i As Short =1 To 32
                         If CheckBox_GetCheck( cbxejec(i))= 1 Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
                             pmEj(i).canalsalida=CUByte(canalx)
                             tocaparam(i).canal=CUByte(canalx)
                        EndIf
                   Next i
              EndIf
              If mitipo=2 Then ' entrada
                  Print #1,"CANAL SELECCIONADO en menu ",canalx
                For  i As Short =1 To 32
                      If  CheckBox_GetCheck( cbxgrab(i))= 1 Then
                        pmEj(i).canalentrada =CUByte(canalx)
                        tocaparam(i).canalent=CUByte(canalx)
                      EndIf
                Next i
             EndIf          
               
               Close_Window(haw)
               Exit Do 
            End If
''            If eventNumber()= 3 Then
''               Close_Window(haw)
''               Exit Do 
''            EndIf
          EndIf
           If event=EventClose Then
                  Close_Window(haw)
                 Exit Do 
           EndIf           
         
         Loop 
  
'' fin ruso

End Sub
'''---------
function sizeMIO() As Integer
   Static As Integer r,g,b
   Dim As Integer selR=GetScrollGadgetPos(1),selG=GetScrollGadgetPos(2),selB=GetScrollGadgetPos(3)
   If r<>selR Or g<>selG Or b<>selB Then
      r=selR : g=selG : b=selB
      SetGadgetText(4,Str(r)):SetGadgetText(5,Str(g)):SetGadgetText(6,Str(b))
      SetGadgetColor(7,BGR(r,g,b),0,1)
   EndIf
   Return TRUE
End Function

Sub SELMIERDA()
'probamos deusar scrollbar ScrollBarGadget
' anda barbaro ELLA SOLA,..pero igual INTERFIERE CON LA SELECCION DE PISTAS 
' EN LA LISTA DE PISTAS ROLL UNA CANCION
' NO SIRVE PARA UNA MIERDA JAJAJAJA O SEGURO NO LA SEUSAR EN MI CONTEXTO
' YO ESTOY CONTROLANDO AL GRAFICO DE ROLLL AL DAR CLICK A UNA PISTA Y SE COMPORTA
' COMO EL TAB CAMBIO DE PISTA EN EL GRAFICO ,,PERO SI USO CUALQUIER GADGET ESPECIAL
' PARA AJUSTAR EL VALOR DEL PARAMETRO SE CAGA LA SELECCION DE PISTA UN FIASCO,,

Dim As HWND hw
Dim As Integer ev

hw=OpenWindow("TESTCOLOR",10,10,370,140): CenterWindow(hw)
ScrollBarGadget(1,10,10,200,20,0,255):SetGadgetColor(1,&hff,1,3) 'R
ScrollBarGadget(2,10,40,200,20,0,255):SetGadgetColor(2,&hff00,1,3) 'G
ScrollBarGadget(3,10,70,200,20,0,255):SetGadgetColor(3,&hff0000,1,3)'B
TextGadget(4,220,10,30,20,Str(GetScrollGadgetPos(1)),SS_CENTER):SetGadgetColor(4,0,&hff,2) 'R
TextGadget(5,220,40,30,20,Str(GetScrollGadgetPos(2)),SS_CENTER):SetGadgetColor(5,0,&hff00,2) 'G
TextGadget(6,220,70,30,20,Str(GetScrollGadgetPos(3)),SS_CENTER):SetGadgetColor(6,0,&hff0000,2) 'B
TextGadget(7,260,10,80,80):SetGadgetColor(7,0,0,1)


SetTimer(hw,1,20,Cast(Any Ptr,@sizeMIO))

Do
   ev=WaitEvent
Loop Until ev=EventClose


 End Sub
'---------
Sub SelEco (ByRef rever As ubyte)

'ESTE GADGET TAMBIEN INTERFIERE EN LA SELECCION DE PISTA DESDE VENTANA DE CONTROL
' OTRA MIERDA,,,
Var hwndeco=OpenWindow("",300,500,1050,150)
ButtonGadget(1,20,20,60,25,"Aplicar")
TrackBarGadget(2,20,60,1000,40,0,127)
TextGadget(3,200,20,30,20)
SetTrackBarPos(2,Clng(pmTk(ntk).eco))
Do
   var event=WaitEvent()
   If event=EventGadget Then
      Select case EventNumber
         Case 2
            rever=GetTrackBarPos(2)
            pmTk(ntk).eco=rever
            pmTk(0).eco=rever 
       Print #1,"ECO SELECCIONADO ->> "; rever
            SetGadgetText(3,Str(rever))
              
         Case 1
            Close_window (hwndeco)
            Exit Do 
      End Select
   EndIf
   If event=Eventclose Then
        Close_window (hwndeco)
        Exit Do 
   EndIf

Loop

End Sub
'-------
Sub  seloctava  ( ByRef octadesde As Integer, ByRef octahasta As integer) 
Dim As hwnd haw,hwl
Dim As Integer aa ,pasoc=0,x  
Dim listoctava(1 To 9) As string ={"0","1","2","3","4","5","6","7","8"}

For x= 1 To 2 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("OCTAVAS",100,50,400,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     hwl=  ListViewGadget(1,10,10,300,400,,,,32,LVS_SINGLESEL  )
    ' listports()
     GadgetToolTip(1,"Dos Columnas la derecha es la numeracion de RollMusic, la izquierda la que se suele usar." )
  If pasoc=0 Then
       AddListViewColumn(1, "Elegir Octava Desde ",0,0,250)
       For aa =1 To 9 
          If  desde = aa  Then
           AddListViewItem(1, "[x] "+listoctava(aa)+"-"+Str(aa),0,aa,0)
          Else  
           AddListViewItem(1, "[ ] " +listoctava(aa)+"-"+Str(aa),0,aa,0)
          EndIf  
       Next
       
  EndIf   
  If pasoc=1 Then 'HASTA  
       AddListViewColumn(1, "Elegir Octava Hasta ",0,0,250)
       For aa =1 To 9
           If  hasta = aa  Then
             AddListViewItem(1, "[x] "+listoctava(aa)+"-"+Str(aa),0,aa,0)
           Else  
             AddListViewItem(1, "[ ] " +listoctava(aa)+"-"+Str(aa),0,aa,0)
           EndIf  
       Next
  EndIf


       ButtonGadget(2,330,30,50,40," OK ")
       GadgetToolTip(2,"Al dar OK el Rango de Octavas elegido sera el usado por el Grafico")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         'menunew=0
Dim As Integer i
         Do

         Var eventC= waitEvent

          If eventC=eventgadget Then
            If eventnumber()=2 Then
'              menunew=0
              If pasoc=0 Then
                 Octadesde = GetItemListView()
                 Octadesde=Octadesde+1
                 pmTk(ntk).desde=CUByte(Octadesde)
                 pmTk(0).desde=CUByte(Octadesde)
              EndIf   
              If pasoc=1 Then
                 Octahasta = GetItemListView()
                 Octahasta=octahasta+1 
                 pmTk(ntk).hasta=CUByte(Octahasta)
                 pmTk(0).hasta=CUByte(Octahasta)
 
              EndIf   

              Close_Window(haw)
              pasoc=1
              Exit Do 
            End If
          EndIf 
          If eventC= EventClose Then
             Close_Window(haw)
             Exit Do 
          EndIf
          
         Loop 
Next x
'' fin ruso
If octadesde=0 Then
octadesde=1
EndIf
If octahasta=0 Then
octahasta=9
EndIf
print #1,"OCtadesde ",octadesde
print #1,"OCtahasta ",octahasta

End Sub
'--------------------------
'---------------------------
Sub selInstORdenAlfa(ByRef instru As Integer)
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw,hwl
Dim As Integer aa ,x=0,i1=0,i2=0 ,Posx,Posy ,x0,y0  
Dim cad As String
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
' la seleccion empieza de 1, no devuelve 0 para el 1er elemento 
 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("PATCH POR ORDEN ALFABETICO CLICK EN UN ITEM  Y EN CAMBIA",500,Posy,700,600, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     ' commctrl.bi modificado
     hwl=  ListViewGadget(1,10,10,500,500,,,,32,LVS_SINGLESEL )
 GadgetToolTip(1,"Seleccionar instrumento por nombre, deseleccionar el que este seleccionado y luego seleccionar otro y pulsar CAMBIA." )    
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
    '' AddListViewItem(1, "CLICK EN UN ITEM  Y EN CAMBIA",0,aa,0)
     
       For aa =1 To 127 
               i2=InStrrev(NombreInstAlfa(aa)," ")
               cad=Mid(NombreInstAlfa(aa),i2)
      '         Print #1,"cadena ",cad
               
     
           If instru = CUByte(ValInt(cad)) Then
              AddListViewItem(1, "[x] "+NombreInstAlfa(aa),0,aa,0)
           Else
              AddListViewItem(1, "[ ] "+NombreInstAlfa(aa),0,aa,0) 
           EndIf
           
       Next
       


       ButtonGadget(2,530,30,100,40,"CAMBIA")


         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventAlfa= waitEvent
         If eventAlfa=EventLBDown Then ' 26-02-2022
            If EventNumberListView=1 Then
               i1 = GetItemListView() +1
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)           
               If InStr(cad,"x")>0 Then
                 cad="[ ] " +NombreInstAlfa(i1) 
               Else  
                 cad="[x] " +NombreInstAlfa(i1) 
               EndIf
               ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If eventAlfa=eventgadget Then
          
            If eventnumber()=2 And InStr(cad,"x") >0 Then
               ''i1 = GetItemListView()
               Print #1,"alfa seleccion in", i1
               Print #1,"NombreInstAlfa ",NombreInstAlfa(i1)
               i2=InStrrev(NombreInstAlfa(i1)," ")
               cad=Mid(NombreInstAlfa(i1),i2)
               Print #1,"cadena ",cad
               instru = CUByte(ValInt(cad))
               pmTk(ntk).patch=CUByte(instru)
               pmTk(0).patch=CUByte(instru)
                print #1,"seleccion instrumento alfa instru = ",instru     
               patchsal=instru
                  Close_Window(haw)
                  Exit Do
              
            End If
          EndIf 
          If eventAlfa=EventClose Then
                  Close_Window(haw)
                 Exit Do 
           EndIf          
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
print #1,"Str(instru) ", Str(instru)
  

end Sub

'-------------------
Sub ComboBox() ' lo deje de usar pero veo si sirve para algo en el futuro

#Ifdef __FB_WIN32__
Var h = 80
#Else
Var h = 30
#EndIf
Dim As hwnd haw,hwl
haw= OpenWindow("",10,10,400,150, WS_VISIBLE,WS_EX_TOPMOST)
hwl=ComboBoxGadget(1,10,10,100,h)
AddComboBoxItem(1,"Hola cero",-1)
AddComboBoxItem(1,"Hola uno ",-1)
AddComboBoxItem(1,"Hola dos",-1)
ButtonGadget(2,150,10,220,20,"Select an item and click here")
 SetForegroundWindow(haw)
'menunew=0
Do
   Var event=WaitEvent()
   If event=eventclose Then End
   If event=eventgadget Then
      If eventnumber()=2 Then
         Print GetItemComboBox(1)
         MessBox("","This is a TextGadget " + Str( GetItemComboBox(1)))
         Close_Window(haw)
         Exit Do
      EndIf
   EndIf
   Sleep 5
Loop


End Sub
 
Sub grabaprueba()
Print #1,"MAxPos inicio ", MAxPos
     Dim Trabajo (1 To Maxpos, NB To NA) As datsec
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0
'eliminar columnas marcadas al grabar disco
 For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=254 And Roll.trk(i2,i1 ).dur=254 Then
          borrocol= 1 'Atrapa al menso un caso
          
       EndIf
       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
          Print #1, Roll.trk(i2,i1 ).nota;"*";Roll.trk(i2,i1 ).dur;"|";
       EndIf
       
       If i1= NA And haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar IMPRIME
       Else
          If borrocol = 1 Then
             res=1  
          EndIf  
       EndIf            
    Next i1
      
      If borrocol = 0 Or res=0 Then 'copio columna no borro
        k=k+1
        For i1 = NB To NA
          Trabajo(k, i1)=Roll.trk(i2,i1 )
          If Trabajo(k, i1).nota >=1 And Trabajo(k, i1).nota<=12 Then
          Print #1,"C:"; Trabajo(k, i1).nota;"*";Trabajo(k, i1).dur;"|";
          EndIf  
        Next i1
      
      Else
       MaxPos=MaxPos -1  
      EndIf
      
      borrocol=0:res=0:haynota=0
      Print #1,
 Next i2
'-------------------------
 Print #1,"MAxPos Final ", MAxPos
Print #1,"------------------------------------------"
For i1 = NB To NA
    For i2 = 1 To MaxPos
     If Trabajo(i2,i1).nota = 181 Then 
        Trabajo(i2,i1).nota = 0
     EndIf
     If Trabajo(i2,i1).dur = 181 Then 
        Trabajo(i2,i1).dur = 0
     EndIf
     If Trabajo(i2,i1).nota = 254 Then 
        Trabajo(i2,i1).nota = 0
     EndIf
     If Trabajo(i2,i1).dur = 254 Then 
        Trabajo(i2,i1).dur = 0
     EndIf
'     If Trabajo(i1,i2).nota >=1 And Trabajo(i1,i2).nota<=12 Then  
'       print #1, Trabajo(i1,i2).nota;"*";Trabajo(i1,i2).dur;"|";
'     EndIf  
     Print #1, Trabajo(i2,i1).nota;"*";Trabajo(i2,i1).dur;"|";
    Next i2
    Print #1,
   Print #1, "------------------------------------------" 
Next i1
'shell"notepad midebug.txt"
Exit Sub 
'-------------------------

End Sub
'
Sub borrarColumnasMarcadas() '<============ ES COMPLEJO cambiarlo basandonos en 
'   borrarZona

' ESTO DEBERIA BORRARLO CREO borrarZona anda bien solo  con pulsar delete 
' no hace falta ctrl-delete ni marcar nada

' toda columna en una posicion que tenga solo 190 en su nota y duracion
' sera eliminada y todas las demas desplazadas a izquierda,.
' es trabajoso hay que pulsar ctrl-9 y dar x en toda la columna 
' anda ok , igualmente implementare por zona 
' pregunto si las zonas estan marcadas uso ese emtodo y sino uso el metodo viejo

'<====== METODO NUEVO POR ZONAS ===> sI ESTAN MARCADAS Y PULSO CTRL-DELETE SE BORRA TODA ESA
' por columna un solo click es medio dificil debo acomodar el click con busconota
' y no tiene sentido las notas en Tick ocupan varios ticks no solo uno solo seria
' el caso de un acorde apilados exactamente todos en un mismo tick,,,
' pero igual es un intervalo de tick de modo que se descarta una columna sola 
' ZONA O COLUMNA SI SE SUPERPONEN LAS MARCAS DE ZONA 2 CLICKS SOBRE LA MISMA COLUMNA
' O UNA SOLO CLIK MEJOR,,,,
' pasoZona1 pasoZona2 
Print #1," BORRACOLUMNASMARCADAS"
' PARA TICKS DEBO PRIMERO BORRAR LAS NOTAS EN ESE INTERVALO QUE TENGAN UN OFF=2
' Y BORRAR SU OFF=1. LOS OFF=1 QUE NO  TENGAN SU OFF=2 NO SE BORRARAN.
' LUEGO COMO SE QUE SE BORRARON LAS NOTAS AL COPIAR Y SOBREESCRIBIR SOLO HACERLO
' SI NO ES UN OFF=1 O SEA LOS OFF=1 QUEDAN SIN PISAR
Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0,delta=0
' 1) se ajusta una zona de trabajo es el caso que debe usarse mejor siemrpe seleccionar
' una zona que contenga a la zona que incluye a la nota aislada en lo posible
If pasoZona1 > 0 Or (pasoZona1 > 0 And pasoZona2 > 0) Then
' metodo nuevo ok
  Print #1,"pasoZona1 pasoZona2 ",pasoZona1, pasoZona2
' ESTO SERA DIFICIL DE EJECUTAR DEBERIA BUSCAR NOTA, BORRAR LA JUNTO A U OFF=1
' Y NO BORRAR NINGUN OFF=1 QUE NO SEA DE ESA NOTA Y EL INTERVALO BORRADO SERIA DESDE
' CUALQUIER OFF1 QUE QUEDE DENTRO DE LA DURACION  DE LA NOTA BORRADA HASTA UN PROXIMO OFF2
 
/' metodo descartado  
  If pasoZona1 > 0 And pasoZona2 = 0 Then ' borrado de esa columna ok
    Print #1,"entra por pasoZona1  "
    For k=pasoZona1 To Maxpos 
     For i1 = NB To NA
         Roll.trk(k, i1)=Roll.trk(k+1,i1 )
     Next i1
    Next k
    pasoZona1=0
  EndIf
'/
' BORRADO DE ZONA: BORRAR LAS NOTAS CUYO OFF=2 ESTE DENTRO DEL RANGO, LUEGO
' COPIAR TODO LO QUE SIGUE DESDE  EL COMIENZO DEL RANGO, SI HAY UN OFF1 NO 
' SOBREESCRIBIRLO SALVO QUE COINCIDA UN OFF1 EXISTENTE CON UN OFF2 COPIADO EN
' ESE CASO EL OFF2 SOBREESCRIBE AL OFF1. LISTO  anda ok!!! 30-05-2025  
  If pasoZona1 > 0 And pasoZona2 > 0 Then ' borrado del rango del paso ok
    Print #1,"entra por pasoZona1 y 2  "
    delta=pasoZona2 - pasoZona1
    Dim  As Integer posnOff,DURX 

    For k=pasoZona1 To pasoZona2
    'borrar notas completas en el intervalo para todo el intervalo  
     For i1 = NB To NA
     'borrar notas completas en el intervalo para esa columna
        
         If Roll.trk(k, i1).onoff=2  Then 'borrar nota
            DURX=Roll.trk(k, i1 ).dur
            posnOff =k + DurXTick(DURX) -1
            Roll.trk(k, i1 ).dur = 0
            Roll.trk(k, i1).nota = 181
            Roll.trk(k, i1).onoff = 0
            Print #1,"dur POSNoFF "; DURX, posnOff   
'busco onoff=1 puede estar mas 1 o menos 1 segun la version en la que se hizo el .roll
' LO EXTIENDO A +2 -2, SIEMRPE SERA ASI EN EL FUTURO SERA EXACTO NO HABRA ARCHIVOS VIEJOS DE ROLL
            Roll.trk(posnOff -2, i1).onoff = 0
            Roll.trk(posnOff -2, i1).dur  = 0
            Roll.trk(posnOff -1, i1).onoff = 0
            Roll.trk(posnOff -1, i1).dur = 0
            Roll.trk(posnOff   , i1).onoff = 0
            Roll.trk(posnOff   , i1).dur = 0
            Roll.trk(posnOff +1, i1).onoff = 0
            Roll.trk(posnOff +1, i1).dur = 0
            Roll.trk(posnOff +2, i1).onoff = 0
            Roll.trk(posnOff +2, i1).dur = 0

         EndIf
     Next i1
' copiar lo que seguia al princio del intervalo pero sin pisar los onoff=1 que quedaron
    Next k

     Dim As Integer k2=pasoZona1, lim
     lim = maxpos -6 - delta

    For k=pasoZona2 To MaxPos -6
     k2=k2+1
     If k2= lim Then
        Exit For
     EndIf
     For i1 = NB To NA
         If Roll.trk(k2, i1).onoff=0 Then   
           Roll.trk(k2, i1)=Roll.trk(k,i1 )
           Roll.trk(k,i1 ).dur =181
           Roll.trk(k,i1 ).nota =0
           Roll.trk(k,i1 ).onoff =0
           Roll.trk(k,i1 ).pb =0
           Roll.trk(k,i1 ).vol =0
           Roll.trk(k,i1 ).pan =0
           Roll.trk(k,i1 ).inst =0
           
         ElseIf Roll.trk(k2, i1).onoff=1 And  Roll.trk(k,i1 ).onoff=2   Then
           Roll.trk(k2, i1)=Roll.trk(k,i1 )
           Roll.trk(k,i1 ).dur =181
           Roll.trk(k,i1 ).nota =0
           Roll.trk(k,i1 ).onoff =0
           Roll.trk(k,i1 ).pb =0
           Roll.trk(k,i1 ).vol =0
           Roll.trk(k,i1 ).pan =0
           Roll.trk(k,i1 ).inst =0
           
     
         EndIf
     Next i1  
     
    Next k
  
     pasoZona1=0 :pasoZona1=0
     MaxPos=MaxPos - delta -1
  EndIf
  
Else
' <====METODO VIEJO ======> NO USAR MARCAS CTRL 9 tal vez lo descarte probaremos 30-05-2025
' est parte es para pazosona1=0 y pasozona2=0
' 1) NO se ajusta una zona de trabajo
For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=190 And Roll.trk(i2,i1 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA Then 
        If haynota=1 Then ' si hay una sola nota en la columna no borro
         res=0 ' no borrar
        Else
          If borrocol = 1 Then
             res=1  
             MaxPos=MaxPos -1
          EndIf  
        EndIf
       EndIf             
    Next i1
    i1=0  
    If borrocol = 0 Or res=0 Then 'copio columna no borro
       k=k+1
      If k<> i2 Then 
         For i1 = NB To NA
            Roll.trk(k, i1)=Roll.trk(i2,i1 )
         Next i1
      EndIf
    EndIf
      
    borrocol=0:res=0:haynota=0
Next i2

End If
 
End Sub

Sub salir()
' NO PUEDE ESTAR EN MUSICCONTROL.BAS DEBE ESTAR EN ALGUNA PARTE DE ROLLMUSIC PARA QUE FUNCIONE

eventM=eventrbdown
Dim  As Integer i3
    cairo_destroy(cm)
    cairo_surface_destroy( surf2 )
    cairo_destroy(c)
    cairo_surface_destroy( surface )

 ' no usar nunca deallocate falla montonde cosas ni si quiera se puede cerrar
 ' un archivo ni borrarlo una cagada la recomendacion del foro!!!!          
            FT_Done_Face( ftface ) ' 04-05-2024 ESTO DIO ERROR UNA VEZ PERO SE EQUIVOCO EL DEBUGUER
           If play=SI Or playb=SI Then
              '''alloff (canal)
              
              For i3 = 1 To tope
               portsal = pmTk(i3).portout 
               alloff (pmTk(i3).canalsalida, portsal )
               close_port(midiout(portsal))
               out_free(midiout(portsal))'

               portsal = pmEj(i3).portout 
               alloff (pmEj(i3).canalsalida, portsal )
               close_port(midiout(portsal))
               out_free(midiout(portsal))

              Next i3
              ThreadDetach(thread1)              
              ThreadDetach(thread2)

           EndIf

'Sleep 10
   If teclado=1 Then
      cancel_callback(midiin(pmEj(ntkp).portin))
     Dim k1 As Integer
       k1=pmEj(ntkp).portout
    '   Print #1,"midiout ",k1, *nombreOut(k1)
       alloff( pmEj(ntkp).canalsalida,k1 )  
       listoutAbierto(k1)=0
       close_port midiout(k1)
       teclado=0

   EndIf

    ffile=4

    If Open (ROLLDIR +"RollMusic.ini" For Output As #ffile) <> 0 Then
       Print #1, "No se puede escribir RollMusic.ini en Salir"
       Close ffile
       Exit Sub 
    Else 
         If nmxold = 0 Then
            nmxold=mxold
            nmyold=myold
         EndIf   
         If nancho=0 Then
            nancho=ANCHO
            nalto =ALTO
         EndIf    
     
         If ndeltaip=0 Then
            ndeltaip=inc_Penta
         EndIf
         nanchofig=anchofig
         Print #ffile,font , " font"
         Print #ffile,nmxold, " mxold "
         Print #ffile,nmyold, " myold"
         Print #ffile,nANCHO, " ANCHO"
         Print #ffile,nALTO, " ALTO"
         Print #ffile,ndeltaip, " inc_Penta"
         Print #ffile,nVerEscalasAuxiliares, "nVerEscalasAuxiliares"
         Print #ffile,nanchofig, "nanchofig"
         Print #ffile,nVerCifradoAcordes, "nVerCifradoAcordes"
        FileFlush (ffile)         
         Close ffile
         Sleep 100
    EndIf

 Dim As Integer nroproc, ppp=0
Dim As String linea
 
 If Open (ROLLDIR+"procesos.txt" For Input As 2) <> 0 Then
   Print #1,"procesos.txt error al leer  "
   Close 2
   Exit Sub
 Else
       ' carga Todos los pid de las instancias

       Do While Not EOF(fa1)
        Line Input #fa1, linea
        nroproc=CInt(linea)
        ' -print #1, "trabajo con este proceso..", linea 
        ' -print #1,"SE SuPONE PID1 PRINCIPAL NO MATAR ",pid1
        ' -print #1,"pid1 <> nroproc ",pid1, nroproc
        Print #1,"pid1 , nroproc ",pid1, nroproc
        If pid1 <> nroproc  Then
          Print #1,"terminar pid ", linea
          WINEXEC ("C:\WINDOWS\SYSTEM32\taskkill /PID "+ linea + " /T /F ",00)        
        EndIf
        
      Loop      
FileFlush (fa1)
    Close fa1

 EndIf


''''''' Screen 0 ''', , ,  GFX_SCREEN_EXIT '''&h80000000
ThreadDetach(threadloop)
Screen 0
Close_Window(hwnd)
''DestroyWindow(hWnd)

''''Sleep 100
 
 'NO ESTA DEFINIDO en  FBGFX.INI ,&h80000000
' UNA MIERDA COMO ESTA ESCRITO LE FALTA CONSTANTES , KEYS ETC HAY QUE HACER UNA PROPIA
' EN EL HELP EXISTE.. ME ENTERE DESPUES DE 3 AÑOS ,,,JAJAJAJA
' SI SE USA &H80000000  QUE ADEMAS DE CERRAR LA VENTANA GRAFICA CERRARIA LA CONSOLA
' SI LO HUBIERA,,,,ESO HACE CANCELAR SI PRIMERO HAGO PLAY DE UNA CANCION Y LUEGO CIERRO
' LA VENTANA,,,PER CON SOLO SCREEN 0 CIERA SOLO EL GRAFICO Y NO CANCELA DESPUES DE
' ESCUCHAR UNA CANCION.... OTRA CAGADA SUPERADA A LOS TUMBOS!! AL FINAL PARA GRAFICOS
' ES BUENO QUE NO ESTE DEFINIDA LA CONSTANTE PORQUE HACE MIERDA EL PROGRAMA,,,

End Sub
'-------
Sub selTipoEscala (ByRef tipoescala_num As Integer)
Dim As hwnd haw,hwl
Dim As Integer aa ,paso1=0,x=0  
tipoescala_num=0

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("ESCALAS ",100,50,600,600,WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
'     Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
' COSA DE MANDINGA LVS_EX_AUTOSIZECOLUMNS ACA NO LA ENCUENTRA PERO EN LAS OTRAS
' SUBRUTINAS LA ENCUENTRA JAJAJAA ANDA PARA LA MIERDA ESTE FB 
''Y ESTE NO ES UN MODULO A PARTE ES UN INCLUDE!!!     
     ' commctrl.bi modificado
Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     hwl=  ListViewGadget(1,10,10,500,500,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 168 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN OK",0,aa,0)
       For aa =1 To 168 
               AddListViewItem(1, escala(aa).nombre ,0,aa,0) 
       Next
       


       ButtonGadget(2,530,30,50,40," OK ")
'       ButtonGadget(3,530,90,50,40,"+Pag")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventEsca= waitEvent

          If eventEsca=eventgadget Then
          
            If eventnumber()=2 Then
               tipoescala_num = GetItemListView()
               pmTk(ntk).tipoescala=CUByte(tipoescala_num)
               pmTk(0).tipoescala=CUByte(tipoescala_num)
                  Close_Window(haw)
                  Exit Do
           End If

          EndIf 
         If eventEsca= EventClose Then
               Close_Window(haw)
               Exit Do 
          EndIf
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
Print #1,"Str(tipoescala_num) ", Str(tipoescala_num)

End Sub
'----
Sub selNotaEscala (ByRef notaescala_num As Integer)
Dim As hwnd haw,hwl
Dim As Integer aa ,paso1=0,x=0  
notaescala_num=0

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("ESCALAS ",100,50,500,500, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
'     Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
' COSA DE MANDINGA LVS_EX_AUTOSIZECOLUMNS ACA NO LA ENCUENTRA PERO EN LAS OTRAS
' SUBRUTINAS LA ENCUENTRA JAJAJAA ANDA PARA LA MIERDA ESTE FB 
''Y ESTE NO ES UN MODULO A PARTE ES UN INCLUDE!!!     
     ' commctrl.bi modificado
Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     hwl=  ListViewGadget(1,10,10,400,350,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 12 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN OK",0,aa,0)
       For aa =1 To 12 
           If alteracion = "bem" Then
             AddListViewItem(1, Notasescala2(aa) ,0,aa,0)
           Else
             AddListViewItem(1, Notasescala(aa) ,0,aa,0)
           EndIf

       Next
       


       ButtonGadget(2,430,30,50,40," OK ")
'       ButtonGadget(3,530,90,50,40,"+Pag")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventNota= waitEvent

          If eventNota=eventgadget Then
          
            If eventnumber()=2 Then
               notaescala_num = GetItemListView()
               pmTk(ntk).notaescala=CUByte(notaescala_num)
               pmTk(0).notaescala=CUByte(notaescala_num)
                  Close_Window(haw)
                  Exit Do
           End If

          EndIf 
         If eventNota= EventClose Then
               Close_Window(haw)
               Exit Do 
         EndIf
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
Print #1,"Str(notaescala_num) ", Str(notaescala_num)


End Sub

Sub EscalaAlternativa (ByRef tipoescala_alt As Integer, ByRef notaescala_alt As Integer)
' si la escala principal es menor le sumo una 3era menor y obtengo la escala Mayor relativa
' ej Lam->+3st=DoM
' si la escala principal es Mayor le resto una 3era menor y obtengo la escala Menor relativa
' ej DoM->-3st=DoM 
'Entrada a este programa son las shared tipoescala_num_ini y notaescala_num_ini
' tipoescala_num_ini es tipoesc en sub armarescala
' notaescala_num_ini es notaesc en sub armarescala
' la nota de la escala alternativa sale de caminar 3 saltos hacia adelante o atras sobre la escala
' cromatica..saltos de 1 semitono, 3 st.-
' veo si estoy en sostenidos o bemoles
'Print #1,"EscalaAlternativa tipoescala_num_ini ",tipoescala_num_ini
'Print #1,"EscalaAlternativa notaescala_num_ini ",notaescala_num_ini
' ï¿½y la menro armonica la considero tambien = 2 ?
If tipoescala_num_ini =2 Or tipoescala_num_ini =3 Or tipoescala_num_ini =4 Then  ' es una escala Menor debo buscar la relativa Mayor
   notaescala_alt=notaescala_num_ini +3  
 

'  If alteracion="sos" Then ' usar
'     notastr=NotasEscalaArmado(notaescala_alt)
'  EndIf 
'  If alteracion="bem" Then ' usar
'     notastr=NotasEscalaArmado2(notaescala_alt)
'  EndIf 
  
' y el tipo escala_alt ES UNA EOLIA O AEOLIA la escal menor natural lo mas simple
' o sea aeolia(18) 2,1,2,2,1,2,2, o melodicaMenorDesc 2,1,2,2,1,2,2 -----(4)
' la natural es realmente la 18 aeolia o eolia...
' ergo
tipoescala_alt=1 ' aeolia

EndIf

  
If tipoescala_num_ini =1 Then  ' es una escala MAyor debo buscar la relativa menor  

  notaescala_num_ini=notaescala_num_ini+12 ' 2 octavas asi retrocedo sin problemas
   notaescala_alt=notaescala_num_ini -3  
 

'  If alteracion="sos" Then ' usar
'     notastr=NotasEscalaArmado(notaescala_alt)
'  EndIf 
'  If alteracion="bem" Then ' usar
'     notastr=NotasEscalaArmado2(notaescala_alt)
'  EndIf 
  If notaescala_alt > 12 Then
    notaescala_alt=notaescala_alt -12 ' para una escala de solo 1 octava
  EndIf
    
' tengo la nota escala y el tipo solo resta armar al escala para la posicion como antes
' debo ousar notaescala_alt y  
tipoescala_alt=18 ' Menor aeolia es la alternativa de cualqueir modo mayor

EndIf

' al salir de aca graba en el vector Roll esta escala...
End Sub
Function BuscarGrado(t3 As String) As Integer
  'Print #1,"=>busca grado entrada nrocromatico ",t3 
  t3=Trim(t3)
Dim iz As Integer
  For iz= 1 To 12
     If t3 = Trim(grado_inicial (iz)) Then
        BuscarGrado=iz
   '     Print #1,"t3, grado encontrado ",t3, iz 
        Exit For
     EndIf
  Next iz
End Function
'--------
Sub armarescala(ByRef cadena As String, tipoesc As Integer, notaesc As Integer, altera As String, orden As Integer )
cadena=""
If tipoesc=0 Then ' 10-06-2022 
   Exit Sub
EndIf
Dim As Integer k3=1,h2,hnro,dif,dif2
Dim p6 As Byte Ptr
' si notaescala es distinto de 1 emepzamos la escala desde esa nota
  ' Print #1,"tipo escala que llego a armar escala ",tipoesc
   p6= escala(tipoesc).pasos
   
   hnro=escala(tipoesc).nropasos 
   'Print #1,"notaescala llego a armarescala ",notaesc
   If notaesc >= 1 Then
     k3=notaesc 
   EndIf
   ' Print #1,"k3 inicial=",k3
    For h2 = 1 To hnro 
     If altera="bem" Then
        cadena = cadena + NotasEscalaArmado2(k3) + " "   ' impresion de la escala con las notas c c# d  etc
        If orden=1 Then
          notas_esc_inicial(h2)=NotasEscalaArmado2(k3)
          notas_esc_inicial(h2+hnro)=NotasEscalaArmado2(k3)
          grado_inicial(h2)=NotasEscalaArmado2(k3)
          grado_inicial(h2+hnro)=NotasEscalaArmado2(k3)

        EndIf
     Else
        cadena = cadena + NotasEscalaArmado(k3) + " "
        If orden=1 Then
          notas_esc_inicial(h2)=NotasEscalaArmado(k3)  ' 06-02-2022
          notas_esc_inicial(h2+hnro)=NotasEscalaArmado(k3)      
          grado_inicial(h2)=NotasEscalaArmado(k3)
          grado_inicial(h2+hnro)=NotasEscalaArmado(k3)

        EndIf
     EndIf
      k3= *p6 + k3
    ' Print #1,"h2, k3",h2, k3
     p6=p6+1
 '    If orden=1 Then 
 '       Print #1,"h2 notas_esc_inicial(h2) ",h2, notas_esc_inicial(h2)
 '       Print #1,"grado_inicial(h2) ",grado_inicial(h2)
 '       Print #1,"h2 notas_esc_inicial(h2+hnro) ",h2+hnro, notas_esc_inicial(h2+hnro)
 '       Print #1,"grado_inicial(h2) ",grado_inicial(h2+hnro)
        
'     EndIf
    Next h2
    
    
  'Print #1,"Final cadena de escala armada ",cadena
 
  
End Sub
Function lugarNota (lugar As Integer) As Integer
          lugar=nsE-lugar
        If lugar <= 0 Then
           lugar=lugar + 12
        EndIf            
        If lugar >= 12 Then
           lugar=lugar -12 
        EndIf  
lugarNota=lugar
End Function

Sub armarAcorde(grado As Integer,n1 As Integer,n2 As Integer,n3 As Integer)
  ' para mayo4      n1=4 y n2=7
  ' para menor      n1=3 y n2=7
  ' para disminuido n1=3 y n2=6
'Print #1,"4 MousexNotaElegida"; MousexNotaElegida
nVerCifradoAcordes=3
Dim As Integer acoposoff=0, resultado,acoposOffNew
   scan_alt=0
   cnt_acor=cnt_acor +1 
   If grado=0 Then grado=1 EndIf ' todo pasa por grado 1 por ahora triadas

'   Print #1,"Grado ",grado

'   Print #1, "5 mousex ";MousexNotaElegida, "anchofig "; anchofig; "gap1 ";gap1

   curpos=(MousexNotaElegida -gap1)/anchofig '''19-07-2025
'   Print #1, "2 curpos o Col "; curpos 
   notacur=nsEelegida
'   Print #1,"notacur "; notacur
   indicePos=curpos+posishow  

   resultado= BuscarNota (1,curpos, notacur)  
   If resultado = 1 Then
'      Print #1,"BuscarNota sin resultados curpos notacur ",curpos,notacur
       Exit sub
   Else
      ''indicePos=curpos + posishow ''- gap1/anchofig
      indicePos= curpos + posishow 
'      Print #1, "(2)indicePos, notacur, curpos,posishow ",indicePos, notacur,curpos, posishow
'      Print #1,"encontro onoff 2 " ,Roll.trk(indicePos, PianoNotaElegida+SumarnR(PianoNotaElegida)).onoff 
                
  EndIf
  Dim As Integer st=0
  Dim pnr As Integer
        ' armar acorde notapiano, Notapiano+4, NotaPiano+7=la anterior +3
        ' haremos todo a mano luego algun dia un algoritmo si es necesario,.,,
        ' qu etenga en cuenta todos los casos (formar mayor-> +4 +7)
        '  n=o es 1 tonica ,n=1 es 3,n=2 es 5,n=3 es 7
''FALTA MUCHO LABURO SI LA DURACION ES IGUAL A UNA ROOT EXISTENTE NO SE TOCA EL ONOFF 1
'' SI ES DISTINRA SE BORRA EL ONOFF 1 Y SE CREA UNO NUEVO
'' SI NO HAY NOTA FUNDAMENTAL ES VACIO SE CREA LA TONICA SE COLOCA SU ONOFF2 Y ONOFF1
'' L UNDO LO DEJAMOS PARA EL FINAL NOS CONCENTRAMOS EN QUE SE ARME BIEN 
'------------------------------------------------------------------
'Guardamos el nro de acorde en la nota origen que tiene el onoff2 de 1 a 255 un ubyte
pnr=PianoNotaElegida+SumarnR(PianoNotaElegida)
Print #1,"indicePos, pnr) ";indicePos, pnr

 Roll.trk(indicePos, pnr).pb=CUByte(acordeNro) ' nuevo para ticks el otro acordeNro es aconro 

      Select Case grado ' la nota elegida sera la Tonica del acorde
        Case 1 To 7  ' es Tonica PROBADO OK!, 
' grabo la nota Tonica de la melodia  a partir de la cual hago el acorde
'        Print #1,"ENTRA POR GRADO 1 ACORDE SIN INVERSION TONICA"
      'TONICA:---------------------------
        pnr=PianoNotaElegida+SumarnR(PianoNotaElegida)    

        undo_kant_intervalos(cnt_acor)=0  ' tonica no hay intervalo es ella sola          
        undo_acorde(cnt_acor,0).posn=indicePos ' shared global la old actual ¿¿?
        undo_acorde(cnt_acor,0).pn=pnr ' se almacena elindice fisico del vector Roll
        undo_acorde(cnt_acor,0).dur=Roll.trk(indicePos, pnr).dur 'old PUEDE SER 0 O 181
        undo_acorde(cnt_acor,0).nota=Roll.trk(indicePos, pnr).nota 'old 
        undo_acorde(cnt_acor,0).onoff=Roll.trk(indicePos, pnr).onoff ''' PUEDE SER 2 Ó 0 NADA
' posicion  anterior del off 1
        If Roll.trk(indicePos, pnr).dur > 0 And Roll.trk(indicePos, pnr).dur <> 181 Then
           acoposOff=indicePos + DurXTick  ( Roll.trk(indicePos, pnr).dur ) -1
        Else
           acoposOff=0 ' porque no hay nota es vacio 
        EndIf
        Roll.trk(indicePos, pnr).dur=CUByte(RollDur) ' CARGAMOS LA DUR DEL USUARIO QUE SERA IGUAL
' posiicon nueva del off 1
        acoposOffNew=indicePos + DurXTick (RollDur) -1
' O DISTINTA A LA QUE EXISTIA EM ESE LUGAR
' saltos de octava
        st=nsEelegida '  tonica 
'Print #1,"armando acorde ,TONICA ",st

       posn=acoposOff +1
       If posn > MaxPos Then 
          MaxPos=posn +6 ' la figura tiene 5 posiciones si pongo 1 o 2  se pisa
          pmTk(ntk).MaxPos=MaxPos
       EndIf 
      If  acoposOffNew <> acoposOff And acoposOff > 0 Then
        Roll.trk(acoposoff, pnr).onoff=0
        Roll.trk(acoposoff, pnr).dur=181
        Roll.trk(acoposoff, pnr).nota=0
      EndIf
        Roll.trk(acoposoffNew, pnr).onoff=1
        Roll.trk(acoposoffNew, pnr).dur=183
        Roll.trk(acoposoffNew, pnr).nota=183
       
      
       pmTk(ntk).posn=posn
       notaOld = nota
       posnOffOld=0 


        
'--------n1 acorde de 3 notas------------------------------------
  '      Print #1,"armando acorde ,5ta,nR ",pnr  

        pnr=PianoNotaElegida+n1 ' TERCERA ya no es la pianonota de la nota elegida 
        pnr=pnr + SumarnR(pnr) ' es nR indice del vector Roll

        undo_kant_intervalos(cnt_acor)=1
        undo_acorde(cnt_acor,1).posn=indicePos
        undo_acorde(cnt_acor,1).pn=pnr
        undo_acorde(cnt_acor,1).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,1).nota=Roll.trk(indicePos, pnr).nota
        undo_acorde(cnt_acor,1).nota=Roll.trk(indicePos, pnr).onoff

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
        
        st=nsEelegida + n1 '''-n2
'Print #1,"armando acorde ,3ERA,nR ",st 
        If st <= 0 Then
           st=12 + st
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            

        Roll.trk(indicePos, pnr).nota=st  
        Roll.trk(indicePos, pnr).onoff=2

        Roll.trk(acoposoffNew, pnr).onoff=1
        Roll.trk(acoposoffNew, pnr).dur=183
        Roll.trk(acoposoffNew, pnr).nota=183
       



'--------n2 acorde de 3 notas------------------------------------

        pnr=PianoNotaElegida+n2 ' QUINTA
        pnr=pnr + SumarnR(pnr) ' es nR indice del vector Roll

        undo_kant_intervalos(cnt_acor)=2
        undo_acorde(cnt_acor,2).posn=indicePos
        undo_acorde(cnt_acor,2).pn=pnr
        undo_acorde(cnt_acor,2).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,2).nota=Roll.trk(indicePos, pnr).nota
        undo_acorde(cnt_acor,2).nota=Roll.trk(indicePos, pnr).onoff

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
        st=nsEelegida + n2 '''-n2 
'Print #1,"armando acorde ,5ta ",st
        If st <= 0 Then
           st=12 + st
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            

        Roll.trk(indicePos, pnr).nota=st  
        Roll.trk(indicePos, pnr).onoff=2


        Roll.trk(acoposoffNew, pnr).onoff=1
        Roll.trk(acoposoffNew, pnr).dur=183
        Roll.trk(acoposoffNew, pnr).nota=183
       



If n3 <> 0 Then ' es un acorde de 4 notas            
        pnr=PianoNotaElegida+n3
        pnr=pnr + SumarnR(pnr)

        undo_kant_intervalos(cnt_acor)=3 
        undo_acorde(cnt_acor,3).posn=indicePos
        undo_acorde(cnt_acor,3).pn=pnr
        undo_acorde(cnt_acor,3).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,3).nota=Roll.trk(indicePos, pnr).nota
        undo_acorde(cnt_acor,3).nota=Roll.trk(indicePos, pnr).onoff

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
        st=nsEelegida+n3 
'    Print #1,"armando acorde ,7ta,nR ",pnr

        If st <= 0 Then
           st=12 + st
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            

        Roll.trk(indicePos, pnr).nota=st
        Roll.trk(indicePos, pnr).onoff=2

        Roll.trk(acoposoffNew, pnr).onoff=1
        Roll.trk(acoposoffNew, pnr).dur=183
        Roll.trk(acoposoffNew, pnr).nota=183
       


EndIf
      End Select   
     '' grado=0   
 
 If Vaciodur Then
    ReCalCompas ()
    Vaciodur=FALSE
 EndIf
 Dim As Integer guardpos
 guardpos=posicion
 pasozona1=indicePos -10 
 pasozona2=indicePos +DurXTick(RollDur)
 playAll (roll)  'PLAYACORDE
 pasozona1=0
 pasozona2=0
 posicion=guardpos
 RollDur=0
 '''DUR=0
 
End Sub
Function FiguraEquivalente(DURk As Double) As UByte
Dim As UByte i=0,j=0,k=0 ,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0
Dim As Double resta,vabs
'p1=DURk  ' parte entera detecta hasta 7 negras juntas (O+P+I)
' comienzo con los valores mas chicos
'Dim As Integer partes_falta (1 To 20), partes_sobra(1 To 20)
'Dim As Integer cantPArtes,falta
' no hace falta el vector!! solo emitir DUR
FiguraEquivalente=0
For i=37 To 45  '2.666 etc
 resta=DURK-relDur(i)
 vabs = Abs(resta)
 If vabs <= 0.0001  Then
    j=j+1 
    FiguraEquivalente=i
' si estoy en el borde del compas necesitarre separadur
'''    separarDur(j,DUR, partes_sobra(),cantSobra,sobra)
'ahra tengo 
    Exit Function
 EndIf

Next i

For i=1 To 9  
 resta=DURK-relDur(i)
 vabs = Abs(resta)
 If vabs <= 0.0001  Then
    j=j+1 
    FiguraEquivalente=i
    Exit Function
 EndIf

Next i

For i=10 To 18  
 resta=DURK-relDur(i)
 vabs = Abs(resta)
 If vabs <= 0.0001  Then
    j=j+1 
    FiguraEquivalente=i
    Exit Function
 EndIf

Next i

For i=19 To 27  
 resta=DURK-relDur(i)
 vabs = Abs(resta)
 If vabs <= 0.0001  Then
    j=j+1 
    FiguraEquivalente=i
    Exit Function
 EndIf

Next i

For i=28 To 36  
 resta=DURK-relDur(i)
 vabs = Abs(resta)
 If vabs <= 0.0001  Then
    j=j+1 
    FiguraEquivalente=i
    Exit Function
 EndIf

Next i

End Function

Sub duraciones (midi() As NProc, k1 As Integer,k2 As Integer  ) ' 15-05-2024
On Local Error Goto fail
' sacado de rutina teclado inmerso en roolloop
' la idea es usarlo para reconstruir las duraciones de la carga
' de un midi plano 
' OPCION NO MOSTRAR SILENCIOS,.,,,IF SILENCIO=0 ...
''filtro1=128 :filtro2=144
' dato1 es on / off en ejecucion midi pero aca tenemos de entrada
' el on y off  del midi plano 
' NOTA: dato1 saldra en este caso de midi(k1).non y off
' en ese caso cargar dato1 con 144 o 128 y listo estoy suponiendo
'  tocada de midi pero es el vector que tiene los on y off
' o sea ..ESte algoritmo como se ve respecto de las duraciones de durcla
' quantiza automaticamente... entre dos valores contiguos.
' trae 3 figuras que unidas representan bastante aproximado al valor real
' las cuales salen de entre las 182 posibilidades con o sin sonido.
' el que cierra el ON es el OFF y   da su duracion aca solo tomamos los ON
' no hay duraciones de OFF 
'If  midi(k1).non > 0 Then
'    dato1=144'
'EndIf 
'If  midi(k1).noff > 0 Then
'    dato1=128
'EndIf
' APLICABLE PARA PASAR DE EJECUCIONES EN TOCA A TRK Y DE AHI A ROLL GRAFICO
'CTRL1207 -'CONVIERTE UNA PISTA DE EJECUCION SELECCIONADA EN PISTA TRK
 
Dim  i2 As Integer
' filtro1 y filtro2 no tiene sentido aca se elimina ...
' DURk es el deltatime O TIMESTAMP ...son segundos en caso entrada por midi
' pero aca ya es el  valor relativo a la duracion de una negra
' la negra vale 1 ,puede ser fraccion de negra o mayor...
'DURk=midi(k1).dur ' lo pasare de nuevo a doble..
Erase duras 
 If midi(k1).dur >0   Then 'And cnf=0  Then
   '  t2k=Timer
      'contcode = 1 'A CERO al pulsar Edit
      Print #31," midi(k1).dur ";midi(k1).dur; " "; "midi(k1).nota "; midi(k1).nota 
     'DURK=t2k-t1k
       numfloat=midi(k1).dur/((60/tiempoPatron) / FactortiempoPatron)
       
Print #31,"numfloat ", numfloat 
      ' la duracion debe mantenerse constante por mas que el tempo cambie 
     ' asi las figuras seran las mismas pero el tempo las tocara mas rapido nada mas
     ' si toco una negra a 120 no sera una corchea, sera una negra,,,
dato1=128 ' tomamos los cierres off, si una nota on tiene vol=0 ahi pasariamos a silencios
' pero no solo con los off aca es diferente ...revisar el mecanismo de play teclado midi..
    If  numfloat  > 0.03  Then
					For i2=1 To 44
					 If numfloat >= durcla(i2,1) And numfloat < durcla(i2+1,1) And i2< 45 Then
					    Print #31, "1) esta entre ",durcla(i2,1), " y ",durcla(i2+1,1), numfloat
					    numfloat=numfloat-durcla(i2,1)  ' le resta la menor del intervalo
             Select Case dato1
      '         Case 144  'on
		'			     duras(1)=durcla(i2,2)+45 ' estos son ilencios no va
               Case 128 'off
					     duramidi(1)=durcla(i2,2) 
              Print #31, "duras(1) " ;duramidi(1)
            End Select 
					     numdurasmidi(1)=1
					    Exit For
					 EndIf
			
			   Next i2
			  '   Print #1,"---------duraciones 1---------------" 
   EndIf
   If  numfloat  > 0.08  Then ' para que entren tresillos chicos pero no todos
 					For i2=1 To 44
						   If numfloat >= durcla(i2,1) And numfloat < durcla(i2+1,1) And i2< 45 Then
						     Print #31,"2) esta entre ",durcla(i2,1), " y ",durcla(i2+1,1),numfloat
						     numfloat=numfloat-durcla(i2,1)
             Select Case dato1
          '     Case 144
					 '    duras(2)=durcla(i2,2)+45
               Case 128
					     duramidi(2)=durcla(i2,2)
  Print #31,"duras(2) " ; duramidi(2)
            End Select 
  	             numdurasmidi(1)=2

						     Exit For
						   End If
						Next i2
   
					'	Print #1,"-----------duraciones 2-----------------"
						For i2=1 To 44
						   If numfloat >= durcla(i2,1) And numfloat < durcla(i2+1,1) And i2< 45 Then
			 			 '    Print #1,"3) esta entre ",durcla(i2,1), " y ",durcla(i2+1,1),numfloat
						      numfloat=numfloat-durcla(i2,1)
             Select Case dato1
      '         Case 144
				'	     duras(3)=durcla(i2,2)+45
               Case 128
					     duramidi(3)=durcla(i2,2)
            End Select 

			           numdurasmidi(1)=3
						     Exit For
						   EndIf
						Next i2
         numfloat=0
   EndIf
  '   Select Case dato1 
  '     Case 128   'off
  '        Velmidi=0
   '     Case 144  ' on
   '       Velmidi=dato3 ' guardarla

  '   End Select
        '     If numdurasmidi =2 Then
  	     	'	       duras(1) = duras(1)+90
         '    EndIf		
        '     If numdurasmidi =3 Then
  	     	'	       duras(1) = duras(1)+90
  	     	'	       duras(2) = duras(2) +90
        '     EndIf		



						If duramidi(1) >0 Then
Print #31, duramidi(1)
			         Print #31, figura(duramidi(1)),
						EndIf
						If duramidi(2) >0 Then
Print #31, duramidi(2)
						   Print #31, figura(duramidi(2)),
						EndIf
						If duramidi(3) >0 Then
Print #31, duramidi(3)
						   Print #31, figura(duramidi(3))
						EndIf
						
    
  EndIf
'-----
If midi(k1).volum =0    Then
   Print #31, "VOLUM=0 "
   If duramidi(1) > 0 Then 
      duramidi(1)=duramidi(1) + 45 'silencio '10 junio 2024
      Print #31,"duramidi(1) "; figura(duramidi(1)) '50 OK DA SL ANTES DE AUTOFRACTURAR
   EndIf
   If duramidi(2) > 0 Then
      duramidi(2)=duramidi(2) + 45 'silencio '10 junio 2024
      Print #31,"duramidi(2) "; figura(duramidi(2))
   EndIf
   If duramidi(3) > 0 Then
      duramidi(3)=duramidi(3) + 45 'silencio '10 junio 2024
      Print #31,"duramidi(3) "; figura(duramidi(3))
   EndIf
EndIf
' ------------fin duraciones
'' midi(k1).nligado
Print #31, "PORQUE MIERDA NO SALE LIGADO ? midi(k1).nligado "; midi(k1).nligado
If midi(k2).nligado > 0   Then
   Print #31, "LIGADO "
   If duramidi(1) > 0 Then 
      duramidi(1)=duramidi(1) + 90 ' ligado 24 julio 2024
      Print #31,"duramidi(1) "; figura(duramidi(1)) '50 OK DA SL ANTES DE AUTOFRACTURAR
   EndIf
   If duramidi(2) > 0 Then
      duramidi(2)=duramidi(2) + 90 'ligado  24 julio 2024
      Print #31,"duramidi(2) "; figura(duramidi(2))
   EndIf
   If duramidi(3) > 0 Then
      duramidi(3)=duramidi(3) + 90 'ligado  24 julio 2024
      Print #31,"duramidi(3) "; figura(duramidi(3))
   EndIf
EndIf

Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error duraciones " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
End If



End Sub
'''-///////////// RUTINAS DE VENTANA PRINCIPAL ROLLCONTROL ////////
''' empezamos a estructurar un poco .....06- marzo 2024
'' REVISAR DE NUEVO LAS CONDICIONES DE SALIDA DE SELECT O EXIT DO

'---------

' error
errorSub:
  
'Dim As Long  ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 1 And ContadorError < 101 Then
 Print #1,"------------------------------------"
  ContadorError=ContadorError+1
  Print #1,"ErrorSub ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
  Print #1,"Error Function: "; *Erfn()
  Print #1, "n ",posicion;" posishow "; posishow; " NroCol ";NroCol
  Print #1, "semitono "; nsE; " *po "; *po 
  Print #1, "valor1 ",posicion; " valor2 "; 12- nsE  + (*po -1)* 13
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
  Print #1, "lbound 2 de Roll.trk ", LBound(Roll.trk, 2)   
  Print #1, "Roll.trk (n,11- nsE  + (*po-1) * 13 ).nota", Asc(Str(Roll.trk (posicion,12- nsE  + (*po -1)* 13 ).nota))
  Print #1,"Roll.trk (n,12- nsE  + (*po -1)* 13 ).dur ",  Asc(Str(Roll.trk (posicion,12- nsE  + (*po -1) * 13 ).dur))
  Print #1,"gap1 + ic * anchofig ",gap1 + ic * anchofig
  Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po
  Print #1, "nsE ";nsE; " nR ";nR; " PianoNota ";PianoNota
  Print #1, "mensaje, Ermn ", *Ermn, Ermn
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2) 
  Print #1,"------------------------------------"

EndIf
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )








