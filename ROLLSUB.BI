 
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

On  Error GoTo errorSub


Sub ArmarDurFrac ()
' los bloques de durcion se repiten de a 3, el incr es por bloque
' si voy al 2do bloque de 3 el incrdeja de ser 0 y pasa a 27 respectro de la
' 1er linea l cargo TRAck como 2da linea
Print #1,"armardurFrac procesa numeroFrac"
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
   numeroFrac = DUR 'era duracion
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
EndIf
' CUART   
If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    numeroFrac = DUR + 9 'era duracion
    'DUR nunca se ahce cero solo para espacio ergo si pulso
    ' la misma u otra nota sigue con la misma duracion
EndIf
' PUNTILLO   
' 3I* = I = 1 , el puntillo a un 3 suma dando la figura de la q proviene.   
If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    numeroFrac = DUR + 18 'era dur
EndIf
'DOBLE PUNTILLO   
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=0 Then 
   numeroFrac = DUR + 27
EndIf
' SEGUIR  JMG
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=0 Then 
   numeroFrac = DUR + 36
EndIf   
' -----fin 1er bloque ------------------------

If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   numeroFrac = DUR + 45 'era dur
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   numeroFrac = DUR + 54
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=0 Then 
   numeroFrac = DUR + 63
EndIf
If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=0 Then 
   numeroFrac = DUR + 72
EndIf

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=0 Then 
   numeroFrac = DUR + 81
EndIf

' -- fin 2do bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    numeroFrac = DUR + 90
EndIf

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    numeroFrac = DUR + 99
EndIf

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    numeroFrac = DUR + 108
EndIf

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=1 Then 
   numeroFrac = DUR + 117
EndIf
If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=1 Then 
   numeroFrac = DUR + 126
EndIf
'----- fin 3er bloque   
If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   numeroFrac = DUR + 135
EndIf   

If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   numeroFrac = DUR + 144
EndIf   

If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
   numeroFrac = DUR + 153
EndIf   

If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=1 Then 
   numeroFrac = DUR + 162
EndIf   

If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=1 Then 
   numeroFrac = DUR + 171
EndIf   
' ---fin 4to bloque -
   cuart=0:pun=0:doblepun=0:tres=0:sil=0:mas=0

Print #1,"armardurFrac  numeroFrac Resultante ", numeroFrac
End Sub


Sub calcCompas( ByRef j As Integer,Roll As inst)
 ' nota:fijarse que pasa con el grupo..1 to 4 de las 64 duraciones
 ' x print #1,"========================================================="
 ' x print #1,"1-calcCompas j o posn RECIBIDA: "; j
 Dim As Integer k, i,cantFalta,CantSobra, tengo, falta,sobra, noct

On Local Error GoTo errorCalc 

 If DUR=0  then ''And j >0 Then '23-03-2022 ÇÇÇ
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

' estonunca seejecuta porque j es 4 nouncaentr con 1!!
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
   If abs(d7 -acumulado) < 10 Then
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
    ' x print #1,"1-calcCompas 3.2 acumulado menor sigue exit sub, carga: ";carga
    Exit Sub
   EndIf
   ' cuando cargamos desde disco aca nunca es necesario pasar pues
   ' ya esta todo fraccionado para tener compases completos sin notas
   '  que sobren o falten
   If carga=1 Then ' se supone que nunca carga=1 se usara en el resto
    'posn=j
    Exit sub
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
     EXIT Sub
    EndIf
    For z1 = 1 To CantFalta
     ' x print #1, " ";partes_falta(z1);" ";figura(partes_falta(z1));
    Next z1
    ' x print #1,
    separarDur(j,DUR, partes_sobra(),cantSobra,sobra)
    ' x print #1,"||||| CantSobra ";CantSobra
    If CantSobra > 100 Then
     ' x print #1, "ERROR CANTSOBRA > 100 ";CantSobra
     EXIT Sub
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
   Exit for
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
   Roll.trk(posn+1,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk(posn,(notaOld-1 +(estoyEnOctava -1) * 13)).dur = 181
   EndIf
'------zzz
   For noct = desde To hasta -1 '01-02-2022
     For i= 0 To 11 ' gracias a esto anda acordes¿?
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
   posn=Posn+1
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

   posn=Posn+1
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
   If carga=0 Then
    Roll.trk(posn,(12-nota +(estoyEnOctava -1) * 13)).nota = notaold
   EndIf
   Roll.trk(posn+1,(12-nota +(estoyEnOctava -1) * 13)).dur = 182
   If notaOld > 0 And notaOld <> nota Then
    Roll.trk(posn,(notaOld-1 +(estoyEnOctava -1) * 13)).dur = 181

   EndIf
'------zzz

   For noct = desde To hasta -1 ' 01-02-2022
     For i= 0 To 11 ' gracias a esto anda acordesï¿?
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
   acumulado=acumulado + pesoDur(partes_sobra(k)) ' para el siguiente compas..¿?.
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
errorCalc:
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
er1 = Err
If er1 > 0 Then
print #1,"Error Calc detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po
Dim As Integer valor1, valor2


print #1,"*po ";*po;" semitono ";i
print #1,Erl, Erfn,Ermn,Err
print #1,"------------------------------------"
ErrorNumber1 = Err
ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
print #1,"Error Function: "; *Erfn()
ers= 11 -i  +(*po) * 13
print #1, "11 - semitono  +(*po) * 13)  "; ers; "semitono  ";i
print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf

 
 
End Sub
' ======================================================================
Sub separarDur(ByRef j As Integer, ByRef DUR As Ubyte,partes() As Integer, ByRef cantPArtes As Integer, ByRef falta As Integer)

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

 ' S print #1,"pun: ";pun;" sil: ";sil;" mas: ";mas; " tres: ";tres
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
Dim CUENTA As INTEGER 
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
      Exit do
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
      Exit do
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
      Exit do
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
      Exit do
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
      Cerrar 0
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
errorSepara:
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
er1 = Err
If er1 > 0 Then
print #1,"Error Separa detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po-1 ";*po-1;" *po+1 ";*po+1
Dim As Integer valor1, valor2


print #1,"*po ";*po 
print #1,Erl, Erfn,Ermn,Err
print #1,"------------------------------------"
ErrorNumber1 = Err
ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
print #1,"Error Function: "; *Erfn()
'ers= 11 -semitono +(*po) * 13


print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf


End Sub

Sub mayorDurEnUnaPosicion (ByRef posn as integer)
 print #1,"1-MayorDurEnUnaPosicion  "
 Dim As Integer i, j,mayor,ia ,valdur
 mayor=1
 j = posn
 For i= NB To NA -13
  valdur => Roll.trk(j,i).dur
  If  valdur >= 1 And valdur <=181 Then
   'print #1, "DUR mayor";DUR, mayor
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
    calcCompas(j, Roll)
'    print #1,"ABRIR compas(j) ";compas(j).Posi; " j :"; j

   EndIf
  EndIf
 Next i
 ''  DUR=0

 ' fin compases

End Sub

'-----------
Sub ReCalCompas(Roll As inst)
On Local Error GoTo errorRecal
'If MaxPos=0 Then ' 23-03-2022 ÇÇÇ
'   Exit Sub
'EndIf
   nroCompas = 0

     carga=1 ' no es lo mismo calcCompas con cargar o procesando
     Dim As Integer i,j , mayor,ia,valdur,cont, semi,i2
     cont=0
     'print #1,"ABRIR MAXPOS ,NB,NA ";MAxPos, NB,NA
     Dim As Integer tope1, tope2, grupo, mayorgrupo, noct
     For j = 1 To MaxPos -2
      'Print #2,"POSICION :";J
      'Print #3,"POSICION :";J
      cont=0
      For i= NB To NA -13 ' 01-02-2022  
      If Roll.trk(j,i).pb = 202 Or Roll.trk(j,i).pb=201 Or Roll.trk(j,i).dur=200  Then
         Continue for
      EndIf
       valdur => Roll.trk(j,i).dur ' de 1 a 180
       ' al cargr tenemos 4 grupos de 16 de duraciones identicas que se repiten
       ' 1-45,46-90,91-135,136-180 , el indicemeindicaa cual grupo pertenece, y
       'en que rango buscar
  '     mayorgrupo=0
''DE LO QUE SIGUE DEBO EXTRCTAR MAYOR ENUNA POSCION Y COPIARLO
'' A ESA SUBRUTINA Y PONERLA EN NUCLEO
       semi => Roll.trk(j,i).nota  
      If valdur > 0 And valdur < 181 And semi >=1 And semi <=12 Then
       ' print #1, "LECTURA DUR: ";valdur;" EN LA CARGA j: ";j;" i: "; i;" figura ";figura(valdur)
       ' Print #3, "nota: "; i; " posi: "; j; " dur: ";valdur
       ' las duraciones e repiten cada 45 de modo que no importan los incr
        Select Case valdur
         Case  1   To 45
          tope1=1:tope2=45:grupo=1:incr=0
         Case  46  To  90
          tope1=46:tope2=90:grupo=2:incr=45
         Case  91  To  135
          tope1=91:tope2=135:grupo=3:incr=90
         Case  136  To  180
          tope1=136:tope2=180:grupo=4:incr=135
        ' POR ESTO NOANDABA SILENCIOS LO LIMITE ANTES Continue For,For V19 incluimos silencios en recaocompas!!!
        End Select

        If  valdur >= tope1 And valdur <=tope2 And tope1 > 0 And tope2> 0 Then
        ' print #1, "Valdur i ";valdur ; i ; " grupo ";grupo;" figura ";figura(valdur)
         ' print #1,"i NB ";i,NB
         If  cont=0   Then 'mira duraciones
          mayor=pesoDur(valdur):mayorgrupo=grupo
          '       print #1,"1er mayor "; mayor; " mayorgrupo";mayorgrupo
          cont=1
         EndIf
         'print #1, "DUR mayor";DUR, mayor
         If pesoDUR(valdur) > mayor  Then ' las duraciones cuando mas
          mayor=pesoDur(valdur):mayorgrupo=grupo
          '        print #1,"Siguientes mayor "; mayor
          ' print #1, " DUR MAYOR";DUR,mayor
         EndIf
         '     print #1,"> i= ";i; " NA= ";NA
        EndIf
      EndIf 'jmg OJO OJO
      Dim As Integer iades =(mayorgrupo -1)*45 + 1
      If mayorgrupo=0 Then '05-07-2021
        iades=0
      EndIf
      Dim As Integer iahas = mayorgrupo * 45 
      
       If i=NA -13 Then ' 01-02-2022 vuelta atras

        'print #1,"i=NA i: ";i;" NA: ";NA;" Mayor ";mayor;" grupo:"; mayorgrupo
        For ia= iades To iahas ' tresillos es grupo 
         If mayor=pesoDur(ia) Then
          valdur=ia
        '     print #1, "peso de mayor encontrado valdur ";valdur; "dur ";ia; " grupo ";mayorgrupo
          Exit For   
         EndIf
        Next ia
        DUR=valdur
      '  If DUR >180  Then ' <- esto nunca se ejecuta creo se filtra antes...
      '    print #1,"ERROR NO SE PUEDE PASAR DUR"
      '    compas(j).Posi=0
      '    Compas(j).nro = 0
'
 '       Else
        Dim durm As Integer
         If DUR >181 or DUR=0 Then
            durm=181
         Else
            durm=DUR   
         EndIf
         
         if DUR<=180 Then
         ' print #1,"ABRIR IN:  J: "; J; " DUR: "; valdur;" GRUPO: "; mayorgrupo;" figura ";figura(durm)
          calcCompas(j,Roll)
         If carga=1 Then
 '------zzz
            For noct = desde To hasta
              For i2= 0 To 11 ' gracias a esto anda acordesï¿?
                 If Roll.trk(j,(i2 +(noct -1) * 13)).nota = 0 Then
                    Roll.trk(j,(i2 +(noct -1) * 13)).nota = 181
                    Roll.trk(j,(i2 +(noct -1) * 13)).dur  = 0
                 EndIf
              Next i2
            Next noct
'-------   
         EndIf   
          
          'print #1,"ABRIR OUT: J: "; J; " compas(j): " ;compas(j).Posi;" figura ";figura(durm)
          valdur=0
          DUR=0
          mayor=0
         EndIf
        'Print #3,"==============fin posicion nro: "; j
       EndIf
       valdur=0:DUR=0
    
      Next i
      
     Next j
     DUR => 0
    '' curpos =>0
     carga=0  ' <======= control de Carga
'' error local
errorRecal:
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
er1 = Err
If er1 > 0 Then
print #1,"Error Recal detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po-1 ";*po-1;" *po+1 ";*po+1
Dim As Integer valor1, valor2
valor2= i2 +(noct -1) * 13

print #1,"*po ";*po;" semitono ";i2; " i2 ";valor1 
print #1,Erl, Erfn,Ermn,Err
print #1,"------------------------------------"
ErrorNumber1 = Err
ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
print #1,"Error Function: "; *Erfn()
ers= i2 +(noct -1) * 13
print #1, "(i2 +(noct -1) * 13) "; ers; " semitono "; i2
print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf


End Sub
' ------
Sub menoryMayorEnColumna (Roll As inst, ejex As integer, Byref menor As ubyte, ByRef mayor As UByte,ByRef i1men As Integer, ByRef i1may As integer)
' estoy en Roll el cual es ùnico tengo entonces definido las globales NB y NA por default
' recorro la columna
Dim As Integer i1,j1=0, DurMen,DurMay

  For i1 = NB To NA -13 ' 01-02-2022
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

Print #1,"encontra menor y mayor ", menor, mayor, "En columna ",ejex
Dim As Integer notapMay, notapMen
notapMay=i1may -restar(i1may)
notapMen=i1men -restar(i1men)
Print #1,"Notapiano menor,mayor ", notapMen, notapMay
i1may=notapMay
i1men=notapMen 
' ejex es fijo es al columna en analis y origen del  fraccionamiento
  
End Sub

''========================= C U R S O R ============
Sub cursor(c As cairo_t Ptr,ByRef n As Integer, ByRef nro As Integer, Roll As inst)
 'print #1,"1-cursor "
 cairo_set_source_rgba c, 1, 1, 0.2, 1
 Dim Wc As Integer
On Local Error GoTo errorhand 

'Dim sobra As Integer
'     sobra=MAXPOS - n -curpos
'  if sobra > 0 Then
'     curpos =curpos -sobra
'  EndIf
    
 'If play=1 Or playb=1 Then
     wc=3*anchofig/4 '31-01-2022 23-02 tapaba la figura
 'Else
 '    wc=inc_penta
 'EndIf 
 'cairo_set_line_width(c, 1)
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
 
 If cursorVert=1 Or cursorVert= 2 Then  ' vertical COMEDIT=TRUE

  cairo_move_to  (c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  )
  cairo_rectangle(c, gap1 + (curpos)*anchofig, Penta_y + (notacur-1) * inc_Penta  ,wc ,inc_Penta) '13-07-2021 inc_penta/2 por inc_penta
  cairo_move_to  (c, gap1 + (curpos)*anchofig , Penta_y  )
  cairo_line_to  (c, gap1 + (curpos)*anchofig , Penta_y + (notacur-1) * inc_Penta  +inc_Penta )
 
  RollDur=Roll.trk(n + curpos,12 - notacur + (*po-1) * 13 ).dur

  RollNota=Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota
 EndIf
 If cursorVert= 0 And cursorHori=0 Then
  If notacur=1 Then
   'print #1,"notaold en cursor ";notaold
   notaOld=notacur
   'print #1,"notaold en cursor laigualo a 1 ";notaold
   'notacur=0
  EndIf
  cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
  cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)

 EndIf
 If cursorHori=1 Or cursorHori = 2 Then
  cairo_move_to  (c,gap1 + (curpos)*anchofig ,  Penta_y + (notaOld-1) * inc_Penta  )
  cairo_rectangle(c,gap1 + (curpos)*anchofig, Penta_y + (notaOld-1) * inc_Penta  , wc,inc_Penta)

 EndIf
 If (cursorHori=1 Or cursorVert = 1) And borrar= 1 Then
' HABILITAR NOTA AL PONER EN CERO SE PUEDE USAR EN EDIT COMUN
''' If  borrar= 1 Then ' 
  cairo_set_source_rgba c, 1, 0, 0.2, 1
  Dim dursaved As Integer
  If notacur=1 Then ' 1 POR 0 JMG
   notacur=notaOld 
  EndIf
  
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13 ).dur = 0
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13 ).nota = 0

  borrar = 0
  ''calcCompas(n) ''mayorDurEnUnaPosicion (n)
 EndIf
 ' ==========================================================================
 ' << CAMBIADUR TAMBIEN INGRESA NOTA NUEVA Y DURACION DONDE NO HABIA >>
 ' << SOBREESCRIBE >> aca si usarAcordesIguales=1 debo hacer el FraccionaDur automatico
 ' si tengo notas en acorde preexistentes de menor, igual  o mayor valor de duracion.
 ' tambien deberé Fraccionar ---TAREA FUTURA---- FUTURO
 ' EN UNA COLUMNA BUSCO LA DURACION MENOR Y LA MAYOR COMPARO CON LA DE ENTRADA NUEVA.
 ' TOMO LA MENOR DE TODAS INCLUSO LA ENTRADA Y FRACCIONO TODAS LAS NOTAS DE LA COLUMNA
 ' ES TANTAS PARTES TENGA CADA FIGURA DE ESA MENOR. PARA ELLO TOMARE LA MAYOR Y AL PARTIRE
 ' EN LAS PARTES CORRESPONDIENTES DE LA MENOR, LUEGO DE ESO SIGO FRACCIONANDO LAS OTRAS
 ' QU ESON MAYORES A LA MENOR PERO MENORES A LA MAYOR ,,,,
  '=============================================================================
 If (cursorHori=1 Or cursorVert = 1) And cambiadur= 1 Then
  ' debo cambiar esa nota por su equivalente silencio
  '1ero se pulsa la nueva duracion y luego la X porque aca se anula luego elflag cambiadur
  'y la dUR debe tener un valor antes de esto
  'If notacur=1 Then  ' 1 PO 0 JMG <- aca ingreso nota aunqne no haya?
  ' notacur=notaOld   ' ESTO DABA EL LIO DE LA NOTA B 
  'EndIf
  '  Roll.trk(notacur + (*po) * 13, n + curpos).dur = DUR
  '  vdur=Roll.trk(notacur + (*po) * 13, n +curpos).dur
  '  ' SI LA NOTA  NO EXITIA LA agrega
  '  Roll.trk(notacur + (*po) * 13, n + curpos).nota = notacur  ' 5.7.2
  '  vroll= Roll.trk(notacur + (*po) * 13, n + curpos).nota
  '  cambiadur = 0
  ' ------------------------------------
  ' nueva verion con puntillo o silencio
  ' ------------------------------------
  'si en una columna todas las notas son 0,181 y sihay un 190 en dur
  ' y nota esa columna se elimina,,,,,implementar 12-05-2021 implementado se uso 190
  ' la forma facil de borrar seria grabar a disco cuando se está
  ' en posicion con 190 saltar esa posicion y restar 1 a la posicion final.
  ' se ahce ya de ambasformas usando delete tambiense borra online
  
   
 If DUR=181 Then
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = 0
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota =181   ' 5.7.2
     cambiadur = 0
     DUR=0
 ElseIf DUR=182 Then ' borrado 
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = 182
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota =181   ' 5.7.2
  '   print #1,"FINDE SECUENCIA !!"
     cambiadur = 0
     DUR=0
 ElseIf DUR=0 Then ' borrado
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = 190
     Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota =190   ' 5.7.2
     cambiadur = 0
     DUR=0
 Else
'desde aca lasnotas se colocan con notacur siemrpe tienen valorde 1 a 12
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR
    incr=0
  EndIf
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 9
   incr=0
  EndIf
 
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 18
   incr=0
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 27
   incr=0
  EndIf 
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 36
   incr=0    
  EndIf
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 45
    incr=45
  EndIf
   
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 54
    incr=45
  EndIf
  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 63
    incr=45
  EndIf
  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=0 Then
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 72
    incr=45
  EndIf
   
  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=0 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 81
    incr=45
  EndIf   
 
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 90
    incr=90
  EndIf   
 
  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 99
    incr=90
  EndIf 

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 108
    incr=90
  EndIf

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 117
    incr=90
  EndIf

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=0 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 126
    incr=90
  EndIf
'----- fin 3er bloque   
  If cuart=0 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 135
   incr=135
  EndIf   

  If cuart=1 And pun = 0 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 144
   incr=135
  EndIf   

  If cuart=0 And pun = 1 And doblepun=0 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 153
   incr=135
  EndIf   

  If cuart=0 And pun = 0 And doblepun=1 And tres=0 And sil=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 162
   incr=135
  EndIf   

  If cuart=0 And pun = 0 And doblepun=0 And tres=1 And sil=1 And mas=1 Then 
    Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR + 171
   incr=135
  EndIf   
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur  ' 5.7.2    
 EndIf


' EN UNA COLUMNA BUSCO LA DURACION MENOR Y LA MAYOR incluido LA DE ENTRADA NUEVA.
' TOMO LA MENOR DE TODAS INCLUSO LA ENTRADA Y FRACCIONO TODAS LAS NOTAS DE LA COLUMNA
' En TANTAS PARTES TENGA CADA FIGURA DE ESA MENOR. PARA ELLO TOMARE LA MAYOR Y LA PARTIRE
' EN LAS PARTES CORRESPONDIENTES DE LA MENOR, LUEGO DE ESO SIGO FRACCIONANDO LAS OTRAS
' QUE SON MAYORES A LA MENOR PERO MENORES A LA MAYOR ,,,,

'  
 '-----------------------------------------------------------
 ' ADAPTAR BIEN PARA CURSOR POR AHORA NO SE USA PONEMOS 3
If usarAcordesIguales = 3 Then ' empeiza Fraccionador..
 ' determino la duracion menor y mayor incluida la de entrada ya cargada
 ' en Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur
   Dim As UByte menor, mayor
   Dim As Integer i1men,i1may,j
   menoryMayorEnColumna (Roll, n+curpos,menor,mayor,i1men,i1may)
' saco la info para usar Fraccionador...pero ojo fraccionador solo reemplaza a notas
' de igual duracion no de distintas, debere hacer un nuevo fraccionador ? o generalizo
' el existente y lo parametrizo   
   Print #1,"En Cursor: menor,mayor,i1men,i1may ",menor,mayor,i1men,i1may
' tengo menor mayor, ergo debo dividir a mayor en varias partes de menor con 
' Fraccionador, una ve zecho eso puede haber duraciones intemedias que no sean
' ni menor ni mayor, pero se deben dividir en varias menores hast aalcanzar al
' valro que tiene. ejempo tengo una negra I, una corchea L y una blanca P
' I
' L
' P
' ----> el resultado debe ser el siguiente: I=2L, P=4L,L=L
' L+  L+  sL+ sL
' L+ sL+ sL+ sL
' L+  L+   L+  L
'---------------
'ERGO 1ERO DETERMINO LAS PARTES DE P COMO HACE FRACCIONADOR DESDE LA CIFRA O FIG MENOR L
' DEBO LLENAR 3 POSICIONES ADICIONALES COMO HACE FRACCIONADOR, EN EL CASO DE QUE 
' LA DURACION SE COMPLETE ANTES EL RESTO SE LLENA CON SILENCIOS DE LA FIGURA MENOR
' durfig=CInt(cifra) de aca uso la global cifra para simular la entrada de usuario para la menor
' l aduracion menor la debo converir en 1 a 9 ...
' mm debo probar para duracione smayores a 9 
    numeroFrac = cint(menor) ' simulamos la entrada por el usuario DE LA DURACION DUR
    ' CON LA QUE FRACCIONAREMOS
    print #1,"Cursor: numeroFrac ", numeroFrac


  ''FraccionarDur Track(),Roll,n + curpos,mayor,nR,ntk
  FracTodoDur Track(),Roll,indicePos, mayor,nR,ntk
  
' RollaTrack
  RollaTrack Track(), 0,Roll 
  
 
EndIf
' ---------luego de cambiado el valor de una nota  o ingresado una nueva
' donde no habia nada debo verificar si estoy en un posible acorde de iguales
'  if   esAcordeIguales(devolver intervalo horizontal deSde hasta) then
   ' un cilco for  desde hasta posicion de modificacio
   ' si no hay nota poner silencios ligados menos el que esta pegado a la
   ' nota nueva...
   ' si hay msa espacio luego de la nota modificada si son espacios
   ' colocar silencioas, lo problematico es que hay que hacerlo a nivel 
   ' pista y a nivel cancion ÇÇÇÇÇ PENDIENTE JMG AUTOMATIZAR
   ' EL LLANDAO DE ACORDES IGUALES CON SILENCIOS --22 02 2022

'  endif 

 '-----------------------------------------------------------
 '   vroll= Roll.trk(notacur + (*po) * 13, n + curpos).nota
  cambiadur = 0
  cuart=0:pun=0:doblepun=0:tres=0:sil=0:mas=0

  ReCalCompas(Roll) 'organizaCompases() 
  nota = 0

  ' fin nuevaversion
 EndIf
 ' ===> PROCESO INSERT
 If (cursorHori=1 Or cursorVert = 1) And insert= 2 Then

  ' insertar  nota
  'ESTANDO EN UNA NOTA ELEGIDA POR EL CURSOR
  '1ero se pulsa la nueva duracion y luego la Tecla INSERT
  ' al final de todo la tecla I (ubicrse en lnota luego 3 teclas
  ' Dur + insert + I)
  'y la dUR debe tener un valor antes de esto
  If notacur=1 Then  ' 1 POR 0 JMG <- aca ingreso nota aunqne no haya..
   notacur=notaOld  'cual toma ? es donde esta el cursor ?
  EndIf
  ' print #1, "Entra a Cursor con insert 2 notacur: ", notacur
  ' StartInsert esglobal,posicion de inicio de la insercion cuando  pulsamos
  ' la tecla insert se guarda la posicion de inicio desde ahi habra reemplzos
  ' usando la posicion comun n
  ' hasta que se pulse FINose alcance el FIN (66)
  ' print #1, "call a movedato con indaux ",indaux
  ' n es StartInsert fijo no se modifica mas
  movedato (n + curpos,indaux, insert,notacur + (*po-1) * 13 ) 'antes de reemplazarlo xxx
 '  print #1, "regreso de movedato con indaux ",indaux
  ' param : posicion comienzo (fijo), indice incremental para el aux
  ' ,insert comando habilitado = 1
  'luego reemplazo
  ' print #1, "inserta DUR, en ROLL n,nota,posicion ",DUR,n,(notacur + (*po) * 13),n
  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = DUR ' reemplazo con lo nuevo
  'vdur=Roll.trk(notacur + (*po) * 13, n+curpos).dur
  ' SI LA NOTA  NO EXITIA LA agrega...osea sirve para acordes tambien
  'print #1, "carga notacur en ROLL n,nota calculada ", n,(notacur + (*po) * 13)

  Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur ' 5.7.3
  'vroll= Roll.trk(notacur + (*po) * 13, n + curpos).nota
  ' TODO IGUAL A cambiadur = 1 deberia andar ,,,,lunica diff es
  ' que se cargala nota en roll,<- pero 1ero debo guardar la nota de Roll en aux
  ' antes de reemplazarla
  insert=1  ' habilita de nuevo la tecla I deingreso de insercion en un lugar
  'elegido con cursor
 ' print #1, "insert 1 ",insert

 EndIf
 'proceso modificar Nuevo secunci limpia terminda en NroCol ,con
 ' nombre de letra pulsada en vez de X CTRL-N
 ' ====> MODIFICAR con ctrl-n
 If (cursorHori=2 Or cursorVert = 2) And agregarNota=1  And nota> 0 Then
 
  Roll.trk(n + curpos, 12-notacur + (*po-1) * 13).dur = DUR
  ' SI LA NOTA  NO EXITIA LA agrega
  Roll.trk(n + curpos, 12-notacur + (*po-1) * 13).nota = nota  ' 5.7.3.4
   ReCalCompas(Roll) 
   nota=0
 EndIf
 ' VEMOS QUE HACE ESTA FUNCION SI SIRVE
 ''cairo_surface_get_device_offset
 cairo_stroke(c) ' aca el stroke da el amarillo y no jode a teclas - + con saltos!

'If Roll.trk(notacur + (*po) * 13, n+curpos).dur >= 1 And _ 
'   Roll.trk(notacur + (*po) * 13, n+curpos).dur <= 181 Then
   If cursorHori=1 And vdur=0 And vnota=0 And copiar=1 Then
      vnota=notacur 
      vdur =Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur  ' 5.7.3
       copiar=2
   EndIf
'EndIf

'If Roll.trk(notacur + (*po) * 13, n+curpos).dur  < 1  And _  
'   Roll.trk(notacur + (*po) * 13, n+curpos).dur   > 180 Then
If cursorHori=1 And vdur>0 And copiar=3 Then
   If notacur<> vnota And MultiKey (SC_ENTER) Then 
      Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).nota = notacur 
      Roll.trk(n + curpos, 12 - notacur + (*po-1) * 13).dur = vdur
      copiar=0
      vdur=0:vnota=0
      ReCalCompas(Roll)
   EndIf 
EndIf
If nro=0 Then ' error al subir mucho mira despues del and el estupido 
nro=1 'jmg 16-05-2021 notcur nodeberi empezar desde 0
EndIf
If notacur=0 Then
  notacur=1
EndIf
' la causa del error era que el limite mayor del ubound es 39 
' el ubound aunque tenga muchas mas posisiones 4000 , no puede ser
' se ahbra definido el vector de 39 ¿???? muy raro
' y n + curpos supera los 39 termina en 67 
' si hago curpos=0 funciona pero lso cuadraditos no avanzan
' ergo salto esto en play y listo deberi aestar condicionado oara edicion eso es
If cursorHori=1 Then ' 17-09-2021 faltaba condicion de modificacion !! 
  If copiar=3 And (Roll.trk(n + curpos, 12 -notacur + (*po-1) * 13).dur = vdur) And notacur = vnota Then
     copiar=0:vdur=0:vnota=0
  EndIf
EndIf
' modelo para poner control de errores en subrutinas lo dejo de modelito
 errorhand:
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
er1 = Err
If er1 > 0 Then
print #1,"Error Cursor detected ", er1, " posicion ";posicion; " maxpos";MaxPos;" curpos ";curpos
print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po-1 ";*po-1;" *po+1 ";*po+1
print #1, "NB "; NB; " NA ";NA; " n ="; n
print #1, "LBound (Roll.trk,1) ",LBound (Roll.trk,1)
print #1, "LBound (Roll.trk,2) ",LBound (Roll.trk,2)
print #1, "UBound (Roll.trk,1) ",UBound (Roll.trk,1)
print #1, "UBound (Roll.trk,2) ",UBound (Roll.trk,2)
 
Dim As Integer valor1 , valor2
valor2=12 - notacur + (*po) * 13
valor1 = n + curpos
print #1,"*po ";*po;" notacur ";notacur; " indice1 ";valor1; " indice2 ";valor2
print #1,Erl, Erfn,Ermn,Err
print #1,"------------------------------------"
ErrorNumber1 = Err
ErrorLine1 = Erl
'Dim As String ProgError1(0 To 17)


print #1,"ERROR = ";ProgError(ErrorNumber1); " on line ";ErrorLine1
print #1,"Error Function: "; *Erfn()
ers= 12 -notacur +(*po) * 13
print #1, "12 - notacur +(*po) * 13) "; ers; "notacur ";notacur
print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
EndIf

 
 Exit Sub
End Sub
'----------------------------------------
Sub Tracks ( ByRef ntk As integer, ByRef nroCanal As UByte,Roll As inst)
  ''copia track a Roll en memoria
  ' nrocanal no se usa ,,,,borraremos mas adelante...
Print #1, "MaxPos <=2 Or pmTK(ntk).maxpos=0 Then " , Maxpos, pmTK(ntk).maxpos 
If MaxPos <=2 Or pmTK(ntk).maxpos=0 Then
  Print #1, "Salimos de Tracks sin ahcer nada "
  Exit Sub
EndIf
nota=0:dur=0
print #1,"EN TRACKS MAXPOS (NTK).MAXPOS ", maxpos, pmTK(ntk).maxpos
 ' sizeof array de ubytepr almcenar el pentagrama roll
 'Notas ( 1 To 65536, 1 To 128) as UByte
' usaremso para cambianr de track en Roll y Track(0)
nombre= titulos(ntk)
' evita leer track vacios   
If nombre=""  Then ' evita revisar track vacios
 Do While nombre=""
 ntk=ntk+1
 If ntk>32 Then
    ntk=1
    Exit Do
 EndIf
 Loop
  nombre= titulos(ntk)
Else   
 
TrackaRoll (Track(), ntk,Roll ) ' no usa ubirtk
print #1,"Tracks  ntk cargado, nombre ",ntk, nombre
print #1,"llama a RecalCompas para ntk,maxpos, .maxpos ",ntk,maxpos,pmTk(ntk).MaxPos
ReCalCompas(Roll) 
print #1,"MAXPOS DESPUES DE RECALCOMPAS ", maxpos, pmTK(ntk).maxpos
EndIf


End Sub
'--------
Sub CargaArchivo(Roll As inst, ByRef ubiroll As integer)
' PRINCIPALMENTE CARGA ROLL Y LO PASA SIEMPRE A TRACK (0)
ntk=0 ' Cargar en 2 vectores Roll y carga a Track (0)
' la carga de tracks aislada o la carga de una cancion con sus tracks
' ira aumentando ntk 
   '  carga roll
' al cargar borro todo el undo    
abrirRoll=2 ' habilita la carga de una cancion
   mel_undo_k=0 :ig=0:cnt_acor=0
   Erase mel_undo, undo_acorde,undo_kant_intervalos
' FIN BORRO UNDO
   
   
'   Print #1,"ubiroll en CA ",ubiroll
     
     Dim As String x,x1,x2,x3,x4,x5,nombrea
     ''' no usar mas  dialogoText("Cargar Archivo")
     Dim myFil As String
      If ubiroll=0 Then
         myfil  = "Roll Files"+Chr(0)  +"*.roll"+Chr(0)
         getfiles(file,myfil,"open")
         nombrea=*file.lpstrFile
      Else
         If ubiroll > 0 Then
'           *file.lpstrFile=titulos(0)
'            fclose(file.lpstrFile)
            nombrea=Trim(titulos(0))
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
   print #1,"CArgarArchivo ",nombre    

   ca=FreeFile   
 ' print #1,"open nombre en CargaArchivo ",nombre
      If  ubiroll =0 Then

         Open nombre  For Binary Access Read As #ca
      Else
         Sleep 100

         Print #1,"lee con control de error.. "
     If Open (nombre  For Binary Access Read Shared As #ca ) <> 0 Then
        print #1,"Error open cargaARchivo 1419, nombre ",nombre
        Exit Sub
     End If
         
     ''    Open "temp.roll"  For Binary Access Read As #ca
         ubiroll=0
      EndIf
     Dim z (1,1)  As dat
     Dim zLim (1,1) As dat
     Dim z3   (1,1)  As dat ' 04-02-2022 se agregan 48 bytes para info futura 
     Dim z4   (1,1)  As dat 
     Dim z5   (1,1)  As dat 
     Dim z6   (1,1)  As dat 
     Dim z7   (1,1)  As dat 
     Dim z8   (1,1)  As dat 
     Dim z9   (1,1)  As dat 
     Dim z10  (1,1)  As dat 
     
     

     'Print #1,"posicion lectura en ca ", Seek(ca)  
'     Seek ca,1
     Get #ca, , z(1,1)
     x1=Bin(z(1,1).nota,4) 
     x2=Bin(z(1,1).dur,4) 
     x3=Bin(z(1,1).vol,4) 
     x4=Bin(z(1,1).pan,4) 
     x5=Bin(z(1,1).pb,4) 
     tipoescala_num_ini =CInt( Z(1,1).inst) ' 20-12-2021 - tipoescala en uso
     If tipoescala_num_ini =0 Then
        tipoescala_num_ini =1
     EndIf
     Print #1,"tipoescala_num_ini ",tipoescala_num_ini ' 
     
     x=x1+x2+x3+x4+x5
        print #1,"reconstruccion x pos bin ", x
     MaxPos=CInt("&B"+x)
     posicion = 1
       print #1,"reconstruccion x pos int ", MaxPos
     
     
     nota=0 '''notaOld
     inicioDeLectura=0' Int(Maxpos/NroCol)
     posn=Maxpos- 2
     
     
     
     If posn < 0 Then posn=0 EndIf
     ' crgamos limites Roll de octavas
     Get #ca, , zLim(1,1)
     desde  = CInt(zLim(1,1).nota)
     Print #1,"desde ",desde
     hasta  = CInt(zLim(1,1).dur)
     Print #1,"hasta ",hasta
     notaold= CInt(zLim(1,1).pb)
     Print #1,"notaold ",notaold     
     notaescala_num_ini =CInt(zLim(1,1).vol) ' notadeescala 20-12-2021
     If notaescala_num_ini = 0 Then
        notaescala_num_ini =1
     EndIf
     Print #1,"notaescala_num_ini ",notaescala_num_ini     
     if  zLim(1,1).pan = 3 Then
         alteracion="sos"
     EndIf    
     if  zLim(1,1).pan = 2 Then
         alteracion="bem"
     EndIf 
     if  zLim(1,1).pan = 3 Then
         alteracion="sos"
     EndIf
     Print #1,"alteracion ",alteracion
      
    cadenaes_inicial=""
   Print #1,"armarescala desde cargarArchivo"

    armarescala (cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini, alteracion ,1)

    tipoescala_inicial=escala(tipoescala_num_ini).nombre ' 13-01-2022 faltaba ini
' carga de escala en guia de escala
    guiaEscala(1).tipoescala=tipoescala_num_ini '13-01-2022 faltaba ini
    guiaEscala(1).notaescala=notaescala_num_ini
    guiaEscala(1).alteracion =CInt(zLim(1,1).pan)
    guiaEscala(1).posicion=1

    tiempoPatron=CInt(zLim(1,1).inst)
     If tiempoPatron = 0 Then 
        tiempoPatron = 60
     EndIf

     NB= 0 + (desde-1) * 13
     NA= 11 + (hasta -1)*13
     pmTk(0).notaold = notaold  'notaold es ubyte en pmTk
     pmTk(0).MaxPos = MaxPos
     pmTk(0).desde = desde
     pmTk(0).hasta = hasta
     pmTk(0).posn = MaxPos -2

     'NB => 0 + (desde-1) * 13   ' 27 para 3
     'NA => 11 + (hasta-1) * 13  ' 90 para  7
     ' los 2 vectores deben tener el mismo tamaño ergo si CantTicks
     ' lo agrando trabajo tambien ? mmm no trabajo debe tener el tamaño del disco!
     ' cargamos trabajo datos
     CantTicks = MaxPos  +1000' jmg 24-09-2021
     '''ReDim Roll.TRK(1 To cantTicks,NB To NA)
     pmTk(0).Ticks=CantTicks
     ntk=0
     CambiarDim(0) ' ya tiene calculo NB y NA Redim Roll y Track con CantTicks
                   ' y carga de pmTk.NB y NA

     Get #ca, ,z3  (1,1)

     canalx  = CInt(z3(1,1).inst) ' canal midi cargado en Global si es un track se pasara a info de trck
     Print #1,"canalx cargado desde archivo ",canalx
     portout = CInt(z3(1,1).dur) 
     Print #1,"portout cargado desde archivo ",portout
     pmTk(0).portout=z3(1,1).dur   ' ubyte  a ubyte
     pmTk(0).canalsalida = z3(1,1).inst ' ubyte a ubyte
     
     
     Get #ca, ,z4  (1,1)
     Get #ca, ,z5  (1,1)
     Get #ca, ,z6  (1,1)
     Get #ca, ,z7  (1,1)
     Get #ca, ,z8  (1,1)
     Get #ca, ,z9  (1,1)
     Get #ca, ,z10 (1,1)




     pmTk(0).NB=NB  ' aunque es un track debe memorizar de que octavas Rol vino
     pmTk(0).NA=NA
     pmTk(0).portout=portout
     
     
     ReDim compas(1 To CantTicks)
     print #1,"En CargarArchivo NB NA ", NB,NA
     Dim Trabajo (1 To MaxPos, NB To NA) As dat     
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
      Dim As integer ip=0,Notapiano
      For i= NB To NA 
       Roll.TRK(j,i)  => Trabajo (j,i)    
' 06-09-2021 carga track en ntk=0 
      
      
       If Trabajo (j,i).nota >= 1 And Trabajo (j,i).nota <=12 And  Trabajo (j,i).dur >=1 and Trabajo (j,i).dur <= 180  Then
         ip+=1
         If ip >12 Then
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
         Track(ntk).trk(j,ip).nnn => Trabajo (j,NA).inst
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
  
cerrar ca
DUR => 0
curpos =>1
''' carga=0
 ReCalCompas(Roll)  
 
' 05-03-2024 estos toques virtuales los sacare por ahora  
           'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0  
          'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0
 'print #1, "CargaARchivo LBound (Roll.trk,1) ",LBound (Roll.trk,1)
 'print #1, "CargaARchivo LBound (Roll.trk,2) ",LBound (Roll.trk,2)
    
 'print #1, "CargaARchivo UBound (Roll.trk,1) ",UBound (Roll.trk,1)
 'print #1, "CargaARchivo UBound (Roll.trk,2) ",UBound (Roll.trk,2)
  Sleep 10
End Sub
' ------------------------------------------

'
Sub Nuevo(Roll As inst,borraNombre As integer)
CantTicks=1000 ' vuelvo al original default
' borro todo el undo
   mel_undo_k=0 :ig=0:cnt_acor=0
   Erase mel_undo, undo_acorde,undo_kant_intervalos
' FIN BORRO UNDO
nota=0
   dur=0
' se inicia el vector desde 1 hasta 9, se puede achicar las octavas ok.
' si ya se habian achicado y quiero agrandar, no se puede, debo borrar el vector.
' En la cantidad de posiciones no hay porblema en ambas direcciones. 
          NB => 0 + (desde-1) * 13   ' 27 para 3
          NA => 11 + (hasta-1) * 13  ' 90 para  7
       '   If desdevector > desde Or hastavector < hasta Then ' si se amplia el rango
               print #1,"entro a redimiensionar vector por octavas"
               Erase  Roll.trk, Track(ntk).trk, RollAux.trk, mel_undo,undo_acorde,undo_kant_intervalos
                ig=0:cnt_acor=0  
               desdevector= desde
               hastavector= hasta
               print #1,"Runtime desde hasta ",desde ,hasta
               print #1,"Runtime NB NA ",NB ,NA
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
          tres=0:pun=0:sil=0:mas=0:doblepun=0:cuart=0
          tres=0:vdur=0:vnota=0:trasponer=0:pasoZona1=0:pasoZona2=0:pasoNota=0
          SelGrupoNota=0:moverZona=0:copiarZona=0:cifra="":digito="":numero=0:copi=0
          
          If borraNombre > 0  And NombreCancion = "" Then 
             nombre = ""
             Dim i As Integer
             For i=1 To 32
               titulos(i)= "" ' borro toda la lista si habia de cancion anterior
               pistas(i)="" 
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
          print #1,"posn,CantTicks ",posn,CantTicks
          print #1,"NB, NA ",NB,NA
          
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
'/

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
 
 Dim As Integer fontOld = font
 font=18 ' ACA AHCE FALTA ESTE TAMAÑO TODO ESTA CALCULADO EN PIXELS PARA QUE ANDE EL MENU
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
 cairo_set_font_size (c, font)

 '---encendido de edit
 Var t = ""
 cairo_font_extents (c, @fe)
 cairo_text_extents (c, t, @te)

 cairo_move_to(c, 0, 30 ) ' aca esta la cosa la ubicacion del font!!!
 ' debo ubicarlo caracter por caracter o posicion no en string!!!

 Dim oclog As Integer
 oclog = 8 - (estoyEnOctava-1) ' jmg ojo hay un hardcodeo ¿?
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
 ' para el caso 0, siempre queda en 0 , debí usar menunew una seguna variable
 ' y asi funciona ..(en ningun lado se ajusta a cero salvo al inicio fuera del
 ' loop.....
 t=" "
   Dim muestra As String ' nombre tambien se usa en cancion
   If NombreCancion > "" And titulos(ntk) > "" And ROLLCARGADO=FALSE Then
      nombre = titulos(ntk)
     ' Print #1,"en menu nombre ",nombre
   EndIf
   Dim ps As Integer
   If NombreCancion > "" Then
      ps=InStr(nombre,"[")
      muestra=mid (nombre,ps) + " "
   Else
     ps=InStrRev (nombre,"\") 
     muestra =Mid(nombre,ps) +" "
     camino= mid (nombre,1,ps-1) + " "
     SetWindowText(hwndC, "RollMusic Path archivo: " + camino)
   EndIf
   
   muestra=Str(PianoNota) + " "+ muestra   + "ANCHO "+ Str(ANCHO)+ " Mx:" + Str(mousex) + " My:" + Str(mousey) 'xxxx Str (BordeSupRoll)
   

 Select Case menuNro
  Case 0
  ' 0.1 ==> [Archivo]
   If mousex > 46 And mousex< 111 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
     font=18 ' el menu esta diseñado para este tamaño 
    If MouseButtons And 1 Then
     menuNew = 3  ''
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
     menuNew=2
     cierroedit= 0 ' habilita EDIT de nuevo
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   ' print #1, "INVESTIGO COMEDIT ENTRO X CLICK EN SUB S3, menuNro: ",S3, menuNro
   t=  " [Edicion]":cairo_show_text(c, t)

' 0.3 ===> [Ver] pondremos aca ver una posicion dada de la secuecnia
' entrando el numero.
   If mousex > 232 And mousex< 258 And mousey < 50 Then
     cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menuOldStr="[Ver]"
         print #1,"voy a menu 8 desde ver"
         menuNew=8
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
   If MultiKey(sc_Control) AND (mousex > 355 And mousex< 451 And mousey < 50 And play=0 and playb=0 And Cplay=0   ) Then
    cairo_set_source_rgba c, 0, 1, 0, 1
     While InKey<>"" :Wend
   '   print #1,"--->entra a reproducir thread1 "
              play=1
        menuNew =0
         print #1,"hizo click para llamar a playall, maxpos ",maxpos 
' 28-07-2021 probamos Mutex con ThreadCall , anduvo bien lo dejo veremos        
        If  MaxPos > 2 Then
               GrabarPenta=0:naco=0:naco2=0
               If instancia=7 Or instancia= 107 Then
               Else
               SetGadgetstate(15,0) ' 10-04-2022
               EndIf  
         print #1,"llama a playall"
         thread1 = ThreadCall  playAll(Roll)
         ''''playAll(Roll)
         
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
        Sleep 100
        menuNew=5
        
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [Opciones]":cairo_show_text(c, t)

' 0.8 ===> [Ayuda]
   If MultiKey(sc_Control) And mousex > 590 And mousex< 638 And mousey < 50  Then
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
      Shell ("start notepad ayuda.txt")
    End If
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   

   t=  " [Ayuda] "+ muestra :cairo_show_text(c, t)



   cairo_stroke(c)
  Case 1
  '1.1
   
   t=  "      <MENU=(<=|=>)>/<VENTANA=(MOVER=DRAGAR CINTA)(POSICON NORMAL,SUBE/BAJA BORDE INFERIOR=F7-F8)> "
   
  Case 2 ' para posicionar  en pantalla NO BORRAR. <= EDIT MENU
 
   menuNew=2
   
   cairo_move_to(c, 0, 36 )
   If COMEDIT= TRUE   Then
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
If indicePos >gap1 And nR <=(NA -13) And indicePos < MaxPos Then
   veonota=Roll.trk(indicePos, nR).nota ' NA-nr cambiado a nR jmg 
EndIf
If copiar >0 Then
   veocopi=copiar
EndIf   
If copi >0 Then
   veocopi=copi
EndIf   

   t = "      EDIT" + " " +  " Maxpos:" + _
   Str(Maxpos) +" Posicion:" + Str(posicion)+ " Posn:"+Str(posn)+ _
   " Font:" +Str(fontOld) + " EnOct:" + Str(estoyEnOctava) + " nA:"+Str(veonota) + _ 
   " NP:"+ Str(PianoNota) + " nE:"+Str(nE) + " Curpos:" +Str(curpos) + _
   " RollDur :"+ Str(RollDur) + " Rollnota:" + Str(RollNota) + _ 
   " Copiar:" + Str(veocopi) +" Col:" +Str(indicePos) + " DUR:"+ Str(Dur) + _ 
   " Nota:"+ Str(nota) + " Mx:" + Str(mousex) + " My:" + Str(mousey) 
  ' + "Desde:" + Str(y1) + "Hasta:" +Str(y2)
   /' " InicioDELectura: " + Str(InicioDelectura) + " BordeSupRoll: " + Str(BordeSupRoll) +
'/
   '''   " modifmouse:" + Str(modifmouse) + " usamousey:" + Str(usamousey) + " usamousex:"+ Str(usamousex) + _

   cairo_show_text(c,t)

   cairo_stroke(c)

   menuNew=2
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
 
  If mousex > 500 And mousey < 50 And mousex < ANCHO-50 Then
     If MouseButtons And 1 Then
      MenuNew=0
     EndIf
  EndIf

  

  Case 3 ' ==========================> caso 3
   '   t = " [NUEVO] [ABRIR] [GRABAR] [GRABAR COMO] [EXPORTAR] [CERRAR] [SALIR] "

   '===> 3.1 [NUEVO]
   If mousex > 42 And mousex < 110  And mousey < 50  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
    
        If MultiKey(SC_ALT) And MousePress=1 Then
         'If dobleclick Then
            Nuevo(Roll,1)
            Sleep 50
            MenuNew=0
         'EndIf
         
      EndIf 
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t = "      [NUEVO]":cairo_show_text(c,t)

   '===> 3.2 <ABRIR>[*.ROLL] abre roll y automaticamente lo pone
   ' en trak 0 ntk=0

   If (mousex > 215 And mousex < 269  And mousey < 50 And carga=0 ) Then ' <= ABRIR lee grabaroll.roll
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And carga = 0  Then
      If play=1 Or playb=1 Then
       CONTROL1=1 ' DETIENE EL PLAY VEREMOS
       playloop=0:playloop2=0
       Sleep 2
      EndIf 
       CargaArchivo(Roll, 0)
       s5=0 '11-06-2022
       cargaCancion=0
    '   print #1,"nombre cargado en Roll ",nombre
       MenuNew=0
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
         If play=1 Or playb=1 Then
            CONTROL1=1 ' DETIENE EL PLAY VEREMOS
            playloop=0:playloop2=0
            Sleep 2
         EndIf 

         'print #1,"carga track veo nombre antes   ",nombre
         CargarTrack (Track() , ntk, 0)
         s5=0 ' 11-06-2022
         If nombre > "" Then  ' evita crash al cancelar el dialogo 
         '   print #1,"carga track veo nombre despues ",nombre
            ROLLCARGADO=TRUE ' NO PERMITOQUE TRACK(0) SE BORRE
            TrackaRoll (Track() , ntk , Roll)
          '  print #1,"TrackaRollcarga rtk veo nombre ",nombre 
            RecalCompas (Roll)
          '  print #1,"despues RecalCompas veo nombre ",nombre
            MenuNew=0
            carga=1 ' <======= control de Carga 
            TRACKCARGADO=TRUE
            ROLLCARGADO=FALSE
            cargaCancion=0
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
   If mousex > 378 And mousex <  459  And mousey < 50 And carga=0  Then ' <= ABRIR lee grabaroll.roll
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 And carga = 0 Then
      ' CargaTema(...) o cancion ! lo tenia...
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
    carga=0  ' <======= control de Carga
   EndIf
   t = " [SIN USO]>":cairo_show_text(c,t)

   
  '===> 3.3[GRABAR] SC_F11 COPIA
' graba el roll a roll y el track a track..
   If (MultiKey(sc_Control) And mousex > 504 And mousex < 573  And mousey < 50) And carga=0 Then ' <=
    cairo_set_source_rgba c, 1, 0.5, 0, 1
     If MouseButtons And 1    Then
      cairo_set_source_rgba c,1,0,0,1
       Dim As String nombreg
       print #1,"EN Grabar nombre ",nombre
       If nombre = "" Then
          getfiles(file,myfilter,"save")
          nombreg=*file.lpstrFile
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
       If ubi1 >0 And ubi2 > 0 Or ubi3 > 0 Then 'graba un track como track
       ' pero como las modificaciones estan en ROLL debo usar GrabarrollaTrack
  Print #1,"2190 menu pierde ntk ??? ",ntk
          GrabarRollaTrack(0) 'ÇÇÇÇÑÑÑÑ
           Sleep 1000           
       Else ' graba roll como roll, si se desea eso solo sacar [xx] del nombre
           GrabarArchivo (0)
           Sleep 1000 
       EndIf
         carga=1
         lockip=2 ' habilita mouse 28-08-2021
        MenuNew=3  '  sera aca ? jjjj decia menunew=0
       
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = "[GRABAR]": cairo_show_text(c,t) :t= ""

  '===> 3.3[GRABAR COMO] SC_F11 COPIA 
   If MultiKey(sc_Control) And mousex > 608 And mousex < 743  And mousey < 50  And carga=0 Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
     '       print #1, "Grabando a disco Roll GRABAR "
       '''''no usar dialogoText("Grabar Archivo")
       Dim As String nombreg
       getfiles(file,myfilter,"save")
       nombreg=*file.lpstrFile
       If nombreg = "" Then
          Exit Sub
       Else
         nombre=nombreg   
       EndIf
        
       GrabarArchivo(1)
          lockip=2 ' habilita mouse 28-08-2021
       MenuNew=0
       carga=1   
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf
   t = " [GRABAR COMO]" : cairo_show_text(c,t) :t= ""

'-----------------------------
  '===> 3.3.1 [ROLL=>RTK] SC_F11 COPIA 
  
   If MultiKey(sc_Control) And mousex > 778 And mousex < 912  And mousey < 50  And carga=0 Then ' <=
    cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
       print #1, "Click Grabando a disco Roll a Track ",nombre
      '''' dialogoText("Grabar Archivo")
       Dim As String nombreg
       If nombre = "" Then
          getfiles(file,myfilter,"save")
          nombreg=*file.lpstrFile
          If nombreg = "" Then
             Exit Sub
          Else
             nombre=nombreg   
          EndIf
       EndIf
''''       grabaprueba()
       Dim As integer esroll, esrtk 
       esroll=InStr(LCase(nombre),".roll")
       esrtk =InStr(LCase(nombre),".rtk")        
'   GrabarRollaTRack aca graba con [00] adelante, pues no es comando de cancion
' la pista de cancion van de 1 a 32 la pista 00 es el track asociado a Roll sin cancion 
        If esrtk >0 Then ' update de un track con o sin cancion
           GrabarRollaTrack(0)
        endif  
        ' convierte roll a track comun le abtepone [00] y rtk al final
        If esroll > 0 Then ' con o sin cancion
           GrabarRollaTrack(1) ' el 0 no lo toma es indiferente
        EndIf
        MenuNew=0           ' por formato de TRack anteponiendo[0]
        carga=1
    EndIf
   Else
    carga=0
    cairo_set_source_rgba c,1,1,1,1
   EndIf


   t = " [ROLL=>TRK(0)]" : cairo_show_text(c,t) :t= ""


 '  If mousex > 941 And mousey < 50 Then
 '   If MouseButtons And 1 Then
 '    MenuNew=0
 '   EndIf
 '  EndIf
   

  Case 4  ' <======= COMO REPRODUCUIR
   cairo_set_source_rgba c,1,1,1,1
   t=  "      [Desde Inicio][Desde Esta Posicion] [Volumen / + / - /] No habilitado"
'----
   If mousex > 330 And mousey < 50 And mousex < ANCHO-50 Then
    If MouseButtons And 1 Then
       MenuNew=0
    EndIf
   EndIf
   

  Case 5 ' OPCIONES MENU, [COMPAS] [OCTAVAS] [TEMPO] [CONFIGURACION]

  If mousex > 38 And mousex < 120 And mousey < 50  Then  'COMPAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew= 7
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [COMPAS]":cairo_show_text(c, t)
  If mousex > 130 And mousex < 235 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=6
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [OCTAVAS]":cairo_show_text(c, t)
  If mousex > 260 And mousex < 329 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuOldStr="[TEMPO]"
     
     menuNew=8
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [TEMPO]":cairo_show_text(c, t)
 '
  If MultiKey(sc_Control) And mousex > 354 And mousex < 514 And mousey < 50  Then  'OCTAVAS
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     menuNew=9
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf

   t=  " [CONFIGURACION]": cairo_show_text(c, t)
 cairo_stroke(c)
 

  Case 6 'OPCIONES => OCTAVAS
   
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
  print #1, "ANTES DE MENUGRAFICO"
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
' cargar archivo anda bien redimensiona,¿porque no puedo redimensionr
' aca aunque no modifique los limites?  
    If dobleclick And redimensionar=0 Then
     cairo_set_source_rgba c, 1, 0, 0, 1
     NB => 0 + (desde) * 13   ' 27 para 3
     NA => 11 + (hasta) * 13  ' 90 para  7
  '   print #1,"NB,NA,DESDE,HASTA ";NB,NA,desde,hasta
     CambiarDim(1)
     MaxPos = 2 'NroCol
     posicion=1:posn=0
     curpos=0
     notacur=1
     nota=0 
     notaOld=0
     inicioDeLectura=0

     redimensionar=1
     RTA="REDIMENSION VISUAL y FISICA OK"
   '  print #1, RTA
      MenuNew=0
    EndIf
   Else
  '  cairo_set_source_rgba c, 1, 1, 1, 1
   
   EndIf
   t = "  [REDIMENSIONAR]<-NO BORRA DATOS" + RTA
   Case 7
   If mousex > 38 And mousex < 90 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=10000000
     menuNew= 7
     If mAXpOS > 2 Then
       ReCalCompas(Roll)
     EndIf
     TCompas="4/4"
    MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  "       [4/4]":cairo_show_text(c, t)
  If mousex > 100 And mousex < 150 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=15000000
     menuNew=7
     TCompas="12/8"
     If mAXpOS > 2 Then
     ReCalCompas(Roll)
     End If
     MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [12/8]":cairo_show_text(c, t)
  If mousex > 160 And mousex < 210 And mousey < 50  Then  'OPCIONES
    cairo_set_source_rgba c, 0, 1, 0, 1
    If MouseButtons And 1 Then
     d7=5000000
     menuNew=7
     If mAXpOS > 2 Then
     ReCalCompas(Roll)
     EndIf
     TCompas="2/4"
     MenuNew=0
    EndIf
   Else
    cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   t=  " [2/4]":cairo_show_text(c, t)
 
 cairo_stroke(c)


  Case 8
 print #1,"entro a menu 8 desde ver"

      Select Case  menuOldStr
          Case "[Ver]" 
          nombreArchivo="0"
         
          If pubi =0 Then
           print #1,"voy a EntrarTEcla desde ver menu8"
            menunew=0
            thread3= ThreadCall EntrarTeclado()
             pubi=1
          EndIf
 '          
          Case "[TEMPO]" 
          nombreArchivo="0"
          
          print #1,">>>>>>>>TIEMPO PATRON viejo>>>>>>>>>>,PUBI ", tiempoPatron,pubi
      
          If pubi =0 Then
             menunew=0
               
             thread3= ThreadCall EntrarTeclado()
            pubi=1   
          EndIf

      End Select
    menunew=0
   

   Case 9
  ' [MIDI] 
    If mousex > 42 And mousex < 90  And mousey < 50  Then ' <= NUEVO
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
         menunew=10 
      EndIf ' mousebuttons
           
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      
    EndIf
    t = "      [MIDI]":cairo_show_text(c,t)

   If mousex > 120 And mousex < 190  And mousey < 50 Then 
     cairo_set_source_rgba c, 0, 1, 0, 1
     If MouseButtons And 1 Then
        menunew=11 ' NRO CANALES 
        
     EndIf ' mousebuttons
   
   Else
     cairo_set_source_rgba c, 1, 1, 1, 1
   EndIf
   
    t = " [CANALES]":cairo_show_text(c,t)

   cairo_stroke(c)
 Case 10
  
    If mousex > 90 And mousex < 190  And mousey < 50  Then ' <= NUEVO
       cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 And cierroport = 0 Then
        cierroport=1 
       Dim miport As Integer=1
       thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))  '' 'GtkListBox()
      EndIf ' mousebuttons
  
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      MenuNew=10
    EndIf

    t = "             [MIDI-OUT]":cairo_show_text(c,t)
    If mousex > 200 And mousex < 300  And mousey < 50  Then ' <= NUEVO
     '''        Print #1,"ENTRO POR EL MIDIIN !!!!!!!"
    cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 And cierroport = 0 Then
       cierroport=1 
       Dim miport As Integer=2
       MenuNew=10:Menunro=10
       ' create con argumentos
       thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))  '' 'GtkListBox()
      EndIf ' mousebuttons
    
    Else
      cairo_set_source_rgba c, 1, 1, 1, 1
          
    EndIf
    t = " [MIDI-IN]":cairo_show_text(c,t):t=""

  Case 11

     If mousex > 40 And mousex < 190  And mousey < 50 Then 'And listaMIDI = 0 Then ' <= NUEVO
      cairo_set_source_rgba c, 0, 1, 0, 1
      If MouseButtons And 1 Then
       Dim ti As Integer
       ti=1 ' seleccion de canales midi de entrada
       thread2 = ThreadCreate(@selcanal(), CPtr(Any Ptr, ti))
         
       'Print #1,"canal elegido luego del thread2 en menu ",canalx
      EndIf ' mousebuttons
      MenuNew=9

     Else
      cairo_set_source_rgba c, 1, 1, 1, 1
      MenuNew=9
     EndIf

     t = "      Seleccione un canal"
      
 End Select
 ' Si nodoy Stroke los show text van apendendo al final como un print;

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
 ' ayuda
 If ayuda = TRUE Then
  Var  cface => cairo_ft_font_face_create_for_ft_face( ftface, 0 )
  Dim As String  text => "HOLA SOY TU AYUDA!"

  Dim As cairo_text_extents_t extents

  If mousey < 50 Then
   cairo_set_font_face( c, cface )
   cairo_set_font_size( c, 36 )
   cairo_text_extents( c, text, @extents )
   cairo_move_to( c, mousex, mousey )
   cairo_set_source_rgba( c, 1, 1, 1, 1.0 )
   cairo_show_text( c, text )
  EndIf
 EndIf
 font=fontOld
   If mousex > 500 And mousey < 50 And mousex < ANCHO-50 Then
     If MouseButtons And 1 Then
      MenuNew=0
     EndIf
  EndIf

 'DeAllocate xo: DeAllocate yo
End Sub
'----------------------------------------------------------------------
' menuacc estaba
'--------------------------
Sub movedato (ByVal n As Integer, ByRef indaux As Integer,ByRef  insert As Integer, ByRef nota As ubyte)

 ' OJO n ya entra conla suma de curpos
 ' indaux parte de cero es el indaux para el auxiliar que se ha borrado
 ' y por ende empieza por 1 no se guarda esun auxiliar...
 '
 indaux = indaux + 1 ' indaux auxiliar SC_INSERT INICIALIZA EN 0
 ' print #1, "----EN MOVEDATO indaux, nota: ",indaux , nota
 'notas gurdadas para luego usar en proceso CORRECCION
 notins = notins + 1
 notasInsertadas (notins) = nota  ' ENTRADA POR TECLADO ANTES DE PULSAR INSERT
 'LA DURACION NO HACE FALTA PARA CONTROL,QUEDARA EN ROLL
 'muevo datos QUE SERAN remplazados al auxiliar PARA ESA POSICION
 Dim As Integer i1,i2 ' no movemos solo el indice <nota> sino todas las notas!
 ' INDAUX PARTE DE 1 Y SE Va INCREMENTANDO CON CADA PULSO SOLO DE  I...

 For i1 = NB To NA
  RollAux.trk(indaux,i1)= Roll.trk(n, i1 ) 'xxx  n ya contiene curpos
 Next i1
 ' movio la nota queestaba antes de pulsar insert
 'print #1, "EN MOVEDATO carga RollAux.trk con valores viejos de roll"
 'print #1,"RollAux.trk(nota,indaux) nota,indaux " ,nota,indaux
 'print #1,"Roll.trk(nota,n) nota,n, curpos" ,nota,n, curpos
 'print #1,"duracion cargda RollAux.trk(nota,indaux).dur: ", RollAux.trk(nota,indaux).dur
 'print #1,"nota  cargda RollAux.trk(nota,indaux).nota : ", RollAux.trk(nota,indaux).nota
 'vamos reemplazando los eventos y sobreescribiendo con los nuevos pero
 '  los viejos se van guardando para desplazarlos a la derecha.
 ' RollAux.trk tendra lo viejo, notas insertdas lo nuevo
End Sub

'--------------------------
Sub moveresto (ByVal StartInsert As Integer, ByRef indaux As Integer, ByRef insert As Integer, ByRef nota As ubyte)
 Dim As Integer indpos,lastPosRollReemplzada,s,t,k,i
 lastPosRollReemplzada=posicion + curpos
 indpos=posicion + curpos'indice posicion partimos de ultima posicion global reemplazada
 '   print #1, "EN MOVERESTO, indaux,StartInsert: ",indaux ,StartInsert
 ' ultima posicion movida a aux  es indaux (indaux POSICION DE AUX) JMG
 Dim As Integer lastIndAuxporInsercion = indaux
 'print #1, "moveresto inicio: indaux,indpos,posicion :", indaux,indpos,posicion
 'print #1, "entra Loop, indaux,StartInsert,indpos: ",indaux,StartInsert ,indpos
 Dim As Integer cantInserciones = indaux ' o notins el indaux continua en movedata las inserciones nuevas
 ' durante lascuales se movio los valores viejso a aux pero quedaron
 ' losnuevos en Roll. que no debo opisar
 '1) COPIAMOS A AUX EL RESTO DE ROLL
 Do

  'indpos ya se le sumo curpos... COPIAMOS A AUX EL RESTO DE ROLL NO MODIFICADO COMPLETA
  indpos=indpos+1 ' empezamos de la siguiente no reemplazada y seguimos al final
  ''suponemos que posicion=indpos era la posicion de ultima reemplazada
  indaux=indaux+1 'nosubicamos en 1er posicon libre en aux para recibir el resto
  'print #1,"antes del FOR Roll,RollAux.trk.. indpos,indaux ", indpos,indaux
  ' debemos barrer solo la octava? no porque puede haber otras octavas cargadas
  For s= NB To NA 'barremos todas las notas verticalmente para una misma posicion
   If Roll.trk(indpos, s).dur = 182 Or indpos=MaxPos Then ' fin de datos de Roll
    RollAux.trk(indaux, s) = Roll.trk(indpos, s)
    insert= 4 ' fin de llenado de almacenamiento auxiliar
  '      print #1,"sale moveresto encontro 182 fin, indaux :" ,indaux
    Exit Do
   EndIf
   RollAux.trk(indaux, s) = Roll.trk(indpos, s)
   If s=1 Then
  '    print #1, "luego de la copia a AUX del resto "
   EndIf
   If (Roll.trk(indpos, s).dur <>181  And Roll.trk(indpos, s).nota <> 181) _
    And (Roll.trk(indpos, s).dur > 0 Or Roll.trk(indpos, s).nota > 0)    Then
   '    print #1, "Roll.trk(";s,indpos;").nota ",Roll.trk(s,indpos).nota
   '    print #1, "Roll.trk(";s,indpos; ").dur ",Roll.trk(s,indpos).dur
   EndIf
   If (RollAux.trk(indaux, s).dur <> 181 And RollAux.trk(indaux, s).nota <> 181) _
    And (RollAux.trk(indaux, s).dur > 0 Or RollAux.trk(indaux,s).nota > 0)    Then
   '    print #1, "RollAux.trk(";s,indaux; ").nota ",RollAux.trk(s,indaux).nota
   '    print #1, "RollAux.trk(";s,indaux; ").dur ",RollAux.trk(s,indaux).dur
   EndIf

  Next s

  'posicion + curpos tiene la ultima posicion de entrada o reemplazo
 Loop
 'tenemso todos los datos viejos en auxiliar
 'hay que copiarlos en Roll luego del ultimo reemplazo o insercion
 ' el 66 lo muevo??? sino debo hcer indaux = indaux -1
 ' indaux=indaux - 1 no le resto 1 copio el 66...el fin se corre

' print #1, "StartInsert: " ,StartInsert '  ya se le sumo curpos
 If insert = 4 Then 'movemos de nuevo todo el aux completo a Roll desde
  ' laultimainsercion o sea Startindice +
 ' print #1, "insert 4 movemos Aux a ROLL indaux maximos elementos en Aux: ",indaux
  t = StartInsert + cantInserciones ' xxx  OK

 ' print #1, "posicion, posn "; posicion, posn
  For k = 1 To indaux ' todo el auxiliar SUMA ROLL REEMPLAZADO + NOREEMPLAZADO
 '  print #1, "k auxiliar "; k
   For s=NB To NA  ' todos los semitonos 96
    Roll.trk(t,s) = RollAux.trk(k,s)
    If s=1 Then
 '    print #1, "luego de la  copia A ROLL "
    EndIf
    If (Roll.trk(t,s).dur <> 181 And Roll.trk(t,s).nota <> 181) _
     And (Roll.trk(t, s).dur > 0 Or Roll.trk(t, s).nota > 0)    Then
   '    print #1, "I) Roll.trk(";s,t;")nota,s,t : ", Roll.trk(s,t).nota, s, t
   '    print #1, "I) Roll.trk(";s,t;")dur, s,t : ", Roll.trk(s,t).dur,s,t
    EndIf
    If (RollAux.trk(k, s).dur <> 181 And RollAux.trk(k, s).nota <> 181) _
     And (RollAux.trk(k, s).dur > 0 Or RollAux.trk(k, s).nota > 0)    Then
   '    print #1, "II) RollAux.trk(";s,k;").dur ", RollAux.trk(s,k).dur
   '    print #1, "II) RollAux.trk(";s,k;").nota ", RollAux.trk(s,k).nota
    EndIf
   Next s
   t=t+1
  Next k
 ' print #1, "Valor despues de END insercion, posicion, posn "; posicion, posn
  '' posicion = MaxPos con posn es suficiente,sino se va al tramo final
  posn = MaxPos
'  print #1, "usamos MxPos pra posicion, posn "; posicion, posn
  '' esto jode porque con insert=0 entra y dej aind= 0' final del ciclo
 ' print #1 ,"final ciclo moveresto a AUX"

  For s = NB To NA
   For k=  StartInsert To MaxPos ' gracias a esto anda acordes
    If Roll.trk(k, s).nota = 0 Then
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
  Dim i As Integer
  For k= StartInsert To StartInsert + cantInserciones -1 ' OK PARA 1 INSERCION ES SOLO EN STRTINSERT
   i = i + 1
   For s= NB To NA
    If s <> notasInsertadas (i) Then
     Roll.trk(k, s).nota = 181
     Roll.trk(k, s).dur  = 0
    EndIf
   Next s
  Next k
 EndIf
 insert=0
 ReCalCompas(Roll) ' 09-05-2021
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
  MayorDurEnUnaPosicion (j)
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
     NB => 0 + (desde-1) * 13   ' 27 para 3
     NA => 11 + (hasta-1) * 13  ' 90 para  7
     pmTk(0).NB= NB
     pmTk(0).NA= NA     

' hacemos un redim preserve por default
     *po=hasta-1
     print #1,"CambiarDim desde ";desde;" hasta ";hasta;" *po ";*po;" NB ";NB;" NA ";NA
     print #1,"CantTicks ";CantTicks
     
If redi=1 Then 
     ReDim  Preserve (Roll.trk ) ( 1 To CantTicks, NB To NA)
     ReDim  Preserve (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
Else
     ReDim  (Roll.trk ) ( 1 To CantTicks, NB To NA)
     ReDim  (Track(ntk).trk ) (1 To CantTicks, 1 To lim3)
EndIf   
ReDim (RollAux.trk) ( 1 To CantTicks,NB To NA )
 desdevector=desde
 hastavector=hasta
 
print #1,"FIN CAMBIARDIM "



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
nombreArchivo=InputBox("IR A COMPAS" ,"Entre una posicion", default)
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
            print #1,"NroCompas: ", NroCompas
            print #1,"CompasX: ", CompasX
            For ni=1 To MaxPos
            print #1,"ni: ,posi, nro Compas ", ni, Compas(ni).Posi, Compas(ni).nro
               If Compas(ni).nro = compasX Then
                  posicion = Compas(ni).Posi
                  print #1,"encontro : ", CompasX
                  Exit For
               EndIf
            Next ni
          EndIf  

 Case "[TEMPO]"
    
' debo generar un default para manual 60 y oro para ejecs 240
   If  EJECCARGADA Then ' cargar lo que hhay aen el archivo
       If  ticksdefault <> "" Then
           default2=ticksdefault
       Else
            default2=Str(tiempoPatronEjec)
       EndIf
       ticksdefault = InputBox("Ajuste Tempo Ejec" ,"Entre un Tempo ",default2)
       tiempoPatronEjec= cint(ticksdefault) '08-01-2022 de doble a int
   Else
       If  ticksdefault<>"" Then
           default=ticksdefault
       Else
          default=Str(tiempoPatron)  ' manual
       EndIf 
     ticksdefault = InputBox("Ajuste Tempo Manual" ,"Entre un Tempo ",default)
      tiempoPatron= cint(ticksdefault) '08-01-2022 de doble a int
   EndIf
    Print #1,"llego a TEMPO en entrar tecla",ticksdefault 
   
' o sea el default de tiempoPAtron para manual es 60, pero para ejecuciones es 240
If  EJECCARGADA=FALSE Then
    If tiempoPatron = 0  Then
       tiempoPatron=60
       TickPlay=TickChico ' equivale a 240
   EndIf
EndIf
If  EJECCARGADA = TRUE  Then
   If tiempoPatronEjec=0 Then
      tiempoPatronEjec=240
      ticksdefault="240"
     TickPlay=TickChico
  Else 
     TickPlay=TickChico*240/tiempoPatronEjec
  EndIf
EndIf

 Case "[FACTOR]"
   default=Str("1.25")
    Print #1,"llego a TEMPO en entrar tecla",NombreArchivo 

    NombreArchivo = InputBox("Ajuste Factor Tempo " ,"Entre un Factor ",default)
      
    Print #1,"nombreArchivo de click ok",nombreArchivo
    FactortiempoPatron= cdbl(nombreArchivo) '08-01-2022 de doble a int
    Print #1,">>>>>>>>Factor TIEMPO PATRON NUEVO>>>>>>>", FactortiempoPatron
 Case "[NROREP]"
  default=Str("2")
  NombreArchivo = InputBox("Repeticiones " ,"Cantidad entera ",default)
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
End Select

End Sub


Sub velocidades(j As integer)
    If TCompas="4/4" Then
      If acumulado <= 2500000 Then 'nollego a negra
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 2500000 And acumulado <= 5000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 5000000 And acumulado <= 7500000 Then 'nollego a 2 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 7500000 And acumulado <= 10000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      
     
      
    EndIf
    If TCompas="12/8" Then
      If acumulado <= 3750000 Then 'nollego a I*
       compas(j).nro = -1 ' fuerte
      EndIf
      If acumulado > 3750000 And acumulado <= 7500000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' debil
      EndIf
      If acumulado > 7500000 And acumulado <= 11250000 Then 'nollego a 2 negra
       compas(j).nro = -3 ' semifuerte
      EndIf
      If acumulado > 11250000 And acumulado <= 15000000 Then 'nollego a 2 negra
       compas(j).nro = -2 ' semifuerte
      EndIf
     
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

Sub GrabarArchivo (grabar_como As Integer)
' GRABA ARCHIVOS CON EXTENSION Y FORMATO *.ROLL
' el borrado de undo solo se deberia ahcer si se crea un archivo nuevo y se borra
' O SI EL USUARIO VACIA EL UNDO BOTON O MENU QUE HARA FALTA
' el que se esta editando o se carga un nuevo archivo
' para roll ntk es siemrpe cero
ntk=0
Dim  As Integer npl
     If nombre = "" Then ' backup antes de aumentar el tamaño del vector
        nombre = "Backup"+ Date + ".roll"
     Else
       npl= InStr(UCase(nombre),".TRK") 
       If npl > 0 Then
          nombre=Mid(nombre,1,npl-1)
          print #1,"un trk grabado como roll",nombre   
       Else
        If InStr(UCase(nombre),".ROLL") = 0 Then
           nombre = nombre +".roll"   
        EndIf
       EndIf 
     EndIf   
     print #1,"NOMBRE A GRABAR COMO",nombre
 
    ga=FreeFile 
    Open nombre  For Binary Access write As #ga
  ''''  Open "test-AAAAA.TXT" For Output    As #2
     Dim Trabajo (1 To Maxpos, NB To NA) As dat
     Dim grabaPos (1,1)  As dat
     Dim grabaLim (1,1)  As dat 
     Dim graba3   (1,1)  As dat ' 04-02-2022 se agregan 48 bytes para info futura 
     Dim graba4   (1,1)  As dat 
     Dim graba5   (1,1)  As dat 
     Dim graba6   (1,1)  As dat 
     Dim graba7   (1,1)  As dat 
     Dim graba8   (1,1)  As dat 
     Dim graba9   (1,1)  As dat 
     Dim graba10  (1,1)  As dat 
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
     Dim As ubyte y1,y2,y3,y4, y5  
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
     ' grabamos maxpos en 5 ubyte y notaold (1 a 12 no notapiano)
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     grabaPos(1,1).pb   = y5
Print #1,"grabando tipoescala_num_ini en el archivo ",tipoescala_num_ini 
     grabaPos(1,1).inst = CUByte(tipoescala_num_ini) ' 20-12-2021 - tipoescala en uso
    '-----------------------
     grabaLim(1,1).nota = CUByte(desde) 
     grabaLim(1,1).dur  = CUByte(hasta)
     grabaLim(1,1).pb   = CUByte(notaold)
Print #1,"grabando notaescala_num_ini en el archivo ",notaescala_num_ini
     grabaLim(1,1).vol  = CUByte(notaescala_num_ini) ' notadeescala 20-12-2021
Print #1,"grabando alteracion en el archivo ",alteracion
     grabaralteracion (grabaLim(1,1).pan)
     grabaLim(1,1).inst = CUByte(tiempoPatron) '08-01-2022 hasta 256 sera suficiente¿?
/' 
Negras por minuto	 tempo
40-43	Grave
44-47	Largo
48-51	Larghetto
52-54	Adagio
55-65	Andante
66-69	Andantino
70-95	Moderato
96-112	Allegretto
113-120	Allegro
121-140	Vivace
141-175	Presto
176-208	Prestissimo
'/    
     If instru=0 Then 
        instru=1
     EndIf
     If instru > 0 Then
       Trabajo(1,NA).inst = CUByte(instru)
     EndIf
     If CANCIONCARGADA =TRUE Then ' SELECCION DE ISNTRUMENTO CON CANCION CARGADA
        Trabajo(1,NA).inst = Track(ntk).trk(1,1).nnn
     EndIf
     graba3(1,1).inst =pmTk(ntk).canalsalida ' el CANAL MIDI 1 A 16 QUE ACA VA DE 0 A 15

     graba3(1,1).dur = pmTk(ntk).portout
     
     ' ------------------------------------
     ' para seguir poniendo notas nuevas
     ' hacemoslo mismo para l ultima not que se grabo que teng el 182
     ' esa es lapapa recorremos todas las dur de las notas en donde este el 182
     ' en dur tomamosla nota y esala grabamos en pb o ins
     ' cuadno la cargamos lo ahcemos en notaold !!!! y Vuala!!!
     ' -------------------------------------
'    Dim As Integer i,j,notafinal
'    For j = 1 To MaxPos
'     For i = NB To NA
'      If Roll.trk(i,posicion+1).dur = 182 Then
'       notafinal= Roll.trk(i,posicion).nota
       '        print #1, "ENCONTRO NOTA FINAL ", notafinal
       '        Print "ENCONTRO NOTA FINAL ", notafinal
       ' esten la posiciond ela ultimanotapero grasoerror
       ' o seaque no tiene duracion solo el 182
       ' esta mal ...
'       Exit For
'      EndIf
'     Next i
'    Next j
'     grabaPos(1,1).pb = notafinal
 '    print #1,"****))) nota final y dur final "; notafinal,Roll.trk(i,posicion+1).dur
     '****))) nota final y dur final  0   0
     ' no existe mas 182 no se graba enningun lado q raro
     ' Grabacion de Trabajo
     ' ------------------------------------

     '  print #1,"MaxPos grabada en Trabajo ",MaxPos

     Put #ga, ,grabaPos(1,1)
     Put #ga, ,grabaLim(1,1)
     Put #ga, ,graba3  (1,1)
     Put #ga, ,graba4  (1,1)
     Put #ga, ,graba5  (1,1)
     Put #ga, ,graba6  (1,1)
     Put #ga, ,graba7  (1,1)
     Put #ga, ,graba8  (1,1)
     Put #ga, ,graba9  (1,1)
     Put #ga, ,graba10 (1,1)
               
     Put #ga, ,Trabajo()
     cerrar(ga)
     While InKey <> "": Wend
     Sleep 150
     
If nombre = "Backup"+ Date + ".roll" Then
   nombre=""
EndIf
End Sub
'
Sub CargarPistasEnCancion ()
' cada vez que cargo borro la info de fechas de pistas anterior
ROLLCARGADO=FALSE
If IX< 3 Then
  DisableGadget(LISTA_DE_PISTAS,0) ' HABILITAMOS LISTA DE PISTAS
EndIf
ReDim As Double fechasPistas(1 To 32)
print #1,"-------------------------------------------------------"
print #1,"inicia CargaPistasEnCancion"
  Dim As String no1, no2
  Dim As Integer ubi1=0,ubi2=0 
     Dim As String filename, filenameold
     ' el Dir me trae lso nombres sin el path de cancion
     filename = Dir (NombreCancion+"\*.rtk")
     print #1,"filename encancion > 0 ",filename
     If filename = "" Then ' no hay ningu archivo dentro del dir de cancion
        ntk=1
        print #1,"dice que no hay archivos "
        CANCIONCARGADA=FALSE
        ' NO HAY NADA QUE CARGAR
        abrirRoll=2 ' habilita otro camino para cargar pistas
        Exit Sub
     Else
       ' carga Todos los tracks de cancion en un Loop
       Dim  As Integer mayor=1
       Do While Len(filename) > 0 ' If len(filename) is 0, exit the loop: no more filenames are left to be read.
        filenameold=filename
        print #1, "trabajo con este filename...", filenameOld '[1]AAA.rtk por ejemplo
        Dim cadena As String
        cadena= sacarExtension(filenameOld) ' [1]AAA
       ''' If pistacreada=0 Then
          If instancia=7 Or instancia= 107 Then ' en batch no hay ventana control 
          Else   
          AddListBoxItem(3, cadena)
          End If  
       ''' EndIf
        ntk= sacarNtk(filenameold)  ' 1
        If mayor< ntk Then
           mayor=ntk
        EndIf
  ' va cargando los track internos, tomando el nro de track 
  ' del nombre del archivo solamente , ergo el usuario puede cambiar el orden
  ' o poner un trakck de otra cancion con un numero que no exista [x]
  '      
         ' cargamos un track simulamos como linea de cmd o cancion cargada
        nombre=NombreCancion+"\"+filename
        titulos(ntk)=nombre ' como agregue en titulos la opcion es 1
        ' como si viniera de lie ad ecomadno puedo usar cualquiera o 0
        pistas (ntk)=filename
        print #1,"nombre en CargarPistasEnCancion ,Numpista",nombre, ntk
         'se carga track nada mas si dialogo simualmos un ubirtk>0 (1)
        CargarTrack  (Track() , ntk, 1) ' se cargo a track x nada mas
 'durante la cargaTrack el programa va a Rolloop se encuentra con SC_TAB
 ' y si cargacancion esta en 1 trata de cargar y no lo debe ahcer de modo
 ' que se ajusta a 0 cargacancion dentro de la rutina veremos 
'        
        Sleep 100
        filename = Dir()
                
       Loop
       tope=mayor  ' el nro tope de la lista
       print #1,"CARGO PISTAS MAXIMA CANTIDAD TOPE=",tope
       CANCIONCARGADA=TRUE
       ntk=1   ' tab le suma 1 pero si creo pistas estria  mal 05-03-2022 ÇÇÇ
    EndIf

cargaCancion=1 '12-02-2022 mientras carg las pista el 1 indica cargando pistas    
    print #1,"FIN CargaPistasEnCancion no debe calcuar *po ",*po
    print #1,"-------------------------------------------------------"
'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0  05-mar-2024 veremos
'mouse_event MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 
      
End Sub

' ---------------------------------
Sub GrabarTrack (ntk As integer)  'Error seguro aca nota=181 posicion1
' solo util para Grabar Cancion en sus track independietnes
' toma todos los valores de pmTk(ntk)
' Graba cancion en sus archivo sde tracks independientes...
' Otra forma será DE MUCHOS TRACKS UN VECTOR, GRABA UNA SOLA OCURRENCIA 
' en donde todos los tracks cargado se grabaran a disco pero no como track 
' individual sino como el track de 2 dimensiones por lo tanto esta
' subruttina debe copiarse a otra y cambiarle el nombre diciendo <<GrabarCancion tipo carpeta>>
' como 1er paso deberia pasar de RollaTrack en memoria para que quede el Track
' en edicion con todo lo modificado en Roll. dEspues sí, grabar el vector
' multiple con todos los tracks. Esa serai al forma compacta de grabar una
' cancion, OTra forma seria crear una carpeta y grabar todos los tracks o pistas 
' individualmente y ahi si usaria esta sub, teniendo en cuenta de que si el track 
' grabado esta en edicion en Roll 1ero pasar de rollatrack ..
' SE SUPONE QUE TODOS LOS VALORES O PARAMETRO SD TRACK EN ESTE MOMENTO 
' ESTAN EN LAS VARIABLES SIMPLES ADEMAS DEL VECTOR POR EOS NO HACE FALTA
' SACARLAS DESDE EL VACTOR ADEMAS EL TRACK PUDO HABER TENIDO MODIFICACIONES
' EL VECTOR ESTARA CON PARAM VIEJOS
 Dim gt As Integer
 print #1,"---------------------------------------------------------------------------------"
 Dim ntknom As String 
 ntknom=titulos(ntk)
 print #1,"  inicia GrabarTrack NUMERO= ",ntk
 gt=FreeFile
 Maxpos=pmTk(ntk).MaxPos
    Open ntknom  For Binary Access write As #gt
     Dim Trabajo (1 To  MAxPos, 1 To lim3 ) As poli
   'r  Erase Trabajo ' todo a cero par aun array fijo no dinamico
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
     Dim As ubyte y1,y2,y3,y4, y5 
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
     grabaPos(1,1).nota = y1
     grabaPos(1,1).dur  = y2
     grabaPos(1,1).vol  = y3
     grabaPos(1,1).pan  = y4
     grabaPos(1,1).pb   = y5
     grabaPos(1,1).nnn = pmTk(ntk).tipoescala
If NombreCancion > "" Then
     If CheckBox_GetCheck( cbxnum(ntk))= 1 Then ' sonido on/off 16-03-2022
         print #1,"grabo grabaPos(1,1).dur2=1,, ntk", ntk
         grabaPos(1,1).dur2=1
     Else
         grabaPos(1,1).dur2=0
     EndIf
EndIf   
     '-----------------------
     grabaLim(1,1).nota = CUByte(pmTk(ntk).desde) 
     grabaLim(1,1).dur  = CUByte(pmTk(ntk).hasta)
     grabaLim(1,1).pb   = CUByte(pmTk(ntk).notaold) 
     grabaLim(1,1).vol  = CUByte(pmTk(ntk).notaescala) ''   CUByte(notaescala_num_ini) ' notadeescala 20-12-2021     


     grabaLim(1,1).pan  = pmTk(ntk).alteracion
     grabaLim(1,1).nnn = CUByte(tiempoPatron)

     graba3(1,1).dur = pmTk(ntk).portout
     graba3(1,1).nnn = pmTk(ntk).canalsalida 
     Print #1,"grabartrack graba3(1,1).nnn, ntk ",graba3(1,1).nnn , ntk

     '  print #1,"MaxPos grabada en Trabajo ",MaxPos

     Put #gt, ,grabaPos(1,1)
     Put #gt, ,grabaLim(1,1)
     Put #gt, ,graba3  (1,1)
     Put #gt, ,graba4  (1,1)
     Put #gt, ,graba5  (1,1)
     Put #gt, ,graba6  (1,1)
     Put #gt, ,graba7  (1,1)
     Put #gt, ,graba8  (1,1)
     Put #gt, ,graba9  (1,1)
     Put #gt, ,graba10 (1,1)
     
     Put #gt, ,Trabajo() ' ACA DEBERIA QUEDAR EL CAMPO ACORDE !!
     cerrar (gt)
     While InKey <> "": Wend
     Sleep 150
print #1,"---------------------------------------------------------------------------------"
print #1,"  Fin GrabarTrack "
End Sub

' ---------------------------------
'---------------------
Sub GrabarCancion() ' PENDIENTE GRABAR TODA LA CANCION EN UN SOLO COMANDO
 '1) recorro titulos(ntk) los titulos que quedan se graban, los otros se borran o
 ' en el momento de borrar se copia a backup y se borra del directorio de cancion
 ' eso si se borro alguna pista. (en vez de borrar mandamos a una carpeta de backup)
 '2) cada pista se graba con GrabarRollaTrack
Dim As Integer ntkvivo =ntk

  If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
     If NombreCancion > ""  And MAxPos > 1 Then
        GrabarRollaTrack(0)
     EndIf
  EndIf


Dim i As Integer 

  For i=1 To tope
   If i<> ntkvivo Then 
      GrabarTrack(i)
   EndIf
  Next i
  
End Sub


Sub  selport (ByVal tipo As Any Ptr)  ' list de ruso
' SELECCION DE CASO 1 PORT DE SALIDA OUT, O CASO 2 ENTRADA MIDI IN
Dim As hwnd haw,hwl
Dim As Integer mitipo,x0,y0,Posx,Posy 
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
Dim cad As string
 
''portsout = port_count (midisal)
' ESTOY ABRIENDO LSO PORT SEGUN LO ELEGIDO CADA VEZ QUE SE ELIJA UNO
' DEJA DE ESTAR DISPONIBLE PUES YA ESTA ABIERTO, PONDREMOS UN MENSAJE ABIERTO CREO ES MEJOR 
    If NombreCancion > "" Then
    Else
       ntk=0
    EndIf



mitipo=  CInt(tipo)
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("MIDI",500,Posy,500,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     hwl=  ListViewGadget(1,10,10,350,400,,,,32,LVS_SINGLESEL  )
     listports()
  

     If mitipo = 1 Then ' out
       AddListViewColumn(1, "Salidas  MIDI",0,0,250)
       For aa As Integer=0 To UBound (listout) 
          If pmTk(ntk).portout = aa Then
             AddListViewItem(1, "[x] "+listout(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listout(aa),0,aa,0)
          EndIf   
           
       Next

     EndIf
     If mitipo = 2 Then ' in entradas
      
       AddListViewColumn(1, "Entradas MIDI",0,0,250)
       For aa As Integer=0 To UBound (listin) 
          If pmTk(ntk).portin = aa Then
             AddListViewItem(1, "[x] "+listin(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listin(aa),0,aa,0)
          EndIf   

       Next

     EndIf


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
               If  mitipo=1 then           
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
              pmTk(ntk).portout=CUByte(portout)

                For  i As Short =1 To 32
                      If CheckBox_GetCheck( cbxnum(i))= 1  Then
                           pmTk(i).portout=CUByte(portout)
                     EndIf
                Next i
               
              EndIf
              If mitipo=2 Then
              Print #1,"PORT SELECCIONADO en menu ",portin
                pmTk(ntk).portin =CUByte(portin)

              EndIf

              If CANCIONCARGADA=TRUE  Or TRACKCARGADO=TRUE  Then
                 If NombreCancion > ""  And MAxPos > 1 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 1  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo??
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
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
' SendMessage(GadgetID(1), EM_UNDO, 0, 0) 'undo the last edit

'SendMessage(GadgetID(1), EM_REDO, 0, 0) 'redo

'SendMessage(GadgetID(1), EM_GETSEL, @Min,@Max) 'COPY

'SendMessage(GadgetID(1), EM_SETSEL, @Min,@Max ) 'select

'SendMessage(GadgetID(1), EM_SETSEL, 0,-1) 'select all

'SendMessage(GadgetID(1), EM_REPLACESEL, 0,@TEXT) 'PASTE
' abrir port

  
'' fin ruso
cierroport=0
End Sub

Sub  selportEjec (ByVal tipo As Integer,ntkp As Integer )  ' list de ruso
Dim As hwnd haw,hwl
Dim As Integer mitipo,x0,y0,Posx,Posy 
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
Dim cad As string
' el port default de entrada y de salida cuandi envio a rtmidi es el cero fisico 
' pero para esta logica es el 1, es mejor usar otra variable
 
''portsout = port_count (midisal)
' ESTOY ABRIENDO LSO PORT SEGUN LO ELEGIDO CADA VEZ QUE SE ELIJA UNO
' DEJA DE ESTAR DISPONIBLE PUES YA ESTA ABIERTO, PONDREMOS UN MENSAJE ABIERTO CREO ES MEJOR 
    If NombreCancion > "" Then
    Else
       ntkp=0
    EndIf



mitipo=  CInt(tipo)
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("MIDI EJECUCIONES",500,Posy,500,400,WS_OVERLAPPEDWINDOW Or WS_VISIBLE , WS_EX_TOPMOST )
     hwl=  ListViewGadget(1,10,10,350,400,,,,32,LVS_SINGLESEL  )
     listports()
     Print #1,"portin ", portin
Print #1, "ntkp ",ntkp
     If mitipo = 1 Then ' out
       AddListViewColumn(1, Str(UBound (listout)+1 )+" Salidas  MIDI",0,0,250)
           For aa As Integer=0 To UBound (listout) 
               If pmTk(ntkp+32).portout = aa  Then 
                  AddListViewItem(1, "[x] "+listout(aa),0,aa,0)
               Else
                 AddListViewItem(1, "[ ] "+listout(aa),0,aa,0)
               EndIf   
         Next
     EndIf
     If mitipo = 2 Then ' in entradas
      
       AddListViewColumn(1, Str(UBound (listin)+1 )+" Entradas MIDI",0,0,250)
       For aa As Integer=0 To UBound (listin) 
          If pmTk(ntkp+32).portin = aa Then
             AddListViewItem(1, "[x] "+listin(aa),0,aa,0)
          Else
             AddListViewItem(1, "[ ] "+listin(aa),0,aa,0)
          EndIf   

       Next

     EndIf


       ButtonGadget(2,380,30,100,40,"CAMBIA")
       ''''ButtonGadget(3,380,70,100,40,"CANCELA")
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
               If  mitipo=1 then           
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

                If mitipo=1 Then ' PORT SLIDA
                  Print #1,"PORT SELECCIONADO en menu ",portout 
                  Print #1,"Nombre PORT SELECCIONADO  ",listout(portout)
                  For  i As Short =1 To 32
                        If CheckBox_GetCheck( cbxejec(i))= 1  Then
                            pmTk(i+32).portout=CUByte(portout)
                       EndIf
                  Next i
                EndIf
                If mitipo=2 Then ' PORT ENTRADA
                    Print #1,"PORT SELECCIONADO en menu ",portin 
                    Print #1,"Nombre PORT SELECCIONADO  ",listin(portin)

                    For  i As Short =1 To 32
                        If CheckBox_GetCheck( cbxejec(i))= 1  Then
                           pmTk(i+32).portin =CUByte(portin)
                        EndIf
                    Next i
                EndIf  
              ' FreeGadget(1)
                Close_Window(haw)
                Exit Do 
             End If
             If eventnumber()=3  Then
              '  FreeGadget(1)
                Sleep 5
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
cierroport=0
End Sub


' 
Sub  selcanal (ByVal tipo As Any Ptr )  ' list de ruso
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw, hwl
Dim As Integer mitipo, x0,y0,Posx,Posy,cambio=0  
mitipo=  CInt(tipo)
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
         If pmTk(ntk).canalsalida > 0 And pmTk(ntk).canalsalida = aa -1 Then
               AddListViewItem(1, "[x] "+listCanal(aa),0,aa,0)
               Print #1,"pmTk(ntk).canalsalida ",pmTk(ntk).canalsalida
               ntk=aa 
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
            If  MAxPos > 1 Then
               pmTk(ntk).canalsalida=CUByte(canalx)
               If NombreCancion > ""  Or TRACKCARGADO =TRUE Then
                   ' sirve para el track cargao actual en cancion
                     GrabarRollaTrack(0)
               Else
                  If ROLLCARGADO=TRUE  Then
                   GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
                  ' no el undo dolo se debe borrar al ahcer nuevo creo
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
Sub  selcanalEjec (ByVal tipo As Any Ptr, ntkp As integer )  ' list de ruso
Dim As hwnd haw,hwl
Dim As Integer mitipo, x0,y0,Posx,Posy,cambio=0  
mitipo=  CInt(tipo)
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
         If pmTk(ntkp+32).canalsalida > 0 And pmTk(ntkp+32).canalsalida = aa -1 Then
               AddListViewItem(1, "[x] "+listCanal(aa),0,aa,0)
               Print #1,"pmTk(ntkp+32).canalsalida ",pmTk(ntkp+32).canalsalida, 
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
              If mitipo=1 Then
                   Print #1,"CANAL SELECCIONADO en menu ",canalx
                   For  i As Short =1 To 32
                         If CheckBox_GetCheck( cbxejec(i))= 1  Then
                             pmTk(i+32).canalsalida=CUByte(canalx)
                        EndIf
                   Next i
              EndIf
              If mitipo=2 Then
                  Print #1,"CANAL SELECCIONADO en menu ",canalx
                For  i As Short =1 To 32
                      If CheckBox_GetCheck( cbxejec(i))= 1  Then
                        pmTk(i+32).canalentrada =CUByte(canalx)
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
   var event=WaitEvent()
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
print #1,"MAxPos inicio ", MAxPos
     Dim Trabajo (1 To Maxpos, NB To NA) As dat
     Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0
'eliminar columnas marcadas al grabar disco
 For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=254 And Roll.trk(i2,i1 ).dur=254 Then
          borrocol= 1 'Atrapa al menso un caso
          
       EndIf
       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
          print #1, Roll.trk(i2,i1 ).nota;"*";Roll.trk(i2,i1 ).dur;"|";
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
          print #1,"C:"; Trabajo(k, i1).nota;"*";Trabajo(k, i1).dur;"|";
          EndIf  
        Next i1
      
      Else
       MaxPos=MaxPos -1  
      EndIf
      
      borrocol=0:res=0:haynota=0
      print #1,
 Next i2
'-------------------------
 print #1,"MAxPos Final ", MAxPos
print #1,"------------------------------------------"
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
     print #1, Trabajo(i2,i1).nota;"*";Trabajo(i2,i1).dur;"|";
    Next i2
    print #1,
   print #1, "------------------------------------------" 
Next i1
'shell"notepad midebug.txt"
Exit Sub 
'-------------------------

End Sub
'
Sub borrarColumnasMarcadas()
' toda columna en una posicion que tengasolo 190 en su nota y durcion
' sera eliminada y todaslas demas desplazadas a izquierda,.
' es trabajoso hay que pulsar 0 y dar x en tola la columna 
' anda ok , igualmente implementare por zona 
' pregunto si las zonas estan marcadas uso ese emtodo y sino uso el metodo viejo
'<====== METODO NUEVO POR ZONAS ===> sI ESTAN MARCADAS Y PULSO CTRL-DELETE SE BORRA TODA ESA
' ZONA O COLUMNA SI SE SUPERPONEN LAS MARCAS DE ZONA 2 CLICKS SOBRE LA MISMA COLUMNA
' O UNA SOLO CLIK MEJOR,,,,
' pasoZona1 pasoZona2 
Print #1," BORRACOLUMNASMARCADAS"

Dim As Integer i1, i2, borrocol=0, haynota=0,res=0,k=0,delta=0
If pasoZona1 > 0 Or (pasoZona1 > 0 And pasoZona2 > 0) Then
' metodo nuevo ok
  Print #1,"pasoZona1 pasoZona2 ",pasoZona1, pasoZona2
  
  If pasoZona1 > 0 And pasoZona2 = 0 Then ' borrado de esa columna ok
    Print #1,"entra por pasoZona1  "
    For k=pasoZona1 To MAxpos 
     For i1 = NB To NA
         Roll.trk(k, i1)=Roll.trk(k+1,i1 )
     Next i1
    Next k
    pasoZona1=0
  EndIf
  
  If pasoZona1 > 0 And pasoZona2 > 0 Then ' borrado del rango del paso ok
    Print #1,"entra por pasoZona1 y 2  "
    delta=pasoZona2 - pasoZona1
    For k=pasoZona1 To MAxpos 
     For i1 = NB To NA
         Roll.trk(k, i1)=Roll.trk(k+delta+1,i1 )
     Next i1
    Next k
  
  
     pasoZona1=0 :pasoZona1=0
  EndIf
  
Else
' <====METODO VIEJO ======>

For i2 = 1 To MaxPos
     For i1 = NB To NA
       If Roll.trk(i2,i1 ).nota=190 And Roll.trk(i2,i1 ).dur=190 Then
          borrocol= 1 'Atrapa al menso un caso
       EndIf
       If Roll.trk(i2,i1 ).nota >=1 And Roll.trk(i2,i1 ).nota<=12 Then
          haynota=1 ' atrapa al menso 1 caso
       EndIf
       
       If i1= NA Then 
        If haynota=1 Then ' sihay unasola notaen la columna no borro
         res=0 ' no borrar
        Else
          If borrocol = 1 Then
             res=1  
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

End if
 
End SUB

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
           FT_Done_Face( ftface )
           If play=1 Or playb=1 Then
              '''alloff (canal)
              
              For i3 = 1 To tope
               portsal = pmTk(i3).portout 
               alloff (pmTk(i3).canalsalida, portsal )
               close_port(midiout(portsal))
               out_free(midiout(portsal))

               portsal = pmTk(i3+32).portout 
               alloff (pmTk(i3+32).canalsalida, portsal )
               close_port(midiout(portsal))
               out_free(midiout(portsal))

              Next i3
              ThreadDetach(thread1)              
           EndIf
Sleep 10
   If teclado=1 Then
       cancel_callback(midiin(pmTk(ntkp+32).portin))
       Dim k1 As Integer
       k1=pmTk(ntkp+32).portout
    '   Print #1,"midiout ",k1, *nombreOut(k1)
       alloff( pmTk(ntkp+32).canalsalida,k1 )  
       listoutAbierto(k1)=0
       close_port midiout(k1)
       teclado=0

   EndIf
           
    ffile=FreeFile

    Open "./RollMusic.ini" For output As #ffile

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

    cerrar ffile
    Sleep 100
 Dim As Integer nroproc, ppp=0
Dim As String linea
fk=FreeFile
 Open "procesos.txt" For Input As #fk
 
       ' carga Todos los pid de las instancias
       Do While Not Eof(fk)
        Line Input #fk, linea
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
    
cerrar fk

   Close 1
'    cerrar 0
'    Close
 
' ESTO HACE CNCELAR AL CERRAR DE LA VENTANA DE CONTROL NO USAR
' 26-02-2024 
'''DeleteObject(Cast(Any Ptr,BRUSH))      
ThreadDetach(threadloop)
 DestroyWindow(hWnd)

    End 0
End Sub
'-------
Sub selTipoEscala (ByRef tipoescala_num As integer)
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
print #1,"Str(tipoescala_num) ", Str(tipoescala_num)

End Sub
'----
Sub selNotaEscala (ByRef notaescala_num As integer)
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
print #1,"Str(notaescala_num) ", Str(notaescala_num)


End sub

Sub EscalaAlternativa (ByRef tipoescala_alt As integer, byref notaescala_alt As Integer)
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
function BuscarGrado(t3 As String) As Integer
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
     If altera="bem" then
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
Function lugarNota (lugar As integer) As Integer
          lugar=nE-lugar
        If lugar <= 0 Then
           lugar=lugar + 12
        EndIf            
        If lugar >= 12 Then
           lugar=lugar -12 
        EndIf  
lugarNota=lugar
End Function

Sub armarAcorde(grado As Integer,n1 As integer,n2 As integer,n3 As integer)
  ' para mayo4      n1=4 y n2=7
  ' para menor      n1=3 y n2=7
  ' para disminuido n1=3 y n2=6
  scan_alt=0
       cnt_acor=cnt_acor +1 

       If grado=0 Then grado=1 EndIf
  '    Print #1,"Grado ",grado
      Select Case grado
        Case 1  ' es Tonica
' grabo la nota Tonica de la melodia  a partir de la cual hago el acorde
        undo_kant_intervalos(cnt_acor)=0          
        undo_acorde(cnt_acor,0).posn=indicePos ' shared global
        Dim pnr As Integer
        pnr=PianoNota+SumarnR(PianoNota)
        undo_acorde(cnt_acor,0).pn=pnr ' se almacena elindice fisico del vector Roll
        undo_acorde(cnt_acor,0).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,0).nota=Roll.trk(indicePos, pnr).nota


        ' armar acorde notapiano, Notapiano+4, NotaPiano+7=la anterior +3
        ' haremos todo a mano luego algun dia un algoritmo si es necesario,.,,
        ' qu etenga en cuenta todos los casos (formar mayor-> +4 +7)
        ' 1,n=1 es 3,n=2 es 5,n=3 es 7
        Dim As Integer st=0
  '      Print #1,"armando acorde ,,indicePos ",indicePos
  '      Print #1,"armando acorde ,,RollDur ",RollDur
        pnr=PianoNota+n1 
        pnr =pnr +SumarnR(pnr) ' es el nR o indice fisico del vector Roll
  '      Print #1,"armando acorde 3ta,,nR ",pnr
'------n1 acorde de 2 notas o intervalo
        undo_kant_intervalos(cnt_acor)=1          
        undo_acorde(cnt_acor,1).posn=indicePos
        undo_acorde(cnt_acor,1).pn=pnr ' se almacena elindice fisico del vector Roll
        undo_acorde(cnt_acor,1).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,1).nota=Roll.trk(indicePos, pnr).nota

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
' saltos de octava
        st=nE-n1 
        If st <= 0 Then
           st=st + 12
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            
        
        Roll.trk(indicePos, pnr).nota=st
'--------n2 acorde de 3 notas------------------------------------
  '      Print #1,"armando acorde ,5ta,nR ",pnr  

        pnr=PianoNota+n2
        pnr=pnr + SumarnR(pnr) ' es nR indice del vector Roll
        undo_kant_intervalos(cnt_acor)=2
        undo_acorde(cnt_acor,2).posn=indicePos
        undo_acorde(cnt_acor,2).pn=pnr
        undo_acorde(cnt_acor,2).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,2).nota=Roll.trk(indicePos, pnr).nota

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
        st=nE-n2 
        If st <= 0 Then
           st=12 + st
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            

        Roll.trk(indicePos, pnr).nota=st  
If n3 <> 0 Then ' es un acorde de 4 notas            
 '   Print #1,"armando acorde ,7ta,nR ",pnr
        pnr=PianoNota+n3
        pnr=pnr + SumarnR(pnr)
        undo_kant_intervalos(cnt_acor)=3 
        undo_acorde(cnt_acor,3).posn=indicePos
        undo_acorde(cnt_acor,3).pn=pnr
        undo_acorde(cnt_acor,3).dur=Roll.trk(indicePos, pnr).dur
        undo_acorde(cnt_acor,3).nota=Roll.trk(indicePos, pnr).nota

        Roll.trk(indicePos, pnr).dur=CUByte(RollDur)
        st=nE-n3 
        If st <= 0 Then
           st=12 + st
        EndIf            
        If st >= 12 Then
           st=st -12 
        EndIf            

        Roll.trk(indicePos, pnr).nota=st  

EndIf
             Case 3
             Case 5
             Case 7
             Case 4
             Case 6
             Case 9
             Case 11
      End Select       
 If Vaciodur Then
    ReCalCompas (Roll)
    Vaciodur=FALSE
 EndIf
 Dim As Integer guardpos
 guardpos=posicion
 pasozona1=indicePos
 pasozona2=indicePos
 playAll (roll)  'PLAYACORDE
 pasozona1=0
 pasozona2=0
 posicion=guardpos
 
End Sub
Function FiguraEquivalente(DURk As double) As UByte
Dim As ubyte i=0,j=0,k=0 ,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0
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
'''-///////////// RUTINAS DE VENTANA PRINCIPAL ROLLCONTROL ////////
''' empezamos a estructurar un poco .....06- marzo 2024
'' REVISAR DE NUEVO LAS CONDICIONES DE SALIDA DE SELECT O EXIT DO

'---------

' error
errorSub:
  
''Dim As Integer  ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 0 And ContadorError < 101 Then
 Print #1,"------------------------------------"
  ContadorError=ContadorError+1
  Print #1,"ErrorSub ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
  Print #1,"Error Function: "; *Erfn()
  Print #1, "n ",posicion;" posishow "; posishow; " NroCol ";NroCol
  Print #1, "semitono "; nE; " *po "; *po 
  Print #1, "valor1 ",posicion; " valor2 "; 12- nE  + (*po -1)* 13
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
  Print #1, "lbound 2 de Roll.trk ", lBound(Roll.trk, 2)   
  Print #1, "Roll.trk (n,11- nE  + (*po-1) * 13 ).nota", Asc(Str(Roll.trk (posicion,12- nE  + (*po -1)* 13 ).nota))
  Print #1,"Roll.trk (n,12- nE  + (*po -1)* 13 ).dur ",  Asc(Str(Roll.trk (posicion,12- nE  + (*po -1) * 13 ).dur))
  Print #1,"gap1 + ic * anchofig ",gap1 + ic * anchofig
  Print #1,"desde ";desde;" hasta ";hasta; "hasta-1 ";hasta-1; " *po ";*po
  Print #1, "nE ";nE; " nR ";nR; " PianoNota ";PianoNota
  Print #1, "mensaje, Ermn ", *Ermn, Ermn
  Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2) 
  print #1,"------------------------------------"

EndIf
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )








