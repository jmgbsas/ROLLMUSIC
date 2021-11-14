
'
Sub veoPasoCol(pasoCol() As vec,cnt As integer, cntold As Integer)
Print #1,"Desarrollo pasoCol() sin todos los campos integer "
Dim As Integer mayor,j
mayor=cnt
If cntold > cnt Then
  Print #1," cntold > cnt ",cntold,cnt
  mayor=cntold
Else 
  Print #1," cntold < cnt ",cntold,cnt

EndIf
If cntold = cnt Then
  Print #1," cntold = cnt ",cntold,cnt

EndIf
' en lso old acumulo los valores actuales solo si estan en cero
' de otro modo se hacen 0 en OFF o se incrementan en ligaduras
Print #1, "-----------------------------------"
For j=1 To mayor
Print #1," cnt ";j
Print #1, "PasoCol(";j;")" 
Print #1, "tiempoFigura ";PasoCol(j).tiempoFigura 
Print #1, "tiempoFiguraOld "; PasoCol(j).tiempoFiguraOld
Print #1, "DUR ";PasoCol(j).DUR
'Print #1, "DURold "; pasoCol(j).DURold  
Print #1, "notapiano "; pasoCol(j).notapiano
'Print #1, "notapianoOld "; pasoCol(j).notapianoOld 
Print #1, "liga ";pasoCol(j).liga       
Print #1, "ligaold ";pasoCol(j).ligaold 
Print #1, "i1 ";pasoCol(j).i1           
'Print #1, "i1 ";pasoCol(j).i1old           
Print #1, "old_time ";pasoCol(j).old_time
Print #1, "old_timeold ";pasoCol(j).old_timeold 
Print #1, "inst ";PasoCol(j).inst
Print #1, "-----------------------------------"      
Next j

End Sub

' 
Sub noteoff( note As UByte, canal As UByte) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	If canal= 1 Then
		 modo = 128
	Else
	  modo = 128 + canal
	EndIf
	
 message(1) = modo 
 message(2) = note
 message(3) = 0
 leng=3
result = send_message (midiout, p, leng)
Print #1,"EN NOTE OFF nota, RESULT", note, result

End Sub
Sub pedaloff( ) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales NOFUNCIONA
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	 modo = 67
	
 message(1) = modo 
 message(2) = 0
 message(3) = 0
 leng=3
result = send_message (midiout, p, leng)

End Sub
Sub allSoundoff(canal As UByte ) 
' canal 1
' 120 da sound off para todas las notas solo hy qu eenvirlo a 
'todoslos canales 120 es canal 0 sera modo + canal 
' habria que hacer un for 120,121,122,123 etc 120 + canal=modo
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	 modo = 120
	
 message(1) = modo 
 message(2) = 0
 message(3) = canal '' ??? donde lo saque 
 leng=3
result = send_message (midiout, p, leng)

End Sub


Sub alloff(canal As UByte ) 
' canal 1 NO FUNCIONA 
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' 120 all sound off https://www.tweakheadz.com/midi_controllers.htm
'https://www.cs.cmu.edu/~music/cmsip/readings/MIDI%20tutorial%20for%20programmers.html
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
 modo = 123
'	 MIDI.sendControlChange(123,0,c);
'CC( 123, velosity 0, channel number)
 message(1) = modo 
 message(2) = 0
 message(3) = canal
 leng=3
result = send_message (midiout, p, leng)

End Sub
Sub ChangeProgram ( ByVal instru As UByte, ByVal canal As ubyte)
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer ' canal 0 to 15 canal 0 es el 1
	If canal = 0 Then  ' 0xC0 = 12x16 = 192
		 modo = 192 ' 0xC0 + 0 = 192 +0 = 192 
	Else
	  modo = 192 + canal 
	EndIf
	' funciona!!!! se manda al inicio PERO SOLO me funcioan con todas las octavas ver por que
 message(1) = modo 
 message(2) = instru

 leng=2
result = send_message (midiout, p, leng)

End Sub
Sub noteon	( note As ubyte, vel As UByte, canal As ubyte)
	' canal 1
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer ' canal 0 to 15 canal 0 es el 1
	If canal = 0 Then
		 modo = 144 ' 0x90 + 0 = 9x16+0=144 +0 = 144 
	Else
	  modo = 144 + canal 
	EndIf
	
 message(1) = modo 
 message(2) = note
 message(3) = vel
 leng=3
result = send_message (midiout, p, leng)
Print #1,"EN NOTE ON nota, RESULT", note, result
End Sub
'-------------------------------------
Sub limpiarLigaduras(cnt As UByte,pasoCol() As vec)
Dim i1 As UByte
dim noHay As Integer=0

For i1 =1 To cnt
 If pasoCol(i1).DUR >=1 and pasoCol(i1).DUR <=90 Then ' ya no hay ligaduras
   nohay=nohay+1
 EndIf    
 
Next i1
Print #1,"LimpiarLigaduras nohay=",nohay,"cnt=";cnt
Print #1,"LimpiarLigaduras 1 cnt =",1 ,cnt
If nohay = cnt Then
  For i1=1 To cnt 
    pasoCol(i1).liga=0 
    pasoCol(i1).tiempoFigura=0
    pasoCol(i1).tiempoFiguraOld=0
  Next i1
  ligaglobal=0
Else
  ligaglobal=1  
End If
' end  jmg 09-06-2021

End Sub

' ----------------------------------

Function vol (dura As UByte,  vel As UByte) As ubyte
 If (dura >=46 And dura <= 90 ) Or (dura >=136 And dura <= 180 ) Then
    vol =0
 Else
    vol= vel
 EndIf

End Function

Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, cntold As UByte,vel As UByte,canal As UByte,tiempoDUR As Double, Roll As inst, velpos As Integer,pis As integer)

Dim As UByte i1
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
'debug..
Print #1,"---------------------------"
Print #1,"AOI:] START ACORDEONIGUALES "
Print #1,"-------------------------------------"
 veoPasoCol(pasoCol(),CInt(cnt), CInt(cntold))
' en .ligaoLD acumulo las ligaduras horizontalmente solo se va a cero en OFF ultimo
' en .liga solo determino la liga actual.-
' la  liga final de esa nota que este en cero 0 me determina que es el OFF
' final y ahi hago .ligaold=0

Dim As integer nj=jply, durj ' indice del vector roll, dur

Print #1,"AOI 1: DUR cnt=1:";pasoCol(1).Dur

old_time_on=Timer
Print #1,"AOI 2: OLD_TIME AL INICIO ",old_time_on
old_time_on_int=old_time_on * d11 
Print #1,"AOI 3: comienza For de 1 a cnt",cnt
' SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
 
For i1=1 To cnt
Print #1,"AOI 4: 1)DUR cnt=";i1;":";pasoCol(i1).Dur
 Print #1,"AOI 5:CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
' sin ligar
  If  pasoCol(i1).Dur <= 90 And pasoCol(i1).Dur >= 1  Then
    ' GUARDO EL old_time_on DE LAS NO LIGADAS SI NO HAY HISTORICO DE OLD_TIME
    ' EN ESE CASO UNA LIGADURA TERMINA EN ESTA NOTA
      If pasoCol(i1).old_timeold = 0 Then
         pasoCol(i1).old_time=old_time_on_int
      Else
         pasoCol(i1).old_time=pasoCol(i1).old_timeold 
      EndIf    
      pasoCol(i1).liga =0
      Print #1,"AOI 6: 4) no ligado,liga ";pasoCol(i1).liga =0 
      If pasoCol(i1).tiempoFiguraOld = 0 Then
          pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * d11
      Else
          pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFiguraOld
      EndIf  
      Print #1,"AOI 7: no ligado pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
      Print #1,"AOI 8: no ligado DUR "; pasoCol(i1).Dur
' el mayor valor de duracion de los no ligados no lo calculo aca
     If pasoCol(i1).ligaold =0 Then
       Print #1,"AOI 9: SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
       Print #1,"AOI 10: sin ligar NOTEON 2]pasocol("+Str(i1)+").inst en AcordeonIguales ",pasoCol(i1).inst
       ChangeProgram (pasoCol(i1).inst,0)
       vel= vol( pasoCol(i1).DUR, velpos)
       noteon pasoCol(i1).notapiano,vel,canal
       '''duracion old_time_on, pasoCol(i1).tiempoFigura/d11
     Else
        Print #1,"AOI 11:No se envia Note on es la continuancion de un ligado"  
     EndIf
  Else  
    If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <= 180 then
     Print #1,"AOI 12: ANALIZO LIGADURAS horizontalmente si hay y toda la columna"
     Print #1,"AOI 13: DUR ";pasoCol(i1).Dur
     pasoCol(i1).liga=1
     If pasoCol(i1).old_timeold = 0 Then
        pasoCol(i1).old_time = old_time_on_int
        pasoCol(i1).old_timeOld = old_time_on_int
     Else
        pasoCol(i1).old_time= pasoCol(i1).old_timeold 
     EndIf

     If pasoCol(i1).tiempoFiguraOld =0  Then
        pasoCol(i1).tiempoFigura= reldur(pasoCol(i1).DUR) * tiempoDUR * d11
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     Else
        pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld  
     EndIf

     If CANCIONCARGADA Then
          durj = Track(pis).trk(i1, pasoCol(i1).i1 ).dur
     Else
          durj = Roll.trk(i1, pasoCol(i1).i1 ).dur
     EndIf
       
     Print #1,"AOI 15:  durj "; i1 ; " "; durj
     If durj > 0 Then
         Print #1,"AOI 15:  reldur ";i1; " "; relDur(durj)
     Else 
        Print #1,"AOI 17:  reldur ";i1; " "; 0 ' revisar jmgjmg  
     EndIf
     ' liga me da la cantidad de acordes y/o nota ligados en esa nota
     ' se va borando hasta que se haya dado el off final
     print #1,"AOI 23:       numero de ligadosOld:";pasoCol(i1).ligaold
     Print #1,"AOI 24: Noteon ligado notepiano "; pasoCol(i1).notapiano
     If pasoCol(i1).ligaold > 0 And  TipoAcorde=1 Then
     ' solo es para nota ligada simple que ataca o se liga a un acorde siguiente
     ' para que el siguiernet acorde no empice jutno a la nota de inicio de la ligadura
     ' se separa la nota ligada simple con esta duracion que no se usa si fuese
     ' parte de un acorde..
'        If pasoCol(i1).ligaold = 1 Then ' si es 1er nota tomo time de la 1er ligadura
     '     duracion pasoCol(i1).old_time/d11, pasoCol(i1).tiempoFigura/d11
     '   Else 
         ' si son siguientes notas ligadas todo el time actual porque
         ' ya hice el pedazo de retardo de la nota anterior, o sea c/nota ligada
         ' hace su retardo  
          duracion old_time_on, pasoCol(i1).tiempoFigura/d11
 '       EndIf
        '''pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFiguraOld -pasoCol(i1).tiempoFigura
        
 ' usar Figura para el off y el oldtime_inicio de off       
     Else
     Print #1,"AOI 25: 1]NOTEON ligado pasocol("+Str(i1)+").inst en AcordeonIguales ",pasoCol(i1).inst
        ChangeProgram (pasoCol(i1).inst,0)
        vel= vol( pasoCol(i1).DUR, velpos)
        noteon pasoCol(i1).notapiano,vel,canal
        If tipoAcorde=1 Then
          duracion pasoCol(i1).old_time/d11, pasoCol(i1).tiempoFigura/d11
        EndIf 
     EndIf
     pasoCol(i1).ligaold = pasoCol(i1).ligaold + 1 ' para el proximo paso
     Print #1,"AOI 26: 3) LIGA=1 ==========> ";pasoCol(i1).liga
    EndIf 
  endif
Next I1

' for de NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS"

Print #1,"=====> FIN ACORDEON IGUALES O SIMPLE"


End Sub 

Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As Double,Roll As inst,velpos As Integer,pis As integer)
Print #1,"-------------------------------------"
Print #1,"AOD :]start AcordeOnDistintos cnt,cntold,pis ",cnt,cntold,pis
Print #1,"-------------------------------------"
'debug..
 veoPasoCol(pasoCol(),CInt(cnt), CInt(cntold))

Dim As UByte i1, liga=0,coff=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
If cntold >cnt Then
 coff=cntold
Else
 coff=cnt 
EndIf
Print #1,"====>>> START AOD ON veo el pasocol que tiene"
Dim As UByte noAnalizo=0 
For i1=1 To coff ' 20-06-2021 JMG
  print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
  " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time
  If pasoCol(i1).liga > 1 Then ' si hay liga viene de antes
     noAnalizo=1
  EndIf 
       
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur

Print #1,"AOD:DUR cnt=1:";pasoCol(1).Dur

old_time_on=Timer
Print #1,"AOD: old_time_on ";old_time_on

Print #1,"start FOR"
For i1=1 To cnt
Print #1,"for cnt=";i1
Print #1,"AOD FOR: pasoCol(i1).Dur       ";pasoCol(i1).Dur
Print #1,"AOD FOR: pasoCol(i1).liga      ";pasoCol(i1).liga
Print #1,"AOD FOR: pasoCol(i1).NOTAPIANO ";pasoCol(i1).notapiano
Print #1,"AOD FOR: pasoCol(i1).tiempofigOld ";pasoCol(i1).tiempoFiguraOld


' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
' Print #1,"AOD:CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
 ' este AND  para iguales no va pero para distintos??? 
 If pasoCol(i1).liga = 0 Then  ' 13-06-2021
 '   Print #1,"1) pasoCol(i1).liga =0 "
  ' de 91 a 180 son todos ligadso pero incluye los silencios ojo!
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 And pasoCol(i1).liga=0 Then
     Print #1,"AOD:ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
     Print #1,"AOD:DUR ";pasoCol(i1).Dur
     Print #1,"AOD:liga anterior";pasoCol(i1).liga
     Print #1,"AOD:notepiano ";pasoCol(i1).notapiano
          
     If pasoCol(i1).liga =0 Then ' 13-06-2021
        pasoCol(i1).liga=1
        pasoCol(i1).old_time=old_time_on * d11
        Print #1,"ligado 1 guardo old_time_on primer liga "; pasoCol(i1).old_time
     Else
        pasoCol(i1).liga=pasoCol(i1).liga + 1 ' 13-06-2021
        ' no se carga old_time sigue siendo el mismo
     EndIf
     ligaglobal=1
     liga=1
     Print #1,"*AOD:LIGA=1 ==========> ";liga
     Print #1,"*AOD: LIGA ACUMULADA ===>";pasoCol(i1).liga
     Print #1,"*AOD: ACUMULADO TIEMPOfIGURA ";pasoCol(i1).tiempoFigura
     pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     Print #1,"*AOD: ACUMULADO TIEMPOfIGURAOLD ";pasoCol(i1).tiempoFiguraOld
     Print #1,"Loop "
     Do
       nj=nj+1
       ' busca la proxima dur 
       Print #1,"pasoCol(i1).i1 ",pasoCol(i1).i1 
       Print #1,"nj ",nj
       If CANCIONCARGADA Then
          Print #1,"AOD: CANCIONCARGADA",
          durj = Track(pis).trk(nj, pasoCol(i1).i1 ).dur
          Print #1,"durj ",durj
       Else
          durj = Roll.trk(nj, pasoCol(i1).i1 ).dur
       EndIf
' problema con las octavas necesita 1 y solo vade 39 a 102 uuu
' en vez de 1 deberia ser 39         
       Print #1,"AOD:LIGA nj, durj "; nj ; " "; durj
       Print #1,"AOD:LIGA nj reldur ";nj; " "; relDur(durj)
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
       Print #1,"AOD:LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
       Print #1,"AOD:LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply '08-06-2021
     Print #1,"2) AOD:LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
     print #1,"numero de ligados:";pasoCol(i1).liga
     Print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
    Print #1,"1]pasocol("+Str(i1)+").inst en AcordeonDistintos ",pasoCol(i1).inst
     ChangeProgram (pasoCol(i1).inst,0)
     vel= vol( pasoCol(i1).DUR, velpos)
     noteon pasoCol(i1).notapiano,vel,canal
     Print #1,"3) AOD:LIGA=1 ==========> ";liga
     
  EndIf
 Else ' ya venia una ligadura ' 13-06-2021
     Print #1,"AOD:ya venia con ligadura de antes" ' 13-06-2021
     Print #1,"AOD: OJO! pasoCol(i1).old_time ";pasoCol(i1).old_time
     Print #1,"AOD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld ' 13-06-2021  
 EndIf 
  Print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then 
      Print #1,"4) AOD:LIGA=1 ==========> ";liga 
      Print #1,"AOD:|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
      Print #1,"AOD:|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
      Print #1,"AOD:|||SIN LIGA DUR "; pasoCol(i1).Dur
      Print #1,"5)AOD: LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
       Print #1,"6)AOD:|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0 And pasoCol(i1).DUR <> 181 Then 
    Print #1,"7)AOD:SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
    Print #1,"2]pasocol("+Str(i1)+").inst en AcordeonDistintos ",pasoCol(i1).inst
     ChangeProgram (pasoCol(i1).inst,0)
    vel= vol( pasoCol(i1).DUR, velpos)
    noteon pasoCol(i1).notapiano,vel,canal
 End If
Next i1

Print #1,"FIN AcordeOnDistintos"
	
 
End Sub

Sub AcordeOffIguales	( pasoCol() As vec, cnt As UByte, cntold As UByte,canal As UByte,Roll As inst, pis As integer)
Dim i1 As UByte
Dim tiempoFigura As Double 
Print #1,"-------------------------------------"
Print #1,"AFI :]========Start AcordeOffIguales cnt,cntold,pis ",cnt,cntold,pis
Print #1,"-------------------------------------"
'debug..
 veoPasoCol(pasoCol(),CInt(cnt), CInt(cntold))

'aca son iguales ergo lso off caen todo iguales al mismo tiempo
' solo que una not aligada hara off en el siguiente paso
'--------


 old_time_off=Timer
 Print #1,"AFI 1:old_time off inicial ";old_time_off
 old_time_off_int = old_time_off * d11 ' solo para guardar y poder hacer sort
 
Print #1,"AFI 2:no ligados calculo tiempo Figura y doy off:" 
' son iguales las duraciones son las mismoas no ahce falta sort
' este 1er for tendra las duraciones mas cortas pues son acordes iguales 
' y si hay un acorde debe ser igual y si esta ligado a un acorde 
' anterior es mayor a este caso y sera el caso FOR siguiente
For i1 = 1 To cnt ' (1) 
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld=0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
     Print #1,"AFI 3:i1 ";i1;" no ligado tiempoFigura ";tiempoFigura
     Print #1,"i1 ";i1;" no ligado old_time ";pasoCol(i1).old_time
     Print #1, "AFI 4:i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     Print #1, "AFI 5:call duracion o retardo con ",pasoCol(i1).old_time 
     Print #1, "AFI 6:call duracion o retardo con ",tiempoFigura
     duracion pasoCol(i1).old_time / d11, tiempoFigura
     Print #1, "AFI 7:SIN LIGAR NOTEOFF==>" 
     noteoff pasoCol(i1).notapiano ,canal
  EndIf


Next i1     

Print #1,"AFI 8:start OFF de ligados ---------" 
 Dim tf As Double
' los ligados pueden terminar en el paso 2 o seguir eso lo determino
' con el numero de liga qu epuede ser 1,2,3,4,etc
'
' duracion algo mayor 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld >0 Then
     Print #1,"AFI 9:HAY LIGADOS que llegan y terminan aca!"
     Print #1,"AFI 10:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
     Print #1,"AFI 11:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
   'old_time= pasoCol(i1).old_time/100000000000 
    ' Print #1,"old_time_on ";old_time_on        
     tf = pasoCol(i1).tiempoFigura  /d11
     Print #1, "AFI 12:retardo tf ";tf
     ' el time tomo del inicio de aca del off porque ya gaste duracon de la 
     ' ligarura en el paso anterior
     If TF > 0 Then 
     ' la nota ligada es simple y ataca luego a una nota de un acorde
     ' se parte la duracion en un pedazo luego de la ligada en noteon
     ' y otro pedazo de la duracion o reardo luego de la resolucion 
       If pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura And pasoCol(i1).tiempoFigura > 0 Then

          duracion old_time_off , tf
'       Else
 ' la nota ligada es de un acorde y atca a otro acore se usa todo el retardo aca 
 '' 
  '        duracion pasoCol(i1).old_time /d11, tf   
       EndIf
       If CANCIONCARGADA Then
          Print #1,"AFI 13: ligado NOTEOFF==> i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" "; _
          figura(pasoCol(i1).DUR)
             
            noteoff pasoCol(i1).notapiano ,canal
          'liga cero no
       '  EndIf
       Else
          Print #1,"AFI 14: ligado DOY OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
          figura(PasoCol(i1).dur)
             
           noteoff pasoCol(i1).notapiano ,canal
         
       EndIf
       pasoCol(i1).tiempoFiguraOld=0
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
       'pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       'pasoCol(i1).i1old=0
     EndIf
       
  EndIf 

  ' le resto 1 a la liga para decir que paso este paso y en el proximo
  ' si llega a cero envia el noteoff entonces
'  pasoCol(i1).liga = pasoCol(i1).liga - 1 ' 05-11-2021
'  If pasoCol(i1).liga < 0 Then
'     pasoCol(i1).liga=0
'  EndIf
'  Print #1,"le resto 1 a  pasoCol(i1).liga ", pasoCol(i1).liga
 '05-11-2021 comentado' ligaglobal = pasoCol(i1).liga ' por si sigue una simple

Next i1
Print #1,"AFI 15: fin ligados off"

Print #1,"AFI 16: start ligados continuacion en este paso " 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
  If pasoCol(i1).liga > 0 And pasoCol(i1).ligaOld >0 Then
     Print #1,"AFI 17:HAY LIGADOS que llegan y siguen!"
     Print #1,"AFI 18:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   'old_time= pasoCol(i1).old_time/100000000000 
    ' Print #1,"old_time_on ";old_time_on        
     pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
     Print #1,"AFI 19:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=pasoCol(i1).old_timeold
  EndIf 
  
Next i1
' limpiando ligaduras 05-11-2021 por cancion	,
' ligalobal dice si al menos una nota esta ligada
'Print #1,"AcordeOFFIguales: liga:", ligaglobal
'limpiarLigaduras(cnt,pasoCol())
'Print #1,"limpiado AcordeOFFIguales: liga:", ligaglobal

Print #1,"AFI 20:FIN AcordeOffIguales o simple"
End sub

Sub AcordeIguales (pasoCol() As vec, cnt As UByte,cntold As UByte, vel as UByte, canal As UByte,tiempoDur As Double,Roll As inst,velpos As Integer,pis As integer) 
' todas las notas son de igual duracion, cnt cantidad de notas
'Print #1,"call acordeon iguales"
AcordeOnIguales	 pasoCol() , cnt , cntold , vel,canal,tiempoDur, Roll,velpos,pis
AcordeOffIguales	 pasoCol(), cnt , cntold , canal,Roll,pis
' start  jmg 09-06-2021

''limpiarLigaduras (cnt,pasoCol())


End Sub

Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte,canal As UByte,tiempoDUR As Double,pis As integer)
' si hay en cadena varios acordes y notas simples ligados
' el 1er acorde da el old_time_on, luego se suma toda su duracion
' a treves de la liga compelta...ese valor va el tiempoFiguraOld
' no cambia a tra ves de la ejecucion de los pasos de la liga
' en el paso final el retado es el total de la liga respecto
' del old_time_on del 1er acorde,,,asoi funciona el rtmidi...
'debug..
Print #1,"--------------------------------------------"
Print #1,"AFD :]start AcordeOffDistintos cnt,cntold,pis ",cnt,cntold,pis
Print #1,"-------------------------------------"
 veoPasoCol(pasoCol(),CInt(cnt), CInt(cntold))


Dim  As UByte i1, coff
'print #1,"====>>> START AOFF OFF veo el pasocol que tiene"
If cntold > cnt Then
  coff=cntold
Else
  coff=cnt  
EndIf
For i1=1 To coff 'reemplazo CNT 20-06-2021 JMG
 ' print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
 ' " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time    
Next i1

Print #1,"SORT POR tiempoFigura calculado en playAll" 
For i1=1 To coff
  print #1,"AOFFD:antes Sort Fig, DUR notapiano ";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura;" ";pasoCol(i1).liga 
Next i1
   qsort(@pasoCol(1).tiempoFigura, cnt, SizeOf(vec), @QCompare )
For i1=1 To coff
  print #1,"AOFFD:deespues sort DUR notapiano fig";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura;" ";pasoCol(i1).liga 
Next i1   
'-----------------------------------------
Print #1,"====>>> LUEGO SORT AOFF OFF veo el pasocol que tiene"
For i1=1 To coff 'CNT 20-06-2021 JMG
  print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
  " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time    
Next i1

' ---------------------------------------
Dim As Double tiempoFigura
'Print #1,"-------------------------------------"
'Print #1,"AOFFD:start AcordeOffDistintos"


 old_time_off=Timer ' para notas no ligadas
 Print #1,"AOFFD:old_time off no ligadas inicial ";old_time_off

Print #1,"FOR: AOFFD:no ligados calculo tiempo Figura y off:" 
Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
Print #1,"FOR 1:AOFFD:cnt "; cnt; "i1 "; I1; " pasoCol(i1).liga "; pasoCol(i1).liga;" notapiano ";pasoCol(i1).notapiano 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/100000000000
     Print #1,"FOR 2:AOFFD:i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time_off, tiempoFigura
     Print #1, "FOR 3:AOFFD:SIN LIGAR OFF==>"; 
     Print #1,"FOR 4:AOFFD:i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     noteoff pasoCol(i1).notapiano ,canal
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * 100000000000
Print #1,"5) AOFFD:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
Print #1,"6) AOFFD:start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
   Print #1,"FOR 7:AOFFD:HAY LIGADOS!"
   Print #1,"FOR 8:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   Print #1,"FOR 9:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
         Print #1,"FOR 10:AOFFD: ligado no se envia off,old  es mayor a la mayor de no ligado "
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
        Print #1,"FOR 11:AOFFD:i1 ";i1;" tiempoFigura q falta para el off de ligado ";
        Print #1,pasoCol(i1).tiempoFigura
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
        Print #1,"FOR 12:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld
     Else 
         old_time_on= pasoCol(i1).old_time/100000000000 '20-06-2021 habilitado
         Print #1,"FOR 13:AOFFD:old_time_on ";old_time_on        
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /100000000000
         Print #1,"FOR 14:AOFFD:pasoCol(i1).tiempoFigura ";pasoCol(i1).tiempoFigura
         Print #1,"FOR 15:AOFFD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
         Print #1, "FOR 16:AOFFD:retardo tf ";tf
         If TF > 0 Then ' usamos el old_time-on que venia de antes
           duracion old_time_on, tf
           If CANCIONCARGADA Then
             If Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
              Print #1,"FOR 17:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
                   figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
                noteoff pasoCol(i1).notapiano ,canal
             EndIf

           Else
             If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
Print #1,"FOR 18:pasoCol(i1).i1 , jply+1 ", pasoCol(i1).i1 , jply+1
Print #1,"FOR 18+:figura(pasoCol(i1).DUR)",figura(pasoCol(i1).DUR)

Print #1,"FOR 19:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" " ;  figura(pasoCol(i1).DUR)
                noteoff pasoCol(i1).notapiano ,canal
             EndIf
           EndIf  
pasoCol(i1).tiempoFigura =0  '01-07-2021
pasoCol(i1).tiempoFiguraOld=0 '01-07-2021
pasoCol(i1).liga=0
           
         Else
           If (pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld) And pasoCol(i1).liga = 1  Then
              tf=tiempoFigMayorNoligado/100000000000 
               Print #1, "FOR 20:AOFFD:retardo recuperado en condicion (=) ..tf= ";tf
               Print #1,"FOR 21:pasoCol(i1).notapiano:";pasoCol(i1).notapiano
              duracion old_time_on, tf
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFiguraOld   '- tiempoFigMayorNoligado  
              Print #1,"FOR 22:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld 
 '    comentado  noteoff pasoCol(i1).notapiano ,canal ' 21-06-2021
           Else

              Print #1,"FOR 23:AOFFD:NO SE ENVIA OFF TF=0"
              pasoCol(i1).liga=1 '13-06-2021 PARA QU ESIGA EN EL OTRO PASO SI NO SE BORRA....
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ' 13
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld '- tiempoFigMayorNoligado
              Print #1,"FOR 24:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld ' 13
           EndIf  
         EndIf 
         Print #1,"FOR 25:pasoCol(i1).liga ", pasoCol(i1).liga
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
         Print #1,"FOR 26:<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"
     EndIf
     ''pasoCol(i1).liga=0 ' ya la use se va 05-11-2021   
  EndIf 

Next i1
''pasoCol(i1).tiempoFiguraOld=0 '13-06-2021

 ' observacion una ligadura de varios acordes en una nota dada podria
 ' terminar o tener una nota simple unica como final o intermedia ligada
 ' ver que pasa en ese caso como lo solucionamos
 ' 01-07-2021 ACA NUNCA SE BLAQUEA no-> ligaglobal=0  porque corta las ligas largas en un acorde distinto 
Print #1,"FIN AcordeOffDistintos  " 

Print #1,"AcordeOFFDistintos: liga:", ligaglobal
limpiarLigaduras(cnt,pasoCol())
Print #1,"limpiado AcordeOFFDistintos: liga:", ligaglobal

End Sub



Sub AcordeDistintos (pasoCol() As vec, cnt As UByte, cntold As UByte,vel As UByte,canal As UByte,tiempoDur As Double,Roll As inst,velpos As Integer, pis As integer ) 
' Hay notas de sitinta duracion, cnt cantidad de notas
Dim i1 As UByte
AcordeOnDistintos  pasoCol(), cnt , cntold ,vel  , canal,tiempoDur,Roll,velpos,pis
AcordeOffDistintos pasoCol(), cnt , cntold ,canal,tiempoDur,pis
limpiarLigaduras (cnt,pasoCol())

End Sub
'-------------playAll-----21-05-2021-------
Sub playAll(Roll As inst) ' play version 2
' tiempo es cuantas negras en un minuto tiempoPAtron
' PLAY masavanzado en un mismo acorde si son de distinta duracion
' sus notas se toca cada una con su propia duracion,el corde no termina
' hasta queterminede tocar la nota mas larga.
 
fueradefoco=1

Dim As Double tiempoDUR, tiempoFigura=0,tiempoFiguraOld=0,old_time_old=0
tiempoDUR=60/tiempoPatron '60 seg/ cuantas negras enun minuto
Dim nombre As ZString ptr
Dim As Integer i1,i2,i3,i4,i5,j ,comienzoDeLoop=0
Dim As Integer comienzo=1, final=MaxPos,  canal=0,vel=100,velpos =0
' canal 0 es el 1 van de 0 a 15

Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim result As Integer

Print #1,"comienzo playaLL ==========> tiempoPatron =",tiempoPatron
Print #1,"playAll         ==========> tiempoDur= 60/tiempoPatron =", tiempoDur
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

' If jply=1 And Roll.trk(1,NA).inst > 0 Then
'   ChangeProgram ( Roll.trk(1,NA).inst , 0)
'    Print #1,"ChangeProgram jply", Roll.trk(1,NA).inst
  ''        On Error GoTo labelerror 
' End If 


For jply=comienzo To final

' If Roll.trk(1,NA).inst > 0 And jply=1 Then
'       ChangeProgram ( Roll.trk(1,NA).inst , 0)
' EndIf

kNroCol= Int(jply/NroCol)
If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < MaxPos)Then
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
   
' ojo con silencios ligados !!!
  cnt=0
  iguales=0
  distintos=0
  duraold=0 ' 04-11-2021 jmg
  Print #1," ---------------000000000000000000000-----------------"
  Print #1," (((PALL 0:)))---START--PASO:[";jply;"] ----------------"
  Print #1," ---------------000000000000000000000-----------------"
  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
  
  ' ============================== 18-08-2021 funciono!!!!
  ' anda mejor qu ela V4 solo que la ligadura I+I+I la toca como I+I
  ' LE FALTA UNA NEGRA DE DURACION, EL RESTO LO TOCA BIEN!!!
  ' SIN TOCAR CASI NADA SOLO ELIMINAR EL ANALISIS DE LIGA EN PLAYALL
  ' SEGUIR CORREGIR CON EL USO DE LSO CAMPOS NUEVOS Y AL TERMINAR 
  ' ELIMINAR LOS CAMPOS DE VEC QUE NO SE USEN
  For i1=NB To NA  
   
   If (Roll.trk(jply, i1).nota >= 1) And Roll.trk(jply, i1).nota <= 12 _
      And Roll.trk(jply, i1).dur >=1 And Roll.trk(jply, i1).dur <= 180 Then ' es semitono
     ' por mas que achique en octavas, Notapiano se calcula respecto del nro 
     ' completo de octavas del piano ergo 115 es fijo siempre mientras
        
      Notapiano= i1
      'Notapiano= 115 - i1 
      Notapiano= Notapiano - restar (Notapiano)
      Print #1,"PALL 0:VEO LO CORRECTO DE NOTAPIANO "; Notapiano
      dura=CInt(Roll.trk(jply, i1).dur) '1) I 2) I dur x 1 to 108
      Print #1,"PALL 1:jply ";jply; "dura ";dura
      cnt=cnt+1
      Print #1,"PALL 2:paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
      If cnt=1 Then 
         duraOld=dura
      EndIf
      ' 04-11-2021 usamos reldur para comparar duraciones !!!
      If reldur(duraOld)=reldur(dura)  And cnt > 1 Then
         iguales=1
         Print #1,"PALL 4:cnt ";cnt;" iguales ";iguales
      EndIf
      If reldur(duraOld)<>reldur(dura)  And cnt > 1 Then
         distintos=1 ' atrapa no importa cuantos elementos tenga el acorde
         Print #1,"PALL 5:cnt ";cnt;" distintos ";distintos
      EndIf         

     
      '+++++++++
  
        Print #1,"PALL 7:-> cnt"; cnt 
        pasoCol(cnt).DUR    =dura
 '       pasoCol(cnt).DURold =dura
        Print #1,"PALL 8:pasoCol(cnt).DUR ", pasoCol(cnt).DUR 
        If pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <=180 Then
            print #1,"PALL 9:PALL 0: nota con + es una liga"
            pasoCol(cnt).liga =  1
        Else
            pasoCol(cnt).liga =  0   
        EndIf
        pasoCol(cnt).notapiano = Notapiano 
        print #1,"PALL 10:Notapiano ",Notapiano
       
        pasoCol(cnt).tiempoFigura    = relDur(pasoCol(cnt).DUR) * tiempoDur * d11
    
        pasoCol(cnt).i1    = i1 'posicion vertical en el vector real
  '      pasoCol(cnt).i1old = i1 'posicion vertical en el vector real
        print #1,"PALL 11: posicion vertical en el vector real ",i1
        print #1,"PALL 12:pasoCol(cnt).tiempoFigura ",pasoCol(cnt).tiempoFigura
      ' 20-06-2021 eliminado duraold=dura repetido    
        '' vel= vol( dura, velpos) 02-11-2021
' llegamos al final de la Columna
   
   EndIf   
  
      If i1=NA  Then 'And cnt >= 1 Then ' envio noteoff 1) no entra

         If cnt > 1 Then' Acorde
           Print #1,"i1=NA=";i1 ; " ACORDE cnt= ";cnt
         Else    
           Print #1,"i1=NA=";i1 ; " SIMPLE cnt= ";cnt
         EndIf  

         Select Case cnt
          Case 1 
 Print #1, "PALL 24:call Notesimple cntold, vel, canal, tiempodur",  cntold, vel, canal,tiempoDur
 ' 04-11-2021 cnt por cntold ....aca|
          TipoAcorde=1 ' simple   
          AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll,velpos,0
          pasoCol(cnt).notapianoOld    = Notapiano             
          Case Is > 1
       print #1,"case is > 1
            If iguales=1 And distintos=0  Then
                TipoAcorde=2 ' iguales
                Print #1,"cnt ";cnt;" call Acordeiguales "
                AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll,velpos,0
                
            EndIf
            If  distintos=1 Then
               TipoAcorde=3 ' distintos
               Print #1,"cnt ";cnt;" call AcordeDistintos"
                 AcordeDistintos pasoCol(),cnt, cntold,vel,canal,tiempoDur,Roll,velpos,0
                
            EndIf
            
         End Select  

        cntold = cnt
 Print #1,"cantidad de elementos Acorde actual y anterior cnt,cntold"; cnt;" ";cntold
        
        
   EndIf  
  Next i1
  Print #1,"---FIN -----paso:"; jply;" --------------------------------" 
  
 mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
 If playloop=1 And jply= final Then
    jply=comienzo -1
    'posicion=comienzo
 EndIf
 tiempoDUR=60/tiempoPatron '13-07-2021 cambiamos velocidad durante el play!!!
 
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
' ================================FIN PLAYALL <<=================
End Sub 

' ---------------
'' Comparison function for qsort
Function QCompare Cdecl (Byval e1 As Any Ptr, _
                         Byval e2 As Any Ptr) As Integer
        Dim As Integer el1, el2
        Static cnt As Integer
        
        'Get the call count and items passed
        cnt += 1
        'Get the values, must cast to integer ptr
        el1 = *(Cptr(Integer Ptr, e1))
        el2 = *(Cptr(Integer Ptr, e2))
       ' Print #1,"Qsort called";cnt;" time(s) with";el1;" and";el2;"."
        'Compare the values
        If el1 < el2 Then
           return( -1 )
        Elseif el1 > el2 Then
           Return( 1 )
        Else
         return( 0 )
        End If
End Function


' ----------------------
 Sub PlayRoll ( ) ' 1er play version 0 
' tiempo es cuantas negras en un minuto tiempoPAtron
' los acrodes se tocan con la duracion de la nota mas larga que lo compone 
Dim As Double tiempoDUR, tiempoFigura=0
tiempoDUR=60/tiempoPatron '60 seg/ cuantas negras enun minuto

'''midiin  = rtmidi_in_create_default()
'midiout = rtmidi_out_create_default()


'''portsin  =  port_count (midiin)
'portsout =  port_count (midiout)
'Print #1, "portsin  "; portsin
'Print #1, "portsout "; portsout
'Dim nombrez As ZString Ptr ' nombre local

'Print #1,""
'Print #1, "Output port"

'Dim i As INTeger
'for i = 0 to portsout -1 
'    nombrez = port_name(midiout, i)
'    Print #1, *nombre
'Next   
'Print #1, ""
'Print #1, "Input port "

'For i = 0 to  portsin -1  
'    nombre = port_name(midiin, i)
'    print #1, *nombre
'Next

Dim leng As UInteger <8>
Dim result As Integer

'portsout = portout
'*nombrez = ""

'open_port (midiout,portsout, nombrez)

'Sleep 50
'nota, velocidad,canal 
' nR=semitono + (*po) * 13

'noteOn 64,87,1
'leng = 3
'Sleep 1500
''nota, canal
'noteoff  64,1
' 1er intento: dado queel procesamientoesmuy veloz,se supone, tocar
'notas en acorde seria lo mismo que tocarlos secuencialmente uno tras otro
' solo los retardos intercalados diferenciaran acordes de notas simples
' luego para tocar copiaremos todo a un bufer o array secuencial,cual
'es la nota mas veloz? una W con negra=160 por ejemplo..
'Negra160= 60/160 segundos=6/16=3/8=0,375 seg 
'                            O  = 1500      mseg.
'                            P  = 750       mseg
'                            I  = 375       mseg.
' la corchea L sera la mitad L  = 187       mseg
' semicorchea F              F  =  93,75    mseg
' fusa                       E  =  46,875   mseg
' semifusa                   H  =  23,4375  mseg
'                            W  =  11,71875 mseg
' ergo si toco 4 5 6 notas a la vez secuecnialmente talvezeltimepo
' enejecutarlo es muy rapido y pareceriaun acorde ¿? ok o no¿?
' buenoenrealidad laspongojuntas en el buffer y luego lasseparo en
' el menssage para send_message....peroel tiempo q tardo enponerlo es
' masqu eenviarlo diretamente
Dim As Integer final=MaxPos  , comienzo=1, notapiano, canal=1,vel=100,j
Dim As Integer dura=0, maxdur=0,con=0,tiempo,ioff,cx=0,durb=0
Dim As Integer non(1 To 180), liga=0,x=0, durval (1 To 45), silencio, fin, inicio
Dim As Integer durl
' la velocidad por ahor l ponemos fija = 100, el canaltmbien 1
   '   noteon 64, vel, canal
   '   Sleep 1500
   '   noteoff 64,canal
'nR=(13-semitono) + (*po) * 13    
' mientras j no cambie se acumula el acorde
' se va enviando elon a cada nota a medida que se la recorre
' se toma su duracion para enviarle luego el off a cada una ellos 
' envian menso off ver esa tecnica...yo enviare todos por ahora
' el timer que se dejara transcurrir para envier el off
' dependera de la duracion de cada nota,,,
Dim As Integer jcompas = 0, velpos =0
Dim As Double tinicio   

'Print #1,"comienzo play ==========> "
' FUTURO: JMG EN CALCOMPAS EN EL VECTOR Compas debere marcar 
' en el con numeros lso tiempos feurtes semifertes y debiles
' ej:partodo el tiempo de negra=1 en 4/4 todas esas figuras son fuertes
' valor en compas(n)= -1
' en negra=2 todas las iguras q lo componenen son debiles compas(n)= -2
' en negra=3 todas son semifuertes compas(n)=-3
' en negra=4 todas son debiles. -2
'luego segun el valor de compas voy cambiando
' si compas(n) = posn es debil -2
'Shell "sendmidi.exe ch 1"
 'cursorVert = 1
 'cursorHori = 1
 'agregarNota=0
 'menuMouse = 0
 'comedit=TRUE
'Dim As tEvent Ptr newEvent
jply=0:curpos=0
mousex=0

For jply=comienzo To final
Print #1,"-----------------------------------------"
 If curpos > NroCol  Then
    curpos=0
    posishow=0
 EndIf

 mousex=jply
 If CONTROL1 = 1 Then
  ' allSoundoff( 1 )
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
' ojo con silencios ligados !!!
 
  For i=NA To NB Step -1 
    
   If (Roll.trk(jply,i).nota >= 1) And Roll.trk(jply,i).nota <= 12 _
      And Roll.trk(jply,i).dur >=1 And Roll.trk(jply,i ).dur <= 180 Then ' es semitono 
      Notapiano= NA - i 
      Notapiano= Notapiano - restar (Notapiano)
      dura=Roll.trk(jply, i).dur '1) I 2) I dur x 1 to 108



      If durb > 0 Then ' 1 to 108
'      Print #1,"durb> 0, i, j ";durb,i,j
         durl=relDur(durb)+relDur(dura)  '2) P
' si durb ya era silencio su continuacion sera silencio tambien solo
' hace falta analizar la 1era parte para saber si suena o en que grupo caera
' la suma nunca caera en otro grupo silencio nosera, y '+' tampoco si era
' el1er grupo por ejemplo 1 a 27       

    '     Select Case durb
    '        Case  1 To  45 
    '       '  silencio=0
    '         inicio=1:fin=45
    '        Case  91 To 135
    '       '  silencio=0
    '         inicio=91:fin=135
    '        Case  37 To  90 
    '       '  silencio=1
    '         inicio=37:fin=90
    '        Case  136 To 180 
    '       '  silencio=1
    '         inicio=136:fin=180
'
 '        End Select          
         For x= 1 To 45  ' el resto de durciones se repiten   
           If durl=reldur(x) Then
              dura=x ' válido hasta duraciones de 7 negras relativas
              exit For
           EndIf 
         Next x
     ' Print #1,"dura + durb "; dura   
         liga=1
         durb=0
         durl=0
      EndIf   
      If dura >= 91 And dura <=180 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
        ' Print #1,"entro nota ligada "; dura, figura(dura)
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
      '  Print #1, jply; " con=0 atrapa dura maxdura ";dura, maxdur
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc...
' SACAR ESTO TOCAR ACORDE CADA ELEMENTO CON SU DURACION        
      If relDur(dura) > relDur(maxdur) Then ' esto lo debo sacar y tocar todas las notas con su duracion
         maxdur= dura ' 1) I, 2) P cuantoms chica dur es mas grnde en relidd
     ' Print #1,jply;"if dura-figura "; dura, figura(dura)
     ' Print #1,jply;"if cambio Maxdur-figura "; Maxdur, figura(Maxdur)
      Else    
        '' notacur=i
     ' Print #1,jply;"else dura-figura "; dura, figura(dura)
     ' Print #1,jply;"sigue igual else Maxdur-figura "; Maxdur, figura(Maxdur)

      EndIf 
      If liga=0 Then  
       ' Print #1,"liga=0 "
        If (maxdur >=46 And maxdur <= 90 ) Or (maxdur >=136 And maxdur <= 180 ) Then
          vel =0
        Else
          vel= velpos
        EndIf
 ' SI ELUSUARIO GRABA VELOCIDADES DEBO USARESA NO LA DEFAULT !!! JMG
 ' Y NO SACRLA MAAX DURACION TOCAR TODAS CON SU DURCION Y VELOCIDAD!!!       
        
        noteon notapiano, vel, canal ' 1) G
 
        cx = cx + 1   ' 1) 1
        non (cx) = notapiano '1) G
       ' Print #1, "ON==>  notapiano, vel, canal ";notapiano, vel, canal
       ' Print #1,"cx ";cx 
      ''''''  Sleep 1,1
        old_time_on=Timer
      Else
       ' Print #1,"liga=1 no se envia noteon " 
        liga=0 
      EndIf 
   EndIf

   If i=NB And durb = 0 Then ' envio noteoff 1) no entra
 ''Sleep segun duracion o Timer de la q mas dura o para cada uno
      ' tiempoPatron input al redimsub
   ' Print #1,"i=NB=";i," maxdur=";maxdur;  
    If maxdur > 0 And maxdur <= 182 Then
      ' Print #1, figura(maxdur)
    Else 
      ' Print #1, "No se puede mostrar"  
    EndIf   
    
  ''''   duracion (maxdur)
'''' DURACION  

 If maxdur >= 1 And maxdur<= 180 Then 
    tiempoFigura = relDur(maxdur)*tiempoDUR
 
 '  Print #1, "tiempoFigura ";tiempoFigura  
   Do

 ' Sleep 1,1
 ' sleep5dm()
' -------------sleep5fm 
    Dim As Double start,final
    start=Timer
    Do
'      If (Timer-start) > 0.0001 Then ' 0.1 MILESIMA DE PRESICION DE DURACION
'        Exit Do
'      EndIf
       Sleep 2  
    Loop

   Loop Until (Timer - old_time_on) >= tiempoFigura
 EndIf 
' ---------------     
' FIN DURACION 
' ACA ODRIA ORDENAR LAS DURACIONE DE MENORA MAYOR CALCULARLSO INCREMENTOS
' DAR EL OFF SECUENCIALMENTE SEPARADOS POR DURACIONES INCREMENTALES IGAUL
'A A LDIFERENCIA DE TAMALÑO EJ SI ELACRODE ES DE NEGRAY BLNCA, 1ERO
' SE EJEUCTA LA DURCION DE 1 NEGRA SE DA EL OFF DELA MISMA,LUEGO
' PARA LA BLANCA SE RESTA UNA NEGRA MAS, Q ES LA DIFERENCIA CON LA ANTERIOR
' SE EJECUTA UNA DURCION DE NEGRA ADICIONAL Y SE ENVIA ELOFF DE LA BLANCA     
   '  Print #1," cantidad cx de off ";cx
     
     For ioff=1 To cx
     noteoff non(ioff),canal

    ' Print #1, "OFF==>   non(ioff),  canal "; non(ioff),canal
     Next ioff
 '    Print #1,"pasó for de off .."
 '    Print #1," ==============> fin paso...j"; j   
   EndIf 
  Next i

'  Print #1,"COMIENZA OTRA  POSICION O J ======"; j
  If durb=0 Then
   cx=0
  EndIf
  con=0 
  maxdur=0 '13-05-2021 16:08
'https://www.freebasic.net/forum/viewtopic.php?t=19174  
  'mouse_event MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0
  mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
Next jply
jply=0:curpos=0
Sleep 1,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
close_port(midiout)
out_free(midiout) 
play=0 
playb=0
mousey=100 'otra mas para evitar rentrar a play en menu
finplay=1
'if finplay=1 Then
 ThreadDetach(thread1)
'  finplay=0
'   Endif   

' velocidades a incorporar
'pppp   8
'ppp   20
'pp    31
' p    42
'mp    53
'mf    64
' f    80
'ff    96
'fff  112
'ffff 127
'http://www.music-software-development.com/music-sdk.html
'http://www.ccarh.org/courses/253/handout/midiprotocol/
' KARAOKEUSA WIN API https://www.freebasic.net/forum/viewtopic.php?t=25312
'For it to run equally on 32/64 bit's FBC, you'll probably have to:
'- replace all *Integer* with *Long* as a first step (fixed 32bit INT-Type) since,
'*Integer* on 64bit FBC, becomes a 64bit variable, which isn't wanted.
' LIBRARY MIDI DLL
'https://freebasic.net/forum/viewtopic.php?f=14&t=26725
End sub
Function restar (notaRoll As Integer) As Integer
' sale Notapiano 
Select Case notaroll
   Case 0 To 11
     restar=0
   Case 13 To 24
     restar=1
   Case 26 To 37
     restar= 2
   Case 39 To 50
     restar= 3
   Case 52 To 63
     restar =4
   Case 65 To 76
     restar= 5
   Case 78 To 89
     restar=6
   Case 91 To 102
     restar= 7
   Case 104 To 115
     restar=8         
   Case 117 To 128
     restar=9         
       
End Select

End Function
Function SumarnR (notaPiano As Integer) As Integer
' dado la NotaPiano encuentro nR el indice del Vector Roll de visualizacion
Select Case notaPiano
   Case 0 To 11
     SumarnR=0
   Case 12 To 23
     SumarnR=1
   Case 24 To 35
     SumarnR= 2
   Case 36 To 47
     SumarnR = 3
   Case 48 To 59
     SumarnR =4
   Case 60 To 71
     SumarnR= 5
   Case 72 To 83
     SumarnR=6
   Case 84 To 95
     SumarnR= 7
   Case 96 To 107
     SumarnR=8         
   Case 108 To 119
     SumarnR=9         
   Case 120 To 131
     SumarnR=10         
       
End Select

End Function


Function sumar( ByVal ind As integer) As Integer 
Dim res As Integer 'campiado segun nuevo algoritmo de octavas,,,
ind=ind +1 
res= ind Mod 13
 If res = 0 Then
    sumar=1 
 Else
    sumar=0   
 EndIf

End Function 

Sub duracion (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off dejo de andar porque ???
Print #1,"En Duracion COMIENZA RETARDO En  time :"; old_time
Print #1, "tiempoFigura " , tiempoFigura
Dim As Double start
dim as LARGE_INTEGER delay 
'la funcion nativa resuelve en unidades de 100 nanosegundos!
'-50000000 '5 seconds
'-1000 ' 0,0001 seconds
'si son unidade sde 100 nanosegundos
'entonces 100 unidades de nsec  son= 10^-9 *10^ 2 = 10^-7 =0,0000001
' SON 0,1, LA DECIMA PARTE DE UN MICROSEGUNDO
' O SEA 5 SEGUNDOS SON CUANTOS 0,1 MICROSEGUNDOS? (10^-7)
' 5 * 10^7 * 10^-7 = 5
'50000000 = 5*10^7 * 10^-7 =5
' SI EN VEZ DE 5 SEGUNDOS QUIERO 0,1 MSEG,
' 10^-4 * 10^7= 1000  
'1000 = 1*10^3*10^-7=1*10^-4 = 0,0001 = 0,1mseg !!!
'obtenermos una resolucion de 0,1 mseg y sin consumo de CPU!
' EL TIEMPO DE LA FIGURA DEBE VENIR EN SEGUNDOS
delay.QuadPart = -1000 ' =0.1 mili segundos 
 start=Timer
  Do
    'Sleep 1
    NtDelayExecution(FALSE,@delay)
  Loop Until Timer - old_time >= tiempoFigura

End Sub
Print #1,"Fin duracion"

Sub duracionokOLD (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off dejo de andar porque ???
Print #1,"COMIENZA RETARDO En  time :"; old_time
Print #1, "tiempoFigura " , tiempoFigura
Dim As Double start, endtime 
 start=Timer
  Do
    Sleep 1
  Loop Until Timer - old_time >= tiempoFigura

End Sub


Sub listports( )


midiin  = rtmidi_in_create_default()
midiout = rtmidi_out_create_default()

portsin  =  port_count (midiin)
portsout =  port_count (midiout)

ReDim listout(0 To portsout -1)
ReDim listin (0 To portsin  -1)
Dim nombre As ZString ptr

' "Output port"

Dim i As INTeger
for i = 0 to portsout -1 
    nombre = port_name(midiout, i)
    listout(i) =*nombre
Next
for i = 0 to portsin -1 
    nombre = port_name(midiin, i)
    listin(i) =*nombre
Next

End Sub
Sub TrasponerGrupo( cant As Integer, Roll As inst, encancion As Integer)
' ANDA BIEN, ES EQUIVALENT EEMPEIZA EN EL EXTREMO QUE ATACA BAJANDO LA POSICION
' DE LA COPIA ES LO MISMO PEOR INVERTIDO FUNCIONA IGUAL, LO IMPORTANE DEL CAMBIO
' FUE EN LA SUBRUTUNA SUMAR COMO EL VECTOR EMPIEZA DE CERO 0 EL ESPACIO ENTRE
' OCTAVAS NO QUEDA MULTIPLO DE 13 ERGO LE SUMO 1 AHORA,,,ANTES DE AHCER EL MOD 13
Print #1,"ARRANCA TRASPONER GRUPO"
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,b1=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha DOWN
If cant < 0 Then ' DOWN
 comienzo= NA
 final = NB  
 inc= -1
EndIf
If cant > 0 Then 'UP
 comienzo= NB
 final = NA  
 inc=  1
EndIf
Dim As Integer desdet, hastat
   desdet=1   
   hastat= MaxPos   
pasoNota=13 ' es 12 25 38 en el vector real 
For jpt = desdet To hastat  
  For i1= comienzo To final Step inc
     If cant < 0 Then  ' DOWN  
        ind = i1+cant 
        ind = ind + sumar(ind)
     EndIf
     If cant > 0 Then  ' UP  
        ind = i1 + cant 
        ind = ind - sumar(ind)
     EndIf
   
    If ( (Roll.trk(jpt, i1).nota >= 0) And Roll.trk(jpt, i1).nota <= 181 ) _
       OR (Roll.trk(jpt, i1).dur >=0 And Roll.trk(jpt, i1).dur <= 181 ) Then ' es semitono
       
       If ind >= NB And ind <= NA  Then
            If pasoNota = Roll.trk(jpt,i1).nota And (Roll.trk(jpt,ind).nota=0 Or Roll.trk(jpt,ind).nota=181 )  Then
               Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
               Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
               Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
               Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
               Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
               Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst
               If Roll.trk(jpt,ind).nota > 0 And Roll.trk(jpt,ind).nota <= 13  Then
                  Roll.trk(jpt,i1).nota = 181
                  Roll.trk(jpt,i1).dur  = 0
               EndIf 
               Roll.trk(jpt,i1).vol  = 0
               Roll.trk(jpt,i1).pan  = 0
               Roll.trk(jpt,i1).pb   = 0
               Roll.trk(jpt,i1).inst = 0
                              
            Else                
               If Roll.trk(jpt,ind).nota >=1 And Roll.trk(jpt,ind).nota <=12  Then
                   If cant < 0 Then  ' DOWN  
                      ind = ind - cant 
                      ind = ind + sumar(ind)
                   EndIf
                   If cant > 0 Then  ' UP  
                      ind = ind - cant 
                      ind = ind - sumar(ind)
                   EndIf

                  if ind > NA Then
                     ind=NA
                  EndIf
                  If ind < NB Then
                     ind=NB
                  EndIf
                  b1=1
                  Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
                  Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
                  Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
                  Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
                  Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
                  Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst

                  Roll.trk(jpt,i1).nota = 181
                  Roll.trk(jpt,i1).dur  = 0
                  Roll.trk(jpt,i1).vol  = 0
                  Roll.trk(jpt,i1).pan  = 0
                  Roll.trk(jpt,i1).pb   = 0
                  Roll.trk(jpt,i1).inst = 0
               EndIf
            EndIf      
       EndIf
    EndIf
  Next i1
Next jpt
' para trasponer tracks debo grabar lo cual copia a track los cambios
' de ese modo al dar play se escuch also cambios sino solo quedan en Roll
' y el play de cancion no lo registra , solo el play de roll lo registraria
If encancion > 0 Then
  Print #1,"en trasponer grupo graba track traspuesto"
   Dim As Integer ubi1=0,ubi2=0 
   Dim As String no1,no2
   ubi1=InStr(nombre,"[")
   ubi2=InStr(nombre,"]")
   If ubi1 >0 And ubi2 > 0 Then ' es un track que se edito se graba como track
       GrabarRollaTrack(0)
   EndIf
EndIf       



''trasponer=0   
End Sub

Sub trasponerRoll( cant As Integer, Roll As inst, encancion As integer)
'AJSUTADO DE NUEVO 11-09-2021 CON EL NUEVO ALGORITMO DE OCTAVAS
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,b1=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(1) si pulso flecha UP
If cant < 0 Then ' DOWN
 comienzo= NB
 final = NA  
 inc= 1
EndIf
If cant > 0 Then 'UP
 comienzo= NA
 final = NB  
 inc=  -1
EndIf
Dim As Integer desdet, hastat
If pasoZona1 > 0 Then 
   desdet = pasoZona1
Else
   desdet=1   
EndIf   
If pasoZona2 > 0 Then 
   hastat = pasoZona2
Else
   hastat= MaxPos   
EndIf   

For jpt = desdet To hastat  
  For i1= comienzo To final Step inc
     If cant > 0 Then  ' UP  
        ind = i1 + cant 
        ind = ind + sumar(ind)
     EndIf
     If cant < 0 Then  ' DOWN  
        ind = i1 + cant 
        ind = ind - sumar(ind)
     EndIf
   
    If ( (Roll.trk(jpt,i1).nota >= 0) And Roll.trk(jpt,i1).nota <= 181 ) _
       OR (Roll.trk(jpt,i1).dur >=0 And Roll.trk(jpt,i1).dur <= 181 ) Then ' es semitono
       
       If ind >= NB And ind <= NA  Then
          If  pasoNota=0  Then    
             Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
             Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
             Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
             Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
             Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
             Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst
             If Roll.trk(jpt,ind).nota > 0 And Roll.trk(jpt,ind).nota <= 12  Then
                Roll.trk(jpt,i1).nota = 181
                Roll.trk(jpt,i1).dur  = 0
             EndIf 
             Roll.trk(jpt,i1).vol  = 0
             Roll.trk(jpt,i1).pan  = 0
             Roll.trk(jpt,i1).pb   = 0
             Roll.trk(jpt,i1).inst = 0
          Else
            If pasoNota = Roll.trk(jpt,i1).nota And (Roll.trk(jpt,ind).nota=0 Or Roll.trk(jpt,ind).nota=181 )  Then
               Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
               Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
               Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
               Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
               Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
               Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst
               If Roll.trk(jpt,ind).nota > 0 And Roll.trk(jpt,ind).nota <= 12  Then
                  Roll.trk(jpt,i1).nota = 181
                  Roll.trk(jpt,i1).dur  = 0
               EndIf 
               Roll.trk(jpt,i1).vol  = 0
               Roll.trk(jpt,i1).pan  = 0
               Roll.trk(jpt,i1).pb   = 0
               Roll.trk(jpt,i1).inst = 0
                              
            Else                
               If Roll.trk(jpt,ind).nota >=1 And Roll.trk(jpt,ind).nota <=12  Then
                   If cant > 0 Then  ' UP  
                      ind = ind+cant 
                      ind = ind + sumar(ind)
                   EndIf
                   If cant < 0 Then  ' DOWN  
                      ind = ind + cant 
                      ind = ind - sumar(ind)
                   EndIf

                  if ind > NA Then
                     ind=NA
                  EndIf
                  If ind < NB Then
                     ind=NB
                  EndIf
                  b1=1
                  Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
                  Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
                  Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
                  Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
                  Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
                  Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst

                  Roll.trk(jpt,i1).nota = 181
                  Roll.trk(jpt,i1).dur  = 0
                  Roll.trk(jpt,i1).vol  = 0
                  Roll.trk(jpt,i1).pan  = 0
                  Roll.trk(jpt,i1).pb   = 0
                  Roll.trk(jpt,i1).inst = 0
               'Else
                ' b1=0   
               EndIf
            EndIf      
          EndIf    
       EndIf
    EndIf
  Next i1
Next jpt

''trasponer=0   
' para trasponer tracks debo grabar lo cual copia a track los cambios
' de ese modo al dar play se escuch also cambios sino solo quedan en Roll
' y el play de cancion no lo registra , solo el play de roll lo registraria
If encancion > 0 Then
   Dim As Integer ubi1=0,ubi2=0 
   Dim As String no1,no2
   ubi1=InStr(nombre,"[")
   ubi2=InStr(nombre,"]")
   If ubi1 >0 And ubi2 > 0 Then ' es un track que se edito se graba como track
       GrabarRollaTrack(0)
   EndIf
EndIf       

End Sub

 Sub moverZonaRoll(posinueva As Integer, Roll As inst,posivieja As Integer)
' ind es donde hago el click! no lo modifico uso inc
' mueve M + Click la zona a la posicion deseada por el click
' o copia c + click en en la posicion deseada   
Dim As Integer jpt=1, i1=1, comienzo , final, inc=posivieja ,b1=0,cant=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha UP
 comienzo= NB
 final = NA  
' revision hay que mover desde la ultima hasta la actual al reves
' sino perdimos las notas.....13-11-2021 

Dim As Integer desdet, hastat
 

If pasoZona1 > 0 Then 
   desdet = pasoZona1
Else
   desdet=1 
   pasoZona1=1  
EndIf   

If pasoZona2 > 0 Then
   hastat = pasoZona2
Else
   hastat= MaxPos   
   pasoZona2=MaxPos
EndIf


cant = pasoZona2 - pasoZona1 'delta original

' sitio donde se copia o mueve indicePos en main (SC_M o SC_C )+ click 
Dim  As Integer  MaxPosOld=MaxPos
  
Print #1, "MaxPosOld ", MaxPosOld
' si movemos a derecha empezamos copiando a la nueva posicion el final de 
' la secuencia, luego en la nueva posicion -1 copiamos el final -1
' asi desde el final haci aadelante...
' si movemos a izqierda el reves..
' o sea lo que está echo es para mover a izquierda donde la posiion destino
' el click esta a la izquierda de pasozona1  

If posinueva > Maxpos Then ' movemos a izquierda
inc=posinueva
Print #1,"ENTRA POR IZQUIERDA"
  For jpt=desdet To hastat
       
     For  i1= comienzo To final
       Roll.trk(inc,i1).nota = Roll.trk(jpt,i1).nota
       Roll.trk(inc,i1).dur  = Roll.trk(jpt,i1).dur
       Roll.trk(inc,i1).vol  = Roll.trk(jpt,i1).vol
       Roll.trk(inc,i1).pan  = Roll.trk(jpt,i1).pan
       Roll.trk(inc,i1).pb   = Roll.trk(jpt,i1).pb
       Roll.trk(inc,i1).inst = Roll.trk(jpt,i1).inst
   '  Print #1,"i1,ind Roll.trk(i1,ind).nota ",i1, ind, Roll.trk(ind,i1).nota
       If moverZona=1 Then ' borro original
          Roll.trk(jpt,i1).nota = 181
          Roll.trk(jpt,i1).dur  = 0
          Roll.trk(jpt,i1).vol  = 0
          Roll.trk(jpt,i1).pan  = 0
          Roll.trk(jpt,i1).pb   = 0
          Roll.trk(jpt,i1).inst = 0
       EndIf
     Next i1
     inc=inc+1
  Next jpt
  Print #1,"TERMINO copia a izquierda ",posinueva   
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
  Print #1,"inicioind  MAxPosOld ",posinueva , MAxPosOld  
  If posinueva > MAxPosOld Then

  ' Print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
     For jpt=MaxPosOld-1 To posinueva -1 
       For  i1= comienzo To final
          Roll.trk(jpt,i1).nota = 181
          Roll.trk(jpt,i1).dur  = 0
          Roll.trk(jpt,i1).vol  = 0
          Roll.trk(jpt,i1).pan  = 0
          Roll.trk(jpt,i1).pb   = 0
          Roll.trk(jpt,i1).inst = 0
  
       Next i1
     Next jpt

  EndIf
  Print #1,"--> TERMINO la vuelta de ind a la izquierda", posinueva
' aca el maxpos deberia achicarse....
MaxPos=inc +1
'-------------------------------
Else ' if ind posiion nueva > pasozona1 movemos a derecha
'---------------------------------

Print #1,"ENTRA POR DERECHA POSINUEVA < MaxPos"
Print #1,"posinueva ",posinueva
Print #1,"MaxPosold ",MaxPosold
Print #1,"posivIEJa ",posivieja
  If posinueva < MaxPos then
    MaxPos=MaxposOld + posinueva - posivieja
  EndIf
Print #1,"MaxPos ",MaxPos
 
  hastat=Maxpos
  desdet=posivieja+1
  inc=MaxPosOld
Print #1,"hastat ",hastat
Print #1, "desdet=inc ",desdet
Print #1,"UBOUND(ROLL,1)", UBOUND (ROLL.TRK,1)
Print #1,"LBOUND(ROLL,1)", LBound (ROLL.TRK,1)
Print #1,"UBOUND(ROLL,2)", UBOUND (ROLL.TRK,2)
Print #1,"LBOUND(ROLL,2)", LBOUND (ROLL.TRK,2)
Print #1,"inc=posivieja+1 ",inc
    
  For jpt= hastat To desdet Step -1
     For  i1= comienzo To final
       Roll.trk(jpt,i1).nota = Roll.trk(inc,i1).nota
       Roll.trk(jpt,i1).dur  = Roll.trk(inc,i1).dur
       Roll.trk(jpt,i1).vol  = Roll.trk(inc,i1).vol
       Roll.trk(jpt,i1).pan  = Roll.trk(inc,i1).pan
       Roll.trk(jpt,i1).pb   = Roll.trk(inc,i1).pb
       Roll.trk(jpt,i1).inst = Roll.trk(inc,i1).inst
   '  Print #1,"i1,ind Roll.trk(i1,ind).nota ",i1, ind, Roll.trk(ind,i1).nota
       If moverZona=1 Then ' borro original
          Roll.trk(inc,i1).nota = 181
          Roll.trk(inc,i1).dur  = 0
          Roll.trk(inc,i1).vol  = 0
          Roll.trk(inc,i1).pan  = 0
          Roll.trk(inc,i1).pb   = 0
          Roll.trk(inc,i1).inst = 0
       EndIf
     Next i1
     inc=inc-1
     If inc < posivieja +1  Then
        Exit For
     EndIf
  Next jpt
  Print #1,"TERMINO copia a derecha ",posinueva   
'---
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
  Print #1,"inicioind  MAxPosOld ",posinueva , MAxPosOld  
  If posinueva > MAxPosOld Then

  ' Print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
     For jpt=MaxPosOld To posinueva  
       For  i1= comienzo To final
          Roll.trk(jpt,i1).nota = 181
          Roll.trk(jpt,i1).dur  = 0
          Roll.trk(jpt,i1).vol  = 0
          Roll.trk(jpt,i1).pan  = 0
          Roll.trk(jpt,i1).pb   = 0
          Roll.trk(jpt,i1).inst = 0
  
       Next i1
     Next jpt

  EndIf
  Print #1,"--> TERMINO la vuelta de ind a derecha ", posinueva


'-------
EndIf 
If posn < 0 Then posn=0 EndIf

End Sub 

Sub correcciondeNotas(Roll As inst)

Dim As Integer jpt=1, i1=1, comienzo , final,i2
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha UP
Dim As Integer desdet, hastat
If pasoZona1 > 0 Then 
   desdet = pasoZona1
Else
   desdet=1   
EndIf   
If pasoZona2 > 0 Then 
   hastat = pasoZona2
Else
   hastat= MaxPos - 1  
EndIf   
'Print #1,"CORRECION DE NOTAS ***********"
For jpt = desdet To hastat  
  For i1= NB To NA   
   
     If ( (Roll.trk(jpt,i1).nota >= 0) And (Roll.trk(jpt,i1).nota <= 13 ) )  Then ' es semitono
           'Print #1,"Roll.trk(i1,jpt).nota ",Roll.trk(i1,jpt).nota
           'Print #1, "i1",i1
           i2= i1 - restar (i1)
          ' Print #1, "i2",i2
          ' Print #1,"relnRNe (i2) ",relnRNe (i2)
          ' Print #1,"---------------"   
          If  Roll.trk(jpt,i1).nota <> relnRNe (i2) Then 
              Roll.trk(jpt,i1).nota = relnRNe (i2)
          EndIf    
    EndIf
  Next i1
Next jpt



End sub
' 06-09-2021 jmg
Sub crearsecuencia(Track() As sec, posn As Integer, ntk As Integer)
' ON LISTOS, FALTA LOS OFFS
'Track(ntk).trk(posn,1) 1 a 12 va la segunda..
' la melodia se carga todo en (posn,1) el acorde en (posn,x) x=2 a 12
' recibe una instancia de Track no todo el vector
' cuadno se carga el acorde se hara un sort del mismo incluido la 
' nota existente en posicion 1 y se ordenara de menor a mayor de modo que
' siemrpe resultara facil dar ON a todas al mismo tiempo y luego dar off
' de menro a mayor, 1er paso para mantener el play de acordes con notas de 
' distinto tamaño.
' si el usu ingresa acorde en campo OK se pone 1, sino sera una nota ordinaria 
' con 0 (eso se hará en sub cursor...) <HACER>
' acorde irá de 1 a 12 , 1 no hay acorde 12 lo maximo
' si cargo un roll debo calcualr la Notapiano
Dim As Integer I,J,cnt
Dim vecs(1 To 12) As poli
'Print #fs,"ntk --------------------------",ntk,posn
'

For i=1 To 12
'Print #fs, i, Track(ntk).trk(posn,i).nota, Track(ntk).trk(posn,i).dur
vecs(i).dur = CInt  (Track(ntk).trk(posn,i).dur )
vecs(i).nota = Track(ntk).trk(posn,i).nota
vecs(i).vol = Track(ntk).trk(posn,i).vol
vecs(i).pan = Track(ntk).trk(posn,i).pan
vecs(i).pb = Track(ntk).trk(posn,i).pb

Next i
' volumenm cero si dur esta entre 46 a 90 o de 136 a 180

Print #fs,"--------------------------"

    If Track(ntk).trk(posn,1).acorde > 1 Then 
      cnt= Track(ntk).trk(posn,1).acorde
      Print #fs,"acorde ",cnt
      qsort( @vecs(1).dur, cnt, SizeOf(poli), @QCompare )
'      Print #fs,"despues de sort muestra"
      For i=1 To cnt
'        Print #fs, vecs(i).nota,vecs(i).dur
      Next i
'      Print #fs,"fin muestra sort "
    EndIf      
For i=1 To 12
Track(ntk).trk(posn,i).dur  = CUByte(vecs(i).dur)
Track(ntk).trk(posn,i).nota = vecs(i).nota
Track(ntk).trk(posn,i).vol  = vecs(i).vol
Track(ntk).trk(posn,i).pan  = vecs(i).pan
Track(ntk).trk(posn,i).pb   = vecs(i).pb


Next i


' SUPONGO ACORDE DE DISTINTAS DURACIONES
 FOR  I= 1 TO 12 'PTrack.trk(posn,I).acorde
    If Track(ntk).trk(posn,I).nota > 0 And  Track(ntk).trk(posn,I).dur >= 1 And _ 
       Track(ntk).trk(posn,I).dur <= 180 Then    
       If posn = 1 Then 
          Track(ntk).trk(posn,I).tick=0
       Else   
          Track(ntk).trk(posn,I).tick=(Pesodur(Track(ntk).trk(posn,I).dur)/d7)*128 + _
          Track(ntk).trk(posn-1,1).tick
       EndIf      
      ' guardar si es > 0 ON, notapiano,vol,pan,pb,inst y guargar tick=0
       Print #fs,posn;",";"ON,";Track(ntk).trk(posn,I).nota;",";Track(ntk).trk(posn,I).vol; ","; _
           Track(ntk).trk(posn,I).inst;",";Track(ntk).trk(posn,I).tick;","; _
           (Pesodur(Track(ntk).trk(posn,I).dur)/d7)*128;",";Track(ntk).trk(posn,I).dur 


    EndIf 
 Next I
        
       
'luego

End Sub

