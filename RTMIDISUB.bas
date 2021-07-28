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
'todoslos canales NOFUNCIONA
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	 modo = 120
	
 message(1) = modo 
 message(2) = 0
 message(3) = canal
 leng=3
result = send_message (midiout, p, leng)

End Sub


Sub alloff(canal As UByte ) 
' canal 1 NO FUNCIONA 
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' 120 all sound off https://www.tweakheadz.com/midi_controllers.htm
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
Sub noteon	( note As ubyte, vel As UByte, canal As ubyte)
	' canal 1
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	If canal = 1 Then
		 modo = 144
	Else
	  modo = 144 + canal
	EndIf
	
 message(1) = modo 
 message(2) = note
 message(3) = vel
 leng=3
result = send_message (midiout, p, leng)

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
If nohay = cnt Then
  For i1=NB To NA 
    pasoCol(i1).liga=0 
    pasoCol(i1).tiempoFigura=0
    pasoCol(i1).tiempoFiguraOld=0
  Next i1
  ligaglobal=0
End If
' end  jmg 09-06-2021

End Sub
' ----------------------------------
Sub noteSimple	( pasoCol() As vec, cntold As integer,vel As UByte,canal As ubyte,tiempoDur As Double)
	' canal 1
Dim As Double tiempoFigura=0
'con o sin liga, recalcula tiempo dur no usa multiplicado por 10a la once
Dim As Integer i1
Print #1,"START noteSimple"
Print #1,"pasoCol(1).tiempoFiguraOld:";pasoCol(1).tiempoFiguraOld
Print #1,"cntold "; cntold
If cntold > 1 And pasoCol(1).tiempoFiguraOld=0 And jply > 1 Then
   For i1=1 To cntold
      If pasoCol(i1).tiempoFiguraOld > 0 Then
         pasoCol(1).tiempoFiguraOld = pasoCol(i1).tiempoFiguraOld
      EndIf
   Next i1
   
EndIf 
If pasoCol(1).tiempoFiguraOld > 0  And cntold=1 Then
 'Print #1,"NoteSimple old > 0 pasoCol(1).tiempoFiguraOld ";pasoCol(1).tiempoFiguraOld
   pasoCol(1).tiempoFigura= (pasoCol(1).tiempoFigura + pasoCol(1).tiempoFiguraOld)
  ' Print #1,"tempofigura + old" ; pasoCol(1).tiempoFigura /100000000000
  ' Print #1,"ESTOS EN NOTEsIMPLE!"
   pasoCol(1).tiempoFiguraOld =0
EndIf

If pasoCol(1).tiempoFiguraOld > 0  And cntold > 1 Then
 'Print #1,"NoteSimple old > 0 pasoCol(1).tiempoFiguraOld ";pasoCol(1).tiempoFiguraOld
   pasoCol(1).tiempoFigura=  pasoCol(1).tiempoFiguraOld
  ' Print #1,"tempofigura " ; pasoCol(1).tiempoFigura /100000000000
  ' Print #1,"ESTOS EN NOTEsIMPLE!"
   pasoCol(1).tiempoFiguraOld =0
EndIf




Print #1,"noteSimple: tiempoFigura:", pasoCol(1).tiempoFigura/100000000000
Print #1,"noteSimple: notapiano:", pasoCol(1).notapiano;" ";figura(pasoCol(1).DUR)

If pasoCol(1).DUR >= 91 And pasoCol(1).DUR <=180  Then 'liga OFF DESPUES
   pasoCol(1).liga=1
  ' Print #1,"no se envia noteon de 1er nota +, se suma su tiempo a la siguiente"
   pasoCol(1).tiempoFiguraOld=pasoCol(1).tiempoFigura
  ' Print #1,"NoteSimple acum pasoCol(1).tiempoFiguraOld ";pasoCol(1).tiempoFiguraOld
Else
  ' Print #1,"pasoCol(1).liga ", pasoCol(1).liga
    If ligaglobal=0 Then 
      ' Print #1,"envio on "
       old_time_on=Timer
       noteon	 pasoCol(1).notapiano, vel ,canal
       For i1=NB To NA
        pasoCol(i1).liga=0
       Next i1 
    EndIf   
       tiempoFigura=pasoCol(1).tiempoFigura/100000000000
      ' Print #1,"noteSimple OFF tiempoFigura "; tiempoFigura
       duracion old_time_on,tiempoFigura 'RETARDO SIN OFF
       noteoff pasoCol(1).notapiano ,canal  
       pasoCol(1).liga=0
      ' Print #1,"ACA MANDE TODO A CERO PERO CREO AFECTA AL PASO 12 FIJAR"
       ' VAMOS SACANDO UNO A UNO NO AFECTO EN NADA NO VOLVIO LO CORRECTO SIGO VIENDO
       pasoCol(1).tiempoFigura=0 '01-07-2021
       ligaglobal=0 
       pasoCol(1).tiempoFiguraOld=0 ' 01-07-2021  
    ' fin completo?
EndIf

Print #1,"noteSimple: liga:", pasoCol(1).liga
limpiarLigaduras(1,pasoCol())
Print #1,"limpiado noteSimple: liga:", pasoCol(1).liga

End Sub

Function vol (dura As UByte, vel As UByte) As ubyte
 If (dura >=46 And dura <= 90 ) Or (dura >=136 And dura <= 180 ) Then
    vol =0
 Else
    vol= vel
 EndIf

End Function

Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, cntold As UByte,vel As UByte,canal As UByte,tiempoDUR As Double, Roll As inst)

Dim As UByte i1, liga=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
For i1=1 To cnt
'  print #1,"DUR notapiano ";pasoCol(i1).DUR;" ";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR) 
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur

Print #1,"DUR cnt=1:";pasoCol(1).Dur

old_time_on=Timer

For i1=1 To cnt
Print #1,"1)DUR cnt=";i1;":";pasoCol(i1).Dur
' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
' Print #1,"CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
 ' este AND  para iguales no va veremos 
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 Then ''''And pasoCol(i1).liga=0 Then
  '   Print #1,"ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
  '   Print #1,"DUR ";pasoCol(i1).Dur
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=old_time_on * 10000000000
     liga=1
   '  Print #1,"LIGA=1 ==========> ";liga
     Do
       nj=nj+1
       ' busca la proxima dur 
       durj = Roll.trk(nj, pasoCol(i1).i1 ).dur 
   '    Print #1,"LIGA nj, durj "; nj ; " "; durj
       If durj > 0 Then
   '      Print #1,"LIGA nj reldur ";nj; " "; relDur(durj)
       Else 
   '    Print #1,"LIGA nj reldur ";nj; " "; 0 ' revisdar jmgjmg  
       EndIf
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       If durj=0 Then
          tiempoFiguraSig = 0
       Else
          tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
       EndIf
   '    Print #1,"LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
  '     Print #1,"LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply ' importante !!! y en distintos???
 '    Print #1,"2) LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
 '    print #1,"numero de ligados:";pasoCol(i1).liga
 '    Print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
     noteon pasoCol(i1).notapiano,vel,canal
 '    Print #1,"3) LIGA=1 ==========> ";liga
     
  EndIf 
 ' Print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then
  '    Print #1,"4) LIGA=1 ==========> ";liga 
   '   Print #1,"|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
   '   Print #1,"|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
   '   Print #1,"|||SIN LIGA DUR "; pasoCol(i1).Dur
   '   Print #1,"5) LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
   '    Print #1,"|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0  Then 
 '   Print #1,"SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
    noteon pasoCol(i1).notapiano,vel,canal
 End If
Next i1



End Sub 


Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,canal As UByte,tiempoDUR As Double,Roll As inst)

Dim As UByte i1, liga=0,coff=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
If cntold >cnt Then
 coff=cntold
EndIf
print #1,"====>>> START AOD ON veo el pasocol que tiene"
Dim As UByte noAnalizo=0 
For i1=1 To coff ' 20-06-2021 JMG
 ' print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
'  " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time
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

  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 And pasoCol(i1).liga=0 Then
   '  Print #1,"AOD:ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
   '  Print #1,"AOD:DUR ";pasoCol(i1).Dur
   '  Print #1,"AOD:liga ";pasoCol(i1).liga
   '  Print #1,"AOD:notepiano ";pasoCol(i1).notapiano
          
     If pasoCol(i1).liga =0 Then ' 13-06-2021
        pasoCol(i1).liga=1
        pasoCol(i1).old_time=old_time_on * 100000000000
       ' Print #1,"guardo old_time_on primer liga "; pasoCol(i1).old_time
     Else
        pasoCol(i1).liga=pasoCol(i1).liga + 1 ' 13-06-2021
        ' no se carga old_time sigue siendo el mismo
     EndIf
     ligaglobal=1
     liga=1
    ' Print #1,"AOD:LIGA=1 ==========> ";liga
    ' Print #1,"AOD: LIGA ACUMULADA ===>";pasoCol(i1).liga
    ' Print #1,"AOD: ACUMULADO TIEMPOfIGURA ";pasoCol(i1).tiempoFigura
     pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
    ' Print #1,"AOD: ACUMULADO TIEMPOfIGURAOLD ";pasoCol(i1).tiempoFiguraOld
     Do
       nj=nj+1
       ' busca la proxima dur 
       durj = Roll.trk(nj, pasoCol(i1).i1 ).dur 
     '  Print #1,"AOD:LIGA nj, durj "; nj ; " "; durj
     '  Print #1,"AOD:LIGA nj reldur ";nj; " "; relDur(durj)
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
     '  Print #1,"AOD:LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
     '  Print #1,"AOD:LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply '08-06-2021
   '  Print #1,"2) AOD:LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
    ' print #1,"numero de ligados:";pasoCol(i1).liga
    ' Print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
     noteon pasoCol(i1).notapiano,vel,canal
   '  Print #1,"3) AOD:LIGA=1 ==========> ";liga
     
  EndIf
 Else ' ya venia una ligadura ' 13-06-2021
   '  Print #1,"AOD:ya venia con ligadura de antes" ' 13-06-2021
   '  Print #1,"AOD: OJO! pasoCol(i1).old_time ";pasoCol(i1).old_time
   '  Print #1,"AOD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld ' 13-06-2021  
 EndIf 
 ' Print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then
     ' Print #1,"4) AOD:LIGA=1 ==========> ";liga 
     ' Print #1,"AOD:|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
     ' Print #1,"AOD:|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
     ' Print #1,"AOD:|||SIN LIGA DUR "; pasoCol(i1).Dur
     ' Print #1,"5)AOD: LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     '  Print #1,"AOD:|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0 And pasoCol(i1).DUR <> 181 Then 
   ' Print #1,"AOD:SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
    noteon pasoCol(i1).notapiano,vel,canal
 End If
Next i1


	
 
End Sub

Sub AcordeOffIguales	( pasoCol() As vec, cnt As UByte, cntold As UByte,canal As UByte,Roll As inst)
Dim i1 As UByte
Dim tiempoFigura As Double 
'--------
Print #1,"-------------------------------------"
Print #1,"start AcordeOffIguales"


 old_time_off=Timer
' Print #1,"old_time off inicial ";old_time_off

Print #1,"no ligados calculo tiempo Figura y off:" 

For i1 = 1 To cnt ' (1) 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/100000000000
    ' Print #1,"i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time_off, tiempoFigura
    ' Print #1, "SIN LIGAR OFF==>"; 
    ' Print #1,"i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     noteoff pasoCol(i1).notapiano ,canal
  EndIf
Next i1     

Print #1,"start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
    ' Print #1,"HAY LIGADOS!"
    ' Print #1,"pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
    ' Print #1,"pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
   'old_time= pasoCol(i1).old_time/100000000000 
    ' Print #1,"old_time_on ";old_time_on        
     tf = pasoCol(i1).tiempoFiguraOld  /100000000000
    ' Print #1, "retardo tf ";tf
     If TF > 0 Then 
       duracion old_time_on, tf
       If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
          Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
         ' Print #1," ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
   '       figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
             
          noteoff pasoCol(i1).notapiano ,canal
          'liga cero no
       EndIf
    EndIf   
  EndIf 
 ' Print #1,"pasoCol(i1).liga ", pasoCol(i1).liga
  ligaglobal = pasoCol(i1).liga ' por si sigue una simple
 ' Print #1,"<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"

Next i1

'-------
For i1 = 1 To cnt 
	noteoff pasoCol(i1).notapiano, canal
	
next i1	


End sub

Sub AcordeIguales (pasoCol() As vec, cnt As UByte,cntold As UByte, vel as UByte, canal As UByte,tiempoDur As Double,Roll As inst) 
' todas las notas son de igual duracion, cnt cantidad de notas
Print #1,"call acordeon iguales"
AcordeOnIguales	 pasoCol() , cnt , cntold , vel,canal,tiempoDur, Roll
AcordeOffIguales	 pasoCol(), cnt , cntold , canal,Roll
' start  jmg 09-06-2021

limpiarLigaduras (cnt,pasoCol())


End Sub


Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte,canal As UByte,tiempoDUR As Double)
' si hay en cadena varios acordes y notas simples ligados
' el 1er acorde da el old_time_on, luego se suma toda su duracion
' a treves de la liga compelta...ese valor va el tiempoFiguraOld
' no cambia a tra ves de la ejecucion de los pasos de la liga
' en el paso final el retado es el total de la liga respecto
' del old_time_on del 1er acorde,,,asoi funciona el rtmidi...
Dim  As UByte i1, coff
print #1,"====>>> START AOFF OFF veo el pasocol que tiene"
If cntold > cnt Then
  coff=cntold
EndIf
For i1=1 To coff 'reemplazo CNT 20-06-2021 JMG
 ' print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
 ' " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time    
Next i1

'Print #1,"SORT POR tiempoFigura calculado en playAll" 
For i1=1 To coff
 ' print #1,"AOFFD:antes Sort Fig, DUR notapiano ";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura 
Next i1
   qsort(@pasoCol(1).tiempoFigura, cnt, SizeOf(vec), @QCompare )
For i1=1 To coff
  print #1,"AOFFD:deespues sort DUR notapiano fig";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura 
Next i1   
'-----------------------------------------
print #1,"====>>> LUEGO SORT AOFF OFF veo el pasocol que tiene"
For i1=1 To coff 'CNT 20-06-2021 JMG
 ' print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
 ' " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time    
Next i1

' ---------------------------------------
Dim As Double tiempoFigura
Print #1,"-------------------------------------"
Print #1,"AOFFD:start AcordeOffDistintos"


 old_time_off=Timer ' para notas no ligadas
' Print #1,"AOFFD:old_time off no ligadas inicial ";old_time_off

Print #1,"AOFFD:no ligados calculo tiempo Figura y off:" 
Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
Print #1,"AOFFD:cnt "; cnt; "i1 "; I1; " pasoCol(i1).liga "; pasoCol(i1).liga;" notapiano ";pasoCol(i1).notapiano 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/100000000000
   '  Print #1,"AOFFD:i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time_off, tiempoFigura
   '  Print #1, "AOFFD:SIN LIGAR OFF==>"; 
    ' Print #1,"AOFFD:i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     noteoff pasoCol(i1).notapiano ,canal
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * 100000000000
Print #1,"AOFFD:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
Print #1,"AOFFD:start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
 '  Print #1,"AOFFD:HAY LIGADOS!"
 '  Print #1,"pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
 '  Print #1,"tiempoFigMayorNoligado ";tiempoFigMayorNoligado
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
  '       Print #1,"AOFFD: ligado no se envia off,old  es mayor a la mayor de no ligado "
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
  '      Print #1,"AOFFD:i1 ";i1;" tiempoFigura q falta para el off de ligado ";
   '     Print #1,pasoCol(i1).tiempoFigura
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
      '  Print #1,"AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld
     Else 
         old_time_on= pasoCol(i1).old_time/100000000000 '20-06-2021 habilitado
        ' Print #1,"AOFFD:old_time_on ";old_time_on        
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /100000000000
        ' Print #1,"AOFFD:pasoCol(i1).tiempoFigura ";pasoCol(i1).tiempoFigura
        ' Print #1,"AOFFD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
        ' Print #1, "AOFFD:retardo tf ";tf
         If TF > 0 Then ' usamos el old_time-on que venia de antes
           duracion old_time_on, tf
           If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
              Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
           '   Print #1,"AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
           '        figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
              noteoff pasoCol(i1).notapiano ,canal
           EndIf
pasoCol(i1).tiempoFigura =0  '01-07-2021
pasoCol(i1).tiempoFiguraOld=0 '01-07-2021
pasoCol(i1).liga=0
           
         Else
           If (pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld) And pasoCol(i1).liga = 1  Then
              tf=tiempoFigMayorNoligado/100000000000 
              ' Print #1, "AOFFD:retardo recuperado en condicion (=) ..tf= ";tf
              ' Print #1,"pasoCol(i1).notapiano:";pasoCol(i1).notapiano
              duracion old_time_on, tf
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFiguraOld   '- tiempoFigMayorNoligado  
             ' Print #1,"pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld 
 '    comentado  noteoff pasoCol(i1).notapiano ,canal ' 21-06-2021
           Else

             ' Print #1,"AOFFD:NO SE ENVIA OFF TF=0"
              pasoCol(i1).liga=1 '13-06-2021 PARA QU ESIGA EN EL OTRO PASO SI NO SE BORRA....
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura ' 13
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld '- tiempoFigMayorNoligado
             ' Print #1,"AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld ' 13
           EndIf  
         EndIf 
        ' Print #1,"pasoCol(i1).liga ", pasoCol(i1).liga
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
        ' Print #1,"<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"
     EndIf   
  EndIf 

Next i1
''pasoCol(i1).tiempoFiguraOld=0 '13-06-2021

 ' observacion una ligadura de varios acordes en una nota dada podria
 ' terminar o tener una nota simple unica como final o intermedia ligada
 ' ver que pasa en ese caso como lo solucionamos
 ' 01-07-2021 ACA NUNCA SE BLAQUEA no-> ligaglobal=0  porque corta las ligas largas en un acorde distinto 
 
End Sub

Sub AcordeDistintos (pasoCol() As vec, cnt As UByte, cntold As UByte,vel As UByte,canal As UByte,tiempoDur As Double,Roll As inst ) 
' Hay notas de sitinta duracion, cnt cantidad de notas
Dim i1 As UByte
AcordeOnDistintos  pasoCol(), cnt , cntold ,vel  , canal,tiempoDur,Roll
AcordeOffDistintos pasoCol(), cnt , cntold ,canal,tiempoDur


End Sub

'-------------playAll-----21-05-2021-------
Sub playAll(Roll As inst) ' play version 2
' tiempo es cuantas negras en un minuto tiempoPAtron
' PLAY masavanzado en un mismo acorde si son de distinta duracion
' sus notas se toca cada una con su propia duracion,el corde no termina
' hasta queterminede tocar la nota mas larga.



Dim As Double tiempoDUR, tiempoFigura=0,tiempoFiguraOld=0,old_time_old=0
tiempoDUR=60/tiempoPatron '60 seg/ cuantas negras enun minuto
Dim nombre As ZString ptr
Dim As Integer i1,i2,i3,i4,i5,j ,comienzoDeLoop=0
Dim As Integer comienzo=1, final=MaxPos,  canal=1,vel=100,velpos =0
Dim pasoCol (NB To NA) As vec  ' entrada de durciones a medida que barro una columna
Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim result As Integer

midiout = rtmidi_out_create_default()
'Print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout)
'Print #1, "portsin  "; portsin
'Print #1, "portsout "; portsout
For i1 = 0 to portsout -1 
    nombre = port_name(midiout, i1)
 '   Print #1, *nombre
Next i1  

portsout = portout
*nombre = ""

open_port (midiout,portsout, nombre)

Print #1,"  "
Print #1,"comienzo playaLL ==========> "
jply=0:curpos=0
mousex=0
' Print #1,"-----------------------------------------"
comienzo=posicion
cntold=0
If pasoZona1 > 0 Then
 comienzo=pasoZona1
EndIf

If pasoZona2 > 0 Then
 final=pasoZona2
EndIf

For jply=comienzo To final
'posicion=jply

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
 ' Print #1,"---START-----paso:";jply;" --------------------------------"
  '115 a 0
  ' recorre una posicion vertical
  For i1=NA To NB Step -1 
   
   If (Roll.trk(jply, i1).nota >= 1) And Roll.trk(jply, i1).nota <= 12 _
      And Roll.trk(jply, i1).dur >=1 And Roll.trk(jply, i1).dur <= 180 Then ' es semitono 
      Notapiano= 115-i1 
      Notapiano= Notapiano - restar (Notapiano)
      dura=CInt(Roll.trk(jply, i1).dur) '1) I 2) I dur x 1 to 108
     ' Print #1,"jply ";jply; "dura ";dura
      cnt=cnt+1
     ' Print #1,"paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
      If cnt=1 Then 
         duraOld=dura
         
         If ligaglobal=1 Then
            For i2=1 To cntold
                pasoCol(i2).DUR =181
'                pasoCol(i2).liga=0
                pasoCol(i2).tiempoFigura =0
'                pasoCol(i2).old_time=0
            Next i2
         EndIf
      EndIf
      If duraOld=dura  Then
         iguales=1
        ' Print #1,"cnt ";cnt;" iguales ";iguales
      Else
         distintos=1
        ' Print #1,"cnt ";cnt;" distintos ";distintos
      EndIf         

     
      '+++++++++
      If ligaglobal=0 Then 
        ' Print #1,"ligaglobal es cero ****"   
         pasoCol(cnt).DUR =dura
        ' Print #1,"pasoCol(cnt).DUR "; pasoCol(cnt).DUR 
         pasoCol(cnt).notapiano=Notapiano 
         pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * 100000000000
         pasoCol(cnt).i1 = i1 'posicion vertical en el vector real
      ' 20-06-2021 eliminado duraold=dura repetido    
         vel= vol( dura, velpos)
      Else
        ' Print #1,"ligaglobal es uno *****"
         If dura >= 91 And dura <=180 Then
            For i2=1 To cntold
                If Notapiano  = pasoCol(i2).notapiano Then
                  ' Print #1,"Notapiano de liga igual "; Notapiano
                   pasoCol(i2).DUR =dura
                  ' Print #1,"pasoCol(i2).DUR "; pasoCol(cnt).DUR 
                   pasoCol(i2).notapiano=Notapiano 
                ' tiempofigur aya calculado
                  ' Print #1,"tiempofigura anterior ";pasoCol(i2).tiempoFiguraOld
                   pasoCol(i2).tiempoFigura=pasoCol(i2).tiempoFiguraOld       
                  ' Print #1,"pasoCol(i2).i1 ";pasoCol(i2).i1
                  ' Print #1," i1 debe ser igual al anterior:"; i1       
                   pasoCol(i2).i1 = i1 'posicion vertical en el vector real es igual
                   vel= vol( dura, velpos)
               EndIf    
            Next i2
         Else
                   pasoCol(cnt).DUR =dura
                  ' Print #1,"pasoCol(cnt).DUR "; pasoCol(cnt).DUR 
                   pasoCol(cnt).notapiano=Notapiano 
                ' tiempofigur aya calculado
                   pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * 100000000000       
                   pasoCol(cnt).i1 = i1 'posicion vertical en el vector real es igual
                   vel= vol( dura, velpos)
         EndIf
      EndIf
' llegamos al final de la Columna
   EndIf
   
   ' i1=1
   If i1=NB  Then 'And cnt >= 1 Then ' envio noteoff 1) no entra
        ' If ligaglobal=1 Then
        '    If cntold<> cnt Then
        '       
        '    EndIf
        ' EndIf
         
         If cnt > 1 Then' Acorde
          ' Print #1,"i1=NB=";i1 ; " ACORDE cnt= ";cnt
         Else    
          ' Print #1,"i1=NB=";i1 ; " SIMPLE cnt= ";cnt
         EndIf  
         Select Case cnt
          Case 1 
           ' con y sin liga
           ' INCLUYE LIGADOS O NO LIGADOS
            noteSimple  pasoCol(), cntold, vel, canal,tiempoDur
                     
          Case Is > 1

            If iguales=1 And distintos=0  Then
               For i2=1 To cnt
                 pasoCol(i2).tiempoFiguraOld=0
               Next i2

               ' Print #1,"cnt ";cnt;" Acordeiguales "
                AcordeIguales pasoCol(),cnt,cntold,vel,canal,tiempoDur,Roll
                
            EndIf
            If  distintos=1 Then
              ' Print #1,"cnt ";cnt;" AcordeDistintos"
                AcordeDistintos pasoCol(),cnt, cntold,vel,canal,tiempoDur,Roll
                
            EndIf
            
         End Select  

        cntold = cnt
       ' Print #1,"cnt,cntold"; cnt;" ";cntold
   End if 
     
  Next i1
 ' Print #1,"---FIN -----paso:"; jply;" --------------------------------" 
'  Print #1,"COMIENZA OTRA  POSICION O J ======"; j
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
Sleep 1000,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 
close_port(midiout)
out_free(midiout) 

'ThreadDetach(thread1) 'JMG REPONER !!!!

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
midiout = rtmidi_out_create_default()


'''portsin  =  port_count (midiin)
portsout =  port_count (midiout)
Print #1, "portsin  "; portsin
Print #1, "portsout "; portsout
Dim nombre As ZString ptr

'Print #1,""
'Print #1, "Output port"

Dim i As INTeger
for i = 0 to portsout -1 
    nombre = port_name(midiout, i)
'    Print #1, *nombre
Next   
'Print #1, ""
'Print #1, "Input port "

'For i = 0 to  portsin -1  
'    nombre = port_name(midiin, i)
'    print #1, *nombre
'Next

Dim leng As UInteger <8>
Dim result As Integer

portsout = portout
*nombre = ""

open_port (midiout,portsout, nombre)

'Sleep 50
'nota, velocidad,canal 
' nR=semitono + (nro-1) * 13

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
'nR=(13-semitono) + (hasta-nro) * 13    
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
    curpos = 1
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
      Notapiano= 115-i 
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
      If (Timer-start) > 0.0001 Then ' 0.1 MILESIMA DE PRESICION DE DURACION
        Exit Do
      EndIf
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
       
End Select

End Function


Function sumar( ind As integer) As Integer 
Dim res As Integer 
res= ind Mod 13
 If res = 0 Then
    sumar=1 
 Else
    sumar=0   
 EndIf

End Function 

Sub duracion (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off
Print #1,"COMIENZA RETARDO En  time :"; old_time
Dim start As double
  Do
   start = Timer
   Do
    If (Timer - start) > 0.0001 Then ' 0.1 MILESIMA DE PRESICION DE DURACION
       Exit Do
    EndIf
   Loop
  Loop Until (Timer - old_time) >= tiempoFigura

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
Sub TrasponerGrupo( cant As Integer, Roll As inst)

Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,b1=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha UP
If cant < 0 Then ' UP
 comienzo= NB
 final = NA  
 inc= 1
EndIf
If cant > 0 Then 'DOWN
 comienzo= NA
 final = NB  
 inc=  -1
EndIf
Dim As Integer desdet, hastat
   desdet=1   
   hastat= MaxPos   
pasoNota=13
For jpt = desdet To hastat  
  For i1= comienzo To final Step inc
     If cant < 0 Then  ' UP  
        ind = i1+cant 
        ind = ind - sumar(ind)
     EndIf
     If cant > 0 Then  ' DOWN  
        ind = i1 + cant 
        ind = ind + sumar(ind)
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
                   If cant < 0 Then  ' UP  
                      ind = ind+cant 
                      ind = ind - sumar(ind)
                   EndIf
                   If cant > 0 Then  ' DOWN  
                      ind = ind + cant 
                      ind = ind + sumar(ind)
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

''trasponer=0   
End Sub

Sub trasponerRoll( cant As Integer, Roll As inst)
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,b1=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha UP
If cant < 0 Then ' UP
 comienzo= NB
 final = NA  
 inc= 1
EndIf
If cant > 0 Then 'DOWN
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
     If cant < 0 Then  ' UP  
        ind = i1+cant 
        ind = ind - sumar(ind)
     EndIf
     If cant > 0 Then  ' DOWN  
        ind = i1 + cant 
        ind = ind + sumar(ind)
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
                   If cant < 0 Then  ' UP  
                      ind = ind+cant 
                      ind = ind - sumar(ind)
                   EndIf
                   If cant > 0 Then  ' DOWN  
                      ind = ind + cant 
                      ind = ind + sumar(ind)
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

End Sub

Sub moverZonaRoll(ind As Integer, Roll As inst)
' mueve M + Click la zona a la posicion indicada pro el click
' o copia c + click ñl azona a la posicion indicada  
Dim As Integer jpt=1, i1=1, comienzo , final, inc,b1=0,cant=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha UP
 comienzo= NB
 final = NA  
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
cant = pasoZona2 - pasoZona1 

' sitio donde se copia o mueve indiceNota en main (SC_M o SC_C )+ click 
Dim  As Integer inicioind=ind , MaxPosOld=MaxPos
  
Print #1, "MaxPosOld ", MaxPosOld
 
For jpt=desdet To hastat 
   For  i1= comienzo To final
     Roll.trk(ind,i1).nota = Roll.trk(jpt,i1).nota
     Roll.trk(ind,i1).dur  = Roll.trk(jpt,i1).dur
     Roll.trk(ind,i1).vol  = Roll.trk(jpt,i1).vol
     Roll.trk(ind,i1).pan  = Roll.trk(jpt,i1).pan
     Roll.trk(ind,i1).pb   = Roll.trk(jpt,i1).pb
     Roll.trk(ind,i1).inst = Roll.trk(jpt,i1).inst
    ' Print #1,"i1,ind Roll.trk(i1,ind).nota ",i1, ind, Roll.trk(i1,ind).nota
     If moverZona=1 Then ' borro original
        Roll.trk(jpt,i1).nota = 181
        Roll.trk(jpt,i1).dur  = 0
        Roll.trk(jpt,i1).vol  = 0
        Roll.trk(jpt,i1).pan  = 0
        Roll.trk(jpt,i1).pb   = 0
        Roll.trk(jpt,i1).inst = 0
     EndIf
   Next i1
   ind=ind+1
 Next jpt
  
If ind > MaxPos then
  MaxPos=ind
EndIf
'i la posicion donde copi es mayor a MaxPos debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Not repectivamente
Print #1,"inicioind  MAxPosOld ",inicioind , MAxPosOld  
If inicioind > MAxPosOld Then
   inicioind = inicioind -1
  ' Print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
   For jpt=MaxPosOld-1 To inicioind  
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

' correccion de maspos al copiar antes de maxpos pero la zona suepra maxpos

'If (inicioind < MaxPosOld) Then
' If ( (MaxPosOld - inicioind ) < cant) Then ' cuadno el inicio de copia est aantes de MAxPos
' pero la zona supera a MAxpos original
     MaxPos=MAxPos+1 
' EndIf
'EndIf

posn=MaxPos -1
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
  For i1= 0 To 115   
   
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

