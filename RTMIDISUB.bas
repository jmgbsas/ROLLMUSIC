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

Sub noteSimple	( pasoCol() As vec, vel As UByte,canal As ubyte,tiempoDur As Double)
	' canal 1
Dim As Double old_time =0,tiempoFigura=0
'con o sin liga, recalcula tiempo dur no usa multiplicado por 10a la once
Dim As Integer i1 
If pasoCol(1).tiempoFiguraOld > 0  Then
 Print #1,"NoteSimple old > 0 pasoCol(1).tiempoFiguraOld ";pasoCol(1).tiempoFiguraOld
   pasoCol(1).tiempoFigura= (pasoCol(1).tiempoFigura + pasoCol(1).tiempoFiguraOld)
   Print #1,"tempofigura + old" ; pasoCol(1).tiempoFigura
   Print #1,"ESTOS EN NOTEsIMPLE!"
   pasoCol(1).tiempoFiguraOld =0
EndIf
Print #1,"noteSimple: tiempoFigura:", pasoCol(1).tiempoFigura
Print #1,"noteSimple: notapiano:", pasoCol(1).notapiano;" ";figura(pasoCol(1).DUR)

If pasoCol(1).DUR >= 91 And pasoCol(1).DUR <=180  Then 'liga OFF DESPUES
   pasoCol(1).liga=1
   Print #1,"no se envia noteon de 1er nota +, se suma su tiempo a la siguiente"
   pasoCol(1).tiempoFiguraOld=pasoCol(1).tiempoFigura
   Print #1,"NoteSimple acum pasoCol(1).tiempoFiguraOld ";pasoCol(1).tiempoFiguraOld
Else
   Print #1,"pasoCol(1).liga ", pasoCol(1).liga
    If ligaglobal=0 Then 
       Print #1,"envio on "
       old_time=Timer
       noteon	 pasoCol(1).notapiano, vel ,canal
       For i1=NB To NA
        pasoCol(i1).liga=0
       Next i1 
    EndIf   
       tiempoFigura=pasoCol(1).tiempoFigura/100000000000
       Print #1,"noteSimple tiempoFigura "; tiempoFigura
       duracion old_time,tiempoFigura 'RETARDO SIN OFF
       noteoff pasoCol(1).notapiano ,canal  
       pasoCol(1).liga=0
       ligaglobal=0   
    ' fin completo?
EndIf

Print #1,"noteSimple: liga:", pasoCol(1).liga

End Sub

Function vol (dura As UByte, vel As UByte) As ubyte
 If (dura >=46 And dura <= 90 ) Or (dura >=136 And dura <= 180 ) Then
    vol =0
 Else
    vol= vel
 EndIf

End Function

Sub AcordeIguales (pasoCol() As vec, cnt As UByte, vel as UByte, canal As UByte,tiempoDur As double) 
' todas las notas son de igual duracion, cnt cantidad de notas
Dim i1 As UByte
Print #1,"acordeon iguales"
AcordeOnIguales	 pasoCol() , cnt , vel,canal,tiempoDur
'Dim As Double old_time,tiempoFigura
'old_time=Timer
'tiempoFigura = relDur(pasoCol(cnt).Dur) * tiempoDUR

'duracion old_time,tiempoFigura
AcordeOffIguales	 pasoCol(), cnt , canal
' start  jmg 09-06-2021
dim noHay As Integer=0
For i1 =1 To cnt
 If pasoCol(i1).DUR >=1 and pasoCol(i1).DUR <=90 Then ' ya no hay ligaduras
   nohay=nohay+1
 EndIf    
 
Next i1
If nohay = cnt Then
  For i1=1 To cnt 
    pasoCol(i1).liga=0 
  Next i1
  ligaglobal=0
End If
' end  jmg 09-06-2021

End Sub
Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, vel As UByte,canal As UByte,tiempoDUR As double)

Dim As UByte i1, liga=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
For i1=1 To cnt
  print #1,"DUR notapiano ";pasoCol(i1).DUR;" ";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR) 
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur

Print #1,"DUR cnt=1:";pasoCol(1).Dur

Dim As Double old_time=Timer

For i1=1 To cnt
Print #1,"1)DUR cnt=";i1;":";pasoCol(i1).Dur
' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
 Print #1,"CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
 ' este AND  para iguales no va veremos 
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 Then ''''And pasoCol(i1).liga=0 Then
     Print #1,"ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
     Print #1,"DUR ";pasoCol(i1).Dur
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=old_time * 10000000000
     liga=1
     Print #1,"LIGA=1 ==========> ";liga
     Do
       nj=nj+1
       ' busca la proxima dur 
       durj = Roll.trk(pasoCol(i1).i1 , nj).dur 
       Print #1,"LIGA nj, durj "; nj ; " "; durj
       If durj > 0 Then
         Print #1,"LIGA nj reldur ";nj; " "; relDur(durj)
       Else 
       Print #1,"LIGA nj reldur ";nj; " "; 0 ' revisdar jmgjmg  
       EndIf
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       If durj=0 Then
          tiempoFiguraSig = 0
       Else
          tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
       EndIf
       Print #1,"LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
       Print #1,"LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply ' importante !!! y en distintos???
     Print #1,"2) LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
     print #1,"numero de ligados:";pasoCol(i1).liga
     Print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
     noteon pasoCol(i1).notapiano,vel,canal
     Print #1,"3) LIGA=1 ==========> ";liga
     
  EndIf 
  Print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then
      Print #1,"4) LIGA=1 ==========> ";liga 
      Print #1,"|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
      Print #1,"|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
      Print #1,"|||SIN LIGA DUR "; pasoCol(i1).Dur
      Print #1,"5) LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
       Print #1,"|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0  Then 
    Print #1,"SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
    noteon pasoCol(i1).notapiano,vel,canal
 End If
Next i1



End Sub 


Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, vel As UByte,canal As UByte,tiempoDUR As double)

Dim As UByte i1, liga=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
For i1=1 To cnt
  print #1,"DUR notapiano ";pasoCol(i1).DUR;" ";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR) 
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur

Print #1,"DUR cnt=1:";pasoCol(1).Dur

Dim As Double old_time=Timer

For i1=1 To cnt
Print #1,"1)DUR cnt=";i1;":";pasoCol(i1).Dur
' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
 Print #1,"CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
 ' este AND  para iguales no va pero para distintos??? 
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 And pasoCol(i1).liga=0 Then
     Print #1,"ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
     Print #1,"DUR ";pasoCol(i1).Dur
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=old_time * 10000000000
     liga=1
     Print #1,"LIGA=1 ==========> ";liga
     Do
       nj=nj+1
       ' busca la proxima dur 
       durj = Roll.trk(pasoCol(i1).i1 , nj).dur 
       Print #1,"LIGA nj, durj "; nj ; " "; durj
       Print #1,"LIGA nj reldur ";nj; " "; relDur(durj)
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
       Print #1,"LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
       Print #1,"LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply '08-06-2021
     Print #1,"2) LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
     print #1,"numero de ligados:";pasoCol(i1).liga
     Print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
     noteon pasoCol(i1).notapiano,vel,canal
     Print #1,"3) LIGA=1 ==========> ";liga
     
  EndIf 
  Print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then
      Print #1,"4) LIGA=1 ==========> ";liga 
      Print #1,"|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
      Print #1,"|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
      Print #1,"|||SIN LIGA DUR "; pasoCol(i1).Dur
      Print #1,"5) LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
       Print #1,"|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0  Then 
    Print #1,"SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
    noteon pasoCol(i1).notapiano,vel,canal
 End If
Next i1


	
 
End Sub

Sub AcordeOffIguales	( pasoCol() As vec, cnt As UByte, canal As UByte)
Dim i1 As UByte
Dim tiempoFigura As Double 
'--------
Print #1,"-------------------------------------"
Print #1,"start AcordeOffIguales"


 old_time=Timer
 Print #1,"old_time off inicial ";old_time

Print #1,"no ligados calculo tiempo Figura y off:" 

For i1 = 1 To cnt ' (1) 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/100000000000
     Print #1,"i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time, tiempoFigura
     Print #1, "SIN LIGAR OFF==>"; 
     Print #1,"i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     noteoff pasoCol(i1).notapiano ,canal
  EndIf
Next i1     

Print #1,"start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
     Print #1,"HAY LIGADOS!"
     Print #1,"pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
     Print #1,"pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
   'old_time= pasoCol(i1).old_time/100000000000 
     Print #1,"old_time ";old_time        
     tf = pasoCol(i1).tiempoFiguraOld  /100000000000
     Print #1, "retardo tf ";tf
     If TF > 0 Then 
       duracion old_time, tf
       If Roll.trk(pasoCol(i1).i1 , jply+1).dur > 0 And _
          Roll.trk(pasoCol(i1).i1 , jply+1).dur <= 181 Then
          Print #1," ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
          figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
             
          noteoff pasoCol(i1).notapiano ,canal
          'liga cero no
       EndIf
    EndIf   
  EndIf 
  Print #1,"pasoCol(i1).liga ", pasoCol(i1).liga
  ligaglobal = pasoCol(i1).liga ' por si sigue una simple
  Print #1,"<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"

Next i1

'-------
For i1 = 1 To cnt 
	noteoff pasoCol(i1).notapiano, canal
	
next i1	


End sub

Sub AcordeDistintos (pasoCol() As vec, cnt As UByte, vel As UByte,canal As UByte,tiempoDur As double) 
' Hay notas de sitinta duracion, cnt cantidad de notas
Dim i1 As UByte
AcordeOnDistintos  pasoCol(), cnt , vel  , canal,tiempoDur
AcordeOffDistintos pasoCol(), cnt , canal,tiempoDur

End Sub


Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte, canal As UByte,tiempoDUR As Double)
Dim i1 As UByte
'FALTARIA UN SORT POR tiempoFigura ,,,,, test agregamso sort 
For i1=1 To cnt
  print #1,"antes Sort Fig, DUR notapiano ";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura 
Next i1
   qsort(@pasoCol(1).tiempoFigura, cnt, SizeOf(vec), @QCompare )
For i1=1 To cnt
  print #1,"deespues sort DUR notapiano fig";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura 
Next i1   
'-----------------------------------------

Dim As Double old_time,tiempoFigura
Print #1,"-------------------------------------"
Print #1,"start AcordeOffDistintos"


 old_time=Timer
 Print #1,"old_time off inicial ";old_time

Print #1,"no ligados calculo tiempo Figura y off:" 
Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
Print #1,"cnt "; cnt; "i1 "; I1; " pasoCol(i1).liga "; pasoCol(i1).liga;" notapiano ";pasoCol(i1).notapiano 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/100000000000
     Print #1,"i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time, tiempoFigura
     Print #1, "SIN LIGAR OFF==>"; 
     Print #1,"i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     noteoff pasoCol(i1).notapiano ,canal
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * 100000000000
Print #1,"tiempoFigMayorNoligado ";tiempoFigMayorNoligado
Print #1,"start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
   Print #1,"HAY LIGADOS!"
   Print #1,"pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   Print #1,"tiempoFigMayorNoligado ";tiempoFigMayorNoligado
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
         Print #1," ligado no se envia off,old  es mayor a la mayor de no ligado "
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
        Print #1,"i1 ";i1;" tiempoFigura q falta para el off de ligado ";
        Print #1,pasoCol(i1).tiempoFigura
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     Else 
         'old_time= pasoCol(i1).old_time/100000000000 
         Print #1,"old_time ";old_time        
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /100000000000
         Print #1,"pasoCol(i1).tiempoFigura ";pasoCol(i1).tiempoFigura
         Print #1,"pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
         Print #1, "retardo tf ";tf
         If TF > 0 Then 
           duracion old_time, tf
           If Roll.trk(pasoCol(i1).i1 , jply+1).dur > 0 And _
              Roll.trk(pasoCol(i1).i1 , jply+1).dur <= 181 Then
              Print #1," ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
                   figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
                     
              noteoff pasoCol(i1).notapiano ,canal
           EndIf
         Else
           If (pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld) And pasoCol(i1).liga = 1  Then
              tf=tiempoFigMayorNoligado/100000000000 
               Print #1, "retardo recuperado en condicion (=) ..tf= ";tf
              duracion old_time, tf
              noteoff pasoCol(i1).notapiano ,canal
           Else

              Print #1,"NO SE ENVIA OFF TF=0"
           EndIf  
         EndIf 
         Print #1,"pasoCol(i1).liga ", pasoCol(i1).liga
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
         Print #1,"<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"
     EndIf   
  EndIf 

Next i1
pasoCol(i1).tiempoFiguraOld=0

 ' observacion una ligadura de varios acordes en una nota dada podria
 ' terminar o tener una nota simple unica como final o intermedia ligada
 ' ver que pasa en ese caso como lo solucionamos
End Sub

'-------------playAll-----21-05-2021-------
Sub playAll() ' play version 2
' tiempo es cuantas negras en un minuto tiempoPAtron
' PLAY masavanzado en un mismo acorde si son de distinta duracion
' sus notas se toca cada una con su propia duracion,el corde no termina
' hasta queterminede tocar la nota mas larga.



Dim As Double tiempoDUR, tiempoFigura=0,tiempoFiguraOld=0,old_time_old=0
tiempoDUR=60/tiempoPatron '60 seg/ cuantas negras enun minuto

midiout = rtmidi_out_create_default()
'Print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout)
'Print #1, "portsin  "; portsin
Print #1, "portsout "; portsout
Dim nombre As ZString ptr
Dim  As Integer i1,i2,i3,i4,i5,j
for i1 = 0 to portsout -1 
    nombre = port_name(midiout, i1)
 '   Print #1, *nombre
Next i1  

Dim leng As UInteger <8>
Dim result As Integer

portsout = portout
*nombre = ""

open_port (midiout,portsout, nombre)

Dim As Integer comienzo=1, final=MaxPos,  canal=1,vel=100,velpos =0
Dim pasoCol (NB To NA) As vec  ' entrada de durciones a medida que barro una columna
Dim As Double start
Dim as Integer cnt=0, cpar=0,dura=0,duraOld=0,nj, durj,tiempoFiguraSig
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Print #1,"  "
Print #1,"comienzo playaLL ==========> "
jply=0:curpos=0
mousex=0
 Print #1,"-----------------------------------------"

For jply=comienzo To final
posicion=jply
 If curpos > NroCol  Then
    curpos = NroCol
    posishow=0
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
  Print #1,"---START-----paso:";jply;" --------------------------------"
  '116 a 1
  For i1=NA To NB Step -1 
   
   If (Roll.trk(i1,jply).nota >= 1) And Roll.trk(i1,jply).nota <= 12 _
      And Roll.trk(i1,jply).dur >=1 And Roll.trk(i1,jply).dur <= 180 Then ' es semitono 
      Notapiano= 117-i1 
      Notapiano= Notapiano - restar (Notapiano)
      dura=CInt(Roll.trk(i1,jply).dur) '1) I 2) I dur x 1 to 108
      Print #1,"jply ";jply; "dura ";dura
      cnt=cnt+1
      Print #1,"paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
      If cnt=1 Then 
         duraOld=dura
         tiempoFiguraOld=relDur(duraOld) * tiempoDUR * 100000000000
      EndIf
      If duraOld=dura  Then
         iguales=1
         Print #1,"cnt ";cnt;" iguales ";iguales
      Else
         distintos=1
         Print #1,"cnt ";cnt;" distintos ";distintos
      EndIf         


      '+++++ veo si esta ligado , si lo esta sera distinto
      ' sino lo esta puedo considera +90 para igualar
      ' veoliga ahcer sub
     If cnt > 1 And distintos=1 Then ' 3do chequeo
        If ( dura >= 91 And dura <=180 )  Then ' hay liga  
           nj=jply '08-06-2021
           tiempoFigura = relDur(dura) * tiempoDUR * 100000000000
           tiempoFiguraOld= relDur(duraOld) * tiempoDur * 100000000000
           Do
             nj=nj+1
       ' busca la proxima dur 
             durj = Roll.trk(i1 , nj).dur 
             Print #1,"carga inicio veo liga"; durj
             tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
             tiempoFigura = tiempoFigura +tiempoFiguraSig
             Print #1,"carga inicio pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
             If durj >= 91 And durj <=180  Then ' si es liga 
             Else
                Exit Do
             EndIf
           Loop
       EndIf    
     ' comparo el tiempoFigura del duraOld con el dura actual
     ' sumado a todas sus ligaduras 
       If tiempoFigura=tiempoFiguraOld Then
          Print #1,"dura ligado ";dura; " duarold ";duraOld
          Print #1,"tiempoFigura ";tiempoFigura
          Print #1,"tiempoFiguraOld ";tiempoFiguraOld
          iguales=1 
       EndIf
        If ( duraOld >= 91 And duraOLd <=180 )  Then ' hay liga  
           nj=jply '08-06-2021
           tiempoFigura = relDur(duraOld) * tiempoDUR * 100000000000
           tiempoFiguraOld= relDur(dura) * tiempoDur * 100000000000
           Do
             nj=nj+1
       ' busca la proxima dur 
             durj = Roll.trk(i1 , nj).dur 
             Print #1,"carga inicio veo liga"; durj
             If durj>0 Then
                tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
                tiempoFigura = tiempoFigura +tiempoFiguraSig
                Print #1,"carga inicio pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
             EndIf
             If durj >= 91 And durj <=180  Then ' si es liga 
             Else
                Exit Do
             EndIf
           Loop
       EndIf    
     ' comparo el tiempoFigura del duraOld con el dura actual
       If tiempoFigura=tiempoFiguraOld Then
          Print #1,"dura ";dura; " duarold ligado ";duraOld
          Print #1,"tiempoFigura ";tiempoFigura
          Print #1,"tiempoFiguraOld ";tiempoFiguraOld
          iguales=1 
       EndIf
        
     EndIf
      
 
      
      '+++++++++
         
      pasoCol(cnt).DUR =dura
      Print #1,"pasoCol(cnt).DUR "; pasoCol(cnt).DUR 
      pasoCol(cnt).notapiano=Notapiano 
      pasoCol(cnt).tiempoFigura=relDur(pasoCol(cnt).DUR) * tiempoDur * 100000000000
      pasoCol(cnt).i1 = i1
      duraOld=dura 
      vel= vol( dura, velpos) 
      
' llegamos al final de la Columna
   EndIf
   
   ' i1=1
   If i1=NB  Then 'And cnt >= 1 Then ' envio noteoff 1) no entra
   
         If cnt > 1 Then' Acorde
           Print #1,"i1=NB=";i1 ; " ACORDE cnt= ";cnt
         Else    
           Print #1,"i1=NB=";i1 ; " SIMPLE cnt= ";cnt
         EndIf  
        'sort delvector pasoCol segun el ordRelDur y asirecuperamos el orden Asc de los relDur
         Select Case cnt
          Case 1 
           ' con y sin liga? en uno solo o parto? veremos
           ' INCLUYE LIGADOS O NO LIGADOS
            noteSimple  pasoCol(),vel,canal,tiempoDur
                     
          Case Is > 1

            If iguales=1 And distintos=0 Then
         For i2=1 To cnt
           pasoCol(i2).tiempoFiguraOld=0
         Next i2

                Print #1,"cnt ";cnt;" Acordeiguales "
                AcordeIguales pasoCol(),cnt,vel,canal,tiempoDur
                
            EndIf
            If  distintos=1 Then
               Print #1,"cnt ";cnt;" AcordeDistintos"
                AcordeDistintos pasoCol(),cnt, vel,canal,tiempoDur
                
            EndIf
            
         End Select  

         
   End if 
     
  Next i1
  Print #1,"---FIN -----paso:"; jply;" --------------------------------" 
'  Print #1,"COMIENZA OTRA  POSICION O J ======"; j
 mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0
 
Next jply

posishow=posicion - 20
posicion=posicion -20
 
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
        Print #1,"Qsort called";cnt;" time(s) with";el1;" and";el2;"."
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
    curpos = NroCol
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
    
   If (Roll.trk(i,jply).nota >= 1) And Roll.trk(i,jply).nota <= 12 _
      And Roll.trk(i,jply).dur >=1 And Roll.trk(i,jply).dur <= 180 Then ' es semitono 
      Notapiano= 117-i 
      Notapiano= Notapiano - restar (Notapiano)
      dura=Roll.trk(i,jply).dur '1) I 2) I dur x 1 to 108



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
      Print #1,"dura + durb "; dura   
         liga=1
         durb=0
         durl=0
      EndIf   
      If dura >= 91 And dura <=180 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
         Print #1,"entro nota ligada "; dura, figura(dura)
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
        Print #1, jply; " con=0 atrapa dura maxdura ";dura, maxdur
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc...
' SACAR ESTO TOCAR ACORDE CADA ELEMENTO CON SU DURACION        
      If relDur(dura) > relDur(maxdur) Then ' esto lo debo sacar y tocar todas las notas con su duracion
         maxdur= dura ' 1) I, 2) P cuantoms chica dur es mas grnde en relidd
      Print #1,jply;"if dura-figura "; dura, figura(dura)
      Print #1,jply;"if cambio Maxdur-figura "; Maxdur, figura(Maxdur)
      Else    
        '' notacur=i
      Print #1,jply;"else dura-figura "; dura, figura(dura)
      Print #1,jply;"sigue igual else Maxdur-figura "; Maxdur, figura(Maxdur)

      EndIf 
      If liga=0 Then  
        Print #1,"liga=0 "
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
        Print #1, "ON==>  notapiano, vel, canal ";notapiano, vel, canal
        Print #1,"cx ";cx 
      ''''''  Sleep 1,1
        old_time=Timer
      Else
        Print #1,"liga=1 no se envia noteon " 
        liga=0 
      EndIf 
   EndIf

   If i=NB And durb = 0 Then ' envio noteoff 1) no entra
 ''Sleep segun duracion o Timer de la q mas dura o para cada uno
      ' tiempoPatron input al redimsub
    Print #1,"i=NB=";i," maxdur=";maxdur;  
    If maxdur > 0 And maxdur <= 182 Then
       Print #1, figura(maxdur)
    Else 
       Print #1, "No se puede mostrar"  
    EndIf   
    
  ''''   duracion (maxdur)
'''' DURACION  

 If maxdur >= 1 And maxdur<= 180 Then 
    tiempoFigura = relDur(maxdur)*tiempoDUR
 
   Print #1, "tiempoFigura ";tiempoFigura  
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

   Loop Until (Timer - old_time) >= tiempoFigura
 EndIf 
' ---------------     
' FIN DURACION 
' ACA ODRIA ORDENAR LAS DURACIONE DE MENORA MAYOR CALCULARLSO INCREMENTOS
' DAR EL OFF SECUENCIALMENTE SEPARADOS POR DURACIONES INCREMENTALES IGAUL
'A A LDIFERENCIA DE TAMALÑO EJ SI ELACRODE ES DE NEGRAY BLNCA, 1ERO
' SE EJEUCTA LA DURCION DE 1 NEGRA SE DA EL OFF DELA MISMA,LUEGO
' PARA LA BLANCA SE RESTA UNA NEGRA MAS, Q ES LA DIFERENCIA CON LA ANTERIOR
' SE EJECUTA UNA DURCION DE NEGRA ADICIONAL Y SE ENVIA ELOFF DE LA BLANCA     
     Print #1," cantidad cx de off ";cx
     
     For ioff=1 To cx
     noteoff non(ioff),canal

     Print #1, "OFF==>   non(ioff),  canal "; non(ioff),canal
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
   Case 1 To 12
     restar=0
   Case 14 To 25
     restar=1
   Case 27 To 38
     restar= 2
   Case 40 To 51
     restar= 3
   Case 53 To 64
     restar =4
   Case 66 To 77
     restar= 5
   Case 79 To 90
     restar=6
   Case 92 To 103
     restar= 7
   Case 105 To 116
     restar=8         
       
End Select

End Function
Sub duracion (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off
Print #1,"COMIENZA RETARDO En  old_time on :"; old_time
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
Sub ligadura()


End Sub