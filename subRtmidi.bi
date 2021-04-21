Sub noteoff( note As UByte, canal As integer) 
' canal 1
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

End sub
Sub noteon	( note As UByte, vel As UByte,canal As integer)
	' canal 1
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	If canal= 1 Then
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
Sub note2on	( note1 As UByte, note2 As UByte, vel As UByte,canal As integer)
	noteon note1,vel, canal
	noteon note2,vel, canal
 
End Sub
Sub note2off	( note1 As UByte, note2 As UByte, canal As integer)
noteoff note1,canal
noteoff note2,canal
 
End Sub
Sub note3on	( note1 As UByte, note2 As UByte, note3 As UByte, vel As UByte,canal As integer)
	noteon note1,vel, canal
	noteon note2,vel, canal
	noteon note3,vel, canal
 
End Sub
Sub note3off	( note1 As UByte, note2 As UByte, note3 As UByte, canal As integer)
noteoff note1,canal
noteoff note2,canal
noteoff note3,canal
 
End Sub
Sub note4on	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, vel As UByte,canal As integer)
	noteon note1,vel, canal
	noteon note2,vel, canal
	noteon note3,vel, canal
	noteon note4,vel, canal
 
End Sub
Sub note4off	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, canal As integer)
noteoff note1,canal
noteoff note2,canal
noteoff note3,canal
noteoff note4,canal
 
End Sub
Sub note5on	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte, note5 As UByte, vel As UByte, canal As integer)
	noteon note1,vel, canal
	noteon note2,vel, canal
	noteon note3,vel, canal
	noteon note4,vel, canal
	noteon note5,vel, canal
 
End Sub
Sub note5off	( note1 As UByte, note2 As UByte, note3 As UByte, note4 As UByte,note5 As UByte, canal As integer)
noteoff note1,canal
noteoff note2,canal
noteoff note3,canal
noteoff note4,canal
noteoff note5,canal
 
End Sub
Sub PlayRoll ()
' tiempo es cuantas negras en un minuto tiempoPAtron

' Dim dur, nota

' Roll.TRK(i,j).dur
midiin  = rtmidi_in_create_default()
midiout = rtmidi_out_create_default()


portsin  =  port_count (midiin)
portsout =  port_count (midiout)
Print #1, "portsin  "; portsin
Print #1, "portsout "; portsout
Dim nombre As ZString ptr

Print #1,""
Print #1, "Output port"

Dim i As INTeger
for i = 0 to portsout -1 
    nombre = port_name(midiout, i)
    Print #1, *nombre
Next   
Print #1, ""
print #1, "Input port "

for i = 0 to  portsin -1  
    nombre = port_name(midiin, i)
    print #1, *nombre
Next

Dim leng As UInteger <8>
Dim result As Integer

portsout = 0
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
Dim As Integer non(1 To 108), liga=0,x=0

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
Print #1,"comienzo play ==========> "
For j=comienzo To final 
  For i=NA To NB Step -1 
   If (Roll.trk(i,j).nota >= 1) And Roll.trk(i,j).nota <= 12 _
      And Roll.trk(i,j).dur >=1 And Roll.trk(i,j).dur <= 109 Then ' es semitono 
      Notapiano= 117-i 
      Notapiano= Notapiano - restar (Notapiano)
      dura=Roll.trk(i,j).dur '1) I 2) I
      If durb > 0 Then 
      Print #1,"durb> 0, i, j ";durb,i,j
         dura=relDur(durb)+relDur(dura)  '2) P
         For x= 1 To 108
           If dura=reldur(x) Then
              dura=x
              exit For
           EndIf 
         Next x
      Print #1,"dura + durb "; dura   
         
         liga=1
         durb=0
      EndIf   
      If dura >= 37 And dura <=108 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
         Print #1,"entro nota ligada "; dura, figura(dura)
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc... 
      If dura > maxdur Then
         maxdur= dura ' 1) I, 2) P
      EndIf 
      If liga=0 Then  
        Print #1,"liga=0 "
        noteon notapiano, vel, canal ' 1) G
        cx = cx + 1   ' 1) 1
        non (cx) = notapiano '1) G
        Print #1, "ON==>  notapiano, vel, canal ";notapiano, vel, canal
        Print #1,"cx ";cx 
        old_time=Timer
      Else
        Print #1,"liga=1 no se envia noteon " 
        liga=0 
      EndIf 
   EndIf
   If i=NB And durb = 0 Then ' envio noteoff 1) no entra
 ''Sleep segun duracion o Timer de la q mas dura o para cada uno
  '   Print #1,"i=NB maxdur: ";maxdur
      ' tiempoPatron input al redimsub
      Print #1,"i=NB and durb=0 , maxdur ";maxdur, figura(maxdur)
     duracion (maxdur)
     Print #1," cantidad cx de off ";cx
     For ioff=1 To cx
       noteoff non(ioff),canal
      Print #1, "OFF==>   non(ioff),  canal "; non(ioff),canal
     Next ioff
     Print #1,"pasó for de off .."
     Print #1," =============================> fin paso...j"   
   EndIf 
  Next i

  Print #1,"COMIENZA OTRA  POSICION O J ================="
  If durb=0 Then
   cx=0
  EndIf
  con=0 
Next j

Sleep 1000  
close_port(midiout)
out_free(midiout) 



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
   Case 104 To 115
     restar=8         
       
End Select

End Function
Sub duracion (dura As Integer)
 ' la duracion dependera del tiempo elegido I=60 o I=160 etc
Dim As Double tiempo, tiempoFigura=0
 
tiempo=60/tiempoPatron '60 seg/ cuantas negras enun minuto
' ej si tiempoPatron = 120=> tiempo=1/2

tiempoFigura = relDur(dura)*tiempo  
Print #1, "    tiempoFigura ";tiempoFigura
Do
    Sleep 1, 1
Loop Until (Timer - old_time) >= tiempoFigura
 
 
End Sub