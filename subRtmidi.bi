Sub noteoff( note As UByte, canal As integer) 
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
Sub allSoundoff(canal As integer ) 
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


Sub alloff(canal As Integer ) 
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
Sub PlayRoll (param as any Ptr)
' tiempo es cuantas negras en un minuto tiempoPAtron

' Dim dur, nota
Dim As UInteger eventCode, runningStatus


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
Dim As Integer final=MaxPos  , comienzo, notapiano, canal=1,vel=100,j
Dim As Integer dura=0, maxdur=0,con=0,tiempo,ioff,cx=0,durb=0
Dim As Integer non(1 To 108), liga=0,x=0, durval (1 To 27), silencio, fin, inicio
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

Print #1,"comienzo play ==========> "
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
Dim As tEvent Ptr newEvent
For j=comienzo To final

 If CONTROL1 = 1 Then
   allSoundoff( 1 )
   alloff( 1 )
   CONTROL1=0
 '  close_port(midiout)
 '  out_free(midiout)
   Exit For
 EndIf  
  

 'jcompas = jcompas + 1
 If Compas(j).nro = -1 Then
    velpos=vfuerte
 EndIf
 If Compas(j).nro = -2 Then
    velpos=vdebil
 EndIf
 If Compas(j).nro = -3 Then
    velpos=vsemifuerte
 EndIf
 If Compas(j).nro = -4 Then
    velpos=vdebil
 EndIf
 If Compas(j).nro > 0 Then ' marca del numero de compas 1 2 3 4 es el ultimo tiempo del compas
    velpos=vdebil
 EndIf
' ojo con silencios ligados !!!

  For i=NA To NB Step -1 

   If (Roll.trk(i,j).nota >= 1) And Roll.trk(i,j).nota <= 12 _
      And Roll.trk(i,j).dur >=1 And Roll.trk(i,j).dur <= 108 Then ' es semitono 
      Notapiano= 117-i 
      Notapiano= Notapiano - restar (Notapiano)
      dura=Roll.trk(i,j).dur '1) I 2) I dur x 1 to 108
      If durb > 0 Then ' 1 to 108
      Print #1,"durb> 0, i, j ";durb,i,j
         durl=relDur(durb)+relDur(dura)  '2) P
' si durb ya era silencio su continuacion sera silencio tambien solo
' hace falta analizar la 1era parte para saber si suena o en que grupo caera
' la suma nunca caera en otro grupo silencio nosera, y '+' tampoco si era
' el1er grupo por ejemplo 1 a 27       

         Select Case durb
            Case  1 To  27 
           '  silencio=0
             inicio=1:fin=27
            Case  55 To 81
           '  silencio=0
             inicio=55:fin=81
            Case  28 To  54 
           '  silencio=1
             inicio=28:fin=54
            Case  82 To 108 
           '  silencio=1
             inicio=82:fin=108

         End Select          
         For x= inicio To fin  ' el resto de durciones se repiten   
           If durl=reldur(x) Then
              dura=x ' determino x 1 to 108 en su grupo original
              exit For
           EndIf 
         Next x
      Print #1,"dura + durb "; dura   
         
         liga=1
         durb=0
      EndIf   
      If dura >= 55 And dura <=108 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
         Print #1,"entro nota ligada "; dura, figura(dura)
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc...
' SACAR ESTO TOCAR ACORDE CADA ELEMENTO CON SU DURACION        
      If dura < maxdur Then ' esto lo debo sacar y tocar todas las notas con su duracion
         maxdur= dura ' 1) I, 2) P cuantoms chica dur es mas grnde en relidd
      EndIf 
      If liga=0 Then  
        Print #1,"liga=0 "
        If (maxdur >=28 And maxdur <= 54 ) Or (maxdur >=82 And maxdur <= 108 ) Then
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
      Print #1,"i=";i," maxdur=";maxdur; " figura=";figura(maxdur)
     duracion (maxdur)
' ACA ODRIA ORDENAR LAS DURACIONE DE MENORA MAYOR CALCULARLSO INCREMENTOS
' DAR EL OFF SECUENCIALMENTE SEPARADOS POR DURACIONES INCREMENTALES IGAUL
'A A LDIFERENCIA DE TAMALÑO EJ SI ELACRODE ES DE NEGRAY BLNCA, 1ERO
' SE EJEUCTA LA DURCION DE 1 NEGRA SE DA EL OFF DELA MISMA,LUEGO
' PARA LA BLANCA SE RESTA UNA NEGRA MAS, Q ES LA DIFERENCIA CON LA ANTERIOR
' SE EJECUTA UNA DURCION DE NEGRA ADICIONAL Y SE ENVIA ELOFF DE LA BLANCA     
     Print #1," cantidad cx de off ";cx
     
     For ioff=1 To cx
     noteoff non(ioff),canal
  'Shell "sendmidi.exe off " +Str(non(ioff)) + " 0 "
      Print #1, "OFF==>   non(ioff),  canal "; non(ioff),canal
     Next ioff
     Print #1,"pasó for de off .."
     Print #1," ==============> fin paso...j"; j   
  EndIf 
  Next i

  Print #1,"COMIENZA OTRA  POSICION O J ======"; j
  If durb=0 Then
   cx=0
  EndIf
  con=0 
Next j

Sleep 1000  
close_port(midiout)
out_free(midiout) 
play=0
playb=0

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
Dim As String cr
cr=""
Do

Sleep 1,1
Loop Until (Timer - old_time) >= tiempoFigura   
 
'If cr<>"" Then
'  Print #1,">>>>>>>>>>>>>>>ENTROO NO EN ESTAMIERDA ?"
'  alloff()
'  allSoundoff()
'  close_port(midiout)
'  out_free(midiout)
'  Exit sub 
'EndIf
 
End Sub