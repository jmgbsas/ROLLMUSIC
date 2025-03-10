On  Error GoTo errorrtmidi
#include once "win/mmsystem.bi" 

function BrowseCallback(byval hWnd as HWND, _
							byval uMsg as UINT, _ 
                            byval lParam as LPARAM, _
                            byval lpData as LPARAM) as long

    select case uMsg 
    case BFFM_INITIALIZED 
        SendMessage( hWnd, BFFM_SETSELECTION, -1, lpData ) 
    
    case BFFM_SELCHANGED 
        dim as zstring * MAX_PATH sPath
      	
      	if SHGetPathFromIDList( cast( LPCITEMIDLIST, lParam ), sPath) = 0 then 
            sPath = "Unknown"
        else 
            sPath = "PATH: " + sPath
        end if 
        
        SendMessage( hWnd, BFFM_SETSTATUSTEXT, 0, cuint( @sPath ) )
   end select 
   
   function = 0 

end function 

function BrowseForFolder(byval hWnd as HWND, _
						 byval Prompt as string, _ 
                         byval Flags as integer, _
                         byval DefaultFolder as string) as string 
    
    dim bi         as BROWSEINFO 
    dim pidlReturn as LPITEMIDLIST
    dim pidlStart  as LPITEMIDLIST
	static sFolder as string 

    CoInitialize( NULL ) 
    SHGetSpecialFolderLocation( NULL, CSIDL_DRIVES, @pidlStart ) 

    sFolder       	= DefaultFolder

    with bi
	    .pidlRoot   = pidlStart 
	    .hwndOwner  = hWnd 
	    .lpszTitle  = strptr(Prompt)
	    .ulFlags    = Flags 
	   	.lpfn       = @BrowseCallback 
	   	.lParam		= cuint( strptr( sFolder ) )
	end with
    
   	pidlReturn = SHBrowseForFolder( @bi ) 
   
   	CoTaskMemFree( pidlStart ) 
   
   	if( pidlReturn <> NULL ) Then 
        dim as zstring * MAX_PATH path
        SHGetPathFromIDList( pidlReturn, path ) 
        CoTaskMemFree( pidlReturn ) 
        function = path    
    else
    	function = ""
    end if 
    
    CoUninitialize( ) 
    
end function


Sub sortaudio( col() As vec,cnt As integer)

Dim As Integer a1=0,i1=0,j1=0,indicefinal=0
' sort rapido por audio solo clasifico por audio 1 suena 2 no suena
' copio el vector solo indice y audio
' construyo mi indice clasificado pasoaudio
For i1=1 To cnt
 If Col(i1).audio=1 Then
    a1=a1+1
    indiceaudio(a1)=i1
    indicefinal=a1
 EndIf 
Next i1
a1=0
For i1=1 To cnt
 If Col(i1).audio=2 Then
    a1=a1+1
    indiceaudio(indicefinal+a1)=i1
 EndIf 
Next i1
For i1=1 To cnt
Next i1


End Sub

'
Sub veoPasoCol( Col() As vec,cnt As integer, cntold As Integer)
Dim As Integer mayor,j
mayor=cnt
If cntold > cnt Then
  mayor=cntold
Else 
EndIf
If cntold = cnt Then
EndIf
' en lso old acumulo los valores actuales solo si estan en cero
' de otro modo se hacen 0 en OFF o se incrementan en ligaduras
For j=1 To cnt  ''''mayor veo solo el actual no el anterior 21-02-2022
Next j

End Sub
'
Sub omnion( note As UByte, canal As UByte, portsal As UByte) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' voy a tener que enviar el nro de canal que se usa en cada pista al tocarla...

	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	
  If canal = 0 Then  ' 0xB0 = 11x16 = 176
		 modo = 176 ' 0xB0 + 0 = 176 +0 = 176 
	Else
	  modo = 176 + canal
  EndIf   	
 message(1) = modo 
 message(2) = 125 ' 123 all note off, 124 omni mode off, 125, omni mode on, 126 mono mode on, 127 poly mode on
 message(3) = 0  
	
 leng=3
result = send_message (midiout(portsal), p, leng)

End Sub

Sub polyon( note As UByte, canal As UByte, portsal As UByte) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' voy a tener que enviar el nro de canal que se usa en cada pista al tocarla...

	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	
  If canal = 0 Then  ' 0xB0 = 11x16 = 176
		 modo = 176 ' 0xB0 + 0 = 176 +0 = 176 
	Else
	  modo = 176 + canal
  EndIf   	
 message(1) = modo 
 message(2) = 127 ' 123 all note off, 124 omni mode off, 125, omni mode on, 126 mono mode on, 127 poly mode on
 message(3) = 0  
	
 leng=3
result = send_message (midiout(portsal), p, leng)

End Sub


' 
Sub noteoff(  note As UByte, canal As UByte,portsal As UByte,i1 As Integer) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' voy a tener que enviar el nro de canal que se usa en cada pista al tocarla...
' PARAMETRO i1 lo deberia sacar 11-10-2024 jmgjmg
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
  If canal = 0 Then  ' 0xB0 = 11x16 = 176
		 modo = 128 ' 0xB0 + 0 = 176 +0 = 176 
	Else
	  modo = 128 + canal
  EndIf   	
 message(1) = modo 
 message(2) = note ' 123 all note off, 124 omni mode off, 125, omni mode on, 126 mono mode on, 127 poly mode on
 message(3) = 0  


/'	
  If canal = 0 Then  ' 0xB0 = 11x16 = 176
		 modo = 176 ' 0xB0 + 0 = 176 +0 = 176 
	Else
	  modo = 176 + canal
  EndIf   	
 message(1) = modo 
 message(2) = 123 ' 123 all note off, 124 omni mode off, 125, omni mode on, 126 mono mode on, 127 poly mode on
 message(3) = 0  
'/	
'indicenotas=indicenotas + 1
If MIDIFILEONOFF = HABILITAR Then 'habilito salida midi
   Dim As Double TiempoAcumNew
   Dim As Integer T1 
   TiempoAcumNew= Timer - STARTMIDI
   T1 =  TiempoAcumNew * 1000 * tiempoPatron/60
   Dim cadenacanal As String = " off ch="+Trim(Str(canal+1))
   ' NOTE PLAY DURACION LO DICTAMINA EL OFF
    Print #midiPlano, T1  ;
    Print #midiPlano, cadenacanal; " n="; note; " v=0"


   '  miditxt(indicenotas).sumatiempo = T1
   '  miditxt(indicenotas).canal      = canal
   '  miditxt(indicenotas).estado     = 1
   '  miditxt(indicenotas).nota       = note
   '  miditxt(indicenotas).vel        = 0
  
EndIf



 leng=3
result = send_message (midiout(portsal), p, leng)

End Sub
Sub pedaloff( portsal As UByte) 
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
result = send_message (midiout(portsal), p, leng)

End Sub
Sub allSoundoff(canal As UByte, portsal As UByte ) 
' canal 1
'1011 = 11 =B -> hexa Bn...11x16= 176
' 120 da sound off para todas las notas solo hy qu eenvirlo a 
'todoslos canales 120 es canal 0 sera modo + canal 
' habria que hacer un for 120,121,122,123 etc 120 + canal=modo
' manda off a todas las notas que todavi ano recibieron su off 
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer
	 modo = 176
  If canal = 0 Then  ' 0xB0 = 11x16 = 176
		 modo = 176 ' 0xB0 + 0 = 176 +0 = 176 
	Else
	  modo = 176 + canal
  EndIf   	
 message(1) = modo 
 message(2) = 120
 message(3) = 0  
 leng=3
result = send_message (midiout(portsal), p, leng)

End Sub


Sub alloff(canal As UByte, portsal As UByte ) 
' canal 1 NO FUNCIONA contar desde 0 a 15?
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
result = send_message (midiout(portsal), p, leng)

End Sub
Sub ChangeProgram ( instru As UByte,  canal As UByte,portsal As UByte)

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
'On Local Error GoTo  errorcp
  result = send_message (midiout(portsal) , p, leng) 
'errorcp:
'Dim n As Integer = Err

End Sub
Sub noteon	( note As ubyte, vel As UByte, canal As UByte, portsal As UByte,i1 As Integer)
	' canal 1 
' NOTE ON, 1001 XXXX, 9 X H, Nº de tecla o nota, Velocidad
' 90 = 9*16=144 	
	Dim modo As UByte
	Dim leng As UInteger <8>
	Dim result As Integer ' canal 0 to 15 canal 0 es el 1
	If canal = 0 Then
		 modo = 144 ' 0x90 + 0 = 9x16+0=144 +0 = 144 
	Else
	  modo = 144 + canal 
	EndIf
	if velmidi > 0 then
     vel=velmidi
  EndIf
 message(1) = modo 
 message(2) = note
 message(3) = vel
 leng=3

result = send_message (midiout(portsal), p, leng)

'indicenotas=indicenotas + 1
If MIDIFILEONOFF = HABILITAR Then  ' habilito escritura a midi 
   
   Dim As Double TiempoAcumNew
   Dim As Integer T1 
   TiempoAcumNew= Timer - STARTMIDI
 
  Dim cadenacanal As String = " on ch="+Trim(Str(canal+1))
   T1 =  TiempoAcumNew *  1000 * tiempoPatron/60
   
   '   miditxt(indicenotas).sumatiempo = T1
   '   miditxt(indicenotas).canal      = canal
   '   miditxt(indicenotas).estado     = 1
   '   miditxt(indicenotas).nota       = note

  
   If pasoCol(i1).audio = 1 Then
      Print #midiPlano, T1; 
      Print #midiPlano, cadenacanal;" n=";note;" v=";vel
      ''miditxt (indicenotas).vel        = vel
 
   EndIf
   If pasoCol(i1).audio = 2 Then
      Print #midiPlano, T1; 
      Print #midiPlano," on"; " ch=";canal+1;" n=";note;" v=0"
     '' miditxt (indicenotas).vel        = 0
      
   EndIf



EndIf
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

' SACAR EL CANAL DE LOS PARAMETROS NO SE USA
Sub AcordeOnIguales ( pasoCol() As vec , cnt As UByte, cntold As UByte,vel As UByte,tiempoDUR As Double, Roll As inst, velpos As Integer,pis As UByte,portsal As UByte)
' 22-11-2021 DEBO DAR RETARDOS EN CADA PASO DONDE HAYA ACORDES LIGADOS 1 UNO DE ELLOS SILENCIO
Dim As integer i1=0,J1=0,k1=0,p1=0
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
Dim canal As UByte
'debug..

' si hay un mix de silencios y audio=2 , los NOTEON debe ejecutarse 1ero de todo
Dim As Integer silen=0, ons=0
For i1=1 To cnt
 If pasoCol(i1).audio=1 Then ' 1= suena, 2 silencio
    ons=1
 EndIf
 If pasoCol(i1).audio=2 Then
    silen=1
 EndIf
Next i1
' qsort
If silen=1 And ons=1 Then
  sortaudio pasocol(),cnt
  p1=1
Else
  p1=0  
EndIf 
 '' veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)
' en .ligaoLD acumulo las ligaduras horizontalmente solo se va a cero en OFF ultimo
' en .liga solo determino la liga actual.-
' la  liga final de esa nota que este en cero 0 me determina que es el OFF
' final y ahi hago .ligaold=0
Dim As integer nj=jply, durj ' indice del vector roll, dur
Dim As Double f
old_time_on=Timer
old_time_on_int=old_time_on * d11 
' LIGADURAS SEGUIDAS. AL INICIO LIGAOLD ES CERO SE DA EL ON
' Y SI HAY VARIAS LIGADURAS SECUENCIALES VOY EJECUTANDO
' LAS DURACIONES O RETARDOS DE CADA PASO Y AL FINAL DOY EL RETARDO FINAL Y EL OFF
' CUANDO LA ULTIMA NOTA NO TENGA MAS LIGADURA ES EL MOMENTO DEL OFF.

For k1=1 To cnt  ' PARA CADA NOTA

 If p1=1 Then
   i1 = indiceaudio(k1)
 Else
   i1 = k1  
 EndIf

 Select Case pasoCol(i1).Dur
  
   CASE   1 To  90  ' SIN LIGADURA 
 
      If pasoCol(i1).old_timeold = 0 Then
         pasoCol(i1).old_time=old_time_on_int
      Else
         pasoCol(i1).old_time=pasoCol(i1).old_timeold 
      EndIf
      pasoCol(i1).liga =0
      pasoCol(i1).tiempoFigura= reldur(pasoCol(i1).DUR) * tiempoDUR * d11    
      If pasoCol(i1).tiempoFiguraOld <> 0 Then
        If tipoAcorde = 1 Then
           pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFiguraOld
        Else ' acumulo si no es ligado pero hay ligaol porque es final de ligadura
           If pasoCol(i1).ligaold > 0 Then
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld+ pasoCol(i1).tiempoFigura
           EndIf
        EndIf  
      EndIf  
      
      If pasoCol(i1).ligaold =0  Then

         canal=pasoCol(i1).canal
         portsal=CUByte(pasoCol(i1).port)
         vel= CUByte(vol( pasoCol(i1).DUR, velpos))
         if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
            vel=0
         EndIf   
         noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1

      Else
         ' aca se da que  si al ultima nota de una sucesion de ligados sin sonido , tiene sonido
         ' y la anterior no tenia entonces se resta los retardos de la nota sin sonido
         ' alas notas con audioold=0 igual que en la otra parte se copia

          Dim As Integer cret 
          If pasoCol(i1).audio=1 And pasoCol(i1).audioold=2 Then

             canal=pasoCol(i1).canal
             portsal=CUByte(pasoCol(i1).port)
             vel= CUByte(vol( pasoCol(i1).DUR, velpos))
             if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                vel=0
             EndIf   
             noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1 
             pasoCol(i1).old_time=old_time_on_int
          EndIf
         ENDIF 
   Case   91  To 180 ' CON LIGADURA con sonido la 1er el resto mute
  
     pasoCol(i1).liga=1
     If pasoCol(i1).old_timeold = 0 Then
        pasoCol(i1).old_time = old_time_on_int
        pasoCol(i1).old_timeOld = old_time_on_int
     Else
        If tipoAcorde=1 Then
           pasoCol(i1).old_time= pasoCol(i1).old_timeold
        EndIf 
     EndIf

     pasoCol(i1).tiempoFigura= reldur(pasoCol(i1).DUR) * tiempoDUR * d11
     If pasoCol(i1).tiempoFiguraOld =0  Then
      ' tiene sonido la 1er nota de un aligadura larga
        'vel= CUByte(vol( pasoCol(i1).DUR, velpos)) ' 28-04-2024 comento 
        'pasoCol(i1).audio=1 comento la original no lo tenia 
        pasoCol(i1).tiempoFiguraOld =pasoCol(i1).tiempoFigura
     Else
        If tipoAcorde = 1 Then
           pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
        Else ' acumulo
           pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld+ pasoCol(i1).tiempoFigura
        EndIf  
     EndIf

     Select Case  pasoCol(i1).ligaold
        Case Is > 0
           If  pasoCol(i1).audio=1 And pasoCol(i1).audioOld = 2  Then
               canal=pasoCol(i1).canal
               portsal=CUByte(pasoCol(i1).port)
               vel= CUByte(vol( pasoCol(i1).DUR, velpos))
               if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                  vel=0
               EndIf   
               
               noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
               pasoCol(i1).old_time=old_time_on_int
               pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ''''aca toma 0.25 
               pasoCol(i1).old_timeold=old_time_on_int
            '  borramos los retardos de silencio anteriores a las notas que tenian noteon antes o sea
            ' que eran audio=2
           EndIf           
           If  tipoAcorde=1 Or pasoCol(i1).audio=2 Then
              f=pasoCol(i1).tiempoFigura/d11

              Dim As Integer k=0
              For J1=1 To CNT
                 If pasoCol(j1).liga =1 Then
                    k=k+1
                 EndIf
                 If j1=cnt then 
                    Exit For
                 EndIf
              Next J1
              If k=cnt Then
                 '0 duracion pasoCol(i1).old_time/d11, pasoCol(i1).tiempoFigura/d11
                 '1 duracion pasoCol(i1).old_timeold/d11, f '25-11

                 If pasoCol(i1).audioOld=1 And pasoCol(i1).audio=2 Then
                    duracion pasoCol(i1).old_timeold/d11, f '25-11
                    portsal=CUByte(pasoCol(i1).port)
                    canal=pasoCol(i1).canal ' 12 junio 2024
                    noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1  ' la notapiano es la misma porque esta ligado 
                 EndIf
                 If pasoCol(i1).liga=1 Then
                    duracion old_time_on, f '25-11
                 EndIf
              EndIf
           EndIf    
        Case Else
          If pasoCol(i1).ligaold =0 And pasoCol(i1).liga=1 Then '16-11-2021 UN SOLO ON
             If pasoCol(i1).audio = 1 Then
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                vel= CUByte(vol( pasoCol(i1).DUR, velpos))
                if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                   vel=0
                EndIf   
                noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
                pasoCol(i1).old_time = old_time_on_int

             EndIf
              
             Dim As Integer k=0
             For J1=1 To CNT
               If pasoCol(j1).liga =1 Then
                  k=k+1
               EndIf
               If j1=cnt then 
                  Exit For
               EndIf
             Next J1
             If k=cnt And tipoAcorde=1 Then
                duracion pasoCol(i1).old_time/d11, pasoCol(i1).tiempoFigura/d11
             EndIf
          EndIf
' ==> 8 junio 2024 LAS NOTAS EN SILENCIO DEBEN TENER SU ON CON V=0 
          If pasoCol(i1).ligaold =0 And pasoCol(i1).audio =2 Then
' nota nueva de silencio sin sonido 
             canal=pasoCol(i1).canal
             portsal=CUByte(pasoCol(i1).port)
             noteon CUByte(pasoCol(i1).notapiano),0 ,canal,portsal,i1
          EndIf
' fin 8 de junio 2024 anda ok !!!   
     End Select 
         pasoCol(i1).ligaold = pasoCol(i1).ligaold + 1 ' para el proximo paso
 End SELECT

Next k1

' for de NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS"


End Sub 

Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,tiempoDUR As Double,Roll As inst,velpos As Integer,pis As UByte,portsal As UByte)
'debug..
' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

Dim As UByte i1, liga=0,coff=0,canal
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
If cntold >cnt Then
 coff=cntold
Else
 coff=cnt 
EndIf
Dim As UByte noAnalizo=0 
For i1=1 To coff ' 20-06-2021 JMG
  If pasoCol(i1).liga > 1 Then ' si hay liga viene de antes
     noAnalizo=1
  EndIf 
       
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur


old_time_on=Timer

For i1=1 To cnt


' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
 ' este AND  para iguales no va pero para distintos??? 
 If pasoCol(i1).liga = 0 Then  ' 13-06-2021
  ' de 91 a 180 son todos ligadso pero incluye los silencios ojo!
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 And pasoCol(i1).liga=0 Then
          
     If pasoCol(i1).liga =0 Then ' 13-06-2021
        pasoCol(i1).liga=1
        pasoCol(i1).old_time=old_time_on * d11
     Else
        pasoCol(i1).liga=pasoCol(i1).liga + 1 ' 13-06-2021
        ' no se carga old_time sigue siendo el mismo
     EndIf
     ligaglobal=1
     liga=1
     pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     Do
       nj=nj+1
       ' busca la proxima dur 
       If CANCIONCARGADA =TRUE Then
          durj = Track(pis).trk(nj, pasoCol(i1).i1 ).dur
       Else
          durj = Roll.trk(nj, pasoCol(i1).i1 ).dur
       EndIf
' problema con las octavas necesita 1 y solo vade 39 a 102 uuu
' en vez de 1 deberia ser 39         
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply '08-06-2021
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     vel= CUByte(vol( pasoCol(i1).DUR, velpos))
     if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
        vel=0
     EndIf   

     noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
     
  EndIf
 Else ' ya venia una ligadura ' 13-06-2021
 EndIf 
  If pasoCol(i1).liga = 0 Then 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
      EndIf
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0 And pasoCol(i1).DUR <> 181 Then 
    canal=pasoCol(i1).canal
    portsal=CUByte(pasoCol(i1).port)
    vel= CUByte(vol( pasoCol(i1).DUR, velpos))
    if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
       vel=0
    EndIf   

    noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
 End If
Next i1

	
 
End Sub

Sub AcordeOffIguales	(  pasoCol() As vec, cnt As UByte, cntold As UByte,Roll As inst, pis As UByte,portsal As UByte)
Dim As UByte i1,canal
Dim tiempoFigura As Double 
'debug..
' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

'aca son iguales ergo lso off caen todo iguales al mismo tiempo
' solo que una not aligada hara off en el siguiente paso
'--------


 old_time_off=Timer
 old_time_off_int = old_time_off * d11 ' solo para guardar y poder hacer sort
 
' son iguales las duraciones son las mismoas no ahce falta sort
' este 1er for tendra las duraciones mas cortas pues son acordes iguales 
' y si hay un acorde debe ser igual y si esta ligado a un acorde 
' anterior es mayor a este caso y sera el caso FOR siguiente
For i1 = 1 To cnt ' (1) 
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld=0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
     duracion pasoCol(i1).old_time / d11, tiempoFigura
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
       pasoCol(i1).tiempoFiguraOld=0
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
             'pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 

  EndIf


Next i1     

 Dim tf As Double
' los ligados pueden terminar en el paso 2 o seguir eso lo determino
' con el numero de liga qu epuede ser 1,2,3,4,etc
'
' duracion algo mayor 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld >0 Then
              'old_time= pasoCol(i1).old_time/100000000000 
    '''' V20 audioOl>=1 o sea 1 o 2
     If tipoAcorde=1 Or pasoCol(i1).audioOld =2 And pasoCol(i1).audio=1 Then        
       tf = pasoCol(i1).tiempoFigura  /d11
     Else
       tf = pasoCol(i1).tiempoFiguraOld  /d11
     EndIf
     ' el time tomo del inicio de aca del off porque ya gaste duracon de la 
     ' ligarura en el paso anterior
     If TF > 0 Then 
     ' la nota ligada es simple y ataca luego a una nota de un acorde
     ' se parte la duracion en un pedazo luego de la ligada en noteon
     ' y otro pedazo de la duracion o reardo luego de la resolucion 
       If pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura And pasoCol(i1).tiempoFigura > 0 Then
          If TipoAcorde=1 Then
            duracion old_time_off , tf
          EndIf  
       Else
         If pasoCol(i1).tiempoFiguraOld > pasoCol(i1).tiempoFigura Then 
  '''' V20 -> And pasoCol(i1).audio=2        
            If  tipoAcorde > 1 And pasoCol(i1).audioOld=1 And pasoCol(i1).audio=2 Then 'zzzzzzz
                duracion pasoCol(i1).old_timeold/d11, tf ' falta cambiar TF 16-11-2021
            Else
               If pasoCol(i1).audioOld=2 And pasoCol(i1).audio >=1 Then
                  duracion pasoCol(i1).old_time/d11,tf
               Else
                  If pasoCol(i1).audioOld=1 And pasoCol(i1).audio=1 Then
                     duracion pasoCol(i1).old_time/d11,tf
                  Else
                     duracion old_time_off , tf
                  EndIf
               EndIf      
            EndIf
         EndIf    
       EndIf   
'       Else
 ' la nota ligada es de un acorde y atca a otro acore se usa todo el retardo aca 
 '' 
  '        duracion pasoCol(i1).old_time /d11, tf   
    
 '      If CANCIONCARGADA Then
            canal=pasoCol(i1).canal
            portsal=CUByte(pasoCol(i1).port)
            noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
        '''  'liga cero no
       ' ''' EndIf
'       Else
             
'           noteoff CUByte(pasoCol(i1).notapiano) ,canal
         
'       EndIf
       
     EndIf
       pasoCol(i1).tiempoFiguraOld=0 ''' no sirve  24-11-2021
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
       '''pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 
       
  EndIf 

  ' le resto 1 a la liga para decir que paso este paso y en el proximo
  ' si llega a cero envia el noteoff entonces
'  pasoCol(i1).liga = pasoCol(i1).liga - 1 ' 05-11-2021
'  If pasoCol(i1).liga < 0 Then
'     pasoCol(i1).liga=0
'  EndIf
 '05-11-2021 comentado' ligaglobal = pasoCol(i1).liga ' por si sigue una simple

Next i1

For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
' and NOT 25-11
  If pasoCol(i1).liga > 0 And pasoCol(i1).ligaOld >0 Then ''And Not (pasoCol(i1).audio=1 And pasoCol(i1).audioOld=2)  Then
   '''''''''old_time= pasoCol(i1).old_time/100000000000 
     pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=pasoCol(i1).old_timeold
'------------audio en un solo for es suficiente
' en cada nota DEL ACORDE guardamos el audioold solo si esta ligado LA NOTA ACTUAL,
' al analizar debo preguntar si la nota recibe ligadura y si es asi
' por audioold de la nota anterior y si esa old es la misma nota nsE de la actual
' o sea la .i1old.
' Solo se guarda si la nota actual es ligada a la siguiente F+, en ese
' caso adectara a la siguiente nota  
     pasoCol(i1).audioOld = pasoCol(i1).audio 'no=1, si=2 
     pasoCol(i1).i1old = pasoCol(i1).i1  'un valor
  Else 
      ' note off agregado 25-11
      If pasoCol(i1).audio=2 And pasoCol(i1).audioOld=1  Then
         canal=pasoCol(i1).canal
         portsal=CUByte(pasoCol(i1).port)
         noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
        
      EndIf
  EndIf 
  
Next i1
' limpiando ligaduras 05-11-2021 por cancion	,
' ligalobal dice si al menos una nota esta ligada
'limpiarLigaduras(cnt,pasoCol())
'' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

End sub

Sub AcordeIguales ( pasoCol() As vec, cnt As UByte,cntold As UByte, vel as UByte, tiempoDur As Double,Roll As inst,velpos As Integer,pis As UByte,portsal As UByte) 
' todas las notas son de igual duracion, cnt cantidad de notas
AcordeOnIguales	 pasoCol() , cnt , cntold , vel,tiempoDur, Roll,velpos,pis,portsal

AcordeOffIguales	 pasoCol(), cnt , cntold , Roll,pis,portsal
' start  jmg 09-06-2021

''limpiarLigaduras (cnt,pasoCol())


End Sub

Sub AcordeOffDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte,tiempoDUR As Double,pis As UByte,portsal As UByte)
' si hay en cadena varios acordes y notas simples ligados
' el 1er acorde da el old_time_on, luego se suma toda su duracion
' a treves de la liga compelta...ese valor va el tiempoFiguraOld
' no cambia a tra ves de la ejecucion de los pasos de la liga
' en el paso final el retado es el total de la liga respecto
' del old_time_on del 1er acorde,,,asoi funciona el rtmidi...
'debug..
' NO DISPLAY veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)


Dim  As UByte i1, coff,canal
If cntold > cnt Then
  coff=cntold
Else
  coff=cnt  
EndIf
For i1=1 To coff 'reemplazo CNT 20-06-2021 JMG
Next i1

' en realida qsort no lo necesito es mejor usar algo como indiceaudio
' es solo una pasada qu em eindica el orden y chauuu una estupidez usar sort
' de todo el vector 
'For i1=1 To coff
'Next i1
   qsort(@pasoCol(1).tiempoFigura, cnt, SizeOf(vec), @QCompare )
'For i1=1 To coff
'Next i1   
'-----------------------------------------
'For i1=1 To coff 'CNT 20-06-2021 JMG
'Next i1

' ---------------------------------------
Dim As Double tiempoFigura


 old_time_off=Timer ' para notas no ligadas

Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
     duracion old_time_off, tiempoFigura
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * d11
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
     Else 
         old_time_on= pasoCol(i1).old_time/d11 '20-06-2021 habilitado
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /d11
         If TF > 0 Then ' usamos el old_time-on que venia de antes
           duracion old_time_on, tf
           If CANCIONCARGADA =TRUE Then
             If Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
             EndIf

           Else
             If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then

                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
             EndIf
           EndIf  
pasoCol(i1).tiempoFigura =0  '01-07-2021
pasoCol(i1).tiempoFiguraOld=0 '01-07-2021
pasoCol(i1).liga=0
           
         Else
           If (pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld) And pasoCol(i1).liga = 1  Then
              tf=tiempoFigMayorNoligado/d11 
              duracion old_time_on, tf
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFiguraOld   '- tiempoFigMayorNoligado  
 '    comentado  noteoff pasoCol(i1).notapiano ,canal ' 21-06-2021
           Else

              pasoCol(i1).liga=1 '13-06-2021 PARA QU ESIGA EN EL OTRO PASO SI NO SE BORRA....
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ' 13
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld '- tiempoFigMayorNoligado
           EndIf  
         EndIf 
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
     EndIf
     ''pasoCol(i1).liga=0 ' ya la use se va 05-11-2021   
  EndIf 

Next i1
''pasoCol(i1).tiempoFiguraOld=0 '13-06-2021

 ' observacion una ligadura de varios acordes en una nota dada podria
 ' terminar o tener una nota simple unica como final o intermedia ligada
 ' ver que pasa en ese caso como lo solucionamos
 ' 01-07-2021 ACA NUNCA SE BLAQUEA no-> ligaglobal=0  porque corta las ligas largas en un acorde distinto 

limpiarLigaduras(cnt,pasoCol())

End Sub



Sub AcordeDistintos (pasoCol() As vec, cnt As UByte, cntold As UByte,vel As UByte, tiempoDur As Double,Roll As inst,velpos As Integer, pis As UByte,portsal As UByte ) 
' Hay notas de sitinta duracion, cnt cantidad de notas
Dim i1 As UByte
AcordeOnDistintos  pasoCol(), cnt , cntold ,vel  ,tiempoDur,Roll,velpos,pis,portsal
AcordeOffDistintos pasoCol(), cnt , cntold ,tiempoDur,pis,portsal
limpiarLigaduras (cnt,pasoCol())

End Sub

Sub GrabaMidiPlano() ' graba eventos midi a txt
Print #1, "entra a GrabamidiPlano indicenotas ",indicenotas


'gp = FreeFile
gp=16
dim j1 As Integer
Dim As String cadena
Dim  As Integer punto= InStr(Nombre,".") 
 
cadena=Mid(Nombre,1,punto )+"txt" 
punto=InStrRev (cadena,"\")
cadena=Mid(cadena,punto+1)

'If Open (cadena For append As #gp ) > 0 Then

'Else
'   Sleep 100
'  If FileExists( cadena  ) Then
'  Else
'  End If

 
  
   For j1=1 To indicenotas
       Print #5, miditxt(j1).sumatiempo ;" ";

       If miditxt(j1).estado =1 Then     
          Print #5, " on ";
       EndIf
       If miditxt(j1).estado =0 Then     
          Print #5, " off";
       EndIf
       Print #5, "ch="; miditxt(j1).canal     ;" ";
  
       Print #5, "n=";miditxt(j1).nota      ;" ";
       Print #5, "v=";miditxt(j1).vel       





   Next j1

''End If

FileFlush(5)


 Close #5

'  If FileExists( cadena  ) Then
'  Else
'  End If


Sleep 1000

End Sub


Function sacarpath (s As String) As string

Dim n As Integer
 n= InStrRev (s,"\")


 
 sacarpath=mid(s,n+1)
 
End Function

Function tempoString (t As ubyte) As String  ' para imprimir en archivo midi txt
  Select Case  t  '''' TipoCompas As UByte
        Case  Tcompas2_4 
              tempoString = "2/4 24 8"
        Case  Tcompas3_4 
              tempoString = "3/4 24 8"
        Case  Tcompas4_4 
              tempoString = "4/4 24 8"   
        Case  Tcompas12_8 
              tempoString = "12/8 36 8"
        Case Tcompas6_8 
              tempoString = "6/8 36 8" 
        Case Else 
              tempoString = "4/4 24 8" 
  End Select

End Function 
'-------------playAll-----21-05-2021-------
Sub playAll(Roll As inst) ' play version 2


If MIDIFILEONOFF = HABILITAR  Then 
   MICROSEGUNDOS_POR_NEGRA = 60000000/tiempoPatron ' 60 MILL /BPM
   '' SE AJUSTO A 2000 PARA ESCUCHAR LO MISMO A 60 DSRG POR NEGRA... 500 
   '' NO ESTA CLARO EL  TEMPO EN BPM SON 60 EN TIEMPO 1000000 ,NI 500 NI 1000 NI 2000
   '' FALTRA ENTENDER MAS 
   indicenotas=0
   Dim As String NombreTrack, tiempo 
   If TipoCompas <> pmTk(0).tempo And TipoCompas > 0 Then
      tiempo = tempoString(TipoCompas)
   Else 
      tiempo = tempoString (pmTk(0).tempo)
   EndIf
   midiplano=20
   Dim numc As Integer = CInt(pmTk(0).canalsalida) + 1
   NombreTrack= sacarpath(titulos(ntk)) 
   Print #midiplano, "MFile 1 2 " + Str (1000)
   Print #midiplano, "MTrk"
   Print #midiplano, "0 Meta SeqName "; Chr(34);NombreTrack;Chr(34)
   Print #midiplano, "0 Meta Text "; chr(34);"Creado por RollMusic"; chr(34)
   Print #midiplano, "0 Tempo " + Str (MICROSEGUNDOS_POR_NEGRA)
   Print #midiplano, "0 TimeSig " + tiempo 
   Print #midiplano, "0 KeySig 0 Major"
   Print #midiplano, "0 Meta TrkEnd"
   Print #midiplano, "TrkEnd"
   Print #midiplano, "MTrk"
   Print #midiplano, "0 Meta 0x09 "; Chr(34); *nombreOut(0) ;Chr(34)
   Print #midiplano, "0 Meta TrkName "; Chr(34); "Piano";Chr(34)
   Print #midiplano, "0 PrCh  ch="+ Trim(Str(numc))+ " "; "p=";Roll.trk(1,NA).inst
     
EndIf
 
' tiempo es cuantas negras en un minuto tiempoPAtron
' PLAY mas avanzado en un mismo acorde si son de distinta duracion
' sus notas se toca cada una con su propia duracion,el corde no termina
' hasta que termine de tocar la nota mas larga.
 ' abrir hasta 32 dispositivos
'Dim Shared midiin As RtMidiInPtr
Dim As Integer porterror,nousar
porterror=Err
CONTROL1=0
Dim As Integer i1,k1
' creoa todos los defaults siempre
 

' los nombres ya fueron cargados al inicio
If  GrabarPenta=0  Then ' con 1 ya esta abierto


  
   k1=CInt(pmTk(0).portout)
   portout=k1
    
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( )
           listoutCreado( k1)=1 
        EndIf 
        open_port midiout(k1),k1, nombreOut(k1)
            porterror=Err 
        listoutAbierto( k1) = 1
        porterrorsub(porterror)
   Else
   EndIf
 EndIf 

'''''''midisal=midiout(0) ' el z
End If


'-------------------------------------------- 
 
    ''''''''midisal=midiout(pmTk(ntk).portout -1)
    
 
indEscala=1 ' inicializamos la guiade escalas a la 1era 

Dim As Double tiempoDUR, tiempoFigura=0,tiempoFiguraOld=0,old_time_old=0
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron '60 seg/ cuantas negras enun minuto
'Dim As Integer i1 
Dim As Integer i2,i3,i4,i5,j ,xmouse, ymouse,RepeIni,RepeFin,finalloop=0,comienzoloop=0
Dim As Integer comienzo=1, final=MaxPos,velpos =0,cntrepe,final2=0,comienzo2=0
Dim As UByte canal=0,vel=90 
' canal 0 es el 1 van d0 a 15
xmouse = mousex
ymouse = mousey

Dim As Double start
Dim as Integer cnt=0, cntold=0,cpar=0,nj, durj,tiempoFiguraSig
Dim As UByte dura=0,duraold=0
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim result As Integer
playloop2=0
' ==============> AJUSTE DE CANAL MIDI PARA UN ARCHIVO ROLL AISLADO <===========

'If canalx > 0 Then
'  canal = CUByte(canalx) 
  ' RECORDAR CANALX EN SELECCION VA DE 0 A 15 no hace falta convertir
 
'  pmTk(0).canalsalida=canal 
        
'Else
'  canal = pmTk(0).canalsalida  '12-02-2022
'  

'EndIf



 ''If Roll.trk(1,NA).inst > 0 Then
     ChangeProgram ( pmTk(0).patch, pmTk(0).canalsalida, pmTk(0).portout)
 ''   ChangeProgram ( Roll.trk(1,NA).inst, pmTk(0).canalsalida, pmTk(0).portout)	
    patchsal =pmTk(0).patch ''Roll.trk(1,NA).inst
 '''EndIf

jply=0:curpos=0
mousex=0
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

' If jply=1 And Roll.trk(1,NA).inst > 0 Then
'   ChangeProgram ( Roll.trk(1,NA).inst , 0)
  ''        On Error GoTo labelerror 
' End If 
'        canal = pmTk(0).canalsalida  '12-02-2022
'        portsal= pmTk(0).portout
STARTMIDI=Timer

For jply=comienzo To final

' If Roll.trk(1,NA).inst > 0 And jply=1 Then
'       ChangeProgram ( Roll.trk(1,NA).inst , 0)
' EndIf

kNroCol= Int(jply/NroCol)
If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < MaxPos)Then
   posicion=jply
   curpos=0
   SetMouse xmouse, ymouse
EndIf


 mousex=jply
 If CONTROL1 = 1 Then
   If InStr(*nombreOut(portout),"Microsoft") > 0 Then
   Else
     alloff( canal,portsal )
   EndIf  
      CONTROL1=0
      Exit For
 EndIf  
' puede pasar que el maxpos sea menro al final de la secuencia porque se agrego espacio
 If jply=MaxPos Then
   If InStr(*nombreOut(portout),"Microsoft") > 0 Then
   Else
    alloff( canal,portsal)
   EndIf 
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
' AL FRACTURAR COLUMNAS LAS PARTES PUEDE QUEDAR SIN SONIDO AL DAR PLAY AUNQUE
' LAS NOTA TENGAN SUS VOLUMENES PORQUE ACA SE DA LA VELOCIDAD DE EJECUCION
' LA CUAL DEBERIA DEPENDER DEL VOL DE LA NOTA TAMBIEN VERIFICAR,,,EN CAL COMPAS O RECAL COMPAS
 If Compas(jply).nro = 0 Then 
    velpos=vsemifuerte  ' para midipolano dividiones por partes veremso si se soluciona el sonido
' en la rutina vol , depende de la dur ajusta vol=0 o vol = velpos... no hay problema con los silencios
 EndIf

' ojo con silencios ligados !!!
  cnt=0
  iguales=0
  distintos=0
  duraold=0 ' 04-11-2021 jmg

  '115 a 0
  ' recorre una posicion vertical
  ' envio de instrumetno o CAMBIO de PROGRAMA PATCH 
  
  ' ============================== 18-08-2021 funciono!!!!
  ' anda mejor qu ela V4 solo que la ligadura I+I+I la toca como I+I
  ' LE FALTA UNA NEGRA DE DURACION, EL RESTO LO TOCA BIEN!!!
  ' SIN TOCAR CASI NADA SOLO ELIMINAR EL ANALISIS DE LIGA EN PLAYALL
  ' SEGUIR CORREGIR CON EL USO DE LSO CAMPOS NUEVOS Y AL TERMINAR 
  ' ELIMINAR LOS CAMPOS DE VEC QUE NO SE USEN
  '=========================================================================
  ' ACA BUSCO SI EN UN PARAMETRO DADO FIJO SOLO DEPENDIENTE DE CUAL ES LA ULTIMA OCTAVA ,
  ' SI LA POSICION ACTUAL ENTRE NA-13 Y HASTA NA TIENE UN COMADNO DE REPETICION 
  ' DE COMPASES Y SI LO HAY CONDICIONO EL PLAY DE MAS ABAJO..
  ' If Roll() tiene inicio derepetiiocn then
  ' simular un pasozona1 pero con repeticion ponemo sun comienzo de loop
  ' al llegar a donde este el pasozona2 no sindicara la cantiad de repetiicones y fin del
  ' loop o sea determinamoe el final del loop y sacmaos el factor N de numeros de repeticiones
  ' con un contado rdescndente y al final se ahce comienzo y final = 0 y la secuencia
  ' seguira el play del resto... 
  For i1=NB To NA 
  ' If Roll.trk(jply, i1).dur = 182 or Roll.trk(jply, i1).nota=182  Then
  '      Continue For 
  ' EndIf 

If i1<= NA-13 Then

   If (Roll.trk(jply, i1).nota >= 1) And Roll.trk(jply, i1).nota <= 12 _
      And Roll.trk(jply, i1).dur >=1 And Roll.trk(jply, i1).dur <= 180 Then ' es semitono
     ' por mas que achique en octavas, Notapiano se calcula respecto del nro 
     ' completo de octavas del piano ergo 115 es fijo siempre mientras
        
      Notapiano= i1
      'Notapiano= 115 - i1 
      Notapiano= Notapiano - restar (Notapiano)
      dura=Roll.trk(jply, i1).dur '1) I 2) I dur x 1 to 108
      cnt=cnt+1
      If cnt=1 Then 
         duraOld=dura
      EndIf
      ' 04-11-2021 usamos reldur para comparar duraciones !!!
      If reldur(duraOld)=reldur(dura)  And cnt > 1 Then
         iguales=1
      EndIf
      If reldur(duraOld)<>reldur(dura)  And cnt > 1 Then
         distintos=1 ' atrapa no importa cuantos elementos tenga el acorde
      EndIf         

     
      '+++++++++
  
        pasoCol(cnt).DUR    =dura
 '       pasoCol(cnt).DURold =dura
        ' DURACIONE CON LIGA O SIN LIGA EJ F+ O F 
        If pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <=180 Then
            pasoCol(cnt).liga =  1 'si es la primera debe tener sonido!!!
        Else
            pasoCol(cnt).liga =  0   
        EndIf
        ' DURACIOENS SILENCIO O NO sF o sF+
        If (pasoCol(cnt).DUR >= 1 And pasoCol(cnt).DUR <=45) Or (pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <= 135) Then
            pasoCol(cnt).audio =  1 ' tiene audio
        Else
            pasoCol(cnt).audio =  2 ' no tiene audio, 0 valor no ajustado no se nada
        EndIf
        
' debo saber si la nota anterior con ligaold   
' tenia audio o era silencio porque si era silencio y la actual tiene audio debo 
' enviar un noteon en la nota actual ver en subrutinas, donde pongo audioOld? al final de los acordes?
' no al final de cada nota, porque el analis de acorde es vertical, pero el de 
' audio es horizontal se agrega el old en el ON de cada tipo de acorde, como figuraold
' o sea que sea la misma posicion horizontal i1 me garantiza que es la misma nota nsE
' que estan ligadas, puede ser que la anterior este ligada pero sin audio y la 
' actual que recibe la ligadura tenga audio => envio el noteon
' en que momento cargo la i1old? al final
        pasoCol(cnt).notapiano = Notapiano 
' >>>>>>>>>>>>>>>CANAL MIDI y POR SALIDA >>>>>>>>>>>>>>>>>>>>>>>>        
         pasoCol(cnt).canal=pmTk(0).canalsalida ' 12-02-2022 canal en pasoCol
         pasoCol(cnt).port=pmTk(0).portout
      
 ' >>>>>>>>>>>>>>>CANAL MIDI >>>>>>>>>>>>>>>>>>>>>>>>         
       
        pasoCol(cnt).tiempoFigura    = relDur(pasoCol(cnt).DUR) * tiempoDur * d11
    
        pasoCol(cnt).i1    = i1 'posicion vertical en el vector real
  '      pasoCol(cnt).i1old = i1 'posicion vertical en el vector real
      ' 20-06-2021 eliminado duraold=dura repetido    
        '' vel= vol( dura, velpos) 02-11-2021
' llegamos al final de la Columna
        pasoCol(cnt).vol=velpos
   EndIf   
      If i1=NA -13  Then 'And cnt >= 1 Then ' envio noteoff 1) no entra

    '''' ya nohace falta     mouse_event MOUSEEVENTF_MOVE, 1, 0, 0, 0
         If cnt > 1 Then' Acorde
         Else    
         EndIf  

         Select Case cnt
          Case 1 
 ' 04-11-2021 cnt por cntold ....aca|
          TipoAcorde=1 ' simple   
          AcordeIguales pasoCol(),cnt,cntold,vel,tiempoDur,Roll,velpos,0,0
          pasoCol(cnt).notapianoOld    = Notapiano             
          Case Is > 1
            If iguales=1 And distintos=0  Then
                TipoAcorde=2 ' iguales
                AcordeIguales pasoCol(),cnt,cntold,vel,tiempoDur,Roll,velpos,0,0
                
            EndIf
            If  distintos=1 Then
               TipoAcorde=3 ' distintos
                 AcordeDistintos pasoCol(),cnt, cntold,vel,tiempoDur,Roll,velpos,0,0
                
            EndIf
            
         End Select  

        cntold = cnt
        
        
      EndIf
EndIf  

If i1 > NA-13 Then
 If Roll.trk(jply,i1).nota = 210 Then
    playloop2=1
    comienzo2=jply
 EndIf

 If Roll.trk(jply,i1).nota = 211 Then
    final2=jply
    If cntrepe > 0 Then
      cntrepe -= 1
    Else
      cntrepe=Roll.trk(jply,i1).vol ' nro repeticiones en vertical +1
    EndIf
    If cntrepe =0 Then
       'comienzo=final+1
       final2=MaxPos
       If finalloop> 0 Then
           final2=finalloop
       EndIf
    EndIf 
 EndIf
EndIf

 
  Next i1
  '''  ya no hace falta mouse_event MOUSEEVENTF_MOVE, 1, 0, 0, 0
  
 
 If playloop=1 And jply= finalloop Then
    jply=comienzoloop -1
 EndIf
 If playloop2=1 And jply= final2 Then
    jply=comienzo2 -1
    If final2=finalloop Then 
       If playloop=1 Then
         jply=comienzoloop -1
       Else
         final=MaxPos
         final2=Maxpos 
         jply=final2 
       EndIf
    EndIf
 EndIf

 tiempoDUR=(60/tiempoPatron) / FactortiempoPatron '13-07-2021 cambiamos velocidad durante el play!!!
 Sleep 1,1 ' para que corranmas de un thread
Next jply
''while (PeekMessage(NULL, hwnd, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE or PM_QS_INPUT))
''Wend

posicion=comienzo
'======================
' IF hay loop de repeticion se detecta en la posicion final Then
' posicion = comienzorepe , con esto vuelvo a repetir un pedazo
' o sea el final es el que me indica desde donde debo repetir solo ahce falta 
' insertar el final en Roll con 2 parametros dede donde repetir cuantas veces y este es el final
' =========================l listo algoritmo mañana implemenamos
'  
jply=0:curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
play=0 
playb=0

mousey=100 'otra mas para evitar rentrar a play en menu
SetMouse xmouse, ymouse
finplay=1

'''mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0

Dim As Integer checkejec
For  iz As Short =1 To 32
      If CheckBox_GetCheck( cbxejec(iz))= 1  Or CheckBox_GetCheck( cbxgrab(iz))= 1 Then
          checkejec=1
      End If
      Exit For
Next iz
if GrabarPenta=0 and GrabarEjec=HabilitaGrabar and repro=0 And checkejec=0 Then 
 ' nada de off estamos en grabarpenta por teclado o Grabar o tocar ejecuciones 

   k1=pmTk(0).portout
   alloff( pmTk(0).canalsalida,k1 )  
   'out_free   midiout(k1)
   listoutAbierto(k1)=0
   close_port midiout(k1)
   ''out_free   midiout(k1)

EndIf
If MIDIFILEONOFF = HABILITAR Then 
   Dim As Double TiempoAcumNew
   Dim As Integer T1

   TiempoAcumNew= Timer - STARTMIDI
   T1 =  TiempoAcumNew *  1000 * tiempoPatron/60
   ''GrabaMidiPlano()
   Print #midiplano, T1 ;" Meta TrkEnd"
   Print #midiplano, "TrkEnd"
   MIDIFILEONOFF = DESHABILITAR
   cerrar (20)

EndIf

Sleep 100,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion



ThreadDetach(thread2) ' 16-06-2022

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
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron'60 seg/ cuantas negras enun minuto

'''midiin  = rtmidi_in_create_default()
'midiout = rtmidi_out_create_default()


'''portsin  =  port_count (midiin)
'portsout =  port_count (midiout)
'Dim nombrez As ZString Ptr ' nombre local


'Dim i As INTeger
'for i = 0 to portsout -1 
'    nombrez = port_name(midiout, i)
'Next   

'For i = 0 to  portsin -1  
'    nombre = port_name(midiin, i)
'Next

Dim leng As UInteger <8>
Dim  As Integer result ,i

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
' ergo si toco 4 5 6 notas a la vez secuecnialmente talvez el timepo
' en ejecutarlo es muy rapido y pareceria un acorde ¿? ok o no¿?
' bueno en realidad las pongo juntas en el buffer y luego las separo en
' el menssage para send_message....peroel tiempo q tardo enponerlo es
' masqu eenviarlo diretamente
Dim As Integer final=MaxPos  , comienzo=1, notapiano, canal=1,vel=100,j
Dim As Integer con=0,tiempo,ioff,cx=0
Dim As UByte  dura=0,maxdur=0,durb
Dim As Integer non(1 To 180), liga=0,x=0, durval (1 To 45), silencio, fin, inicio
Dim As float durl
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
 'COMEDIT=TRUE
'Dim As tEvent Ptr newEvent
jply=0:curpos=0
mousex=0

For jply=comienzo To final
 If curpos > NroCol  Then
    curpos=0
    posishow=0
 EndIf

 mousex=jply
 If CONTROL1 = 1 Then
  ' allSoundoff( canal,portsal )
   alloff( canal,portsal)
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
         liga=1
         durb=0
         durl=0
      EndIf   
      If dura >= 91 And dura <=180 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc...
' SACAR ESTO TOCAR ACORDE CADA ELEMENTO CON SU DURACION        
      If relDur(dura) > relDur(maxdur) Then ' esto lo debo sacar y tocar todas las notas con su duracion
         maxdur= dura ' 1) I, 2) P cuantoms chica dur es mas grnde en relidd
      Else    
        '' notacur=i

      EndIf 
      If liga=0 Then  
        vel=vol(maxdur,velpos)
       
 ' SI ELUSUARIO GRABA VELOCIDADES DEBO USARESA NO LA DEFAULT !!! JMG
 ' Y NO SACRLA MAAX DURACION TOCAR TODAS CON SU DURCION Y VELOCIDAD!!!       
  ''      canal=pasoCol(i1).canal
        portsal=pmTk(ntk).portout
        noteon notapiano, vel, canal,portsal,1 ' 1) G
 
        cx = cx + 1   ' 1) 1
        non (cx) = notapiano '1) G
      ''''''  Sleep 1,1
        old_time_on=Timer
      Else
        liga=0 
      EndIf 
   EndIf

   If i=NB And durb = 0 Then ' envio noteoff 1) no entra
 ''Sleep segun duracion o Timer de la q mas dura o para cada uno
      ' tiempoPatron input al redimsub
 '   If maxdur > 0 And maxdur <= 182 Then
 '   Else 
 '   EndIf   
    
  ''''   duracion (maxdur)
'''' DURACION  

 If maxdur >= 1 And maxdur<= 180 Then 
    tiempoFigura = relDur(maxdur)*tiempoDUR
 
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
     
     For ioff=1 To cx
     portsal=pmTk(ntk).portout  
     noteoff non(ioff),canal,portsal,1

     Next ioff
   EndIf 
  Next i

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
For i=0 To portsout -1 
 close_port(midiout(i))
 out_free(midiout(i))
Next i 

play=0 
playb=0
mousey=100 'otra mas para evitar rentrar a play en menu
finplay=1
'if finplay=1 Then
 ThreadDetach(thread2)
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
' sale Notapiano dsde el nR indice vector,tambien sale la octava -> restar(i1) +1 
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

Function SumarnR (notaPiano As integer) As integer
' dado la NotaPiano encuentro nR el indice del Vector Roll de visualizacion
' puedo devolver la octava tambien
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

Function SumarnRk (notaPiano As ubyte) As ubyte
' dado la NotaPiano encuentro nR el indice del Vector Roll de visualizacion
' puedo devolver la octava tambien
Select Case notaPiano
   Case 0 To 11
     SumarnRk=0
   Case 12 To 23
     SumarnRk=1
   Case 24 To 35
     SumarnRk= 2
   Case 36 To 47
     SumarnRk = 3
   Case 48 To 59
     SumarnRk =4
   Case 60 To 71
     SumarnRk= 5
   Case 72 To 83
     SumarnRk=6
   Case 84 To 95
     SumarnRk= 7
   Case 96 To 107
     SumarnRk=8         
   Case 108 To 119
     SumarnRk=9         
   Case 120 To 131
     SumarnRk=10         
       
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
'Static As Double start
Static as LARGE_INTEGER delay 
delay.QuadPart = -1 
  Do
    NtDelayExecution(FALSE,@delay)
  Loop Until Timer - old_time >= tiempoFigura

End Sub

Sub duracionokOLD (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off dejo de andar porque ???
Dim As Double  endtime 
 
  Do
    Sleep 1
  Loop Until Timer - old_time >= tiempoFigura

End Sub


Sub listports( )


If  portsin=0 Then
    portsin=2
EndIf
If  portsout=0 Then
    portsout=2
EndIf

ReDim listout(0 To portsout -1)
ReDim listin (0 To portsin  -1) 
' saco de la lista los ports ya abiertos

Dim nombre As ZString ptr
Dim aviso As String = " Abierto"
Dim temp As String=""
Dim lg As Integer  
' "Output port"

Dim  As Integer i,j
If  portsout= 0 Then 
  Exit Sub
EndIf 
for i = 0 to portsout -1 
  If listoutAbierto (i) =0 Then
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then ' microsoft no funa bien
      listout(i) = "Crash No usar Microsoft" 

    Else
     listout(i) = *nombre
    endif
  EndIf  
  If listoutAbierto (i) =1 Then
'    lg=Len(*port_name(, i))
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then
      listout(i) = "Crash No usar Microsoft" 

    Else
     listout(i)=*nombreOut(i) +aviso 
    EndIf
    
  EndIf  
  
Next
temp=""
' entradas copia
If  UBound (nombreIn,1) > 0 Then
    for i = 0 to portsin -1 
      If listInAbierto (i) =0 Then
        nombre = nombreIn(i)
        If InStr(*nombre,"Microsoft") > 0 Then ' microsoft no funa bien
          listin(i) = "Crash No usar Microsoft" 
    
        Else
         listin(i) = *nombre
        endif
      EndIf  
      If listInAbierto (i) =1 Then
        nombre = nombreIn(i)
        If InStr(*nombre,"Microsoft") > 0 Then
          listin(i) = "Crash No usar Microsoft" 
        Else
        
        listin(i)=*nombreIn(i) +aviso 
         
        EndIf
        
      EndIf  
      
    Next i
Else
listin(0)="No hay ports de entrada"
End If 
temp=""



End Sub
Sub TrasponerGrupo( cant As Integer, Roll As inst, encancion As Integer)
' ANDA BIEN, ES EQUIVALENT EEMPEIZA EN EL EXTREMO QUE ATACA BAJANDO LA POSICION
' DE LA COPIA ES LO MISMO PERO INVERTIDO FUNCIONA IGUAL, LO IMPORTANE DEL CAMBIO
' FUE EN LA SUBRUTUNA SUMAR COMO EL VECTOR EMPIEZA DE CERO 0, EL ESPACIO ENTRE
' OCTAVAS NO QUEDA MULTIPLO DE 13 ERGO LE SUMO 1 AHORA,,,ANTES DE AHCER EL MOD 13
' 31-01-2022 CORREGIDO EN BASE A tRASPONERROLL SEGUN OCTAVA NUEVA TIENE SOLO UN 
' SOLO TRASPONE DENTRO DE LA MISMA OCTAVA HAY QUE VER SI PODEMOS HACER LO MISMO
' QUE CON TRASPONERROLL Y MOVER A OTROS OCTAVAS....

Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,b1=0
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha DOWN
If cant < 0 Then ' DOWN
 comienzo= NB 
 final = NA -13  
 inc= 1
EndIf
If cant > 0 Then 'UP
 comienzo= NA -13
 final = NB  
 inc=  -1
EndIf

Dim As Integer desdet, hastat
   desdet=1   
   hastat= MaxPos   
pasoNota=13 ' es 12 25 38 en el vector real 
' 30-01-2022 CORREGIDO en base a la verion ROLLMUSIC-0.1.0.0.0-U-TRACKS 
For jpt = desdet To hastat  
  For i1= comienzo To final Step inc
     If cant > 0 Then  ' UP  
        ind = i1 + cant 
        ind = ind + sumar(ind)
     EndIf

     If cant < 0 Then  ' DOWN  
        ind = i1+cant 
        ind = ind - sumar(ind)
     EndIf
   
    If ( (Roll.trk(jpt, i1).nota >= 0) And Roll.trk(jpt, i1).nota <= 181 ) _
       OR (Roll.trk(jpt, i1).dur >=0 And Roll.trk(jpt, i1).dur <= 181 ) Then ' es semitono
       
       If ind >= NB And ind <= NA -13 Then
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
                   If cant > 0 Then  ' UP  
                      ind = ind + cant 
                      ind = ind + sumar(ind)
                   EndIf

                   If cant < 0 Then  ' DOWN  
                      ind = ind + cant 
                      ind = ind - sumar(ind)
                   EndIf

                  if ind > NA -13 Then
                     ind=NA -13
                  EndIf
                  If ind < NB Then
                     ind=NB
                  EndIf
                  b1=1
 ' CON ESTE IF SE CORRIGE QU ENOMUEVA OTRA COSA QUE LAS NOTAS CON 13
 ' PERO TODAVIA SOLO MUEVE DENTRO DE UNA OCTAVA NO VA MAS HALLA-....                 
   If Roll.trk(jpt,ind).nota > 0 And Roll.trk(jpt,ind).nota <= 13  Then '31-01-2022    
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
    EndIf
  Next i1
Next jpt
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


''trasponer=0   
End Sub
'---------
Sub trasponerRoll( cant As Integer, Roll As inst, encancion As integer)
'AJSUTADO DE NUEVO 11-09-2021 CON EL NUEVO ALGORITMO DE OCTAVAS
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,octavaDeAcorde,verticalEnOctavaVacia  

' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(1) si pulso flecha UP
  If trasponer=0 Then
     Exit Sub
  EndIf
  
If cant < 0 Then ' DOWN
 comienzo= NB
 final = NA  -13 '30-01-2022 NA->NA -13
 inc= 1
EndIf
If cant > 0 Then 'UP
 comienzo= NA -13
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

For jpt = desdet To hastat  ' eje x posiciones
  For i1= comienzo To final Step inc ' indice roll nR vertical
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
       If Roll.trk(jpt, i1).pb = 201 Then ''ÇÇÇ cuando traspongo muevo el cifrado ...¿?
           octavaDeAcorde=1+ (i1-12)/13
           verticalEnOctavaVacia= 12 + (hasta-2)*13 + octavaDeAcorde - desde  
           If cant> 0 Then 
             Roll.trk(jpt, verticalEnOctavaVacia).nota=Roll.trk(jpt, verticalEnOctavaVacia).nota - 1
           Else
             Roll.trk(jpt, verticalEnOctavaVacia).nota=Roll.trk(jpt, verticalEnOctavaVacia).nota + 1
           EndIf
           Continue For
       EndIf  
  
       If ind >= NB And ind <= NA  -13 Then
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

                  if ind > NA -13Then
                     ind=NA -13
                  EndIf
                  If ind < NB Then
                     ind=NB
                  EndIf
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
    EndIf
  Next i1
Next jpt

''trasponer=0   
' para trasponer tracks debo grabar lo cual copia a track los cambios
' de ese modo al dar play se escucha los cambios sino solo quedan en Roll
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
  
' si movemos a derecha empezamos copiando a la nueva posicion el final de 
' la secuencia, luego en la nueva posicion -1 copiamos el final -1
' asi desde el final haci aadelante...
' si movemos a izqierda el reves..
' o sea lo que est  echo es para mover a izquierda donde la posiion destino
' el click esta a la izquierda de pasozona1  
/'
For jpt= 1 To maxpos
  For i1=comienzo To final
       If Roll.trk(jpt,i1).nota < 13 And i1= 67 Then

       EndIf

  Next i1
Next jpt
'/
If posinueva > Maxpos Then ' movemos a izquierda
inc=posinueva
  For jpt=desdet To hastat
       
     For  i1= comienzo To final
       Roll.trk(inc,i1).nota = Roll.trk(jpt,i1).nota
       Roll.trk(inc,i1).dur  = Roll.trk(jpt,i1).dur
       Roll.trk(inc,i1).vol  = Roll.trk(jpt,i1).vol
       Roll.trk(inc,i1).pan  = Roll.trk(jpt,i1).pan
       Roll.trk(inc,i1).pb   = Roll.trk(jpt,i1).pb
       Roll.trk(inc,i1).inst = Roll.trk(jpt,i1).inst
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
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
  If posinueva > MAxPosOld Then

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
' aca el maxpos deberia achicarse....
MaxPos=inc +1
'-------------------------------
Else ' if ind posiion nueva > pasozona1 movemos a derecha
'---------------------------------

  If posinueva < MaxPos then
    MaxPos=MaxposOld + posinueva - posivieja
  EndIf
 
  hastat=Maxpos
  desdet=posivieja+1
  inc=MaxPosOld
  '        mAXpOS TO  POSIVIEJA+1          inc=MAxPosOld   
  For jpt= hastat To desdet Step -1
     For  i1= comienzo To final
       Roll.trk(jpt,i1).nota = Roll.trk(inc,i1).nota
       Roll.trk(jpt,i1).dur  = Roll.trk(inc,i1).dur
       Roll.trk(jpt,i1).vol  = Roll.trk(inc,i1).vol
       Roll.trk(jpt,i1).pan  = Roll.trk(inc,i1).pan
       Roll.trk(jpt,i1).pb   = Roll.trk(inc,i1).pb
       Roll.trk(jpt,i1).inst = Roll.trk(inc,i1).inst

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
'---
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
  'If posinueva > MAxPosOld Then

  '   For jpt=MaxPosOld To posinueva  
  '     For  i1= comienzo To final
  '        Roll.trk(jpt,i1).nota = 181
  '        Roll.trk(jpt,i1).dur  = 0
  '        Roll.trk(jpt,i1).vol  = 0
  '        Roll.trk(jpt,i1).pan  = 0
  '        Roll.trk(jpt,i1).pb   = 0
  '        Roll.trk(jpt,i1).inst = 0
 ' 
 '      Next i1
 '    Next jpt

  'EndIf


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
For jpt = desdet To hastat  
  For i1= NB To NA -13 ' 26-01-2022  
   
     If ( (Roll.trk(jpt,i1).nota >= 0) And (Roll.trk(jpt,i1).nota <= 13 ) )  Then ' es semitono
           i2= i1 - restar (i1)
          If  Roll.trk(jpt,i1).nota <> relnRNe (i2) Then 
              Roll.trk(jpt,i1).nota = relnRNe (i2)
          EndIf    
    EndIf
  Next i1
Next jpt



End sub
' 06-09-2021 jmg
'
'
Function RollCallback ( ByVal delta As double, ByVal vec1 As UByte Ptr, ByVal leng as UInteger<64>, ByVal otro As Any ptr ) as RtMidiCCallback 
'LOS PORTS Y CANLAES ESTAN EN PMTK PORQUE USA ROLL
' LAS PARTES Y D ATOS SE GRABAN EN TOCAPARAM(1) QUE SERAN CONVERTIDOS
' A ROLL EN EL NUEVO ALGORITMO...18-11-2024
Dim As UByte Ptr memoria = vec1
Dim As Integer partes
dato1=*memoria: memoria += 1
dato2=*memoria: memoria += 1  
dato3=*memoria 
Dim As Double sumadelta=0
'    If GrabarPenta=1 Then
'       nRk=dato2
'       PianoNota=CInt(nRk)
'       
'    EndIf

' 
' los datos midi llegan en tiempo real CON TIEMPO ENTRE EVENTOS DELTA
' Y A MEDIDA QUE LLEGAN SE REPRODUCEN TODAVIA NO ESTAN CARGADOS EN UNVECTOR
' ACA SE CARGA EN UN VECTOR (podra ser Roll)
partes=delta/TickChico
     Select Case  dato1 
         Case 144 ' on
            noteon dato2,dato3,pmTk(0).canalsalida, pmTk(0).portout, 1

           
         Case 128 'off
            noteoff dato2,pmTk(0).canalsalida,pmTk(0).portout,1 'message(2)'

     End Select

' tick mas chico es 0.005208325 (ver [TickChico] en RTMIDIDEC)
' ergo divido timestamp por ese valor y obtengo la cantiad de divisiones
' que ocupara ese retardo timestamp/TickChico
 
' ENTRE EVENTOS
     jgrbRoll += 1
     If jgrbRoll=1 Then ' detiene la acumulacion de timestamp en PlayTocaAll 
        arrancaPlay=1
 
     EndIf
      
  '   CargaInRoll( jgrbRoll).modo=dato1 '144 o 128
  '   CargaInRoll( jgrbRoll).nota=dato2 ' Piano nota
  '   CargaInRoll( jgrbRoll).vel=dato3  ' velocidad
 
   If delta > 0  Then   
      notamidi(jgrbRoll).partes=partes
     'CargaInRoll( jgrbRoll).partes=partes ' o cantidad de Ticks QUE HAY 
      tocaparam(0).maxpos=tocaparam(0).maxpos+partes
   Else
      notamidi(jgrbRoll).partes=0
     ' CargaInRoll(jgrbRoll).partes=0
      tocaparam(0).maxpos=tocaparam(0).maxpos +1
   EndIf

'---------------------------------------------------------
Dim As Integer i1
'''esto debe ser una estructura pasada por dato usuario de rollcallback
DURK=delta
If dato1=144 Then
  DURK144=delta ' es DURK
EndIf
If dato1=128 Then
  DURK128=delta
EndIf

'' Print DURK,dato1,dato2,dato3
   Select Case dato1
      Case 144 'on
        If DURK144 >= 0.005 or DURK144 > 0 Then 'el 1er evento durk144=0  es valido
          cont144=cont144+1
          contid=contid+1
          notamidi(contid).dato1=dato1
          notamidi(contid).nota =dato2 'pianonota
          notamidi(contid).vel  =dato3 ' velocidad  
          notamidi(contid).durk = DURK  ' para detectar acordes
      ' Print "cont144 ";cont144  

          Select Case cont144
            Case 12 
            durafig(11)=DURK144
            durafig(10)=durafig(10)+ DURK
            durafig(9)= durafig(9) + DURK
            durafig(8)= durafig(8) + DURK
            durafig(7)= durafig(7) + DURK 
            durafig(6)= durafig(6) + DURK
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 11
            durafig(10)=DURK144
            durafig(9)= durafig(9) + DURK
            durafig(8)= durafig(8) + DURK
            durafig(7)= durafig(7) + DURK 
            durafig(6)= durafig(6) + DURK
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 10
            durafig(9)= DURK144
            durafig(8)= durafig(8) + DURK
            durafig(7)= durafig(7) + DURK 
            durafig(6)= durafig(6) + DURK
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 9
            durafig(8)= DURK144
            durafig(7)= durafig(7) + DURK 
            durafig(6)= durafig(6) + DURK
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 8
            durafig(7)= DURK144 
            durafig(6)= durafig(6) + DURK
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 7
            durafig(6)= DURK144
            durafig(5)= durafig(5) + DURK
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 6
            durafig(5)= DURK144
            durafig(4)= durafig(4) + DURK
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 5
            durafig(4)= DURK144
            durafig(3)= durafig(3) + DURK
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 4
            durafig(3)= DURK144
            durafig(2)= durafig(2) + DURK
            durafig(1)= durafig(1) + DURK
   
             Case 3
            durafig(2)= DURK144 
            durafig(1)= durafig(1)+ DURK
             Case 2
            durafig(1)= durafig(1)+ DURK
             Case 1
            durafig(1)= DURK144
          End Select
          DURK144old=DURK144
       End If 
       'Print "durafig(1) "; durafig(1)
      Case 128 ' off
        If DURK128 >= 0.005   Then
           cont128=cont128+1
           contid =contid +1
          notamidi(contid).dato1=dato1
          notamidi(contid).nota =dato2 'pianonota
          notamidi(contid).vel  =dato3 ' velocidad    
          notamidi(contid).durk = DURK ' para detectar acordes
          'Print "cont128 ";cont128
           Select Case cont128
            Case 1
           durafig(1)=durafig(1)+DURK
           durafig(2)=durafig(2)+DURK
           durafig(3)=durafig(3)+DURK 
           durafig(4)=durafig(4)+DURK
            Case 2
           durafig(2)=durafig(2)+DURK
           durafig(3)=durafig(3)+DURK 
           durafig(4)=durafig(4)+DURK
            Case 3
           durafig(3)=durafig(3)+DURK
           durafig(4)=durafig(4)+DURK
            Case 4
           durafig(4)=durafig(4)+DURK
   
           End Select  
           DURK128old=DURK128
       'Print "durafig(1) "; durafig(1)
        End If  
     End Select

     If cont144=cont128 Then
   
'      For i1= 1 To cont128
'         Print "durafig nota ";nota(i1); " ";durafig(i1) 
'      Next i1    
        If contid <> cont144 *  2 Then ' error
           huboerror=1
        Else
           huboerror=0  ' HABILITA EL PROCESAMIENTO  
        EndIf  
        cont144=0
        cont128=0
        If contid > 24 Then
           huboerror=2
           contid = 24 
        EndIf
     'Else
     '   cantnotas=0 
     EndIf  
  


End Function
'-------------

Function mycallback ( ByVal deltatime As double, ByVal vec As UByte Ptr, ByVal leng as UInteger<64>, ByVal otro As Any ptr ) as RtMidiCCallback
' en otro podre poner un ptr a Toca...
Dim As UByte Ptr memoria = vec
dato1=*memoria: memoria += 1
dato2=*memoria: memoria += 1  
dato3=*memoria 
DURk =deltatime

Dim As Double sumadelta=0
    If GrabarPenta=1 Then
       nRk=dato2
       PianoNota=CInt(nRk)
       
    EndIf

dim i As Integer
Dim As Integer partes , traba=0
  
' jgrb global por ahora

/'
  For i =1 To leng
     Print " Byte "; i ; "=", *memoria ;
     memoria=memoria+1
  Next i  
  If leng > 1 Then
   Print deltatime
  EndIf
'/
'--------------play de lo que entre

' velocidad I=240 -> t=60/240=1/4=0,25 la negra, para llegar a W / por 2 6 veces= 0,00390625
' cuantos ticks en tiempoPatron=60?
't de negra= 60/velocidad
' cant  ticks= t negra/ t ticks= (60/velocidad)/0.005
' para v=60 = 1/0.005=200 ticks para una negra -> un compas tendra 400 ticks
' para v=240= (60/240) /0.005= 50 ticks para una negra -> un compas 200 ticks
'para v=240. ->I=50ticks, L=25,F=12.5, E=6.25, X=3.125
' sino puedo decir que grabo siempre a la misma velocidad y cambio en la reproduccion..

      '    If GrabarEjec=GrabarPistaEjecucion Then 
             ChangeProgram ( tocaparam(canalDeGrabacion).patch, tocaparam(canalDeGrabacion).canal,tocaparam(canalDeGrabacion).portout )
      '    EndIf


     Select Case  dato1 
         Case 144 ' on
' DIFERENCIA HABIA UN CHANGGEPROGRAM EN 148  
            noteon dato2,dato3,tocaparam(ntoca).canal, tocaparam(ntoca).portout, 1

'     Print   dato1;" ";  dato2;" "; dato3
           
         Case 128 'off
            noteoff dato2,tocaparam(ntoca).canal,tocaparam(ntoca).portout,1 'message(2)'
' DIFERENCIA 
'     Print   dato1;" ";  dato2;" "; dato3

     End Select


' tick mas chico es 0.005208325 (ver [TickChico] en RTMIDIDEC)
' ergo divido deltatime por ese valor y obtengo la cantiad de divisiones
' que ocupara ese retardo deltatime/TickChico

  If GrabarEjec =GrabarPistaEjecucion  Then ''graba en la pista seleccioanda
     partes=(deltatime/TickChico) ' o cantidad de Ticks 
     jgrb += 1
     If jgrb=1 And nroCompasesPatron> 0  Then
' RECUPERO LOS TICKS QUE CONTENDRA EL PATRON, LO BLANQUEO PARA LUEGO
' ONROLAR SU LLENADO
         nroTicksPatron =pmTk(ntoca+32).MaxPos 'hay patron 
         pmTk(ntoca+32).MaxPos=0
         tocaparam(ntoca).maxpos=0
     EndIf
     If ntoca > 1 And jgrb=1 Then ' detiene la acumulacion de deltatime en PlayTocaAll 
          arrancaPlay=1
 
     EndIf
      
     CargaIn( jgrb).modo=dato1
     CargaIn( jgrb).nota=dato2
     CargaIn( jgrb).vel=dato3
 
     If pmTk(ntoca+32).MaxPos >= nroTicksPatron And nroTicksPatron > 0   Then
' termino la grabacion del patron no se graba mas en CargaIn
         GrabarEjec =PatronDeEjecucionCompleto
     Else 
   
' aca vamos a marcar los compases para controlar el numero de ellos las repeticiones
' las grabaciones encima o reemplazando datos y la creacion  de patrones,
          If deltatime > 0.005  Then '   5mseg  
            CargaIn( jgrb).partes=partes ' o nro Ticks, convierto deltatime en tickschico 
            pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos +partes
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
          Else
            CargaIn(jgrb).partes=0
            pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos +1
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos  
          EndIf
     EndIf

  EndIf
End Function

'--------------------------------------
Sub GrabarMidiIn ( ByRef  par As  paramGrabamidi,i1 As integer)
On Local Error Goto fail

' tocap As vivo, ntkp As Integer,tocaparam  As ejecparam  Ptr) 
    Dim As Long j, driver=0
    Dim As String  nombreg
' y ntkp de donde vien quien lo ajusta? ntkp debe venir informado!!!
     pathdir=GetPathPart( DirEjecSinBarra)

     nombreg =pgmidi.tocap.nombre  ' 23-04-2024
     par.tocap=tocaparam(i1)
     
     
      driver=InStr(pathdir,":\")
      Dim  As Integer barra1, barra2  
      If  NombreCancion > "" And driver=0 Then
           barra1=InStrRev(NombreCancion,"\")
           barra2=InStr(nombreg,"\")
          If Len(NombreCancion)=barra1  Then
        CreateDir(NombreCancion+"Temp") ' ok
             If barra2 =0 Then
                nombreg=NombreCancion+nombreg
             EndIf
          EndIf
          If Len(NombreCancion)>barra1 And barra2=1 Then
           CreateDir(NombreCancion+"\Temp") ' ok
             nombreg=NombreCancion+nombreg
          EndIf          
      Else 
        If InStr(nombreg,"(")=0 Then
           nombreg="("+ doscifras(CInt(pgmidi.tocap.orden))+")"+nombreg  
        EndIf
      EndIf
      If InStr (nombreg,"ejec") = 0 Then
        nombreg=nombreg+".ejec" '"18-10-2024
      EndIf

 ' carga de parametros:

     
   '''  par.tocap.delta   =tocaparam(ntoca).delta no estaba hace fañta o no
' delta sol ose usa en unif que no separa que esta en playtocaAll 
     
     nombreg=Trim(nombreg)
     par.tocap.nombre  =nombreg
      maxgrb=par.tocap.maxpos '27-11-2024
      
      ngm=15 
    if   Open( nombreg  For Binary Access Write As #ngm)  <> 0 Then
    Else  
    Dim tocaparamaux As ejecparam
    tocaparamaux=tocaparam(i1) 
    tocaparamaux.nombre=nombreg
         Put #ngm,, tocaparamaux '1ero parametros como siempre o en cabezado
         Put #ngm,, Toca(i1).trk()   '2do datos  
         FileFlush(ngm) 
    End If
      Sleep 100
      Close ngm
'tocatope no lo estoy usando deberia ser el nro de pistas maximo
Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error GrabarMidiIn" & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
End If   


End Sub



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

'Print #fs,"--------------------------"

    If Track(ntk).trk(posn,1).acorde > 1 Then 
      cnt= Track(ntk).trk(posn,1).acorde
 '     Print #fs,"acorde ",cnt
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
' el maximo valor  de estos ticks es 128 la q sigue de semifusa garrapatea
' y eo maximo de duracion es 1,75 * 128 segun pesoDur=224 ticks
' ergo a tick lo puedo representar por un UBYTE como siempre
' SUPONGO ACORDE DE DISTINTAS DURACIONES
 FOR  I= 1 TO 12 'PTrack.trk(posn,I).acorde
    If Track(ntk).trk(posn,I).nota > 0 And  Track(ntk).trk(posn,I).dur >= 1 And _ 
       Track(ntk).trk(posn,I).dur <= 180 Then    
       If posn = 1 Then 
          Track(ntk).trk(posn,I).tick=0
       Else   
          Track(ntk).trk(posn,I).tick=(pesoDur(Track(ntk).trk(posn,I).dur)/d7)*128 + _
          Track(ntk).trk(posn-1,1).tick
       EndIf      
      ' guardar si es > 0 ON, notapiano,vol,pan,pb,inst y guargar tick=0
       Print #5,posn;",";"ON,";Track(ntk).trk(posn,I).nota;",";Track(ntk).trk(posn,I).vol; ","; _
           Track(ntk).trk(posn,I).nnn;",";Track(ntk).trk(posn,I).tick;","; _
           (Pesodur(Track(ntk).trk(posn,I).dur)/d7)*128;",";Track(ntk).trk(posn,I).dur 


    EndIf 
 Next I

End Sub
'---------------

Sub metronomo ()
Dim  As integer pista , k

Do
   PlaySound(".\recur\RIMSHOT.wav", 0, SND_FILENAME+SND_NODEFAULT +SND_ASYNC)
     duracion(Timer, (60/tiempoPatron) / FactortiempoPatron) 'jmgtiempo
     If terminar_metronomo=1 Then
         Exit Do
     EndIf
      

Loop  
 PlaySound(NULL, NULL, 0)
 threadDetach (threadmetronomo)
End Sub
'--------------------
Sub abrirPortoutEjec(j As Integer)
'------------hace falta abrir la salida
Dim k1 As Integer

  
   k1=CInt(pmTk(j+32).portout )
    
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( )
           listoutCreado( k1)=1 
        EndIf 
        open_port midiout(k1),k1, nombreOut(k1)
        Dim As integer    porterror=Err 
        listoutAbierto( k1) = 1
        porterrorsub(porterror)
   Else
   EndIf
 EndIf 


End Sub
'------------------
Sub PlayTocaAll(nt As Integer Ptr )
On Local Error Goto fail
'////////////////////////////TOCAALL/////////////////////////
' perfeccionar los eventos deven seguir un patron de tiempo de ticks
' para c/tick ver si hay un evento si hay enviarlo...el problem ason lso retardos
' TOCA VARIAS PISTAS EJEC 
'TickPlay = TickChico por default si quiero cambiar la velocidad debo cambiar el TickChico
'o sea se grabasiempre con elTick mas chico que es para veloc=240 y el valor del  tresillo
' 
ntoca=*nt  ''' almacena tocatope la cant max de ejecuciones o archivos cargados
Dim  As long j=0,k=0,partes,cuenta=0,pis=0
Dim As UByte dato1,dato2, dato3 
' cargo retardos de ejecucion ?? y no los usé para nada?? 
'For j=1 To 32
 ' espera(j)=tocaparam(j).delta ' empieza siemrpe por la 2
'Next j
'--------------play TOCA


'ChangeProgram ( 1, ntoca, 0)
' todas las pistas empiezan en el mismo Timer
timex(01)=Timer

For j=2 To 32 
 timex(j)=  timex(01)  
Next j


Dim  topeDuranteGrabacion As integer
'canal=1 ' por ahora
portsal=0 ' por ahora ???

If GrabarEjec=GrabarPistaEjecucion Then 
  If   tocatope >1 Then
   topeDuranteGrabacion=tocatope-1
  EndIf
Else
   topeDuranteGrabacion=tocatope

EndIf   
Dim As Integer prox=2 
 'For pis=1 To topeDuranteGrabacion
'
'      ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)	'
'
'Next pis
CONTROL2=0
'''TickPlay=TickChico*tiempoPatronEjec/240
' no hay caso no cambia nada el play deberia modificar los datos ???
' los tiempos son fijos la unica forma seria cambiar sus valores
' lo que debo hcer es restar una cantidad de ticks fijos a cada
' timex(kply)  por ejemplo timex(kply) - xcant * tickchico
' timex(kply) +  x3H * Tickchico
Dim resta  As Double 
'************************************************************************
'************************************************************************
 
For jToca=1 To maxgrb 
  If CONTROL2 = 1 Then
     For kply =1 To topeDuranteGrabacion
         alloff( 1 ,pmTk( kply +32).portout  )
     Next  kply
      CONTROL2=0
      repro=0
      Exit For
  EndIf  


  For kply =1 To topeDuranteGrabacion
   ' este cambio de patch anda bien no produce retardo por ahora y se hace cada
' vez que cambio de pista en un bariddo vertical de una posicion dada
' verificar esto en play cancion donde creo daba retardo ver,,,,ÇÇÇ 04-06-2022

' con LoopBe Internal MIDI" no puedo hacer change program si esta conectado
' ZynAddSubFx... nose si con otro tipo de sintetizador que reciba patch se podrá
' o solo es problema de LoopBe Internal MIDI seguir probando a ese sinte solo
' se envia canales , la configuracionde instrumentos y bancos se carga y graba
' en ZynAddSubFX
    If CheckBox_GetCheck( cbxejec(kply))= 1 Then
       If  InStr (*nombreOut(tocaparam(kply).portout),"LoopBe Internal MIDI") = 0  Then  
          ChangeProgram ( tocaparam(kply).patch , tocaparam(kply).canal, tocaparam(kply).portout)
       EndIf
    Else
       Continue For ' saltear no tocar 
    EndIf 
'    If kply = topeDuranteGrabacion And CheckBox_GetCheck( cbxgrab(tocatope))= 1 Then
'            ChangeProgram ( tocaparam(tocatope).patch , tocaparam(tocatope).canal, tocaparam(tocatope).portout)   
'    EndIf  
' TickPlay 14-07-2022
    dato1=Toca(kply).trk(jToca).modo
    dato2=Toca(kply).trk(jToca).nota
    dato3=Toca(kply).trk(jToca).vel
    portsal=pmTk(kply+32).portout '04-05-2022

     If  dato1=1 Then ' marcamos con 1 un delta de tick en modo
         duracion (timex(kply) ,TickPlay) ' si es cero no 1 no hay duracion es acorde
         timex(kply)=timex(kply) + TickPlay 'jmgtiempo
         
       If  GrabarEjec =GrabarPistaEjecucion And ntoca > 1 And arrancaPlay=0 And kply=1 Then
     '.....????       tocaparam(ntoca).delta=tocaparam(ntoca).delta+TickPlay  'jmgtiempo
           ' retardos respecto del inicio de play y de pista 1, kply=1 
       EndIf

     EndIf

' PARA CALCULO DE RETARDO DEL INICIO DE PLAY CANCION RESPECTO PLAYTOCAALL
' HABILITAMOS SOLO PARA PRUEBAS ,tenemos 2.7 mseg de retardo ,medio Tick (5mseg)
' If  jToca=1 Then
'EndIf
'--------------------------------------
     Select Case  dato1 
         Case 144 ' on
            noteon dato2,dato3,tocaparam(kply).canal, tocaparam(kply).portout, 1 'message(3) ' noter vel canal
         Case 128 'off
            noteoff dato2,tocaparam(kply).canal ,tocaparam(kply).portout,1 'message(2)'
     End Select
 
  Next kply
''''Sleep 1,1 ' para que corranmas de un thread ¿??? 20-06-2022 porque lo puse?
Next jToca
''jToca=0
repro=0
If instancia=7 Or instancia= 107 Then ''' Or instancia < 3 Then
' las instancias son formas de cargar roll
Else
SetGadgetstate(BTN_MIDI_EJECUTAR,0)
EndIf
Sleep 1

  For kply =1 To topeDuranteGrabacion
    
    If CheckBox_GetCheck( cbxejec(kply))= 1 Then
       ' tpcar
    Else
     Continue For ' saltear no tocar 
    EndIf 
    portsal=tocaparam(kply).portout 
     alloff( tocaparam(kply).canal,portsal )  
     allSoundoff( tocaparam(kply).canal, portsal ) 
  Next kply
  If   GrabarEjec =PatronDeEjecucionCompleto Then
         Dim rta As string
         rta= inputBox("Guardo esta ejecucion", "SI O NO", "SI") 'JJJJJ SEGUIR ACA
         rta = UCase(Trim(rta))
         Select Case  rta 
              Case  "SI", "S"
              GrabarEjec = GrabarPatronaDisco ' ORDEN DE GRABAR A DISCO
         
         Case  "NO", "N", ""
              GrabarEjec = HabilitaGrabar ' ORDEN DE  NO GRABAR
         Case Else
              GrabarEjec = HabilitaGrabar ' ORDEN DE  NO GRABAR
       
         End Select

  EndIf 
  repro=0
  Sleep 20,1
  ThreadDetach threadG
Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error PlayTocaAll" & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
End If   

End Sub

Sub VerCompasTicksEjecucion()
'ver como en unteclado una pantalla queindique compas:tiempo:ticks
' la cual puede usarse para marcar principio y fin de reproduccion y limites de corte
' para generar un patron

End Sub
'-------------------------------------------
Sub EditarPatronEjecucion()

End Sub
'-------------------------------------------
Sub  CrearSecuenciaPatrones()
' casos posibles 
' 1) tomar una ejecucion por  midi o pista manual existente y grabarla como FUTURO patron asi como está.
' 2) cortar la ejecucion o pista manual y convertirla en patron
' 3) definir un patron a partir de parametros como duracion o cantidad de compases.
' 4) cargar patron en un determinado punto
' 5) repetir patron, fisicamente (copiar) o logicamente(loop play).
' 6) ordenar en un plano (una ventana) los  patrones para ejecutar en ese orden y para ello
'   despues de ordenado pulsar un boton de plasmar la secuencia, que haria unir todos esos 
'   patrones ordenados toquen en el orden dado , como en la lectura de una pagina en vez de palabras
'   patrones.
' 7) usar el help con el mouse para saber que hay en cada patron saldra un globito con la info
' ese globito toma la identificacion del patron y muestra su nro, nombre ,instrumento etc,,,
' 8) usar botones con label que sera un nro y letras como las usadas en canciones A,A,B,B,AA 
' 9) Poder mover los botones dentro de una grilla que representara el orden deseado de ejecucion
' 10) para todo esto crear una venta separada que muestre los botones de la ejecuion presente
'11) poder copiar el resultado del armado de patrones como una pista nueva de ejecuciones
' Y SI HACEMOS TODO EN FORMATO TEXTO?? 
' NRO-PATRON, NRO-PATRON, CORCHETES PARA REPETICON NRO PARA REPETICION
 ' -------------
'implementacion ya crea unboton lo mueve y lo deja donde se desea
' falta un evento que lo selecciones y deseleccione...
' dragar patrones, crearlos darles nombre grabarlos etc etc  paso a paso 
'hwndPatronEjec =OpenWindow("PAtrones Ejecucion ",100,100,ANCHOSYSTEM*0.5 ,ALTOSYSTEM*0.5,WS_VISIBLE , WS_EX_NOPARENTNOTIFY  OR WS_EX_ACCEPTFILES Or WS_EX_TOPMOST)
'ButtonGadget(1,ANCHOSYSTEM*0.5 ,140 ,40 ,40,"[X]")
' WS_VISIBLE, WS_EX_TOPMOST
Dim As  Long parar=0
'#define EventMouseMove &H200
Type  punto
As Long x
As Long y
End Type
Dim pun As  punto
Dim  p As  Any Ptr
p=@pun
'' => desde acaecho con tool del ruso no anda muy bien
hwndPatronEjec=OpenWindow("Patrones Ejecucion",100,50,ANCHOSYSTEM*0.5,ANCHOSYSTEM*0.5,WS_VISIBLE, WS_EX_TOPMOST )

        ButtonGadget(1,ANCHOSYSTEM*0.5 -30,0,20,20,"[X]")
       '    SetForegroundWindow(hwndPatronEjec)


Do
       Var eventP= waitEvent
        Select Case  eventP
              Case EventMouseMove  
              'If EventKEY And MK_LBUTTON Then
                   '? "Left Button press AND mousemove"   
                   '? "MOUSE: " & MouseX & "  " & MouseY
       If GetCursorPos(p)  And parar=0   then             
          ScreenToClient(hwndPatronEjec, p)
          ButtonGadget(2,pun.x,pun.y,40,20,"P-01")
      EndIf         
          Case EventLBDown 
              parar=1
          Case EventRBDown
              parar=0
          Case eventgadget 
            If eventnumber()=1  Then
               Close_Window(hwndPatronEjec)
                  Exit Do
           End If

          End Select 
          
         Loop
         



 
End Sub 
'
Sub EscribirMidiSobreRoll ( notamidi() As notacallback, durafig() As Double, _
         ByRef contid As Integer) 
' sesupone maximo doce notas doce on y doce off
' TODO ESTO LO PASARE A RollCallback


    

End Sub

' ---------------------------------------
' error

errorrtmidi:
 
Dim As Integer er1, ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 0 And ContadorError < 101 Then

  ContadorError = ContadorError+1

EndIf
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )




