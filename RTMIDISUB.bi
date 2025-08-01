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
  Print #1, "indice, oreden por audio",i1, indiceaudio(i1)
Next i1


End Sub

'
Sub veoPasoCol( Col() As vec,cnt As integer, cntold As Integer)
print #1,"Desarrollo pasoCol() sin todos los campos integer "
Dim As Integer mayor,j
mayor=cnt
If cntold > cnt Then
  print #1," cntold > cnt ",cntold,cnt
  Print #1," muestro el acorde anterior de mas notas, la actual es menor o una nota simple"
  mayor=cntold
Else 
  print #1," cntold < cnt ",cntold,cnt
  Print #1," el acorde anterior tenia menos notas"
EndIf
If cntold = cnt Then
  print #1," cntold = cnt ",cntold,cnt
  Print #1," el acorde anterior y el actual tienen = cant de notas"
EndIf
' en lso old acumulo los valores actuales solo si estan en cero
' de otro modo se hacen 0 en OFF o se incrementan en ligaduras
Print #1, "-----------------------------------"
For j=1 To cnt  ''''mayor veo solo el actual no el anterior 21-02-2022
Print #1," cnt ";j
Print #1, "PasoCol(";j;")" 
Print #1, "tiempoFigura ";Col(j).tiempoFigura 
Print #1, "tiempoFiguraOld "; Col(j).tiempoFiguraOld
Print #1, "DUR ";Col(j).DUR
'print #1, "DURold "; pasoCol(j).DURold  
Print #1, "notapiano "; Col(j).notapiano
'print #1, "notapianoOld "; pasoCol(j).notapianoOld 
Print #1, "liga ";Col(j).liga       
Print #1, "ligaold ";Col(j).ligaold 
Print #1, "audio ";Col(j).audio       
Print #1, "audioOld ";Col(j).audioOld 
Print #1, "i1 ";Col(j).i1           
Print #1, "i1old ";Col(j).i1old           
Print #1, "old_time ";Col(j).old_time
Print #1, "old_timeold ";Col(j).old_timeold 
Print #1, "inst ";col(j).inst
Print #1, "-----------------------------------"      
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
print #1,"EN NOTE OFF nota, RESULT", note, result

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
print #1,"EN NOTE OFF nota, RESULT", note, result

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
   'print #1,"EN NOTE OFF nota, portsal ", note, portsal
   '''Print #1, "Not Cantmicroseg "; note, Cantmicroseg
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
'print #1,"EN NOTE OFF nota, portsal ", note, portsal

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
'Print #1,"send_message midiout(portsal) , p, leng ",midiout(portsal) , p, leng
'On Local Error GoTo  errorcp
  result = send_message (midiout(portsal) , p, leng) 
'errorcp:
'Dim n As Integer = Err
'  Print #1,"error send_message change program",n

'Print #1,"resultado de cambiar elpatch result patch", result, instru
End Sub
Sub noteon	( note As ubyte, vel As UByte, canal As UByte, portsal As UByte,i1 As Integer)
	' canal 1 
' NOTE ON, 1001 XXXX, 9 X H, N� de tecla o nota, Velocidad
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
'Print #1,"EN NOTE ON nota, portsal ", note, portsal

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
print #1,"LimpiarLigaduras nohay=",nohay,"cnt=";cnt
print #1,"LimpiarLigaduras 1 cnt =",1 ,cnt
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
'print #1,"-----------------------------"
'print #1,"AOI:] START ACORDEONIGUALES "
'print #1,"-----------------------------"

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
  Print #1,"===>LLAMO A SORTAUDIO "
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
Print #1,"AOI 00:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
Dim As integer nj=jply, durj ' indice del vector roll, dur
Dim As Double f
print #1,"AOI 1: DUR cnt=1:";pasoCol(1).Dur
old_time_on=Timer
print #1,"AOI 2: OLD_TIME AL INICIO ",old_time_on
old_time_on_int=old_time_on * d11 
print #1,"AOI 3: comienza For de 1 a cnt",cnt
' LIGADURAS SEGUIDAS. AL INICIO LIGAOLD ES CERO SE DA EL ON
' Y SI HAY VARIAS LIGADURAS SECUENCIALES VOY EJECUTANDO
' LAS DURACIONES O RETARDOS DE CADA PASO Y AL FINAL DOY EL RETARDO FINAL Y EL OFF
' CUANDO LA ULTIMA NOTA NO TENGA MAS LIGADURA ES EL MOMENTO DEL OFF.
Print #1, " analizamos la columna:" 
Print #1,"AOI 3a:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano

For k1=1 To cnt  ' PARA CADA NOTA

 If p1=1 Then
   i1 = indiceaudio(k1)
 Else
   i1 = k1  
 EndIf
Print #1,"AOI 4: 1)DUR cnt=";i1;":";pasoCol(i1).Dur
print #1,"AOI 5:CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
Print #1,"aoi 5a:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld

 Select Case pasoCol(i1).Dur
  
   CASE   1 To  90  ' SIN LIGADURA 
Print #1,"AOI 5B:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano
print #1,"AOI 6:  no ligado,liga ";pasoCol(i1).liga
 
      If pasoCol(i1).old_timeold = 0 Then
         pasoCol(i1).old_time=old_time_on_int
      Else
         pasoCol(i1).old_time=pasoCol(i1).old_timeold 
      EndIf
      pasoCol(i1).liga =0
 print #1,"AOI 7: no ligado,old_time ";pasoCol(i1).old_time
      pasoCol(i1).tiempoFigura= reldur(pasoCol(i1).DUR) * tiempoDUR * d11    
Print #1,"AOI 7W:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano
      If pasoCol(i1).tiempoFiguraOld <> 0 Then
Print #1,"=>AOI 7X:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
        If tipoAcorde = 1 Then
           pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFiguraOld
Print #1,"=>AOI 7Y:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
        Else ' acumulo si no es ligado pero hay ligaol porque es final de ligadura
           If pasoCol(i1).ligaold > 0 Then
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld+ pasoCol(i1).tiempoFigura
Print #1,"=>AOI 7Z:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano
           EndIf
        EndIf  
      EndIf  
      Print #1,"AOI 7a: notapiano " ,pasoCol(i1).notapiano
      print #1,"AOI 8 : no ligado pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
      Print #1,"AOI 8a: no ligado,tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld            
      print #1,"AOI 9 : no ligado DUR "; pasoCol(i1).Dur
      print #1,"AOI 10: no ligado Audio "; pasoCol(i1).audio
      print #1,"AOI 10a: no ligado AudioOld "; pasoCol(i1).audioOld
      print #1,"AOI 11: no ligado i1 "; pasoCol(i1).i1
      print #1,"AOI 11a: no ligado i1Old "; pasoCol(i1).i1old
      
      If pasoCol(i1).ligaold =0  Then
         print #1,"AOI 12: SIN LIGAR Noteon 1] de notepiano "; pasoCol(i1).notapiano
         print #1,"AOI 13: sin ligar inst pasocol("+Str(i1)+").inst ",pasoCol(i1).inst

         canal=pasoCol(i1).canal
         portsal=CUByte(pasoCol(i1).port)
         vel= CUByte(vol( pasoCol(i1).DUR, velpos))
         if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
            vel=0
         EndIf   
         Print #1,"AOI 12: i1, vel, velpos "; i1, vel, velpos      
         noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
Print #1,"AOI 13A:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld

      Else
Print #1,"AOI 13B:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
Print #1,"AOI 14:No se envia Note on es la continuancion de un ligado, notapiano ",pasoCol(i1).notapianoOld
Print #1,"No se envia duracion en ligados la duracion se tira toda al final"
Print #1,"o podria tirarla de a pedazos tambien..�? hare eso.."
Print #1,"AOI 14W:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano
         ' aca se da que  si al ultima nota de una sucesion de ligados sin sonido , tiene sonido
         ' y la anterior no tenia entonces se resta los retardos de la nota sin sonido
         ' alas notas con audioold=0 igual que en la otra parte se copia

          Dim As Integer cret 
          If pasoCol(i1).audio=1 And pasoCol(i1).audioold=2 Then
Print #1,"AOI 14a: SIN LIGAR Noteon de nota que era silencio ahora se enciende 3] de notepiano "; pasoCol(i1).notapiano
print #1,"AOI 14b: sin ligar inst pasocol("+Str(i1)+").inst ",pasoCol(i1).inst
Print #1,"AOI 14X:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld

             canal=pasoCol(i1).canal
             portsal=CUByte(pasoCol(i1).port)
             vel= CUByte(vol( pasoCol(i1).DUR, velpos))
             if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                vel=0
             EndIf   
Print #1,"AOI 14X VEL "; vel 
             noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1 
             pasoCol(i1).old_time=old_time_on_int
          EndIf
         ENDIF 
   Case   91  To 180 ' CON LIGADURA con sonido la 1er el resto mute
Print #1,"AOI 14Y:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  Print #1,"AOI 15: DUR debe ser ligado >=91 <=180 ";pasoCol(i1).Dur
 Print #1,"AOI 15a: notapiano " ,pasoCol(i1).notapiano
 Print #1,"AOI 16: liga debe ser 1: ",pasoCol(i1).liga
  
     pasoCol(i1).liga=1
     If pasoCol(i1).old_timeold = 0 Then
        pasoCol(i1).old_time = old_time_on_int
        pasoCol(i1).old_timeOld = old_time_on_int
     Else
        If tipoAcorde=1 Then
           pasoCol(i1).old_time= pasoCol(i1).old_timeold
        EndIf 
     EndIf
Print #1,"AOI 16A:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld         
Print #1,"AOI 17:  ligado,old_time ";pasoCol(i1).old_time    

     pasoCol(i1).tiempoFigura= relDur(pasoCol(i1).DUR) * tiempoDUR * d11
     If pasoCol(i1).tiempoFiguraOld =0  Then
      ' tiene sonido la 1er nota de un aligadura larga
        'vel= CUByte(vol( pasoCol(i1).DUR, velpos)) ' 28-04-2024 comento 
        'pasoCol(i1).audio=1 comento la original no lo tenia 
        Print #1,"17a pasoCol(i1).DUR vel velpos "; vel , velpos
        pasoCol(i1).tiempoFiguraOld =pasoCol(i1).tiempoFigura
Print #1,"17a pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano  
     Else
        If tipoAcorde = 1 Then
           pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
Print #1,"AOI 17X:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld              
        Else ' acumulo
 Print #1,"AOI 17b: notapiano " ,pasoCol(i1).notapiano
           pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld+ pasoCol(i1).tiempoFigura
  Print #1,"17c:pasoCol(i1).tiempoFiguraOld, notapiano ",pasoCol(i1).tiempoFiguraOld,pasoCol(i1).notapiano
        EndIf  
     EndIf
         print #1,"AOI 18 :  ligado,tiempoFigura ";pasoCol(i1).tiempoFigura
         Print #1,"AOI 18a:  ligado,tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld            
         print #1,"AOI 19 :  ligado DUR "; pasoCol(i1).Dur
         print #1,"AOI 20 :  ligado Audio "; pasoCol(i1).audio
         print #1,"AOI 20a:  ligado AudioOld "; pasoCol(i1).audioOld
         print #1,"AOI 21 :  ligado i1 "; pasoCol(i1).i1
         print #1,"AOI 21a:  ligado i1Old "; pasoCol(i1).i1old

     Select Case  pasoCol(i1).ligaold
        Case Is > 0
           If  pasoCol(i1).audio=1 And pasoCol(i1).audioOld = 2  Then
 Print #1,"AOI 21b:En NOTEON de ligado con audio, pero su old era sin audio notepiano:"; pasoCol(i1).notapiano
               canal=pasoCol(i1).canal
               portsal=CUByte(pasoCol(i1).port)
               vel= CUByte(vol( pasoCol(i1).DUR, velpos))
Print #1,"   21b:velpos,vel ",velpos,vel
               if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                  vel=0
               EndIf   
               
               noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
               pasoCol(i1).old_time=old_time_on_int
Print #1,"AOI 21W:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
               pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ''''aca toma 0.25 
               pasoCol(i1).old_timeold=old_time_on_int
            '  borramos los retardos de silencio anteriores a las notas que tenian noteon antes o sea
            ' que eran audio=2
Print #1,"AOI 21X:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld   
           EndIf           
           If  tipoAcorde=1 Or pasoCol(i1).audio=2 Then
Print #1,"AOI 21X:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
              f=pasoCol(i1).tiempoFigura/d11
 Print #1,"AOI 21d: ligadoold notapiano, duracion ",pasoCol(i1).notapiano, f
 Print #1,"AOI 21d: notapiano " ,pasoCol(i1).notapiano

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
 Print #1,"AOI 21E: se usa el time almacenado del apso anterior ",pasoCol(i1).old_timeold 
                    duracion pasoCol(i1).old_timeold/d11, f '25-11
                    portsal=CUByte(pasoCol(i1).port)
                    canal=pasoCol(i1).canal ' 12 junio 2024
                    noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1  ' la notapiano es la misma porque esta ligado 
                 EndIf
                 If pasoCol(i1).liga=1 Then
 Print #1,"AOI 21E: se usa el time de este paso ",old_time_on 
                    duracion old_time_on, f '25-11
                 EndIf
              EndIf
           EndIf    
        Case Else
Print #1,"AOI 21Y:pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld        
          If pasoCol(i1).ligaold =0 And pasoCol(i1).liga=1 Then '16-11-2021 UN SOLO ON
             If pasoCol(i1).audio = 1 Then
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
Print #1,"AOI 21Y: i1, velpos , pasoCol(i1).DUR "; i1, velpos, pasoCol(i1).DUR 
                vel= CUByte(vol( pasoCol(i1).DUR, velpos))
                if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
                   vel=0
                EndIf   
Print #1,"AOI 21Y VEL "; vel
                noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
                pasoCol(i1).old_time = old_time_on_int
 Print #1, "AOI 22: 2]NOTEON ligado pasocol("+Str(i1)+").inst en AcordeonIguales ", pasoCol(i1).notapiano
 Print #1,"AOI 22A:OLD_TIME DE NOTEON ", old_time_on_int

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
  Print #1,"AOI 23:  cantidad Ligaold ==========> ";pasoCol(i1).ligaold
 End SELECT

Next k1

' for de NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS"

Print #1,"=================> FIN ACORDEON IGUALES O SIMPLE"
print #1,"=AOI tenia   old_time_on_int ", old_time_on_int

End Sub 

Sub AcordeOnDistintos	( pasoCol() As vec , cnt As UByte, cntold As UByte, vel As UByte,tiempoDUR As Double,Roll As inst,velpos As Integer,pis As UByte,portsal As UByte)
'print #1,"-------------------------------------"
'print #1,"AOD :]start AcordeOnDistintos cnt,cntold,pis ",cnt,cntold,pis
'print #1,"-------------------------------------"
'debug..
' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

Dim As UByte i1, liga=0,coff=0,canal
Dim As Integer tiempoFigura=0, tiempoFiguraSig=0
' 2 o mas acordes ligados ...en 1 o mas notas
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SER� LA MAYOR DURACION 
'  DEL ACORDE.(MDA)
If cntold >cnt Then
 coff=cntold
Else
 coff=cnt 
EndIf
'print #1,"====>>> START AOD ON veo el pasocol que tiene"
Dim As UByte noAnalizo=0 
For i1=1 To coff ' 20-06-2021 JMG
'  print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
''  " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time
  If pasoCol(i1).liga > 1 Then ' si hay liga viene de antes
     noAnalizo=1
  EndIf 
       
Next i1


' 2) SI HAY LIGADURA TRAIGO LA DURACION DEL ACORDE SIGUIENTE O NOTA AL ACORDE ACTUAL
' ASI HASTA LLEGAR A LA ULTIMA POSICION SIN LIGADURA puede haber varios acordes ligados
Dim As integer nj=jply, durj ' indice del vector roll, dur

'print #1,"AOD:DUR cnt=1:";pasoCol(1).Dur

old_time_on=Timer
'print #1,"AOD: old_time_on ";old_time_on

'print #1,"start FOR"
For i1=1 To cnt
'print #1,"for cnt=";i1
'print #1,"AOD FOR: pasoCol(i1).Dur       ";pasoCol(i1).Dur
'print #1,"AOD FOR: pasoCol(i1).liga      ";pasoCol(i1).liga
'print #1,"AOD FOR: pasoCol(i1).NOTAPIANO ";pasoCol(i1).notapiano
'print #1,"AOD FOR: pasoCol(i1).tiempofigOld ";pasoCol(i1).tiempoFiguraOld


' SOLO EL 1ER ACORDE LIGADO SE ANALIZA EL RESTO POR MAS LIGADURAS QUE TENGA YA NO
' PORQUE .LIGA SERA > 0
' print #1,"AOD:CNT CNT CNT ";cnt ;" dur";pasoCol(i1).Dur
 ' este AND  para iguales no va pero para distintos??? 
 If pasoCol(i1).liga = 0 Then  ' 13-06-2021
 '   print #1,"1) pasoCol(i1).liga =0 "
  ' de 91 a 180 son todos ligadso pero incluye los silencios ojo!
  If pasoCol(i1).Dur >= 91 And pasoCol(i1).Dur <=180 And pasoCol(i1).liga=0 Then
'     print #1,"AOD:ANALIZO LIGADURAS SUbSIGUIENTES si hay y toda la columna"
'     print #1,"AOD:DUR ";pasoCol(i1).Dur
'     print #1,"AOD:liga anterior";pasoCol(i1).liga
'     print #1,"AOD:notepiano ";pasoCol(i1).notapiano
          
     If pasoCol(i1).liga =0 Then ' 13-06-2021
        pasoCol(i1).liga=1
        pasoCol(i1).old_time=old_time_on * d11
'        print #1,"ligado 1 guardo old_time_on primer liga "; pasoCol(i1).old_time
     Else
        pasoCol(i1).liga=pasoCol(i1).liga + 1 ' 13-06-2021
        ' no se carga old_time sigue siendo el mismo
     EndIf
     ligaglobal=1
     liga=1
'     print #1,"*AOD:LIGA=1 ==========> ";liga
'     print #1,"*AOD: LIGA ACUMULADA ===>";pasoCol(i1).liga
'     print #1,"*AOD: ACUMULADO TIEMPOfIGURA ";pasoCol(i1).tiempoFigura
     pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
'     print #1,"*AOD: ACUMULADO TIEMPOfIGURAOLD ";pasoCol(i1).tiempoFiguraOld
'     print #1,"Loop "
     Do
       nj=nj+1
       ' busca la proxima dur 
 '      print #1,"pasoCol(i1).i1 ",pasoCol(i1).i1 
 '      print #1,"nj ",nj
       If CANCIONCARGADA =TRUE Then
  '        print #1,"AOD: CANCIONCARGADA",
          durj = Track(pis).trk(nj, pasoCol(i1).i1 ).dur
  '        print #1,"durj ",durj
       Else
          durj = Roll.trk(nj, pasoCol(i1).i1 ).dur
       EndIf
' problema con las octavas necesita 1 y solo vade 39 a 102 uuu
' en vez de 1 deberia ser 39         
  '     print #1,"AOD:LIGA nj, durj "; nj ; " "; durj
  '     print #1,"AOD:LIGA nj reldur ";nj; " "; relDur(durj)
'3) calculo tiempofigura de cada nota y su acumulacion en ligaduras
       tiempoFiguraSig = relDur(durj) * tiempoDUR * 100000000000
 '      print #1,"AOD:LIGA paso nj tiempoFiguraSig ";nj; " "; tiempoFiguraSig
' almaceno todo el tiempo en la nota 1er acorde       
       pasoCol(i1).tiempoFigura = pasoCol(i1).tiempoFigura +tiempoFiguraSig
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
 '      print #1,"AOD:LIGA pasoCol(i1).tiempoFigura+sig "; pasoCol(i1).tiempoFigura
       If durj >= 91 And durj <=180  Then ' si es liga 
         pasoCol(i1).liga= pasoCol(i1).liga +1
       Else
         Exit Do
       EndIf
     Loop
     nj=jply '08-06-2021
 '    print #1,"2) AOD:LIGA=1 ==========> ";liga
     ' liga me da la cantidad de acordes ligados en esa nota
     ' se va borando hasta que se haya dado el off final
 '    print #1,"numero de ligados:";pasoCol(i1).liga
 '    print #1,"Noteon ligado notepiano "; pasoCol(i1).notapiano
 '   print #1,"1]pasocol("+Str(i1)+").inst en AcordeonDistintos ",pasoCol(i1).inst
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     vel= CUByte(vol( pasoCol(i1).DUR, velpos))
     if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
        vel=0
     EndIf   

     noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
 '    print #1,"3) AOD:LIGA=1 ==========> ";liga
     
  EndIf
 Else ' ya venia una ligadura ' 13-06-2021
'     print #1,"AOD:ya venia con ligadura de antes" ' 13-06-2021
'     print #1,"AOD: OJO! pasoCol(i1).old_time ";pasoCol(i1).old_time
'     print #1,"AOD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld ' 13-06-2021  
 EndIf 
'  print #1, "pasoCol(i1).Dur ";pasoCol(i1).Dur; " pasoCol(i1).liga ";pasoCol(i1).liga
  If pasoCol(i1).liga = 0 Then 
'      print #1,"4) AOD:LIGA=1 ==========> ";liga 
'      print #1,"AOD:|||| la liga dentro if liga=0 debe dar 1 en algun momento.";liga 
      pasoCol(i1).tiempoFigura = relDur(pasoCol(i1).Dur) * tiempoDUR * 100000000000  
'      print #1,"AOD:|||SIN LIGA pasoCol(i1).tiempoFigura "; pasoCol(i1).tiempoFigura
'      print #1,"AOD:|||SIN LIGA DUR "; pasoCol(i1).Dur
'      print #1,"5)AOD: LIGA=1 ==========> ";liga
      If liga=1 Then
       pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura
 '      print #1,"6)AOD:|||LIGA=1, pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
      EndIf
      ' cuando termine el for, habr� guardado el tiempoFigura mayor de lso 
      ' no ligados..
  EndIf
Next I1

' NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS

For i1=1 To cnt

 If pasoCol(i1).liga = 0 And pasoCol(i1).DUR <> 181 Then 
 '   print #1,"7)AOD:SIN LIGAR Noteon de notepiano "; pasoCol(i1).notapiano
 '   print #1,"2]pasocol("+Str(i1)+").inst en AcordeonDistintos ",pasoCol(i1).inst
    canal=pasoCol(i1).canal
    portsal=CUByte(pasoCol(i1).port)
    vel= CUByte(vol( pasoCol(i1).DUR, velpos))
    if pasoCol(i1).vol = 0 And CANCIONCARGADA= TRUE Then
       vel=0
    EndIf   

    noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal,i1
 End If
Next i1

'print #1,"FIN AcordeOnDistintos"
	
 
End Sub

Sub AcordeOffIguales	(  pasoCol() As vec, cnt As UByte, cntold As UByte,Roll As inst, pis As UByte,portsal As UByte)
Dim As UByte i1,canal
Dim tiempoFigura As Double 
Print #1,"-------------------------------------"
Print #1,"AFI :]========Start AcordeOffIguales cnt,cntold,pis ",cnt,cntold,pis
Print #1,"-------------------------------------"
'debug..
' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

'aca son iguales ergo lso off caen todo iguales al mismo tiempo
' solo que una not aligada hara off en el siguiente paso
'--------


 old_time_off=Timer
' print #1,"AFI 1:old_time off inicial ";old_time_off
 old_time_off_int = old_time_off * d11 ' solo para guardar y poder hacer sort
 
Print #1,"AFI 2:no ligados calculo tiempo Figura y doy off:" 
' son iguales las duraciones son las mismoas no ahce falta sort
' este 1er for tendra las duraciones mas cortas pues son acordes iguales 
' y si hay un acorde debe ser igual y si esta ligado a un acorde 
' anterior es mayor a este caso y sera el caso FOR siguiente
For i1 = 1 To cnt ' (1) 
 
Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld=0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
     print #1,"AFI 3:i1 ";i1;" no ligado tiempoFigura ";tiempoFigura
     print #1,"i1 ";i1;" no ligado old_time ";pasoCol(i1).old_time
     print #1, "AFI 4:i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     print #1, "AFI 5:call old_time o retardo con ",pasoCol(i1).old_time 
     print #1, "AFI 6:call duracion o retardo con ",tiempoFigura
     duracion pasoCol(i1).old_time / d11, tiempoFigura
     print #1, "AFI 7:SIN LIGAR NOTEOFF==>" , pasoCol(i1).notapiano
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld     
   print #1,"AFI 7a: reseteo los old"
       pasoCol(i1).tiempoFiguraOld=0
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
             'pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 

Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
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
Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld >0 Then
     print #1,"AFI 9:HAY LIGADOS que llegan y terminan aca!"
     print #1,"AFI 10:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
     print #1,"AFI 11:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
              'old_time= pasoCol(i1).old_time/100000000000 
    '''' print #1,"old_time_on ";old_time_on
    '''' V20 audioOl>=1 o sea 1 o 2
     If tipoAcorde=1 Or pasoCol(i1).audioOld =2 And pasoCol(i1).audio=1 Then        
       tf = pasoCol(i1).tiempoFigura  /d11
     Else
       tf = pasoCol(i1).tiempoFiguraOld  /d11
     EndIf
     print #1, "AFI 12:retardo tf ";tf
     ' el time tomo del inicio de aca del off porque ya gaste duracon de la 
     ' ligarura en el paso anterior
     If TF > 0 Then 
     Print #1,"puta entras o no pro aca "
     ' la nota ligada es simple y ataca luego a una nota de un acorde
     ' se parte la duracion en un pedazo luego de la ligada en noteon
     ' y otro pedazo de la duracion o reardo luego de la resolucion 
       If pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura And pasoCol(i1).tiempoFigura > 0 Then
          If TipoAcorde=1 Then
          Print #1,"por aca no debe entrar tfold=tf" 
            duracion old_time_off , tf
          EndIf  
       Else
         If pasoCol(i1).tiempoFiguraOld > pasoCol(i1).tiempoFigura Then 
  '''' V20 -> And pasoCol(i1).audio=2        
            If  tipoAcorde > 1 And pasoCol(i1).audioOld=1 And pasoCol(i1).audio=2 Then 'zzzzzzz
                Print #1,"audioold=1 y tfold> tf"
                duracion pasoCol(i1).old_timeold/d11, tf ' falta cambiar TF 16-11-2021
            Else
               If pasoCol(i1).audioOld=2 And pasoCol(i1).audio >=1 Then
                 Print #1,"uso old_time ",pasoCol(i1).old_time
                  duracion pasoCol(i1).old_time/d11,tf
               Else
                  If pasoCol(i1).audioOld=1 And pasoCol(i1).audio=1 Then
                     duracion pasoCol(i1).old_time/d11,tf
                  Else
                     Print #1,"uso old_time_off de aca ", old_time_off  
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
 '         print #1,"AFI 13: ligado NOTEOFF==> i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" "; _
 '          figura(pasoCol(i1).DUR)
            canal=pasoCol(i1).canal
            portsal=CUByte(pasoCol(i1).port)
            noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
        '''  'liga cero no
       ' ''' EndIf
'       Else
'          print #1,"AFI 14: ligado DOY OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
'          figura(PasoCol(i1).dur)
             
'           noteoff CUByte(pasoCol(i1).notapiano) ,canal
         
'       EndIf
       
     EndIf
     print #1,"AFI 14a: reseteo los old ,,notapiano ",pasoCol(i1).notapiano
       pasoCol(i1).tiempoFiguraOld=0 ''' no sirve  24-11-2021
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
       '''pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 
       
    Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld   
  EndIf 

  ' le resto 1 a la liga para decir que paso este paso y en el proximo
  ' si llega a cero envia el noteoff entonces
'  pasoCol(i1).liga = pasoCol(i1).liga - 1 ' 05-11-2021
'  If pasoCol(i1).liga < 0 Then
'     pasoCol(i1).liga=0
'  EndIf
  print #1,"le resto 1 a  pasoCol(i1).liga ", pasoCol(i1).liga
 '05-11-2021 comentado' ligaglobal = pasoCol(i1).liga ' por si sigue una simple

Next i1
 print #1,"AFI 15: fin ligados off"

 print #1,"AFI 16: start ligados continuacion en este paso " 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
' and NOT 25-11
  If pasoCol(i1).liga > 0 And pasoCol(i1).ligaOld >0 Then ''And Not (pasoCol(i1).audio=1 And pasoCol(i1).audioOld=2)  Then
     print #1,"AFI 17:HAY LIGADOS que llegan y siguen!"
     print #1,"AFI 18:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   '''''''''old_time= pasoCol(i1).old_time/100000000000 
   '''''' ' print #1,"old_time_on ";old_time_on        
     pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
     print #1,"AFI 19:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=pasoCol(i1).old_timeold
'------------audio en un solo for es suficiente
' en cada nota DEL ACORDE guardamos el audioold solo si esta ligado LA NOTA ACTUAL,
' al analizar debo preguntar si la nota recibe ligadura y si es asi
' por audioold de la nota anterior y si esa old es la misma nota nsE de la actual
' o sea la .i1old.
' Solo se guarda si la nota actual es ligada a la siguiente F+, en ese
' caso adectara a la siguiente nota  
'     print #1,"AFI 19a: muevo nuevos >0  a old"
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
Print #1,"AcordeOFFIguales: liga:", ligaglobal
'limpiarLigaduras(cnt,pasoCol())
Print #1,"limpiado AcordeOFFIguales: liga:", ligaglobal
'' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

Print #1,"============> AFI 20:FIN AcordeOffIguales o simple"
End sub

Sub AcordeIguales ( pasoCol() As vec, cnt As UByte,cntold As UByte, vel as UByte, tiempoDur As Double,Roll As inst,velpos As Integer,pis As UByte,portsal As UByte) 
' todas las notas son de igual duracion, cnt cantidad de notas
'print #1,"call acordeon iguales"
AcordeOnIguales	 pasoCol() , cnt , cntold , vel,tiempoDur, Roll,velpos,pis,portsal
'Print #1,"**AOI ACO:pasoCol(1).notapiano, tiempo FiguraOld ", pasoCol(1).notapiano,pasoCol(1).tiempoFiguraOld
'Print #1,"**AOI ACO:pasoCol(2).notapiano, tiempo FiguraOld ", pasoCol(2).notapiano,pasoCol(2).tiempoFiguraOld
'Print #1,"**AOI ACO:pasoCol(3).notapiano, tiempo FiguraOld ", pasoCol(3).notapiano,pasoCol(3).tiempoFiguraOld
'Print #1,"**AOI ACO:pasoCol(4).notapiano, tiempo FiguraOld ", pasoCol(4).notapiano,pasoCol(4).tiempoFiguraOld
'Print #1,"**AOI ACO:pasoCol(5).notapiano, tiempo FiguraOld ", pasoCol(5).notapiano,pasoCol(5).tiempoFiguraOld

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
'print #1,"--------------------------------------------"
'print #1,"AFD :]start AcordeOffDistintos cnt,cntold,pis ",cnt,cntold,pis
'print #1,"-------------------------------------"
' NO DISPLAY veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)


Dim  As UByte i1, coff,canal
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

'print #1,"SORT POR tiempoFigura calculado en playAll"
' en realida qsort no lo necesito es mejor usar algo como indiceaudio
' es solo una pasada qu em eindica el orden y chauuu una estupidez usar sort
' de todo el vector 
'For i1=1 To coff
 ' print #1,"AOFFD:antes Sort Fig, DUR notapiano ";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura;" ";pasoCol(i1).liga 
'Next i1
   qsort(@pasoCol(1).tiempoFigura, cnt, SizeOf(vec), @QCompare )
'For i1=1 To coff
'  print #1,"AOFFD:deespues sort DUR notapiano fig";pasoCol(i1).Dur;" ";pasoCol(i1).notapiano;" ";pasoCol(i1).tiempoFigura;" ";pasoCol(i1).liga 
'Next i1   
'-----------------------------------------
'print #1,"====>>> LUEGO SORT AOFF OFF veo el pasocol que tiene"
'For i1=1 To coff 'CNT 20-06-2021 JMG
'  print #1,"DUR:";pasoCol(i1).DUR;" ";"notepiano:";pasoCol(i1).notapiano;figura(pasoCol(i1).DUR); _
''''''  " .liga:";pasoCol(i1).liga;" old_time:";pasoCol(i1).old_time    
'Next i1

' ---------------------------------------
Dim As Double tiempoFigura
print #1,"-------------------------------------"
Print #1,"AOFFD:start AcordeOffDistintos"


 old_time_off=Timer ' para notas no ligadas
 print #1,"AOFFD:old_time off no ligadas inicial ";old_time_off

Print #1,"FOR: AOFFD:no ligados calculo tiempo Figura y off:" 
Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
Print #1,"FOR 1:AOFFD:cnt "; cnt; "i1 "; I1; " pasoCol(i1).liga "; pasoCol(i1).liga;" notapiano ";pasoCol(i1).notapiano 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
     print #1,"FOR 2:AOFFD:i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time_off, tiempoFigura
     print #1, "FOR 3:AOFFD:SIN LIGAR OFF==>"; 
     print #1,"FOR 4:AOFFD:i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * d11
Print #1,"5) AOFFD:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
Print #1,"6) AOFFD:start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
   print #1,"FOR 7:AOFFD:HAY LIGADOS!"
   print #1,"FOR 8:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   print #1,"FOR 9:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
         print #1,"FOR 10:AOFFD: ligado no se envia off,old  es mayor a la mayor de no ligado "
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
       print #1,"FOR 11:AOFFD:i1 ";i1;" tiempoFigura q falta para el off de ligado ";
        print #1,pasoCol(i1).tiempoFigura
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
        print #1,"FOR 12:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld
     Else 
         old_time_on= pasoCol(i1).old_time/d11 '20-06-2021 habilitado
         print #1,"FOR 13:AOFFD:old_time_on ";old_time_on        
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /d11
         print #1,"FOR 14:AOFFD:pasoCol(i1).tiempoFigura ";pasoCol(i1).tiempoFigura
         print #1,"FOR 15:AOFFD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
         print #1, "FOR 16:AOFFD:retardo tf ";tf
         If TF > 0 Then ' usamos el old_time-on que venia de antes
           duracion old_time_on, tf
           If CANCIONCARGADA =TRUE Then
             If Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
              print #1,"FOR 17:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal,i1
             EndIf

           Else
             If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
Print #1,"FOR 18:pasoCol(i1).i1 , jply+1 ", pasoCol(i1).i1 , jply+1
Print #1,"FOR 18+:figura(pasoCol(i1).DUR)",figura(pasoCol(i1).DUR)

Print #1,"FOR 19:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" " ;  figura(pasoCol(i1).DUR)
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
               print #1, "FOR 20:AOFFD:retardo recuperado en condicion (=) ..tf= ";tf
               print #1,"FOR 21:pasoCol(i1).notapiano:";pasoCol(i1).notapiano
              duracion old_time_on, tf
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFiguraOld   '- tiempoFigMayorNoligado  
              print #1,"FOR 22:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld 
 '    comentado  noteoff pasoCol(i1).notapiano ,canal ' 21-06-2021
           Else

              print #1,"FOR 23:AOFFD:NO SE ENVIA OFF TF=0"
              pasoCol(i1).liga=1 '13-06-2021 PARA QU ESIGA EN EL OTRO PASO SI NO SE BORRA....
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ' 13
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld '- tiempoFigMayorNoligado
              print #1,"FOR 24:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld ' 13
           EndIf  
         EndIf 
         print #1,"FOR 25:pasoCol(i1).liga ", pasoCol(i1).liga
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
         print #1,"FOR 26:<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"
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

'print #1,"AcordeOFFDistintos: liga:", ligaglobal
limpiarLigaduras(cnt,pasoCol())
'print #1,"limpiado AcordeOFFDistintos: liga:", ligaglobal

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
'   Print #1, "error no abre el archivo  ", cadena
'   Print #1, "Nombre  "; cadena
'   Print #1, "error  "; Err'

'Else
'   Sleep 100
'   Print #1, "abrio archivo  gp "; cadena, gp
'  If FileExists( cadena  ) Then
'    Print #1, "File found: " & cadena
'  Else
'    Print #1, "1 File not found: " & cadena
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

       Print #1, miditxt(j1).sumatiempo ;" ";
       Print #1, miditxt(j1).canal     ;" ";     
       Print #1, miditxt(j1).estado    ;" ";
       Print #1, miditxt(j1).nota      ;" ";
       Print #1, miditxt(j1).vel       




   Next j1

''End If

FileFlush(5)


 Close #5
Print #1,"cerro gp?? donde mierda lo crea "

'  If FileExists( cadena  ) Then
'    Print #1, "File found: " & cadena
'  Else
'    Print #1, "2 File not found: " & cadena
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
'-------------playAll-----08-02-2025-------
Sub playAll(Roll As inst) ' play version 3 CON TICKS
'<<< 30-03-2025 anda ok para roll desde ejec o Roll desde entrada manual >>>>
' en manual las velocidades son una sola semi fuerte, hasta que compas pueda 

On Local Error Goto fail
'''VerMenu=0
If MIDIFILEONOFF = HABILITAR  Then 
   MICROSEGUNDOS_POR_NEGRA = 60000000/tiempoPatron ' 60 MILL /BPM
   '' SE AJUSTO A 2000 PARA ESCUCHAR LO MISMO A 60 DSRG POR NEGRA... 500 
   '' NO ESTA CLARO EL  TEMPO EN BPM SON 60 EN TIEMPO 1000000 ,NI 500 NI 1000 NI 2000
   '' FALTRA ENTENDER MAS 
   indicenotas=0
   Dim As String NombreTrack, tiempo 
   If TipoCompas <> pmTk(0).tipocompas And TipoCompas > 0 Then
      tiempo = tempoString(TipoCompas)
   Else 
      tiempo = tempoString (pmTk(0).tipocompas)
   EndIf
   midiplano=20
   Dim numc As Integer = CInt(pmTk(0).canalsalida) + 1
   NombreTrack= sacarpath(titulosTk(ntk)) 
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
 '''''View Print 1 To 4

' tiempo es cuantas negras en un minuto tiempoPAtron
' PLAY mas avanzado en un mismo acorde si son de distinta duracion
' sus notas se toca cada una con su propia duracion,el corde no termina
' hasta que termine de tocar la nota mas larga.
 ' abrir hasta 32 dispositivos
'Dim Shared midiin As RtMidiInPtr
Dim As Integer porterror,nousar
porterror=Err
PARAR_PLAY_MANUAL=0
Dim As Integer i1,k1
' creoa todos los defaults siempre
 

' los nombres ya fueron cargados al inicio
If  GrabarPenta=0  Then ' con 1 ya esta abierto
Print #1,"abriendo port....play All"


  
   k1=CInt(pmTk(0).portout)
   portout=k1
    
'   Print #1,"midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
'     Print #1,"No se usa Microsoft"
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( )
           listoutCreado( k1)=1 
        EndIf 
        open_port midiout(k1),k1, nombreOut(k1)
            porterror=Err 
        listoutAbierto( k1) = 1
 '       Print #1,"abro ",*nombreOut(k1)
        porterrorsub(porterror)
   Else
      Print #1,"pORT SALIDA YA ABIERTO EN PLAYALL"
   EndIf
 EndIf 

'Print #1,"-------------------------------------"
'''''''midisal=midiout(0) ' el z
End If


'-------------------------------------------- 
 
    ''''''''midisal=midiout(pmTk(ntk).portout -1)
    
 '  Print #1,"Port usando en Play All ",portout
 
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
Dim as Integer cpar=0,nj, durj,tiempoFiguraSig
Dim As UByte dura=0,duraold=0
Dim As Integer liga=0,notapiano=0,old_notapiano=0, iguales=0, distintos=0
Dim leng As UInteger <8>
Dim result As Integer
playloop2=NO



     ChangeProgram ( pmTk(0).patch, pmTk(0).canalsalida, pmTk(0).portout)
    patchsal =pmTk(0).patch ''Roll.trk(1,NA).inst

Print #1,"comienzo playaLL ==========> tiempoPatron =",tiempoPatron," FactortiempoPatron",FactortiempoPatron
print #1,"playAll         ==========> tiempoDur= 60/tiempoPatron*FactortiempoPatron =", tiempoDur
jply=0:curpos=0
mousex=0
' print #1,                    "-----------------------------------------"
comienzo=posicion


If pasoZona1 > 0 Then
 comienzo=pasoZona1
 comienzoloop=comienzo
EndIf

If pasoZona2 > 0 Then
 final=pasoZona2
 finalloop=final
EndIf

STARTMIDI=Timer
old_time_on=STARTMIDI
Print #1,"old_time_on "; old_time_on
Dim As Double  tickUsuario=0.01041666 * 240/tiempoPatron
' SI TEMPOPATRON O VELOCIDAD ES 240 LA SEMIFUSA VALE ESO 0.01041666
' SI TIEMPOPATRON VALE 60 LA SEMIFUSA VALE X 4= 0,0416666
Print #1,"TickUsuario "; tickUsuario

Print #1,"VEO marca ejec enb onoff Roll.trk(1,NA).onoff   "; Roll.trk(1,NA).onoff
'Print #1,"VEO Track(0).trk(1,1).ejec "; Track(0).trk(1,1).ejec 
' ======================= FOR JPLY PLAYALL =======================
Print #1,"cominzo final PARAR_PLAY_MANUAL "; comienzo , final, PARAR_PLAY_MANUAL


For jply=comienzo To final
''Print "*******************************************************************"
''Print "AL TERMINAR EL PLAY APARECE EL MENU. CON SHIFT-M APARECERA DE NUEVO"
''Print "******************************************************************"

  kNroCol= Int(jply/NroCol)
  If (kNroCol > 0) And (jply = NroCol * kNroCol) And (jply < MaxPos)Then
     posicion=jply
     curpos=0
     SetMouse xmouse, ymouse
  EndIf


  mousex=jply * anchofig '<=== jmgjmg ' ver esto andaba bien en Roll sin ticks sin el anchofig
  If PARAR_PLAY_MANUAL = 1 Then
    If InStr(*nombreOut(portout),"Microsoft") > 0 Then
    Else
      alloff( pmTk(0).canalsalida,pmTk(0).portout )
    EndIf  
      PARAR_PLAY_MANUAL=0
      Print #1,"mefui de playall"
      Exit For
  EndIf  
' puede pasar que el maxpos sea menro al final de la secuencia porque se agrego espacio
  If jply=MaxPos Then
    If InStr(*nombreOut(portout),"Microsoft") > 0 Then
    Else
       alloff( pmTk(0).canalsalida,pmTk(0).portout)
    EndIf 
      Exit For
  EndIf
' no se si eliminarlo esto Compas no se si almacena los -1 -2 etc
If Roll.trk(1,NA).onoff = 1 Then
 'usar la velocidad de grabacion.,,
Else

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

  If Compas(jply).nro = 0 Then 
    velpos=vsemifuerte  ' para midipolano dividiones por partes veremso si se soluciona el sonido
' en la rutina vol , depende de la dur ajusta vol=0 o vol = velpos... no hay problema con los silencios
  EndIf
EndIf
'  print #1," ---------------000000000000000000000-----------------"
  print #1," (((PALL 0:)))---TICK NRO:[";jply;"] ----------------"
'  print #1," ---------------000000000000000000000-----------------"

  
' ============= For NB To NA ===============
  For i1=NB To NA 
' poner la velocidad original  del ejec si viene de una ejec
' si es manual la velocidad se calcula como esta aca resta ver como identificar que
' viene deuna ejec debo poner una marca
If i1<= NA-13 Then
' el off ya anda 01-03-2025!!! agregamos al final del if el 183 para que pase

    If (Roll.trk(jply, i1).nota >= 1) And Roll.trk(jply, i1).nota <= 12  And Roll.trk(jply, i1).dur >=1 And Roll.trk(jply, i1).dur <= 180 Or Roll.trk(jply, i1).dur <= 183 Or Roll.trk(jply, i1).dur <= 185 then 
     ' por mas que achique en octavas, Notapiano se calcula respecto del nro 
     ' completo de octavas del piano ergo 115 es fijo siempre mientras
        
      Notapiano= i1
      '**** Notapiano= 115 - i1 <<<<< con este calculo de notapiano no hace falta grabar nota en el OFF
      '**** tampoco haria falta en el on pero  si es necesario para la visualizacion >>>>>
      ' en menu podemos hacer lo mismo RollNota con off podemos sacar la nota de Notapiano
      Notapiano= Notapiano - restar (Notapiano)
      'print #1,"PALL 0:VEO LO CORRECTO DE NOTAPIANO "; Notapiano
      dura=Roll.trk(jply, i1).dur ' es una N 185 la duracion son lso ticks hasael off 1
' llegamos al final de la Columna
      portsal=pmTk(0).portout
      canal=pmTk(0).canalsalida

      If Roll.trk(1, NA).onoff=1  Then
        'Print #1,"playAll Roll.trk(jply, i1).onoff ,vol ";Roll.trk(jply, i1).onoff, Roll.trk(jply, i1).vol
        vel=Roll.trk(jply, i1).vol
      Else
        vel=velpos
        'Print #1,"2 velpos vel ";velpos, vel
      EndIf

       If Roll.trk(jply, i1).onoff =2 Then
            Print #1,"noteon CUByte(Notapiano),vel,canal,portsal  ";CUByte(Notapiano),vel,canal,portsal
            noteon CUByte(Notapiano),vel,canal,portsal,1
       EndIf
       If Roll.trk(jply, i1).onoff= 1 Then
Print #1,"noteoff CUByte(Notapiano),canal,portsal,1 "; CUByte(Notapiano),canal,portsal
           noteoff CUByte(Notapiano),canal,portsal,1
      EndIf    


     '  Print #1, "PLAYALL AcordeOnIguales velpos ", velpos 
   EndIf   
' YA BARRIO TODA LA COLUMNA AHORA ENVIA LOS NOTEON Y NOTEOFF
' PERO LOS PODRIA HABER ENVIADO A MEDIDA QUE RECORRO NO NECESITO UN VECTOR DE 
' ACUMULAICON AHORA,,, creo!! habra necesidad de procesar algo antes de enviar
' losnoteon y noteoff? talvez por ahora lo dejoa asi mas adelante veo de si
' eliminar el control de simple y acorde....!!! 28-03-2025 

EndIf  

If i1 > NA-13 Then
 If Roll.trk(jply,i1).nota = 210 Then
   ' Print #1,"210 leido jply",jply
    playloop2=SI
    comienzo2=jply
 EndIf

 If Roll.trk(jply,i1).nota = 211 Then
   ' Print #1,"211 leido jply",jply 
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
 ' print #1,"---FIN -----paso:"; jply;" --------------------------------" 
     duracion (old_time_on ,tickUsuario) ' si es cero no 1 no hay duracion es acorde
     old_time_on=old_time_on + tickUsuario 'jmgtiempo
   
 
 If playloop=SI And jply= finalloop Then
    jply=comienzoloop -1
 EndIf
 If playloop2=SI And jply= final2 Then
    jply=comienzo2 -1
    If final2=finalloop Then 
       If playloop=SI Then
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
' =========================l listo algoritmo ma�ana implemenamos
'  
jply=0:curpos=0
' 11-06-2021 se volvio a colocar 1 seg retardo para no escuchar un corte abrubto
' al final, por ahroa no parpadea mas veremos.... 
play=NO 
playb=NO


mousey=100 'otra mas para evitar rentrar a play en menu
''SetMouse xmouse, ymouse


'''mouse_event MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0

Dim As Integer checkejec
For  iz As Short =1 To 32
      If CheckBox_GetCheck( cbxejec(iz))= 1  Or CheckBox_GetCheck( cbxgrab(iz))= 1 Then
          checkejec=1
      End If
      Exit For
Next iz
if GrabarPenta=0 and GrabarEjec=HabilitaGrabar and Parar_De_Dibujar=NO And checkejec=0 Then 
 ' nada de off estamos en grabarpenta por teclado o Grabar o tocar ejecuciones 

   k1=pmTk(0).portout
  ' Print #1,"midiout ",k1, *nombreOut(k1)
   alloff( pmTk(0).canalsalida,k1 )  
   'out_free   midiout(k1)
   ''Print #1,"desmarco ",*nombreOut(k1)
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
VerMenu=1
Sleep 100,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion



''ThreadDetach(thread2) ' 16-06-2022

Exit Sub

fail:
 Dim errmsg As String
FileFlush (-1)
If  Err > 0 Then
  errmsg = "FAIL PlayAll Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg ,"jply ", jply, "i1 ";i1
EndIf
End 0

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
       ' print #1,"Qsort called";cnt;" time(s) with";el1;" and";el2;"."
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

'http://www.music-software-development.com/music-sdk.html
'http://www.ccarh.org/courses/253/handout/midiprotocol/
' KARAOKEUSA WIN API https://www.freebasic.net/forum/viewtopic.php?t=25312
'For it to run equally on 32/64 bit's FBC, you'll probably have to:
'- replace all *Integer* with *Long* as a first step (fixed 32bit INT-Type) since,
'*Integer* on 64bit FBC, becomes a 64bit variable, which isn't wanted.
'------------------------
' LIBRARY MIDI DLL CARGA EJECUTA MIDI KARAOKE LICENCIA LPGL SE PUEDE USAR COMO LIB
'https://freebasic.net/forum/viewtopic.php?f=14&t=26725

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
'print #1,"En Duracion COMIENZA RETARDO En  time :"; old_time
'print #1, "tiempoFigura " , tiempoFigura ' o timestamp
'Static As Double start
Static as LARGE_INTEGER delay 
delay.QuadPart = -1 
  Do
    NtDelayExecution(FALSE,@delay)
  Loop Until Timer - old_time >= tiempoFigura

End Sub
'print #1,"Fin duracion"

Sub duracionokOLD (old_time As Double, tiempoFigura As Double)
' retardo puro sin on ni off dejo de andar porque ???
'print #1,"COMIENZA RETARDO En  time :"; old_time
'print #1, "tiempoFigura " , tiempoFigura
Dim As Double  endtime 
 
  Do
    Sleep 1
  Loop Until Timer - old_time >= tiempoFigura

End Sub


Sub listports( )


Print #1,"LISTPORTS portsout, portsin", portsout, portsin
If  portsin=0 Then
    portsin=2
EndIf
If  portsout=0 Then
    portsout=2
EndIf

ReDim listout(0 To portsout -1)
ReDim listin (0 To portsin  -1) 
' saco de la lista los ports ya abiertos
Print #1,"LISTPORTS despuesde redim "

Dim nombre As ZString ptr
Dim aviso As String = " Abierto"
Dim temp As String=""
Dim lg As Integer  
' "Output port"

Dim  As Integer i,j
Print #1,"LISTPORTS p portsout "; portsout 
If  portsout= 0 Then 
  Print #1,"LISTPORTS portsout 0 "
  Exit Sub
EndIf 
for i = 0 to portsout -1 
  If listoutAbierto (i) =0 Then
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then ' microsoft no funa bien
      listout(i) = "Crash No usar Microsoft" 
      Print #1,"listout(i) ",listout(i)

    Else
     listout(i) = *nombre
      Print #1,"listout(i) ",listout(i)
    endif
  EndIf  
  If listoutAbierto (i) =1 Then
'    lg=Len(*port_name(, i))
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then
      listout(i) = "Crash No usar Microsoft" 
      Print #1,"listout(i) ",listout(i)

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
          Print #1,"listin(i) micro ",listin(i)
    
        Else
         listin(i) = *nombre
          Print #1,"listin(i)  2 ",listin(i)
        endif
      EndIf  
      If listInAbierto (i) =1 Then
        nombre = nombreIn(i)
        If InStr(*nombre,"Microsoft") > 0 Then
          listin(i) = "Crash No usar Microsoft" 
    Print #1,"listin(i) micro  2",listin(i)
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
' SUBRUTINAS PARA TRASPONER ADICIONALES PARA TICKS
Sub BuscoFinalNota   (Roll As inst, hastat As integer, jpt As integer, i1 As integer, ByRef jpt3 As integer, cant As integer)

Dim As Integer kx, ky
Print #1,"Entra Busco OFF del ON recibido,, hastat ,jpt,  i1 ",hastat, jpt, i1
' el on si esta en el intervalo se mueve si o si 
' solo debo detectar 1ero que el off esta fuera del intervalo y 2do ir mas halla
' en contrar el off y trasponerlo junto al ON que se mueve si o si,

Dim ind As Integer
jpt3=0

   If cant > 0 Then  ' UP  
      ind = i1 + cant 
      ind = ind + sumar(ind)
   EndIf
   If cant < 0 Then  ' DOWN  
      ind = i1 + cant 
      ind = ind - sumar(ind)
   EndIf

 For kx =jpt+1 To Maxpos  '' + una redonda ?? 96*4

  If  Roll.trk (kx,i1).onoff = 1 Then '''And Roll.trk (kx,i1).dur > 0 Then
      Print #1,"//encontro un OFF// jpt3 i1  ";kx,i1  
      jpt3=kx
    If jpt3 <= hastat Then
      Print #1,"Encontro OFF  dentro del rango jpt3 i1 ",jpt3,i1
    Else 
    Print #1,"Encontro OFF fuera del rango jpt3 i1 ",jpt3,i1 
    EndIf
      Exit Sub
  EndIf
 Next kx 



End Sub
'--------------
Sub BuscoComienzoNota(Roll As inst, desdet As Integer, jpt As Integer, i1 As Integer, ByRef jpt2 As Integer, cant As integer  )
'buscamos onoff=2 , dada la posicion de un onoff=1 dentro del rango
'jpt, i1  es la posicion de entrada del onoff=1
'jpt2 , i2 la salida si existe de onoff=2 ,si existe todo se traspone
' jpt es el eje x horizontal, i1 el vertical
' barremos el horizontal desde jpt hacia valores mas peque�os vamos a la izquierda
' O PODEMOS IR DESDE IZQUIERDA HASTA EL OFF 1 ES IGUAL
Dim As Integer kx, ky
Print #1,"Entra Busco ON  desdet ,jpt,  i1 ",desdet, jpt, i1
'Print #1,"puta porque mierda no encontaras onof 2???? !!!!"
Dim ind As Integer
jpt2=0
   If cant > 0 Then  ' UP  
      ind = i1 + cant 
      ind = ind + sumar(ind)
   EndIf
   If cant < 0 Then  ' DOWN  
      ind = i1 + cant 
      ind = ind - sumar(ind)
   EndIf

 For kx =desdet To jpt   
  ' Print #1,"kx ";kx;" onoff i1 "; Roll.trk (kx,i1).onoff,i1
  ' If Roll.trk (kx,i1).onoff > 0 Then
  '    Print #1,"kx Roll.trk (kx,i1).onoff ";kx;" ";Roll.trk (kx,i1).onoff
  '    Print #1,"kx Roll.trk (kx,i1).dur   ";kx;" ";Roll.trk (kx,i1).dur
  ' EndIf

  If  Roll.trk (kx,ind).onoff = 2 Then '''And Roll.trk (kx,i1).dur > 0 Then
      Print #1,"//encontro un ON// jpt2 i1  ";kx,i1  
      jpt2=kx
      Exit For
  EndIf
 Next kx 

Print #1,"sale Busco ON  jpt2 i1 ",jpt2,i1 
End Sub

Sub moverDatosenY (Roll As inst, jpt As Integer, i1 As Integer,cant As integer)
Dim ind As integer
   If cant > 0 Then  ' UP  
      ind = i1 + cant 
      ind = ind + sumar(ind)
   EndIf
   If cant < 0 Then  ' DOWN  
      ind = i1 + cant 
      ind = ind - sumar(ind)
   EndIf


Roll.trk(jpt,ind).nota = Roll.trk(jpt,i1).nota
Roll.trk(jpt,ind).dur  = Roll.trk(jpt,i1).dur
Roll.trk(jpt,ind).vol  = Roll.trk(jpt,i1).vol
Roll.trk(jpt,ind).pan  = Roll.trk(jpt,i1).pan
Roll.trk(jpt,ind).pb   = Roll.trk(jpt,i1).pb
Roll.trk(jpt,ind).inst = Roll.trk(jpt,i1).inst
Roll.trk(jpt,ind).onoff = Roll.trk(jpt,i1).onoff

   Roll.trk(jpt,i1).nota = 181
   Roll.trk(jpt,i1).dur  = 0
   Roll.trk(jpt,i1).vol  = 0
   Roll.trk(jpt,i1).pan  = 0
   Roll.trk(jpt,i1).pb   = 0
   Roll.trk(jpt,i1).inst = 0
   Roll.trk(jpt,i1).onoff = 0
  

End Sub

'---------
Sub trasponerRoll( cant As Integer, Roll As inst, encancion As integer)
On Local Error Goto failtraspo
'AJSUTADO DE NUEVO 11-09-2021 CON EL NUEVO ALGORITMO DE OCTAVAS
' adaptado para ticks 06-03-2025
'-----------------------------------------------
' PARA TICKS DEBO IDENTIFICAR ELONOFF=2 MOVERLO CON SU CORRESPONDIENTE ONOFF=1
' SI HAY UN ONOFF=1 O SEA FIN DE NOTA, Y NO ESTA EL COMIENZO DE LA NOTA DENTRO DEL
' GRUPO O ZONA, ESA NOTA NO SE MUEVE!!
' EL INTERVALO DE ZONA COMENZARA EN UNA POSICION O TICKS=X Y FINALIZARA EN OTRA TICKS=Y
' EL ALGORITMO SOLO NECESITA CORREGIR LOS FINALES DE NOTA,SERIA SI ENCUENTRO UN FINAL
' ONOFF=1 Y ME MUEVO HACIA ATRAS BUSCANDO HORIZONTALMENTE UN ONOFF=2 DE COMIEZNO DE ESA NOTA
' ENTONCES SE MUEVE EL FINAL.. SI NO LO ENCUENTRO ESE ONOFF=1 NO SE MUEVE,,
' AL REVES SI TENGO UN ONOFF=2 COMIENZO DENTRO DEL RANGO SE MUEVE AUNQUE SU ONOFF=1 O 
'FINAL ESTE FUERA DEL RANGO, PARA ESO DEBO BUSCAR EL ONOFF=1 DE ESA NOTA AUNQUE ESTE MAS 
' HALLA DEL PASOZONA2 O FINAL DEL GRUPO Y MOVERLO,,,
' CONSTRUIR UNA RUTINA QUE BUSQUE COMIENZO DE NOTA SI LE LLEGA UN ONOFF1 Y SI LE LLEGA
' UN COMIENZO ONOFF2 QUE BUSQUE SU ONOFF1 Y VER SI ESTA DENTRO DEL GRUPO Y EN CASO QUE NO LO 
' ESTE MOVERLO DE TODAS FORMAS AUNQUE ESTE MAS HALLA DEL RANGO.  
'------------------------------------------------
' 1) FUNCIONA BUSCAR EL ON A IZQUIERDA DADO UN OFF 1 O SEA OFF. <--- LISTO
' 2) AHORA FALTA BUSCAR UN OFF 1 A DERECHA DADO UN OFF 2 O SEA ON, EN ESTE  CASO SI LO ENCUENTRO
'    QUE SE FUE DEL INTERVALO A LA DERECHA SOLO DEBO AGRANDAR EL INTERVALO A LA DERECHA HASTA
'    ALCANZAR ESE OFF, PERO PODRIAN ENTRAR OTRAS NOTAS. LUEGO SOLO DEBO MOVER ESE  OFF
'    EXCLUSIVAMENTE,,!!
''' pasoNota se ajusta en SELECCION DE ZONA PARA TRASPONER NOTAS, Ctrl click izq
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,octavaDeAcorde,verticalEnOctavaVacia  

' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(1) si pulso flecha UP
  Print #1,"ARRANCA  TRASPONER ROLL !!!!!!!!!!!!!!",trasponer
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
Print #1, " desdet hastat comienzo final "; desdet, hastat, comienzo, final  
Dim  As Integer jpt3, jpt2 , i2 'posicion del onoff=2 inicio nota
Dim As Integer  k2, k2fin, oldjpt,oldind 
For jpt = desdet To hastat  ' eje x posiciones horizontal
  For i1= comienzo To final Step inc ' indice roll nR vertical
     If cant > 0 Then  ' UP  
        ind = i1 + cant 
        ind = ind + sumar(ind)
     EndIf
     If cant < 0 Then  ' DOWN  
        ind = i1 + cant 
        ind = ind - sumar(ind)
     EndIf
' las notas son nro de semitono 1 a 12 (en crearPenta van de 0 a 11)
    '''Print #1, "jpt,i1 "; jpt,i1

    If ( (Roll.trk(jpt,i1).nota >= 0) And Roll.trk(jpt,i1).dur <= 185 ) _
        OR (Roll.trk(jpt,i1).dur >=0 And Roll.trk(jpt,i1).dur <= 185 ) _
      Or  Roll.trk(jpt,i1).onoff > 0       Then ' es semitono
       If Roll.trk(jpt, i1).pb = 201 Then '' cuando traspongo muevo el cifrado ...�?
           octavaDeAcorde=1+ (i1-12)/13
           verticalEnOctavaVacia= 12 + (hasta-2)*13 + octavaDeAcorde - desde  
           If cant> 0 Then 
             Roll.trk(jpt, verticalEnOctavaVacia).nota=Roll.trk(jpt, verticalEnOctavaVacia).nota - 1
           Else
             Roll.trk(jpt, verticalEnOctavaVacia).nota=Roll.trk(jpt, verticalEnOctavaVacia).nota + 1
           EndIf
           Continue For
       EndIf  
  
       If ind >= NB And ind <= NA  -13 Then '' vertical
          If  pasoNota=0  Then    ' no se clickeo sobre una nota especifica 
            ' >> 183, N 185 (para ejec si no 1, 2,3,4,5,6,7,8,9)
             'Print #1,"ENTRO POR PASONOTA=0 "
' para mover un off 1 si su on esta en el intervalo, sino no se mueve
             If  Roll.trk(jpt,i1).onoff = 1   Then
              ' busco en el intervalo a izquierda si no alcanzo a  ver el on dentro del intervalo no se mueve el off
                BuscoComienzoNota(Roll, desdet, jpt, i1 , jpt2,cant )
                if jpt2 > 0 And jpt2 >= desdet Then
                   moverDatosenY (Roll, jpt,i1,cant) ' esta en el intervalo se mueve el off 1
                   jpt2=0
                EndIf
             EndIf
' para mover  un off 2 q esta en rango y su off 1 aunque este fuera de rango
             If  Roll.trk(jpt,i1).onoff = 2  Then
                 moverDatosenY (Roll, jpt,i1,cant)
                 BuscoFinalNota(Roll, hastat, jpt, i1 , jpt3,cant )
               If jpt3 > 0 and jpt3 > hastat Then ' muevo el off fuera de intevalo
                  Print #1,"Hay jpt3 > 0 EL ON  TIENE SU OFF fuera DEL INTERVALO SE MUEVE EL OFF dur, nota ";Roll.trk(jpt3,i1).dur; Roll.trk(jpt3,i1).nota
                  moverDatosenY (Roll, jpt3,i1,cant)
                  jpt3=0 
               EndIf  
             EndIf
              
          Else ' se clickeo sobre una nota especifica en el caso de que sea una sola columna 
            Print #1,"3 ENTRO POR PASONOTA=ROLL.. "
            If pasoNota = Roll.trk(jpt,i1).nota And (Roll.trk(jpt,ind).nota=0 Or Roll.trk(jpt,ind).nota=181 )  Then
               moverDatosenY (Roll, jpt,i1,cant)
                             
            Else                
             Print #1,"4 ENTRO POR PASONOTA=ROLL ELSE "
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
               moverDatosenY (Roll, jpt,i1,cant)

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

Exit Sub

failtraspo:
 Dim errmsg As String
 Dim As Long er1 = Err()
If  er1 > 0 Then
print #1,"-----------------err trasponerroll-----------------"
  errmsg = "TRASPONERROOLL FAIL Error " & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(er1)
  Print #1, errmsg
  PRINT #1, " jpt i1 jpt2 i2 ";  jpt, i1, jpt2, i2 
  FileFlush (-1)
  Close
  End 0
End If


End Sub

'-----------------------------
Sub trasponerGrupo( cant As Integer, Roll As inst, encancion As Integer,X1 As Integer , X2 As Integer)
' POR AHORA HICE QUE MUEVA SOLO UN ONOFF2, SI AGRANDO EN X Y LE DOY CLICK A VARIOS
' VOLVERA A GRUPO PERO OVERIA TODO A UN MISMA LINEA.,,,PIERDE EL SENTIDO ,,,,MMM
' Que es: el grupo es una seleccion puntual de notas que van a trasponerse se clickea 
' cada una y el resto no se toca...
' cant-> cantidad de saltos en vertical Y
' ANDA BIEN, ES EQUIVALENTE EMPIEZA EN EL EXTREMO QUE ATACA BAJANDO LA POSICION
' DE LA COPIA ES LO MISMO PERO INVERTIDO FUNCIONA IGUAL, LO IMPORTANE DEL CAMBIO
' FUE EN LA SUBRUTUNA SUMAR COMO EL VECTOR EMPIEZA DE CERO 0, EL ESPACIO ENTRE
' OCTAVAS NO QUEDA MULTIPLO DE 13 ERGO LE SUMO 1 AHORA,,,ANTES DE HACER EL MOD 13
' 31-01-2022 CORREGIDO EN BASE A tRASPONERROLL SEGUN OCTAVA NUEVA TIENE SOLO UN 
' SOLO TRASPONE DENTRO DE LA MISMA OCTAVA HAY QUE VER SI PODEMOS HACER LO MISMO
' QUE CON TRASPONERROLL Y MOVER A OTROS OCTAVAS....

'print #1,"ARRANCA TRASPONER GRUPO"
Dim As Integer Xj=1, ind=1,i1=1, Y1 , Y2, inc,b1=0, marca,jpt3
' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(-1) si pulso flecha DOWN
If cant < 0 Then ' DOWN
 Y1= NB 
 Y2 = NA -13  
 inc= 1
EndIf
If cant > 0 Then 'UP
 Y1= NA -13
 Y2 = NB  
 inc=  -1
EndIf


If X1=0 Then   X1=1 EndIf   
If X2=0 Then   X2= MaxPos EndIf   
Print #1,"trasponerGrupo X1 "; X1
Print #1,"trasponerGrupo X2 "; X2


' 30-01-2022 CORREGIDO en base a la verion ROLLMUSIC-0.1.0.0.0-U-TRACKS 
For Xj = X1 To X2  
  For i1= Y1 To Y2 Step inc
     If cant > 0 Then  ' UP  
        ind = i1 + cant 
        ind = ind + sumar(ind)
     EndIf

     If cant < 0 Then  ' DOWN  
        ind = i1+cant 
        ind = ind - sumar(ind)
     EndIf
   
      If (Roll.trk(Xj, i1).nota > 12 And Roll.trk(Xj, i1).nota < 25) And (Roll.trk(Xj, i1).dur <> 183)Then 
       If ind >= NB And ind <= NA -13 Then
           marca=Roll.trk(Xj,i1).nota
            If marca > 12 And marca < 24   And (Roll.trk(Xj,ind).nota=0 Or Roll.trk(Xj,ind).nota=181 )  Then
               Roll.trk(Xj,ind).nota =  Roll.trk(Xj,i1).nota - 12
               Roll.trk(Xj,ind).dur  =  Roll.trk(Xj,i1).dur
               Roll.trk(Xj,ind).vol  =  Roll.trk(Xj,i1).vol
               Roll.trk(Xj,ind).pan  =  Roll.trk(Xj,i1).pan
               Roll.trk(Xj,ind).pb   =  Roll.trk(Xj,i1).pb
               Roll.trk(Xj,ind).inst =  Roll.trk(Xj,i1).inst
               Roll.trk(Xj,ind).onoff = Roll.trk(Xj,i1).onoff
' ----MOVER EL OFF1 EN BASE AL  OFF2 Y SU DURACION, mueve para la ultima nota corregir!!!
               BuscoFinalNota(Roll, X2, Xj, i1 , jpt3,cant )
               If jpt3 > 0 Then ''And jpt3 > X2 Then ' muevo el off fuera de intevalo
                  Print #1,"Hay jpt3 > 0 EL ON  TIENE SU OFF fuera DEL INTERVALO SE MUEVE EL OFF dur, nota ";Roll.trk(jpt3,i1).dur; Roll.trk(jpt3,i1).nota
                  moverDatosenY (Roll, jpt3,i1,cant)
                  jpt3=0 
               EndIf  
'---------------
     abrirPortoutEjec(100)
     noteon(cubyte(PianoNota),60,1,0,1)
     noteoff(60,1,0,1)
     ''duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
     duracion(Timer, relDur(Roll.trk(Xj,ind).dur) )
               Roll.trk(Xj,i1).nota = 181
               Roll.trk(Xj,i1).dur  = 0
               Roll.trk(Xj,i1).vol  = 0
               Roll.trk(Xj,i1).pan  = 0
               Roll.trk(Xj,i1).pb   = 0
               Roll.trk(Xj,i1).inst = 0
               Roll.trk(Xj,i1).onoff = 0
                            
            Else                
               'If Roll.trk(Xj,ind).nota >=1 And Roll.trk(Xj,ind).nota <=12  Then
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
            EndIf      
       EndIf
      EndIf   
  Next i1
Next Xj
' para trasponer tracks debo grabar lo cual copia a track los cambios
' de ese modo al dar play se escuch also cambios sino solo quedan en Roll
' y el play de cancion no lo registra , solo el play de roll lo registraria
If encancion > 0 Then
 ' print #1,"en trasponer grupo graba track traspuesto"
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
' o sea lo que est  echo es para mover a izquierda donde la posiion destino
' el click esta a la izquierda de pasozona1  
/'
Print #1," chequeo"
For jpt= 1 To maxpos
  For i1=comienzo To final
       If Roll.trk(jpt,i1).nota < 13 And i1= 67 Then
        Print #1,"Roll.trk(inc,i1).nota ",Roll.trk(inc,i1).nota
        Print #1,"Roll.trk(inc,i1).dur ",Roll.trk(inc,i1).dur
        Print #1,"Roll.trk(jpt,i1).nota ",Roll.trk(jpt,i1).nota
        Print #1,"Roll.trk(jpt,i1).dur ",Roll.trk(jpt,i1).dur

       EndIf

  Next i1
Next jpt
Print #1,"fin chequeo"
'/
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
'Print #1,"UBOUND(ROLL,1)", UBOUND (ROLL.TRK,1)
'Print #1,"LBOUND(ROLL,1)", LBound (ROLL.TRK,1)
'Print #1,"UBOUND(ROLL,2)", UBOUND (ROLL.TRK,2)
'Print #1,"LBOUND(ROLL,2)", LBOUND (ROLL.TRK,2)
'Print #1,"inc=posivieja+1 ",inc
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
  Print #1,"TERMINO copia a derecha ",posinueva   
'---
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
  Print #1,"inicioind  MAxPosOld ",posinueva , MAxPosOld  
  'If posinueva > MAxPosOld Then

  ' Print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
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
'print #1,"CORRECION DE NOTAS ***********"
For jpt = desdet To hastat  
  For i1= NB To NA -13 ' 26-01-2022  
   
     If ( (Roll.trk(jpt,i1).nota >= 0) And (Roll.trk(jpt,i1).nota <= 13 ) )  Then ' es semitono
           'print #1,"Roll.trk(i1,jpt).nota ",Roll.trk(i1,jpt).nota
           'print #1, "i1",i1
           i2= i1 - restar (i1) 'Notapiano en funcion del indice del vector nR=i1  
          ' print #1, "i2",i2 ''Notapiano
          ' print #1,"relnRNe (i2) ",relnRNe (i2)
          ' print #1,"---------------"   
          If  Roll.trk(jpt,i1).nota <> relnRnE (i2) Then 
              Roll.trk(jpt,i1).nota = relnRnE (i2)
          EndIf    
    EndIf
  Next i1
Next jpt

' Notapiano=nR - restar(nR) no sirve de mucho 
' como llego de mousey a nR? kkkkkkkkkkkkk
'BordeSupRoll = BordeSupRoll -  inc_Penta
'inc_Penta = Int((ALTO - BordeSupRoll) /(40))
'Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1) ES GLOBAL
'Penta_y + (notacur-1) * inc_Penta 
'lugar=Penta_y + (semitono +1) * inc_Penta  ''ES MOUSEY!!
' LUEGO SI TENGO MOUSEY 
' nR=12 - notacur + (*po-1) * 13 'NOTACUR ES nsE 
' con saber nsE y la po octava saco nR indice vector Roll !!
' nsE=semitono + 1 'semitono ahora va desde 0 a 11 usado por entrada de teclado y ahroa mouse
'   nR=(11-semitono) + (*po -1 ) * 13
' osea semitono=nsE -1
'    nR=(11- nse  +1) + (*po -1 ) * 13 = 12 - nsE + (*po-1)*13
'   nR= 12 - nsE + (*po-1)*13
'   nR=(12-nota) + (estoyEnOctava -1 ) * 13 
'   PianoNota= nR - restar (nR)
' nR=(12-nsE) + (estoyEnOctava -1 ) * 13 !!


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
Print #1,"delta d1 d2 d3 ", delta; " ";dato1;" ";dato2;" ";dato3
Dim As Double sumadelta=0
'    If GrabarPenta=1 Then
'       nRk=dato2
'       PianoNota=CInt(nRk)
'      ' Print #1,"mycallback nRk/pianonota ";nRk  
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
        arrancaPlay=SI
        Print #1,"arranco play o sea el usuario empezo a tocar la siguiente pista"
 
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
           Print #1,"durafig(1) ",durafig(1)  
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
''Print #1,"entra a mycallback " 
Dim As UByte Ptr memoria = vec
dato1=*memoria: memoria += 1
dato2=*memoria: memoria += 1  
dato3=*memoria 
DURk =deltatime

Dim As Double sumadelta=0
    If GrabarPenta=1 Then
       nRk=dato2
       PianoNota=CInt(nRk)
      ' Print #1,"mycallback nRk/pianonota ";nRk  
       
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
         nroTicksPatron =pmEj(ntoca).MaxPos 'hay patron 
         pmEj(ntoca).MaxPos=0
         tocaparam(ntoca).maxpos=0
     EndIf
     If ntoca > 1 And jgrb=SI Then ' detiene la acumulacion de deltatime en PlayTocaAll 
          arrancaPlay=SI
 
     EndIf
      
     CargaIn( jgrb).modo=dato1
     CargaIn( jgrb).nota=dato2
     CargaIn( jgrb).vel=dato3
 
     If pmEj(ntoca).MaxPos >= nroTicksPatron And nroTicksPatron > 0   Then
' termino la grabacion del patron no se graba mas en CargaIn
        Print #1, "ENTRO POR PATRON"  
         GrabarEjec =PatronDeEjecucionCompleto
     Else 
   
' aca vamos a marcar los compases para controlar el numero de ellos las repeticiones
' las grabaciones encima o reemplazando datos y la creacion  de patrones,
          If deltatime > 0.005  Then '   5mseg  
            CargaIn( jgrb).partes=partes ' o nro Ticks, convierto deltatime en tickschico 
            pmEj(ntoca).MaxPos=pmEj(ntoca).MaxPos +partes
            tocaparam(ntoca).maxpos=pmEj(ntoca).MaxPos
          Else
          Print #1,"PARTES 0000" ' ENTRA UNA SOLA VEZ, casi no se usa??
            CargaIn(jgrb).partes=0
            pmEj(ntoca).MaxPos=pmEj(ntoca).MaxPos +1
            tocaparam(ntoca).maxpos=pmEj(ntoca).MaxPos  
          EndIf
     EndIf

  EndIf
End Function

'--------------------------------------
Sub GrabarMidiIn ( ByRef  par As  paramGrabamidi,i1 As integer)
On Local Error Goto fail
' tengo que grabar el tempo 
' tocap As vivo, ntkp As Integer,tocaparam  As ejecparam  Ptr) 
    Dim As Long j, driver=0
    Dim As String  nombreg
' y ntkp de donde vien quien lo ajusta? ntkp debe venir informado!!!
     pathdir=GetPathPart( DirEjecSinBarra)

     nombreg =pgmidi.tocap.nombre  ' 23-04-2024
     par.tocap=tocaparam(i1)
     
 Print #1,"GrabarMidiIn NombreCancion, nombre sin path",NombreCancion, nombreg
     
      driver=InStr(pathdir,":\")
      Dim  As Integer barra1, barra2  
      If  NombreCancion > "" And driver=0 Then
           barra1=InStrRev(NombreCancion,"\")
           barra2=InStr(nombreg,"\")
           Print #1, "barra1 barra2 ", barra1, barra2
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
        Print #1," va a grabar sin path ",nombreg
        If InStr(nombreg,"(")=0 Then
           nombreg="("+ doscifras(CInt(pgmidi.tocap.orden))+")"+nombreg  
        EndIf
      EndIf
      If InStr (nombreg,"ejec") = 0 Then
        nombreg=nombreg+".ejec" '"18-10-2024
      EndIf

Print #1,"nombre de archivo  grabando de ejec",nombreg
 ' carga de parametros:
Print #1, "GrabarMidiIn pmEj(ntkp).MaxPos ";pmEj(ntkp).MaxPos
Print #1, "GrabarMidiIn pgmidi.tocap.maxpos "; pgmidi.tocap.maxpos

     
   '''  par.tocap.delta   =tocaparam(ntoca).delta no estaba hace fa�ta o no
' delta sol ose usa en unif que no separa que esta en playtocaAll 
     
     nombreg=Trim(nombreg)
     par.tocap.nombre  =nombreg
      Print #1,"GrabarMidiIn titulos   ", nombreg
      Print #1,"GrabarMidiIn MAXPOS ",    par.tocap.maxpos
      Print #1,"GrabarMidiIn delta "     ,par.tocap.delta
      Print #1,"GrabarMidiIn nombre " ,   par.tocap.nombre
      Print #1,"GrabarMidiIn portout " ,  par.tocap.portout
      Print #1,"GrabarMidiIn portin "   , par.tocap.portin
      Print #1,"GrabarMidiIn patch "     ,par.tocap.patch
      Print #1,"GrabarMidiIn canal "     ,par.tocap.canal
      Print #1,"GrabarMidiIn orden "     ,par.tocap.orden 
      maxgrb=par.tocap.maxpos '27-11-2024
     Print #1,"GrabarMidiIn maxgrb" ;maxgrb
      
      ngm=15 
Print #1,"GrabaMidiin freefile ngm ",ngm
    if   Open( nombreg  For Binary Access Write As #ngm)  <> 0 Then
              Print #1,"Imposible Grabar midi in" + nombreg 
    Else  
    Dim tocaparamaux As ejecparam
    tocaparamaux=tocaparam(i1) 
    tocaparamaux.nombre=nombreg
         Put #ngm,, tocaparamaux '1ero parametros como siempre o en cabezado
         Put #ngm,, Toca(i1).trk()   '2do datos  
         FileFlush(ngm) 
    End If
Print #1,"grabado ARCHIVO ", nombreg
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
  Print #1, errmsg
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
' distinto tama�o.
' si el usu ingresa acorde en campo OK se pone 1, sino sera una nota ordinaria 
' con 0 (eso se har� en sub cursor...) <HACER>
' acorde ir� de 1 a 12 , 1 no hay acorde 12 lo maximo
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
Print #1,"abrirPortoutEjec abriendo port.... "
Dim k1 As Integer

   If j=100 Then ' para tocar una sola nota y saltarl los portout 
     k1=0
   Else
     k1=CInt(pmEj(j).portout )
   EndIf 
   Print #1,"abrirPortoutEjec midiout ",k1, *nombreOut(k1)
   If InStr(*nombreOut(k1),"Microsoft")>0 Then
     Print #1,"No se usa Microsoft"
   Else
     If listoutAbierto( k1) = 0 Then
        If listoutCreado( k1)=0 Then
           midiout(k1) = rtmidi_out_create_default ( )
           listoutCreado( k1)=1 
        EndIf 
        open_port midiout(k1),k1, nombreOut(k1)
        Dim As integer    porterror=Err 
        listoutAbierto( k1) = 1
        Print #1,"abro ",*nombreOut(k1)
        porterrorsub(porterror)
   Else
         Print #1,"PORT OUT YA ABIERTO ",listoutAbierto( k1),*nombreout(k1)
   EndIf
 EndIf 

 Print #1,"Port usando en Play Ejec teclado ",portout
Print #1,"-------------------------------------"

End Sub
'------------------
Sub PlayTocaAll(nt As Integer Ptr )
On Local Error Goto fail
' las pistas fueron cargadas en un vector redim de 384000 no uso el maxgrb habria que 
' usarlo y ponerlo como se debe,,,usar un redim de mzgrb el probel aes que maxgrb se pisaba
' en cargarmidiin ahora se arreglo veo si anda ,,,probar
'////////////////////////////TOCAALL/////////////////////////
' perfeccionar los eventos deven seguir un patron de tiempo de ticks
' para c/tick ver si hay un evento si hay enviarlo...el problem ason lso retardos
' TOCA VARIAS PISTAS EJEC 
'TickPlay = TickChico por default si quiero cambiar la velocidad debo cambiar el TickChico
'o sea se grabasiempre con elTick mas chico que es para veloc=240 y el valor del  tresillo
' 
Print #1,"PlayTocaAll 1"
ntoca=*nt  ''' almacena tocatope la cant max de ejecuciones o archivos cargados
Dim  As long j=0,k=0,partes,cuenta=0,pis=0
Dim As UByte dato1,dato2, dato3 
' cargo retardos de ejecucion ?? y no los us� para nada?? 
'For j=1 To 32
 ' espera(j)=tocaparam(j).delta ' empieza siemrpe por la 2
'Next j
Print #1,"PlayTocaAll 2"
'--------------play TOCA


'ChangeProgram ( 1, ntoca, 0)
' todas las pistas empiezan en el mismo Timer
timex(01)=Timer

For j=2 To 32 
 timex(j)=  timex(01)  
Next j

Print #1,"PlayTocaAll 3"

''Print #1,"=====> EN PLAY StartPlayejec ",StartPlayejec
Dim  topeDuranteGrabacion As integer
Print #1,"nPLAY VERDE: maxgrb ",maxgrb
'canal=1 ' por ahora
portsal=0 ' por ahora ???

Print #1,"playtoca maxgrb ", maxgrb
Print #1,"playtoca tocatope ", tocatope
If GrabarEjec=GrabarPistaEjecucion Then 
  If   tocatope >1 Then
   topeDuranteGrabacion=tocatope-1
   Print #1,"tocatope -1 "; topeDuranteGrabacion 
  EndIf
Else
   topeDuranteGrabacion=tocatope

EndIf   
Dim As Integer prox=2 
Print #1,"PlayTocaAll 4"
Print #1,"topeDuranteGrabacion ", topeDuranteGrabacion, " PISTAS"
 'For pis=1 To topeDuranteGrabacion
 '     Print #1,"ON patch pis canal ",	 , pis,tocaparam(pis).canal
 '     Print #1,"tocaparam(pis).portout ",tocaparam(pis).portout
 '     Print #1,"tocaparam(pis).patch ",tocaparam(pis).patch
'
'      ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)	'
'
'Next pis
PARAR_PLAY_EJEC=NO
'''TickPlay=TickChico*tiempoPatronEjec/240
' no hay caso no cambia nada el play deberia modificar los datos ???
' los tiempos son fijos la unica forma seria cambiar sus valores
' lo que debo hcer es restar una cantidad de ticks fijos a cada
' timex(kply)  por ejemplo timex(kply) - xcant * tickchico
' timex(kply) +  x3H * Tickchico
Dim resta  As Double 
'************************************************************************
 Print #1,"empieza el play de ejec, maxgrb, PARAR_PLAY_EJEC ", maxgrb,PARAR_PLAY_EJEC
'************************************************************************
 
Print #1,"playTocaAll jToca=maxgrb, kply=topeDuranteGrabacion ";maxgrb, topeDuranteGrabacion
'los archivos de ejec tienen distintas longitudes al cargarlos se normaliza a la mayor
' volvimos aponer maxgrb en el redim ver como funciona porque teniamos un 384 mil
' deberiamos hacer el redim con un % mas 2 por ejemplo asi estamos con 2 veremos
For jToca=1 To maxgrb ' se calcula al cargr los archivos de ejec 
  If PARAR_PLAY_EJEC = SI Then
     For kply =1 To topeDuranteGrabacion
         alloff( 1 ,pmEj( kply).portout  )
     Next  kply
      PARAR_PLAY_EJEC=NO
      Parar_De_Dibujar=NO
      Exit For
  EndIf  
''Print #1,"topeDuranteGrabacion ",topeDuranteGrabacion


  For kply =1 To topeDuranteGrabacion
   ' este cambio de patch anda bien no produce retardo por ahora y se hace cada
' vez que cambio de pista en un bariddo vertical de una posicion dada
' verificar esto en play cancion donde creo daba retardo ver,,,,��� 04-06-2022

' con LoopBe Internal MIDI" no puedo hacer change program si esta conectado
' ZynAddSubFx... nose si con otro tipo de sintetizador que reciba patch se podr�
' o solo es problema de LoopBe Internal MIDI seguir probando a ese sinte solo
' se envia canales , la configuracionde instrumentos y bancos se carga y graba
' en ZynAddSubFX
    If CheckBox_GetCheck( cbxejec(kply))= 1 Then
       If  InStr (*nombreOut(tocaparam(kply).portout),"LoopBe Internal MIDI") = 0  Then  
          ChangeProgram ( tocaparam(kply).patch , tocaparam(kply).canal, tocaparam(kply).portout)
         ' Print #1,"CHANGE KPLY, .canal, .portout) "; kply, tocaparam(kply).canal, tocaparam(kply).portout 
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
    portsal=pmEj(kply).portout '04-05-2022

     If  dato1=1 Then ' marcamos con 1 un delta de tick en modo
         duracion (timex(kply) ,TickPlay) ' si es cero no 1 no hay duracion es acorde
         timex(kply)=timex(kply) + TickPlay 'jmgtiempo
         
       If  GrabarEjec =GrabarPistaEjecucion And ntoca > 1 And arrancaPlay=0 And kply=1 Then
     '.....????       tocaparam(ntoca).delta=tocaparam(ntoca).delta+TickPlay  'jmgtiempo
         '  Print #1,"En PlayToca Toca(calltoca).delta ",Tocaparam(calltoca).delta
           ' retardos respecto del inicio de play y de pista 1, kply=1 
       EndIf

     EndIf

' PARA CALCULO DE RETARDO DEL INICIO DE PLAY CANCION RESPECTO PLAYTOCAALL
' HABILITAMOS SOLO PARA PRUEBAS ,tenemos 2.7 mseg de retardo ,medio Tick (5mseg)
' If  jToca=1 Then
'  Print #1, "playTocaAll inicio datos:", Timer
'EndIf
'--------------------------------------
'Print #1,"dato1", dato1
     Select Case  dato1 
         Case 144 ' on
            noteon dato2,dato3,tocaparam(kply).canal, tocaparam(kply).portout, 1 'message(3) ' noter vel canal
           'Print #1,"ON ",dato2,dato3,pmEj(kply).canalsalida, pmEj(kply).portout
         Case 128 'off
            noteoff dato2,tocaparam(kply).canal ,tocaparam(kply).portout,1 'message(2)'
           'Print #1,"OFF ",dato2,pmEj(kply).canalsalida ,pmEj(kply).portout  
     End Select
 
  Next kply
''''Sleep 1,1 ' para que corranmas de un thread �??? 20-06-2022 porque lo puse?
Next jToca
''jToca=0
Parar_De_Dibujar=NO
If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Then ''' Or instancia < 3 Then
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
         rta= inputBoxJmg("Guardo esta ejecucion", "SI O NO", "SI", ES_MULTILINE + ES_AUTOVSCROLL,0  )
'( USTRING, Message , DefaultString, flag ,  flag2, hParentWin as Hwnd = 0) 
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
  Parar_De_Dibujar=NO
  Sleep 20,1
  ThreadDetach threadG
Exit Sub 

fail:
 Dim errmsg As String
If  Err > 0 Then
  errmsg = "FAIL Error PlayTocaAll" & Err & _
           " in function " & *Erfn & _
           " on line " & Erl & " " & ProgError(Err)
  Print #1, errmsg
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
' 1) tomar una ejecucion por  midi o pista manual existente y grabarla como FUTURO patron asi como est�.
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
          Print #1, "mousex,mousey ", pun.x,pun.y
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

Print #1,"------------------------------------"
  ContadorError = ContadorError+1
  Print #1,"ErrorRTMIDI ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
  Print #1,"Error Function: "; *Erfn()

EndIf
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )




