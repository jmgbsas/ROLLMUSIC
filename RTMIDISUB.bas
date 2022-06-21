On  Error GoTo errorrtmidi

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
Sub noteoff(  note As UByte, canal As UByte,portsal As UByte) 
' canal 1
' 123 da note off para todas las notas solo hy qu eenvirlo a 
'todoslos canales
' voy a tener que enviar el nro de canal que se usa en cada pista al tocarla...

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
Sub noteon	( note As ubyte, vel As UByte, canal As UByte, portsal As UByte)
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
'Print #1,"EN NOTE ON nota, portsal ", note, portsal
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
Dim As Integer sil=0, ons=0
For i1=1 To cnt
 If pasoCol(i1).audio=1 Then ' 1= suena, 2 silencio
    ons=1
 EndIf
 If pasoCol(i1).audio=2 Then
    sil=1
 EndIf
Next i1
' qsort
If sil=1 And ons=1 Then
'  Print #1,"===>LLAMO A SORTAUDIO "
  sortaudio pasocol(),cnt
  p1=1
Else
  p1=0  
EndIf 
'''' NOT DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)
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
         if pasoCol(i1).vol = 0 Then
            vel=0
         EndIf   
         noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal
      Else
          Dim As Integer cret 
          If pasoCol(i1).audio=1 And pasoCol(i1).audioold=2 Then
             canal=pasoCol(i1).canal
             portsal=CUByte(pasoCol(i1).port)
             vel= CUByte(vol( pasoCol(i1).DUR, velpos))
             if pasoCol(i1).vol = 0 Then
                vel=0
             EndIf   

             noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal 
             pasoCol(i1).old_time=old_time_on_int
          EndIf
         ENDIF 
   Case   91  To 180 ' CON LIGADURA  
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
               if pasoCol(i1).vol = 0 Then
                  vel=0
               EndIf   
               
               noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal
               pasoCol(i1).old_time=old_time_on_int
               pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ''''aca toma 0.25 
               pasoCol(i1).old_timeold=old_time_on_int
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
                 If pasoCol(i1).audioOld=1 And pasoCol(i1).audio=2 Then
                    duracion pasoCol(i1).old_timeold/d11, f '25-11
                    portsal=CUByte(pasoCol(i1).port)
                    noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal  ' la notapiano es la misma porque esta ligado 
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
                if pasoCol(i1).vol = 0 Then
                   vel=0
                EndIf   

                noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal
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
     End Select 
         pasoCol(i1).ligaold = pasoCol(i1).ligaold + 1 ' para el proximo paso
     
 End SELECT

Next k1

' for de NOTEON DE NO LIGADOS, EN NOTEON NO HACE FALTA SORT SOLO EN OFFS"

'print #1,"=====> FIN ACORDEON IGUALES O SIMPLE"
'print #1,"=AOI tenia   old_time_on_int ", old_time_on_int

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
'1)HAGO EL SORT POR RELDUR ASC., TOMO LA ULTIMA, SERÁ LA MAYOR DURACION 
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
     if pasoCol(i1).vol = 0 Then
        vel=0
     EndIf   

     noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal
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
      ' cuando termine el for, habré guardado el tiempoFigura mayor de lso 
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
    if pasoCol(i1).vol = 0 Then
       vel=0
    EndIf   

    noteon CUByte(pasoCol(i1).notapiano),vel,canal,portsal
 End If
Next i1

'print #1,"FIN AcordeOnDistintos"
	
 
End Sub

Sub AcordeOffIguales	(  pasoCol() As vec, cnt As UByte, cntold As UByte,Roll As inst, pis As UByte,portsal As UByte)
Dim As UByte i1,canal
Dim tiempoFigura As Double 
'print #1,"-------------------------------------"
'print #1,"AFI :]========Start AcordeOffIguales cnt,cntold,pis ",cnt,cntold,pis
'print #1,"-------------------------------------"
'debug..
' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

'aca son iguales ergo lso off caen todo iguales al mismo tiempo
' solo que una not aligada hara off en el siguiente paso
'--------


 old_time_off=Timer
' print #1,"AFI 1:old_time off inicial ";old_time_off
 old_time_off_int = old_time_off * d11 ' solo para guardar y poder hacer sort
 
'print #1,"AFI 2:no ligados calculo tiempo Figura y doy off:" 
' son iguales las duraciones son las mismoas no ahce falta sort
' este 1er for tendra las duraciones mas cortas pues son acordes iguales 
' y si hay un acorde debe ser igual y si esta ligado a un acorde 
' anterior es mayor a este caso y sera el caso FOR siguiente
For i1 = 1 To cnt ' (1) 
'Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld=0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
'     print #1,"AFI 3:i1 ";i1;" no ligado tiempoFigura ";tiempoFigura
'     print #1,"i1 ";i1;" no ligado old_time ";pasoCol(i1).old_time
'     print #1, "AFI 4:i1 ";i1;" AcordeOffIguales: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
'     print #1, "AFI 5:call old_time o retardo con ",pasoCol(i1).old_time 
'     print #1, "AFI 6:call duracion o retardo con ",tiempoFigura
     duracion pasoCol(i1).old_time / d11, tiempoFigura
'     print #1, "AFI 7:SIN LIGAR NOTEOFF==>" , pasoCol(i1).notapiano
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
'Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld     
'      print #1,"AFI 7a: reseteo los old"
       pasoCol(i1).tiempoFiguraOld=0
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
             'pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 

'Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  EndIf


Next i1     

'print #1,"AFI 8:start OFF de ligados ---------" 
 Dim tf As Double
' los ligados pueden terminar en el paso 2 o seguir eso lo determino
' con el numero de liga qu epuede ser 1,2,3,4,etc
'
' duracion algo mayor 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
'Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
  If pasoCol(i1).liga = 0 And pasoCol(i1).ligaOld >0 Then
'     print #1,"AFI 9:HAY LIGADOS que llegan y terminan aca!"
'     print #1,"AFI 10:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
'     print #1,"AFI 11:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
              'old_time= pasoCol(i1).old_time/100000000000 
    '''' print #1,"old_time_on ";old_time_on
    '''' V20 audioOl>=1 o sea 1 o 2
     If tipoAcorde=1 Or pasoCol(i1).audioOld =2 And pasoCol(i1).audio=1 Then        
       tf = pasoCol(i1).tiempoFigura  /d11
     Else
       tf = pasoCol(i1).tiempoFiguraOld  /d11
     EndIf
'     print #1, "AFI 12:retardo tf ";tf
     ' el time tomo del inicio de aca del off porque ya gaste duracon de la 
     ' ligarura en el paso anterior
     If TF > 0 Then 
'     Print #1,"puta entras o no pro aca "
     ' la nota ligada es simple y ataca luego a una nota de un acorde
     ' se parte la duracion en un pedazo luego de la ligada en noteon
     ' y otro pedazo de la duracion o reardo luego de la resolucion 
       If pasoCol(i1).tiempoFiguraOld = pasoCol(i1).tiempoFigura And pasoCol(i1).tiempoFigura > 0 Then
          If TipoAcorde=1 Then
'          Print #1,"por aca no debe entrar tfold=tf" 
            duracion old_time_off , tf
          EndIf  
       Else
         If pasoCol(i1).tiempoFiguraOld > pasoCol(i1).tiempoFigura Then 
  '''' V20 -> And pasoCol(i1).audio=2        
            If  tipoAcorde > 1 And pasoCol(i1).audioOld=1 And pasoCol(i1).audio=2 Then 'zzzzzzz
'                Print #1,"audioold=1 y tfold> tf"
                duracion pasoCol(i1).old_timeold/d11, tf ' falta cambiar TF 16-11-2021
            Else
               If pasoCol(i1).audioOld=2 And pasoCol(i1).audio >=1 Then
 '                 Print #1,"uso old_time ",pasoCol(i1).old_time
                  duracion pasoCol(i1).old_time/d11,tf
               Else
                  If pasoCol(i1).audioOld=1 And pasoCol(i1).audio=1 Then
                     duracion pasoCol(i1).old_time/d11,tf
                  Else
'                     Print #1,"uso old_time_off de aca ", old_time_off  
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
            noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
        '''  'liga cero no
       ' ''' EndIf
'       Else
'          print #1,"AFI 14: ligado DOY OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; _
'          figura(PasoCol(i1).dur)
             
'           noteoff CUByte(pasoCol(i1).notapiano) ,canal
         
'       EndIf
       
     EndIf
'     print #1,"AFI 14a: reseteo los old ,,notapiano ",pasoCol(i1).notapiano
       pasoCol(i1).tiempoFiguraOld=0 ''' no sirve  24-11-2021
       pasoCol(i1).ligaOld =0
       pasoCol(i1).old_timeold=0
       '''pasoCol(i1).DURold=0  comento als que no uso todavia
       pasoCol(i1).notapianoOld=0
       pasoCol(i1).audioOld = 0 
       pasoCol(i1).i1old = 0 
       
'    Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld   
  EndIf 

  ' le resto 1 a la liga para decir que paso este paso y en el proximo
  ' si llega a cero envia el noteoff entonces
'  pasoCol(i1).liga = pasoCol(i1).liga - 1 ' 05-11-2021
'  If pasoCol(i1).liga < 0 Then
'     pasoCol(i1).liga=0
'  EndIf
'  print #1,"le resto 1 a  pasoCol(i1).liga ", pasoCol(i1).liga
 '05-11-2021 comentado' ligaglobal = pasoCol(i1).liga ' por si sigue una simple

Next i1
' print #1,"AFI 15: fin ligados off"

' print #1,"AFI 16: start ligados continuacion en este paso " 
For i1=1 To cnt
' solo la tiro despues en el s2do paso cuando ligaglobal se ajsuto a 0 
' verificar si la ajusto
'Print #1,"pasoCol(i1).notapiano, tiempo FiguraOld ", pasoCol(i1).notapiano,pasoCol(i1).tiempoFiguraOld
' and NOT 25-11
  If pasoCol(i1).liga > 0 And pasoCol(i1).ligaOld >0 Then ''And Not (pasoCol(i1).audio=1 And pasoCol(i1).audioOld=2)  Then
'     print #1,"AFI 17:HAY LIGADOS que llegan y siguen!"
'     print #1,"AFI 18:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
   '''''''''old_time= pasoCol(i1).old_time/100000000000 
   '''''' ' print #1,"old_time_on ";old_time_on        
     pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld
'     print #1,"AFI 19:pasoCol(i1).tiempoFigura ",pasoCol(i1).tiempoFigura
     pasoCol(i1).liga=1
     pasoCol(i1).old_time=pasoCol(i1).old_timeold
'------------audio en un solo for es suficiente
' en cada nota DEL ACORDE guardamos el audioold solo si esta ligado LA NOTA ACTUAL,
' al analizar debo preguntar si la nota recibe ligadura y si es asi
' por audioold de la nota anterior y si esa old es la misma nota nE de la actual
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
         noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
        
      EndIf
  EndIf 
  
Next i1
' limpiando ligaduras 05-11-2021 por cancion	,
' ligalobal dice si al menos una nota esta ligada
'print #1,"AcordeOFFIguales: liga:", ligaglobal
'limpiarLigaduras(cnt,pasoCol())
'print #1,"limpiado AcordeOFFIguales: liga:", ligaglobal
'' NO DISPLAY  veoPasoCol pasoCol(),CInt(cnt), CInt(cntold)

'print #1,"AFI 20:FIN AcordeOffIguales o simple"
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
'print #1,"-------------------------------------"
'print #1,"AOFFD:start AcordeOffDistintos"


 old_time_off=Timer ' para notas no ligadas
' print #1,"AOFFD:old_time off no ligadas inicial ";old_time_off

'print #1,"FOR: AOFFD:no ligados calculo tiempo Figura y off:" 
Dim tiempoFigMayorNoligado As Integer 
For i1 = 1 To cnt ' (1)
'print #1,"FOR 1:AOFFD:cnt "; cnt; "i1 "; I1; " pasoCol(i1).liga "; pasoCol(i1).liga;" notapiano ";pasoCol(i1).notapiano 
  If pasoCol(i1).liga = 0 Then
     tiempoFigura = pasoCol(i1).tiempoFigura/d11
'     print #1,"FOR 2:AOFFD:i1 ";i1;" tiempoFigura ";tiempoFigura
     duracion old_time_off, tiempoFigura
'     print #1, "FOR 3:AOFFD:SIN LIGAR OFF==>"; 
'     print #1,"FOR 4:AOFFD:i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" ";figura(pasoCol(i1).Dur)
     canal=pasoCol(i1).canal
     portsal=CUByte(pasoCol(i1).port)
     noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
  EndIf
Next i1     
tiempoFigMayorNoligado=  tiempofigura * d11
'print #1,"5) AOFFD:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
'print #1,"6) AOFFD:start OFF de ligados ---------" 
 Dim tf As Double

For i1=1 To cnt
  If pasoCol(i1).liga >0  Then
'   print #1,"FOR 7:AOFFD:HAY LIGADOS!"
'   print #1,"FOR 8:pasoCol(i1).tiempoFiguraOld ",pasoCol(i1).tiempoFiguraOld
'   print #1,"FOR 9:tiempoFigMayorNoligado ";tiempoFigMayorNoligado
     If  pasoCol(i1).tiempoFiguraOld < tiempoFigMayorNoligado Then
'         print #1,"FOR 10:AOFFD: ligado no se envia off,old  es mayor a la mayor de no ligado "
         pasoCol(i1).tiempoFigura= tiempoFigMayorNoligado - pasoCol(i1).tiempoFiguraOld
         'le reste el mayor de los no ligados OLD al ligado 
 '       print #1,"FOR 11:AOFFD:i1 ";i1;" tiempoFigura q falta para el off de ligado ";
  '      print #1,pasoCol(i1).tiempoFigura
     ' added cambio 07 06 acum old jmg   
        pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFigura
 '       print #1,"FOR 12:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld
     Else 
         old_time_on= pasoCol(i1).old_time/d11 '20-06-2021 habilitado
 '        print #1,"FOR 13:AOFFD:old_time_on ";old_time_on        
         tf = (pasoCol(i1).tiempoFiguraOld - pasoCol(i1).tiempoFigura) /d11
 '        print #1,"FOR 14:AOFFD:pasoCol(i1).tiempoFigura ";pasoCol(i1).tiempoFigura
 '        print #1,"FOR 15:AOFFD:pasoCol(i1).tiempoFiguraOld ";pasoCol(i1).tiempoFiguraOld
 '        print #1, "FOR 16:AOFFD:retardo tf ";tf
         If TF > 0 Then ' usamos el old_time-on que venia de antes
           duracion old_time_on, tf
           If CANCIONCARGADA =TRUE Then
             If Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Track(pis).trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
 '             print #1,"FOR 17:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" "; figura(Roll.trk(pasoCol(i1).i1 , jply+1).dur)
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
             EndIf

           Else
             If Roll.trk(jply+1, pasoCol(i1).i1 ).dur > 0 And _
                Roll.trk(jply+1, pasoCol(i1).i1 ).dur <= 181 Then
'print #1,"FOR 18:pasoCol(i1).i1 , jply+1 ", pasoCol(i1).i1 , jply+1
'print #1,"FOR 18+:figura(pasoCol(i1).DUR)",figura(pasoCol(i1).DUR)

'print #1,"FOR 19:AOFFD: ligado OFF==> i1 ";i1;" AcordeOffDistintos: notapiano:", pasoCol(i1).notapiano;" " ;  figura(pasoCol(i1).DUR)
                canal=pasoCol(i1).canal
                portsal=CUByte(pasoCol(i1).port)
                noteoff CUByte(pasoCol(i1).notapiano) ,canal,portsal
             EndIf
           EndIf  
pasoCol(i1).tiempoFigura =0  '01-07-2021
pasoCol(i1).tiempoFiguraOld=0 '01-07-2021
pasoCol(i1).liga=0
           
         Else
           If (pasoCol(i1).tiempoFigura=pasoCol(i1).tiempoFiguraOld) And pasoCol(i1).liga = 1  Then
              tf=tiempoFigMayorNoligado/d11 
'               print #1, "FOR 20:AOFFD:retardo recuperado en condicion (=) ..tf= ";tf
'               print #1,"FOR 21:pasoCol(i1).notapiano:";pasoCol(i1).notapiano
              duracion old_time_on, tf
              pasoCol(i1).tiempoFiguraOld=pasoCol(i1).tiempoFiguraOld   '- tiempoFigMayorNoligado  
'              print #1,"FOR 22:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld 
 '    comentado  noteoff pasoCol(i1).notapiano ,canal ' 21-06-2021
           Else

 '             print #1,"FOR 23:AOFFD:NO SE ENVIA OFF TF=0"
              pasoCol(i1).liga=1 '13-06-2021 PARA QU ESIGA EN EL OTRO PASO SI NO SE BORRA....
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFigura ' 13
              pasoCol(i1).tiempoFiguraOld= pasoCol(i1).tiempoFiguraOld '- tiempoFigMayorNoligado
 '             print #1,"FOR 24:AOFFD:pasoCol(i1).tiempoFiguraOld:";pasoCol(i1).tiempoFiguraOld ' 13
           EndIf  
         EndIf 
'         print #1,"FOR 25:pasoCol(i1).liga ", pasoCol(i1).liga
         ligaglobal = pasoCol(i1).liga ' por si sigue una simple
'         print #1,"FOR 26:<<<<<<<<<<FIN ELSE>>>>>>> CUANTAS VECES PASA?"
     EndIf
     ''pasoCol(i1).liga=0 ' ya la use se va 05-11-2021   
  EndIf 

Next i1
''pasoCol(i1).tiempoFiguraOld=0 '13-06-2021

 ' observacion una ligadura de varios acordes en una nota dada podria
 ' terminar o tener una nota simple unica como final o intermedia ligada
 ' ver que pasa en ese caso como lo solucionamos
 ' 01-07-2021 ACA NUNCA SE BLAQUEA no-> ligaglobal=0  porque corta las ligas largas en un acorde distinto 
'print #1,"FIN AcordeOffDistintos  " 

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


'-------------playAll-----21-05-2021-------
Sub playAll(Roll As inst) ' play version 2 
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
If  GrabarPenta=0 Then ' con 1 ya esta abierto
Print #1,"abriendo port....play All"


  
   k1=CInt(pmTk(0).portout)
    
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
      Print #1,"pORT SALIDA Y AABIERTO EN PLAYALL"
   EndIf
 EndIf 

'Print #1,"-------------------------------------"
'''''''midisal=midiout(0) ' el z
End If

'-------------------------------------------- 
 
    ''''''''midisal=midiout(pmTk(ntk).portout)
    
 '  Print #1,"Port usando en Play All ",portout
 ''fueradefoco=1
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
'  Print #1,">>>>>> canal elegido  en menu o de achivo ",canalx  
'  canal = CUByte(canalx) 
  ' RECORDAR CANALX EN SELECCION VA DE 0 A 15 no hace falta convertir
 
'  pmTk(0).canalsalida=canal 
        
'Else
'  canal = pmTk(0).canalsalida  '12-02-2022
'  
'  Print #1,"  ajusta CANAL al del archivo canal= ",canal        '

'EndIf


' Print #1,"ON patch ntk canal ",	Roll.trk(1,NA).inst, ntk,pmTk(0).canalsalida

 ''If Roll.trk(1,NA).inst > 0 Then
    ChangeProgram ( Roll.trk(1,NA).inst, pmTk(0).canalsalida, pmTk(0).portout)	
    patchsal =Roll.trk(1,NA).inst
 '''EndIf

'print #1,"comienzo playaLL ==========> tiempoPatron =",tiempoPatron," FactortiempoPatron",FactortiempoPatron
'print #1,"playAll         ==========> tiempoDur= 60/tiempoPatron*FactortiempoPatron =", tiempoDur
jply=0:curpos=0
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

' If jply=1 And Roll.trk(1,NA).inst > 0 Then
'   ChangeProgram ( Roll.trk(1,NA).inst , 0)
'    print #1,"ChangeProgram jply", Roll.trk(1,NA).inst
  ''        On Error GoTo labelerror 
' End If 
'        canal = pmTk(0).canalsalida  '12-02-2022
'        Print #1,"  ajusta CANAL ",canal        
'        portsal= pmTk(0).portout

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
   
' ojo con silencios ligados !!!
  cnt=0
  iguales=0
  distintos=0
  duraold=0 ' 04-11-2021 jmg
'  print #1," ---------------000000000000000000000-----------------"
'  print #1," (((PALL 0:)))---START--PASO:[";jply;"] ----------------"
'  print #1," ---------------000000000000000000000-----------------"

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
    'print #1,"Roll.trk(jply, i1).nota ",Roll.trk(jply, i1).nota
    'print #1,"Roll.trk(jply, i1).DUR ",Roll.trk(jply, i1).dur
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
      'print #1,"PALL 0:VEO LO CORRECTO DE NOTAPIANO "; Notapiano
      dura=Roll.trk(jply, i1).dur '1) I 2) I dur x 1 to 108
'      Print #1,"PALL 1:jply ";jply; "dura ";dura
      cnt=cnt+1
 '     Print #1,"PALL 2:paso ";jply;" cnt ";cnt;" notapiano "; Notapiano
      If cnt=1 Then 
         duraOld=dura
      EndIf
      ' 04-11-2021 usamos reldur para comparar duraciones !!!
      If reldur(duraOld)=reldur(dura)  And cnt > 1 Then
         iguales=1
  '       print #1,"PALL 4:cnt ";cnt;" iguales ";iguales
      EndIf
      If reldur(duraOld)<>reldur(dura)  And cnt > 1 Then
         distintos=1 ' atrapa no importa cuantos elementos tenga el acorde
  '       print #1,"PALL 5:cnt ";cnt;" distintos ";distintos
      EndIf         

     
      '+++++++++
  
        'print #1,"PALL 7:-> cnt"; cnt 
        pasoCol(cnt).DUR    =dura
 '       pasoCol(cnt).DURold =dura
        'print #1,"PALL 8:pasoCol(cnt).DUR ", pasoCol(cnt).DUR
        ' DURACIONE CON LIGA O SIN LIGA EJ F+ O F 
        If pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <=180 Then
   '         print #1,"PALL 9:PALL 0: nota con + es una li DUR R ",pasoCol(cnt).DUR
            pasoCol(cnt).liga =  1
        Else
            pasoCol(cnt).liga =  0   
        EndIf
        ' DURACIOENS SILENCIO O NO sF o sF+
        If (pasoCol(cnt).DUR >= 1 And pasoCol(cnt).DUR <=45) Or (pasoCol(cnt).DUR >= 91 And pasoCol(cnt).DUR <= 135) Then
    '        print #1,"PALL 9a:PALL 0: nota tiene audio 1"
            pasoCol(cnt).audio =  1 ' tiene audio
        Else
            pasoCol(cnt).audio =  2 ' no tiene audio, 0 valor no ajustado no se nada
     '       print #1,"PALL 9a:PALL 0: nota NO tiene audio 2"  
        EndIf
        
' debo saber si la nota anterior con ligaold   
' tenia audio o era silencio porque si era silencio y la actual tiene audio debo 
' enviar un noteon en la nota actual ver en subrutinas, donde pongo audioOld? al final de los acordes?
' no al final de cada nota, porque el analis de acorde es vertical, pero el de 
' audio es horizontal se agrega el old en el ON de cada tipo de acorde, como figuraold
' o sea que sea la misma posicion horizontal i1 me garantiza que es la misma nota nE
' que estan ligadas, puede ser que la anterior este ligada pero sin audio y la 
' actual que recibe la ligadura tenga audio => envio el noteon
' en que momento cargo la i1old? al final
        pasoCol(cnt).notapiano = Notapiano 
        'print #1,"PALL 10:Notapiano ",Notapiano
' >>>>>>>>>>>>>>>CANAL MIDI y POR SALIDA >>>>>>>>>>>>>>>>>>>>>>>>        
         pasoCol(cnt).canal=pmTk(0).canalsalida ' 12-02-2022 canal en pasoCol
  '       Print #1,"pasocol guarda canal, pista --> ",pasoCol(cnt).canal, 
         pasoCol(cnt).port=pmTk(0).portout
  '       Print #1,"pasocol guarda port , pista --> ",pasoCol(cnt).port   
      
 ' >>>>>>>>>>>>>>>CANAL MIDI >>>>>>>>>>>>>>>>>>>>>>>>         
       
        pasoCol(cnt).tiempoFigura    = relDur(pasoCol(cnt).DUR) * tiempoDur * d11
    
        pasoCol(cnt).i1    = i1 'posicion vertical en el vector real
  '      pasoCol(cnt).i1old = i1 'posicion vertical en el vector real
        'print #1,"PALL 11: posicion vertical en el vector real ",i1
        'print #1,"PALL 12:pasoCol(cnt).tiempoFigura ",pasoCol(cnt).tiempoFigura
      ' 20-06-2021 eliminado duraold=dura repetido    
        '' vel= vol( dura, velpos) 02-11-2021
' llegamos al final de la Columna
        pasoCol(cnt).vol=velpos
   EndIf   
      If i1=NA -13  Then 'And cnt >= 1 Then ' envio noteoff 1) no entra

    '''' ya nohace falta     mouse_event MOUSEEVENTF_MOVE, 1, 0, 0, 0
         If cnt > 1 Then' Acorde
          '  print #1,"i1=NA=";i1 ; " ACORDE cnt= ";cnt
         Else    
          '  print #1,"i1=NA=";i1 ; " SIMPLE cnt= ";cnt
         EndIf  

         Select Case cnt
          Case 1 
 'print #1, "PALL 24:call Notesimple cntold, vel, canal, tiempodur",  cntold, vel, canal,tiempoDur
 ' 04-11-2021 cnt por cntold ....aca|
          TipoAcorde=1 ' simple   
          AcordeIguales pasoCol(),cnt,cntold,vel,tiempoDur,Roll,velpos,0,0
          pasoCol(cnt).notapianoOld    = Notapiano             
          Case Is > 1
  '     print #1,"case is > 1"
            If iguales=1 And distintos=0  Then
                TipoAcorde=2 ' iguales
  '              print #1,"cnt ";cnt;" call Acordeiguales "
                AcordeIguales pasoCol(),cnt,cntold,vel,tiempoDur,Roll,velpos,0,0
                
            EndIf
            If  distintos=1 Then
               TipoAcorde=3 ' distintos
   '            print #1,"cnt ";cnt;" call AcordeDistintos"
                 AcordeDistintos pasoCol(),cnt, cntold,vel,tiempoDur,Roll,velpos,0,0
                
            EndIf
            
         End Select  

        cntold = cnt
 'print #1,"cantidad de elementos Acorde actual y anterior cnt,cntold"; cnt;" ";cntold
        
        
      EndIf
EndIf  

If i1 > NA-13 Then
 If Roll.trk(jply,i1).nota = 210 Then
   ' Print #1,"210 leido jply",jply
    playloop2=1
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
'''fueradefoco=0
Dim As Integer checkejec
For  iz As Short =1 To 32
      If CheckBox_GetCheck( cbxejec(iz))= 1  Or CheckBox_GetCheck( cbxgrab(iz))= 1 Then
          checkejec=1
      End If
      Exit For
Next iz
if GrabarPenta=0 and GrabarEjec=0 and repro=0 And checkejec=0 Then 
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


Sleep 20,1 ' si se coloca 1000 parpadea la pantlla hasta se cierra la aplicacion 

ThreadDetach(thread1) ' 16-06-2022

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
 Sub PlayRoll ( ) ' 1er play version 0 
' tiempo es cuantas negras en un minuto tiempoPAtron
' los acrodes se tocan con la duracion de la nota mas larga que lo compone 
Dim As Double tiempoDUR, tiempoFigura=0
tiempoDUR=(60/tiempoPatron) / FactortiempoPatron'60 seg/ cuantas negras enun minuto

'''midiin  = rtmidi_in_create_default()
'midiout = rtmidi_out_create_default()


'''portsin  =  port_count (midiin)
'portsout =  port_count (midiout)
'print #1, "portsin  "; portsin
'print #1, "portsout "; portsout
'Dim nombrez As ZString Ptr ' nombre local

'print #1,""
'print #1, "Output port"

'Dim i As INTeger
'for i = 0 to portsout -1 
'    nombrez = port_name(midiout, i)
'    print #1, *nombre
'Next   
'print #1, ""
'print #1, "Input port "

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

'print #1,"comienzo play ==========> "
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
'print #1,"-----------------------------------------"
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
'      print #1,"durb> 0, i, j ";durb,i,j
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
     ' print #1,"dura + durb "; dura   
         liga=1
         durb=0
         durl=0
      EndIf   
      If dura >= 91 And dura <=180 Then ' se suma la duración al siguiente
         durb=dura  ' 1) I+, 2) no entra
        ' print #1,"entro nota ligada "; dura, figura(dura)
      EndIf   
      If con=0 Then
        maxdur=dura  ' 1) I, 2) P
      '  print #1, jply; " con=0 atrapa dura maxdura ";dura, maxdur
         con = 1
      EndIf
      'vel=Roll.trk(i,j).vel
      ' etc...
' SACAR ESTO TOCAR ACORDE CADA ELEMENTO CON SU DURACION        
      If relDur(dura) > relDur(maxdur) Then ' esto lo debo sacar y tocar todas las notas con su duracion
         maxdur= dura ' 1) I, 2) P cuantoms chica dur es mas grnde en relidd
     ' print #1,jply;"if dura-figura "; dura, figura(dura)
     ' print #1,jply;"if cambio Maxdur-figura "; Maxdur, figura(Maxdur)
      Else    
        '' notacur=i
     ' print #1,jply;"else dura-figura "; dura, figura(dura)
     ' print #1,jply;"sigue igual else Maxdur-figura "; Maxdur, figura(Maxdur)

      EndIf 
      If liga=0 Then  
       ' print #1,"liga=0 "
        vel=vol(maxdur,velpos)
       
 ' SI ELUSUARIO GRABA VELOCIDADES DEBO USARESA NO LA DEFAULT !!! JMG
 ' Y NO SACRLA MAAX DURACION TOCAR TODAS CON SU DURCION Y VELOCIDAD!!!       
  ''      canal=pasoCol(i1).canal
        portsal=pmTk(ntk).portout
        noteon notapiano, vel, canal,portsal ' 1) G
 
        cx = cx + 1   ' 1) 1
        non (cx) = notapiano '1) G
       ' print #1, "ON==>  notapiano, vel, canal ";notapiano, vel, canal
       ' print #1,"cx ";cx 
      ''''''  Sleep 1,1
        old_time_on=Timer
      Else
       ' print #1,"liga=1 no se envia noteon " 
        liga=0 
      EndIf 
   EndIf

   If i=NB And durb = 0 Then ' envio noteoff 1) no entra
 ''Sleep segun duracion o Timer de la q mas dura o para cada uno
      ' tiempoPatron input al redimsub
   ' print #1,"i=NB=";i," maxdur=";maxdur;  
 '   If maxdur > 0 And maxdur <= 182 Then
      ' print #1, figura(maxdur)
 '   Else 
      ' print #1, "No se puede mostrar"  
 '   EndIf   
    
  ''''   duracion (maxdur)
'''' DURACION  

 If maxdur >= 1 And maxdur<= 180 Then 
    tiempoFigura = relDur(maxdur)*tiempoDUR
 
 '  print #1, "tiempoFigura ";tiempoFigura  
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
   '  print #1," cantidad cx de off ";cx
     
     For ioff=1 To cx
     portsal=pmTk(ntk).portout  
     noteoff non(ioff),canal,portsal

    ' print #1, "OFF==>   non(ioff),  canal "; non(ioff),canal
     Next ioff
 '    print #1,"pasó for de off .."
 '    print #1," ==============> fin paso...j"; j   
   EndIf 
  Next i

'  print #1,"COMIENZA OTRA  POSICION O J ======"; j
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
'print #1, "tiempoFigura " , tiempoFigura
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


'Print #1,"LISTPORTS portsout, portsin", portsout, portsin

ReDim listout(0 To portsout -1)
ReDim listin (0 To portsin  -1) 
' saco de la lista los ports ya abiertos

Dim nombre As ZString ptr
Dim aviso As String = " Abierto"
Dim temp As String=""
Dim lg As Integer  
' "Output port"

Dim  As Integer i,j

for i = 0 to portsout -1 
  If listoutAbierto (i) =0 Then
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then ' microsoft no funa bien
      listout(i) = "Crash No usar Microsoft" 
 '     Print #1,"listout(i) ",listout(i)

    Else
     listout(i) = *nombre
 '     Print #1,"listout(i) ",listout(i)
    endif
  EndIf  
  If listoutAbierto (i) =1 Then
'    lg=Len(*port_name(, i))
    nombre = nombreOut(i)
    If InStr(*nombre,"Microsoft") > 0 Then
      listout(i) = "Crash No usar Microsoft" 
 '     Print #1,"listout(i) ",listout(i)

    Else
     listout(i)=*nombreOut(i) +aviso 
    EndIf
    
  EndIf  
  
Next
temp=""
' entradas copia
for i = 0 to portsin -1 
  If listInAbierto (i) =0 Then
    nombre = nombreIn(i)
    If InStr(*nombre,"Microsoft") > 0 Then ' microsoft no funa bien
      listin(i) = "Crash No usar Microsoft" 
   '   Print #1,"listin(i) ",listin(i)

    Else
     listin(i) = *nombre
   '   Print #1,"listin(i) ",listin(i)
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
  
Next
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

'print #1,"ARRANCA TRASPONER GRUPO"
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
 '                 Print #1,"encontro una nota 13"
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
'---------
Sub trasponerRoll( cant As Integer, Roll As inst, encancion As integer)
'AJSUTADO DE NUEVO 11-09-2021 CON EL NUEVO ALGORITMO DE OCTAVAS
Dim As Integer jpt=1, ind=1,i1=1, comienzo , final, inc,octavaDeAcorde,verticalEnOctavaVacia  

' NA ES EL MAYOR VALOR NUMERICO, 
' NB EL MENOR VALOR NUMERICO
' cant=(1) si pulso flecha UP
  'Print #1,"ARRANCA  TRASPONER ROLL !!!!!!!!!!!!!!",trasponer
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
  
'print #1, "MaxPosOld ", MaxPosOld
' si movemos a derecha empezamos copiando a la nueva posicion el final de 
' la secuencia, luego en la nueva posicion -1 copiamos el final -1
' asi desde el final haci aadelante...
' si movemos a izqierda el reves..
' o sea lo que está echo es para mover a izquierda donde la posiion destino
' el click esta a la izquierda de pasozona1  
/'
print #1," chequeo"
For jpt= 1 To maxpos
  For i1=comienzo To final
       If Roll.trk(jpt,i1).nota < 13 And i1= 67 Then
        print #1,"Roll.trk(inc,i1).nota ",Roll.trk(inc,i1).nota
        print #1,"Roll.trk(inc,i1).dur ",Roll.trk(inc,i1).dur
        print #1,"Roll.trk(jpt,i1).nota ",Roll.trk(jpt,i1).nota
        print #1,"Roll.trk(jpt,i1).dur ",Roll.trk(jpt,i1).dur

       EndIf

  Next i1
Next jpt
print #1,"fin chequeo"
'/
If posinueva > Maxpos Then ' movemos a izquierda
inc=posinueva ' aca inc son los datos copiados 
'print #1,"ENTRA POR IZQUIERDA"
  For jpt=desdet To hastat
       
     For  i1= comienzo To final
       Roll.trk(inc,i1).nota = Roll.trk(jpt,i1).nota
       Roll.trk(inc,i1).dur  = Roll.trk(jpt,i1).dur
       Roll.trk(inc,i1).vol  = Roll.trk(jpt,i1).vol
       Roll.trk(inc,i1).pan  = Roll.trk(jpt,i1).pan
       Roll.trk(inc,i1).pb   = Roll.trk(jpt,i1).pb
       Roll.trk(inc,i1).inst = Roll.trk(jpt,i1).inst
   '  print #1,"i1,ind Roll.trk(i1,ind).nota ",i1, ind, Roll.trk(ind,i1).nota
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
 ' UNDO
   mel_undo_k = mel_undo_k + 1

   mel_undo(mel_undo_k).trk = ntk
   mel_undo(mel_undo_k).posn = inc

 ' FIN UNDO    
     
  Next jpt
 ' print #1,"TERMINO copia a izquierda ",posinueva   
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
 ' print #1,"inicioind  MAxPosOld ",posinueva , MAxPosOld  
  If posinueva > MAxPosOld Then

  ' print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
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
  'print #1,"--> TERMINO la vuelta de ind a la izquierda", posinueva
' aca el maxpos deberia achicarse....
MaxPos=inc +1
'-------------------------------
Else ' if ind posiion nueva > pasozona1 movemos a derecha
'---------------------------------

'print #1,"ENTRA POR DERECHA POSINUEVA < MaxPos"
'print #1,"posinueva ",posinueva
'print #1,"MaxPosold ",MaxPosold
'print #1,"posivIEJa ",posivieja ' es pasozona1 inamovible
  If posinueva < MaxPos then
    MaxPos=MaxposOld + pasozona2 - pasozona1 +1 ' -1 01-12-21
  EndIf
'print #1,"MaxPos ",MaxPos
 
  'hastat=Maxpos
  desdet=posinueva
  'desdet=posivieja+1 30-01-2022 
  hastat=posinueva-pasozona1+pasozona2 +1
  'inc=MaxPosOld
  inc=pasozona1 ' aca inc son lso datos a copiar
'print #1,"hastat ",hastat
'print #1, "desdet,inc ",desdet,inc 
'print #1,"UBOUND(ROLL,1)", UBOUND (ROLL.TRK,1)
'print #1,"LBOUND(ROLL,1)", LBound (ROLL.TRK,1)
'print #1,"UBOUND(ROLL,2)", UBOUND (ROLL.TRK,2)
'print #1,"LBOUND(ROLL,2)", LBOUND (ROLL.TRK,2)
'print #1,"inc=posivieja+1 ",inc
  '        mAXpOS TO  POSIVIEJA+1          inc=MAxPosOld   
  For jpt= desdet To hastat
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
     inc=inc+1
     If inc = pasozona2+1  Then
        Exit For
     EndIf
 ' UNDO
   mel_undo_k = mel_undo_k + 1

   mel_undo(mel_undo_k).trk = ntk
   mel_undo(mel_undo_k).posn = jpt

 ' FIN UNDO    
  Next jpt
 ' print #1,"TERMINO copia a derecha ",posinueva   
'---
'si la posicion donde copio es mayor a MaxPos, debo llenar el espacio entre MAxPos y 
'el punto inicial de copia con 0 y 181 para dur y Nota repectivamente
 ' print #1,"inicioind  MAxPosOld ",posinueva , MAxPosOld  
  'If posinueva > MAxPosOld Then

  ' print #1,"MAxPosOld, inicioind ", MAxPosOld, inicioind
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
  'print #1,"--> TERMINO la vuelta de ind a derecha ", posinueva


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
           i2= i1 - restar (i1)
          ' print #1, "i2",i2
          ' print #1,"relnRNe (i2) ",relnRNe (i2)
          ' print #1,"---------------"   
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
       PianoNota=nRk  
       
    EndIf

dim i As Integer
Dim As Integer partes , traba=0
Static old_time As Double
  
' jgrb global por ahora
Dim new_time As Double

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
 t2call=Timer

' velocidad I=240 -> t=60/240=1/4=0,25 la negra, para llegar a W / por 2 6 veces= 0,00390625
    new_time=Timer
    If old_time - new_time > 0.005  Then  ' 0.005208325 eltick maschico 5 mseg
       duracion (old_time,deltatime)
    EndIf 
'    dato1=*memoria: memoria += 1
'    dato2=*memoria: memoria += 1
'    dato3=*memoria 
'    DURk =deltatime
'    If GrabarPenta=1 Then
'       nRk=CInt(dato2)
'       PianoNota=nRk  
'     '  Print #1,"PianoNota-> ",nRk 
'       nRk=nRk + SumarnR(nRk)
'       
'    EndIf
' velocidad I=240 -> t=60/240=1/4=0,25 la negra, para llegar a W / por 2 6 veces= 0,00390625

     Select Case  dato1 
         Case 144 ' on
ChangeProgram pmTk(tocatope+32).patch,pmTk(tocatope+32).canalsalida ,pmTk(tocatope+32).portout
            noteon dato2,dato3,pmTk(ntoca+32).canalsalida, pmTk(ntoca+32).portout

'     Print   dato1;" ";  dato2;" "; dato3
           
         Case 128 'off

            noteoff dato2,pmTk(ntoca+32).canalsalida,pmTk(ntoca+32).portout 'message(2)'
'     Print   dato1;" ";  dato2;" "; dato3

     End Select

    old_time=new_time
' tick mas chico es 0.005208325 (ver [TickChico] en RTMIDIDEC)
' ergo divido deltatime por ese valor y obtengo la cantiad de divisiones
' que ocupara ese retardo deltatime/TickChico

  If GrabarEjec =1 Then ''graba en al pista seleccioanda
     partes=(deltatime/TickChico) 
     jgrb += 1
     If ntoca > 1 And jgrb=1 Then ' detiene la acumulacion de deltatime en PlayTocaAll 
          arrancaPlay=1
          Print #1,"arranco play o sea el usuario empezo a tocar la siguiente pista"
 
     EndIf

     CargaIn( jgrb).modo=dato1
     CargaIn( jgrb).nota=dato2
     CargaIn( jgrb).vel=dato3
     If deltatime > 0.005  Then '   5mseg  
       CargaIn( jgrb).partes=partes 'convierto deltatime en tickschico
       pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos +partes
       tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
     Else
       CargaIn(jgrb).partes=0
       pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos +1
       tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
     EndIf
    '    Print #1,"partes ",partes
   '  tocaparam(ntoca).portout=pmTk(ntoca+32).portout
   '  tocaparam(ntoca).portin =pmTk(ntoca+32).portin
   '  tocaparam(ntoca).canal=pmTk(ntoca+32).canalsalida
   ' tocaparam(ntoca).patch =pmTk(ntoca+32).patch

  EndIf
End Function
'--------------------------------------
Sub GrabarMidiIn ( ByRef  par As  paramGrabamidi)
' tocap As vivo, ntkp As Integer,tocaparam  As ejecparam  Ptr) 
    Dim As Long j, driver=0
    Dim As String nombreg
    Dim As Integer ngm

      nombreg =titulos(ntkp+32) ' no tiene path , extension y [nro]
Print #1,"GrabarMidiIn NombreCancion, nombre ",NombreCancion," --- ",nombreg
      driver=InStr(nombreg,":\")
      If  NombreCancion > "" And driver=0 Then
          nombreg=NombreCancion+"\"+nombreg
      Else 
        Print #1," va a grabar sin path ",nombreg
      EndIf
Print #1,"nombre de archivo con path grabando de ejec",nombreg
 
     par.tocap.portout =pmTk(ntkp+32).portout
     par.tocap.portin   = pmTk(ntkp+32).portin
     par.tocap.patch   =pmTk(ntkp+32).patch
     par.tocap.canal   =pmTk(ntkp+32).canalsalida
     par.tocap.maxpos   =pmTk(ntkp+32).MaxPos
   
      Print #1,"GrabarMidiIn titulos   ", nombreg
      Print #1,"GrabarMidiIn MAXPOS ",par.tocap.maxpos
      Print #1,"GrabarMidiIn delta "     ,par.tocap.delta
      Print #1,"GrabarMidiIn nombre " ,par.tocap.nombre
      Print #1,"GrabarMidiIn portout " ,par.tocap.portout
      Print #1,"GrabarMidiIn portin "   ,par.tocap.portin
      Print #1,"GrabarMidiIn patch "   ,par.tocap.patch
      Print #1,"GrabarMidiIn canal "   ,par.tocap.canal
      nombre=Trim(nombreg)
      ngm=FreeFile 
Print #1,"GrabaMidiin freefile ngm ",ngm
    if   Open( nombreg  For Binary Access Write As #ngm)  <> 0 Then
              Print #1,"Imposible Grabar " + nombreg 
    Else  
         Put #ngm,, par.tocap '1ero parametros como siempre o en cabezado
         Put #ngm,, par.toc.trk()   '2do datos  
         FileFlush(ngm) 
    End If
Print #1,"grabado ARCHIVO ", nombreg
      Sleep 100
      Close ngm


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
ChangeProgram ( 1, 1, 0)
Do
      'noteon(50,50,1,0)
      'duracion(Timer, 0.1)
     ' noteoff(50,1,0)

      noteon(80,60,1,0)
      noteoff(80,1,0)
      duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
     If terminar_metronomo=1 Then
         Exit Do
     EndIf
      

Loop  

 threadDetach (threadmetronomo)
End Sub
'--------------------
Sub abrirPortoutEjec(j As Integer)
'------------hace falta abrir la salida
Print #1,"abrirPortoutEjec abriendo port.... "
Dim k1 As Integer

  
   k1=CInt(pmTk(j+32).portout)
    
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
'////////////////////////////TOCAALL/////////////////////////
' perfeccionar los eventos deven seguir un patron de tiempo de ticks
' para c/tick ver si hay un evento si hay enviarlo...el problem ason lso retardos
' TOCA VARIAS PISTAS EJEC 
Print #1,"PlayTocaAll 1"
ntoca=*nt
Dim  As long j=0,k=0,partes,cuenta=0,ks(1 To 32),pis=0
Dim As UByte dato1,dato2, dato3 
' cargo retardos de ejecucion
For j=1 To 32
  espera(j)=tocaparam(j).delta ' empieza siemrpe por la 2
Next j
Print #1,"PlayTocaAll 2"
'--------------play TOCA

'ChangeProgram ( 1, ntoca, 0)
timex(01)=Timer
For j=2 To 32 
 timex(j)=timex(01)
Next j

Print #1,"PlayTocaAll 3"

''Print #1,"=====> EN PLAY StartPlayejec ",StartPlayejec
Dim  topeDuranteGrabacion As integer
Print #1,"nPLAY VERDE: maxgrb ",maxgrb
'canal=1 ' por ahora
portsal=0 ' por ahora ???

Print #1,"playtoca maxgrb ", maxgrb
Print #1,"playtoca tocatope ", tocatope
If GrabarEjec=1 Then 
  If   tocatope >1 Then
   topeDuranteGrabacion=tocatope-1
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
CONTROL2=0

Print #1,"empieza el play de ejec, maxgrb, CONTROL2 ", maxgrb,CONTROL2
For jToca=1 To maxgrb 
  If CONTROL2 = 1 Then
     alloff( 1 ,pmTk(jToca+32).portout )
      CONTROL2=0
      repro=0
      Exit For
  EndIf  
'''Print #1,"topeDuranteGrabacion ",topeDuranteGrabacion
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
   '    Else
   '        ''Print #1,"salteo LoopBe Internal MIDI"
       EndIf
    Else
     Continue For ' saltear no tocar 
    EndIf 

    dato1=Toca(kply).trk(jToca).modo
    dato2=Toca(kply).trk(jToca).nota
    dato3=Toca(kply).trk(jToca).vel
    portsal=pmTk(kply+32).portout '04-05-2022

     If  dato1=1 Then ' marcamos con 1 un delta de tick en modo
         duracion (timex(kply),TickChico) ' si es cero no 1 no hay duracion es acorde
         timex(kply)=timex(kply)+TickChico
       If  GrabarEjec =1 And ntoca> 1 And arrancaPlay=0 And kply=1 Then
           tocaparam(ntoca).delta=tocaparam(ntoca).delta+TickChico
           ''Print #1,"En PlayToca Toca(ntoca).delta ",Toca(ntoca).delta
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
            noteon dato2,dato3,pmTk(kply+32).canalsalida, pmTk(kply+32).portout 'message(3) ' noter vel canal
           'Print #1,"ON ",dato2,dato3,pmTk(kply+32).canalsalida, pmTk(kply+32).portout
         Case 128 'off
            noteoff dato2,pmTk(kply+32).canalsalida ,pmTk(kply+32).portout 'message(2)'
           'Print #1,"OFF ",dato2,pmTk(kply+32).canalsalida ,pmTk(kply+32).portout  
     End Select
 
  Next kply
''''Sleep 1,1 ' para que corranmas de un thread ¿??? 20-06-2022 porque lo puse?
Next jToca
''jToca=0
repro=0
SetGadgetstate(14,0)
Sleep 1

  For kply =1 To topeDuranteGrabacion
    
    If CheckBox_GetCheck( cbxejec(kply))= 1 Then
       ' tpcar
    Else
     Continue For ' saltear no tocar 
    EndIf 
    portsal=pmTk(kply+32).portout 
     alloff( pmTk(kply+32).canalsalida,portsal )  
     allSoundoff( pmTk(kply+32).canalsalida, portsal ) 
  Next kply

  
End Sub


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



