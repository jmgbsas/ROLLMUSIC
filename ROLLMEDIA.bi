'
Function valorfrec (nronota As Integer) As double
	' EXPANDIR UN GRAFICO DE TECLADO SENSIBLE A MOUSE DE 88 LUGARES EN CADA UNO
	' SONAR LA FRECUENCIA PURA DE LA NOTA CORREPONDIENTE
	' f3 = 440 * (1.059463..)3 = 523.3 Hz   LA4=440 DEL MEDIO RAZIA CUADRADA DE 2
	' POTENCIA SEMITONOES POR ARRIBA + O POR ABAJO -
	' C5 = the C an octave above middle C. This is 3 half steps above A4 and so the frequency is
	' f3 = 440 * (1.059463..)3 = 523.3 Hz
	
	'Middle C is 9 half steps below A4 and the frequency is:
	'f -9 = 440 * (1.059463..)-9 = 261.6 Hz
	Dim As Double RC2=2^(1/12)
	Print #1,"RC2 ";RC2
	RC2=RC2 ^ (nronota-69)
	Print #1,"RC2 ";RC2
	valorfrec = 440*RC2
	Print #1,"VALORFREC ";440*RC2
	
End Function
' Función para generar y tocar un tono de ondas.bas
Sub EmitirTono(frecuencia As Double, segs As Double)
	Print #1,"EmitirTono(frecuencia As Double, segs As Double) ", segs
	
	' 1. Calcular tamaños
	Dim numMuestras As UInteger = SAMPLE_RATE * segs
	Dim bytesPorMuestra As Integer = (BITS \ 8) * CANALES
	Dim tamDatos As UInteger = numMuestras * bytesPorMuestra
	Dim tamTotal As UInteger = 44 + tamDatos ' 44 bytes header + datos
	
	' 2. Reservar memoria (Buffer)
	PlaySoundbuffer = Allocate(tamTotal)
	If PlaySoundbuffer = 0 Then
		Print "Error: No se pudo reservar memoria."
		Exit Sub
	End If
	
	' 3. Escribir el Encabezado WAV (Formateamos el buffer para que parezca un WAV)
	Dim p As Byte Ptr = PlaySoundbuffer
	
	' "RIFF"
	*p = &H52: p+=1: *p = &H49: p+=1: *p = &H46: p+=1: *p = &H46: p+=1
	' File Size
	Dim tamArchivo As UInteger = tamTotal - 8
	Dim i As Integer
	Dim tamBytes As UByte Ptr = @tamArchivo ' Truco para escribir bytes endian correcto en Little Endian (Intel/Windows)
	For i = 0 To 3: *p = tamBytes[i]: p+=1: Next
	
	' "WAVE"
	*p = &H57: p+=1: *p = &H41: p+=1: *p = &H56: p+=1: *p = &H45: p+=1
	
	' "fmt "
	*p = &H66: p+=1: *p = &H6D: p+=1: *p = &H74: p+=1: *p = &H20: p+=1
	
	' Chunk size (16 for PCM)
	Dim tamFmt As UInteger = 16
	tamBytes = @tamFmt
	For i = 0 To 3: *p = tamBytes[i]: p+=1: Next
	
	' Audio Format (1 = PCM)
	Dim fmt As UShort = 1
	Dim tamShort As UByte Ptr = @fmt
	*p = tamShort[0]: p+=1: *p = tamShort[1]: p+=1
	
	' Channels (1 = Mono)
	Dim chan As UShort = CANALES
	tamShort = @chan
	*p = tamShort[0]: p+=1: *p = tamShort[1]: p+=1
	
	' Sample Rate (44100)
	Dim sRate As UInteger = SAMPLE_RATE
	tamBytes = @sRate
	For i = 0 To 3: *p = tamBytes[i]: p+=1: Next
	
	' Byte Rate (SampleRate * Channels * BitsPerSample/8)
	Dim bRate As UInteger = SAMPLE_RATE * CANALES * (BITS \ 8)
	tamBytes = @bRate
	For i = 0 To 3: *p = tamBytes[i]: p+=1: Next
	
	' Block Align (Channels * BitsPerSample/8)
	Dim bAlign As UShort = CANALES * (BITS \ 8)
	tamShort = @bAlign
	*p = tamShort[0]: p+=1: *p = tamShort[1]: p+=1
	
	' Bits Per Sample (16)
	Dim bPS As UShort = BITS
	tamShort = @bPS
	*p = tamShort[0]: p+=1: *p = tamShort[1]: p+=1
	
	' "data"
	*p = &H64: p+=1: *p = &H61: p+=1: *p = &H74: p+=1: *p = &H61: p+=1
	
	' Data Size
	tamBytes = @tamDatos
	For i = 0 To 3: *p = tamBytes[i]: p+=1: Next
	
	' 4. Generar la Onda Senoidal
	' Ahora apuntamos al area de datos (el puntero p ya esta ahí despues del header)
	
	Dim ptrMuestras As Short Ptr = Cast(Short Ptr, p)
	
	For i As UInteger = 0 To numMuestras - 1
		Dim tiempo As Double = i / SAMPLE_RATE
		
		' FORMULA DEL SENO: Amplitud * Sin(2 * PI * Frecuencia * Tiempo)
		Dim valorFlotante As Double = Sin(2.0 * PI * frecuencia * tiempo)
		
		' Escalar a rango 16-bit (-32768 a 32767)
		' Multiplicamos por 32767 para volumen maximo. Usa 10000 para volumen medio.
		Dim muestra As Short = valorFlotante * 32767
		
		ptrMuestras[i] = muestra
	Next
	
	' 5. Reproducir usando API de Windows
	' SND_MEMORY = El sonido esta en memoria
	' SND_SYNC   = Esperar a que termine el sonido antes de seguir (no superponer notas)
	' SND_ASYNC   = No Esperar a que termine el sonido antes de seguir
	PlaySound( PlaySoundBuffer, NULL, SND_MEMORY Or SND_ASYNC)
	' 6. Liberar memoria solo con SYNC wn ASYNC terminaria todo
	''''''''   Deallocate(PlaySoundBuffer)
	
End Sub

