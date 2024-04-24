
Public Function midiconv( Nombre1 As String, Nombre2 As string) As Integer Export

         '''Nombre="algo\DISTINTOS-F+SF+SF+F.roll"
' saco nombre del archivo secuenciaPLAY.txt
   Dim As String linea, nombre3
   Dim n As Integer

   Open "secuenciaPLAY.txt" For Input Shared As 24 
    Do While Not Eof(24)
       Line Input #24,linea
       n=InStr(linea,"Meta SeqName")
       If n > 0 Then  
          Nombre3=Mid(linea,n+13)
          n=Len(Nombre3)
          If Nombre3>"" Then
             Exit Do
          EndIf
       EndIf     
    Loop 
    Close (24)     

    If Nombre3 > "" Then
       Nombre2=Nombre3
    EndIf 
'--------------------------------------------------
    Dim As String cadena
    cadena =  " -c  " + Nombre1 + " "+ Nombre2
    Dim  As Integer punto= InStr(Nombre2,".") 
    If punto > 0 Then 
       cadena=Mid(Nombre2,1,punto -1 )
       punto=InStrRev (cadena,"\")
       If punto > 0 Then 
         cadena=Mid(cadena,punto+1)
       EndIf
       cadena =  " -c  " + Nombre1 + " "+ cadena + ".mid"
    Else
       cadena =  " -c  " + Nombre1 + " "+ Nombre2 + ".mid"
    EndIf 
                Dim result As Integer
                result = Exec( "./midicomp.exe" , cadena )
               Open "salida.txt" For Output As 23
               
                If result = -1 Then
                    Print #23, "Error running "; "midicomp.exe", cadena
                End If
                Print #23, "Exit code:"; result, cadena
                Close 23
   Return result

End Function 

