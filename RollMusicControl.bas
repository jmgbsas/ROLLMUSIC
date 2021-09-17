' este es  un modulo no es un include asi se podria hacer mas modulos...
'  o sacar algunos de include en elmain
#Define __FB_WIN64__
#If Defined (__FB_WIN64__) 
#LibPath "C:\msys64\mingw64\lib"
#Else
#LibPath "/usr/lib"
#EndIf
#Define EXTCHAR Chr(255)
#Include "fbgfx.bi"
#If __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf
' para GTK Gtk:list()
#Include Once "crt.bi"
#Include "window9.bi"
#Include Once "gtk/gtk.bi"
#Include "midiinfo.bi"
#Include "ROLLCONTROLDEC.bi"

Dim As Integer ancho, alto
ancho = GetSystemMetrics(SM_CXSCREEN)
alto = GetSystemMetrics(SM_CYSCREEN)


'Var hwnd = OpenWindow("RollMusic Control",10,10,ancho*3/4,alto*3/4)

'Dim As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8
COMMON Shared As Long eventc
Common Shared As hwnd hwndC, hwndListBox

'Dim As Long event=0
Dim Shared As Integer desde , hasta,MaxPos=2
#Include "RTMIDIDEC.BI"

'------
Sub  seloctava  ( ByRef octadesde As Integer, ByRef octahasta As integer) 
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x  
Dim listoctava(1 To 10) As string ={"-1","0","1","2","3","4","5","6","7","8"}

For x= 1 To 2 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("OCTAVAS",100,50,400,400,WS_VISIBLE, WS_EX_TOPMOST )
     hwl=  ListViewGadget(1,10,10,300,400,,,,32,LVS_SINGLESEL  )
    ' listports()

  If paso=0 Then
       AddListViewColumn(1, "Elegir Desde ",0,0,250)
       For aa =1 To 10 
           AddListViewItem(1, listoctava(aa)+" "+Str(aa),0,aa,0)
       Next
       
  EndIf   
  If paso=1 Then  
       AddListViewColumn(1, "Elegir Hasta ",0,0,250)
       For aa =1 To 10 
           AddListViewItem(1, listoctava(aa)+" "+Str(aa),0,aa,0)
       Next
  EndIf


       ButtonGadget(2,330,30,50,40," OK ")
       
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         'menunew=0
Dim As Integer i
         Do

         Var eventC= waitEvent

          If eventC=eventgadget Then
            If eventnumber()=2 Then
'              menunew=0
              If paso=0 Then
                 Octadesde = GetItemListView()
                 octadesde=octadesde+1

              EndIf   
              If paso=1 Then
                 Octahasta = GetItemListView()
                 Octahasta=octahasta+1
 
              EndIf   

              Close_Window(haw)
              paso=1
              Exit Do 
            End If
          EndIf 
          Sleep 5  
          
         Loop 
Next x
'' fin ruso
If octadesde=0 Then
octadesde=1
EndIf
If octahasta=0 Then
octahasta=9
EndIf
Print #1,"OCtadesde ",octadesde
Print #1,"OCtahasta ",octahasta

End Sub
Sub selInstORdenAlfa(ByRef instru As Integer)
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x=0,in=0  
instru=0

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("INSTRUMENTOS PATCH",100,50,600,600,WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     ' commctrl.bi modificado
     hwl=  ListViewGadget(1,10,10,500,500,,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN OK",0,aa,0)
       For aa =1 To 127 
           AddListViewItem(1, NombreInstAlfa(aa),0,aa,0)
       Next
       


       ButtonGadget(2,530,30,50,40," OK ")
'       ButtonGadget(3,530,90,50,40,"+Pag")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventC= waitEvent

          If eventC=eventgadget Then
          
            If eventnumber()=2 Then
               Instru = GetItemListView()
               Print #1,"in = ",in 
              If instru=0 Then  instru=1 EndIf 
               
              ' If in >= 1 And in <=127 Then
                 instru = IndiceInstAlfa(instru)
              ' EndIf
'''               instru=instru + 1
            ''   If instru > 1 Then
                  Close_Window(haw)
                  Exit Do
           ''    EndIf
            End If

          EndIf 
          Sleep 5  
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
Print #1,"Str(instru) ", Str(instru)
  

end Sub

Sub selInstORdenNum (ByRef instru As integer)
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x=0  
instru=0

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("INSTRUMENTOS PATCH",100,50,600,600,WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     ' commctrl.bi modificado
     hwl=  ListViewGadget(1,10,10,500,500,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN OK",0,aa,0)
       For aa =1 To 127 
           AddListViewItem(1, NombreInst(aa),0,aa,0)

       Next
       


       ButtonGadget(2,530,30,50,40," OK ")
'       ButtonGadget(3,530,90,50,40,"+Pag")
         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventC= waitEvent

          If eventC=eventgadget Then
          
            If eventnumber()=2 Then
               Instru = GetItemListView()
'''               instru=instru + 1
            ''   If instru > 1 Then
                  Close_Window(haw)
                  Exit Do
           ''    EndIf
           End If

          EndIf 
          Sleep 5  
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
Print #1,"Str(instru) ", Str(instru)

End Sub
' ---------
Sub EntrarNombreCancion(ByRef NombreCancion As string)
  NombreCancion=Date
  NombreCancion = InputBox("InputBox",,NombreCancion)
  If NombreCancion ="" Then
    SetWindowText(hwndC, "RollMusic Control")
  Else
    SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + NombreCancion)
  EndIf
 'AL GRABAR tracks AGREGAREMOS ".cnr" COMO EXTENSION cancion roll pero no para
 ' dir de cancion,
End Sub
' -------
Sub CrearDirCancion (ByVal NombreCancion As string)

Dim pathdir As String
pathdir = ShellFolder( "Select Folder", "C:\")
pathdir=pathdir+"\"+NombreCancion
Print #1, "DIRECTORIO CANCION EN ",pathdir
CreateDir(pathdir)
SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + pathdir)
NombreCancion=pathdir

End Sub
Sub cargarDirectorioCancion (ByRef NombreCancion As string)
Dim pathdir As string
NombreCancion = ShellFolder( "Select Folder", "C:\")
SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + NombreCancion)
Print #1,"cargarDirectorioCancion ", NombreCancion 
' aca NombreCancion contiene el path tambien....

End Sub
Sub EntrarNombrePista(ByRef NombrePista As String)

  NombrePista = InputBox("InputBox",,NombrePista)

End Sub
