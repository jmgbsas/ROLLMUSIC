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
Dim As Integer ancho, alto
ancho = GetSystemMetrics(SM_CXSCREEN)
alto = GetSystemMetrics(SM_CYSCREEN)

Var hwnd = OpenWindow("RollMusic Control",10,10,ancho*3/4,alto*3/4)

Dim As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8

Dim As Long event=0
Dim As Integer desde , hasta
hMessages=Create_Menu()
MenName1=MenuTitle(hMessages,"Archivo")
MenName2=MenuTitle(hMessages,"Edicion")
MenName3=MenuTitle(hMessages,"Nuevo Track")
MenName4=MenuTitle(hMessages,"Ver")
MenName5=MenuTitle(hMessages,"Pista")
MenName6=MenuTitle(hMessages,"Reproducir")
MenName7=MenuTitle(hMessages,"Opciones")
MenName8=MenuTitle(hMessages,"Ayuda")



MenuItem(1001,MenName1, "1 Menu")
MenuItem(1002,MenName2,"2 Menu")
MenuItem(1003,MenName3,"Seleccion Octavas")
MenuItem(1004,MenName3,"Crear Track")


MenuItem(1005,MenName4,"5 Menu")
MenuItem(1006,MenName5,"6 Menu")
MenuItem(1007,MenName6,"7 Menu")
MenuItem(1008,MenName7,"8 Menu")
MenuItem(1009,MenName8,"9 Menu")
'-------
Sub CreaTrack  (ByRef octadesde As Integer , ByRef octahasta As Integer ) 

Shell ("start RollMusic "+ Str(octadesde)+" "+ Str(octahasta) + " Track_"+Str(octadesde)+"_"+Str(octahasta))
 
  
End sub


'------
Sub  seloctava  ( ByRef octadesde As Integer, ByRef octahasta As integer) 
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x  
Dim listoctava(1 To 9) As string ={"-1","0","1","2","3","4","5","6","7"}

For x= 1 To 2 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("OCTAVAS",100,50,400,400,WS_VISIBLE, WS_EX_TOPMOST )
     hwl=  ListViewGadget(1,10,10,300,400,,,,32,LVS_SINGLESEL  )
    ' listports()

  If paso=0 Then
       AddListViewColumn(1, "Elegir Desde ",0,0,250)
       For aa =1 To 9 
           AddListViewItem(1, listoctava(aa)+" "+Str(aa),0,aa,0)
       Next
       
  EndIf   
  If paso=1 Then  
       AddListViewColumn(1, "Elegir Hasta ",0,0,250)
       For aa =1 To 9 
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

         Var event= waitEvent

          If event=eventgadget Then
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
'Return IUP_DEFAULT
End Sub
'---------------------------- main -----------------

Do
   event=WaitEvent
   If event=EventMenu then
      Select case EventNumber
         Case 1001
            MessBox("","1 Menu")
         Case 1002
            MessBox("","2 Menu")
       Case 1003 ' seleccion octavas 
           seloctava (desde, hasta)
       Case 1004 ' crear Track
        CreaTrack (desde, hasta)
       Case 1005
       MessBox("","5 Menu")
       Case 1006
       MessBox("","6 Menu")
      Case  1007 
         MessBox("","7 Menu")
      Case  1008
         MessBox ("","8 Menu")
      Case  1009
         MessBox ("","9 Menu")
     
      End Select
   EndIf
   If event=EventClose Then End
Loop

End 0