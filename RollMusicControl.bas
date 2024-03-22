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
#Include "foro/window9.bi"
#Include Once "gtk/gtk.bi"
'#Include "midiinfo.bi"
#Include "ROLLCONTROLDEC.bi"
'#include "vbcompat.bi"
#Include Once "freetype2/freetype.bi"
#Include once  "file.bi"
'#include "mod_rtmidi_c.bi"
'#Inclib  "rtmidi.dll" 'usa librerias estaticas 
#include "fbthread.bi"
#Include "crt.bi" ' QSORT



'Var hwnd = OpenWindow("RollMusic Control",10,10,ancho*3/4,alto*3/4)

'Dim As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8
Common shared As Integer menuNro, menuNew, desde , hasta, rango,RollDur,RollNota,compasX
common Shared As Integer  ANCHO,ALTO
Common Shared As FLOAT font
COMMON Shared As Long eventc, eventM
Common Shared As hwnd hwndC, hwndListBox, hwndPatronEjec
Common Shared As BOOLEAN ROLLCARGADO, TRACKCARGADO, CANCIONCARGADA , NADACARGADO, CANCIONCREADA,EJECCARGADA
Common Shared As string pathdir,nombre,nombreMidiIn
common Shared As String NombreCancion, NombrePista
Common Shared As Integer cargaCancion, pid1,clickpista',pistacreada
Common Shared As cairo_t  Ptr c, c2
Common Shared As Any Ptr surface,surf2 
Common Shared As FT_Face ftface
common Shared as any ptr thread1, thread2,threadPenta,thread3,pubi,threadloop,p1,threadMenu ,threadmetronomo
Common Shared As Any Ptr thread4
Common Shared As Integer nfont,nmxold,nmyold,nancho,nalto,ndeltaip,nVerEscalasAuxiliares,nanchofig
Common Shared As Integer mxold,myold, w,h,grado,nVerCifradoAcordes, HabilitarPatrones
Common Shared As integer ubirtk, ubiroll,trasponer,canalx
common Shared As Integer NB , NA, CantTicks, tempo, CantMin,CantCompas
Common Shared  portsal As UByte, patchsal As ubyte
COMMON Shared As Integer MaxPos,ntk,CPlay, guardopos,ntktab,ntoca,ntkp, npi,calltoca,npo
Common SHARED  As Integer EstaBarriendoPenta, instancia

 MaxPos=2:ntk=0:CPlay=0: guardopos=0:ntktab=0
Common Shared As Integer  posicion,posicionOld,posn,terminar
 posicion=0:posicionOld=0:posn=0
 


Type dat Field=1
 nota As UByte =0 ' 1 a 12, en un futuro contendra nota, octava, canal etc 
 dur As  UByte =0 ' duracion 1 a 180, tambien tendra rasguidos distintos programables por usuario o fijos
 vol As  UByte =0 ' volumen hasta 127 es volumen desde ahi es escala 128 a 255 =127 escalas
 pan As  UByte =0 ' paneo + o -
 pb  As  UByte =0 ' pitch bend + o -
 inst As UByte =0 ' instrumento para cada nota podra ser distinto 1 to 128
 ' Nota de escala son 12 ..bemol o sostenido son 2
 ' entonces en 14 numero stengo la info
 ' 129 -> c,130->c#,131->d...140->B--, 141-sos,142,bemol
 ''t   As Ulong   '  ticks por ahroa no 
End Type
' dentro del vol pondremso las escalas
' chords http://www.looknohands.com/chordhouse/piano/ ahi hay 168 escalas..!!
' en vol tengo desde 129 a 255 para numerar escalas. si faltan puedo usar pan o pb
' l aidea es poner en que escala esta cada nota o compas y asi poder tener cambios de escla
' y construir los acordes que se quieran construir es esa escala de esea nota o del compas o 
' la escala del ultimo cambio...por default la escala sera C mayor.. 




'Type esc1 
'  nombre   As String
'  nropasos As Integer
'  pasos    As Byte Ptr   
'End Type

'common shared as esc1 ptr pescala
  trasponer=0

'-------------


'-----------
dim Shared As String  ProgError(0 To 17)
Dim Shared As Integer ContadorError=0
'Dim As Long event=0

#Include "RTMIDIDEC.BI"


On  Error GoTo errorControl
'------
Sub  seloctava  ( ByRef octadesde As Integer, ByRef octahasta As integer) 
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x  
Dim listoctava(1 To 9) As string ={"0","1","2","3","4","5","6","7","8"}

For x= 1 To 2 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("OCTAVAS",100,50,400,400, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
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
          If eventC= EventClose Then
             Close_Window(haw)
             Exit Do 
          EndIf
          
         Loop 
Next x
'' fin ruso
If octadesde=0 Then
octadesde=1
EndIf
If octahasta=0 Then
octahasta=9
EndIf
print #1,"OCtadesde ",octadesde
print #1,"OCtahasta ",octahasta

End Sub
'---------------------------
Sub selInstORdenAlfa(ByRef instru As Integer)
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw,hwl
Dim As Integer aa ,paso=0,x=0,i1=0,i2=0 ,Posx,Posy ,x0,y0  
Dim cad As String
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100
' la seleccion empieza de 1, no devuelve 0 para el 1er elemento 
 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("INSTRUMENTOS PATCH POR ORDEN ALFABETICO",500,Posy,700,600, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS=&h10000000
     ' commctrl.bi modificado
     hwl=  ListViewGadget(1,10,10,500,500,,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN CAMBIA",0,aa,0)
        If patchsal > 0 Then ' ya habia un instrumento es un cambio 
           instru=CInt(patchsal) 
       EndIf

       For aa =1 To 127 
               i2=InStrrev(NombreInstAlfa(aa)," ")
               cad=Mid(NombreInstAlfa(aa),i2)
      '         Print #1,"cadena ",cad
               
     
           If instru = CUByte(ValInt(cad)) Then
              AddListViewItem(1, "[x] "+NombreInstAlfa(aa),0,aa,0)
           Else
              AddListViewItem(1, "[ ] "+NombreInstAlfa(aa),0,aa,0) 
           EndIf
           
       Next
       


       ButtonGadget(2,530,30,100,40,"CAMBIA")


         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventAlfa= waitEvent
         If eventAlfa=EventLBDown Then ' 26-02-2022
            If EventNumberListView=1 Then
               i1 = GetItemListView()
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)           
               If InStr(cad,"x")>0 Then
                 cad="[ ] " +NombreInstAlfa(i1) 
               Else  
                 cad="[x] " +NombreInstAlfa(i1) 
               EndIf
               ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If eventAlfa=eventgadget Then
          
            If eventnumber()=2 And InStr(cad,"x") >0 Then
               ''i1 = GetItemListView()
               Print #1,"alfa seleccion in", i1
               Print #1,"NombreInstAlfa ",NombreInstAlfa(i1)
               i2=InStrrev(NombreInstAlfa(i1)," ")
               cad=Mid(NombreInstAlfa(i1),i2)
               Print #1,"cadena ",cad
               instru = CUByte(ValInt(cad))
                print #1,"seleccion instrumento alfa instru = ",instru     
               patchsal=instru
                  Close_Window(haw)
                  Exit Do
              
            End If
          EndIf 
          If eventAlfa=EventClose Then
                  Close_Window(haw)
                 Exit Do 
           EndIf          
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
print #1,"Str(instru) ", Str(instru)
  

end Sub

Sub selInstORdenNum (ByRef instru As integer) ' NO TIENE EN CUENTA EL TIPO NI GRABA A DISCO
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw,hwl
Dim As Integer aa=0 ,paso=0,x=0, Posx,Posy ,x0,y0 
Dim As String cad
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("INSTRUMENTOS PATCH ORDEN NUMERICO",500,Posy,700,600, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
     ' commctrl.bi modificado
Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
     hwl=  ListViewGadget(1,10,10,500,500,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
     AddListViewItem(1, "CLICK EN UN ITEM  Y EN CAMBIA",0,aa,0)
           If patchsal > 0 Then ' ya habia un instrumento es un cambio 
              instru=CInt(patchsal) ' 
           EndIf
       For aa =1 To 127 
           If instru = aa Then
              AddListViewItem(1, "[x] "+NombreInst(aa),0,aa,0)
           Else
              AddListViewItem(1, "[ ] " +NombreInst(aa),0,aa,0) 
           EndIf
 
           '''Track(ntk).trk(1,1).inst=CUByte(instru)
           
       Next
       


       ButtonGadget(2,530,30,100,40,"CAMBIA")

         #Ifdef __FB_WIN64__
           SetFocus (hwl) 
           SetForegroundWindow(haw)
          #Else
           gtk_widget_grab_focus(GadgetID(1))
         #EndIf
         Do

         Var eventNum= waitEvent
         If eventNum=EventLBDown Then ' 26-02-2022
            If EventNumberListView=1 Then
               instru = GetItemListView()
               cad=GetTextItemListView(1,GetItemListView,GetSubItemListView)           
               If InStr(cad,"x")>0 Then
                 cad="[ ] " +NombreInst(instru) 
               Else  
                 cad="[x] " +NombreInst(instru) 
               EndIf
               ReplaceTextItemListView(1,GetItemListView,GetSubItemListView, cad)
           EndIf
        EndIf

          If eventNum=eventgadget Then
          
             If eventnumber()=2 And InStr(cad,"x") > 0 Then
               ''Instru = GetItemListView()
                Print #1,"inst seleccionado numerico ",instru
                Close_Window(haw)
                Exit Do
            End If
          EndIf 
            If eventNum= EventClose Then
               Close_Window(haw)
               Exit Do 
            EndIf
          
         Loop
         

'' fin ruso
'Return IUP_DEFAULT
print #1,"Str(instru) ", Str(instru)

End Sub
' ---------

'------------
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
Sub CrearDirCancion (Byref NombreCancion As string)

If NombreCancion = "" Then
   NombreCancion= Date
EndIf
pathdir = ShellFolder( "Select Folder", "C:\")
pathdir=pathdir+"\"+NombreCancion
print #1, "DIRECTORIO CANCION EN ",pathdir
CreateDir(pathdir)
SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + pathdir)
NombreCancion=pathdir
print #1,"NombreCancion en CrearDirCancion ",NombreCancion
CANCIONCREADA=TRUE
CreateDir(pathdir+"\Temp") ' ok

End Sub
'
Sub cargarDirectorioCancion (ByRef NombreCancion As string)
''Dim pathdir As string
    SetForegroundWindow(hwndc)
NombreCancion = ShellFolder( "Select Folder", "C:\")
SetWindowText(hwndC, "RollMusic Cancion: " + NombreCancion)


print #1,"cargarDirectorioCancion ", NombreCancion 
' aca NombreCancion contiene el path tambien....
'Sleep 100
End Sub
'
Sub EntrarNombrePista(ByRef NombrePista As String)

  NombrePista = InputBox("InputBox",,NombrePista)

End Sub
'
Function sacarNtk (item As String) As Integer
Dim As Integer ubi1,ubi2
 ubi1 = InStr(item,"[")
 ubi2 = InStr (item,"]")
 sacarntk=CInt(Mid(item,ubi1+1,ubi2-ubi1-1))

End Function
'
Function sacarExtension(file As string) As String
 Dim ubi1 As Integer
 ubi1=InStrRev (file,".")
 sacarExtension=Mid(file,1,ubi1-1)
 
End Function

'
Sub copiarATemp ( titulo As String, pista As String)
Dim As String destino 
destino=NombreCancion+"\Temp\"+pista

copyFileA (StrPtr(titulo),StrPtr(destino),TRUE)
print #1,titulo, destino   
End Sub
'
Sub BorrarPista (titulo As String)

deleteFileA (StrPtr(titulo))

End Sub
'
Sub  verayuda (  arch As string)
 ' no hace falta por ahora pero en elfuturo haremos ayuda puntual
' y esta sera la sub recibira donde se requiere ayuda y se devolvera la ayuda 
' correspondiente

End Sub 
 

'
' error
errorControl:
ProgError(0) = "No error"
ProgError(1) = "Illegal function call"
ProgError(2) = "File not found signal"
ProgError(3) = "File I/O error"
ProgError(4) = "Out of memory"
ProgError(5) = "Illegal resume"
ProgError(6) = "Out of bounds array access"
ProgError(7) = "Null Pointer Access"
ProgError(8) = "No privileges"
ProgError(9) = "interrupted signal"
ProgError(10) = "illegal instruction signal"
ProgError(11) = "floating point error signal "
ProgError(12) = "segmentation violation signal"
ProgError(13) = "Termination request signal"
ProgError(14) = "abnormal termination signal"
ProgError(15) = "quit request signal"
ProgError(16) = "return without gosub"
ProgError(17) = "end of file"
  
Dim As Integer er1, ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 0 And ContadorError < 101 Then
Print #1,"------------------------------------"
  ContadorError=ContadorError+1
  Print #1,"ErrorControl ContadorError ",ContadorError
  Print #1,"ErrorNumber1 ",ErrorNumber1
  Print #1,"progerror ", ProgError(ErrorNumber1); " on line ";ErrorLine1
  Print #1,"Error Function: "; *Erfn()
  Print #1, "mensaje, Ermn ", *Ermn, Ermn
  Print #1,"------------------------------------"

EndIf
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )

