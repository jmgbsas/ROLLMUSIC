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

#Include "ROLLGLOBALDEC.bi"
'#include "vbcompat.bi"
#Include Once "freetype2/freetype.bi"
#Include once  "file.bi"
'#include "mod_rtmidi_c.bi"
'#Inclib  "rtmidi.dll" 'usa librerias estaticas 
#include "fbthread.bi"
#Include "crt.bi" ' QSORT

Declare Function InputBoxJmg(ByRef Caption As USTRING, ByRef Message As USTRING, ByRef DefaultString As USTRING, ByVal flag As Integer, ByVal flag2 As Integer, hParentWin as Hwnd = 0) As USTRING
Type plano 
  sumatiempo As Integer  'tiempo acumulado de los eventos midis
  canal      As UByte 
  estado     As UByte  ' nota on of 
  nota       As UByte  ' notapiano 
  vel        As UByte  ' velocidad
End Type

ROLLCARGADO    = FALSE
TRACKCARGADO   = FALSE
CANCIONCARGADA = FALSE
NADACARGADO    = TRUE
CANCIONCREADA  = FALSE
EJECCARGADA    = FALSE

Dim Shared As HWND velimg, Figimg, FigVol, FigKey
Const IMAGE_VEL= 30
Const IMAGE_FIG1 = 31
Const IMAGE_FIG2 = 32
Const IMAGE_FIG3 = 33

Const As BOOLEAN HABILITAR = TRUE
Const As BOOLEAN DESHABILITAR = FALSE
Const NO=0

MICROSEGUNDOS_POR_NEGRA=1000000 ' 60 MILLONES / 60 BPM DEFAULT


 MaxPos=2:ntk=0:CPlay=NO: guardopos=0:ntktab=0
/'
Common Shared As Integer  posicion,posicionOld,posn,terminar,posnOffOld,posnOff,deltax,deltay,guardaguardaposnOffOld
 posicion=0:posicionOld=0:posn=0

deltax=1 
trasponer=0
'/
'-------------


'-----------
dim Shared As String  ProgError(0 To 17)
Dim Shared As Integer ContadorError=0
'Dim As Long event=0

#Include "RTMIDIDEC.BI"


On  Error GoTo errorControl
Sub CuadroVel()
         velimg=OpenWindow("Cuadro de Tempos Clasicos ",400,100,800,400)
         ImageGadget(IMAGE_VEL,10,10,1100,600,Load_image(".\recur\velocidades.jpg"),0,SS_BITMAP )
         #Ifdef __FB_WIN64__
          SetFocus (velimg) 
         SetForegroundWindow(velimg)
         #Else
            gtk_widget_grab_focus(GadgetID(IMAGE_VEL))
         #EndIf
          
        
           Do 
              Var eventVel= waitEvent()
              If eventVel=EventClose Then
                 Close_Window(velimg)
                 Exit Do 
              EndIf
           Loop   


End Sub
Sub CuadroDur()
               Figimg=  OpenWindow("Duraciones de Figuras y sus Teclas",200,50,1300,900  )
               ImageGadget(IMAGE_FIG1,10,10,1100,800,Load_image(".\recur\FIGURAS.jpg"))
               #Ifdef __FB_WIN64__
                SetFocus (Figimg) 
               SetForegroundWindow(Figimg)
               #Else
                 gtk_widget_grab_focus(GadgetID(IMAGE_FIG1))
               #EndIf
          
           Do 
              Var eventDur= waitEvent
              If eventDur=EventClose Then
                 Close_Window(Figimg)
                 Exit Do 
              EndIf

           Loop   

End Sub 
Sub CuadroVol()
               FigVol=  OpenWindow("Volumen tipicos en partituras ",800,100,400,600  )
               ImageGadget(IMAGE_FIG2,10,10,1100,800,Load_image(".\recur\VOLUMEN.jpg"))
               #Ifdef __FB_WIN64__
                SetFocus (FigVol) 
               SetForegroundWindow(FigVol)
               #Else
                  gtk_widget_grab_focus(GadgetID(IMAGE_FIG2))
               #EndIf
          
          
           Do      
              Var eventVol= waitEvent
              If eventVol=EventClose Then
                 Close_Window(FigVol)
                 
              EndIf
           Loop
   

End Sub
'---------
Sub CuadroKey()
               FigKey=  OpenWindow("Teclas Rapidas, keystroke  ",800,100,600,600  )
               ImageGadget(IMAGE_FIG3,10,10,1100,800,Load_image(".\recur\TECLAS_RAPIDAS.jpg"))
               #Ifdef __FB_WIN64__
                SetFocus (FigKey) 
               SetForegroundWindow(FigkEY)
               #Else
                  gtk_widget_grab_focus(GadgetID(IMAGE_FIG3))
               #EndIf
          
          
           Do      
              Var eKey= waitEvent
              If eKey=EventClose Then
                 Close_Window(FigKey)
                 
              EndIf
           Loop
   

End Sub

'------

Sub selInstORdenNum (ByRef instru As integer) ' NO TIENE EN CUENTA EL TIPO NI GRABA A DISCO
'If ntk =0 Then
'   Exit Sub
'EndIf
Dim As hwnd haw,hwl
Dim As Integer aa=0 ,paso=0,x=0, Posx,Posy ,x0,y0,i2 
Dim As String cad
ScreenControl GET_WINDOW_POS, x0, y0
Posx=x0 +50
Posy=y0 +100

 
'' => desde acaecho con tool del ruso no anda muy bien
     haw=OpenWindow("PATCH ORDEN NUMERICO CLICK EN UN ITEM  Y EN CAMBIA",500,Posy,700,600, WS_OVERLAPPEDWINDOW Or WS_VISIBLE, WS_EX_TOPMOST )
     'Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
     ' commctrl.bi modificado
Var LVS_EX_AUTOSIZECOLUMNS = &h10000000
     hwl=  ListViewGadget(1,10,10,500,500,LVS_EX_AUTOSIZECOLUMNS,,,32,LVS_SINGLESEL )
     
     AddListViewColumn(1, "Elegir De 1 a 128 ",0,0,250)
    
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
               instru = GetItemListView() +1
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
  NombreCancion = InputBoxJmg("InputBox","",NombreCancion, ES_MULTILINE + ES_AUTOVSCROLL, 0)
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
pathdir = ShellFolder( "Select Folder", CurDir())
pathdir=pathdir+"\"+NombreCancion
print #1, "DIRECTORIO CANCION EN ",pathdir
CreateDir(pathdir)
SetWindowText(hwndC, "RollMusic Control Editando Cancion: " + pathdir)
NombreCancion=pathdir
print #1,"NombreCancion en CrearDirCancion ",NombreCancion
CANCIONCREADA=TRUE
NADACARGADO=FALSE
CreateDir(pathdir+"\Temp") ' ok

End Sub
'
Sub cargarDirectorioCancion (ByRef NombreCancion As string)
''Dim pathdir As string
    SetForegroundWindow(hwndc)
NombreCancion = ShellFolder( "Seleccionar Carpeta de Cancion", CurDir())

SetWindowText(hwndC, "RollMusic Cancion: " + NombreCancion)


print #1,"cargarDirectorioCancion ", NombreCancion 
' aca NombreCancion contiene el path tambien....
'Sleep 100
End Sub
'

Sub EntrarNombrePista(ByRef NombrePista As String,hwndC as Hwnd )


NombrePista = InputBoxJmg("Nombre de Pista","",NombrePista, ES_MULTILINE + ES_AUTOVSCROLL , 0,hwndC  )
Dim As String limpio, a1
Dim I As Integer
For I=1 To Len(NombrePista)
  a1=Mid(NombrePista,i,1)
  If a1<> Chr(13) And a1<> Chr(10) Then
   limpio=limpio+a1
  EndIf
Next I
NombrePista=limpio

End Sub
'
Function sacarNtk (item As String) As Integer
Print #1," sacarNtk string que llega nombre pista ";item
Dim As Integer ubi1=0,ubi2=0
 ubi1 = InStr(item,"[")
 ubi2 = InStr (item,"]")
If ubi1=0 Then
 ubi1 = InStr(item,"(")
EndIf
If ubi2=0 Then
 ubi2 = InStr(item,")")
EndIf

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
Print #1,"en copia titulo", titulo
Print #1,"en copia pista", pista

copyFileA (StrPtr(titulo),StrPtr(destino),TRUE)
print #1,titulo, destino   
End Sub
'
Sub BorrarPista (titulo As String)
'Print #1, "me piden borrar ", titulo
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
  
Dim As Long er1, ErrorNumber1, ErrorLine1
ErrorNumber1 = Err
ErrorLine1 = Erl

If ErrorNumber1 > 1 And ContadorError < 101 Then
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

FileFlush (-1)
