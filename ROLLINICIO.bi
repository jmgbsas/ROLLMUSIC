' 149 corregi la entrada de port y salada para las ejecuciones,,, ver si anda para cancion,,,
' lo importante es que toque en varios lados del codigo el pvalor de portin y portout
' y hay diferencia en port reales que se envian a rtmidi comenzando en cero y ports logicos para el 
' programa que empiezan desde 1 ,,,,verificar que todo funciona,,, a lo mejor sonoalgo o talvez
' anda mejor porque en varios lados se enviaba al fisico el portout sin restarle 1 verificar exhaustivamnete!!!
' ============================================
' apertura de ports en play  
' tiempoPatron a entero no tiene porque ser double, se graba en archivo
' Se agrego formar acordes aun sin nota en el lugar elegido, se deb eentrar al duracion
' Triadas desde Tonica completo Mayor Menor Disminuido formacion y play 
' fix consumo cpu S5=2 y fueradefoco=0 eliminado
' Funciona Acorde en Tonica triaca,,Ctrl+clik derecho luego seguir con mayor hasta no inversion
' SE ELIMINO DE 'Q' la configuracion de tama�os, proporciones y font
' se agrego nverEscalasAuxiliares y nanchofig a RollMusic.ini
' se agrego en Ver, si se ven o no las Escalas Auxiliares en el grafico
' TODO MULTIKEY IR PASANDO DE A POCO PROBANDO A E.SCANCODE MULTIKEY ES UNA BASURA REPITE EL COMANDO MIL VECES
' paso previo para armar acordes: necesitamos poder INGRESAR CAMBIOS DE ESCALA  y guardarlos en la secuencia
' pero al tocar se saltean como si no existieran,,,al retroceder o avanzar en la secuencia se debe ir actualizando
' la escala en uso, esto permite al ingresar un acorder construirlo en base a la escala usada en ese tramo.
' usaremos xml para leer y escribir musicxml e intercambia rocn otros programas
' antes que midi despue salgun dia haremos midi no se veremos.... 
' http://xmlsoft.org/examples/index.html
' YA cierra todas las sesiones de rollmusic desde control
' futuro grabar mxold y algo mas para conservar el tama�o de la ventana y el tama�o del font
' usado por el usuario !!!! OK Y AANDA
' el borrado de columna esta defectuoso hay que dar 0 y luego 12 x en toda la octava para
' que borre mejor usaremos marcas de zona para borrar.
' dejo de andar marcado de zonas porque habia un exit do en COMEDIT=False con mousex>50
' se movio zonas dentro de mousex> 50 y luefo COMEDIT=false volvio a funcionar
 ' se intento usar Byte en vez de Ubyte para usar negativos pero hay qu emodificar mucho
 ' se usara IF variabrlee > 127 par ausar por ejemplo Vol > 127 para indicar escalas...
 ' Esta nota base...es Tonica 3era 5ta 7ma ...
 ' uso ctrl+click para ingresar notas nuevas en Edit, sino al pasar a Ctrl-M u otras acciones
 ' entraba notas no deseadas..
 ' 11-12-2021 redusco la camtidad de partes a 20 partes_falta (1 To 20), partes_sobra(1 To 20)
 ' rooloop 2673 menu contextual acordes desarrollo 06-12-2021
 ' correccion Clcik end EDIT 06-12-2021 s3=0 movido a y > 50 
 ' correccion de abrir nota si menor=mayor no hace nada, allevantar click rompia todo
 ' v23 fraccionar automaticamente en COMEDIT cursor al poner notas menores o 
 ' mayores en duracion a otra nota en acorde existente, tambien armar acordes desde una nota
 ' existente como tonica mayores menores etc,,buscar al tonica si consideramos es una 3era
 ' o una 5ta..
 ' V22 agregamso menu contextual en lectura con click derecho para acordes falta desarrollar
 ' v22  SetStateMenu(hmessages,1102,3) o  SetStateMenu(hmessages,1103,0) check items menu
 ' V22 abrir nota se ajuto final dejaba una columna vacia
 ' V21 SE AJSUTO MOVER LA VENTANA DRAGANDO LA CINTA SUPERIOR FUNCIONA MEJOR
 ' V21 TREADdETACH DE tHEREADlOOP Y THREAD1 PLAY CLOSE PORTS ETC EN EL CIERRE DE CONTROL 
 'V21 ESTRUCTURO ACORDESONIGUALES Y COLOCO ALLOF EN VARIAS PARTES,Q,FIN PLAY, P.
 ' V19 TOCA BASTANTE BIEN ACORDES IGUALES CON SILENCIOS EN SU FORMACION
 ' Y CALCOMPAS AHROA INCLUYE SILENCIOS 
 ' v14 ...AOI-NUEVO PERFECTO TODOS LOS ACORDES IGUALES EN ACORDES TODAS LAS POSICIONES DBEN ESAR LLENAS
 ' CON NOTAS CON SONIDO O SIN SONIDO PERO TODOS CON LA MISMA CANTIDAD DE NOTAS POR AHORA UNAS PODRAN SONAR
 ' OTRAS NO SEGUIR CON MAS PREUBAS...ANDA OK CON LOS POCOS CASOS QUE TENGO...
 ' V10 FRACCIONADOR divido la nota seleccionada en n partes 
  ' v8 fix nucle dur=0 nota=181 sino el borrado de notas anda mal
  ' toda celda debe tener 0,181 nada de 181,181...eso se cambio
  ' V7 CRASH DE SPACE EN PLAY, Y VER ACORDES DISTINTOS SI SE PUEDE CAMBIAR UNA NOTA LARGA
  ' EN 2 CORTAS AUTOAMTICAMENTE PARA PONER EN ACORDE OTRAS 2 MAS CHIVAS EL:
  ' P    ==> L+I* || DISCERNIR (1) |P|     DE (2)| P   |
  ' L+I      L+I                   |L| I         | L I |
  ' EL ULTIMO CASO (2) NO SE PUEDE EN ROLL , EL (1) SI
  ' O SEA QUE EL PROGRAMA AUTOMATICAMNETE PARTA UNA NOTA LARGA COMO P I O ETC
  ' FRENTE A OTRAS EN ACORDE MAS CHICAS Y UNIDAS O NO...
  ' V5 CORREGIDO, V6 CORREGIDO OTRAS COSAS,,QUEDA CRASH DE PLAY CON SPACE...
  ' 08-11 V5 anda mejor qu ela V4 solo que la ligadura I+I+I la toca como I+I
  ' LE FALTA UNA NEGRA DE DURACION, EL RESTO LO TOCA BIEN!!!
  ' SIN TOCAR CASI NADA SOLO ELIMINAR EL ANALISIS DE LIGA EN PLAYALL
  ' SEGUIR CORREGIR CON EL USO DE LSO CAMPOS NUEVOS Y AL TERMINAR 
  ' ELIMINAR LOS CAMPOS DE VEC QUE NO SE USEN
' ------------------------------
'  -Wc -fstack-usage genera un archivo en runtime *.su con el uso del stack
' por cada funcoin el default total es 1024k, -t 1024 no haria falta colocarlo
' en la linea de comando
' https://gcc.gnu.org/onlinedocs/gnat_ugn/index.html#SEC_Contents
'====> clave DEVF (desarrollo futuro, comentarios en lugares apr adessarrollar mas
' funcionalidades.)
'============================================================
' ROLLMUSIC SECUENCIADOR CON PISTAS DE INGRESO POR PASOS O EJECUCION POR TECLADO  
'============================================================
' -------------------------------------------------
#define __FB_WIN64__
#if defined (__FB_WIN64__) 
#libpath "C:\msys64\mingw64\lib"
#else
#libpath "/usr/lib"
#endif
#define EXTCHAR Chr(255)


#include "mod_rtmidi_c.bi"
#Inclib  "rtmidi.dll" 'usa librerias estaticas 
'#Inclib  "rtmidi"  '''uso al dedeisco rtmidi.dll
#include "fbthread.bi"
#include "crt.bi" ' QSORT
 
#define WIN_INCLUDEALL
#include once "windows.bi"
#include once "win/shlobj.bi"

'#Include Once "/win/commctrl.bi"
'#Include "crt/stdio.bi"
#include "file.bi"
#include "fbgfx.bi" ' se carga antes de windows.bi para evitar duplicates..o conflictos
'#Include Once "win/mmsystem.bi" '' FUNCIONES MIDIde windows!!!! perousaremos RtmidiC por hora
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif

' Nota: algun dia si quiero midifile intentar usar una libreria de C pura 
' C:\IT64\AREAWORKAUX\MIDI-LIBRARY\midilib-master\midilib-master\freeBasic
#include Once "rollutil.bi"

'Sub getfiles(ByRef File As OpenFileName,flag As String, accion As String)
'    Dim As ZString * 2048 SELFILE
'    Dim As String MYFILTER=flag+Chr(0)
'    With File
'  .lStructSize = SizeOf(OpenFileName)
'  .hwndOwner = NULL
'  .hInstance = NULL
'  .lpstrFilter = StrPtr(MYFILTER)
'  .nFilterIndex = 0
'  .lpstrFile = @SELFILE
'  .nMaxFile = 2048
'  .lpstrFileTitle = NULL
'  .nMaxFileTitle = 0
'  .lpstrInitialDir = @"nosuch:\"
'  .lpstrTitle = @"Open"
'  .Flags = 4096
'  .nFileOffset = 0
'  .nFileExtension = 0
'  .lpstrDefExt = NULL
'    End With
'    If accion="open" Then
'    GetOpenFileName(@File)
'    EndIf
'    If accion="save" Then
'    GetSaveFileName(@File)
'    EndIf
'
'End Sub
' FILE DIALOG adicionales
'Sub  abrirSecuencia(nf As integer) 

'  If  Open ("./secuenciaPLAY.txt" For Output Shared As nf ) <> 0 Then
'    		Print #1, "secuenciaPLAY.txt no abre!"
'  Else
'      Print #1, "secuenciaPLAY.txt abierta!"
'  EndIf
'End  sub 

'--------
Sub cerrar (ByVal n As Integer)
   If n=0 Then
    FileFlush (-1)
    Close 
   Else 
    FileFlush (n)
    Close n
   EndIf 
End Sub
'--------
Sub  porterrorsub(porterror As integer)
          Select Case porterror
            Case RTMIDI_ERROR_WARNING
              'strerror(n as integer) as zstring ptr

            Case RTMIDI_ERROR_DEBUG_WARNING
              cerrar 0             
              End
      
            Case RTMIDI_ERROR_UNSPECIFIED
              cerrar 0
              End
      
            Case RTMIDI_ERROR_NO_DEVICES_FOUND
              cerrar 0
              End
      
            Case RTMIDI_ERROR_INVALID_DEVICE
              cerrar 0
              End
      
            Case RTMIDI_ERROR_MEMORY_ERROR
              cerrar 0
              End
      
            Case RTMIDI_ERROR_INVALID_PARAMETER
              cerrar 0
              End
      
            Case RTMIDI_ERROR_INVALID_USE
              cerrar 0
              End
      
            Case RTMIDI_ERROR_DRIVER_ERROR
              cerrar 0
              End
      
            Case RTMIDI_ERROR_SYSTEM_ERROR
              cerrar 0
              End
      
            Case RTMIDI_ERROR_THREAD_ERROR
              cerrar 0
              End
          End Select


End Sub
On Error Goto errorhandler

Dim Shared file As OpenFileName
Dim Shared As String myfilter
myfilter  = "Roll Files"+Chr(0)  +"*.roll"+Chr(0)
'myfilter += "Ini files"+Chr(0)   +"*.ini;*.txt;*.cfg"+Chr(0)
myfilter += "Rtk  Files"+Chr(0)   +"*.rtk"+Chr(0)
Dim Shared As Long pd1, fa1,ffini,ca,ffile,ct,ga,grt ,ngm,fk

fk=5
Open "AAAAA-test.TXT" For Output As #fk
Dim Shared As Integer abierto=0
Common Shared  mensaje As Integer 
'''  end file dialog  
' para GTK Gtk:list()
'#Include Once "crt.bi"
#include once "gtk/gtk.bi"

' This is our data identification string to store data in list item
Dim  Shared   As String  pathinicio 
pathinicio = CurDir  
Const list_item_data_key ="list_item_data"
' fin GTK
' -- INICIO openGL GLFW 3.1.1 
/'
' habilitamos OPENGL para ver si podemso mostrar la grabacion de ejecucion u otra cosa
' de entrada por MIDI-IN.
#Include once "glfw3.bi"
If glfwInit()=GL_FALSE then
  print "error: can't init GLFW"
  beep : sleep : end 1
end If
Dim As GLFWwindow ptr  win
' ----FIN OPENGL 
'/
'===============================
#include "ROLLDEC.BI"


'------------------

pd1 = GetCurrentProcessId()  

Open "midebug.txt" For Output As #1
'' Open "midebug"+ "["+Str(pd1)+"]" + ".txt" For Output As 1



'Open "mivector.txt" For Output As #3
'Open "miplayall.txt" For Output As #4
'Open "test-AAAAA.TXT" For Output As #5
'Dim fcon As Integer 
'fcon=freefile
'Open cons  for Output As #8

''Open "figuras.txt" For Output As #1
' secuenciador de 9 octavas estereo, modo Piano Roll,hace uso de
'letras para las duraciones en vez de rectangulos...
' edicion modificacion insercion,,,12 eventos c/u con
'nota, duracion,volumen ,paneo, pitch bend e instrumento
' version actual front end solamente 1 track...no reproduce
' no genera midi todavia..
'-------------
''
'=======================
'--------------
#include "string.bi"
#include once "cairo/cairo.bi"
#include "midiinfo.bi"
'==============================
'#Include "NOTAS.bi"
#include "RTMIDIDEC.bi"
'==============================
#include "ROLLCONTROLDEC.bi"
'=============================
' iup start
' SOLO PARA WINDOWS LEE MIDI FILES PERO FALTA FUNCIONES PARA GRABAR!!!
'---> #Include once "foro/fmidi.bi"
#include once "fbthread.bi"
#include "foro/window9.bi"

'#Include "crt/win32/unistd.bi"
#inclib "ntdll"


'''Open "secuencia.txt" For Output As 5

Const NEWLINE = !"\n"

'tempo I=160, seri equivalente a O=40 como l maxima cantdad de ticks de O es
' eldela figura mas peque�a=128 ticks...40 * 128 = 5120 por minuto.
' Si deseo un secuecnia de CantMin minutos
tempo=160  ' negra=160
CantMin=15
'NotaBaja=1 : NotaAlta=128

'Dim direp As ZString  Ptr
'Dim dires As String

Dim As Integer com_usarmarco =0
For ix = 0 To __FB_ARGC__

 If ix=1 And Command(ix) > "" Then ' deberia entregarme el archjivo el SO pero no lo hace
  
 ubirtk = InStr (LCase(Command(ix)),".rtk")
 ubiroll=  InStr(LCase(Command(ix)),".roll")
' esto es por comando interno no fisico con click
''' ubicancion=InStr(LCase(Command(ix)),"@dir")

 If ubirtk > 0 or ubiroll>0  Then
   ntk=0 
   titulos(0)=Command(ix)
   Instancia=1 ' no se condice con el caso real da 2 ???
 Else
    desde= CInt(Command(ix))
'    pmTk(ntk).desde=desde
   Instancia=1    
 EndIf
    'sigue en roolloop principio
 EndIf
 If ix=2 And Command(ix) > "" Then
  hasta= CInt (Command(ix))
 ' pmTk(ntk).hasta=hasta
    Instancia=2
 EndIf
 
 If ix=3 And Command(ix) > "" Then
  titu=  (Command(ix))
     Instancia=3
 EndIf

 If ix=4 And Command(ix) > "" Then
  instru=  CUByte (Command(ix))
     Instancia=4
 EndIf

 If ix=5 And Command(ix) > "" Then
  pid1=  CInt (Command(ix))
     Instancia=5
 EndIf

 If ix=6  And Command(ix) > "" Then
  com_usarmarco=  CInt (Command(ix))
     Instancia=6
 EndIf
' en 7 diria @dir en la linea de comando  
' puedo poner basura en todos los otros
 If ix=7 And Command(ix) > "" Then ' ok probado pasa bien el NombreCancion con el path
  NombreCancion =  Command(ix)
     Instancia=7
 EndIf

Next ix

' uso para volcar midi a text odesde un roll o trk usamos cancion
' para pasar solo una pista 
If NombreCancion > "" Then 
 ubirtk = InStr (LCase(Command(7)),".rtk")
 ubiroll=  InStr(LCase(Command(7)),".roll")

If ubirtk > 0 or ubiroll>0  Then
   ntk=0 
   titulos(0)=Command(ix)
   Instancia=1 ' no se condice con el caso real da 2 ???
EndIf

EndIf

''SI DESDE CTRL TRAEMOS UN GRAFICO SOLITO ->' Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))

'Dim Shared As Integer pd1, fa1 

'pd1 = GetCurrentProcessId()  
'Open "midebug" + "["+Str(pd1)+"]" + ".txt" For Output As #1

''Open "midebug.txt" For Output As #1
fa1=2
Open "procesos.txt" For Append As fa1
If pid1=0   Then ' EMPEZO EL ONLINE SU PID NO HACE FALTA GRABARLO
  pid1=pd1
Else
  If pid1 <>0 Then ' INDICA QUE UN PID1 ARGUMENTO VINO DE UN BATCH O CALL 
     Print #fa1,pd1 ' GRABA EL PD1 ACTUAL QUE ES LA EJECUCION DEL BATCH
  EndIf 
EndIf 
cerrar fa1

Sleep 100


If desde = 0 And hasta = 0  And instancia=0 Then
 desde => 4  ' -> 3  
 hasta => 8  ' -> 6 le debo restar la octava oculta +1
 
' internamente no usamos cero , empezamos desde 1 pero en roll se ve tal cual es
' va desde 0 a 7 ergo de 4 a 8 el default es la eleccion en roll 3 a 7 pero al 7 es de
' ontrol o sea es de 3 a 6 y la de control no aparece en el grafico...
' l a3 es al 4t octava y la 6 es al 7ma..pues empieza desde 0 en la visualizacion.
' asi coincidimos con lo que se usa en piano la ocatava se numera desde 0.. 
 'pmTk(ntk).desde=desde
 'pmTk(ntk).hasta=hasta
EndIf
' calculo teorico a tiempopatron 160 , pero roollmusic arranca a 120
CantTicks=cantMin * 128 * tempo/4  ' 76800 ticks...o pasos
'CantTicks=76800
CantTicks=1000 ' 3 MINUTOS A NEGRA 160/min=500 Y Q TODAS SEAN FUSA
' 4000*30=120000 x 16=1920000
'  

Dim Shared As paso compas (1 To CantTicks) 'cada item es la posicion en donde

desdevector = desde
hastavector = hasta
estoyEnOctava =desde
estoyEnOctavaOld =desde
' test test el maximo es desde=1...,0 ... hasta=9 ..,115 y el default es 
'           el default   desde=4..,39 ... hasta=8 ...102 
' --------
NB => 0 + (desde-1) * 13   ' 39 , Notapiano=36, nR=39 -coincide no sobra nada
NA => 11 + (hasta-1) * 13  ' 102, Notapiano= 83, nR=89 - no coincide sobra desde
' sobra desde 90 a 102 inclisive o sea 13 posiciones...
' automatiando podemos decier para cualqueir definicion de intervalo de octavas que
' CALCULO DE POSICION DE LA INFORMACION DE ACORDES:
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default ->11+ 6*13 +1=90  ==> 11 + 7*13=102
' PARA EL MAxIMO SERIA
' sobra desde -> [ 11 + (hasta-2)*13+1 ],  hasta -> [11+ (hasta -1)*13]
' en este caso default 9-2 ->11+ 7*13 +1=103  ==> 11 + 8*13=115
' O sea maximo desde 103 a 115 son las posiciones libres...
' vamos a reservar en una posicion dada para la info de acordes por ejemplo en la 
' maxima 103 para octava0,104 octava1,105 octava2, 106 oct3,107 oc4,108 oct5
' 109 oct6, 110 oct 7...ergo quedan libres 111,112,113,114,115  

ReDim (Roll.trk ) (1 To CantTicks,NB To NA) ' Roll de trabajo en Pantalla

' ojo debe se NB al reducir octabas NB cambia
If instru > 0 Then
  Roll.trk(1,NA).inst = CUByte(instru)
  patchsal=instru
EndIf


param.Roll=Roll
param.ubiroll=ubiroll
param.ubirtk=ubirtk


Dim  As Integer  ctres=1 ' 5 octavas por track
Dim As Integer lim1 

lim1=1 ' lim3 vale 25 se reserva el ultimo par avalores de control, no alcanza
' 26-01-2022 la zona de control debe ser 1 octava mas o sea lim3=25
ReDim (Track(00).trk ) (1 To CantTicks,1 To lim3) ' lo usa instancia sin cancion
ReDim (Track(01).trk ) (1 To CantTicks,1 To lim3) ' lo usa sin instancia
ReDim (Track(02).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(03).trk ) (1 To ctres,1 To lim1)
ReDim (Track(04).trk ) (1 To ctres,1 To lim1)
ReDim (Track(05).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(06).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(07).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(08).trk ) (1 To ctres,1 To lim1)
ReDim (Track(09).trk ) (1 To ctres,1 To lim1)
ReDim (Track(10).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(11).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(12).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(13).trk ) (1 To ctres,1 To lim1)
ReDim (Track(14).trk ) (1 To ctres,1 To lim1)
ReDim (Track(15).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(16).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(17).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(18).trk ) (1 To ctres,1 To lim1)
ReDim (Track(19).trk ) (1 To ctres,1 To lim1)
ReDim (Track(20).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(21).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(22).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(23).trk ) (1 To ctres,1 To lim1)
ReDim (Track(24).trk ) (1 To ctres,1 To lim1)
ReDim (Track(25).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(26).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(27).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(28).trk ) (1 To ctres,1 To lim1)
ReDim (Track(29).trk ) (1 To ctres,1 To lim1)
ReDim (Track(30).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(31).trk ) (1 To Ctres,1 To lim1)
ReDim (Track(32).trk ) (1 To Ctres,1 To lim1)

' c/u de estos track es redimensionable preserve en la 1era dimension 
' o sea en las posiciones, lo que debo hacer es cargar estos Tracks
' con eventos pero para mostrarlso usaria el Roll 
' suponemos que cada track solo puede tener acordes de hasta 12 notas,1 to lim2
' en el momento de la carga de Roll cargar tambien Track �?
' creo que no solo debo copiar en el momento antes de grabar o reproducir
' debere hAcer otro play PlayTracks en donde se ejecutara todos los tracks
' barriendo la posicion comun a todos y las notas de cada uno EN SINCRONIA CON LA VISUALIZACION!
' ENTONCES EN PLAYALL RECORRO TRACKS PERO NO EL DE VISUALIZACION,,,,,VEREMOS.,

ReDim (RollAux.trk) ( 1 To CantTicks,NB To NA )
CantCompas = 40 * CantMin
' 600 COMPASES DE 32 POSICIONES 14,74 MBYTES , 235 MBYTES 16 TRACKS
' si  I=160 -> O=40 , 40 compases/min en 15 min=600 compases
' 600 * 128ticks maximo de H = 76800 ticks
' 5 min seria / 3 = 200 compases, aprox 5Mbytes *16 = 80 mbytes 16 trcks
' maso menso funciona pero debo empezar desde cero de vuelta....


'''Dim Shared As ubyte Insercion (1 To 128, 1 To 12000)
' si inserto en un track o canal debo desplazar a todos!!!
'  de modoque tener una insercion para c7useracrisimo...
' solo resta usar copia y Redim preserve

' indice notas, fin compas, nota con puntillo o ligadura,
' se almcena en el heap porque es shared
' 12k posiciones de 96 notas o 128 con help y espacios ...8.388.608 bytes 8 megas
/'
Dim l As Integer
For l = 1 To 65
Next l
''Close aca estaba habilitado el close humm ah pero esta comentado

End
'/
''https://www.freebasic.net/forum/viewtopic.php?t=15127
' ------------ play de usuario - datos por midiin -------------------
' 16 CANALES DE ENTRADA, PAR AL REPRODUCION O ARMADO
' SUMAREMOS SIEMRE ENTRE AMBAS FORMAS NO AMS DE 32 PORQUE AL REPRODUCIR
' SE SUMAN ESTOS 16 , OSEA OUTPUT+INPUT <=32 SINO EN LA REPRODUCCION
' TENDRIASMO UN MAZIMO DE 32+16=48...Y LA POLIFONIA DE RTMIDI
   
ReDim (Toca(01).trk ) (1 To Ctres) 
ReDim (Toca(02).trk ) (1 To Ctres) 
ReDim (Toca(03).trk ) (1 To Ctres) 
ReDim (Toca(04).trk ) (1 To Ctres) 
ReDim (Toca(05).trk ) (1 To Ctres) 
ReDim (Toca(06).trk ) (1 To Ctres) 
ReDim (Toca(07).trk ) (1 To Ctres) 
ReDim (Toca(08).trk ) (1 To Ctres) 
ReDim (Toca(09).trk ) (1 To Ctres) 
ReDim (Toca(10).trk ) (1 To Ctres) 
ReDim (Toca(11).trk ) (1 To Ctres) 
ReDim (Toca(12).trk ) (1 To Ctres) 
ReDim (Toca(13).trk ) (1 To Ctres) 
ReDim (Toca(14).trk ) (1 To Ctres) 
ReDim (Toca(15).trk ) (1 To Ctres) 
ReDim (Toca(16).trk ) (1 To Ctres) 
ReDim (Toca(17).trk ) (1 To Ctres) 
ReDim (Toca(18).trk ) (1 To Ctres) 
ReDim (Toca(19).trk ) (1 To Ctres) 
ReDim (Toca(20).trk ) (1 To Ctres) 
ReDim (Toca(21).trk ) (1 To Ctres) 
ReDim (Toca(22).trk ) (1 To Ctres) 
ReDim (Toca(23).trk ) (1 To Ctres) 
ReDim (Toca(24).trk ) (1 To Ctres) 
ReDim (Toca(25).trk ) (1 To Ctres) 
ReDim (Toca(26).trk ) (1 To Ctres) 
ReDim (Toca(27).trk ) (1 To Ctres) 
ReDim (Toca(28).trk ) (1 To Ctres) 
ReDim (Toca(28).trk ) (1 To Ctres) 
ReDim (Toca(30).trk ) (1 To Ctres) 
ReDim (Toca(31).trk ) (1 To Ctres) 
ReDim (Toca(32).trk ) (1 To Ctres) 

'1) tomar de midin los eventos
'2) si como vienen imprimirlos para ir viendo que sale
'3) luego de verificados los eventos, guardarloe es el vector Toca
'   colocandole el timestamp con una presicion mayo ra 15 mseg,
' consideramos I=250  por minuto (240ms) => L=500 (120ms),=>F=1000(60ms)
' E=2000 (30ms), H=4000 (15ms).
' PENDIENTE DE ANALISIS SI HAY PROBLEMAS:usar rtmidi_in_create cdecl alias "rtmidi_in_create" (byval api as RtMidiApi, byval clientName as zstring ptr, byval queueSizeLimit as uinteger) as RtMidiInPtr
' para modificar el limite de la cola , default 1024 odriamos pasarlo a mas
' 4096 o 8192 valro tipico ,,,de un buffer

'--------------------------
Dim As String driver

posmouseOld = 0:posmouse = 0
COMEDIT = False:resize = False
po = @octaroll
*po = hasta -1 ' test 09-09-2021 
s1=0:s2=0:s3=0:s4=0:s5=2:s6=0:s7=0:s8=0
If font=0 Then 
 font=18
EndIf
indaux=0:carga=0
' -----------------------------------------------------------------------
' notas redonda (O),blanca(P),negra(I),corchea(C),semicorchea(S), Fusa(F),Semifusa(E)
' O P I L F E W H 
' puntillo O* P*  C* elsigno  *
' -puntillo resta en vez de sumar -O -P -C ...-U
' -------------------------------------------------------------------------
BordeSupRoll = Int((ALTO ) /18) ' (1400 )/18 integer = 77
inc_Penta = Int((ALTO - BordeSupRoll) /(40)) ' 26 double 1330/66 para 1400 resolu

' *******************************************************++
BordeSupRoll = BordeSupRoll -  66* inc_Penta ' de inicio muestro octava 4 la central C3
' *************************************************+
' inc_Penta=separacion de lineas
'---------------------
gap1= anchofig* 2315/1000 ' 81 default
gap2= (914 * gap1) /1000 ' 74 default
gap3= (519 * gap1) /1000 ' 42 default

'---------
 
Dim As String sfont,smxold,smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig,sVerCifradoAcordes
Dim openfalla As integer
ffini=3
 If  Open ("./RollMusic.ini" For Input As #ffini) <> 0 Then
 ' si no existe la creo
     Open "./RollMusic.ini" For Append As #ffini
      
 End If

Line Input #ffini, sfont
Line Input #ffini, smxold
Line Input #ffini, smyold
Line Input #ffini, sancho
Line Input #ffini, salto
Line Input #ffini, sdeltaip
Line Input #ffini, sVerEscalasAuxiliares
Line Input #ffini, sanchofig
Line Input #ffini, sVerCifradoAcordes


cerrar ffini
Sleep 100


nfont=ValInt(sfont)
nmxold=ValInt(smxold)
nmyold=ValInt(smyold)
nancho=ValInt(sancho)
nalto=ValInt(salto)
ndeltaip=ValInt(sdeltaip)
nVerEscalasAuxiliares=ValInt(sVerEscalasAuxiliares)
nanchofig =ValInt(sanchofig)
nVerCifradoAcordes=ValInt(sVerCifradoAcordes)


If nfont > 0 Then
  font=nfont
EndIf
If nmxold <> 0 Then
   mxold=nmxold
EndIf
If nmyold <> 0 Then
   myold=nmyold
EndIf

If ndeltaip <> 0 Then
   inc_Penta=ndeltaip
EndIf
If nanchofig <> 0 Then
   anchofig=nanchofig
   gap1= anchofig* 2315/1000
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) - 4
   ANCHO3div4 = ANCHO *3 / 4 
EndIf

'---------
If mxold=0 And myold=0 Then
GetMouse mxold,myold, , MouseButtons
EndIf

posicion = 1 ' comienzo del roll
'indice   = 1  ' numero de nota al comienzo del programa B8
espacio = 0
backspace = 0
fijarEspacio=0

'--FFT FREE FONT-
Var Shared ft => FreeType()
'' Load a font with FreeType
Common Shared As FT_Face ftface
FT_New_Face( ft, "Bebaskai.otf", 0, @ftface )
common shared as any ptr BRUSH
' ========== CONTROL DEL NRO DE OCTAVAS MOSTRADO SE PODRA PONER PARA EL USUARIO
' VER SI SE PUEDE USAR ARRAYS POR PORCIONES
'----- -FIN
'----- MIDI MICROSOFT
'https://docs.microsoft.com/en-us/windows/win32/multimedia/midi-functions
'DIM CAN As UINT
'CAN= midiOutGetNumDevs()
 
'-----
' ancho de figura,separaciondelasmismas en pantalla anchofig
'' ---------------  LOOP 1 ---------------
 On Error Goto errorhandler
' enviamos esto a una sub ROLLLoop, creaPenta esta al principio y no tiene declare
' el declare falla si se usa con este tipo de vector udt no se puede usar declare
'stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


'Dim Shared nombreport As ZString Ptr
'Dim Shared midiout (0 To 3) As  RtMidiOutPtr ' abrir hasta 32 dispositivos
'Dim Shared midiin As RtMidiInPtr
'Dim Shared As Integer porterror
Dim Shared nombreport As ZString Ptr

midiin(0)     = rtmidi_in_create_default()  ''' new RtMidiIn();
midiout(0) = rtmidi_out_create_default() ''  new RtMidiOut();


portsout =  port_count (midiout(0)) ' es una constante
portsin  =  port_count(midiin(0)) ' es una constante
Dim i1 As Integer

ReDim  listOutAbierto (0 To portsout)
ReDim  listInAbierto  (0 To portsin)
ReDim  listOutCreado (0 To portsout)
ReDim  listInCreado  (0 To portsin)

' cada midiout o midiin default creado puede ver contar todos los ports disponibles de in o out
' y ver sus nombres,
' pero solo pueden abrir un port ocerrarlo. Ergo si quiereo abrir masde1 port
' defino un vector de midiout u otro de midin,asi puedo abrir masdeun port de out o in
' estos vectores controlan si estanabiertos o cerrdos los ports de in o out.    
listOutAbierto(0)=0
listInAbierto(0)=0
listOutCreado(0) =1 
listInCreado(0)  =1 

Dim Shared nombreOut(0 To portsout) As ZString Ptr
Dim Shared nombreIn (0 To portsin)  As ZString Ptr

Type plano 
  sumatiempo As Integer  'tiempo acumulado de los eventos midis
  canal      As UByte 
  estado     As UByte  ' nota on of 
  nota       As UByte  ' notapiano 
  vel        As UByte  ' velocidad
End Type

ReDim Shared As plano miditxt()
Dim Shared As Integer Indicenotas=0
'Dim i1 As integer

'For i= 1 To portsin - 1
' midiin = rtmidi_in_create_default ( )
'Next i

For i1 = 0 To portsin -1 
    nombrein(i1) = port_name(midiin(0), i1)
Next i1  
For i1 = 0 To portsout -1 
    nombreOut(i1) = port_name(midiout(0), i1)
Next i1  

'---------------------
