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

' FILE DIALOG adicionales
'Sub  abrirSecuencia(nf As integer) 

'  If  Open ("./secuenciaPLAY.txt" For Output Shared As nf ) <> 0 Then
'    		Print #1, "secuenciaPLAY.txt no abre!"
'  Else
'      Print #1, "secuenciaPLAY.txt abierta!"
'  EndIf
'End  sub 

'--------
'--------
Sub  porterrorsub(porterror As integer)
          Select Case porterror
            Case RTMIDI_ERROR_WARNING
              Print #1, "RTMIDI_ERROR_WARNING, sin importancia"
              'strerror(n as integer) as zstring ptr
              'Print #1, "sterror"; *strerror (porterror)

            Case RTMIDI_ERROR_DEBUG_WARNING
              Print #1, "RTMIDI_ERROR_DEBUG_WARNING"
              Close 0             
              End
      
            Case RTMIDI_ERROR_UNSPECIFIED
              Print #1,"RTMIDI_ERROR_UNSPECIFIED"
              Close  0
              End
      
            Case RTMIDI_ERROR_NO_DEVICES_FOUND
              Print #1,"RTMIDI_ERROR_NO_DEVICES_FOUND"
              Close 0
              End
      
            Case RTMIDI_ERROR_INVALID_DEVICE
              Print #1,"RTMIDI_ERROR_INVALID_DEVICE"
              Close 0
              End
      
            Case RTMIDI_ERROR_MEMORY_ERROR
              Print #1,"RTMIDI_ERROR_MEMORY_ERROR"
              Close  0
              End
      
            Case RTMIDI_ERROR_INVALID_PARAMETER
              Print #1,"RTMIDI_ERROR_INVALID_PARAMETER"
              Close 0
              End
      
            Case RTMIDI_ERROR_INVALID_USE
              Print #1,"RTMIDI_ERROR_INVALID_USE"
              Close  0
              End
      
            Case RTMIDI_ERROR_DRIVER_ERROR
              Print #1,"RTMIDI_ERROR_DRIVER_ERROR!"
              Close 0
              End
      
            Case RTMIDI_ERROR_SYSTEM_ERROR
              Print #1,"RTMIDI_ERROR_SYSTEM_ERROR"
              Close 0
              End
      
            Case RTMIDI_ERROR_THREAD_ERROR
              Print #1,"RTMIDI_ERROR_THREAD_ERROR"
              Close 0
              End
          End Select


End Sub

'------------------

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

pd1 = GetCurrentProcessId()  

Open "midebug.txt" For Output As #1
'' Open "midebug"+ "["+Str(pd1)+"]" + ".txt" For Output As 1
Print #1,"start"
Print #1,"PID DE ESTE PROCESO ",pd1



'Open "mivector.txt" For Output As #3
'Open "miplayall.txt" For Output As #4
'Open "test-AAAAA.TXT" For Output As #5
'print #1, "version para ceros!!!!!! "
'Dim fcon As Integer 
'fcon=freefile
'Open cons  for Output As #8

''Open "figuras.txt" For Output As #1
Print #1,Date;Time
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
''Print #1,"ANTES ROLLDEC "
#include "ROLLDEC.BI"
''Print #1,"DESPUES ROLLDEC "
'------------------

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
tempo=121  ' negra=120
CantMin=15
'NotaBaja=1 : NotaAlta=128

Print #1, "__FB_ARGV__ ",__FB_ARGV__
Print #1, "__FB_ARGC__ ",__FB_ARGC__
'Dim direp As ZString  Ptr
'Dim dires As String

Print #1,"__FB_ARGC__ ", __FB_ARGC__
Dim As Integer com_usarmarco =0
For ix = 0 To __FB_ARGC__
  Print #1, "arg "; ix; " = '"; Command(ix); "'"''

 If ix=1 And Command(ix) > "" Then ' deberia entregarme el archjivo el SO pero no lo hace
  
 ubirtk = InStr (LCase(Command(ix)),".rtk")
 ubiroll=  InStr(LCase(Command(ix)),".roll")
' esto es por comando interno no fisico con click
''' ubicancion=InStr(LCase(Command(ix)),"@dir")

 If ubirtk > 0 or ubiroll>0  Then
   ntk=0 
   titulosTk(0)=Command(ix)
   Instancia=ARG1_1_TITULO ' no se condice con el caso real da 2 ???
 Else
    desde= CInt(Command(ix))
'    pmTk(ntk).desde=desde
   Instancia=ARG1_2_DESDE     
 EndIf
 Print #1,"ubirtk ",ubirtk
 Print #1,"ubiroll ",ubiroll
    'sigue en roolloop principio
 EndIf
 If ix=2 And Command(ix) > "" Then
  hasta= CInt (Command(ix))
 ' pmTk(ntk).hasta=hasta
    Instancia=ARG2_HASTA 
 EndIf
 
 If ix=3 And Command(ix) > "" Then
  titu=  (Command(ix))
     Instancia=ARG3_TITU 
 EndIf

 If ix=4 And Command(ix) > "" Then
  instru=  CUByte (Command(ix))
     Instancia=ARG4_INSTRU 
 EndIf

 If ix=5 And Command(ix) > "" Then
  pid1=  CInt (Command(ix))
     Instancia=ARG5_PID1
 EndIf

 If ix=6  And Command(ix) > "" Then
  com_usarmarco=  CInt (Command(ix))
     Instancia=ARG6_USARMARCO
 EndIf
' en 7 diria @dir en la linea de comando  
' puedo poner basura en todos los otros
 If ix=7 And Command(ix) > "" Then ' ok probado pasa bien el NombreCancion con el path
  NombreCancion =  Command(ix)
     Instancia=ARG7_NOMBRECANCION
 EndIf

Next ix

' uso para volcar midi a text odesde un roll o trk usamos cancion
' para pasar solo una pista 
If NombreCancion > "" Then 
 ubirtk = InStr (LCase(Command(7)),".rtk")
 ubiroll=  InStr(LCase(Command(7)),".roll")

  If ubirtk > 0 or ubiroll>0  Then
     ntk=0 
     titulosTk(0)=Command(1) ' DECIA IX  ERROR? 15-04-2025
     Instancia=ARG1_1_TITULO ' PUEDE SER desde TAMBIEN
  EndIf
    Print #1,"ubirtk ",ubirtk
    Print #1,"ubiroll ",ubiroll

EndIf

Print #1, "instancia, ix  ", instancia, ix 
''SI DESDE CTRL TRAEMOS UN GRAFICO MODIFICACION_INSERCION ->' Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))

'Dim Shared As Integer pd1, fa1 

'pd1 = GetCurrentProcessId()  
'Open "midebug" + "["+Str(pd1)+"]" + ".txt" For Output As #1

''Open "midebug.txt" For Output As #1
'Print #1,"start"
'Print #1,"PID DE ESTE PROCESO ",pd1
fa1=2
Open "procesos.txt" For Append As fa1
If pid1=0   Then ' EMPEZO EL ONLINE SU PID NO HACE FALTA GRABARLO
  pid1=pd1
Else
  If pid1 <>0 Then ' INDICA QUE UN PID1 ARGUMENTO VINO DE UN BATCH O CALL 
     Print #fa1,pd1 ' GRABA EL PD1 ACTUAL QUE ES LA EJECUCION DEL BATCH
  EndIf 
EndIf 
Close fa1

Sleep 100


If desde = 0 And hasta = 0  And instancia=ARG0_EN_LINEA Then
 Print #1,"intervalo no dado usando default!"
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
' calculo teorico a tiempopatron 60, 1 SEG 192 DIVISIONES 
CantTicks=cantMin * 60 * 96 '  (15 MIN * 60 * 96) = 86400
' 60 seg * 96 divisiones= 5760 divisiones en 1 min
' Y 3W LA MENOR FIGURA 3W DE LA 5ta LINEA de tresillos
' iniciamos vector de 15 minutos=660 segundos
' como la division mas chica TOMAREMOS la figura tresillo de W,3W de 0,01041666 segundos
' PARA TEMPO 60 entonces tomaremos el tiempo mas chico en 10.41666 mseg..para negra =1 seg
' tiempo 60 seg.
' 1 SEG DE TRACK= 1000/10,41666 = 96 posiciones de 3W,
' 1min=86400 posiciones 
' LA CANTIDAD DE TICKS DEBE SER CONSTANTE LUEGO SE TOCARA MAS RAPIDO O NO
' AJUSTANDO TEMPO Y PAR ESO SE LEERA MAS RAPIDO EL TRACK ..
' O SEA LA CANTIDAD DE TICKS DEBE SER LA MISMA PARA TODO TEMPO
' E IGUA A LA DE NEGRA = 96 posiciones
' CUANDO EL TEMPO SEA 240 ESTAREMOS TOCANDO EL TRACK A MAS VELOCIDAD Y LA 
' DIVISION MAS CHICA SERA 1/4 DE LA DE 60 O SEA 0.01041666/ 4= 2.6041 mseg
' ESTARIAMOS HABLANDO DE 2.6 MILI SEGUNDoS LA FIGURA MAS CHICA,,,TFMC
' LUEGO CUANDO PREGUNTE POR LA FIGURA MAS CHICA SERA TFMC*60/TEMPO !!!
' ,,para 60 10,41666 mseg, para 120 5,20833 mseg
' para 240 2,6041 mseg...pero la cantidad de Ticks es fija siempre  igual
' para un determinado tiempo de track..
' como tomamos tempo=120 la TMFC sera 5,20833 mseg,
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
Print #1,"NB, NA",NB,NA 
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

'Print #1,"instru ",instru
' ojo debe se NB al reducir octabas NB cambia
If instru > 0 Then
  Roll.trk(1,NA).inst = CUByte(instru)
  patchsal=instru
EndIf
'Print #1,"Roll.trk(1,NA).inst ",Roll.trk(1,NA).inst
'Print #1,"NB ",NB
'Print #1,"NA ",NA

'Print #1,"desde ",desde
'Print #1,"hasta ",hasta

param.Roll=Roll
param.ubiroll=ubiroll
param.ubirtk=ubirtk


Dim  As Integer  ctres=1 ' 5 octavas por track
Dim As Integer lim1 

' en Rolldec ReDim Shared Track  (0 To 32) As sec ' tracks para guardar,.. y tocar 

lim1=1 ' lim3 vale 25 se reserva el ultimo para valores de control, no alcanza
''' lim2=12,lim3=25
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
print #1, l;" ";figura(l)
Next l
''Close aca estaba habilitado el close humm ah pero esta comentado

End
'/
''https://www.freebasic.net/forum/viewtopic.php?t=15127
'print #1,"NroCol, ancho, anchofig ",NroCol, ANCHO, anchofig
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
COMEDIT=LECTURA:resize = False
po = @octaroll
*po = hasta -1 ' test 09-09-2021 
s1=0:s2=0:s3=0:s4=0:s5=2:s6=0:s7=0:s8=0:s9=0
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
'If font >=5 And font <= 34 Then
'   anchofig= mispx(font-4,2)
'Else
'   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
'EndIf

 
   gap1= anchofig* 6 ''2315/1000
   NroCol =  (ANCHO / anchofig ) + 4
'   gap2= (914 * gap1) /1000 ' 74 default
'   gap3= (519 * gap1) /1000 ' 42 default

'''gap1= anchofig* 20 ' 81 default
gap2= (914 * gap1) /1000 ' 74 default
gap3= (519 * gap1) /1000 ' 42 default

print #1,"gap1 ",gap1
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

'Print #1,"sfont, smxold, smyold,sANCHO,sALTO..  ",sfont, smxold, smyold,sancho,salto,sdeltaip,sVerEscalasAuxiliares,sanchofig

CLOSE ffini

Sleep 100


nfont=ValInt(sfont)
nmxold=ValInt(smxold)
nmyold=ValInt(smyold)
nancho=ValInt(sancho)
nalto=ValInt(salto)
ndeltaip=ValInt(sdeltaip)
nVerEscalasAuxiliares=ValInt(sVerEscalasAuxiliares)
nanchofig =CSng(sanchofig)
nVerCifradoAcordes=ValInt(sVerCifradoAcordes)


Print #1,"nanchofig " ,nanchofig
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

'If font >=5 And font <= 34 Then
'   anchofig= mispx(font-4,2)
'Else
'   anchofig =(ANCHO- gap1 )/ (MaxPos-posishow)
'EndIf

   
'   gap1= anchofig* 6 ''2315/1000
'   NroCol =  (ANCHO / anchofig ) + 6
'   gap2= (914 * gap1) /1000 ' 74 default
'   gap3= (519 * gap1) /1000 ' 42 default


   gap1= anchofig*6  ''' porque tanto??
   gap2= (914 * gap1) /1000 ' 74 default
   gap3= (519 * gap1) /1000 ' 42 default
   NroCol =  (ANCHO / anchofig ) + 4
   ANCHO3div4 = ANCHO * 3/4 
EndIf
'Print #1,"NROCOL AL INICIO, ANCHO, anchofig ",NroCol, ANCHO, anchofig

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
'print #1, "MIDI NUM DEVS ";CAN
 
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


'print #1,"PLAYALL---------->>>>>>>"
portsout =  port_count (midiout(0)) ' es una constante
portsin  =  port_count(midiin(0)) ' es una constante
Dim i1 As Integer
Print #1, "portsin  "; portsin
Print #1, "portsout ";portsout

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
' Print #1,"creando default ",i
'Next i

For i1 = 0 To portsin -1 
    nombrein(i1) = port_name(midiin(0), i1)
    Print #1, *nombrein(i1)
Next i1  
Print #1,"-----------------------------"
For i1 = 0 To portsout -1 
    nombreOut(i1) = port_name(midiout(0), i1)
    Print #1, *nombreout(i1)
Next i1  

'---------------------
' inputbox de window9 no usa el return para la salida o fin del box modifique el input box
' y tengo mi InputBoxJmg

Type InputBoxJmg_ 'basado en InputBox de windows9, para que detecte CR 13
	As MSG msg
	As HWND hWnd,hwnd1,hwnd2,hwnd3
	#ifdef UNICODE
		As WString*1024 mess
	#else	
		As String*1024 mess
	#EndIf 
	As BOOL flag
	as DEVMODE dm(0)
	As HFONT font,font1
	As Integer size
End Type

Function InputBoxJmg(ByRef Caption As USTRING, ByRef Message As USTRING, ByRef DefaultString As USTRING, ByVal flag As Integer, ByVal flag2 As Integer, hParentWin as Hwnd = 0) As USTRING
' Autor:JMG modificacion windows9 inputBox ...experimental si anda bien al vez de incropore
' a windows9 y avisamos si quieren usarlo  
	Dim InputBoxJmg_ As InputBoxJmg_
	InputBoxJmg_.dm(0).dmSize = sizeof(DEVMODE)
	EnumDisplaySettings( 0, ENUM_CURRENT_SETTINGS, @InputBoxJmg_.dm(0))
	#ifdef UNICODE
		InputBoxJmg_.hWnd  = CreateWindowEx(0, "#32770", *Caption, WS_TILED Or WS_VISIBLE, InputBox_.dm(0).dmPelsWidth/2-155, InputBox_.dm(0).dmPelsHeight/2-70, 310, 130, 0, 0, 0, 0 )
		InputBoxJmg_.hWnd1 = CreateWindowEx(WS_EX_CLIENTEDGE, "Edit", *DefaultString, WS_CHILD Or WS_VISIBLE Or flag, 10, 33, 275, 50, InputBox_.hwnd,0,0,0)
		InputBoxJmg_.hWnd2 = CreateWindowEx(0, "Button", "OK", WS_CHILD Or WS_VISIBLE, 106, 65, 80, 25, InputBox_.hwnd,0,0,0)
		InputBoxJmg_.hWnd3 = CreateWindowEx(0, "Static", *Message, WS_CHILD Or WS_VISIBLE, 10, 10, 275, 20, InputBox_.hwnd,0,0,0)  	
	#else	
		InputBoxJmg_.hWnd  = CreateWindowEx(0, "#32770", Caption, WS_TILED Or WS_VISIBLE, InputBoxJmg_.dm(0).dmPelsWidth/2-155, InputBoxJmg_.dm(0).dmPelsHeight/2-70, 310, 130, 0, 0, 0, 0 )
		InputBoxJmg_.hWnd1 = CreateWindowEx(WS_EX_CLIENTEDGE, "Edit", DefaultString, WS_CHILD Or WS_VISIBLE Or flag, 10, 33, 275, 50, InputBoxJmg_.hwnd,0,0,0)
		InputBoxJmg_.hWnd2 = CreateWindowEx(0, "Button", "OK", WS_CHILD Or WS_VISIBLE, 106, 65, 80, 25, InputBoxJmg_.hwnd,0,0,0)
		InputBoxJmg_.hWnd3 = CreateWindowEx(0, "Static", Message, WS_CHILD Or WS_VISIBLE, 10, 10, 275, 20, InputBoxJmg_.hwnd,0,0,0)  	
	#EndIf
	
	InputBoxJmg_.size  = -MulDiv(10, GetDeviceCaps(CreateDC("DISPLAY",0,0,0), LOGPIXELSY), 72)
	InputBoxJmg_.font  = CreateFont(InputBoxJmg_.size,0,0,0,0,1,0,0,DEFAULT_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH Or FF_DONTCARE,"Times New Roman")
	SendMessage(InputBoxJmg_.hWnd3,WM_SETFONT,Cast(WPARAM,InputBoxJmg_.font),0)
	InputBoxJmg_.font1 = CreateFont(InputBoxJmg_.size,0,0,0,0,0,0,0,DEFAULT_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH Or FF_DONTCARE,"Times New Roman")
	SendMessage(InputBoxJmg_.hWnd2,WM_SETFONT,Cast(WPARAM,InputBoxJmg_.font1),0)
	SendMessage(InputBoxJmg_.hWnd1,WM_SETFONT,Cast(WPARAM,InputBoxJmg_.font1),0)
	
	While GetMessage(@InputBoxJmg_.msg, 0, 0, 0 )
		TranslateMessage(@InputBoxJmg_.msg )
		DispatchMessage(@InputBoxJmg_.msg )
' el windows pone el 13 CR al principio de la cadena sin que se lo pidan,por eso esta en posicion 1
' y al final una pelotudes en fin, claro para multiline va pero no para una linea pero
' solo asi funciona el CR 13 usando multiline, hay que dar dos return
  SetFocus (InputBoxJmg_.hWnd1)
		Select Case InputBoxJmg_.msg.hwnd
			Case InputBoxJmg_.hWnd1 ' CAJA ENTRADA CLIENTE
				Select Case InputBoxJmg_.msg.message
					Case WM_KEYDOWN
                   SendMessage(InputBoxJmg_.hWnd1,WM_GETTEXT,1024,Cast(LPARAM ,@InputBoxJmg_.mess))
                
				       Dim as USTRING sRet = InputBoxJmg_.mess
 				       Function = sRet
                
                   Dim As String * 1 F1,F2
                   Dim As Integer LL=Len(sRET)
                   F1=Mid (sRET,1) 'el primero
                   F2=Mid (sRET,LL-1) ' el ultimo ascii

                   If Asc(F1) =13 Or Asc(F2) =13 Then
 		   		       DestroyWindow(InputBoxJmg_.hWnd)
					       InputBoxJmg_.flag=0
					       Exit Function
                  EndIf
           End Select
        Case InputBoxJmg_.hWnd2 ' boton ok
          Select Case InputBoxJmg_.msg.message
              Case WM_LBUTTONDOWN
  						SendMessage(InputBoxJmg_.hWnd1,WM_GETTEXT,1024,Cast(LPARAM ,@InputBoxJmg_.mess))
						dim as USTRING sRet = InputBoxJmg_.mess
						Function = sRet
						DestroyWindow(InputBoxJmg_.hWnd)'
						InputBoxJmg_.flag=0
						Exit Function
          End Select
      End Select 
	Wend
End Function

