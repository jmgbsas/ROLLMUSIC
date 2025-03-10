#Include "ROLLINICIO.bi"
#include "WinGUI.bi"
#include "RTMIDISUB.bi"
#Include "ROLLTRACKS.bi"
#include "ROLLSUB.bi"
'' DEV02
'' PROBAR FileListBoxItem !!! 15-12-2024
'===========================
 Dim As Integer MenuFlag=0, LoopFlag=0 
'========================== 
#include "ROllLoop.bi"
'==========================
' comparar con F:\IT64\AREAWORK\ROLLMUSIC-138-INPUT-OK\ que si funciona graba ejecuciones
'----------------


''https://www.freebasic.net/forum/viewtopic.php?f=3&t=22821&p=204270&hilit=loop+through+an+array+with+the+pointer#p204270

'Dim As Integer  tilde1102=MF_UNCHECKED,tilde1103=MF_UNCHECKED  
'Dim As Integer anchoK, altoK
'anchoK = GetSystemMetrics(SM_CXSCREEN)
'altoK = GetSystemMetrics(SM_CYSCREEN)
'-------------
/'
Dim As Integer g1, h1,h2
Dim As  byte Ptr p5,p6

Dim hnro As Integer
' prueba desarrolllo escalas 
 For g1 =1 To 47
    hnro=escala(g1).nropasos 
    p5= escala(g1).pasos 
    For h1 = 1 To hnro -1
     
     p5=p5+1 
    Next h1
' CONSTRUCCION DE LA ESCALA
 Dim As Integer k3=1
   p6= escala(g1).pasos
    For h2 = 1 To hnro -1
     
     k3= *p6 + k3
     
     p6=p6+1 
    Next h2

 Next g1
 '/
 ' ahora debo traducir las notas a notas de Roll e incorporarlas al vector
 ' las notas  en Roll van de 0 a 11 para el indice son los semitonos, pero se cargan 
 ' los valores 1 a 12. En Roll C=12 ...B=1 ergo si la escala dice 12=B debe ser 1
 ' o sea =12-valor+1 (12-1+1) o podemos crear el vector de escalas al reves de 12 a 1 pero
 ' se complicaria todo para analizar. ergo invertimos y listo 1 a 12 es 12 a 1...
 ' Entonces para taducir a Roll hago 12-valor+1 . pero par aanalizar y mostrar es valor
 ' ejemplo triada=1,3,5=C,E,G, en Roll seria = 12,10,8
 
 ' puedo recorrer un array con un pointer!!!!
'------- muchas fallas y algorimos complejos,,ultima version se abandona
nroversion="2025-01-14 0.4900 Ultima Version sin Ticks" 'melodia de ejcucion y acorde se ve en Roll
' playAll no lo toca bien los acordes porque nos son de igual duracion
' pero la representacion grafica andaria ,??,bien?    
' hemos  convertido las ejecuciones melodicas simples en archivos trk y roll.
' agregamos los silencios!! veremos un desafio
acercade = "RollMusic Version "+ nroVersion +" Jose M Galeano, Buenos Aires Argentina 2021-2022, 2024. Ejecuta secuencias " + _
 "entrada por pasos usando algoritmos sin una linea conductora de tiempos, se basa en las duraciones de las notas. " + _
 "Para entrada por teclado midi usa ticks. Los algoritmos pueden fallar en condiciones no estudiadas o no detectadas durante la entrada de datos " + _
 "manual o por ejecucion. OS:Windows 64bits, " + _
 "Usa Cairo como libreria de graficos, Windows9 ,WinGUI y Gtk como GUI; Rtmidi como libreria midi, " + _
 "Editor de codigo FbEdit. Echo en Freebasic como hobby. FreeBASIC Compiler - Version 1.09.0 (2021-12-31), built for win64 (64bit) " + _
" Copyright (C) 2004-2021 The FreeBASIC development team." +_ 
" Consultas: mail:galeanoj2005@gmail.com. (Na, No aplicable, no implementado todavia)."+_
" ABANDONo ESTA LINEA DE DESARROLLO. AGREGARE LINEA DE TIEMPO Y TICKS. RollMusic Ticks 0.5000."

 
'------------///// GUI GUI GUI  GUI ///////////////

#include Once "ROLLCTRLGUI.BI"


' SE HABILITAN SI USO 2.0 EN SU CASE

'AddKeyboardShortcut(hwndC,FCONTROL,VK_A,1006) 'CTRL+A ABRIR PISTAS

' opengl funciona bien, en futuro usare opnegl para otro roll grafico adicional
' o cambiar el actual. Tal vez para mostrar un roll comun de barras horizontales
' al grabar midi desde teclado (muchos meses adelante ja, 
' ideas sobran tiempo no hay y organizar el codigo de a poco optimizarlo
'usar listas enlazadas ? podria ser, faltaria ahcer un prototipo a ver si es
'rapido o no ventajas y desventajas o hacer un mix com arrays)
/'
sub FramebuffersizefunCB GLFWCALLBACK (win As GLFWwindow ptr, w as long, h as long)
  'print "FramebuffersizefunCB " & w & " x " & h
  glViewport(0,0,w,h)
end sub

Sub correwin( vwin As GLFWwindow Ptr, ta As Any Ptr)
glfwMakeContextCurrent(vwin) 
glfwSetFramebufferSizeCallback(vwin,@FramebuffersizefunCB)

While glfwWindowShouldClose(vwin)=GL_FALSE andalso glfwGetKey(vwin,GLFW_KEY_ESCAPE)=GL_FALSE
glfwMakeContextCurrent(vwin)

' press [ESC] or close the window with the mouse or press [ALT] + [F4]
'while glfwWindowShouldClose(win2)=GL_FALSE andalso glfwGetKey(win,GLFW_KEY_ESCAPE)=GL_FALSE
  dim as double t=glfwGetTime()
  glClearColor(.5+cos(t)*.5, 0, 0,1)
  glClear(GL_COLOR_BUFFER_BIT)
  glfwSwapBuffers(vwin)
  glfwPollEvents() ' handle OS events (window,keyboard,joystick,mouse ...)
  sleep 20
Wend
    Close
    End 0


End Sub
'/
'---------------------------- main -----------------
#define EventKeyDown &h100
' Dim Shared As Any Ptr mutex no se usa todavia exoermental de sync
' Dim Shared As Any Ptr cond
' Dim Shared As String txt
' Dim As Any Ptr pt
' Dim Shared As Integer ok = 0
Dim  As Integer  gi = 0

If mxold > 0 Then

'MoveWindow( hWnd , 1, 1 , ANCHO - mxold, ALTO - myold, TRUE )
  If ANCHO = nancho Then
      ANCHO= nancho -mxold 
  EndIf

  AnchoInicial=ANCHO
  If anchofig=0 Then
    anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
  EndIf
  NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tama o figuras, nota guia 6 columnas "B_8_[ "
  ANCHO3div4 = ANCHO *3 / 4
  gap1= anchofig* 2315/1000 ' 81 default
  gap2= (914 * gap1) /1000 ' 74 default
  gap3= (519 * gap1) /1000 ' 42 default
  mxold=0
EndIf
If myold > 0 Then
  If ALTO = nalto Then
    ALTO= nalto -myold 
  EndIf

 AltoInicial=ALTO
 myold=0
EndIf
stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, ANCHO)


    param.ancho = ANCHO 
    param.alto = ALTO
'''mxold=0:myold=0

abrirRoll=0
'pistacreada=0
Dim As Integer k=0, salida=0

' //// DESHABILITAR LOS CLICK EN LISTA SI NO HAY CARGADO NADA
If instancia =0  Then  ' cuando es online y recien levanta 
'DisableGadget(PISTASROLL,1) 25-10-2024 SWE HABILITA PARA QUE SE  VEA
' EL MENSAJE DE AYUDA EN EL CUERPO DE LA LISTA PERO CREO PRODUCIA UN PROBLEMA
' CON EL CONTEXTO MANUAL
End If 
'---------------veremos si aca anda mejor despues de roolloop 
#Include "ROLLCTRLSUB.Bi"
'----------------
Do
  COMEDIT = False
param.titulo ="RollMusic Ctrl V "+ nroversion
' abriRoll=1 orden de llamar a Roll grafico
' abrirRoll=0 no hay orden de abrir Roll

  If abrirRoll=1 And cargacancion=1 Then
     abrirRoll=0
     param.encancion=0
     ResetAllListBox(3)
     Resetear (pmTk()) 

      CargarPistasEnCancion ()
     ''CANCIONCARGADA=TRUE
     ROLLCARGADO=False
     '''lo hace tab-cargaCancion=0
     param.encancion=1
     
   If pid1=0 And instancia =0  Then
      pid1=pd1
   EndIf
   If CANCIONCARGADA=True  Then
     ntk=0 '16-03-2022
      If  EventNumber = 10061 Then
          cargaCancion=1 
          CargarSinRoll () ''' play sin roll 
      Else
      EstaBarriendoPenta=1 
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      EndIf 
    ''''''''RollLoop ( param)  ' SOLO PARA DEBUG
   Else     ''''''''RollLoop ( param) '<--con esto anda
     cargacancion=0
   EndIf 
    ''''cargacancion=0 esto me ponia en cero antes que lo use el thread!!!!
    ''' RollLoop(param)
    ''Sleep 200 ' NO HACE FALTA AHORA sin este retardo no le da teimpo al thread de cargar a Roll
    abrirRoll=0
  Else
    If abrirRoll=1 And cargacancion=0 Then
       CANCIONCARGADA=False
       ''cargaCancion=0  
       param.encancion=0 
       If  EventNumber=10061 Then
           cargaCancion=1 
           CargarSinRoll () '''28-02-2024 play sin roll
       Else
       EstaBarriendoPenta=1
       threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
       EndIf

       ''RollLoop ( param)
       abrirRoll=0
    EndIf
  EndIf

     
  If instancia = 0   Then 
    
'PREPARADO PARA EL FUTURO OTRA PANTALLA GRAFICA OPENGL
 ''win = glfwCreateWindow(800,600,"Track OPENGL" )
'' Dim ta As Any Ptr = threadcall correwin(win,ta)
 
    Do
       'If  repro=1 Then ' damos mas recursos si hay play de PlayTocaAll y mas si hay tambien playAll o PlayCancion
       '    Sleep 10
      ' EndIf
' *********************************************************************
' AVISO TRATAR DE USAR SetWindowCallback PARA LIBERAR EL LOOP DE  CARGA
' funtion de windows9.bi, pero es para eventos... no serviria creo
' SI iria donde se detecta el evento CheckBox_GetCheck( cbxgrab(k))
' ********************************************************************
' esto solo se debe ejecutar 1) si se cargo ejec, 2) si se creo una ejec 
'''      If tocatope < 32  And CANCIONCARGADA=TRUE  Then   
' PARA CONVERTIR A MID SI HAY UN PLANO NUEVO



     eventC=WaitEvent()
'WindowStartDraw(hwndC)
'  fillrectdraw(40,40,&h888888)
'  TextDraw(10,10,NombreCancion,-1,&hff0000)
'StopDraw


     Select Case EVENTC 
       Case EventMenu

'''' //////////////////////////  EVENT EVENT EVENT /////////
          #Include "ROLLCTRLEVENTMENU.BI"
'-----------------------------------------------------------------------
       Case eventgadget

     '   SetForegroundWindow(hwndC)
      ' el codigo anterior que traia de disco esta en notas
' TODOS DICEN RUSO Y USA QUE VK_LBUTTON ES 1 PERO CON 1 NO ANDA
' SIN EMBARGO CON 3 ANDA A VECES.. podremos usar otro hilo ?? 
' y de esa forma se terminara el loop de popup..? probemos no anda logico
' porque el loop principal debe detenerse esperando al usiario...
' tampoco anda la reproduccion en ventana ni los gadget de tilde check ni sonido s
' volvemos atras,,,este movimiento es mas duro de trabajar
' 07 marzo 2024 ya anda ok el menu contextua landa al deshabilitarse 
' el gadget de lista con click derecho luego de este se habilita de nuevo 
     ' If ix < 3 Then 
    '  'DisableGadget(PISTASROLL,0)
    '  EndIf  
    '  CTRL_EVENTGADGET()
   #Include "ROLLCTRLEVENTGADGET.bi"  
'--------------------------------------------------------
    '  If ix < 3 Then   
    '  'DisableGadget(PISTASROLL,1)
    '  EndIf
'''           Exit Do ''ESTA DEMAS CREO

      'SetForegroundWindow(hwnd)
'////////// PULSAR TECLAS EN VENTANA MODO CONTROL NO GRAFICO DE ROLL /////
       case EventKeyDOWN
            Select Case  EventKEY 
                Case VK_F1 
                   Shell ("start notepad " + pathinicio + "\ayuda.txt")
                Case VK_SPACE '' HARIA FALTA QUE TOQUE LA CANCION CON
' SPACE SIN ESTAR EL ROLL CARGADO PRIMERO DEBO PODER CARGAR UN CAQNCION SIN ROLL 
               ''''' REMEDAR EL BOTON VERDE ??? CHEQUEAR EL BOTON VERDE
          ''''' NO SE SI ESTA TOCANDO TODA LA CANCION TL VEZ FALTA 
'''' CARGAR EL SINTETIAZADOR 13-02-2024

            End SELECT
'-----------------------------------------------------------------------
       Case EventClose  ''<==== SALIR TERMINA ROLL lax de win control???
' no lo usamos 
        ''si ponemos aca da asercion de cairo.c 
        terminar=2
        Exit Do ,Do    

       Case Else   
' aca muevo lo de check detectara el evento desconocido   
' de check del grab de ejecucion que no es un gadget de window9
' si uso LBDown no funciona bien debo clickear dos veces no se aviva
' en este lugar podrian ir todos los eventos sobre la ventana que no son
'  de window9
          
      If tocatope < 32   Then           
         For k=1 To tocatope+1
         ' al inicio lim sup del for = 1   
          If CheckBox_GetCheck( cbxgrab(k))= 1 Then
             ultimo_chequeado= k 
             If tocaparam(k).nombre="" And k= tocatope+1 Then 
              ntoca=k 'ntoca es la  pista ejec que se esta grabando global entera
              If tocaparam(ntoca).nombre ="" Then
                     ReDim (Toca(ntoca).trk ) (1 To 384000)  ' 1000 compases
                                  tocaparam(ntoca).delta=0
                                  tocaparam(ntoca).nombre =""
                                  tocaparam(ntoca).maxpos =0
                                  tocaparam(ntoca).orden=CUByte(ntoca)
                                  tocaparam(ntoca).patch=0
                                  tocaparam(ntoca).canal=0
                     Redim  CargaIn (1 To 384000) ' 1000 compases
                     If nombrePatron > "" Then
                        tocaparam(ntoca).nombre=nombrePatron
                     Else
                      Dim nompista As String
                        EntrarNombrePista (nompista, hwndC)  
                        tocaparam(ntoca).nombre = Mid (nompista ,1,29)
                      'cargamos nombre solo sin numerar cuando se muestra
                      ' agregamos el orden
                     EndIf
                     ntkp=ntoca 
                     AddListBoxItem(PISTASEJECUCIONES, "("+ doscifras(ntoca)+")"+tocaparam(ntoca).nombre,ntoca-1)
                     tocatope=ntoca 
                     If nombrePatron > "" And nroCompasesPatron > 0 Then
                        pmTk(ntoca+32).MaxPos=nroCompasesPatron  * 384 '' jjjjj
' en la 5ta linea de duracions  0.0208/4 =TickChico a I=240
' " O "," P "," I "," L "," F "," E "," X "," H "," W ",   <-- la 8 es H
'2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,[0.0208333] ,0.01041666, _ '37 45
'"3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45
' es un tresillo de H -> 3H entonces cuantos tresillos de H hay en un compas?
' 128 * 3= 384 TicksChico
' el TickChico vale en tiempo 0.0208 a I=60, pero a I=240 vale 1/4 o sea 0.005 seg, o 5 mseg aprox
                     Else
                        pmTk(ntoca+32).MaxPos=0
                     EndIf
                    If   NombreCancion >"" Then        
             ' sacamos la "\" del final si la tiene 
                         NombreCancion=Trim(NombreCancion)
                         Dim  ls As Short=Len(NombreCancion)
                         If  Mid(NombreCancion, ls, 1)= "\" Then
                              NombreCancion=Mid(NombreCancion,1, ls-1)
                         EndIf
                         Titulos(ntoca+32)=NombreCancion+"\("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
                  else
                         Titulos(ntoca+32)="("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
                  EndIf
              EndIf

              Exit For
             End If 
          EndIf
         Next k
 
      EndIf 


     End Select
     ''' NO ESTA EN VERION F Sleep 5 ' 
    Loop
'-----------------------------------------------------------------------
  Else
      param.titulo ="RollMusic Editor" ' esto no sale si no hay marco
      param.ubiroll=ubiroll
      param.ubirtk=ubirtk
      param.midionof=usarmarco ' para volcado de midi si o no ,si con 4

      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      ThreadWait threadloop
      threadDetach(threadloop)
             Sleep 20   
      cerrar(0)  
  End If
'-----------------------------------------------------------------------
   
Loop
'-----------------------------------------------------------------------
''DisableGadget(PISTASROLL,1) ' para que desactive y salga de ahi 
'' eventM=eventClose
''eventM=eventrbdown
FINALIZAR_ROLLMUSIC = 1
Sleep 5

salir()
Kill "procesos.txt"
Close
    End 0


'---------fin iup---    
errorhandler:
Dim As Integer er, ErrorNumber, ErrorLine
er = Err

ErrorNumber = Err
ErrorLine = Erl




'Dim ers As Integer = 12 - nota +(estoyEnOctava ) * 13 
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )
