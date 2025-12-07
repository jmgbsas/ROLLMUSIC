#Include "ROLLINICIO.bi"
#include "WinGUI.bi"
#include "RTMIDISUB.bi"
#Include "ROLLTRACKS.bi"
#include "ROLLSUB.bi"
'===========================
 Dim As Integer MenuFlag=0, LoopFlag=0 
'Print #1, "ANTES ROLLLOOP"
'========================== 
#include "ROllLoop.bi"
'==========================
On Error Goto errorhandler
 ' ahora debo traducir las notas a notas de Roll e incorporarlas al vector
 ' las notas  en Roll van de 0 a 11 para el indice son los semitonos, pero se cargan 
 ' los valores 1 a 12. En Roll C=12 ...B=1 ergo si la escala dice 12=B debe ser 1
 ' o sea =12-valor+1 (12-1+1) o podemos crear el vector de escalas al reves de 12 a 1 pero
 ' se complicaria todo para analizar. ergo invertimos y listo 1 a 12 es 12 a 1...
 ' Entonces para taducir a Roll hago 12-valor+1 . pero par aanalizar y mostrar es valor
 ' ejemplo triada=1,3,5=C,E,G, en Roll seria = 12,10,8
 
 ' genial puedo recorrer un array con un pointer!!!!
'-------
' esto es muy viejo se debera probar de nuevo y sin ofalla borrar ->ESTA DANDO 
' SEGMENTACION FAULT AL GRABAR 2DA PISTA CON CANCION CARGADA
' AUNQUE YA NO PERMITO BARRER PENTAMIENTRAS GRABA O SEA CONGRABAREJEC=1 
' SALTA EL BARRIDO DEPENTA Y MENU,,,,,PERO IGUAL REVIENTA AL REPRODUCIR 
'EL RESULTADO Y NO GRABA ELARCHIVO ??REPETIR TEST 16-06-2022 17:00
' 1 CARGAR PISTA 1 
' 2 CARGAR ROLL
' 3 CREAR PISTA NUEVA, DEJAR SOLO SELECCION EN ESTA PISTA AJUSTAR PORSAL CANAL 
' Y PATCH,ABRIR MIDI IN, TOCAR ALGO PARA VER SI ANDA MIDI.IN
' 4 GRABAR - REPRODUCIR  <- AHI DA SEGMENTAICON FAULT
'----------------------------------------------------
' --------------------------------------------
nroversion="0.35092 fix borrado de notas, los silencios tenian sonido, secuencia nueva sin sonido "
'"0.35091 SE PUEDEN USAR TECLAS EN LA VENTANA DE CONTROL F1, SPACE, Y P YA ANDAN"
'' AGREGAR TIPOS DE ARCHIVOS "SOLO" ESTA INCOMPELTO PERO ALGO DESARROLLADO
'  TOCA LOS SILENCIOS ARREGLAR Y NO BORRA EL OFF DE UN ON DESPUES DE FIN DE ARCHIVO
' "0.35092 agregamos la extendion *.solo para que el programa sepa que es un solo"
' 3508 sigue produciendose alguna cancelacion por ahora paramos de invesigar y seguimos
' tal vez los cambios nuevos hagan reducir o eliminar al resto...veremos
' 0.3505 ctrl-home fixed hace que entre toda la secuencia en la pantalla
'"0.3504 PRUEBA DE LOOPS, USAR UNA PISTA DE CANCION COMO SOLO Y REPETICIONES  "
' 0.3503 ES CREO EL PRINCIPIO DE PODER DESARROLLAR PATRONES QUE  NO DEBEN SEGUIR A LA
' CANCION Y TENER SUS PROPIAS REPETICIONES INDEPENDIENTE DE LA CANCION POR EJEMPLO BATERIA
' PULSOS DE BAJO ETC,,, 
' GRABAR CANCION OK 
' NUEVA IDEA EN PLAYTOCAALL ESTA ESCRITO , ES TOCAR UNA PISTA DE CANCION RTK SOLO CON PLAYALL
' DE FORMA QUE TOQUE SOLA FUERA DEL LOOP DE CANCION, TAMBIEN PODRIA HACERSE EN EJECUCIONES!
' falta borrar repeticiones!! OK LISTO
' 0.3502 ya funciona !! verificar que se graba a disco y se levanta
' PENDIENTE: -------------------
' ||==> 351 probar que funcionen las grabaciones midi y las conversiones a rtk o roll!!
'-------------------------------
' IDEA CREAR LOOPS, SU LONGITUD SERA ZONA2 -ZONA1 LASREPETICIONES UN ARCHIVO RTK
' PODREMOS PONERLE OTRA EXTENSION PERO ADENTRO UN LOOP O REPETICIONES DENTRO DE UN RTK, 
' EXTENSION *.rep o *.loop ambas seran validas..las ejecuciones se haran en serie
' calculando la duraicon de un loop como (zona2-zona1 )* duracion(1 tick).
'--------------------------- 
' fixed reproducir todo desde Control menu Reproducir 
'  fixed VER AHORA EL RENAME...Y GRABACION A DISCO,ok,GRABA Y LEVANTA EN VERSION 2 y 1 DE ARCHIVOS EJEC. 
' ARREGLAR Y LISTO ok..JUGAREMOS  CON LOS PARAMETROS PAN ECO ETC SOBRE EJEC DIRECTAMENTE ok!!
' 1310 de rolldec seguir nuevo metodo de reconocer versiones con la fecha en un double now
' acordarse de probar el track bar ahora con elcambio de eliminacion de mousebutton en modulo gadget ok funciona ok ahora!
' SEGUIR EN ROLLTRACKS LINEA 224
' ARREGLE PMEJ VOLUMEN DE EJECUCIONES Y DEPASO ENCONTRE QUE IF MOUSEBUTTON AND LEFTBUTTON
' NO FUNCIONA Y ESE DEBE SER EL PROBLEMA TAL VEZ DEL PROBLEMA DE PISTA ROLL QUE DEJABA
' DE FUNCIONAR EL CONTROL SOBRE TAB DE LA PISTA EN EL GRAFICO!!!
' YA INDEPENDICE EL NRO DE PISTA DEL ORDEN ..NO NECESITO ORDEN EN LAS EJECUCIONES
' LUEGO PUEDO ELIMINAR EL NUMERO (1) (2) ETC 
' ------ VER LO MISMO DE LOS NUMEROS EN ROLL
' crear o cargar nueva pista en cancion andaba mal, VERIFICAR CON LOS CAMBIOS NUEVOS
' queda  por revisar
' 1) AL GRABAR EN ROLL UNA PISTA RTK SOBREESCRIBE CON LOS DATOS o al cambiar algun parametro
' haria grabacion automatica (creo que las grabaciones automaticas las eliminare)
' DE OTRA AL USAR GRABAR,   CON ROLL=>TRACK parece que bien.. ARREGLAR GRABAR detectanto formato.
' 2) NUEVO PROBLEMA OK 347 AHORA MIENTRAS HACE PLAY SI CHEQUEO O DESCHEQUEO PISTAS EN CONTROL
' PARA APAGAR UNA PISTA Y ENCENDER OTRA EL PLAY SE CONGELA VER ESO QUEDA LAS NOTAS
' EN ON ....era por diferentes pistas si todas son ejec anda bien,,
' PUDE COMPATIBILIZAR UN POCO SIN CONGELAR AL PONER EL MISMO Tiempo patRON 0.005
' EN EL PLAYCANCION.como en grabar ejec.ES LO QUE TIENE LA GRABACION POR TECLADO. COMPATIBILICE LOS CANTTICKS 
' EL POSN A VECES VENIA EN CERO AJUSTE PARA QUE NO SEA CERO VERIFICAR QUE SEA
' EN LA CARGA MAXPOS -6. 
'3)-> AL dar PLAY ANDA LAS PISTAS MANUAL CON EJEC JUNTAS,PERO AL DAR TAB SE CONGELA EL PLAY
'4)  otro problemas el volumen ,roll sigue bien porque se fija en si Roll(1).onof=1
' entonces toma Roll(1).vol.Se arreglo tocando REalCompas.. Es momento de hacerarchivos nuevos
' y cambianr en todos lados el control del vol entre ejecs y mauales y todo el resto
' patch coro pan olvidar el metodo viejo de poner la info en los extremos del vector
'5) ECO,PAN U CORO SE ELIMINO EL TRACKBAR FUNCIONAMAL IMPIDE SELECCIONAR PISTAS EN
' LISTA DE ROLL, SE REEMPLAZO POR SELECCION NUMERICA
'----------------------------------
' ALT O  tyrasponer=1 por zona  ya esta ok. 
' ctrl-O trasponer=3  traspone todo se habilito ok
' todo implementado pero ha yuna falla al grabar a ROLL a TRACK no graba el patch
' verificar!!!
' solo cancela con PANTESTECO.roll nuevisima version con datroll
'"0.344 control de PANEO salida izquierda derecha de sonido o intermedio, grabacion del paneo de entrada y ajuste del mismo "
' chequear formatos 0 y 1 del archivo midi en el 1 hay un track sin notas y de referencia de tiempos
' yo creo que estoy generando en formato 0 verificarlo
' mas adelante intentaremos desarrollar la escritura de archivos midi sin pasar por planos...
' se desarrolla por fuera y luego  se insertara
' cursorVert = 0 +  cursorHori = 0 + COMEDIT=FALSE  LECTURA
' cursorVert = 0 +  cursorHori = 0 + COMEDIT=TRUE   ENTRADA DE NOTA MANUAL SIEMPRE AL FINAL DE LA SECUENCIA
' cursorVert = 1 +  cursorHori = 1 + COMEDIT=TRUE   CTRL-M SOLO_MODIFICACION  CON X AL FINAL
' cursorVert = 0 +  cursorHori = 2 + COMEDIT=TRUE   CTRL-N MODIFICACION_INSERCION SIN X CON NOTA CDEFGAB
' FALTA:cursorVert = 3 +  cursorHori = 0 + COMEDIT=TRUE   CTRL-O MODIFICACION DE COLUMNAS O ACORDES 

' REVEER: si tiene  sentido Borrar nota si ya lo hace modificar y luego insertar
' fin de secuencia eso se hace en ctrl-m o ctrl-n si pulsamos ctrl-0 (||)
 
' Si inserto la duracion 0 no debo buscar el comienzo de nota? podriamos simular en dos pasos
' insertar una nota cualquiera y luego modificarla con 0 vemos,,,
' trabajos futuros mediatos:
' 1) reveer borrar nota insertar nota...¿borrar dejando un hueco o achicando la secuencia?
'   modo CURSOR: borrar con 9 y X lo hace bien ya lo corregi..agregue buscar la
'   nota cuando este cerca para facilitar el posicionamiento.Si me
' muevo a otro inico de nota y le doy x borra tambien conservando el 9 creo que deberia
' poner nota=0 para sacar el 9 ...¿? conviene o no? lo dejamos asi por ahora. 
'  Borrado de columna con 9 y X? no hay creo , eso es de zonas   
' 2) ver si funcionan la seleccion de zonas o partes
' de la secuencia para copiar insertar etc,,,,para trasponer ya lo hace bien
' 3) la repeticion de zonas en el play, insertar espacios fin de secuencia 
'    Borrar zonas ,cambiar notas insertar notas, mover nota horizontalmente
'    verticalmente ya lo hace con zona y trasponer...podriamos hacer el move
'    con ctrl mouse como hacia cakewalk. mover en maza una zona seleccionandola
'    con el mouse o borrandola con delete...algo se hace con zona pero seria
'    mas elegante con mouse 
' ---------------------------------------------------------------------
' esta version no será compatible con las anteriores de rollmusic sin ticks se agrego por ahora
' un campo nuevo al type dat el onoff ubyte, con 2 indicara on, y con 1 el off
' ----------------
' para saber si   ROLLGRAFICO ESTA LEVANTADO O NO, USAMOS Screenbuffer !!!!!
' FUTURO APLICARLO CUANDO SEA NECESARIO 09-05-2025
'------------------- 
acercade = "RollMusic "+ nroVersion +" Jose M Galeano, Buenos Aires Argentina 2021-2025,. Ejecuta secuencias " + _
 "entrada por pasos usando algoritmos con ticks de tiempos. " + _
 "Los algoritmos pueden fallar en condiciones no estudiadas o no detectadas durante la entrada de datos " + _
 "manual o por ejecucion. OS:Windows 64bits, " + _
 "Usa Cairo como libreria de graficos, Windows9 ,WinGUI y Gtk como GUI; Rtmidi como libreria midi, " + _
 "Editor de codigo FbEdit. Echo en Freebasic como hobby. FreeBASIC Compiler - Version 1.09.0 (2021-12-31), built for win64 (64bit) " + _
" Copyright (C) 2004-2021 The FreeBASIC development team." +_ 
" Consultas: mail:galeanoj2005@gmail.com. (Na, No aplicable, no implementado todavia"


 GrabarPenta=0 ' 07-12-2025 buscando porque no suena al crear 
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

'Print #1,"ANTES ANCHO , ALTO ", ANCHO, ALTO
If mxold > 0 Then

'MoveWindow( hWnd , 1, 1 , ANCHO - mxold, ALTO - myold, TRUE )
'Print #1,"rollmusic.bas 742: ANCHO,nancho ",ANCHO, nancho
  If ANCHO = nancho Then
      ANCHO= nancho -mxold 
  EndIf
'Print #1,"rollmusic.bas 745: ANCHO resultante  ",ANCHO

  AnchoInicial=ANCHO
  If anchofig=0 Then
    anchofig=ANCHO/45 ' decia 700 SON 45 COL PERO SE USAN MENOS 41
  EndIf
  NroCol =  (ANCHO / anchofig ) + 4 ' 20 Tama o figuras, nota guia 6 columnas "B_8_[ "
  ANCHO3div4 = ANCHO *3 / 4
  gap1= anchofig* 6 ' porque era tanto 20 ' 81 default
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


'Print #1,"nfont, nmxold, nmyold, nancho,nalto  ",nfont, nmxold, nmyold,nancho,nalto
    param.ancho = ANCHO 
    param.alto = ALTO
'Print #1,"DESPUES ANCHO , ALTO ", ANCHO, ALTO
'''mxold=0:myold=0

abrirRoll=NO_CARGAR
'pistacreada=0
Dim As Integer k=0, salida=0,ix0,ix1,ix2

' //// DESHABILITAR LOS CLICK EN LISTA SI NO HAY CARGADO NADA
If instancia =ARG0_EN_LINEA  Then  ' cuando es online y recien levanta 
'DisableGadget(PISTASROLL,1) 25-10-2024 SWE HABILITA PARA QUE SE  VEA
' EL MENSAJE DE AYUDA EN EL CUERPO DE LA LISTA PERO CREO PRODUCIA UN PROBLEMA
' CON EL CONTEXTO MANUAL
End If 
'---------------veremos si aca anda mejor despues de roolloop 
'Print #1, "ANTES ROLLCTRLSUB.Bi"
#Include "ROLLCTRLSUB.Bi"
'Print #1, "DESPUES ROLLCTRLSUB.Bi"
'----------------
Do

  COMEDIT=LECTURA
param.titulo ="RollMusic Ctrl V "+ nroversion
'Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
'Print #1,"inicio ubound roll.trk ", UBound(param.Roll.trk,2)
'Print #1,"iniio lbound roll.trk ", LBound(param.Roll.trk,2)
'Print #1, "abrirRoll=1 And cargacancion=CARGAR_NO_PUEDE_DIBUJAR ",abrirRoll,cargacancion
' abriRoll=1 orden de llamar a Roll grafico
' abrirRoll=NO_CARGAR_ROLL no hay orden de abrir Roll 0

  If abrirRoll=CARGAR And cargacancion=CARGAR_NO_PUEDE_DIBUJAR Then
     abrirRoll=NO_CARGAR
  '   Print #1," ENTRA A CARGAR PISTAS  cargaCancion = ",cargaCancion
     param.encancion=SIN_CANCION
     ResetAllListBox(3)
     Resetear () 

      CargarPistasEnCancion ()
      cargariniciotxt(NombreCancion, CANCION)
 '   Print #1,"CARGAR PISTAS cargacancion = ",cargaCancion 
''     CANCIONCARGADA=TRUE
     ROLLCARGADO=False
     '''lo hace tab-cargaCancion=0
     param.encancion=CON_CANCION
     RecalCompas()
   If pid1=0 And instancia =ARG0_EN_LINEA  Then
      pid1=pd1
   EndIf
 ' Print #1,"cALL rOLLLOOP I) cargaCancion ES 1 SI O SI ",cargaCancion
   If CANCIONCARGADA=True  Then
     ntk=0 '16-03-2022
      If  EventNumber = 10061 Then
          cargaCancion=CARGAR_NO_PUEDE_DIBUJAR 
          CargarSinRoll () ''' play sin roll 
      Else
      EstaBarriendoPenta=1 
Print #1, "///1 entro por ThreadCreate rollLoop NOMBRECANCION TITuLOSTK(0) ", NombreCancion, titulosTk(0)
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      clickpista=SI 'abre tab una sola vez seposiciona en psita 1 
      EndIf 
    ''''''''RollLoop ( param)  ' SOLO PARA DEBUG
   Else     ''''''''RollLoop ( param) '<--con esto anda
     cargacancion=NO_CARGAR_PUEDE_DIBUJAR
   EndIf 
 'Print #1,"ENTRA A CARGAR PISTAS cargaCancion ES 1 SI O SI ",cargaCancion   
    ''''cargacancion=0 esto me ponia en cero antes que lo use el thread!!!!
    ''' RollLoop(param)
    ''Sleep 200 ' NO HACE FALTA AHORA sin este retardo no le da teimpo al thread de cargar a Roll
    abrirRoll=NO_CARGAR
  Else
    If abrirRoll=CARGAR And cargacancion=NO_CARGAR_PUEDE_DIBUJAR Then
       CANCIONCARGADA=FALSE
       NADACARGADO=TRUE
       ''cargaCancion=0  
       param.encancion=SIN_CANCION 
       Print #1,"CALL ROLLLOOP II) "
       If  EventNumber=10061 Then
           cargaCancion=CARGAR_NO_PUEDE_DIBUJAR 
           CargarSinRoll () '''28-02-2024 play sin roll
       Else
Print #1, "///2 entro por ThreadCreate RollLoop NOMBRECANCION TITuLOSTK(0) ", NombreCancion, titulosTk(0)
       EstaBarriendoPenta=1
       threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
       EndIf

       ''RollLoop ( param)
       abrirRoll=NO_CARGAR
    EndIf
  EndIf

'Print #1, "IX LLEGA A 337 ANTES LOOP PRINCIPAL " ,  instancia
     
  If instancia = ARG0_EN_LINEA   Then 
    
'PREPARADO PARA EL FUTURO OTRA PANTALLA GRAFICA OPENGL
 ''win = glfwCreateWindow(800,600,"Track OPENGL" )
'' Dim ta As Any Ptr = threadcall correwin(win,ta)
 Dim As Integer unasola=0 
    Do
       'If  Parar_De_Dibujar=1 Then ' damos mas recursos si hay play de PlayTocaAll y mas si hay tambien playAll o PlayCancion
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


           ''Print #1,"TitulosEj(ntoca), ntoca ",TitulosEj(ntoca),ntoca
     
     eventC=WaitEvent() 
          
      If eventC=EventKeyUp  Then
         If  EventKEY = VK_F1 Then  
           Shell ("start notepad " + pathinicio + "\ayuda.txt")
          
         EndIf
         If  EventKEY = VK_SPACE And trabaspace=0 Then
             trabaspace=1
           ReproducirTodasLaSPistas()  
         EndIf
         If  EventKEY = VK_P  Then
            trabaspace=0
           If COMEDIT=LECTURA   Then
             PARAR_PLAY_MANUAL=SI ' DETIENE EL PLAY VEREMOS
             PARAR_PLAY_EJEC=SI
             Cplay=NO
             Sleep 20 
             playloop=NO:playloop2=NO
             play=NO:Cplay=no:playb=No:playEj=NO
             s5=2 ' el loop necesita menos cpu se libera
             trasponer=0
             For i3 As Integer  = 1 To Tope
                portsal=CInt(pmTk(i3).portout) 
                alloff(pmTk(i3).canalsalida,portsal)
                allSoundoff( pmTk(i3).canalsalida, portsal ) 
             Next i3
             Sleep 1
             For i3 As Integer  = 1 To TopeEjec
               portsal=CInt(pmEj(i3).portout) 
               alloff(pmEj(i3).canalsalida,portsal)
               allSoundoff( pmEj(i3).canalsalida, portsal ) 
             Next i3
             Sleep 1
             Parar_De_Dibujar=NO
' cada vez que hago detech cancela no usar nuncas adetach solo en la salida
             STARTMIDI=0
            If instancia=ARG7_NOMBRECANCION Or instancia= ARG107_FICTICIO Or instancia < ARG3_TITU Then 
            Else
              SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_LIBERADO)
              SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_LIBERADO)
            EndIf
           EndIf
             
         EndIf
           
      EndIf 


'WindowStartDraw(hwndC)
'  fillrectdraw(40,40,&h888888)
'  TextDraw(10,10,NombreCancion,-1,&hff0000)
'StopDraw


     Select Case eventC 
       Case EventMenu
'''' //////////////////////////  EVENT EVENT EVENT /////////
          #Include "ROLLCTRLEVENTMENU.BI"
'-----------------------------------------------------------------------
       Case eventgadget
        DisableGadget(3,0)
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
'Print #1,"antes  ctrl_eventgadget  DirEjecSinBarra ", DirEjecSinBarra
    '  CTRL_EVENTGADGET()
   #Include "ROLLCTRLEVENTGADGET.bi"  
'Print #1,"despues de ctrl_eventgadget  DirEjecSinBarra ", DirEjecSinBarra
'--------------------------------------------------------
    '  If ix < 3 Then   
    '  'DisableGadget(PISTASROLL,1)
    '  EndIf
'''           Exit Do ''ESTA DEMAS CREO
      SetFocus (hwndC) ' LA PAPA...
      'SetForegroundWindow(hwnd)
'////////// PULSAR TECLAS EN VENTANA MODO CONTROL NO GRAFICO DE ROLL /////
'-----------------------------------------------------------------------
       Case EventClose  ''<==== SALIR TERMINA ROLL la X de win control
           If play=SI Or playb=SI Or Cplay=SI Or playEj=SI Then 'rollLroop esta levantando en play
              PARAR_PLAY_MANUAL=SI   ' si hay algun play los manda  a detener
              PARAR_PLAY_EJEC=SI
              terminar=TERMINAR_POR_ESCAPE ' 1 va a usar SC_ESCAPE para terminar         
              Exit Do
           Else  ' rollLoop se cerro el grafico no tengo el sc_escape, se supone que no hay play pero podria haberlo 
              PARAR_PLAY_MANUAL=SI   ' si hay algun play los manda  a detener
              PARAR_PLAY_EJEC=SI
              terminar=TERMINAR_POR_LOOP_PRINCIPAL ' aca veo como salgo
              Exit Do
           EndIf
 
       Case Else   
' aca muevo lo de check detectara el evento desconocido   
' de check del grab de ejecucion que no es un gadget de window9
' si uso LBDown no funciona bien debo clickear dos veces no se aviva
' en este lugar podrian ir todos los eventos sobre la ventana que no son
'  de window9
      If terminar=TERMINAR_POR_LOOP_PRINCIPAL Then
         Exit Do
      EndIf     
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
                        pmEj(ntoca).MaxPos=nroCompasesPatron  * 384 '' jjjjj
' en la 5ta linea de duracions  0.0208/4 =TickChico a I=240
' " O "," P "," I "," L "," F "," E "," X "," H "," W ",   <-- la 8 es H
'2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,[0.0208333] ,0.01041666, _ '37 45
'"3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45
' es un tresillo de H -> 3H entonces cuantos tresillos de H hay en un compas?
' 128 * 3= 384 TicksChico
' el TickChico vale en tiempo 0.0208 a I=60, pero a I=240 vale 1/4 o sea 0.005 seg, o 5 mseg aprox
                     Else
                        pmEj(ntoca).MaxPos=0
                     EndIf
                    If   NombreCancion >"" Then        
             ' sacamos la "\" del final si la tiene 
                         NombreCancion=Trim(NombreCancion)
                         Dim  ls As Short=Len(NombreCancion)
                         If  Mid(NombreCancion, ls, 1)= "\" Then
                              NombreCancion=Mid(NombreCancion,1, ls-1)
                         EndIf
                         Print #1,"CHECK GRAB NOMBRECANCION ",NombreCancion
                         TitulosEj(ntoca)=NombreCancion+"\("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
                  else
                         TitulosEj(ntoca)="("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
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
      Print #1,"cALL rOLLLOOP III)"
Print #1, "3 entro por ThreadCreate RollLoop NOMBRECANCION TITuLOSTK(0) ", NombreCancion, titulosTk(0)
      param.ubiroll=ubiroll
      param.ubirtk=ubirtk
      If usarmarcoins > 0 Then
      param.midionof=usarmarcoins '  para volcado de midi si o no ,si con 4
      Else
       param.midionof=usarmarco
      EndIf 

Print #1, "///3 ubiroll ubirtk ", ubiroll,ubirtk
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      ThreadWait threadloop
      threadDetach(threadloop)
             Sleep 20   
      Close 0  
  End If
'-----------------------------------------------------------------------
      If terminar=TERMINAR_POR_LOOP_PRINCIPAL Then  ' 2
       salir()
       Kill "procesos.txt"
       Close
       End 0
     EndIf 
Loop
''-----------------------------------------------------------------------
''DisableGadget(PISTASROLL,1) ' para que desactive y salga de ahi 
'' eventM=eventClose
''eventM=eventrbdown



'---------fin iup---    
errorhandler:

Dim As Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error  Rollmusic detected ", er, posicion, MaxPos
Print #1,Erl, Erfn,Ermn,Err
Print #1,"------------------------------------"
ErrorNumber = Err
ErrorLine = Erl

Print #1,"ERROR = ";ProgError(ErrorNumber); " on line ";ErrorLine
Print #1,"Error Function: "; *Erfn()
'Dim ers As Integer = 12 - nota +(estoyEnOctava ) * 13 
Print #1, "12 -nota +(estoyEnOctava ) * 13) "; ers
Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
Print #1, "error number: " + Str( Err ) + " at line: " + Str( Erl )
FileFlush (-1)