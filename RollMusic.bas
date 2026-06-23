#Include "ROLLINICIO.bi"
#include "WinGUI.bi"
#include "ROLLMEDIA.bi"
#include "RTMIDISUB.bi"
#include "ROLLTRACKS.bi"
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
' SEGMENTACION FAULT AL GRABAR 2DA PISTA CON CANCION CARGADA (EJECUCIONES)
' AUNQUE YA NO PERMITO BARRER PENTA MIENTRAS GRABA O SEA CON GRABAREJEC=1
' SALTA EL BARRIDO DE PENTA Y MENU,,,,,PERO IGUAL REVIENTA AL REPRODUCIR
'EL RESULTADO Y NO GRABA EL ARCHIVO ??REPETIR TEST 16-06-2022 17:00
' 1 CARGAR PISTA 1
' 2 CARGAR ROLL
' 3 CREAR PISTA NUEVA, DEJAR SOLO SELECCION EN ESTA PISTA AJUSTAR PORSAL CANAL
' Y PATCH,ABRIR MIDI IN, TOCAR ALGO PARA VER SI ANDA MIDI.IN
' 4 GRABAR - REPRODUCIR  <- AHI DA SEGMENTAICON FAULT
'----------------------------------------------------
' buena pagina para codigos de control 5-Control Numbers https://www.tolaemon.com/docs/midi4.htm
' agregaremos seleccion de bancos para mas sonidos
' 0x00 Control: Bank Select ( byte alto ) ( valor de 0 a 127 como 7 bits altos ): este número de
'control se utiliza como 7 bits altos del control de selección de banco ( el byte bajo es el
'control número 0x20 ). Por lo general los sintetizadores MIDI permiten seleccionar entre un total
' de 128 instrumentos, que es el máximo que se puede codificar con el byte de datos del comando
'Program Change . No obstante podemos conmutar el banco de 128 instrumentos por otro banco con otros
'128 instrumentos utilizando los comandos Bank Select ( alto y bajo ) . De este modo , combinando
'los dos controles de Bank Select ( controles numero 0x00 y 0x20 ) podremos conmutar entre
'128 * 128 bancos, que junto con el comando Program Change nos permitirán seleccionar entre un total
'de 128*128*128 = 2097152 instrumentos. Como la mayoría de dispositivos no tienen más de 128 bancos
'de instrumentos, suele bastar con utilizar solo el control 0, el de Bank Select ( alto ) para
'conmutar de banco, dejando siempre a 0 el del control 0x20, es decir el de Bank Select ( bajo ) .
' Cuando un equipo recibe el comando de cambio del control Bank Select conmuta el banco pero no ha
'de modificar el instrumento hasta que reciba el comando Program Change de modo que si queremos
'realizar un cambio de instrumento que se encuentra en otro banco primero deberemos enviar el comando
'para modificar el control Bank Select, y luego el comando Program Change, y no al reves.
' https://www.tolaemon.com/docs/midi1.htm
'-------------------------------
' falta el mapa de percusion El canal 10 está reservado a la percusión,
' y cada número de nota corresponde a un sonido. aca seria al reves quiero un sonido
' el prog debe darme el nro de nota o sea en ve de notas CDEFGAB hay que poner nombres de
' sonidos de percusion  https://www.eumus.edu.uy/eme/ensenanza/electivas/python/2020/GM.html
'https://academia-lab.com/enciclopedia/general-midi-nivel-2/
' standard https://markus.com.ar/articulostecnicos/LosInstrumentosMIDI.html
' da numeros http://midi.teragonaudio.com/tutr/bank.htm
'http://midi.teragonaudio.com/progs/software.htm
' --------------------------------------------
nroversion="0.392 BOTON CELESTE CARGA ROLL EN VENTANA DE CONTROL, ENTRENAR VOZ,"
' Ejercicios de ritmo se puede anotar en un notepad los distintos ejercicios notas con silencio 0
' con sonido 1,2,3 de normal semi fuerte y fuerte. 
' el ejecutable tiene su primer icon,,,RM JMG.
' NUEVO: SONIDOS PUROS DE NOTAS PARA AFINAR LA VOZ
' NUEVO OK: CARGO ROLL SIN CANCION EN MENU DE ARCHIVO Y LO PONGO EN LA LISTA DE PISTAS. sI LO CARGO DESDE
'   GRAFICO DEPENDIENTE TAMBIEN VA  A LA LISTA PERO SI TENGO UN GRAFICO SIN CTRL NO LO CARGA EN LISTA
'   COMO ES LOGICO NO HAY VENTANA DE CTRL.
'-------------------------------------------------------------------
' NUEVO NO FUNCA >RETROCESO EN ENTRADA DE NOTAS PARA BORRAR UNA NOTA ENTRADA NO FUNCA
' NUEVO OK >0.39x KEY FAST ENTRADA RAPIDA DE NOTAS SIN ESPERAR, BAJO PRUEBA.... OK
' fix si se va a abrir un medio y se cancela..cancelaba el programa
' FIX messageCallback(127) BLOQUEABA EL PLAY DESDE TECLADO ELIMINADO DE TODOS LADOS
' ARCHIVOS m4a, mp3, mid abiertos en explorador, debe estar instalado el codec de todos
' FIX CANCELACION 06-06-2026 AL CARGAR ARCHIVO PARA EXPORTAR METODO VIEJO 4.5 DE MENU ARCHIVO
' tareas al 4 DE jUNIO
' QUEDA SEGUIR DESARROLLO CARGA MIDI A ROLL O RTK O EJEC VEREMOS CUAL ELIJO
'----------------------------------------------------------------------------------
' FIX: QUEDABA SOLO ESTE CASO:
'   problemas VIEJOS DE trasponer PUEDE  NO SER POR FIX en COPIA, ES UN PROB NO DETECTADO ANTES
'   TRASPONE falla en zonas donde hay muChas notas ligadas
'   ( ANTES DE COPIA YA estaba esta falla)
'    tanto en trasposicion completa o por zona..EJEMPLO
'   ORIGINAL:     P+    I+ P     > EN UN 3/4 SE MUEVE P+ Y EL ULTIMO OFF=1  >
'   AL TRASPONER: P+             > SE TRASPONE POR EJE PARA ARRIBA
'   AL TRASPONER:       I+ P      -> QUEDA  I+ P EN EL MEDIO QUEDA SIN MOVER EN LA ORIGINAL
'   HAY QUE DECIRLE QUE AL MOVER EL 1ER ON Y QUE SEA LIGADO P+, MUEVA EL ON QUE SIGUE
'   Y SI ESTE ES LIGADO TAMBIEN LO MUEVA Y AL QUE SIGA INCLUSO AL ULTIMO QUE NO TENGA LIGAZON
'   ESTE PROCESO PUEDE HACERSE AL DETECTAR UN ON CON + LIGADURA..VERIFICAR QUE ENTRE
'   EL 1ER ON MOVIDO P+ Y EL ULTIMO OFF1 MOVIDO >..HALLA OTROS ON, MOVER A TODOS TENGAN O NO
'   TENGAN LIGADURA Y LISTO! desarrollar codigo en ---> 352026
'----------------------------------------------------------------------------------
' FIX EN 390: EN TRASPONER ZONA  DESAPARECIAN NOTAS DEL CENTRO TAL VEZ POR CAMBIOS EN COPIA,
''FIX ESTO NO fallaba en zonas donde no habia ligaduras seguidas lo hacia perfecto
''FIX TAREA COMPARAR ESTA VERSION NUEVA CON FIX COPIA Y LA 388!
'---------------------------------------------------
' FIX: METRONOMO M PRINCIPAL Y DEL REPRO DE MEDIOS PODIAN ANDAR JUNTOS CORREGIDO
' Fix: recuperado Loop infinito zona + ALT + L
' Fix : Se ajusto en numero de cuenta previo del metronomo al tipo de Compas(ritmo en el programa) seleccionado
' Fix : Se ajusto pasoZona1   pasoZona2 pasoNota para copia andaba mal..
' Probar: probar si afecto a trasponer!!
' Desa: CON FBMIDI.bi YA LEO MIDI CON ESA LIBRERIA SOLO DEBO CARGAR ROLL/RTK/EJEC
' (USAR CondCreate PARA QUE LAS EJECUCIONES DE REPRODUCCION DE EJECS CANCION Y MEDIOS EMPIEZEN
' A EJECUTR AAL MISMO TIEMPO CON ELLO OBTENDRE LA SYNCRONIZACION!))
'--------fin 31 mayo----------------------
' 0.387: BOTON METRONOMO VOLUMEN RETRASO METRONOMO SI NO VOLUMEN AUDIO ETC TODO EN LA VENTANA
'   DEL REPRODUCTOR DE MEDIOS
' 0.386: BOTON CICLO O LOOP EN REPRODUCTOR DE MEDIOS.
' 0.385
' - graba bien archivo mid con percusion canal 10 Y EL RESTO DE CANALES CON SU PATCH
' - graba a midi con repeticiones de toda la secuencia completa,
'   Y CON repeticiones internas TAMBIEN, es igual en vez de cero se pondra la pasozona1 o sea la posicion
'   del 210 cera el cero de antes
' el reproductor de medios (mp3, midi, wav) se puede mover con flechas de teclado ademas de mouse
'------------------
'384
' - fixed  AL AGREGAR UNA NOTA NUEVA A UNA SECUENCIA O PISTA CANCELA
' - fixed Toca la nota ingresada en ENTRADA MANUAL
' - NUEVO:ENTRAR NOTAS ADENTRO DE UNA SECUENCIA con pasoZona1, la posicion de pasoZona1 incrementa
'   en la duracion de la nota, si se quiere volver usar Backspace o Retroceso.
' - fixed SE REPUSO LOOP INFINITO ALT-L PLAYLOOP=NO ELIMINADO DE LAS EJECUCIONES
' - fixed PASOZONA1 Y 2 SE LIMITARON A GAP1 Y MAXPOS PARA EVITAR CANCELAIONES
' - fixed SE PERDIA EL NOMBRE DE LA PISTA NUEVA AL GRABAR COMO RTK..
' - NUEVO:ALT+DELETE ELIMINA UNA ZONA DE LA SECUENCIA SIN DEJAR ESPACIO COMO LO HACE DELETE SOLO.
' EDITAR TRACKS DE CANCION queda pendiente
' agegamos la grabacion en grabartrak del maxposTope que  sera de la cancion a la cual
' pertenece luego agregar la carga de maspostope en cargartrack
' asi tendremos fin de datos del track y fin de cancion.
' al editar un track  de una cancion marcar donde empezar a grabar con pasozona1
' y ahi el programa debe tomar el maxpos nuevo = pasozona1 y se supone que podriamos
' entrar notas en edit al final de esa marca en EDIT VERDE entrada de notas.
' luego de terminar de grabar el nuevo valor de maxpos grabarlo en el track
' si no supera el maxpostope todo sigue igual. si lo supera ese nuevo valor
' se grabara en todos los tracks.
' CREO QUE CONVIENE AL LEVANTAR UNA CANCION HACER REDIM PRESERVE DE TODOS LOS TRACKS
' AL MAXPOSTOPE PAR IGUALARLOS!!! SEGUIR
' 384 en preparacion editar cada track en el mismo grafico de cancion..
' se cambio color de fondo del menu a celeste
' (otra version sera una insercion dentro de la secuencia original
' 2) posn=MaxPos-6 podriamos eliminarlo de los parametros.
' -> E NUN FUTURO 5) poder mover la copia A derecha O izquierda hasta lograr una posicion exacta
' deseada..eso es parte de mover dentro de la longitud de la secuencia....
'-------------------------------
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
' 1) reveer borrar nota insertar nota...ï¿½borrar dejando un hueco o achicando la secuencia?
'   modo CURSOR: borrar con 9 y X lo hace bien ya lo corregi..agregue buscar la
'   nota cuando este cerca para facilitar el posicionamiento.Si me
' muevo a otro inico de nota y le doy x borra tambien conservando el 9 creo que deberia
' poner nota=0 para sacar el 9 ...ï¿½? conviene o no? lo dejamos asi por ahora.
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
' esta version no serï¿½ compatible con las anteriores de rollmusic sin ticks se agrego por ahora
' un campo nuevo al type dat el onoff ubyte, con 2 indicara on, y con 1 el off
' ----------------
' para saber si   ROLLGRAFICO ESTA LEVANTADO O NO, USAMOS Screenbuffer !!!!!
' FUTURO APLICARLO CUANDO SEA NECESARIO 09-05-2025
'-------------------
acercade = "RollMusic "+ nroVersion +" Jose M Galeano, Buenos Aires Argentina 2021-2026,. Ejecuta secuencias " + _
"entrada por pasos usando algoritmos con ticks de tiempos. " + _
"Los algoritmos pueden fallar en condiciones no estudiadas o no detectadas durante la entrada de datos " + _
"manual o por ejecucion. OS:Windows 64bits, " + _
"Usa Cairo como libreria de graficos, Windows9 ,WinGUI y Gtk como GUI; Rtmidi como libreria midi, " + _
"Editor de codigo FbEdit. Echo en Freebasic como hobby. FreeBASIC Compiler - Version 1.09.0 (2021-12-31), built for win64 (64bit) " + _
" Copyright (C) 2004-2021 The FreeBASIC development team." +_
" Consultas: mail:galeanoj2005@gmail.com. (Na, No aplicable, no implementado todavia"


GrabarPenta=0 ' 07-12-2025 buscando porque no suena al crear
'------------///// GUI GUI GUI  GUI ///////////////
'' LA GUI SE EJECUTA EN SU PROPIO LOOP APARTE DEL SISTEMA POR ESO LA BARRA DE ESTADO
'' NO SE PUEDE LLENAR AHI PORQUE SIEMPRE PISA OTROS MENSAJES LA GUI SE REFRESCA
'' SOLA Y MANDARIA SU MENSAJE
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
	
	If ANCHO = nancho Then
		ANCHO= nancho -mxold
	End If
	'Print #1,"rollmusic.bas 745: ANCHO resultante  ",ANCHO
	
	AnchoInicial=ANCHO
	If anchofig=0 Then
		anchofig=ANCHO/200 ' decia 700 SON 45 COL PERO SE USAN MENOS 41
	End If
	NroCol =  (ANCHO / anchofig ) + 4 ' 20 Tama o figuras, nota guia 6 columnas "B_8_[ "
	ANCHO3div4 = ANCHO *3 / 4
	gap1= anchofig* 6 ' porque era tanto 20 ' 81 default
	gap2= (914 * gap1) /1000 ' 74 default
	gap3= (519 * gap1) /1000 ' 42 default
	mxold=0
End If
If myold > 0 Then
	If ALTO = nalto Then
		ALTO= nalto -myold
	End If
	
	AltoInicial=ALTO
	myold=0
End If
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
'----------------ABRIMOS UN PORTOUT DEFAULT
abrirPortoutEjec(100) ''no abre para playAll


'' ------TIPS AYUDA EN LA BARRA DE ESTADO
If UBIRTK> 0 Or UBIROLL > 0 Then
Else
	' esto no se porque lo puse ...channn
	'StatusBarGadget(BARRA_DE_ESTADO,"CADA TECLA EN ESTA VENTANA DE CONTROL SE PUEDE USAR UNA SOLA VEZ, PARA MAS VECES OPRIMIR Q" )
End If
'' LA GUI SE EJECUTA EN SU PROPIO LOOP APARTE DEL SISTEMA POR ESO LA BARRA DE ESTADO
'' NO SE PUEDE LLENAR AHI PORQUE SIEMPRE PISA OTROS MENSAJES LA GUI SE REFRESCA
'' SOLA Y MANDARIA SU MENSAJE

'-----------------
SetStateMenu(hmessages,1118,BatchGraficoOCtrl)
Print #1,"antes del LOOP main ====hwndC ", hwndC
tic=0
CPCS=0: CPSS=0
MOV_FLAG=0:CPlay=NO:Playb=NO:medio_metronomo_on=FALSE
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
		Resetear () ' NO SESETEA EL 0 VA DE 1 A 32
		
		CargarPistasEnCancion ()
		clickpista=SI
		cargariniciotxt(NombreCancion, CANCION)
		RecalCompas(ritmo)
		'   Print #1,"CARGAR PISTAS cargacancion = ",cargaCancion
		''     CANCIONCARGADA=TRUE
		ROLLCARGADO=FALSE
		clickpista=SI
		'''lo hace tab-cargaCancion=0
		param.encancion=CON_CANCION
		RecalCompas(ritmo)
		If pid1=0 And instancia =ARG0_EN_LINEA  Then
			pid1=pd1
		End If
		' Print #1,"cALL rOLLLOOP I) cargaCancion ES 1 SI O SI ",cargaCancion
		If CANCIONCARGADA=True  Then
			ntk=0 '16-03-2022
			If  EventNumber = 10061 Then
				cargaCancion=CARGAR_NO_PUEDE_DIBUJAR
				CargarSinRoll () ''' play sin roll
			Else
				EstaBarriendoPenta=1
				Print #1,"0=====hwndC ", HwndC
				Print #1, "///1 entro por ThreadCreate rollLoop NOMBRECANCION TITuLOSTK(0) ", NombreCancion, titulosTk(0)
				
				threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
				SetThreadPriority(threadloop , 20 ) ' decia 1
				clickpista=SI 'abre tab una sola vez seposiciona en psita 1
				Print #1,"0 1=====hwndC ", HwndC
			End If
			''''''''RollLoop ( param)  ' SOLO PARA DEBUG
		Else     ''''''''RollLoop ( param) '<--con esto anda
			cargacancion=NO_CARGAR_PUEDE_DIBUJAR
		End If
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
				Print #1,"1=====hwndC ", HwndC
				Print #1, "///2 entro por ThreadCreate RollLoop NOMBRECANCION TITuLOSTK(0) ", NombreCancion, titulosTk(0)
				EstaBarriendoPenta=1
				threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
			End If
			Print #2,"=====hwndC ", HwndC
			''RollLoop ( param)
			abrirRoll=NO_CARGAR
		End If
	End If
	
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
				If  EventKEY = VK_F1 Then 'e nventana control
					Shell ("start notepad " + ROLLDIR + "ayuda.txt")
					Exit Do
				End If
				
				If  EventKEY = VK_SPACE And trabaspace=0 Then
					trabaspace=1
					ReproducirTodasLaSPistas()
				End If
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
						End If
					End If
					
				End If
				
			End If
			
			
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
				''   DisableGadget(PISTASROLL,0) deshabilitaba PISTASROLL esto areglaba un problema pero....
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
				End If
				
			Case Else
				' aca muevo lo de check detectara el evento desconocido
				' de check del grab de ejecucion que no es un gadget de window9
				' si uso LBDown no funciona bien debo clickear dos veces no se aviva
				' en este lugar podrian ir todos los eventos sobre la ventana que no son
				'  de window9
				If mensajeEstadoOld<>mensajeEstado  Then
					StatusBarGadget(BARRA_DE_ESTADO, mensajeEstado)
					mensajeEstadoOld=mensajeEstado
					If  mensajeEstado="PISTA A MIDI TERMINADA."   Then
						Sleep 2000
						PlaySound(NULL, NULL, 0 )
					End If
				End If
				
				If terminar=TERMINAR_POR_LOOP_PRINCIPAL Then
					Exit Do
				End If
				'---------------REPRODUCCIONES DESDE EXPLORADOR
				If  ubiejec = 1 Then
					ubiejec =2
					'''30-03-2026 LEVANTAR UN *. EJEC DEDE EL EXPLORADOR
					Print #1,"5 Instancia ",Instancia
					Dim lugar As string
					lugar=Command(1)
					titulosEj(1) =lugar
					Print #1,"lugar ",lugar
					CTRL10165 (lugar,"BATCH")
					DirEjecSinBarra = lugar
					Print #1,"entro por ubiejec > 0 ",lugar
					' vamos a dar play automatico
					SetForegroundWindow hwndC
					PISTASEJECSELECCIONADA=1
					CheckBox_SetCheck( cbxejec(1),1)
					TopeEjec=1:ntoca=1
					playEj=SI
					threadG = ThreadCall  PlayTocaAll (ptoca)
					SetGadgetstate(BTN_MIDI_EJECUTAR,BTN_PRESIONADO)
					SetGadgetstate(BTN_MIDI_PARAR,BTN_LIBERADO)
					Parar_De_Dibujar=NO
					ntoca =1
				End If
				'------------------------------------------------
				If  ubimedia = 1 Then
					ubimedia =0
					'''06-06-2026 LEVANTAR RCHIVOS AUDIO DESDE EL EXPLORADOR
					Print #1,"6 Instancia ",Instancia
					Dim entrada As string
					entrada=TitulosEj(1)
					Print #1,"entrada de media ",entrada
					'Dim ppp As Integer  Ptr
					'ppp=StrPtr(entrada)
					'Print #1," *PPP ",*PPP
					fileflush(-1)
					threadmedia = threadCall  CTRL1094(TitulosEj(1))
					SetThreadPriority(threadmedia , 10 )
					SetForegroundWindow(hwndMEDIA)
					
				End If
				'-------------------------------------------------
				
				'-------------------------------------------------
				If  ubionline = 1 And ubiejec=0 Then
					ubionline =0
					SetForegroundWindow hwndC
					'''30-03-2026 LEVANTAR UN *.ROLL O RTK DESDE EL EXPLORADOR
					Print #1,"5 Instancia ",Instancia
					nombre=Command(1)
					titulosTk(0) =nombre
					Print #1,"nombre ",nombre
					
					If BatchGraficoOCtrl=4 Then ''un roll
						CargaArchivo(Roll, 3) ' lo pone en 2 ver ubiroll
						ubiroll=0:ubirtk=0
						Sleep 100
						thread2 = ThreadCall  playAll (Roll)
						playB=SI
						BatchGraficoOCtrl=3 ' condicion inicial
					End If
					If BatchGraficoOCtrl=5 Then '' un rtk
						CargarTrack(Track(),0, 3)
						ROLLCARGADO=TRUE
						TrackaRoll(Track(),0,Roll,"")
						ubirtk=0: ubiroll=0
						Sleep 100
						thread2 = ThreadCall  playAll (Roll)
						ubirtk=0: ubiroll=0
						playB=SI
						BatchGraficoOCtrl=3  'condicion inicial
					End If
					
					SetGadgetstate(BTN_ROLL_EJECUTAR,BTN_PRESIONADO)
					SetGadgetstate(BTN_ROLL_PARAR,BTN_LIBERADO)
					Parar_De_Dibujar=NO
					
				End If
				'---------------------------------------------------
				If tocatope < 32   Then
					For k=1 To tocatope+1
						' al inicio lim sup del for = 1
						If CheckBox_GetCheck( cbxgrab(k))= 1 Then
							ultimo_chequeado= k
							If tocaparam(k).nombre="" And k= tocatope+1 Then
								ntoca=k 'ntoca es la  pista ejec que se esta grabando global entera
								If tocaparam(ntoca).nombre ="" Then
									CheckBox_SetCheck( cbxejec(k),1) 'automaticamente chequea ejec 26-12-2025
									CantTicks=CantMin*PPQN*tiempoPatron
									ReDim (Toca(ntoca).trk ) (1 To CantTicks)
									tocaparam(ntoca).delta=0
									tocaparam(ntoca).nombre =""
									tocaparam(ntoca).maxpos =0
									tocaparam(ntoca).orden=CUByte(ntoca)
									tocaparam(ntoca).patch=0
									tocaparam(ntoca).canal=0
									Redim  CargaIn (1 To CantTicks)
									If nombrePatron > "" Then
										tocaparam(ntoca).nombre=nombrePatron
									Else
										Dim nompista As String
										EntrarNombrePista (nompista, hwndC)
										tocaparam(ntoca).nombre = Mid (nompista ,1,29)
										'cargamos nombre solo sin numerar cuando se muestra
										' agregamos el orden
									End If
									ntkp=ntoca
									AddListBoxItem(PISTASEJECUCIONES, tocaparam(ntoca).nombre,ntoca-1)
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
									End If
									If   NombreCancion >"" Then
										' sacamos la "\" del final si la tiene
										NombreCancion=Trim(NombreCancion)
										Dim  ls As Short=Len(NombreCancion)
										If  Mid(NombreCancion, ls, 1)= "\" Then
											NombreCancion=Mid(NombreCancion,1, ls-1)
										End If
										Print #1,"CHECK GRAB NOMBRECANCION ",NombreCancion
										TitulosEj(ntoca)=NombreCancion+"\" + tocaparam(ntoca).nombre+".ejec"
									else
										TitulosEj(ntoca)= tocaparam(ntoca).nombre+".ejec"
									End If
								End If
								
								Exit For
							End If
						End If
					Next k
					
				End If
				
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
		End If
		
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
		Kill ROLLDIR+"procesos.txt"
		Close
		End 0
	End If
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
ers  = 12 - nota +(estoyEnOctava ) * 13
Print #1, "12 -nota +(estoyEnOctava ) * 13) "; ers
Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
Print #1, "error number: " + Str( Err ) + " at line: " + Str( Erl )
FileFlush (-1)