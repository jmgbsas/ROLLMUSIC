#Include "ROLLINICIO.BAS"
#include "WinGUI.bi"
#include "RTMIDISUB.bas"
#Include "ROLLTRACKS.bas"
#include "ROLLSUB.BAS"
#Include "ROLLCTRLSUB.BAS"

''' AGREGAREMOS ROLLCTRLSUB ...porque rollsub ya es muy grande no se y separamos
'' las rutinas, estructuramos y despues parametrizaremos sacando lo mas 
'' posible las globales chicas pero las grandes quedaran globales o algo asi
' depende de si se pasan por referencia o valor....07-03-2024  
 

'  LAIDEA EN ESTA VERSION ES LLAMAR SIEMPRE AL ROLL GRAFICO DESDE
' AFUERA EN UN BATCH YDE ESE MODO PUEDO CERRAR LA VENTANA DE CADA UNO
' DEBO PASARLE EL ARCHICO QUE DEBE ABRIR O EL DIRECTORIO 
' QUE DEBE ABRIR,,, DEBO AGREGARLE EL PATCH Y LSITO !!!
' CADA PISTA SE PODRA LEVANTAR UNA POR UNA CON LA OTRAOPCION
' SOLO DEBO PASAR LOS PARAMETROS...Y SI MODIFICO ALGO
' DEBO GRABAR A DISCO Y ENVIAR ORFEN DE RECARGA DE ESA PISTA EN LA CANCION 
' ENE STA FORMA SE PODRA ESCUCHAR LA CANCION SIN ROLL DE CANCION
'  PERO SE PODRA VER Y MODIFICAR CADA PISTA Y ESCUCHARLA
' PARA ESCUCHAR LA CANCION SE BENE IR A VANTANA DE CTROL Y PULASR
' BOTON VERDE DE CANCION,
'========================== 
' esta andando bien seguir verificando 04-06-2022
' colocar HELP-CONTEXTUAL-POPUP <--- es undirectorio con ejemplo de como poner
' esos help tipo globito!!!
'===========================
 Dim As Integer MenuFlag=0, LoopFlag=0 

'========================== 
#include "ROllLoop.BAS"
'========================== 
' aca puedo llamar a varios thread 1 por vez segun el instrumento editado
' o sea 1 vector de roll distinto para casa Thread o llamar a cada Vector
' con el mismo Thread para verlo en pantalla. Se veria 1 por vez o si queremos podriamos
' ver mas de 1 pero apra eso deberia llamar a roll music mas de una vez 
' y eso lo haria desde call roll 
'----------------
#include "ROLLMIDI.BAS"

'----------------

'>>>>>>portsout = portout
'>>>>>*nombreport = ""
' SE ABRE MAS DE UN PUERTO SI SE DESEA ,,
' CUANDO TENGA ABIERTO MAS DE UN PUERTO DEBERE ASIGNAR CIERTOS PUERTOS A CADA PISTA
' LUEGO EN CADA PISTA PUEDO TENER UN PUERTO O DISPOSITIVO CADA UNO CON SUS 16 CNALES
' Y EN CADA CANAL SUS 128 INSTRUMENTOS.,,LUEGO VEREMOS ESO DE LOS BANCOS ETC
   ' nombreport = port_name(midiout, portsout)
    
' para abrir un port no hace falta el nombre en el comando ,puede estar en vacio ""
' por ello vamos a grabar en el archivo el numero de port , podre abrir solo con el nombre
' sin el numero de port? el nombre identifica mas al dispositivo,,,deber? grabarlo
' deberia guardar ambos nro port y nombre,,,,ufff
' si la pc no cambia de configuracion de hardware los numeros serian siempre lso mismos
' para los ports y no haria falta los nombres, pero debo dejar constancia cual era el dispositivo
' o los dispositivos usados....para poder elegir el port en el menu debo cerrar el port actual
' y abrir el otro esto de ponerlo en el inicio esta mal

'Print #1,"  VA A ABRIR EL PORT portsout, nombreport ",portsout, *nombreport
'open_port (midiout, portsout, nombreport)

'print #1,"  "
'If Roll.trk(1,NA).inst > 0 Then
' ChangeProgram ( Roll.trk(1,NA).inst , 0)
'EndIf
' print #1,"ChangeProgram inst ", Roll.trk(1,NA).inst
'Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
'Print #1,"INSTANCIA ",instancia
' carga de opciones iniciales RollMusic.ini

''https://www.freebasic.net/forum/viewtopic.php?f=3&t=22821&p=204270&hilit=loop+through+an+array+with+the+pointer#p204270

Dim As Integer  tilde1102=MF_UNCHECKED,tilde1103=MF_UNCHECKED  
Dim As Integer anchoK, altoK
anchoK = GetSystemMetrics(SM_CXSCREEN)
'altoK = GetSystemMetrics(SM_CYSCREEN)
'-------------
/'
Dim As Integer g1, h1,h2
Dim As  byte Ptr p5,p6

Dim hnro As Integer
' prueba desarrolllo escalas 
 For g1 =1 To 47
    Print #1, escala(g1).nombre
    hnro=escala(g1).nropasos 
    p5= escala(g1).pasos 
    For h1 = 1 To hnro -1
     
     Print #1, *p5;",";  ' impresion de intervalos de la escala
     p5=p5+1 
    Next h1
    Print #1, *p5
' CONSTRUCCION DE LA ESCALA
 Dim As Integer k3=1
   p6= escala(g1).pasos
    For h2 = 1 To hnro -1
     
     'Print #1,NotasEscala(k3);" " ;  ' impresion de la escala con las notas c c# d  etc
     Print #1, 12-k3+1;" ";
     k3= *p6 + k3
     
     p6=p6+1 
    Next h2
    Print #1, 12-k3+1 
    'Print #1, NotasEscala(k3)

 Next g1
 '/
 ' ahora debo traducir las notas a notas de Roll e incorporarlas al vector
 ' las notas  en Roll van de 0 a 11 para el indice son los semitonos, pero se cargan 
 ' los valores 1 a 12. En Roll C=12 ...B=1 ergo si la escala dice 12=B debe ser 1
 ' o sea =12-valor+1 (12-1+1) o podemos crear el vector de escalas al reves de 12 a 1 pero
 ' se complicaria todo para analizar. ergo invertimos y listo 1 a 12 es 12 a 1...
 ' Entonces para taducir a Roll hago 12-valor+1 . pero par aanalizar y mostrar es valor
 ' ejemplo triada=1,3,5=C,E,G, en Roll seria = 12,10,8
 
 ' genial puedo recorrer un array con un pointer!!!!
'-------
' ESTA DANDO SEGMENTACIONFAULT AL GRABAR 2DA PISTA CON CANCION CARGADA
' AUNQUE YA NO PERMITO BARRER PENTAMIENTRAS GRABA O SEA CONGRABAREJEC=1 
' SALTA EL BARRIDO DEPENTA Y MENU,,,,,PERO IGUAL REVIENTA AL REPRODUCIR 
'EL RESULTADO Y NO GRABA ELARCHIVO ??REPETIR TEST 16-06-2022 17:00
' 1 CARGAR PISTA 1 
' 2 CARGAR ROLL
' 3 CREAR PISTA NUEVA, DEJAR SOLO SELECCION EN ESTA PISTA AJUSTAR PORSAL CANAL 
' Y PATCH,ABRIR MIDI IN, TOCAR ALGO PARA VER SI ANDA MIDI.IN
' 4 GRABAR - REPRODUCIR  <- AHI DA SEGMENTAICON FAULT
nroversion="0.4582 contextual TERMINADO " ':Patrones de Ejecucion 03-07-2022
' despues de un año de bajones personales veo si me da gan de seguirlo
' usando canal 7 con portout loopbe y ZynAddSubFk parece que no envia el OFF de las notas,,
'4536-> 1) Repeticion con 1 pista de Track. 2) luego con cancion.- Pendiente
acercade = "RollMusic Version "+ nroVersion +" Jose M Galeano, Buenos Aires Argentina 2021-2022, 2024. Ejecuta secuencias " + _
 "entrada por pasos usando algoritmos sin una linea conductora de tiempos, se basa en las duraciones de las notas. " + _
 "Para entrada por teclado midi usa ticks. Los algoritmos pueden fallar en condiciones no estudiadas o no detectadas durante la entrada de datos " + _
 "manual o por ejecucion. OS:Windows 64bits 7,10,y 11, Proc:AMD Phenom-II Black Edition 4 Nucleos. " + _
 "Usa Cairo como libreria de graficos, Windows9 como GUI y Rtmidi como libreria midi, " + _
 "Editor de código FbEdit. Echo en Freebasic como hobby. FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for win64 (64bit) " + _
" Copyright (C) 2004-2023 The FreeBASIC development team." +_ 
" Consultas: mail:galeanoj2005@gmail.com"
 
'------------
Static Shared As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8,MenName10
Static Shared As HMENU MenName31,MenName32 
If ix < 3 And ubirtk=0 And ubiroll=0 And menuabierto=0 Then ' rollmusic CON control
  menuabierto=1 ' evita apertura de mas de un menu
  instancia=0
  hwndC = OpenWindow("RollMusic Ctrl V "+ nroversion,10,10,ANCHOSYSTEM*0.91 ,ALTOSYSTEM*0.91,WS_OVERLAPPEDWINDOW Or WS_VISIBLE,  WS_EX_ACCEPTFILES   )
''UpdateInfoXserver()
Var bitmap = Load_image("fondo.bmp")
BRUSH = WindowBackgroundImage(hwndC,bitmap,1)

  hwndListBox= ListBoxGadget(LISTA_DE_PISTAS,80,40,290,685,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT  )
  SetGadgetFont(LISTA_DE_PISTAS,CINT(LoadFont("consolas bold",13))) 
' botton todo o nada , sonido o mudo para todas las pistas
  ButtonGadget(CHECK_PISTA_ROLL, 60,20,20,20,"S")
  SendMessage(GadgetID(LISTA_DE_PISTAS),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
 ' TextGadget(4,250,10,240,20,, SS_SIMPLE  )
 ' 04-02-2023

  SetGadgetColor(LISTA_DE_PISTAS,cint("&HC0C0C0"),0,1)
  SetGadgetColor(CHECK_PISTA_ROLL,cint("&HC0C0C0"),0,1)
' check para encender o apagar, sonido de salida de c/pista  
  cbxnum(1) =  CheckBox_New( 60 ,  40, 20, 20, "",, hwndc) 
  cbxnum(2) =  CheckBox_New( 60 ,  60, 20, 20, "",, hwndc)
  cbxnum(3) =  CheckBox_New( 60 ,  80, 20, 20, "",, hwndc)
  cbxnum(4) =  CheckBox_New( 60 , 100, 20, 20, "",, hwndc)
  cbxnum(5) =  CheckBox_New( 60 , 120, 20, 20, "",, hwndc)
  cbxnum(6) =  CheckBox_New( 60 , 140, 20, 20, "",, hwndc) 
  cbxnum(7) =  CheckBox_New( 60 , 160, 20, 20, "",, hwndc) 
  cbxnum(8) =  CheckBox_New( 60 , 180, 20, 20, "",, hwndc)
  cbxnum(9) =  CheckBox_New( 60 , 200, 20, 20, "",, hwndc)
  cbxnum(10) = CheckBox_New( 60 , 220, 20, 20, "",, hwndc)
  cbxnum(11) = CheckBox_New( 60 , 240, 20, 20, "",, hwndc)
  cbxnum(12) = CheckBox_New( 60 , 260, 20, 20, "",, hwndc)
  cbxnum(13) = CheckBox_New( 60 , 280, 20, 20, "",, hwndc) 
  cbxnum(14) = CheckBox_New( 60 , 300, 20, 20, "",, hwndc) 
  cbxnum(15) = CheckBox_New( 60 , 320, 20, 20, "",, hwndc)
  cbxnum(16) = CheckBox_New( 60 , 340, 20, 20, "",, hwndc)
  cbxnum(17) = CheckBox_New( 60 , 360, 20, 20, "",, hwndc)
  cbxnum(18) = CheckBox_New( 60 , 380, 20, 20, "",, hwndc)
  cbxnum(19) = CheckBox_New( 60 , 400, 20, 20, "",, hwndc) 
  cbxnum(20) = CheckBox_New( 60 , 420, 20, 20, "",, hwndc) 
  cbxnum(21) = CheckBox_New( 60 , 440, 20, 20, "",, hwndc)
  cbxnum(22) = CheckBox_New( 60 , 460, 20, 20, "",, hwndc)
  cbxnum(23) = CheckBox_New( 60 , 480, 20, 20, "",, hwndc)
  cbxnum(24) = CheckBox_New( 60 , 500, 20, 20, "",, hwndc)
  cbxnum(25) = CheckBox_New( 60 , 520, 20, 20, "",, hwndc) 
  cbxnum(26) = CheckBox_New( 60 , 540, 20, 20, "",, hwndc) 
  cbxnum(27) = CheckBox_New( 60 , 560, 20, 20, "",, hwndc)
  cbxnum(28) = CheckBox_New( 60 , 580, 20, 20, "",, hwndc)
  cbxnum(29) = CheckBox_New( 60 , 600, 20, 20, "",, hwndc)
  cbxnum(30) = CheckBox_New( 60 , 620, 20, 20, "",, hwndc)
  cbxnum(31) = CheckBox_New( 60 , 640, 20, 20, "",, hwndc)
  cbxnum(32) = CheckBox_New( 60 , 660, 20, 20, "",, hwndc) 

' fast? http://www.forosdelweb.com/f69/archivos-ayuda-chm-con-visual-basic-6-0-a-801611/


  EventC=0

'---------------------------LISTA DE EJECUCIONES------------
  hwndListEjec= ListBoxGadget(LISTA_DE_EJECUCIONES, 430,40,290,685,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )
SetGadgetFont(LISTA_DE_EJECUCIONES,CINT(LoadFont("consolas bold",14)))
SetGadgetColor(LISTA_DE_EJECUCIONES,cint("&HC0C0C0"),0,1)
  ButtonGadget(CHECK_SELECCION_EJECUCION,380,20,20,20,"S")
SetGadgetColor(CHECK_SELECCION_EJECUCION,cint("&HC0C0C0"),0,1)

  SendMessage(GadgetID(LISTA_DE_EJECUCIONES),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
' BS_PUSHLIKE se hune el boton al seelccioanrlo
' CHECK PARA ESCUCHAR SONIDO de las ejecuciones desde teclado grabadas
  cbxejec(1) =  CheckBox_New( 380 ,  40, 20, 20, "",, hwndC) 
  cbxejec(2) =  CheckBox_New( 380 ,  60, 20, 20, "",, hwndC)
  cbxejec(3) =  CheckBox_New( 380 ,  80, 20, 20, "",, hwndC)
  cbxejec(4) =  CheckBox_New( 380 , 100, 20, 20, "",, hwndC)
  cbxejec(5) =  CheckBox_New( 380 , 120, 20, 20, "",, hwndC)
  cbxejec(6) =  CheckBox_New( 380 , 140, 20, 20, "",, hwndC) 
  cbxejec(7) =  CheckBox_New( 380 , 160, 20, 20, "",, hwndC) 
  cbxejec(8) =  CheckBox_New( 380 , 180, 20, 20, "",, hwndC)
  cbxejec(9) =  CheckBox_New( 380 , 200, 20, 20, "",, hwndC)
  cbxejec(10) = CheckBox_New( 380 , 220, 20, 20, "",, hwndC)
  cbxejec(11) = CheckBox_New( 380 , 240, 20, 20, "",, hwndC)
  cbxejec(12) = CheckBox_New( 380 , 260, 20, 20, "",, hwndC)
  cbxejec(13) = CheckBox_New( 380 , 280, 20, 20, "",, hwndC) 
  cbxejec(14) = CheckBox_New( 380 , 300, 20, 20, "",, hwndC) 
  cbxejec(15) = CheckBox_New( 380 , 320, 20, 20, "",, hwndC)
  cbxejec(16) = CheckBox_New( 380 , 340, 20, 20, "",, hwndC)
  cbxejec(17) = CheckBox_New( 380 , 360, 20, 20, "",, hwndc)
  cbxejec(18) = CheckBox_New( 380 , 380, 20, 20, "",, hwndc)
  cbxejec(19) = CheckBox_New( 380 , 400, 20, 20, "",, hwndc) 
  cbxejec(20) = CheckBox_New( 380 , 420, 20, 20, "",, hwndc) 
  cbxejec(21) = CheckBox_New( 380 , 440, 20, 20, "",, hwndc)
  cbxejec(22) = CheckBox_New( 380 , 460, 20, 20, "",, hwndc)
  cbxejec(23) = CheckBox_New( 380 , 480, 20, 20, "",, hwndc)
  cbxejec(24) = CheckBox_New( 380 , 500, 20, 20, "",, hwndc)
  cbxejec(25) = CheckBox_New( 380 , 520, 20, 20, "",, hwndc) 
  cbxejec(26) = CheckBox_New( 380 , 540, 20, 20, "",, hwndc) 
  cbxejec(27) = CheckBox_New( 380 , 560, 20, 20, "",, hwndc)
  cbxejec(28) = CheckBox_New( 380 , 580, 20, 20, "",, hwndc)
  cbxejec(29) = CheckBox_New( 380 , 600, 20, 20, "",, hwndc)
  cbxejec(30) = CheckBox_New( 380 , 620, 20, 20, "",, hwndc)
  cbxejec(31) = CheckBox_New( 380 , 640, 20, 20, "",, hwndc)
  cbxejec(32) = CheckBox_New( 380 , 660, 20, 20, "",, hwndc) 
'---------------------------------------------------------
' CHECK PARA GRABAR O SEA ARMA LA PISTA PARA RECIBIR DATOS MIDI-IN
  ButtonGadget(CHECK_GRABAR_EJECUCION,410,20,20,20,"G")  
 ' ButtonGadget(8,450,0,100,30,"PARAR",BS_RADIOBUTTON )
 ' ButtonGadget(9,580,0,120,30,"GRABAR",BS_RADIOBUTTON  )
Var IMGP=Load_image(".\recur\Parar.bmp")
Var IMGG=Load_image(".\recur\Grabar.bmp")
Var IMGE=Load_image(".\recur\Ejec.bmp")
SetGadgetColor(CHECK_GRABAR_EJECUCION,cint("&HC0C0C0"),0,1)

' pistas de ejec MIDI-IN
GroupGadget(GRUPO_BTNS_MIDI,445,0,113,40,"")
ButtonImageGadget(BTN_MIDI_PARAR,450,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(BTN_MIDI_GRABAR,490,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(BTN_MIDI_EJECUTAR,530,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )

 TextGadget(21,570,12,95,20,"         ")
' pistas manuales  PARA CARGAR CANCION DESDE DIRECTORIO PISTAS ECHAS CON ROLL
GroupGadget( GRUPO_BTNS_MANUAL,95,0,113,40,"") 'play cancion
ButtonImageGadget(BTN_ROLL_PARAR, 100,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(BTN_ROLL_EJECUTAR, 140,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )
ButtonImageGadget(BTN_ROLL_GRABAR_MIDI, 180,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )



 'rbparar = RadioButton_New( 450 , 10, 40, 20, "P",BS_LEFTTEXT , hwndC) '65
 'rbgrabar =RadioButton_New( 500 , 10, 40, 20, "G",, hwndC) ' 80





' checks para habilitar grabacion en una pista de MIDI-IN
  cbxgrab(1) =  CheckBox_New( 410 ,  40, 20, 20, "",, hwndC) 
  cbxgrab(2) =  CheckBox_New( 410 ,  60, 20, 20, "",, hwndC)
  cbxgrab(3) =  CheckBox_New( 410 ,  80, 20, 20, "",, hwndC)
  cbxgrab(4) =  CheckBox_New( 410 , 100, 20, 20, "",, hwndC)
  cbxgrab(5) =  CheckBox_New( 410 , 120, 20, 20, "",, hwndC)
  cbxgrab(6) =  CheckBox_New( 410 , 140, 20, 20, "",, hwndC) 
  cbxgrab(7) =  CheckBox_New( 410 , 160, 20, 20, "",, hwndC) 
  cbxgrab(8) =  CheckBox_New( 410 , 180, 20, 20, "",, hwndC)
  cbxgrab(9) =  CheckBox_New( 410 , 200, 20, 20, "",, hwndC)
  cbxgrab(10) = CheckBox_New( 410 , 220, 20, 20, "",, hwndC)
  cbxgrab(11) = CheckBox_New( 410 , 240, 20, 20, "",, hwndC)
  cbxgrab(12) = CheckBox_New( 410 , 260, 20, 20, "",, hwndC)
  cbxgrab(13) = CheckBox_New( 410 , 280, 20, 20, "",, hwndC) 
  cbxgrab(14) = CheckBox_New( 410 , 300, 20, 20, "",, hwndC) 
  cbxgrab(15) = CheckBox_New( 410 , 320, 20, 20, "",, hwndC)
  cbxgrab(16) = CheckBox_New( 410 , 340, 20, 20, "",, hwndC)
  cbxgrab(17) = CheckBox_New( 410 , 360, 20, 20, "",, hwndc)
  cbxgrab(18) = CheckBox_New( 410 , 380, 20, 20, "",, hwndc)
  cbxgrab(19) = CheckBox_New( 410 , 400, 20, 20, "",, hwndc) 
  cbxgrab(20) = CheckBox_New( 410 , 420, 20, 20, "",, hwndc) 
  cbxgrab(21) = CheckBox_New( 410 , 440, 20, 20, "",, hwndc)
  cbxgrab(22) = CheckBox_New( 410 , 460, 20, 20, "",, hwndc)
  cbxgrab(23) = CheckBox_New( 410 , 480, 20, 20, "",, hwndc)
  cbxgrab(24) = CheckBox_New( 410 , 500, 20, 20, "",, hwndc)
  cbxgrab(25) = CheckBox_New( 410 , 520, 20, 20, "",, hwndc) 
  cbxgrab(26) = CheckBox_New( 410 , 540, 20, 20, "",, hwndc) 
  cbxgrab(27) = CheckBox_New( 410 , 560, 20, 20, "",, hwndc)
  cbxgrab(28) = CheckBox_New( 410 , 580, 20, 20, "",, hwndc)
  cbxgrab(29) = CheckBox_New( 410 , 600, 20, 20, "",, hwndc)
  cbxgrab(30) = CheckBox_New( 410 , 620, 20, 20, "",, hwndc)
  cbxgrab(31) = CheckBox_New( 410 , 640, 20, 20, "",, hwndc)
  cbxgrab(32) = CheckBox_New( 410 , 660, 20, 20, "",, hwndc) 

'---------------------------------------------------------------------
' port de salida ,Volumen y Paneo para ejecuciones y tracks manuales...
' solo se da click en 'S' de sonido de la pista y luego uno de estos3 botones..
' se ajusta asi en una pista de track o ejecucion por vez el port de salida o 
' midi-out...igual para volumen y paneo,si sequiereajustar mas de uno a la vez
' simplemente se da click en todos los deseados y elajuste sera el mismo en cada uno
' o sea S tiene doblefuncion desmutear odar sonido o ajustar alguno de los 3 parametros.  
ButtonGadget(BTN_EJEC_PORTSAL,420,710,70,20,"PortSal")
ButtonGadget(BTN_EJEC_VOL,   490, 710, 35, 20, "Vol")
ButtonGadget(BTN_EJEC_PAN,   530, 710, 35, 20,"Pan")
ButtonGadget(BTN_EJEC_PATCH,  570,710, 50, 20,"Patch")
ButtonGadget(BTN_EJEC_CANAL,  620,710, 50, 20,"Canal")

'---------------------------
ButtonGadget(BTN_ROLL_PORTSAL, 70,710,70,20,"PortSal")
ButtonGadget(BTN_ROLL_VOL,   140, 710, 35, 20, "Vol")
ButtonGadget(BTN_ROLL_PAN,   180, 710, 35, 20,"Pan")
ButtonGadget(BTN_ROLL_PATCH,  220,710, 50, 20,"Patch")
ButtonGadget(BTN_ROLL_CANAL,  280,710, 50, 20,"Canal")



'StatusBarGadget(1,"StatusBarGadget")

  hMessages=Create_Menu()

  MenName1=MenuTitle(hMessages,"Archivo")
  MenName2=MenuTitle(hMessages,"Nueva Cancion")
  MenName3=MenuTitle(hMessages,"Crear Pistas Manuales")
  MenName31=MenuTitle(hMessages,"Crear Patrones")

  MenName4=MenuTitle(hMessages,"Ver")
  MenName5=MenuTitle(hMessages,"Cambiar Tiempo Y Ritmo")
  MenName6=MenuTitle(hMessages,"Reproducir")
  MenName7=MenuTitle(hMessages,"Opciones")
  MenName8=MenuTitle(hMessages,"Puertos de Ejecuciones")
  MenName10=MenuTitle(hMessages,"Info")

''MenuItem(1005,MenName1, "Na.Cargar archivo de Cancion")
MenuItem(1006,MenName1, "1.0 Cargar directorio de Cancion con Pistas separados con Ventana de Control y Roll Grafico")
Menubar(MenName1)
MenuItem(10061,MenName1,"2.0 Abrir directorio de Cancion con Pistas separados con Ventana de Control  sin Roll Grafico")
MenuItem(10062,MenName1,"2.1 Abrir Roll Grafico dependiente de Control si se uso la opcion (2.0) ")
MenuItem(10063,MenName1,"2.2 Externo:Abrir un Roll Grafico independiente de Control, si se uso la opcion (2.0) ")
Menubar(MenName1)
MenuItem(1007,MenName1, "3.0 Grabar Cancion")
'MenuItem(1008,MenName1, "Na.Grabar Cancion Como")
'MenuItem(1009,MenName1, "Na.Exportar Cancion a midi")
Menubar(MenName1)
MenuItem(1010,MenName1, "4.0 Cargar una Pista (rtk o roll) externa en Cancion")
MenuItem(1011,MenName1, "4.1 Grabar una Pista de la Cancion con modificaciones, carga pista si no hubiera cargada")
MenuItem(1012,MenName1, "4.2 Copia una pista a otra  nueva en cancion")
'MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "4.3 Grabar una Pista rtk a roll TrackaRoll")
Menubar(MenName1)
MenuItem(1015,MenName1, "5.0 MIDI-IN Grabar Pistas ejecucion")
MenuItem(1016,MenName1, "5.1 MIDI-IN Cargar Pistas ejecucion")
'''MenuItem(1017,MenName1, "Elegir MIDI-OUT o Driver o Port de Salida de pista previamente chequeda en S (sonido)")
Menubar(MenName1)
MenuItem(1019,MenName1, "    Salir")


MenuItem(1020,MenName2, "Nombre o Título (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "Tiempo I=60 por omision")
MenuItem(1022,MenName2, "Na.Ritmo 4/4 por omision")
MenuItem(1023,MenName2, "Na.Duracion Estimada Min.(Por Omision 3 estimada)")
'MenuItem(1024,MenName2, "Na.Crear Cancion en un solo archivo")
MenuItem(1025,MenName2, "Crear un directorio de Cancion con Pistas separadas")
'MenuItem(1026,MenName2, "Na.Ver Lista Tracks de la Cancion (Nombre y numero)")
MenuItem(1027,MenName2, "Na.Modificar Nombre de Pistas de Cancion")


MenuItem(1028,MenName3, "Cambia Octavas, si rango es mayor al anterior, se borran datos  (0,1,2,3,4,5,6,7,8)")
MenuItem(1029,MenName3, "Na.Seleccion rango de 3 octava repetidas 2 veces ")
MenuItem(1030,MenName3, "Na.Octavas de Instrumetnos Estandares")
'MenuItem(1031,MenName3, "Na.Seleccion Canal (futuro se repetira por comodidad...)")
Menubar(MenName3)
MenuItem(1040,MenName3, "Cambia Instrumento por orden Alfabetico")
MenuItem(1050,MenName3, "Cambia Instrumento por orden Numérico")
Menubar(MenName3)
MenuItem(1060,MenName3, "Crear pista aislada, En Roll dependiente, con lo elegido y reemplaza la existente en la edicion")
MenuItem(1061,MenName3, "Crear Pista nueva en la Cancion en Edicion, Con lo elegido")
Menubar(MenName3)
MenuItem(1062,MenName3, "Crear Instancia de RollMusic Sin Control alguno Con lo elegido")
'MenuItem(1063,MenName3, "Cargar una pista de cancion en RollMusic Grafico")

'MenuItem(1065,MenName31, "Crear Patrones de Ejecuciones por Teclado",MF_POPUP )
MenName32=OpenSubmenu(MenName31, "Crear Patrones de Ejecuciones por Teclado" )
MenuItem(1064,MenName32,"Nombre del Patron")
MenuItem(1065,MenName32,"Numero de Compases del Patron")
MenuItem(1066,MenName32,"Crear Secuencia de  Patrones insertados")
MenuItem(1067,MenName31, "Na. Crear Patrones de Ediciones manuales")
MenuItem(1068,MenName32,"Habilitar Patrones",MF_UNCHECKED)


MenuItem(1070,MenName4,"Ver Escalas auxiliares ajustadas", MF_CHECKED)
MenuItem(1071,MenName4,"Ver Cifrado de Acordes", MF_CHECKED)
  
MenuItem(1080,MenName5,"TEMPO, Manual Por omision=60, Ejecucion Tick por omision=5mseg equivale a 240")
MenuItem(1081,MenName5,"Factor para Aumentar velocidad de ejecucion, No se graba en archivo 1,5 o 0,5 etc")

'MenuItem(1082,MenName5,"Na. TEMPO por nombres, Lento,adagio etc y control fino")
'MenuItem(1083,MenName5,"Na. TEMPO insertar cambio de tempo")
'MenuItem(1084,MenName5,"Na. TEMPO borrar cambio de tempo")
'MenuItem(1085,MenName5,"Na. TEMPO ver marcas de cambio de tempo")
'MenuItem(1086,MenName5,"Na. TEMPO ocultar marcas de tempo")
'MenuItem(1087,MenName5,"Na. TEMPO incremento de tempo gradual alcanzado en N compases")

/' futuro agregar limite menor de c/rango con opcion de incrementarlo hasta el tope
  d esu rango 
Negras por minuto	 tempo
40-43	Grave
44-47	Largo
48-51	Larghetto
52-54	Adagio
55-65	Andante
66-69	Andantino
70-95	Moderato
96-112	Allegretto
113-120	Allegro
121-140	Vivace
141-175	Presto
176-208	Prestissimo
'/    

  
MenuItem(1090,MenName6,"Reproducir desde la posicion o en el rango ajustado")
MenuItem(1091,MenName6,"Fijar Repeticiones de un numero de Compases elegido como zona")
MenuItem(1092,MenName6,"Reproducir MIDI-IN (teclado) por  MIDI-OUT.  ")
MenuItem(1093,MenName6,"Detener Reproduccion MIDI-IN (teclado) por  MIDI-OUT. (test de Input) ")


MenuItem(1100,MenName7,"Usar MARCO de Ventana Para el Gráfico",MF_UNCHECKED)
MenuItem(1101,MenName7,"Usar MARCO de Ventana en instancias Gráficas",MF_UNCHECKED)
Menubar(MenName7)
MenuItem(1102,MenName7,"Fracciona Acorde [Con <> Duraciones], notas similares en una pista (no hay silencios)",MF_UNCHECKED  )
MenuItem(1103,MenName7,"Fracciona NOTA o Acorde [CDD], agregando silencios en una pista ",MF_UNCHECKED  )
MenuItem(1104,MenName7,"Fracciona [CDD], notas automaticamente en una pista ",MF_CHECKED  )
MenuItem(1105,MenName7,"No Fraccionar, NO Usar Acordes iguales ", MF_UNCHECKED )
Menubar(MenName7)
MENUITEM(1106,MenName7,"Seleccionar TIPO DE ESCALA PRINCIPAL de la PISTA (Por omision Mayor)")
MENUITEM(1107,MenName7,"Seleccionar NOTA DE LA ESCALA ESCALA PRINCIPAL DE LA PISTA (Por omision C )")
MenuItem(1111,MenName7,"Insertar escala libre en la Posicion actual (Pasozona1)")
MenuItem(1112,MenName7,"Insertar escala Alternativa de la Principal en la Posicion actual (Pasozona1)")
Menubar(MenName7)
MENUITEM(1108,MenName7,"Trabajar con sostenidos (Por omision Sostenidos #)",MF_CHECKED )
MENUITEM(1109,MenName7,"Trabajar con bemoles ",MF_UNCHECKED )
Menubar(MenName7)
MenuItem(1113,MenName7,"Usar metronomo para Tocar MIDI-IN)",MF_CHECKED)

MenuItem(1200,MenName8,"Puerto MIDI-IN Ejecucion")
' MenuItem(1201,MenName8,"Na. Abrir      Puerto MIDI-IN")
' MenuItem(1202,MenName8,"Na. Cerrar    Puerto MIDI-IN")
' MenuItem(1203,MenName8,"Na. DesTruir Puerto MIDI-IN")
Menubar(MenName8)
MenuItem(1204,MenName8,"Puerto MIDI-OUT Ejecucion")
MenuItem(1205,MenName8,"Abrir  Puertos MIDI-OUT Ejecucion")
MenuItem(1206,MenName8,"Cerrar Puertos MIDI-OUT Ejecucion")
'MenuItem(1207,MenName8,"Na. DesTruir Puertos MIDI-OUT")


MenuItem(2000,MenName10,"Acerca de")
'  MenuBackColor NO FUNCIONA SOLO AL PRINCIPIO
' CASA  MENU QUEDA LIMITADO POR BARRAS DE COLOR EN ESTE CASO GRIS
' Y LAS LINEAS DE SEPARACION QUEDAN GRIS MAS GRUESAS
' LA PRIMERA VEZ QUE EJECUTADEJA TODO AZUL PERO DESPUES SE VA
' USA BGR, PARA CONVERTIR UN RGB SIMPLEMENTE 
' CAMBIAMOS DE POSICION LA 1ER Y 3ER CIFRA HEX DEL RGB
 MenuBackColor(hMessages,CInt("&hC0C0C0"),1) ' GRIS DIFERENCIA
' --MenuBackColor(hMessages,CInt("&hA56E3A"),1)
' EL COLOR SALE TODO AL REVES SI PONES AZUL SALE MARRON
' Y SI PONES MARRON COMO ES EL CASO SALE AZUL JAJAJA
' PARECE QUE NO ES RGB SINO BGR
' O SEA USA BGR NO RGB ,BGR EXISTE
End If
'1) 1200 SELECCIONAR  PUERTO MIDI-IN
'2) SELECCIONAR PORTSAL,CANAL,PATCH
'3) 1092 REPRODUCIR MIDI-IN POR MIDI-OUT
' YA PUEDO GRABAR -...SELECIONAR PORT OUT Y ABRIR NO USO PARECE... 



' default de FRACCIOANR autodur 
   usarAcordesIguales=1
   TipoFrac="autodur"
metronomo_si=1
usarmarcoins=0
usarmarco=0 
If com_usarmarco =0 Then
   usarmarco=0
   usarmarcoOld=0   
Else
   usarmarco=com_usarmarco  
   usarmarcoOld=usarmarco
EndIf
' condicion inicial para ver o no escalas auxiliares en el grafico
Select Case nVerEscalasAuxiliares
  Case 0             
       SetStateMenu(hMessages,1070,0)
  Case 3
       SetStateMenu(hMessages,1070,3)
End Select

' condicion inicial para ver o no cifrado acorde  en el grafico
Select Case nVerCifradoAcordes
  Case 0             
       SetStateMenu(hMessages,1071,0)
  Case 3
       SetStateMenu(hMessages,1071,3)
End Select
' al inicio por omision patrones deshabilitados
     SetStateMenu(hmessages,1064,1)
     SetStateMenu(hmessages,1065,1)
     SetStateMenu(hmessages,1066,1)
' AL INICIO DESHABILITADOS 2.1 Y .2.2 AUNQUE AL CLIQUEAR NO PASA NADA
  SetStateMenu(hmessages,10062,1)
  SetStateMenu(hmessages,10063,1)

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
    anchofig=ANCHO/45 ' SON 45 COL PERO SE USAN MENOS 41
  EndIf
  NroCol =  (ANCHO / anchofig ) - 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "
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


'Print #1,"nfont, nmxold, nmyold, nancho,nalto  ",nfont, nmxold, nmyold,nancho,nalto
    param.ancho = ANCHO 
    param.alto = ALTO
'Print #1,"DESPUES ANCHO , ALTO ", ANCHO, ALTO
'''mxold=0:myold=0

abrirRoll=0
'pistacreada=0
Dim As Integer k=0, salida=0

' //// DESHABILITAR LOS CLICK EN LISTA SI NO HAY CARGADO NADA
If ix < 3 Then 
DisableGadget(LISTA_DE_PISTAS,1)
End If 
'------------
Do
  COMEDIT = False
param.titulo ="RollMusic Ctrl V "+ nroversion
'Print #1,"param.ancho ",param.ancho;" param.alto ";param.alto
'Print #1,"inicio ubound roll.trk ", UBound(param.Roll.trk,2)
'Print #1,"iniio lbound roll.trk ", LBound(param.Roll.trk,2)
'Print #1, "abrirRoll=1 And cargacancion=1 ",abrirRoll,cargacancion
' abriRoll=1 orden de llamar a Roll grafico
' abrirRoll=0 no hay orden de abrir Roll

  If abrirRoll=1 And cargacancion=1 Then
     abrirRoll=0
  '   Print #1," ENTRA A CARGAR PISTAS  cargaCancion = ",cargaCancion
     param.encancion=0
     ResetAllListBox(3)
     Resetear (pmTk()) 

      CargarPistasEnCancion ()
 '   Print #1,"CARGAR PISTAS cargacancion = ",cargaCancion 
     ''CANCIONCARGADA=TRUE
     ROLLCARGADO=False
     '''lo hace tab-cargaCancion=0
     param.encancion=1
     
   If pid1=0 And ix < 3 Then
      pid1=pd1
   EndIf
 ' Print #1,"cALL rOLLLOOP I) cargaCancion ES 1 SI O SI ",cargaCancion
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
 'Print #1,"ENTRA A CARGAR PISTAS cargaCancion ES 1 SI O SI ",cargaCancion   
    ''''cargacancion=0 esto me ponia en cero antes que lo use el thread!!!!
    ''' RollLoop(param)
    ''Sleep 200 ' NO HACE FALTA AHORA sin este retardo no le da teimpo al thread de cargar a Roll
    abrirRoll=0
  Else
    If abrirRoll=1 And cargacancion=0 Then
       CANCIONCARGADA=False
       ''cargaCancion=0  
       param.encancion=0 
       Print #1,"CALL ROLLLOOP II) "
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


     
  If ix < 3 Then 
    instancia=0
'PREPARADO PARA EL FUTURO OTRA PANTALLA GRAFICA OPENGL
 ''win = glfwCreateWindow(800,600,"Track OPENGL" )
'' Dim ta As Any Ptr = threadcall correwin(win,ta)
    Do
       'If  repro=1 Then ' damos mas recursos si hay play de PlayTocaAll y mas si hay tambien playAll o PlayCancion
       '    Sleep 10
      ' EndIf
        
         For k=1 To 32 
           If CheckBox_GetCheck( cbxgrab(k))= 1 And tocaparam(k).nombre="" Then 
              ntoca=k 'ntoca es la  pista ejec que se esta grabando global entera
              If tocaparam(ntoca).nombre ="" Then
                     ReDim (Toca(ntoca).trk ) (1 To 384000)  ' 1000 compases
                                  tocaparam(ntoca).delta=0
                                  tocaparam(ntoca).nombre =""
                                  tocaparam(ntoca).maxpos =0
                                  tocaparam(ntoca).orden=ntoca
                                  tocaparam(ntoca).patch=0
                                  tocaparam(ntoca).canal=0
                     Redim  CargaIn (1 To 384000) ' 1000 compases
                     If nombrePatron > "" Then
                        tocaparam(ntoca).nombre=nombrePatron
                     Else
                        EntrarNombrePista  tocaparam(ntoca).nombre
                     EndIf
                     ntkp=ntoca 
                     AddListBoxItem(4, tocaparam(ntoca).nombre,ntoca-1)
                     tocatope=ntoca '''tocatope+1
                     If nombrePatron > "" And nroCompasesPatron > 0 Then
                        pmTk(ntoca+32).MaxPos=nroCompasesPatron  * 384 '' jjjjj
' en la 5ta linea de duracions  0.0208/4 =TickChico a I=240
' " O "," P "," I "," L "," F "," E "," X "," H "," W ",   <-- la 8 es H
'2.666666,1.333333,0.666666,0.333333,0.166666,0.083333,0.041666,[0.0208333] ,0.01041666, _ '37 45
'"3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45
' es un tresillo de H -> 3H entonces cuantos tresillos de H hay en un compas?
' 128 * 3= 384 TicksChico
' el TickChico vale en tiempo 0.0208 a I=60, pero a I=240 vale 1/4 o sea 0.005 mseg aprox
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
                         Print #1,"CHECK GRAB NOMBRECANCION ",NombreCancion
                         Titulos(ntoca+32)=NombreCancion+"\("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
                  else
                         Titulos(ntoca+32)="("+doscifras(ntoca)+")"+ tocaparam(ntoca).nombre+".ejec"
                  EndIf
              EndIf
              Exit For
          EndIf
        Next k

           ''Print #1,"Titulos(ntoca+32), ntoca ",Titulos(ntoca+32),ntoca

     eventC=WaitEvent()
'WindowStartDraw(hwndC)
'  fillrectdraw(40,40,&h888888)
'  TextDraw(10,10,NombreCancion,-1,&hff0000)
'StopDraw

     Select Case EVENTC 
       Case EventMenu
  If NombreCancion > "" And S5=0 Then 
     SetForegroundWindow(hwndC)
  EndIf
         Select Case EventNumber
            'CON ROLL , SIN ROLL
           Case 1006, 10061   '<=========== CARGAR CANCION con roll, o sin Roll
             'cargamos todos los tracks
             ' ok anda bien, una vez cagados se permuta en memoria con TAB
             ' o haciedno click en la lista
            '' UseGadgetList(hwndC)
         
              CTRL100610061 (hMessages , Tope )
             
             If abrirRoll=0 And NombreCancion > ""  Then
                abrirRoll=1
                cargaCancion=1
                Print #1,"SALE A CARGAR ROLL POR 1ERA VEZ ABRIRROLL=1 EXIT DO"
                Exit Do                 
             EndIf
  '           Print #1,"termino 1006 va a abrir Roll"
          SetForegroundWindow(hwnd)


         Case 10062
' LO ABRE ACA PERO ES DEPENDIENTE DE LA VENTANA  DE CONTROL 
' ESTA NO SE PUEDE CERRAR EL USUARIO LO DEBE SABER
' CADA PISTA SE PODRA LEVANTAR UNA POR UNA CON LA OTRAOPCION
' SOLO DEBO PASAR LOS PARAMETROS...Y SI MODIFICO ALGO
' DEBO GRABAR A DISCO Y ENVIAR ORFEN DE RECARGA DE ESA PISTA EN LA CANCION
   Print #1," CASE 10062 abrirRoll=0 And NombreCancion > ", abrirRoll, NombreCancion

           CTRL1062 (hmessages )

          Case 10063 ' CARGAR CANCION EN UN ROLL SIN VENTANA DE CONTROL
' HAY QUE PASA EL NOMBRE DEL DIRECTORIO NADA MAS,,,Y EL PATH
            CTRL1063 ()             
' ----------------------------------------------------------------------
           Case 1007 '<============ grabar cancion bosquejo
' 26-02-2022 desarrollo           
            CTRL1007 ()

          SetForegroundWindow(hwnd)

           Case 1010 '<================ Cargar Pista externa a cancion

   '        Print #1,"entro a 1010 Cargar Pista externa a cancion"

           CTRL1010 (salida )
           If salida =1 Then 
              salida=0
              Exit Select
           End If   
          
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------            
           Case 1011 ' <======= Grabar una Pista de la Cancion con modificaciones, que son tracks
    '        print #1,"entro a 1011 esto lo hace menu de Roll tambien" '' jmg probar es nuevo...
 ' copiamos logica Rolla Track 
   '         print #1, "Click Grabando a disco pista modif con GrabarRollaTrack ",nombre
            Dim As String nombreg
            ROLLCARGADO=FALSE 
           If NombreCancion > ""  Then
              GrabarRollaTrack(0)
           EndIf
          MenuNew=0           
          carga=1
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1012 ' <====== Grabar Pista Como, Copia una pista a otra  nueva nueva
   '        print #1,"entro a 1012 Grabar Pista Como, Copia una pista a otra  nueva nueva"
             
             CTRL1012 (SALIDA)

             If SALIDA=1 Then
                 salida=0
                 Exit Select
             End If 
'-----------------------------------------------------------------------
           Case 1014  ' <============= TRACK A ROLL
           TrackaRoll (Track(), ntk , Roll ) ' no usa ubirtk
           GrabarArchivo(0)
           SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1015 '<========== Grabar MIDI-In aca sera para grabar 
 ' EN ejecuciones, CON CANCION CARGADA NO GRABA NADA, la grabacion se hace en STOP SIN CANCION
Print 1,"GRABA MIDI IN EN CASE 1015  "
'--------------------------
' preparamos para grabar la pista por cambio de patch
           CTRL1015 ()
'-----------------------------------------------------------------------
           Case 1016 '<============Cargar MIDI-IN
' deberia borrar la cancion cargada ??? creo que si totdavia no ejecuta
' ambas cosas a la vez,,,,,debo almenos poner dos opciones mas
' borrar cancion cargada y borrar ejecucion cargada
           CTRL1016 ()

'-----------------------------------------------------------------------
            Case 1017 'seleccionar  por SALIDA de lapista ejecucion 

'-----------------------------------------------------------------------
            Case  1018 ' elegir PATCH si corresponde de la pista de ejecucion 

'-----------------------------------------------------------------------
           Case 1019  ''<============= SALIR TERMINA ROLL
             'eventM=eventrbdown
             eventM=eventClose
  ' no funciona si hay loop en listapista por popup menu ,,no hay caso
  ' ni poniendo end 0.. nada de nada ni engañando forzando eventC a close ni 
  ' eventM a EVENTRBdown nada de nada, no pasa nunca por aca solo obedece
  ' a un eventClose en EventC, el cual hay que pulsar dos veces para este caso
            terminar=1
            Exit Do ,Do    
'-----------------------------------------------------------------------
           Case 1020 ' <=========== Entrar Nombre o Título de la Cancion     
               NombreCancion = ""
               pathdir=""
               EntrarNombreCancion(NombreCancion)
      '    SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1021 ' <=========== Entar Tempo 
             menuOldStr="[TEMPO]"
             nombreArchivo="0"
               thread3= ThreadCall EntrarTeclado()
      '        SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1025 ' <======== Crear un directorio de Cancion con Pistas separadas
               CrearDirCancion (NombreCancion)
               If NombreCancion > "" Then
                  param.encancion=1
               EndIf    
    '      SetForegroundWindow(hwnd)        
'-----------------------------------------------------------------------
           Case 1028 ' <========== seleccion octavas menores a 1 9 
               seloctava (desde, hasta)
               *po = hasta -1
                posn=1
                Nuevo (Roll,1 )
                param.ubiroll=ubiroll
                param.ubirtk=ubirtk

                posn=0
     '     SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1031 ' <========  SELECCION DE CANAL DE LA PISTA (10 DRUMS)
'-----------------------------------------------------------------------               
           Case 1040 ' <========== seleccion de instrumento por orden Alfabetico

                CTRL1040
 
        '     SetForegroundWindow(hwnd)    
'-----------------------------------------------------------------------
           Case 1050 ' <=========== seleccion de instrumento por orden Numerico

                CTRL1050
  
        '      SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1060 ' <========== crea track y reemplaza al existente en la edicion
                CTRL1060 salida
                If salida = 1 Then 
                   salida=0
                   Exit Do
                End If
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1061 ' <====== crear pista en cancion con lo elegido

               CTRL1061 (SALIDA)
               If SALIDA = 1 Then
                  salida=0
                  Exit Select  
               EndIf
               
               Exit Do                 
               
' FALTA CREAR LA PISTA !!! jmg ERO PUEDO USAR UNA PISTA YA CREADA EN 1011
' la graba igual desde roll parece pero debe ser en orden
       '   SetForegroundWindow(hwnd)            
'-----------------------------------------------------------------------
           Case 1062 ' <======== crear instancia independiente sin control
 ' ponerle diferente color y/o tamaño para poder distinguirlo adma sde l nombre
 ' estudiar si puedo hacer IPC entre Menus de GUI pero son loop tambien no creo.
             Print #fa1,pd1  
     
             Shell (" start RollMusic.exe "+ Str(desde)+" "+ Str(hasta) + " Track_"+Str(desde)+"_"+Str(hasta) + " "+Str(instru) + " " +Str(pid1) + " "+ Str(usarmarcoins))
      '    SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
' aca debo tomar de la seleccion del usuario con ctrl+p por ejemplo sobre
' una pista y tomar los parametros de la cancion cargada de esa pista
' y enviarla a un grafico para que la vea o toda la cancion !!! 
'-----------------------------------------------------------------------
           Case 1064 ' <========= Nombre del Patrï¿½n
         Dim As String patronPorOmision
         patronPorOmision=nombrePatron 
         nombrePatron = InputBox("Nombre del Patron Nuevo" ,"Entre un Nombre ",patronPorOmision)
         
'-----------------------------------------------------------------------
           Case 1065 ' <========== numero de compases del patron
         Dim As String nroCompasesPatronOmision
         nroCompasesPatronOmision=Str(nroCompasesPatron) 
         nroCompasesPatron = CInt(InputBox("Numero de Compases del Patron" ,"Entre un Numero ",nroCompasesPatronOmision))

'-----------------------------------------------------------------------

           Case 1066 '<======== SECUENCIA DE PATRONES DE EJECUCION
           CrearSecuenciaPatrones()
'-----------------------------------------------------------------------
           Case 1068
               CTRL1068 (hmessages)

           Case 1070
               CTRL1070(hmessages)

              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1071
               CTRL1071(hmessages)

          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1080 ' tempo
              nombreArchivo="0"
              menuOldStr="[TEMPO]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1081 ' factor de multi tempo
              nombreArchivo="0"
              menuOldStr="[FACTOR]"
              thread3= ThreadCall EntrarTeclado()
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1090 ' Reproducir cancion
                CTRL1090 ()
          '    Dim As Any Ptr thplayC = ThreadCall  playCancion(track())
          '    CONTROL1 = 1
 
             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1091 ' <=========== Repeticiones de un nro de compases
             If pasoZona1 > 0 And pasoZona2 > 0  Then
                menuOldStr="[NROREP]"
                nombreArchivo="0"
                thread3= ThreadCall EntrarTeclado()
             Else
               MessBox ("Repeticiones", "Debe entrar una zona de campases, Ctrl-clik en comienzo y Ctrl-click final")
             EndIf
             SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1092 ' abrir un midi-in ...con callback
' no depende del numero de pista de ejecucion,sino del portin solamente,,,
             CTRL1092 ()

'----------------------------------------------------
           Case 1093
             abrirMIDIin=2
           For  i As Short =1 To 32
               If CheckBox_GetCheck( cbxgrab(i))= 1  Then
                   cancel_callback(midiin(pmTk(i+32).portin )) ' porque lso port fisicos empiezan desde cero
                   listinAbierto( pmTk(i+32).portin) = 0
                  teclado=0
              EndIf
           Next i 

'-----------------------------------------------------------------------
           Case 1100 '<======== usar o no, marco de ventana de Roll
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarco
             usarmarco=GetStateMenu (hMessages,1100)
             Select Case usarmarco
               Case 0             
                  SetStateMenu(hMessages,1100,3)
                  usarmarco=3
               Case 3
                  SetStateMenu(hMessages,1100,0)
                  usarmarco=0
             End Select
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1101 ' marco o no marco par ainstancias
'0 - the menu is active, the checkbox is not selected
'1 - the menu item is unavailable, grayed out
'2 - the menu item is unavailable (on Linux the same as under the number 1)
'3 - Check the box
             usarmarcoOld=usarmarcoins
             usarmarcoins=GetStateMenu (hMessages,1101)
             Select Case usarmarcoins
               Case 0             
                  SetStateMenu(hMessages,1101,3)
                  usarmarcoins=3
               Case 3
                  SetStateMenu(hMessages,1101,0)
                  usarmarcoins=0
             End Select
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1102
                 usarAcordesIguales=1
                 TipoFrac="igualdur" 
                 SetStateMenu(hmessages,1102,3)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1103
                 usarAcordesIguales=1
                 TipoFrac="tododur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,3)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1104
                 usarAcordesIguales=1
                 TipoFrac="autodur" 
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,3)
                 SetStateMenu(hmessages,1105,0)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1105
                 usarAcordesIguales=0
                 SetStateMenu(hmessages,1102,0)
                 SetStateMenu(hmessages,1103,0)
                 SetStateMenu(hmessages,1104,0)
                 SetStateMenu(hmessages,1105,3)
            SetForegroundWindow(hwnd)                 
'-----------------------------------------------------------------------
           Case 1106 ' <======== escala de la secuencia, similar a la de instrumentos
               pasozona1=0
               selTipoEscala (tipoescala_num_ini)

' GRABADO EN grabaPos(1,1).inst = CUByte(tipoescala) ' 20-12-2021 - tipoescala en uso
' CUADNO QUEIRO UN CAMBIO PUEDO DEJAR UN ACOLUMNA VACIA Y PONER TODO ESTA INFO
' PERO DEBO INDICAR AL PROGRAM QUE SALTEE ESTA COLUMNA CREO CON TENER NOTA=181 Y DUR181
' PODRI AINDICAR ESO DEBO PROBARLO Y USAR LSO DEMAS CAMPOS PARA INTRODUCIR ALGUN  CAMBIO               
'''            Roll.trk(1,NA).vol= CUByte(tipoescala + 127) ' a partir de 128
''               Print #1,"Roll.trk(1,NA).vol ",Roll.trk(1,NA).vol
''               END
''               Track(ntk).trk(1,1).vol=CUByte(tipoescala + 127)
              ' grabar el track 
'' NOTA: LA VARIABLES DE ESCALA DE TODA LA SECUENCIA TIENEN SUBFIJOS _STR O _NUM
'' LAS QUE SON PARA USO DE ESCLAS EN POSICIONES NO LO TIENEN
      '      Print #1,"tipo de escala seleccionado ", tipoescala_num_ini
              
' -------cadena de escala, construye dsde C hay que hacer las otras esclas
    ' C,D,E,F,G,A,B,Bb,Ab,Gb ver las debo pedir escala y 1er nota desde donde empieza uff
      '        Print #1,"armarescla desde 1106"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
' --------------------------   
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1107 ' <======== usamos sostenidos o bemoles ???
              pasozona1=0
              selNotaEscala (notaescala_num_ini)
 
       '       Print #1, "seleccion de Nota de la escala num  ",notaescala_num
       '       Print #1,"armarescla desde 1107"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)

            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1108 '<======= alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="sos" ' grabado en grabaLim(1,1).pan  = CUByte(3)
              SetStateMenu(hmessages,1108,3)  
              SetStateMenu(hmessages,1109,0)
            ' si hay nombre de archivo grabar sino no   
      ''        GrabarArchivo()
       '       Print #1,"armarescla desde 1108"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
          
            SetForegroundWindow(hwnd)
' --------------------------   
           Case 1109 ' <======== alteraciones sotenidos o bemoles
              pasozona1=0
              alteracion="bem" ' grabado en grabaLim(1,1).pan  = CUByte(2)
              SetStateMenu(hmessages,1108,0)  
              SetStateMenu(hmessages,1109,3) 
       '       Print #1,"armarescla desde 1109"
              cadenaes_inicial=""
              armarescala(cadenaes_inicial,tipoescala_num_ini, notaescala_num_ini,alteracion,1)
      
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1110
   
             MessBox ("", acercade)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1111 '<========== cambiode escala
              CTRL1111 ()
 
            SetForegroundWindow(hwnd)             
'-----------------------------------------------------------------------
           Case 1112 '<========= cambiode a escala Alternativa de la Principal
              CTRL1112() 
            SetForegroundWindow(hwnd)  

            Case 1113 ' usar metronomo
                
             metronomo_si=GetStateMenu(hmessages,1113)
              Select Case metronomo_si 
                     Case  1 
                    metronomo_si=0
                    SetStateMenu(hmessages,1113,0)
                     Case 0
                    metronomo_si=1
                    SetStateMenu(hmessages,1113,1)

              End Select
              SetForegroundWindow(hwnd)

           Case 1200 'Seleccionar  Puertos MIDI-IN SOLO PARA PORTS DE EJECUCION POR AHORA
' seleccion de portin , 2:portin. ntkp:salida
' ->  npi: numero port entrada DIFERENCIA PARA ABAJO
' portsin es la cantidad de ports que hay de entrada
' seleccionamos un port de entrada para cada track de ejecucion
' como pensamos por ahora que hay un solo usuario ejecutando ese port se usara para todos 
' los tracks pero probarmos usar distintos solo que no tengo 2 port sanos para probar en este momento 
' el port de la tarjeta no anda y estoy entrando por USB midi cable 
' portin  'GLOBAL ULTIMO PUERTO IN SELECCIONADO, por omision el cero
' si solo hay ejecuciones .....
           CTRL1200 ()
 
      
'           Case 1201 'Abrir      Puertos MIDI-IN
'              listinAbierto(npi)=1
'           Case 1202'Cerrar    Puertos MIDI-IN
           Case 1203 'DesTruir Puertos MIDI-IN

           Case 1204 'Seleccionar      Puertos MIDI-OUT PARA EJECUCIONES
' seleccion de portout , 1:portout. ntkp:salida
' ->  npo: numero port salida
' portsin es la cantidad de ports que hay de entrada
            CTRL1204 ()
 

           Case 1205'Abrir      Puertos MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES
  
           CTRL1205 ()
  

'-------------------------------

           Case 1206 'Cerrar    Puertos MIDI-OUT de ejecucion play por el usuario
               CTRL1206()

           Case 1207'DesTruir Puertos MIDI-OUT

           Case 2000
   
             MessBox ("", acercade)
            SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
                   
         End Select
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
    '  'DisableGadget(LISTA_DE_PISTAS,0)
    '  EndIf  
        CTRL_EVENTGADGET() 
    '  If ix < 3 Then   
    '  'DisableGadget(LISTA_DE_PISTAS,1)
    '  EndIf
           Exit Do

'      SetForegroundWindow(hwnd)
'////////// PULSAR TECLAS EN VENTANA MODO CONTROL NO GRAFICO DE ROLL /////
       case EventKeyDOWN
            Select Case  EventKEY 
                Case VK_F1 
                   Shell ("start notepad ayuda.txt")
                Case VK_SPACE '' HARIA FALTA QUE TOQUE LA CANCION CON
' SPACE SIN ESTAR EL ROLL CARGADO PRIMERO DEBO PODER CARGAR UN CAQNCION SIN ROLL 
               ''''' REMEDAR EL BOTON VERDE ??? CHEQUEAR EL BOTON VERDE
          ''''' NO SE SI ESTA TOCANDO TODA LA CANCION TL VEZ FALTA 
'''' CARGAR EL SINTETIAZADOR 13-02-2024

            End SELECT
'-----------------------------------------------------------------------
       Case EventClose  ''<==== SALIR TERMINA ROLL lax de win control???
        ''si ponemos aca da asercion de cairo.c 
        terminar=2
        Exit Do ,Do    

     End Select
     Sleep 5 ' consume mucha cpu 05-03-2024
   Loop
'-----------------------------------------------------------------------
  Else
      param.titulo ="RollMusic Editor" ' esto no sale si no hay marco
  '    Print #1,"cALL rOLLLOOP III)"
      threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))
      ThreadWait threadloop
      cerrar(0)  
  End If
'-----------------------------------------------------------------------
   
Loop
'-----------------------------------------------------------------------
''DisableGadget(LISTA_DE_PISTAS,1) ' para que desactive y salga de ahi 
'' eventM=eventClose
eventM=eventrbdown
Sleep 5
Dim sale As Any Ptr 
 sale= threadcall salir() ''<==== SALIR TERMINA ROLL
 '''threadwait (sale)
'Sleep 100
'Kill "procesos.txt"
'----FIN CONTROL-------------------
'   threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1)) 
'   ThreadWait threadloop
 
'RollLoop ( param)
'Sleep 100
    End 0


'---------fin iup---    
errorhandler:
Dim As Integer er, ErrorNumber, ErrorLine
er = Err
Print #1,"Error  Rollmusic detected ", er, posicion, MaxPos
Print #1,Erl, Erfn,Ermn,Err

Print #1,"------------------------------------"
ErrorNumber = Err
ErrorLine = Erl


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


Print #1,"ERROR = ";ProgError(ErrorNumber); " on line ";ErrorLine
Print #1,"Error Function: "; *Erfn()
'Dim ers As Integer = 12 - nota +(estoyEnOctava ) * 13 
Print #1, "12 -nota +(estoyEnOctava ) * 13) "; ers
Print #1, "ubound 2 de Roll.trk ", UBound(Roll.trk, 2)
 Print "error number: " + Str( Err ) + " at line: " + Str( Erl )