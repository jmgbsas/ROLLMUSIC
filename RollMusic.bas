#Include "ROLLINICIO.BAS"
#include "WinGUI.bi"
#include "RTMIDISUB.bas"
#Include "ROLLTRACKS.bas"
#include "ROLLSUB.BAS"
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
nroversion="0.4573 cancion sin roll" ':Patrones de Ejecucion 03-07-2022
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

  hwndListBox= ListBoxGadget(LISTA_DE_PISTAS,80,40,290,670,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )
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

EVENTc=0
'  funciona trae el help de freebasic implementar para unhelp futuro...
Dim As Any Ptr library = DyLibLoad( "hhctrl.ocx" )
HtmlHelpA  = DyLibSymbol( library, "HtmlHelpA" )
HtmlHelp    = DyLibSymbol( library, "HtmlHelp" )
HH_POPUP = DyLibSymbol( library, "HH_POPUP" )

Dim As HWND hwndTip = CreateWindow(TOOLTIPS_CLASS, NULL, _ 
                            WS_POPUP OR TTS_NOPREFIX Or TTS_BALLOON,  _
                            0, 0, 0, 0, NULL, NULL, NULL, NULL)
Dim As TOOLINFO ti
    ti.cbSize   = sizeof(ti)
    ti.uFlags   = TTF_TRANSPARENT Or TTF_CENTERTIP
    ti.hwnd     = hwndC
    ti.uId      = 0
    ti.hinst    = NULL
    ti.lpszText = LPSTR_TEXTCALLBACK

    GetClientRect(hwndC, @ti.rect)

    SendMessage(hwndTip, TTM_ADDTOOL, 0, @ti )

    SendMessage(hwndTip, TTM_ACTIVATE, 0,  @ti)
'http://www.forosdelweb.com/f69/archivos-ayuda-chm-con-visual-basic-6-0-a-801611/
'HTMLHelp(GetDesktopWindow(), "C:\IT64\AREAWORK\ROLLMUSIC-143-MENU-PORTS\hola.txt", HH_DISPLAY_TOPIC, NULL)


  EVENTc=0

'---------------------------LISTA DE EJECUCIONES------------
  hwndListEjec= ListBoxGadget(LISTA_DE_EJECUCIONES, 430,40,290,670,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT )
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
ButtonGadget(BTN_EJEC_PORTSAL,420,700,70,20,"PortSal")
ButtonGadget(BTN_EJEC_VOL,   490, 700, 35, 20, "Vol")
ButtonGadget(BTN_EJEC_PAN,   530, 700, 35, 20,"Pan")
ButtonGadget(BTN_EJEC_PATCH,570,700, 50, 20,"Patch")
ButtonGadget(BTN_EJEC_CANAL,620,700, 50, 20,"Canal")

'---------------------------
ButtonGadget(BTN_ROLL_PORTSAL,70,700,70,20,"PortSal")
ButtonGadget(BTN_ROLL_VOL,   140, 700, 35, 20, "Vol")
ButtonGadget(BTN_ROLL_PAN,   180, 700, 35, 20,"Pan")
ButtonGadget(BTN_ROLL_PATCH,220,700, 50, 20,"Patch")
ButtonGadget(BTN_ROLL_CANAL,280,700, 50, 20,"Canal")



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

MenuItem(1005,MenName1, "Na.Cargar archivo de Cancion")
MenuItem(1006,MenName1, "Cargar directorio de Cancion con Pistas separados")
MenuItem(10061,MenName1, "Cargar directorio de Cancion con Pistas separados sin roll")
MenuItem(10062,MenName1, "Abrir Roll Grafico para una cancion cargada sin Roll")

MenuItem(1007,MenName1, "Grabar Cancion")
MenuItem(1008,MenName1, "Na.Grabar Cancion Como")
MenuItem(1009,MenName1, "Na.Exportar Cancion a midi")
MenuItem(1010,MenName1, "Cargar una Pista (rtk ? roll) externa en Cancion")
MenuItem(1011,MenName1, "Grabar una Pista de la Cancion con modificaciones, carga pista si no hubiera cargada")
MenuItem(1012,MenName1, "Copia una pista a otra  nueva en cancion")
MenuItem(1013,MenName1, "Na.Exportar Pista a midi")
MenuItem(1014,MenName1, "Grabar una Pista rtk a roll TrackaRoll")
Menubar(MenName1)
MenuItem(1015,MenName1, "MIDI-IN Grabar Pistas ejecucion")
MenuItem(1016,MenName1, "MIDI-IN Cargar Pistas ejecucion")
MenuItem(1017,MenName1, "Elegir MIDI-OUT o Driver o Port de Salida de pista previamente chequeda en S (sonido)")
MenuItem(1019,MenName1, "Salir")


MenuItem(1020,MenName2, "Nombre o Título (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "Tiempo I=60 por omision")
MenuItem(1022,MenName2, "Na.Ritmo 4/4 por omision")
MenuItem(1023,MenName2, "Na.Duracion Estimada Min.(Por Omision 3 estimada)")
MenuItem(1024,MenName2, "Na.Crear Cancion en un solo archivo")
MenuItem(1025,MenName2, "Crear un directorio de Cancion con Pistas separadas")
MenuItem(1026,MenName2, "Na.Ver Lista Tracks de la Cancion (Nombre y numero)")
MenuItem(1027,MenName2, "Na.Modificar Nombre de Pistas de Cancion")


MenuItem(1028,MenName3, "Cambia Octavas, si rango es mayor al anterior, se borran datos  (0,1,2,3,4,5,6,7,8)")
MenuItem(1029,MenName3, "Na.Seleccion rango de 3 octava repetidas 2 veces ")
MenuItem(1030,MenName3, "Na.Octavas de Instrumetnos Estandares")
MenuItem(1031,MenName3, "Na.Seleccion Canal (futuro se repetira por comodidad...)")
MenuItem(1040,MenName3, "Cambia Instrumento por orden Alfabetico")
MenuItem(1050,MenName3, "Cambia Instrumento por orden Numérico")
MenuItem(1060,MenName3, "Crea pista aislada con lo elegido y reemplaza la existente en la edicion")
MenuItem(1061,MenName3, "Crear Pista en la Cancion en Edicion, Con lo elegido")
MenuItem(1062,MenName3, "Crear Instancia de RollMusic Sin Control alguno Con lo elegido")

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
MenuItem(1082,MenName5,"Na. TEMPO por nombres, Lento,adagio etc y control fino")
MenuItem(1083,MenName5,"Na. TEMPO insertar cambio de tempo")
MenuItem(1084,MenName5,"Na. TEMPO borrar cambio de tempo")
MenuItem(1085,MenName5,"Na. TEMPO ver marcas de cambio de tempo")
MenuItem(1086,MenName5,"Na. TEMPO ocultar marcas de tempo")
MenuItem(1087,MenName5,"Na. TEMPO incremento de tempo gradual alcanzado en N compases")

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

MenuItem(1102,MenName7,"Fracciona Acorde [Con <> Duraciones], notas similares en una pista (no hay silencios)",MF_UNCHECKED  )
MenuItem(1103,MenName7,"Fracciona NOTA o Acorde [CDD], agregando silencios en una pista ",MF_UNCHECKED  )
MenuItem(1104,MenName7,"Fracciona [CDD], notas automaticamente en una pista ",MF_CHECKED  )
MenuItem(1105,MenName7,"No Fraccionar, NO Usar Acordes iguales ", MF_UNCHECKED )
MENUITEM(1106,MenName7,"Seleccionar TIPO DE ESCALA PRINCIPAL de la PISTA (Por omision Mayor)")
MENUITEM(1107,MenName7,"Seleccionar NOTA DE LA ESCALA ESCALA PRINCIPAL DE LA PISTA (Por omision C )")
MENUITEM(1108,MenName7,"Trabajar con sostenidos (Por omision Sostenidos #)",MF_CHECKED )
MENUITEM(1109,MenName7,"Trabajar con bemoles ",MF_UNCHECKED )
MenuItem(1111,MenName7,"Insertar escala libre en la Posicion actual (Pasozona1)")
MenuItem(1112,MenName7,"Insertar escala Alternativa de la Principal en la Posicion actual (Pasozona1)")
MenuItem(1113,MenName7,"Usar metronomo para Tocar MIDI-IN)",MF_CHECKED)

MenuItem(1200,MenName8,"Puerto MIDI-IN Ejecucion")
' MenuItem(1201,MenName8,"Na. Abrir      Puerto MIDI-IN")
' MenuItem(1202,MenName8,"Na. Cerrar    Puerto MIDI-IN")
' MenuItem(1203,MenName8,"Na. DesTruir Puerto MIDI-IN")
Menubar(MenName8)
MenuItem(1204,MenName8,"Puerto MIDI-OUT Ejecucion")
MenuItem(1205,MenName8,"Abrir   Puertos MIDI-OUT Ejecucion")
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
Dim As Integer k=0


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

           Case 1006, 10061   '<=========== CARGAR CANCION con roll
             'cargamos todos los tracks
             ' ok anda bien, una vez cagados se permuta en memoria con TAB
             ' o haciedno click en la lista
            '' UseGadgetList(hwndC)
               nombre=""
              ROLLCARGADO=FALSE 'NINGUN ARCHIVO ROLL CARGADO  
             Sleep 20
' resetea todo para limpiar y cargar cancion
             If NombreCancion > "" And cargaCancion=0 Then 
                  NombreCancion = ""
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  cargarDirectorioCancion(NombreCancion)
                  CANCIONCARGADA=False
                  ntk=0
                  If Tope >0 Then ' tenia datos se supone q pudo abrir Roll y abrirRoll=0
                     CargarPistasEnCancion ()
                     If tope=0 Then  ' directorio fallido
                        NombreCancion = ""
                        cargacancion=0
                        param.encancion=0
                        abrirRoll=2 ' roll ya esta abierto abre mas abajo
                     Else
                        cargacancion=1
                        param.encancion=1
                        abrirRoll=3 ' para evitar que abra rolloop de nuevo
                     EndIf

                  Else
                  ' si Tope=0 no cargo nada ni abrio roll posiblemente
                     NombreCancion = "" ' serai como una carga inicial...  
                     cargaCancion=1
                     abrirRoll=0 ' para que abra rolloop nunca abrio tal vez
                  EndIf   

             EndIf
             If NombreCancion = "" Then
                nombre=""
                ntk=0
               ' pistacreada=0
                CANCIONCARGADA=FALSE
' toma solo el nombre y path de la cancion no carga las pistas todavia
                cargarDirectorioCancion(NombreCancion)
                param.encancion=1
               If abrirRoll=2 Then ' ver rollloop roll esta cargado vengo a cargar cancion de nuevo
               ' por ejemplo tenia solo un roll abierto
                  param.encancion=0
                  ResetAllListBox(3)
                  Resetear (pmTk()) 
                  CargarPistasEnCancion ()
                  If tope=0 Then
                    NombreCancion = "" ' directorio fallido
                  EndIf
                  CANCIONCARGADA=True
                  param.encancion=1

               EndIf
             EndIf
             
             If abrirRoll=0 And NombreCancion > ""  Then
                abrirRoll=1
                cargaCancion=1
                Print #1,"SALE A CARGAR ROLL POR 1ERA VEZ ABRIRROLL=1 EXIT DO"
                Exit Do                 
             EndIf
  '           Print #1,"termino 1006 va a abrir Roll"
          SetForegroundWindow(hwnd)

         Case 10062
             Print #1," CASE 10062 abrirRoll=0 And NombreCancion > ", abrirRoll, NombreCancion
             If abrirRoll=0 And NombreCancion > ""  Then
                threadloop= ThreadCreate (@RollLoop,CPtr(Any Ptr, p1))   
                Print #1,"CARGO ROLL PARA cancion sin roll"
            ' ES TAN RAPIDO QUE PARECE EJECUTA DOS VECES EL 10062
            ' AL DEBUGUEAR NO LOA HACE ERGO PONEMOS UN RETARDO 0,1 SEG
                Sleep 100        
             EndIf
   

' ----------------------------------------------------------------------
           Case 1007 '<============ grabar cancion bosquejo
' 26-02-2022 desarrollo           
           If CANCIONCARGADA =True  Then
             Dim As String nombreg
            ROLLCARGADO=False 
           If NombreCancion > ""  Then
               GrabarCancion()
           EndIf
          MenuNew=0           
          carga=1
              
           EndIf
          SetForegroundWindow(hwnd)
           Case 1010 '<================ Cargar Pista externa a cancion
   '        Print #1,"entro a 1010 Cargar Pista externa a cancion"
           ROLLCARGADO=False 
            Dim As String nombreg
            
            getfiles(file,myfilter,"save")
            nombreg=*file.lpstrFile
            If nombreg = "" Then
               Print #1,"exit select por nombreg vacio "
               Exit Select 
            Else
               nombre=nombreg   
            EndIf
            If NombreCancion > ""  Then 
               ImportarPistaExterna() ' estoy en cancion importando  una pista rtk
            EndIf   
          MenuNew=0           
          carga=1
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
           ROLLCARGADO=FALSE 
            Dim As String nombreg
            If nombre = "" Then
               getfiles(file,myfilter,"save")
               nombreg=*file.lpstrFile
               If nombreg = "" Then
                  print #1,"exit select por nombreg vacio "
                  Exit Select 
               Else
                  nombre=nombreg   
               EndIf
           EndIf
           If NombreCancion > ""  Then 
              GrabarCopiadePista() ' estoy en cancion copiando una pista desde otra pista
           EndIf   
          MenuNew=0           
          carga=1
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
           Dim As Integer  pis=0
           For k=1 To 32 ' pistas ejec de grabaciondesde teclado
             If CheckBox_GetCheck( cbxejec(k))= 1  Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
                Exit For
             EndIf
           Next k

                  reDim  toc.trk(1 To tocaparam(pis).maxpos)

                  Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                  Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                '        Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
                   Next j
 Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal


' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
'threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi)  ' POR 1015
'  ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))



'-----------------------------------------------------------------------
           Case 1016 '<============Cargar MIDI-IN
' deberia borrar la cancion cargada ??? creo que si totdavia no ejecuta
' ambas cosas a la vez,,,,,debo almenos poner dos opciones mas
' borrar cancion cargada y borrar ejecucion cargada
      Dim As String nombrea,myfil
       print #1,"EN Cargar midi-in nombre ",nombreMidiIn
       ResetAllListBox(4)
      Dim As String lugar
       If  NombreCancion= "" Then
lugar= BrowseForFolder( NULL, "SELECCION DE CARPETA", BIF_RETURNONLYFSDIRS Or BIF_USENEWUI, "c:\" )
       Else
          lugar=NombreCancion
       EndIf
          If lugar = "" Then
          Else
''             nombreMidiIn=nombrea
             ''aca hay que hacerun loop
              Print #1,"lugar ";lugar
              'NTKP ES UNA SALIDA DE LA SUB
              CargarPistasEjec lugar, ntkp
  
              Dim j As integer
              For  j=1 To ntkp
                 If  tocaparam(j).nombre > "" Then
'nombre debe estar sin extension,las ejecuciones tienen un orden estricto
' vamos a tenerque igualar la cantidad de ticks en todas las pistas de modo
' que el ordende las pistas sea indistinto,elnumero de la pista ejec esta ensu archivo
'veremos si funciona cualqueira sea el orden en el disco alcargar se ordenara por
' ese numero Toca().orden,si funciona tal vez loaplicariamos a roll (mucho trabajo porahora queda asi) 
 
                   ntoca=j
                   pmTk(j+32).portout=tocaparam(j).portout
                   abrirPortoutEjec(j)
' en una carga abri los ports de salida pero todavia no los de entrada
' ergo el mycalback deberia tomar el patch si lo ajusto aca
'volver
'          ChangeProgram ( tocaparam(j).patch , tocaparam(j).canal, tocaparam(j).portout)
'       Print #1,"1016 tocaparam(j).patch ",tocaparam(j).patch
'       Print #1,"1016 tocaparam(j).canal ",tocaparam(j).canal
'       Print #1,"1016 tocaparam(j).portout ",tocaparam(j).portout
'
'-----------------06-06-2022-- abre portin siempre toca piano no toma el patch!!! no se como 
'      portin= CInt(tocaparam(j).portin)
'      If  listinAbierto( portin) = 0 Then
'              calltoca=j 'para el portout en mycallback
'              open_port (midiin(portin ), portin, *nombrein( portin ) )
'              set_callback midiin(portin ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
'  ignoreTypes(false, false, false);
 '             rtmidi_in_ignore_types  (midiin(portin ), 1, 2, 4)
 '             teclado=1 
 '             listinAbierto( portin) = 1
 '             jgrb=0
 '      End If
'-----------------06-06-2022 fin
               EndIf 
           Next j 

           tocatope=ntkp
      EndIf 
'-----------------------------------------------------------------------
            Case 1017 'seleccionar  por SALIDA de lapista ejecucion 

'-----------------------------------------------------------------------
            Case  1018 ' elegir PATCH si corresponde de la pista de ejecucion 

'-----------------------------------------------------------------------
           Case 1019  ''<============= SALIR TERMINA ROLL
            terminar=2
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
               selInstORdenAlfa (instru)
                If CANCIONCARGADA =TRUE  Then
               Else
                  'midisal = midiout(portout)
                  ntk=0
                EndIf
                portsal=pmTk(ntk).portout
                
 ' los canales van de 0 a 15 (1 a 16) no se si en todos los dispositivos
 ' van de 0 a 15 o en alguno de 1 a 16 opto por 0 a 15                
             ''  If pmTk(ntk).canalsalida > 0 Then
            '      ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de neuvo 13-02-2022 ï¿½
             ''  EndIf
               If instru=0 Then instru=1 EndIf
                Roll.trk(1,NA).inst= CUByte(instru)
                Track(ntk).trk(1,1).nnn=CUByte(instru)
              ' grabar la pistacomo en 1011
            print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg
              If CANCIONCARGADA =TRUE Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos > 2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undo¿?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  


              MenuNew=0           
              carga=1
        '     SetForegroundWindow(hwnd)    
'-----------------------------------------------------------------------
           Case 1050 ' <=========== seleccion de instrumento por orden Numerico
               selInstORdenNum (instru)
               If CANCIONCARGADA =TRUE Then
               Else
                 ' midisal = midiout(portout)
                 ntk=0
               EndIf
             ' no se cuadno funciona esto  si midisal y canal tienen valores 
             ' la seleccion de instrumento se ahce tanto par auna pista aislada como no
               portsal=pmTk(ntk).portout
      '         If pmTk(ntk).canalsalida > 0 Then
          '         ChangeProgram ( CUByte (instru) , pmTk(ntk).canalsalida,portsal) ' habilito de nuevo
       '        EndIf

               Roll.trk(1,NA).inst= CUByte(instru)
               Track(ntk).trk(1,1).nnn =CUByte(instru)
              ' grabar el track 
   '         print #1, "Click Grabando inst a disco pista con GrabarRollaTrack(0) ",nombre
            Dim As String nombreg

              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos >2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  

              MenuNew=0           
              carga=1
        '      SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1060 ' <========== crea track y reemplaza al existente en la edicion
               'If ntk=0 Then  ' no se cargo ningun track
               '   *po = hasta -1
               '   posn=1
               '   Nuevo(Roll,1 )
               '   posn=0
                  instruOld=instru
                  Roll.trk(1,NA).inst= CUByte(instru)
                  Track(ntk).trk(1,1).nnn= CUByte(instru)
                  'ChangeProgram ( CUByte (instru) , 0)
               'EndIf   
               If abrirRoll=0 Then
Print #1,"1060 abrirRoll=0 entro"
                  abrirRoll=1
                  cargacancion=0
                  If reiniciar=1 Then
                     ThreadDetach(threadloop)
                     usarmarcoOld=usarmarco
                     reiniciar=1
                  EndIf
                  If reiniciar=0 Then
                     reiniciar=1
                     usarmarcoOld=usarmarco
                  EndIf   
                  Print #1,"sale de 1060 abrirrol,reiniciar, cargacancion ", abrirRoll, reiniciar, cargacancion
                  Exit Do
               EndIf
          SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1061 ' <====== crear pista en cancion con lo elegido
               
               ntk = CountItemListBox(3)+ 1
   '            Print #1,"creando Pista nto ",ntk
               If ntk > 32 Then
                  print #1,"exit select ntk> 32"
                  Exit Select
               EndIf 

               If instru=0 Then 
                  instru=1
               EndIf
   '            print #1,"instru en 1061 , toma el ultimo ",instru
               NombrePista=RTrim(Mid(NombreInst(instru), 1,21))
   '            Print #1, "NombrePista en 1061 sin nro track ",NombrePista
   '            print #1,"porque se resetea? pathdir",pathdir
               If CANCIONCARGADA=true Or NombreCancion <> "" Then
                 ' armó el nombre de pista nuevo, pero permite modicifar 
               
                  EntrarNombrePista(NombrePista)
               EndIf
               'If NombrePista ="" Then
               ' NombrePista = "["+doscifras(ntk)+"]"+ RTrim(Mid(NombreInst(instru), 1,21))
               'Else
                NombrePista = "["+doscifras(ntk)+"]" + NombrePista 
                
               'EndIf
    '           print #1, "NombrePista en 1061",NombrePista
               AddListBoxItem(3, NombrePista)
               
              ' crear pista en disco 
               'MaxPos=2
               nombre= NombreCancion+"\"+NombrePista+".rtk"
    '           print #1,"nombre en 1061",nombre
               CantTicks=1000
    '           Print #1,"CantTicks ",CantTicks
               
               ''' para cuando las pistas esten juntas en un archivo ->ZGrabarTrack(ntk)
               If ntk=1 Then
                  ReDim (Roll.trk ) (1 To CantTicks,NB To NA)
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               Else
                  ReDim (Track(ntk).trk ) (1 To CantTicks,1 To lim3)
               EndIf
               ' EL TRACK SE CREA AL GrabarRollaTrack y debe tener CantTicks
               NB => 0 + (desde-1) * 13   
               NA => 11 + (hasta-1) * 13  

               titulos(ntk)=nombre
               pmTk(ntk).desde=desde
               pmTk(ntk).hasta=hasta
               pmTk(ntk).NB=NB
               pmTk(ntk).NA=NA                  
               pmTk(ntk).MaxPos=2
               pmTk(ntk).posn=0
               pmTk(ntk).notaold=0                  
               pmTk(ntk).Ticks=4000
               ' usamos encancion=1 para grabar dentro de la cancion

               NombrePista="" 
               posicion=1
               posn=0
               MaxPos=2
               nota=0
               dur=0
               tope=ntk
               *po = hasta -1
               GrabarTrack(ntk)
               abrirRoll=0
               If ntk=1 Then 
                  abrirRoll=1
                  cargacancion=0
                  CANCIONCARGADA=TRUE
               EndIf
               If ntk>=2 Then
                 cargacancion=1
                 abrirRoll=0
                 CANCIONCARGADA=FALSE
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
           HabilitarPatrones=GetStateMenu(hmessages,1068)
              Select Case HabilitarPatrones 
                     Case  3 
                    HabilitarPatrones=0
                    SetStateMenu(hmessages,1068,0)
                    SetStateMenu(hmessages,1064,1)
                    SetStateMenu(hmessages,1065,1)
                    SetStateMenu(hmessages,1066,1)
                    SetGadgetText(21,"        ")
                     Case 0
                    HabilitarPatrones=3
                    SetStateMenu(hmessages,1068,3)
                    SetStateMenu(hmessages,1064,0)
                    SetStateMenu(hmessages,1065,0)
                    SetStateMenu(hmessages,1066,0)

                    SetGadgetText(21,"PATRONES")
              End Select
           Case 1070
              nVerEscalasAuxiliares=GetStateMenu(hmessages,1070)
              Select Case nVerEscalasAuxiliares 
                     Case  3 
                    nVerEscalasAuxiliares=0
                    SetStateMenu(hmessages,1070,0)
                     Case 0
                    nVerEscalasAuxiliares=3
                    SetStateMenu(hmessages,1070,3)
              End Select
              SetForegroundWindow(hwnd)
'-----------------------------------------------------------------------
           Case 1071
              nVerCifradoAcordes=GetStateMenu(hmessages,1071)
              Select Case nVerCifradoAcordes 
                     Case  3 
                    nVerCifradoAcordes=0
                    SetStateMenu(hmessages,1071,0)
                     Case 0
                    nVerCifradoAcordes=3
                    SetStateMenu(hmessages,1071,3)

              End Select
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
          
          '    Dim As Any Ptr thplayC = ThreadCall  playCancion(track())
          '    CONTROL1 = 1
              If Cplay = 0 And MaxPos > 2 Then
                 GrabarPenta=0:naco=0:naco2=0
                 CPlay=1
                 If NombreCancion > "" Then
                    If play=1 Or playb=1 Then
                       CONTROL1=1 ' DETIENE EL PLAY VEREMOS
                       playloop=0:playloop2=0
                       play=0 : playb=0
                       Sleep 20
                    EndIf 
    '                Print #1,"USANDO PLAYCANCION"
                    thread1 = ThreadCall  playCancion(Track())
                 EndIf
             EndIf   

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
             abrirMIDIin=1
'If abrirMIDIin=1 Then ' grabacion desde teclado ...entr mal los valores por ahora
'   abrirMIDIin=0
'NTK??? vale cero al comienzo ergo es la ultima  ejecucion de trackas ...32+1 sera de ejec
'' çççç SEGUIRACA AJUSTE DEPORTOUT....05-05-2022
 ' deberiamos, 1) seleccionar el portIN de la pista ejec y luego abrir ejecutando esta accion
' se podra usar mycallback mas de 1 vez en distinto port ? supongo que si,.,,,
' SI ya esta abierto el portin no abrirlo por 2da vez...eso requiere controlar la apertura
' de los portin tambien JMGJMGJMG QUEDA PARA DESPUES AHORA VEREMOS APERTURA
' Y CIERRE EN DISPOSITIVOS....
 '----> seguir aca ÇÇÇÇÇÇ debo
   
       For  i As Short =1 To 32
             If  CheckBox_GetCheck( cbxgrab(i))= 1 Then
                portin  = CInt(tocaparam(i).portin) ' PREVIAMENTE SELECCIONADO
                portout= CInt(tocaparam(i).portout) ' PREVIAMENTE SELECCIONADO
                calltoca= i ' 04-06-2022
                ntoca=i
                portsal=portout

              ChangeProgram ( 1 , 1, 0)
                Exit For  ' termina con el 1er seleccionado solo se toma 1 sola accion
             EndIf
       Next i
/' lo bore  de mycallback
        For k As Short =1 To 32 
           If CheckBox_GetCheck( cbxejec(k))= 1  Then
              calltoca=k
Print #1,"DENTRO DE MYCALLBACK k ",k
Print #1,"tocaparam(k).patch ",tocaparam(k).patch
Print #1,"tocaparam(k).canal ",tocaparam(k).canal
Print #1,"tocaparam(k).portout ",tocaparam(k).portout
              ChangeProgram ( tocaparam(k).patch , tocaparam(k).canal, tocaparam(k).portout)
              Exit For
           EndIf
         Next k 
'/
Print #1,"listinAbierto( portin) ",listinAbierto( portin)
Print #1,"listInCreado(portin) ",listInCreado(portin) 
       If  listinAbierto( portin) = 0 Then
              If listInCreado(portin)  = 0 Then
                 midiin(portin) = rtmidi_in_create_default()
                 listInCreado(portin)  =1
             EndIf
Print #1,"abriendo portin y call back",*nombrein( portin )
              open_port (midiin(portin ), portin, *nombrein( portin ) )
              set_callback midiin(portin ), @mycallback, p
       ' por ahrao iognoramos otros tipsod de mensaje
              rtmidi_in_ignore_types  (midiin(portin ), 1, 2, 4)
              teclado=1 
              listinAbierto( portin) = 1
              jgrb=0
       End If

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
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                selTipoEscala (tipoescala_num)
                selNotaEscala (notaescala_num) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala_num
                guiaEscala(indEscala).notaescala=notaescala_num
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1111 TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
             EndIf
            SetForegroundWindow(hwnd)             
'-----------------------------------------------------------------------
           Case 1112 '<========= cambiode a escala Alternativa de la Principal
             If pasozona1 > 0 Then ' gurdamos en la posicion actual los valores cambiode escala
                Dim As String notastr
                'tipoescala_num,notaescala_num se pisan de modo q no hace falta inicializar
                EscalaAlternativa (tipoescala_num,notaescala_num) 
                cambioescala=1
                indEscala=indEscala+1

                guiaEscala(indEscala).tipoescala=tipoescala_num
                guiaEscala(indEscala).notaescala=notaescala_num
                If alteracion="sos" Then
                   guiaEscala(indEscala).alteracion=3
                EndIf 
                If alteracion="bem" Then
                   guiaEscala(indEscala).alteracion=2
                EndIf 
              Print #1,"1112 Alternativa TIPOESCALA NOTAESCALA ",tipoescala_num, notaescala_num
             EndIf
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
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 ntkp =i +32 
             EndIf
          Next i  
Print #1, "1] case 1200 portin, ntkp  ", portin, ntkp
         selportEjec (2,ntkp ) ' fix 13-03-23 enviamos el track es una seleccion para ese track
' en esa rutina se ajusta la global npi nuemro  de puerto entrada
         Print #1, "2] case 1200 portin, ntkp  ", portin, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portin entrado
' podemos asignar muchos a la vez 
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 pmTk(i+32).portin=CUByte(portin) ' evitamos el cero
             EndIf
          Next i  
      
'           Case 1201 'Abrir      Puertos MIDI-IN
'              listinAbierto(npi)=1
'           Case 1202'Cerrar    Puertos MIDI-IN
           Case 1203 'DesTruir Puertos MIDI-IN

           Case 1204 'Seleccionar      Puertos MIDI-OUT PARA EJECUCIONES
' seleccion de portout , 1:portout. ntkp:salida
' ->  npo: numero port salida
' portsin es la cantidad de ports que hay de entrada
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Then
                 ntkp =i +32 
             EndIf
          Next i  

         selportEjec (1,ntkp )
         Print #1, "portout, ntkp ",portout, ntkp
                   
' control de portin ,si hay un track de ejec seleccionado le asignamos este portin
' podemos asignar muchos a la vez 
          For i As Short =1 To 32
             If CheckBox_GetCheck( cbxejec(i))= 1 Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
                 pmTk(i+32).portout=CUByte(portout)
             EndIf
          Next i  

           Case 1205'Abrir      Puertos MIDI-OUT EJECUCIONES
'------------ABRIR PORT DE SALIDA ---<<<<< EJECUCIONES
 
Print #1,"abriendo port....si no se selecciona previamnete toma cero"
Dim k1 As Integer
 For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Or CheckBox_GetCheck( cbxgrab(i))= 1 Then
        k1=CInt(pmTk(i+32).portout)
        Print #1,"midiout ",k1, *nombreOut(k1)
        If InStr(*nombreOut(k1),"Microsoft")>0 Then
           Print #1,"No se usa Microsoft"
        Else
           If listoutAbierto( k1) = 0 Then
              If listoutCreado( k1) = 0 Then
                 midiout(k1) = rtmidi_out_create_default ( )
                 listoutCreado( k1) =1
              EndIf
              open_port midiout(k1),k1, nombreOut(k1)
              Dim As integer    porterror=Err
               listoutAbierto( k1) = 1
              Print #1,"1205 abro MIDI-OUT ",*nombreOut(k1)
              porterrorsub(porterror) 
          Else
              Print #1,"PORT YA ABIERTO",*nombreOut(k1)
          EndIf
      EndIf 
      Print #1,"Port usando en Play teclado ",portout
      Print #1,"-------------------------------------"
' el portsal de rtmidi empieza desde cero luego le resto 1
    ChangeProgram ( tocaparam(ntoca).patch, tocaparam(ntoca).canal, tocaparam(ntoca).portout-1)
    
    EndIf
 Next i

'-------------------------------

           Case 1206'Cerrar    Puertos MIDI-OUT de ejecucion play por el usuario
For  i As Short =1 To 32
    If CheckBox_GetCheck( cbxejec(i))= 1  Then
        Dim k1 As Integer
        k1=pmTk(i+32).portout 
        Print #1,"midiout ",k1, *nombreOut(k1)
        alloff( pmTk(i+32).canalsalida,k1 )  
        listoutAbierto(k1)=0
        close_port midiout(k1)
   EndIf
Next i 

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
       If eventnumber()=3 Then  ' VK_LBUTTON ?
         borrapos=0
 ' esto servia cuando cargaba solo la lista y no los tracks en 1006
 ' pero ahroa solo deberia hacer switch no cargar de disco sino
 ' directamente cargar Roll desde el numero de track correspondiente
 ' en memoria       
      '       print #1,"CLICK EN LISTA"
             ROLLCARGADO=FALSE
             CANCIONCARGADA=TRUE
             Dim item As String
             Dim As Integer ubi1,ubi2
          '   item= "                        "
          '   setgadgettext(4,item)
              
             item=GetListBoxText(3,GetItemListBox(3))
             Print #1,"item 1580 ",item  ' 28-02-2024 esto aparece en debug sin roll
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf
         '    setgadgettext(4,item)
             item=Trim(item)
      '       Print "item ",item
             If item > "" Then
             '  Dim nombre1 As String
             '   nombre1= NombreCancion + "\"+item +".rtk"
             '   print #1," NUEVO eventgadget click en lista nombre", nombre1
              ubirtk=3 ' ahora indice carga desde lista o memoria
             ' No mas de disco  cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0
             ' pero si quiero volver a disco solo debo resetear ubirtk=0
              ntk=sacarNtk(item) ' este ntk no sirve para boorar
 ' aca no copia track a Roll
         nombre= titulos(ntk)
      '   Print #1,"ntk, nombre ",ntk, nombre

'--------------------------------------------------------------

   clickpista=1 ' simula SC_TAB el cual carga el track a Roll
 
'--------------------------------------------------------------
' este ntk sirve para identificar el ntk del arcchivo t dle vector
' pero el ntk de la lista es otro vector y al borrar el indice cambia
' debo obtener el indice primero                
'' esta andando con defectos verlos borrado en la lista LBS_WANTKEYBOARDINPUT
                If WM_VKEYTOITEM Then '
       '           print #1,"---------->>> APRETO TEcla ",NTK,NombreCancion
                If EventKEY = VK_DELETE Then 
       '          print #1,"---------->>> APRETO DELETE ",NTK,NombreCancion
                  If NombreCancion > "" And ntk > 0  Then
                     borrar=2
                     DeleteListBoxItem(3,GetItemListBox(3))
        '            print #1,"LISTABOX EventKeyDown borrar ntk",ntk
         '           print #1,"LISTBOX  titulos(ntk)= ",titulos(ntk)
                    copiarATemp (titulos(ntk),pistas(ntk))
                    BorrarPista (titulos(ntk))
                    titulos(ntk)=""
                    pistas(ntk)=""
                    pmTk(ntk).desde=0
                    pmTk(ntk).hasta=0
                    pmTk(ntk).NB=0
                    pmTk(ntk).NA=0                  
                    pmTk(ntk).MaxPos=0
                    pmTk(ntk).posn=0
                    pmTk(ntk).notaold=0                  
                    pmTk(ntk).Ticks=0
                    pmTk(ntk).portout=portout
                    Sleep 1
                    'SetItemListBox(3,ind+1) no funca
                    'SetGadgetState(3,1) no funca 
                    borrar=0
                  EndIf
                EndIf 
 
           ' aca no debe leer a disco solo conmutar de track en track
            EndIf
                                  
         '       Print #1," CLICK EN LISTA FIN "
                  
             EndIf
  
       EndIf

'  CUAL PISTA DE ROLL SE ESCUCHA SEGUN LO SELECCIONADO
       If eventnumber()=CHECK_PISTA_ROLL Then
       
         If cntsuena =0 Then
            SuenaTodo=0
         EndIf

         If cntsuena =Tope And suenaTodo >= 1 Then
            SuenaTodo=1
            cntsuena=0
         EndIf
 
         Select Case  SuenaTodo
            Case 0
             SuenaTodo=1
           Case Is >= 1
             SuenaTodo=0
         End Select
         For i=1 To tope 
            CheckBox_SetCheck(ByVal cbxnum(i), SuenaTodo)
            cntsuena+=SuenaTodo
          Next i
         SuenaTodo=3
       EndIf
'------------------
' revisar CheckBox_GetCheck de las ejecuciones

'//////////////// BOTON ROJO COMIENZO GRABACION EJEC ////////////////// O PATRON
' llamar a un list  port y ajustar portout  
     If eventnumber()= BTN_MIDI_GRABAR And GrabarEjec=NoGrabar Then ' BOTON GRABAR ROJO
         jgrb=0:repro=0
         For k=1 To 32 
           If CheckBox_GetCheck( cbxgrab(k))= 1 Then 
              ntoca=k 'ntoca es la  pista ejec que se esta grabando
              calltoca=ntoca 'calltoca se usa en mycallback use otra variable , ver si conviene
              Exit For
           EndIf
         Next k

        
' mil negras a I=60 son 192 * mil ticks (16 minutos a I=60)
' a i=240 todo *4---192*4*1000=768000(16min a I=240)
'la idea es que el usuario grabe a I=60 o I=120 384000
' pero cada nota requiere 2 eventos on y off se multiplicaria por 2

         SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
         GrabarEjec=GrabarPistaEjecucion
         arrancaPlay=0

'metronomo de 4 pulsos para comenzar a grabar
 '    If  GrabarEjec=1 And metronomo_si=1 Then
 '       terminar_metronomo=0
 '       Dim As Integer im=0
 '       For im=1 To 4  
 '           noteon(60,60,1,0)
 '           noteoff(60,1,0)
 '           duracion(Timer, (60/tiempoPatron) / FactortiempoPatron)
 '       Next im
 '       threadmetronomo = ThreadCall metronomo()
             '    EndIf



     EndIf ' end event 10

'//////////////// BOTON NEGRO STOP EJEC  , GRABA A DISCO //////////////////

' 
      If eventnumber()= BTN_MIDI_PARAR   Or  GrabarEjec=GrabarPatronaDisco Then ' BOTON STOP NEGRO DE MIDI-IN
         SetGadgetstate(BTN_MIDI_GRABAR,BTN_LIBERADO)
         If GrabarEjec=GrabarPistaEjecucion  Or GrabarEjec=GrabarPatronaDisco Then
            Print #1,"STOP:pmTk(ntoca+32).MaxPos ",pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).maxpos=pmTk(ntoca+32).MaxPos
            tocaparam(ntoca).orden=CUByte(ntoca)
   '         Print #1,"stop MaxPos ",pmTk(ntoca).MaxPos
            GrabarEjec=NoGrabar
            repro=0
            arrancaPlay=0
' terminar cualquier metrono que este funcionando 
         terminar_metronomo=1
'detiene el play de cancion o roll
If  play=1 Or playb=1 Then
  CONTROL1=1 ' DETIENE EL PLAY DE CANCION O ROLL
   play=0: playb=0 
  playloop=0:playloop2=0
  SetGadgetstate(12,0)
  Sleep 2
EndIf
CONTROL2=1
Sleep 2
' -------cargamos toca
          k=0 
         Dim As Integer i1=1, j =0, partes, pj
   ' tocaparam(ntoca).delta toma valor desde la 2dapista grabada  en PlayTocaAll
         If  tocaparam(ntoca).delta > 0 And ntoca >1 Then
             partes=tocaparam(ntoca).delta/TickChico
             Print #1,"STOP: numero de partes de retardo ",partes 
             k=partes
             pmTk(ntoca+32).MaxPos=pmTk(ntoca+32).MaxPos+partes
'             Toca(ntoca).maxpos=pmTk(ntoca+32).MaxPos
'             print #1,"STOP Toca(ntoca).maxpos, ntoca ",Toca(ntoca).maxpos,ntoca
             For pj=1 To partes 
               Toca(ntoca).trk(pj).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(pj).nota = 0
               Toca(ntoca).trk(pj).vel  = 0
              Next pj

         EndIf
          k=partes+1
         Do 
           if k=pmTk(ntoca+32).MaxPos+1  Then
              Print #1," k=pmTk(ntoca+32).MaxPos+1, GrabaMidiIn "
              Exit Do
           EndIf  
     '  Print #1,"CargaIn(i1).modo ",CargaIn(i1).modo

            Select Case  CargaIn( i1).modo
               Case 144,128
            Toca(ntoca).trk(k).modo = CargaIn( i1).modo
            Toca(ntoca).trk(k).nota = CargaIn( i1).nota
            Toca(ntoca).trk(k).vel  = CargaIn( i1).vel
                 i1 = i1 +1    
              If i1=jgrb+1 Then
                 Exit Do 
              EndIf           
           End Select
           If CargaIn( i1).partes > 0 Then
           '   Print #1,"CargaIn(i1).partes ",CargaIn(i1).partes
              For j=1 To CargaIn( i1).partes 
                k=k+1
                
               Toca(ntoca).trk(k).modo = 1 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' un retardo de tick
               Toca(ntoca).trk(k).vel  = 0
            
              Next j
           Else
             k=k+1
               Toca(ntoca).trk(k).modo = 0 ' ojo, si modo=1 no se envia note on ni off
               Toca(ntoca).trk(k).nota = 0  ' no hay retardo de tick
               Toca(ntoca).trk(k).vel  = 0
                 
           EndIf
          k=k+1
         Loop
             jgrb=0
'----------------------grabar archivo de pista 
      Dim As String nombreg,myfil
  ' una cosa es grabar una pista y otra todas las pistas
  ' aca estamos grabando  una  sola pista,la marca da con G alpulsar el Boton Rojo
' esta funcion deberia estar en STOP 
      print #1,"EN Grabar midi-in nombre ",tocaparam(ntoca).nombre
''        nombreMidiIn=Toca(ntoca).nombre
   'y el path dondeesta? cuadnograbopareceque no 
  ' al cargar si debo afinar eso....   
   ReDim (toc.trk)(1 To tocaparam(ntoca).maxpos)

 
      Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",ntoca   
      Print #1,"tocaparam(ntoca).maxpos),ntoca ",tocaparam(ntoca).maxpos, ntoca
    
       For j As Integer =1 To   tocaparam(ntoca).maxpos
              toc.trk(j).modo=Toca(ntoca).trk(j).modo
              toc.trk(j).nota=Toca(ntoca).trk(j).nota
              toc.trk(j).vel=Toca(ntoca).trk(j).vel
'''''' VER DATOS            Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
       Next j
   Dim tocap As ejecparam = tocaparam(ntoca)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal

 
      maxgrb=tocap.maxpos
' para una sola pista grabada el maxgrb es el maxpos de esa pista
      If  maxgrb > 0 And ntoca > 1 Then
          If  maxgrb <  tocap.maxpos Then ' para cada pista se define tocap de nuevo  
              maxgrb=tocap.maxpos
'------------adapto la grabacion anterior a mayo rlongitud              
               ReDim  Preserve (Toca(ntoca-1).trk) (1 To maxgrb)
''''           pmTk(ntoca-1 +32).MaxPos=maxgrb ' conservo la long original
' el asunto es que igualo las  longitudes para que no reviente el play, pero
' mantengo las longitudes para menor espacio en disco al grabar
' al cargar reconstruyo en memoria igualo todo de nuevo         
          EndIf
      EndIf 
' si es grabacion nueva tocatope va incrementando apuntando a 1,2,3,4 etc
' y graba 1 por vez,el ultimo corriente...o actual
Print #1,"STOP: llama a GrabarMidiIn,,,porque  no esta grabando ahora? "   
' EN ROLLDEC 
'Type paramGrabamidi
'  As vivo toc
'  As Integer  tocatope
'  As ejecparam tocap'
'End Type

'EN ROLLDEC ...Dim  Shared  As paramGrabamidi pgmidi
pgmidi.toc=toc 'pgmidi es global
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap ' dentro esta orden  ubyte
'threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi) 'POR STOP aca se graba bien el orden
  'ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

         Else
            CONTROL2=1
            If play=1 Or playb=1 Then
               CONTROL1 =1
            EndIf   
            repro=0
            arrancaPlay=0
         EndIf


'----------------
      EndIf

'//////////////// BOTON VERDE PLAY EJEC //////////////////
' si hay una cancion de pistas trk el el grafico cargada, al dar play debera tocar
' lacancion y las pistas chequeadas en columna 'S' de las ejecuciones,deese modo
' sincronizaremos el arranque solamente. Esto se puede usar para escuchar o 
' al grabar una pista nueva de ejecuciones por uncontrolador midi.,(teclado midi por ej)
 
      If eventnumber()= BTN_MIDI_EJECUTAR And repro=0 Or  GrabarEjec =PatronDeEjecucionCompleto Then ' BOTON PLAY VERDE DE MIDI-IN
            repro=1
            CONTROL2=0
            CONTROL1=0
            Dim p As Integer Ptr
            p=@ntoca 'ntoca se ajusta en CargarPstasEjec tambien
            t1play=Timer
' tocar cancion de trakc si esta cargada 
' son 2 threadas que se inician casi simultanemente pero sin control entre ellos
' por ahora
' ACA DEBERIA USAR MUTEX!!! ï¿½ï¿½ï¿½???
'''Dim As Any Ptr sync =MutexCreate
Print #1,"MaxPos en play verde ejec deberia ser cero si no hay grafico ",MaxPos
        If  MaxPos > 2 Then  ''''' And GrabarEjec=1 And repro=1 Then 
            If CANCIONCARGADA = TRUE And playb=0 Then
               Print #1,"USANDO PLAYCANCION"
               playb=1   
               thread1 = ThreadCall  playCancion(Track())
            Else
               If  MaxPos > 2 And  Play=0 Then
          '        print #1,"llama a playall"
                   Play=1
 Print #1,"Va Play All ????,maxpos  ", MaxPos
                   thread1 = ThreadCall  playAll(Roll)
               EndIf 
            EndIf
       EndIf   
            threadG  = ThreadCreate (@PlayTocaAll, p)
            threadDetach(threadG)
        '  Print #1,"llama a  PlayTocaAll(p)"

         '   PlayTocaAll(p)
       EndIf
' test de retardos  de inicio en ejecucion de datos entre playCancion y PlayTocaAll
' CALCULO DE RETARDO DEL INICIO DE PLAY CANCION RESPECTO PLAYTOCAALL
'playTocaAll inicio datos:    9751.75934545541
'playcancion inicio datos:   9751.76207997309
' 9751,76208744816
' 9751,75934545541
'--------------------------
'       0,00274199275  seg= 2,7 mseg o sea la mitad del Tick (5 mseg)
'podemos decir que el inicio esta casi sincronizado solo un delta de medio Tick
'---- OJO ACA ESTAMOS GRABANDO PARANDO Y EJECUTANDO GRABACIONES
' DEL USUARIO PERO SOBRE ROLL SIN NECESIDAD DE ENTRADA MANULA!!!
'----------------------------------------------------------------------------------------------
'//////////////// BOTON ROJO GRABAR EN ROLL //////////////////

      If eventnumber()= BTN_ROLL_GRABAR_MIDI Then 
         GrabarPenta=1
      EndIf 
'-------------------------------
      If eventnumber()= BTN_ROLL_PARAR  Then
         SetGadgetstate(BTN_ROLL_EJECUTAR, BTN_LIBERADO)
         GrabarPenta=0
''      If NombreCancion > "" Then ' detiene todo pista aisalda o cancion 
            If play=1 Or playb=1 Or CPlay=1 Then
               CONTROL1=1 ' DETIENE EL PLAY 
               playloop=0:playloop2=0
               play=0 : playb=0:CPlay=0
               Sleep 20
            EndIf 
      ' EndIf
      EndIf   
' ///////////////// BOTON VERDE PLAY CANCION ROLL ////////  28-02-2024 GUIA
      If eventnumber()= BTN_ROLL_EJECUTAR Then ' 13-02-2024 PROBAR BIEN
         SetGadgetstate(BTN_ROLL_PARAR, BTN_LIBERADO)
         If Cplay = 0 And MaxPos > 2 Then
            CPlay=1
            If NombreCancion > "" Then
               If play=1 Or playb=1 Then
                  CONTROL1=1 ' DETIENE EL PLAY 
                  playloop=0:playloop2=0
                  play=0 : playb=0
                  Sleep 20
               EndIf 
               thread1 = ThreadCall  playCancion(Track())
            EndIf
         EndIf   

      EndIf
' ---------------- BOTONES PORTSAL VOL PATCH CANAL A LA DERECHA Y ARRIBA ...
'--------------------------------------------------------------------------------------------
' ////////////// PORT SAL EJEC ////////////////
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' osea el orden se crea al crear el nombre de la pista
      If  eventnumber()=BTN_EJEC_PORTSAL Then ' boton PortSal de track cbxnum o ejec cbxejec
          Dim As Integer miport =1, pis=0,num=0
          For k=1 To 32 
              If CheckBox_GetCheck( cbxejec(k))= 1 Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
             EndIf
          Next k
         If  pis >=1 Then 
      ' Si la pista tiene unnombre y tiene datos de ejecucion
             If  tocaparam(pis).nombre  >""  And  tocaparam(pis).maxpos > 0 Then
                 miport=1   ' 1= VA A seleccion port Salida
                 ntkp=pis
Dim As Integer k1 = pmTk(pis+32).portout
Print #1,"antes del cambio k1, listOutAbierto(k1) ", k1, listOutAbierto(k1)
Print #1,"pmTk(pis+32).portout previo al cambio",pmTk(pis+32).portout
     ''''     thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))

         selportEjec(miport,ntkp)
Print #1,"pmTk(pis+32).portout despues del cambio",pmTk(pis+32).portout

    '  preparamos la grabacion SI HAY DATOS por cambio de portsal

                ReDim  toc.trk(1 To tocaparam(pis).maxpos)
                Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
              
                 For j As Integer =1 To   tocaparam(pis).maxpos
                        toc.trk(j).modo=Toca(pis).trk(j).modo
                        toc.trk(j).nota=Toca(pis).trk(j).nota
                        toc.trk(j).vel=Toca(pis).trk(j).vel
                  '    Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
                 Next j
    Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal
    
    ' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje

pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi) ' por PORSAL
  '''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))


             Else ' no hay nombre y/o no hay datos
               miport=1   ' seleccion port Salida sin pista para tocar teclado
               ntkp=pis
               Print #1,"pmTk(pis+32).portout previo al cambio",pmTk(pis+32).portout
   ''              thread3 = ThreadCreate(@selportEjec(), CPtr(Any Ptr, miport))
               selportEjec(miport,ntkp)
               Print #1,"pmTk(pis+32).portout despues del cambio",pmTk(pis+32).portout
 
             EndIf
             Dim k1 As Integer
' buscamos  elport de esta pista
             Print #1,"pmTk(pis).portout cambiado ",pmTk(pis+32).portout
             k1=CInt(pmTk(pis+32).portout)
             Print #1,"k1 portout, listOutAbierto(k1) ", k1, listOutAbierto(k1)
             If listOutAbierto(k1)=0 Then  'abrir port
                If listoutCreado( k1)=0 Then
                    midiout(k1) = rtmidi_out_create_default ( )
                    listoutCreado( k1)=1
                EndIf
                open_port midiout(k1),k1, nombreOut(k1)
                Dim As integer    porterror=Err 
                listoutAbierto( k1) = 1
               Print #1,"abro ",*nombreOut(k1)
                porterrorsub(porterror)
            EndIf
        EndIf
      EndIf
'--------------  
      If  eventnumber()=BTN_EJEC_VOL Then ' VOL

      EndIf 
'--------------
      If  eventnumber()=BTN_EJEC_PAN Then 'PAN

      EndIf 
'----------------
'////////////////// PATCH EJEC /////////////////////////////
      If  eventnumber()=BTN_EJEC_PATCH Then 'PATCH o insrumento de un Sinte,,,
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

           Dim As Integer instrum =0, pis=0,num=0
           For k=1 To 32 ' pistas ejec de grabaciondesde teclado
             If CheckBox_GetCheck( cbxejec(k))= 1  Or CheckBox_GetCheck( cbxgrab(k))= 1 Then
                pis=k
                patchsal=CInt(tocaparam(pis).patch)
             EndIf
           Next k
           If  pis >=1 Then  
                If tocaparam(pis).nombre > ""  Then ''''And  tocaparam(pis).maxpos > 0  Then 
                   selInstORdenNum (instrum)
                    '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
                   Print #1," pista ejec  nro ",pis
                   tocaparam(pis).patch=CUByte (instrum)
                   pmTk(pis+32).patch=CUByte (instrum)
                   patchsal=instrum
                   ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
                   Print #1,"ejecucion patch elegido tocaparam(pis).patch ", tocaparam(pis).patch
'--------------------------
' preparamos para grabar la pista por cambio de patch

                  ReDim  toc.trk(1 To tocaparam(pis).maxpos)

                  Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                  Print #1,"tocaparam(pis).maxpos),ntoca ",tocaparam(pis).maxpos, pis
                
                   For j As Integer =1 To   tocaparam(pis).maxpos
                          toc.trk(j).modo=Toca(pis).trk(j).modo
                          toc.trk(j).nota=Toca(pis).trk(j).nota
                          toc.trk(j).vel=Toca(pis).trk(j).vel
                '        Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
                   Next j
    Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal

' aca es diferente elchequeo me da el nro de la pista, en estecaso =eje
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi) ' POR PATCH
  'ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

             Else
                  patchsal=1
                  instru=patchsal
                  selInstORdenNum (instrum)
                   '''thread3 = ThreadCreate(@selInstORdenNum (), CPtr(Any Ptr, instrum))
                  Print #1," pista ejec  nro ",pis
                  tocaparam(pis).patch=CUByte (instrum)
                  pmTk(pis+32).patch=CUByte (instrum)
                  ChangeProgram ( tocaparam(pis).patch , tocaparam(pis).canal, tocaparam(pis).portout)
                  Print #1,"ejecucion patch elegido tocaparam(pis).patch ", tocaparam(pis).patch
             EndIf
         EndIf
      EndIf 


'////////////////// CANAL EJEC ///////////////// 
    If  eventnumber()=BTN_EJEC_CANAL Then ' CANAL de un synthe por ejemplo
' si todavia no grabe nada tocaparam tendra el nombre y el orden
' o sea el orden se crea al crear el nombre de la pista

          Dim As Integer canal =0, pis=0,num=0
         For k=1 To 32 ' pistas ejec de grabaciondesde teclado
           If CheckBox_GetCheck( cbxejec(k))= 1 Or CheckBox_GetCheck( cbxgrab(k))= 1  Then
              pis=k
           EndIf
         Next k

         If  pis >=1  Then  
             If tocaparam(pis).nombre > ""  And  tocaparam(pis).maxpos > 0  Then 
                 selcanalEjec (1,pis) ' 1 salida
                 Print #1," pista ejec  nro ",pis
                 tocaparam(pis).canal =pmTk(pis+32).canalsalida
                 Print #1,"ejecucion canal elegido tocaparam(pis).canal ", tocaparam(pis).canal
    '--------------------------
    ' preparamos para grabar la pista por cambio de patch
                ReDim  (toc.trk)(1 To tocaparam(pis).maxpos)
                Print #1,"----------datos almacenados en toc()-------------pista midiin----> ",pis   
                Print #1,"tocaparam(pis).maxpos),pis ",tocaparam(pis).maxpos, pis
        ' PREPARAMOS PARA GRABAR A ARCHIVO
                For j As Integer =1 To   tocaparam(pis).maxpos
                     toc.trk(j).modo=Toca(pis).trk(j).modo
                     toc.trk(j).nota=Toca(pis).trk(j).nota
                     toc.trk(j).vel=Toca(pis).trk(j).vel
               '    Print #1, toc(j).modo;" ";toc(j).nota;" ";toc(j).vel
               Next j
   Dim tocap As ejecparam = tocaparam(pis)
                Print #1,"PARAMETROS EJEC nombre ",tocap.nombre
                Print #1,"PARAMETROS EJEC mapos ",tocap.maxpos
                Print #1,"PARAMETROS EJEC orden ",tocap.orden
                Print #1,"PARAMETROS EJEC delta ",tocap.delta
                Print #1,"PARAMETROS EJEC portout ",tocap.portout
                Print #1,"PARAMETROS EJEC patch ",tocap.patch
                Print #1,"PARAMETROS EJEC canal ",tocap.canal
   
pgmidi.toc=toc
'pgmidi.tocatope = tocatope
pgmidi.tocap = tocap
threadGrabamidi=@pgmidi
GrabarMidiIn(pgmidi)  'POR CANAL
  ''ThreadCreate (@GrabarMidiIn,CPtr(Any Ptr, threadGrabamidi))

            Else
                selcanalEjec (1,pis) ' 1 salida
                Print #1," pista ejec  nro ",pis
                tocaparam(pis).canal =pmTk(pis+32).canalsalida
                Print #1,"ejecucion canal elegido tocaparam(pis).canal ", tocaparam(pis).canal
            EndIf
         EndIf
'-----------------------
         For k=1 To 32 ' pistastrack de cancion
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=k
           EndIf
         Next k 
         If  num >=1 Then
             selcanal (1)
            
         EndIf

    EndIf

''' en base alanterior terminar esta parte que es para pistas de cancion manual
'' mas adelante....cuando termine todo pistas ejec 
''para pistas de cancion manual futuro ???pero si ya hay para pistas manual??
'//////////////// SEL PORT DE ROLL O MANUALES O CANCION
      If  eventnumber()=BTN_ROLL_PORTSAL  And cierroport= 0 Then
         Dim As Integer miport =1, pis=0,num=0
         cierroport=1 ' asi entra una sola vez,,,
         For k=1 To 32 
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=1
           EndIf
         Next k
' miport=1 estamos seleccionadno port de salida , de entrada es 2 midi in
        If  num=1 Then  ' se chequeop una pista no importa cual
         thread2 = ThreadCreate(@selport(), CPtr(Any Ptr, miport))
        EndIf
            
     EndIf 
'-------------------
'////////////////// BOTON PATCH ROLL O CANCION O MANUAL /////////////////////////////
' futuro todas estos codigos de  case si son parecidos luego  algun dia 
' los convertiremos en rutinas,,,JMG RECORDAR...!
      If  eventnumber()=BTN_ROLL_PATCH Then 'PATCH o insrumento de un Sinte,,,
' //////// PATCH PARA CANCION PERO NO GRABA A DISCO,,    
'Print #1,"EN BTN_ROLL_PATCH" 
         Dim  as Integer num = 0  , instrum =0             
         For k=1 To 32 ' pistastrack de cancion
           If CheckBox_GetCheck( cbxnum(k))= 1  Then
              num=k
              instrum=CInt(pmTk(num).patch)  'TOMA LO QUE EXISTE EN EL A RCHIVO
' toma la 1era de arrib  abajo el resto las ignora si hay mas chequeadas
' y si instrum es > 0 es un cambio
              Exit For
           EndIf
         Next k 
'         Print #1, "PATCH . num,instrum ", num, instrum
         If  num >=1 Then
             selInstORdenNum (instrum)
              '''thread2 = ThreadCreate(@selInstORdenNum(), CPtr(Any Ptr, instrum))
 '             Print #1, "patch instrum seleccionado ", instrum
             pmTk(num).patch=CUByte(instrum)
             If CANCIONCARGADA =TRUE Then
             Else
                      ntk=0
             EndIf
            portsal=pmTk(ntk).portout
 '           Print #1, "patch portsal almacenado, instru ", portsal, instrum
            Roll.trk(1,NA).inst= CUByte(instrum)
            Track(ntk).trk(1,1).nnn =CUByte(instrum)
            Dim As String nombreg

              If CANCIONCARGADA =TRUE  Or TRACKCARGADO =TRUE Then
                 If NombreCancion > ""  And MAxPos >2 Then
                    GrabarRollaTrack(0)
                 EndIf
              Else
                If MaxPos > 2  And ROLLCARGADO  Then
                  'aca graba el roll con Roll.trk(1,NA).inst
                 GrabarArchivo (0) ' graba roll en edicion, borro todo el undoï¿½?
                 ' no el undo dolo se debe borrar al ahcer nuevo creo
                EndIf  
              EndIf  
              carga=1 ' control de carga, anula calcompas durante la carga ,,etc

        EndIf

      End If


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

salir() ''<==== SALIR TERMINA ROLL
'Sleep 100
'Kill "procesos.txt"
FileFlush (-1)
    cerrar 0
    cerrar 1
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


