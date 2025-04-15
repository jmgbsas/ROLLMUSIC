'ROLLCTRLGUI... GUI DE VENTANA DE CONTROL
On Error Goto errorhandler
Static Shared As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8,MenName10
Static Shared As HMENU MenName31,MenName32,MenName18,MenName19,MenName30 

If instancia < 3 And ubirtk=0 And ubiroll=0 And menuabierto=0 Then ' rollmusic CON control
  menuabierto=1 ' evita apertura de mas de un menu
  instancia=0
  
  hwndC = OpenWindow("RollMusic Ctrl V "+ nroversion,10,10,ANCHOSYSTEM*0.91 ,ALTOSYSTEM*0.91, _
   WS_OVERLAPPEDWINDOW Or WS_VISIBLE ,  WS_EX_ACCEPTFILES   )
' cancela  AddKeyboardShortcut(hwndC,FCONTROL,VK_A,1006) 'CTRL+A ABRIR PISTAS cancion
'cancela  AddKeyboardShortcut(hwndC,FCONTROL,VK_E,1016) 'CTRL+E ABRIR PISTAS ejecucion
'' ------TIPS AYUDA EN LA BARRA DE ESTADO
   StatusBarGadget(33,"Tips de ayuda" )

  'CenterWindow(hwndC)
''UpdateInfoXserver()
Var bitmap = Load_image("fondo.bmp")
BRUSH = WindowBackgroundImage(hwndC,bitmap,1)

  hwndListBox= ListBoxGadget(PISTASROLL,80,40,290,685,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT Or LBS_NOINTEGRALHEIGHT Or LBS_NOTIFY  )
  GadgetToolTip(PISTASROLL,"Pistas de Cancion con Tracks manuales cargados desde un archivo o creados en una cancion o convertidos e importados desde ejecucion real midi" )
  SetGadgetFont(PISTASROLL,CINT(LoadFont("consolas bold",13))) 

' botton todo o nada , sonido o mudo para todas las pistas
  ButtonGadget(BOTON_PISTA_ROLL, 60,20,20,20,"S")
  SendMessage(GadgetID(PISTASROLL),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
 ' TextGadget(4,250,10,240,20,, SS_SIMPLE  )
 ' 04-02-2023

  SetGadgetColor(PISTASROLL,CInt("&HC0C0C0"),0,1)
  SetGadgetColor(BOTON_PISTA_ROLL,cint("&HC0C0C0"),0,1)
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
  hwndListEjec= ListBoxGadget(PISTASEJECUCIONES, 430,40,290,685,LBS_EXTENDEDSEL Or LBS_DISABLENOSCROLL  Or WS_VSCROLL Or WS_HSCROLL Or LBS_WANTKEYBOARDINPUT Or LBS_NOINTEGRALHEIGHT Or LBS_NOTIFY  )
SetGadgetFont(PISTASEJECUCIONES,CINT(LoadFont("consolas bold",13)))
GadgetToolTip(PISTASEJECUCIONES,"Pistas grabadas desde un teclado midi")
SetGadgetColor(PISTASEJECUCIONES,cint("&HC0C0C0"),0,1)
  ButtonGadget(BOTON_SELECCION_EJECUCION ,410,20,20,20,"S")
SetGadgetColor(BOTON_SELECCION_EJECUCION ,cint("&HC0C0C0"),0,1)

  

  SendMessage(GadgetID(PISTASEJECUCIONES),LB_SETHORIZONTALEXTENT,450,0) ' width scroll = 430 pixels
' BS_PUSHLIKE se hune el boton al seelccioanrlo
' CHECK PARA ESCUCHAR SONIDO de las ejecuciones desde teclado grabadas
  cbxejec(1) =  CheckBox_New( 410 ,  40, 20, 20, "",, hwndC) 
  cbxejec(2) =  CheckBox_New( 410 ,  60, 20, 20, "",, hwndC)
  cbxejec(3) =  CheckBox_New( 410 ,  80, 20, 20, "",, hwndC)
  cbxejec(4) =  CheckBox_New( 410 , 100, 20, 20, "",, hwndC)
  cbxejec(5) =  CheckBox_New( 410 , 120, 20, 20, "",, hwndC)
  cbxejec(6) =  CheckBox_New( 410 , 140, 20, 20, "",, hwndC) 
  cbxejec(7) =  CheckBox_New( 410 , 160, 20, 20, "",, hwndC) 
  cbxejec(8) =  CheckBox_New( 410 , 180, 20, 20, "",, hwndC)
  cbxejec(9) =  CheckBox_New( 410 , 200, 20, 20, "",, hwndC)
  cbxejec(10) = CheckBox_New( 410 , 220, 20, 20, "",, hwndC)
  cbxejec(11) = CheckBox_New( 410 , 240, 20, 20, "",, hwndC)
  cbxejec(12) = CheckBox_New( 410 , 260, 20, 20, "",, hwndC)
  cbxejec(13) = CheckBox_New( 410 , 280, 20, 20, "",, hwndC) 
  cbxejec(14) = CheckBox_New( 410 , 300, 20, 20, "",, hwndC) 
  cbxejec(15) = CheckBox_New( 410 , 320, 20, 20, "",, hwndC)
  cbxejec(16) = CheckBox_New( 410 , 340, 20, 20, "",, hwndC)
  cbxejec(17) = CheckBox_New( 410 , 360, 20, 20, "",, hwndc)
  cbxejec(18) = CheckBox_New( 410 , 380, 20, 20, "",, hwndc)
  cbxejec(19) = CheckBox_New( 410 , 400, 20, 20, "",, hwndc) 
  cbxejec(20) = CheckBox_New( 410 , 420, 20, 20, "",, hwndc) 
  cbxejec(21) = CheckBox_New( 410 , 440, 20, 20, "",, hwndc)
  cbxejec(22) = CheckBox_New( 410 , 460, 20, 20, "",, hwndc)
  cbxejec(23) = CheckBox_New( 410 , 480, 20, 20, "",, hwndc)
  cbxejec(24) = CheckBox_New( 410 , 500, 20, 20, "",, hwndc)
  cbxejec(25) = CheckBox_New( 410 , 520, 20, 20, "",, hwndc) 
  cbxejec(26) = CheckBox_New( 410 , 540, 20, 20, "",, hwndc) 
  cbxejec(27) = CheckBox_New( 410 , 560, 20, 20, "",, hwndc)
  cbxejec(28) = CheckBox_New( 410 , 580, 20, 20, "",, hwndc)
  cbxejec(29) = CheckBox_New( 410 , 600, 20, 20, "",, hwndc)
  cbxejec(30) = CheckBox_New( 410 , 620, 20, 20, "",, hwndc)
  cbxejec(31) = CheckBox_New( 410 , 640, 20, 20, "",, hwndc)
  cbxejec(32) = CheckBox_New( 410 , 660, 20, 20, "",, hwndc) 
'---------------------------------------------------------
' CHECK PARA GRABAR O SEA ARMA LA PISTA PARA RECIBIR DATOS MIDI-IN
  ButtonGadget(CHECK_GRABAR_EJECUCION,380,20,20,20,"G")  
 ' ButtonGadget(8,450,0,100,30,"PARAR",BS_RADIOBUTTON )
 ' ButtonGadget(9,580,0,120,30,"GRABAR",BS_RADIOBUTTON  )
Var IMGP=Load_image(".\recur\Parar.bmp")
Var IMGG=Load_image(".\recur\Grabar.bmp")
Var IMGE=Load_image(".\recur\Ejec.bmp")
SetGadgetColor(CHECK_GRABAR_EJECUCION,cint("&HC0C0C0"),0,1)

' pistas de ejec MIDI-IN
GroupGadget(GRUPO_BTNS_MIDI,445,0,113,40,"")

ButtonImageGadget(BTN_MIDI_PARAR,450,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_MIDI_PARAR,"DETENER ejecucion o grabacion midi")

ButtonImageGadget(BTN_MIDI_GRABAR,490,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_MIDI_GRABAR,"GRABAR ejecucion midi")

ButtonImageGadget(BTN_MIDI_EJECUTAR,530,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_MIDI_EJECUTAR,"EJECUTAR Grabacion midi")

 TextGadget(21,570,12,95,20,"         ")
' pistas manuales  PARA CARGAR CANCION DESDE DIRECTORIO PISTAS ECHAS CON ROLL
GroupGadget( GRUPO_BTNS_MANUAL,95,0,113,40,"") 'play cancion
GadgetToolTip(GRUPO_BTNS_MANUAL,"DETENER Cancion, Ejecutar Cancion, Grabar midi en Roll ")

ButtonImageGadget(BTN_ROLL_PARAR, 100,12,25,25,IMGP, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_ROLL_PARAR,"DETENER ejecucion cancion o Grabacion midi sobre Roll")

ButtonImageGadget(BTN_ROLL_EJECUTAR, 140,12,25,25,IMGE, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_ROLL_EJECUTAR,"EJECUTAR cancion Tracks, o Grabacion midi Roll ")

ButtonImageGadget(BTN_ROLL_GRABAR_MIDI, 180,12,25,25,IMGG, FB_BS_PUSHLIKE or BS_BITMAP  )
GadgetToolTip(BTN_ROLL_GRABAR_MIDI,"GRABAR midi en Roll")


 'rbparar = RadioButton_New( 450 , 10, 40, 20, "P",BS_LEFTTEXT , hwndC) '65
 'rbgrabar =RadioButton_New( 500 , 10, 40, 20, "G",, hwndC) ' 80





' checks para habilitar grabacion en una pista de MIDI-IN
  cbxgrab(1) =  CheckBox_New( 380 ,  40, 20, 20, "",, hwndC) 
  cbxgrab(2) =  CheckBox_New( 380 ,  60, 20, 20, "",, hwndC)
  cbxgrab(3) =  CheckBox_New( 380 ,  80, 20, 20, "",, hwndC)
  cbxgrab(4) =  CheckBox_New( 380 , 100, 20, 20, "",, hwndC)
  cbxgrab(5) =  CheckBox_New( 380 , 120, 20, 20, "",, hwndC)
  cbxgrab(6) =  CheckBox_New( 380 , 140, 20, 20, "",, hwndC) 
  cbxgrab(7) =  CheckBox_New( 380 , 160, 20, 20, "",, hwndC) 
  cbxgrab(8) =  CheckBox_New( 380 , 180, 20, 20, "",, hwndC)
  cbxgrab(9) =  CheckBox_New( 380 , 200, 20, 20, "",, hwndC)
  cbxgrab(10) = CheckBox_New( 380 , 220, 20, 20, "",, hwndC)
  cbxgrab(11) = CheckBox_New( 380 , 240, 20, 20, "",, hwndC)
  cbxgrab(12) = CheckBox_New( 380 , 260, 20, 20, "",, hwndC)
  cbxgrab(13) = CheckBox_New( 380 , 280, 20, 20, "",, hwndC) 
  cbxgrab(14) = CheckBox_New( 380 , 300, 20, 20, "",, hwndC) 
  cbxgrab(15) = CheckBox_New( 380 , 320, 20, 20, "",, hwndC)
  cbxgrab(16) = CheckBox_New( 380 , 340, 20, 20, "",, hwndC)
  cbxgrab(17) = CheckBox_New( 380 , 360, 20, 20, "",, hwndc)
  cbxgrab(18) = CheckBox_New( 380 , 380, 20, 20, "",, hwndc)
  cbxgrab(19) = CheckBox_New( 380 , 400, 20, 20, "",, hwndc) 
  cbxgrab(20) = CheckBox_New( 380 , 420, 20, 20, "",, hwndc) 
  cbxgrab(21) = CheckBox_New( 380 , 440, 20, 20, "",, hwndc)
  cbxgrab(22) = CheckBox_New( 380 , 460, 20, 20, "",, hwndc)
  cbxgrab(23) = CheckBox_New( 380 , 480, 20, 20, "",, hwndc)
  cbxgrab(24) = CheckBox_New( 380 , 500, 20, 20, "",, hwndc)
  cbxgrab(25) = CheckBox_New( 380 , 520, 20, 20, "",, hwndc) 
  cbxgrab(26) = CheckBox_New( 380 , 540, 20, 20, "",, hwndc) 
  cbxgrab(27) = CheckBox_New( 380 , 560, 20, 20, "",, hwndc)
  cbxgrab(28) = CheckBox_New( 380 , 580, 20, 20, "",, hwndc)
  cbxgrab(29) = CheckBox_New( 380 , 600, 20, 20, "",, hwndc)
  cbxgrab(30) = CheckBox_New( 380 , 620, 20, 20, "",, hwndc)
  cbxgrab(31) = CheckBox_New( 380 , 640, 20, 20, "",, hwndc)
  cbxgrab(32) = CheckBox_New( 380 , 660, 20, 20, "",, hwndc) 

'---------------------------------------------------------------------
' port de salida ,Volumen y Paneo para ejecuciones y tracks manuales...
' solo se da click en 'S' de sonido de la pista y luego uno de estos3 botones..
' se ajusta asi en una pista de track o ejecucion por vez el port de salida o 
' midi-out...igual para volumen y paneo,si sequiereajustar mas de uno a la vez
' simplemente se da click en todos los deseados y elajuste sera el mismo en cada uno
' o sea S tiene doblefuncion desmutear odar sonido o ajustar alguno de los 3 parametros.  
ButtonGadget(BTN_EJEC_PORTSAL,420,730,70,20,"PortSal")
'ButtonGadget(BTN_EJEC_VOL,   490, 730, 35, 20, "Vol")
'ButtonGadget(BTN_EJEC_PAN,   530, 730, 35, 20,"Pan")
ButtonGadget(BTN_EJEC_PATCH,  570,730, 50, 20,"Patch")
ButtonGadget(BTN_EJEC_CANAL,  620,730, 50, 20,"Canal")

'---------------------------LISTVIEW ROLL MANUALES
ButtonGadget(BTN_ROLL_PORTSAL, 70,730,70,20,"PortSal")
'ButtonGadget(BTN_ROLL_VOL,   140, 730, 35, 20, "Vol")
'ButtonGadget(BTN_ROLL_PAN,   180, 730, 35, 20,"Pan")
ButtonGadget(BTN_ROLL_PATCH,  220,730, 50, 20,"Patch")
ButtonGadget(BTN_ROLL_CANAL,  280,730, 50, 20,"Canal")



'StatusBarGadget(1,"StatusBarGadget")

  hMessages=Create_Menu()

  MenName1=MenuTitle(hMessages, "ARCHIVO")
  MenName2=MenuTitle(hMessages, "NUEVA CANCION")
  MenName3=MenuTitle(hMessages, "PISTAS MANUALES")
  
  'MenName31=MenuTitle(hMessages,"Patrones")

  MenName4=MenuTitle(hMessages,"VER")
  MenName5=MenuTitle(hMessages,"TIEMPO Y RITMO")
  MenName6=MenuTitle(hMessages,"REPRODUCIR")
  MenName7=MenuTitle(hMessages,"OPCIONES")
  MenName8=MenuTitle(hMessages,"PUERTOS DE EJECUCION")
  MenName10=MenuTitle(hMessages,"INFO")

''MenuItem(1005,MenName1, "Na.Cargar archivo de Cancion")
MenuItem(1006,MenName1, "1.0 Cargar directorio de Cancion con Pistas separados con Ventana de Control y Roll Grafico")
Menubar(MenName1)
MenuItem(10061,MenName1,"2.0 Abrir directorio de Cancion con Pistas separados con Ventana de Control  sin Roll Grafico")
MenuItem(10062,MenName1,"2.1 Abrir Roll Grafico dependiente de Control si se uso la opcion (2.0) o se cerro el grafico. ")
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
MenuItem(10075,MenName1,"4.4 Cargar Pista (rtk o roll) en Roll aislado ")

Menubar(MenName1)

MenName18=OpenSubmenu(MenName1, "4.5 Cargar Pista/Cancion para Exportar a midi durante Reproduccion")
MenuItem(1008,MenName18, "Cargar Pista en Roll independiente" )
MenuItem(10081,MenName18,"Usar Cancion cargada en 2.0 " )

    

MenuItem(1009,MenName1,  "4.5.1 Exportar a MIDI Pista/cancion de 4.5 ")


Menubar(MenName1)
MenuItem(1015,MenName1, "5.0 GRABAR PISTA EJECUCION MIDI-IN")
MenuItem(1016,MenName1, "5.1 CARGAR PISTAS EJECUCION MIDI-IN")
MenuItem(1017,MenName1, "5.2 RENOMBRAR PISTA EJECUCION SELECCIONADA")

MenuItem(10181,MenName1,"5.3 Cargar txt plano midi generado por RollMusic")
MenuItem(1018 ,MenName1,"5.4 Cargar txt plano midi externo, MUY INEXACTO")
Menubar(MenName1)
MenuItem(1019,MenName1, "    SALIR")


MenuItem(1020,MenName2, "1.0 Nombre o Título (fecha por omision), la cancion es un directorio")
MenuItem(1021,MenName2, "2.0 Tiempo I=60 por omision")
MenuItem(1022,MenName2, "3.0 Na.Ritmo 4/4 por omision")
MenuItem(1023,MenName2, "4.0 Na.Duracion Estimada Min.(Por Omision 3 estimada)")
'MenuItem(1024,MenName2, "Na.Crear Cancion en un solo archivo")
MenuItem(1025,MenName2, "5.0 Crear un directorio de Cancion con Pistas separadas")
'MenuItem(1026,MenName2, "Na.Ver Lista Tracks de la Cancion (Nombre y numero)")
MenuItem(1027,MenName2, "6.0 Na.Modificar Nombre de Pistas de Cancion")


MenuItem(1028,MenName3, "1.0 Cambia Octavas, si rango es mayor al anterior, se borran datos  (0,1,2,3,4,5,6,7,8)")
MenuItem(1029,MenName3, "2.0 Na.Seleccion rango de 3 octava repetidas 2 veces ")
MenuItem(1030,MenName3, "3.0 Na.Octavas de Instrumetnos Estandares")
'MenuItem(1031,MenName3, "Na.Seleccion Canal (futuro se repetira por comodidad...)")
Menubar(MenName3)
MenuItem(1040,MenName3, "4.0 Cambia Instrumento por orden Alfabetico")
MenuItem(1050,MenName3, "5.0 Cambia Instrumento por orden Numérico")
Menubar(MenName3)
MenuItem(1060,MenName3, "6.0 Crear pista aislada, En Roll dependiente, con lo elegido y reemplaza la existente en la edicion")
MenuItem(1061,MenName3, "7.0 Crear Pista nueva en la Cancion en Edicion, Con lo elegido")
Menubar(MenName3)
MenuItem(1062,MenName3, "8.0 Crear Instancia de RollMusic Sin Control alguno Con lo elegido")
' ahora se llama 10063 en menu artchivo 


'MenuItem(1065,MenName31, "Crear Patrones de Ejecuciones por Teclado",MF_POPUP )
'MenName32=OpenSubmenu(MenName31, "Na/Crear Patrones de Ejecuciones por Teclado" )
'MenuItem(1064,MenName32,"Nombre del Patron")
'MenuItem(1065,MenName32,"Numero de Compases del Patron")
'MenuItem(1066,MenName32,"Crear Secuencia de  Patrones insertados")
'MenuItem(1067,MenName31, "Na. Crear Patrones de Ediciones manuales")
'MenuItem(1068,MenName32,"Habilitar Patrones",MF_UNCHECKED)


MenuItem(1070,MenName4,"1.0 Escalas auxiliares ajustadas", MF_CHECKED)
MenuItem(1071,MenName4,"2.0 Cifrado de Acordes", MF_CHECKED)
MenuItem(1072,MenName4,"3.0 Separacion de Notas en pantalla de 1 en adelante  ")

Menubar(MenName4)
'MenuItem(1072,MenName4,"Na/Parametros de un archivo Roll  ")
'MenuItem(1073,MenName4,"Na/Parametros de un archivo Track ")
'MenuItem(1074,MenName4,"Na/Parametros de Roll y Track(0) en memoria")
'MenuItem(1075,MenName4,"Na/Parametros de Track(n) en memoria ")

  
MenuItem(1080,MenName5,"1.0 TEMPO, Por omision=60, Ejecucion Tick por omision=5mseg equivale a 240")
MenuItem(1081,MenName5,"2.0 Factor para Aumentar velocidad de ejecucion, No se graba en archivo 1,5 o 0,5 etc")


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

  
MenuItem(1090,MenName6,"1.0 Reproducir desde la posicion o en el rango ajustado")
MenuItem(1091,MenName6,"2.0 Fijar Repeticiones de un numero de Compases elegido como zona")
MenuItem(1092,MenName6,"3.0 Abre y Reproduce Puerto MIDI-IN Ejecucion teclado por  MIDI-OUT.")
MenuItem(1093,MenName6,"4.0 Cierra Puerto MIDI-IN Ejecucion.")


MenuItem(1100,MenName7,"1.0 Usar MARCO de Ventana Para el Gráfico",MF_UNCHECKED)
MenuItem(1101,MenName7,"2.0  Usar MARCO de Ventana en instancias Gráficas",MF_UNCHECKED)
'Menubar(MenName7)
'MenuItem(1102,MenName7,"Fracciona Acorde [Con <> Duraciones], notas similares en una pista (no hay silencios)",MF_UNCHECKED  )
'MenuItem(1103,MenName7,"Fracciona NOTA o Acorde [CDD], agregando silencios en una pista ",MF_UNCHECKED  )
'MenuItem(1104,MenName7,"Fracciona [CDD], notas automaticamente en una pista ",MF_CHECKED  )
'MenuItem(1105,MenName7,"No Fraccionar, NO Usar Acordes iguales ", MF_UNCHECKED )
Menubar(MenName7)
MENUITEM(1106,MenName7,"3.0 Seleccionar TIPO DE ESCALA PRINCIPAL de la PISTA (Por omision Mayor)")
MENUITEM(1107,MenName7,"4.0 Seleccionar NOTA DE LA ESCALA ESCALA PRINCIPAL DE LA PISTA (Por omision C )")
MenuItem(1111,MenName7,"5.0 Insertar escala libre en la Posicion actual (Pasozona1)")
MenuItem(1112,MenName7,"6.0 Insertar escala Alternativa de la Principal en la Posicion actual (Pasozona1)")
Menubar(MenName7)
MENUITEM(1108,MenName7,"7.0 Trabajar con sostenidos (Por omision Sostenidos #)",MF_CHECKED )
MENUITEM(1109,MenName7,"8.0 Trabajar con bemoles ",MF_UNCHECKED )
Menubar(MenName7)
MenuItem(1113,MenName7,"9.0 Usar metronomo para Tocar MIDI-IN",MF_CHECKED)
MenuItem(1114,MenName7,"10.0 Usar sonido de pista para pulsos de inicio de grabacion",MF_UNCHECKED)


MenuItem(1200,MenName8,"1.0 Seleccion Puerto MIDI-IN Ejecucion")

Menubar(MenName8)
MenuItem(1204,MenName8,"2.0 1Seleccion de Puerto MIDI-OUT Ejecucion")
MenuItem(1205,MenName8,"3.0 Abrir  Puertos MIDI-OUT Ejecucion")
MenuItem(1206,MenName8,"4.0 Cerrar Puertos MIDI-OUT Ejecucion")
Menuitem(1207,MenName8,"5.0 Convertir una pista iluminada de ejecucion o archivo cargado de *.ejec a *.roll en memoria y lo cargue en Roll Grafico previamente abierto.")
Menuitem(1208,MenName8,"6.0 Convertir todas las ejecuciones *.ejec seleccionadas para que toquen columna [S] a *.rtk y los grabe a disco")


MenuItem(2000,MenName10,"1.0 Acerca de")
MenuItem(2001,MenName10,"2.0 Cuadro Ayuda TEMPO por nombres, Lento,adagio etc y control fino")
MenuItem(2002,MenName10,"3.0 Cuadro de Figuras de duracion de notas")

'MenuItem(2502,MenName30,"Seleccion Puerto MIDI-IN ROLL")
'MenuItem(2504,MenName30,"Seleccion Puerto MIDI-OUT ROLL")
'MenuItem(2505,MenName30,"Abrir  Puerto  MIDI-OUT ROLL")
'MenuItem(2506,MenName30,"Cerrar Puertos MIDI-OUT ROLL")
'Menubar(MenName30)
'MenuItem(2500,MenName30,"Abre   Puerto MIDI-IN Sobre Roll")
'MenuItem(2501,MenName30,"Cierra Puerto MIDI-IN Sobre Roll")


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
''OJO EL MENU "ROLLCTRLGUI.BI" ES REENTRANTE TODO ESTO SE EJECUTA
'' COMO SI ESTUVIERA EN UN LOOP
' ERGO TODAS ESTAS CONDICIONES INICIALES SE REPITEN
' LAS PASAMOS A ROLLCONTROLGUI.BI
' default de FRACCIOANR autodur 
   usarAcordesIguales=1
   TipoFrac="autodur"
metronomo_si=3 'no funciona
sonidopista_si=0 ' sonido por midi out si
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
  '' habilito grabar cancion punto 2) SetStateMenu(hmessages,1007,1)

  SetStateMenu(hmessages,1009,1)
'' sin seleccionar portin y portout no se permite abrir midiin teclado
  
'   Print #1,"deshabilita 1092 y 1093  al inicio >>>>>>>>>>>>>"
    SetStateMenu(hmessages,1092,1)
    SetStateMenu(hmessages,1093,1)

    SetStateMenu(hmessages,2500,1)
    SetStateMenu(hmessages,2501,1)


End If