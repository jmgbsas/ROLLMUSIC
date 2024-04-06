'ROLLCTRLGUI... GUI DE VENTANA DE CONTROL
Static Shared As HMENU hMessages,MenName1,MenName2,MenName3,MenName4,MenName5,MenName6,MenName7,MenName8,MenName10
Static Shared As HMENU MenName31,MenName32 
If instancia < 3 And ubirtk=0 And ubiroll=0 And menuabierto=0 Then ' rollmusic CON control
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
'MenuItem(10071,MenName1, "Na.Grabar Cancion Como")
MenuItem(1008,MenName1, "3.1 Exportar Pista a midi durante Reproduccion")
MenuItem(1009,MenName1, "Na.3.2 Exportar Cancion a midi, No desarrollado ")
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
' ahora se llama 10063 en menu artchivo 


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

MenuItem(1082,MenName5,"Cuadro Ayuda TEMPO por nombres, Lento,adagio etc y control fino")
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
