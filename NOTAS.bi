'01-12-2021
' crearpente cursor y playAll se comunican par amover el cursor simplemente por variables globales
' de modo que por ejemplo si yo tengo PlayAll funionando apenas empieza el play ajusta una Global a
' start o 1 de comienzo yo llamo a PlayAll como thread puedo llamar como thread a otro play
' pero de eventos grabados desde un teclado y sincronizo ambos loops por una variable..
' l aposicion x de la secuencia , puedo llamar pero que no comienza que quede en esperra en un loop
' al llegar la señal de comienzo se dispara el loop mas interno para lograr esto mejro se disparan los
' escalvos primero que se quedan esperando y luego al conductor el playall o playcancion
' lso cuales van enviando la posicion o columna tocada con la variable global como la jply
' con ella el for en el eje x toma los valores de jply .. si uso un For el toma el comando
' debo encontrar algo que reemplace al for o sea que el for del Play indirectamente controle la ejecucion de 
' el otro proceso. El otro proceso serai otro thread en loop con un if dentro del loop que pregunte por
' 3 condiciones si las varaibel sglobales son > 0 se ejecuta el proceso al terminar el loop
' lo vuelve al principio y ahi x o y tendrian ptrp valor se puede preguntar si cambio,,,si no cambio
' se espera que cambie...
' si x e y son cero se ejeuta un retardo 1 o 2 mseg digamos....
' si x e y son negativos se sale del loop fin , y el thread termina por detach...
' se complicaria al sincronizaion pero es posible ...
' lo mas practivo seria que los track grabados graben directo a lso tracks existentes y se lo tome
' como cualqueir track o agregar al mismo vector tracks especiales...
' voy atener que empezar a usar ticks creo....

'-----------------------------------
' IMPLEMENTAR LINKED LIST CON XOR https://www.geeksforgeeks.org/xor-linked-list-a-memory-efficient-doubly-linked-list-set-2/
' HACER UNA LIBRERIA EN C? Y USARAL DESDE FREEBASIC? O IMPLEMENTAR TODO EN FREEBASIC?
' se corrigio lso 4 byte  para maxpos solo permitia 65000 posiciones se agrepgo un byte mas a5 y5 .pb
' se arreglo como crear nuevas pistas desde control con lo elegido, luego de creada la pista solo
' existe en la lista de Control, para grabar la a disco usar ROLL visualizacion y dalre grabar , como es un rtk
' le asignara el nrk siguiente si es 0 va ser 1 y asi sucecivamente,
' 21-09-2021 este codigo permite clickear en un alista
' y segun elnombre ir a disco y cargarlo, pero ya no necesito eso
' ahroa sera solo permutar de track y se pasara a Roll en memoria no de disco pero lo guardo
' este anda muy bien tambien...ademsa si siempre traido de disco no
' conservare las modificaciones, se perderian
' ListBoxGadget(3... es el eventnumber()=3
 
      Case eventgadget
         If eventnumber()=3 Then
 ' esto servi acuando cargaba solo la ista y no los track sen 1006
 ' pero ahroa solo deberi ahcer switch no cargar de disco sino
 ' directamente cargar Roll desdeel nuemro de track correspondiente        
             Dim item As String
             item= "                        "
             setgadgettext(4,item)
              
             item=GetListBoxText(3,GetItemListBox(3))
             If Len (item) < 24 Then
               item = item + String( 40-Len(item),32)
             EndIf
             setgadgettext(4,item)
             item=Trim(item)
             If item > "" Then
                nombre= NombreCancion + "\"+item +".rtk"
                Print #1," eventgadget click en lista nombre", nombre
                lineadecomando=1
                cargarTrack (Track(), ntk) ' este ntk se resuelve dentro de la sub
             ' donde se lo saca del nombre por lotanto devuelve el numero de ntk
             ' despues dela rutina,cargarTrack pone a 0 lineadecomadno=0 
                TrackaRoll (Track(), ntk , Roll ) ' no usa lineadecomando
                Print #1,"ntk cargado, nombre ",ntk, nombre
                ReCalCompas(Roll)
                item=""
             EndIf  
         EndIf


' 19-08-2021: ahora har redimensionar con preserve entonces cuando cargo un arhivo
' las ocatvas se acomodan per qu epasa con el instrumento? rta
' si n otenia instrumento queda la ajsutada en la llamada con RollMusicControll
' pero si lo tenia que hace lo pisa? spongo que si redim preserve, preservara
' pero luego se copia de Trabajo y asi pisa logicamente,,,pero inst debia estar
' en cero en trabajo y al pisar no lo piso !!! rarisimo!!!! ah no es raro
' lo que pasa es que si lo pisa pero el ChangeProgram no se envio 
' entonce swueda el ultimo valor !!!
' 14-07-2021: debo tener una pantalla inicial que permita agregar track
' agregado un track se solicita editarlo y se copia el nro de track 
' en track de edicion, se guarda el nro de track en trackold..
's evuelve al menu y se agrega otro track el 2, se solicita editar el 2
' para ello se copia el trackold en su correspondiente track (track =trackol)
' se edita el track2...y asi sucesivamente
' si se solicita grabar , se copia el track en edicion al trackold
' y se graban todos los track en una carpeta de tracks con el nombre de la cancion
' --------------------------------------------------------------
' para cargar una cansion se selecciona la carpeta de la cancion, luego
' se carga cada track en su correspondiente vector de roll n
'---------------------------------------------------------------
' esta forma tan simple de tracks y cancion permite al usuario, copiar
' tracks cambiarlos de posicion, dentro de la misma cancion o pasar directamenet
' algunso track a otra cancion,,,
'-----------------------------------------------------------
' simbolos de velocidades tipicas meso fuerte legato etc
' http://www.music-software-development.com/midi-tutorial.html 
' investigar bload and save texto en help lo encntre con chunk
'BLOAD/BSAVE text mode work-around These functions allow you to use BSAVE and BLOAD in a text mode.'
' https://github.com/dharmatype/Bebas-Kai
'
'Sub _bsave( file As String, p As Any Ptr, sz As Integer ) 
'
 ' Dim As Integer ff 
'  ff = FreeFile 
'  
'  Open file For Binary As ff 
'    fb_fileput( ff, 0, ByVal p, sz ) 
'    
'  Close 
'  
'End Sub '
'
'Sub _bload( file As String, p As Any Ptr ) ''
'
'  Dim As Integer ff 
'  ff = FreeFile 
'  
'  Open file For Binary As ff 
'    fb_fileget( ff, 0, ByVal p, LOF( ff ) ) 
'    
'  Close 
'  
'End Sub
'' Cairo Based GUI widgets [Update 2.20]
'' https://www.freebasic.net/forum/viewtopic.php?f=7&t=17319&p=152001&hilit=MENU+AND+CAIRO#p152001
''
'' menu easy
'' https://www.freebasic.net/forum/viewtopic.php?f=6&t=28855&p=276452&hilit=menu+easy#p276452
'' TAREAPARA EL FUTURO IMPLEMENTAR DESHACER CTRL-Z
'' OTRO MANUAL DE CAIRO http://www.non-gnu.org/guile-cairo/docs/html/index.html#Top
'' sdl2 usando mouse!!! NO HACE FLTA FREEBASIC TIENE MOUSE EVENT
'' https://github.com/DevAccess/SDL2-OpenGL-RPi/blob/master/code/main.cpp
'' cairo tutorial https://www.cairographics.org/tutorial/#L1understandingtext
'' - sdl2 and cairo ,busqueda de googleesta bueno...
''  1 ) https://www.cairographics.org/SDL/
''     DEMOS https://cgit.freedesktop.org/~joonas/cairosdl/tree/?h=demos
'' bajado en fbedit/sdl-dev/cairo....
'' acahay mucho codigo https://cgit.freedesktop.org/
'' https://cgit.freedesktop.org/cairo/ es un mirror
'' cairo's central repository (mirrored from https://gitlab.freedesktop.org/cairo/cairo)
'' https://www.cairographics.org/samples/
''  download the latest SDL development kit from http://www.libsdl.org.
''  http://www.libsdl.org./download-2.0.php me baje pra mingw   
'' WIKI http://wiki.libsdl.org/FrontPage
'' https://www.onlineprogrammingbooks.com/
'' LOOP BASICO sdl http://gamedevgeek.com/tutorials/getting-started-with-sdl/ 
'' https://valadoc.org/cairo/index.htm tiene sobre cairo vala compilacon gcc 
'' instalado en msys2 ylo probe valawork
'' https://valadoc.org/sdl2/index.htm
'' interfce de C con FB http://bourabai.kz/einf/freebasic/TutInterfacingWithC.html
'' Using FreeBASIC Built Libraries with GCC http://bourabai.kz/einf/freebasic/TutUsingLibrariesWithGCC.html
'' GITtoBAC de Gnome a Freebasic (vala)
'GirToBac
'Postby TJF » Aug 11, 2013 19:39'
'Image
'GirToBac is a tool to generate FreeBasic (FB) header files for libraries based on GObject. 
' For these libraries a GOject-Introspection file (suffix *.gir) can get generated 
' containing all information to create language bindings for high-level programming languages
' like JavaScript, Vala and others. The aim is to support polyglot application development.
' GirToBac is the first approach to connect the FreeBasic programming language to 
' this tool-chain for easy creating and up-dating FreeBasic header files of GObject 
' based libraries (ie like GTK, GDA, GooCanvas, ...). A set of example headers 
' is available here.
' Find more information in the online-documentation (or download here for offline-reading).
' The source code and some example files are available at
' http://www.freebasic-portal.de/downloads/ressourcencompiler/girtobac-296.html
' COMPLETOS HEADERS DE CAIRO !!! SE SACAN DE ESO:<== NO CONVIENE DAN PROBLEMAS 2009!!
' https://www.freebasic.net/forum/viewtopic.php?f=14&t=21488
' GNOME FREEBASIC https://www.freebasic.net/forum/viewtopic.php?f=14&t=21488&p=190157#p190157
' GNOME HEADERS -> https://www.freebasic-portal.de/downloads/bibliotheken/gtk-3-header-dateien-fuer-freebasic-191.html
' Installation instructions:
' download the package and uncompress it
' copy (or move) folder GnomeHeaders-1.0/include/Gir to your FreeBASIC header folder .../freebasic/include/
' if you need full cairo support, replace files in .../freebasic/include/cairo by the files from folder GnomeHeaders-1.0/include/cairo
' make sure that you have installed the library binaries
' compile and test examples in GnomeHeaders-1.0/examples
' Array OPTIMO: USAR a c-like array management using zstring ptr, or FB-like array 
' managed using lzae (whenever mature one day)
' https://code.google.com/archive/p/fb-extended-lib/downloads?page=1
' https://www.slant.co/topics/983/~best-cross-platform-gui-toolkits
' TOOLS T RESOURCES !! - ABAJO COMERCIAL VENDE TUTI
' http://www.music-software-development.com/ NO
' http://lilypond.org/doc/v2.18/Documentation/notation/creating-midi-files
' CON LILIPOND PUEDO INTERFACEAR PARA HACER COSAS
' http://lilypond.org/doc/v2.18/Documentation/notation/index
' utf8 http://www.nubaria.com/en/blog/?p=289
' DRAG AND DROP DEMO https://www.freebasic.net/forum/viewtopic.php?t=19586
' http://www.non-gnu.org/guile-cairo/docs/html/Scaled-Fonts.html#Scaled-Fonts 
' ALGO SOBRE TECLADOS EN C GETCH
' https://stackoverflow.com/questions/10463201/getch-and-arrow-codes
'' DRAG MOUSE https://devblogs.microsoft.com/oldnewthing/20100304-00/?p=14733
' SM_CXDRAG EN WINUSER.BI
' GLFW AND CAIRO PERO X11 LINUX UFF https://discourse.glfw.org/t/glfw-and-cairo-use/1697
' CAIRO-GL LIBTARY ME FALTA .!! 
' TUTORIAL https://docs.tizen.org/application/native/api/mobile/2.4/group__CAPI__CAIRO__EVAS__GL__MODULE.html
' CAIRO BUENA DESSCRIPCION 
' https://www.cairographics.org/manual-1.2.0/cairo-cairo-surface-t.html
' INVESTIGAR ESTO EL OFFSET !!!!! URGENTE
'cairo_surface_set_device_offset ()
'
'void                cairo_surface_set_device_offset     (cairo_surface_t *surface,
'                                                         double x_offset,
'                                                         double y_offset);
'
'Sets an offset that is added to the device coordinates determined by the CTM when drawing to surface. 
' One use case for this function is when we want to create a cairo_surface_t that redirects drawing for 
' a portion of an onscreen surface to an offscreen surface in a way that is completely invisible to 
' the user of the cairo API. Setting a transformation via cairo_translate() isn't sufficient to do this,
' since functions like cairo_device_to_user() will expose the hidden offset.
'
'Note that the offset affects drawing to the surface as well as using the surface in a source pattern.
'
'surface :
'	a cairo_surface_t
'
'x_offset :
'	the offset in the X direction, in device units
'
'y_offset :
'	the offset in the Y direction, in device units
'-------------
' cairo_surface_get_device_offset ()
'
'void                cairo_surface_get_device_offset     (cairo_surface_t *surface,
'                                                         double *x_offset,
'                                                         double *y_offset);
'
'This function returns the previous device offset set by cairo_surface_set_device_offset().
'
'surface :
'	a cairo_surface_t
'
'x_offset :
'	the offset in the X direction, in device units
'
'y_offset :
'	the offset in the Y direction, in device units
'
'Since 1.2 
' CAIROGRAPHICS.ORG---https://www.cairographics.org/manual-1.2.0/pt02.html
'*************************************************
' manejo de notas en el vector Roll echo hasta ahora
' 1 ) puede ingresar notas pulsando directamente en el teclado
' sus nombre CDEFGAB enminusculas o en mayusculas para
' sus respectivos sostenidos, se elige la oactava automaticemente
' con solo estar el icono del mouse en cualqueirparte de dicha octava
' solo queseencuentre encima de la misma nadamas.
' se graba sus valores en dicho vector con su posicion.
' si en esa posicion y para esa octava y nota existe ya
' una nota se incrementa la posicion. Siemrpe se arte de
' la posicion 1 por ahora para escriir las notas de cada fila
' Acordes,quedan grabados en la misma posicion...
' todavia no se coloco las duraciones de las notas...sera facil
' e inmediato por tecldo OPILFEW....o lasfiguras graficas...
' 1) cuando se coloque un silencio debera colocarselo en todas las notas
' que el usuaio decida en esa octava, para ello esas notas deberan pertenecer 
' a un mismo instrumento activo para esas notas.
' 2) lso instrumentos o canal midi se podra seleccionar para un grupo
' de notas contiguos en este roll u otros.
' 3) podria haber 1 Roll para cda instrumento o para cada canal o
' un mismo roll ejecutar mas de 1 instrumento a la vez pero en distintos
' grupos de notas u octavas del mismo Roll
'4) IMPORTNTE ARESLVER YA: una vez cargdas las notas para 
' una posicion y si no se desea ninguna otra nota en esa posicion
' ya se puede incrementar la posicion con flecha derecha o volver 
' atras con flecha izquierda, colocar el nro de posicion visible...
' siquiero volver a una posicion anterior debo indicrlo tmbien
' en elmismo lugar visible y a eleccion del usuario colocar una
' linea vertical para esa octava de donde estoy parado al comienzo
' de la posicion,
'5) las notas serán separadas visualmente por un espacio de letra..
' acciones: a) seprar visualmente las notas...ok listo.
'colocar el nro de posicion visible, o pongo un numerito enalgun lugar
' o coloco un cuadrito de color blnco o amarillo que indique donde estoy
' asi como un cursor d eedicion de texto!! si es lo mejro luego 
' pondre un indicador de numero en algun lugar me aprece mas 
' importnte el cursor! que algundia opdra cambairse de forma,,
'CAIRO OPERADORES COMO UN DIBUJO SE IMPRIMESOBRE OTRO
'https://www.cairographics.org/operators/
'-=-=-=-=-=-=-=-=-
'ya se construye melodiassepeude cambiar l duraciondena Not o reemplaarl por un 
'silenciode su misma duracion o onerun espacio(nos epara que serviria je)
'-hora haremos paginado horiontal l cntidad de posiciones que hay son 66
'si fuesen posiciones de negras seria 14,5 compases, si fuesen redondas
'serian 66 compases, si fuesen corcheas 8,5 compases...
'cuano se llega al posicion 64 o 66 hay que hcer scroll derecho..rop codigo
'o sea borrar datos y empear a dibujardesdela posicion 65 o mejrodede el compas
'siguiente o del pedzo de compas que haya quedado incompleto....
'para ello hay que calcular cuantos compases hy segun las duraciones
'cargadas, considereremos por hora 4/4. o seaa 4 negras enun compas.
'Independientede las durciones podemos empezarcon desplazamiento desde 
'la 65, quedndo 64 invisibles,,,,paraello el vector ROLL y Duracion
'deberan leerse desde la posicion 65 en adelante, un nuev variable que 
'cuente la cntidad de paginas horiontles de 64 cada una recorrida...
'para retroceder se saltara por compas o sea 4 posiciones o por las
'64 posiciones +o - .o +4 -4-=-listo mnso a la obra
'finalmente pude con cairo ajustar font true type..si!
' ubicacion de una nota por sus pixels x e y-
'  Penta_y = BordeSupRoll + 14 * ( inc_Penta ) *( nro -1) base de medida de Roll
'  cairo_move_to(c, 0, Penta_y + semitono * inc_Penta- 6) <--es la linea de c/nota 
'  BordeSupRoll = Int((ALTO ) /18) ' (1050 )/18 integer = 58
'  inc_Penta = Int((ALTO - BordeSupRoll) /(40)) ' 26 double 1330/66 para 1400 resolu
'  Inc_Penta = int((1050 -58 )/40) = 24
'con esos valores puedo determinar cualqiernota de las 96 es todo calculo 
' ANCHO Y ALTO LO CONOSCO
' Penta_y = 58 + 14 * (24) * (nro-1) = 58  para octava =1
' Penta_y = 58 + 14 * 24 * 1 = 394
' cairo_move_to 

'18-09-2021 DOBLE CLIK EN UN ARCHIVO RTK O ROLL ABRE EL PROGRAM Y CARGA EL ARCHIVO
' SI LA EXTENSION ESTA ASOCIADA AL PROGRAMA
' 09-09-2021: error yendo y viniendo con octavas, las notas cargadas en la misma
' octava suenen diferente hay corrimiento de 1 octava parece chequear
' dragar ventana desde cinta fix. en mousex=1048 anda mejor
' VERSION DE PRUENA 0.4.1.0.0 INTEGRAMOS UNA GUI Y 2 GRAFICAS UNA SIN USO
' PREPARADO PARA 2 PANTALLAS GRAFICAS LA OTRA ES OPENGL WIN COMENTADA  
' COMPILAMOS CON fbc64  -s gui rollMusicControl.bas RollMusic.bas -x RollMusic.exe
' con el tiempo pasar toa a e.scancode  event If e.scancode = SC_P And Play=1
' anduvo mejor sino no cortaba el play en secuecnias largas...
' se puede usar llamdo desde ROllMusicControl o standAlone.
' 18-08-2021 envio de cambio de programa patch INSTRUMENTO EN ROLL.TRK(1,1)
' FUNCIONO,,,
' Correccion de notas cuadno se usa pocas OCTAVAS PianoNota no es correcta. LISTO OK
' USAR CALLROLL CON SHELL OK LISTO
' ROLLMUSIC-0.3.0.0.0-U-TRACKS CARGA Y GRABACION DE TRACKS ANDA BIEN,
' para conectar instancias podriamos usar ZEROMQ
' O ALGUN PTR A UNA UDT CON DATOS COMUNES 
' AHROA LE PUSIMOS REDIM PRESERVE PARA TODO ANDA OK, DE MODO QUE EL VECTOR ROLL
' ES CHICO AL INICIO Y SE VA AGRANDANDO SEGUN NECESIDAD 
' ROLLMUSIC-0.1.0.0.0-U-TRACKS
' ADOPTAMOS LIBRERIA PAR LEER Y GRABAR MIDI midilib de github
'---------------------------------------------
' PLAN 0.1.0.0.0-U-TRACKS :
' i) Vamos a testear si funciona la conversion 01-08-2021
' ii) dejamos por ahora la creacion de songs por medio de tracks con formato Roll
' y pasamos a grabar perfomance real midi desde teclado en tracks midi de Roll o midi con midilib, 
' pero para grabar usamos un tick o pulso de ritmo o una pista de roll o track ya
' grabada pro el usuario y a medida que la escucha puede empezar a tocar
' al 5to pulso [1 2 3 4],5 y con ese ritmo pedimos toque el usuario
' a medida que toca vamos calculando las duraciones en base a calCompas,,
' y almacenando las mismas en el formato de track comprimido donde no 
' interviene el tiempo para nada no hay ticks guardados,hay tick de posicion,
' todo es relativo y solo depende del tiempo ajustado, por default usara 
' el tiempo que ajusto el usuario el cual quedara grabado  en un archivo aparte
' por ahora como nombredelTema.cfg configuracion del teme  *.cfg
' ademas del tiempo de ejecucion se pondran otros parametros para grabar
' como instrumento efectos volumen del canal etc etc.,..tambien el usuario
' podrá cambiarlo en el archivo plano previamente sin necesidad de editarlo 
' en el programa. Todo se puede hacer en formato Roll. Pero vermos o no la convenicncia
' de usar el formato midi para grabar a archivo y luego volverlo a cargar..
' para eso Roll graba y carga transformando de *.trk a *.Roll 
' en esta version ya tenemos un alibreria par agrabar y leer *.mid 
'----------------------------------------------------------------------
' TRACKS: 1) 1ER ETAPA GRABAMOS TRACKS A PARTIR DEL VECTOR DE VISUALIZACION
' EN EDICION. listo ok
'    2DA ETAPA) CARGAMOS TRACKS Y PASAMOS AL DE VISUALIZACION PARA EDITAR
'  UNO QUE SE ELIJA..EMPEZAREMOS CON UN SOLO TRACK EL 1..O SEA GRABAR Y CARGAR 
' EN FORMATO TRACK Y EDITAR EN FORMATO VISUALIZACION, LEUGO SUPERADA ESA ETAPA
' SEGUIMSO AGREGANDO TRACKS---listo ok
' 3) si cargamos un trak desde disco, se convierte y carga a visualizacion ahi se modifica 
' y al GRABAR convierte de nuevo a track sin cambiar el nombre o sea reemplaza 
' el track cargado con su modificacion. Podriamos grabar en formato roll 
' si hace falta renombrando el archivo eliminando [nn] del principio
' asi Roll pensara que no se cargo un track y se grabara como *.Roll.
'
' 9.9 cambiamos la sdimensiones de lugar, asi maxpos es la 1era y podemos
' redimensionar con preserve !! algunaso archivos *.roll fueron reconvertidos
' ESTANDARIZACION OCTAVAS Y NUMEROS DE NOTAS DE PIANO EMPIEZAN EN CERO DESDE
' OCTAVA -1 --V ok. cambio masivo de variabel semitono va de 0 a 11
' cuadno nota ocupa el lugar de semitono se resta 1 porque semitono va de 0 a 11
' las notas letras van de 1 a 12 eso sigue igual pues ausencia de nota es 0
' de C a B van de 12 a 1 decreciente internamente limites de vector 0 a 115
' sub restar se adecuo al desplazamiento -1 de semitono etc etc etc gran cambio..! 
' 9.8 ya anda pasar como parametro Roll, ahora creacion de tracks
' seleccion de track y visualizacion del elegido grabacio etc gran trabajo!
' 9.7 ubicar nro compas entrando el numero en [Ver] ok
' 9.7 el RollLoop lo envie a una Sub con thread, de ese modo podre
' llamar varias pistas graficas a la vez pongamosle 8 par a8 instrumentos
' debere ahcer 8 pantallas o superficies ¿?
' 9.6 uso de thread para grafico, parametrizacion gap1 gap2y gap3
' 9.6 ajustar ancho de columnas Y FONT con F2 F3,luego F9 F10 font solamente
' 9.5 acordestriadas a partir de una fundamental. pendiente
' 9.5 NUEVO ARCHIVO BORRA nombre y todo lo de 'Q'.
' -> en desa: 9.5 repetir play zona grabado y marcado en Roll 
' Ubicar Home, End de secuecnia pulsando esas teclas.
' copiar una zona 1 o mas veces en la posicion elegida
'0.0.8.9.2.0 11-07-2021  mover zona a la derecha o izquierda con M + Click en la pos elegida
' despues haremos dragado, como de trasponer tambien pero mucho mas adelante,,,
' la idea es hacer algo parecido a trasponer,,pero horizontal ALT-M
' los espacios vacios los dejaremos con dur=0 nota=181
' insert tambien se hara mas facil usando estos metodos cambiaremos en 
' versiones futuras....
 
'01-07-2021 se agrego loop ya sea total o por zona funciona se termina con P
'01-07-2021 ya no permite entrar notas con click mucho antes de maxpos
'01-07-2021 posicionado cursor ctrl-m, pasoZona1 y 2 seusan en playAll tambien 
' ver ciertos problemas de play de las notas que subsistem !!!
'0.0.8.9.0.0 seleccion de una sola nota en el 3er Ctrl-Click para una accion, en modo lectura.
'0.0.8.8.1.0 seleccion de ZONA para accion con CTRL-Click en lectura 2 puntos.
'           borrado de zona con Q como siempre. ok 27-06-2021
' 26-06-2021:trasponer UP or Down ok
' 24-06-2021: trasponer..tiene un defecto cuando hace el pasaje de una octava a otra 
' hay una zona muerta ojo ajustar ,,,yo me complique todo con esta separacion o salto
' hay una linea muerta entre octavas!!!
' 22-05-2021:WHEEL LUEGO CTRL CAMBIA ESPACIADO DE LINEAS, REEMPLAZO BOLD POR NORMAL 
' 20-06-2021: ha que corregir el caso DESAFIO-LIGA-ACORDE-3-O-sin-O-ok
'  dura poco el sonido de la ligadura
' en el otro anda bien se corrigio uno se jodio el anterior!
' 12-06-2021: fix cursor durante play
' 12-06-2021: CAMBIO DEL ORDEN EN SEPARADUR DE LSO RANGOS DE BUSQUEDA
' 11-06-2021: scroll horizontal durante play
' 11-06-2021:[NUEVO] fix se agrego que limpie toda la secuecnia hasta el final.
' 11-06-2021 posible desarrollo: usar teclado para entrada de notas al estilo
' de entrada por teclado de la PC..los numeros 1 al 0 de duraciones seran
' cdefgabcde = 1,2,3,4,5,6,7,8,9,0...otra octava cualqueira seran los semitonos


