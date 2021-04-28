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
'Sets an offset that is added to the device coordinates determined by the CTM when drawing to surface. One use case for this function is when we want to create a cairo_surface_t that redirects drawing for a portion of an onscreen surface to an offscreen surface in a way that is completely invisible to the user of the cairo API. Setting a transformation via cairo_translate() isn't sufficient to do this, since functions like cairo_device_to_user() will expose the hidden offset.
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



