Una  ejercitacion para ver Cairo.(Estado:Alpha Debug escribe archivos.)
Un secuenciador que usa letras en vez de rectangulitos...
Ya funciona puede editarse una sola pista o crear una cancion de hasta 32 pistas.
Ajustar el instrumento de cada pista o patch, usar uno o mas puertos de salida MIDI-OUT.
Usa las 8 octavas musicales desde C0 a B8 con sostenidos y en vez de figuras musicales 
o rectangulitos, colocar letras que simbolizan duraciones de notas con la opcion de cambiar 
insertar etc. Duraciones "O","P","I","L","F","E","X","H" , W, Redonda, blanca,negra,corchea
semicorchea,fusa y semifusa..Busque las letras mas parecidas a las figuras
musicales L corchea tiene un palito, F semicorchea tiene 2, E tiene 3 como fusa,
 X 4 semifusa, H 5...
los silencios se arman poniendo una s minuscula adelante.
se entran por teclado del 1 al 8 o por mouse click derecho en Edit Cursor. 
Es un codigo  algo organizado usa muchas globals.
Al inicio se puede hacer scroll con flechas arriba abajo,,
cambiar tamaño de font con F9-F10 y F2-F3, levantar el borde inferior dela ventana como un telon
y asi achicar la pantalla con F7, volver a agrandar con F8, achicando o agrandando separaciones 
entre lineas. 
Clickeando <> cambia a rojo para redimensionar la pantalla mejor apuntar el mouse en la 
zona central y dragar algo para achicar o agrandar. Volver a clickear <> para
freezar el tamaño. Para volver al tamaño normal pulsar F7 u F8. Para mover la ventana de
lugar se clickea (insitir haste que lo tome) y draga en la cinta morada de arriba como en las
ventanas de win 7. 
Para editar hacer click en la palabra EDIT, la misma cambiara de color a verde.
Elegir una octava para ello hacer scroll, y dejar el cursor del mouse en cualquier parte 
de esa octava. Pulsar la durcion deseada de 1 a 8 luego la nota B,A#,A,G#,G,F#,F etc
Las notas van de C a B.. C,D,E,F,G,A,B , para las notas con sostenido pulsar Ctrl y despues
la nota. Automaticamente aparecera la duracion en la linea correspondiente a esa nota,,
Para Cambiar una duracion 1ero, se pasa a modo Cursor,con Ctrl-M asi se podra mover el cursor
por las lineas ya escritas en todas las direcciones. (el cursor muestra doble el de view y 
de navegacion y una linea vertical para visualizar mejor la posicon.
Para modificar una nota existente o agregar una en una linea en que no existia nada
pulsar la duracion y luego X, la posicion del cursor dara la nota deseada.
Al finalizar pulsar Crl-P para terminar el modo cursor,,,
Para insertar se usa el modo Cursor, pero luego de elegir la posicion de la insercion
sobre la linea con datos  se pulsa Insert, luego una duracion y 
luego la Tecla I, asi iremos insertando sobreescribiendo lo existente.
Sin mover el cursor al finalizar pulsar la telca Fin, y todo lo antiguo se desplazara  a derecha 
y se mostrara la nueva insercion incluida,,
Los acordes se ingresan sobre Escribiendo en una zona cualquiera en el modo Cursor y con la 
tecla X. Por ahora para insertar un acorde se hace con insercion de una nota con la tecla Insert,
luego la tecla I, y terminamos con la tecla fin o End y luego con la opcion X de modificar se 
agrega el resto del acorde insertado.Tambien se puede hacer por mouse en Edit Cursor, 
Ctrl mouse derecho, aparece menu Blanquear, Insertar, Fin insertar
y Modifcar. Ahora hay ingreso de algunso tipos de acorde con Click derecho delmouse en el modo lectura.
Falta mucho por hacer. Una distraccion durante la pandemia,,,,y poder hacer mi propio
sequencer es mi 1er aplicación gráfica todo un desafio.
Los silencios se agregan igual que las notas pero luego de pulsar la duración
se pulsa "s" y la nota correspondiente. los simbolso usados son los dados.
(s epodra modificar a gusto en el futuro que tipo de letra para que duracion...)
También se puede agregar notas o silencios con puntillo, el mismo procedimeinto pero 
pulsando punto (.) antes del nombre de la nota,. Las ligaduras se simbolizan con un + del lado 
de la conexion con la otra nota (siempre a derecha).
La aplicacion termina pulsando Escape..o la X en el extremo derecho superior..El ejecutable ha 
sido escaneado con Antivirus windows defender.
Tiene un menu basico  en la pantalla grafica. Reacciona con click o ALT click o Ctrl Click..
Se agrego una ventana que ahora es la de inicio de Control mas indicada para crear una Cancion.
Se usa un play que solo tiene en cuenta las duraciones de las notas, no hay tiempo, pero algun dia 
usaré ticks al estilo de archivos midi, para compatibilizar. 
Es solo un código imperativo sin clase alguna ni constructores ni destructores.
(con el OOP, todo se veria engorroso, tal vez lo incorpere a futuro ) 
Por ahora los instrumentos distintos son a nivel Pista o Track, luego sera tambien a nivel nota,
volumen , (pitch bend produce mucho retardo dicen). 
Ya puede grabar tracks  pero solo es el vector de visualizacion comprimido, lo que se
observa es que en un track o instrumento, el acorde no tendrá mas de 12 notas cuanto mucho, 
se hará posiblemente dinamico en el futuro. Tiene 32 tracks (no me da más la pantallita por ahora
no probe como anda con 32 hasta 8 bien...dependerá de la cpu, pero con 4 nucleos no usa
nada de CPU la reproduccion sin el grafico, con el grafico llega al 20%) c/u con 12 notas de acorde,
lo que no existirá nunca.
Ya se puede cargar un archivo (.Roll) y luego grabarlo en formato track (.trk) y volverlo a cargar
viendolo en pantalla (se convierte al grabar de Roll a Trk y al cargar de Trk a Roll )
Debido a eso la columna vertical de la pantalla visual solo tendra 12 notas en cualquier acorde 
como maximo el resto vacio,en el archivo Trk, el de Visualizacion tiene 116 como máximo, puede tener menos,
en cada track o pista se puede definir un rango de octavas de esa pista solamente.
Todo se prepara para que el Nro de posiciones pueda ser dinamico tambien,a medida que se usa
se implementó que vaya incrementando el numero de posiciones, con lo cual el limite sera
la memoria de la Ram .
Pulsando F1 aparece un Notepad con Ayuda Basica. 
