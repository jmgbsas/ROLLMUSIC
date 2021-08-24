Una  ejercitacion para ver Cairo.(Estado:ALpha Debug escribe archivos.)
No encuentro secuenciadores que usen letras en vez de esos rectangulitos...
Lo que pretendo hacer lento pero seguro y a mi ritmo.. es usar las 8 octavas 
musicales desde C0 a B8 con sostenidos y en vez de figuras musicales o rectangulitos
colocar duraciones de notas con la opcion de cambiar insertar etc...
Es solo el front end...inicial,,luego por su puesto usare RTmidiC para reproducir
Por ahora permitira 78000 duraciones o eventos secuenciales en vector Visual.
Duraciones "O","P","I","L","F","E","W","H" , Redonda, blanca,negra,corchea
semicorchea,fusa y semifusa..Busque las letras mas parecidas a las figuras
musicales L corchea tiene un palito, F semicorchea tiene 2, E tiene 3 como fusa,
 W 4 semifusa, H 5...
los silencios se colocan una s delante de la nota al mostrarla, pero las duraciones
se entran por teclado del 1 al 8 o por mouse click derecho en Edit Cursor. 
Si alguien quiere colaborar lo compartire. Es un codigo  sucio no está optimizado ni 
ordenado usa muchas globals.
Al inicio se puede hacer scroll con flechas arriba abajo,,
cambiar tamaño de font con F9 F10, levantar el borde inferior dela ventana como un telon
y asi achicar la pantalla con F7, volver a agrandar con F8. 
Clickeando <> cambia a rojo para redimensionar la pantalla mejor apuntar el mouse en la 
zona central y dragar algo para achicar o agrandar. Volver a clickear <> para
freezar el tamaño. Para volver al tamaño normal pulsar F7 u F8. Para mover la ventana de
lugar se clickea (insitir haste que lo tome) y draga en la cinta morada de arriba como en las
ventanas de win 7. 
Para editar hacer click en la palabra EDIT, la pantalla cambiara de color.
Elegir una octava para ello hacer scroll, y dejar el cursor del mouse en cualquier parte 
de esa octava. Pulsar la durcion deseada de 1 a 8 luego la nota B,A#,A,G#,G,F#,F etc
Las notas van de C a B.. C,D,E,F,G,A,B , para las notas con sostenido pulsar Ctrl y despues
la nota. Automaticamente aparecera la duracion en la linea correspondiente a esa nota,,
Para Cambiar una duracion 1ero, se pasa a modo Cursor,con Ctrl-M asi se podra mover el cursor
por las lineas ya escritas en todas las direcciones. (el cursor muestra dobleel de view y 
de navegacion y una linea vertical para visualizar mejor la posicon.
Para modificar una nota existente o agregar una en una linea en que no existia nada
pulsar la duracion y luego X, la posicion del cursor dara la nota deseada.
Si se desea seguir usando la misma duracion solo navegar con las flechas y pulsar otra
vez X. al finalizar pulsar Crl-P para terminar el modo cursor,,,
Para insertar se usa el modo Cursor, pero luego de elegir la posicion de la insercion
sobre la linea con datos  se pulsa Insert, luego una duracion y 
luego la Tecla I, asi iremos insertando sobreescribiendo lo existente.
Sin mover el cursor alfinalizar pulsar la telca Fin, y todo lo antiguo se desplazara  a derecha 
y se mostrara la nueva insercion incluida,,
Los acordes se ingresan sobre Escribiendo en una zona cualqueira 
en el modo Cursor y con la tecla X. Por ahora para insertar un acorde se
hace con insercion de una nota con Insert y tecla I,finalizar y luego 
con la opcion X de modificar se agrega el resto del acorde insertdo.Tmbien se puede hacer 
por mouse en Edit Cursor, Ctrl mouse derecho, aparece menu Blanquear, Insertar, Fin insertar
y Modifcar. Falta mucho por hacer. Una distraccion durante la pndemia,,,,y poder hacer mi propio
sequencer es mi 1er aplicación gráfica todo un desafio.
Cuando funcione algo liberare el resto....por ahora no tiene sentido es solo un front end,
Los silencios se agregan igual que las notas pero luego de pulsar la duración
se pulsa "s" y la nota correspondiente.
También se puede agregar notas o silencios con puntillo, el mismo procedimeinto pero 
pulsando punto (.) antes del nombre de la nota,. en elfuturo las ligaduras se simbolizaran
con un + del lado de la conexion con la otra nota
La aplicacion termina pulsando Escape....El ejecutable ha sido escaneado con Antivirus Clam..
Tiene un menu basico (alfa)  cambiante con Flecha Derecha Iquierda, siemrpe y cuando el mouse 
esté posicionado enla cinta superior (menu archivo Nuevo, bri, Guardar ya implementado.
Con Cairo es dificil agregar menues, y embuir la superficie en un Frame limita 
las posibilidades de Cairo...en mi experiencia...se podria usar GTK pero hasta ahora no 
concosco como GTK maneja un loop de dibujo...solo veo ejemplos sin loop...
MenuPopup de Microsoft es incontrolable....
Probando distintas formas de play, pero algun dia usaré ticks al estilo de archivos midi. 
Estoy viendo la dificultad del play sin usar ticks, solo las duraciones de las notas, 
a puro codigo sin clases. En esta Version se Inició Tracks. Podra poner instrumentos distintos
en un mismo track a nivel nota,o un instrumento pro Track segun se desee, volumen y pitch bend 
tambien. Los tracks seran tal vez en el formato de midi de modo que habrá que pasar del modo 
midi al del Vector Roll para mostrar  los datos en edicion, se hará en el futuro.
Ya puede grabar tracks pero solo es el vector de visualizacion comprimido,lo que se
obserba es que en un track o instrumento, el acorde no tendrá mas de 12 notas cuanto mucho, 
se hará posiblemente dinamico en el futuro. 32 tracks c/u con 12 notas de acorde no existira nunca.
Ya se puede cargar un archivo (.Roll) y luego grabarlo en formato track (.trk) y volverlo a cargar
viendolo en pantalla (se convierte al grabar de Roll a Trk y al cargar de Trk a Roll )
Debido a eso la columna vertical de la pantalla visual solo tendra 12 notas en cualquier acorde 
como maximo el resto vacio,en el archivo Trk, el de Visualizacion tiene 116 en vertical acorde por
78000 posiciones de largo de secuencia.De modo que la memoria usada es mucho menor.
Todo se prepara para que el Nro de posiciones pueda ser dinamico tambien,a medida que se usa
se implementara que vaya incrementando el numero de posiciones, con lo cual el limite sera
la memoria de la Ram .
Pulsando F1 aparece un Notepad con Ayuda Basica. 
