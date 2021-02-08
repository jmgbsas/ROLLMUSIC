Recien empiezo con esto, una buena ejercitacion para ver Cairo.
No encuentro secuenciadores que usen letras en vez de esos rectangulitos...
Lo que pretendo hacer lento pero seguro y a mi ritmo.. es usar las 8 octavas 
musicales desde C0 a B8 con sostenidos y en vez de figuras musicales o rectangulitos
colocar duraciones de notas con la opcion de cambiar insertar etc...
Es solo el front end...inicial,,luego por su puesto usare RTmidiC para reproducir
Por ahora permitira 12000 duraciones o eventos secuenciales
Duraciones "O","P","I","L","F","E","H" , Redonda, blanca,negra,corchea
semicorchea,fusa y semifusa..Busque las letras mas parecidas a las figuras
musicales L corchea tiene un palito, F semicorchea tiene 2, E tiene 3 como fusa
y H o W segun el gusto proahro esta H seria la semifusa...
los silencios se colocan una s delante de la nota al mostrarla, pero las duraciones
se entran por teclado del 1 al 8. 
El codigo de las subrutinas por ahora me los reservo...si alguien quiere colaborar
lo compartire. Es un codigo  sucio no está optimizado ni ordenado usa muchs globals.
Al inicio se puede hacer scroll con flechas arriba abajo,,
cambiar tamaño de font con F9 F10, levantar el borde inferior dela ventana como un telon
y asi achicar la pantalla con F7, volver a agrandar con F8. 
Clickeando <> cambia a rojo para redimensionar la pantalla mejor apuntar el mouse en la 
zona central y dragar algo para achicar o agrandar. Volver a clickear <> para
freezar el tamaño. Para vovler al tamaño normal pulsar F7 u F8. Para mover la ventana de
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
con la opcion X de modificar se agrega el resto del acorde insertdo.
Falta mucho por hacer..creo es el momento de grabar y cargar una secuencia..y luego
agregarle la reproduccion por midi...
Cuando funcione algo liberare el resto....por hro no tiene sentido es solo un front end,
Los silencios se agregan igual que las notas pero luego de pulsar la durcion
se pulsa "s" y la nota correspondiente.
Tmbien se peude agregar notas o silencios con puntillo, el mismo procedimeinto pero 
pulsando punto (.) antes del nombre de la nota,.



