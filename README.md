Este programa está publicado bajo la licencia GPL.
(ver carpeta Licences/RollMusic)
  Funciona bien en WINDOWS 7 a 10 para 64bits, Windows 11 tambien esta bajo prueba. 
 Si hay partes lentas es mi mala tecnica de programacion sorry...se sugiere pulsar las teclas rapidas por mas tiempo para que responda el comando.
  Una  ejercitacion para ver Cairo.(Estado:Alpha  ...)
Un secuenciador que usa letras en vez de rectangulitos...
Puede editarse una sola pista o crear una cancion de hasta 32 pistas.
Ajustar el instrumento de cada pista o patch, usar uno o mas puertos de salida MIDI-OUT.
  Usa las 8 octavas musicales desde C0 a B8 con sostenidos y en vez de figuras musicales 
o rectangulitos, colocar letras que simbolizan duraciones de notas con la opcion de cambiar 
insertar etc. Duraciones "O","P","I","L","F","E","X","H", W, Redonda, blanca,negra,corchea
semicorchea,fusa y semifusa..Busque las letras mas parecidas a las figuras
musicales L corchea tiene un palito, F semicorchea tiene 2, E tiene 3 como fusa,
 X 4 semifusa, H 5...los silencios se arman poniendo una s minuscula adelante. 
  Las ligaduras poniendo un + al final.
Las duraciones se entran por teclado del 1 al 8 o por mouse click derecho en Edit Cursor. 
Es un codigo  algo organizado usa muchas globals.
En el grafico se puede hacer scroll con flechas arriba abajo,,
cambiar tamaño de font con F9-F10 y F2-F3 la proporcion.Levantar el borde inferior dela ventana como un telon
y asi achicar la pantalla con F7, volver a agrandar con F8, achicando o agrandando separaciones 
entre lineas etc. 
  Clickeando <> cambia a rojo para redimensionar la pantalla, mejor apuntar el mouse en la 
zona central y dragar algo para achicar o agrandar. Volver a clickear <> para
freezar el tamaño. Para volver al tamaño normal pulsar F7 u F8. Para mover la ventana de
lugar se clickea (insitir haste que lo tome) y draga en la cinta morada de arriba como en las
ventanas de windows. Se puede cargar un roll o cancion sin grafico yaabrirlo despues. 
  Para editar hacer click en la palabra EDIT de lgrafico, la misma cambiara a color verde.
Elegir una octava para ello hacer scroll, y dejar el cursor del mouse en cualquier parte 
de esa octava dar un click en el extremo final para asegurar esa octava .
  Pulsar la duracion deseada de 1 a 8 luego la nota B,A#,A,G#,G,F#,F etc
Las notas van de C a B.. C,D,E,F,G,A,B , para las notas con sostenido pulsar Ctrl y despues
la nota. En el menu de Control sepuede elegir ver sostenidos o bemoles.
 Automaticamente aparecera la duracion en la linea correspondiente a esa nota,,
Para Cambiar una duracion 1ero, se pasa a modo Cursor,con Ctrl-M (marron )asi se podrá mover el cursor
por las lineas ya escritas en todas las direcciones. (el cursor muestra un rectangulito y una linea 
vertical para visualizar mejor la posicon. Solo se escibira en esa ocatava si quiero ir mas arriba o
abajo dar de preferencia un click en otra octava en el extremo derecho de la octava deseada.
 Para modificar una nota existente o agregar una en una linea en que no existia nada
pulsar la duracion y luego X, la posicion del cursor dara la nota deseada.
Al finalizar pulsar Crl-P para terminar el modo cursor, y luego Edit hasta que vuelva a blanco, posar 
el mouse o dar un click en la cinta superior extremo derecho despues de la linea.
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
y Modifcar. Tambien hay ingreso de acordes con Click derecho del mouse en el modo lectura.
Una distraccion durante la pandemia, que sigue por ahora mientras estudio canto,,
Es mi 1er aplicación gráfica todo un desafio.
Los silencios se agregan igual que las notas pero luego de pulsar la duración
se pulsa "s" y la nota correspondiente (para su ubicacion visual). los simbolos usados son los dados.
(se podrá modificar a gusto en el futuro que tipo de letra para que duracion...)
También se puede agregar notas o silencios con puntillo, el mismo procedimeinto pero 
pulsando punto (.) antes del nombre de la nota o despues de la duracion,. Las ligaduras se simbolizan
con un + del lado de la conexion con la otra nota (siempre a derecha).
La aplicacion termina pulsando Escape sobre el gráfico..o la X en el extremo derecho superior de la 
ventana de Control o menu archivo salir..El ejecutable ha sido escaneado con Antivirus windows defender.
Tiene un menu basico  en la pantalla grafica. Reacciona con click, o ALT-Click o Ctrl-Click..es probar
si no  se desea memorizar, hay un txt con teclas rapidas accesible desde menu info.
Se agrego una ventana que ahora es la de inicio de Control más indicada para crear una Cancion.
Se usa un play que solo tiene en cuenta las duraciones de las notas, no hay tiempo, pero algun dia 
se usa ticks en las ultimasa versiones de este año, al estilo de archivos midi, para compatibilizar. 
Es solo un código imperativo sin clase alguna ni constructores ni destructores.
(con el OOP, todo se veria engorroso o no, segun gustos, tal vez lo incorpere a futuro..) 
Por ahora los instrumentos distintos son a nivel Pista o Track, luego sera tambien a nivel nota,
volumen , (pitch bend produce mucho retardo dicen). Tien efectos pan, eco, chorus. No usa vst
por ahora solo instrumentos General Midi. 
Graba en 2 formatos Roll y Tracks. Roll es el vector de visualizacion y Track el comprimido, lo que se
observa es que en un track o instrumento, el acorde no tendrá mas de 12 notas cuanto mucho, o sea
es un vector de ticks en eje X, y 12 notas en vertical Y(algun otro para controles). No es lineal largo
en eje X, asi el track sigue la estructura visual pero comprimida y en cada ticks la misma informacion
esta en visual y comprimida. Roll hay solo uno seria el Track cero. Luego hasta 32 track ademas del cero
cada track comprimido se copia al visual Roll y Track(0), cuando pasamos de uno a otro con TAB en una cancion.
No probe como anda con 32 hasta queda pendiente con 8 bien...dependerá de la cpu, pero con 4 nucleos no usa
nada de CPU la reproduccion sin el grafico, con el grafico llega al 20%) cada track con 12 notas de acorde,
que no existirá nunca creo (un piano con 12 teclas sonando al mismo tiempo? ...no). Para la sincronizacion
usé variables globales algo sencillo podrá haber minimos retardos que serán parecidos a los retardos humanos
de modo que seria de paso una reproduccion mas humana que algo super exacto.

Se puede cargar un archivo (.Roll) y luego grabarlo en formato track (.trk) y volverlo a cargar
viendolo en pantalla (se convierte al grabar de Roll a Trk y al cargar de Trk a Roll ), o si es 
nuevo al poner un nombre se debe poner la extension si es .roll o .rtk los grabara en esos formatos.
Pulsando F1 aparece un Notepad con Ayuda Basica. En el menu info hay mas ayuda.
Hubo una conversion del programa de no usar ticks a usarlo por eso la ayuda puede tener partes
de cuando no tenia ticks, se ira adaptando de a poco..Agregue alguna ayudita para los que estudian
lenguaje musical y canto como es mi caso,, a medida que lo vaya usando y encuentre monton de errores los
ire corrigiendo, eso espero...saludos
