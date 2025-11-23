#Include Once "freetype2/freetype.bi"
'' The FB headers don't contain this prototype
'extern "c"
'Declare function usleep( as ulong) as long '=0 when working
'end Extern
' ESCALAS Y ACORDES http://www.looknohands.com/chordhouse/guitar/index_rb.html
'                  http://www.looknohands.com/chordhouse/piano/
' DUDA DE GUITARRISTAS https://www.guitarristas.info/foros/dudas-sobre-conceptos-armonia/239164
' code from
'https://freebasic.net/forum/viewtopic.php?t=23680
'Declare sub RtlInitUnicodeString alias "RtlInitUnicodeString"(as PUNICODE_STRING,as PCWSTR)
'Declare function NtDisplayString alias "NtDisplayString"(as PUNICODE_STRING)as NTSTATUS
Declare function NtDelayExecution alias "NtDelayExecution"(as BOOLEAN,as PLARGE_INTEGER)as NTSTATUS
Declare function NtTerminateProcess alias "NtTerminateProcess"(as HANDLE,as NTSTATUS)as NTSTATUS
'#define NtCurrentProcess() cast(HANDLE,-1)
'our entry point
'sub startup cdecl() 'NtProcessStartup... any name is ok
'    dim as UNICODE_STRING us
'    dim as LARGE_INTEGER delay
'    delay.QuadPart = -10000 '1 m seconds
'  ' RtlInitUnicodeString(@us,@wstr("FreeBasic Native Application"))
'  '  NtDisplayString(@us) 'show the message
'    NtDelayExecution(FALSE,@delay) 'so we can see the message
'    NtTerminateProcess(cast(HANDLE,-1),0)
'end Sub


extern "C"
declare function _
cairo_ft_font_face_create_for_ft_face ( as FT_Face, as long ) as cairo_font_face_t ptr
end extern

'' Convenience
type FreeType
 public:
 declare constructor()
 Declare destructor()

 declare operator Cast() as FT_Library

 private:
 as FT_Library _library
end type

constructor FreeType()
FT_Init_FreeType( @_library )
end constructor

destructor FreeType()
FT_Done_FreeType( _library )
end destructor

operator FreeType.cast() as FT_Library
 return( _library )
end Operator

'Declare Sub RollLoop (c As cairo_t Ptr, Roll As inst)
'Declare Sub creaPenta (c AS cairo_t Ptr, Roll As inst  )
' HELP PARA EL PROGRAMA FUTURO...
'https://www.freebasic.net/forum/viewtopic.php?p=187407&hilit=HtmlHelp#p187407
'https://www.pinvoke.net/default.aspx/hhctrl.htmlhelp
#Define HH_DISPLAY_TOPIC 0000 '  extracted from h file.
#define HH_HELP_CONTEXT  0015
#Define  HH_DISPLAY_TOC   0001
#Define  HH_DISPLAY_INDEX 0002
#Define  HH_DISPLAY_SEARCH 0003
#Define  HH_CLOSE_ALL  0018
/'
Const BOTON_PISTA_ROLL=5
Const HabilitaGrabar=0
Const BTN_LIBERADO=0
Const GrabarPatronaDisco=4
Const GrabarPistaEjecucion=1
Const PatronDeEjecucionCompleto=32
Const PISTASROLL = 3
Const PISTASEJECUCIONES   =4
Const BOTON_SELECCION_EJECUCION =6
Const CHECK_GRABAR_EJECUCION    = 7
Const GRUPO_BTNS_MIDI           =8
Const BTN_MIDI_PARAR        = 9
Const BTN_MIDI_GRABAR     = 10
Const BTN_MIDI_EJECUTAR   = 14
Const GRUPO_BTNS_MANUAL =13
Const BTN_ROLL_PARAR    = 11
Const BTN_ROLL_EJECUTAR = 12
Const BTN_ROLL_GRABAR_MIDI = 15
Const BTN_EJEC_PORTSAL =16
Const BTN_EJEC_VOL        = 17
Const BTN_EJEC_PAN        =18
Const BTN_EJEC_PATCH    = 19
Const BTN_EJEC_CANAL    = 20
Const GRUPO_BTNS_OKCAN    =21
Const BTN_ROLL_PORTSAL =22
Const BTN_ROLL_VOL        = 23
Const BTN_ROLL_PAN        =24
Const BTN_ROLL_PATCH    = 25
Const BTN_ROLL_CANAL    = 26
Const BARRA_DE_ESTADO = 33
Const TEXT_TOPE =34
Const LINEA_COMANDO=35
Const OK = 36

Const HABILITAR = TRUE
Const DESHABILITAR = FALSE 
Const SI=1
Const NO=0
Const CARGAR = 1
Const NO_CARGAR =0 
Const CARGAR_MAS_PISTAS_O_CANCION = 2
Const REABRIR_ROLL_CON_DATOS_CARGADOS=4
Const NO_CARGAR_PUEDE_DIBUJAR =0 
Const CARGAR_NO_PUEDE_DIBUJAR =1
Const EVITAR_LLAMAR_ROLLLOOP_DE_NUEVO = 3 
Const CON_CANCION = 1
Const SIN_CANCION = 0
'------menu grafico
Const MENU_INICIAL = 0
Const COMANDOS_ARCHIVO = 3
Const PARAMETROS_ROLL = 2
Const OPCIONES_MENU = 5
Const AJUSTE_OCTAVAS = 6
Const PARAMETROS_SECUENCIA = 8
Const AJUSTE_OCTAVAS = 6
Const TIPO_DE_COMPAS = 7
Const CONFIGURACION = 9
Const SEL_MIDI = 10
Const NRO_CANAL = 11
'-----------------instancias
Const ARG0_EN_LINEA = 0
Const ARG1_1_TITULO = 1
Const ARG1_2_DESDE = 1 
Const ARG2_HASTA = 2
Const ARG3_TITU = 3
Const ARG4_INSTRU = 4
Const ARG5_PID1 = 5
Const ARG6_USARMARCO = 6
Const ARG7_NOMBRECANCION = 7
Const ARG107_FICTICIO = 107
'----------------------------------
Const TERMINAR_POR_ESCAPE =1
Const NO_TERMINAR_BARRE_PANTALLA = 0
Const TERMINAR_POR_LOOP_PRINCIPAL = 2
Const NO_TERMINAR_CON_DATOS_CARGADOS  = 3 
'--------------------------------------
Const CTRL_M = 1 ' PUEDE HABER UN 1 0 o 0 1  o 2 0 ?
Const CTRL_N = 2
Const ObtenerDuracion1erClickIzquierdo=3
Const DesplegarMenuModifClickDerecho =1
Const SeleccionarComandoClickIzquierdo = 2
'----------------------------------------------------
'/
Dim Shared As Integer PISTASEJECSELECCIONADA=0
Dim Shared As Integer PISTASROLLSELECCIONADA=0

Dim HtmlHelpA As Function (hwndCaller As HWND,pszFile As LPCSTR,uCommand As UINT,dwData As DWORD_PTR) As HWND
Dim HTMLHelp  As Function ( hWndCaller As hwnd,  pszFile As String,  ByVal uCommand As Integer,  dwData As DWORD_PTR) As Integer
Dim HH_POPUP  As Function ( hWndCaller As hwnd,  pszFile As String,  ByVal uCommand As Integer,  dwData As DWORD_PTR) As Integer

'Declare Sub menu (c0 AS cairo_t Ptr,c AS cairo_t Ptr,n As Integer,menuNro As Integer)
declare Sub botones(hWnd As HWND,cm As cairo_t Ptr,x As Integer,y As integer)
'Declare Sub menuAcc (c AS cairo_t Ptr)
Declare Sub moveresto ()
Declare Sub movedato ()
'Declare sub calcCompas(ByRef j As Integer)
Declare Sub mayorDurEnUnaPosicion (ByRef posn as integer)
Declare Sub organizaCompases()
Declare Function KeyPress(Key As Integer) As Integer
' 128 track,16 canales o sea cada canal con 8 voces
'Declare Sub cursor(c AS cairo_t Ptr, ByRef n As Integer, ByRef nro As Integer)
'Declare Function print_text(ByVal x As Integer, ByVal y As Integer, ByRef text As String, _
'        ByVal font As FT_Face, ByVal size As Integer, ByVal col As UInteger ) As Integer
Declare Sub separarDur(ByRef j As Integer,ByRef DUR As ubyte,  partes() As Integer,  ByRef cantPartes As Integer, ByRef falta As Integer )
Declare Sub menugrafico(c0 AS cairo_t Ptr, Cantitems As Integer, items() As String )
Declare Sub CambiarDim(redi As integer)
' para IUP textbox 
Declare Sub dialogoText(cadena As string)
Declare Sub velocidades(j as integer)
Declare Sub playthread()
Declare Function GrabarRoll () As integer
Declare Sub abrirSecuencia (nf As integer)
Declare Sub GrabarTrack   (ntk As integer) 
Declare Sub ActualizarRollyGrabarPistaTrk (funcion As String, nroEjec As integer)
Declare Sub GrabarRollaTrack (cambiaext As Integer, nroejec As integer)
Declare Sub CargarPistasEnCancion ()
Declare sub GrabarCopiadePista()
Declare Sub ImportarPistaExterna(nombre As string )

Declare sub selport(ByVal mitipo As Integer)
Declare Sub selcanal(ByVal mitipo As Integer)  
Declare Sub ComboBox()
Declare Sub grabaprueba()
Declare Sub borrarColumnasMarcadas()
Declare Function SumarnR (notaPiano As Integer) As Integer
Declare Function SumarnRk (notaPiano As ubyte) As ubyte
Declare Sub salir()
Declare Sub IgualaMidiPlano(externo As integer, ByRef CantEventos  As Integer)
Declare Sub LLAMA_GRABAR_ROLL()
 
Dim shared As Integer cnota, cdur, cvel, cinst,cnEOld,CnE, veoPent=1 
Dim Shared As Integer ev1,ev2,ev3,ev4,ev5,ev6,notaGuia=0
Dim Shared As String RTA, alteracion '',alteracion_str
alteracion="sos"
Dim Shared As Integer acumulado=0, controlEdit =0,octavaEdicion=0, pasoZona1=0, pasoNota=0,OCTAVAFIJA
Dim Shared As integer pasoZona2=0,nroRep=0
Dim Shared As Integer vnotaOld=0, vdur=0 ,vnota =0, playloop=0, playloop2=0,SelGrupoNota =0,moverZona=0, copiarZona=0,SelGrupoNotaT =0
Dim Shared as ubyte  DUR,nota,notacur
ReDim Shared as ubyte  notasInsertadas (1 to 1500)
Dim Shared As String t,t2
Dim Shared As Integer mouseX,mouseY, MouseButtons, notaOld, cursormove,notaoldmidi
Dim shared As integer Penta_y, indice, espacio
Dim shared as integer estoyEnOctava=1, estoyEnOctavaOld=1,backspace,fijarEspacio, offset,borrar,cambiadur
Dim Shared As Integer x2,y2,x1, y1, x0, y0,InicioDeLectura,kNroCol, agregarNota,posishow=1
Dim Shared As Integer octavas,  jc,ic, id,ShowNroCol=0,nro,trak=1,trackold=1
Dim Shared As Integer ers, borrapos=0,comienza=0, acordeNro=0,resumen=0  
Dim Shared As Integer menuAccion,insert,notins,haycompas,nroCompas=0,copiar=0,copi=0,TicksInsertados
Common  Shared As String comando
 

Dim Shared NotasGuia (0 To 11) As String * 2 => _
{"B_","A#","A_","G#","G_","F#","F_","E_","D#","D_","C#","C_"}
Dim Shared NotasEscala (1 To 12) As String *2 => _
{"C ","C#","D ","D#","E ","F ","F#","G ","G#","A ","A#","B "}
Dim Shared NotasEscalaArmado (1 To 24) As String *2 => _
{"C ","C#","D ","D#","E ","F ","F#","G ","G#","A ","A#","B ","C ","C#","D ","D#","E ","F ","F#","G ","G#","A ","A#","B "}


Dim Shared NotasGuia2 (0 To 11) As String * 2 => _
{"B_","Bb","A_","Ab","G_","Gb","F_","E_","Eb","D_","Db","C_"}
Dim Shared NotasEscala2 (1 To 12) As String *2 => _
{"C ","Db","D ","Eb","E ","F ","Gb","G ","Ab","A ","Bb","B "}

Dim Shared NotasEscalaArmado2 (1 To 24) As String *2 => _
{"C ","Db","D ","Eb","E ","F ","Gb","G ","Ab","A ","Bb","B ","C ","Db","D ","Eb","E ","F ","Gb","G ","Ab","A ","Bb","B "}
Type claseac
  clase As String * 7
  nro As Integer
  tipo As integer ' sonido mas grave 1 fundamental, 3 1era inv,5 2da inv, 7 3era inv
End Type
' Para Mostrar Acordes en Roll
Dim Shared ClaseAcorde(1 To 100) As claseac => _
{("       ",1,1) ,("/      ",2,3) ,("/      ",3,5) ,("m      ",4,1) ,("m/     ",5,3) ,("m/     ",6,5), _
 ("d      ",7,1) ,("d/     ",8,3) ,("d/     ",9,5) ,("+      ",10,1),("+/     ",11,3),("+/     ",12,5), _
 ("Maj7   ",13,1),("Maj7/  ",14,3),("Maj7/  ",15,5),("Maj7/  ",16,7),("m7     ",17,1),("m7/    ",18,3),_ 
 ("m7/    ",19,5),("m7/    ",19,7),("m7(b5) ",21,1),("m7(b5)/",22,3),("m7(b5)/",23,5),("m7(b5)/",24,7), _   
 ("7      ",25,1),("7/     ",26,3),("7/     ",27,5),("7/     ",28,7), _
 ("7+     ",29,1),("7+/    ",30,3),("7+/    ",31,5),("7+/    ",32,7), _
 ("d7     ",33,1),("d7/    ",34,3),("d7/    ",35,5),("d7/    ",36.7), _ 
 ("(6)    ",37,1),("(6)/   ",38,3),("(6)/   ",39,5),("(6)/   ",40,7), _ 
 ("Sus2   ",41,1),("Sus2/  ",42,3),("Sus2/  ",43,5)}
'Dim Shared deltanota(1 To 43) As Integer => _
'{0,-8,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0, 0,0 ,0,0,0,0,0,0,0,0, _
' 0,0 ,0,0,0,0,0,0,0,0, 0,0 ,0}     
'Dim Shared As integer nota3era=0,nota5ta=0,nota7ma=0
Type esc1 
  nombre   As String
  nota     As string
  nropasos As Integer
  pasos    As Byte Ptr   
End Type
Dim Shared escala (1 To 168) As esc1 
common shared as esc1 ptr pescala
 pescala=@escala(1)

'escalawork 

Dim Shared mayor  (1 To 7)  As byte => {2,2,1,2,2,2,1}  _
,  harmonicaMenor     (1 To 7)  As Byte => {2,1,2,2,1,3,1}  _
,  melodicaMenorAsc   (1 To 7)  As Byte => {2,1,2,2,2,2,1} _
,  melodicaMenorDesc  (1 To 7)  As Byte => {2,1,2,2,1,2,2} _ ' A,B,C,D,E,F,G,A
,  cromatica          (1 To 12) As Byte => {1,1,1,1,1,1,1,1,1,1,1,1} _
,  tonoEntero         (1 To 6)  As Byte => {2,2,2,2,2,2} _
,  pentatonicaMayor   (1 To 5)  As byte => {2,2,3,2,3} _
,  pentatonicaMenor   (1 To 5)  As byte => {3,2,2,3,2} _
,  pentatonicaBlues   (1 To 6)  As Byte => {3,2,1,1,3,2} _
,  pentatonicaNeutral (1 To 5)  As Byte => {2,3,2,3,2} _
,  octatonicHW        (1 To 8)  As Byte => {1,2,1,2,1,2,1,2} _
,  octatonicWH        (1 To 8)  As Byte => {2,1,2,1,2,1,2,1} _
,  ionian             (1 To 7)  As Byte => {2,2,1,2,2,2,1} _
,  dorian             (1 To 7)  As Byte => {2,1,2,2,2,1,2} _
,  frigia             (1 To 7)  As byte => {1,2,2,2,1,2,2} _
,  lidia              (1 To 7)  As Byte => {2,2,2,1,2,2,1} _
,  mixolidia          (1 To 7)  As Byte => {2,2,1,2,2,1,2} _
,  aeolia             (1 To 7)  As Byte => {2,1,2,2,1,2,2} _ ' MENOR NATURAL EOLIA A,B,C,D,E,F,G,A, LA RELATIVA MENOR MAS SIMPLE 
,  locria             (1 To 7)  As Byte => {1,2,2,1,2,2,2} _
,  algeriana          (1 To 8)  As Byte => {2,1,2,1,1,1,3,1} _
,  arabianA           (1 To 8)  As Byte => {2,1,2,1,2,1,2,1} _
,  arabianB           (1 To 7)  As Byte => {2,2,1,1,2,2,2} _
,  aumentada          (1 To 6)  As Byte => {3,1,2,2,3,1} _
,  disminuidaAuxiliar (1 To 8)  As Byte => {2,1,2,1,2,1,2,1} _
,  disminuidaAumentada  (1 To 6)  As Byte => {2,2,2,2,2,2} _  ' igual a tono entero..¿?
,  disminuidaAuxiliarBlues (1 To 8) As Byte=> {1,2,1,2,1,2,1,2} _ ' igual a octatonicHW
,  balinese           (1 To 5)  As Byte => {1,2,4,1,4} _
,  blues              (1 To 6)  As Byte => {3,2,1,1,3,2} _
,  bisantina          (1 To 7)  As Byte => {1,3,1,2,1,3,1} _
,  china              (1 To 5) As Byte => {4,2,1,4,1} _
,  chinaMongolia      (1 To 5) As Byte => {2,2,3,2,3} _
,  diatonica          (1 To 5)  as Byte => {2,2,3,2,3}  _
,  disminuida         (1 To 8)  As Byte => {2,1,2,1,2,1,2,1} _
,  disminuidaAmedias  (1 To 8)  As Byte => {1,2,1,2,1,2,1,2} _
,  disminuidaEntera   (1 To 8)  As Byte => {2,1,2,1,2,1,2,1} _
,  disminuidaTonoEntero (1 To 7) As Byte => {1,2,1,2,2,2,2} _
,  dominante7h        (1 To 7) As Byte => {2,2,1,2,2,1,2} _
,  dobleharmonica     (1 To 7) As Byte => {1,3,1,2,1,3,1} _
,  egipcia            (1 To 5) As Byte => {2,3,2,3,2} _
,  espanola8Tonos     (1 To 8) As Byte => {1,2,1,1,1,2,2,2} _
,  engimatic          (1 To 7) As Byte => {1,3,2,2,2,1,1} _    
,  etiopiaAraray     (1 To 7) As Byte => {2,2,1,2,2,2,1} _
,  etiopiaGeezYEzel  (1 To 7) As Byte => {2,1,2,2,1,2,2} _
,  MitadDisminuidaLocrian (1 To 7) As Byte =>  {1,2,2,1,2,2,2} _
,  MitadDisminuida2Locrian2 (1 To 7) As Byte => {2,1,2,1,2,2,2} _
,  Hawaiana          (1 To 7) As byte => {2,1,2,2,2,2,1} _ 
,  Hindu             (1 To 7) As Byte => {2,2,1,2,1,2,2}

escala(1).nombre="mayor"
escala(1).nropasos=7
escala(1).pasos = @mayor(1)
  
escala(2).nombre="harmonicaMenor"
escala(2).nropasos=7
escala(2).pasos = @harmonicaMenor(1)
     
escala(3).nombre="melodicaMenorAsc"
escala(3).nropasos=7
escala(3).pasos = @melodicaMenorAsc(1)

escala(4).nombre="melodicaMenorDesc"
escala(4).nropasos=7
escala(4).pasos = @melodicaMenorDesc(1)

escala(5).nombre="cromatica"
escala(5).nropasos=12
escala(5).pasos = @cromatica(1)

escala(6).nombre="tonoEntero"
escala(6).nropasos=6
escala(6).pasos = @tonoEntero(1)

escala(7).nombre="pentatonicaMayor"
escala(7).nropasos=5
escala(7).pasos = @pentatonicaMayor(1)

escala(8).nombre="pentatonicaMenor"
escala(8).nropasos=5
escala(8).pasos = @pentatonicaMenor(1)

escala(9).nombre="pentatonicaBlues"
escala(9).nropasos=6
escala(9).pasos = @pentatonicaBlues(1)

escala(10).nombre="pentatonicaNeutral"
escala(10).nropasos=5
escala(10).pasos = @pentatonicaNeutral(1)

escala(11).nombre="octatonicHW"
escala(11).nropasos=8
escala(11).pasos = @octatonicHW(1)

escala(12).nombre="octatonicWH"
escala(12).nropasos=8
escala(12).pasos = @octatonicWH(1)

escala(13).nombre="ionian"
escala(13).nropasos=7
escala(13).pasos = @ionian(1)

escala(14).nombre="dorian"
escala(14).nropasos=7
escala(14).pasos = @dorian(1)

escala(15).nombre="frigia"
escala(15).nropasos=7
escala(15).pasos = @frigia(1)

escala(16).nombre="lidia"
escala(16).nropasos=7
escala(16).pasos = @lidia(1)

escala(17).nombre="mixolidia"
escala(17).nropasos=7
escala(17).pasos = @mixolidia(1)

escala(18).nombre="aeolia"
escala(18).nropasos=7
escala(18).pasos = @aeolia(1)

escala(19).nombre="locria"
escala(19).nropasos=7
escala(19).pasos = @locria(1)

escala(20).nombre="algeriana"
escala(20).nropasos=8
escala(20).pasos = @algeriana(1)

escala(21).nombre="arabianA"
escala(21).nropasos=8
escala(21).pasos = @arabianA(1)

escala(22).nombre="arabianB"
escala(22).nropasos=7
escala(22).pasos = @arabianB(1)

escala(23).nombre="aumentada"
escala(23).nropasos=6
escala(23).pasos = @aumentada(1)

escala(24).nombre="disminuidaAuxiliar"
escala(24).nropasos=8
escala(24).pasos = @disminuidaAuxiliar(1)

escala(25).nombre="disminuidaAumentada"
escala(25).nropasos=6
escala(25).pasos = @disminuidaAumentada(1)

escala(26).nombre="disminuidaAuxiliarBlues"
escala(26).nropasos=8
escala(26).pasos = @disminuidaAuxiliarBlues(1)

escala(27).nombre="balinese"
escala(27).nropasos=5
escala(27).pasos = @balinese(1)

escala(28).nombre="blues"
escala(28).nropasos=6
escala(28).pasos = @blues(1)

escala(29).nombre="bisantina"
escala(29).nropasos=7
escala(29).pasos = @bisantina(1)

escala(30).nombre="china" 
escala(30).nropasos=5
escala(30).pasos = @china(1)

escala(31).nombre="chinaMongolia"
escala(31).nropasos=5
escala(31).pasos = @chinaMongolia(1)

escala(32).nombre="diatonica"
escala(32).nropasos=5
escala(32).pasos = @diatonica(1)

escala(33).nombre="disminuida"
escala(33).nropasos=8
escala(33).pasos = @disminuida(1)

escala(34).nombre="disminuidaAmedias"
escala(34).nropasos=8
escala(34).pasos = @disminuidaAmedias(1)

escala(35).nombre="disminuidaEntera"
escala(35).nropasos=8
escala(35).pasos = @disminuidaEntera(1)

escala(36).nombre="disminuidaTonoEntero"
escala(36).nropasos=7
escala(36).pasos = @disminuidaTonoEntero(1)

escala(37).nombre="dominante7h"
escala(37).nropasos=7
escala(37).pasos = @dominante7h(1)

escala(38).nombre="dobleharmonica"
escala(38).nropasos=7
escala(38).pasos = @dobleharmonica(1)

escala(39).nombre="egipcia"
escala(39).nropasos=5
escala(39).pasos = @egipcia(1)

escala(40).nombre="española8Tonos"
escala(40).nropasos=8
escala(40).pasos = @espanola8Tonos(1)

escala(41).nombre="engimatic"
escala(41).nropasos=7
escala(41).pasos = @engimatic(1)

escala(42).nombre="etiopiaAraray"
escala(42).nropasos=7
escala(42).pasos = @etiopiaAraray(1)

escala(43).nombre="etiopiaGeezYEzel"
escala(43).nropasos=7
escala(43).pasos = @etiopiaGeezYEzel(1)

escala(44).nombre="MitadDisminuidaLocrian"
escala(44).nropasos=7
escala(44).pasos = @MitadDisminuidaLocrian(1)

escala(45).nombre="MitadDisminuida2Locrian2"
escala(45).nropasos=7
escala(45).pasos = @MitadDisminuida2Locrian2(1)

escala(46).nombre="Hawaiana"
escala(46).nropasos=7
escala(46).pasos = @Hawaiana(1)

escala(47).nombre="Hindu"
escala(47).nropasos=7
escala(47).pasos = @Hindu(1)
/'
escala(48).nombre=""
escala(48).nropasos=7
escala(48).pasos = @(1)

escala(49).nombre=""
escala(49).nropasos=7
escala(49).pasos = @(1)

escala(50).nombre=""
escala(50).nropasos=7
escala(50).pasos = @(1)
'/

' 5.7.6.00 tresillos
espacio=0:notacur=1 '16-05-2021 jmg
/'
Dim  Shared figura (1 To 182) As ZString  * 4 => _
{" O "," P "," I "," L "," F "," E "," X "," H "," W ",_  '  1  9
 " O4"," P4"," I4"," L4"," F4"," E4"," X4"," H4"," W4",_  ' 10 18
 " O*"," P*"," I*"," L*"," F*"," E*"," X*"," H*"," W*",_  ' 19 27
 " O:"," P:"," I:"," L:"," F:"," E:"," X:"," H:"," W:",_  ' 28 36
 "3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45 
 "@ ","% ","& ","/ ","// ","{ ","{{ ","# ","## ",_  ' 46 54 
 "@.","%.","&.","/.","//.","{.","{{.","#.","##.",_  ' 55 63
 "@*","%*","&*","/*","//*","{*","{{*","#*","##*",_  ' 64 72 
 "@:","%:","&:","/:","//:","{:","{{:","#:","##:",_  ' 73 81
 "3@","3%","3&","3/","3//","3{","3{{","3#","3##", _ ' 82 90 
 " O+"," P+"," I+"," L+"," F+"," E+"," X+"," H+"," W+",_  ' 91 99 
 "O4+","P4+","I4+","L4+","F4+","E4+","X4+","H4+","W4+",_  ' 100 108
 "O*+","P*+","I*+","L*+","F*+","E*+","X*+","H*+","W*+",_  ' 109 117 
 "O:+","P:+","I:+","L:+","F:+","E:+","X:+","H:+","W:+",_  ' 118 126
 "3O+","3P+","3I+","3L+","3F+","3E+","3X+","3H+","3W+", _ ' 127 135 
 "@+","%+","&+","/+","//+","{+","{{+","#+","##+",_  ' 136 144 
 "@4+","%4+","&4+","/4+","//4+","{4+","{{4+","#4+","##4+",_  '145 153
 "@*+","%*+","&*+","/*+","//*+","{*+","{{*+","#*+","##*+", _ '154 162
 "@:+","%:+","&:+","/:+","//:+","{:+","{{:+","#:+","##:+",_  '163 171 
 "3@+","3%+","3&+","3/+","3//+","{3+","3{{+","3#+","3##+", _ '172 180
 "   ","|| " } ' 181 182 - 109 110, son 108 (old 73 74 son 72)
'/
' redonda,blanca,negra,corchea, semicorchea,fusa,semifusa,cuartifusa,garrapatea
' 4      ,2     , 1   ,1/2,   1/4          , 1/8,1/16    ,1/32      , 1/64   
Dim  Shared figura (1 To 190) As ZString  * 4 => _
{" O "," P "," I "," L "," F "," E "," X "," H "," W ",_  '  1  9
 " O4"," P4"," I4"," L4"," F4"," E4"," X4"," H4"," W4",_  ' 10 18
 " O*"," P*"," I*"," L*"," F*"," E*"," X*"," H*"," W*",_  ' 19 27
 " O:"," P:"," I:"," L:"," F:"," E:"," X:"," H:"," W:",_  ' 28 36
 "3O ","3P ","3I ","3L ","3F ","3E ","3X ","3H ","3W ", _ ' 37 45 
 "sO ","sP ","sI ","sL ","sF ","sE ","sX ","sH ","sW ",_  ' 46 54 
 "sO4","sP4","sI4","sL4","sF4","sE4","sX4","sH4","sW4",_  ' 55 63
 "sO*","sP*","sI*","sL*","sF*","sE*","sX*","sH*","sW*",_  ' 64 72 
 "sO:","sP:","sI:","sL:","sF:","sE:","sX:","sH:","sW:",_  ' 73 81
 "s3O","s3P","s3I","s3L","s3F","s3E","s3X","s3H","s3W", _ ' 82 90 
 " O+"," P+"," I+"," L+"," F+"," E+"," X+"," H+"," W+",_  ' 91 99 
 "O4+","P4+","I4+","L4+","F4+","E4+","X4+","H4+","W4+",_  ' 100 108
 "O*+","P*+","I*+","L*+","F*+","E*+","X*+","H*+","W*+",_  ' 109 117 
 "O:+","P:+","I:+","L:+","F:+","E:+","X:+","H:+","W:+",_  ' 118 126
 "3O+","3P+","3I+","3L+","3F+","3E+","3X+","3H+","3W+", _ ' 127 135 
 "sO+","sP+","sI+","sL+","sF+","sE+","sX+","sH+","sW+",_  ' 136 144 
 "sO4+","sP4+","sI4+","sL4+","sF4+","sE4+","sX4+","sH4+","sW4+",_  '145 153
 "sO*+","sP*+","sI*+","sL*+","sF*+","sE*+","sX*+","sH*+","sW*+", _ '154 162
 "sO:+","sP:+","sI:+","sL:+","sF:+","sE:+","sX:+","sH:+","sW:+",_  '163 171 
 "s3O+","s3P+","s3I+","s3L+","s3F+","s3E+","s3X+","s3H+","s3W+", _ '172 180
 "   "," ||",">  "," - "," N ","   ","   ","   ","  "," 0 " } '181 185 , son 185, ahora 190 
' la 185 la pondremos como 1er etapa de pasar  de ejecucion a Roll
' indicara una nota pero no sabemos su duracion, 2da etapa le pondremos
' una duracion cuantificada,,,
' silencios entre  46 a 90 y 136 a 180    
' los puntillos de tresillos coinciden con elvalor superior
' ej (3I)*=I=1.. 1/3 tresillo de negra con puntillo = negra
' (3L)* = L ..1/3 tresillo de corchea con puntillo = corchea
' ergo no hace falta simbolos adicionales para tresillos con puntillo
' el program debera reemplazar si se entra tresillo con puntillo por
' la figura sin puntillo...
' 4/3 4 fig en espacio de 3 ya lo tenemos por ejemplo 
' L*L*L*L*=III 
' 73 ESPACIO 74 FIN....181 espacio 182 FIN 
'cambiar 107 por 108, 109 por 110,  109 por 110.
' ligaduras se eligio todas con simbolo + 

Dim Shared As Integer comienzo1, fin1, comienzo2,fin2,PianoNota, _
loopDeThreads=0 ,sefini=0
'comienzo1=28:fin1=54
'comienzo2=82:fin2=180

''Dim Shared as any ptr thread1, thread2,threadPenta,thread3,pubi,threadloop,p1,threadMenu
Dim Shared As Any Ptr p2,tl1,tl2,tl3,tl4,tl5,tl6,tl7,tl8,tl9,tl10,tl11,tl12 

Dim Shared pun As Integer = 0 'entrada con punto = 1 con puntillo
Dim Shared silen As Integer = 0 'silencio entrada con s = 1 con silencio
Dim Shared mas As Integer = 0 ' + continuacion o ligadura
Dim Shared  As Integer incr=0 , entroActrlM =0, salgoDeCtrlM=0
Dim Shared tres As Integer = 0
Dim shared doblepun As Integer =0
Dim Shared cuart As Integer=0 'nro q indica sumarle 1/4 de su valor 
'' futuro=> Dim notmus(13 To 44) As String = {} bravura fonts?? pero son glyphs
''los deberia cargar cairo...experimentar...tarea futura
' 32 tipo de duraciones basicas encendidas o apagadas y lo mismo con puntillo
' I** negra con 2 puntilos seguidos tiene sentido?
' es I+L+L+F=I+I+F .' entre las notas entradas...
' la tecla para entrar puntillo el punto (.), para entrar silencio la s
' ej tecleo 1+.+A=O* en linea A dela octava que sea
' ej tecleo s+1+.+A =sO* en A..
' tambien en vez de estos simbolos usare los de un pentagrama comun,,,,futuro
Dim Shared As BOOLEAN ayuda, SeleccionarNuevaNota, ArmarMenuModif, VolverEntradaDatos,dobleclick 
haycompas=0
dobleclick=>FALSE

Dim Shared TipoCompas As UByte
Const As UByte Tcompas2_4 = 6
Const As UByte Tcompas3_4 = 7
Const As UByte Tcompas4_4 = 8
Const As UByte Tcompas6_8 = 14
Const As UByte Tcompas12_8 = 20

TipoCompas=Tcompas4_4

Dim Shared d7 As Integer => 10000000 ' PROBAREMOS CON 12/8 O 6 NEGRAS,,,15 MILL
Dim Shared d6N As Integer => 15000000 ' 6 negras o 12/8 o 6/4
Dim Shared d3N    As Integer => 7500000 ' 3 negras 3/4
Dim Shared d11 As Integer => 100000000000
Dim Shared d4 As Integer => 10000 'para parametrizar menu roll 
' comienza el compas , se calculara dinamicamente,,,según posicion  y la historia anterior
' del mismo array (acumulado, nro Compas, posicion)
Dim Shared As Integer TipoAcorde=0 '1 simple,2 acordes iguales, 3 acorde  distintos
espacio=0:borrar=0:cambiadur=0:InicioDeLectura=0
menuAccion=0:insert=0:agregarNota=0:octavas=0
' Const LEFTBUTTON = 1 
' Const MIDDLEBUTTON = 4 
' Const RIGHTBUTTON = 2 
' Const SHOWMOUSE = 1
' Const HIDEMOUSE = 0 
Dim Shared As Integer MousePress = 0, MouseMove=0 '02-07-2021 no sirven
Type mouse
 As Integer res
 As Integer x
 As integer y
 As Integer  wheel
 As Integer  clip
 Union
 buttons As Integer
 Type
  Left:1 As Integer
  Right:1 As Integer
  middle:1 As Integer
 End Type
 End Union
End Type

Dim shared As mouse m
Dim Shared As Double old_btn_press_time, new_btn_press_time,dbl_click_time,redimensionar=0
dbl_click_time = 0.18
Common shared As Integer menuNro, menuNew, desde , hasta, rango,RollDur,RollNota,compasX,RollDurOld,RollNotaOld

''Declare Function MessageBox Alias "MessageBoxW"(n1 As Integer,s1 As Wstring,s2 As Wstring,MB As Integer) As Integer:Sleep 1
Dim Shared As Integer x, y, buttons, res,xscreen,gap1,gap2,gap3
common Shared As float anchofig
Dim Shared as Integer StartInsert,indaux=0,NroCol, carga, curpos=0, curposold, resultado, indicePos,curpos1, mouseyOld,indicePosOld,indicePosUltimaGrupo
Dim Shared As Integer  Xjreset(1 To 100, 1 To 2),indXjreset=0,indXjresetOld,curposClickDErecho,curposClickIzquierdo  
resultado=0
' este pesodur lo deberia cambiar a ticks
Dim Shared As Integer pesoDur (0 To 182)  => {0, _ 'era 108
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '1  9
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '10 18
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '19 27
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '28 36
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '37 45
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '46 54 
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '55 63
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '64 72
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '73 81
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '82 90
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '91 99
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '100 108
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '109 117
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '118 126
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042, _ '127 135
10000000,5000000,2500000,1250000, 625000,312500,156250, 78125,39063, _ '136 144
12500000,6250000,3125000,1562500, 781250,390625,195313, 97656,48828, _ '145 153
15000000,7500000,3750000,1875000, 937500,468750,234375,117187,58594, _ '154 162
17500000,8750000,4375000,2187500,1093750,546875,273438,136719,68360, _ '163 171
 6666666,3333333,1666666, 833333, 416666,208333,104166, 52083,26042,0,0 }  '172 182

Dim Shared As String nombreArchivo, TCompas,menuOldStr, ticksdefault,nombrePatron, intervaloTxt
Dim Shared As Integer nroCompasesPatron, nroTicksPatron
nroCompasesPatron=0
nroTicksPatron=0
nombreArchivo="" : ticksdefault="64"

TCompas ="4/4"
'Dim Shared As Integer MaxPosTrack(0 To 32) ' cada track tendra su max pos
'Dim Shared As Integer posnTrack(0 To 32) ' cada track tendra su posn
Dim Shared As Integer fueradefoco=0 ,onoff=0

Type rangoOct Field=1
 As Integer desde = 0
 As Integer hasta =0
 As Integer NB =0
 As Integer NA =0
 As Integer MaxPos =0
 As Integer posn =2 
 As UByte   notaold=0 
 As Integer Ticks =0
 As UByte   patch
 As UByte   notaescala
 As UByte   tipoescala
 As UByte   alteracion  ' sos 3, bem 2
 As Double  fechasPistas
 As UByte   canalsalida
 As UByte   canalentrada
 As UByte   portout      ' dispositivo midi de salida 
 As UByte   zona1
 As UByte   zona2
 As UByte   nroRep
 As UByte   portin 
 As UByte  tipoCompas
 As UByte   ejec
 As UByte    vol
 As Integer  tiempopatron ' 240 60 etc
 As UByte   pan
 As UByte   Eco
 As UByte   Coro
 As Integer ajuste
 As UByte   sonido
 As UByte   vibrato
 As UByte   canalx
 As UByte   pitchbend
 As UByte   orden
End Type
' faltaria agregar grabar el nombre del dispositivo ........
Dim Shared As rangoOct pmTk (0 To 32)
Dim Shared As rangoOct pmEj (1 To 32)
Dim Shared As String titulosTk(0 To 32)
Dim Shared As String pistasTk (0 To 32) ' titulos temporarios en el proceso de borradod e pistas en la lista
Dim Shared As String titulosEj(1 To 32)
Dim Shared As String pistasEj (1 To 32)
  
Dim Shared As Integer ContadorError=0   
' Si revasa el ultimo,8 16,24,32,40,48,56,64, divido por 8
' tengo elindice del revsado 1,2,3,4,5,6,7,8....
' comprobar qellimite este entre 1 a 8 

' se repiten 3 veces el patron 1 a 16 por comodidad lo repetimos.
' equivalencias L+F=4 notas en 5=4:5, 4 figuras en 5 negras.
' 5I=12500000,5I/4=3125000=I+F o sea I+F I+F I+F I+F = 4 en 5
' podria ponerlo asi IF IF IF IF. IF=3125000 NUEVO VALOR A AGREGAR?
' VALORES LIGADOS EN UNA SOLA FIGURA FACIL PARA CARGAARLO PONGO
' I-F O SEA PULSO 3 LUEGO PULSO - Y LUEGO PULSO F = 3-5 = IF
'  ASI PUEDO LIGAR LAS NOTAS QUE QUIERO...LAS NOTAS LIGADAS
'LA SPONDRE AL FINAL DEL VECTOR...! CUANTAS TENGO?
'
' - 4 figuras en 3 negras. si tomamos como base el 4/4
' 3 negras=7500000 / 4 = 1875000=L*
' o sea 4 L* =4/3=>L*L*L*L*=III (4/3 lo tengo)
' DOBLE PUNTILLO : ó ** formula x,x+1,x+2 unidos
'Ctrl nro 1,2,3,4,5,6,7,8 9 notiene sentido
' 123 234 345 456 567 678 789 son7 doblepuntilo simple

Dim Shared As Integer menuMouse, savemousex, savemousey, usamousex, usamousey, _
modifmouse, EnOctava,nsE,nR,lugarOld, nroClick, posinterna, indCompas,menuabierto=0,nROld
Dim Shared As ubyte nC=0,nF=0,nRk=0, cnc=0,cnf=0,lockfont=0, lockhoriz=1

Dim Shared As Double t1k,t2k,DURK,DURK1,DURk2, DURKOLD,DURK144, DURK128,DURK144old, DURK128old,cont144, cont128,huboerror=0

Dim Shared As Double notaMidiDurk,notaMidiDurkOld,penta2=0
'Dim Shared As Integer abrirMIDIin=0
' nC indica 144, nF indica 128

Type poli Field=1 ' para guardar la secuencia EN Tacks 15 bytes
 dur    As UByte =0   ' duracion 
 sonido   As UByte =0   ' SONIDO ON/OFF ? se usa?
 canal  As UByte =0   '  
 onoff  As UByte =0   ' nota on=2, nota off=1  
 ejec   As UByte =0   ' marca viene de un ejec=1, no viene de ejec=0 
 eco    As UByte =0   ' era dur6  
 patch   As UByte =0   '  
 nanchofig As UByte =0   '  
 nota   As UByte =0 ' en un futuro contendra nota, octava, canal etc 
 vol    As UByte =0 ' volumen
 pan    As UByte =0 ' paneo
 pb     As UByte =0 ' pitch bend
 nnn    As UByte =0 ' se usa para escala canal etc 
 tick   As UByte =0 ' 64 tiene la redonda 
 acorde As UByte =0 ' 1 a 12 , son el se hara el sort    
End Type
' posn As Integer =0' de roll todavia no lo uso para generar secuencia
' comentarios Para Futuro:
' tick y posn seria para tener una relacion entre ticks los 128, y la posicin 
' de roll, o sea  en que posicion o columna esta la nota en Roll
' acorde, no se si seria necesario quiere indicar si hay o no un acorde
' una forma de disminuir el algoritmo de lectura posterior....a verlo....
Type sec
 As poli trk(Any, any)
End Type

'-------------------------------
' grabaPos y GrabaLim estan completos no quedo nada libre deberia grabar una tercera
' o grabar en la ocatva libre de NA> NA-13, creo debo poner un block mas para
' dejar al octava libre para los cambios dinamicos como cifrado y varias cosas mas
' como repeticion , loop etc y aque esta en vez d poner 1 solo bloque pondremos 2 mas
' 1ero modificare la grabacion para leer en formato viejo y grabar en el nuevo..
' DAT ES SOLO VISUAL PARA TOCAR SE USARA MAS TRACK DONDE ESTA EL CAMPO ON OFF
'Type poli Field=1 ' para guardar la secuencia
' dur   As UByte =0   ' duracion 
' dur2  As UByte =0   ' SONIDO ON/OFF 
' canal As UByte =0   '  
' dur4  As UByte =0   ' nota on=2, nota off=1 <===CAMPO ON OFF
' EL ON ES APENAS APARECE LA NOTA EL OFF APARECERA EN LA SECUENCIA Y NO 
' HABARA QUE CALCULAR NADA,,
' REDEFINE USHORT muevo a este type el tempo, luego puedo copiar sus partes
' a pan y pb y poder reconstruir ..es un redefine de cobol!
  
' campos de roll datos secuencia 
Type datsec Field=1
 nota  As UByte =0 ' 1 a 12, en un futuro contendra nota, octava, canal etc 
 dur   As UByte =0 ' duracion 1 a 180, tambien tendra rasguidos distintos programables por usuario o fijos
 vol   As UByte =0 ' volumen hasta 127 es volumen desde ahi es escala 128 a 255 =127 escalas
 pan   As UByte =0 ' alteracion  bemoL o sostenido
 pb    As UByte =0 ' acorde 201 202
 inst  As UByte =0 ' instrumento para cada nota podra ser distinto 1 to 128
 onoff As UByte =0 ' 2 on , 1 off
End Type

' datos roll encabezado 70 BYTES, solo campos ubyte
Type rolldat Field=1 'con esto se define roll tendra pan,vol,nota,dur,pb,inst variables
 x1    As UByte =0  'z.nota
 x2    As UByte =0  'z.dur
 x3    As UByte =0  'z.vol
 x4    As UByte =0  'z.pan
 x5    As UByte =0  'z.pb
 tipoescala_num_ini As UByte 'z.inst
   librezonoff As UByte ' z.onoff----------- 7
 desde   As UByte =0 'zlim.nota 
 hasta   As UByte =0 'zlim.dur
 notaescala_num_ini As UByte =0   ' zlim.vol 
 alteracion As UByte =0   'zlim.pan
 notaold  As UByte =0 ' zlim.pb   
 canalx   As UByte =0 'zlim.inst 
 ejec    As UByte =0 'zlim.onoff ---- 14 xxxxx nuevo
 patch    As  UByte =0 'z3.nota
 portout As   UByte =0  'z3.dur
 nanchofig  As UByte  =0   'z3.vol nanchofig*10 cuando lo uso lo dividopo 10
 vol As UByte = 90 '''' librez3pan    As UByte =0  'z3.pan
 TipoCompas  As UByte =0 'z3.pb
 canalsalida As UByte =0 'z3.inst
   librez3onoff     As UByte =0 'z3.onoff --- 21
   librez4nota As UByte =0  'z4.nota
   librez4dur As UByte =0  'z4.dur
   librez4vol As UByte =0  'z4.vol
 tiempoPatron1 As UByte=0 'z4.pan
 tiempoPatron2 As UByte=0 'z4.pb
   librez4inst As UByte =0 'z4.inst
   librez4onoff As UByte =0 'z4.onoff ---28
 eco    As UByte =0       'z5.eco  
 pan    As UByte =0       'z5.pan
 coro   As UByte =0 'z5.canalsalida repetido
   libreportout     As UByte =0 'z5.portout repetido
   librepatch       As UByte =0 'z5.patch repetido
 pitchbend   As UByte =0 'z5.pitchbend
 vibrato   As UByte =0   'z5.vibrato ---35
'---vienen 35 ubyte
 mas1 As UByte =0
 mas2 As UByte =0
 mas3 As UByte =0
 mas4 As UByte =0
 mas5 As UByte =0
 mas6 As UByte =0
 mas7 As UByte =0
 mas8 As UByte =0
 mas9 As UByte =0
 mas10 As UByte =0
 mas11 As UByte =0
 mas12 As UByte =0
 mas13 As UByte =0
 mas14 As UByte =0
 mas15 As UByte =0
 mas16 As UByte =0
 mas17 As UByte =0
 mas18 As UByte =0
 mas19 As UByte =0
 mas20 As UByte =0
 mas21 As UByte =0
 mas22 As UByte =0
 mas23 As UByte =0
 mas24 As UByte =0
 mas25 As UByte =0
 mas26 As UByte =0
 mas27 As UByte =0
 mas28 As UByte =0
 mas29 As UByte =0
 mas30 As UByte =0
 mas31 As UByte =0
 mas32 As UByte =0
 mas33 As UByte =0
 mas34 As UByte =0
 mas35 As UByte =0 
End Type ' tambien se usa para el encabezado
' el encabezado deberia ser distintos todos
''202 ' codigo de exsitencia de cifrado en el cabezado
' por cada 201 en el INTERESPACIO hay un 202 de una octava de roll , en track solo
' existe el 202 en el encabezado
' con esta info reconstruyo el acorde que luego muestro solo en la octava
' la octava la se por donde esta el 201 no hace falta guardarla, peRo para pasarla a track si
' hace falta!! 27-01-2022 -. YA INCORPORADO A TRACK
' Lo maximo que uso son 8 posiciones para las octavas de 0 a 7 o 1 a 8 , como tengo 13
' posiciones verticales en la ultima octava, me quedan 5 posiciones libres (13-8) y tambien
' como la estructura dat tiene 6 (8 ahora) campos me quedan 7 campos en las 8 primeras ,acorde solo
' usa una la pb (201 202)=56
' y todas las 8 en las 5 siguietnes = 40 , +56 = 96 sitios donde poner informaicon para una posicion dada
' las repeticiones las colocaremos en la 1era de la posicion libre o sea en este caso 
' la posicion 98 como si fuera una octava 9 ficticia...las repeticiones no dependen de la
' octava todas las pistas se repiten al unisono. solo hace falta inforamcion
' arriba en al octava que no se usa. REVEEER ESTO NO SE ENTIENDE QUE PASA CON EL CAMBIO,,
' ---------


 ' Nota de escala son 12 ..bemol o sostenido son 2, 1 a 12 sostenidos, 13 a 24 bemoles
 ' entonces en 24 numeros tengo la info de nota
 ' octavas son 8 desde 15 a 20 son las octavas, 
' canal son 16 de 21 a 36 ... etc etc pero no se hizo de esa forma... 
 ''t   As Ulong   '  ticks por ahroa no 

' dentro del vol pondre mos las escalas
' chords http://www.looknohands.com/chordhouse/piano/ ahi hay 168 escalas..!!
' en vol tengo desde 129 a 255 para numerar escalas. si faltan puedo usar pan o pb
' la idea es poner en que escala esta cada nota o compas y asi poder tener cambios de escala
' y construir los acordes que se quieran construir en esa escala de esea nota o del compas o 
' la escala del ultimo cambio...por default la escala sera C mayor..la 129
' Nombre de Nota de la escala C,C#,..B son 12 129 a 140, ocupara una posicion
' de tick la mas chica usada es 6 asi que un retardo de 1 o 2 no afectara
' o veo de saltar esos datos no tomandolo como tick sino de control 
'''--------------------------------- nota vieja ----- ver si sirve 
 ' Nota de escala son 12 ..bemol o sostenido son 2
 ' entonces en 14 numeros tengo la info
 ' 129 -> c,130->c#,131->d...140->B--, 141-sos,142,bemol
 ''t   As Ulong   '  ticks por ahroa no 

' dentro del vol pondremso las escalas
' chords http://www.looknohands.com/chordhouse/piano/ ahi hay 168 escalas..!!
' en vol tengo desde 129 a 255 para numerar escalas. si faltan puedo usar pan o pb
' l aidea es poner en que escala esta cada nota o compas y asi poder tener cambios de escla
' y construir los acordes que se quieran construir es esa escala de esea nota o del compas o 
' la escala del ultimo cambio...por default la escala sera C mayor.. 
' ---------------------------------fin nota vieja --------------- 
Type inst
 As datsec trk(Any, Any)
End Type

Type paso Field=1
 Posi As Integer =0
 nro  As Integer =0
End Type

Type pasa Field=1 
  As cairo_t Ptr c
  As inst Roll
  As String  titulo = ""
  As Integer ancho =0
  As Integer alto=0
  As Integer ubiroll=0
  As Integer ubirtk=0
  As Integer encancion=0
  As Integer midionof=0
End Type


Declare Sub Tracks (ByRef nroTrack As integer, ByRef nroCanal As UByte, Roll As inst)
'Dim Shared As Long PARAR_PLAY_MANUAL = NO,PARAR_PLAY_EJEC = NO,CONTROL3 = NO
Dim Shared As Long CONTROL3 = NO
common Shared As Integer NB , NA, CantTicks, tempo, CantMin,CantCompas
CantTicks=CantMin * 60 *96 ''86400  ' 96*15*60 96 ticks una negra, 15 minutos, 60 segundos
Dim Shared As inst Roll ' para visualizar y tocar
 
ReDim Shared Track  (0 To 32) As sec ' tracks para guardar,.. y tocar 

Dim Shared ix As Integer=0
Dim Shared As String titu, cadenaes,cadenaes_inicial,tipoescala_inicial, notaescala_inicial 
Dim Shared As Integer instru=1, instruOld, tipoescala=1, tipoescalaOld=1, notaescala=1,notaescalaOld
Dim Shared As Integer tipoescala_num = 1, notaescala_num = 1,tipoescala_num_ini=1,notaescala_num_ini=1
Dim Shared As String notas_esc_inicial (1 To 24)

Dim Shared As Integer desdevector,undo_kant_intervalos(1 To 500)
Dim Shared As Integer hastavector,abrirRoll=NO_CARGAR '',abrirRollCargaMidi=0   
Dim Shared As String grado_inicial(1 To 24)
' memorias de Tracks..por ahora igual que Roll de trabajo luego veremos si achicamos
' mas parecido a midi.,,,,8 tracks ocupasn 0,544 giga 32 1,8 gigas de memoria virtual
'Dim Shared As Integer lim2=12, ctres=CantTicks
Declare Sub calcCompas(Roll As inst)

Dim Shared As Integer lim2=12,mel_undo_k=0,lim3=25,cnt_acor=0,ig=0
Dim Shared As Boolean Vaciodur=FALSE,ResetVariables=FALSE
Type notas_agregadas Field=1
  As Integer pn ' pianoNota
  As Integer posn
  As UByte   dur
  As UByte   nota
  As UByte   onoff  ' para ticks

End Type

Type melodia_undo Field=1
  As Integer trk  ' NRO DE TRACK POR DEFAULT 0
  As Integer posn
  'As notas_agregadas columna
End Type
Dim Shared As melodia_undo mel_undo(1 To 5000)

' el acorde tiene 1,3,5,7 aca almacenamos 3,5,7 pero no la funamental
' esa es el origen donde pulso Ctrl-click derecho, si suponemo teorico
' un acorde de 12 notas (raro), son 11 los lugares distintos de la funfamental
Dim Shared As notas_agregadas  undo_acorde(1 To 500, 0 To 11) ' undo para 1 acorde 
' entrado de hasta 12 notas adicionales  
' c/inst puede tocar como una persona hasta 12 notas juntas de acorde
' entonces no se justifica tener en un solo instrumento una polifonia mas de 12
' ni de 108,,,ergo puedo poner mas trakcs o mas longitud
' asi ocupa,recalcular.., 270k , 64 ocupara 550 y 128 ocupara 1 giga...listo!
' lo hare de 32 tracks 275 mgbytes !
'CREAR UN MENU ACA DE CREACION DE TRACKS SEGUN SE ELIJA SE VAN EJECUTANDO LOS
' REDIM Y LUEGO SE PUEDE ELEGIR CADA UNO PARA EDITAR, PARA ELLOS
' SE PASARA SU NOMBRE A RollLoop EN UN SELECT CASE , CASE1 ROLL1 CASE2 ROLL2 ETC
' debo modificar todo el programa para incorporar el indice del inst o Track
' Sigo enviando  Roll, pero cuadno lo uso debo especificar cual track estoy usando
' creo 4 tracks por default a pedido del ususraio se pueden agregar mas
'=============================================================
' ctres SERA DINAMICO A MEDIDA QUE SE ENTRA MAS NOTAS SE IRA EXTENDIENDO LAS POSICIONES
'============== IMPLEMENTARLO AL FINAL 
Dim Shared As inst RollAux
common Shared As Integer  ANCHO,ALTO,ANCHOSYSTEM, ALTOSYSTEM
Dim Shared As Integer   deltaip=0,scan_alt=0,scan_d=0,mueveHorizontalmayor50=0
Dim Shared As Double   BordeSupRoll, inc_Penta,arranqueDo1=0
Dim Shared As Integer  AnchoInicial,AltoInicial,ibarre
ibarre=desde   
Common Shared As FLOAT font
Dim Shared As float  deltaipf=0, lockip=0 
Dim Shared q As String * 1
Dim Shared As UByte  ii,indf,indfa,s1, s2, s3, s4, s5,s6, s7 ,s8, s9,s10
Dim Shared As UByte numdurasmidi( 1 To 24)
''Dim Shared escala As float = 1.0
Dim Shared translado As Integer = 1
' CAIRO NO SOPORTA LA ñ!!! ESO ERA TODO!!!!
Dim shared As Integer   posmouse, posmouseOld,incWheel, edity1,edity2,octavaloop
Dim Shared As Integer octaroll,cierroedit= 0,cierroport=0,COMEDIT
Dim Shared As BOOLEAN  resize
Const As Integer LECTURA=1 ' 0 0
Const As Integer ENTRADA_NOTAS=2 ' 0 0 
Const As Integer SOLO_MODIFICACION=3  '1 1 
Const As Integer MODIFICACION_INSERCION=4 ' 0 2
Const As Integer MODIFICACION_COLUMNA=5

COMEDIT=LECTURA

Dim Shared po As Integer Ptr
Dim Shared e As EVENT
Dim Shared rmerr As Integer

Dim Shared cm As cairo_t  Ptr
Dim Shared c3 As cairo_t  Ptr
Dim Shared As Integer stride, IhWnd,Mhwnd,Style,desktopwidth,desktopheight,creapista=0

Dim Shared comienzo As Integer = 0
'ScreenControl  SET_DRIVER_NAME,"GDI"
Dim Shared As hWnd hwnd,hwndMenu 

'hwnd = Cast(hwnd,IhWnd)
'hwndmENU = Cast(hwnd,MhWnd)

Dim Shared As UINT codsalida=0
Dim shared As Any Ptr lpExitCode
Dim Shared As String TipoFrac ' para fraccionar notas segun estilo deseado
' suponemos 64 divisiones dentro de un compas semifusa...y 5000 compases
'Dim Shared Guia (1 To 300000  ) As ULong
' la idea es poner las posiciones de track o roll dentro de este otro
' array, el array salta de una posicion a otra en 1/128 en tiempo relativo
' entonces para poner una negra por ejemplo si su inicio va en 1 entonces
' un salto de 128 seria una redonda, 64 una blanca y 32 una negra
' ergo la negra se representa comienzo en 1 fin en 32-33 o sea empeiza en 1
' y termina en el comienzo de 33.
'---------------------

ANCHO = GetSystemMetrics(SM_CXSCREEN)
ALTO = GetSystemMetrics(SM_CYSCREEN)
ANCHOSYSTEM=ANCHO
ALTOSYSTEM=ALTO
ANCHO = ANCHO *11/12
ALTO = (ALTO -25)*11/12
AnchoInicial=ANCHO
AltoInicial=ALTO
anchofig=ANCHO/45 ' decia 700 SON 45 COL PERO SE USAN MENOS 41 JMGJMG
NroCol =  (ANCHO / anchofig ) + 4 ' 20 Tamaño figuras, nota guia 6 columnas "B_8_[ "
Dim Shared ANCHO3div4 As Integer 
 ANCHO3div4 = ANCHO *3 / 4
'vector relacion entre point y pixels
'6pt   	8 px 	0.5em 	50%
'7pt 	   9 px 	0.55em 	55%
'7.5pt 	10px 	0.625em 	62.5%
'8pt   	11px 	0.7em 	70%
'9pt 	   12px 	0.75em 	75%
'10pt 	13px 	0.8em 	80%
'10.5pt 	14px 	0.875em 	87.5%
'11pt 	15px 	0.95em 	95%
'12pt 	16px 	1em 	100%
'13pt 	17px 	1.05em 	105%
'13.5pt 	18px 	1.125em 	112.5%
'14pt 	19px 	1.2em 	120%
'14.5pt 	20px 	1.25em 	125%
'15pt 	21px 	1.3em 	130%
'16pt 	22px 	1.4em 	140%
'17pt 	23px 	1.45em 	145%
'18pt 	24px 	1.5em 	150%
'20pt 	26px 	1.6em 	160%
'22pt 	29px 	1.8em 	180%
'24pt 	32px 	2em 	200%
'26pt 	35px 	2.2em 	220%
'27pt 	36px 	2.25em 	225%
'28pt 	37px 	2.3em 	230%
'29pt 	38px 	2.35em 	235%
'30pt 	40px 	2.45em 	245%
'32pt 	42px 	2.55em 	255%
'34pt 	45px 	2.75em 	275%
'36pt 	48px  3em 	   300% 	
'px	1/96 of 1 inch (96px = 1 inch)	font-size: 12px
'pt	1/72 of 1 inch (72pt = 1 inch)	font-size: 12pt;
' mis valores
' font 8  son 5  pixels
' font 9  son 5.5 px
' font 10 son 6 px
'      11     7 
'      12     7.5
'      13     7.75   
'      14     8.75 
'      15     9.5
' font 16 son 9.75 pixels 
' font 18 son 12 pixels
' font 20 son 13 pixels
' font 34 som 21.5 pixels
Dim Shared As float mispx (1 To 30, 1 To 2) => { _ 'mispx (1,1)=5,mispx (1,2)=4
{5 ,4 } , _
{6 ,4 } , _
{7 ,5 } , _
{8 ,5 } , _
{9 ,6 } , _
{10,6 } , _
{11,7 } , _
{12,7 } , _
{13,8 } , _
{14,9 } , _
{15,9 } , _
{16,10} , _
{17,11} , _
{18,12} , _
{19,12} , _
{20,13} , _
{21,13} , _
{22,14} , _
{23,14} , _
{24,15} , _
{25,15} , _
{26,16} , _
{27,16} , _
{28,17} , _
{29,17} , _
{30,18} , _
{31,19} , _
{32,20} , _
{33,20} , _
{34,21} }

Dim Shared As float ptpx(1 To 28, 1 To 2) => { _
{ 6   , 8  },_
{ 7   , 9  },_
{ 7.5 , 10 },_
{ 8   , 11 },_
{ 9   , 12 },_
{ 10  , 13 },_
{ 10.5, 14 },_
{ 11  , 15 },_
{ 12  , 16 },_
{ 13  , 17 },_
{ 13.5, 18 },_
{ 14  , 19 },_
{ 14.5, 20 },_
{ 15  , 21 },_
{ 16  , 22 },_
{ 17  , 23 },_
{ 18  , 24 },_
{ 20  , 26 },_
{ 22  , 29 },_
{ 24  , 32 },_
{ 26  , 35 },_
{ 27  , 36 },_
{ 28  , 37 },_
{ 29  , 38 },_
{ 30  , 40 },_
{ 32  , 42 },_
{ 34  , 45 },_
{ 36  , 48 } }
          
          



     

 
'--------------------
    
#Include "mod_rtmidi_c.bi"
''#Inclib  "rtmidi.dll" 'usa librerias estaticas
#Inclib  "rtmidi" 
#Include "fbthread.bi"
#Include "crt.bi" ' QSORT

'----------------------------
Dim Shared  As String ProgError(0 To 17)
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

Static Shared As HWND cbxnum(1 To 32), cbxejec(1 To 32),cbxgrab(1 To 32),rbparar,rbgrabar,cbxejecout(1 To 32)
Static Shared As Integer sonidoPista(1 To 32)
static  Shared As Integer SuenaTodo =0, cntsuena=0
Static Shared  As Integer jgrb=0, jgrbRoll
Declare Sub PlayCancion(Track() As sec)
Declare Sub TrackaRoll (Track() As sec, ByVal ntk As Integer, Roll As inst,funcion As string)
Declare Sub CargaArchivo(Roll As inst, ByRef ubiroll As integer)
Declare Sub ReCalCompas()
Declare Sub menoryMayorEnColumna (Roll As inst, ejex As integer, Byref menor As ubyte, ByRef mayor As UByte,ByRef i1men As Integer, ByRef i1may As integer)
Declare Sub RollaTrack(Track() As sec, ntk As Integer,Roll As inst)
Declare Sub armarescala(ByRef cadena As String, tipoesc As Integer, notaesc As Integer, altera As String, orden As integer )
Declare Function lugarNota (lugar As integer) As Integer
Declare Sub metronomo ()
Declare Sub copiarPmtkaPmtk(r as Integer, e As integer)
''patchsal=Roll.trk(1,1).inst
Dim Shared As Any Ptr threadG 


ReDim Shared As Integer tt(1 To 32)
Dim Shared As Integer cual=0, Parar_De_Dibujar=NO
Dim Shared  midiin  (0 To 15) As  RtMidiInPtr  ' 16 dispo de entrada..mucho no¿?
Dim Shared  midiout (0 To 15) As  RtMidiOutPtr ' abrir hasta 16 dispositivos DE SALIDA
' parece que empieza desde cero 0 to 31 decia...el doble seria 0 to 63
Dim Shared As Integer teclado=0 ' salida de ejecucion teclado 

Common Shared midisal As RtMidiOutPtr, portsal As ubyte

Dim Shared as boolean vieneDeListport=FALSE

'Common Shared As Any Ptr thplayC
trasponer=0
pid1=0


cargaCancion=NO_CARGAR_PUEDE_DIBUJAR
Dim Shared  As pasa param ', param2 
    param.c= c
    param.Roll = Roll
    param.titulo = titu
    param.ancho = ANCHO 
    param.alto = ALTO
    param.ubiroll=0
    param.ubirtk=0
    param.encancion=SIN_CANCION
    p1=@param

' a disco puedo grabarsolo CargaIn para cada ejecucion y al cargarla reconstruir en memoria
' los Toca,,,,eso es!!! y sumando todas las partes y dividiento por TickChico obtengo 
' elmaxposy por ende el valor a dimensioar el Toca....
' este tipo de ejec lo podrmos ampliar si hace falta....tal vez no la estructura
' se reusa con modo para notas sysex efectos y mas no  creo tener que modificarla 
Type ejec Field=1
 As UByte modo
 As UByte nota
 As UByte vel
End Type
' en ejecparam faltaria el tempo! lo puedo grbar en Roll y luego en trk cuadno convierto!
' EN LA VERSWION NUEVA GRABARE ejecparam y ejecparam2. pero antes para indicar esto
' grabare un indicador de version 2  una string * 6 "ejec2 "
' entonces leo uan string  de 6 bytes si dice ejec2  es lanueva sino es vieja, con la nuevaa
' retrocedo hago seeek 0 y empiezo a leer de nuevo con ejecparm y luego ejecparm2.. si
' Si es vieja los 6 bytes seran de un double...
#include "datetime.bi"
''Dim FECHA AS double  ' como detectar que fecha o delta es fecha
'' de esta forma distingo version vieja 1 de la nueva 2
''FECHA=Now  '''Date
''Dim fecha1 As String
''fecha1 =Format (fecha ,"ddddd")
''Print fecha1 
''Dim result As Long
''result= ISDATE(FECHA1)
'' If result <> 0  THEN
'' PRINT "SI LO ES" ===> prueba que fecha es fecha!!
''ELSE
'' PRINT "NO LO ES"
''ENDIF

' => Now es una Dateserial
Type ejeccabeza Field=1  ' 45 bytes 
 As Double fecha '6 se carga con Now antes de grabar
 As String * 29  autor '29
 As UByte version  '1 se ira incrementando en 1 cada vez que se grabe
 As UByte mas1  'sobran 9 bytes para futuro
 As UByte mas2
 As UByte mas3
 As UByte mas4
 As UByte mas5
 As UByte mas6
 As UByte mas7
 As UByte mas8
 As UByte mas9
End Type
Type  ejecparam Field=1  'son 45 bytes ORIGINAL 
  As Double        delta  ' entre pistas de ejec 6 byes
  As String  * 29  nombre 'de cada Toca         29 bytes
  As  Integer      maxpos  '                     4 bytes              
  As  UByte        orden   'nro pista ejec       1 btye indicara si hay mas datos de encabezado con valor=0
  As  UByte        portout
  As  UByte        patch 
  As  UByte        canal
  As  UByte        canalent
  As  UByte        portin
End Type
' EN TRACKS Y ROLL NO ALMACENAMOS EL NOMBRE PARA ASOCIAR EL ORDEN CON EL NOMBRE
' EN EJEC IMPORTA COMO UBICAMOS LOS DATOS LA PISTA 1 SE TOCA PRIMER OLUEGO LA 2 
' Y ASI PROQUE ENTRE ELLAS HAY UN RETARDO DE COMIENZO, TALVEZ SI CAMBIO EL ORDEN
' NO PASA NADA DEBO VER LA LOGICA DE PLAY DE EJECUIOCNES COMO LA IMPLEMENTE
' SI ES RIGIDA O NO IMPORTA EL ORDEN,,,LUEGO WGUIMOS CON ESTAS ESTRUCTURAS
Type  ejecparam2 Field=1  'son 25 bytes mas para llegar a los 70 
  As UByte   sonido
  As UByte   pan
  As UByte   Eco
  As UByte   coro
  As UByte   vibrato ''' nuevo hay que ver los codigos
  As UByte   ejec
  As UByte   canalx
  As UByte  canalsalida 
  As UByte   vol
  As UByte  TipoCompas
  As UByte  tiempoPatron1 
  As UByte   tiempoPatron2 
  As UByte  pitchbend
''' 12 bytes mas 
 mas1 As UByte =0
 mas2 As UByte =0
 mas3 As UByte =0
 mas4 As UByte =0
 mas5 As UByte =0
 mas6 As UByte =0
 mas7 As UByte =0
 mas8 As UByte =0
 mas9 As UByte =0
 mas10 As UByte =0
 mas11 As UByte =0
 mas12 As UByte =0
End Type
/'
Type ejecparamNuevo
  As Double        delta  ' entre pistas de ejec 6 byes
  As String  * 29  nombre 'de cada Toca         29 bytes
  As  Integer      maxpos  '                     4 bytes              
  As  UByte        orden   'nro pista ejec       1 btye indicara si hay mas datos de encabezado con valor=0
  As  UByte        portout
  As  UByte        patch 
  As  UByte        canal
  As  UByte        canalent
  As  UByte        portin
'-------------------
  As UByte   sonido
  As UByte   pan
  As UByte   Eco
  As UByte   coro
  As UByte   vibrato ''' nuevo hay que ver los codigos
  As UByte   ejec
  As UByte   canalx
  As UByte  canalsalida 
  As UByte   vol
  As UByte  TipoCompas
  As UByte  tiempoPatron1 
  As UByte   tiempoPatron2 
  As UByte  pitchbend
''' 12 bytes mas 
 mas1 As UByte =0
 mas2 As UByte =0
 mas3 As UByte =0
 mas4 As UByte =0
 mas5 As UByte =0
 mas6 As UByte =0
 mas7 As UByte =0
 mas8 As UByte =0
 mas9 As UByte =0
 mas10 As UByte =0
 mas11 As UByte =0
 mas12 As UByte =0


End Type
'/

'' UFF ME FALTA tiempoPatronEjec DONDE LO GRABO ? A DISCO COMO SONIDO
' faltaba un parametro "sonido" para indicar si esta sonando o no
' pero si agrego ese param pierdo los archivos que ejecute,,,
' es necesario guardar que se reproducia o no ??? no me parece
' tan importante,,, per opodria ser necesario,,per obueno lo agregamos
' si la fecha del archivo es anterior a esta version usaremos
' ejecparam para cargar, si la fecha es mayor a la  de esta version
' usaremos ejecparam2 y listo ... solo debo detectar la fecha del
' archivo....
' faltan todos los efectos posible en encabezado 
' -----solucion...
' hare dos tipos de encabezado el viejo y el nuevo
' 1) para detectar el nuevo tipo pondremos en ordeb=0 pista 0 nunca podra existir
' de modo que indicara nuevo tipo de encabezado. si Orden > 0 es el tipo de encabezado
' viejo y solo una struct o UT sera leida el resto son datos de secuencia
' 2) Todos los otros campos se usaran para otra cosa respetando el tamaño de bytes
' podrian ser fecha y hora de creacion, tiempopatron, tiempoCompas, volumen inicial dela pista
' pan, eco,coro iniciales de la pista, canalsalida, canalx, vibrato ,pitchbend etc
' para todo le pondremos 70 bytes total como los rtk. para ellos definimos un nuevo tye
' no usamos type anidado porque lso nombres se harian muy largos y puntos por todos lados
' proceso: 
'  leemos ejecparam el viejo si orden> 0 termino siguen los datos
'  si orden venia en cero 0, es nuevo formato 
' leemos lo que falta del nuevo type con ejecparamNuevo


Type vivo Field=1
  As  ejec    trk  (Any)  
End Type


'Type pEjec Field=1 ' paramejec 
' As Double delta  ' entre pistas de ejec
' As Integer maxpos ' de c/Toca
' As String  nombre 'decada Toca
' As UByte  orden ' nro orden de pista para cargar,no se si necesario''
'End Type
' deberia ahcer dim no redim y usar erase
' VECTOR EJECUCION REAL 
Dim Shared Toca (1 To 32) as vivo  ' vector ejecucion real
Dim Shared  As vivo toc
' tocaparam son los parametros de la ejecucion y se graban al principio
' de un archivo ejec, luegog se graban los datods de las pistas de ejec midi
Dim  Shared  tocaparamCabeza ( 0 To 32) As ejeccabeza
Dim  Shared  tocaparam ( 0 To 32) As ejecparam 
Dim Shared   tocaparam2 (0 To 32) As ejecparam2
'''Dim  Shared  tocaparamNuevo ( 0 To 32) As ejecparamNuevo
 

Dim Shared As Integer result =0 

'Dim Shared As ejecparam tocap 

'ReDim Shared pToca (1 To 32) As pEjec ' parametros
Dim Shared As Integer tocatope=0,arrancaPlay=0
Dim Shared As Double StartPlayejec=0
'ReDim Shared As Double   espera (1 To 32)
Dim Shared As Integer kply,jToca=0
Dim Shared As Double timex(1 To 32)
Dim Shared As UByte dato1, dato2 , dato3, dato1old,dato4=0,filtro1,filtro2 
Dim Shared  As Double stime1=0, stime2=0, sdura=0
Dim Shared datot4 As double
Dim Shared  As integer maxgrb=0,maxcarga=0 
' usaremos xml par aleer y escribir musicxml 
' http://xmlsoft.org/examples/index.html
Dim Shared As String acercade, camino,nroversion

Dim Shared As UBYTE GrabarEjec=0, GrabarPenta=0  
Dim Shared As Double Tiempodelta=0, Tiemposilencio=0

Type paramGrabamidi
  As vivo toc
  ''As Integer  tocatope ''' no hace falta esta estructura solo contiene una pista
  As ejecparam tocap
  As ejecparam2 tocap2 ' nuevo
End Type


Dim Shared pgmidi As paramGrabamidi


Declare Sub GrabarMidiIn ( ByRef  par As  paramGrabamidi, pis As integer)


Declare Sub CargarSinRoll () '28-02-2024
Declare Sub SetOptionsWindowScroll (iParam As Long)
Declare Sub cargariniciotxt(lugar As String,ejecutar As ubyte)
Declare Sub grabariniciotxt(lugar As String, ejecutar As ubyte)
 

Type NProc Field=1 ''' NOTA CAMBIAR TEMPO POR 2 BYTES DE TIEMPOPATRON
 Mfile    As UByte
 Tipo     As UByte
 Trkname  As String
 valFig   As Integer
 compas   As string 
 Tempo    As UByte  'le deberiamos poner 2 bytes como tiempopatron?
 Meta     As UByte 
 Mtrk     As UByte
 Trkend   As UByte
 numTrk   As UByte 
 non      As UByte
 noff     As UByte
 nligado  As UByte
 match    As UByte
 nini     As Integer
 nfin     As Integer
 canal    As UByte
 dur      As double
 patch    As UByte
 pb       As Short
 pan      As Short
 acum     As Integer
 TimeSig  As String
 notacant as Integer
 nota     As UByte
 volum    As UByte
 j1       As Integer
 i1       As integer
End Type
Dim Shared  As  NProc midi (1 To 8000) 
Declare Sub duraciones (midi() As NProc, k1 As Integer, k2 As integer) 
Declare Sub  RefacturarPista() 

 Type lineaSecuencia
   linea As String
   indicador As Integer
 End Type
' indicadores de type linea
Const As integer inicioAcorde=     1
Const As Integer finAcorde   =     2
Const As Integer inicioSimple=     3
Const As Integer finSimple   =     4
Const As UByte   EJECUCION   =     1
Const As UByte   CANCION     =     2



Type mididat ' la duracion total de una nota se acumulara en duron en estado 2
 indiceX    As Integer
 indiceY    As integer
 estado    As UByte  ' on = 2, off=1
 nota      As UByte
 deltatime As Double
 vel       As UByte
 duron     As Double
End Type

Type miditxtsalida 
  pista As UByte
  datos (1 To 30000) As String * 50  
End Type
Common Shared As miditxtsalida MidiDatos()

ReDim  Shared NroEventoPista(1 ) As Integer

Union aUshort
 ST As UShort
   Type
    tp1 As UByte
    tp2 As UByte
   End Type
End Union

Union Encabezado
 cabeza As String * 6
   Type
    delta As Double
   End Type
End Union
declare Function Otroinputbox(message As String) As String   