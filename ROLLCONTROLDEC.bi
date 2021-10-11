Declare Sub EntrarNombreCancion(ByRef NombreCancion As string)
Declare Sub EntrarNombrePista(ByRef NombrePista As string)
Declare Sub EntrarTeclado (cadena As String)
Declare Sub cerrar(n As integer)
Declare Sub  seloctava  ( ByRef octadesde As Integer, ByRef octahasta As integer)
Declare Sub reproducir()
Declare Sub CrearDirCancion (ByVal NombreCancion As string)
Declare Sub cargarDirectorioCancion (ByRef NombreCancion As string)
Declare Function sacarNtk (cadena As String) As Integer
Declare Function sacarExtension(file As string) As String
Declare Function doscifras (NTK As Integer) As String
Declare Sub copiarATemp ( titulo As String, pista As String)
Declare Sub GrabarCancion()

'Declare Sub CreaTrack  (ByRef octadesde As Integer , ByRef octahasta As Integer, ByRef instru As Integer, param As pasa )
Declare Sub selInstORdenAlfa (ByRef instru As Integer)
Declare Sub selInstORdenNum (ByRef instru As Integer)

Dim Shared As ubyte GrabarPistaCancion=0
 
Dim Shared As Integer  usarmarco= 0 , usarmarcoOld=0,reiniciar=0
#Include "dir.bi"  
  

Dim Shared  NombreInst(1 to 127) As string * 24 => _ 
             { "ACOUSTIC_GRAND_PIANO   1" , _
               "BRIGHT_ACOUSTIC_PIANO  2" , _
               "ELECTRIC_GRAND_PIANO   3" , _
               "HONKY_TONK_PIANO       4" , _
               "ELECTRIC_PIANO_1       5" , _
               "ELECTRIC_PIANO_2       6" , _
               "HARPSICHORD            7" , _
               "CLAVI                  8" , _
               "CELESTA                9" , _
               "GLOCKENSPIEL          10" , _
               "MUSIC_BOX             11" , _
               "VIBRAPHONE            12" , _
               "MARIMBA               13" , _
               "XYLOPHONE             14" , _
               "TUBULAR_BELLS         15" , _
               "DULCIMER              16" , _
               "DRAWBAR_ORGAN         17" , _
               "PERCUSSIVE_ORGAN      18" , _
               "ROCK_ORGAN            19" , _
               "CHURCH_ORGAN          20" , _
               "REED_ORGAN            21" , _
               "ACCORDION             22" , _
               "HARMONICA             23" , _
               "TANGO_ACCORDION       24" , _
               "ACOUSTIC_GUITAR_NYLON 25" , _
               "ACOUSTIC_GUITAR_STEEL 26" , _
               "ELECTRIC_GUITAR_JAZZ  27" , _
               "ELECTRIC_GUITAR_CLEAN 28" , _
               "ELECTRIC_GUITAR_MUTED 29" , _
               "OVERDRIVEN_GUITAR     30" , _
               "DISTORTION_GUITAR     31" , _
               "GUITAR_HARMONICS      32" , _
               "ACOUSTIC_BASS         33" , _
               "ELECTRIC_BASS_FINGER  34" , _
               "ELECTRIC_BASS_PICK    35" , _
               "FRETLESS_BASS         36" , _
               "SLAP_BASS_1           37" , _
               "SLAP_BASS_2           38" , _
               "SYNTH_BASS_1          39" , _
               "SYNTH_BASS_2          40" , _
               "VIOLIN                41" , _
               "VIOLA                 42" , _
               "CELLO                 43" , _
               "CONTRABASS            44" , _
               "TREMOLO_STRINGS       45" , _
               "PIZZICATO_STRINGS     46" , _
               "ORCHESTRAL_HARP       47" , _
               "TIMPANI               48" , _
               "STRING_ENSEMBLE_1     49" , _
               "STRING_ENSEMBLE_2     50" , _
               "SYNTHSTRINGS_1        51" , _
               "SYNTHSTRINGS_2        52" , _
               "CHOIR_AAHS            53" , _
               "VOICE_OOHS            54" , _
               "SYNTH_VOICE           55" , _
               "ORCHESTRA_HIT         56" , _
               "TRUMPET               57" , _
               "TROMBONE              58" , _
               "TUBA                  59" , _
               "MUTED_TRUMPET         60" , _
               "FRENCH_HORN           61" , _
               "BRASS_SECTION         62" , _
               "SYNTHBRASS_1          63" , _
               "SYNTHBRASS_2          64" , _
               "SOPRANO_SAX           65" , _
               "ALTO_SAX              66" , _
               "TENOR_SAX             67" , _
               "BARITONE_SAX          68" , _
               "OBOE                  69" , _
               "ENGLISH_HORN          70" , _
               "BASSOON               71" , _
               "CLARINET              72" , _
               "PICCOLO               73" , _
               "FLUTE                 74" , _
               "RECORDER              75" , _
               "PAN_FLUTE             76" , _
               "BLOWN_BOTTLE          77" , _
               "SHAKUHACHI            78" , _
               "WHISTLE               79" , _
               "OCARINA               80" , _
               "LEAD_1_SQUARE         81" , _
               "LEAD_2_SAWTOOTH       82" , _
               "LEAD_3_CALLIOPE       83" , _
               "LEAD_4_CHIFF          84" , _
               "LEAD_5_CHARANG        85" , _
               "LEAD_6_VOICE          86" , _
               "LEAD_7_FIFTHS         87" , _
               "LEAD_8_BASS_AND_LEAD  88" , _
               "PAD_1_NEW_AGE         89" , _
               "PAD_2_WARM            90" , _
               "PAD_3_POLYSYNTH       91" , _
               "PAD_4_CHOIR           92" , _
               "PAD_5_BOWED           93" , _
               "PAD_6_METALLIC        94" , _
               "PAD_7_HALO            95" , _
               "PAD_8_SWEEP           96" , _
               "FX_1_RAIN             97" , _
               "FX_2_SOUNDTRACK       98" , _
               "FX_3_CRYSTAL          99" , _
               "FX_4_ATMOSPHERE      100" , _
               "FX_5_BRIGHTNESS      101" , _
               "FX_6_GOBLINS         102" , _
               "FX_7_ECHOES          103" , _
               "FX_8_SCIFI           104" , _
               "SITAR                105" , _
               "BANJO                106" , _
               "SHAMISEN             107" , _
               "KOTO                 108" , _
               "KALIMBA              109" , _
               "BAG_PIPE             110" , _
               "FIDDLE               111" , _
               "SHANAI               112" , _
               "TINKLE_BELL          113" , _
               "AGOGO                114" , _
               "STEEL_DRUMS          115" , _
               "WOODBLOCK            116" , _
               "TAIKO_DRUM           117" , _
               "MELODIC_TOM          118" , _
               "REVERSE_CYMBAL       119" , _
               "GUITAR_FRET_NOISE    120" , _
               "BREATH_NOISE         121" , _
               "SEASHORE             122" , _
               "BIRD_TWEET           123" , _
               "TELEPHONE_RING       124" , _
               "HELICOPTER           125" , _
               "APPLAUSE             126" , _
               "GUNSHOT              127" }

Dim Shared  NombreInstAlfa(1 to 127) as string * 24  => _
             { "ACCORDION             22" , _
               "ACOUSTIC_BASS         33" , _
               "ACOUSTIC_GRAND_PIANO   1" , _
               "ACOUSTIC_GUITAR_NYLON 25" , _
               "ACOUSTIC_GUITAR_STEEL 26" , _
               "AGOGO                114" , _
               "ALTO_SAX              66" , _
               "APPLAUSE             126" , _
               "BAG_PIPE             110" , _
               "BANJO                106" , _
               "BARITONE_SAX          68" , _
               "BASSOON               71" , _
               "BIRD_TWEET           123" , _
               "BLOWN_BOTTLE          77" , _
               "BRASS_SECTION         62" , _
               "BREATH_NOISE         121" , _
               "BRIGHT_ACOUSTIC_PIANO  2" , _
               "CELESTA                9" , _
               "CELLO                 43" , _
               "CHOIR_AAHS            53" , _
               "CHURCH_ORGAN          20" , _
               "CLARINET              72" , _
               "CLAVI                  8" , _
               "CONTRABASS            44" , _
               "DISTORTION_GUITAR     31" , _
               "DRAWBAR_ORGAN         17" , _
               "DULCIMER              16" , _
               "ELECTRIC_BASS_FINGER  34" , _
               "ELECTRIC_BASS_PICK    35" , _
               "ELECTRIC_GRAND_PIANO   3" , _
               "ELECTRIC_GUITAR_CLEAN 28" , _
               "ELECTRIC_GUITAR_JAZZ  27" , _
               "ELECTRIC_GUITAR_MUTED 29" , _
               "ELECTRIC_PIANO_1       5" , _
               "ELECTRIC_PIANO_2       6" , _
               "ENGLISH_HORN          70" , _
               "FIDDLE               111" , _
               "FLUTE                 74" ,  _
               "FRENCH_HORN           61" ,  _
               "FRETLESS_BASS         36" ,  _
               "FX_1_RAIN             97" ,  _
               "FX_2_SOUNDTRACK       98" ,  _
               "FX_3_CRYSTAL          99" ,  _
               "FX_4_ATMOSPHERE      100" ,  _
               "FX_5_BRIGHTNESS      101" ,  _
               "FX_6_GOBLINS         102" , _
               "FX_7_ECHOES          103" , _
               "FX_8_SCIFI           104" , _
               "GLOCKENSPIEL          10" , _
               "GUITAR_FRET_NOISE    120" , _
               "GUITAR_HARMONICS      32" , _
               "GUNSHOT              127" , _ 
               "HARMONICA             23" , _
               "HARPSICHORD            7" , _
               "HELICOPTER           125" , _
               "HONKY_TONK_PIANO       4" , _
               "KALIMBA              109" ,  _
               "KOTO                 108" ,  _
               "LEAD_1_SQUARE         81" ,  _
               "LEAD_2_SAWTOOTH       82" , _
               "LEAD_3_CALLIOPE       83" , _
               "LEAD_4_CHIFF          84" , _
               "LEAD_5_CHARANG        85" , _
               "LEAD_6_VOICE          86" , _
               "LEAD_7_FIFTHS         87" , _
               "LEAD_8_BASS_AND_LEAD  88" , _
               "MARIMBA               13" , _
               "MELODIC_TOM          118" ,  _
               "MUSIC_BOX             11" , _
               "MUTED_TRUMPET         60" , _
               "OBOE                  69" , _
               "OCARINA               80" , _
               "ORCHESTRAL_HARP       47" , _
               "ORCHESTRA_HIT         56" , _
               "OVERDRIVEN_GUITAR     30" , _
               "PAD_1_NEW_AGE         89" , _
               "PAD_2_WARM            90" , _
               "PAD_3_POLYSYNTH       91" , _
               "PAD_4_CHOIR           92" , _
               "PAD_5_BOWED           93" , _
               "PAD_6_METALLIC        94" , _
               "PAD_7_HALO            95" , _
               "PAD_8_SWEEP           96" , _
               "PAN_FLUTE             76" , _
               "PERCUSSIVE_ORGAN      18" , _
               "PICCOLO               73" , _
               "PIZZICATO_STRINGS     46" , _
               "RECORDER              75" , _
               "REED_ORGAN            21" , _
               "REVERSE_CYMBAL       119" ,  _
               "ROCK_ORGAN            19" , _
               "SEASHORE             122" ,  _
               "SHAKUHACHI            78" , _
               "SHAMISEN             107" ,  _
               "SHANAI               112" ,  _
               "SITAR                105" ,  _
               "SLAP_BASS_1           37" , _
               "SLAP_BASS_2           38" , _
               "SOPRANO_SAX           65" , _
               "STEEL_DRUMS          115" ,  _
               "STRING_ENSEMBLE_1     49" , _
               "STRING_ENSEMBLE_2     50" , _
               "SYNTHBRASS_1          63" , _
               "SYNTHBRASS_2          64" , _
               "SYNTHSTRINGS_1        51" , _
               "SYNTHSTRINGS_2        52" , _
               "SYNTH_BASS_1          39" , _
               "SYNTH_BASS_2          40" , _
               "SYNTH_VOICE           55" , _
               "TAIKO_DRUM           117" ,  _
               "TANGO_ACCORDION       24" , _
               "TELEPHONE_RING       124" ,  _
               "TENOR_SAX             67" , _
               "TIMPANI               48" , _
               "TINKLE_BELL          113" ,  _
               "TREMOLO_STRINGS       45" , _
               "TROMBONE              58" , _
               "TRUMPET               57" , _
               "TUBA                  59" , _
               "TUBULAR_BELLS         15" , _
               "VIBRAPHONE            12" , _
               "VIOLA                 42" , _
               "VIOLIN                41" , _
               "VOICE_OOHS            54" , _
               "WHISTLE               79" , _
               "WOODBLOCK            116" ,  _
               "XYLOPHONE             14" }

Dim Shared  IndiceInstAlfa(1 to 127) as integer   => _
             { 22, _
               33, _
                1, _
               25, _
               26, _
               114, _
               66,  _
               126, _
               110, _
               106, _
                68, _
                71, _
                123, _
                77, _
                62, _
                121, _
                2 , _
                9 , _
                43 , _
                53 , _
                20 , _
                72 , _
                8 , _
                4 , _
                31, _
                17, _
                16, _
                34, _
                35, _
                3, _
                28, _
                27, _
                29, _
                5, _
                6, _
                70, _
                111,_
                74, _
                61, _
                36, _
                97, _
                98, _
                99, _
                100, _
                101 , _
                102, _
                103, _
                104, _
                10, _
                120, _
                32, _
                127, _ 
                23, _
                7, _
                125, _
                4, _
                109, _
                108, _
                81 , _
                82, _
                83, _
                84, _
                85, _
                86, _
                87, _
                88, _
                13, _
                118, _
                11, _
                60, _
                69, _
                80, _
                47, _
                56, _
                30, _
                89, _
                90, _
                91, _
                92, _
                93, _
                94, _
                95, _
                96, _
                76, _
                18, _
                73, _
                46, _
                75, _
                21, _
                119, _
                19, _
                122, _
                78, _
                107, _
                112, _
                105, _
                37, _
                38, _
                65, _
                115, _
                49, _
                50, _
                63, _
                64, _
                51, _
                52, _
                39, _
                40, _
                55, _
                117, _
                24, _
                124, _
                67, _
                48, _
                113, _
                45, _
                58, _
                57, _
                59, _
                15, _
                12, _
                42, _
                41, _
                54, _
                79, _
                116, _
                14  }
               