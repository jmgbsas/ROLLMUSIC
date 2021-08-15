' ROLLMIDI 30-07-2021
' ENTRADA POR TECLADO DE EVENTOS MIDI, LUEGO SE VOLCARAN A UN TRACK MIDI
' ESE TRACK SI NO LO ANALIZAMOS SERIA UN TRACK NORMAL DE MIDI
' EL CUAL DEBERIAMOS RPESENTARLO COMO RAYAS DE COMINZO Y FINAL DE CADA EVENTO
' PERO EN VEZ DE ESO LAS RAYZA SE SUPLANTARAN POR LOS VALORRES EN LETRA
' O LETRAS LIGADAS PARA REPRESENTAR EL VALOR DE LA DURACION RELATIVA.
' EL TIEMPO DETERMINA EL VALOR RELATIVO DE CADA NEGRA EN UN 4/4...
' SE DESARROLLARA PARA 4X4 LUEGO SE APLICARA PARA OTROS TIEMPOS
' EN OPCIONES SE AJUSTARÁ LA ENTRADA MIDI A USAR.
'--------------midilib estatica para midi files---libmidilib.a-----compilar con nombre midilib
' si estos include los pongo en otro lado que no sea este fichero
' el compilador me tira un monton de warnings de mix bollean error rarisimo
#Include "./midilib/mod_midiinfo.bi"
#Include "./midilib/mod_midifile.bi"
#Include "./midilib/mod_midiutil.bi"
' -------------------



