REM EN UNA LINEA DE COMANDOS DESCOMENTAR ESTO SOLAMENTE
REM Y COLOCAR 2 NOMBRES DE ARCHIVO EL TXT A CONVERTIR 
REM Y LUEGO EL NOMBRE DEL ARCHIVO ONVERTIDO CON EXTENSION .MID 

REM midicomp.exe -c %1 %2

REM o usar esta otra linea fonfr secuenciaPLAY.txt es le nombre del archivo
rem generado por rollmusic, agregar el nombre del convertido
rem aca o usar una variavle para ejecutarlo %1 en lineas de comandos CMD.exe
rem o cambiar %1 por el nombre de la salida con extendion mid
rem 
rem midicomp.exe -c secuenciaPLAY.txt %1

 midicomp.exe -c secuenciaPLAY.txt archivo.mid
del /f secuenciaPLAY.txt
rem touch vacio.txt
rem copy /Y vacio.txt secuenciaPLAY.txt
