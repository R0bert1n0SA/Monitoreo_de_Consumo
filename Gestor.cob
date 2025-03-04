       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gestor.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-Flag          PIC 9(1) VALUE 0.
           01 WS-opcion        PIC 9(1).
           01 WS-EnterT        PIC X(1).
           01 WS-Meses.
               05 Indice        PIC 9(2).
               05 Max           PIC 9(10) VALUE 0.
               05 Min           PIC 9(10) VALUE 9999999999.
               05 Mensual       PIC 9(10) VALUE 0.
               05 Mes           PIC X(20).
               05 MesMax        PIC X(20).
               05 MesMin        PIC X(20).
               05 MesNombre     PIC X(10)  OCCURS 12 TIMES.
               05 UltimoMes     PIC 9(2).
           *> Estructura que almacena los parámetros del modulo "Consumo".
           01 WS-Consumo-Parametros.
               05 Comparacion   PIC 9(1).
               05 Costo         PIC 9(4)V99 VALUE 200.15.
               05 Consumo       PIC 9(20).
               05 Aumento       PIC S9(3)V99.
               05 gasto         PIC 9(12)V99.
               05 Mensaje       PIC X(45).
           01 WS-Reporte-Parametros.
               05 Tabla         PIC X(15) OCCURS 12 TIMES.
               05 Total         PIC 9(10).
               05 ConsumoR      PIC 9(10).
               05 Promedio      PIC 9(10)V99.
           01 WS-Maximo-Minimo-Parametros.
               05 IDclie        PIC 9(5).
               05 IDMax         PIC 9(5).
               05 IDMin         PIC 9(5).
               05 Mayor         PIC 9(10).
               05 Minimo        PIC 9(10).
               05 rango-Ini     PIC 9(5).
               05 rango-Fin     PIC 9(5).
               05 Bajo          PIC 9(5).
               05 Alto          PIC 9(5).
               05 ConsumoT      PIC 9(10).
               05 MensajeM       PIC X(45).


       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM Menu
       STOP RUN.

           IniciarMes.
               MOVE "Enero"      TO MesNombre(1)
               MOVE "Febrero"    TO MesNombre(2)
               MOVE "Marzo"      TO MesNombre(3)
               MOVE "Abril"      TO MesNombre(4)
               MOVE "Mayo"       TO MesNombre(5)
               MOVE "Junio"      TO MesNombre(6)
               MOVE "Julio"      TO MesNombre(7)
               MOVE "Agosto"     TO MesNombre(8)
               MOVE "Septiembre" TO MesNombre(9)
               MOVE "Octubre"    TO MesNombre(10)
               MOVE "Noviembre"  TO MesNombre(11)
               MOVE "Diciembre"  TO MesNombre(12)
           EXIT.



       *> Procedimiento para mostrar el menú principal y gestionar la interacción del usuario.
           Menu.
               PERFORM IniciarMes
               CALL "Consumo" USING WS-Consumo-Parametros
               CALL "Reporte" USING WS-Reporte-Parametros
               PERFORM Meses-Recorrer
               *> Bucle que muestra el menú hasta que el usuario elija salir (WS-Flag = 1).
               PERFORM UNTIL WS-Flag = 1
                   DISPLAY "--------------Consumos---------------"
                   DISPLAY "1.Mostrar Total Global de Consumo."
                   DISPLAY "2.Comparar Con El Año Anterior"
                   DISPLAY "3.Mostrar Costo total de Consumo"
                   DISPLAY "4.Obtener Consumo Por mes "
                   DISPLAY "5.Mostrar Mes con mayor y menor consumo "
                   DISPLAY "6.Obtener Consumo Promedio entre clientes"
                   DISPLAY "7.Mostrar Cliente con mayor y menor consumo"
                   DISPLAY "8.Obtener Consumo total de cliente"
                   DISPLAY "9.Identificar Clientes dentro de un rango"
                   DISPLAY "10.Mostrar reporte "
                   DISPLAY "11.Mostrar Cantidad de clientes "
                   "consumo bajo medio o alto"
                   DISPLAY "12.Mostrar Distribucion Porcentual de "
                   "consumo Por mes"
                   DISPLAY "0.Salir"
                   ACCEPT Ws-Opcion
                   DISPLAY X"1B" & "[2J" *> Código ANSI para limpiar la pantalla
                   PERFORM Evaluar
               END-PERFORM
           EXIT.


       *> Procedimiento para evaluar la opción seleccionada y ejecutar la acción correspondiente.
           Evaluar.
               EVALUATE WS-opcion
                   WHEN 1
                       DISPLAY "Consumo GLOBAL: " Consumo
                   WHEN 2
                       PERFORM Opcion_2
                   WHEN 3
                       DISPLAY "El Costo total del Consumo: "Gasto
                   WHEN 4
                       PERFORM Meses-mostrar
                   WHEN 5
                       DISPLAY "Mes con mayor consumo fue" MesMax
                       " con :" Max " KWH"
                       DISPLAY "Mes con Menor consumo fue" MesMin
                       " con :" Min " KWH"
                   WHEN 6
                       DISPLAY "Consumo Promedio entre clientes: "
                       Promedio
                   WHEN 7
                       DISPLAY "Cliente con mayor Consumo: "IDMax
                       DISPLAY "Cliente con menor Consumo: "IDMin
                   WHEN 8
                       DISPLAY "ID del cliente a buscar: "
                       ACCEPT IDclie
                       CALL "Maximo-Minimo"
                       USING WS-opcion,WS-Maximo-Minimo-Parametros
                       DISPLAY MensajeM
                   WHEN 9
                       DISPLAY "Ingrese valor inicio del rango: "
                       ACCEPT rango-Ini
                       DISPLAY "Ingrese valor fin del rango: "
                       ACCEPT rango-Fin
                       CALL "Maximo-Minimo"
                       USING WS-opcion,WS-Maximo-Minimo-Parametros
                   WHEN 10
                       CALL "Maximo-Minimo"
                       USING WS-opcion, WS-Maximo-Minimo-Parametros
                   WHEN 11
                       DISPLAY "Consumo Bajo: "
                       ACCEPT Bajo
                       DISPLAY "Consumo Alto: "
                       ACCEPT Alto
                       CALL "Maximo-Minimo"
                       CALL "Maximo-Minimo"
                       USING WS-opcion, WS-Maximo-Minimo-Parametros
                   WHEN 12
                       DISPLAY "Promedio Mensual por mes: "Mensual
                   WHEN 0
                       DISPLAY "Gracias Por Usar la App "
                       STOP RUN
                   WHEN OTHER
                        DISPLAY "ERROR: opcion no valida"
               END-EVALUATE
           *> Mensaje para que el usuario presione Enter antes de continuar.
               DISPLAY "Presione Enter para continuar"
               ACCEPT WS-EnterT
               DISPLAY X"1B" & "[2J"
           EXIT.

       *> Mensaje para que el usuario presione Enter antes de continuar.
           Opcion_2.
               IF Comparacion = 0 THEN
                   DISPLAY "El consumo con respecto al año anterior "
                   "aumento un: "Aumento " %"
               ELSE
                   DISPLAY "No Hay informacion"
               END-IF
           EXIT.




           Meses-Recorrer.
               PERFORM VARYING Indice FROM 1 BY 1 UNTIL Indice > 12
                    MOVE MesNombre(Indice) TO Mes
                    IF  Tabla(Indice) > Max THEN
                        MOVE Tabla(Indice) TO Max
                        MOVE Mes TO MesMax
                    END-IF
                    IF  Tabla(Indice) < Min THEN
                        MOVE Tabla(Indice) TO Min
                        MOVE Mes TO MesMin
                    END-IF
                    IF Tabla(Indice) NOT = 0 THEN
                        MOVE Indice TO UltimoMes
                    END-IF
               END-PERFORM
               COMPUTE  Mensual=(Consumo / UltimoMes )
           EXIT.


           Meses-mostrar.
               PERFORM VARYING Indice FROM 1 BY 1 UNTIL Indice > 12
                    MOVE MesNombre(Indice) TO Mes
                    DISPLAY Mes ":" Tabla(Indice)
               END-PERFORM
           EXIT.
