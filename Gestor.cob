       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gestor.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-Flag          PIC 9(1) VALUE 0.
           01 WS-opcion        PIC 9(2).
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
               05 Tabla         PIC 9(15) OCCURS 12 TIMES.
               05 Total         PIC 9(10).
               05 ConsumoR      PIC 9(10).
               05 Promedio      PIC 9(10)V99.
           01 WS-Maximo-Minimo-Parametros.
               05 IDclie        PIC 9(5).
               05 NombreMax     PIC X(30).
               05 NombreMin     PIC X(30).
               05 Mayor         PIC 9(10).
               05 Minimo        PIC 9(10).
               05 rango-Ini     PIC 9(5).
               05 rango-Fin     PIC 9(5).
               05 Bajo          PIC 9(5).
               05 Alto          PIC 9(5).
               05 ConsumoT      PIC 9(10).
               05 MensajeM      PIC X(45).
               05 Mensaje11     PIC X(100).
               05 Cont-Bajo     PIC 9(15).
               05 Cont-Medio    PIC 9(15).
               05 Cont-Alto     PIC 9(15).
           01 Aux               PIC 9(3)V99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM Inicio
       STOP RUN.

      *>================================================================*
        *> Seccion Inicio
        *> Inicia la ejecucion del programa y ejecuta procesos batch
      *>================================================================*
       S-Inicio SECTION.
           Inicio.
               MOVE 0 TO Mayor
               MOVE 9999999999 TO Minimo
               PERFORM IniciarMes
               CALL "Consumo" USING WS-Consumo-Parametros
               CALL "Reporte" USING WS-Reporte-Parametros
               PERFORM Meses-Recorrer
               PERFORM Menu
           EXIT.


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


      *>================================================================*

      *>================================================================*
        *> Seccion Menu
        *> Contine toda la logica de iteraccion
      *>================================================================*

       S-Menu SECTION.
       *> Procedimiento para mostrar el menú principal y gestionar la interacción del usuario.
           Menu.
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
                   DISPLAY "13.Salir"
                   ACCEPT WS-Opcion
                   DISPLAY X"1B" & "[2J"
                   PERFORM Evaluar
               END-PERFORM
           EXIT.


       *> Procedimiento para evaluar la opción seleccionada y ejecutar la acción correspondiente.
           Evaluar.
               EVALUATE WS-opcion
                   WHEN 1
                       DISPLAY "Consumo GLOBAL: " Consumo
                   WHEN 2
                       PERFORM CompararAnio
                   WHEN 3
                       DISPLAY "El Costo total del Consumo: "Gasto
                   WHEN 4
                       PERFORM Consumo-Mensual
                   WHEN 5
                       PERFORM Mostrar-Mayor-Menor-Meses
                   WHEN 6
                       DISPLAY "Consumo Promedio entre clientes: "
                       Promedio
                   WHEN 7
                       PERFORM Mostrar-Mayor-Menor-Consumo
                   WHEN 8
                       PERFORM Consumo-Total-Cliente
                   WHEN 9
                       PERFORM Clientes-Rango
                   WHEN 10
                       PERFORM Mostrar-Reporte
                   WHEN 11
                       PERFORM Tipos-Consumo
                   WHEN 12
                       DISPLAY "Promedio Mensual por mes: "Mensual
                   WHEN 13
                       DISPLAY "Gracias Por Usar la App "
                       STOP RUN
                   WHEN OTHER
                        DISPLAY "ERROR: opcion no valida"
               END-EVALUATE
               PERFORM Continuar
           EXIT.

           Continuar.
               DISPLAY "Presione Enter para continuar"
               ACCEPT WS-EnterT
               DISPLAY X"1B" & "[2J"
           EXIT.

      *>================================================================*

      *>================================================================*
         *> SECCION Opciones
         *> Contiene Todos Los modulos de las opciones de logica amplia
      *>================================================================*
       S-Opciones SECTION.

           Llamar-Maximo-Minimo.
               CALL "Maximo-Minimo" USING WS-opcion
              ,WS-Maximo-Minimo-Parametros
           EXIT.


           CompararAnio.
               IF Comparacion = 0 THEN
                   MOVE Aumento TO Aux
                   IF Aumento > 0 THEN
                       DISPLAY "El consumo con respecto al año anterior"
                       " aumento un: "Aux " %"
                   ELSE
                       DISPLAY "El consumo con respecto al año anterior"
                       " disminuyo un: "Aux " %"
                   END-IF
               ELSE
                   DISPLAY "No Hay informacion"
               END-IF
           EXIT.



           Mostrar-Mayor-Menor-Meses.
               DISPLAY "Mes con mayor consumo fue" MesMax
               " con :" Max " KWH"
               DISPLAY "Mes con Menor consumo fue" MesMin
               " con :" Min " KWH"
           EXIT.



           Mostrar-Mayor-Menor-Consumo.
               PERFORM Llamar-Maximo-Minimo
               DISPLAY "Cliente con mayor Consumo: "NombreMax
               DISPLAY "Cliente con menor Consumo: "NombreMin
           EXIT.



           Consumo-Total-Cliente.
               DISPLAY "ID del cliente a buscar: "
               ACCEPT IDclie
               PERFORM Llamar-Maximo-Minimo
               DISPLAY MensajeM
           EXIT.


           Clientes-Rango.
               DISPLAY "Ingrese valor inicio del rango: "
               ACCEPT rango-Ini
               DISPLAY "Ingrese valor fin del rango: "
               ACCEPT rango-Fin
               DISPLAY "--------Clientes en Rango---------"
               PERFORM Llamar-Maximo-Minimo
           EXIT.


           Mostrar-Reporte.
               DISPLAY "--------Reporte Clientes---------"
               PERFORM Llamar-Maximo-Minimo
           EXIT.



           Tipos-Consumo.
               DISPLAY "Consumo Bajo: "
               ACCEPT Bajo
               DISPLAY "Consumo Alto: "
               ACCEPT Alto
               PERFORM Llamar-Maximo-Minimo
               DISPLAY Mensaje11
               MOVE 0 TO Cont-Bajo,Cont-Medio,Cont-Alto
           EXIT.


           Consumo-Mensual.
               PERFORM VARYING Indice FROM 1 BY 1 UNTIL Indice > 12
                    MOVE MesNombre(Indice) TO Mes
                    DISPLAY Mes ":" Tabla(Indice)
               END-PERFORM
           EXIT.
      *>================================================================*





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
