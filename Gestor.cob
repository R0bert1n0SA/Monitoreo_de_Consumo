       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gestor.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-File-Status   PIC XX.
           01 WS-Flag          PIC 9(1) VALUE 0.
           01 WS-opcion        PIC 9(1).
           01 WS-EnterT              PIC X(1).
           *> Estructura que almacena los parámetros del modulo "Consumo".
           01 WS-Consumo-Parametros.
               05 Comparacion   PIC 9(1).
               05 Costo         PIC 9(4)V99 VALUE 200.15.
               05 Consumo       PIC 9(20).
               05 Aumento       PIC 9(3)V99.
               05 gasto         PIC 9(12)V99.
               05 Mensaje       PIC X(25).
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM Menu
       STOP RUN.


       *> Procedimiento para mostrar el menú principal y gestionar la interacción del usuario.
           Menu.
               CALL "Consumo" USING WS-Consumo-Parametros
               *> Bucle que muestra el menú hasta que el usuario elija salir (WS-Flag = 1).
               PERFORM UNTIL WS-Flag = 1
                   DISPLAY "--------------Consumos---------------"
                   DISPLAY "1.Mostrar Total Global de Consumo."
                   DISPLAY "2.Comparar Con El Año Anterior"
                   DISPLAY "3.Costo total de Consumo"
                   DISPLAY "4.Informe completo"
                   DISPLAY "5.Salir"
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
                       DISPLAY "Consumo GLOBAL: " Consumo
                       PERFORM Opcion_2
                       DISPLAY "El Costo total del Consumo: "Gasto
                   WHEN 5
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
