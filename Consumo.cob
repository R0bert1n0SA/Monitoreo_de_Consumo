       IDENTIFICATION DIVISION.
       PROGRAM-ID. Consumo AS "Consumo".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Definición de archivos secuenciales con su estado.
           SELECT Consumo2024 ASSIGN TO 'Consumo2024.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-Status.

           SELECT Consumo2023 ASSIGN TO 'Consumo2023.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-Statusant.

       DATA DIVISION.
       FILE SECTION.
       FD Consumo2024.
       01 Consumo2024R.
           05 C2024-ID                 PIC 9(5).
           05 C2024-NombreUsuario      PIC X(40).
           05 C2024-Mes                PIC X(15).
           05 C2024-Consumo            PIC 9(5).

       FD Consumo2023.
       01 Consumo2023R.
           05 C2023-ID                 PIC 9(5).
           05 C2023-NombreUsuario      PIC X(40).
           05 C2023-Mes                PIC X(15).
           05 C2023-Consumo            PIC 9(5).

       WORKING-STORAGE SECTION.
           01 WS-File-Status       PIC XX.
           01 WS-File-Statusant    PIC XX.
           01 WS-Flag              PIC X(1) VALUE 'N'.
           01 sum2024              PIC 9(20).
           01 sum2023              PIC 9(20).

       LINKAGE SECTION.
       01 LK-Parametros.
           05 P-Flag               PIC 9(1).
           05 P-Costo              PIC 9(4)V99 VALUE 200.15.
           05 P-Consumo            PIC 9(20).
           05 P-Aumento            PIC 9(3)V99.
           05 P-Gasto              PIC 9(12)V99.
           05 P-Mensaje            PIC X(25).

       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Verificar
       EXIT PROGRAM.

           *> Procedimiento que verifica la existencia de archivos y realiza cálculos.
           Verificar.
               OPEN INPUT Consumo2024
               MOVE 0 TO P-Flag
               IF WS-File-Status NOT = '00'
                   MOVE 1 TO P-Flag
                   MOVE 0 TO P-Aumento
                   MOVE "no hay archivo Actual" TO P-Mensaje
                   GO TO salir
               END-IF

               OPEN INPUT Consumo2023
               IF WS-File-Statusant NOT = '00'
                   PERFORM Anio-Actual
                   MOVE sum2024 TO P-Consumo
                   COMPUTE P-Gasto = (P-Consumo * P-Costo)
                   MOVE 1 TO P-Flag
                   MOVE 0 TO P-Aumento
                   CLOSE Consumo2024
                   GO TO salir
               END-IF

               DISPLAY "sali"
               PERFORM Anio-Actual
               PERFORM Anio-Anterior
               CLOSE Consumo2024, Consumo2023

               *> Cálculo del porcentaje de aumento de consumo.
               COMPUTE P-Aumento = ((sum2024 - sum2023) / sum2024) * 100
               MOVE sum2024 TO P-Consumo
               COMPUTE P-Gasto = (P-Consumo * P-Costo)
           EXIT.

           *> Procedimiento que suma el consumo del año actual.
           Anio-Actual.
               PERFORM UNTIL WS-Flag = 'Y'
                   READ Consumo2024 INTO Consumo2024R
                       AT END
                           MOVE 'Y' TO WS-Flag
                       NOT AT END
                           ADD C2024-Consumo TO sum2024
                   END-READ
               END-PERFORM
               MOVE 'N' TO WS-Flag
           EXIT.

           *> Procedimiento que suma el consumo del año anterior.
           Anio-Anterior.
               PERFORM UNTIL WS-Flag = 'Y'
                   READ Consumo2023 INTO Consumo2023R
                       AT END
                           MOVE 'Y' TO WS-Flag
                       NOT AT END
                           ADD C2023-Consumo TO sum2023
                   END-READ
               END-PERFORM
               MOVE 'N' TO WS-Flag
           EXIT.

           *> Punto de salida del programa.
           salir.
           EXIT.
