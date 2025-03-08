       IDENTIFICATION DIVISION.
       PROGRAM-ID. Consumo AS "Consumo".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AnioActual ASSIGN TO 'Consumo2025.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-Status.


           SELECT AnioAnterior ASSIGN TO 'Consumo2024.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-Statusant.

       DATA DIVISION.
       FILE SECTION.
       FD AnioActual.
       01 ConsumoActualR.
           05 Act-ID                 PIC 9(5).
           05 Act-NombreUsuario      PIC X(40).
           05 Act-Mes                PIC X(15).
           05 Act-Consumo            PIC 9(5).

       FD AnioAnterior.
       01 ConsumoAnteriorR.
           05 Ant-ID                 PIC 9(5).
           05 Ant-NombreUsuario      PIC X(40).
           05 Ant-Mes                PIC X(15).
           05 Ant-Consumo            PIC 9(5).

       WORKING-STORAGE SECTION.
           01 WS-File-Status       PIC XX.
           01 WS-File-Statusant    PIC XX.
           01 WS-Flag              PIC X(1) VALUE 'N'.
           01 sum2024              PIC 9(20).
           01 sum2023              PIC 9(20).

       LINKAGE SECTION.
       01 LK-Parametros.
           05 P-Flag               PIC 9(1).
           05 P-Costo              PIC 9(4)V99.
           05 P-Consumo            PIC 9(20).
           05 P-Aumento            PIC S9(3)V99.
           05 P-Gasto              PIC 9(12)V99.
           05 P-Mensaje            PIC X(45).

       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Iniciar
       EXIT PROGRAM.

      *>================================================================*
       *> Sección de Inicio
       *> Verifica archivos y calcula consumo
      *>================================================================*
       S-Inicio SECTION.
           *> Procedimiento que verifica la existencia de archivos y realiza cálculos.
           Iniciar.
               PERFORM Verificar
               PERFORM Anio-Actual
               PERFORM Anio-Anterior
               CLOSE AnioActual,AnioAnterior
               PERFORM Calculos
           EXIT.

      *>================================================================*

      *>================================================================*
       *> Sección Verificar
       *> Verifica la existencia de archivos
      *>================================================================*
       S-Verificar SECTION.
           Verificar.
               OPEN INPUT AnioAnterior
               OPEN INPUT AnioActual
               MOVE 0 TO P-Flag
               IF WS-File-Status NOT = '00' AND
                  WS-File-Statusant NOT = '00' THEN
                   MOVE 1 TO P-Flag
                   MOVE 0 TO P-Aumento
                   MOVE "No hay archivos para trabajar" TO P-Mensaje
                   GO TO salir
               ELSE
                   PERFORM VerificarActual
                   PERFORM VerificarAnterior
               END-IF
           EXIT.


           VerificarActual.
               IF WS-File-Status NOT = '00'
                   MOVE 1 TO P-Flag
                   MOVE 0 TO P-Aumento
                   MOVE "No hay archivo de este año" TO P-Mensaje
                   GO TO salir
               END-IF
           EXIT.


           VerificarAnterior.
               IF WS-File-Statusant NOT = '00'
                   PERFORM Anio-Actual
                   MOVE sum2024 TO P-Consumo
                   COMPUTE P-Gasto = (P-Consumo * P-Costo)
                   MOVE 1 TO P-Flag
                   MOVE 0 TO P-Aumento
                   CLOSE AnioAnterior
                   MOVE "No hay Informacion del año anterior"
                   TO P-Mensaje
               END-IF
           EXIT.

      *>================================================================*


      *>================================================================*
       *> Sección Recorrer
       *> Lee y suma los consumos de cada año
      *>================================================================*
       Recorrer SECTION.
           Anio-Actual.
               PERFORM UNTIL WS-Flag = 'Y'
                   READ AnioActual INTO ConsumoActualR
                       AT END
                           MOVE 'Y' TO WS-Flag
                       NOT AT END
                           ADD Act-Consumo TO sum2024
                   END-READ
               END-PERFORM
               MOVE 'N' TO WS-Flag
           EXIT.

           *> Procedimiento que suma el consumo del año anterior.
           Anio-Anterior.
               PERFORM UNTIL WS-Flag = 'Y'
                   READ AnioAnterior INTO ConsumoAnteriorR
                       AT END
                           MOVE 'Y' TO WS-Flag
                       NOT AT END
                           ADD Ant-Consumo TO sum2023
                   END-READ
               END-PERFORM
               MOVE 'N' TO WS-Flag
           EXIT.
      *>================================================================*


      *>================================================================*
       *> Sección de Calcular
       *> Calcula el aumento de consumo y costo total
      *>================================================================*
       S-Calcular SECTION.
           Calculos.
               COMPUTE P-Aumento = ((sum2024 - sum2023) / sum2023) * 100
               MOVE sum2024 TO P-Consumo
               COMPUTE P-Gasto = (P-Consumo * P-Costo)
           EXIT.
      *>================================================================*
      *>================================================================*
       *> Sección de Salida
      *>================================================================*
       salir.
       EXIT.
