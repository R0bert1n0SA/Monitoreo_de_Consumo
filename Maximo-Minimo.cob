       IDENTIFICATION DIVISION.
       PROGRAM-ID. Maximo-Minimo AS "Maximo-Minimo".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT Reporte ASSIGN TO 'Reporte.Txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusRep.


       DATA DIVISION.
       FILE SECTION.
       FD Reporte.
       01 Reporte-R.
           05 R-ID                  PIC 9(5).
           05 R-FILLER1             PIC X .
           05 R-NombreUsuario       PIC X(40).
           05 R-FILLER2             PIC X .
           05 R-Consumo             PIC 9(5).
           05 R-FILLER3             PIC X .


       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-File-StatusRep     PIC XX.
               05 respuesta             PIC X(1).
           01 WS-Flags.
               05 FlagReport            PIC X(1) VALUE 'N'.


       LINKAGE SECTION.
           01 LK-Flag           PIC 9(2).
           01 LK-Parametros.
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
               05 Mensaje       PIC X(45).
               05 Mensaje11     PIC X(100).
               05 Cont-Bajo     PIC 9(15).
               05 Cont-Medio    PIC 9(15).
               05 Cont-Alto     PIC 9(15).

       PROCEDURE DIVISION USING LK-Flag,LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Verificar
       EXIT PROGRAM.

           Convesion-Strings.
               IF ConsumoT = 0 THEN
                   MOVE "No existe Cliente" TO Mensaje
               ELSE
                   STRING "Cliente: " IDclie " Consumo: " ConsumoT
                       DELIMITED BY SIZE
                       INTO Mensaje
                   END-STRING
                END-IF

                STRING "Bajo consumo: " Cont-Bajo  X"0A"
                       "Medio consumo: "Cont-Medio X"0A"
                       "Alto consumo: " Cont-Alto  X"0A"
                   DELIMITED BY SIZE
                   INTO Mensaje11
                END-STRING
           EXIT.


           Verificar.
                   OPEN INPUT Reporte
                   PERFORM Reportar
                   CLOSE Reporte
                   PERFORM Convesion-Strings
           EXIT.

      *>================================================================*
        *> Seccion  Reporte
        *> Contiene todas las operaciones sobre el reporte en base a la flag recibida
      *>================================================================*
           Reportar.
               PERFORM UNTIL FlagReport = "Y"
                   READ Reporte INTO Reporte-R
                       AT END
                           MOVE "Y" TO FlagReport
                       NOT AT END
                           PERFORM Evaluar
                   END-READ
               END-PERFORM
               MOVE 'N' TO FlagReport
           EXIT.

           Evaluar.
               EVALUATE LK-Flag
                   WHEN 7
                       PERFORM Mayor-Menor
                   WHEN 8
                       PERFORM Buscar
                   WHEN 9
                       PERFORM Rango
                   WHEN 10
                       DISPLAY R-ID" | "R-NombreUsuario " | "R-Consumo
                   WHEN 11
                       PERFORM Tipos
               END-EVALUATE
           EXIT.


           Mayor-Menor.
               IF R-Consumo > Mayor THEN
                   MOVE R-NombreUsuario TO NombreMax
                   MOVE R-Consumo TO Mayor
               END-IF

               IF R-Consumo < Minimo THEN
                   MOVE R-NombreUsuario TO NombreMin
                   MOVE R-Consumo TO Minimo
               END-IF
           EXIT.


           Buscar.
               IF  R-ID = IDclie THEN
                   MOVE R-ID TO IDclie
                   MOVE R-Consumo To ConsumoT
               END-IF
           EXIT.


           Rango.
               IF R-Consumo >= rango-Ini AND R-Consumo <= rango-Fin
                   DISPLAY "Nombre: "R-NombreUsuario
               END-IF
           EXIT.


           Tipos.
               IF R-Consumo <= Bajo THEN
                   ADD 1 TO Cont-Bajo GIVING Cont-Bajo
               END-IF

               IF R-Consumo >= Bajo AND R-Consumo <=Alto THEN
                   ADD 1 TO Cont-Medio GIVING Cont-Medio
               END-IF

               IF R-Consumo <= Alto THEN
                   ADD 1 TO Cont-Alto GIVING Cont-Alto
               END-IF
           EXIT.
      *>================================================================*
