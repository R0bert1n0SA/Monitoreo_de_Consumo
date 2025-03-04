       IDENTIFICATION DIVISION.
       PROGRAM-ID. Reporte AS "Reporte".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Definición de archivos secuenciales con su estado.

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
           01 Contadores.
               05 Cont-Bajo             PIC 9(15) VALUE 0.
               05 Cont-Medio            PIC 9(15) VALUE 0.
               05 Cont-Alto             PIC 9(15) VALUE 0.

       LINKAGE SECTION.
           01 LK-Flag          PIC 9(2).
           01 LK-Parametros.
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
               05 Mensaje       PIC X(45).

       PROCEDURE DIVISION USING LK-Flag,LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Verificar
       EXIT PROGRAM.

           Verificar.
                   OPEN INPUT Reporte
                   PERFORM Reportar
                   CLOSE Reporte
                   IF ConsumoT = 0 THEN
                       MOVE "No existe Cliente" TO Mensaje
                   ELSE
                       STRING "Cliente: " IDclie " Consumo: " ConsumoT
                           DELIMITED BY SIZE
                           INTO Mensaje
                       END-STRING
                   END-IF
           EXIT.

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
               DISPLAY "Bajo consumo: " Cont-Bajo
               DISPLAY "Medio consumo: "Cont-Medio
               DISPLAY "Alto consumo: " Cont-Alto
               MOVE 0 TO Cont-Bajo,Cont-Medio,Cont-Alto
           EXIT.

           Evaluar.
               EVALUATE LK-Flag
                   WHEN 8
                       IF IDclie = R-ID THEN
                         MOVE R-Consumo TO ConsumoT
                       END-IF
                   WHEN 9
                       IF R-Consumo > rango-Ini AND
                       R-Consumo < rango-Fin THEN
                           DISPLAY "Cliente: "R-ID
                           "Nombre :" R-NombreUsuario
                       END-IF
                   WHEN 10
                       DISPLAY "Cliente: "R-NombreUsuario " Consumo: "
                       R-Consumo
                   WHEN 11
                       IF  R-Consumo < Bajo
                           ADD 1 TO Cont-Bajo GIVING Cont-Bajo
                       END-IF
                       IF  R-Consumo > Bajo AND R-Consumo < Alto
                           ADD 1 TO Cont-Medio GIVING Cont-Medio
                       END-IF
                       IF  R-Consumo > Alto
                           ADD 1 TO Cont-Alto GIVING Cont-Alto
                       END-IF
               END-EVALUATE
           EXIT.
