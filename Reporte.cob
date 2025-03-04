       IDENTIFICATION DIVISION.
       PROGRAM-ID. Reporte AS "Reporte".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Definición de archivos secuenciales con su estado.
           SELECT Maestro ASSIGN TO 'Clientes.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusMas.

           SELECT Detalle ASSIGN TO 'Consumo2025.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusDet.


           SELECT MaestroO ASSIGN TO 'MaestroO.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusMasO.

           SELECT DetalleO ASSIGN TO 'DetalleO.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusDetO.

           SELECT Reporte ASSIGN TO 'Reporte.Txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-File-StatusRep.

           SELECT Work-FileM ASSIGN TO SORT-WORK.
           SELECT Work-FileD ASSIGN TO SORT-WORK.

       DATA DIVISION.
       FILE SECTION.
       FD Maestro.
       01 MaestroR.
           05 M-ID                 PIC 9(5).
           05 M-NombreUsuario      PIC X(40).

       FD Detalle.
       01 DetalleoR.
           05 D-ID                 PIC 9(5).
           05 D-NombreUsuario      PIC X(40).
           05 D-Mes                PIC X(15).
           05 D-Consumo            PIC 9(5).

       FD MaestroO.
       01 MaestroOR.
           05 MO-ID                 PIC 9(5).
           05 MO-NombreUsuario      PIC X(40).

       FD DetalleO.
       01 DetalleoOR.
           05 DO-ID                 PIC 9(5).
           05 DO-NombreUsuario      PIC X(40).
           05 DO-Mes                PIC X(15).
           05 DO-Consumo            PIC 9(5).

       FD Reporte.
       01 Reporte-R.
           05 R-ID                  PIC 9(5).
           05 R-FILLER1             PIC X .
           05 R-NombreUsuario       PIC X(40).
           05 R-FILLER2             PIC X .
           05 R-Consumo             PIC 9(5).
           05 R-FILLER3             PIC X .

       SD Work-FileM.
       01 WorkRM.
           05 W-ID                 PIC 9(5).
           05 W-NombreUsuario      PIC X(40).


       SD Work-FileD.
       01 WorkRD.
           05 WD-ID                 PIC 9(5).
           05 WD-NombreUsuario      PIC X(40).
           05 WD-Mes                PIC X(15).
           05 WD-Consumo            PIC 9(5).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-File-StatusMas     PIC XX.
               05 WS-File-StatusDet     PIC XX.
               05 WS-File-StatusMasO    PIC XX.
               05 WS-File-StatusDetO    PIC XX.
               05 WS-File-StatusRep     PIC XX.
               05 respuesta             PIC X(1).
           01 WS-Flags.
               05 FlagMaster           PIC X(1) VALUE 'N'.
               05 FlagDetalle          PIC X(1) VALUE 'N'.
               05 FlagReport           PIC X(1) VALUE 'N'.
           01 WS-Temp.
               05 Temp-ID              PIC 9(5).
               05 Temp-Nombre          PIC X(40).
           01 WS-Total                 PIC 9(15).

       LINKAGE SECTION.
           01 LK-Parametros.
               05 Meses                PIC 9(15) OCCURS 12 TIMES.
               05 Total                PIC 9(10).
               05 Consumo              PIC 9(10).
               05 Promedio             PIC 9(10)V99.

       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Verificar
           GOBACK
       EXIT PROGRAM.

           Ordenar.
               SORT Work-FileM
               ON ASCENDING KEY W-ID
               USING Maestro
               GIVING MaestroO

               SORT Work-FileD
               ON ASCENDING KEY W-ID
               USING Detalle
               GIVING DetalleO
           EXIT.



           *> Procedimiento que verifica la existencia de archivos y realiza cálculos.
           Verificar.
               OPEN INPUT Maestro
               OPEN INPUT Detalle
               IF WS-File-StatusMas = '00' AND
                  WS-File-StatusDet = '00' THEN
                   PERFORM Ordenar
                   PERFORM Maestro-Detalle
                   OPEN INPUT Reporte
                   PERFORM Reportar
                   CLOSE Reporte
                   CLOSE Maestro
               ELSE
                   DISPLAY "ERROR"
               END-IF
           EXIT.

           Maestro-Detalle.
               OPEN INPUT MaestroO
               OPEN INPUT DetalleO
               OPEN OUTPUT Reporte
               PERFORM Leer-Maestro
               PERFORM Leer-Detalle
               MOVE MO-ID TO Temp-ID
               MOVE MO-NombreUsuario TO Temp-Nombre
               PERFORM Evaluar
               CLOSE MaestroO
               CLOSE DetalleO
               DELETE FILE MaestroO
               DELETE FILE DetalleO
               CLOSE Reporte
           EXIT.


           Leer-Maestro.
               READ MaestroO INTO MaestroOR
                   AT END
                       MOVE "Y" TO FlagMaster
               END-READ
           EXIT.

           Leer-Detalle.
               READ DetalleO INTO DetalleOR
                   AT END
                       MOVE "Y" TO FlagDetalle
               END-READ
           EXIT.


           Contar-Mes.
                EVALUATE DO-Mes
                   WHEN "Enero"
                       ADD 1 TO Meses(1)
                   WHEN "Febrero"
                       ADD 1 TO Meses(2)
                   WHEN "Marzo"
                       ADD 1 TO Meses(3)
                   WHEN "Abril"
                       ADD 1 TO Meses(4)
                   WHEN "Mayo"
                       ADD 1 TO Meses(5)
                   WHEN "Junio"
                       ADD 1 TO Meses(6)
                   WHEN "Julio"
                       ADD 1 TO Meses(7)
                   WHEN "Agosto"
                       ADD 1 TO Meses(8)
                   WHEN "Septiembre"
                       ADD 1 TO Meses(9)
                   WHEN "Octubre"
                       ADD 1 TO Meses(10)
                   WHEN "Noviembre"
                       ADD 1 TO Meses(11)
                   WHEN "Diciembre"
                       ADD 1 TO Meses(12)
                END-EVALUATE
           EXIT.

           Evaluar.
               PERFORM UNTIL FlagMaster = 'Y' OR FlagDetalle = 'Y'
                   PERFORM Contar-Mes
                   IF DO-ID = Temp-ID THEN
                       ADD DO-Consumo TO WS-Total
                       GIVING WS-Total
                       PERFORM Leer-Detalle
                   ELSE IF DO-ID > Temp-ID THEN
                       PERFORM Agregar-Report
                       MOVE 0 TO WS-Total
                       PERFORM Leer-Maestro
                       MOVE MO-ID TO Temp-ID
                       MOVE MO-NombreUsuario TO Temp-Nombre
                   ELSE
                       DISPLAY "ERROR No esta en el maestro"
                   END-IF
               END-PERFORM
               *> Si el maestro terminó, pero aún quedan detalles
               IF FlagMaster = 'Y' AND FlagDetalle = 'N' THEN
                   PERFORM UNTIL FlagDetalle = 'Y' OR DO-ID
                   NOT = Temp-ID
                       ADD DO-Consumo TO WS-Total
                       PERFORM Leer-Detalle
                   END-PERFORM
               END-IF

           *> Agregar el último registro al reporte si hay un consumo acumulado
               IF WS-Total > 0 THEN
                   PERFORM Agregar-Report
               END-IF
           EXIT.

       Informe SECTION.
           Reportar.
               PERFORM UNTIL FlagReport = "Y"
                   READ Reporte INTO Reporte-R
                       AT END
                           MOVE "Y" TO FlagReport
                       NOT AT END
                           ADD 1 TO Total GIVING Total
                           ADD R-Consumo TO Consumo GIVING Consumo
                   END-READ
               END-PERFORM
               COMPUTE Promedio = (Consumo / total)
           EXIT.



           Agregar-Report.
               MOVE Temp-ID TO R-ID
               MOVE "|" To R-FILLER1
               MOVE Temp-Nombre TO R-NombreUsuario
               MOVE "|" To R-FILLER2
               MOVE WS-Total TO R-Consumo
               MOVE "|" To R-FILLER3
               WRITE Reporte-R
           EXIT.







           *> Punto de salida del programa.
           salir.
           EXIT.
