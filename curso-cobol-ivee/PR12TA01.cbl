      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 28.01.2019
      * Purpose: TR03 FLUX - Capítulo 03 - Aula 02 - Amostra 01
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PR12TA01.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  CTE-PROG                  PIC X(18) VALUE '*** PR12TA01 ***'.
       77  CTE-VERS                  PIC X(06) VALUE          'VRS001'.

       01  GRP-AUXILIARES.
           03  OPE01                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE02                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE03                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  AUX01                   PIC S9(05) COMP-5   VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       INICIO.
           DISPLAY CTE-PROG 'INICIA'

           DISPLAY "VALOR DE OPE01" UPON CONSOLE.
           ACCEPT   OPE01           FROM CONSOLE.

           DISPLAY "VALOR DE OPE02" UPON CONSOLE.
           ACCEPT   OPE02           FROM CONSOLE.

           DISPLAY "VALOR DE OPE03" UPON CONSOLE.
           ACCEPT   OPE03           FROM CONSOLE.

           IF  OPE01 = 0
               DISPLAY "OPE01 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               GO TO INICIO
           END-IF

           IF  OPE02 = 0
               DISPLAY "OPE02 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               GO TO INICIO
           END-IF

           IF  OPE03 = 0
               DISPLAY "OPE03 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               GO TO INICIO
           END-IF
           .
       CALCULA.
           MOVE 0 TO AUX01

           IF  OPE01 > OPE02
               MOVE OPE01 TO AUX01
               MOVE OPE02 TO OPE01
               MOVE AUX01 TO OPE02
           END-IF

           IF  OPE02 > OPE03
               MOVE OPE02 TO AUX01
               MOVE OPE03 TO OPE02
               MOVE AUX01 TO OPE03
           END-IF

           IF  AUX01 NOT EQUAL 0
               GO TO CALCULA
           END-IF
           .
           DISPLAY "OPE01: " OPE01
           DISPLAY "OPE02: " OPE02
           DISPLAY "OPE03: " OPE03

           DISPLAY CTE-PROG 'FINALIZA'

           STOP RUN
           .
