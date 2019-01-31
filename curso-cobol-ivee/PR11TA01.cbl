      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 22.01.2019
      * Purpose: AMOSTRA PRATICA 11 TAREFA 01
      * Description:
      *    Desenvolver um programa com as seguintes caracter�sticas:

      *    - criar duas vari�veis num�ricas com valor inicial nulo na
      *     WORKING-STORAGE SECTION
      *    - solicitar inser��o de dados na console
      *    - as vari�veis dever�o ter valores diferentes entre si
      *    - em sendo inserido o valor j� existente, solicitar
      *     novamente a inser��o
      *    - se o valor continuar j� existindo encerrar o programa
      *    - receber os dados inseridos na console
      *    - apresentar conte�do das vari�veis na console do computador
      *     em ordem ascendente

      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PR11TA01.
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
       77  CTE-PROG                  PIC X(18) VALUE '*** PR11TA01 ***'.
       77  CTE-VERS                  PIC X(06) VALUE          'VRS001'.

       01  GRP-AUXILIARES.
           03  OPE01                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE02                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE0A                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE0B                   PIC S9(05) COMP-5   VALUE ZEROS.


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           DISPLAY CTE-PROG 'INICIA'

           DISPLAY "VALOR DE OPE01" UPON CONSOLE
           ACCEPT   OPE01           FROM CONSOLE

           DISPLAY "VALOR DE OPE02" UPON CONSOLE
           ACCEPT   OPE02           FROM CONSOLE

           IF  OPE01 = OPE02
               DISPLAY "OPE02 INV�LIDO, INSERIR DE NOVO" UPON CONSOLE
               ACCEPT OPE02            FROM CONSOLE
           END-IF


           IF  OPE01 = OPE02
               DISPLAY "VALOR INV�LIDO POR DUAS VEZES"   UPON CONSOLE
               STOP RUN
           END-IF

           IF  OPE01 > OPE02
               MOVE OPE01 TO OPE0A
               MOVE OPE02 TO OPE0B
           ELSE
               MOVE OPE02 TO OPE0A
               MOVE OPE01 TO OPE0B
           END-IF

           DISPLAY "OPE0A = "     OPE0A   UPON CONSOLE
           DISPLAY "OPE0B = "     OPE0B   UPON CONSOLE

           DISPLAY CTE-PROG 'FINALIZA'

           STOP RUN
           .
