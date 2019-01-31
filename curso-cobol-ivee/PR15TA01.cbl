      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 30.01.2019
      * Purpose: TR03 FLUX - Cap�tulo 03 - Aula 05
      * Description:
      *    Desenvolver um programa utilizando a instru��o PERFORM ...
      *    UNTIL .., com as seguintes caracter�sticas:
      *    - criar tr�s vari�veis num�ricas com valor inicial nulo na
      *     WORKING-STORAGE SECTION
      *    - solicitar inser��o de dados na console
      *    - as vari�veis dever�o ter valores diferentes de zero
      *    - em sendo inserido o valor zero em qualquer das vari�veis,
      *     solicitar novamente a inser��o
      *    - receber os dados inseridos na console
      *    - apresentar conte�do das vari�veis na console do computador
      *     em ordem ascendente
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PR15TA01.
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
       77  NOME-PROGRAMA               PIC X(18) VALUE '** PR15TA01 **'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS001'.

       01  GRP-AUXILIARES.
           03  OPE01                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE02                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE03                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  AUX01                   PIC S9(05) COMP-5   VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*

           PERFORM INICIA

           PERFORM PROCESSA

           PERFORM FINALIZA

           .

      *----------------------------------------------------------------*
           INICIA                      SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'INICIA'

           PERFORM UNTIL OPE01         NOT EQUAL 0
             DISPLAY "DIGITE UM VALOR DIFERENTE DE ZERO PARA OPE01: "
                                       UPON CONSOLE
             ACCEPT   OPE01            FROM CONSOLE
           END-PERFORM


           PERFORM UNTIL OPE02         NOT EQUAL 0
             DISPLAY "DIGITE UM VALOR DIFERENTE DE ZERO PARA OPE02: "
                                       UPON CONSOLE
             ACCEPT   OPE02            FROM CONSOLE
           END-PERFORM


           PERFORM UNTIL OPE03         NOT EQUAL 0
             DISPLAY "DIGITE UM VALOR DIFERENTE DE ZERO PARA OPE03: "
                                       UPON CONSOLE
             ACCEPT   OPE03            FROM CONSOLE
           END-PERFORM

           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*


           PERFORM CALCULA UNTIL OPE03 >= OPE02 AND OPE02 >= OPE01

           .

           EXIT.

      *----------------------------------------------------------------*
           CALCULA                     SECTION.
      *----------------------------------------------------------------*

           IF  OPE01                   > OPE02
               MOVE OPE01              TO AUX01
               MOVE OPE02              TO OPE01
               MOVE AUX01              TO OPE02
           END-IF

           IF  OPE02                   > OPE03
               MOVE OPE02              TO AUX01
               MOVE OPE03              TO OPE02
               MOVE AUX01              TO OPE03
           END-IF

           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*

           DISPLAY "OPE01: " OPE01
           DISPLAY "OPE02: " OPE02
           DISPLAY "OPE03: " OPE03

           DISPLAY NOME-PROGRAMA 'FINALIZA'

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
