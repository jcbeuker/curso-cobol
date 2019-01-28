      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 14.01.2018
      * Purpose: APRENDER
      * Tectonics: cobc
      *----------------------------------------------------------------*
      * VRS001 14.01.2017 - CAETANO - IMPLANTACAO
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     TESTE.
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
       77  CTE-INICIO-SS               PIC X(40)           VALUE
                         '*** S. S. COMECA AQUI ***'.
       77  CTE-PROG                    PIC X(18)           VALUE
                                                        '*** TESTE ***'.
       77  CTE-VERS                    PIC X(06)           VALUE
                                                               'VRS001'.
      * Constante com o nome da cidade
       01  DF-BRASILIA                 PIC X(11)           VALUE
                                                          'DF-BRASILIA'.
      *----------------------------------------------------------------*
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*
       01  GRP-AUXILIARES.
           03  VL-EXBR-1               PIC S9(07) COMP-5   VALUE ZEROS.
           03  VL-EXBR-2               PIC S9(07) COMP-5   VALUE ZEROS.
           03  WS-CNT1                 PIC S9(09) COMP-5   VALUE ZEROS.
           03  CONTA-CARACTERES        PIC  X(30)          VALUE SPACES.

       77  CTE-FINAL-SS                PIC X(40)           VALUE
                        '*** S. S. TERMINA AQUI ***'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIA

           PERFORM 200000-PROCESSA

           PERFORM 300000-FINALIZA

           .
       000000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       100000-INICIA                   SECTION.
      *----------------------------------------------------------------*

           DISPLAY CTE-PROG '100000-INICIA'

           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSA                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY CTE-PROG '200000-PROCESSA'

           MOVE 'JOSE CAETANO BEUKER' TO CONTA-CARACTERES

           MOVE 0 TO WS-CNT1
           INSPECT CONTA-CARACTERES TALLYING WS-CNT1
           FOR CHARACTERS BEFORE '  '
           DISPLAY "WS-CNT1 : " WS-CNT1
           MOVE 01                     TO VL-EXBR-1

           DISPLAY CTE-PROG 'VL-EXBR-1: ' VL-EXBR-1
           DISPLAY CTE-PROG 'VL-EXBR-2: ' VL-EXBR-2

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       300000-FINALIZA                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY CTE-PROG '300000-FINALIZA'

           GOBACK

           .
       300000-SAI.
           EXIT.
