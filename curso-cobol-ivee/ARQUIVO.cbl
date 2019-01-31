      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 29.01.2019
      * Purpose: EXEMPLO - USO DE ASSIGN COM VARIÁVEL LENDO ARQUIVO
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     ARQUIVO.
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
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO OPE01
           ORGANIZATION IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQUIVO-ENTRADA.
       01  DATA-RECORD.
           03  GOODS-RECORD.
               05  GOODS-CODE          PIC X(4).
               05  FILLER              PIC X.
               05  GOODS-NAME          PIC X(30).
               05  FILLER              PIC X.
               05  GOODS-PRICE         PIC 9(4).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  NOME-PROGRAMA               PIC X(18) VALUE        'ARQUIVO'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS001'.

       01  VARIAVEIS-AUXILIARES.
           03  OPE01                   PIC X(51)
           VALUE "D:\Documentos\Cursos\COBOL\curso-cobol\DATAFILE.TXT".
           03 AUX-1                    PIC S9(4) COMP-5 VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM INICIA

           PERFORM PROCESSA

           PERFORM FINALIZA

           .

      *----------------------------------------------------------------*
           INICIA                      SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'INICIA'

           OPEN INPUT ARQUIVO-ENTRADA

           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'PROCESSA'

           PERFORM UNTIL AUX-1 = 1
               READ ARQUIVO-ENTRADA
               AT END MOVE 1 TO AUX-1
               NOT AT END
               DISPLAY "GOODS-CODE: " GOODS-CODE UPON CONSOLE
               DISPLAY "GOODS-NAME: " GOODS-NAME UPON CONSOLE
               DISPLAY "GOODS-PRICE: " GOODS-PRICE UPON CONSOLE
               DISPLAY "###### " UPON CONSOLE
           END-PERFORM
           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'FINALIZA'

           CLOSE ARQUIVO-ENTRADA

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
