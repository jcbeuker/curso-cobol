      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 29.01.2019
      * Purpose: Lê dados de municípios em txt.
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     MUNICIP.
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
           SELECT LISTA-MUNICIPIOS-BRASIL ASSIGN TO OPE01
           ORGANIZATION IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  LISTA-MUNICIPIOS-BRASIL.
       01  LS-MUN-BR.
           03  CD-UF-E                 PIC  9(02)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE ' '.
           03  SG-UF-E                 PIC  X(02)           VALUE ' '.
           03  FILLER                  PIC  X(01)           VALUE ' '.
           03  CD-MUN-DV-E             PIC  9(08)           VALUE ZEROS.
           03  CD-MUN-E                PIC  9(07)           VALUE ZEROS.
           03  NM-MUN-E                PIC  X(35)           VALUE ' '.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  NOME-PROGRAMA               PIC X(18) VALUE  '** MUNICIP **'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS001'.

       01  VARIAVEIS-AUXILIARES.
           03  OPE01                   PIC X(59)            VALUE ' '.
           03  IND-FIM-ARQUIVO         PIC X(01)            VALUE 'N'.
               88  FIM-DO-ARQUIVO                           VALUE 'S'.
           03  CONTA-MUNIC-BRASIL      PIC S9(09) COMP-5    VALUE ZEROS.
           03  CONTA-MUNIC-MG          PIC S9(09) COMP-5    VALUE ZEROS.
           03  AUX-1                   PIC S9(09) COMP-5    VALUE ZEROS.

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

           INITIALIZE LS-MUN-BR

           MOVE
           "D:\Documentos\Cursos\COBOL\curso-cobol\listaMunicipios.txt"
                                       TO OPE01

           OPEN INPUT LISTA-MUNICIPIOS-BRASIL

           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'PROCESSA'

           INITIALIZE                  CONTA-MUNIC-BRASIL
                                       CONTA-MUNIC-MG

           PERFORM UNTIL FIM-DO-ARQUIVO
               READ LISTA-MUNICIPIOS-BRASIL
               AT END MOVE 'S' TO IND-FIM-ARQUIVO
               NOT AT END
               ADD 1                   TO AUX-1
               IF  AUX-1               > 1
                   ADD 1               TO CONTA-MUNIC-BRASIL
                   IF  CD-UF-E         = "31"
                       ADD 1           TO CONTA-MUNIC-MG
                       DISPLAY "CD-UF-E: " CD-UF-E
                       DISPLAY "SG-UF-E: " SG-UF-E
                       DISPLAY "CD-MUN-DV-E: " CD-MUN-DV-E
                       DISPLAY "CD-MUN-E: " CD-MUN-E
                       DISPLAY "NM-MUN-E: " NM-MUN-E
                   END-IF
               END-IF
           END-PERFORM
           DISPLAY "CONTA-MUNIC-BRASIL: " CONTA-MUNIC-BRASIL
           DISPLAY "CONTA-MUNIC-MG: " CONTA-MUNIC-MG
           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'FINALIZA'

           CLOSE LISTA-MUNICIPIOS-BRASIL

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
