      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 29.01.2019
      * Purpose: Lê dados de municípios em txt e separa os de MG
      *     em outro arquivo.
      *----------------------------------------------------------------*
      * VERSAO 02 - Gravar os municípios de MG em outro arquivo.
      * VERSAO 01 - Display nos municípios de MG.
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     MUNICI.
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

           SELECT LISTA-MUNICIPIOS-MG ASSIGN TO OPE02
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
           03  CD-MUN-E-R REDEFINES CD-MUN-E.
               05  CD-MUN-N            PIC 9(07).
           03  NM-MUN-E                PIC  X(35)           VALUE ' '.

       FD  LISTA-MUNICIPIOS-MG.
       01  LS-MUN-MG.
           03  NR-SEQL-MUN-MG          PIC  9(07)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE '/'.
           03  CD-MUN-MG               PIC  9(07)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE '/'.
           03  NM-MUN-MG               PIC  X(35)           VALUE ' '.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  NOME-PROGRAMA               PIC X(18) VALUE   '** MUNICI **'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS002'.

       01  VARIAVEIS-AUXILIARES.
           03  OPE01                   PIC X(59)            VALUE ' '.
           03  OPE02                   PIC X(56)            VALUE ' '.
           03  IND-FIM-ARQUIVO         PIC X(01)            VALUE 'N'.
               88  FIM-DO-ARQUIVO                           VALUE 'S'.
           03  CONTA-MUNIC-BRASIL      PIC S9(04) COMP-5    VALUE ZEROS.
           03  CONTA-MUNIC-MG          PIC S9(04) COMP-5    VALUE ZEROS.
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
           MOVE
           "D:\Documentos\Cursos\COBOL\curso-cobol\municipiosMG.txt"
                                       TO OPE02

           OPEN INPUT LISTA-MUNICIPIOS-BRASIL

           OPEN OUTPUT LISTA-MUNICIPIOS-MG

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
                       MOVE CONTA-MUNIC-MG TO NR-SEQL-MUN-MG
                       MOVE CD-MUN-E-R   TO CD-MUN-MG
                       MOVE NM-MUN-E   TO NM-MUN-MG
                       PERFORM GRAVA-ARQUIVO-S
                   END-IF
               END-IF
           END-PERFORM
           DISPLAY "CONTA-MUNIC-BRASIL: " CONTA-MUNIC-BRASIL
           DISPLAY "CONTA-MUNIC-MG: " CONTA-MUNIC-MG
           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-ARQUIVO-S             SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'GRAVA-ARQUIVO-S'

           WRITE LS-MUN-MG


           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'FINALIZA'

           CLOSE LISTA-MUNICIPIOS-BRASIL

           CLOSE LISTA-MUNICIPIOS-MG

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
