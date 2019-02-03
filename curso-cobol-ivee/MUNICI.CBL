      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 29.01.2019
      * Purpose: Lê dados de municípios em txt e separa os da UF
      *    escolhida em outro arquivo.
      *----------------------------------------------------------------*
      * VERSAO 03 - Gravar os municípios de um estado escolhido
      *    em outro arquivo.
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

           SELECT LISTA-MUNICIPIOS-UF-ESCOLHIDA ASSIGN TO OPE02
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

       FD  LISTA-MUNICIPIOS-UF-ESCOLHIDA.
       01  LS-MUN-ESCOLHIDA.
           03  NR-SEQL-MUN-ESCOLHIDA   PIC  9(07)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE '/'.
           03  CD-MUN-ESCOLHIDA        PIC  9(07)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE '/'.
           03  NM-MUN-ESCOLHIDA        PIC  X(35)           VALUE ' '.
           03  FILLER                  PIC  X(01)           VALUE '/'.
           03  SG-UF-ESCOLHIDA         PIC  X(02)           VALUE ' '.
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
           03  CONTA-MUNIC-UF          PIC S9(04) COMP-5    VALUE ZEROS.
           03  AUX-1                   PIC S9(09) COMP-5    VALUE ZEROS.
           03  SG-UF-DESEJADA          PIC  X(02)           VALUE ' '.

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

           PERFORM UNTIL SG-UF-DESEJADA
                                       NOT EQUAL SPACES
             DISPLAY "DIGITE A SIGLA DA UF: "
                                       UPON CONSOLE
             ACCEPT   SG-UF-DESEJADA  FROM CONSOLE
           END-PERFORM

           MOVE FUNCTION UPPER-CASE(SG-UF-DESEJADA)
                                       TO SG-UF-DESEJADA

           INITIALIZE LS-MUN-BR

           MOVE
           "D:\Documentos\Cursos\COBOL\curso-cobol\listaMunicipios.txt"
                                       TO OPE01
           MOVE
           "D:\Documentos\Cursos\COBOL\curso-cobol\municipiosUF.txt"
                                       TO OPE02

           OPEN INPUT LISTA-MUNICIPIOS-BRASIL

           OPEN OUTPUT LISTA-MUNICIPIOS-UF-ESCOLHIDA

           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'PROCESSA'

           INITIALIZE                  CONTA-MUNIC-BRASIL
                                       CONTA-MUNIC-UF

           PERFORM UNTIL FIM-DO-ARQUIVO
               READ LISTA-MUNICIPIOS-BRASIL
               AT END
                   MOVE 'S' TO IND-FIM-ARQUIVO
                   PERFORM GRAVA-RODAPE
               NOT AT END
               ADD 1                   TO AUX-1
               IF  AUX-1               > 1
                   ADD 1               TO CONTA-MUNIC-BRASIL
                   IF  SG-UF-E         = SG-UF-DESEJADA
                       ADD 1           TO CONTA-MUNIC-UF
                       IF  CONTA-MUNIC-UF
                                       = 1
                           PERFORM GRAVA-CABECALHO
                       END-IF
                       MOVE CONTA-MUNIC-UF
                                       TO NR-SEQL-MUN-ESCOLHIDA
                       MOVE CD-MUN-E-R TO CD-MUN-ESCOLHIDA
                       MOVE NM-MUN-E   TO NM-MUN-ESCOLHIDA
                       MOVE SG-UF-E    TO SG-UF-ESCOLHIDA
                       PERFORM GRAVA-ARQUIVO-S
                   END-IF
               END-IF
           END-PERFORM

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-CABECALHO             SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'GRAVA-CABECALHO'

           MOVE 0                      TO NR-SEQL-MUN-ESCOLHIDA
           MOVE 0                      TO CD-MUN-ESCOLHIDA
           MOVE "NM-MUNICIPIO"         TO NM-MUN-ESCOLHIDA
           MOVE "UF"                   TO SG-UF-ESCOLHIDA

           PERFORM GRAVA-ARQUIVO-S

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-ARQUIVO-S             SECTION.
      *----------------------------------------------------------------*

           WRITE LS-MUN-ESCOLHIDA

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-RODAPE                SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'GRAVA-RODAPE'

           MOVE 0                      TO NR-SEQL-MUN-ESCOLHIDA
           MOVE 0                      TO CD-MUN-ESCOLHIDA
           MOVE "QT-MUN-BR"            TO NM-MUN-ESCOLHIDA
           MOVE "##"                   TO SG-UF-ESCOLHIDA

           PERFORM GRAVA-ARQUIVO-S

           MOVE 0                      TO NR-SEQL-MUN-ESCOLHIDA
           MOVE 0                      TO CD-MUN-ESCOLHIDA
           MOVE CONTA-MUNIC-BRASIL     TO NM-MUN-ESCOLHIDA
           MOVE "##"                   TO SG-UF-ESCOLHIDA

           PERFORM GRAVA-ARQUIVO-S

           MOVE 0                      TO NR-SEQL-MUN-ESCOLHIDA
           MOVE 0                      TO CD-MUN-ESCOLHIDA
           MOVE "QT-MUN-UF"            TO NM-MUN-ESCOLHIDA
           MOVE "##"                   TO SG-UF-ESCOLHIDA

           PERFORM GRAVA-ARQUIVO-S

           MOVE 0                      TO NR-SEQL-MUN-ESCOLHIDA
           MOVE 0                      TO CD-MUN-ESCOLHIDA
           MOVE CONTA-MUNIC-UF         TO NM-MUN-ESCOLHIDA
           MOVE "##"                   TO SG-UF-ESCOLHIDA

           PERFORM GRAVA-ARQUIVO-S

           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'FINALIZA'

           CLOSE LISTA-MUNICIPIOS-BRASIL

           CLOSE LISTA-MUNICIPIOS-UF-ESCOLHIDA

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
