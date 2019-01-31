      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 28.01.2019
      * Purpose: TR03 FLUX - Capítulo 03 - Aula 04
      * Description:
      *    Desenvolver um programa utilizando a instrução PERFORM, com
      *    as seguintes características:

      *    - estruturar a PROCEDURE DIVISION em SECTIONS
      *    - criar três variáveis numéricas com valor inicial nulo na
      *     WORKING-STORAGE SECTION
      *    - solicitar inserção de dados na console
      *    - as variáveis deverão ter valores diferentes de zero
      *    - em sendo inserido o valor zero em qualquer das variáveis,
      *     solicitar novamente a inserção
      *    - receber os dados inseridos na console
      *    - apresentar conteúdo das variáveis na console do computador
      *     em ordem ascendente
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PR14TA01.
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
       77  NOME-PROGRAMA             PIC X(18) VALUE '*** PR14TA01 ***'.
       77  VERSAO-PROGRAMA           PIC X(06) VALUE          'VRS001'.

       01  GRP-AUXILIARES.
           03  OPE01                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE02                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  OPE03                   PIC S9(05) COMP-5   VALUE ZEROS.
           03  AUX01                   PIC S9(05) COMP-5   VALUE ZEROS.

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

           DISPLAY "VALOR DE OPE01" UPON CONSOLE.
           ACCEPT   OPE01           FROM CONSOLE.

           DISPLAY "VALOR DE OPE02" UPON CONSOLE.
           ACCEPT   OPE02           FROM CONSOLE.

           DISPLAY "VALOR DE OPE03" UPON CONSOLE.
           ACCEPT   OPE03           FROM CONSOLE.

           IF  OPE01 = 0
               DISPLAY "OPE01 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               PERFORM INICIA
           END-IF

           IF  OPE02 = 0
               DISPLAY "OPE02 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               PERFORM INICIA
           END-IF

           IF  OPE03 = 0
               DISPLAY "OPE03 INVÁLIDO, INSERIR VALORES NOVAMENTE"
               UPON CONSOLE
               PERFORM INICIA
           END-IF

           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*
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
               PERFORM PROCESSA
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
