      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 15.02.2019
      * Purpose: TR03 FLUX - Capítulo 03 - Aula 06
      * Description:
      *    Desenvolver um programa para criar um arquivo sequencial com
      *    registros, utilizando a estrutura abaixo e recebendo os dados
      *    da console.
      *    CODIGO - PIC 9(4)
      *    PRECO-UNITARIO - PIC 999V99
      *    QTD-VENDA - PIC 9(5)
      *    VENDEDOR - PIC X(20)
      *    - incluir no mínimo 10 registros
      *    - o código deverá ser único
      *    - inventar seus próprios registros
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PR16TA01.
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
           SELECT ARQUIVO-SAIDA ASSIGN TO OPE01
           ORGANIZATION IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQUIVO-SAIDA.
       01  REGISTRO-SAIDA-DETALHE.
           03  CODIGO-ITEM-S           PIC  9(04)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE  ' '.
           03  PRECO-UNITARIO-S        PIC  9(03)V99        VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE  ' '.
           03  QTD-VENDA-S             PIC  9(05)           VALUE ZEROS.
           03  FILLER                  PIC  X(01)           VALUE  ' '.
           03  VENDEDOR-S              PIC  X(40)           VALUE   ' '.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  NOME-PROGRAMA               PIC X(18) VALUE '** PR16TA01 **'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS001'.

       01  REG-ENT.
           03  CODIGO-ITEM-E           PIC  9(04)           VALUE ZEROS.
           03  PRECO-UNITARIO-E        PIC  9(03)V99        VALUE ZEROS.
           03  QTD-VENDA-E             PIC  9(05)           VALUE ZEROS.
           03  VENDEDOR-E              PIC  X(40)           VALUE   ' '.


       01  VARIAVEIS-AUXILIARES.
           03  CONTA-REGISTROS         PIC S9(09) COMP-5    VALUE ZEROS.
           03  TOTALIZA-VENDAS         PIC S9(09) COMP-5    VALUE ZEROS.


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

           INITIALIZE REG-ENT
                      VARIAVEIS-AUXILIARES

           MOVE "D:\Documentos\Cursos\COBOL\curso-cobol\S1-PR16TA01.txt"
                                       TO OPE01

           OPEN OUTPUT ARQUIVO-SAIDA


           .

           EXIT.

      *----------------------------------------------------------------*
           PROCESSA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'PROCESSA'

           PERFORM UNTIL CODIGO-ITEM-E = 9999
               IF  CONTA-REGISTROS = 0
                 PERFORM GRAVA-CABECALHO
               END-IF
               DISPLAY 'DIGITE o código do item 9999 para finalizar: '
                                       UPON CONSOLE
               ACCEPT CODIGO-ITEM-E    FROM CONSOLE
               IF  CODIGO-ITEM-E       NOT EQUAL 9999
                   COMPUTE
                   CODIGO-ITEM-E = CODIGO-ITEM-E + CONTA-REGISTROS + 109

                   DISPLAY "DIGITE o preço unitário: "
                                       UPON CONSOLE
                   ACCEPT PRECO-UNITARIO-E FROM CONSOLE

                   DISPLAY "DIGITE a quantidade vendida: "
                                       UPON CONSOLE
                   ACCEPT QTD-VENDA-E      FROM CONSOLE

                   DISPLAY "DIGITE o nome do vendedor: "
                                       UPON CONSOLE
                   ACCEPT VENDEDOR-E   FROM CONSOLE

                   ADD 1               TO CONTA-REGISTROS

                   MOVE CODIGO-ITEM-E  TO CODIGO-ITEM-S
                   MOVE PRECO-UNITARIO-E
                                       TO PRECO-UNITARIO-S
                   MOVE QTD-VENDA-E    TO QTD-VENDA-S
                   MOVE VENDEDOR-E     TO VENDEDOR-S

                   PERFORM GRAVA-ARQUIVO-S
               ELSE
                   PERFORM GRAVA-RODAPE
               END-IF
           END-PERFORM

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-CABECALHO             SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'GRAVA-CABECALHO'

           MOVE 0000                   TO CODIGO-ITEM-S
           MOVE 0000                   TO PRECO-UNITARIO-S
           MOVE 00000                  TO QTD-VENDA-S
           MOVE 'NOME DO VENDEDOR'     TO VENDEDOR-S

           PERFORM GRAVA-ARQUIVO-S

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-ARQUIVO-S             SECTION.
      *----------------------------------------------------------------*

           WRITE REGISTRO-SAIDA-DETALHE

           .

           EXIT.

      *----------------------------------------------------------------*
           GRAVA-RODAPE                SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'GRAVA-RODAPE'

           MOVE 9999                   TO CODIGO-ITEM-S
           MOVE 999                    TO PRECO-UNITARIO-S
           MOVE 99999                  TO QTD-VENDA-S
           MOVE '---'                  TO VENDEDOR-S

           PERFORM GRAVA-ARQUIVO-S

           .

           EXIT.

      *----------------------------------------------------------------*
           FINALIZA                    SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA 'FINALIZA'

           CLOSE ARQUIVO-SAIDA

           STOP RUN

           .

           EXIT.
      *----------------------------------------------------------------*
