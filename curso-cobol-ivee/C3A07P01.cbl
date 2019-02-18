      *----------------------------------------------------------------*
      * Author: JOSE CAETANO BEUKER
      * Date: 18.02.2019
      * Purpose: TR03 FLUX - Capítulo 03 - Aula 07 - Prática 01
      * Description:
      *    Ler o arquivo sequencial com registros E1-C3A07P01.txt e
      *    criar o arquivo sequencial com registros S1-C3A07P01.txt.
      *    O arquivo de entrada contém as horas trabalhadas no mês por
      *    funcionário. O layout do arquivo é:
      *    MATRICULA  PIC 9(4)
      *    VALOR-HORA PIC 999V99
      *    HORAS-MES PIC 9(5)
      *    FUNCIONARIO PIC x(20)
      *
      *    O arquivo de saída deverá conter o adiantamento por
      *    funcionario. O layout do arquivo deverá ser:
      *    MATRICULA  PIC 9(4)
      *    VALOR-HORA PIC 999V99
      *    HORAS-MES PIC 9(5)
      *    ADIANTAMENTO PIC 9(7)V99
      *
      *    Calcular o adiantamento e gravar no registro de saída, bem
      *    como os demais campos.
      *    Adiantamento = Salario-Bruto * 0,40.
      *    Salário-Bruto = Valor-Hora * Horas-Mes.
      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     C3A07P01.
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
           SELECT E1-C3A07P01          ASSIGN TO OPE01.

           SELECT S1-C3A07P01          ASSIGN TO OPE02.
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  E1-C3A07P01.
       01  REG-ENTADA                  PIC X(34)            VALUE   ' '.

       FD  S1-C3A07P01.
       01  REG-SAIDA                   PIC X(23)              VALUE ' '.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  NOME-PROGRAMA               PIC X(18) VALUE '** C3A07P01 **'.
       77  VERSAO-PROGRAMA             PIC X(06) VALUE         'VRS001'.

       01  ARQUIVO-ENTRADA.
           03  E1-MATRICULA            PIC  9(04)           VALUE ZEROS.
           03  E1-VALOR-HORA           PIC  9(03)V99        VALUE ZEROS.
           03  E1-HORAS-MES            PIC  9(05)           VALUE ZEROS.
           03  E1-FUNCIONARIO          PIC  X(20)           VALUE   ' '.

       01  ARQUIVO-SAIDA.
           03  S1-MATRICULA            PIC  9(04)           VALUE ZEROS.
           03  S1-VALOR-HORA           PIC  9(03)V99        VALUE ZEROS.
           03  S1-HORAS-MES            PIC  9(05)           VALUE ZEROS.
           03  S1-ADIANTAMENTO         PIC  9(07)V99        VALUE ZEROS.

       01  VAR-AUX.
           03  OPE01                   PIC X(54)              VALUE ' '.
           03  OPE02                   PIC X(54)              VALUE ' '.
           03  AUX-1                   PIC S9(09)             VALUE  +0.
           03  SALARIO-BRUTO           PIC S9(09)V99          VALUE  +0.
           03  IND-FIM-ARQUIVO         PIC X(01)              VALUE 'N'.
               88  FIM-DO-ARQUIVO                             VALUE 'S'.

      *
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIA

           PERFORM 2000-PROCESSA

           PERFORM 3000-FINALIZA

           .
       0000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       1000-INICIA                 SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA '1000-INICIA'

           MOVE 'D:\Documentos\Cursos\COBOL\curso-cobol\E1-C3A07P01.txt'
                                       TO OPE01

           MOVE 'D:\Documentos\Cursos\COBOL\curso-cobol\S1-C3A07P01.txt'
                                       TO OPE02

           OPEN INPUT E1-C3A07P01

           OPEN OUTPUT S1-C3A07P01

           .
       1000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESSA               SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA '2000-PROCESSA'

           PERFORM UNTIL FIM-DO-ARQUIVO
               READ E1-C3A07P01 INTO ARQUIVO-ENTRADA
               AT END
                   MOVE 'S'            TO IND-FIM-ARQUIVO
                   PERFORM 2300-GRAVA-RODAPE
               NOT AT END
                   ADD 1               TO AUX-1
                   IF  AUX-1           = 1
                       PERFORM 2100-GRAVA-CABECALHO
                   END-IF
                   MOVE E1-MATRICULA   TO S1-MATRICULA
                   MOVE E1-VALOR-HORA  TO S1-VALOR-HORA
                   MOVE E1-HORAS-MES   TO S1-HORAS-MES
                   COMPUTE SALARIO-BRUTO =  E1-VALOR-HORA * E1-HORAS-MES
                   COMPUTE S1-ADIANTAMENTO = SALARIO-BRUTO * 0,4
                   PERFORM 2200-GRAVA-ARQUIVO-SAIDA
           END-PERFORM

           .
       2000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       2100-GRAVA-CABECALHO            SECTION.
      *----------------------------------------------------------------*

           MOVE 0                      TO S1-MATRICULA
           MOVE 0                      TO S1-VALOR-HORA
           MOVE 0                      TO S1-HORAS-MES
           MOVE 0                      TO S1-ADIANTAMENTO

           PERFORM 2200-GRAVA-ARQUIVO-SAIDA

           .
       2100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       2200-GRAVA-ARQUIVO-SAIDA        SECTION.
      *----------------------------------------------------------------*

           WRITE REG-SAIDA FROM ARQUIVO-SAIDA

           .
       2200-SAI.
           EXIT.

      *----------------------------------------------------------------*
       2300-GRAVA-RODAPE               SECTION.
      *----------------------------------------------------------------*

           MOVE 9999                   TO S1-MATRICULA
           MOVE 999,99                 TO S1-VALOR-HORA
           MOVE 99999                  TO S1-HORAS-MES
           MOVE 9999999,99             TO S1-ADIANTAMENTO

           PERFORM 2200-GRAVA-ARQUIVO-SAIDA

           .
       2300-SAI.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZA               SECTION.
      *----------------------------------------------------------------*
           DISPLAY NOME-PROGRAMA '3000-FINALIZA'

           CLOSE E1-C3A07P01

           CLOSE S1-C3A07P01

           STOP RUN
           .
       3000-SAI.
           EXIT.

      *----------------------------------------------------------------*
