       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGRAVA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI ASSIGN TO DISK
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-FS-CLI.
           SELECT MOVIMEN ASSIGN TO DISK
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-FS-MOV.
       DATA DIVISION.
       FILE SECTION.
       FD CADCLI VALUE OF FILE-ID IS "CLIENTE.DAT".
       01 REG-CLI.
          03 ID-CLI.
             05 AGENCIA-CLI        PIC 9(03).
             05 CONTA-CLI          PIC 9(06).
          03 NOME-CLI              PIC X(20).
          03 SALDO                 PIC S9(09).

       FD MOVIMEN VALUE OF FILE-ID IS "MOVIMENTO.DAT".
       01 REG-MOV.
          03 ID-MOV.
             05 AGENCIA-MOV        PIC 9(03).
             05 CONTA-MOV          PIC 9(06).
          03 MOVIMENTO             PIC 9(09).


       WORKING-STORAGE SECTION.
       01 WS-FS-CLI           PIC X(02) VALUE ZEROS.
       01 WS-FS-MOV           PIC X(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRE-ARQ.
           PERFORM GRAVA-REG-CLI.
           PERFORM GRAVA-REG-MOV.
           CLOSE CADCLI.
           CLOSE MOVIMEN.
           STOP RUN.

       ABRE-ARQ.
           OPEN OUTPUT CADCLI.
           IF WS-FS-CLI NOT EQUAL "00"
             DISPLAY "ERRO DE ABERTURA CLIENTE - " WS-FS-CLI
             STOP RUN.
           OPEN OUTPUT MOVIMEN.
           IF WS-FS-MOV NOT EQUAL "00"
             DISPLAY "ERRO DE ABERTURA MOVIMENTO - " WS-FS-MOV
             STOP RUN.

       GRAVA-REG-CLI.
           CLOSE CADCLI.
           OPEN EXTEND CADCLI.

           MOVE 001                 TO AGENCIA-CLI.
           MOVE 002222              TO CONTA-CLI.
           MOVE "Manuel de Almeida" TO NOME-CLI.
           MOVE 001000000            TO SALDO.
           WRITE REG-CLI.

           MOVE 001                 TO AGENCIA-CLI.
           MOVE 011111              TO CONTA-CLI.
           MOVE "Joao da Silva"     TO NOME-CLI.
           MOVE 002000000            TO SALDO.
           WRITE REG-CLI.

           MOVE 001                 TO AGENCIA-CLI.
           MOVE 033333              TO CONTA-CLI.
           MOVE "Alfredo das Neves" TO NOME-CLI.
           MOVE 003500000            TO SALDO.
           WRITE REG-CLI.

           MOVE 012                 TO AGENCIA-CLI.
           MOVE 044444              TO CONTA-CLI.
           MOVE "Maria Aparecida" TO NOME-CLI.
           MOVE 004200000            TO SALDO.
           WRITE REG-CLI.

       GRAVA-REG-MOV.
           CLOSE MOVIMEN.
           OPEN EXTEND MOVIMEN.

           MOVE 001                 TO AGENCIA-MOV.
           MOVE 002222              TO CONTA-MOV.
           MOVE 000050000           TO MOVIMENTO.
           WRITE REG-MOV.

           MOVE 001                 TO AGENCIA-MOV.
           MOVE 002222              TO CONTA-MOV.
           MOVE 000250000           TO MOVIMENTO.
           WRITE REG-MOV.

           MOVE 001                 TO AGENCIA-MOV.
           MOVE 031313              TO CONTA-MOV.
           MOVE 000550000           TO MOVIMENTO.
           WRITE REG-MOV.

           MOVE 012                 TO AGENCIA-MOV.
           MOVE 044444              TO CONTA-MOV.
           MOVE -000200000          TO MOVIMENTO.
           WRITE REG-MOV.
