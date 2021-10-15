       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATTMOV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      ***
      *** DEFINE OS ARQUIVOS DE ENTRADA E SAIDA
      ***
       FILE-CONTROL.

           SELECT ARQMOV ASSIGN TO "MOVIMEN.DAT"
             ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS ST-ALU.

           SELECT ARQFOR ASSIGN TO "FORNECE.DAT"
           ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS ST-HOM.

           SELECT ARQSAI  ASSIGN TO "SAIDA.DAT"
             ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS ST-MUL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQMOV.
       01  REG-MOVIMENTO.
            05  CDFORNM PIC 9(03).
            05  NMFORMM    PIC  X(30).
            05  NMCIDADEM    PIC  X(30).
            05  SGMOVIM  PIC  X(01).

       FD  ARQFOR.
       01  REG-FORNECEDOR.
            05  CDFORNF PIC 9(03).
            05  NMFORMF    PIC  X(30).
            05  NMCIDADEF    PIC  X(30).

       FD  ARQSAI.
       01  REG-SAIDA.
            05  CDFORNS PIC 9(03).
            05  NMFORMS    PIC  X(30).
            05  NMCIDADES    PIC  X(30).

       WORKING-STORAGE SECTION.

       01 WS-DADOS.
           02  FIM-ARQ      PIC 9(01).
           02  FIM-ARQ1     PIC 9(01).
           02  ST-ALU       PIC X(02).
           02  ST-HOM       PIC X(02).
           02  ST-MUL       PIC X(02).

       PROCEDURE DIVISION.

       INICIO.
           PERFORM ABRE-ARQ.
           MOVE ZEROS TO WS-DADOS.
           READ ARQMOV.
           PERFORM PROCESSO.
           PERFORM FINALIZA.
           STOP RUN.

       ABRE-ARQ.
           OPEN INPUT ARQMOV.
           IF ST-ALU NOT EQUAL '00'
              DISPLAY 'ERRO DE ABERTURA - CAD ALUNO' ST-ALU
              STOP RUN.

           OPEN INPUT ARQFOR.
           IF ST-HOM NOT EQUAL '00'
              DISPLAY 'ERRO DE ABERTURA - CAD HOMEM' ST-HOM
              STOP RUN.

           OPEN OUTPUT ARQSAI.
           IF ST-MUL NOT EQUAL '00'
              DISPLAY 'ERRO DE ABERTURA - CAD MULHE' ST-MUL
              STOP RUN.

       PROCESSO.
           READ ARQMOV AT END MOVE 1 TO FIM-ARQ.
           READ ARQFOR AT END MOVE 1 TO FIM-ARQ1.
           PERFORM LER-ARQ UNTIL FIM-ARQ = 1.

       LER-ARQ.


           PERFORM LER-ARQ1 UNTIL FIM-ARQ1 = 1.
           READ ARQMOV AT END MOVE 1 TO FIM-ARQ.

       LER-ARQ1.
              DISPLAY 'COMPARANDO CODIGO FORNECEDOR ' CDFORNF.
              DISPLAY 'COM CODIGO DO MOVIMENTO ' CDFORNM.
              IF CDFORNF<CDFORNM
                  MOVE REG-FORNECEDOR TO REG-SAIDA
                  READ ARQFOR AT END MOVE 1 TO FIM-ARQ1
                  WRITE REG-SAIDA
              ELSE
                  IF SGMOVIM = 'I'
                      IF CDFORNF = CDFORNM
                          DISPLAY 'ERRO'
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                          READ ARQFOR AT END MOVE 1 TO FIM-ARQ1
                      ELSE
                          MOVE REG-MOVIMENTO TO REG-SAIDA
                          WRITE REG-SAIDA
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                      END-IF
                  END-IF

                  IF SGMOVIM = 'A'
                      IF CDFORNF = CDFORNM
                          MOVE REG-MOVIMENTO TO REG-SAIDA
                          WRITE REG-SAIDA
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                          READ ARQFOR AT END MOVE 1 TO FIM-ARQ1
                      ELSE
                          DISPLAY 'ERRO - REGISTRO NAO ATENDE REGRA'
                          DISPLAY 'DE MOVIMENTO'
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                      END-IF
                  END-IF

                   IF SGMOVIM = 'E'
                      IF CDFORNF = CDFORNM
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                          READ ARQFOR AT END MOVE 1 TO FIM-ARQ1
                      ELSE
                          DISPLAY 'ERRO'
                          READ ARQMOV AT END MOVE 1 TO FIM-ARQ
                      END-IF
                  END-IF

              END-IF
              IF FIM-ARQ=1
                  MOVE 999 TO CDFORNM
              END-IF

              IF FIM-ARQ1=1
                  MOVE 999 TO CDFORNF
              END-IF

              READ ARQFOR AT END MOVE 1 TO FIM-ARQ1.
       FINALIZA.
           CLOSE ARQMOV
                 ARQFOR
                 ARQSAI.
