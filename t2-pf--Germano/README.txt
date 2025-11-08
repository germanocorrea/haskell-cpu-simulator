T2 de Programação Funcional
Germano Bruscato Corrêa
===============================================

Para rodar:
runhaskell computador-t2.hs <nome-do-arquivo>

Exemplo:
runhaskell computador-t2.hs programa1.txt

O arquivo de input é uma representação do estado inicial da memória, por ex:
0 2
1 240
2 14
3 241
4 4
// exemplo de comentario
5 251
6 20
7 18 // exemplo de comentario
240 10
241 5
251 0

Qualquer informação a partir de // não é considerado, pois é um comentário

Após realizar a computação, o programa faz um print do estado final dos endereços de memória relativos a memória de vídeo (de 251 a 255).

Cada arquivo com a memória inicial possui a versão em assembly como comentário no próprio arquivo, e aqui repito os mesmos códigos:

-------------------------------

Programa 1: Resp = A + B - 2
Este programa pré carrega A = 10 e B = 5

0  LOD 240  // ACC = A
2  ADD 241  // ACC = ACC + B
4  SUB 245  // ACC = ACC - 2 (Constante '2' em 245)
6  STO 251  // Resp = ACC
8  HCF NOP  // Parar (18 = NOP, usado como filler)

0 2   // LOD
1 240 // <end> 240
2 14  // ADD
3 241 // <end> 241
4 16  // SUB
5 245 // <end> 245
6 4   // STO
7 251 // <end> 251
8 20  // HCF
9 18  // NOP
240 10  // A = 10 (Exemplo)
241 5   // B = 5 (Exemplo)
245 2   // Constante 2
251 0   // Resp (inicializado com 0)

-------------------------------

Programa 2: Resp = A * B
Este programa pré carrega A = 3 e B = 4

0  LOD 245  // ACC = 0
2  STO 251  // Resp = 0 (Inicializa Resp)
4  LOD 241  // loop: ACC = B
6  JMZ 22   // Se ACC == 0 (B == 0), pule para HCF (addr 22)
8  LOD 251  // ACC = Resp
10 ADD 240  // ACC = Resp + A
12 STO 251  // Resp = ACC
14 LOD 241  // ACC = B
16 SUB 246  // ACC = B - 1
18 STO 241  // B = ACC (Decrementa B)
20 JMP 4    // Pula para loop (addr 4)
22 HCF NOP  // HALT

0 2   // LOD
1 245 // <end> 245 (Const 0)
2 4   // STO
3 251 // <end> 251 (Resp)
4 2   // LOD (Início Loop)
5 241 // <end> 241 (B)
6 8   // JMZ
7 22  // <end> 22 (Endereço HCF)
8 2   // LOD
9 251 // <end> 251 (Resp)
10 14  // ADD
11 240 // <end> 240 (A)
12 4   // STO
13 251 // <end> 251 (Resp)
14 2   // LOD
15 241 // <end> 241 (B)
16 16  // SUB
17 246 // <end> 246 (Const 1)
18 4   // STO
19 241 // <end> 241 (B)
20 6   // JMP
21 4   // <end> 4 (Início Loop)
22 20  // HCF
23 18  // <end> NOP (filler)
240 3   // A = 3 (Exemplo)
241 4   // B = 4 (Exemplo)
245 0   // Constante 0
246 1   // Constante 1
251 0   // Resp (inicializado com 0)

-------------------------------

Programa 3: A=0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }
A e Resp são iniciados vazios pois o próprio programa os preenche

0  LOD 245  // ACC = 0
2  STO 240  // A = 0
4  LOD 246  // ACC = 1
6  STO 251  // Resp = 1
8  LOD 240  // Loop: ACC = A
10 CPE 247  // Compara ACC (A) com 5. Se A==5, ACC=0. Se A!=5, ACC=1.
12 JMZ 28   // Se ACC == 0 (A == 5), pule para HCF (addr 28)
14 LOD 240  // ACC = A
16 ADD 246  // ACC = A + 1
18 STO 240  // A = ACC
20 LOD 251  // ACC = Resp
22 ADD 248  // ACC = Resp + 2
24 STO 251  // Resp = ACC
26 JMP 8    // Pula para Loop (addr 8)
28 HCF NOP  // HALT
0 2   // LOD
1 245 // <end> 245 (Const 0)
2 4   // STO
3 240 // <end> 240 (A)
4 2   // LOD
5 246 // <end> 246 (Const 1)
6 4   // STO
7 251 // <end> 251 (Resp)
8 2   // LOD (Início Loop)
9 240 // <end> 240 (A)
10 10  // CPE
11 247 // <end> 247 (Const 5)
12 8   // JMZ
13 28  // <end> 28 (Endereço HCF)
14 2   // LOD
15 240 // <end> 240 (A)
16 14  // ADD
17 246 // <end> 246 (Const 1)
18 4   // STO
19 240 // <end> 240 (A)
20 2   // LOD
21 251 // <end> 251 (Resp)
22 14  // ADD
23 248 // <end> 248 (Const 2)
24 4   // STO
25 251 // <end> 251 (Resp)
26 6   // JMP
27 8   // <end> 8 (Início Loop)
28 20  // HCF
29 18  // <end> NOP (filler)
240 0   // A, não inicializamos pela memória, o programa o inicia
245 0   // Constante 0
246 1   // Constante 1
247 5   // Constante 5
248 2   // Constante 2
251 0   // Resp, não inicializamos pela memória, o programa o inicia
