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

