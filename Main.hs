-- Leonardo Silva de Abreu

{-
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. 
-}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1..n], mod n x == 0]

{-
2. Usando  List Comprehension  escreva  uma  função, chamada contaCaractere, que  conte  a ocorrência de um caractere específico, em uma string dada. 
-}
contaCaractere :: [Char] -> Char -> Int
contaCaractere [] c = 0
contaCaractere x c = length [ n | n <- x, n == c]

{-
3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. 
-}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo [] = []
dobroNaoNegativo x = [ n*2 | n <- x, n >= 0]

{-
4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. 
-}
pitagoras :: Int -> [(Int,Int,Int)]
pitagoras n = [ (h,c,a) | let l = [1..n], h <- l, c <- l, a <- l, h*h == (c*c + a*a) ]


{-
5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. 
-}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [ x | x <- [1..n], let l = (divisoresden (x)), sum (take ((length l)-1) (l)) == x]

{-
6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis. 
-}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar x y = sum [ (fst a) * (snd a) | a  <- zip x y]

{-
7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2. 
-}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [x | x <- [1..], divisoresden x == [1,x]]

{-
8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes.
-}
paresOrdenados :: Int -> [(Int,Int)]
paresOrdenados n = [ (x^2, x^3) | x <- [1..n] ]

main = do
  putStrLn "Trabalho 6 - List Comprehension"

  --divisoresden teste
  let divisoresdenInput = 57
  let divisoresdenResultado = divisoresden divisoresdenInput
  putStrLn ("Func. divisoresden: entrada:" ++ show divisoresdenInput ++ "; resultado:" ++ show divisoresdenResultado)

  let divisoresdenInput = 44
  let divisoresdenResultado = divisoresden divisoresdenInput
  putStrLn ("Func. divisoresden: entrada:" ++ show divisoresdenInput ++ "; resultado:" ++ show divisoresdenResultado)

  --contaCaractere teste
  let contaCaractereInput = "Hello World"
  let contaCaractereInput2 = 'l'
  let contaCaractereResultado = contaCaractere contaCaractereInput contaCaractereInput2
  putStrLn ("Func. contaCaractere: entrada:" ++ show contaCaractereInput ++ ", " ++ show contaCaractereInput2 ++ "; resultado:" ++ show contaCaractereResultado)

  let contaCaractereInput = "Hello World"
  let contaCaractereInput2 = 'f'
  let contaCaractereResultado = contaCaractere contaCaractereInput contaCaractereInput2
  putStrLn ("Func. contaCaractere: entrada:" ++ show contaCaractereInput ++ ", " ++ show contaCaractereInput2 ++ "; resultado:" ++ show contaCaractereResultado)

  --dobroNaoNegativo teste
  let dobroNaoNegativoInput = [-3,2,5,6,-4,-1]
  let dobroNaoNegativoResultado = dobroNaoNegativo dobroNaoNegativoInput
  putStrLn ("Func. dobroNaoNegativo: entrada:" ++ show dobroNaoNegativoInput ++ "; resultado:" ++ show dobroNaoNegativoResultado)

  let dobroNaoNegativoInput = [-5,-3,-4]
  let dobroNaoNegativoResultado = dobroNaoNegativo dobroNaoNegativoInput
  putStrLn ("Func. dobroNaoNegativo: entrada:" ++ show dobroNaoNegativoInput ++ "; resultado:" ++ show dobroNaoNegativoResultado)


  --pitagoras teste
  let pitagorasInput = 12
  let pitagorasResultado = pitagoras pitagorasInput
  putStrLn ("Func. pitagoras: entrada:" ++ show pitagorasInput ++ "; resultado:" ++ show pitagorasResultado)

  --numerosPerfeitos teste
  let numerosPerfeitosInput = 500
  let numerosPerfeitosResultado = numerosPerfeitos numerosPerfeitosInput
  putStrLn ("Func. numerosPerfeitos: entrada:" ++ show numerosPerfeitosInput ++ "; resultado:" ++ show numerosPerfeitosResultado)

  --produtoEscalar teste
  let produtoEscalarInput = [3,4]
  let produtoEscalarInput2 = [-2,5]
  let produtoEscalarResultado = produtoEscalar produtoEscalarInput produtoEscalarInput2
  putStrLn ("Func. produtoEscalar: entrada:" ++ show produtoEscalarInput ++ ", " ++ show produtoEscalarInput2 ++ "; resultado:" ++ show produtoEscalarResultado)

  let produtoEscalarInput = [2,3,4]
  let produtoEscalarInput2 = [4,5,6]
  let produtoEscalarResultado = produtoEscalar produtoEscalarInput produtoEscalarInput2
  putStrLn ("Func. produtoEscalar: entrada:" ++ show produtoEscalarInput ++ ", " ++ show produtoEscalarInput2 ++ "; resultado:" ++ show produtoEscalarResultado)

  --primeirosPrimos teste
  let primeirosPrimosInput = 15
  let primeirosPrimosResultado = primeirosPrimos primeirosPrimosInput
  putStrLn ("Func. primeirosPrimos: entrada:" ++ show primeirosPrimosInput ++ "; resultado:" ++ show primeirosPrimosResultado)

  --paresOrdenados teste
  let paresOrdenadosInput = 10
  let paresOrdenadosResultado = paresOrdenados paresOrdenadosInput
  putStrLn ("Func. paresOrdenados: entrada:" ++ show paresOrdenadosInput ++ "; resultado:" ++ show paresOrdenadosResultado)




