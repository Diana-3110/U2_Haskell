funcionPrincipal = do
    funcionOpciones
funcionOpciones = do
    putStrLn("-------MENU-------")
    putStrLn("1.- Serie Fibonacci")
    putStrLn("2.- Numeros del 1 al 10")
    putStrLn("3.- Factorial")
    putStrLn("4.- Desaparece numeros")
    putStrLn("5.- Palindromo")
    putStrLn("6.- Calculadora")
    putStrLn("7.- Salir")
    putStrLn("¿Que opciones deseas saber?")
    opcion <- getLine

    case opcion of
        "1" -> funcionFibo
        "2" -> funcionNum 
        "3" -> funcionFact
        "4" -> funcionNumeros [0,1,2,3,4,5,6,7,8,9,10]
        "5" -> funcionPalin
        "6" -> funcionCalcu
        "7" -> putStrLn("Salir")
        _   -> putStrLn("Opcion no disponible")


funcionFibo = do
        putStrLn("----Serie Fibonacci----")
        pedirfibo

pedirfibo = do
    putStrLn("Que posicion deseas")
    a <- getLine
    putStrLn("El resultado es:"++show(fibonacci (read a)))
    funcionPrincipal

fibonacci a = do
        if a < 2
            then do 
                a
        else do
            fibonacci (a-1) + fibonacci (a-2)


funcionNum = do
    putStrLn("----Numeros del 1 al 10----")
    b[]
b num = do
        if null num
            then do
                let termina =  take 10 (iterate(+1)1)
                b(termina)
                        
        else do
            print(num)
            funcionPrincipal



funcionFact = do
    putStrLn("----Factorial----")
    pedirfact

pedirfact = do
    putStrLn("Que factorial deseas")
    c <- getLine
    putStrLn("El resultado es:"++show (factorial(read c)))
    funcionPrincipal

factorial c = do
        if c==0
            then do 
                1
            else do
            c * factorial (c-1)




funcionNumeros d = do

    if null d
        then do
            putStrLn("Termina")
            funcionPrincipal
    else do
        print(d)
        let d2 = init d
        funcionNumeros(d2)


funcionPalin = do
    putStrLn("----Palindromo----")
    pedirfrase

pedirfrase = do
    putStrLn("Escribe tu frase para que sepas si es un palindromo")
    e <- getLine
    putStrLn("El resultado es:"++show (palindromo(e)))
    funcionPrincipal

palindromo e = do
    e ==reverse e



funcionCalcu = do
    putStrLn ("----Menu Calculadora----")
    putStrLn("1.- Suma")
    putStrLn("2.- Resta")
    putStrLn("3.- Multiplicación")
    putStrLn("4.- Division")
    putStrLn("5.- Salir")
    op <- getLine


    case op of
        "1" -> suma
        "2" -> resta
        "3" -> multiplicacion
        "4" -> division
        "5" -> funcionSalir
        _   -> putStrLn("Error Elige Otra Opcion")

suma = do
    putStrLn("----Suma----")
    putStrLn ("Ingresa el numero 1: ")
    f <- getLine
    putStrLn ("Ingresa el numero 2:")
    g <- getLine

    let fInt = (read f)
    let gInt = (read g)
    let resultado = fInt + gInt

    putStrLn("El resultado de la suma es: "++show resultado) 
    funcionPrincipal  

resta = do
    putStrLn("----Resta----")
    putStrLn ("Ingresa el numero 1: ")
    f <- getLine
    putStrLn ("Ingresa el numero 2:")
    g <- getLine

    let fInt = (read f)
    let gInt = (read g)
    let resultado = fInt - gInt


    putStrLn("El resultado de la resta es: "++show resultado)
    funcionPrincipal 
 

multiplicacion = do
    putStrLn("----Multiplicacion----")
    putStrLn ("Ingresa el numero 1: ")
    f <- getLine
    putStrLn ("Ingresa el numero 2:")
    g <- getLine

    let fInt = (read f)
    let gInt = (read g)
    let resultado = fInt * gInt


    putStrLn("El resultado de la multiplicacion es: "++show resultado)
    funcionPrincipal
    

division = do
    putStrLn("----Division----")
    putStrLn ("Ingresa el numero 1: ")
    f <- getLine
    putStrLn ("Ingresa el numero 2:")
    g <- getLine

    let fInt = (read f)
    let gInt = (read g)
    let resultado = div fInt  gInt


    putStrLn("El resultado de la division es: "++show resultado)
    funcionPrincipal

funcionSalir = do
    funcionPrincipal

