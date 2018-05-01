let respuestas = [0,0,1,1,2,1,2,3,5,1,2,2,2,6]
print("Valores: \(respuestas)" )

func obtenerFrecuencias (respuestas: [Int]) -> [Int]
{
    var frecuencias: [Int] = [0,0,0,0,0,0,0,0,0,0]

    for r in respuestas
    {
        if r == 0 {
            frecuencias [0] += 1
        }
        else if r == 1 {
            frecuencias [1] += 1
        }
        else if r == 2 {
            frecuencias [2] += 1
        }
        else if r == 3 {
            frecuencias [3] += 1
        }
        else if r == 4 {
            frecuencias [4] += 1
        }
        else if r == 5 {
            frecuencias [5] += 1
        }
        else if r == 6 {
            frecuencias [6] += 1
        }
        else if r == 7 {
            frecuencias [7] += 1
        }
        else if r == 8 {
            frecuencias [8] += 1
        }
        else if r == 9 {
            frecuencias [9] += 1
        }
    }
    
    return frecuencias
}

let frec = obtenerFrecuencias (respuestas: respuestas)
print ("Frecuencias:  \(frec)")


func cadenaAsteriscos (numAsteriscos: Int) -> String
{
    var asteriscos: String = ""
    var maximo: Int = (2 * numAsteriscos)

    for i in 0..<maximo
    {
        asteriscos += "*"
    }

    return asteriscos
}

func imprimir (frecuencias: [Int], maxAsteriscos: Int)
{
    var aux: String
    for i in 0...9
    {
        aux = cadenaAsteriscos (numAsteriscos: frecuencias[i])
        print ("\(i): \(aux)")
    }
}

print ("\nHistograma")
print  ("-----------")
imprimir (frecuencias: frec, maxAsteriscos: 10)
