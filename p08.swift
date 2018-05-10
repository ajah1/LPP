print ("\n\n=========== EJERCICIO 1 ===========\n")

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

/*
func cadenaAsteriscos (numAsteriscos: Int) -> String
{
    var asteriscos: String = ""
    let maximo: Int = (2 * numAsteriscos)

    for _ in 0..<maximo
    {
        asteriscos += "*"
    }

    return asteriscos
}
*/

func imprimir (frecuencias: [Int], maxAsteriscos: Int)
{
    var aux: String
    for i in 0...9
    {
        //aux = cadenaAsteriscos (numAsteriscos: frecuencias[i])
        aux = String(repeating: "*", count: frecuencias[i])
        print ("\(i): \(aux)")
    }
}

print ("\nHistograma")
print  ("-----------")
imprimir (frecuencias: frec, maxAsteriscos: 10)




print ("\n\n=========== EJERCICIO 2 ===========\n")

func cuadrado (x: Int) -> Int
{
    return x * x
}


func compruebaParejas (array: [Int], funcion: (Int)->Int) -> [(Int,Int)]
{
    var aux_array = [0]

    // caso base: array con un elemento
    if array.count == 1
    {
        return []
    }

    else if array[1] == funcion (array[0])
    {
        aux_array = Array(array.dropFirst())

        return [(array[0],array[1])]
            + compruebaParejas (array: aux_array, funcion: funcion)
    }

    else
    {
        aux_array = Array(array.dropFirst())

        return compruebaParejas (array: aux_array, funcion: funcion)
    }
}

var entrada = [2, 4, 16, 5, 10, 100, 105]
print ("Para el array: \(entrada)")
print ("compruebaParejas: \(compruebaParejas(array: entrada, funcion: cuadrado))")


print ("\n\n=========== EJERCICIO 3 ===========\n")

indirect enum ArbolBinario
{
  case nodo(Int, ArbolBinario, ArbolBinario)
  case vacio
}

let arboliz: ArbolBinario = .nodo(2, .vacio, .vacio)
let arbolder: ArbolBinario = .nodo(12, .vacio, .vacio)
let arbol: ArbolBinario = .nodo(8, arboliz, arbolder)

func suma (arbol: ArbolBinario) -> Int
{
  switch arbol
  {
  case .vacio:
    return 0

  case let .nodo(valor, .vacio, .vacio):
    return valor

  case let .nodo(valor, iz, de):
    return valor + suma(arbol: iz) + suma(arbol: de)
  }
}

print ("Suma del arbol: \(suma (arbol: arbol))")



print ("\n\n=========== EJERCICIO 4 ===========\n")

let array_ej4 = [(2,4), (4,14), (4,16), (5,25), (10,100)]

func coinciden(parejas: [(Int,Int)], funcion: (Int)->Int) -> [Bool]
{
  var resultados = Array(repeating: false, count: parejas.count)

  for i in 0..<parejas.count
  {
    if cuadrado(x: parejas[i].0) == parejas[i].1
    {
      resultados[i] = true
    }
  }

  return resultados
}

print ("Para el array: \(array_ej4)")
print ("coinciden: \(coinciden(parejas: array_ej4, funcion: cuadrado))")



print ("\n\n=========== EJERCICIO 5 ===========\n")

enum Movimiento
{
case deposito (Double)
case cargoRecibo (String,Double)
case cajero (Double)
}

let movimientos: [Movimiento] = [
  .deposito(830.0),
  .cargoRecibo("Gimnasio", 45.0),
  .deposito(400.0),
  .cajero(100.0),
  .cargoRecibo("Fnac", 38.70)]


func aplica (movimientos:[Movimiento]) -> (Double, [String])
{
  var aplicados: (Double, [String]) = (0.0, [])

  for movimiento in movimientos
  {
      switch movimiento
      {
      case let .deposito(valor):
        aplicados.0 += valor

      case let .cargoRecibo(lugar, valor):
        aplicados.0 -= valor
        aplicados.1 += [lugar]

     case let .cajero(valor):
          aplicados.0 -= valor
      }
  }

  return aplicados
}

print ("Para el array: \(movimientos) \n")

print ("aplicados \(aplica(movimientos: movimientos))")



print ("\n\n=========== EJERCICIO 6 ===========\n")
