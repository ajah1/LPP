/***************************************
  EJERCICIO 1
****************************************/

/*
//a.1) ==> [3,6,9] ==> 3
let nums = [1,2,3,4,5,6,7,8,9,10]
print (nums.filter{$0 % 3 == 0}.count)

//a.2) ==> 215
let nums2 = [1,2,3,4,5,6,7,8,9,10]
print (nums2.map{$0+100}.filter{$0 % 5 == 0}.reduce(0,+))

//a.3) ==> [2, 2, 2, 2, 5, 6]
let cadenas = ["En", "un", "lugar", "de", "La", "Mancha"]
print (cadenas.sorted{$0.count < $1.count}.map{$0.count})

//a.4) ==> lo mismo que a.3 pero con tuplas (string, count)
let cadenas2 = ["En", "un", "lugar", "de", "La", "Mancha"]
print (cadenas2.reduce([]) {(res: [(String, Int)], c: String) -> [(String, Int)] in
res + [(c, c.count)]}.sorted(by: {$0.1 < $1.1}))


//b.1)
func f(nums: [Int], n: Int) -> Int {
    return nums.filter{$0 == n}.count
}
let b1: [Int  ] = [1, 2, 3, 2, 5, 6, 7, 9, 3]
print (f (nums: b1, n: 3))


//b.2)
func g(nums: [Int]) -> [Int] {
    return nums.reduce([], {(res: [Int], n: Int) -> [Int] in
                        if !res.contains(n) {
                            return res + [n]
                        } else {
                            return res
                        }
                    })
}
let b2:  [Int] = [1, 1, 1, 4]
print (g (nums: b2))

//b.3)
func h(nums: [Int], n: Int) -> ([Int], [Int]) {
   return nums.reduce(([],[]), {(res: ([Int],[Int]), num: Int ) -> ([Int],[Int]) in
                            if (num >= n) {
                                return (res.0, res.1 + [num])
                            } else {
                                return ((res.0 + [num], res.1))
                            }
                        })
}
let b3: [Int] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print (h (nums: b3, n: 5))


//c.1) Función suma(palabras:contienen:):
func suma(palabras: [String], contiene: Character) -> Int
{
    let sumar: [Int] = palabras.reduce([], {(res: [Int], s: String) -> [Int] in
    if (s.contains(contiene)) {
        return (res + [s.count])
    }
    else {
        return res
    }})

    return sumar.reduce(0,+)
}

let c1: [String] = ["hola", "queeeee", "tal"]
print (suma (palabras: c1, contiene: "a"))


//c.2) Función sumaMenoresMayores(nums:pivote:):
func sumaMenoresMayores(nums: [Int], pivote: Int) -> (Int, Int)
{
  return nums.reduce ((0,0),
      {(res: (Int,Int), numero: Int) -> (Int,Int)  in
          if numero > pivote {
            return (res.0, (res.1 + numero))
          }
          else if numero < pivote {
            return ((res.0 + numero), res.1)
          }
          else {
            return res
          }
      })
}

let c2: [Int] = [1, 2, 3, 4, 5]
print (sumaMenoresMayores (nums: c2, pivote: 3))
*/

/***************************************
  EJERCICIO 2
****************************************/
indirect enum Arbol<T> {
      case nodo (T, [Arbol<T>])
}

let arbolInt: Arbol = .nodo(53, [.nodo(13, []),
                                 .nodo(32, []),
                                 .nodo(41, [.nodo(36, []), .nodo(39, [])])
                               ])
 let arbolString: Arbol = .nodo("Zamora",
  [.nodo("Buendía",
  [.nodo("Albeza", []), .nodo("Berenguer", []),
  .nodo("Bolardo", [])]),
  .nodo("Galván", [])])


  /* Versión recursión mutua */
  func toArray<T>(arbol: Arbol<T>) -> [T]
  {
      switch arbol
      {
          case let .nodo(dato, hijos):
              return [dato] + toArray(bosque: hijos)
      }
  }

  func toArray<T>(bosque: [Arbol<T>]) -> [T]
  {
      if let primero = bosque.first
      {
          let resto = Array(bosque.dropFirst())
          return toArray(arbol: primero) + toArray(bosque: resto)
      }
      else
      {
          return []
      }
  }
  print ("arbolString to array: \(toArray (arbol: arbolString))")


func toArrayFos<T> (arbol: Arbol<T>) -> [T] {
  switch arbol
  {
  case let .nodo (dato, hijos):
    return hijos.map(toArrayFos).reduce([dato],+)
  }
}
print ("arbolInt to array: \(toArrayFos (arbol: arbolInt))")



/***************************************
  EJERCICIO 3
****************************************/
let listaAlumnos = [("Pepe", 8.45, 3.75, 6.05, 1),
                    ("Maria", 9.1, 7.5, 8.18, 1),
                    ("Jose", 8.0, 6.65, 7.96, 1),
                    ("Carmen", 6.25, 1.2, 5.41, 2),
                    ("Felipe", 5.65, 0.25, 3.16, 3),
                    ("Carla", 6.25, 1.25, 4.23, 2),
                    ("Luis", 6.75, 0.25, 4.63, 2),
                    ("Loli", 3.0, 1.25, 2.19, 3)]

import Foundation

func imprimirListadoAlumnos(_ alumnos: [(String, Double, Double, Double, Int)]) {
    print("Alumno   Parcial1   Parcial2   Cuest  Años")
    for alu in alumnos {
        alu.0.withCString {
            print(String(format:"%-10s %5.2f      %5.2f    %5.2f  %3d", $0, alu.1,alu.2,alu.3,alu.4))
        }
    }
}

func imprimirListadosNotas  (alumnos: [(String, Double, Double, Double, Int)])
{
  print ("LISTADO ORIGINAL")
  imprimirListadoAlumnos (alumnos)

  print ("\n** DECR PARCIAL1")
  imprimirListadoAlumnos (alumnos.sorted{$0.1 < $1.1})

  print ("\n** CREC PARCIAL2")
  imprimirListadoAlumnos (alumnos.sorted{$1.2 < $0.2})

  print ("\n** DECR AÑOS Y CUEST ")
  imprimirListadoAlumnos (alumnos.sorted{ ($0.4 < $1.4) && ($0.3 < $1.3)})

  print ("\n** DECR NOTA PARCIAL3 PARA APROBAR")
  imprimirListadoAlumnos (alumnos.sorted{ (15 - $0.1 + $0.2) > (15 - $1.1 + $1.2)})
}

imprimirListadosNotas (alumnos: listaAlumnos)
