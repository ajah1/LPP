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
*/

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



indirect enum Arbol<T> {
case nodo (T, [Arbol<T>])
}

let arbolInt: Arbol = .nodo(53, [.nodo(13, []),
                                 .nodo(32, []),
                                 .nodo(41, [.nodo(36, []), .nodo(39, [])])
                               ])
 let arbolString: Arbol = .nodo("Zamora", [.nodo("Buendía", [.nodo("Albeza", []), .nodo("Berenguer", []), .nodo("Bolardo", [])]),
                                   .nodo("Galván", [])])


func toArray (arbol: Arbol<Int>)
{
}

func toArrayBosque (bosque: [Arbol<Int>])
{
  if bosque.count == 0 {
    return []
  }
  else {
    var car: Bosque<Int> = bosque[0]
    var cdr: Bosque<Int> = car.dropfirst
    return toArray (arbol: car) + toArrayBosque (bosque: cdr)
  }
}
