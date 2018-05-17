/*
struct Valor {
    var valor: Int = 0 {
        willSet {
            Valor.z += newValue
        }
        didSet {
            if valor > 10 {
                valor = 10
            }
        }
    }
    static var z = 0
}

var c1 = Valor()
var c2 = Valor()
c1.valor = 20
c2.valor = 8
print (c1.valor)
print (c2.valor)
print (Valor.z)
print (c1.valor + c2.valor + Valor.z)
*/
/*
func prueba(x: Int) -> Int? {
    if x < 100 {
        return nil
    } else {
        return x
    }
}

class A {
    var a = prueba(x: 100)
    var b, c: Int
    init(b: Int) {
        self.c = 0;
        self.b = c
    }
}

let a = A(b: 0)
let b: Int? = a.a
//print("El valor de a.a es: \(b!)")
*/


class A
{
  var observaCosas: Int = 3000;
  static var almacenadaEstatica = -23;

  var almacenada: Int = 0 {
    willSet (nuevoValor) {
      print ("nuevo valor a la proìedad almacenada: \(nuevoValor)")
    }
    didSet {
      print ("basta")
    }
  }

  var calculada: Int {
    get { return almacenada + 3 }
    set { almacenada += 3 }
  }

   static func MetodoMuyEstatico () {
    print ("Esto es un metodo muy estatico")
  }

  func MetodoASobreecribir () {
    print ("meotodo de padre")
  }
}

class B: A
{
  func Almacenada () {
    print (almacenada)
  }

  func MetodoMuyEstatico () {
   print ("Chu CHU")
 }

 override func MetodoASobreecribir () {
 print ("metodo sobreescrito")
 }
}



var b : B = B()
//b.MetodoMuyEstatico()
//print (b.almacenadaEstatica)
b as! A


/*
¿Se puede sobreescribir el valor de una propiedad almacenada? ¿Y calculada?
  - No se puede sobreescribir una variable almacenada
  - No se puede sobreescribir una variable calculada

¿Se puede añadir un observador a una propiedad de la clase base en una clase derivada?
  - No se puede, sería como sobreescribir la propiedad

¿Hereda la clase derivada propiedades y métodos estáticos de la clase base?
  - Si se heredan

¿Cómo se puede llamar a la implementación de un método de la clase base en una
sobreescritura de ese mismo método en la clase derivada
  -

*/








/*















*/
