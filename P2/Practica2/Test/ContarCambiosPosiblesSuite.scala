

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ContarCambiosPosiblesSuite extends FunSuite {
  import Main.contarCambiosPosibles

  // Prueba 1
  test("contar cambios posibles: cambio de 4 con monedas de 1 y 2") {
    assert(contarCambiosPosibles(4,List(1,2)) === 3)
  }

  // Prueba 2
  test("contar cambios posibles: cambio de 300") {
    assert(contarCambiosPosibles(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  // Prueba 3
  test("contar cambios posibles: cambio de 301") {
    assert(contarCambiosPosibles(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("contar cambios posibles: cambio de 300 (cambiando de orden las monedas)") {
    assert(contarCambiosPosibles(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
