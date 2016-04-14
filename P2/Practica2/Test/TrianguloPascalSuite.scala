/**
  * Created by cris on 14/04/16.
  */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TrianguloPascalSuite extends FunSuite {


  // Se importa la clase donde estan definidas las funciones a probar
  import Main.calcularValorTrianguloPascal

  // Prueba 1: se calcula el valor de la columna 0 y fila 2
  test("trianguloPascal: columna=0, fila=2") {
    assert(calcularValorTrianguloPascal(0,2) === 1)
  }

  // Prueba 2: calculo del valor de columna 1 y fila 2
  test("trianguloPascal: columna=1, fila=2") {
    assert(calcularValorTrianguloPascal(1,2) === 2)
  }

  // Prueba 3: calculo de valor de columna 1, fila 3
  test("trianguloPascal: columna=1, fila=3") {
    assert(calcularValorTrianguloPascal(1,3) === 3)
  }

  // Prueba 4: calculo de valor de columna 5, fila 10
  test("trianguloPascal: columna=5, fila=10") {
    assert(calcularValorTrianguloPascal(5,10) === 252)
  }

  // Prueba 5: calculo del valor de columna 10 y fila 15
  test("trianguloPascal: columna=10, fila=15") {
    assert(calcularValorTrianguloPascal(10,15) === 3003)
  }

  // Prueba 5: calculo del valor de columna 0 y fila 0
  test("trianguloPascal: columna=0, fila=0") {
    assert(calcularValorTrianguloPascal(0,0) === 1)
  }
}