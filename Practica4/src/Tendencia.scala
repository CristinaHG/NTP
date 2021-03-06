/**
  * Clase para proporcionar una secuencia lineal de mensajes
  */
abstract class Tendencia {
  /**
    * Sobrecarga del operador +
    *
    * @param tw
    * @return
    */
  def +(tw: Tweet): Tendencia

  /**
    * Devuelve el primer mensaje de la coleccion
    *
    * @return
    */
  def head: Tweet

  /**
    * Devuelve el ultimo mensaje de la coleccion
    *
    * @return
    */
  def tail: Tendencia

  /**
    * Indica si la coleccion esta vacia
    *
    * @return
    */
  def isEmpty: Boolean

  /**
    * Iteracion sobre los elementos para aplicar una funcion
    * a todos ellos
    *
    * @param funcion
    */
  def foreach(funcion: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      funcion(this.head)
      this.tail.foreach(funcion)
    }
  }

  // -------------------------- A IMPLEMENTAR --------------------
  // (o dejar como abstracto)
  // -------------------------------------------------------------
  /**
    *Longitud de la tendencia
    * @return
    */
  def length : Integer = if (this.isEmpty) 0 else 1+this.tail.length
  /**
    *devuelve el último tweet de la tendencia.
    * Método abstracto mplementado en las clases derivadas
    * @return
    */
  def getLast:Tweet
}
