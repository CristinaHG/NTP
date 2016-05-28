/**
  * Clase para representar conjuntos no vacios
  *
  * @param raiz      elemento de la raiz
  * @param izquierda conjunto a la izquierda
  * @param derecha   conjunto a la derecha
  */
class ConjuntoTweetNoVacio(raiz: Tweet, izquierda: ConjuntoTweet,
                           derecha: ConjuntoTweet) extends ConjuntoTweet {

  // ------------------- A IMPLEMENTAR ----------------------------
  // quizas algunos de los metodos pedidos se dejen como abstractos y
  // haya que ofrecer las implementaciones aqui
  // -------------------------------------------------------------

  // METODOS YA IMPLEMENTADOS: no cambiar
  // -------------------------------------------------------------------------
  /**
    * Determina si el conjunto contiene un mensaje
    *
    * @param mensaje
    * @return
    */
  def contiene(mensaje: Tweet): Boolean =
  // Si el mensaje de texto es anterior en orden lexicografico,
  // entonces habra que buscar en la izquierda; en caso contrario,
  // en la derecha. Si no es ni menor ni mayor, sera igual y se devuelve
  // true
    if (mensaje.texto < raiz.texto) izquierda.contiene(mensaje)
    else if (raiz.texto < mensaje.texto) derecha.contiene(mensaje)
    else true

  /**
    * Metodo para incluir un nuevo mensaje
    *
    * @param mensaje
    * @return
    */
  def incluir(mensaje: Tweet): ConjuntoTweet = {
    // Igual idea que en el metodo anterior: si el mensaje tiene menor
    // orden lexicografico que la raiz, se inserta como raiz. En caso
    // contrario se inserta a la derecha. Si tiene igual orden se devuelve
    // el propio conjunto
    if (mensaje.texto < raiz.texto) new ConjuntoTweetNoVacio(raiz, izquierda.incluir(mensaje), derecha)
    else if (raiz.texto < mensaje.texto) new ConjuntoTweetNoVacio(raiz, izquierda, derecha.incluir(mensaje))
    else this
  }

  /**
    * Indica si el conjunto esta vacio; por definicion no lo estara
    *
    * @return
    */
  def estaVacio = false

  /**
    * Devuelve el primer mensaje (con menor orden)
    *
    * @return
    */
  def head = if (izquierda.estaVacio) raiz else izquierda.head

  /**
    * Devuelve los demas
    *
    * @return
    */
  def tail = if (izquierda.estaVacio) derecha else new ConjuntoTweetNoVacio(raiz, izquierda.tail, derecha)

  /**
    * Elimina mensaje del conjunto
    *
    * @param mensaje
    * @return
    */
  def eliminar(mensaje: Tweet): ConjuntoTweet =
    if (mensaje.texto < raiz.texto) new ConjuntoTweetNoVacio(raiz, izquierda.eliminar(mensaje), derecha)
    else if (raiz.texto < mensaje.texto) new ConjuntoTweetNoVacio(raiz, izquierda, derecha.eliminar(mensaje))
    else izquierda.union(derecha)
}
