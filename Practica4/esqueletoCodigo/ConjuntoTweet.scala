
/**
  * Clase abstracta para representar conjuntos de tweets
  */
abstract class ConjuntoTweet {

  // ----------------------- A IMPLEMENTAR -----------------------
  // (haciendo uso del auxiliar)
  // -------------------------------------------------------------
  def filtrar(predicado: Tweet => Boolean): ConjuntoTweet = ???


  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def filtrar0(predicado: Tweet => Boolean, conjunto: ConjuntoTweet): ConjuntoTweet = ???

  
  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def union(otro: ConjuntoTweet): ConjuntoTweet = ???

  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def interseccion(otro : ConjuntoTweet) : ConjuntoTweet = ???

  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def ordenacionAscendentePorRetweet: Tendencia = ??? 


  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def numeroMensajes: Integer = ???

  // METODOS YA IMPLEMENTADOS QUE NO ES NECESARIO CAMBIAR (desde aqui al final
  // de la descripcion de la clase)
  // -------------------------------------------------------------------------
  /**
    * YA IMPLEMENTADO: metodo para incluir un nuevo mensaje en el
    * conjunto; como resultado se produce un nuevo conjunto. Este
    * metodo es abstracto, pero estara implementado en las subclases
    *
    * @param x
    * @return
    */
  def incluir(x: Tweet): ConjuntoTweet

  /**
    * Determina si el conjunto contiene un determinado mensaje
    *
    * @param mensaje
    * @return
    */
  def contiene(mensaje: Tweet): Boolean

  /**
    * Determina si el conjunto esta vacio
    *
    * @return
    */
  def estaVacio: Boolean

  /**
    * Devuelve el mensaje inicial del conjunto
    *
    * @return
    */
  def head: Tweet

  /**
    * Devuelve el resto de mensajes
    *
    * @return
    */
  def tail: ConjuntoTweet

  /**
    * Elimina un mensaje del conjunto
    *
    * @param mensaje
    * @return
    */
  def eliminar(mensaje: Tweet): ConjuntoTweet

  /**
    * Aplica una funcion a todos los mensajes de la coleccion
    *
    * @param funcion
    */
  def foreach(funcion: Tweet => Unit): Unit = {
    // Si el conjunto esta vacio, nada que hacer. En caso
    // contrario
    if (!this.estaVacio) {
      // Se aplica sobre el primer elemento
      funcion(this.head)

      // Se itera sobre el resto de elementos
      this.tail.foreach(funcion)
    }
  }

  /**
    * Busca el mensaje con el menor numero de retweets. Se basa en el uso
    * de la funcion auxuliar buscarMinimo0, que recibe como argumento el
    * minimo actual
    *
    * @return
    */
  def buscarMinimo: Tweet =
  // Se inicia la busqueda con el primer mensaje
    this.tail.buscarMinimo0(this.head)

  /**
    * Funcion auxiliar para busqueda de mensaje con menor numero
    * de retweets
    *
    * @param minimoActual
    * @return
    */
  private def buscarMinimo0(minimoActual: Tweet): Tweet =
    // Si la lista esta vacia, se devuelve el minimo actual
    if (this.estaVacio) minimoActual
    // en caso contrario, se comprueba si el primer mensaje en
    // el conjunto es menor que el minimo actual, se actualiza el
    // minimo y la busqueda prosigue con el
    else if (this.head.retweets < minimoActual.retweets) this.tail.buscarMinimo0(this.head)
    // en caso contrario sigue la busqueda sobre el resto de elementos
    else this.tail.buscarMinimo0(minimoActual)
}

