
/**
  * Clase abstracta para representar conjuntos de tweets
  */

abstract class ConjuntoTweet {

  // ----------------------- A IMPLEMENTAR -----------------------
  // (haciendo uso del auxiliar)
  // -------------------------------------------------------------
  def filtrar(predicado: Tweet => Boolean): ConjuntoTweet = filtrar0(predicado, new ConjuntoTweetVacio)


  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def filtrar0(predicado: Tweet => Boolean, conjunto: ConjuntoTweet): ConjuntoTweet
  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------

  /*
  caso base:otro vacio=>devolver this
  inductivo:
        this no contiene mensaje cabeza de otro => nuevo<-incluir mensaje cabeza en this
        realizar union de nuevo con resto de mensajes de otro

        en otro caso: this contiene cabeza de otro

         union de this con resto de mensajes de otro
  */
  def union(otro: ConjuntoTweet): ConjuntoTweet = {
    if(otro.estaVacio) this
    else if (this.contiene(otro.head)==false) {
      val conjuntoNuevo = this.incluir(otro.head)
      conjuntoNuevo.union(otro.tail)
    }else
      union(otro.tail)
    //return conjuntoNuevo
  }

  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def interseccion(otro : ConjuntoTweet) : ConjuntoTweet

  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def ordenacionAscendentePorRetweet: Tendencia = ordenacionAscendentePorRetweet0(buscarMinimo,this.eliminar(buscarMinimo),new TendenciaVacia)


  def ordenacionAscendentePorRetweet0(minimo:Tweet,conjunto:ConjuntoTweet,tendencia: Tendencia):Tendencia={
    if(conjunto.estaVacio) tendencia
    else{
      val minimoretwiteado=conjunto.buscarMinimo
      ordenacionAscendentePorRetweet0(minimoretwiteado,conjunto.eliminar(minimoretwiteado),tendencia.+(minimoretwiteado))
    }
  }


  // ----------------------- A IMPLEMENTAR -----------------------
  // (o dejar como abstracto para implementar en clases derivadas)
  // -------------------------------------------------------------
  def numeroMensajes: Integer = if (this.estaVacio) 0 else 1+this.tail.numeroMensajes



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

  /**
    * Busca el Tweet con mayor nº de RTs del conjunto.
    * es la complementaria de la función buscarMinimo
    *
    * @return
    */

def buscarMaximo: Tweet =
// Se inicia la busqueda con el primer mensaje
  this.tail.buscarMaximo0(this.head)

/**
  * Funcion auxiliar para busqueda de mensaje con mayor numero
  * de retweets. Basada en la función buscarMinimo0 proporcionada
  *
  * @param maximoActual
  * @return
  */
private def buscarMaximo0(maximoActual: Tweet): Tweet =
// Si la lista esta vacia, se devuelve el maximo actual
  if (this.estaVacio) maximoActual
  // en caso contrario, se comprueba si el primer mensaje en
  // el conjunto es mayor que el maximo actual, se actualiza el
  // maximo y la busqueda prosigue con el
  else if (this.head.retweets > maximoActual.retweets) this.tail.buscarMaximo0(this.head)
  // en caso contrario sigue la busqueda sobre el resto de elementos
  else this.tail.buscarMaximo0(maximoActual)

}
