
/**
  * Lector de tweets
  */
object LectorTweets {

  /**
    * Objeto para realizar el parseo de los mensajes
    */
  object AnalizadorTweets {
    import scala.util.parsing.json._

    /**
      * Obtiene una lista del tipo indicado a partir del analisis de un
      * texto
 *
      * @param texto texto a analizar
      * @tparam T tipo asociado a la lista
      * @return
      */
    def obtenerLista[T](texto: String): List[T] =
      JSON.parseFull(texto).get.asInstanceOf[List[T]]

    def getMap(s: String): Map[String, Any] =
      JSON.parseFull(s).get.asInstanceOf[Map[String, Any]]

    def getTweets(user: String, json: String): List[Tweet] =
      for (map <- obtenerLista[Map[String, Any]](json)) yield {
        val text = map("text")
        val retweets = map("retweet_count")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }

    /**
      * Obtiene los mensajes de un determinado usuario a partir del analisis
      * de una cadena de texto con la informacion
 *
      * @param usuario
      * @param texto
      * @return
      */
    def getTweetData(usuario: String, texto: String): List[Tweet] = {
      // Se obtiene la lista de mensajes a partir del texto. Se indica
      // Any como tipo
      val lista = obtenerLista[Map[String, Any]](texto)

      // Se itera sobre la lista de elementos obtenidos del parseo
      for (elemento <- lista) yield {
        // Se recupera la entrada de elemento asociada al texto
        val textoElemento = elemento("text")

        // Se recupera el contador de retweets
        val retweets = elemento("retweets")

        // Se crea ub objeto de la clase Tweet
        new Tweet(usuario, textoElemento.toString, retweets.toString.toDouble.toInt)
      }
    }
  }

  /**
    * Se crea un conjunto de mensajes a partir de una lista
 *
    * @param lista
    * @return
    */
  def toTweetSet(lista: List[Tweet]): ConjuntoTweet = {
    //lista.foldLeft(new ConjuntoTweetVacio: ConjuntoTweet)(_.incluir(_))
    lista.foldLeft(new ConjuntoTweetVacio : ConjuntoTweet)((conjunto:ConjuntoTweet, tweet:Tweet) => conjunto.incluir(tweet))
  }

  /**
    * Genera una cadena a partir de los objetos de la clase Tweet
 *
    * @param mensajes
    * @return
    */
  def unparseToData(mensajes: List[Tweet]): String = {
    val buf = new StringBuffer
    for (mensaje <- mensajes) {
      val json = "{ \"user\": \"" + mensaje.usuario + "\", \"text\": \"" +
                                    mensaje.texto.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
                                    mensaje.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  /**
    * Descripcion de los usuarios considerados en el archivo de datos
    */
  val usuarios = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  /**
    * Se obtienen los mensajes para cada usuario del analisis de la cadena correspondiente
    */
  private val gizmodoTweets = LectorTweets.AnalizadorTweets.getTweetData("gizmodo", DatosMensajes.gizmodo)
  private val techCrunchTweets = LectorTweets.AnalizadorTweets.getTweetData("TechCrunch", DatosMensajes.TechCrunch)
  private val engadgetTweets = LectorTweets.AnalizadorTweets.getTweetData("engadget", DatosMensajes.engadget)
  private val amazondealsTweets = LectorTweets.AnalizadorTweets.getTweetData("amazondeals", DatosMensajes.amazondeals)
  private val cnetTweets = LectorTweets.AnalizadorTweets.getTweetData("CNET", DatosMensajes.CNET)
  private val gadgetlabTweets = LectorTweets.AnalizadorTweets.getTweetData("gadgetlab", DatosMensajes.gadgetlab)
  private val mashableTweets = LectorTweets.AnalizadorTweets.getTweetData("mashable", DatosMensajes.mashable)

  /**
    * Se genera una lista (de listas) con todos los mensajes disponibles, de todos los usuarios
    */
  private val fuentesTweets = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets,
                            cnetTweets, gadgetlabTweets, mashableTweets)

  /**
    * Se crea un diccionario asociando usuarios y lista de mensajes correspondientes
    */
  val diccionarioTweets: Map[String, List[Tweet]] =
    Map() ++ Seq((usuarios(0) -> gizmodoTweets),
      (usuarios(1) -> techCrunchTweets),
      (usuarios(2) -> engadgetTweets),
      (usuarios(3) -> amazondealsTweets),
      (usuarios(4) -> cnetTweets),
      (usuarios(5) -> gadgetlabTweets),
      (usuarios(6) -> mashableTweets))

  /**
    * Se crea una lista de conjuntos de tweets, uno para cada usuario
    */
  val conjuntosTweets: List[ConjuntoTweet] = fuentesTweets.map(lista => toTweetSet(lista))

  /**
    * Almacena un mapa con pares usuario - conjunto mensajes asociados
    */
  private val diccionarioUsurarioConjunto: Map[String, ConjuntoTweet] =
    Map() ++ (usuarios zip conjuntosTweets)

  /**
    * Metodo privado para unir todos los conjuntos de mensajes en uno solo
 *
    * @param listaConjuntos
    * @param conjuntoActual
    * @return
    */
  private def unionOfAllTweetSets(listaConjuntos: List[ConjuntoTweet], conjuntoActual: ConjuntoTweet): ConjuntoTweet =
    // Si la lista esta vacia se devuelve el conjunto actual
    if (listaConjuntos.isEmpty) conjuntoActual
    // En caso contrario, se llama recursivamente al metodo sobre el resto de la lista y
    // produciendo la union del conjunto actual con el de la cabeza de la lista
    else unionOfAllTweetSets(listaConjuntos.tail, conjuntoActual.union(listaConjuntos.head))


  /**
    * Se almacenan aqui todos los mensajes, en forma de conjunto
    */
  val mensajes: ConjuntoTweet = unionOfAllTweetSets(conjuntosTweets, new ConjuntoTweetVacio)

  // ------------------- A IMPLEMENTAR ----------------------------------
  def obtenerConjuntoConTerminos(terminos : List[String]) : ConjuntoTweet = ???
}
