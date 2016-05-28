
/**
  * Clase de utilidad para representar conjuntos de tweets con temas de
  * google y apple, junto con un objeto de la clase Tendencia con todos
  * ellos
  */
object TerminosGoogleApple {
  // Lista de terminos de interes para google
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  // Lista de terminos de interes para apple
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  // Conjuntos de tweets para ambas listas de terminos
  // ------------------------ A IMPLEMENTAR -------------------------
  val mensajesGoogle: ConjuntoTweet = LectorTweets.obtenerConjuntoConTerminos(google)
  val mensajesApple: ConjuntoTweet = LectorTweets.obtenerConjuntoConTerminos(apple)

  // Se genera la lista completa de mensajes de ambos temas
  val tendencia: Tendencia = mensajesApple.union(mensajesGoogle).ordenacionAscendentePorRetweet
}

/**
  * Clase para probar la funcionalidad
  */
object Main extends App {
  // ------------------------ A IMPLEMENTAR -------------------------
  // A obtener informacion sobre: 
  // 1. numero de mensajes en mensajesGoogle y mensajesApple
  val terminosGA=TerminosGoogleApple
  printf("MENSAJES Google:%s", terminosGA.mensajesGoogle.numeroMensajes)
  printf("MENSAJES Apple:%s", terminosGA.mensajesApple.numeroMensajes)
 // printf("MENSAJES Apple:%s", terminosGA.mensajesApple.foreach((t:Tweet)=>print(t)))
  //printf("terminos Google:%s", terminosGA.mensajesGoogle.numeroMensajes )
  printf("terminos Google:%s", TerminosGoogleApple.mensajesGoogle.numeroMensajes )
  //printf("%n terminos Apple:%s", terminosGA.mensajesApple.numeroMensajes)
  // 2. numero de mensajes en la tendencia
  //printf("%n número de mensajes en la tendencia:%s", terminosGA.tendencia.length)
  // 3. numero de mensajes comunes
 // val comunes=terminosGA.mensajesApple.interseccion(terminosGA.mensajesGoogle)
 // printf("%n número de mensajes comunes:%s%n",comunes.numeroMensajes)
  // 4. orden de influencia de los mensajes comunes
 // printf("%n orden de influencia de mensajes comunes:%s",comunes.ordenacionAscendentePorRetweet.foreach((t:Tweet)=>print(t)))
  // 5. maximo y minimo numero de retweets en los mensajes comunes
 // printf("%n minimo nº de RTs en mensajes comunes:%s",comunes.ordenacionAscendentePorRetweet.head.retweets)
 // printf("%n maximo nº de RTs en mensajes comunes:%s",comunes.buscarMaximo.retweets)
  // 6. maximo y minimo de retweets en toda la coleccion de tendencia
  //printf("%n minimo nº de RTs en toda la tendencia:%s",terminosGA.tendencia.head.retweets)
  //como la tendencia está ordenada, sabemos que el mas RT estará al final del conjunto
  //printf("%n maximo nº de RTs en toda la tendencia:%s",terminosGA.tendencia.getLast.retweets)
  //printf("%ntweets en tendencia:%s",terminosGA.tendencia.foreach((t:Tweet)=>print(t)))
}
