
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
  val mensajesGoogle: ConjuntoTweet = ??? 
  val mensajesApple: ConjuntoTweet = ???

  // Se genera la lista completa de mensajes de ambos temas
  val tendencia: Tendencia = ??? 
}

/**
  * Clase para probar la funcionalidad
  */
object Main extends App {
  // ------------------------ A IMPLEMENTAR -------------------------
  // A obtener informacion sobre: 
  // 1. numero de mensajes en mensajesGoogle y mensajesApple
  // 2. numero de mensajes en la tendencia
  // 3. numero de mensajes comunes
  // 4. orden de influencia de los mensajes comunes
  // 5. maximo y minimo numero de retweets en los mensajes comunes
  // 6. maximo y minimo de retweets en toda la coleccion de tendencia
}
