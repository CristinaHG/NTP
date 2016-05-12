
/**
  * Clase para representar tweets
  *
  * @param usuario
  * @param texto
  * @param retweets
  */
class Tweet(val usuario: String, val texto: String, val retweets: Int) {

  /**
    * Sebrescritura del metodo toString
    * @return
    */
  override def toString: String =
    "User: " + usuario + "\n" +
      "Text: " + texto + " [" + retweets + "]"
}
