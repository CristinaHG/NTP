
/**
  * Practica de conjuntos funcionales
  */
object ConjuntoFuncional {
  /**
    * Un conjunto funcional se representa mediante una funcion
    * caracteristica, un predicado. De esta forma, se declara
    * el tipo conjunto como un predicado que recibe un entero
    * (elemento) como argumento y dvuelve un valor booleano
    * que indica si pertenece o no al conjunto
    */
  type Conjunto = Int => Boolean

  /**
    * Metodo para determinar si un elemento pertenece al conjunto
    * @param conjunto
    * @param elemento
    * @return
    */
  def contiene(conjunto: Conjunto, elemento: Int): Boolean = conjunto(elemento)

  /**
    * Devuelve un conjunto asociado al elemento pasado como
    * argumento
    * @param elemento
    * @return
    *         
    */
  def conjuntoUnElemento(elemento: Int): Conjunto = 

  /**
    * Union de dos conjuntos
    * @param conjunto1
    * @param conjunto2
    * @return
    */
  def union(conjunto1: Conjunto, conjunto2: Conjunto): Conjunto = 

  /**
    * Interseccion de dos conjuntos
    * @param conjunto1
    * @param conjunto2
    * @return
    */
  def interseccion(conjunto1: Conjunto, conjunto2: Conjunto): Conjunto = 

  /**
    * Diferencia entre dos conjuntos
    * @param conjunto1
    * @param conjunto2
    * @return
    */
  def diferencia(conjunto1: Conjunto, conjunto2: Conjunto): Conjunto = 

  /**
    * Filtrado para obtener el conjunto de los elementos que cumplen
    * el predicado pasado como argumento
    * @param conjunto
    * @param p
    * @return
    */
  def filter(conjunto: Conjunto, p: Int => Boolean): Conjunto = 
  
  /**
    * Limite para la iteracion necesaria con paraTodo y existe,
    * entre -1000 y 1000
    */
  private val LIMITE = 1000

  /**
    * Determina si todos los elementos del conjunto cumplen
    * la condicion indicada por el predicado
    * @param conjunto
    * @param p
    * @return
    */
  def paraTodo(conjunto: Conjunto, p: Int => Boolean): Boolean = {
    // Funcion auxiliar para iterar sobre los valores desde
    // -LIMITE a LIMITE
    def iter(elemento: Int): Boolean = {
      if (???) ???
      else if (???) ???
      else iter(???)
    }
    
    iter(???)
  }

  /**
    * Determina si existe al menos un elemento en el conjunto
    * que cumple el predicado indicado
    * @param conjunto
    * @param p
    * @return
    */
  def existe(conjunto: Conjunto, p: Int => Boolean): Boolean = 

  /**
    * Genera un nuevo conjunto transformando los elementos del
    * conjunto pasado como argumento y aplicando la transformacion
    * dada por la funcion pasada como segundo argumento
    * @param conjunto
    * @param funcion
    * @return
    */
  def map(conjunto: Conjunto, funcion: Int => Int): Conjunto = 

  /**
    * Crea una cadena con el contenido completo del conjunto
    * @param conjunto
    * @return
    */
  def toString(conjunto: Conjunto): String = {
    val elementos = for (
       i <- -limite to limite if contiene(conjunto, i)) yield i
    elementos.mkString("{", ",", "}")
  }

  /**
    * Muestra el contenido completo del conjunto por pantalla
    * @param conjunto
    */
  def printSet(conjunto: Conjunto) {
    println(toString(conjunto))
  }
}
