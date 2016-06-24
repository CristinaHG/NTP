/**
  * Created by cris on 23/06/16.
  */
object Huffman {


  abstract class Nodo {}
  /**
    * Clase para representar nodos terminales
    *
    * @param representado      caracter representado
    * @param peso  peso correspondiente
    *
    */
  case class NodoHoja(representado:Char,peso:Int) extends Nodo{}
  /**
    * Clase para representar Nodos intermedios
    *
    * @param derecha      hijo a la derecha
    * @param izquierda  hijo a la izquierda
    * @param representadosPorNodo   lista de caracteres representada por el nodo
    * @param peso peso correspondiente
    *
    */
  case class NodoIntermedio(derecha:Nodo,izquierda:Nodo,representadosPorNodo:List[Char],peso:Int) extends Nodo{}


  /**
    * recibe como argumento un nodo y devuelve el peso asociado calculando
    * los pesos de los nodos inferiores, desde las hojas hasta sus hijos
    *
    * @param nodo     nodo sobre el que calcular peso asociado
    *
  **/
  def calcularPeso(nodo:Nodo): Int = nodo match {
    case NodoHoja(_,peso)=> peso
    case NodoIntermedio(_,_,_,peso)=>peso
  }

  /**
    * recibe como argumento un ́arbol de codificación (un nodo, su ráız)
    * y devuelve la lista de caracteres que representa, considerando todos los nodos
    * inferiores
    *
    *@param nodo
    *
    **/
  def obtenerCaracteres(nodo:Nodo):List[Char]=nodo match {
    case NodoHoja(representado,_)=> List(representado)
    case NodoIntermedio(_,_,representadosPorNodo,_)=>representadosPorNodo
  }

  /**
    * recibe como argumento los sub ́arboles a izquierda y derecha y genera
    * un nuevo  ́arbol a partir de ellos
    *
    *@param nodosIzq
    *@param nodosDer
    *
    **/
  def generarArbol(nodosIzq:Nodo,nodosDer:Nodo) ={
    NodoIntermedio(nodosIzq,nodosDer,obtenerCaracteres(nodosIzq)::: obtenerCaracteres(nodosDer),calcularPeso(nodosIzq)+calcularPeso(nodosDer))
  }


  /**
    * calcule la frecuencia de aparición de cada caŕacter en el texto a analizar
    *
    * @param caracteres lista de caracteres
    *
    **/
  def obtenerTuplasOcurrencias(caracteres:List[Char]): List[(Char, Int)] = {
    def timesAdd(chars: List[Char], times: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) times
      else {
        val o = times.find(p => (p._1 == chars.head))
        if (o.isEmpty) {
          timesAdd(chars.tail, (chars.head, 1) :: times)
        } else {
          val p = o.get
          timesAdd(chars.tail, times.updated(times.indexOf(p),(p._1, p._2 + 1)))
        }
      }
    }
    timesAdd(caracteres, Nil)

  }

  /**
    * genera una lista con todos los nodos hoja del ́arbol de codificacíon.
    * Esta lista de nodos terminales debe estar ordenada por pesos
    * de forma ascendente
    *
    * @param listaOrdenada lista de hojas ordenada por pesos de forma asc
    *
    **/
  def generarListHojasOrdenadas(listaOrdenada: List[(Char, Int)]) : List[NodoHoja] = {
    def loop(freqs: List[(Char, Int)], accu: List[NodoHoja]): List[NodoHoja] = {
      if (freqs.isEmpty)
        accu
      else
        loop(freqs.tail, NodoHoja(freqs.head._1, freqs.head._2) :: accu)
    }
    loop(listaOrdenada.sortWith((x, y) => x._2 > y._2), List[NodoHoja]())
  }


  /**
    * comprueba si una lista de nodos (́arboles) contiene a un único elemento
    *
    * @param arboles lista de hojas ordenada por pesos de forma asc
    *
    **/
  def singleton(arboles:List[Nodo]):Boolean={arboles.length==1}


  /**
    * combina todos los nodos terminales. Su funcionamiento se basa en:
    *elimina de la lista de arboles (nodos) los dos con menos peso
    * los combina para formar un nodo intermedio con ellos
    * inserta este nodo (́arbol) en la lista de nodos a combinar.
    * La insercíon de realizarse de forma que se preserve el orden.
    * funcíon hasta
    *
    * @param arboles lista de hojas ordenada por pesos de forma asc
    *
    **/
  def combinar(arboles:List[Nodo]):List[Nodo]={
    if (arboles.tail.isEmpty) arboles
    else (generarArbol(arboles.head, arboles.tail.head) :: arboles.drop(2)).sortBy(calcularPeso)
  }

  /**
    *funcion que hace llamadas a las funciones definidas en
    * pasos anteriores hasta que la lista de nodos contenga un único
    * elemento.
    *
    * @param parada
    * @param accion
    * @param listaNodos
    *
    **/
  def hasta(parada:List[Nodo]=>Boolean,accion:List[Nodo]=>List[Nodo])(listaNodos:List[Nodo]): List[Nodo] ={
    if(parada(listaNodos)==true) listaNodos
    else hasta(parada,accion)(accion(listaNodos))
  }


  /**
    * recibe como argumento una lista de caracteres a analizar y devuelve el
    * arbol generado.
    *
    *@param caracteres lista de caracteres
    **/
 def generarArbolCodificacion(caracteres:List[Char]) = {

    val ordenadas=obtenerTuplasOcurrencias(caracteres)
    hasta(singleton,combinar)(generarListHojasOrdenadas(ordenadas)).head
  }

  /**
    * hace el paso entre una cadena de texto normal y la lista de caracteres
    *
    *@param cadena lista de caracteres
    **/

 def stringAListaCaracteres(cadena:String):List[Char] = cadena.toList


  /**
    * decodifica una lista de 0’s y 1’s que ha sido codificada mediante
    * un arbol espećıfico que se facilitaŕa
    *
    *@param arbol arbol proporcionado
    **/
  def decodificar(arbol: Nodo, bits : List[Int]) : List[Char] = {
    def iter(bits: List[Int], node: Nodo): List[Char] = {
      node match {
        case NodoHoja(c, _) => c :: iter(bits, arbol)
        case NodoIntermedio(l, r, _, _) => {
          if (bits.isEmpty)
            List[Char]()
          else {
            if (bits.head == 0)
              iter(bits.tail, l)
            else
              iter(bits.tail, r)
          }
        }
      }

    }

    iter(bits, arbol)
  }


  /**
    * función de codificado en secuencia de bits
    *
    *@param arbol arbol proporcionado
    *@param texto texto a codificar
    **/
  def codificar(arbol : Nodo, texto : List[Char]) : List[Int]={
    def encodeAcc(tree: Nodo)(char: Char, bits: List[Int]): List[Int] = {
      tree match {
        case NodoHoja(c, _) => if (c == char) bits else throw new Error
        case NodoIntermedio(left, right, _, _) => left match {
          case NodoHoja(c, _) => if (c == char) 0 :: bits else encodeAcc(right)(char, 1 :: bits)
          case NodoIntermedio(_, _, chars, _) => if (chars.exists((ch: Char) => (ch == char))) encodeAcc(left)(char, 0 :: bits)
          else encodeAcc(right)(char, 1 :: bits)
        }
      }
    }
    encodeAcc(arbol)(texto.head, Nil).reverse ::: { if (texto.tail.isEmpty) Nil else codificar(arbol,texto.tail) }
  }

//tabla de códigos
  type TablaCodigo=List[(Char, List[Int])]

  /**
    * acceso a la tabla
    *
    *@param tabla
    *@param caracter
    **/
  def codificarConTabla(tabla : TablaCodigo)(caracter : Char) : List[Int]={
    tabla.find(t => t._1 == caracter).get._2
//    if (tabla.head._1 == caracter)
//      tabla.head._2
//    else
//      codificar(tabla.tail)(caracter)
  }

  /**
    * creación de la tabla. Se reliza visitando el árbol de
    * codificación.
    *
    *@param arbolCodificacion
    **/
  def convertirArbolTabla(arbolCodificacion : Nodo) : TablaCodigo={
    arbolCodificacion match {
      case NodoHoja(representado, _) => (representado,Nil)::Nil
      case NodoIntermedio(l, r, _, _) => mergeCodeTables(convertirArbolTabla(l), convertirArbolTabla(r))
    }
  }

  def mergeCodeTables(a: TablaCodigo, b: TablaCodigo): TablaCodigo = {
    def trans(c: TablaCodigo, b: Int): TablaCodigo = for (p <- c) yield (p._1, b :: p._2)
    trans(a, 0) ::: trans(b, 1)
  }

  /**
    * recibe como argumento el arbol de codificacíon y el texto a codificar
    *
    *@param arbolCodificacion
    **/
  def codificacionRapida(arbolCodificacion:Nodo)(texto:List[Char]):List[Int]={
    val table = convertirArbolTabla(arbolCodificacion)
    def iter(text: List[Char]): List[Int] = {
      if (text.isEmpty)
        List[Int]()
      else
        codificarConTabla(table)(text.head) ::: iter(text.tail)
    }
    iter(texto)

  }
}