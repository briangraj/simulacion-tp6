import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val simulador = new Simulador()
    print("Cantidad de cocineros: ")
    val nc = scala.io.StdIn.readLine().toInt
    println(nc)
    print("Cantidad de repartidores: ") 
    val nr = scala.io.StdIn.readLine().toInt
    println(nr)
    simulador.simular(nc,nr)
  }
}

class Simulador(var tcc: Array[Double] = Array.emptyDoubleArray, var stoc: Array[Double] = Array.emptyDoubleArray, var ptoc: Array[Double] = Array.emptyDoubleArray,
                var tcr: Array[Double] = Array.emptyDoubleArray, var stor: Array[Double] = Array.emptyDoubleArray, var ptor: Array[Double] = Array.emptyDoubleArray,
                var NC: Int = 0, var NR: Int = 0) extends Variables {
 
  def inicializar: Unit = {
    tcc = new Array[Double](NC)
    stoc = new Array[Double](NC)
    ptoc = new Array[Double](NC)
    tcr = new Array[Double](NR)
    stor = new Array[Double](NR)
    ptor = new Array[Double](NR)
  }

  def simular(_NC: Int, _NR: Int): Unit = {
    NC = _NC
    NR = _NR
    inicializar
    
    while (t <= tf) {
      t = tllp
      ia = intervaloEntreArribos
      tllp = t + ia
      r = random
      p = porcentajeSegunDia
      i = menorTiempo(tcc, stoc)
      tep = tiempoEnvioPedido
      tpp = tiempoPreparacionPedido

      if (r <= p)
        atenderEnvio()
      else 
        atenderLocal()
    }

    calcularResultados
  }

  def atenderEnvio(): Unit = {
    if (seArrepienteEnEnvio) {
      carre += 1
      return
    }

    j = menorTiempo(tcr, stor)
    
    if (t > tcc(i)) {
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp 
    } else {
      tcc(i) += tpp
    }

    if (tcc(i) > tcr(j)) {
      stor(j) += tcc(i) - tcr(j)
      stee += tcc(i) + tep - t
      tcr(j) = tcc(i) + tep 
    } else {
      stee += tcr(j) + tep - t
      tcr(j) += tep
    }

    spae += 1
  }

  def atenderLocal(): Unit = {
    if (seArrepienteEnLocal) {
      carrl += 1
      return
    }

    if (t > tcc(i)) {
      stel += tpp
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp
    } else {
      stel += (tcc(i) + tpp) - t 
      tcc(i) += tpp
    }

    spal += 1
  }

  def calcularResultados: Unit = {
    println("Variables de estado: ")
    println("Cantidad de repartidores: " + NR)
    println("Cantidad de cocineros: " + NC)
    println(" ")
    println("Resultados obtenidos: ")
    
    for (i <- 0 until NC) {
      ptoc(i) = (stoc(i) * 100 / t).toInt 
      println("PTOC(" + i + "): " + ptoc(i) + "%" )
    }

    for (j <- 0 until NR) {
      ptor(j) = (stor(j) * 100 / t).toInt 
      println("PTOR(" + j + "): " + ptor(j) + "%")
    }
    
    ptee = stee / spae
    ptel = stel / spal
    
    println("PTEE: " + ptee/60 + " minutos")
    println("PTEL: " + ptel/60 + " minutos")

    pae = (carre * 100 / (carre + spae)).toInt
    pal = (carrl * 100 / (carrl + spal)).toInt
    println("PAE: " + pae + "%")
    println("PAL: " + pal + "%")
  }

  def seArrepienteEnEnvio: Boolean = {
    val tlp: Double = tiempoEnQueEstaListoElPedido
    val teep: Double = tiempoEnQueSeEntregaElPedido(tlp)

    val seg_espera = teep - t

    seg_espera > (65 * 60) || seg_espera > (50 * 60) && random < 30
  }

  def seArrepienteEnLocal: Boolean = {
    val tlp: Double = tiempoEnQueEstaListoElPedido

    val seg_espera = tlp - t

    seg_espera > (30 * 60) && random <= 60
  }

  def tiempoEnQueEstaListoElPedido: Double = {
    if (t < tcc(i))
      tcc(i) + tpp
    else
      t + tpp
  }

  def tiempoEnQueSeEntregaElPedido(tlp: Double): Double = {
    if (tlp < tcr(j))
      tcr(j) + tep
    else
      tlp + tep
  }

  def random: Double = Random.between(1, 101)

  def semanaOFinde: SemanaOFinde = {
    if (dia < 96)
     Semana
    else 
     Finde
  }

  def dia: Double = (t / (60 * 60)) % 168

  def intervaloEntreArribos: Double = semanaOFinde.intervaloEntreArribos

  def porcentajeSegunDia: Double = semanaOFinde.porcentajeEnvios

  def menorTiempo(tc: Array[Double], sto: Array[Double]): Int = {
    var minimo_tc: Double = 1000000000
    var tc_minimos: Array[Int] = Array.emptyIntArray
    var maximo_ocioso: Double = -1000
    var index_minimo: Int = 0

    for (i <- 0 until tc.length) {
      if (tc(i) <= minimo_tc) {
        minimo_tc = tc(i)
        tc_minimos :+ i
        index_minimo = i
      }
    }

    // creo que nunca empatan en tc
    if (tc_minimos.length > 0) {
      for (j <- 0 until tc_minimos.length) {
        if (sto(tc_minimos(j)) >= maximo_ocioso) {
          maximo_ocioso = sto(tc_minimos(j))
          index_minimo = tc_minimos(j)
        }
      }
    }

    return index_minimo
  }

  def tiempoEnvioPedido: Double = (9 + (22 - 9) * random / 100) * 60

  def tiempoPreparacionPedido: Double = (12 + (20 - 12) * random / 100) * 60 
}

abstract class Variables {
  var t: Double = 0
  var tf: Double = 10000000
  var tllp: Double = 0
  var ia: Double = 0
  var tep: Double = 0
  var tpp: Double = 0
  var j: Int = 0
  var a: Boolean = true
  var tcc, stoc: Array[Double]
  var tcr, stor: Array[Double]
  var ptoc, ptor: Array[Double]

  var stee: Double = 0
  var stel: Double = 0
  var spal: Double = 0
  var spae: Double = 0
  var ptee: Double = 0
  var ptel: Double = 0
  var carre: Double = 0
  var carrl: Double = 0
  var pae: Double = 0
  var pal: Double = 0

  var r: Double = 0
  var p: Double = 0
  var i: Int = 0
}

trait SemanaOFinde {
  def intervaloEntreArribos: Double

  def porcentajeEnvios: Double
}

object Semana extends SemanaOFinde {
  override def intervaloEntreArribos: Double = ( 66 + (460 - 66) * Random.between(1, 101) ) / 100 

  override def porcentajeEnvios: Double = 75
}

object Finde extends SemanaOFinde {
  override def intervaloEntreArribos: Double = ( 45 + (300 - 45) * Random.between(1, 101) ) / 100
    //(2000/((1/ math.pow(Random.between(2,101)-1,(1/67000)))) + 67000).toInt / 100
    //Random.nextInt(3) + 1
    //val a: Double = 65.9929 + 53.9361 * ( math.sqrt(2) * Erf.erf( 2 * Random.between(1, 101) -1 ))
    //val b: Float = math.exp(a).toFloat
    //return math.round(b)

  override def porcentajeEnvios: Double = 60
}