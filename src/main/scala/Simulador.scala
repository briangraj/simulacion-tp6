import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val simulador = new Simulador()

    simulador.simular(3, 2)
  }
}

class Simulador(var tcc: Array[Int] = Array.emptyIntArray, var stoc: Array[Int] = Array.emptyIntArray, var ptoc: Array[Int] = Array.emptyIntArray,
                var tcr: Array[Int] = Array.emptyIntArray, var stor: Array[Int] = Array.emptyIntArray, var ptor: Array[Int] = Array.emptyIntArray,
                var NC: Int = 0, var NR: Int = 0) extends Variables {

  def inicializar: Unit = {
    tcc = new Array[Int](NC)
    stoc = new Array[Int](NC)
    ptoc = new Array[Int](NC)
    tcr = new Array[Int](NR)
    stor = new Array[Int](NR)
    ptor = new Array[Int](NR)
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
    j = menorTiempo(tcr, stor)
    if (seArrepienteEnEnvio)
      return

    if (t > tcc(i)) {
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp
    } else {
      tcc(i) += tpp
    }

    if (tcc(i) > tcr(j)) {
      stor(j) += tcc(i) - tcr(j)
      println
      stee += tcc(i) + tep / 2 - t
      tcr(j) = tcc(i) + tep
    } else {
      stee += tcr(j) + tep / 2 - t
      tcr(j) += tep
    }

    spae += 1
  }

  def atenderLocal(): Unit = {
    if (seArrepienteEnLocal)
      return

    if (t > tcc(i)) {
      stel += tpp
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp
    } else {
      stel += tcc(i) + tpp
      tcc(i) += tpp
    }

    spal += 1
  }

  def calcularResultados: Unit = {
    println("Resultados obtenidos: ")
    println("-------------------------------------")
    println("Cantidad de repartidores: " + NR)
    println("Cantidad de cocineros: " + NC)
    
    for (i <- 0 until NC) {
      ptoc(i) = stoc(i) * 100 / t 
      println("PTOC(" + i + "): " + ptoc(i) + " minutos")
    }

    for (j <- 0 until NR) {
      ptor(j) = stor(j) * 100 / t 
      println("PTOR(" + j + "): " + ptor(j) + " minutos")
    }
    
    ptee = stee / spae
    ptel = stel / spal
    println("PTEE: " + ptee + " minutos")
    println("PTEL: " + ptel + " minutos")
  }

  def seArrepienteEnEnvio: Boolean = {
    val tlp: Int = tiempoEnQueEstaListoElPedido
    val teep: Int = tiempoEnQueSeEntregaElPedido(tlp)

    val mines = teep - t

    mines > 50 || mines > 40 && random > 30
  }

  def seArrepienteEnLocal: Boolean = {
    val tlp: Int = tiempoEnQueEstaListoElPedido

    val mines = tlp - t

    mines > 30 && random <= 60
  }

  def tiempoEnQueEstaListoElPedido: Int = {
    if (t < tcc(i))
      tcc(i) + tpp
    else
      t + tpp
  }

  def tiempoEnQueSeEntregaElPedido(tlp: Int): Int = {
    if (tlp < tcr(j))
      tcr(j) + tep
    else
      tlp + tep
  }

  def random: Int = Random.between(1, 101)

  def semanaOFinde: SemanaOFinde = {
    if (dia < 4)
      Semana
    else
      Finde
  }

  def dia: Int = (t / 2) % 7

  def intervaloEntreArribos: Int = {
    semanaOFinde.intervaloEntreArribos
  }

  def porcentajeSegunDia: Int = {
    semanaOFinde.porcentajeEnvios
  }

  def menorTiempo(tc: Array[Int], sto: Array[Int]): Int = {
    var minimo: Int = 1000
    var tc_minimos: Array[Int] = Array.emptyIntArray
    var maximo: Int = 0
    var index: Int = 0

    for (i <- 0 until tc.length) {
      if (tc(i) <= minimo) {
        minimo = tc(i)
        tc_minimos :+ i
        index = i
      }
    }
    
    if (tc_minimos.length > 0) {
      for (i <- 0 until tc_minimos.length) {
        if (sto(i) >= maximo) {
          maximo = sto(i)
          index = tc_minimos(i)
        }
      }
    }
    return index
  }

  def tiempoEnvioPedido: Int = 9 + (20 - 9) * random / 100

  def tiempoPreparacionPedido: Int = 5 + (16 - 5) * random / 100
}



abstract class Variables {
  var t: Int = 0
  var tf: Int = 10000
  var tllp: Int = 0
  var ia: Int = 0
  var tep: Int = 0
  var tpp: Int = 0
  var j: Int = 0
  var a: Boolean = true
  var tcc, stoc: Array[Int]
  var tcr, stor: Array[Int]
  var ptoc, ptor: Array[Int]

  var stee: Int = 0
  var stel: Int = 0
  var spal: Int = 0
  var spae: Int = 0
  var ptee: Int = 0
  var ptel: Int = 0

  var r: Int = 0
  var p: Int = 0
  var i: Int = 0
}

trait SemanaOFinde {
  def intervaloEntreArribos: Int

  def porcentajeEnvios: Int
}

object Semana extends SemanaOFinde {
  override def intervaloEntreArribos: Int = 15

  override def porcentajeEnvios: Int = 75
}

object Finde extends SemanaOFinde {
  override def intervaloEntreArribos: Int = 5

  override def porcentajeEnvios: Int = 60
}