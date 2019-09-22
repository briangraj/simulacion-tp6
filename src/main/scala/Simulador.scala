import scala.util.Random

class Simulador(var tcc: Array[Int], var stoc: Array[Int], var ptoc: Array[Int],
                var tcr: Array[Int], var stor: Array[Int], var ptor: Array[Int],
                var NC: Int, var NR: Int) extends Variables {

  def inicializar: Unit = {
    tcc = new Array[Int](NC)
    stoc = new Array[Int](NC)
    tcr = new Array[Int](NR)
    stor = new Array[Int](NR)
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
      i = menorTiempo()
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
    j = menorTiempo()
    if (seArrepienteEnEnvio)
      return

    if (t >= tcc(i)) {
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp
    } else {
      tcc(i) += tpp
    }

    if (tcc(i) >= tcr(j)) {
      ste += tcc(i) + tep - t
      stor(j) += t - tcr(j)
      tcr(j) = tcc(i) + tep * 2
    } else {
      ste += tcr(j) + tep - t
      tcr(j) += tep * 2
    }

    spa += 1
  }

  def atenderLocal(): Unit = {
    if (seArrepienteEnLocal)
      return

    if (t >= tcc(i)) {
      ste += tpp
      stoc(i) += t - tcc(i)
      tcc(i) = t + tpp
    } else {
      ste += tcc(i) + tpp
      tcc(i) += tpp
    }

    spa += 1
  }

  def calcularResultados: Unit = {
    for (i <- 0 to NC) ptoc(i) = stoc(i) / t * 100

    for (j <- 0 to NR) ptor(j) = stor(j) / t * 100

    pte = ste / spa
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








  def menorTiempo(): Int = 1

  def tiempoEnvioPedido: Int = 15

  def tiempoPreparacionPedido: Int = 10
}


abstract class Variables {
  var t: Int = 0
  var tf: Int = 500
  var tllp: Int = 0
  var ia: Int = 0
  var tep: Int = 0
  var tpp: Int = 0
  var j: Int = 0
  var a: Boolean = true
  var tcc, stoc: Array[Int]
  var tcr, stor: Array[Int]
  var ptoc, ptor: Array[Int]

  var ste: Int = 0
  var spa: Int = 0
  var pte: Int = 0


  var r: Int = 0
  var p: Int = 0
  var i: Int = 0
}

trait SemanaOFinde {
  def intervaloEntreArribos: Int

  def porcentajeEnvios: Int
}
object Semana extends SemanaOFinde {
  override def intervaloEntreArribos: Int = 10

  override def porcentajeEnvios: Int = 75
}
object Finde extends SemanaOFinde {
  override def intervaloEntreArribos: Int = 15

  override def porcentajeEnvios: Int = 60
}