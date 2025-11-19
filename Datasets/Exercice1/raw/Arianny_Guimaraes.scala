import scala.io.StdIn._
import scala.util.Random

object NospressoCafe {

  //stock management
  var cafe_stock = 50 // grammes
  var sucre_stock = 30 // grammes
  var lait_stock = 500 // ml
  val admPin: String = "434343"

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      println(" NespressoCafe ")
      println("Choisissez votre mode")
      println("1) ModeClient")
      println("2) ModeAdmin")
      println("3) ModeQuitter")
      println("> ")
      val mode = readLine()

      // choix du menu

      if (mode == "1") {
        ModeClient()
      } else if (mode == "2") {
        ModeAdmin()
      } else if (mode == "3") {
        running = false
        println("Merci beaucoup, au revoir")
      } else {
        println("réessayer")
      }
    }
  }


  // Mode Client
  def ModeClient(): Unit = {
    println("Quelle boisson aimeriez boire:")
    println("1) Expresso 2,00")
    println("2) Cappuccino 2.50")
    println("3) Latte 2.70 (Petit) , 3.20 (Medium), 3.70 (Grand) ")
    println(">")
    val BoissonChoix = readLine()
    if (BoissonChoix == "1") {
      Boisson("Espresso", 8, 0, 2.0)
    } else if (BoissonChoix == "2") {
      Boisson("Cappuccino", 6, 100, 2.5)
    } else if (BoissonChoix == "3") {
      Boisson("Lait", 6, 120, 2.7)
    } else {
      println("réessayer")
      return // volta pro inicio pq n esta laranja
    }
  }


  //preparation du boisson
  def Boisson(nombre: String, Cafe: Int, Lait: Int, Prix: Double): Unit = {
    var var_cafe = Cafe
    var var_lait = Lait
    var var_prix = Prix

    if (nombre == "Lait") {
      println(" Quelle tail du Lait")
      println("1) Petit - 2.70")
      println("2)Medium - 3.20 ")
      println("3) Grand - 3.70")
      println(">")
      val tailchoix = readLine()
      if (tailchoix == "1") {
        var_prix += 0.0
        var_cafe += 0
        var_lait += 0
      } else if (tailchoix == "2") {
        var_prix += 0.5
        var_cafe += 2
        var_lait += 30
      } else if (tailchoix == "3") {
        var_prix += 1.0
        var_cafe += 6
        var_lait += 80
      } else {
        println("réessayer")
        return
      }

    }

    // sucre
    println(" vous voulliez sucre?")
    println("1) sans sucre")
    println("2) peu (5g) - 0.10")
    println("3) Moyen (10g)- CHF 0.20")
    println("4) Beaucoup (15g)- CHF 0.30")
    println(">")
    val sucrechoix = readLine()


    var quantitesucre = 0

    if (sucrechoix == "1") {
      var_prix += 0.0
      quantitesucre = 0
    } else if (sucrechoix == "2") {
      var_prix += 0.1
      quantitesucre = 5
    } else if (sucrechoix == "3") {
      var_prix += 0.2
      quantitesucre = 10
    } else if (sucrechoix == "4") {
      var_prix += 0.3
      quantitesucre = 15
    } else {
      println("Option non valable, reéssayer")
      return
    }

    // plus de lait?
    if (nombre == "Cappuccino" || nombre == "Lait") {
      println("Voulliez-vous extra lait?")
      println("Seulement pour Cappuccino et Lait")
      println("1) Oui")
      println("2) Non")
      println(">")
      val extralait = readLine()

      if (extralait == "2") {
        var_prix += 0.0
      } else if (extralait == "1") {
        println("Combien de doses? Le maximum de doses c'est 1, 2 ou 3")
        val doses = readLine()
        if (doses == "1" || doses == "2" || doses == "3") {
          var_prix += 0.05 * doses.toInt
          var_lait += 50 * doses.toInt
        } else {
          println("La quantite c'est superieur à la valeur autorisée, Veulliez entrer un nombre valide de doses")
          return
        }
      } else {
        println("Option non valable, réessayer")
        return
      }
    }

    if (cafe_stock >= var_cafe && sucre_stock >= quantitesucre && lait_stock >= var_lait) {
      cafe_stock -= var_cafe
      lait_stock -= var_lait
      sucre_stock -= quantitesucre


      Payment(var_prix)
      println("Preparation de votre boisson")
      println("[...]")
      Thread.sleep(2000) // atendez 2 second
      println(s" Votre $nombre est prête! Profiter ")
    } else {
      if (cafe_stock < var_cafe) {
        println("Error: Inssufissant la quantite de cafe pour faire votre boisson.")
      } else if (sucre_stock < quantitesucre) {
        println("Error: Inssufissant la quantite de sucre pour faire votre boisson.")
      } else if (lait_stock < var_lait) {
        println("Error: Inssufissant la quantite de lait pour faire votre boisson.")
      } else {
        println("S'il te plâit selectione un'outre boisson ou selectioner Admin mode pour regarder le stock. ")
        return
      }
    }

  }
  // mode Payment Twint
  def Payment(prixtotal: Double): Unit = {
    println(f" le prix total c'est: CHF $prixtotal%.2f")
    val Twint = Random.alphanumeric.take(5).mkString
    println(s"Payment avec le code Twint: $Twint")
    println("Atendez le payment de validation...")
    Thread.sleep(3000) // esperar 3 segundos
    println("Merci, Au revoir!")

  }

  // Mode Admin
  def ModeAdmin(): Unit = {
    println("Saisir le mot de passe PIN du admin")
    val pin = readLine()
    if (pin == admPin){
      println("Accès perméeable")
      println("Stock: ")
      println(f" Cafe poudre: $cafe_stock g")
      println(f" Lait: $lait_stock ml")
      println(f"Sucre: $sucre_stock g")
      println("Remplissage stock...")
      println("Addition")
      println(" Cafe poudre:")
      cafe_stock += readLine().toInt
      println("Lait:")
      lait_stock += readLine().toInt
      println("Sucre:")
      sucre_stock += readLine().toInt
      println(" Stock niveaux  mise a jour")
      println(" Returner au menu")
      return
    }else {
      println("Accès refusé")
      return
    }
  }


} // nespresso
