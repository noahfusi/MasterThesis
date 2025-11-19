import scala.io.StdIn._
import scala.util.Random

object NospressoCafe {

  // numero de maquinas
  val nbMachines = 5

  //stock management
  var cafe_stock = Array.fill(nbMachines)(50) // grammes
  var sucre_stock = Array.fill(nbMachines)(30) // grammes
  var lait_stock = Array.fill(nbMachines)(500) // ml
  var machinePins = Array.fill(nbMachines)("434343") // PIN

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      println(" NespressoCafe ")
      println("Choisissez votre mode")
      println("1) ServeClient")
      println("2) ModeAdmin")
      println("3) ModeQuitter")
      println("> ")
      val mode = readLine()

      // choix du menu

      if (mode == "1") {
        serveClient()
      } else if (mode == "2") {
        ModeAdmin()
      } else if (mode == "3") {
        running = false
        println("Merci beaucoup, au revoir!")
      } else {
        println("réessayer")
      }
    }
  }

  // Mode Client
  def serveClient(): Unit = {
    println("Machine sélectionée (1-5) ")
    val machineId = readLine().toInt - 1
    if (machineId >= 0 && machineId < nbMachines) {
      println("Quelle boisson aimeriez boire:")
      println("1) Expresso 2,00")
      println("2) Cappuccino 2.50")
      println("3) Latte 2.70 (Petit) , 3.20 (Moyen), 3.70 (Grand) ")
      println(">")
      val BoissonChoix = readLine()
      if (BoissonChoix == "1") {
        Boisson(machineId, "Espresso", 8, 0, 2.0)
      } else if (BoissonChoix == "2") {
        Boisson(machineId, "Cappuccino", 6, 100, 2.5)
      } else if (BoissonChoix == "3") {
        Boisson(machineId, "Lait", 6, 120, 2.7)
      } else {
        println("réessayer")
        return // volta pro inicio pq n esta laranja
      }
    } else {
      println(" Le nombre de la machine c'est invalide. retour au début du menu")
    }
  }

  //preparation du boisson
  def Boisson(machineId: Int, nombre: String, Cafe: Int, Lait: Int, Prix: Double): Boolean = {
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
        return false
      }
    }

    // sucre
    println(" Vous voulliez sucre?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10")
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
      return false
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
          return false
        }
      } else {
        println("Option non valable, réessayer")
        return false
      }
    }

    if (cafe_stock(machineId) >= var_cafe && sucre_stock(machineId) >= quantitesucre && lait_stock(machineId) >= var_lait) {
      cafe_stock(machineId) -= var_cafe
      lait_stock(machineId) -= var_lait
      sucre_stock(machineId) -= quantitesucre

      Payment(var_prix)
      println("Preparation de votre boisson")
      println("[...]")
      Thread.sleep(2000) // atendez 2 second
      println(s" Votre $nombre est prête! Profiter ")
      return true

    } else {
      if (cafe_stock(machineId) < var_cafe) {
        println("Error: Inssufissant la quantite de cafe pour faire votre boisson.")
      } else if (sucre_stock(machineId) < quantitesucre) {
        println("Error: Inssufissant la quantite de sucre pour faire votre boisson.")
      } else if (lait_stock(machineId) < var_lait) {
        println("Error: Inssufissant la quantite de lait pour faire votre boisson.")
      } else {
        println("S'il te plâit selectione un'outre boisson ou selectioner Admin mode pour regarder le stock. ")

      }
      return false
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
    println("Machine sélectionée (1-5)")
    val machineId = readLine().toInt - 1
    if (machineId >= 0 && machineId < nbMachines) {
      if (validatePin(machineId, machinePins)) {
        println("Accès perméeable")
        println("1) Entrez le code PIN: ")
        println("2) Mise à jour le stock")
        val adminChoix = readLine()
        if (adminChoix == "1") {
          updatePin(machineId, machinePins)
        } else if (adminChoix == "2") {
          restockMachine(machineId, cafe_stock, lait_stock, sucre_stock)
        }
      } else {
        println("Accès refusé")
        return
      }
    } else {
      println("Machine invalide. Returné au menu principal")
      return
    }
  }

  // validar o PIN
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN:")
      val saisiPin = readLine()
      if (saisiPin == machinePins(machineId)) {
        return true
      } else {
        tentatives -= 1
        println(s"Code Pin incorrect. Vous avez $tentatives restant")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    return false
  }

  // atualizar PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var validPin = false
    while (!validPin) {
      println(f"Mise à jour du code PIN pour la ${machineId + 1}.")
      println("Invalid. Entrez un nouveau code PIN à 6 chiffres")
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succés.")
        validPin = true

      } else {
        println("Entrez un nouveau code PIN à 6 chiffres")

      }
    }
    println("Le code PIN à etè mis à jour avec succèes.Retour au menu principal...")
    return
  }

  // atualizando stock
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels:")
    println(f" Poudre de cafe: ${cafe_stock(machineId)} g")
    println(f" Lait: ${lait_stock(machineId)} ml")
    println(f"Sucre: ${sucre_stock(machineId)} g")
    println("Entrez les quantiées à ajouter:")
    println("Poudre de cafe :")
    cafe_stock(machineId) += readLine().toInt
    println("Sucre : ")
    sucre_stock(machineId) += readLine().toInt
    println("Lait : ")
    lait_stock(machineId) += readLine().toInt
    println("Stock niveaux mise à jour.")
    println("Returner au menu...")

    }
  } // nospresso
