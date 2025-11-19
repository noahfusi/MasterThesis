import io.StdIn._
import math._
import scala.util.Random

object Main {

  val nbMachines = 5
  var machineId = 0

  // Tableaux des stocks
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  var Machinepins = Array.fill(nbMachines)("434343")


  val expresso = 1
  val cappucino = 2
  val latte = 3
  val coutexpresso = 2.0
  val coutcappuccino = 2.5
  val coutlatte = 2.7
  val sanssucre = 1
  val peudesucre = 2
  val moyendesucre = 3
  val bcpdesucre = 4
  val coutpeusucre = 0.10
  val coutmoyensucre = 0.20
  val coutbcpsucre = 0.30




  def main(args: Array[String]): Unit = {
    var continuer = true
    do {
      choisirMachine()
      var continuerMachine = true

      while (continuerMachine) {
        println("\nNospresso Café")
        println("Veuillez sélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Changer de machine")
        println("4) Quitter")

        val mode = readInt()
        if (mode == 1) {
          serveClient(machineId)
        } else if (mode == 2) {
          if (validatePin(machineId)) {
            println("\nMenu Admin:")
            println("1) Voir et mettre à jour les stocks")
            println("2) Modifier le PIN")
            println("3) Retour")

            val choix = readInt()
            if (choix == 1) {
              restockMachine(machineId)
            } else if (choix == 2) {
              updatePin(machineId)
            }
            Thread.sleep(1000)
          }
        } else if (mode == 3) {
          continuerMachine = false
        } else if (mode == 4) {
          continuerMachine = false
          continuer = false
        } else {
          println("Option non valide")
        }
      }
    } while (continuer)
    println("Merci de votre visite. Bonne journée!")
  }


  def choisirMachine(): Unit = {
    println("Entrez le numéro de la machine (1-5):")
    do {
      machineId = readInt() - 1
      if (machineId < 0 || machineId >= nbMachines) {
        println(s"Option non valide. Veuillez choisir entre 1 et $nbMachines")
      }
    } while (machineId < 0 || machineId >= nbMachines)
    println(s"Machine ${machineId + 1} sélectionnée")
  }

  def serveClient(machineId: Int): Boolean = {
    println("Choisir une boisson:")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte (Petit CHF 2.70 / Moyen CHF 3.20 / Grand CHF 3.70)")

    var choixboisson = 0
    do {
      choixboisson = readInt()
      if (choixboisson < 1 || choixboisson > 3) {
        println("Option non valide. Veuillez choisir entre 1 et 3")
      }
    } while (choixboisson < 1 || choixboisson > 3)

    if (choixboisson == expresso) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")

      var choixsucre = 0
      do {
        choixsucre = readInt()
        if (choixsucre < 1 || choixsucre > 4) {
          println("Option non valide. Veuillez choisir entre 1 et 4")
        }
      } while (choixsucre < 1 || choixsucre > 4)

      preparerExpresso(machineId, choixsucre)

    } else if (choixboisson == cappucino) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      var choixsucre = 0
      do {
        choixsucre = readInt()
        if (choixsucre < 1 || choixsucre > 4) {
          println("Option non valide. Veuillez choisir entre 1 et 4")
        }
      } while (choixsucre < 1 || choixsucre > 4)

      preparerCappuccino(machineId, choixsucre)

    } else if (choixboisson == latte) {
      preparerLatte(machineId)
    }

    true
  }

  def preparerExpresso(machineId: Int, choixsucre: Int): Unit = {
    val quantiteCafe = 8
    var quantiteSucre = 0

    if (choixsucre == peudesucre) quantiteSucre = 5
    else if (choixsucre == moyendesucre) quantiteSucre = 10
    else if (choixsucre == bcpdesucre) quantiteSucre = 15

    if (coffeeStocks(machineId) < quantiteCafe) {
      println("Stock insuffisant : Plus assez de café")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")
      return
    }
    if (quantiteSucre > 0 && sugarStocks(machineId) < quantiteSucre) {
      println("Stock insuffisant : Plus assez de sucre")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")
      return
    }

    var prix = coutexpresso
    if (choixsucre == peudesucre) prix += coutpeusucre
    else if (choixsucre == moyendesucre) prix += coutmoyensucre
    else if (choixsucre == bcpdesucre) prix += coutbcpsucre

    println("Prix total: CHF $prix")
  Twint()

    coffeeStocks(machineId) -= quantiteCafe
    if (quantiteSucre > 0) sugarStocks(machineId) -= quantiteSucre

    preparerBoisson("Expresso")
  }

  def preparerCappuccino(machineId: Int, choixsucre: Int): Unit = {
    val quantiteCafe = 6
    var quantiteLait = 100
    var quantiteSucre = 0
    var prix = coutcappuccino

    println("Souhaitez-vous ajouter du lait ?")
    println("1) OUI")
    println("2) NON")

    var choixLait = 0
    do {
      choixLait = readInt()
      if (choixLait != 1 && choixLait != 2) {
        println("Option non valide. Veuillez choisir entre 1 et 2")
      }
    } while (choixLait != 1 && choixLait != 2)

    if (choixLait == 1) {
      println("Combien de doses de lait souhaitez-vous ? (1-3)")
      var dosesLait = 0
      do {
        dosesLait = readInt()
        if (dosesLait < 1 || dosesLait > 3) {
          println("Erreur : veuillez choisir une valeur entre 1 et 3")
        }
      } while (dosesLait < 1 || dosesLait > 3)

      quantiteLait = dosesLait * 50
      prix += dosesLait * 0.05
    }

    if (choixsucre == peudesucre) quantiteSucre = 5
    else if (choixsucre == moyendesucre) quantiteSucre = 10
    else if (choixsucre == bcpdesucre) quantiteSucre = 15

    if (coffeeStocks(machineId) < quantiteCafe) {
      println("Stock insuffisant : Plus assez de café")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")
     }
    if (quantiteLait > 0 && milkStocks(machineId) < quantiteLait) {
      println("Stock insuffisant : Plus assez de lait")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")

    }
    if (quantiteSucre > 0 && sugarStocks(machineId) < quantiteSucre) {
      println("Stock insuffisant : Plus assez de sucre")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")

    }

    if (choixsucre == peudesucre) prix += coutpeusucre
    else if (choixsucre == moyendesucre) prix += coutmoyensucre
    else if (choixsucre == bcpdesucre) prix += coutbcpsucre

    println(s"Prix total: CHF $prix")
   Twint()

    coffeeStocks(machineId) -= quantiteCafe
    if (quantiteLait > 0) milkStocks(machineId) -= quantiteLait
    if (quantiteSucre > 0) sugarStocks(machineId) -= quantiteSucre

    preparerBoisson("Cappuccino")
  }

  def preparerLatte(machineId: Int): Unit = {
    var quantiteCafe = 0
    var quantiteLait = 0
    var quantiteSucre = 0
    var prix = 0.0

    println("Choisissez la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")

    var choixTaille = 0
    do {
      choixTaille = readInt()
      if (choixTaille < 1 || choixTaille > 3) {
        println("Option non valide. Veuillez choisir entre 1 et 3")
      }
    } while (choixTaille < 1 || choixTaille > 3)

    if (choixTaille == 1) {
      ( prix = 2.70 ); quantiteCafe = 5;quantiteLait= 120
    }
    else if (choixTaille == 2) {
      prix = 3.20; quantiteCafe = 8;quantiteLait =150
    }

    else if (choixTaille == 3){
      (prix = 3.70); quantiteCafe = 12;quantiteLait= 200
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    var choixsucre = 0
    do {
      choixsucre = readInt()
      if (choixsucre < 1 || choixsucre > 4) {
        println("Option non valide. Veuillez choisir entre 1 et 4")
      }
    } while (choixsucre < 1 || choixsucre > 4)

    if (choixsucre == peudesucre) quantiteSucre = 5
    else if (choixsucre == moyendesucre) quantiteSucre = 10
    else if (choixsucre == bcpdesucre) quantiteSucre = 15

    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) OUI")
    println("2) NON")

    var choixLait = 0
    do {
      choixLait = readInt()
      if (choixLait != 1 && choixLait != 2) {
        println("Option non valide. Veuillez choisir entre 1 et 2")
      }
    } while (choixLait != 1 && choixLait != 2)

    if (choixLait == 1) {
      println("Combien de doses de lait souhaitez-vous ? (1-3)")
      var dosesLait = 0
      do {
        dosesLait = readInt()
        if (dosesLait < 1 || dosesLait > 3) {
          println("Erreur : veuillez choisir une valeur entre 1 et 3")
        }
      } while (dosesLait < 1 || dosesLait > 3)

      quantiteLait += dosesLait * 50
      prix += dosesLait * 0.05
    }

    if (coffeeStocks(machineId) < quantiteCafe) {
      println("Stock insuffisant : Plus assez de café")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")
      return
    }
    if (quantiteLait > 0 && milkStocks(machineId) < quantiteLait) {
      println("Stock insuffisant : Plus assez de lait")
      println("Veuillez choisir une autre taille, une autre boisson ou accéder au mode admin pour recharger les stocks")
      return
    }
    if (quantiteSucre > 0 && sugarStocks(machineId) < quantiteSucre) {
      println("Stock insuffisant : Plus assez de sucre")
      println("Veuillez choisir une autre boisson ou accéder au mode admin pour recharger les stocks")
      return
    }

    if (verifierStocks(quantiteCafe, quantiteLait, quantiteSucre)) {
      if (choixsucre == peudesucre) prix += coutpeusucre
      else if (choixsucre == moyendesucre) prix += coutmoyensucre
      else if (choixsucre == bcpdesucre) prix += coutbcpsucre

      println(s"Prix total: CHF $prix")
    Twint()

      coffeeStocks(machineId) -= quantiteCafe
      milkStocks(machineId) -= quantiteLait
      if (quantiteSucre > 0) sugarStocks(machineId) -= quantiteSucre

      preparerBoisson("Latte")
    } else {
      println("Stock insuffisant pour préparer cette boisson")
    }
  }

  def preparerBoisson(nomBoisson: String): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    println(s"Votre $nomBoisson est prêt ! Bonne dégustation !")
  }

  def verifierStocks(cafeDemande: Int, laitDemande: Int = 0, sucreDemande: Int = 0): Boolean = {
    coffeeStocks(machineId) >= cafeDemande &&
      (laitDemande == 0 || milkStocks(machineId) >= laitDemande) &&
      (sucreDemande == 0 || sugarStocks(machineId) >= sucreDemande)
  }

  def Twint(): Unit = {
    println("Veuillez payer en utilisant Twint.")
    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    val length = 5
    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString
    println(s"Votre code Twint : $codeTwint")
    println("En attente de paiement...")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
  }


  def validatePin(machineId: Int): Boolean = {
    println(s"\nMode Admin - Machine ${machineId + 1}")
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN (${tentatives} tentatives restantes):")
      val pin = readLine()
      if (pin == Machinepins(machineId)) {
        println(s"Accès accordé pour la machine ${machineId + 1}.")
        return true
      }
      tentatives -= 1
      if (tentatives > 0) {
        println(s"Code PIN incorrect. ${tentatives} tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres:")
    var nouveauPin = ""
    do {
      nouveauPin = readLine()
      if (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
        println("Le PIN doit contenir exactement 6 chiffres. Réessayez:")
      }
    }while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit))
    Machinepins (machineId) = nouveauPin
    println("\nLe code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(2000)
  }

  def restockMachine(machineId: Int): Unit = {
    println(s"Stocks actuels de la machine ${machineId + 1}:")
    println(s"Poudre de café : ${coffeeStocks(machineId)}g")
    println(s"Sucre : ${sugarStocks(machineId)}g")
    println(s"Lait : ${milkStocks(machineId)}ml")

    println("\nEntrez les quantités à ajouter :")

    println("Quantité de café à ajouter (en g) :")
    var ajoutCafe = 0
    do {
      ajoutCafe = readInt()
      if (ajoutCafe < 0) {
        println("La quantité ne peut pas être négative. Veuillez réessayer :")
      }
    } while (ajoutCafe < 0)
    println("Quantité de sucre à ajouter (en g) :")
    var ajoutSucre = 0
    do {
      ajoutSucre = readInt()
      if (ajoutSucre < 0) {
        println("La quantité ne peut pas être négative. Veuillez réessayer :")
      }
    } while (ajoutSucre < 0)

    println("Quantité de lait à ajouter (en ml) :")
    var ajoutLait = 0
    do {
      ajoutLait = readInt()
      if (ajoutLait < 0) {
        println("La quantitée ne peut pas être négative. Veuillez réessayer :")
      }
    } while (ajoutLait < 0)
    coffeeStocks(machineId) += ajoutCafe
    sugarStocks(machineId) += ajoutSucre
    milkStocks(machineId) += ajoutLait

    println("\nMise à jour des stocks effectuée :")
    println(s"Café : ${coffeeStocks(machineId)}g (+${ajoutCafe}g)")
    println(s"Sucre : ${sugarStocks(machineId)}g (+${ajoutSucre}g)")
    println(s"Lait : ${milkStocks(machineId)}ml (+${ajoutLait}ml)")
  }
}

