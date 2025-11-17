import io.StdIn._
import math._
import scala.util.Random
import collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}

class Machine (val id: Int,var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar"=> sugar += amount
      case "coffee"=> coffee += amount
      case _ => println("Ingrédient non reconnu ")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean= {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount =>
        milk -= amount
        true
      case "sugar" if sugar >= amount =>
        sugar -= amount
        true
      case "coffee" if coffee >= amount =>
        coffee -= amount
        true
      case _ => false
    }
  }
}
object Main {
  var machines = new ArrayBuffer[Machine]()
  val filname = "machines.csv"

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("Chargement des machines depuis machines.csv")
    try {
      val fr = Source.fromFile(filname)
      val lines = fr.getLines()
      val machines = new ArrayBuffer[Machine]()

      lines.next()

      while (lines.hasNext) {
        val Array(pincode, laitEnMl, sucreEnGrammes, caffeEnGrammes) = lines.next().split(",")

        val milk = laitEnMl.toInt
        val sugar = sucreEnGrammes.toInt
        val coffee = caffeEnGrammes.toInt

        machines += new Machine(
          id = machines.size + 1,
          pincode = pincode,
          milk = milk,
          sugar = sugar,
          coffee = coffee
        )
        val laitEnLitres = milk / 1000.0

        println("\n Machine" + {
          machines.size
        } + "chargée :")
        println("ID:" + {
          machines.size
        })
        println(f"Lait:${laitEnLitres}%.3fL")
        println("Sucre:" + {
          sugar
        } + "g")
        println("Café:" + {
          coffee
        } + "g")
      }
      fr.close()
      println({
        machines.size
      } + "machines chargées avec succès.")
      machines

    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable.Vérifiez le chemin d'accès et réessayer")
        return machines
      case _: NumberFormatException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme")
        return machines
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println("Sauvegarde des machines dans machines.csv...")
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      writer.close()
    } catch {
      case _: java.io.IOException
      =>
        println("Erreur: Echec de l'écriture dans machines.csv.")
        println("Le fichier peut etre verrouillé ou en lecture seule.")
    }
  }

  val nbMachines = 5
  var machineId = 0

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
    machines = loadcsv(filname)
    if (machines.isEmpty) {
      println("Erreur : Échec du chargement des machines.")
      println("Fermeture du programme")
      return
    }

    var continuer = true
    while (continuer) {
      println("\nNospresso Cafe")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      readInt() match {
        case 1 =>
          println("\nselection machine (1-5)")
          print("> ")
          choisirMachine()
          serveClient(machineId)
        case 2 =>
          println("\nselection machine (1-5)")
          print("> ")
          choisirMachine()
          if (validatePin(machineId)) {
            println("\nMenu Admin:")
            println("1) Voir et mettre à jour les stocks")
            println("2) Modifier le PIN")
            println("3) Retour")
            print("> ")

            readInt() match {
              case 1 => restockMachine(machineId)
              case 2 => updatePin(machineId)
              case 3 => // Retour au menu principal
              case _ => println("Option non valide")
            }
          }
        case 3 =>
          continuer = false
          println("Sauvegarde des données avant la fermeture...")
          savecsv(filname, machines)
          println("Merci de votre visite. Bonne journée!")
        case _ =>
          println("Option non valide")
      }
    }
  }

  def choisirMachine(): Unit = {
    var choixValide = false
    while (!choixValide) {
      try {
        machineId = readInt() - 1
        if (machineId >= 0 && machineId < nbMachines) {
          choixValide = true
        } else {
          println("Veuillez choisir un numéro entre 1 et " + nbMachines)
          print("> ")
        }
      } catch {
        case _: NumberFormatException =>
          println("Veuillez entrer un numéro valide")
          print("> ")
      }
    }
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
    val machine = machines(machineId)
    val quantiteCafe = 8 // 8g de café pour un expresso
    var quantiteSucre = 0

    if (choixsucre == peudesucre) quantiteSucre = 5
    else if (choixsucre == moyendesucre) quantiteSucre = 10
    else if (choixsucre == bcpdesucre) quantiteSucre = 15

    // Vérification des stocks
    if (machine.coffee < quantiteCafe) {
      println("Stock insuffisant : café")
      return
    }
    if (quantiteSucre > 0 && machine.sugar < quantiteSucre) {
      println("Stock insuffisant : sucre")
      return
    }

    var prix = coutexpresso
    if (choixsucre == peudesucre) prix += coutpeusucre
    else if (choixsucre == moyendesucre) prix += coutmoyensucre
    else if (choixsucre == bcpdesucre) prix += coutbcpsucre

    println("Prix total: CHF " + {
      prix
    })
    Twint()

    // Réduction des stocks
    machine.removeIngredient("coffee", quantiteCafe)
    if (quantiteSucre > 0) machine.removeIngredient("sugar", quantiteSucre)

    preparerBoisson("Expresso")
  }

  def preparerCappuccino(machineId: Int, choixsucre: Int): Unit = {
    val machine = machines(machineId)
    val quantiteCafe = 6 // 6g de café pour un cappuccino
    var quantiteLait = 100 // 100ml de lait de base
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

    if (machine.coffee < quantiteCafe) {
      println("Stock insuffisant : café")
      return
    }
    if (machine.milk < quantiteLait) {
      println("Stock insuffisant : lait")
      return
    }
    if (quantiteSucre > 0 && machine.sugar < quantiteSucre) {
      println("Stock insuffisant : sucre")
      return
    }

    if (choixsucre == peudesucre) prix += coutpeusucre
    else if (choixsucre == moyendesucre) prix += coutmoyensucre
    else if (choixsucre == bcpdesucre) prix += coutbcpsucre

    println("Prix total: CHF  " + {
      prix
    })
    Twint()


    machine.removeIngredient("coffee", quantiteCafe)
    machine.removeIngredient("milk", quantiteLait)
    if (quantiteSucre > 0) machine.removeIngredient("sugar", quantiteSucre)

    preparerBoisson("Cappuccino")
  }

  def preparerLatte(machineId: Int): Unit = {
    val machine = machines(machineId)
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

    // Définition des quantités selon la taille
    choixTaille match {
      case 1 => // Petit
        prix = 2.70
        quantiteCafe = 6 // 6g de café
        quantiteLait = 120 // 120ml de lait
      case 2 => // Moyen
        prix = 3.20
        quantiteCafe = 8 // 8g de café
        quantiteLait = 150 // 150ml de lait
      case 3 => // Grand
        prix = 3.70
        quantiteCafe = 12 // 12g de café
        quantiteLait = 200 // 200ml de lait
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
  }

  def preparerBoisson(nomBoisson: String): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    println("Votre " + nomBoisson + "est prêt ! Bonne dégustation !")
  }

  def verifierStocks(cafeDemande: Int, laitDemande: Int = 0, sucreDemande: Int = 0): Boolean = {
    machines(machineId).coffee >= cafeDemande &&
      (laitDemande == 0 || machines(machineId).milk >= laitDemande) &&
      (sucreDemande == 0 || machines(machineId).sugar >= sucreDemande)
  }

  def Twint(): Unit = {
    println("Veuillez payer en utilisant Twint.")
    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    val length = 5
    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString
    println("Votre code Twint : " + codeTwint)
    println("En attente de paiement...")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
  }


  def validatePin(machineId: Int): Boolean = {
    println("\nMode Admin - Machine " + {
      machineId + 1
    })
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN (" + {
        tentatives
      } + "tentatives restantes):")
      val pin = readLine()
      if (machines(machineId).pincode == pin) {
        println("Accès accordé pour la machine " + {
          machineId + 1
        })
        return true
      }
      tentatives -= 1
      if (tentatives > 0) {
        println("Code PIN incorrect. " + {
          tentatives
        } + "tentatives restantes.")
      } else {
        println("Trop de tentatives échouées.")
      }
    }
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
    } while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit))
    machines(machineId).pincode = nouveauPin
    println("\n" + "Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(2000)
  }

  def restockMachine(machineId: Int): Unit = {
    println("Stocks actuels de la machine" + {
      machineId + 1
    })
    println("Poudre de café : " + {
      machines(machineId).coffee
    } + "g")
    println(s"Sucre : " + {
      machines(machineId).sugar
    } + "g")
    println(s"Lait : " + {
      machines(machineId).milk
    } + "ml")

    println("\n" + "Entrez les quantités à ajouter :")

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
    machines(machineId).addIngredient("coffee", ajoutCafe)
    machines(machineId).addIngredient("sugar", ajoutSucre)
    machines(machineId).addIngredient("milk", ajoutLait)

    println("\n" + "Mise à jour des stocks effectuée :")
    println("Café : " + {
      machines(machineId).coffee
    } + "g" + "(+" + {
      ajoutCafe
    } + "g)")
    println("Sucre :" + {
      machines(machineId).sugar
    } + "g" + "(+" + {ajoutSucre} + "g)")
    println("Lait : " + {machines(machineId).milk} + "ml" + "(+" + {ajoutLait} + "ml)")
  }

}
