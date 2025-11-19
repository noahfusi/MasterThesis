import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val FICHIER = "machines.csv"

    var machines = loadcsv(FICHIER)
    var continuer = true

    if (machines == null) {
      println("Erreur du chargement ou de la sauvegarde des machines.\nFermeture du programme")
      continuer = false
    }

    val nbMachines = if (machines != null) machines.length else 0

    while (continuer) {
      println(
        "Nospresso Cafe\nVeuillez sélectionner votre mode:\n 1) Client\n 2) Admin\n 3) Quitter"
      )
      var mode: Int = readLine("> ").toInt

      if (mode == 1) {
        var machine =
          readLine(s"Choisissez une machine (1-${nbMachines}) >").toInt - 1

        while (machine < 0 || machine > nbMachines - 1) {
          machine = readLine(
            s"Votre sélection n'est pas correcte, veuillez sélectionner une machine entre 1 et ${nbMachines}\n >"
          ).toInt - 1
        }

        println(s"Vous avez sélectionné la machine ${machine + 1}")
        serveClient(machines(machine))
      } else if (mode == 2) {
        var machineId =
          readLine(s"Choisissez une machine (1-${nbMachines}) >").toInt - 1

        while (machineId < 0 || machineId > nbMachines - 1) {
          machineId = readLine(
            s"Votre sélection n'est pas correcte, veuillez sélectionner une machine entre 1 et ${nbMachines} \n >"
          ).toInt - 1
        }

        if (validatePin(machines(machineId))) {
          println(s"Accès accordé à la machine ${machineId + 1}")

          var action = readLine(
            "Que souhaitez-vous sélectionner ? \n 1) Réaprovisionner le stock \n 2) Mettre à jour le code PIN \n > "
          ).toInt

          while (action != 1 && action != 2) {
            action = readLine(
              "Votre sélection n'est pas correcte, veuillez choisir une machine entre 1 et 5 \n >"
            ).toInt
          }

          if (action == 1) {
            restockMachine(machines(machineId))

          } else if (action == 2) {
            updatePin(machines(machineId))
          }
        } else {
          println("Trop de tentative échouées. Fin du programme. \n")
          continuer = false
        }
      } else if (mode == 3) {
        continuer = false
      } else {
        println("Entrée incorrecte")
      }
    }

    savecsv(FICHIER, machines)
  }

  def validatePin(machine: Machine): Boolean = {
    // valider codes PIN d'une des machines
    var tentative = 0
    val max_tentative = 3

    while (tentative < max_tentative) {
      val entrerPIN = readLine(
        s"Veuillez entrer le code PIN correspondant à la machine sélectionnée : " + (machine.id) + " \n >"
      )

      if (entrerPIN == machine.pincode) {
        println("Code PIN correct, Accès accordé.")
        return true
      } else {
        tentative += 1
        println(
          s"Code PIN incorrect. Tentative ${tentative} sur ${max_tentative}."
        )
      }
    }
    return false
  }

  def updatePin(machine: Machine): Unit = {
    var continuer = true
    while (continuer) {
      println("Veuillez entrer le nouveau code PIN (6 chiffres) : ")
      var nouveauPin = readLine(">")
      if (nouveauPin.length == 6) {
        continuer = false
        machine.pincode = nouveauPin
        println("Le code Pin a été mis à jour avec succès.")
      }
    }
  }

  def serveClient(
                   machine: Machine
                 ): Boolean = {
    // mettre toutes les valeurs
    var prixdosesucre = 0.10
    var prixdoselait = 0.0
    var prixexpresso = 2.00
    var prixcappuccino = 2.50
    var prixlattepetit = 2.70
    var prixlattemoyen = 3.20
    var prixlattegrand = 3.70

    var taillelatte: Int = 0
    var prixboisson = 0.0
    var besoincafe: Int = 0
    var besoinsucre: Int = 0
    var besoinlait = 0
    var nomboisson: String = ""

    var nomsucre = ""
    var sucre: Int = 0
    var prixsucre = 0.0
    var sans_sucre = 0
    var peu_sucre = 5
    var moyen_sucre = 10
    var beaucoup_sucre = 15

    println(
      "Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"
    )

    var boisson: Int = 0
    boisson = readLine("> ").toInt

    while (boisson < 1 || boisson > 3) {
      println(
        "Votre sélection n'est pas correcte, veuillez choisir entre:\n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"
      )
      boisson = readLine("> ").toInt
    }

    if (boisson == 1) {
      nomboisson = "Expresso"
      prixboisson = prixexpresso
      besoincafe = 8
      besoinsucre = (sucre - 1) * 5
      println("Boisson sélectionnée : Expresso")
    } else if (boisson == 2) {
      nomboisson = "Cappuccino"
      prixboisson = prixcappuccino
      besoincafe = 6
      besoinsucre = (sucre - 1) * 5
      besoinlait = 100
      println("Boisson sélectionnée : Cappuccino")
    } else {
      var taillelatte = 0
      println(
        "Choisissez la taille de votre Latte : \n 1) Petit - CHF 2.70 \n 2) Moyen - CHF 3.20 \n 3) Grand - CHF 3.70)"
      )
      taillelatte = readLine("> ").toInt

      while (taillelatte < 1 || taillelatte > 3) {
        println(
          "Votre sélection n'est pas correcte, veuillez choisir entre : \n 1) Latte Petit \n 2) Latte Moyen \n 3) Latte Grand "
        )
        taillelatte = readLine("> ").toInt
      }

      if (taillelatte == 1) {
        nomboisson = "Latte Petit"
        prixboisson = prixlattepetit
        besoincafe = 6
        besoinsucre = (sucre - 1) * 5
        besoinlait = 120
        println("Boissons sélectionnée : Latte (Petit)")
      } else if (taillelatte == 2) {
        nomboisson = "Latte Moyen"
        prixboisson = prixlattemoyen
        besoincafe = 8
        besoinsucre = (sucre - 1) * 5
        besoinlait = 150
        println("Boisson sélectionnée : Latte (Moyen)")
      } else {
        nomboisson = "Latte Grand"
        prixboisson = prixlattegrand
        besoincafe = 12
        besoinsucre = (sucre - 1) * 5
        besoinlait = 200
        println("Boisson sélectionnée : Latte (Grand)")
      }
    }

    println(
      "Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30"
    )

    sucre = readLine("> ").toInt

    while (sucre < 1 || sucre > 4) {
      println(
        "Votre sélection n'est pas correcte. Veuillez choisir entre : \n1) Sans sucre\n2) Peu\n3) Moyen\n4) Beaucoup "
      )
      sucre = readLine("> ").toInt
    }

    if (sucre == 1) {
      nomsucre = "Sans sucre"
      besoinsucre = sans_sucre
      prixsucre = 0.0
    } else if (sucre == 2) {
      nomsucre = "Peu"
      besoinsucre = peu_sucre
      prixsucre = prixdosesucre
    } else if (sucre == 3) {
      nomsucre = "Moyen"
      besoinsucre = moyen_sucre
      prixsucre = prixdosesucre * 2
    } else if (sucre == 4) {
      nomsucre = "Beaucoup"
      besoinsucre = beaucoup_sucre
      prixsucre = prixdosesucre * 3
    }

    var nomchoixlaitsupp: String = ""
    var lait = 0
    var doselait = 0
    var prixlait = 0.0
    var laitsupplement = 0
    var prixlaitsupplement: Double = 0

    if (boisson == 2 || boisson == 3) {
      println(
        "Souhaitez-vous ajouter du lait en supplément ? \n1) Oui\n2) Non"
      )
      lait = readLine("> ").toInt

      while (lait < 1 || lait > 2) {
        println("Erreur. Veuillez sélectionner votre choix (1 ou 2).")
        lait = readLine("> ").toInt
      }

      if (lait == 1) {
        nomchoixlaitsupp = "Oui"
        println(
          "Combien de dose ? max 3 doses, CHF 0.05 une dose de lait supplémentaire."
        )

        doselait = readLine("> ").toInt

        while (doselait < 1 || doselait > 3) {
          println("Erreur. Veuillez sélectionner une valeur entre 1 et 3.")
          doselait = readLine("> ").toInt
        }

        laitsupplement = doselait * 50
        prixlaitsupplement = doselait * 0.05
      } else {
        nomchoixlaitsupp = "Non"
        laitsupplement = 0
        prixlaitsupplement = 0
      }
    }

    // vérification des stocks
    if (machine.coffee < besoincafe) {
      println(
        "Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
      )
      return false
    }
    if (machine.milk < besoinlait + laitsupplement) {
      println(
        "Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson."
      )
      return false
    }
    if (machine.sugar < besoinsucre) {
      println(
        "Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
      )
      return false
    }

    machine.removeIngredient("coffee", besoincafe)
    machine.removeIngredient("milk", besoinlait + laitsupplement)
    machine.removeIngredient("sugar", besoinsucre)

    var prixfinalboisson = prixboisson + prixsucre + prixlaitsupplement

    println(
      "Boisson sélectionnée : " + nomboisson + "\nNiveau de sucre : " + nomsucre + "(" + besoinsucre + "g)"
    )
    if (boisson == 2 || boisson == 3) {
      println("Lait en supplément : " + nomchoixlaitsupp)
    }

    printf(
      "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n",
      prixboisson,
      prixsucre,
      prixlaitsupplement,
      prixfinalboisson
    )

    // Payement
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est :" + codeTwint)
    println("\n")
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté..")
    println("Préparation de votre boisson...")
    println("Votre " + nomboisson + " est prêt ! Bonne dégustation !")

    return true
  }

  def restockMachine(
                      machine: Machine
                    ): Unit = {
    println("Stocks:")

    println(s"Poudre de café : ${machine.coffee}g")
    println(s"Sucre: ${machine.sugar}g")
    println(s"Lait : ${machine.milk.toDouble / 1000.0}l")

    val coffee = readLine("Quantité de café à ajouter : ").toInt
    val sugar = readLine("Quantité de sucre à ajouter : ").toInt
    val laitenlitres = readLine("Quantité de lait à ajouter : ").toInt

    machine.addIngredient("coffee", coffee)
    machine.addIngredient("sugar", sugar)
    machine.addIngredient("milk", laitenlitres)
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      println("Lecture du fichier")
      var conteur = 0
      val machines = ArrayBuffer[Machine]()

      var ligne = Source.fromFile(filename).getLines()
      var ligne2 = ligne.next()

      while (ligne.nonEmpty) {
        conteur += 1
        ligne2 = ligne.next()
        val arr = ligne2.split(",")
        val m =
          new Machine(conteur, arr(0), arr(1).toInt, arr(2).toInt, arr(3).toInt)
        machines += m
        println(s"Machine $conteur chargée :\n\tID: ${m.id}\n\tCode PIN: ${m.pincode}\n\tLait: ${m.milk.toDouble / 1000}l\n\tSucre: ${m.sugar}g\n\tCafé: ${m.coffee}g\n")
      }
      return machines
    } catch {
      case _ =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        return null
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println(s"Sauvegarde de ${machines.length} machines dans ${filename}")
      var fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR.COFFEE")
      for (m <- machines) {
        fw.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      fw.close()
      println("Fichier sauvegardé avec succès")
    } catch {
      case _ =>
        println(s"Erreur : Échec de l'écriture dans ${filename}.\nLe fichier peut être verrouillé ou en lecture seule.")
    }
  }
}

class Machine(
               var id: Int,
               var pincode: String,
               var milk: Int,
               var sugar: Int,
               var coffee: Int
             ) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (amount > 0) {
      if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      } else if (ingredient == "coffee") {
        coffee += amount
      } else {
        println("L'ingrédient n'existe pas")
      }
    } else if (amount < 0) {
      println("La quantité doit être supérieure à 0")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (amount == 0) {
      return true
    }
    else if (amount > 0) {
      if (ingredient == "milk") {
        if (milk >= amount) {
          milk -= amount
          return true
        } else {
          println("Pas assez de lait en stock")
          return false
        }
      }

      if (ingredient == "sugar") {
        if (sugar >= amount) {
          sugar -= amount
          return true
        } else {
          println("Pas assez de sucre en stock")
          return false
        }
      }

      if (ingredient == "coffee") {
        if (coffee >= amount) {
          coffee -= amount
          return true
        } else {
          println("Pas assez de café en stock")
          return false
        }
      }

      println("L'ingrédient n'existe pas")
      return false
    }
    else {
      println("La quantité doit être supérieure à 0")
      return false
    }
  }
}
