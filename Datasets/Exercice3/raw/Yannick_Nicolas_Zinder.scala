import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient inconnu: $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
       case _ => println(s"Stock insuffisant pour $ingredient"); false
    }
  }
}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.io.StdIn._

object Main {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    loadcsv("machines.csv")
    var mode: Int = 0

    // Selection du mode
    while (mode != 3) {
      mode = readLine("Veuillez sélectionner votre mode:\n1) Client \n2) Admin  \n3) Quitter \n> ").toInt
      if (mode < 1 || mode > 3) {
        println("Vous n'avez pas sélectionné un mode valide.")
      } else if (mode == 1) {
        serveClient()
      } else if (mode == 2) {
        adminMode()
      }
    }
    println("Merci d'avoir utilisé la machine à café")
    println("Sauvegarde des machines dans machines.csv...")
    savecsv("machines.csv")
  }

  def loadcsv(filename: String): Unit = {
    try {
      println(s"Tentative de chargement du fichier : $filename")
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1) // Ignorer l'en-tête
      println("Fichier ouvert avec succès. Lecture des lignes...")
      for ((line, index) <- lines.zipWithIndex) {
        println(s"Traitement de la ligne $index : $line")
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        machines += new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        println(s"Machine ${index + 1} chargée :")
        println(s"ID: ${index + 1}")
        println(s"Code PIN: $pincode")
        println(f"Lait: ${milk.toInt / 1000.0}%.3fL")
        println(s"Sucre: $sugar g")
        println(s"Café: $coffee g")
      }
      source.close()
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      case e: Exception => println(s"Erreur : ${e.getMessage}")
    }
  }

  def savecsv(filename: String): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: Exception => println(s"Erreur : ${e.getMessage}")
    }
  }

  def validatePin(machineId: Int): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      val enteredPin = readLine("Entrez le code PIN : ")
      if (enteredPin == machines(machineId - 1).pincode) {
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. " + attempts + " tentatives restantes")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  def updatePin(machineId: Int): Unit = {
    if (validatePin(machineId)) {
      var newPin = ""
      do {
        newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      } while (newPin.length != 6 || !newPin.forall(_.isDigit))
      machines(machineId - 1).pincode = newPin
      println("Le code PIN a été mis à jour avec succès.")
    }
  }

  def serveClient(): Unit = {
    val machineId = selectMachine()
    if (machineId >= 1) {
      var commandepossible: Boolean = false
      while (!commandepossible) {
        // Initialisation des variables
        var cafe: Int = 0
        var sucre: Int = 0
        var lait: Int = 0
        var ouiounon: Int = 0
        var boisson: String = ""
        var niveausucre: String = ""
        var niveaulait: String = ""
        var prixtotal: String = ""
        var somme: Double = 0
        var augmentationprixsucre: Double = 0
        var dimcafe: Int = 0
        var dim_sucre: Int = 0
        var dimlait: Double = 0
        val coderandom = scala.util.Random.nextInt(90000)

        // selection type de café
        while (cafe < 1 || cafe > 3) {
          cafe = readLine("Quelle boisson désirez-vous : \n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte  - CHF 2.70 (Petit), CHF 3.20 (moyen), CHF 3.70 (grand) \n> ").toInt
          if (cafe == 1) {
            boisson = "Expresso"
            somme = 2.00
            dimcafe = 8
          } else if (cafe == 2) {
            boisson = "Cappuccino"
            somme = 2.50
            dimcafe = 6
            dimlait = 0.1
          } else if (cafe == 3) {
            boisson = "Latte"
            var taillelatte: Int = 0
            while (taillelatte < 1 || taillelatte > 3) {
              taillelatte = readLine("Quelle taille de Latte désirez-vous ? \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
              if (taillelatte == 1) {
                somme = 2.70
                dimcafe = 6
                dimlait = 0.12
              } else if (taillelatte == 2) {
                somme = 3.20
                dimcafe = 8
                dimlait = 0.15
              } else if (taillelatte == 3) {
                somme = 3.70
                dimcafe = 12
                dimlait = 0.2
              } else {
                println("Veuillez sélectionner un nombre valide ")
              }
            }
          } else {
            println("Veuillez sélectionner un nombre valide")
          }
        }

        // MAJ lait et café
        machines(machineId - 1).removeIngredient("coffee", dimcafe)
        machines(machineId - 1).removeIngredient("milk", (dimlait * 1000).toInt)
        prixtotal = "CHF " + somme

        // sucre
        while (sucre < 1 || sucre > 4) {
          sucre = readLine("Voulez-vous du sucre dans votre café ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          if (sucre == 1) {
            niveausucre = "Pas de sucre"
          } else if (sucre == 2) {
            augmentationprixsucre = 0.10
            somme += augmentationprixsucre
            niveausucre = "Peu (5g)"
            dim_sucre = 5
          } else if (sucre == 3) {
            augmentationprixsucre = 0.20
            somme += augmentationprixsucre
            niveausucre = "Moyen (10g)"
            dim_sucre = 10
          } else if (sucre == 4) {
            augmentationprixsucre = 0.30
            somme += augmentationprixsucre
            niveausucre = "Beaucoup (15g)"
            dim_sucre = 15
          } else {
            println("Veuillez sélectionner un nombre valide")
          }
        }

        machines(machineId - 1).removeIngredient("sugar", dim_sucre)
        prixtotal += "+ CHF " + augmentationprixsucre + " = CHF " + somme

        // Lait, si applicable
        if (cafe == 2 || cafe == 3) {
          while (ouiounon < 1 || ouiounon > 2) {
            ouiounon = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
            if (ouiounon == 1) {
              niveaulait = "Oui"
              var nbdose: Double = 0
              while (nbdose < 1 || nbdose > 3) {
                nbdose = readLine("Combien de dose de lait désirez-vous ? ").toInt
                if (nbdose < 1 || nbdose > 3) {
                  println("Vous pouvez sélectionner maximum 3 doses.")
                }
              }
              machines(machineId - 1).removeIngredient("milk", (nbdose * 50).toInt)
              somme += nbdose * 0.05
            } else {
              niveaulait = "Non"
            }
            if (ouiounon < 1 || ouiounon > 2) {
              println("Veuillez sélectionner un nombre valide")
            }
          }
        }

        // Affichage de la commande et vérification des stocks
        if (machines(machineId - 1).coffee >= dimcafe && machines(machineId - 1).milk >= (dimlait * 1000).toInt && machines(machineId - 1).sugar >= dim_sucre) {
          println("Boisson sélectionnée : " + boisson)
          println("Niveau de sucre : " + niveausucre)
          println("Lait supplémentaire : " + niveaulait)
          println("Prix total : CHF " + somme)
          println("\nVeuillez payer en utilisant Twint")
          println("Votre code de paiement est :" + coderandom)
          println("En attente de paiement...")
          Thread.sleep(2000)
          println(" ")
          println("Paiement confirmé. ")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre " + boisson + " est prêt ! Bonne dégustation ! ")
          println(" ")
          commandepossible = true // Les stocks sont suffisant, on peut faire la commande
        } else {
          if (cafe == 3) {
            if (machines(machineId - 1).coffee < dimcafe) println("Erreur : Quantité de café insuffisante pour la boisson sélectionnée.")
            if (machines(machineId - 1).milk < (dimlait * 1000).toInt) println("Erreur : Quantité de lait insuffisante pour la boisson sélectionnée.")
            if (machines(machineId - 1).sugar < dim_sucre) println("Erreur : Quantité de sucre insuffisante pour la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          } else {
            if (machines(machineId - 1).coffee < dimcafe) println("Erreur : Quantité de café insuffisante pour la boisson sélectionnée.")
            if (machines(machineId - 1).milk < (dimlait * 1000).toInt) println("Erreur : Quantité de lait insuffisante pour la boisson sélectionnée.")
            if (machines(machineId - 1).sugar < dim_sucre) println("Erreur : Quantité de sucre insuffisante pour la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
        }
      }
    }
  }

  def restockMachine(machineId: Int): Unit = {
    if (validatePin(machineId)) {
      machines(machineId - 1).addIngredient("coffee", readLine("Combien de grammes de poudre de café voulez vous rajouter ? ").toInt)
      machines(machineId - 1).addIngredient("sugar", readLine("Combien de grammes de sucre voulez vous rajouter ? ").toInt)
      machines(machineId - 1).addIngredient("milk", (readLine("Combien de litres de lait voulez vous rajouter ? ").toDouble * 1000).toInt)
      println("Les stocks ont été mis à jour avec succès.")
    }
  }

  def adminMode(): Unit = {
    val machineId = selectMachine()
    if (machineId >= 1) {
      println("Mode Admin")
      if (validatePin(machineId)) {
        println("Accès autorisé à la Machine " + machineId + ".")
        println("Stocks actuels :\nPoudre de café: " + machines(machineId - 1).coffee + "g\nSucre: " + machines(machineId - 1).sugar + "g\nLait: " + machines(machineId - 1).milk + "mL")
        val reaprovisionnement = readLine("Souhaitez-vous réapprovisionner le stock ? (true/false)").toBoolean
        if (reaprovisionnement) {
          restockMachine(machineId)
        }
        val updatePinOption = readLine("Souhaitez-vous mettre à jour le code PIN ? (true/false)").toBoolean
        if (updatePinOption) {
          updatePin(machineId)
        }
      }
    }
  }

  def selectMachine(): Int = {
    var machineId = -1
    while (machineId < 1 || machineId > machines.size) {
      machineId = readLine(s"Veuillez sélectionner une machine (1-${machines.size}) > ").toInt
      if (machineId < 1 || machineId > machines.size) {
        println("Veuillez sélectionner un identifiant de machine valide.")
      }
    }
    machineId
  }
}