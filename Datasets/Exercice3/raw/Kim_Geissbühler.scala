import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var continue = true
    var machines = loadcsv("machines.csv")

    while (continue) {
      print("Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter \n> ")
      var modechoisi = readLine().toInt

      // vérification du mode choisi
      while (!(modechoisi == 1 || modechoisi == 2 || modechoisi == 3)) {
        println("Votre sélection n'est pas correcte, veuillez choisir entre : \n1) Client\n2) Admin\n3) Quitter \n>")
        modechoisi = readInt()
      }

      if (modechoisi == 1) {
        val nbMachines = machines.length
        var machine: Int = 0

        while (machine < 1 || machine > nbMachines) {
          machine = readLine(s"Choisissez une machine (1-${nbMachines}) > ").toInt
        }

        var machineId = machine - 1

        if (!serveClient(machines(machineId))) {
          println("Stocks insuffisants : veuillez contacter un administrateur ou changer de machine.")
        }
      }
      else if (modechoisi == 2) {
        val nbMachines = machines.length
        var machine: Int = 0

        while (machine < 1 || machine > nbMachines) {
          machine = readLine(s"Choisissez une machine (1-${nbMachines}) > ").toInt
        }
        var machineId = machine - 1

        if (validatePin(machines(machineId))) {
          var action = readLine(" \nQue souhaitez vous faire ? \n1) Modifier le code PIN\n2) Réapprovisionner les stocks \n> ").toInt
          while (action != 1 && action != 2) {
            action = readLine("Votre selection n'est pas correcte, choisissez entre 1) et 2) > ").toInt - 1
          }

          if (action == 1) {
            udpatePin(machines(machineId))
          }
          if (action == 2) {
            restockMachine(machines(machineId))
          }
        }
        else {
          continue = false
          println("Trop de tentatives. Fin du programme.")
        }
      }
      else {
        continue = false
      }
    }

    savecsv("machines.csv", machines)
  }

  def validatePin(machine: Machine): Boolean = {
    // Valider code PIN d'une des machines
    var nombretentatives = 3

    while (nombretentatives > 0) {
      println("Entrer le code PIN :  \n> ")
      val PIN = readLine()
      if (PIN == machine.pincode) {
        print("Accès accordé à la Machine " + (machine.id) + ".")
        return true
      } else {
        nombretentatives -= 1
        println("Code PIN incorrect. " + nombretentatives + " tentative(s) restante(s).")
      }
    }

    return false
  }

  // Mettre "Entrez un nouveau code PIN à 6 chiffres > "
  def udpatePin(machine: Machine): Unit = {
    var continuer = true
    while (continuer) {
      println("Mise à jour du code PIN pour la Machine " + (machine.id) + " (à 6 chiffres).")
      var nouveauPin = readLine("> ")
      if (nouveauPin.length == 6) {
        continuer = false
        machine.pincode = nouveauPin
        println("Le code PIN a été mis à jour avec succès. \nRetour au menu principal...")
      }
    }
  }


  def serveClient(machine: Machine): Boolean = {
    val prixExpresso = 2.00
    val prixCapuccino = 2.50
    val prixLattepetit = 2.70
    val prixLattemoyen = 3.20
    val prixLattegrand = 3.70

    val prixPeuSucre = 0.10
    val prixMoyenSucre = 0.20
    val prixBeaucoupSucre = 0.30

    val prixLaitsupplement = 0.05
    val quantiteLaitsupplement = 50

    val quantiteCafeExpresso = 8
    val quantiteCafeCapuccino = 6
    val quantiteCafeLattePetit = 6
    val quantiteCafeLatteMoyen = 8
    val quantiteCafeLatteGrand = 12

    val quantiteLaitCapuccino = 100
    val quantiteLaitLattePetit = 120
    val quantiteLaitLatteMoyen = 150
    val quantiteLaitLatteGrand = 200

    val quantitePeuSucre = 5
    val quantiteMoyenSucre = 10
    val quantiteBeaucoupSucre = 15

    var nomBoisson = ""
    var prix = 0.0
    var quantiteCafe = 0
    var quantiteLait = 0
    var nomSucre = ""
    var quantiteSucre = 0
    var prixSucre = 0.0
    var quantiteLaitSupp = 0
    var prixLaitSupp = 0.0
    var nomLaitSupp = ""

    println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ")
    var boissonchoisie = readInt()

    while (!(boissonchoisie == 1 || boissonchoisie == 2 || boissonchoisie == 3)) {
      println("Votre sélection n'est pas correcte, veuillez chosir entre :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ")
      boissonchoisie = readInt()
    }

    if (boissonchoisie == 1) {
      nomBoisson = "Expresso"
      prix = prixExpresso
      quantiteCafe = quantiteCafeExpresso
    } else if (boissonchoisie == 2) {
      nomBoisson = "Capuccino"
      prix = prixCapuccino
      quantiteCafe = quantiteCafeCapuccino
      quantiteLait = quantiteLaitCapuccino
    } else {
      println("Veuillez sélectionner la taille de votre Latte : \n1) Latte Petit\n2) Latte Moyen\n3) Latte Grand")
      var taillelatte = readLine("> ").toInt

      while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
        print("Votre sélection n'est pas correcte, veuillez choisir entre : \n1) Latte Petit \n2) Latte Moyen \n3) Latte Grand \n> ")
        taillelatte = readInt()
      }

      if (taillelatte == 1) {
        nomBoisson = "Latte Petit"
        prix = prixLattepetit
        quantiteCafe = quantiteCafeLattePetit
        quantiteLait = quantiteLaitLattePetit
      } else if (taillelatte == 2) {
        nomBoisson = "Latte Moyen"
        prix = prixLattemoyen
        quantiteCafe = quantiteCafeLatteMoyen
        quantiteLait = quantiteLaitLatteMoyen
      } else {
        nomBoisson = "Latte Grand"
        prix = prixLattegrand
        quantiteCafe = quantiteCafeLatteGrand
        quantiteLait = quantiteLaitLatteGrand
      }
    }

    println("Quel niveau de sucre souhaitez-vous ? : \n1) Sans sucre \n2) Un peu de sucre (5g) - 0.10 CHF \n3) Moyen (10g) - 0.20 CHF \n4) Beaucoup (15g) - 0.30 CHF \n>")
    var niveauSucre = readInt()

    if (niveauSucre == 1) {
      nomSucre = "Pas de sucre"
    } else if (niveauSucre == 2) {
      nomSucre = "Peu"
      quantiteSucre = quantitePeuSucre
      prixSucre = prixPeuSucre
    } else if (niveauSucre == 3) {
      nomSucre = "Moyen"
      quantiteSucre = quantiteMoyenSucre
      prixSucre = prixMoyenSucre
    } else {
      nomSucre = "Beaucoup"
      quantiteSucre = quantiteBeaucoupSucre
      prixSucre = prixBeaucoupSucre
    }

    if (boissonchoisie == 2 || boissonchoisie == 3) {
      print("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ")
      val Laitsupplement = readInt()

      if (Laitsupplement == 1) {
        var doseLaitSupp = 0
        nomLaitSupp = "Oui"

        while (doseLaitSupp < 1 || doseLaitSupp > 3) {
          print("Combien de doses ? (Maximum 3 doses) \n> ")
          doseLaitSupp = readInt()

          quantiteLaitSupp = doseLaitSupp * quantiteLaitsupplement
          prixLaitSupp = doseLaitSupp * prixLaitsupplement
        }
      }
      else {
        nomLaitSupp = "Non"
      }
    }

    if (machine.coffee < quantiteCafe) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson séléctionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    }
    if (machine.sugar < quantiteSucre) {
      println("Erreur : Quantité de sucre insuffisante pour le niveau de sucre séléctionné. \nVeuillez choisir un autre niveau de sucre.")
      return false
    }
    if (machine.milk < quantiteLait + quantiteLaitSupp) {
      println("Erreur : Quantité de lait insuffisante pour la dose de lait supplémentaire séléctionnée. \nVeuillez choisir un autre niveau de supplément de lait.")
      return false
    }

    var prixFinalBoisson = prix + prixSucre + prixLaitSupp

    println("Boisson sélectionnée : " + nomBoisson + "\nNiveau de sucre : " + nomSucre + "(" + quantiteSucre + "g)")
    if (boissonchoisie == 2 || boissonchoisie == 3) {
      println("Lait en supplément : " + nomLaitSupp)
    }
    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix, prixSucre, prixLaitSupp, prixFinalBoisson)

    // Paiement
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase

    println("Veuillez payer en utilisant Twint")
    println("Votre code de paiement est :" + codeTwint)
    println("En attente de la validation du paiement...")

    Thread.sleep(3000)

    println("Paiement confirmé")
    println("Préparation de votre boisson...")
    println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

    machine.removeIngredient("coffee", quantiteCafe)
    machine.removeIngredient("milk", quantiteLait + quantiteLaitSupp)
    machine.removeIngredient("sugar", quantiteSucre)

    return true
  }

  def restockMachine(machine: Machine): Unit = {
    var continue = true
    println("Accès autorisé. \n\nStocks : \n")
    printf("Poudre de café : %d g\n", machine.coffee)
    printf("Lait : %.2f L\n", machine.milk / 1000.0)
    printf("Sucre : %d g\n", machine.sugar)
    println("Réapprovisionnement des stocks...")
    println("Ajout :")

    val coffee = readLine("Quantité de café à ajouter : ").toInt
    val sugar = readLine("Quantité de sucre à ajouter : ").toInt
    val laitenlitres = readLine("Quantité de lait à ajouter (L):").toDouble

    machine.milk += (laitenlitres * 1000).toInt
    machine.coffee += coffee
    machine.sugar += sugar
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var machines = new ArrayBuffer[Machine]()
    try {
      val file = Source.fromFile(filename)
      val ligne = file.reset.getLines()

      var lignesui = ligne.next()

      println("Lecture du fichier ")
      var counter = 0;
      while (ligne.nonEmpty) {
        counter += 1
        lignesui = ligne.next()
        val arr = lignesui.split(",")

        val m = new Machine(counter, arr(0), arr(1).toInt, arr(2).toInt, arr(3).toInt)
        machines += m

        println(s"Machine $counter chargée : \n${m.toString()}")
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")
      return machines
    }
    catch {
      case _ =>
        println("Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return new ArrayBuffer[Machine]()
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println(s"Sauvegarde de ${machines.length} machines dans ${filename}")
      var fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (m <- machines) {
        fw.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      fw.close()
      println("Fichier sauvegardé avec succès :)")
    }
    catch {
      case _ => println("Error while saving file " + filename)
    }
  }
}


class Machine(
               val id: Int,
               var pincode: String,
               var milk: Int,
               var sugar: Int,
               var coffee: Int,
             ) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    }
    else if (ingredient == "sugar") {
      sugar += amount
    }
    else if (ingredient == "coffee") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk") {
      milk -= amount
      return true
    }
    else if (ingredient == "sugar") {
      sugar -= amount
      return true
    }
    else if (ingredient == "coffee") {
      coffee -= amount
      return true
    }
    return false
  }

  override def toString: String = {
    var milkL: Double = (milk / 1000).toDouble
    return s"\tID: $id\n\tCode PIN: $pincode\n\tLait: ${milk.toDouble / 1000}l\n\tSucre: ${sugar}g\n\tCafé: ${coffee}g"
  }
}
