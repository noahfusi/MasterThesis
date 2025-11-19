import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}


object Main {
  def main(args: Array[String]): Unit = {

    var continuer = true
    var machines = loadcsv("machines.csv")

    while (continuer) {

      var mode = readLine("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readInt()
      }

      if (mode == 1) {
        val nbMachines = machines.length

        var machine = 0
        var commandevalidee = false

        while (!commandevalidee) {
          machine = readLine("Machine sélectionnée (1-5) > ").toInt
          while (machine < 1 || machine > nbMachines) {
            println("Machine sélectionnée (1-5) > ")
            machine = readInt()
          }
          val machineId = machine - 1

          commandevalidee = serveClient(machines(machineId))

          if (!commandevalidee) {
          }
        }
        savecsv("machines.csv", machines)

      } else if (mode == 2) {
        val nbMachines = machines.length
        var adminchoix = readLine("\nSéléctionnez votre choix en mode Administarteur :\n1) Réapprovisionner le stock ou\n2) Mettre à jour le code PIN ?\n> ").toInt

        while(!(adminchoix==1 || adminchoix==2)){
          println("\nSéléctionnez votre choix en mode Administarteur :\n1) Réapprovisionner\n2) Mettre à jour le code PIN\n> ")
          adminchoix = readInt()
        }

        var machine = readLine("Machine sélectionnée (1-5) > ").toInt
        while(machine < 1 || machine > nbMachines) {
          println("Machine sélectionnée (1-5) > ")
          machine = readInt()
        }
        var machineId = machine - 1
        if (!(machine < 1 || machine > nbMachines)) {
          if (validatePin(machines(machineId))) {
            println("Accès accordé à la Machine " + machine)

            if (adminchoix == 1) {
              restockMachine(machines(machineId))
              savecsv("machines.csv", machines)
            } else if (adminchoix == 2) {
              updatePin(machines(machineId))
              savecsv("machines.csv", machines)
            }

          } else {
            println("Trop de tentatives échouées. Fin du programme.\n")
          }
        }
      } else if (mode == 3) {
        continuer = false
      }
    }
    savecsv("machines.csv", machines)
  }

  def serveClient(machine: Machine): Boolean = {

    val prixexpresso = 2.00
    val prixcappuccino = 2.50
    val prixlattePetit = 2.70
    val prixlatteMoyen = 3.20
    val prixlatteGrand = 3.70

    val cafeexpresso = 8
    val cafecappuccino = 6
    val cafelattePetit = 6
    val cafelatteMoyen = 8
    val cafelatteGrand = 12

    val laitcappuccino = 100
    val laitlattePetit = 120
    val laitlatteMoyen = 150
    val laitlatteGrand = 200

    var choixboisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ").toInt
    while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3)) {
      println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ")
      choixboisson = readInt()
    }

    var nomboisson = ""
    var prixboisson = 0.0
    var cafebesoin = 0
    var laitbesoin = 0

    if (choixboisson == 1) {
      nomboisson = "Expresso"
      prixboisson = prixexpresso
      cafebesoin = cafeexpresso
    } else if (choixboisson == 2) {
      nomboisson = "Cappuccino"
      prixboisson = prixcappuccino
      cafebesoin = cafecappuccino
      laitbesoin = laitcappuccino
    } else if (choixboisson == 3) {
      var taillelate = readLine("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ").toInt
      while (!(taillelate == 1 || taillelate == 2 || taillelate == 3)) {
        print("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ")
        taillelate = readInt()
      }
      if (taillelate == 1) {
        nomboisson = "Latte(Petit)"
        prixboisson = prixlattePetit
        cafebesoin = cafelattePetit
        laitbesoin = laitlattePetit
      } else if (taillelate == 2) {
        nomboisson = "Latte(Moyen)"
        prixboisson = prixlatteMoyen
        cafebesoin = cafelatteMoyen
        laitbesoin = laitlatteMoyen
      } else {
        nomboisson = "Latte(Grand)"
        prixboisson = prixlatteGrand
        cafebesoin = cafelatteGrand
        laitbesoin = laitlatteGrand
      }
    }

    var choixsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
    while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4)) {
      println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
      choixsucre = readInt()
    }
    var prixsucre = 0.0
    var sucrebesoin = 0
    var nomsucre = ""

    if (choixsucre == 2) {
      prixsucre = 0.10
      sucrebesoin = 5
      nomsucre = "Peu (5g)"
    } else if (choixsucre == 3) {
      prixsucre = 0.20
      sucrebesoin = 10
      nomsucre = "Moyen (10g)"
    } else if (choixsucre == 4) {
      prixsucre = 0.30
      sucrebesoin = 15
      nomsucre = "Beaucoup (15g)"
    } else if (choixsucre ==1){
      nomsucre = "Sans sucre"
    }
    var laitsupplement = 0
    var prixlaitsupplement = 0.0
    var nomchoixlaitsupp = ""

    if (choixboisson == 2 || choixboisson == 3) {
      var choixlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
      while (!(choixlait == 1 || choixlait == 2)) {
        println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        nomchoixlaitsupp = "Oui"
        var choixdose = readLine("Combien de dose ?\n> ").toInt
        while (!(choixdose == 1 || choixdose == 2 || choixdose == 3)) {
          println("Combien de dose ?\n> ")
          choixdose = readInt()
        }
        laitsupplement = choixdose * 50
        prixlaitsupplement = choixdose * 0.05

        if (choixdose ==1 && machine.milk >= laitbesoin + 0.05){
          choixdose = 1
          laitsupplement = 50
          prixlaitsupplement = 0.05
        } else if (choixdose == 2 && machine.milk >= laitbesoin + 0.10) {
          choixdose = 2
          laitsupplement = 100
          prixlaitsupplement = 0.10
        } else if (choixdose == 3 && machine.milk >= laitbesoin + 0.15) {
          choixdose = 3
          laitsupplement = 150
          prixlaitsupplement = 0.15
        }

      } else if(choixlait==2){
        nomchoixlaitsupp = "Non"
      }
    }

    if (machine.coffee < cafebesoin) {
      println("Boisson séléctionnée : " + nomboisson)
      println("Niveau de sucre: " + nomsucre)
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    } else if (machine.milk < laitbesoin + laitsupplement) {
      println("Boisson séléctionnée : " + nomboisson)
      println("Niveau de sucre : " + nomsucre)
      println("Lait en supplément :" + nomchoixlaitsupp)
      println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    } else if (machine.sugar < sucrebesoin) {
      println("Boisson séléctionée : " + nomboisson)
      println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    } else {
      val total = prixboisson + prixsucre + prixlaitsupplement
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n\n", prixboisson, prixsucre, prixlaitsupplement, total)

      val chaine = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var codepaiementtwint = ""
      println("Veuillez payer en utilisant Twint.")
      for (i <- 1 to 5) {
        val caractere = (Math.random() * chaine.length).toInt
        codepaiementtwint = codepaiementtwint + chaine(caractere)
      }
      println("Votre code de paiement est : " + codepaiementtwint + "\n(En attente de paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé.\nPréparation de votre boisson...")
      println("Votre " + nomboisson + " est prêt ! Bonne dégustation !\n")

      machine.removeIngredient("coffee", cafebesoin)
      machine.removeIngredient("milk", (laitbesoin + laitsupplement))
      machine.removeIngredient("sugar", sucrebesoin)

      return true
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN :")
      val codepin = readLine()
      if (codepin == machine.pincode) {
        return true
      } else {
        tentatives -= 1
        println("Code PIN incorrect. " + tentatives + " tentative(s) restante(s).")
      }
    }
    false
  }

  def updatePin(machine: Machine): Unit = {
    println("Mise à jour pour du code PIN pour la machine " + machine.id)
    println("Entrez un nouveau code PIN à 6 chiffres > ")
    val nouveauPIN = readLine()
    if (nouveauPIN.length == 6 && nouveauPIN.forall(_.isDigit)) {
      machine.pincode = nouveauPIN
      println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
    } else {
      println("Le code PIN n'a pas été mis à jour.\nRetour au menu principal\n")
    }
  }

  def restockMachine(machine: Machine): Unit = {
    printf("Niveaux de stock actuels :\nPoudre de café : %d g\nSucre : %d g\nLait : %.2f L\n\n", machine.coffee, machine.sugar, machine.milk / 1000.0)

    println("Entrez les quantités à ajouter : ")
    println("Poudre de café > ")
    machine.coffee += readInt()
    println("Sucre > ")
    machine.sugar += readInt()
    println("Lait > ")
    val laitenlitres = readDouble()
    machine.milk += (laitenlitres * 1000).toInt

    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
      var machines = new ArrayBuffer[Machine]()
      try {
        val file = Source.fromFile(filename)
        val ligne = file.reset.getLines()
        var lignesuivante = ligne.next()

        println("Lecture du fichier")
        var counter = 0
        while (ligne.nonEmpty) {
          counter += 1
          lignesuivante = ligne.next()
          val arr = lignesuivante.split(",")

          val m = new Machine(counter, arr(0), arr(1).toInt, arr(2).toInt, arr(3).toInt)
          machines += m

          println("Machine " + counter + " chargée : \n" + m.printMachinechargee())
        }
        println(machines.length + " machine(s) chargée(s) avec succès.")
        return machines
      }
      catch {
        case _ =>
          println("Erreur : Fichier" + filename + " introuvable. Vérifiez le chemin d’accès et réessayez.")
          return new ArrayBuffer[Machine]()
      }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        println("Sauvegarde de " + machines.length + " machines dans " + filename)
        var fw = new PrintWriter(filename)
        fw.print("")
        fw.println("PINCODE,MILK,SUGAR,COFFEE")
        for(m <- machines) {
          fw.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
        }
        fw.close()
        println("Fichier sauvegardé avec succès")
      } catch {
        case _ =>
          println("Erreur de chargement du fichier " + filename)
      }
  }
}

class Machine(
               val id: Int,
               var pincode: String,
               var milk: Int,
               var sugar: Int,
               var coffee: Int
             ) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk") {
      milk -= amount
      return true
    } else if (ingredient == "sugar") {
      sugar -= amount
      return true
    } else if (ingredient == "coffee") {
      coffee -= amount
      return true
    }
    false
  }

  def printMachinechargee() : Unit = {
    val s = "Machine " + id + " chargée\nCode PIN: " + pincode + "\nLait: "+ milk + "\nSucre: " + sugar + "\nCafé: " + coffee
    println(s)
  }
}