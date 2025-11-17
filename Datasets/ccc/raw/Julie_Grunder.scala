import io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    try {
      if (ingredient == "lait") {
        milk += amount
      } else if (ingredient == "sucre") {
        sugar += amount
      } else if (ingredient == "café") {
        coffee += amount
      }
    }
    catch {
      case _=> println("Ingrédient non reconnu")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    try {
      if (ingredient == "lait") {
        if (milk >= amount) {
          milk -= amount
        }
      } else if (ingredient == "sucre") {
        if (sugar >= amount) {
          sugar -= amount
        }
      } else if (ingredient == "café") {
        if (coffee >= amount) {
          coffee -= amount
        }
      }
      true
    }
    catch {
      case _=> println("Erreur")
        return false
    }
  }

  def Affichage(): Unit = {
    println("Machine " + id + " chargée :\n" +
      "ID : " + id + "\n" +
      "Code PIN : " + pincode + "\n" +
      "Lait : " + milk + "\n" +
      "Sucre : " + sugar + "\n" +
      "Café : " + coffee + "\n") //+
      //machine.length + "machine(s) chargée(s) avec succès.")
  }
}

//Gestion du fichier CSV
object CSV {
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var machines = ArrayBuffer[Machine]()
    try {
      val file = Source.fromFile(filename)
      val ligne = file.getLines()
      if (! ligne.isEmpty) {
        ligne.next()
      }
      var id = 1
      for (line <- ligne) {
        val index = line.split(",")
        var pincode = index(0).toString
        var milk = index(1).toInt
        var sugar = index(2).toInt
        var coffee = index(3).toInt
        var machine = new Machine(id, pincode, milk, sugar, coffee)
        machines += machine
        machine.Affichage()
        id += 1
      }
      file.close()
      println(machines.length + " machine(s) chargée(s) avec succès.")
    }
    catch {
      case _=> println("Erreur : pas de fichier machine.csv")
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(new FileWriter("machines.csv", true))
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      fw.close()
      println("Données sauvegardées avec succès")
    }
    catch {
      case _=> println("Échec de modification du fichier machines.csv ")
    }
  }
}

//Main : programme principal
object Main {
  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = CSV.loadcsv(filename)

    var structureProgramme = true
    var mode = 0
    while (structureProgramme) {
      do {
        try {
          mode = readLine("\n\tNospresso Café\nVeuillez sélectionnez votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt
          if (mode != 1 && mode != 2 && mode != 3) {
            println("Veuillez svp entrer une valeur valide.")
          }
        }
        catch {
          case _=> println("Entrée invalide : veuillez entrer un nombre entier.")
            mode = 0 // Réinitialiser `mode` pour rester dans la boucle
        }
      } while (mode != 1 && mode != 2 && mode != 3)
        if (mode == 1) { // Mode Client
          println("Mode client sélectoinné")
          println("Sélectionnez une machine (ID) :")
          machines.foreach(_.Affichage())
          val id = readLine(">  ").toInt
          if (id > 0 && id <= machines.length) {
            serveClient(machines(id - 1))
          } else {
            println("ID invalide.")
          }
        } else if (mode == 2) { // Mode Admin
          println("Sélectionnez une machine (ID) :")
          machines.foreach(_.Affichage())
          val id = readLine(">  ").toInt
          if (id > 0 && id <= machines.length) {
            if (validatePin(machines(id - 1))) {
              modeAdmin(machines(id - 1))
            } else {
              println("Accès refusé.")
            }
          } else {
            println("ID invalide.")
          }
        } else {
        println("Sauvegarde des données...")
        CSV.savecsv(filename, machines)
        println("Programme terminé.")
        structureProgramme = false
      }
    }

  }

  def validatePin(machine: Machine): Boolean = {
    val tentativesMax = 3
    var tentativesRestantes = tentativesMax
    println("Mode Admin")
    while (tentativesRestantes > 0) {
      var pinUtilisateur = readLine("Entrez le code PIN : \n> ")
      if (pinUtilisateur == machine.pincode) {
        println("Accès accordé.")
        return true
      } else {
        tentativesRestantes -= 1
        println("Code PIN incorrect. " + tentativesRestantes + " tentative(s) restante(s).")
      }
    }
    false
  }

  def modeAdmin (machine: Machine): Unit = {
    var choixActionAdmin = 0
    do {
      choixActionAdmin = readLine("\nVeuillez svp choisir une action\n1) Gérer les stocks\n2) Modier le code PIN\n> ").toInt
      if (choixActionAdmin != 1 && choixActionAdmin != 2) {
        println("Veuillez svp entrer une valeur valable.")
      }
    } while (choixActionAdmin != 1 && choixActionAdmin != 2)
    if (choixActionAdmin == 1) {
      println("Quantité de lait à ajouter (en ml) :")
      machine.addIngredient("lait", readInt())
      println("Quantité de sucre à ajouter :")
      machine.addIngredient("sucre",readInt())
      println("Quantité de café à ajouter :")
      machine.addIngredient("café", readInt())
    } else {
      println("Nouveau PIN :")
      machine.pincode = scala.io.StdIn.readLine()
    }
  }

  def serveClient(machine: Machine): Unit = {
    var commande = true
    while (commande) {
      val prixExpresso = 2.00 //en CHF
      val prixCappuccino = 2.50
      val prixLattePetit = 2.70
      val prixLatteMoyen = 3.20
      val prixLatteGrand = 3.70
      val prixSucrePeu = 0.10
      val prixSucreMoyen = 0.20
      val prixSucreBeaucoup = 0.30
      val prixDoseLait = 0.05
      var prixTotal = 0.0
      // val stockCafeInitial = 50 //en g -> pas besoin car dans tableau
      val doseCafeExpresso = 8
      val doseCafeCappucino = 6
      val doseCafeLattePetit = 6
      val doseCafeLatteMoyen = 8
      val doseCafeLatteGrand = 12
      var stockCafeNecessaire = 0
      var stockCafeAjoute = 0
      // val stockLaitInitial = 500 //en millilitres -> pas besoin car dans tableau
      val doseLaitCappucino = 100
      val doseLaitLattePetit = 120
      val doseLaitLatteMoyen = 150
      val doseLaitLatteGrand = 200
      val doseLaitSupp = 50
      var stockLaitNecessaire = 0
      var stockLaitAjoute = 0
      // val stockSucreInitial = 30 //en g -> pas besoin car dans tableau
      val doseSucrePeu = 5
      val doseSucreMoyen = 10
      val doseSucreBeaucoup = 15
      var stockSucreNecessaire = 0
      var stockSucreAjoute = 0
      var stockTotalNecessaire = true //par défaut le stockTotalNecessaire commence vrai, car le programme commence avec assez de stock pour effectuer toutes les possibilités de café
      var boissonNom = "boisson"
      var niveauSucre = "Sans sucre"
      var prixBoisson = 0.0
      var prixSucre = 0.0
      var prixSuppLait = 0.0

      // choix de la boisson :
      var boisson = 0
      do {
        boisson = readLine("Veuillez sélectionner votre boisson :" +
          "\n1) Expresso - CHF 2.00" +
          "\n2) Cappuccino - CHF 2.50" +
          "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
        if (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (boisson != 1 && boisson != 2 && boisson != 3)

      //adaptation prix total :
      if (boisson == 1) {
        prixTotal = prixExpresso
        stockCafeNecessaire = doseCafeExpresso
        boissonNom = "Expresso"
        prixBoisson = prixExpresso
      }
      else if (boisson == 2) {
        prixTotal = prixCappuccino
        stockCafeNecessaire = doseCafeCappucino
        stockLaitNecessaire = doseLaitCappucino
        boissonNom = "Cappuccino"
        prixBoisson = prixCappuccino
      }
      else {
        var tailleLatte = 0
        boissonNom = "Latte"
        do {
          tailleLatte = readLine("Veuillez précisez la taille de votre latte : \n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Veuillez svp entrer une valeur valide.")
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
        if (tailleLatte == 1) {
          prixTotal = prixLattePetit
          stockCafeNecessaire = doseCafeLattePetit
          stockLaitNecessaire = doseLaitLattePetit
          prixBoisson = prixLattePetit
        } else if (tailleLatte == 2) {
          prixTotal = prixLatteMoyen
          stockCafeNecessaire = doseCafeLatteMoyen
          stockLaitNecessaire = doseLaitLatteMoyen
          prixBoisson = prixLatteMoyen
        } else {
          prixTotal = prixLatteGrand
          stockCafeNecessaire = doseCafeLatteGrand
          stockLaitNecessaire = doseLaitLatteGrand
          prixBoisson = prixLatteGrand
        }
      }

      //personnalisation :
      //sucre
      var sucre = 0
      do {
        sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
      //adaptation prix et des stocks :
      if (sucre == 1) {
        stockSucreNecessaire = 0
        niveauSucre = "Sans sucre"
        prixSucre = 0.0
      } else if (sucre == 2) {
        prixTotal += prixSucrePeu
        stockSucreNecessaire = doseSucrePeu
        niveauSucre = "Peu (5g)"
        prixSucre = prixSucrePeu
      } else if (sucre == 3) {
        prixTotal += prixSucreMoyen
        stockSucreNecessaire = doseSucreMoyen
        niveauSucre = "Moyen (10g)"
        prixSucre = prixSucreMoyen
      } else { // (sucre ==4)
        prixTotal += prixSucreBeaucoup
        stockSucreNecessaire = doseSucreBeaucoup
        niveauSucre = "Beaucoup (15g)"
        prixSucre = prixSucreBeaucoup
      }

      //lait :
      var lait = 0
      var nbDoseLait = 0
      if (boisson == 2 || boisson == 3) {
        do {
          lait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          if (lait != 1 && lait != 2) {
            println("Veuillez svp entrer une valeur valide.")
          }
        } while (lait != 1 && lait != 2)
        if (lait == 1) {
          do {
            nbDoseLait = readLine("Combien de dose ?\n> ").toInt
            if (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3) {
              if (nbDoseLait == 0) {
                println("Veuillez entrez une valeur svp.")
              } else if (nbDoseLait < 0) {
                println("Le nombre de dose de lait ne peut pas être négatif.")
              } else { // (nbDoseLait > 3)
                println("Il n'est pas possible d'ajouter plus de 3 doses de lait en supplément.")
              }
            }
          } while (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3)
          prixTotal += (nbDoseLait * prixDoseLait)
          stockLaitNecessaire += (nbDoseLait * doseLaitSupp)
          prixSuppLait = nbDoseLait * prixDoseLait
        }
      }

      // Mise à jour des stocks
      machine.removeIngredient("café", stockCafeNecessaire)
      machine.removeIngredient("lait", stockLaitNecessaire)
      machine.removeIngredient("sucre", stockSucreNecessaire)

      // résumé commande et affichange prix
      print("Boisson sélectionnée : " + boissonNom + "\nNiveau de sucre : " + niveauSucre + "\nLait supplémentaire : ")
      if (nbDoseLait == 0) {
        print("Non\n")
      } else {
        print(nbDoseLait + " dose(s)\n")
      }
      //+ nbDoseLait + " dose(s)")
      printf("Prix total : CHF %.2f", prixBoisson)
      if (sucre != 1) {
        printf(" + CHF %.2f", prixSucre)
      }
      if (lait == 1) {
        printf(" + CHF %.2f", prixSuppLait)
      }
      printf(" = CHF %.2f", prixTotal)
      println() //ligne vide// résumé commande et affichange prix
      print("Boisson sélectionnée : " + boissonNom + "\nNiveau de sucre : " + niveauSucre + "\nLait supplémentaire : ")
      if (nbDoseLait == 0) {
        print("Non\n")
      } else {
        print(nbDoseLait + " dose(s)\n")
      }
      //+ nbDoseLait + " dose(s)")
      printf("Prix total : CHF %.2f", prixBoisson)
      if (sucre != 1) {
        printf(" + CHF %.2f", prixSucre)
      }
      if (lait == 1) {
        printf(" + CHF %.2f", prixSuppLait)
      }
      printf(" = CHF %.2f", prixTotal)
      println() //ligne vide

      //payement
      val codeTwint = Random.alphanumeric.take(5).mkString
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint + "\n(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println() //ligne vide
      println("Merci ! Votre paiement a été accepté.")
      //Préparation de la boisson
      println("Préparation de votre boisson...\n[...]\nVotre " + boissonNom + " est prêt ! Bonne dégustation !\n")
      return true
      false
    }
  }
}