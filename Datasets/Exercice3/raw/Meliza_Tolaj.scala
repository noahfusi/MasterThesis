import scala.util.Random
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {


  // Ajouter un ingrédient au stock
  def addIngredient(ingredient: String, amount: Int): Unit = {
    /* Cas de l'ajout du lait */
    if (ingredient == "milk") {
      milk += amount

      /* Cas de l'ajout pour le sucre */
    } else if (ingredient == "sugar") {
      sugar += amount

      /* Cas de l'ajout du café */
    } else if (ingredient == "coffee") {
      coffee += amount
      /* Cas dans l'ajout d'un alimnet inconnu */
    } else {
      println("Ingrédient inconnu.")
    }
  }

  // Retirer une quantité d'ingrédient si y'a assez de stock
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    /* Cas pour le lait */
    if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      true

      /* Cas pour le sucre */
    } else if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true

      /* Cas pour le café */
    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
      /* Cas pour un aliment inconnu */
    } else {
      println(s"Stock insuffisant pour $ingredient.")
      false
    }
  }
}



object Main {
  /* Met à jour le Pin de la machine selectiionnée */
  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    /* Initialise une valeur booléenne pour avoir un pin valide*/

    var validPin = false
    while (!validPin) {
      val nouveauPin = StdIn.readLine()

      /* La taille du pin doit être de 6 et doit être que des chiffres */
      if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
        machine.pincode = nouveauPin
        println(s"Le code PIN a été mis à jour pour la machine ${machine.id} avec succès.")
        validPin = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Réessayez :")
      }
    }
  }

  def serveClient(machine: Machine): Boolean = {
    var choixmenu = 1
    var success = false // Indique si la transaction a réussi

    while (choixmenu == 1) { // Cas menu client
      // Initialisation des variables
      var quantiteSucre = 0
      var sucreLettre = ""
      var coutCafe = 0
      var coutLait = 0
      var coutSucre = 0
      var nomBoisson = ""
      var prixBoisson = 0.0
      var prixSucre = 0.0
      var prixLait = 0.0
      var prixTotal = 0.0
      var choixBoisson = 0

      // Sélection de la boisson
      while (!(choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3)) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        choixBoisson = scala.io.StdIn.readInt()
      }

      // Configuration de la boisson
      if (choixBoisson == 1) { // Expresso
        prixBoisson = 2.00
        nomBoisson = "Expresso"
        coutCafe = 8
      } else if (choixBoisson == 2) { // Cappuccino
        prixBoisson = 2.5
        nomBoisson = "Cappuccino"
        coutCafe = 6
        coutLait = 100
      } else if (choixBoisson == 3) { // Latte
        println("Quelle taille souhaitez-vous pour votre Latte ?")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")
        val tailleLatte = scala.io.StdIn.readInt()

        if (tailleLatte == 1) {
          prixBoisson = 2.7
          coutLait = 120
          coutCafe = 6
          nomBoisson = "Latte (Petit)"
        } else if (tailleLatte == 2) {
          prixBoisson = 3.2
          coutLait = 150
          coutCafe = 8
          nomBoisson = "Latte (Moyen)"
        } else if (tailleLatte == 3) {
          prixBoisson = 3.7
          coutCafe = 12
          coutLait = 200
          nomBoisson = "Latte (Grand)"
        }
      }

      // Ajouter du lait en supplément
      if (choixBoisson == 2 || choixBoisson == 3) {
        var laitPlus = 0
        while (!(laitPlus == 1 || laitPlus == 2)) {
          println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui \n2) Non \n>")
          laitPlus = scala.io.StdIn.readInt()
        }
        if (laitPlus == 1) {
          var doseLait = 0
          while (!(doseLait == 1 || doseLait == 2 || doseLait == 3)) {
            print("Combien de doses ? (max 3) \n>")
            doseLait = scala.io.StdIn.readInt()
          }
          coutLait += 50 * doseLait
          prixLait = doseLait * 0.05
        }
      }

      // Ajouter du sucre
      while (!(quantiteSucre == 1 || quantiteSucre == 2 || quantiteSucre == 3 || quantiteSucre == 4)) {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        quantiteSucre = scala.io.StdIn.readInt()
      }

      prixSucre = (quantiteSucre - 1) * 0.1
      if (quantiteSucre == 1) {
        sucreLettre = "Sans sucre"
        coutSucre = 0
      } else if (quantiteSucre == 2) {
        coutSucre = 5
        sucreLettre = "Peu (5g)"
      } else if (quantiteSucre == 3) {
        coutSucre = 10
        sucreLettre = "Moyen (10g)"
      } else if (quantiteSucre == 4) {
        coutSucre = 15
        sucreLettre = "Beaucoup (15g)"
      }

      // Vérification des stocks
      if (!machine.removeIngredient("coffee", coutCafe) ||
        !machine.removeIngredient("milk", coutLait) ||
        !machine.removeIngredient("sugar", coutSucre)) {
        println("Stock insuffisant. Retour au choix")
        return false

      }

      // Calcul du prix total et paiement
      prixTotal = prixBoisson + prixSucre + prixLait
      println(f"Prix total : CHF $prixBoisson%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF $prixTotal%.2f")

      val alphabetNum = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var codeTwint = ""
      for (_ <- 1 to 5) {
        val index = (Math.random() * alphabetNum.length).toInt
        codeTwint += alphabetNum(index)
      }
      println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $codeTwint")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println(s"Votre $nomBoisson avec $sucreLettre est prêt ! Bonne dégustation !")

      success = true
      choixmenu = 0
    }

    success
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    /* Initialise  mon ArrayBuffer */
    val machines = ArrayBuffer[Machine]()
    try {
      /* Lit le fichier et place les lignes dans un Array de String */
      val lines = Source.fromFile(filename).getLines().toArray
      var i = 1
      /* Iteration sur le Array pour obtenir les informations désirées */
      while (i < lines.length) {
        val parties = lines(i).split(",")
        if (parties.length == 4) {
          machines += new Machine(i, parties(0), parties(1).toInt, parties(2).toInt, parties(3).toInt)
        }
        i += 1
      }

    } catch {
      case e: Exception =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println(s"Sauvegarde des machines dans $filename.")
    } catch {
      case _: java.io.IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur  : ${e.getMessage}")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        sys.exit(1)
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var essais = 3
    var validPin = false
    while (essais > 0 && !validPin) {
      println(s"Entrez le code PIN pour la machine ${machine.id} :")
      val enteredPin = StdIn.readLine()
      if (enteredPin == machine.pincode) {
        println("Accès accordé.")
        validPin = true
      } else {
        essais -= 1
        println(s"Code PIN incorrect. $essais tentative(s) restante(s).")
      }
    }
    if (!validPin) {
      println("Trop de tentatives échouées. Fin du programme.")
    }
    validPin
  }

  def restockMachine(machine: Machine): Unit = {
    println(s"Réapprovisionnement de la machine ${machine.id}.")
    println(s"Niveaux de stock actuels :\nCafé : ${machine.coffee}g\nSucre : ${machine.sugar}g\nLait : ${machine.milk}L")
    /* Initialise les valeurs à ajouter dans la machine */
    var cafe_ajouter = -1
    var sucre_ajouter = -1
    var lait_ajouter = -1

    while (cafe_ajouter < 0) {
      println("Entrez la quantité de café à ajouter (en grammes, positif) :")
      cafe_ajouter = scala.io.StdIn.readInt()
      if (cafe_ajouter < 0) println("La quantité doit être positive. Réessayez.")
    }

    while (sucre_ajouter < 0) {
      println("Entrez la quantité de sucre à ajouter (en grammes, positif) :")
      sucre_ajouter = scala.io.StdIn.readInt()
      if (sucre_ajouter < 0) println("La quantité doit être positive. Réessayez.")
    }

    while (lait_ajouter < 0) {
      println("Entrez la quantité de lait à ajouter (en litres, positif) :")
      lait_ajouter = scala.io.StdIn.readInt()
      if (lait_ajouter < 0) println("La quantité doit être positive. Réessayez.")
    }

    machine.addIngredient("coffee", cafe_ajouter)
    machine.addIngredient("sugar", sucre_ajouter)
    machine.addIngredient("milk", lait_ajouter)


    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)

    println("\nMachines chargées :")
    for (machine <- machines) {
      println(s"Machine ${machine.id} chargée :")
      println(f"\tID: ${machine.id}")
      println(f"\tCode PIN: ${machine.pincode}")
      println(f"\tLait: ${machine.milk.toDouble / 1000}%.3fL")
      println(f"\tSucre: ${machine.sugar}g")
      println(f"\tCafé: ${machine.coffee}g")
      println()
    }
    println(s"${machines.length} machine(s) chargée(s) avec succès.")

    var continue = true

    while (continue) {
      println("\n\tNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choix = StdIn.readInt()

      if (choix == 1) {
        var machineSelected = false
        while (!machineSelected) {
          var machineId = -1
          while (machineId < 1 || machineId > machines.length) {
            println(s"Entrez l'ID de la machine (1 à ${machines.length}) :")
            machineId = StdIn.readInt()
            if (machineId < 1 || machineId > machines.length) {
              println("ID de machine invalide. Veuillez réessayer.")
            }
          }

          val machine = machines(machineId - 1)
          println(s"Service client pour la machine ${machine.id}")
          if (serveClient(machine)) {
            machineSelected = true
          } else {
            println("Veuillez sélectionner une autre machine.")
          }
        }
      }

      else if (choix == 2) {
        var machineId = -1
        while (machineId < 1 || machineId > machines.length) { // Boucle pour un ID valide
          println(s"Entrez l'ID de la machine (1 à ${machines.length}) :")
          machineId = StdIn.readInt()
          if (machineId < 1 || machineId > machines.length) {
            println("ID de machine invalide. Veuillez réessayer.")
          }
        }

        val machine = machines(machineId - 1) // On convertit en index (commençant à 0)
        if (validatePin(machine)) {
          println("1) Réapprovisionner")
          println("2) Mettre à jour le PIN")
          print("> ")
          val action = StdIn.readInt()
          if (action == 1) {
            restockMachine(machine)
          } else if (action == 2) {
            updatePin(machine)
          }
        } else {
          continue = false // Si validation du PIN échoue, on termine le programme
          savecsv(filename, machines)
        }
      }
      else if (choix == 3) {
        savecsv(filename, machines)
        continue = false
        println("Fichier sauvegardé avec succès.")
      }
      else {
        println("Choix invalide.")
      }
    }
  }
}