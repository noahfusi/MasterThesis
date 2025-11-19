import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.{FileNotFoundException, IOException, PrintWriter, BufferedReader, FileReader}



class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "1") {
      milk += amount
    } else if (ingredient == "2") {
      sugar += amount
    } else if (ingredient == "3") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "1" && milk >= amount) {
      milk -= amount
      return true
    } else if (ingredient == "2" && sugar >= amount) {
      sugar -= amount
      return true
    } else if (ingredient == "3" && coffee >= amount) {
      coffee -= amount
      return true
    } else {
      return false
    }
  }
}



object Main {

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val reader = new BufferedReader(new FileReader(filename))
      var line = reader.readLine()
      var id = 1
      while ({ line = reader.readLine(); line != null }) {
        val data = line.split(",")
        val pincode = data(0)
        val milk = data(1).toInt
        val sugar = data(2).toInt
        val coffee = data(3).toInt
        machines.append(new Machine(id, pincode, milk, sugar, coffee))
        id += 1
      }
      reader.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
      case _: IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        sys.exit(1)
    }
    return machines
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(filename)
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      writer.close()
    } catch {
      case _: IOException =>
        println("Erreur : Echec de l'écriture dans " + filename + ".\nLe fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case _: Exception =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        sys.exit(1)
    }
  }



  def validatePin(machine: Machine): Boolean = {

    var validation = false

    print("\nEntrez le code PIN : ")
    val PIN = scala.io.StdIn.readLine()
    if ( PIN == machine.pincode) {
      validation = true
    }

    return validation
  }


  def updatePin(machine: Machine): Unit = {

    print("\nEntrez un nouveau code PIN à 6 chiffres > ")
    var PIN = scala.io.StdIn.readLine()
    while(PIN.length() != 6 || !PIN.matches("\\d+")) {  // trouvé sur un forum
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      PIN = scala.io.StdIn.readLine()
    }
    machine.pincode = PIN

  }


  def serveClient(machine:Machine): Boolean = {

    var prix = 0.0
    var prix_total = ""

    var boisson = ""
    var sucre = ""
    var lait = ""

    var conso_poudre = 0
    var conso_sucre = 0
    var conso_lait = 0.0


    // séléction boisson
    print("\nVeuillez sélectionner votre boisson : \n1) Expresso - CHF 2,00 \n2) Cappuccino - CHF 2,50 \n3) Latte - CHF 2,70 (Petit), CHF 3,20 (Moyen), CHF 3,70 (Grand)\n> ")
    var choix_boisson = scala.io.StdIn.readLine()
    while (choix_boisson != "1" && choix_boisson != "2" && choix_boisson != "3") {
      choix_boisson = scala.io.StdIn.readLine()
    }

    if (choix_boisson == "1") {
      boisson = "Expresso"
      prix = prix + 2.0
      prix_total = "CHF 2,00"
      conso_poudre = 8

    } else if (choix_boisson == "2") {
      boisson = "Cappucino"
      prix = prix + 2.5
      prix_total = "CHF 2,50"
      conso_poudre = 6
      conso_lait = 0.1

    } else {

      // taille latte
      print("\nVeuillez sélectionner une taille de Latte : \n1) Petit - CHF 2,70\n2) Moyen - CHF 3,20\n3) Grand - CHF 3,70 \n> ")
      var choix_taille = scala.io.StdIn.readLine()
      while (choix_taille != "1" && choix_taille != "2" && choix_taille != "3") {
        choix_taille = scala.io.StdIn.readLine()
      }

      if (choix_taille == "1") {
        boisson = "Latte (Petit)"
        prix = prix + 2.7
        prix_total = "CHF 2,70"
        conso_poudre = 6
        conso_lait = 0.12

      } else if (choix_taille == "2") {
        boisson = "Latte (Moyen)"
        prix = prix + 3.2
        prix_total = "CHF 3,20"
        conso_poudre = 8
        conso_lait = 0.15

      } else {
        boisson = "Latte (Grand)"
        prix = prix + 3.7
        prix_total = "CHF 3,70"
        conso_poudre = 12
        conso_lait = 0.2

      }

    }


    // séléction sucre
    print("\nSouhaitez-vous ajouter du sucre ? : \n1) Sans sucre \n2) Peu (5g) - CHF 0,10 \n3) Moyen (10g) - CHF 0,20 \n4) Beaucoup (15g) - CHF 0,30\n> ")
    var choix_sucre = scala.io.StdIn.readLine()
    while (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre != "4") {
      choix_sucre = scala.io.StdIn.readLine()
    }

    if (choix_sucre == "2") {
      prix = prix + 0.1
      prix_total = prix_total + " + CHF 0,10"
      sucre = "Peu (5g)"
      conso_sucre = 5

    } else if (choix_sucre == "3") {
      prix = prix + 0.2
      prix_total = prix_total + " + CHF 0,20"
      sucre = "Moyen (10g)"
      conso_sucre = 10

    } else if (choix_sucre == "4") {
      prix = prix + 0.3
      prix_total = prix_total + " + CHF 0,30"
      sucre = "Beaucoup (15g)"
      conso_sucre = 15

    } else {
      sucre = "Sans sucre"
    }


    // séléction lait seulement si cappuccino (2) et latte (3)
    if (choix_boisson == "2" || choix_boisson == "3") {

      print("\nSouhaitez-vous ajouter du lait en supplément ? : \n1) Oui \n2) Non\n> ")
      var choix_lait = scala.io.StdIn.readLine()
      while (choix_lait != "1" && choix_lait != "2") {
        choix_lait = scala.io.StdIn.readLine()
      }

      if (choix_lait == "1") {
        print("\nCombien de dose ? (3 doses maximum à CHF 0,05 par dose) \n> ")
        var choix_doses = scala.io.StdIn.readLine()
        while (choix_doses != "1" && choix_doses != "2" && choix_doses != "3") {
          choix_doses = scala.io.StdIn.readLine()
        }

        if (choix_doses == "1") {
          prix = prix + 0.05
          prix_total = prix_total + " + CHF 0,05"
          lait = "1 dose"
          conso_lait = conso_lait + 0.05

        } else if (choix_doses == "2") {
          prix = prix + 0.1
          prix_total = prix_total + " + CHF 0,10"
          lait = "2 doses"
          conso_lait = conso_lait + 0.1

        } else {
          prix = prix + 0.15
          prix_total = prix_total + " + CHF 0,15"
          lait = "3 doses"
          conso_lait = conso_lait + 0.15
        }

      } else {
        lait = "Non"
      }
    }

    // récapitulatif :
    println("\nBoisson sélectionnée : " + boisson)
    println("Niveau de sucre : " + sucre)
    if (choix_boisson == "2" || choix_boisson == "3") {
      println("Lait en supplément : " + lait)
    }


    // vérification des stocks
    if (machine.coffee < conso_poudre || machine.milk / 1000.0 < conso_lait || machine.sugar < conso_sucre) {
      println("\nErreur : Quantités insuffisantes pour préparer la boisson sélectionnée.")
      return false

    } else {

      machine.removeIngredient("3", conso_poudre)
      machine.removeIngredient("1", (conso_lait * 1000.0).toInt)
      machine.removeIngredient("2", conso_sucre)

      if (choix_sucre == "1" && (choix_boisson == "1" || lait == "Non")) {
        println("Prix total : " + f"CHF $prix%.2f")
      } else {
        println("Prix total : " + prix_total + f" = CHF $prix%.2f")
      }

      // paiement
      val code_twint = Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_twint + "\n(En attente de validation du paiement...)\n")
      Thread.sleep(3000)
      println("Paiement confirmé.\n" + "Préparation de votre boisson...")
      Thread.sleep(3000)
      println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
      return true

    }
  }


  def restockMachine(machine: Machine): Unit = {


    // stock initial
    println("Stocks :\n   Poudre de café : " + machine.coffee + "g\n   Lait           : " + machine.milk /1000.0 + "L\n   Sucre          : " + machine.sugar + "g")


    // réapprovisionnement
    print("Veuillez entrer la quantité de poudre à café (en g) à ajouter : ")
    var ajout_poudre = scala.io.StdIn.readLine()
    while (ajout_poudre.toInt < 0) {
      ajout_poudre = scala.io.StdIn.readLine()
    }

    print("\nVeuillez entrer la quantité de lait (en L) à ajouter : ")
    var ajout_lait = scala.io.StdIn.readLine()
    while (ajout_lait.toDouble < 0.0) {
      ajout_lait = scala.io.StdIn.readLine()
    }

    print("\nVeuillez entrer la quantité de sucre (en g) à ajouter : ")
    var ajout_sucre = scala.io.StdIn.readLine()
    while (ajout_sucre.toInt < 0) {
      ajout_sucre = scala.io.StdIn.readLine()
    }

    machine.addIngredient("3", ajout_poudre.toInt)
    machine.addIngredient("1", (ajout_lait.toDouble * 1000.0).toInt)
    machine.addIngredient("2", ajout_sucre.toInt)

    println("\nRéapprovisionnement des stocks...")
    Thread.sleep(2000)
    println("\nAjout :\n   Poudre de café : " + machine.coffee + "g\n   Lait           : " + machine.milk/1000.0 + "L\n   Sucre          : " + machine.sugar + "g")
    println("Niveaux de stock mis à jour.\nRetour au menu principal...\n\n")
    Thread.sleep(1000)


  }



  def main(args: Array[String]): Unit = {


    var mode = ""
    val filename = "machines.csv"
    val machines = loadcsv(filename)

    println("Chargement des machines depuis " + filename + "...\n")

    for (machine <- machines) {
      println("Machine " + machine.id + " chargée :")
      println("   ID: " + machine.id)
      println("   Code PIN: " + machine.pincode)
      println("   Lait: " + (machine.milk / 1000.0) + "L")
      println("   Sucre: " + machine.sugar + "g")
      println("   Café: " + machine.coffee + "g\n")
    }
    println(machines.length + " machine(s) chargée(s) avec succès.")

    while (mode != "3") {

      println("\n        Nospresso Café")
      print("Veuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ")

      mode = scala.io.StdIn.readLine()

      while (mode != "1" && mode != "2" && mode != "3") {
        mode = scala.io.StdIn.readLine()
      }

      // client
      if (mode == "1") {

        println("\nMode Client\n")
        var transaction = false

        while(transaction==false){

          print("Veuillez sélectionner une machine (" + machines.map(_.id).mkString(",") + ") : ")
          var machine_ID = scala.io.StdIn.readLine()
          while (!machines.exists(_.id.toString == machine_ID)) {
            print("Veuillez sélectionner une machine valide : ")
            machine_ID = scala.io.StdIn.readLine()
          }

          val selectedMachine = machines.find(_.id.toString == machine_ID).get
          println("\nMachine "+ selectedMachine.id + " séléctionée.\n")


          transaction = serveClient(selectedMachine)
        }

        println("Retour au menu principal...\n\n")
        Thread.sleep(1000)

      }

      // admin
      else if (mode == "2") {

        // pin
        println("\nMode Admin")
        print("Veuillez sélectionner une machine (" + machines.map(_.id).mkString(",") + ") : ")
        var machine_ID = scala.io.StdIn.readLine()
        while (!machines.exists(_.id.toString == machine_ID)) {
          print("Veuillez sélectionner une machine valide : ")
          machine_ID = scala.io.StdIn.readLine()
        }

        val selectedMachine = machines.find(_.id.toString == machine_ID).get
        println("\nMachine "+ selectedMachine.id + " séléctionée.\n")

        var validation = validatePin(selectedMachine)
        var tentative = 2
        while (validation == false && tentative > 0){
          println("Code PIN incorrect. " + tentative + " tentatives restantes.")
          validation = validatePin(selectedMachine)
          tentative = tentative - 1
        }

        if (validation == false){
          println("Code PIN incorrect. 0 tentatives restantes.")
          println("\nTrop de tentatives échouées.\nFin du programme.")
          mode = "3"

          println("\nSauvegarde de " + machines.length + " machine(s) dans " + filename + "...")
          savecsv(filename, machines)
          Thread.sleep(1000)
          println("Fichier sauvegardé avec succès.")

          } else {

          println("\nAccès autorisé.\n")
          print("Que souhaitez-vous faire ?\n1) Mettre à jour le mot de passe\n2) Réapprovisionner les stocks\n> ")
          var option = scala.io.StdIn.readLine()
          while (option != "1" && option !="2") {
            option = scala.io.StdIn.readLine()
          }

          if(option == "1"){
            updatePin(selectedMachine)
            println("Le code PIN a  été mis à jour.\nRetour au menu principal...")
            Thread.sleep(1000)
          } else {
            restockMachine(selectedMachine)
            println("Retour au menu principal...)")
            Thread.sleep(1000)
          }
        }
      }
      else {
        println("\nMerci d'avoir utilisé Nospresso !")
        println("À bientôt !")

        println("\nSauvegarde de " + machines.length + " machine(s) dans " + filename + "...")
        savecsv(filename, machines)
        Thread.sleep(1000)
        println("Fichier sauvegardé avec succès.")
      }
    }
  }
}