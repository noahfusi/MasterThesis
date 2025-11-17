import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.io.StdIn._
import scala.util.Random

class Machine(var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (amount > 0) {
      if (ingredient == "milk"|| ingredient == "MILK"|| ingredient=="Milk") {
        milk += amount
      } else if (ingredient == "sugar" || ingredient == "SUGAR" || ingredient == "Sugar") {
        sugar += amount
      } else if (ingredient == "coffee" || ingredient == "COFFEE" || ingredient == "Coffee") {
        coffee += amount
      } else {
        println("Erreur : ingrédient invalide.")
      }
    } else {
      println("Erreur : La quantité doit être positive.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if ((ingredient == "milk" || ingredient == "MILK" || ingredient == "Milk") && milk >= amount) {
      milk -= amount
      true
    } else if ((ingredient == "sugar" || ingredient == "SUGAR" || ingredient == "Sugar") && sugar >= amount) {
      sugar -= amount
      true
    } else if ((ingredient == "coffee" || ingredient == "COFFEE" || ingredient == "Coffee") && coffee >= amount) {
      coffee -= amount
      true
    } else {
      false
    }
  }

  def displayStock(): Unit = {
    println("Stocks actuels :")
    printf("Lait : %.2fL\n", milk /1000.0)
    printf("Sucre: %d g\n ",sugar)
    printf("Café: %d g\n",coffee)
  }

  def summary(): Unit = {
    println("Machine chargée :")
    println(s"ID: "+id)
    println(s"Code PIN: "+pincode)
    printf("Lait : %.2fL\n", milk /1000.0)
    printf("Sucre: %d g\n ",sugar)
    printf("Café: %d g\n",coffee)
  }
}

object Nospresso {
  val filename = "machines.csv"

  def main(args: Array[String]): Unit = {
    val machines = loadcsv(filename)

    if (machines.isEmpty) {
      println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
      println("Fin du programme.")
      return
    }

    println("Chargement des machines...")
    for (i <- machines.indices){
      machines(i).summary()
    }

    var continuer = true
    while (continuer) {
      println("\nNospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      var choix = -1
      while (choix < 1 || choix >3){
        try {
          choix = readLine().toInt
          if (choix <1 || choix >3){
            println("Veuillez choisir entre 1 et 3.")
          }
        } catch {
          case _: Exception =>
            println("Entrée invalide. Veuillez saisir un nombre entier.")
        }
      }

      if (choix == 1) {
        println("Veuillez entrer l'ID de la machine que vous souhaitez utiliser :")
        var machineId = -1
        while (machineId < 1 || machineId > machines.length){
          try {
            machineId = readLine().toInt
            if (machineId < 1 || machineId > machines.length){
              println("Entrez un ID valide :")
            }
          }catch {
            case _: Exception =>
              println("Entrée invalide. Veuillez saisir un nombre entier.")
          }
        }
        var machine: Machine = null
        for (m <- machines) {
          if (m.id == machineId) {
            machine = m
          }
        }
        if (machine != null) {
          serveClient(machine)
          savecsv(filename, machines)
        } else {
          println("Machine invalide.")
        }
      } else if (choix == 2) {
        println("Veuillez entrer l'ID de la machine pour la gestion :")
        var machineId = -1
        while (machineId < 1 || machineId > machines.length){
          try {
            machineId = readLine().toInt
            if (machineId < 1 || machineId > machines.length){
              println("Entrez un ID valide :")
            }
          }catch {
            case _: Exception =>
              println("Entrée invalide. Veuillez saisir un nombre entier.")
          }
        }
        var machine: Machine = null
        for (m <- machines) {
          if (m.id == machineId) {
            machine = m
          }
        }
        if (machine != null) {
          adminMenu(machine, machines)
        } else {
          println("Machine invalide.")
        }
      } else if (choix == 3) {
        println(s"Sauvegarde de "+machines.size+" machines dans "+filename+"...")
        if (savecsv(filename, machines)) {
          println("Fichier sauvegardé avec succès. Au revoir !")
        } else {
          println("Erreur : Échec de la sauvegarde des machines.")
        }
        continuer = false
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      var index = 1
      for (line <- source.getLines()) {
        if (index > 1) {
          val cols = line.split(",")
          if (cols.length == 4) {
            machines += new Machine(index, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
          } else {
            println(s"Erreur : Ligne incorrecte dans le fichier CSV : " +line)
          }
        }
        index += 1
      }
      source.close()

      for (i <- machines.indices) {
        machines(i).id = i + 1
      }
      println(machines.size +" machine(s) chargée(s) avec succès.")
    } catch {
      case _: Exception =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return machines
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Boolean = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode +","+machine.milk+","+machine.sugar+","+machine.coffee +".")
      }
      writer.close()
      true
    } catch {
      case _: Exception =>
        println("Erreur : Échec de l’écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule.")
        false
    }
  }

  def serveClient(machine: Machine): Unit = {
    println("Bienvenue dans le mode Client.")
    println("Veuillez sélectionner une boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    var validChoice = false
    var choice = 0
    while (!validChoice) {
      try {
        choice = readLine().toInt
        if (choice >= 1 && choice <= 3) {
          validChoice = true
        } else {
          println("Veuillez sélectionner une option valide (1, 2 ou 3).")
        }
      } catch {
        case _: Exception => println("Entrée invalide. Veuillez saisir un nombre.")
      }
    }

    var coffeeAmount = 0
    var milkAmount = 0
    var basePrice = 0.0
    var boisson = ""

    if (choice == 1) {
      boisson = "Expresso"
      coffeeAmount = 8
      milkAmount = 0
      basePrice = 2.00
    } else if (choice == 2) {
      boisson = "Cappuccino"
      coffeeAmount = 6
      milkAmount = 100
      basePrice = 2.50
    } else if (choice == 3) {
      println("Choisissez la taille : 1) Petit, 2) Moyen, 3) Grand")
      var validSize = false
      var sizeChoice = 0
      while (!validSize) {
        try {
          sizeChoice = readLine().toInt
          if (sizeChoice >= 1 && sizeChoice <= 3) {
            validSize = true
          } else {
            println("Veuillez sélectionner une taille valide (1, 2 ou 3).")
          }
        } catch {
          case _: Exception => println("Entrée invalide. Veuillez saisir un nombre.")
        }
      }
      if (sizeChoice == 1) {
        boisson = "Latte (Petit)"
        coffeeAmount = 6
        milkAmount = 120
        basePrice = 2.70
      } else if (sizeChoice == 2) {
        boisson = "Latte (Moyen)"
        coffeeAmount = 8
        milkAmount = 150
        basePrice = 3.20
      } else if (sizeChoice == 3) {
        boisson = "Latte (Grand)"
        coffeeAmount = 12
        milkAmount = 200
        basePrice = 3.70
      }
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    var sugarChoice = 0
    var sugarAmount = 0
    var sugarPrice = 0.0
    var sugarLevel = "Sans sucre"
    while (sugarChoice < 1 || sugarChoice > 4) {
      try {
        sugarChoice = readLine().toInt
        if (sugarChoice == 1) {
          sugarAmount = 0
          sugarPrice = 0.0
        } else if (sugarChoice == 2) {
          sugarAmount = 5
          sugarPrice = 0.10
        } else if (sugarChoice == 3) {
          sugarAmount = 10
          sugarPrice = 0.20
        } else if (sugarChoice == 4) {
          sugarAmount = 15
          sugarPrice = 0.30
        } else {
          println("Veuillez sélectionner une option valide (1, 2, 3 ou 4).")
        }
      } catch {
        case _: Exception => println("Entrée invalide. Veuillez saisir un nombre.")
      }
    }

    var extraMilk = 0
    var extraMilkPrice = 0.0
    var milkChoice = 0
    if (choice == 2 || choice == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? (1) Oui, (2) Non")
      while (milkChoice != 1 && milkChoice != 2) {
        try {
          milkChoice = readLine().toInt
          if (milkChoice == 1) {
            println("Combien de doses de lait ? (1 à 3 doses, 50mL par dose, CHF 0.05 par dose)")
            var validDoses = false
            while (!validDoses) {
              try {
                extraMilk = readLine().toInt
                if (extraMilk >= 1 && extraMilk <= 3) {
                  extraMilkPrice = extraMilk * 0.05
                  validDoses = true
                } else {
                  println("Veuillez sélectionner un nombre de doses entre 1 et 3.")
                }
              } catch {
                case _: Exception => println("Entrée invalide. Veuillez saisir un nombre.")
              }
            }
          } else if (milkChoice != 2) {
            println("Veuillez sélectionner une option valide (1 ou 2).")
          }
        } catch {
          case _: Exception => println("Entrée invalide. Veuillez saisir un nombre.")
        }
      }
    }


    var extraMilkText = ""
    if (extraMilk >0){
      extraMilkText = extraMilk + "doses"
    }else{
      extraMilkText = "Non"
    }
    var totalPrice = basePrice + sugarPrice + extraMilkPrice
    if (coffeeAmount > machine.coffee) {
      println(s"Boisson sélectionnée : "+boisson)
      println(s"Niveau de sucre : "+sugarLevel)
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return
    }

    if (sugarAmount > machine.sugar) {
      println(s"Boisson sélectionnée : "+boisson)
      println(s"Niveau de sucre : "+sugarLevel)
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return
    }

    if (milkAmount + extraMilk * 50 > machine.milk) {
      println(s"Boisson sélectionnée : "+boisson)
      println(s"Niveau de sucre : "+sugarLevel)
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return
    }

    machine.removeIngredient("coffee",coffeeAmount)
    machine.removeIngredient("milk", milkAmount+extraMilk *50)
    machine.removeIngredient("sugar", sugarAmount)

    println("Récapitulatif  de votre boisson")
    println("Boisson : " + boisson)
    if (sugarChoice == 1) {
      println(" Sans sucre ")
    } else if (sugarChoice == 2) {
      println(" Peu de sucre (5g).")
    } else if (sugarChoice == 3) {
      println(" Moyen de sucre (10g).")
    } else {
      println("Beaucoup de sucre (15g).")
    }
    if (extraMilk > 0) {
      println("Lait en supplément : " + extraMilk + " doses")
    } else {
      println("Pas de lait en supplément.")
    }
    if ((sugarChoice == 1) && (milkChoice == 2)) {
      printf("Prix Total : CHF %.2f\n", totalPrice)
    } else if (((sugarChoice == 2) || (sugarChoice == 3) || (sugarChoice == 4)) && (milkChoice == 2)) {
      printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f\n", basePrice, sugarPrice, totalPrice)
    } else if ((sugarChoice == 1) && (milkChoice == 1)) {
      printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f\n", basePrice, extraMilkPrice, totalPrice)
    } else if (((sugarChoice == 2) || (sugarChoice == 3) || (sugarChoice == 4)) && (milkChoice == 1)) {
      printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", basePrice, extraMilkPrice, sugarPrice, totalPrice)
    }

    println("Veuillez payer en utilisant Twint.")
    val codeTwint = Random.alphanumeric.take(5).mkString
    println("Votre code de paiment est :" + codeTwint)
    println("(En attente de paiement...)")
    Thread.sleep(5000)
    println("Paiement confirmé.")
    println("Préparation de votre boisson...")


  }

  def adminMenu(machine: Machine, machines: ArrayBuffer[Machine]): Unit = {

    var attempts = 0
    val maxAttempts = 3
    var accessGranted = false
    while (attempts < maxAttempts && !accessGranted){
      println("Entrez le code PIN :")
      val pin = readLine()
      if (pin == machine.pincode) {
        accessGranted = true
        println("1) Réapprovisionner")
        println("2) Modifier le PIN")
        var choix = -1
        while (choix < 1 || choix > 2) {
          try {
            choix = readLine().toInt
            if (choix < 1 || choix > 2) {
              println("Choix invalide, Veuillez entrer 1 ou 2.")
            }
          } catch {
            case _: Exception =>
              println("Entrée invalide, Veuillez saisir un nombre.")
          }
        }
        if (choix == 1) {
          machine.displayStock()
          println("Entrez les quantités à ajouter pour chaque ingrédient :")
          println("Lait (en mL) :")
          machine.addIngredient("milk", readLine().toInt)
          println("Sucre (en g) :")
          machine.addIngredient("sugar", readLine().toInt)
          println("Café (en g) :")
          machine.addIngredient("coffee", readLine().toInt)
          println("Les stocks ont été mis à jour avec succès.")
          println("Niveaux de stocks actuels :")
          machine.displayStock()
          println("Retour au menu principal...")
          savecsv(filename, machines)
        } else if (choix == 2) {
          updatePin(machine)
          savecsv(filename, machines)
        }
      } else {
        attempts += 1
        val remainingAttempts = maxAttempts - attempts
        if (remainingAttempts >0){
          println("Code PIN incorrect." + remainingAttempts + " tentative(s) restante(s).")
        }else {
          println ("Trop de tentatives échouées. Fin du programme.")
          System.exit(0)
        }
      }
    }
  }

  def updatePin(machine: Machine) : Unit = {
    println("Mise à jour du code PIN pour la machine "+ machine.id+ ".")
    var pinValide = false
    while (!pinValide){
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val nouveauPin = readLine()

      if (nouveauPin.length == 6){
        var valide = true
        for (i <- 0 until nouveauPin.length){
          if (nouveauPin(i) < '0' || nouveauPin(i) > '9'){
            valide = false
          }
        }
        if (valide){
          machine.pincode = nouveauPin
          pinValide = true
          println("Code PIN mis à jour avec succés.")
          println("Retour au menu principal. ")
        }else{
          println("Erreur : le code PIN doit contenur uniquement des chiffres.")
        }
      }else{
        println("Erreur : le code PIN doit comporter exactement 6 caractères.")
      }
    }
  }
}

