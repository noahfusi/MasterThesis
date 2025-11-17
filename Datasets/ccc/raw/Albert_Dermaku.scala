import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "coffee") {
      coffee += amount
    } else if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "coffee") {
      if (coffee - amount >= 0) {
        coffee -= amount
        true
      } else {
        false
      }
    } else if (ingredient == "milk") {
      if (milk - amount >= 0) {
        milk -= amount
        true
      } else {
        false
      }
    } else if (ingredient == "sugar") {
      if (sugar - amount >= 0) {
        sugar -= amount
        true
      } else {
        false
      }
    } else {
      false
    }
  }
}


object Nospresso {
  var pin: String = ""

  // Prix
  val expressoPrix = 2.00
  val cappuccinoPrix = 2.50
  val lattePetitPrix = 2.70
  val latteMoyenPrix = 3.20
  val latteGrandPrix = 3.70
  val scrPeuPrix = 0.10
  val scrMoyPrix = 0.20
  val scrBcpPrix = 0.30

  // Stock
  var machines = new ArrayBuffer[Machine]()

  // Consommation des ingrédients par type de boisson
  val expressoPdr = 8
  val cappuccinoPdr = 6
  val lattePetitPdr = 6
  val latteMoyPdr = 8
  val latteGrandPdr = 12

  val cappuccinoLait = 100
  val lattePetitLait = 120
  val latteMoyLait = 150
  val latteGrandLait = 200

  val sucrePeu = 5
  val sucreMoy = 10
  val sucreBcp = 15

  val uneDoseQtn = 50
  val uneDoseLaitPrix = 0.05

  var latteTailleChoix = 0

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1)
      var i = 1
      for (line <- lines) {
        val cols = line.split(",")
        machines += new Machine(i, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
        i += 1
      }
      source.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(3)
      case _: IOException =>
        println("Le fichier peut être verouillé ou en lecture seul.")
        println("Fermeture du programme")
        System.exit(3)
    }

    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      try {
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
        }
        println("Fichier sauvegardé avec succès.")
      } finally {
        writer.close()
      }
    } catch {
      case _: FileNotFoundException =>
        println(s"Erreur : Echec de l'écriture dans machines.csv.")
        System.exit(3)
      case _: IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme")
        System.exit(3)
    }
  }



  def isDigit(pin: String): Boolean = {
    for (c <- pin) {
      if (!c.isDigit) {
        return false
      }
    }
    true
  }

  def validatePin(machineId: Int): Boolean = {
    print("Entrez le code PIN : ")
    var isPinOk = false
    var tentatives = 3
    while (!isPinOk) {
      pin = readLine()
      if (machines(machineId).pincode.equals(pin)) {
        isPinOk = true
        println("Accès autorisé.\n")
        return true
      } else {
        tentatives -= 1
        if (tentatives < 0) {
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(3)
        } else {
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
          print("> ")

        }
      }
    }
    false
  }

  def updatePin(machineId: Int): Unit = {
    var isPinOk = false
    var tentatives = 6
    println("Mise à jour du code PIN pour la Machine " + machineId)
    while (!isPinOk) {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      pin = readLine()
      if (pin.length == 6 && isDigit(pin)) {
        isPinOk = true
      } else {
        tentatives -= 1
        if (tentatives < 0) {
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(3)
        }
      }
    }
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")
    Thread.sleep(1000)
    machines(machineId).pincode = pin
  }

  def serveClient(machineId: Int): Boolean = {
    var boissonChoix = 0
    while (boissonChoix < 1 || boissonChoix > 3) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      boissonChoix = readLine().toInt
    }
    var prixTot = 0.0
    var boissonChoisiPrix = 0.0
    var poudreEnCours = 0
    var laitEnCours = 0
    var laitEnCoursPrix = 0.0
    if (boissonChoix == 1) {
      boissonChoisiPrix = expressoPrix
      prixTot += expressoPrix
      poudreEnCours += expressoPdr
    } else if (boissonChoix == 2) {
      boissonChoisiPrix = cappuccinoPrix
      prixTot += cappuccinoPrix
      poudreEnCours += cappuccinoPdr
      laitEnCours += cappuccinoLait
    } else if (boissonChoix == 3) {
      while (latteTailleChoix < 1 || latteTailleChoix > 3) {
        println("Veuillez sélectionner la taille :")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        print("> ")
        latteTailleChoix = readLine().toInt
      }
      if (latteTailleChoix == 1) {
        boissonChoisiPrix = lattePetitPrix
        prixTot += lattePetitPrix
        poudreEnCours += lattePetitPdr
        laitEnCours += lattePetitLait
      } else if (latteTailleChoix == 2) {
        boissonChoisiPrix = latteMoyenPrix
        prixTot += latteMoyenPrix
        poudreEnCours += latteMoyPdr
        laitEnCours += latteMoyLait
      } else if (latteTailleChoix == 3) {
        boissonChoisiPrix = latteGrandPrix
        prixTot += latteGrandPrix
        poudreEnCours += latteGrandPdr
        laitEnCours = latteGrandLait
      }
    }

    var sucreChoix = 0
    var sucreEnCours = 0
    var sucreEnCoursPrix = 0.0
    while (sucreChoix < 1 || sucreChoix > 4) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      sucreChoix = readLine().toInt
    }
    if (sucreChoix == 2) {
      sucreEnCours += sucrePeu
      sucreEnCoursPrix = scrPeuPrix
    } else if (sucreChoix == 3) {
      sucreEnCours = sucreMoy
      sucreEnCoursPrix = scrMoyPrix
    } else if (sucreChoix == 4) {
      sucreEnCours += sucreBcp
      sucreEnCoursPrix = scrBcpPrix
    }
    var laitChoix = 0
    if (boissonChoix != 1) {
      while (laitChoix < 1 || laitChoix > 2) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        print("> ")
        laitChoix = readLine().toInt
      }
      var laitDoseQtnChoisi = 0
      if (laitChoix == 1) {
        while (laitDoseQtnChoisi < 1 || laitDoseQtnChoisi > 3) {
          println("Combien de dose ?")
          print("> ")
          laitDoseQtnChoisi = readLine().toInt
        }
        laitEnCours += laitDoseQtnChoisi * uneDoseQtn
        laitEnCoursPrix = uneDoseLaitPrix * laitDoseQtnChoisi
      }
    }

    var boissonNom = ""
    if (boissonChoix == 1) {
      boissonNom = "Expresso"
    } else if (boissonChoix == 2) {
      boissonNom = "Cappuccino"
    } else {
      boissonNom = "Latte"
    }
    if (boissonChoix == 1 || boissonChoix == 2) {
      println("Boisson sélectionnée : " + boissonNom)
    } else {
      if (latteTailleChoix == 1) {
        println("Boisson sélectionnée : " + boissonNom + " (Petit)")
      } else if (latteTailleChoix == 2) {
        println("Boisson sélectionnée : " + boissonNom + " (Moyen)")
      } else {
        println("Boisson sélectionnée : " + boissonNom + " (Grand)")
      }
    }

    if (sucreChoix == 1) {
      println("Niveau de sucre : Sans sucre")
    } else if (sucreChoix == 2) {
      println("Niveau de sucre : Peu (5g)")
    } else if (sucreChoix == 3) {
      println("Niveau de sucre : Moyen (10g)")
    } else {
      println("Niveau de sucre : Beaucoup (15g)")
    }

    if (laitChoix == 1) {
      println("Lait supplémentaire: Oui\n")
    } else {
      println("Lait supplémentaire: Non\n")
    }
    if (!machines(machineId).removeIngredient("milk", laitEnCours)) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Choisissez une autre machine.")
      return false
    }
    if (!machines(machineId).removeIngredient("coffee", poudreEnCours)) {
      println("Erreur : Quantité de poude de café insuffisante pour préparer la boisson sélectionnée.")
      println("Choisissez une autre machine.")
      return false
    }
    if (!machines(machineId).removeIngredient("sugar", sucreEnCours)) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println("Choisissez une autre machine.")
      return false
    }
    printf("Prix total : CHF %.2f", boissonChoisiPrix)
    if (sucreChoix > 1) {
      printf(" + CHF %.2f", sucreEnCoursPrix)
    }
    if (laitChoix == 1) {
      printf(" + CHF %.2f", laitEnCoursPrix)
    }
    prixTot = laitEnCoursPrix + sucreEnCoursPrix + boissonChoisiPrix
    printf(" = CHF %.2f \n\n", prixTot)

    println("Veuillez payer en utilisant Twint.")
    val random = Random.alphanumeric.take(5).mkString("")
    println("Votre code de paiement est : " + random)
    println("(En attente de validation du paiement...)\n")
    Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
    println("Merci ! Votre paiement a été accepté.\n")

    println("Préparation de la boisson...")
    println("[...]")
    Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
    println("Votre " + boissonNom + " est prêt ! Bonne dégustation !\n")
    true
  }

  def restockMachine(machineId: Int): Unit = {
    println("Stock:")
    println("    Poudre de café: " + machines(machineId).coffee + "g")
    println("    Lait         : " + machines(machineId).milk + "ML")
    println("    Sucre        : " + machines(machineId).sugar + "g\n")

    println("Réapprovisionnement des stocks...")
    println("Ajout :")
    print("    Poudre de café: ")
    val ajoutPdr = readLine().toInt
    print("    Lait          : ")
    val ajoutLait = readLine().toInt
    print("    Sucre         : ")
    val ajoutSucre = readLine().toInt
    if (ajoutPdr < 0 || ajoutLait < 0 || ajoutSucre < 0){
      println("Erreur : Veuillez saisir des valeurs valides.")
    } else {
      machines(machineId).addIngredient("coffee", ajoutPdr)
      machines(machineId).addIngredient("sugar", ajoutSucre)
      machines(machineId).addIngredient("milk", ajoutLait)
      println("Niveaux de stock mis à jour.")
    }
    println("Retour au menu principal...")
    Thread.sleep(1000)
  }


  def selectMachine(): Int = {
    print("Machine sélectionnée (1-" + machines.size + ") > ")
    var machineId = readLine().toInt
    while (machineId < 1 || machineId > machines.size) {
      println("Veuillez saisir une valeur entre 1 et " + machines.size + ".")
      print("Machine sélectionnée > (1-" + machines.size + ")")
      machineId = readLine().toInt
    }
    machineId - 1
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    machines = loadcsv("machines.csv")

    for (machine <- machines) {
      println("\nMachine " + machine.id + " chargée :")
      println("    ID: " + machine.id)
      println("    Code PIN: " + machine.pincode)
      println("    Lait: " + f"${machine.milk / 1000.0}%.3fL")
      println("    Sucre: " + machine.sugar)
      println("    Café: " + machine.coffee)
    }
    println("\n" + machines.size + " machine(s) chargée(s) avec succès.")


    while(true) {
      var choixMode = 0
      while (choixMode < 1 || choixMode > 3) {
        println("\n        Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        choixMode = readLine().toInt
      }

      var machineId = -1
      if (choixMode == 1) {
        machineId = selectMachine()
        var isSucceed = serveClient(machineId)
        while (!isSucceed) {
          machineId = selectMachine()
          isSucceed = serveClient(machineId)
        }
      } else if (choixMode == 2) {
        var choixModeAdmin = 0
        while (choixModeAdmin != 1 && choixModeAdmin != 2) {
          println("Veuillez sélectionner votre action :")
          println("1) Remplir le stock")
          println("2) Modifier le PIN")
          print("> ")
          choixModeAdmin = readLine().toInt
        }
        println()
        machineId = selectMachine()
        validatePin(machineId)
        if (choixModeAdmin == 1) {
          restockMachine(machineId)
        } else if (choixModeAdmin == 2) {
          updatePin(machineId)
        }
      } else if (choixMode == 3) {
        println("Sauvegarde des machines dans machines.csv...")
        savecsv("machines.csv", machines)
        System.exit(0)
      }
    }
  }
}