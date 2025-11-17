import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}

object Main {

  // ---FONCTION--------------------------------------------------------------------------------------------

  class Machine (var ID: Int, var codePin: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient match {
        case "coffee" =>
          coffee += amount
        case "milk" =>
          milk += amount
        case "sugar" =>
          sugar += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient match {
        case "coffee" =>
          if (coffee - amount < 0) {
            return false
          }
          coffee -= amount
        case "milk" =>
          if (milk - amount < 0) {
            return false
          }
          milk -= amount
        case "sugar" =>
          if (sugar - amount < 0) {
            return false
          }
          sugar -= amount
        case _ =>
          println("Erreur l'ingrédient n'existe pas!")
          return false
      }
      return true
    }

    def display(): Unit = {
      println("Machine " + ID + " chargée:")
      println("ID: " + ID)
      println("Code PIN : " + codePin)
      println("MILK : " + (milk/1000.0) + "L")
      println("SUGAR : " + sugar + "g")
      println("COFFEE : " + coffee + "g")
      println()
    }
  }

  def loadCsv(filePath: String): ArrayBuffer[Machine] = {
    val listMachine = ArrayBuffer[Machine]()

    var source: Source = null

    try {
      source = Source.fromFile(filePath)
    }
    catch {
      case e: Exception => println("Erreur: Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
    }

    source.getLines().next()

    println("Chargement des machines depuis " + filePath + "...")
    println()

    var counter = 1

    for (line <- source.getLines.toList) {
      val cols = line.split(",").toList

      try{

        val machine = new Machine(counter, cols.head, cols(1).toInt, cols(2).toInt, cols(3).toInt)
        machine.display()
        listMachine += machine
        counter += 1
      }
      catch {
        case e: Exception => println("Erreur: Échec de l'écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
      }
    }

    println((counter-1) + "  machine(s) chargée(s) avec succès.")
    println()
    source.close()
    return  listMachine
  }

  def saveCsv(filePath: String, listMachine: ArrayBuffer[Machine]): Unit = {
    var source: Source = null

    try {
      source = Source.fromFile(filePath)
    }
    catch {
      case e: Exception => println("Erreur: Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }

    val writer = new java.io.PrintWriter(filePath)
    writer.write("ID,PIN,MILK,SUGAR,COFFEE\n")
    var counter = 0
    for (machine <- listMachine) {
      counter += 1
      writer.write(machine.codePin + "," + machine.milk + "," + machine.sugar + "," + machine.coffee + "\n")
    }

    println("Sauvegarde de " + counter + " machine(s) dans " + filePath + "...")
    println("Fichier sauvegardé avec succès.")
    println()
    writer.close()
  }

  // Cette fonction verifie l'input donné par l'User et correspond
  // au code Pin de la machine selectionnee
  def validatePin(machineId: Int, machines : ArrayBuffer[Machine]): Boolean = {
    println("Entrez le pin de la machine n°" + machineId + ": ")
    val inputPinCode = readLine()

    if (machineId <= 0 || machineId > machines.length){
      println("Erreur: ID machine incorrect.")
      return false
    }

    if (inputPinCode == machines(machineId-1).codePin) {
      println("Accès accordé à la machine n°" + machineId)
      return true
    }
    println("Accès non autorisé!")
    return false
  }

  // Affiche les stocks de la machine donnée
  def displayStockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    machines(machineId - 1).display()
  }

  // Renvoi une valeur Int
  // Cela correspond a la quantité que l'Admin souhaite rajouter
  def quantiteIngredient(ingredient: String, machineId: Int): Int = {
    var quantite = 0 // Initialiser à une valeur négative pour entrer dans la boucle

    do {
      if (quantite < 0) {
        println(s"Erreur valeur négative : $ingredient. Quantité impossible. SVP donner une valeur positive ou nulle.")
      }
      println(s"Quantité de $ingredient à rajouter dans la machine n°$machineId : ")
      quantite = scala.io.StdIn.readInt()
    } while (quantite < 0)

    quantite
  }

  // Cette fonction ajoute du stock en plus à une machine donnée (machineId)
  def restockMachine(machineId: Int, machines : ArrayBuffer[Machine]): Unit = {
    println("Ajout stock dans la machine n°" + machineId)

    displayStockMachine(machineId, machines)

    var coffee = quantiteIngredient("Poudre de café", machineId)
    machines(machineId - 1).addIngredient("coffee", coffee)

    var sugar = quantiteIngredient("Sucre", machineId)
    machines(machineId - 1).addIngredient("sugar", sugar)

    var milk = quantiteIngredient("Lait en mL", machineId)
    machines(machineId - 1).addIngredient("milk", milk)

    println("Les stocks ont été mise à jour avec succès.")
    println()

    displayStockMachine(machineId, machines)
  }

  // Modifier le Pin de la machine selectionnée
  def updatePin(machineId: Int, machines : ArrayBuffer[Machine]): Unit = {
    var respectRegle = false

    println("Mise à jour du code PIN pour la machine n°" + machineId)
    println()

    do{
      println("Entrez le nouveau code PIN de la machine: ")
      val inputCodePin = readLine()

      respectRegle = ((inputCodePin.length == 6) && inputCodePin.forall(_.isDigit))

      if (respectRegle == true) {
        machines(machineId-1).codePin = inputCodePin
        println("Le code PIN a été mis à jour avec succès")
      } else {
        println("Erreur : Le code PIN donné ne respecte pas la règle.")
        println("Le code PIN doit avoir 6 chiffres ")
        println()
      }
    } while (respectRegle != true)
  }

  def paiementTwint(totalprix: Double): Unit = {

    println(f"Prix total: CHF $totalprix%.2f. Veuillez payer en utilisant Twint.")

    val lettre = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var aleatoire = ""

    for (i <- 1 to 5) {
      aleatoire += lettre((Math.random() * lettre.length).toInt)
    }
    println("Votre code de paiement est:" + aleatoire + " \n(En attente de validation du paiement)")
    Thread.sleep(3000)

    println("Merci ! Votre paiement a été accepté.")
    Thread.sleep(3000)
    println("Préparation de votre boisson...\n[...]")
    Thread.sleep(3000)

    println("Votre boisson est prête ! Bonne dégustation !")
    Thread.sleep(5000)
  }

  def serveClient(machineId: Int, machines : ArrayBuffer[Machine]): Boolean = {

    var coffee = 0
    var sugar = 0
    var milk = 0
    var prixBoisson = 0.0
    var choixBoisson = 0

    do {
      println("Veuillez sélectionner votre boisson:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
      choixBoisson = readInt()
      println()
    }
    while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)

    do {
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre - CHF 0.00\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
      sugar = readInt()
    } while (sugar != 1 && sugar != 2 && sugar != 3 && sugar != 4)
    var prixdusucre = (sugar - 1) * 0.10


    var doseMilk = 0
    var prixLait = 0.0

    if (choixBoisson == 2 || choixBoisson == 3) {


      var supplementLait = 0

      do {
        println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui \n2) Non\n>")
        supplementLait = readInt()
      } while (supplementLait != 1 && supplementLait != 2)

      if (supplementLait == 1) {
        do {
          println("Combien de doseMilk ?\n1) 1 doseMilk (50mL) - CHF 0.05\n2) 2 doseMilk (100mL) - CHF 0.10\n3) 3 doseMilk (150mL) - CHF 0.15\n>")
          doseMilk = readInt()
        }
        while (doseMilk != 1 && doseMilk != 2 && doseMilk != 3)
        prixLait = doseMilk * 0.05
      }
    }

    if (choixBoisson == 1) {
      coffee = 8
      prixBoisson = 2.00
    }
    else if (choixBoisson == 2) {
      coffee = 6
      milk = 100
      prixBoisson = 2.50
    }
    else if (choixBoisson == 3) {
      var taille = 0
      do {
        println("Veuillez choisir votre taille:\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>")
        taille = readInt()
      }
      while (taille != 1 && taille != 2 && taille != 3)

      if (taille == 1) {
        coffee = 6
        milk = 120
        prixBoisson = 2.70

      } else if (taille == 2) {

        coffee = 8
        milk = 150
        prixBoisson = 3.20

      } else if (taille == 3) {
        coffee = 12
        milk = 200
        prixBoisson = 3.70
      }
    }
    milk = milk + doseMilk * 50


    if ((machines(machineId - 1).coffee - coffee) < 0) {
      println("Erreur: Quantité de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
      return true
    }

    if ((machines(machineId - 1).sugar - sugar) < 0) {
      println("Erreur: Quantité de sucre  insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
      return true
    }

    if ((machines(machineId - 1).milk - milk) < 0) {
      println("Erreur : Quantité de lait  insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
      return true
    }

    var prixTotal = prixBoisson + prixdusucre + prixLait
    paiementTwint(prixTotal)

    println("ServerClient")
    displayStockMachine(machineId, machines)

    machines(machineId - 1).removeIngredient("coffee", coffee)
    machines(machineId - 1).removeIngredient("sugar", coffee)
    machines(machineId - 1).removeIngredient("milk", coffee)

    displayStockMachine(machineId, machines)

    return false
  }

  // ---FIN - FONCTION--------------------------------------------------------------------------------------------


  def main(args: Array[String]): Unit = {

    println("Entrez le nom du fichier qu'il faut load:")
    val filePath = readLine()

    var machines = loadCsv(filePath)

    var machineId = 0 // Numero de la machine selectionnée
    var tentativePin = 3

    var mode = 0

    while (mode == 0 || mode == 1 || mode == 2) {

      if (mode == 0) {

        do {
          println("Noespresso Café\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin\n3) Quitter\n>")
          mode = readInt()
          println()
        }
        while (mode != 1 && mode != 2 && mode != 3)
      }

      if (mode == 1) {
        do{
          println("Entrer l'Id d'une machine: ")
          machineId = readInt()
          println()
        } while ((machineId < 1) || (machineId > machines.length))

        var nbError = 0
        var error = serveClient(machineId, machines)

        if (error) {
          nbError = 1
        }

        if (error == true ) {
          mode = 1
        }
        else {
          println("Retour au menu principal...")
          println()
          mode = 0
        }

      }
      else if (mode == 2) {

        do {
          println("Mode Admin\nEntrer l'Id d'une machine: ")
          machineId = readInt()
          println()

        } while ((machineId < 1) || (machineId > machines.length))


        var codePinIncorrect = 0
        var valide = false

        do {
          valide = validatePin(machineId, machines)

          if (valide == false) {
            codePinIncorrect += 1
            println("Code PIN incorret." + (tentativePin - codePinIncorrect) + " tentatives restantes.")
            println()
          }
          else {
            valide = true

          }
        } while (valide != true && codePinIncorrect != 3)

        if (codePinIncorrect == 3) {
          println("Trop de tentatives échouées. Fin du programme.")
          println()
          mode = 3
        } else {

          var action = 0

          do {
            println("Bonjour que voulez vous faire:\n1) AJouter du stock à la machine n°" + machineId + ("\n2) Modifier le PIN de la machine n°" + machineId) +  "\n3) Ajouter une nouvelle machine" + "\n4) Quitter")
            println()
            action = readInt()

          } while (action != 1 && action != 2 && action != 3 && action != 4 )


          if (action == 1) { // Ajouter du stock
            restockMachine(machineId, machines)


          }
          else if (action == 2) { // Changer le PIN
            updatePin(machineId, machines)

          } else if (action == 3) {
            println("Ajouter une nouvelle machine")

            val idMachine = machines.length+1
            machines += new Machine(idMachine, "000000", 0, 0, 0)

            machines(idMachine-1).display()

            println("S'il vous plait entrer le code PIN de la nouvelle machine:")
            println("Mot de passe actuel: 000000")
            updatePin(idMachine, machines)


            println("Voulez vous ajouter du stock à la machine n°" + idMachine + " ? \n1) Oui \n2) Non")
            val reponse = readInt()

            if (reponse == 1) {
              restockMachine(idMachine, machines)
            }
          }

          println("Retour au menu principal...")
          mode = 0
        }

      }
      else {

        saveCsv(filePath, machines)

        println("Fin du programme!")
      }
    }

  }
}