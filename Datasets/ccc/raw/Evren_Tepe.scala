import collection.mutable.ArrayBuffer
import scala.io.StdIn._
import java.io.{FileWriter, PrintWriter}
import scala.io.Source._
import scala.util.Random

object Main {
  //Classe de machines
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    //Ajout d'une nouvelle quantité de stock
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk")
        milk += amount
      else if (ingredient == "sugar")
        sugar += amount
      else if (ingredient == "coffee")
        coffee += amount
      else
        println("Erreur : le nom du ingredient est incorrect.")
    }

    //Déduction du stock en cas de consommation de boissons
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "coffee" && amount <= coffee) {
        coffee -= amount
        return true
      }
      else if (ingredient == "sugar" && amount <= sugar) {
        sugar -= amount
        return true
      }
      else if (ingredient == "milk" && amount <= milk) {
        milk -= amount
        return true
      }
      else {
        return false
      }
    }

    //Pour les lignes de fichiers CSV
    def imprimerCSV(): String = {
      //PINCODE,MILK,SUGAR,COFFEE
      return pincode + "," + milk + "," + sugar + "," + coffee
    }

    //Impression des informations sur la machine à l'écran
    def affichageMachine(): Unit = {
      println("Machine " + id + " chargée :")
      println("    ID: " + id)
      println("    Code PIN: " + pincode)
      printf("    Lait: %.3fL", milk * 0.001)
      println("\n    Sucre: " + sugar + "g")
      println("    Café: " + coffee + "g\n")
    }

    //Impression des informations de stock de la machine sur l'écran
    def affichageStock(): Unit = {
      println("\nNiveaux de stock actuels :")
      println("    Poudre de café : " + coffee + "g")
      println("    Sucre : " + sugar + "g")
      printf("    Lait: %.3fL\n", milk * 0.001)
    }

    //Contrôle de validité du code PIN
    def validatePin(): Boolean = {
      var tentative = 3
      var saisiPIN = ""
      println("Entrez le code PIN : ")
      while (tentative > 0) {
        saisiPIN = readLine(">")
        if (pincode != saisiPIN) {
          tentative -= 1
          if (tentative > 1)
            println("Code PIN incorrect. " + tentative + " tentatives restantes.")
          else if (tentative == 1)
            println("Code PIN incorrect. " + tentative + " tentative restante.")
          else
            return false
        }
        else {
          return true
        }
      }
      return false
    }

    //Changer le code PIN
    def updatePin(): Unit = {
      var nouveauPin = ""
      println("Mise à jour du code PIN pour la Machine " + id + ".")
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
        nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      pincode = nouveauPin
      println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    }
  }

  //Chargement de machines à partir d'un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("\nChargement des machines depuis machines.csv...")
    try {
      val fr = fromFile(filename)
      val lignefr = fr.reset.getLines
      var ligne = lignefr.next
      val machines = new ArrayBuffer[Machine]()
      var i = 0
      while (!lignefr.isEmpty) {
        ligne = lignefr.next
        var machine = ligne.split(",")
        machines += new Machine(i + 1, machine(0), machine(1).toInt, machine(2).toInt, machine(3).toInt)
        i += 1
      }
      return machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("\nErreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return null
      case ex: java.lang.ArrayIndexOutOfBoundsException => println("\nErreur : Ce fichier contient des lignes corrompues, ce qui empêche sa lecture.")
        return null
      case ex: java.util.NoSuchElementException => println("\nErreur : Le fichier semble être vide ou ne contient pas de données exploitables.")
        return null
      case ex: java.lang.NumberFormatException => println("\nErreur : Ce fichier contient des lignes corrompues, ce qui empêche sa lecture.")
        return null
      case ex: Exception => println("\nErreur : Une erreur inattendue s'est produite")
        return null
    }
  }

  //Enregistrer les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println("Sauvegarde de " + machines.length + " machines dans machines.csv...")

    try {
      val printWriter = new PrintWriter(filename)
      //val printWriter = new PrintWriter(new FileWriter(filename, true))
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for (i <- 0 until machines.length) {
        printWriter.println(machines(i).imprimerCSV())
      }
      printWriter.close()
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case ex: java.io.FileNotFoundException => println("\nErreur : Le fichier peut être verrouillé ou en lecture seule.")
    }
  }

  //Situations où l'utilisateur doit effectuer des sélections dans différentes parties du programme
  def selection(question: String, choix: Array[String]): Int = {
    var result = 0
    while (result < 1 || result > choix.length) {
      try {
        println("\n" + question + "\n" + choix.mkString("\n"))
        print(">")
        result = readLine().toInt
        if (result < 1 || result > choix.length)
          println("Erreur : Votre sélection n'est pas valide. Veuillez entrer un nombre valide.")
      }
      catch {
        case ex: NumberFormatException =>
          println("Erreur : Votre sélection n'est pas valide. Veuillez entrer un nombre valide.")
          result = 0
      }
    }
    result
  }

  //Sélection de la machine par l'utilisateur
  def selectionMachine(machines: ArrayBuffer[Machine]): Int = {
    var machineId = 0
    while (machineId <= 0 || machineId > machines.length) {
      try {
        machineId = readLine("Machine sélectionnée (1-" + machines.length + ") > ").toInt
        if (machineId <= 0 || machineId > 5)
          println("Erreur : Vous avez fait le mauvais choix.\n")
      }
      catch {
        case ex: java.lang.NumberFormatException => println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer uniquement des chiffres entre les valeurs spécifiées.\n")
      }
    }
    return machineId - 1
  }

  //Sélection des boissons, préparation et opérations de paiement pour le client
  def serveClient(machine: Machine): Boolean = {
    //Créé comme modèle pour la boisson à sélectionner et personnaliser par l'utilisateur
    class Boisson(var id: Int = 0, var typeBoisson: String = "", var tailleBoisson: String = "", var prix: Double = 0.0, var cafePoudre: Int = 0, var lait: Int = 0, var laitSupp: Int = 0, var sucre: Int = 0)

    var boisson = new Boisson()
    var choix = 0

    //Selectionner boisson
    choix = selection("Veuillez sélcectionner votre boisson :", Array("1) Expresso - CHF 2.00", "2) Cappucino - CHF 2.50", "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"))

    if (choix == 1) {
      boisson.typeBoisson = "Expresso"
      boisson.prix = 2.00
      boisson.cafePoudre = 8
    }
    else if (choix == 2) {
      boisson.typeBoisson = "Cappuccino"
      boisson.prix = 2.50
      boisson.cafePoudre = 6
      boisson.lait = 100
    }
    else if (choix == 3) {
      boisson.typeBoisson = "Latte"

      //Selectionner taille
      choix = selection("Veuillez sélectionner la taille de votre boisson :", Array("1) Petit", "2) Moyen", "3) Grand"))

      if (choix == 1) {
        boisson.tailleBoisson = "(Petit)"
        boisson.prix = 2.70
        boisson.cafePoudre = 6
        boisson.lait = 120
      }
      else if (choix == 2) {
        boisson.tailleBoisson = "(Moyen)"
        boisson.prix = 3.20
        boisson.cafePoudre = 8
        boisson.lait = 150
      }
      else if (choix == 3) {
        boisson.tailleBoisson = "(Grand)"
        boisson.prix = 3.70
        boisson.cafePoudre = 12
        boisson.lait = 200
      }
    }

    //Supplementaire sucre
    val options = Array("1) Sans sucre", "2) Peu (5g) - CHF 0.10", "3) Moyen (10g) - CHF 0.20", "4) Beaucoup (15g) - CHF 0.30")
    boisson.sucre = selection("Souhaitez-vous ajouter du sucre ?", options) - 1

    //Supplementaire lait
    if (boisson.typeBoisson != "Expresso") {
      choix = selection("Souhaitez-vous ajouter du lait en supplément ?", Array("1) Oui", "2) Non"))
      if (choix == 1) {
        boisson.laitSupp = selection("Combien de dose ?", Array("1) 1 dose : 50mL", "2) 2 doses : 100mL", "3) 3 doses : 150mL"))
      }
    }

    //Contrôle de l'adéquation des stocks pour la boisson sélectionnée et déduction de la consommation du stock
    if(machine.removeIngredient("coffee", boisson.cafePoudre)){
      if(machine.removeIngredient("sugar", boisson.sucre * 5)){
        if(machine.removeIngredient("milk", boisson.lait + boisson.laitSupp * 50)){
          //Boisson information
          println("-------------------------------------------------------------------------------------")
          println("Boisson sélectionnée : " + boisson.typeBoisson + " " + boisson.tailleBoisson)
          if(boisson.sucre == 0)
            println("Niveau de sucre : Sans sucre")
          else if(boisson.sucre == 1)
            println("Niveau de sucre : Peu (5g)")
          else if(boisson.sucre == 2)
            println("Niveau de sucre : Moyen (10g)")
          else if(boisson.sucre == 3)
            println("Niveau de sucre : Beaucoup (15g)")

          if (boisson.typeBoisson != "Expresso")
            println("Lait en supplément : " + boisson.laitSupp + " dose(s)")

          //Les prix boisson
          printf("Prix total : CHF " + "%.2f", boisson.prix)

          //sucre
          if (boisson.sucre > 0)
            printf(" + CHF %.2f", boisson.sucre * 0.10)

          //lait
          if (boisson.laitSupp > 0)
            printf(" + CHF %.2f", boisson.laitSupp * 0.05)

          //total
          printf(" = CHF %.2f", (boisson.prix + (boisson.sucre * 0.10) + (boisson.laitSupp * 0.05)))
          return true
        }
        else{
          machine.addIngredient("coffee", boisson.cafePoudre)
          machine.addIngredient("sugar", boisson.sucre * 5)
          return false
        }
      }
      else{
        machine.addIngredient("coffee", boisson.cafePoudre)
        return false
      }
    }
    else{
      return false
    }
  }


  def main(args: Array[String]): Unit = {
    var programRunning = true
    var mode = 0
    var machineId = 0
    var choix = 0

    //les machines sont chargées à partir d'un fichier CSV
    val machines: ArrayBuffer[Machine] = loadcsv("machines.csv")
    if (machines != null && machines.length > 0) {
      for (i <- 0 to machines.length - 1) {
        machines(i).affichageMachine()
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")
    }
    else {
      programRunning = false
      println("Échec du chargement des machines")
      println("Fermeture du programme.")
    }

    //Démarrage du programme
    while (programRunning) {
      println("\n               Nospresso Café")

      //Mode de sélection
      var mode = selection("Veuillez sélectionner votre mode: ", Array("1) Client", "2) Admin", "3) Quitter"))

      //Client Mode
      if (mode == 1) {
        println("\nMode Client")

        //Selectionner machine
        machineId = selectionMachine(machines)

        //Si les stocks sont suffisants et que la boisson est préparée
        if (serveClient(machines(machineId))) {
          //Paiment
          println("\n\nVeuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString)
          println("(En attente de paiement...)")
          Thread.sleep(3000)

          //Paiment confirmé
          println("\nPaiement confirmé.")
          println("Préparation de votre boisson...")
          println("Votre boisson est prêt ! Bonne dégustation !")
          println("-------------------------------------------------------------------------------------")
        }
        //Si les stocks ne suffisent pas
        else {
          print("\nErreur : Les stocks de cette machine sont insuffisants pour la boisson choisie. Veuillez choisir une autre boisson ou une autre machine.\n")
        }
      }

      //Admin Mode
      else if (mode == 2) {
        println("\nMode Admin")
        machineId = selectionMachine(machines) //Selectionner machine

        //Contrôle du code PIN et opérations d'administration
        if (machines(machineId).validatePin()) {
          println("\nAccès accordé à la Machine " + machines(machineId).id + ".")

          //Selectionner une action
          choix = selection("Veuillez choisir une action :", Array("1) Ajouter un nouveau stock", "2) Modifier le code PIN"))

          //Ajouter un nouveau stock
          if (choix == 1) {
            var amountGram = 0
            var amountLitre = 0.0
            var valide = true
            machines(machineId).affichageStock()

            //Poudre de café
            while(valide){
              try{
                amountGram = readLine("\nPoudre de café (en gram) >").toInt
                if(amountGram >= 0){
                  machines(machineId).addIngredient("coffee", amountGram)
                  amountGram = 0
                  valide = false
                } else {
                  println("Erreur : Veuillez entrer un nombre entier positif.")
                }
              }
              catch {
                case ex: NumberFormatException => println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre entier positif.(En gram)")
                  valide = true
              }
            }

            //Sucre
            valide = true
            while(valide){
              try{
                amountGram = readLine("\nSucre (en gram) >").toInt
                if(amountGram >= 0){
                  machines(machineId).addIngredient("sugar", amountGram)
                  amountGram = 0
                  valide = false
                } else {
                  println("Erreur : Veuillez entrer un nombre entier positif.")
                }
              }
              catch {
                case ex: NumberFormatException => println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre entier positif.(En gram)")
                  valide = true
              }
            }

            //Lait
            valide = true
            while(valide){
              try{
                print("\nLait (en litre) >")
                amountLitre = readDouble()
                if(amountLitre >= 0){
                  machines(machineId).addIngredient("milk", (amountLitre * 1000).toInt)
                  amountLitre = 0.0
                  valide = false
                } else {
                  println("Erreur : Veuillez entrer un nombre positif.")
                }
              }
              catch {
                case ex: NumberFormatException => println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre valide.(En litre)")
                  valide = true
              }
            }

            println("\nLes stocks ont été mis à jour avec succès.")
            machines(machineId).affichageStock()
            println("\nRetour au menu principal...")
          }

          //Modifier le code PIN
          else if (choix == 2) {
            machines(machineId).updatePin()
          }
        }
        //Si le code PIN est mal saisi 3 fois
        else {
          programRunning = false // Pour terminer la boucle while
          println("Trop de tentatives échouées. Fin du programme.")
          savecsv("machines.csv", machines) //Les dernières modifications sont enregistrées dans un fichier CSV
        }
      }

      //Quitter Mode
      else {
        savecsv("machines.csv", machines) //Le dernier état des machines est enregistré dans un fichier CSV
        programRunning = false // Pour terminer la boucle while
        println("Le programme a été terminé.")
      }
    }
  }
}