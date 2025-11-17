import java.io._
import scala.io.Source
import scala.io.StdIn._
import scala.util._
import scala.collection.mutable.ArrayBuffer

object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    // Méthode permettant d'ajouter des quantités de x ingrédiants (utilisé dans fa méthose main quand tu es dans le mode 2 (admin) et que tu veux restock)
    def addIngrediant(ingredient: String, amount: Int): Unit = {
      if (ingredient == "Sucre") {
        sugar += amount
      }
      if (ingredient == "Lait") {
        milk += amount
      }
      if (ingredient == "Poudre de café") {
        coffee += amount
      }
    }

    // Méthode permettant de retirer des quantités de x ingrédiants (utilisé dans fa méthose main quand tu es dans le mode 1 (client) et que tu veux retirer dans les stocks quand tu prépare une boisson)
    // Elle ressort un booléen afin de vérifier que niveau stocj on est pas dans le négatif
    def removeIngredient(ingredient:String, amount: Int): Boolean = {
      var StockOK = false
      if (ingredient == "Sucre" && (sugar - amount)>=0) {
        sugar -= amount
        StockOK = true
      }
      if (ingredient == "Lait" && (milk - amount)>=0) {
        milk -= amount
        StockOK = true
      }
      if (ingredient == "Café" && (coffee - amount)>=0) {
        coffee -= amount
        StockOK = true
      }
      return StockOK
    }

    //Cette méthode permet d'afficher les machines et leur caractéristqieues (Id, pin, ...)
    // Appelez dand loadcsv(), on profite qu'une machines est insérée dans le pour immédiatement l'afficher
    def affiche: Unit = {
      println("Machine "+id+" chargée :")
      println("    ID: "+id)
      println("    Code PIN: " + pincode)
      printf("    Lait: %.3fL", milk/1000.0)
      println("\n    Sucre : "+sugar+"g")
      println("    Café : "+coffee+"g\n")
      Thread.sleep(500)
    }

    //Permet de valider l'accès à une machine en tant qu'admin
    //Utiliser dans le mode 2
    def validatePin: Boolean = {
      var validationPin = false
      val vraiPIN = pincode.toString
      var tentatives = 3
      var tentativePin = readLine("\nEntrez le code PIN : ")
      while ((vraiPIN != tentativePin) && (tentatives > 0)) {
        tentatives -= 1
        if (tentatives == 0) {
          println("\nTrop de tentatives échouées. Fin du programme.")
          validationPin = false
        } else {
          println("Code PIN incorrect. " + tentatives + " tentative restante.")
          tentativePin = readLine("> ")
        }
      }
      if (vraiPIN == tentativePin) {
        validationPin = true
      }
      return validationPin
    }
    //Permet de mettre à jour le pin d'une machine
    def updatePin: Unit = {
      var nouveauPin = ""
      println("Mise à jour du code PIN pour la Machine " + id + ".")
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while (nouveauPin.toIntOption.isEmpty && (nouveauPin.length != 6)) {
        nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      //remplacement du nouveau pin
      pincode = nouveauPin
      Thread.sleep(2000)
      println("Le code PIN a été mis à jour avec succès.")
      Thread.sleep(2000)
      println("Retour au menu principal...")
      Thread.sleep(1000)
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("Chargement des machines depuis machines.csv...\n")
    var toutesLesMachines = new ArrayBuffer[Machine] //arraybuffer ou ya toutes les machines
    val fr = Source.fromFile(filename + ".csv")
    val lignefr = fr.reset.getLines.drop(1)
    var i = 0 //sert pour l'ID
    while (!lignefr.isEmpty) {
      var ligne = lignefr.next
      var uneMachine = ligne.split(",")
      i += 1
      toutesLesMachines += new Machine(i, uneMachine(0), uneMachine(1).toInt, uneMachine(2).toInt, uneMachine(3).toInt)
      toutesLesMachines(i-1).affiche //-1 parce que le premier élément de toutesLesMachines(0), donc i-1
      //affiche les caractéristiques
    }
    fr.close
    println(i+" machine(s) chargée(s) avec succès")
    return toutesLesMachines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    var fw = new PrintWriter(filename + ".csv")
    fw.println("PINCODE,MILK,SUGAR,COFFEE")
    for (i <- machines) {
      fw.println(i.pincode+","+i.milk+","+i.sugar+","+i.coffee)
    }
    fw.close()
    println("Fichier sauvegardé avec succès")
  }

  def selectionMachine : Int = {
    var IDmachine = 0
    do {
      try {
        IDmachine = readLine("Machine sélectionnée (1-5) > ").toInt
      }
      catch {
        case _=> IDmachine = selectionMachine
        //Je boucle la fonction afin d'avoir encore et encore la question si je donne une valeur érronée
      }
    } while ((IDmachine < 1) || (IDmachine > 5))
    return IDmachine
  }

  def main(args: Array[String]): Unit = {
    var running = false // si running est true, alors on continue, si running est false alors on finit le programme (cas du mode 3)
    var machines = new ArrayBuffer[Machine]
    try {
      machines = loadcsv("machines")
      running = true
      //si on arrive à mettre les données des machines dans la var machines, alors ca va commencé à runner et boucler jusqu'à qu'on lui demande de s'arrêter
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Erreur : fichier introuvable. Vérifiez le chemin d'accès et réessayez")
    }
    while (running) {
      println("\n        Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var mode = readLine("> ")

      while ((mode != "1") && (mode != "2") && (mode != "3")) {
        println()
        mode = readLine("Insérez une valeur correcte : ")
      } //cette boucle while va tourner tant que l'utilisateur ne va pas donner une valeur égale à 1 ou 2 ou 3

      Thread.sleep(1000)

      if (mode == "1") {
        var cafeNecessaire = 0
        var laitNecessaire = 0
        var sucreNecessaire = 0

        // Code de paiement
        var codePaiement = ""

        var prixFinal = 0.0
        var prixCafe = 0.0
        var prixSucre = 0.0
        val prixLait = 0.05

        var prixLaitSupp = 0.0
        var nomBoisson = "" // permet d'afficher le nom de la boisson
        var Boisson = "" //ca sert pour quand on donne le choix et il faut donner 1, 2 ou 3
        var QuantiteSucre = ""

        var nbDosesLaitSup = ""
        var laitSupp = "non"

        var IDmachine = selectionMachine
        IDmachine -= 1

        println("\n        Mode Client")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        Boisson = readLine("> ")
        while (Boisson != "1" && Boisson != "2" && Boisson != "3") {
          Boisson = readLine("Insérez une valeur correcte : ")
        }

        if (Boisson == "1") {
          nomBoisson = "Expresso"
          cafeNecessaire += 8
          prixCafe += 2.0
        }
        if (Boisson == "2") {
          nomBoisson = "Cappuccino"
          cafeNecessaire += 6
          laitNecessaire += 100
          prixCafe += 2.50
        }
        if (Boisson == "3") {
          println()
          println("De quelle taille souhaitez-vous votre Latte ?")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          var tailleLatte = readLine("> ")

          while ((tailleLatte != "1") && (tailleLatte != "2") && (tailleLatte != "3")) {
            println()
            tailleLatte = readLine("Insérez une valeur correcte : ")
          }

          if (tailleLatte == "1"){
            nomBoisson = "Latte (Petit)"
            cafeNecessaire += 6
            laitNecessaire += 120
            prixCafe += 2.70
          }
          if (tailleLatte == "2"){
            nomBoisson = "Latte (Moyen)"
            cafeNecessaire += 8
            laitNecessaire += 150
            prixCafe += 3.20
          }
          if (tailleLatte == "3"){
            nomBoisson = "Latte (Grand)"
            cafeNecessaire += 12
            laitNecessaire += 200
            prixCafe += 3.70
          }
        }

        // Ajouter du sucre
        println()
        println("\nSouhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        var choixSucre = readLine("> ")

        while ((choixSucre != "1") && (choixSucre != "2") && (choixSucre != "3") && (choixSucre != "4")) {
          choixSucre = readLine("Insérez une valeur correcte : ")
        }

        if (choixSucre == "1") {
          QuantiteSucre = "Sans sucre"
          sucreNecessaire = 0
          prixSucre = 0
        }
        if (choixSucre == "2" ){
          QuantiteSucre = "Peu (5g)"
          sucreNecessaire = 5
          prixSucre = 0.1
        }
        if (choixSucre == "3") {
          QuantiteSucre = "Moyen (10g)"
          sucreNecessaire = 10
          prixSucre = 0.2
        }
        if (choixSucre == "4") {
          QuantiteSucre = "Beaucoup (15g)"
          sucreNecessaire = 15
          prixSucre = 0.3
        }
        if ((Boisson == "2") || (Boisson == "3")) {
          println()
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          var choixLaitSup = readLine("> ")
          while ((choixLaitSup != "1") && (choixLaitSup != "2")) {
            println()
            println("Entrée invalide, veuillez réessayer : ")
            choixLaitSup = readLine("> ")
          }
          if (choixLaitSup == "1") {
            println()
            println("Combien de doses ?")
            nbDosesLaitSup = readLine("> ")
            while ((nbDosesLaitSup != "1") && (nbDosesLaitSup != "2") && (nbDosesLaitSup != "3")) {
              println("\nErreur : Vous pouvez ajouter entre 1 et 3 doses de lait seulement")
              println("Combien de doses ?")
              nbDosesLaitSup = readLine("> ")
            }
            // Réduction des stocks de lait en fonction des doses
            laitSupp = "Oui"
            laitNecessaire += nbDosesLaitSup.toInt * 50 // chacune des doses mesure 0.050 L
            prixLaitSupp += nbDosesLaitSup.toInt * prixLait
          }
        }
        // Vérification des stocks
        val cafeOK = machines(IDmachine).removeIngredient("Café", cafeNecessaire)
        val sucreOK = machines(IDmachine).removeIngredient("Sucre", sucreNecessaire)
        val laitOK = machines(IDmachine).removeIngredient("Lait", laitNecessaire)
        try {
          savecsv("machines", machines)
        }
        catch {
          case ex : java.nio.file.AccessDeniedException => println("\nErreur : Échec de l'écriture dans la machine.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
          case _=> running = false
        }
        if (cafeOK && sucreOK && laitOK) {
          println("\n\nBoisson sélectionnée : "+ nomBoisson)
          println("Niveau de sucre : "+ QuantiteSucre)
          println("Lait en supplément : "+ laitSupp)

          // Calcul et affichage du prix total
          prixFinal = (prixCafe + prixSucre + prixLaitSupp).toFloat

          //Affichage
          printf("Prix total : CHF %.2f", prixCafe)
          if (prixSucre > 0 || prixLaitSupp > 0) {
            if (prixSucre > 0) {
              printf(" + CHF %.2f",prixSucre)
            }
            if (prixLaitSupp > 0) {
              printf(" + CHF %.2f",prixLaitSupp)
            }
            printf(" = CHF %.2f", prixFinal)
          }

          codePaiement = Random.alphanumeric.take(5).mkString.toUpperCase() //génère un code aléatoire de 5 nombres ou chiffres
          println("\nVeuillez payer avec Twint")
          println("Votre code de paiement est :"+ codePaiement)
          println("(En attente de validation du paiement...)")
          Thread.sleep(2000) // Attend pendant 2000 millisecondes (2 secondes)
          println()
          println("Merci, votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(2000) // Attend pendant 2000 millisecondes (2 secondes)
          if (Boisson == "3") {
            println("Votre Latte est prêt ! Bonne dégustation !")
          } else {
            println("Votre "+nomBoisson+" est prêt ! Bonne dégustation !")
          }
          Thread.sleep(5000)
        } else {
          running = false
        }
      }
      if (mode == "2") {
        var actionAdmin = ""
        var IDmachine = selectionMachine
        IDmachine -= 1
        println("\n        Mode Admin")
        println("Quelle opération voulez vous faire? ")
        println("1) Restock")
        println("2) Changement du PIN")
        actionAdmin = readLine("> ")
        while ((actionAdmin != "1") && (actionAdmin != "2")) {
          actionAdmin = readLine("Insérez une valeur correcte : ")
        }

        if (actionAdmin == "1" && machines(IDmachine).validatePin){
          println("\nNiveaux de stock actuels:")
          println("    Poudre de café : " + machines(IDmachine).coffee + "g")
          println("    Sucre : " + machines(IDmachine).sugar + "g")
          printf("    Lait : %.3fL", machines(IDmachine).milk/1000.0)
          println("\nEntrez les quantités à ajouter")
          print("Poudre de café > ")
          var addcoffee = readInt()
          machines(IDmachine).addIngrediant("Poudre de café", addcoffee)
          print("Sucre > ")
          var addSugar = readInt()
          machines(IDmachine).addIngrediant("Sucre", addSugar)
          print("Lait > ")
          var addMilk = (readFloat()*1000).toInt
          machines(IDmachine).addIngrediant("Lait", addMilk)
          Thread.sleep(1000)
          try {
            savecsv("machines", machines)
          }
          catch {
            case ex : java.nio.file.AccessDeniedException => println("\nErreur : Échec de l'écriture dans la machine.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
            case _=> running = false
          }
          println("Les stocks ont été mis à jour avec succès.")
          Thread.sleep(1000)
          println("Retour au menu principal...")
          Thread.sleep(1000)
        }
        if (actionAdmin == "2" && machines(IDmachine).validatePin){
          machines(IDmachine).updatePin
          try {
            savecsv("machines", machines)
          }
          catch {
            case ex : java.nio.file.AccessDeniedException => println("\nErreur : Échec de l'écriture dans la machine.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
            case _=> running = false
          }
        }
      }
      if (mode == "3") {
        try {
          println("Sauvegarde de 5 machines dans csv")
          savecsv("machines", machines)
          println("Fichier sauvegardé avec succès")
        }
        catch {
          case  ex : java.nio.file.AccessDeniedException => println("\nErreur : Échec de l'écriture dans la machine.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        }
        finally {
          running = false
        }
      }
    }
  }
}