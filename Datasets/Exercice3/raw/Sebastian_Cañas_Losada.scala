import io.StdIn._
import collection.mutable.ArrayBuffer
import java.io._
import io.Source._

object Main {

  val machines = new ArrayBuffer[Machine]()

  class Machine(id: Int = 0, pincode: String = "123456", milk: Int = 0, sugar: Int = 0, coffee: Int = 0) {
    var machineId = id
    var machinePin = pincode
    var milkStocks = milk
    var sugarStocks = sugar
    var coffeeStocks = coffee


    def affichage(): Unit = {
      println(s"Machine $machineId chargée :")
      println(s"  ID: $machineId")
      println(s"  Code PIN: $machinePin")
      println(f"  Lait    : ${milkStocks / 1000.0}%.3f L")
      println(s"  Sucre   : $sugarStocks g")
      println(s"  Café    : $coffeeStocks g")
      println()
    }

    def validatePin(): Boolean = {
      val tentativeMax = 3
      var tentative = 0
      println("Entrez le code PIN:")

      while (tentative < tentativeMax) {
        val pin = readLine("> ")

        if (pin == machinePin) {
          println(s"Accès accordé pour la machine $machineId")
          println()
          return true
        }
        else {
          tentative += 1
          println(s"Code PIN incorrect. ${tentativeMax - tentative} tentative(s) restante(s).")
        }
      }
      println()
      println("Trop de tentatives échouées.")
      false
    }

    def updatePin(): Unit = {
      var pinValable = false

      println(s"Mise à jour du code PIN pour la Machine $machineId.")

      while (!pinValable) {
        val newPin = readLine(s"Entrez un nouveau code PIN à 6 chiffres > ")

        if (newPin.length == 6 && newPin.forall(_.isDigit)) {
          machinePin = newPin
          println("Le code PIN a été mis à jour avec succès." +
            "\nRetour au menu principal...")
          Thread.sleep(2500)
          pinValable = true
        }
        else {
          // en boucle tant que la condition if n'est pas valide comme demandé par le prof
        }
      }
    }

    def newStock(): Unit = {
      println()
      println("Validation nouveaux niveaux de stock:")
      println(f"  Lait          : ${milkStocks / 1000.0}%.3f L")
      println(s"  Sucre         : ${sugarStocks}g")
      println(s"  Poudre de café: ${coffeeStocks}g")


      println()
      println("Les stocks ont été mis à jour avec succès." +
        "\nRetour au menu principal...")
      println()
      Thread.sleep(2500)
    }

    def positiveAmount(): Int = {
      var valide = false
      var quantite = 0

      while (!valide) {
        try {
          quantite = readLine("> ").toInt
          if (quantite >= 0.0) {
            valide = true
          }
          else {
            println("La quantité doit être plus grande ou égale à 0. Veuillez réessayer.")
          }
        } catch {
          case _: NumberFormatException => println("Entrée invalide. Veuillez entrer uniquement des nombres entiers.")
        }
      }
      return quantite
    }

    def addIngredient(ingredient: String = "ingredient", amount: Int = 0): Unit = {
      var Ingredient = ingredient
      var Amount = amount

      println("Pour quel ingrédient souhaitez-vous modifier la quantité ?" +
        "\n1)lait" +
        "\n2)sucre" +
        "\n3)café")


      try {
        var ingredientValide = false
        while (!ingredientValide) {
          Ingredient = readLine("> ")
          if (Ingredient == "1") {
            Ingredient = "lait"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en millilitre) souhaitez vous ajouter ?")
            Amount = positiveAmount()
            milkStocks += Amount // transformer en ml
            newStock()
            ingredientValide = true
          }
          else if (Ingredient == "2") {
            Ingredient = "sucre"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en gramme) souhaitez-vous ajouter ?")
            Amount = positiveAmount()
            sugarStocks += Amount
            newStock()
            ingredientValide = true
          }
          else if (Ingredient == "3") {
            Ingredient = "café"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en gramme) souhaitez-vous ajouter ?")
            Amount = positiveAmount()
            coffeeStocks += Amount
            newStock()
            ingredientValide = true
          }
          else {
            println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3 qui correspond à:" +
              "\n1)lait" +
              "\n2)sucre" +
              "\n3)café")
          }
        }
      } catch {
        case _: NumberFormatException => println("Entrée invalide. Veuillez entrer uniquement des nombres entiers.")
      }
    }

    def removeIngredient(ingredient: String = "ingredient", amount: Int = 0): Boolean = {
      var Ingredient = ingredient
      var Amount = amount


      try {
        var valide = false
        println("Pour quel ingrédient souhaitez-vous modifier la quantité ?" +
          "\n1)lait" +
          "\n2)sucre" +
          "\n3)café")
        while (!valide) {
          Ingredient = readLine("> ")

          if (Ingredient == "1") {
            Ingredient = "lait"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en millilitre) souhaitez vous enlever ?")
            Amount = positiveAmount()
            Amount = Amount // transformer en ml
            if (Amount <= milkStocks) {
              milkStocks -= Amount
              println(s"$Amount L de $Ingredient retiré. Nouveau stock de $Ingredient: ${milkStocks / 1000.0} L.")
              valide = true
            }
            else {
              println(s"Pas assez de stock pour retirer cette quantité de $Ingredient.")
            }
          }
          else if (Ingredient == "2") {
            Ingredient = "sucre"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en gramme) souhaitez-vous enlever ?")
            Amount = positiveAmount()
            if (Amount <= sugarStocks) {
              sugarStocks -= Amount
              println(s"$Amount g de $Ingredient retiré. Nouveau stock de $Ingredient: $sugarStocks g.")
              valide = true
            }
            else {
              println(s"Pas assez de stock pour retirer cette quantité de $Ingredient.")
            }
          }
          else if (Ingredient == "3") {
            Ingredient = "café"
            println()
            println(s"Vous avez sélectionné: $Ingredient")
            println(s"Quelle quantité de $Ingredient (en gramme) souhaitez-vous ajouter ?")
            Amount = positiveAmount()
            if (Amount <= coffeeStocks) {
              coffeeStocks -= Amount
              println(s"$Amount g de $Ingredient retiré. Nouveau stock de $Ingredient: $coffeeStocks g.")
              valide = true
            }
            else {
              println(s"Pas assez de stock pour retirer cette quantité de $Ingredient.")
            }
          }
          else {
            println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3 qui correspond à:" +
              "\n1)lait" +
              "\n2)sucre" +
              "\n3)café")
          }
        }
      }
      catch {
        case _: NumberFormatException => println("Entrée invalide. Veuillez entrer uniquement des nombres entiers.")
      }
      true
    }

    def serveClient(): Unit = {
      println()
      println("Mode Client")

      // déclarer tableau alpha num
      val AlphaNum = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      //déclaration variable utile
      var prixBase = 0.0 //en CHF
      var cafeBoisson = 0 //en g
      var laitBoisson = 0 //en ml
      var doseLaitSupp = 0 //en ml
      var prixLaitSupp = 0.00 //CHF
      var laitSupp = ""
      var sucreG = 0 //en g
      var prixSucre = 0.00 //en CHF
      var quantiteSucreS = "" //expression avec des mots
      var nomBoisson = ""
      var tailleBoisson = ""
      var codeTwint = ""


      var choixBoissonValable = false

      while (!choixBoissonValable) {
        val choixBoisson = readLine("Veuillez sélectionner votre boisson : " +
          "\n1) Expresso - CHF 2.00" +
          "\n2) Cappuccino - CHF 2.50" +
          "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" +
          "\n> ")

        //développement des choix
        if (choixBoisson == "1") {
          nomBoisson = "Expresso"
          println(s"Vous avez sélectionné: $nomBoisson")
          choixBoissonValable = true
          println()

          //attribuer les valeurs du type de boisson aux variables utiles
          prixBase = 2.00
          cafeBoisson = 8
        }
        else if (choixBoisson == "2") {
          nomBoisson = "Cappuccino"
          println(s"Vous avez sélectionné: $nomBoisson")
          choixBoissonValable = true
          println()

          //attribuer les valeurs du type de boisson aux variables utiles
          prixBase = 2.50
          cafeBoisson = 6
          laitBoisson = 100
        }
        else if (choixBoisson == "3") {
          nomBoisson = "Latte"
          println(s"Vous avez sélectionné: $nomBoisson")
          var tailleLatteValable = false
          println()

          while (!tailleLatteValable) {
            val tailleLatte = readLine("Quelle taille désirez-vous ?" +
              "\n1) Petit = CHF 2.70" +
              "\n2) Moyen = CHF 3.20" +
              "\n3) Grand = CHF 3.70" +
              "\n> ")

            if (tailleLatte == "1") {
              tailleBoisson = "(Petit)"
              prixBase = 2.70
              cafeBoisson = 6
              laitBoisson = 120
              tailleLatteValable = true
            }
            else if (tailleLatte == "2") {
              tailleBoisson = "(Moyen)"
              prixBase = 3.20
              cafeBoisson = 8
              laitBoisson = 150
              tailleLatteValable = true
            }
            else if (tailleLatte == "3") {
              tailleBoisson = "(Grand)"
              prixBase = 3.70
              cafeBoisson = 12
              laitBoisson = 200
              tailleLatteValable = true
            }
            // si mauvaise entrée
            else {
              println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu taille latte
              println()
            }
          }
          //si taille latte valable
          choixBoissonValable = true
        }
        else {
          println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu choix boisson
          println()
        }
      }

      //boucle donnée valable quantité de sucre
      var choixSucreValable = false

      while (!choixSucreValable) {
        val choixSucre = readLine("Souhaitez-vous ajouter du sucre ?" +
          "\n0) Sans sucre" +
          "\n1) Peu (5g) - CHF 0.10" +
          "\n2) Moyen (10g) - CHF 0.20" +
          "\n3) Beaucoup (15g) - CHF 0.30" +
          "\n> ")

        if (choixSucre == "0") {
          sucreG = 0
          quantiteSucreS = "Sans sucre"
          prixSucre = 0.00
          choixSucreValable = true
        }
        else if (choixSucre == "1") {
          sucreG = 5
          quantiteSucreS = "Peu"
          prixSucre = 0.10
          choixSucreValable = true
        }
        else if (choixSucre == "2") {
          sucreG = 10
          quantiteSucreS = "Moyen"
          prixSucre = 0.20
          choixSucreValable = true
        }
        else if (choixSucre == "3") {
          sucreG = 15
          quantiteSucreS = "Beaucoup"
          prixSucre = 0.30
          choixSucreValable = true
        }
        else {
          println("Entrée invalide. Veuillez choisir un numéro entre 0 et 3.") //menu choix quantité sucre
        }
      }

      if (choixBoissonValable && (nomBoisson == "Latte" || nomBoisson == "Cappuccino")) {

        var choixLaitSupplementValable = false
        var doseLaitValable = false

        while (!choixLaitSupplementValable && !doseLaitValable) {
          println()
          val choixLait = readLine("Souhaitez-vous ajouter du lait en supplément ?" +
            "\n1) Oui" +
            "\n2) Non" +
            "\n> ")
          if (choixLait == "1") {

            while (!doseLaitValable) {
              println()

              println("Vous pouvez ajouter jusqu'à 3 doses supplémentaire." +
                "\nChacune contient 50 ml de lait et vaut 0.05 CHF.")

              val doseLaitS = readLine("Combien de dose désirez-vous ?" +
                "\n0) Aucune" +
                "\n1) Une dose" +
                "\n2) Deux doses" +
                "\n3) Trois doses" +
                "\n> ")
              println()

              if (doseLaitS == "0") { // 0 dose
                doseLaitSupp = 0
                laitSupp = "Non"
                doseLaitValable = true
                choixLaitSupplementValable = true
              }
              else if (doseLaitS == "1") { // 1 dose
                doseLaitSupp = 50
                prixLaitSupp = 0.05
                laitSupp = "Oui"
                doseLaitValable = true
              }
              else if (doseLaitS == "2") { // 2 doses
                doseLaitSupp = 100
                prixLaitSupp = 0.10
                laitSupp = "Oui"
                doseLaitValable = true
              }
              else if (doseLaitS == "3") { // 3 doses
                doseLaitSupp = 150
                prixLaitSupp = 0.15
                laitSupp = "Oui"
                doseLaitValable = true
              }
              else {
                println("Entrée invalide. Veuillez choisir un numéro entre 0 et 3.") //menu choix quantité sucre
              }
            }
          }
          else if (choixLait == "2") {
            doseLaitSupp = 0
            laitSupp = "Non"
            println()
            choixLaitSupplementValable = true
          }
          else {
            println("Entrée invalide. Veuillez choisir un numéro entre 1 et 2.") //menu choix quantité lait supplément
          }
        }
      }

      //vérification de stock et variation

      if (coffeeStocks >= cafeBoisson && milkStocks >= (laitBoisson + doseLaitSupp) && sugarStocks >= sucreG) {

        //variation stock
        coffeeStocks -= cafeBoisson
        milkStocks -= (laitBoisson + doseLaitSupp)
        sugarStocks -= sucreG

        //génération du code pour paiement Twint
        for (_ <- 1 to 5) {
          val randomIndex = (math.random() * AlphaNum.length).toInt //génère un index aléatoire compris dans alphanumerique
          val randomChar = AlphaNum(randomIndex)
          codeTwint += randomChar
        }
        val prixLaitSuppArrondi = math.round(prixLaitSupp * 100.0) / 100.0
        val prixTotal = prixBase + prixSucre + prixLaitSuppArrondi
        val prixTotalArrondi = math.round(prixTotal * 100.0) / 100.0

        if (nomBoisson == "Latte" || nomBoisson == "Cappuccino") {
          println(s"Boisson sélectionnée: $nomBoisson $tailleBoisson" +
            s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
            s"\nLait en supplément: $laitSupp" +
            s"\nPrix total: CHF $prixBase + CHF $prixSucre + CHF $prixLaitSuppArrondi = CHF $prixTotalArrondi")
          println()

          println("Veuillez payer en utilisant Twint." +
            s"\nVotre code de paiement est: $codeTwint " +
            "\n(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println()
          println(s"Votre $nomBoisson est prêt! Bonne dégustation!")
          Thread.sleep(2000)
          println()

        }
        else {
          println(s"Boisson sélectionnée: $nomBoisson" +
            s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
            s"\nPrix total: CHF $prixBase + CHF $prixSucre = CHF $prixTotalArrondi")

          println()

          println("Veuillez payer en utilisant Twint." +
            s"\nVotre code de paiement est: $codeTwint" +
            "\n(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println()
          println(s"Votre $nomBoisson est prêt! Bonne dégustation!")
          Thread.sleep(2000)
          println()

        }

      }
      else {
        println(s"Boisson sélectionnée: $nomBoisson" +
          s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)")

        if (nomBoisson == "Latte" || nomBoisson == "Cappuccino") {
          println(s"Lait en supplément: $laitSupp")
          println()
          if (milkStocks < (laitBoisson + doseLaitSupp)) {
            println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
              "\nVeuillez choisir une taille plus petite, une autre boisson ou une autre machine.")
            println()

          }
          else if (coffeeStocks < cafeBoisson) {
            println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
              "\nVeuillez choisir une autre boisson ou vérifier les stocks en Admin.")
            println()

          }
          else if (sugarStocks < sucreG) {
            println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
              "\nVeuillez choisir une dose de sucre plus petite.")
            println()

          }
        }
        else if (milkStocks < (laitBoisson + doseLaitSupp)) {
          println()
          println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une autre boisson.")
          println()

        }
        else if (coffeeStocks < cafeBoisson) {
          println()
          println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une autre boisson.")
          println()

        }
        else if (sugarStocks < sucreG) {
          println()
          println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une dose de sucre plus petite.")
          println()

        }

      }
    }

  }

  def serveAdmin(machine: Machine): Unit = {
    var menuPrincipal = false
    println("1)Voir les stocks" +
      "\n2)Changer le code PIN" +
      "\n3)Retour au menu principal")
    while (!menuPrincipal) {
      val choix = readLine("> ").toInt
      //val laitArrondi = math.round(milkStocks(machineId) * 1000) / 1000.0
      if (choix == 1) {
        println()
        machine.affichage()

        val varStock = readLine("Que souhaitez vous faire ?" +
          "\n1) Ajouter du stock" +
          "\n2) Retirer du stock" +
          "\n3) Retour au menu principal" +
          "\n> ")
        println()
        if (varStock == "1") {
          machine.addIngredient()
          menuPrincipal = true
        }
        else if (varStock == "2") {
          machine.removeIngredient()
          menuPrincipal = true
        }
        else if (varStock == "3") {
          println("Retour au menu principal...")
          println()
          Thread.sleep(2000)
          menuPrincipal = true
        }
        else {
          println("Entrée invalide. Veuillez choisir entre 1 et 3.")
        }
      }
      else if (choix == 2) {
        machine.updatePin()
        menuPrincipal = true
      }
      else if (choix == 3) {
        println("Retour au menu principal...")
        println()
        Thread.sleep(2000)
        menuPrincipal = true
      }
      else {
        println("Entrée invalide. Veuillez choisir entre 1 et 3.")
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = fromFile(filename) //source d'entrée (fichier) pour la lecture
      val lignefr = fr.getLines() // déclaration d'un itérateur de lecture de fichier ligne par ligne
      //initialiser un tableau dynamique qui va par la suite lire le fichier machine.csv

      var ID = 1
      var i = 0 //déclarer une variable qui va nous permettre d'utiliser les indices

      if (lignefr.hasNext) {
        lignefr.next()
      }
      while (!lignefr.isEmpty) { //tant que la ligne n'est pas vide
        var ligne = lignefr.next //lecture de la ligne (String)
        var machineIndividuel = ligne.split(",") //on lit les informations comme des chaines de char indépendant lorsqu'ils sont séparé par un ","
        machines += new Machine() //on déclare des ArrayBuffer tel que nouvel objet de la classe Machine
        machines(i).machineId = ID
        machines(i).machinePin = machineIndividuel(0)
        machines(i).milkStocks = machineIndividuel(1).toInt
        machines(i).sugarStocks = machineIndividuel(2).toInt
        machines(i).coffeeStocks = machineIndividuel(3).toInt
        i += 1
        ID += 1
      }
      fr.close()
      println()
      println(s"Chargement des machines depuis $filename...")
      println()
      return machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Le fichier est introuvable ou n'existe pas.")
        return null
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val filename = "machine.csv"
    try {
      val pw = new PrintWriter(new File("machines.csv"))

      pw.println("PINCODE,MILK,SUGAR,COFFEE")

      machines.foreach { machines => pw.println(s"${machines.machinePin},${machines.milkStocks},${machines.sugarStocks},${machines.coffeeStocks}")
      }
      pw.close()
      println(s"Sauvegarde des données dans le fichier $filename...")
      Thread.sleep(2500)
    } catch {
      case ex: Exception => println("Erreur lors de la sauvegarde des données")
    }
  }

  def main(args: Array[String]): Unit = {
    val machine = loadcsv("machines.csv")
    try {
      if (machine != null) {
        for (i <- 0 until machine.length) {
          machine(i).affichage()
        }
        println(s"${machine.length} machine(s) chargée(s) avec succès.")
        println()

        var programme = true
        while (programme) {
          println("        Nospresso Café")
          println("Veuillez sélectionner votre mode :")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter")
          val choix = readLine("> ").toInt
          println()

          if (choix == 1) {
            println("Machine sélectionnée (1-5)")
            var valide = false
            val totalMachines = machines.length
            var machineID = 0

            while (!valide) {
              machineID = readLine("> ").toInt

              if (machineID >= 1 && machineID <= totalMachines) {
                valide = true
              }
              else {
                println(s"Entrée invalide. Veuillez choisir un nombre entre 1 et $totalMachines.")
              }
            }
            val choixMachine = machines(machineID - 1)
            choixMachine.serveClient()
          }

          else if (choix == 2) {
            println("Machine sélectionnée (1-5)")
          var valide = false
            val totalMachines = machines.length
            var machineID = 0

            while (!valide) {
              machineID = readLine("> ").toInt

              if (machineID >= 1 && machineID <= totalMachines) {
                valide = true
              }
              else {
                println(s"Entrée invalide. Veuillez choisir un nombre entre 1 et $totalMachines.")
              }
            }
            val choixMachine = machines(machineID - 1)

            if (choixMachine.validatePin()) {
              println(s"Mode Admin")
              serveAdmin(choixMachine)
            }
            else {
              println("Accès refusé. Retour au menu principal")
              println()
            }
          }
          else if (choix == 3) {
            savecsv("machines.csv", machines)
            println("Merci d'avoir utilisé Nospresso !")
            programme = false
            //quitter le programme
          }
          else {
            println("Entrée invalide, veuillez réessayer.") //menu principal
            println()
            //entrée non valide
          }
        }
      }
      else {
        println()
        println("fin du programme")
      }
    } catch {
      case _: NumberFormatException => println("Entrée invalide. Veuillez entrer uniquement des nombres entiers.")
    }
    finally {
      println()
      println("FIN DU PROGRAMME")
    }
  }
}

