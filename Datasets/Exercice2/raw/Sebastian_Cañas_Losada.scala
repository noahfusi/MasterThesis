import io.StdIn._
import math._

object Main {
  //nbr de machines
  val nbMachines = 5

  // tableau des stocks pour toutes les machines
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50) // en g
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30) // en g
  val milkStocks: Array[Int] = Array.fill(nbMachines)(500) // en l

  // tableau des codes PIN pour toutes les machines
  val machinePins: Array[String] = Array.fill(nbMachines)("434343")

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val tentativeMax = 3
    var tentative = 0
    println("Entrez le code PIN:")

    while (tentative < tentativeMax) {
      val pin = readLine("> ")

      if (pin == machinePins(machineId)) {
        println(s"Accès accordé pour la machine ${machineId+1}")
        println()
        return true
      }
      else {
        tentative += 1
        println(s"Code PIN incorrect. ${tentativeMax - tentative} tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    println()
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var pinValable = false

    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}.")

    while (!pinValable) {
      val newPin = readLine(s"Entrez un nouveau code PIN à 6 chiffres > ")

      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succèes." +
          "\nRetour au menu principal...")
        Thread.sleep(2500)
        pinValable = true
      }
      else {
        // en boucle tant que la condition if n'est pas valide comme demandé par le prof
      }
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Entrez les quantités à ajouter:")
    val ajoutCafe = readLine("Poudre de café (g) > ").toInt
    val ajoutSucre = readLine("Sucre (g) > ").toInt
    val ajoutLait = readLine("Lait (mL) > ").toInt
    coffeeStocks(machineId) += ajoutCafe
    sugarStocks(machineId) += ajoutSucre
    milkStocks(machineId) += ajoutLait
    //val laitArrondi = math.round(milkStocks(machineId) * 1000) / 1000.0
    println()
    println("Validation nouveaux niveaux de stock:")
    println(s"  Poudre de café: ${coffeeStocks(machineId)}g")
    println(s"  Sucre         : ${sugarStocks(machineId)}g")
    println(s"  Lait          : ${milkStocks(machineId)}ml")
    println()
    println("Les stocks ont été mis à jour avec succès." +
      "\nRetour au menu principal...")
    println()
    Thread.sleep(2500)
  }

  def serveAdmin(machineId: Int): Unit = {
    if(validatePin(machineId, machinePins)){
      println("Mode Admin")
      var menuPrincipal = false
      println("1)Voir les stocks" +
        "\n2)Changer le code PIN" +
        "\n3)Retour au menu principal")
      while (!menuPrincipal) {
        val choix = readLine("> ").toInt
        //val laitArrondi = math.round(milkStocks(machineId) * 1000) / 1000.0

        if (choix == 1) {
          println()
          println("Niveaux de stock actuels:")
          println(s"  Poudre de café: ${coffeeStocks(machineId)}g")
          println(s"  Sucre         : ${sugarStocks(machineId)}g")
          println(s"  Lait          : ${milkStocks(machineId)}ml")
          println()

          val varStock = readLine("Voulez-vous ajouter du stock ?" +
            "\n1) Oui" +
            "\n2) Non" +
            "\n> ")
          println()

          if (varStock == "1") {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            menuPrincipal = true
          }
          else if(varStock == "2"){
            println("Les stocks sont inchangés. " +
              "\nRetour au menu principal ...")
            Thread.sleep(2000)
            println()
            menuPrincipal = true
          }
        }
        else if (choix == 2) {
          updatePin(machineId, machinePins)
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
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
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

    if (coffeeStocks(machineId) >= cafeBoisson && milkStocks(machineId) >= (laitBoisson + doseLaitSupp) && sugarStocks(machineId) >= sucreG) {

      //variation stock
      coffeeStocks(machineId) -= cafeBoisson
      milkStocks(machineId) -= (laitBoisson + doseLaitSupp)
      sugarStocks(machineId) -= sucreG

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
        //modeClient = false
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
        //modeClient = false
      }
      true
    }
    else {
      println(s"Boisson sélectionnée: $nomBoisson" +
        s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)")

      if (nomBoisson == "Latte" || nomBoisson == "Cappuccino") {
        println(s"Lait en supplément: $laitSupp")
        println()
        if (milkStocks(machineId) < (laitBoisson + doseLaitSupp)) {
          println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une taille plus petite, une autre boisson ou une autre machine.")
          println()
          //modeClient = false
        }
        else if (coffeeStocks(machineId) < cafeBoisson) {
          println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une autre boisson ou vérifier les stocks en Admin.")
          println()
          //modeClient = false
        }
        else if (sugarStocks(machineId) < sucreG) {
          println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
            "\nVeuillez choisir une dose de sucre plus petite.")
          println()
          //modeClient = false
        }
      }
      else if (milkStocks(machineId) < (laitBoisson + doseLaitSupp)) {
        println()
        println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
          "\nVeuillez choisir une autre boisson.")
        println()
        //modeClient = false
      }
      else if (coffeeStocks(machineId) < cafeBoisson) {
        println()
        println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
          "\nVeuillez choisir une autre boisson.")
        println()
        //modeClient = false
      }
      else if (sugarStocks(machineId) < sucreG) {
        println()
        println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
          "\nVeuillez choisir une dose de sucre plus petite.")
        println()
        //modeClient = false
      }
      false
    }
  }


  def main(args: Array[String]): Unit = {


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
        var machineIdValable = false

        while (!machineIdValable) {
          val machineIdselection = readLine("> ").toInt
          val machineId = machineIdselection - 1

          if (machineIdselection < 1 || machineIdselection > 5) {
            println("Entrée invalide. Veuillez choisir une machine entre 1 et 5.")
          }
          else {
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
            machineIdValable = true
          }
        }
      }
      else if (choix == 2) {
        println("Machine sélectionnée (1-5)")
        var machineIdValable = false

        while (!machineIdValable) {
          val machineIdselection = readLine("> ").toInt
          val machineId = machineIdselection - 1

          if (machineIdselection < 1 || machineIdselection > 5) {
            println("Entrée invalide. Veuillez choisir une machine entre 1 et 5.")
          }
          else {
            serveAdmin(machineId)
            machineIdValable = true
          }
        }

      }
      else if (choix == 3) {
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
}