object Main {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    import scala.util._

    var running = true  // si running est true, alors on continue, si running est false alors on finit le programme (cas du mode 3)
    val codePIN = "434343"
    var codePaiement = ""

    // Stocks
    var stockPoudreDeCafe = 50 //en gramme
    var stockSucre = 30 //en gramme
    var stockLait = 500 // en ml

    // Expresso
    val prixExpresso = 2.00 // en CHF
    val quantiteCafeExpresso = 8 // en gramme

    // Cappucino
    val prixCappuccino = 2.50 // en CHF
    val quantiteCafeCappuccino = 6 // en gramme
    val quantiteLaitCappuccino = 100  // en ml

    // Latte (Petit)
    val prixLattePetit = 2.70 // en CHF
    val quantiteCafeLattePetit = 6 // en gramme
    val quantiteLaitLattePetit = 120 // en ml

    // Latte (Moyen)
    val prixLatteMoyen = 3.20 // en CHF
    val quantiteCafeLatteMoyen = 8 // en gramme
    val quantiteLaitLatteMoyen = 150 // en ml

    // Latte (Grand)
    val prixLatteGrand = 3.70 // en CHF
    val quantiteCafeLatteGrand = 12 // en gramme
    val quantiteLaitLatteGrand = 200 // en ml


    // Prix du sucre
    val prixPeuSucre = 0.1  // en CHF
    val prixMoyenSucre = 0.2 // en CHF
    val prixBeaucoupSucre = 0.3 // en CHF

    val prixLait = 0.05


    while (running) {
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var mode = readLine("> ")  // mode permet de savoir quelle mode est choisis

      while ((mode != "1") && (mode != "2") && (mode != "3")) {
        mode = readLine("Insérez une valeur correcte : ")
      } //cette boucle while va tourner tant que l'utilisateur ne va pas donner une valeur égale à 1 ou 2 ou 3

      if (mode == "1") {
        var nbDosesLaitSup = 0
        var nomBoisson = ""
        var QuantiteSucre = ""
        var laitSupp = "Non"

        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        var Boisson = readLine("> ")


        while ((Boisson != "1") && (Boisson != "2") && (Boisson != "3")) {
          Boisson = readLine("Insérez une valeur correcte : ")
        }

        // Initialisation des besoins
        var cafeNecessaire = 0
        var laitNecessaire = 0
        var sucreNecessaire = 0
        var prixTotale = 0.0
        var prixFinal = 0.0
        var prixSucre = 0.0
        var prixLaitSupp = 0.0

        if (Boisson == "1") {
          nomBoisson = "Expresso"
          cafeNecessaire += quantiteCafeExpresso
          prixTotale += prixExpresso
        }
        if (Boisson == "2") {
          nomBoisson = "Cappuccino"
          cafeNecessaire += quantiteCafeCappuccino
          laitNecessaire += quantiteLaitCappuccino
          prixTotale += prixCappuccino
        }
        if (Boisson == "3") {
          println("De quelle taille souhaitez-vous votre Latte ?")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          var tailleLatte = readLine("> ")

          while ((tailleLatte != "1") && (tailleLatte != "2") && (tailleLatte != "3")) {
            tailleLatte = readLine("Insérez une valeur correcte : ")
          }

          if (tailleLatte == "1"){
            nomBoisson = "Latte Petit"
            cafeNecessaire += quantiteCafeLattePetit
            laitNecessaire += quantiteLaitLattePetit
            prixTotale += prixLattePetit
          }
          if (tailleLatte == "2"){
            nomBoisson = "Latte Moyen"
            cafeNecessaire += quantiteCafeLatteMoyen
            laitNecessaire += quantiteLaitLatteMoyen
            prixTotale += prixLatteMoyen
          }
          if (tailleLatte == "3"){
            nomBoisson = "Latte Grand"
            cafeNecessaire += quantiteCafeLatteGrand
            laitNecessaire += quantiteLaitLatteGrand
            prixTotale += prixLatteGrand
          }
        }

        // Ajouter du sucre
        println("Souhaitez-vous ajouter du sucre ?")
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
          prixSucre = prixPeuSucre
        }
        if (choixSucre == "3") {
          QuantiteSucre = "Moyen (10g)"
          sucreNecessaire = 10
          prixSucre = prixMoyenSucre
        }
        if (choixSucre == "4") {
          QuantiteSucre = "Beaucoup (15g)"
          sucreNecessaire = 15
          prixSucre = prixBeaucoupSucre
        }
        if ((Boisson == "2") || (Boisson == "3")) {
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          var choixLaitSup = readLine("> ")
          while ((choixLaitSup != "1") && (choixLaitSup != "2")) {
            println("Entrée invalide, veuillez réessayer : ")
            choixLaitSup = readLine("> ")
          }
          if (choixLaitSup == "1") {
            println("Combien de doses ?")
            nbDosesLaitSup = readLine("> ").toInt
            while ((nbDosesLaitSup != 1) && (nbDosesLaitSup != 2) && (nbDosesLaitSup != 3)) {
              println("Erreur : Vous pouvez ajouter entre 1 et 3 doses de lait seulement")
              println("Combien de doses ?")
              nbDosesLaitSup = readLine("> ").toInt
            }
            // Réduction des stocks de lait en fonction des doses
            laitSupp = "Oui"
            laitNecessaire += nbDosesLaitSup * 50 // chacune des doses mesure 0.05 l
            prixLaitSupp += nbDosesLaitSup*0.05
          }
        }
        println("Boisson sélectionnée :"+ nomBoisson)
        println("Niveau de sucre :"+ QuantiteSucre)
        println("Lait en supplément :"+ laitSupp)

        // Vérification des stocks
        if (stockPoudreDeCafe < cafeNecessaire) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (stockLait < laitNecessaire) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        if (stockSucre < sucreNecessaire) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir un niveau de sucre inférieur ou vérifier les stocks en mode Admin.")
        }
        else {
          // Mise à jour des stocks
          stockPoudreDeCafe -= cafeNecessaire
          stockLait -= laitNecessaire
          stockSucre -= sucreNecessaire

          // Calcul et affichage du prix total
          var prixLaitSupp = (prixLait*nbDosesLaitSup).toFloat
          prixFinal = (prixTotale + prixSucre + prixLaitSupp).toFloat
          var affichagePrix = "Prix total : CHF "
          if (prixSucre > 0) {
            affichagePrix+= prixTotale+"0 + CHF "+prixSucre+"0 "
          }
          if (prixLaitSupp > 0) {
            affichagePrix+= "+ CHF "+((prixLaitSupp.toFloat + "0").take(4)+" ")
          }

          var affichagePrixTotale: String = "= CHF " + (prixFinal.toFloat + "0").take(4) // .take(4)m limite l'affichage du prixFinal à 4 éléments
          println(affichagePrix + affichagePrixTotale)
        }
        codePaiement = Random.alphanumeric.take(5).mkString.toUpperCase() //génère un code aléatoire de 5 nombres ou chiffres
        println("Veuillez payer avec Twint")
        println("Votre code de paiement est :"+ codePaiement)
        println("(En attente de validation du paiement...)")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (5 secondes)
        println("Merci, votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (5 secondes)
        println("Votre "+nomBoisson+"est prêt ! Bonne dégustation !")
      }
      if (mode == "2") {
        println("Mode Admin")
        var essai_Code = readLine("Entrez le code Pin : ")
        while (essai_Code != codePIN) {
          essai_Code = readLine("Entrez le code Pin : ")
        }
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (5 secondes)
        println("Accès autorisé.")
        println("Stocks :")
        println("Poudre de café : "+stockPoudreDeCafe)
        println("Lait           : "+stockLait)
        println("Sucre          : "+stockSucre)
        println("Réapprovisionnement des stocks...")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (2 secondes)
        println("Ajout :")
        stockPoudreDeCafe += readLine("    Poudre de café : ").toInt
        stockLait += (readLine("    Lait           : ")*1000).toInt
        stockSucre += readLine("    Sucre          : ").toInt
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (5 secondes)
        println("Stocks mis à jour.")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (5 secondes)
        println("Retour au menu principale")
      }
      if (mode == "3") {
        print("Au revoir !")
        running = false
      }
    }
  }
}