import scala.io.StdIn._

object main {
  def main(args: Array[String]): Unit = {
    val pin = 434343
    var quantiteCafePoudre = 50.00
    var quantiteSucre = 30.00
    var quantiteLait = 0.500

    var boisson = ""
    var boissonPrix = 0.00
    var cafeUtiliser = 0.00
    var laitUtiliser = 0.00

    var sucreNivNom = ""
    var sucreUtiliser = 0.00
    var sucrePrix = 0.00

    var laitSupNom = ""
    var dosePrix = 0.00

    var choixMode = 1
    var erreur = true


    while(choixMode!=3){
      println("\tNospresso Café\n Veuillez sélectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter")
      choixMode = readLine(">").toInt
      while((choixMode!=1)&&(choixMode!=2)&&(choixMode!=3)){
        println("\tNospresso Café\n Veuillez sélectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter")
        choixMode = readLine(">").toInt
      }
      if (choixMode == 1) {
        erreur = true
        while (erreur) {
          erreur = false
          println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
          var choixBoisson = readLine(">").toInt
          while ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) {
            println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
            choixBoisson = readLine(">").toInt
          }
          if (choixBoisson == 1) {
            boisson = "Expresso"
            boissonPrix = 2.00
            cafeUtiliser = 8.00
            laitUtiliser = 0.00
          }

          else if (choixBoisson == 2) {
            boisson = "Cappuccino"
            boissonPrix = 2.50
            cafeUtiliser = 6.00
            laitUtiliser = 0.100
          }
          else {
            println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
            var taille = readLine(">").toInt
            while ((taille != 1) && (taille != 2) && (taille != 3)) {
              println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
              taille = readLine(">").toInt
            }
            boisson = "Latte (Petit)"
            boissonPrix = 2.70
            cafeUtiliser = 6.00
            laitUtiliser = 0.120
            if (taille == 2) {
              boisson = "Latte (Moyen)"
              boissonPrix = 3.20
              cafeUtiliser = 8.00
              laitUtiliser = 0.150
            }
            else if (taille == 3) {
              boisson = "Latte (Grand)"
              boissonPrix = 3.70
              cafeUtiliser = 12.00
              laitUtiliser = 0.200
            }
          }
          println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
          var sucreNiv = readLine(">").toInt
          while ((sucreNiv != 1) && (sucreNiv != 2) && (sucreNiv != 3) && (sucreNiv != 4)) {
            println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
            sucreNiv = readLine(">").toInt
          }
          if (sucreNiv == 1) {
            sucreNivNom = "Sans Sucre"
            sucreUtiliser = 0
          }

          if (sucreNiv == 2) {
            sucreNivNom = "Peu (5g)"
            sucreUtiliser = 5
            sucrePrix = 0.10
          }
          else if (sucreNiv == 3) {
            sucreNivNom = "Moyen (10g)"
            sucreUtiliser = 10
            sucrePrix = 0.20
          }
          else if (sucreNiv == 4) {
            sucreNivNom = "Grand (15g)"
            sucreUtiliser = 15
            sucrePrix = 0.30
          }
          if ((choixBoisson == 2) || (choixBoisson == 3)) {
            println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
            var laitSup = readLine("").toInt
            while ((laitSup != 1) && (laitSup != 2)) {
              println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
              laitSup = readLine("").toInt
            }

            if (laitSup == 1) {
              laitSupNom = "Oui"
              println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
              var dose = readLine(">").toInt
              while ((dose != 0) && (dose != 1) && (dose != 2) && (dose != 3)) {
                println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
                dose = readLine(">").toInt
              }
              dosePrix = dose * 0.05
            }
            else {
              laitSupNom = "Non"
              dosePrix = 0.00
            }


          }
          //Gestion de stock et d'erreur
          if (choixBoisson == 1) {
            if (quantiteCafePoudre < cafeUtiliser) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              erreur = true
            }
            if (quantiteSucre < sucreUtiliser) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              erreur = true
            }
          }
          else if (choixBoisson == 2) {
            if (quantiteCafePoudre < cafeUtiliser) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              erreur = true
            }
            if (quantiteSucre < sucreUtiliser) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              erreur = true
            }
            if (quantiteLait < laitUtiliser) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              erreur = true
            }
          }
          else {
            if (quantiteCafePoudre < cafeUtiliser) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              erreur = true
            }
            if (quantiteSucre < sucreUtiliser) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              erreur = true
            }
            if (quantiteLait < laitUtiliser) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              erreur = true
            }
          }
        }
        // resume de la boisson personnaliser
        println("Boisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucreNivNom + "\nLait en supplément : " + laitSupNom)
        if(laitSupNom == "Oui"){
          val prixFinal = boissonPrix + sucrePrix + dosePrix
          println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " + CHF "+dosePrix + " = CHF " + prixFinal)
        }
        else{
          val prixFinal = boissonPrix + sucrePrix
          println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " = CHF " + prixFinal)

        }


        //payment
        // On genere le code de twint
        val Alphanumerique = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

        var codetwint = ""
        for (i <- 0 to 4) {
          codetwint = codetwint + Alphanumerique((math.random() * Alphanumerique.length).toInt)
        }
        println(" Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000) // Attend pendant 3000 millisecondes (31 secondes)
        println("Merci ! Votre paiement a été accepté.")
        //preparation de boisson
        println("Préparation de votre boisson...\n[...]\nVotre "+ boisson +" est prêt ! Bonne dégustation !")
        //aprés chaque transaction réussie, les quantités d’ingrédients utilisées
        //(poudre de café, sucre, lait) sont automatiquement déduites des stocks
        quantiteCafePoudre = quantiteCafePoudre - cafeUtiliser
        quantiteSucre = quantiteSucre - sucreUtiliser
        quantiteLait = quantiteLait - laitUtiliser
      }
      // Choix de mode admin
      else if (choixMode == 2) {
        println("Mode Admin")
        var pass = readLine("Entrez le code PIN :").toInt
        //test de pin
        while (pass != pin) {
          println("Mode Admin\n ")
          pass = readLine("Entrez le code PIN :").toInt
        }
        println("Accès autorisé.\n")
        println("Stocks:\n\tPoudre de café : " + quantiteCafePoudre + "g\n\tLait : " + quantiteLait + "L\n\tSucre : " + quantiteSucre + "g")
        println("\nRéapprovisionnement des stocks...\nAjout :")
        val ajoutPoudre = readLine("\tPoudre de café : ").toDouble
        quantiteCafePoudre = quantiteCafePoudre + ajoutPoudre
        val ajoutlait = readLine("\tLait : ").toDouble
        quantiteLait = quantiteLait + ajoutlait
        val ajoutSucre = readLine("\tSucre : ").toDouble
        quantiteSucre = quantiteSucre + ajoutSucre
      }
    }

  }
}