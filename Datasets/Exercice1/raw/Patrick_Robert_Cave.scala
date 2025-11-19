import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var running = true

    var stockCafe = 50 //g
    var stockSucre = 30 //g
    var stockLait = 0.5 //L

    while (running){

      var cafeNecessaire = 0
      var laitNecessaire = 0.0
      var sucreNecessaire = 0

      var nomBoisson = ""
      var nomSucre = ""
      var nomLait = "Non"

      var prixBoisson = 0.00
      var prixSucre = 0.00
      var prixLait = 0.00

      var mode = ""
      while (mode != "1" && mode != "2" && mode != "3") {
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        mode = readLine()
        if (mode != "1" && mode != "2" && mode != "3") {
          println("Entrée non valide, veuillez taper une valeur autorisée.")
        }
      }

      if (mode == "1") {
        var boisson = ""
        while (boisson != "1" && boisson != "2" && boisson != "3") {
          println("Veuillez séléctionner votre boisson : ")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print(">")
          boisson = readLine()
          if (boisson != "1" && boisson != "2" && boisson != "3") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
        }

        if (boisson == "1") {
          nomBoisson = "Expresso"
          prixBoisson += 2.00
          cafeNecessaire = 8
        }
        else if (boisson == "2") {
          nomBoisson = "Cappuccino"
          prixBoisson += 2.50
          cafeNecessaire = 6
          laitNecessaire = 0.10
        }
        else if (boisson == "3") {
          var tailleLatte = ""
          while (tailleLatte != "1" && tailleLatte != "2" && tailleLatte != "3") {
            println("1) Petit Latte - CHF 2.70")
            println("2) Moyen Latte - CHF 3.20")
            println("3) Grand Latte - CHF 3.70")
            print(">")
            tailleLatte = readLine()
            if (tailleLatte != "1" && tailleLatte != "2" && tailleLatte != "3") {
              println("Entrée non valide, veuillez taper une valeur autorisée.")
            }
          }
          if (tailleLatte == "1") {
            nomBoisson = "Latte (Petit)"
            prixBoisson += 2.70
            cafeNecessaire = 6
            laitNecessaire = 0.12
          }
          else if (tailleLatte == "2") {
            nomBoisson = "Latte (Moyen)"
            prixBoisson += 3.20
            cafeNecessaire = 8
            laitNecessaire = 0.15
          }
          else if (tailleLatte == "3") {
            nomBoisson = "Latte (Grand)"
            prixBoisson += 3.70
            cafeNecessaire = 12
            laitNecessaire = 0.20
          }
        }

        var sucre = ""
        while (sucre != "1" && sucre != "2" && sucre != "3" && sucre != "4") {
          println("Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10 ")
          println("3) Moyen (10g) - CHF 0.20 ")
          println("4) Beaucoup (15g) - CHF 0.30 ")
          print(">")
          sucre = readLine()
          if (sucre != "1" && sucre != "2" && sucre != "3" && sucre != "4") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
          if (sucre == "1") {
            nomSucre = "Sans Sucre"
            prixSucre += 0.00
            sucreNecessaire = 0
          }
          else if (sucre == "2") {
            nomSucre = "Peu (5g)"
            prixSucre += 0.10
            sucreNecessaire = 5
          }
          else if (sucre == "3") {
            nomSucre = "Moyen (10g)"
            prixSucre += 0.20
            sucreNecessaire = 10
          }
          else if (sucre == "4") {
            nomSucre = "Beaucoup (15g)"
            prixSucre += 0.30
            sucreNecessaire = 15
          }
        }

        if (boisson == "2" || boisson == "3") {
          var lait = ""
          while (lait != "1" && lait != "2"){
            println("Souhaitez-vous ajouter du lait en supplément ? ")
            println("1) Oui")
            println("2) Non")
            print(">")
            lait = readLine()
            if (lait != "1" && lait != "2") {
              println("Entrée non valide, veuillez taper une valeur autorisée.")
            }
            if (lait == "1") {
              nomLait = "Oui"
              var doses = ""
              while (doses != "1" && doses != "2" && doses != "3") {
                println("Combien de doses ? (3 maximum)")
                print(">")
                doses = readLine()
                if (doses == "1" || doses == "2" || doses == "3") {
                  laitNecessaire += 0.05 * doses.toInt
                  prixLait += 0.05 * doses.toInt
                }
                else {
                  println("Entrée non valide, veuillez taper une valeur autorisée.")
                }
              }
            }
            else if (lait == "2") {
              nomLait = "Non"
              prixLait += 0.00
            }
          }
        }

        println("Boisson sélectionnée : " + nomBoisson)
        println("Niveau de sucre : " + nomSucre)
        if( boisson == "2" || boisson == "3" ){
          println("Lait en supplément : " + nomLait)
        }

        if (stockCafe >= cafeNecessaire && stockLait >= laitNecessaire && stockSucre >= sucreNecessaire) {
          stockCafe -= cafeNecessaire
          stockLait -= laitNecessaire
          stockSucre -= sucreNecessaire
          var prixTotal = prixBoisson + prixSucre + prixLait
          printf("Prix total: CHF %.2f CHF %.2f CHF %.2f = CHF %.2f\n", prixBoisson, prixSucre, prixLait, prixTotal)
          println()

          var alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var code = ""
          for (i <- 0 to 4) {
            code += alphanumericChars((math.random() * alphanumericChars.length).toInt)
          }
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + code)
          println("(En attente de paiement...)")
          Thread.sleep(5000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          println("[...]")
          println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

        }
        else {

          if (stockCafe < cafeNecessaire) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")

            if(boisson == "1" || boisson == "2"){
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if(boisson == "3"){
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
          }
          if (stockLait < laitNecessaire) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")

            if(boisson == "1" || boisson == "2"){
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if(boisson == "3"){
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
          }
          if (stockSucre < sucreNecessaire) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez rajouter moins de sucre dans votre boisson.")
          }
        }
        mode = ""
      }

      if (mode == "2") {
        println("Mode Admin")
        println("Entrez le code PIN : ")
        var codeAdmin = "434343"
        var codeEntre = readLine()

        while (codeEntre != codeAdmin) {
          println("Code PIN incorrect.")
          println("Veuillez réessayer : ")
          codeEntre = readLine()
        }

        println("Accès autorisé.")

        println("   Poudre de café : " + stockCafe + "g")
        println("   Lait : " + stockLait + "L")
        println("   Sucre : " + stockSucre + "g")
        println("Réapprovisionnement des stocks...")
        println("Ajout : ")

        print("Poudre de café : ")
        var cafeAjoute = readInt()
        stockCafe += cafeAjoute

        print("Lait           : ")
        var laitAjoute = readDouble()
        stockLait += laitAjoute

        print("Sucre          : ")
        var sucreAjoute = readInt()
        stockSucre += sucreAjoute

        if (cafeAjoute >= 0 && laitAjoute >= 0 && sucreAjoute >= 0) {
          println("Niveaux des stocks mis à jour.")
          println("Retour au menu principal...")
          Thread.sleep(2000)
        }
        mode = ""
      }
      if (mode == "3"){
        println("Vous avez quitté.")
        running = false
      }
    }
  }
}
