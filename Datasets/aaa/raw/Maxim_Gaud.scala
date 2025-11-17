import io.StdIn._
import math._

      object Main {
      def main(args: Array[String]): Unit = {
        var mode = 0
        var sucre = 0
        var lait = 0
        var choix = ""
        var PIN = 434343
        var Latte = 0
        var reponse = ""
        //Stocks-----------------------------------------------------------------------------------------------------------------------------
        var stocklait = 500.0//en ml ou bien 0.5l
        var stocksucre = 30.0//en grammes
        var stockcafe = 50.0 //en grammes
        //variables pour le code
        val chiffresetlettres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var code = ""
        for(i <- 1 to 5) {
          var aleatoire = (random() * 36).toInt
          code += chiffresetlettres(aleatoire)
        }
        //Prix-----------------------------------------------------------------------------------------------------------------------------
        var prixFinal = 0.0
        val prixExpresso = 2.0
        val prixLattePetit = 2.7
        val prixLatteMoyen = 3.2
        val prixLatteGrand = 3.7
        var prixCappuccino = 2.5
        var prixSucre = 0.1
        var prixLaitsup = 0.05



        do {
          println("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter ")
          mode = readLine("> ").toInt
          if (mode == 1) {
            do {
              println(" Vous êtes dans le mode client. Veuillez sélectionner votre boisson:\nVous avez le choix entre un Expresso, un Cappuccino et un Latte : ")
              choix = readLine("> ").toString

            } while ((choix != "Expresso") && (choix != "Cappuccino") && (choix != "Latte"))
            if(choix == "Expresso"){
              stockcafe -= 8
              prixFinal = prixExpresso
              if(stockcafe < 8){
                println("Erreur : Il n'y a pas assez de cafe pour votre Expresso! ")

              }
            }
            if(choix == "Cappucino"){
              stockcafe -= 6
              stocklait -= 100
              prixFinal = prixCappuccino
              prixFinal = 2.5
              if((stockcafe < 6) && (stocklait < 100)){
                println("Erreur : Il n'y a pas assez de cafe ou de lait pour votre Cappuccino! ")

              }
            }
            if(choix == "Latte"){
              if(Latte == 1) {
                stockcafe -= 6
                stocklait -= 120
                prixFinal = prixLattePetit
                if((stockcafe < 6) && (stocklait < 120)){
                  println("Erreur : Il n'y a pas assez de cafe ou de lait pour votre Latte! ")
                  mode = readLine("Appuyez sur 3 pour quitter : ").toInt
                }
              }
              if(Latte == 2) {
                stockcafe -= 8
                stocklait -= 150
                prixFinal = prixLatteMoyen
                if((stockcafe < 8) && (stocklait < 150)){
                  println("Erreur : Il n'y a pas assez de cafe ou de lait pour votre Latte! ")
                  mode = readLine("Appuyez sur 3 pour quitter : ").toInt
                }
              }
              if(Latte == 3) {
                stockcafe -= 12
                stocklait -= 200
                prixFinal = prixLatteGrand
                if((stockcafe < 12) && (stocklait < 200)){
                  println("Erreur : Il n'y a pas assez de cafe ou de lait pour votre Latte! ")
                  mode = readLine("Appuyez sur 3 pour quitter : ").toInt
                }
              }
              do{
                println("Choisissez entre: 1 pour Petit, 2 pour Moyen, 3 pour Grand")
                Latte = readLine("> ").toInt
              }while((Latte != 1) && (Latte != 2) && (Latte != 3))
            }
          }
          //prixboisson = prixFinal
          //Sucre-----------------------------------------------------------------------------------------------------------------------------
          if ((choix == "Expresso") || (choix == "Cappuccino") || (choix == "Latte")){
            do{
              println("Souhaitez-vous ajouter du sucre ? : \n1) Sans sucre, tapez 0 : \n2) Peu (5g) - CHF 0.10, tapez 1 : \n3) Moyen (10g) - CHF 0.20, tapez 2 : \n4) Beaucoup (15g) - CHF 0.30, tapez 3 : ")
              sucre = readLine("> ").toInt
              if (sucre == 1){
                stocksucre -= 5
                prixFinal += prixSucre
              }else if(stocksucre < 5){
                println("Erreur : Il n'y a pas assez de sucre pour votre " + choix)
              }
              if (sucre == 2){
                stocksucre -= 10
                prixFinal += (prixSucre + prixSucre)
              }else if(stocksucre < 10){
                println("Erreur : Il n'y a pas assez de sucre pour votre " + choix)
              }
              if (sucre == 3){
                stocksucre -= 15
                prixFinal += (prixSucre + prixSucre + prixSucre)
              }else if(stocksucre < 15){
                println("Erreur : Il n'y a pas assez de sucre pour votre " + choix)
              }
            }while((sucre != 0) && (sucre != 1) && (sucre != 2) && (sucre != 3))
          }
          //Paiement Expresso-----------------------------------------------------------------------------------------------------------------------------
          if(choix == "Expresso"){
            println("Très bien. Nous allons procéder au payement.\nBoisson sélectionnée : Expresso\nNiveau de sucre : " + sucre  + ("\nLait supplémentaire : " + reponse + lait + " dose"))
            printf("Prix final = %.2f + %.2f = %.2f",prixExpresso,prixSucre * sucre ,prixFinal)
            println("\nVeuillez payer en utilisant Twint.")
            Thread.sleep(3000)
            println("Votre code de paiement est : "+ code)
            println("(En attente de validation du paiement...) ")
            Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
            println("Merci ! Votre paiement a été accepté.\n----------------------\nPréparation de votre boisson...\n[...] ")
            Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
            println("Votre Expresso est prêt ! Bonne dégustation ! ")
          }
          //Lait suplémentaire-----------------------------------------------------------------------------------------------------------------------------
          if((choix == "Cappuccino") || (choix == "Latte")){
            do{
              var prixboisson = 0.0
              println("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non ")
              reponse = readLine("> ")
              if(reponse == "Oui" ){
                println("Combien de dose ? (une dose équivaut a 50 ml de lait et coûte 0.05 francs) : \nVous avez le droit a trois doses maximum. ")
                lait = readLine("> ").toInt
                if(lait == 1){
                  if(choix == "Cappuccino"){
                    prixboisson = prixCappuccino
                    prixFinal = prixboisson //+ prixLaitsup + prixSucre
                  }else{
                    prixboisson = prixLattePetit
                    stocklait -= 50
                    prixFinal += prixboisson //+ prixLaitsup
                  }
                  if(stocklait < 50){
                    println("Erreur : Il n'y a pas assez de lait pour votre " + choix)

                  }
                }else if(lait == 2){
                  if(choix == "Cappuccino"){
                    prixboisson = prixCappuccino
                    prixFinal = prixboisson //+ (prixLaitsup + prixLaitsup) + prixSucre
                  }else{
                  prixboisson = prixLatteMoyen
                  stocklait -= 100
                  prixFinal += prixboisson //+ (prixLaitsup + prixLaitsup)
                  }
                  if(stocklait < 100){
                    println("Erreur : Il n'y a pas assez de lait pour votre " + choix)

                  }
                }else if((lait == 3) && (stocklait >= 150)){
                  if(choix == "Cappuccino"){
                    prixboisson = prixCappuccino
                    prixFinal = prixboisson //+ (prixLaitsup + prixLaitsup + prixLaitsup) + prixSucre
                  }else{
                  prixboisson = prixLatteGrand
                  stocklait -= 150
                  prixFinal += prixboisson //+ (prixLaitsup + prixLaitsup + prixLaitsup)
                  }
                }else{
                    println("Erreur : Il n'y a pas assez de lait pour votre " + choix)

                }
              }
              if(reponse == "Oui"){
                if(choix == "Cappuccino"){
                  prixboisson = prixCappuccino
                  prixFinal = prixboisson + (prixSucre * sucre) + (prixLaitsup * lait)
                }else if(choix == "Latte"){
                  if(Latte == 1){
                    prixboisson = prixLattePetit
                    prixFinal = prixboisson + (prixSucre * sucre)  + (prixLaitsup * lait)
                  }else if(Latte == 2){
                    prixboisson = prixLatteMoyen
                    prixFinal = prixboisson + (prixSucre * sucre)  + (prixLaitsup * lait)
                  }else if(Latte == 3){
                    prixboisson = prixLatteGrand
                    prixFinal = prixboisson + (prixSucre * sucre)  + (prixLaitsup * lait)
                  }
                }
              }
              if(reponse == "Non"){
                if(choix == "Cappuccino"){
                  prixboisson = prixCappuccino
                  prixFinal = prixboisson + prixSucre * sucre
                }else if(choix == "Latte"){
                  if(Latte == 1){
                    prixboisson = prixLattePetit
                    prixFinal = prixboisson + prixSucre * sucre
                  }else if(Latte == 2){
                    prixboisson = prixLatteMoyen
                    prixFinal = prixboisson + prixSucre * sucre
                  }else if(Latte == 3){
                    prixboisson = prixLatteGrand
                    prixFinal = prixboisson + prixSucre * sucre
                  }
                }
              }
              println(("Très bien. Nous allons procéder au payement.\nBoisson sélectionnée : " + choix) + ("\nNiveau de sucre : " + sucre)  + ("\nLait supplémentaire : " + reponse +", " + lait + " dose "))
              printf("Prix final = %.2f + %.2f + %.2f = %.2f", prixboisson, prixSucre * sucre, prixLaitsup * lait, prixFinal)
              println(". Veuillez payer en utilisant Twint. ")
              Thread.sleep(3000)
              println("Votre code de paiement est : " + code)
              println("(En attente de validation du paiement...) ")
              Thread.sleep(3000) // Attend pendant 5000 millisecondes (5 secondes)
              println("Merci ! Votre paiement a été accepté.\n----------------------\nPréparation de votre boisson...\n[...] ")
              Thread.sleep(3000) // Attend pendant 5000 millisecondes (5 secondes)
              println("Votre " + choix + " est prêt ! Bonne dégustation ! ")
              println("Merci de votre visite ! ")
            }while((lait != 0) && (lait != 1) && (lait != 2) && (lait != 3))

          }


        } while(mode == 3)
        //Stocks-----------------------------------------------------------------------------------------------------------------------------
        if(mode == 2){
          do{
            println(" Veuillez entrer le code PIN: ****** ")
            PIN = readLine("> ").toInt

          }while(PIN != 434343)

          if(PIN == 434343){

            println("Accès autorisé. Voici les stocks de café : " + stockcafe + "\nles stocks de lait: " + stocklait + "\nles stocks de sucre: " + stocksucre)
            println("Réapprovisionnement des stocks... ")
            println("Ajout : ")
            if((stockcafe <= 50.0) && (stocklait <= 500.0) && (stocksucre <= 30.0)) {
              stockcafe = readLine("Poudre de cafe : ").toInt
              stocklait = readLine("Lait : ").toInt
              stocksucre = readLine("Sucre : ").toInt
              println("Niveaux de stocks mis à jour.\nRetour au menu principal... ")
              mode = readLine("Appuyez sur 3 pour quitter : ").toInt
            }
          }
        }












      }
      }


