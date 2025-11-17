import scala.io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var machineId = 0
    val nbmachines = 5
    var coffeeStocks = Array.fill(nbmachines)(30)
    var sugarStocks = Array.fill(nbmachines)(50)
    var milkStocks = Array.fill(nbmachines)(500)
    var machinePins = Array.fill(nbmachines)("434343")

    //VALIDATE PIN-------------------------------------------------------------------------------
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var entreepin = readLine("Entrez le code pin > ")
      var essai = 2
      if (entreepin != machinePins(machineId)) {
        while ((essai != 0) ){
          if(entreepin == machinePins(machineId)){
            essai = 0
          }else{
            essai = essai - 1
            entreepin = readLine("Code PIN incorrect. " + (essai + 1) + " tentatives restantes.\n> ")
          }
        }
      }
      entreepin == machinePins(machineId)
    }

    //UPDATEPIN-------------------------------------------------------------------------------------
    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine "+(machineId +1))
      var NouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      var NouveauPinTaille = NouveauPin.toCharArray
      while (NouveauPinTaille.length != 6) {
        println("Votre code PIN ne contient pas exactement 6 chiffres")
        NouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        NouveauPinTaille = NouveauPin.toCharArray
      }
      machinePins(machineId) = NouveauPin
      println("Votre mot de passe a été mis à jour\nRetour au menu principal...")
    }

    //SERVE CLIENT-------------------------------------------------------------------------------------
    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      //Variable du paiement + TWINT
      var alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var code_twint = ""
      for(i <- 1 to 5){
        var numero = (math.random()*alphanumerique.length).toInt
        code_twint = code_twint + alphanumerique(numero)
      }


      var choix_boisson = readLine("Quelle boisson souhaitez vous commander ?\n1) Espresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
      while((choix_boisson < 1)||(choix_boisson > 3)){
        choix_boisson = readLine("Option indisponible, veuillez réessayer...\nQuelle boisson souhaitez vous commander ?\n1) Espresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
      }
      var q_sucre = 0
      var a_sucre = "test"
      var a_lait = "test"
      var prix_sucre = 0.00
      var prix_lait = 0.00
      var prix = 0.00
      var prix_b = 0.00
      var sucre = 0
      var lait = 0
      var d_lait = 0
      var pdc = 0
      var q_lait = 0
      var coffee_avant = 0
      var lait_avant = 0
      var sugar_avant = 0
      if(choix_boisson == 1){
        sucre =  readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 >").toInt
        while((sucre < 1)||(sucre > 4)){
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        pdc = 8
        prix_b = 2.00
        prix = prix_b
        if((sucre == 1)){
          q_sucre = 0
          a_sucre = "Sans sucre"
        }else if(sucre == 2){
          q_sucre = 5
          prix_sucre = 0.10
          prix = prix + prix_sucre
          a_sucre = "Peu (5g)"
        }else if(sucre == 3){
          q_sucre = 10
          prix_sucre = 0.20
          prix = prix + prix_sucre
          a_sucre = "Moyen (10g)"
        }else if(sucre == 4){
          q_sucre = 15
          prix_sucre = 0.30
          prix = prix + prix_sucre
          a_sucre = "Beaucoup (15g)"
        }
        if((coffeeStocks(machineId) >= pdc)&&(sugarStocks(machineId) >= q_sucre)){
          //Resumer Commande
          println("\nBoisson selectionnée : Expresso\nNiveau de sucre :" + a_sucre)
          if(sucre == 1){
            printf("Prix total : CHF %.2f = CHF %.2f \n", prix_b, prix)
          }else{
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix)
          }
          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Espresso est prêt ! Bonne dégustation !")
          coffee_avant = coffeeStocks(machineId)
          sugar_avant = sugarStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - pdc
          sugarStocks(machineId) = sugarStocks(machineId) - q_sucre
        }else{
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
        //CAPPUCCINO--------------------------------------------------------------------------------------------------------
      }else if(choix_boisson == 2){
        pdc = 6
        prix_b = 2.50
        prix = prix_b
        sucre =  readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while((sucre < 1)||(sucre > 4)){
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        while((lait < 1)||(lait > 2)){
          lait = readLine("Option indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        }
        if(lait == 1){
          d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
          if(d_lait <= 3){
            q_lait = 100 + (d_lait * 50)
            prix = prix + (d_lait * 0.05)
            if(d_lait == 1){
              a_lait = "1 dose (0.05L)"
              prix_lait = 0.05
            }else if(d_lait == 2){
              a_lait = "2 doses (0.1L)"
              prix_lait = 0.10
            }else{
              a_lait = "3 doses (0.15L)"
              prix_lait = 0.15
            }
          }else{
            println("Erreur: Le nombre de doses demandés est trop élevés.")
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
          }
        }else if(lait == 2){
          q_lait = 100
          a_lait = "Non"
        }
        if((sucre == 1)){
          q_sucre = 0
          a_sucre = "Sans sucre"
        }else if(sucre == 2){
          q_sucre = 5
          prix_sucre = 0.10
          prix = prix + prix_sucre
          a_sucre = "Peu (5g)"
        }else if(sucre == 3){
          q_sucre = 10
          prix_sucre = 0.20
          prix = prix + prix_sucre
          a_sucre = "Moyen (10g)"
        }else if(sucre == 4){
          q_sucre = 15
          prix_sucre = 0.30
          prix = prix + prix_sucre
          a_sucre = "Beaucoup (15g)"
        }
        if((coffeeStocks(machineId) >= pdc)&&(sugarStocks(machineId) >= q_sucre)&&(milkStocks(machineId) >= q_lait)){
          //Resumer Commande
          if((lait ==1)&&(sucre !=1)){
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix_lait, prix)
          }else if((lait == 2)&&(sucre != 1)){
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_sucre, prix)
          }else if((lait == 1)&&(sucre == 1)){
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_lait, prix)
          }else{
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f \n", prix)
          }

          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Cappuccino est prêt ! Bonne dégustation !")
          coffee_avant = coffeeStocks(machineId)
          sugar_avant = sugarStocks(machineId)
          lait_avant = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - pdc
          sugarStocks(machineId) = sugarStocks(machineId) - q_sucre
          milkStocks(machineId) = milkStocks(machineId) - q_lait
        }else{
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
      }else if(choix_boisson == 3){
        var nom_latte = "Latte (xxx)"
        var taille = readLine("Quelle taille souahitez vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
        while((taille < 1)||(taille > 3)){
          taille = readLine("Taille indisponible, veuillez réessayer...\nQuelle taille souahitez vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
        }
        sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while((sucre < 1)||(sucre > 4)){
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        while((lait < 1)||(lait > 2)){
          lait = readLine("Option indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        }
        //PETIT-----------------------------------------------------------------------------------------
        if(taille == 1){
          pdc = 6
          prix_b = 2.70
          if(lait == 1){
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            if(d_lait <= 3){
              q_lait = 120 + (d_lait * 50)
              prix_b = prix_b + (d_lait * 0.05)
              if(d_lait == 1){
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              }else if(d_lait == 2){
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              }else{
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            }else{
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            }
          }else if(lait == 2){
            q_lait = 120
            a_lait = "Non"
          }
          if((sucre == 1)){
            q_sucre = 0
            a_sucre = "Sans sucre"
          }else if(sucre == 2){
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix_b + prix_sucre
            a_sucre = "Peu (5g)"
          }else if(sucre == 3){
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix_b + prix_sucre
            a_sucre = "Moyen (10g)"
          }else if(sucre == 4){
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix_b + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        }else if(taille == 2){
          pdc = 8
          prix_b = 3.20
          prix = prix_b
          if(lait == 1){
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            if(d_lait <= 3){
              q_lait = 150 + (d_lait * 50)
              prix = prix_b + (d_lait * 0.05)
              if(d_lait == 1){
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              }else if(d_lait == 2){
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              }else{
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            }else{
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            }
          }else if(lait == 2){
            q_lait = 150
            a_lait = "Non"
          }
          if((sucre == 1)){
            q_sucre = 0
            a_sucre = "Sans sucre"
          }else if(sucre == 2){
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix + prix_sucre
            a_sucre = "Peu (5g)"
          }else if(sucre == 3){
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix + prix_sucre
            a_sucre = "Moyen (10g)"
          }else if(sucre == 4){
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        }else if(taille == 3){
          pdc = 6
          prix_b = 2.70
          prix = prix_b
          if(lait == 1){
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toInt
            if(d_lait <= 3){
              q_lait = 200 + (d_lait * 50)
              prix = prix + (d_lait * 0.05)
              if(d_lait == 1){
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              }else if(d_lait == 2){
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              }else{
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            }else{
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toInt
            }
          }else if(lait == 2){
            q_lait = 200
            a_lait = "Non"
          }
          if((sucre == 1)){
            q_sucre = 0
            a_sucre = "Sans sucre"
          }else if(sucre == 2){
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix + prix_sucre
            a_sucre = "Peu (5g)"
          }else if(sucre == 3){
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix + prix_sucre
            a_sucre = "Moyen (10g)"
          }else if(sucre == 4){
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        }
        if((coffeeStocks(machineId) >= pdc)&&(sugarStocks(machineId) >= q_sucre)&&(milkStocks(machineId) >= q_lait)){
          if(taille == 1){
            nom_latte = "Latte (Petit)"
          }else if(taille == 2){
            nom_latte = "Latte (Moyen)"
          }else if(taille ==3){
            nom_latte = "Latte (Grand)"
          }
          //Resumer Commande
          if((lait ==1)&&(sucre !=1)){
            println("\nBoisson selectionnée : "+nom_latte +"\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix_lait, prix)
          }else if((lait == 2)&&(sucre != 1)){
            println("\nBoisson selectionnée : "+nom_latte +"\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_sucre, prix)
          }else if((lait == 1)&&(sucre == 1)){
            println("\nBoisson selectionnée : "+nom_latte +"\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_lait, prix)
          }else{
            println("\nBoisson selectionnée : "+nom_latte +"\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait +"\nPrix total : CHF"+prix_b)
            printf("Prix total : CHF %.2f \n", prix)
          }

          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Cappuccino est prêt ! Bonne dégustation !")
          coffee_avant = coffeeStocks(machineId)
          sugar_avant = sugarStocks(machineId)
          lait_avant = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - pdc
          sugarStocks(machineId) = sugarStocks(machineId) - q_sucre
          milkStocks(machineId) = milkStocks(machineId) - q_lait
        }else{
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
      }
      if(choix_boisson == 1){
        (coffee_avant >= pdc)&&(sugar_avant >= q_sucre)
      }else{
        (coffee_avant >= pdc)&&(sugar_avant >= q_sucre)&& (lait_avant >= q_lait)
      }
    }
    //RESTOCK MACHINE---------------------------------------------------------------------------------------------------------------------
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      var lait_en_litre = milkStocks(machineId).toDouble
      lait_en_litre = (lait_en_litre/1000.0)
      println("\nNiveaux de stock actuels :\nPoudre de café: " + coffeeStocks(machineId) +"g\nSucre : "+ sugarStocks(machineId)+"g\nLait : "+ lait_en_litre + "L\n")
      println("Entrez les quantitésà ajouter :")
      var pdc_a = readLine("Poudre de café : ").toInt
      var sucre_a = readLine("Sucre : ").toInt
      var lait_a = readLine("Lait :").toDouble
      sugarStocks(machineId) = sugarStocks(machineId) + sucre_a
      coffeeStocks(machineId) = coffeeStocks(machineId) + pdc_a
      var lait_en_ml = (lait_a*1000).toInt
      milkStocks(machineId) = milkStocks(machineId) + lait_en_ml
      println("Les stocks ont été mis à jours avec succès.\nRetour au menu principal...")
    }
    //PROGRAMME PRINCIPALE
    var programme = 0
    var panne = 0
    while(programme == 0){
      panne = 0
      print("\nNospresso Café")
      var choix = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin\n3) Quitter\n> ").toInt
      while((choix < 1)||(choix > 3)){
        choix = readLine("Action impossible, veuillez réessayer...\nVeuillez sélectionner votre mode : \n1) Client \n2) Admin\n3) Quitter\n> ").toInt
      }
      if(choix == 1){
        while(panne == 0){
          machineId = readLine("Machine sélectionée (1-5) >  ").toInt
          while((machineId < 1)||(machineId > 5)){
            machineId = readLine("Numéro de Machine introuvable, veuillez réessayer...\nMachine sélectionée (1-5) >  ").toInt
          }
          machineId = machineId -1
          if(serveClient(machineId, coffeeStocks, sugarStocks, milkStocks) == false){
          }else{
            panne = 1
          }
        }
      }else if(choix == 2){
        machineId = readLine("Machine sélectionée (1-5) >  ").toInt
        while((machineId < 1)||(machineId > 5)){
          machineId = readLine("Numéro de Machine introuvable, veuillez réessayer...\nMachine sélectionée (1-5) >  ").toInt
        }
        machineId = machineId -1
        if(validatePin(machineId, machinePins)== true){
          println("Accès autorisé")
          var choix_admin = readLine("1) Changer de pin\n2) Réapprovisionner le stock\n > ").toInt
          while((choix_admin < 1)||(choix_admin > 2)){
            choix_admin = readLine("Action impossible, veuillez réessayer...\n1) Changer de pin\n2) Réapprovisionner le stock\n > ").toInt
          }
          if(choix_admin == 1){
            updatePin(machineId, machinePins)
          }else if(choix_admin == 2){
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
        }else{
          println("Trop de tentatives échouées. Fin du programme.")
          programme = 1
        }
      }else if(choix == 3){
        programme = 1
      }
    }


  }
}