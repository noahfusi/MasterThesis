import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Main {

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var machineID = -1
    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(0.5)
    val machinePins = Array.fill(nbMachines)("434343")

    var mode = 0
    var tentatives = 3
    var mode_admin = 0
    var ajout_cafe = 0.0
    var sucre_quantite_ajout = 0.0
    var lait_ajout_supp = 0.0
    var ajout_lait = 0.0
    val ajout_petit_lait = 0.050
    val ajout_moyen_lait = 0.100
    val ajout_grand_lait = 0.150
    val expresso_prix = 2.0
    val capuccino_prix = 2.5
    val petit_latte_prix = 2.7
    val moyen_latte_prix = 3.2
    val grand_latte_prix = 3.7
    val sucre_petit = 5
    val sucre_moyen = 10
    val sucre_grand = 15
    val extra_petit_lait = 0.05
    val extra_moyen_lait = 0.10
    val extra_grand_lait = 0.15
    val extra_petit_sucre = 0.10
    val extra_moyen_sucre = 0.20
    val extra_grand_sucre = 0.30
    var lait_prix_total = 0.0
    var lait_quantite_ajout = 0.0
    //var recapitulatif_prix = ""
    val alphaNumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var boisson_prix = 0.0
    var cafe_utilise = 0.0
    var prix_ajout_sucre = 0.0
    var prix_ajout_lait = 0.0
    var lait_utilise = 0.0
    var prix_total = 0.0
    var quantite_tot_lait = 0.0
    var latte_prix = 0.0
    var forwhile = 0
    var boisson_nom = ""
    var sucre_niveau = ""
    var lait_supplement = "Non"

    //fonctions
    def validatePin(machineId: Int, machinePins: Array[String]):Boolean = {
      val code = readLine(">")
      if (machinePins(machineId) == code) {
        true
      }
      else {
        false
      }
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var nouveau_pin = readLine("Entrez un nouveau code PIN a 6 chiffres > ")
      while (nouveau_pin.length != 6) {
        nouveau_pin = readLine("Entrez un nouveau code PIN a 6 chiffres > ")
      }
      if (nouveau_pin.length == 6) {
        machinePins(machineId) = nouveau_pin
        println("Le code PIN a ete mis a jour avec succes.\nRetour au menu principal...")
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean ={
      var boisson_selectionne = readLine("Veuillez selectionner votre boisson :\n 1) Expresso- CHF2.00\n 2) Cappuccino- CHF 2.50\n 3) Latte- CHF2.70 (Petit),CHF 3.20(Moyen), CHF3.70 (Grand)\n>").toInt
      while((boisson_selectionne<1)||(boisson_selectionne>3)){
        boisson_selectionne = readLine("Veuillez selectionner votre boisson :\n 1) Expresso- CHF2.00\n 2) Cappuccino- CHF 2.50\n 3) Latte- CHF2.70 (Petit),CHF 3.20(Moyen), CHF3.70 (Grand)\n>").toInt
      }
      if (boisson_selectionne == 1) {
        boisson_prix = expresso_prix
        cafe_utilise = 8.0
        boisson_nom = "Expresso"
      }
      if (boisson_selectionne == 2) {
        boisson_prix = capuccino_prix
        cafe_utilise = 8.0
        lait_utilise = 0.100
        boisson_nom = "Cappuccino"

        lait_ajout_supp = readLine("Souhaitez-vous ajouter du lait en supplement ?\n (Disponible uniquement pour Cappuccino et Latte)\n 1) Oui\n 2) Non\n>").toInt
        while ((lait_ajout_supp <1)||(lait_ajout_supp>2)){
          lait_ajout_supp = readLine("Souhaitez-vous ajouter du lait en supplement ?\n (Disponible uniquement pour Cappuccino et Latte)\n 1) Oui\n 2) Non\n>").toInt
        }
        if (lait_ajout_supp == 2) {
          prix_ajout_lait = 0.0
          lait_quantite_ajout = 0.0
        }
        if (lait_ajout_supp == 1) {
          lait_supplement = "Oui"
          println("combien de portion voulez-vous ? ( jusqu'a 3 )")
          ajout_lait = readInt()
          while ((ajout_lait <0)||(ajout_lait>3)){
            ajout_lait = readLine("combien de portion voulez-vous ? ( jusqu'a 3 )\n>").toInt
          }
          if (ajout_lait == 1) {
            prix_ajout_lait = extra_petit_lait
            lait_quantite_ajout += ajout_petit_lait
            lait_prix_total = prix_ajout_lait
          }
          if (ajout_lait == 2) {
            prix_ajout_lait = extra_moyen_lait
            lait_quantite_ajout += ajout_moyen_lait
            lait_prix_total = prix_ajout_lait
          }
          if (ajout_lait == 3) {
            prix_ajout_lait = extra_grand_lait
            lait_quantite_ajout += ajout_grand_lait
            lait_prix_total = prix_ajout_lait
          }
        }
      }
      if (boisson_selectionne == 3) {
        latte_prix = readLine("De quelle taille voulez-vous votre latte \n 1) pour un petit \n 2) pour un moyen\n 3) pour un grand\n>").toInt
        while ((latte_prix <1)||(latte_prix>3)){
          latte_prix = readLine("De quelle taille voulez-vous votre latte \n 1) pour un petit \n 2) pour un moyen\n 3) pour un grand\n>").toInt
        }
        if (latte_prix == 1) {
          boisson_prix = petit_latte_prix
          cafe_utilise = 6.0
          lait_utilise = 0.120
        }
        if (latte_prix == 2) {
          boisson_prix = moyen_latte_prix
          cafe_utilise = 8.0
          lait_utilise = 0.150
        }
        if (latte_prix == 3) {
          boisson_prix = grand_latte_prix
          cafe_utilise = 12.0
          lait_utilise = 0.200
        }
        lait_ajout_supp = readLine(" Souhaitez-vous ajouter du lait en supplement ?\n (Disponible uniquement pour Cappuccino et Latte)\n 1) Oui\n 2) Non\n>").toInt
        while ((lait_ajout_supp <1)||(lait_ajout_supp>3)){
          lait_ajout_supp = readLine(" Souhaitez-vous ajouter du lait en supplement ?\n (Disponible uniquement pour Cappuccino et Latte)\n 1) Oui\n 2) Non\n>").toInt
        }
        if (lait_ajout_supp == 2) {
          prix_ajout_lait = 0.0
          lait_quantite_ajout = 0.0
        }
        if (lait_ajout_supp == 1) {
          lait_supplement = "Oui"
          println("combien de portion voulez-vous ? ( jusqu'a 3 )")
          ajout_lait = readInt()
          while ((ajout_lait <0)||(ajout_lait>3)){
            ajout_lait = readLine("combien de portion voulez-vous ? ( jusqu'a 3 )\n>").toInt
          }
          if (ajout_lait == 1) {
            prix_ajout_lait = extra_petit_lait
            lait_quantite_ajout += ajout_petit_lait
            lait_prix_total = prix_ajout_lait
          }
          if (ajout_lait == 2) {
            prix_ajout_lait = extra_moyen_lait
            lait_quantite_ajout += ajout_moyen_lait
            lait_prix_total = prix_ajout_lait
          }
          if (ajout_lait == 3) {
            prix_ajout_lait = extra_grand_lait
            lait_quantite_ajout += ajout_grand_lait
            lait_prix_total = prix_ajout_lait
          }
        }
      }

      sucre_quantite_ajout = readLine("Souhaitez-vous ajouter du sucre ?\n 1) Sans sucre\n 2) Peu (5g)- CHF 0.10\n 3) Moyen (10g)- CHF 0.20\n 4) Beaucoup (15g)- CHF 0.30\n>").toInt
      while ((sucre_quantite_ajout<1)||(sucre_quantite_ajout>4)){
        sucre_quantite_ajout = readLine("Souhaitez-vous ajouter du sucre ?\n 1) Sans sucre\n 2) Peu (5g)- CHF 0.10\n 3) Moyen (10g)- CHF 0.20\n 4) Beaucoup (15g)- CHF 0.30\n>").toInt
      }
      if (sucre_quantite_ajout == 1) {
        sucre_niveau = "Sans Sucre"
        prix_ajout_sucre = 0.0
        sucre_quantite_ajout = 0.0
      }
      if (sucre_quantite_ajout == 2) {
        sucre_niveau = "Peu (5g)"
        prix_ajout_sucre = extra_petit_sucre
        sucre_quantite_ajout = sucre_petit
      }
      if (sucre_quantite_ajout == 3) {
        sucre_niveau = "Moyen (10g)"
        prix_ajout_sucre = extra_moyen_sucre
        sucre_quantite_ajout = sucre_moyen
      }
      if (sucre_quantite_ajout == 4) {
        sucre_niveau = "Beaucoup (15g)"
        prix_ajout_sucre = extra_grand_sucre
        sucre_quantite_ajout = sucre_grand
      }
      ajout_cafe = cafe_utilise
      quantite_tot_lait = lait_quantite_ajout + lait_utilise

      println("Boisson selectionnee : " + boisson_nom )
      println("Niveau de sucre : " + sucre_niveau)
      if(boisson_selectionne != 1){
        println("Lait supplementaire  : " + lait_supplement)
      }
      if (coffeeStocks(machineId) < ajout_cafe){
        println("Erreur : Quantite de poudre de cafe insuffisante pour\npreparer la boisson selectionnee.\nVeuillez choisir une autre boisson ou verifier les\nstocks en mode Admin.")
      }
      if (sugarStocks(machineId) < sucre_quantite_ajout){
        println("Erreur : Quantite de sucre insuffisante pour\npreparer la boisson selectionnee.\nVeuillez choisir une autre boisson ou verifier les\nstocks en mode Admin.")
      }
      if (milkStocks(machineId) < quantite_tot_lait){
        println(" Erreur : Quantite de lait insuffisante pour preparer\nla boisson selectionnee.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
        println
      }
      if ((coffeeStocks(machineId) > ajout_cafe) && (sugarStocks(machineId) > sucre_quantite_ajout) && (milkStocks(machineId) > lait_quantite_ajout)) {
        prix_total = boisson_prix + lait_prix_total + prix_ajout_sucre
        if (lait_ajout_supp == 1) {
          println("Prix totale : CHF " + boisson_prix + " + CHF " + prix_ajout_sucre + " + CHF " + prix_ajout_lait +"= CHF " + prix_total)
        }
        if((boisson_selectionne == 1)||(lait_ajout_supp == 2)){
          println("Prix totale : CHF " + boisson_prix +  " + CHF " + prix_ajout_sucre + "= CHF " + prix_total)
        }

        val lettre_hasard1 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
        val lettre_hasard2 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
        val lettre_hasard3 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
        val lettre_hasard4 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
        val lettre_hasard5 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
        val code_twint = lettre_hasard1 + lettre_hasard2 + lettre_hasard3 + lettre_hasard4 + lettre_hasard5

        println("Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + code_twint + "\n(En attente de paiement...)")
        Thread.sleep(3000)
        println("Paiement confirme \nPreparation de votre boisson...\n[...]\nVotre Cafe est pret ! Bonne dégustation !")
        coffeeStocks(machineId) = coffeeStocks(machineId) - cafe_utilise.toInt
        milkStocks(machineId) = milkStocks(machineId) - lait_quantite_ajout - lait_utilise
        sugarStocks(machineId) = sugarStocks(machineId) - sucre_quantite_ajout.toInt
        mode = 0
        lait_ajout_supp = 0
        true
      }
      else{
        println("Veuillez selectionne une autre machine")
        mode = 0
        lait_ajout_supp = 0
        false
      }
    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit ={
      println("Reapprovisionnement des stocks...\nAjout :")
      var cafe_rajout = readLine("\tPoudre de cafe : ").toDouble
      while(cafe_rajout<0){
        cafe_rajout = readLine("\tPoudre de cafe : ").toDouble
      }
      var lait_rajout = readLine("\tLait :").toDouble
      while (lait_rajout <0){
        lait_rajout = readLine("\tLait :").toDouble
      }
      var sucre_rajout = readLine("\tSucre :").toDouble
      while(sucre_rajout<0){
        sucre_rajout = readLine("\tSucre :").toDouble
      }
      println(" Niveaux de stock mis a jour.\nRetour au menu principal...")

      coffeeStocks(machineId) += cafe_rajout.toInt
      sugarStocks(machineId) += sucre_rajout.toInt
      milkStocks(machineId) += lait_rajout
    }




    // demander a l'utilisateur quel mode il souhaite choisir......
    while (forwhile != 1) {
      mode = readLine("\tNospresso Cafe\n Veuillez selectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter\n>").toInt
      while ((mode<1)||(mode>3)){
        mode = readLine("\tNospresso Cafe\n Veuillez selectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter\n>").toInt
      }

      while (mode == 1) {
        machineID = readLine("Mode Client\nMachine selectionnee (1-"+nbMachines+") > ").toInt
        while ((machineID<1)||(machineID>nbMachines)){
          machineID = readLine("Mode Client\nMachine selectionnee (1-"+nbMachines+") > ").toInt
        }
        machineID -= 1
        serveClient(machineID, coffeeStocks, sugarStocks, milkStocks)
      }
      while (mode == 2) {
        machineID = readLine("Mode Admin\nMachine selectionnee (1-"+nbMachines+") > ").toInt
        while ((machineID<1)||(machineID>nbMachines)){
          machineID = readLine("Mode Admin\nMachine selectionnee (1-"+nbMachines+") > ").toInt
        }
        machineID -= 1
        println("Entrez le code PIN :")
        while ((!validatePin(machineID, machinePins)) && (tentatives >1)) {
          tentatives -= 1
          println("Code PIN incorrect. " + tentatives + "  tentatives restantes.")
        }
        if (tentatives == 1 ) {
          println("Code PIN incorrect. 0 tentatives restantes. \n\nTrop de tentatives echouees. Fin du programme.")
          forwhile =1
          mode = 0
        }
        if(tentatives>1){
          println("Acces accorde a la Machine " + (machineID+1))
          mode_admin = readLine("Souhaitez-vous :\n1) Reapprovisionner les ingredients\n2) Mettre a jour le code PIN\n>").toInt
          while((mode_admin<1)||(mode_admin>2)){
            mode_admin = readLine("Souhaitez-vous :\n1) Reapprovisionner les ingredients\n2) Mettre a jour le code PIN\n>").toInt
          }
          if(mode_admin ==1){
            println("Stocks :\n\tPoudre de cafe: "+coffeeStocks(machineID)+"g\n\tLait : "+ milkStocks(machineID) +"L\n\tSucre : " + sugarStocks(machineID) +  "g")

            restockMachine(machineID, coffeeStocks, sugarStocks, milkStocks)


            mode = 0
            tentatives=3
          }
          else {
            updatePin(machineID,machinePins)
            mode = 0
            tentatives=3
          }

        }
      }
      if(mode ==3){
        forwhile =1
        println("Merci! A bientot!")
      }
    }
  }
}