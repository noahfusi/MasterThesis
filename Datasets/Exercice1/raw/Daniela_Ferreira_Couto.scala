object Main {
  def main(args: Array[String]): Unit = {
    import io.StdIn._

    //initiation des valeurs de stock initiaux
    var StockCafe = 50.0
    var StockSucre = 30.0
    var StockLait = 500.0

    //boucle de retour au menu
    var retourmenu = 0
    while (retourmenu == 0){
      var SCpoudreCafe = 0
      var SCsucre = 0
      var SClait = 0

      //lancement du programme
    println (" ")
    println("     " + "Nospresso Café")
    println("Veuillez selectionner votre mode : ")
    println("1) Client")
    println("2) Admin ")
    println("3) Quitter ")
    print("> ") // l'utilisateur doit choisir son mode 1,2 ou 3
    var mode = readInt()
    while (!((mode == 1) || (mode == 2) || (mode == 3))) {
      mode = readInt()
    }

      //mode client
      if (mode == 1) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        var boisson = readInt()
        while (!((boisson == 1) || (boisson == 2) || (boisson == 3))) {
          boisson = readInt()
        }

       //initialisation du prix
      var prixStandard = 0.0
      var recapBoisson = " "
      var recapQuantSucre = " "
      var recapLait = " "
      var prixSucreSupp = 0.0
      var prixLaitSupp = 0.0
      var prixTotal = 0.0


        //boisson : Expresso
        if (boisson == 1) {
          println(" Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucre = readInt()
          while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
            sucre = readInt()}
           //stock consommé
           SCpoudreCafe = 8
           SCsucre = (sucre - 1) * 5

          //prix + recaps commande
          prixStandard = 2.00
          recapBoisson = "Expresso"

          if (sucre == 1){
            recapQuantSucre = "Sans sucre"
          }
          else if (sucre == 2){
            recapQuantSucre = "Peu (5g)"
          }
          else if (sucre == 3){
            recapQuantSucre = "Moyen (10g)"
          }
          else if (sucre == 4){
            recapQuantSucre = "Beaucoup (15g)"
          }
          prixSucreSupp = (sucre - 1) * 0.10
          prixTotal = prixStandard + prixSucreSupp

        }

        // boisson : Cappuccino
        if (boisson == 2) {
          println(" Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucre = readInt()
          while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
            sucre = readInt()
          }
          //stock consommé de poudre café lait et si choisi sucre
           SCpoudreCafe = 6
           SClait = 100
           SCsucre = (sucre - 1) * 5

          //prix + recaps commande
          prixStandard = 2.50
          recapBoisson = "Cappuccino"
          if (sucre == 1){
            recapQuantSucre = "Sans sucre"
          }
          else if (sucre == 2){
            recapQuantSucre = "Peu (5g)"
          }
          else if (sucre == 3){
            recapQuantSucre = "Moyen (10g)"
          }
          else if (sucre == 4){
            recapQuantSucre = "Beaucoup (15g)"
          }

          prixSucreSupp = (sucre - 1) * 0.10

          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          var lait = readInt()
          while (!((lait == 1) || (lait == 2))) {
            lait = readInt()
          }
          if (lait == 1) {
            println("combien de doses ? ")
            print("> ")
            var doselait = readInt()
            while (!((doselait == 1) || (doselait == 2) || (doselait == 3))) {
              doselait = readInt()
            }
            SClait += doselait * 50 //stock consommé lait si supplément

              recapLait = "Oui, " + doselait + " dose(s)"
              prixLaitSupp = doselait * 0.05
          }
          else {
            recapLait = "Non"
          }
          prixTotal = prixStandard + prixSucreSupp + prixLaitSupp
        }

        //boisson: latte
        if (boisson == 3) {
          println("veuillez selectionner la taille de votre boisson ")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          var taillelatte = readInt()
          while (!((taillelatte == 1) || (taillelatte == 2) || (taillelatte == 3))) {
            taillelatte = readInt()
          }

          //stock consommé en fonction de la taille
          if (taillelatte == 1) {
            SCpoudreCafe = 6
            SClait = 120
            recapBoisson = "Latte (Petit)"
            prixStandard = 2.70
          }
          else if (taillelatte == 2) {
            SCpoudreCafe = 8
            SClait = 150
            recapBoisson = "Latte (Moyen)"
            prixStandard = 3.20
          }
          else {
            SCpoudreCafe = 12
            SClait = 200
            recapBoisson = "Latte (Grand)"
            prixStandard = 3.70
          }

          println(" Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucre = readInt()
          while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
            sucre = readInt()
          }
          //stock consommé de sucre si choisi
           SCsucre = (sucre - 1) * 5

          //prix supp lait
          if (sucre == 1){
            recapQuantSucre = "Sans sucre"
          }
          else if (sucre == 2){
            recapQuantSucre = "Peu (5g)"
          }
          else if (sucre == 3){
            recapQuantSucre = "Moyen (10g)"
          }
          else if (sucre == 4){
            recapQuantSucre = "Beaucoup (15g)"
          }
          prixSucreSupp = (sucre - 1) * 0.10

          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          var lait = readInt()
          while (!((lait == 1) || (lait == 2))) {
            lait = readInt()
          }
          if (lait == 1) {
            println("combien de doses ? ")
            print("> ")
            var doselait = readInt()
            while (!((doselait == 1) || (doselait == 2) || (doselait == 3))) {
              doselait = readInt()
            }
            SClait += doselait * 50 //stock consommé lait si supplément

            recapLait = "Oui, " + doselait + " dose(s)"
            prixLaitSupp = doselait * 0.05

          }
          else {
            recapLait = "Non"

          }
          prixTotal = prixStandard + prixSucreSupp + prixLaitSupp
        } //fin de personnalisation des boissons


        //gestion des stock
        if ((SCpoudreCafe <= StockCafe) && (SCsucre <= StockSucre) && (SClait <= StockLait)) {
          StockCafe -= SCpoudreCafe
          StockSucre -= SCsucre
          StockLait -= SClait

          //Interface de Paiement (code Twint)
          val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var CodePaiement = " "
          for (i <- 1 to 5) {
            var caractere = alphanumerique((math.random * alphanumerique.length).toInt).toString
            CodePaiement += caractere
          }
          // récap géneralisé de la commande + interface de paiement
          if (boisson == 1) {
            println()
            println("Boisson selectionnée : " + recapBoisson)
            println("Niveau de sucre: " + recapQuantSucre)
            printf ("prix Total : CHF %.2f + CHF %.2f = CHF %.2f ",prixStandard,prixSucreSupp,prixTotal)
            println()
            println()
          }
          else if ((boisson == 2) || (boisson == 3)){
            println()
            println("Boisson selectionnée : " + recapBoisson)
            println("Niveau de sucre: " + recapQuantSucre)
            println ("Lait supplémentaire : " + recapLait)
            printf ("prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ",prixStandard,prixSucreSupp,prixLaitSupp,prixTotal)
            println()
            println()
          }

          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + CodePaiement)
          println("(En attente de validation du paiement...")
          Thread.sleep(3000) // attente de 3 secondes
          println(" ") // saut de ligne
          println("Merci ! Votre paiement a été accepté. ")
          println ("Préparation de votre boisson...")
          Thread.sleep(5000) //attente de 5 secondes
          println ("Votre " + recapBoisson + " est prêt ! Bonne dégustation !")

        }
          //gestion des erreurs en cas de stock insuffisants
        else if (SCpoudreCafe > StockCafe) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ")
        }
        else if (SCsucre > StockSucre) {
          println("Erreur : la quantité de sucre est insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une quantité de sucre inférieur ou vérifier les sotcks en mode Admin ")
        }
        else if (SClait > StockLait) {
          if (boisson == 2) {
            println(" Erreur : la quantité de lait est insuffisante pour préparer votre commande")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          }
              else if (boisson == 3){
              println ("Erreur : la quantité de lait est insuffisante pour préparer la boisson sélectionnée")
                println ("Veuillez choisir une taille plus petite ou essayer une autre boisson")
            }
          }

      } //fin du mode client


      //mode admin
      if (mode == 2) {
        var codePIN = 434343
        println ("Entrez le Code PIN : ******")
        codePIN = readInt()
        while (codePIN != 434343) {
          codePIN = readInt()
        }
        var reapprovisCafe = 0.0
        var reapprovisLait = 0.0
        var reapprovisSucre = 0.0

        if (codePIN == 434343){
          println("Accès autorisé.")
          println()
          println ("Stocks : ")
          println ("Poudre de café : " + StockCafe + "g")
          println ("Lait           : " + StockLait + "ml")
          println ("Sucre          : " + StockSucre + "g")
          println()

          println("Réapprovisionnement des stocks ...")
          println ("Ajout :")
          reapprovisCafe = readLine ("    Poudre de café : " ).toDouble
          reapprovisLait = readLine ("    Lait           : " ).toDouble
          reapprovisSucre = readLine ("    Sucre          : " ).toDouble

          StockCafe += reapprovisCafe
          StockLait += reapprovisLait
          StockSucre += reapprovisSucre
        }
      }

      //mode quitter
      if (mode == 3) {}
    } // fin de la boucle de retour au menu
  }
}
