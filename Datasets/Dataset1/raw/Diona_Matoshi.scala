object Main {
  def main(args: Array[String]):Unit = {
    import io.StdIn
    import scala.util.Random

    // Declaration des variables :
    // Variable d'itération du menu principal
    var MenuPrincipal = true

    // Variables de stock (à l'état initial) :
    var StockCafe = 50
    var StockSucre = 30
    var StockLait = 500.0 // en mL

    var ReapPoudre = 0
    var ReapLait = 0.0
    var ReapSucre = 0

    //Variable d'input utilisateur
    var UInput = 0

    // Variables de l'option Client
    var error = true
    var ChoixCafe = ""
    var PrixInitial = 0.0
    var PrixSucre = 0.0
    var PrixLait = 0.0
    var UtilisationPoudre = 0
    var UtilisationLait = 0
    var AjoutDeSucre = 0
    var NiveauSucreChoisi = ""
    var LaitSupplement = ""
    // Paiement
    var CodeTWINT = ""
    val chiffre_lettre: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var PrixTotal = 0.0

    //Variables de l'option Admin
    var PIN = 434343




    while(MenuPrincipal){            // Boucle while menu principal -----------------------------------------------------------
      //println(s"Poudre : $StockCafe Lait : $StockLait Sucre : $StockSucre") // DEBUG - STOCK VALUES
      print("\t Nospresso Café \nVeuillez sélectionner votre mode :   \n1) Client \n2) Admin\n3) Quitter \n> ")
      UInput = StdIn.readInt()
      while (UInput < 1 || UInput > 3) {      // While controle d'input utilisateur
        println("Veuillez indiquer une option correcte.\n> ")
        UInput = StdIn.readInt()
      }

      if(UInput == 1){

        while(error){       // Boucle si manque de stock
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          UInput = StdIn.readInt()

          while(!(UInput >= 1 && UInput <= 3)){ // Controle input
            print("Veuillez indiquer une option correcte.\n> ")
            UInput = StdIn.readInt()
          }

          if(UInput == 1)
          { // If  pour expresso
            ChoixCafe = "Expresso"
            PrixInitial = 2.0
            UtilisationPoudre = 8
          }

          else if (UInput == 2)
          { // If pour capuccino
            ChoixCafe = "Capuccino"
            PrixInitial = 2.50
            UtilisationPoudre = 6
            UtilisationLait = 100
          }

          else if (UInput == 3)
          { // If latte
            println ("Quelle taille ? ")
            println ("1) Petit - CHF 2.70")
            println ("2) Moyen - CHF 3.20")
            print ("3) Grand - CHF 3.70\n> ")
            UInput = StdIn.readInt()

            while (!(UInput >= 1 || UInput <= 3))
            {
              print("Veuillez indiquer une option correcte. \n> ")
              UInput = StdIn.readInt()
            }

            if(UInput == 1)
            {
              ChoixCafe = "Latte (Petit)"
              PrixInitial = 2.70
              UtilisationPoudre = 6
              UtilisationLait = 120
            } else if(UInput == 2)
            {
              ChoixCafe = "Latte (Moyen)"
              PrixInitial = 3.20
              UtilisationPoudre = 8
              UtilisationLait = 150
            }else if(UInput == 3)
            {
              ChoixCafe = "Latte (Grand)"
              PrixInitial = 3.70
              UtilisationPoudre = 12
              UtilisationLait = 200
            }

          }
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          UInput = StdIn.readInt()

          while (!(UInput >= 1 && UInput <= 4)) //Verif entrée utilisateur
          {
            println("Veuillez indiquer une option correcte.")
            UInput = StdIn.readInt()
          }
          NiveauSucreChoisi = "Sans sucre"
          if(UInput == 2)
          {
            AjoutDeSucre = 5
            PrixSucre = 0.1
            NiveauSucreChoisi = "Peu (5g)"
          } else if(UInput == 3)
          {
            AjoutDeSucre = 10
            PrixSucre = 0.2
            NiveauSucreChoisi = "Moyen (10g)"
          } else  if (UInput == 4)
          {
            AjoutDeSucre = 15
            PrixSucre = 0.3
            NiveauSucreChoisi = "Beaucoup (15g)"
          }

          if(ChoixCafe != "Expresso"){ //Traitement doses de lait
            println("Souhaitez-vous ajouter du lait en supplement ?")
            println("(Disponible uniquement pour Capuccino et Latte)")
            println("1) Oui\n2) Non")
            print("> ")
            UInput = StdIn.readInt()

            while (UInput != 1 && UInput != 2) {
              println ("Veuillez indiquer une option correcte.")
              print ("> ")
              UInput  =  StdIn.readInt()
            }
            if(UInput  == 1){
              LaitSupplement  = "Oui"
              println ("Combien de dose ? \n> ")
              UInput   =  StdIn.readInt()

              while(!(UInput>=1 && UInput <= 3)){
                println("Veuillez indiquer une option correcte.")
                print("> ")
                UInput   =  StdIn.readInt()
              }

              PrixLait = UInput * 0.05
              UtilisationLait += UInput * 50

            } else {LaitSupplement = "Non"}

          } // if pas expresso

          error = false // On sort provisoirement de la boucle, nous rentreront si il y a erreur

          // Erreurs
          if(UtilisationPoudre > StockCafe){
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            error = true
            UInput = 0
            AjoutDeSucre = 0
            UtilisationLait = 0
            UtilisationPoudre = 0

            PrixLait = 0
            PrixSucre = 0
          }
          if(UtilisationLait > StockLait){
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            error = true
            UInput = 0
            AjoutDeSucre = 0
            UtilisationLait = 0
            UtilisationPoudre = 0

            PrixLait = 0
            PrixSucre = 0
          }
          if(AjoutDeSucre > StockSucre){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            error = true
            UInput = 0
            AjoutDeSucre = 0
            UtilisationLait = 0
            UtilisationPoudre = 0

            PrixLait = 0
            PrixSucre = 0
          }
          if(error){
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }

          if(!error){ //Si pas erreur - Paiement
            // Mise à jour du stock :
            StockCafe -= UtilisationPoudre
            StockLait -= UtilisationLait
            StockSucre -= AjoutDeSucre





            for (i <- 1 to 5) {
              val pose = (Math.random() * 36).toInt
              CodeTWINT += chiffre_lettre(pose)}

            println("Boisson selectionnée :" + ChoixCafe)
            println(s"Niveau de sucre : $NiveauSucreChoisi")
            if (ChoixCafe != "Expresso") {
              println("lait supplémentaire:" + LaitSupplement)
            }
            PrixTotal = PrixLait + PrixInitial + PrixSucre
            println(f"Prix total : CHF $PrixInitial%.2f + CHF $PrixSucre%.2f + CHF $PrixLait%.2f = $PrixTotal%.2f")

            println("Veuillez payer en utilisant TWINT.")
            println("Votre code de payement est :" + CodeTWINT)
            println("En attente de payement...")
            Thread.sleep(3000)

            println("Payement confirmé.")
            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println(s"Votre $ChoixCafe est prêt ! Bonne dégustation !")
          }


        }


        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0

        error = true // Pour rerentrer dans choix boisson a la prochaine iteration
      }

      else if (UInput == 2){  // ADMIN MODE
        println ("Mode Admin  ")
        print("Entrez le code PIN : ")
        UInput = StdIn.readInt()
        while(UInput != PIN){             // Boucle contrôle de PIN
          println("Mot de passe erroné. Veuillez réessayer.")
          UInput = StdIn.readInt()
        }

        println("Accès autorisé.")
        //Affichage stocks et MaJ du stock manuellement - ATTENTION Input et affichange en Litres mais variable stock de lait en mL
        println("Stocks: ")
        println(f"Poudre de café: $StockCafe g\nLait          : ${StockLait*0.001}%.2f L\nSucre         : $StockSucre g")

        println("Réapprovisionnement des stocks...")

        print("Poudre de café > ")
        ReapPoudre = StdIn.readInt()
        print("Lait (en L)>")
        ReapLait = StdIn.readDouble()
        print("Sucre >")
        ReapSucre = StdIn.readInt()

        println("Ajout :")
        println(s"\tPoudre de cafe : $ReapPoudre g")
        println(s"\tLait           : " + ReapLait + "L")
        println(s"\tSucre          : $ReapSucre g")

        StockCafe += ReapPoudre
        StockLait += ReapLait*1000
        StockSucre += ReapSucre

        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal...")
      }
      else if (UInput == 3){ // Quitter
        MenuPrincipal = false
      }
    }
  }
}