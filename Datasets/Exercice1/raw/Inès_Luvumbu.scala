object Main {
  import io.StdIn
  import scala.util.Random
  def main(args: Array[String]): Unit = {
    var Mainmenu: Boolean = true
    var Input: Int = 0
    val ChoixClient: Int = 1
    val ChoixAdmin: Int = 2
    val ChoixQuitter: Int = 3
    var InputCode = 0
    var PrixBoisson = 0.0
    var BoissonChoisie = ""
    var Sugarlevel = ""
    var PriceSugar = 0.000
    var AddSugar = 0
    var AddLait = 0
    var AddPoudre = 0
    var DoseLait = 0
    var PrixLait = 0.0
    var SupLait = ""
    var taillelatte = ""
    var nerror = 0 // en cas de stock insuffisant
    var Jesuicoince = true
    var TotalPrice = 0.0

    var stockdePoudre = 50.0 // stock initial en grammes
    var stockdeLait = 500.0 // stock initial en mililitres
    var stockdeSucre = 30.0 // stock initial wn grammes

    var TWINT = ""

    var CodePIN = 434343

    var approvSucre = 0.0
    var approvLait = 0.0
    var approvPoudre = 0.0

    while(Mainmenu) { // BOUCLE MAINMENU
      print("\t Nospresso Cafe\n")
      print("1) Client\n")
      print("2) Admin\n")
      print("3) Quitter\n")
      print("> ")
      Input = StdIn.readInt()

      while (1 > Input && Input > 3) { // pour s'assurer que la valeur entrée soit correcte
        print("Veuillez séléctionner une valeur correcte.")
        print("> ")
        Input = StdIn.readInt()
      }

      if (Input == ChoixClient) {
        while (Jesuicoince) {   // Boucle en cas d'erreur de stock
          print("Veuillez selectionner votre boisson :\n")
          print("1) Expresso - CHF 2.00\n")
          print("2) Cappuccino - CHF 2.50\n")
          print("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")
          print("> ")
          var InputClient = 0
          InputClient = StdIn.readInt()

          while (InputClient != 1 && InputClient != 2 && InputClient != 3) { // Verification valeur correcte
            print("Veuillez insérer une valeur correcte     \n> ")
            InputClient = StdIn.readInt()
          }

          if (InputClient == 1) { // Si le client choisi un expresso
            BoissonChoisie = "Expresso"
            PrixBoisson = 2
            AddPoudre = 8
          } else if (InputClient == 2) {
            BoissonChoisie = "Cappuccino"
            PrixBoisson = 2.5
            AddPoudre = 6
            AddLait = AddLait + 100
          } else if (InputClient == 3) {
            BoissonChoisie = "Latte"
            println("Quelle taille ?    ")
            println("1) Petit - CHF 2.70  ")
            println("2) Moyen - CHF 3.20 ")
            print("3) Grand - CHF 3.70  \n> ")
            InputClient = StdIn.readInt()

            while (InputClient < 1 || InputClient > 3) { // Verification valeur correcte
              println("Veuillez sélectionner une valeur correcte.    \n> ")
              InputClient = StdIn.readInt()
            }
            // Traitement des différentes tailles de latte:
            if (InputClient == 1) {
              BoissonChoisie = "Latte (Petit)"
              PrixBoisson = 2.7000
              AddPoudre = 6
              AddLait = AddLait + 120
            } else if (InputClient == 2) {
              BoissonChoisie = "Latte (Moyen)"
              PrixBoisson = 3.200
              AddPoudre = 8
              AddLait = AddLait + 150
            } else if (InputClient == 3) {
              BoissonChoisie = "Latte (Grand)"
              PrixBoisson = 3.70
              AddPoudre = 12
              AddLait = AddLait + 200
            }

          }

          println("Souhaitez-vous ajouter du sucre en supplément ?   ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10    ")
          println("3) Moyen (10g) - CHF 0.20   ")
          println("4) Beaucoup (15g) - CHF 0.30  ")
          print("> ")
          InputClient = StdIn.readInt()

          while (InputClient < 1 || InputClient > 4) {
            println("Veuillez selectionner une valeur correcte.     ")
            InputClient = StdIn.readInt()
          }

          AddSugar = (InputClient - 1) * 5
          PriceSugar = (InputClient - 1) * 0.1

          if (InputClient == 1) {
            Sugarlevel = "Sans sucre"
          } else if (InputClient == 2) {
            Sugarlevel = "Peu (5g)"
          } else if (InputClient == 3) {
            Sugarlevel = "Moyen (10g)"
          } else if (InputClient == 4) {
            Sugarlevel = "Beaucoup (15g)"
          }

          if (BoissonChoisie != "Expresso") { // Le client peut demander du lait en supplément seulement s'il choisi un latte ou un cappuccino, donc tout sauf un expresso
            println("Souhaitez-vous ajouter du lait en supplément ?      ")
            println("(Disponible uniquement pour Capuccino et Latte   ")
            println("1) Oui        \n2) Non")
            print("> ")
            InputClient = StdIn.readInt()

            while (InputClient != 1 && InputClient != 2) {
              println("Veuillez selectionner une valeur correcte.")
              print("> ")
              InputClient = StdIn.readInt()
            }

            if (InputClient == 1) {
              SupLait = "Oui"
              println("Combien de dose ?    ")
              print("> ")
              DoseLait = StdIn.readInt()

              while ((DoseLait < 1) || (DoseLait > 3)) {
                println("Veuillez sélectionner une valeur correcte.         ")
                print("> ")
                DoseLait = StdIn.readInt()
              }

              PrixLait = DoseLait * 0.05
              AddLait = AddLait + DoseLait * 50

            } else {
              SupLait = "Non"
            }
          }

          // Pour envisager les cas où le stock est insuffisant
          if (stockdePoudre < AddPoudre) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            nerror += 1
            AddLait = 0
            AddSugar = 0
            AddPoudre = 0
            DoseLait = 0
            PrixLait = 0
          }
          // Lorsque le stock de lait est insuffisant on procède comme pour la poudre...
          if (stockdeSucre < AddSugar) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            nerror += 1
            AddLait = 0
            AddSugar = 0
            AddPoudre = 0
            DoseLait = 0
            PrixLait = 0
          }

          if (stockdeLait < AddLait) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            nerror += 1
            AddLait = 0
            AddSugar = 0
            AddPoudre = 0
            DoseLait = 0
            PrixLait = 0
          }

          if (nerror == 0) { // Si le stock est suffisant le compte nerro est nul ainsi, on sort de la boucle et passe au payement
            Jesuicoince = false
            stockdeLait = stockdeLait - AddLait
            stockdeSucre -= AddSugar
            stockdePoudre -= AddPoudre

            // permet de générer un code aléatoire composé de 5 caractères
            val CodeTwint: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            for (i <- 1 to 5) {
              val emplacement = (Math.random() * 36).toInt
              TWINT += CodeTwint(emplacement)}

            println("Boisson sélectionnée : " + BoissonChoisie)
            println("Niveau de sucre : " + Sugarlevel)
            if (BoissonChoisie != "Expresso") {
              println("lait supplémentaire:" + SupLait)
            }
            TotalPrice = PrixBoisson + PriceSugar + PrixLait
            println(f"Prix total : $PrixBoisson%.2f  + CHF $PriceSugar%.2f + CHF $PrixLait%.2f = $TotalPrice%.2f")

            print("Veuillez payer en utilisant TWINT.\n")
            println("Votre code de payement est :" + TWINT)
            println("En attente de payement...")
            Thread.sleep(3000)   // Attendre 3 secondes

            println("Payement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(5000)   // Attendre 5 secondes
            println("Votre " + BoissonChoisie + " est prêt ! Bonne dégustation !")
          }
          else {
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            nerror = 0
          }

          AddLait = 0
          AddSugar = 0
          AddPoudre = 0
          DoseLait = 0
          PrixLait = 0

          Input = 0
        }
        Jesuicoince = true

      }

      if (Input == 2) { // MODE ADMIN
        print("Mode Admin      \n")

        println("Entrez le code PIN : ")
        InputCode = StdIn.readInt()
        while(InputCode != CodePIN){        // VERIFICATION PIN : boucle tant que le code pin entré est incorrect
          print("Code incorrect. Veuillez reessayer.\n> ")
          InputCode = StdIn.readInt()
        }
        if(InputCode == CodePIN){
          println("Accès autorisé.")
          println("") // Afficher le stock puis le mettre a jour
          println(f"Stocks: \nPoudre de cafe:  $stockdePoudre%.2f g     \nLait          :   ${stockdeLait*0.001}%.2f L \nSucre         : $stockdeSucre%.2f g")

          print("Poudre de cafe >")
          approvPoudre = StdIn.readDouble()
          stockdePoudre += approvPoudre
          print("Lait >")
          approvLait = StdIn.readDouble()
          stockdeLait += approvLait*1000 // le réapprovisionnement du lait est exprimé en litre alors que pour le stock initial nous avous indiqué des mililitres,donc on multiplie par 1000
          print("Sucre >")
          approvSucre = StdIn.readDouble()
          stockdeSucre += approvSucre

          println("Ajout :")
          println("\tPoudre de cafe : " + approvPoudre + "g")
          println("\tLait           : " + approvLait + "L")
          println("\tSucre          : " + approvSucre + "g")

          println("Réapprovisionnement des stocks...")
        }

        print  ("Niveaux de stock mis à jour.\n" )
        println     ("Retour au menu principal." )

        Input = 0
      }

      if (Input == 3) {  // Quitter la machine
        Mainmenu = false
      }

    }
  }
}
