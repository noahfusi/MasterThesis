import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var code = 0
    var quitter = 0
    var mode = 0
    val pagedintro = "\n              Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> "
    val pageselecafe = "\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> "
    val errorcafe = "\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin."
    val errorlait = "\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson."
    val errorsucre = "\nErreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson."

    //-----------------Paramètres----------------------------------------------------------------------------------------
    var choixcafe = 0
    var choixsucre = 0
    var choixtaillelatte = 0
    var laitsupp = 0
    var nomcafe = ""
    var niveausucre = ""
    var laitsupplement = ""
    var doseslait = 0
    //----------------Calculs--------------------------------------------------------------------------------------------

    var prixsucre = 0.00
    var prixlait = 0.00
    var prixcafe = 0.00
    var prixtotal = 0.00
    //----------------Ingrédients----------------------------------------------------------------------------------------
    var stockcafe = 50 // grammes
    var restockcafe = 0
    var stocksucre = 30 //grammes
    var restocksucre = 0

    var stocklait = 0.500 // en L
    var restocklait = 0.00

    val espresso = 8 //grammes de café utilisé
    val capuccino = 6
    val petitlatte = 6 // pour la clareté jai utilisé petitlatte = 6 au lieu de faire petitlatte = capuccino
    val moyenlatte = 8
    val grandlatte = 12



    //----------------Conditions-----------------------------------------------------------------------------------------


    while (quitter == 0) {
      print(pagedintro) //Selection du mode
      mode = readInt()

      if (mode == 1) {//Mode client
        do {
          print(pageselecafe)
          choixcafe = readInt()
        }while(choixcafe != 1 && choixcafe != 2 && choixcafe != 3)
        if (choixcafe == 1) { // ici on est dans "espresso"
          stockcafe = stockcafe - espresso
          nomcafe = "Espresso"
          prixcafe = 2.00

          do{//Choix sucre
            print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
            choixsucre = readInt()
          }while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4)

          if (choixsucre == 1) {
            niveausucre = "Sans sucre"
            stocksucre = stocksucre
            prixsucre = 0
          } else if (choixsucre == 2) {
            niveausucre = "Peu (5g)"
            stocksucre = stocksucre - 5
            prixsucre = 0.10

          } else if (choixsucre == 3) {
            niveausucre = "Moyen (10g)"
            stocksucre = stocksucre - 10
            prixsucre = 0.20

          } else if (choixsucre == 4) {
            niveausucre = "Beaucoup (15g)"
            stocksucre = stocksucre - 15
            prixsucre = 0.30
          }


        } else if (choixcafe == 2) { //ici on est dans "cappucino"
          nomcafe = "Cappucino"
          prixcafe = 2.50
          stockcafe = stockcafe - capuccino
          stocklait = stocklait - 0.1

          do{//choix sucre
            print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
            choixsucre = readInt()
          }while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4)

          if (choixsucre == 1) {
            niveausucre = "Sans sucre"
            stocksucre = stocksucre
            prixsucre = 0
          } else if (choixsucre == 2) {
            niveausucre = "Peu (5g)"
            stocksucre = stocksucre - 5
            prixsucre = 0.10

          } else if (choixsucre == 3) {
            niveausucre = "Moyen (10g)"
            stocksucre = stocksucre - 10
            prixsucre = 0.20

          } else if (choixsucre == 4) {
            niveausucre = "Beaucoup (15g)"
            stocksucre = stocksucre - 15
            prixsucre = 0.30
          }
          do{
            print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
            laitsupp = readInt()
          }while (laitsupp != 1 && laitsupp != 2 && laitsupp != 3)

          if (laitsupp == 1) {
            laitsupplement = "Oui"
            do{
              print("Combien de doses?\n> ")
              doseslait = readInt()
            }while (doseslait != 1 && doseslait != 2 && doseslait != 3)

            if (doseslait == 1) {
              stocklait = stocklait - 0.05
              prixlait = 0.05

            } else if (doseslait == 2) {
              stocklait = stocklait - 0.1
              prixlait = 0.10

            } else if (doseslait == 3) {
              stocklait = stocklait - 0.15
              prixlait = 0.15
            }} //ici on a fini de choisir le lait (ou non) laitsupp


        } else if (choixcafe == 3) { //ici on est dans "Latte"
          do {
            print("Veuillez choisir la taille du Latte \n 1) Petit \n 2) Moyen \n 3) Grand \n > ")
            choixtaillelatte = readInt() // variable du choix de la taille du latte.
          }while(choixtaillelatte != 1 && choixtaillelatte != 2 && choixtaillelatte != 3)

          if (choixtaillelatte == 1) { // petit latte
            nomcafe = "Petit Latte"
            prixcafe = 2.70
            stockcafe = stockcafe - petitlatte
            stocklait = stocklait - 0.12
          } else if (choixtaillelatte == 2) { // latte moyen
            nomcafe = "Latte Moyen"
            prixcafe = 3.20
            stockcafe = stockcafe - moyenlatte
            stocklait = stocklait - 0.15

          } else if (choixtaillelatte == 3) { // grand latte
            nomcafe = "Grand Latte"
            prixcafe = 3.70
            stockcafe = stockcafe - grandlatte
            stocklait = stocklait - 0.2
          }
          do{
            print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
            choixsucre = readInt()
          }while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4)

          if (choixsucre == 1) {
            niveausucre = "Sans sucre"
            stocksucre = stocksucre
            prixsucre = 0
          } else if (choixsucre == 2) {
            niveausucre = "Peu (5g)"
            stocksucre = stocksucre - 5
            prixsucre = 0.10

          } else if (choixsucre == 3) {
            niveausucre = "Moyen (10g)"
            stocksucre = stocksucre - 10
            prixsucre = 0.20

          } else if (choixsucre == 4) {
            niveausucre = "Beaucoup (15g)"
            stocksucre = stocksucre - 15
            prixsucre = 0.30
          }

          do{
            print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
            laitsupp = readInt()
          }while (laitsupp != 1 && laitsupp != 2)

          if (laitsupp == 1) {

            laitsupplement = "Oui"
            do{
              print("Combien de doses?\n> ")
              doseslait = readInt()
            }while (doseslait != 1 && doseslait != 2 && doseslait != 3)

            if (doseslait == 1) {
              stocklait = stocklait - 0.05
              prixlait = 0.05

            } else if (doseslait == 2) {
              stocklait = stocklait - 0.1
              prixlait = 0.10

            } else if (doseslait == 3) {
              stocklait = stocklait - 0.15
              prixlait = 0.15
            }} //fin laitsupp Latte
        } //fin selection cafe


        prixtotal = prixcafe + prixsucre + prixlait

        print("Boisson séléctionnée: " + nomcafe + "\nNiveau de sucre: " + niveausucre)

        if (choixcafe == 2 || choixcafe == 3) { //ici on affiche oui ou non si on a choisi du lait supp
          if (doseslait == 1 || doseslait == 2 || doseslait == 3) {
            print("\nLait en supplément: Oui")
          } else if (doseslait != 1 || doseslait != 2 || doseslait != 3) {
            print("\nLait en supplément: Non")
          }
        }

        if (stockcafe >= 6 && stocklait >= 0 && stocksucre >= 0) {   //on rentre dans l'écran de paiement.
          print("\nPrix total : " + prixsucre + " CHF " + prixcafe + " = CHF " + prixtotal + " CHF \n")
          println("\nVeuillez payer en utilisant Twint.")
          print("Votre code de paiement est: ")
          for (i <- 1 to 5) { //afin de générer le code alphanumérique, il fallait d'abord générer des nombres aléatoires entre 1 et 26, puis les associer au lettres de l'alphabet. Une fois cela fait, il fallait répéter l'opération 5 fois.
            val code = (math.random() * 26).toInt
            if (code == 0) {
              print('A')
            } else if (code == 1) {
              print('B')
            } else if (code == 2) {
              print('C')
            } else if (code == 3) {
              print('D')
            } else if (code == 4) {
              print('E')
            } else if (code == 5) {
              print('F')
            } else if (code == 6) {
              print('G')
            } else if (code == 7) {
              print('H')
            } else if (code == 8) {
              print('I')
            } else if (code == 9) {
              print('J')
            } else if (code == 10) {
              print('K')
            } else if (code == 11) {
              print('L')
            } else if (code == 12) {
              print('M')
            } else if (code == 13) {
              print('N')
            } else if (code == 14) {
              print('O')
            } else if (code == 15) {
              print('P')
            } else if (code == 16) {
              print('Q')
            } else if (code == 17) {
              print('R')
            } else if (code == 18) {
              print('S')
            } else if (code == 19) {
              print('T')
            } else if (code == 20) {
              print('U')
            } else if (code == 21) {
              print('V')
            } else if (code == 22) {
              print('W')
            } else if (code == 23) {
              print('X')
            } else if (code == 24) {
              print('Y')
            } else if (code == 25) {
              print('Z')
            }
          }

          print("\n(En attente de validation du paiement...)\n ") //ici on est à la fin de l'écran de paiement.
          Thread.sleep(3000)
          print("\nMerci ! Votre paiement a été accepté\n")
          Thread.sleep(2000)
          print("\nPréparation de votre boisson...\n[...]")
          Thread.sleep(5000)
          if (choixcafe == 1) {
            print("\nVotre " + nomcafe + " est prêt ! Bonne dégustation !\n")
          } else if (choixcafe == 2) {
            print("\nVotre " + nomcafe + " est prêt ! Bonne dégustation !\n")
          } else if (choixcafe == 3) {
            print("\nVotre " + nomcafe + " est prêt ! Bonne dégustation !\n")
          }
          //fin while quitter



        }else if (stockcafe < 0){print(errorcafe) }
        else if (stocklait < 0){print(errorlait)}
        else if (stocksucre < 0){print(errorsucre)}
        Thread.sleep(2000)
      }//fin mode 1

      else if (mode == 2) {

        do {
          print("Veuillez entrer le code secret: \n> ")
          code = readInt()
        }while(code != 434343)

        if(code == 434343) {
          println("Accès autorisé.\n")
          Thread.sleep(1000)
          print("Stocks \n")
          if (stockcafe < 0 ){
            stockcafe = 0
            println("Stock de café : " + stockcafe)}
          else
          {println("Stock de café : " + stockcafe)}
          if (stocklait < 0) {
            stocklait = 0
            println("Stock de lait : " + stocklait + "L")}
          else {
            println("Stock de lait : " + stocklait + "L")}
          if (stocksucre < 0){
            stocksucre = 0
            println("Stock de sucre : " + stocksucre)}
          else
            println("Stock de sucre : " + stocksucre)

          Thread.sleep(2000)
          print("\nRéapprovisionnement des stocks...\n\nAjout \n")
          print("Poudre de café : ")
          restockcafe = readInt()
          print("Lait : ")
          restocklait = readDouble()
          print("Sucre : ")
          restocksucre = readInt()
          stocklait = stocklait + restocklait
          stocksucre = stocksucre + restocksucre
          stockcafe = stockcafe + restockcafe
          print("Niveaux de stock mis à jour.\n")
          Thread.sleep(1000)
          print("Retour au menu principal...\n")
          Thread.sleep(2000)
        } //Fin if code
      }//Fin mode 2




      else if (mode == 3) {//Mode "Quitter"
        quitter = 1}
    }
  }
}