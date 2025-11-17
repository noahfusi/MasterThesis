import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {


    var choixdumode = 0
    var quitter = false
    var PIN = 0
    var ajoutpoudreacafe = 0
    var ajoutsucre = 0
    var ajoutlait = 0
    var choixducafe = 0
    var choixtaillelatte = 0
    var transactionterminee = false
    var stockpoudrecafe = 50 //grammes
    var stocksucre = 30 // grammes
    var stocklait = 0.500 // litres
    var choixdusucre = 0
    var choixouiounonlait = 0
    var choixnbdosedelait = 0
    var prixcafe = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var prixtotal = 0.0
    var retouraumenu = false
    var cafechoisi = "Expresso".toString
    var sucrechoisi = "Peu (5g)"
    var doselaitchoisie = " 50ml"
    var laitensupplement = " Oui "
    //code TWINT
    val caractères = "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789" //chiffres à double pour avoir moins de chances de tomber sur un code composé uniquement de lettres
    var codeTWINT = ""
    // fin code Twint

    while ((choixdumode != 3) && (quitter == false)) { //boucle générale première
      transactionterminee = false // réinitialisation en cas de gestion des stocks
      println("        Nospresso café")


      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      choixdumode = readInt()

      while ((choixdumode != 1) && (choixdumode != 2) && (choixdumode != 3)) {
        println("Entrée non-valide")
        println("Veuillez sélectionner votre mode")
        choixdumode = readInt()

      }

      //mode admin
      if (choixdumode == 2) {
        println("Entrez le PIN")
        PIN = readInt()
        while (PIN != 434343) {
          println("PIN érroné, Veuillez réessayer")
          PIN = readInt()
        }
        if (PIN == 434343) {
          println("Accès autorisé")
          println(" ")
          println("Stocks :")
          println("Poudre à café :      " + stockpoudrecafe)
          printf("Lait :  %.3f \n", stocklait)
          println("Sucre :      " + stocksucre)

          println("Réapprovisionnement des stocks : ")
          println("Entrez le nombre de grammes de poudre à café que vous voulez ajouter")
          ajoutpoudreacafe = readInt()
          println(" Entrez le nombre de grammes de sucre que vous voulez ajouter")
          ajoutsucre = readInt()
          println("Entrez le nombre de litres de lait que vous voulez ajouter")
          ajoutlait = readInt()
          println("Ajout : ")
          println("Poudre à café : " + ajoutpoudreacafe)
          println("Lait : " + ajoutlait)
          println("Sucre : " + ajoutlait)
          println("Niveaux de stock mis à jour.")
          stockpoudrecafe += ajoutpoudreacafe
          stocksucre += ajoutsucre
          stocklait += ajoutlait
          transactionterminee = true

        }
      }


      if (choixdumode == 3) {
        quitter = true
        println("Programme quitté")
        transactionterminee = true
      }


      // mode 1
      while (transactionterminee == false) {


        retouraumenu = false
        while ((transactionterminee == false) && (retouraumenu == false)) {

          if (choixdumode == 1) {
            println("Veuillez sélectionner votre boisson :")
            println("1) Expresso - CHF 2.00")
            println("2) Capuccino - CHF 2.50")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            var choixducafe = readInt()

            while ((choixducafe != 1) && (choixducafe != 2) && (choixducafe != 3)) {
              println("Entrée non-valide")
              println("Veuillez sélectionner votre café")
              choixducafe = readInt()
            }
            if (choixducafe == 1) {


              prixcafe = 2.00
              println("Voulez-vous ajouter du sucre ?")
              println("1) Sans sucre")
              println("2) Peu (5g) - CHF 0.10")
              println("3) Moyen (10g) - CHF 0.20")
              println("4) Beaucoup (15g) - CHF 0.30")
              choixdusucre = readInt()


              while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
                println("Entrée non-valide")
                println("Veuillez sélectionner le niveau de sucre")
                choixdusucre = readInt()
              }
              if (stockpoudrecafe < 8) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                retouraumenu = true
              }

              if ((choixdusucre == 2) && (stocksucre < 5)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }

              if ((choixdusucre == 3) && (stocksucre < 10)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option.")
                retouraumenu = true
              }
              if ((choixdusucre == 4) && (stocksucre < 15)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Erreur : stock de sucre insuffisant. Il reste. Veuillez sélectionner une autre option.")
                retouraumenu = true
              }

              if (choixdusucre == 1) {
                prixsucre = 0.00
              }
              if (choixdusucre == 2) {
                prixsucre = 0.10
              }
              if (choixdusucre == 3) {
                prixsucre = 0.20
              }
              if (choixdusucre == 4) {
                prixsucre = 0.30
              }

              // diminution des stocks de sucre
              {
                if (choixdusucre == 2) {
                  stocksucre -= 5
                }
                if (choixdusucre == 3) {
                  stocksucre -= 10
                }
                if (choixdusucre == 4) {
                  stocksucre -= 15
                }
              }

              //fin de la sélection + paiement
              if (retouraumenu == false) {
                transactionterminee = true
                if (choixducafe == 1) {
                  stockpoudrecafe -= 8
                  cafechoisi = "Expresso".toString
                }
                if (choixdusucre == 2) {
                  sucrechoisi = " Peu (5g)"
                }
                if (choixdusucre == 3) {
                  sucrechoisi = " Moyen (10g)"
                }
                if (choixdusucre == 4) {
                  sucrechoisi = " Beaucoup (15g)"
                }
                var codeTWINT = ""
                for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génere un nouveau code pour ne pas avoir toujours le même code twint

                prixtotal = prixcafe + prixsucre
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                printf("Prix total : CHF %.2f CHF  + %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre,prixlait,prixtotal)
                println(" ")
                println("Veuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + codeTWINT)
                println("(En attente de paiement...)")
                println(" ")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Paiement confirmé")
                println("préparation de votre boisson...")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Votre Expresso est prêt ! Bonne dégustation !")
                println(" ")
                println(" ")
              }
            } // fin expresso 1

//capucino
            if (choixducafe == 2) {


              println("Voulez-vous ajouter du sucre ?")
              println("1) Sans sucre")
              println("2) Peu (5g) - CHF 0.10")
              println("3) Moyen (10g) - CHF 0.20")
              println("4) Beaucoup (15g) - CHF 0.30")

              choixdusucre = readInt()
              while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
                println("Entrée non-valide")
                println("Veuillez sélectionner le niveau de sucre")
                choixdusucre = readInt()
              }







              // Choix du lait
              println("Souhaitez-vous ajouter du lait en supplément?")
              println("1) Oui")
              println("2) Non")
              choixouiounonlait = readInt()

              while ((choixouiounonlait != 1) && (choixouiounonlait != 2)) {
                println("Entrée non-valide")
                println("Veuillez entrer 1 ou 2")
                choixouiounonlait = readInt()
              }

              if (choixouiounonlait == 1) {
                println("Combien de doses? (1 dose = 50 ml) 3 doses maximum")
                println("1) 1 dose (50 ml) - CHF 0.05")
                println("2) 2 doses (100 ml) - CHF 0.10")
                println("3) 3 doses (150 ml) - CHF 0.15")
                choixnbdosedelait = readInt()
                while ((choixnbdosedelait != 1) && (choixnbdosedelait != 2) && (choixnbdosedelait != 3)) {
                  println("Entrée non-valide, Combien de doses de lait?")
                  choixnbdosedelait = readInt()
                }
              }



//gestion des insuffisances de stock

              if ((stockpoudrecafe < 6) && (stocklait < 0.100)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println(" ")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                retouraumenu = true
              }

              if ((choixdusucre == 2) && (stocksucre < 5)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println(" ")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option.")
                retouraumenu = true
              }
              if ((choixdusucre == 3) && (stocksucre < 10)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option.")
                retouraumenu = true
              }
              if ((choixdusucre == 4) && (stocksucre < 15)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une option.")
                retouraumenu = true
              }


              if ((choixnbdosedelait == 1) && (stocklait < 0.05)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de lait insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixnbdosedelait == 2) && (stocklait < 0.10)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixnbdosedelait == 3) && (stocklait < 0.15)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              //section de paiement et réduction des stocks
              if (retouraumenu == false) {
                transactionterminee = true
                if (choixducafe == 2) {
                  cafechoisi = "Capucino".toString
                  stockpoudrecafe -= 6
                  stocklait -= 0.100
                }
                if (choixdusucre == 2) {
                  sucrechoisi = " Peu (5g)"
                  stocksucre -= 5
                  prixsucre = 0.10
                }
                if (choixdusucre == 3) {
                  sucrechoisi = " Moyen (10g)"
                  stocksucre -= 10
                  prixsucre = 0.20
                }
                if (choixdusucre == 4) {
                  sucrechoisi = " Beaucoup (15g)"
                  stocksucre -= 15
                  prixsucre = 0.30
                }


                if (choixnbdosedelait == 1) {
                  doselaitchoisie = " 50ml"
                  stocklait -= 0.050
                  prixlait = 0.05
                }
                if (choixnbdosedelait == 2) {
                  doselaitchoisie = " 100 ml"
                  stocklait -= 0.100
                  prixlait = 0.10
                }
                if (choixnbdosedelait == 3) {
                  doselaitchoisie = " 150ml"
                  stocklait -= 0.150
                  prixlait = 0.15
                }
                if (choixouiounonlait == 1) {
                  laitensupplement = " Oui "
                }
                if (choixouiounonlait == 2) {
                  laitensupplement = " Non "
                }
                if (choixducafe == 2) {
                  prixcafe = 2.50
                }
                var codeTWINT = ""
                for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génere un nouveau code pour ne pas avoir toujours le même code twint
                prixtotal = prixcafe + prixsucre + prixlait
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                printf("Prix total : CHF %.2f CHF  + %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre,prixlait,prixtotal)
                println(" ")
                println("Veuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + codeTWINT)
                println("(En attente de paiement...)")
                println(" ")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Paiement confirmé")
                println("préparation de votre boisson...")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Votre Capucino est prêt ! Bonne dégustation !")
                println(" ")
                println(" ")
              }

              // FIN CAFE 2

            } //fin  boucle choix du cafe = 2

            //début café 3 Latte

            if (choixducafe == 3) {
              println("Veuillez choisir la taille de votre Latte : ")
              println("1) Petit - CHF 2.70")
              println("2) Moyen - CHF 3.20")
              println("3) Grand - CHF 3-70")
              choixtaillelatte = readInt()
              while ((choixtaillelatte != 1) && (choixtaillelatte != 2) && (choixtaillelatte != 3)) {
                println("Entrée invalide, veuillez entrer une valeur valide")
                choixtaillelatte = readInt()
              }

              //sucre

              println("Voulez-vous ajouter du sucre ?")
              println("1) Sans sucre")
              println("2) Peu (5g) - CHF 0.10")
              println("3) Moyen (10g) - CHF 0.20")
              println("4) Beaucoup (15g) - CHF 0.30")

              choixdusucre = readInt()
              while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
                println("Entrée non-valide")
                println("Veuillez sélectionner le niveau de sucre")
                choixdusucre = readInt()
              }

              // Choix du lait
              println("Souhaitez-vous ajouter du lait en supplément?")
              println("1) Oui")
              println("2) Non")
              choixouiounonlait = readInt()

              while ((choixouiounonlait != 1) && (choixouiounonlait != 2)) {
                println("Entrée non-valide")
                println("Veuillez entrer 1 ou 2")
                choixouiounonlait = readInt()
              }

              if (choixouiounonlait == 1) {
                println("Combien de doses? (1 dose = 50 ml) 3 doses maximum")
                println("1) 1 dose (50 ml) - CHF 0.05")
                println("2) 2 doses (100 ml) - CHF 0.10")
                println("3) 3 doses (150 ml) - CHF 0.15")
                choixnbdosedelait = readInt()
                while ((choixnbdosedelait != 1) && (choixnbdosedelait != 2) && (choixnbdosedelait != 3)) {
                  println("Entrée non-valide, Combien de doses de lait?")
                  choixnbdosedelait = readInt()
                }
              }
              //gestion des insuffisances de stock


              if ((choixtaillelatte == 1) && (stockpoudrecafe < 6)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                retouraumenu = true
              }
              if ((choixtaillelatte == 1) && (stocklait < 0.120)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez essayer une autre boisson.")
                retouraumenu = true
              }

              if ((choixtaillelatte == 2) && (stockpoudrecafe < 8)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                retouraumenu = true
              }
              if ((choixtaillelatte == 2) && (stocklait < 0.150)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                retouraumenu = true
              }

              if ((choixtaillelatte == 3) && (stockpoudrecafe < 12)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer autre boisson.")
                retouraumenu = true
              }
              if ((choixtaillelatte == 3) && (stocklait < 0.200)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                retouraumenu = true
              }
              if ((choixnbdosedelait == 1) && (stocklait < 0.05)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de lait insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixnbdosedelait == 2) && (stocklait < 0.10)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de lait insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixnbdosedelait == 3) && (stocklait < 0.15)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de lait insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixdusucre == 2) && (stocksucre < 5)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixdusucre == 3) && (stocksucre < 10)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }
              if ((choixdusucre == 4) && (stocksucre < 15)) {
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                println("")
                println("Erreur : stock de sucre insuffisant. Veuillez sélectionner une autre option")
                retouraumenu = true
              }

              //section de paiement + réduction des stocks

              if (retouraumenu == false) {
                transactionterminee = true
                if ((choixducafe == 3) && (choixtaillelatte == 1)) {
                  cafechoisi = "Latte (Petit)".toString
                  stockpoudrecafe -= 6
                  stocklait -= 0.120
                }
                if ((choixducafe == 3) && (choixtaillelatte == 2)) {
                  cafechoisi = "Latte (Moyen)".toString
                  stockpoudrecafe -= 8
                  stocklait -= 0.150
                }
                if ((choixducafe == 3) && (choixtaillelatte == 3)) {
                  cafechoisi = "Latte (Grand)".toString
                  stockpoudrecafe -= 12
                  stocklait -= 0.200
                }


                if (choixdusucre == 2) {
                  sucrechoisi = " Peu (5g)"
                  stocksucre -= 5
                  prixsucre = 0.10
                }
                if (choixdusucre == 3) {
                  sucrechoisi = " Moyen (10g)"
                  stocksucre -= 10
                  prixsucre = 0.20
                }
                if (choixdusucre == 4) {
                  sucrechoisi = " Beaucoup (15g)"
                  stocksucre -= 15
                  prixsucre = 0.30
                }


                if (choixnbdosedelait == 1) {
                  doselaitchoisie = " 50ml"
                  stocklait -= 0.050
                  prixlait = 0.05
                }
                if (choixnbdosedelait == 2) {
                  doselaitchoisie = " 100 ml"
                  stocklait -= 0.100
                  prixlait = 0.10
                }
                if (choixnbdosedelait == 3) {
                  doselaitchoisie = " 150ml"
                  stocklait -= 0.150
                  prixlait = 0.15
                }
                if (choixouiounonlait == 1) {
                  laitensupplement = " Oui "
                }
                if (choixouiounonlait == 2) {
                  laitensupplement = " Non "
                }
                if ((choixducafe == 3) && (choixtaillelatte == 1)) {
                  prixcafe = 2.70
                }
                if ((choixducafe == 3) && (choixtaillelatte == 2)) {
                  prixcafe = 3.20
                }
                if ((choixducafe == 3) && (choixtaillelatte == 3)) {
                  prixcafe = 3.70
                }
                var codeTWINT = ""
                for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génere un nouveau code pour ne pas avoir toujours le même code twint
                prixtotal = prixcafe + prixsucre + prixlait
                println("Boisson sélectionnée : " + cafechoisi)
                println("Niveau de sucre : " + sucrechoisi)
                println("Lait en supplément : " + laitensupplement)
                printf("Prix total : CHF %.2f CHF  + %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre,prixlait,prixtotal)
                println(" ")

                println("Veuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + codeTWINT)
                println("(En attente de paiement...)")
                println(" ")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Paiement confirmé")
                println("préparation de votre boisson...")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
                println("Votre Latte est prêt ! Bonne dégustation !")
                println(" ")
                println(" ")
              }
            } //fin café 3
          }
        } //fin mode 1
      }
    }


  }
}








