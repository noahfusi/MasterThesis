import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var running = true
    var poudre_a_cafe = 50.00
    var sucre_stock = 30.00
    var lait_stock = 0.50
    while (running == true) {
      var dose_cafe_a_soustraire = 0.00
      var dose_lait_a_soustraire = 0.00
      var dose_sucre_a_soustraire = 0.00
      var mode = readLine("\n \t Nospresso Café \nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt ; while (mode != 1 && mode != 2 && mode != 3) {
        mode = readLine("Veuillez sélectionner un mode correct : \n> ").toInt
      }
      if (mode == 1) {
        var paiement_reussi = false
        while(paiement_reussi == false) {
          val lettre_et_chiffres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var code_Twint = ""
          var response_sucre = " "
          var supplement_lait = 0
          var nb_dose_lait = 0
          var reponse_lait = "Non"
          var taille_latte = 0
          var reponse_boisson = " "
          var prix_boisson = 0.0
          for (i <- 1 to 5) {
            val index = (math.random * 36).toInt
            var j = 0
            var symbole = ' '
            for (character <- lettre_et_chiffres) {
              if (j == index) {
                symbole = character
              }; j += 1
            }; code_Twint += symbole
          }
          var boisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso - 2 CHF \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
          while (boisson != 1 && boisson != 2 && boisson != 3) {
            boisson = readLine("Veuillez choisir une boisson existante : \n> ").toInt
          }
          if(boisson == 3) {
            taille_latte = readLine("\nChoisissez la taille de votre Latte : 1) CHF 2.70 (Petit), 2) CHF 3.20 (Moyen), 3) CHF 3.70 (Grand) \n> ").toInt
          while (taille_latte != 1 && taille_latte != 2 && taille_latte != 3) {
            taille_latte = readLine("Veuillez choisir une taille adéquate : \n> ").toInt
          }
          }
          var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
          while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
            sucre = readLine("Veuillez choisir une dose de sucre existante : \n> ").toInt }
          // prix sucre = 0.10*(sucre - 1)
          if (sucre == 1) {
            response_sucre = "Sans sucre"
          } else if (sucre == 2) {
            response_sucre = "Peu"
            dose_sucre_a_soustraire = 5.00
          } else if (sucre == 3) {
            response_sucre = "Moyen"
            dose_sucre_a_soustraire = 10.00
          } else {
            response_sucre = "Beaucoup"
            dose_sucre_a_soustraire = 15.00
          }
          if (boisson != 1) {
            supplement_lait = readLine("\nVoulez-vous un supplément de lait ? \n1) Oui \n2) Non \n> ").toInt
            while (supplement_lait != 2 && supplement_lait != 1) {
              supplement_lait = readLine("Veuillez effectuer un choix adéquat : \n> ").toInt
            }
            if (supplement_lait == 1) {
              reponse_lait = "Oui"
              nb_dose_lait = readLine("\nCombien de doses ? \n> ").toInt
              while (nb_dose_lait > 3) {
                nb_dose_lait = readLine("Vous ne pouvez pas excéder trois doses de lait ! Veuillez choisir une quantité adéquate. \n> ").toInt
              }
            }
          }
          if (boisson == 1) {
            dose_cafe_a_soustraire = 8.00
            prix_boisson = 2.00
            reponse_boisson = "Expresso"
            println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : " + response_sucre)
          } else if (boisson == 2) {
            dose_cafe_a_soustraire = 6.00
            prix_boisson = 2.50
            reponse_boisson = "Cappuccino"
            dose_lait_a_soustraire = 0.10 + nb_dose_lait * 0.05
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
          } else {
            reponse_boisson = "Latte"
            if (taille_latte == 1) {
              dose_cafe_a_soustraire = 6.00
              prix_boisson = 2.70
              dose_lait_a_soustraire = 0.12 + nb_dose_lait * 0.05
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
            } else if (taille_latte == 2) {
              prix_boisson = 3.20
              dose_cafe_a_soustraire = 8.00
              dose_lait_a_soustraire = 0.15 + nb_dose_lait * 0.05
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
            } else {
              prix_boisson = 3.70
              dose_cafe_a_soustraire = 12.00
              dose_lait_a_soustraire = 0.20 + nb_dose_lait * 0.05
              println("\nBoisson sélectionnée : Latte (Grand) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
            }
          }
          if (poudre_a_cafe - dose_cafe_a_soustraire < 0) {
            print("\nErreur : Quantité de poudre de café insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
          } else if (sucre_stock - dose_sucre_a_soustraire < 0) {
            println("\nErreur : Quantité de sucre insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
          } else if (lait_stock - dose_lait_a_soustraire < 0) {
            println("\nErreur : Quantité de lait insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
          } else {
            val prix_total = prix_boisson + 0.10 * (sucre - 1) + nb_dose_lait * 0.05
            if (sucre != 1) {
              if (supplement_lait == 1) {
                printf("Prix total : %.2f  CHF + %.2f CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, 0.10*(sucre - 1), nb_dose_lait * 0.05, prix_total)
                Thread.sleep(3000)
                println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
                paiement_reussi = true
              } else {
                printf("Prix total : %.2f  CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, 0.10*(sucre - 1), prix_total)
                Thread.sleep(3000)
                println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
                paiement_reussi = true
              }
            } else {
              if (supplement_lait == 1) {
                printf("Prix total : %.2f  CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, nb_dose_lait * 0.05, prix_total)
                Thread.sleep(3000)
                println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
                paiement_reussi = true

              } else {
                println("Prix total : " + prix_boisson + " CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)")
                Thread.sleep(3000)
                println("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
                paiement_reussi = true
              }
            }
            poudre_a_cafe -= dose_cafe_a_soustraire
            sucre_stock -= dose_sucre_a_soustraire
            lait_stock -= dose_lait_a_soustraire
          }
        }
      }
      else if (mode == 2) {
        println("\nMode admin")
        var code_admin = readLine("Entrez le code PIN : \n> ").toInt
        while (code_admin != 434343) {
          code_admin = readLine("Veuillez entrer le PIN correct : \n> ").toInt
        } ; println("Accès autorisé")
        printf("\nStocks : \n \tPoudre de café : %.0fg \n \tLait : %.1fL \n \tSucre : %.0fg", poudre_a_cafe, lait_stock, sucre_stock )
        println("\n \nRéapprovisionnement des stocks... \nAjout : ")
        var ajout_poudre = readLine("\tPoudre de café : \n> ").toFloat
        while (ajout_poudre < 0) {
          ajout_poudre = readLine("L'ajout ne peut pas être négatif ! Veuillez entrer une quantité valide : \n> ").toFloat
        }
        var ajout_lait = readLine("\tLait : \n> ").toFloat
        while (ajout_lait < 0) {
          ajout_lait = readLine("L'ajout ne peut pas être négatif ! Veuillez entrer une quantité valide : \n> ").toFloat
        }
        var ajout_sucre = readLine("\tSucre : \n> ").toFloat
        while (ajout_sucre < 0) {
          ajout_sucre = readLine("L'ajout ne peut pas être négatif ! Veuillez entrer une quantité valide : \n> ").toFloat
        }
        poudre_a_cafe += ajout_poudre
        lait_stock += ajout_lait
        sucre_stock += ajout_sucre
        println("Niveau des stocks mis à jour. \nRetour vers le menu principal...")
      } else {
        running = false
      }
    }
  }
}