import io.StdIn._
import util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // ADMIN
    // Stock de départ
    var StockInitialCafe = 50.0
    var StockInitialSucre = 30.0
    var StockInitialLait = 0.5 // litre

    // Quantité restante après utilisation
    var PoudreCafe = StockInitialCafe
    var Sucre = StockInitialSucre
    var Lait = StockInitialLait

    // Code PIN
    val CodePin = "434343"

    // CLIENTS
    // Prix des boissons
    val Expresso = 2.0
    val Cappuccino = 2.5
    val LattePetit = 2.7
    val LatteMoyen = 3.2
    val LatteGrand = 3.7

    // Prix du sucre
    val SansSucre = 0.0
    val PeuSucre = 0.10
    val MoyenSucre = 0.20
    val BeaucoupSucre = 0.30

    // Prix Extra Lait
    val UneDose = 0.05

    // Quantités nécessaires d’ingrédients
    // Boissons
    val QuantiteExpressoCafe = 8.0 // 8 grammes
    val QuantiteCappuccinoCafe = 6.0
    val QuantiteCappuccinoLait = 0.1 // 0.1 litre = 100 ml
    val QuantiteLattePetitCafe = 6.0
    val QuantiteLattePetitLait = 0.12
    val QuantiteLatteMoyenCafe = 8.0
    val QuantiteLatteMoyenLait = 0.15
    val QuantiteLatteGrandCafe = 12.0
    val QuantiteLatteGrandLait = 0.2

    // Sucre
    val QuantitePeuSucre = 5.0 // 5 grammes
    val QuantiteMoyenSucre = 10.0
    val QuantiteBeaucoupSucre = 15.0

    // Extra Lait
    val QuantiteExtraLait = 0.05// 0.05 litre = 50 ml

    var SuiteProgramme = true
    var StocksSuffisants = true

    while (SuiteProgramme) {
      // Séléction du mode
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val Mode = readLine("> ").toInt

      if (Mode == 1) {
        // Mode Client
        var PrixBoisson = 0.0
        var ChoixBoisson = 0
        var NomBoisson = ""
        var TailleLatte = 0
        do {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          ChoixBoisson = readLine("> ").toInt

          if (ChoixBoisson == 1) {
            PrixBoisson = Expresso
            NomBoisson = "Expresso"
          } else if (ChoixBoisson == 2) {
            PrixBoisson = Cappuccino
            NomBoisson = "Cappuccino"
          } else if (ChoixBoisson == 3) {

            do {
              println("Choisissez la taille du Latte :")
              println("1) Petit - CHF 2.70")
              println("2) Moyen - CHF 3.20")
              println("3) Grand - CHF 3.70")
              TailleLatte = readLine("> ").toInt

              if (TailleLatte == 1) {
                PrixBoisson = LattePetit
                NomBoisson = "Latte (petit)"
              } else if (TailleLatte == 2) {
                PrixBoisson = LatteMoyen
                NomBoisson = "Latte (moyen)"
              } else if (TailleLatte == 3) {
                PrixBoisson = LatteGrand
                NomBoisson = "Latte (grand)"
              } else {
                  println("Taille invalide, veuillez réessayer.")
              }
            } while (TailleLatte != 1 && TailleLatte != 2 && TailleLatte != 3)
          } else {
            println("Choix invalide, veuillez réessayer.")
          }
        } while (ChoixBoisson != 1 && ChoixBoisson != 2 && ChoixBoisson != 3)

        //  Sucre
        var PrixSucre = 0.0
        var BesoinSucre = 0.0
        var ChoixSucre = 0
        var NomSucre = ""

        do {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          ChoixSucre = readLine("> ").toInt

          if (ChoixSucre == 1) {
            PrixSucre = SansSucre
            NomSucre = "Sans sucre"
            BesoinSucre = 0.0
          } else if (ChoixSucre == 2) {
            PrixSucre = PeuSucre
            NomSucre = "Peu (5g) "
            BesoinSucre = QuantitePeuSucre
          } else if (ChoixSucre == 3) {
            PrixSucre = MoyenSucre
            NomSucre = "Moyen (10g) "
            BesoinSucre = QuantiteMoyenSucre
      } else if (ChoixSucre == 4) {
        PrixSucre = BeaucoupSucre
        NomSucre = "Beaucoup (15g) "
        BesoinSucre = QuantiteBeaucoupSucre
      } else {
        println("Choix invalide, veuillez entrer 1,2,3 ou 4.")
      }
    } while (ChoixSucre != 1 && ChoixSucre != 2 && ChoixSucre != 3 && ChoixSucre != 4)

        // Extra lait
        var NombreDoses = 0
        var PrixExtraLait = 0.0
        var ChoixDose = 0
        var LaitSupp = ""

        // Extra lait si le client a choisi un cappuccino ou un latte
        if (ChoixBoisson == 2 || ChoixBoisson == 3) {
          do {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            ChoixDose = readLine("> ").toInt

            if (ChoixDose == 1) {
              do {
                println("Combien de doses de lait souhaitez-vous ajouter ? (1 à 3 doses)")
                NombreDoses = readLine("> ").toInt
                if (NombreDoses >= 1 && NombreDoses <= 3) {
                  PrixExtraLait = NombreDoses * UneDose
                  LaitSupp = " Oui, vous avez séléctionné " + NombreDoses+ " dose(s)"
                }
                if (NombreDoses < 1 || NombreDoses > 3) {
                  println("Veuillez entrer un nombre de doses valide (1 à 3).")
                }
              } while (NombreDoses != 1 && NombreDoses != 2 && NombreDoses != 3)
            } else if (ChoixDose == 2) {
              NombreDoses = 0
              PrixExtraLait = 0
              LaitSupp = "Non"
              println("Pas de supplément de lait ajouté.")
            } else {
              if (ChoixDose != 1 && ChoixDose != 2 && ChoixDose != 3 )
                println("Choix invalide, veuillez entrer 1,2 ou 3.")
            }
          } while (ChoixDose < 1 || ChoixDose > 2)
          // Affichage de se que le client à pris et des prix
          println("Boisson sélectionnée : " + NomBoisson)
          println("Niveau de sucre : " + NomSucre)
          println("Lait supplémentaire : " + LaitSupp)
        }
        // Vérification des besoins
        var BesoinCafe = 0.0
        var BesoinLait = 0.0

        if (ChoixBoisson == 1) {
          BesoinCafe = QuantiteExpressoCafe
          BesoinLait = 0.0
        } else if (ChoixBoisson == 2) {
          BesoinCafe = QuantiteCappuccinoCafe
          BesoinLait = QuantiteCappuccinoLait + (QuantiteExtraLait * NombreDoses)
        } else if (ChoixBoisson == 3) {
          if (TailleLatte == 1) {
            BesoinCafe = QuantiteLattePetitCafe
            BesoinLait = QuantiteLattePetitLait + (QuantiteExtraLait * NombreDoses)
          } else if (TailleLatte == 2) {
            BesoinCafe = QuantiteLatteMoyenCafe
            BesoinLait = QuantiteLatteMoyenLait + (QuantiteExtraLait * NombreDoses)
          } else if (TailleLatte == 3) {
            BesoinCafe = QuantiteLatteGrandCafe
            BesoinLait = QuantiteLatteGrandLait + (QuantiteExtraLait * NombreDoses)
          }
        }
        // Vérification des stocks avant de préparer la boisson
        if (PoudreCafe < BesoinCafe) {
          println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          StocksSuffisants = false
        } else if (Lait < BesoinLait) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
          StocksSuffisants = false
        } else if (Sucre < BesoinSucre) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          StocksSuffisants = false
        }
        if (StocksSuffisants) {
          // Préparer la boisson
          PoudreCafe -= BesoinCafe
          Lait -= BesoinLait
          Sucre -= BesoinSucre

          // Calculer le prix total
          val PrixTotal = PrixBoisson + PrixSucre + PrixExtraLait
          printf("Prix total:\n CHF %.2f\n + CHF %.2f\n + CHF %.2f\n = CHF %.2f\n", PrixBoisson, PrixSucre, PrixExtraLait, PrixTotal)

          println("Veuillez payer en utilisant Twint.")

          // Code aléatoire de 5 caractères
          val Caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          val LongueurCode = 5

          val PaymentCode = (1 to LongueurCode).map(_ => Caracteres(Random.nextInt(Caracteres.length))).mkString
          // Paiement
          println("Veuillez effectuer votre paiement...")
          println("Votre code de paiement est :" + PaymentCode)

          println("En attente de validation du paiement...")
          Thread.sleep(3000)

          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")

          Thread.sleep(5000)

          if (ChoixBoisson == 1) {
            println("Votre Expresso est prêt! Bonne dégustation !")
          } else if (ChoixBoisson == 2) {
            println("Votre Cappuccino est prêt! Bonne dégustation !")
          } else if (ChoixBoisson == 3) {
            println("Votre Latte (Petit) est prêt! Bonne dégustation !")
          } else if (ChoixBoisson == 4) {
            println("Votre Latte (Moyen) est prêt! Bonne dégustation !")
          } else if (ChoixBoisson == 5) {
            println("Votre Latte (Grand) est prêt! Bonne dégustation !")
          }
        }
      } else if (Mode == 2) {
        // Mode Admin : Demander le code PIN jusqu'à ce qu'il soit correct
        var pinAdminCorrect = false
        do {
          val pinAdmin = readLine("Veuillez entrer le code PIN : ")
          if (pinAdmin == CodePin) {
            pinAdminCorrect = true
            println("Accès autorisé")

            //Affichage des stocks actuels
            println("Niveau actuel du stock :\n" + "Poudre de café : " + PoudreCafe + " g\n" + "Sucre : " + Sucre + " g\n" + "Lait : " + Lait + " L")

            // Saisie des quantités à remettre dans la machine
            println("Entrez la quantité de café à ajouter (en grammes) :")
            PoudreCafe = readLine().toDouble

            println("Entrez la quantité de sucre à ajouter (en grammes) :")
            Sucre = readLine().toDouble

            println("Entrez la quantité de lait à ajouter (en litres) :")
            Lait = readLine().toDouble

            // Nouveaux niveaux de stocks
            println ("Réapprovisionnement des stocks ...")
            println("Ajout : \n" + "Poudre de café : " + PoudreCafe + " g\n" + "Sucre : " + Sucre + " g\n" + "Lait : " + Lait + " L")
            println("Niveaux de stocks mis à jour. \n Retour au menu principal ...")
          } else {
            println("Code PIN incorrect. Accès refusé.")
          }
        } while (! pinAdminCorrect)

      } else if (Mode == 3) {
        // Quitter
        println("Vous avez choisi de quitter. A bientôt.")
        SuiteProgramme = false // Sortie de la boucle principale
      } else {
        if (Mode != 1 || Mode != 2 || Mode != 3 )
          println("Mode invalide, veuillez entrer la valeur 1,2 ou 3.")
      }
      }
    }
  }