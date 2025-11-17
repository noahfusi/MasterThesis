import io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    val pinAttendu = "434343"
    var pinUtilisateur = ""
    val prixExpresso = 2.00 //en CHF
    val prixCappuccino = 2.50
    val prixLattePetit = 2.70
    val prixLatteMoyen = 3.20
    val prixLatteGrand = 3.70
    val prixSucrePeu = 0.10
    val prixSucreMoyen = 0.20
    val prixSucreBeaucoup = 0.30
    val prixDoseLait = 0.05
    var prixTotal = 0.0
    val stockCafeInitial = 50 //en g
    val doseCafeExpresso = 8
    val doseCafeCappucino = 6
    val doseCafeLattePetit = 6
    val doseCafeLatteMoyen = 8
    val doseCafeLatteGrand = 12
    var stockCafeActuel = stockCafeInitial
    var stockCafeNecessaire = 0
    var stockCafeAjoute = 0
    val stockLaitInitial = 0.5 //en litres
    val doseLaitCappucino = 0.1
    val doseLaitLattePetit = 0.12
    val doseLaitLatteMoyen = 0.15
    val doseLaitLatteGrand = 0.2
    val doseLaitSupp = 0.05
    var stockLaitActuel = stockLaitInitial
    var stockLaitNecessaire = 0.0
    var stockLaitAjoute = 0.0
    val stockSucreInitial = 30 //en g
    val doseSucrePeu = 5
    val doseSucreMoyen = 10
    val doseSucreBeaucoup = 15
    var stockSucreActuel = stockSucreInitial
    var stockSucreNecessaire = 0
    var stockSucreAjoute = 0
    var stockTotalNecessaire = true //par défaut le stockTotalNecessaire commence vrai, car le programme commence avec assez de stock pour effectuer toutes les possibilités de café
    var boissonNom = "boisson"
    var niveauSucre = "Sans sucre"
    var prixBoisson = 0.0
    var prixSucre = 0.0
    var prixSuppLait = 0.0
    //Choix du mode (+ def prix et stocks nécessaires) :
    var mode = 0
    var structureProgramme = true
    while (structureProgramme) { // sturctureProgramnme commence vraie donc à la fin des modes 1 et 2, ça recommence, mais comme elle devient faux pour le mode 3, ça quitte cette boucle et il se passe rien après donc ça quitte
      do {
        mode = readLine("\tNospresso Café\nVeuillez sélectionnez votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (mode != 1 && mode != 2 && mode != 3)
      //Actions selon modes :
      //1) Client
      if (mode == 1) {
        var boisson = 0
        var sucre = 0
        var lait = 0
        var nbDoseLait = 0
        do { // do while pour que tant que les stocks sont insuffisnat on rechoisi une boisson
          //boisson :
          // choix de la boisson :
          do {
            boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
            if (boisson != 1 && boisson != 2 && boisson != 3) {
              println("Veuillez svp entrer une valeur valide.")
            }
          } while (boisson != 1 && boisson != 2 && boisson != 3)
          //adaptation prix total :
          if (boisson == 1) {
            prixTotal = prixExpresso
            stockCafeNecessaire = doseCafeExpresso
            boissonNom = "Expresso"
            prixBoisson = prixExpresso
          }
          else if (boisson == 2) {
            prixTotal = prixCappuccino
            stockCafeNecessaire = doseCafeCappucino
            stockLaitNecessaire = doseLaitCappucino
            boissonNom = "Cappuccino"
            prixBoisson = prixCappuccino
          }
          else {
            var tailleLatte = 0
            boissonNom = "Latte"
            do {
              tailleLatte = readLine("Veuillez précisez la taille de votre latte : \n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
              if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
                println("Veuillez svp entrer une valeur valide.")
              }
            } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
            if (tailleLatte == 1) {
              prixTotal = prixLattePetit
              stockCafeNecessaire = doseCafeLattePetit
              stockLaitNecessaire = doseLaitLattePetit
              prixBoisson = prixLattePetit
            } else if (tailleLatte == 2) {
              prixTotal = prixLatteMoyen
              stockCafeNecessaire = doseCafeLatteMoyen
              stockLaitNecessaire = doseLaitLatteMoyen
              prixBoisson = prixLatteMoyen
            } else {
              prixTotal = prixLatteGrand
              stockCafeNecessaire = doseCafeLatteGrand
              stockLaitNecessaire = doseLaitLatteGrand
              prixBoisson = prixLatteGrand
            }
          }
          //personnalisation :
          //sucre
          do {
            sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
              println("Veuillez svp entrer une valeur valide.")
            }
          } while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
          //adaptation prix et des stocks :
          if (sucre == 1) {
            stockSucreNecessaire = 0
            niveauSucre = "Sans sucre"
            prixSucre = 0.0
          } else if (sucre == 2) {
            prixTotal += prixSucrePeu
            stockSucreNecessaire = doseSucrePeu
            niveauSucre = "Peu (5g)"
            prixSucre = prixSucrePeu
          } else if (sucre == 3) {
            prixTotal += prixSucreMoyen
            stockSucreNecessaire = doseSucreMoyen
            niveauSucre = "Moyen (10g)"
            prixSucre = prixSucreMoyen
          } else { // (sucre ==4)
            prixTotal += prixSucreBeaucoup
            stockSucreNecessaire = doseSucreBeaucoup
            niveauSucre = "Beaucoup (15g)"
            prixSucre = prixSucreBeaucoup
          }
          //lait :
          if (boisson == 2 || boisson == 3) {
            do {
              lait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
              if (lait != 1 && lait != 2) {
                println("Veuillez svp entrer une valeur valide.")
              }
            } while (lait != 1 && lait != 2)
            if (lait == 1) {
              do {
                nbDoseLait = readLine("Combien de dose ?\n> ").toInt
                if (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3) {
                  if (nbDoseLait == 0) {
                    println ("Veuillez entrez une valeur svp.")
                  } else if (nbDoseLait < 0) {
                    println("Le nombre de dose de lait ne peut pas être négatif.")
                  } else { // (nbDoseLait > 3)
                    println("Il n'est pas possible d'ajouter plus de 3 doses de lait en supplément.")
                  }
                }
              } while (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3)
              prixTotal += (nbDoseLait * prixDoseLait)
              stockLaitNecessaire += (nbDoseLait * doseLaitSupp)
              prixSuppLait = nbDoseLait * prixDoseLait
            }
          }
          //Gestion des stocks
          if ((stockCafeNecessaire > stockCafeActuel) || (stockSucreNecessaire > stockSucreActuel) || (stockLaitNecessaire > stockLaitActuel)) { //stockTotalNecessaire commence vrai mais si un des stocks nécessaire est insuffisant, alors le stockTotalNecessaire devient faux
            stockTotalNecessaire = false
            println() //ligne vide
            if (stockCafeNecessaire > stockCafeActuel) {
              print("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
              //!!! si pas assez de café pour le cappuccino -> proposer mode admin ???
            } else if (stockLaitNecessaire > stockLaitActuel) {
              print("Erreur : Quantité de plait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
            } else { // (stockSucreNecessaire > stockSucreActuel)
              print("Erreur : Quantité de sucre de insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une dose plus petite.\n")
            }
          }
        } while (!stockTotalNecessaire) //si stockTotalNecessaire est faux (insuffisant), alors on ne peut pas faire la boisson -> message erreur
        //Payement twint
        // résumé commande et affichange prix
        print("Boisson sélectionnée : " + boissonNom + "\nNiveau de sucre : " + niveauSucre + "\nLait supplémentaire : ")
        if (nbDoseLait == 0) {
          print("Non\n")
        } else {
          print(nbDoseLait + " dose(s)\n")
        }
        //+ nbDoseLait + " dose(s)")
        printf("Prix total : CHF %.2f", prixBoisson)
        if (sucre != 1) {
          printf(" + CHF %.2f", prixSucre)
        }
        if (lait == 1) {
          printf(" + CHF %.2f", prixSuppLait)
        }
        printf(" = CHF %.2f", prixTotal)
        println() //ligne vide
        //payement
        val codeTwint = Random.alphanumeric.take(5).mkString
        println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println() //ligne vide
        println("Merci ! Votre paiement a été accepté.")
        //Préparation de la boisson
        println("Préparation de votre boisson...\n[...]\nVotre " + boissonNom + " est prêt ! Bonne dégustation !\n")
        //Adaptation des stocks après commande payée et préparée
        stockCafeActuel -= stockCafeNecessaire
        stockLaitActuel -= stockLaitNecessaire
        stockSucreActuel -= stockSucreNecessaire

        //2) Admin
      } else if (mode ==2) {
        println("Mode Admin")
        do {
          pinUtilisateur = readLine("Entrez le code Pin : ****** ")
          if (pinUtilisateur != pinAttendu) {
            println("Le code Pin est erroné. Veuillez svp réessayer.")
          }
        } while (pinUtilisateur != pinAttendu)
        println("Accès autorisé.")
        println() //ligne vide
        //Stocks actuels :
        println("Stocks : \n\tPoudre de café : " + stockCafeActuel + "g\n\tLait : " + stockLaitActuel + "L\n\tSucre : " + stockSucreActuel + "g")
        println() //ligne vide
        //Adapation des stocks
        println("Réapprovisionnement des stocks... \nAjout :")
        stockCafeAjoute = readLine("\tPoudre de café (en g) : ").toInt
        stockLaitAjoute = readLine("\tLait (en L) : ").toInt
        stockSucreAjoute = readLine("\tSucre (en g) : ").toInt
        stockCafeActuel += stockCafeAjoute
        stockLaitActuel += stockLaitAjoute
        stockSucreActuel += stockSucreAjoute
        println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")

        //3) quitter
      } else {
        structureProgramme = false
      }
    }
  }
}