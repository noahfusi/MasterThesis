import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    // Prix
    val expressoPrix = 2.00
    val cappuccinoPrix = 2.50
    val lattePetitPrix = 2.70
    val latteMoyenPrix = 3.20
    val latteGrandPrix = 3.70
    val scrPeuPrix = 0.10
    val scrMoyPrix = 0.20
    val scrBcpPrix = 0.30

    // Stock
    var poudreCafe = 50
    var sucre = 30
    var lait = 500.00

    // Consommation des ingrédients par type de boisson
    val expressoPdr = 8
    val cappuccinoPdr = 6
    val lattePetitPdr = 6
    val latteMoyPdr = 8
    val latteGrandPdr = 12

    val cappuccinoLait = 100
    val lattePetitLait = 120
    val latteMoyLait = 150
    val latteGrandLait = 200

    val sucrePeu = 5
    val sucreMoy = 10
    val sucreBcp = 15

    val uneDoseQtn = 50
    val uneDoseLaitPrix = 0.05

    while(true) {
      var choixMode = 0
      while (choixMode < 1 || choixMode > 3) {
        println("        Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        choixMode = readLine().toInt
      }
      var latteTailleChoix = 0

      if (choixMode == 1) {
        var boissonChoix = 0
        while (boissonChoix < 1 || boissonChoix > 3) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          boissonChoix = readLine().toInt
        }
        var prixTot = 0.0
        var boissonChoisiPrix = 0.0
        var poudreEnCours = 0
        var laitEnCours = 0
        var laitEnCoursPrix = 0.0
        if (boissonChoix == 1) {
          boissonChoisiPrix = expressoPrix
          prixTot += expressoPrix
          poudreEnCours += expressoPdr
        } else if (boissonChoix == 2) {
          boissonChoisiPrix = cappuccinoPrix
          prixTot += cappuccinoPrix
          poudreEnCours += cappuccinoPdr
          laitEnCours += cappuccinoLait
        } else if (boissonChoix == 3) {
          while (latteTailleChoix < 1 || latteTailleChoix > 3) {
            println("Veuillez sélectionner la taille :")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            print("> ")
            latteTailleChoix = readLine().toInt
          }
          if (latteTailleChoix == 1) {
            boissonChoisiPrix = lattePetitPrix
            prixTot += lattePetitPrix
            poudreEnCours += lattePetitPdr
            laitEnCours += lattePetitLait
          } else if (latteTailleChoix == 2) {
            boissonChoisiPrix = latteMoyenPrix
            prixTot += latteMoyenPrix
            poudreEnCours += latteMoyPdr
            laitEnCours += latteMoyLait
          } else if (latteTailleChoix == 3) {
            boissonChoisiPrix = latteGrandPrix
            prixTot += latteGrandPrix
            poudreEnCours += latteGrandPdr
            laitEnCours = latteGrandLait
          }
        }

        var sucreChoix = 0
        var sucreEnCours = 0
        var sucreEnCoursPrix = 0.0
        while (sucreChoix < 1 || sucreChoix > 4) {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          sucreChoix = readLine().toInt
        }
        if (sucreChoix == 2) {
          sucreEnCours += sucrePeu
          sucreEnCoursPrix = scrPeuPrix
        } else if (sucreChoix == 3) {
          sucreEnCours = sucreMoy
          sucreEnCoursPrix = scrMoyPrix
        } else if (sucreChoix == 4) {
          sucreEnCours += sucreBcp
          sucreEnCoursPrix = scrBcpPrix
        }
        var laitChoix = 0
        if (boissonChoix != 1) {
          while (laitChoix < 1 || laitChoix > 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            print("> ")
            laitChoix = readLine().toInt
          }
          var laitDoseQtnChoisi = 0
          if (laitChoix == 1) {
            while (laitDoseQtnChoisi < 1 || laitDoseQtnChoisi > 3) {
              println("Combien de dose ?")
              print("> ")
              laitDoseQtnChoisi = readLine().toInt
            }
            laitEnCours += laitDoseQtnChoisi * uneDoseQtn
            laitEnCoursPrix = uneDoseLaitPrix * laitDoseQtnChoisi
          }
        }

        var boissonNom = ""
        if (boissonChoix == 1) {
          boissonNom = "Expresso"
        } else if (boissonChoix == 2) {
          boissonNom = "Cappuccino"
        } else {
          boissonNom = "Latte"
        }
        if (boissonChoix == 1 || boissonChoix == 2) {
          println("Boisson sélectionnée : " + boissonNom)
        } else {
          if (latteTailleChoix == 1) {
            println("Boisson sélectionnée : " + boissonNom + " (Petit)")
          } else if (latteTailleChoix == 2) {
            println("Boisson sélectionnée : " + boissonNom + " (Moyen)")
          } else {
            println("Boisson sélectionnée : " + boissonNom + " (Grand)")
          }
        }

        if (sucreChoix == 1) {
          println("Niveau de sucre : Sans sucre")
        } else if (sucreChoix == 2) {
          println("Niveau de sucre : Peu (5g)")
        } else if (sucreChoix == 3) {
          println("Niveau de sucre : Moyen (10g)")
        } else {
          println("Niveau de sucre : Beaucoup (15g)")
        }

        if(laitChoix == 1) {
          println("Lait supplémentaire: Oui\n")
        } else {
          println("Lait supplémentaire: Non\n")
        }

        var isErreur = false
        if (laitEnCours > lait) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          isErreur = true
        }
        else if (sucreEnCours > sucre) {
          println("Erreur : Quantité de sucre insuffisant pour préparer la boisson sélectionnée.")
          isErreur = true
        }
        else if (poudreEnCours > poudreCafe) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          isErreur = true
        } else {
          lait -= laitEnCours
          sucre -= sucreEnCours
          poudreCafe -= poudreEnCours
        }

        if (isErreur) {
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
          isErreur = false
        } else {

          printf("Prix total : CHF %.2f", boissonChoisiPrix)
          if (sucreChoix > 1) {
            printf(" + CHF %.2f", sucreEnCoursPrix)
          }
          if (laitChoix == 1) {
            printf(" + CHF %.2f", laitEnCoursPrix)
          }
          prixTot = laitEnCoursPrix + sucreEnCoursPrix + boissonChoisiPrix
          printf(" = CHF %.2f \n\n", prixTot)


          println("Veuillez payer en utilisant Twint.")
          val random = Random.alphanumeric.take(5).mkString("")
          println("Votre code de paiement est : " + random)
          println("(En attente de validation du paiement...)\n")
          Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
          println("Merci ! Votre paiement a été accepté.\n")

          println("Préparation de la boisson...")
          println("[...]")
          Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
          println("Votre " + boissonNom + " est prêt ! Bonne dégustation !\n")
        }
      } else if (choixMode == 2) {
        println("Mode Admin")
        print("Entrez le code PIN : ")
        val pin = readLine().toInt
        if (pin == 434343) {
          println("Accès autorisé.\n")
          println("Stock:")
          println("    Poudre de café: " + poudreCafe + "g")
          println("    Lait         : " + lait + "L")
          println("    Sucre        : " + sucre + "g\n")

          println("Réapprovisionnement des stocks...")
          println("Ajout :")
          print("    Poudre de café: ")
          val ajoutPdr = readLine().toInt
          print("    Lait          : ")
          val ajoutLait = readLine().toDouble
          print("    Sucre         : ")
          val ajoutSucre = readLine().toInt
          poudreCafe += ajoutPdr
          lait += ajoutLait
          sucre += ajoutSucre
          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
        }
      } else if (choixMode == 3) {
        System.exit(0)
      }
    }
  }
}

