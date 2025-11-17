import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Variables pour les stocks et déclaration de pin admin.
    var cafe = 50 //en gramme
    var sucre = 30 //en gramme
    var lait = 0.5 //en gramme
    val pinAdmin = "434343"

    // Boucle principale qui permet de redémarrer après chaque opération
    var redemarrer = true

    do {
      //Boucle pour aaaurer les valeur valable des modes.
      var mode = 0
      while (mode != 1 && mode != 2 && mode != 3) {
        println()
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")

        mode = readLine("> ").toInt

        if (mode == 1) {
          var recommencer = 0 //Boucle pour le stock insuffisant des boissons.
          do {

            var cafeUtilise = 0
            var laitUtilise = 0.0
            var prixBase = 0.0
            var prixLait = 0.0
            var tailleLatte = 0

            //Boucle pour les trois boissons
            var boisson = 0

            while (boisson != 1 && boisson != 2 && boisson != 3) {
              println()
              println("Veuillez sélectionner votre boisson:")
              println("1) Expresso - CHF 2.00")
              println("2) Cappuccino - CHF 2.50")
              println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

              boisson = readLine("> ").toInt

              //Expresso
              if (boisson == 1) {
                cafeUtilise = 8
                prixBase = 2.00

                //Cappuccino
              } else if (boisson == 2) {
                cafeUtilise = 6
                laitUtilise = 0.1
                prixBase = 2.50

                //Latte
              } else if (boisson == 3) {

                //Boucle pour la taille de latte
                while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
                  println()
                  println("Veuillez choisir la taille:\n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70")
                  tailleLatte = readLine(">").toInt
                  if (tailleLatte == 1) {
                    cafeUtilise = 6
                    laitUtilise = 0.12
                    prixBase = 2.70
                  } else if (tailleLatte == 2) {
                    cafeUtilise = 8
                    laitUtilise = 0.15
                    prixBase = 3.20
                  } else if (tailleLatte == 3) {
                    cafeUtilise = 12
                    laitUtilise = 0.20
                    prixBase = 3.70
                  }
                }
              }
            }

            //Boucle pour le sucre ajouté
            var choixdusucre = 0
            var sucreUtilise = 0
            var prixSucre = 0.0

            while (choixdusucre != 1 && choixdusucre != 2 && choixdusucre != 3 && choixdusucre != 4) {
              println()
              println("Souhaitez-vous ajouter du sucre?")
              println("1) Sans sucre")
              println("2) Peu (5g) - CHF 0.10")
              println("3) Moyen (10g) - CHF 0.20")
              println("4) Beaucoup (15g) - CHF 0.30")

              choixdusucre = readLine(">").toInt

              if (choixdusucre == 2) {
                sucreUtilise = 5
                prixSucre = 0.10
              } else if (choixdusucre == 3) {
                sucreUtilise = 10
                prixSucre = 0.20
              } else if (choixdusucre == 4) {
                sucreUtilise = 15
                prixSucre = 0.30
              }
            }

            // Demander ajouter lait supplémentaire + boucle pour les deux questions
            var choixdulait = 0
            var laitsupplementaire = 0.0
            var dosedulait = 0

            if (boisson == 2 || boisson == 3) {

              while (choixdulait != 1 && choixdulait != 2) {
                println()
                println("Voulez-vous ajouter du lait en supplément?")
                println("(Disponible uniquement Pour Cappuccino et Latte)")
                println("1) Oui")
                println("2) Non")
                choixdulait = readLine(">").toInt
                if (choixdulait == 1) {
                  while (dosedulait != 1 && dosedulait != 2 && dosedulait != 3) {
                    println()
                    println("Combien de dose?")
                    println("1) 1 dose (50ml) - CHF 0.05")
                    println("2) 2 doses (100ml) - CHF 0.10")
                    println("3) 3 doses (150ml) - CHF 0.15")

                    dosedulait = readLine(">").toInt

                    if (dosedulait == 1) {
                      laitsupplementaire = 0.05
                      prixLait = 0.05
                    } else if (dosedulait == 2) {
                      laitsupplementaire = 0.10
                      prixLait = 0.10
                    } else if (dosedulait == 3) {
                      laitsupplementaire = 0.15
                      prixLait = 0.15
                    }
                  }
                }
              }
            }

            println()
            if (boisson == 1) {
              println("Boisson sélectionnée: Expresso")
            } else if (boisson == 2) {
              println("Boisson sélectionnée: Cappuccino")
            } else if (boisson == 3) {
              if (tailleLatte == 1) {
                println("Boisson séléctionnée: Latte (Petit)")
              } else if (tailleLatte == 2) {
                println("Boisson séléctionnée: Latte (Moyen)")
              } else if (tailleLatte == 3) {
                println("Boisson séléctionnée: Latte (Grand)")
              }
            }

            if (choixdusucre == 1) {
              println("Niveau du sucre: Sans sucre")
            } else if (choixdusucre == 2) {
              println("Niveau du sucre: Peu (5g)")
            } else if (choixdusucre == 3) {
              println("Niveau du sucre: Moyen (10g)")
            } else if (choixdusucre == 4) {
              println("Niveau du sucre: Beaucoup (15g)")
            }

            if (choixdulait == 2) {
              println("Lait en supplémentaire: Non")
            } else if (dosedulait == 1) {
              println("Lait en supplémentaire: 1 dose (50ml)")
            } else if (dosedulait == 2) {
              println("Lait en supplémentaire: 2 doses (100ml)")
            } else if (dosedulait == 3) {
              println("Lait en supplémentaire: 3 doses (150ml)")
            }

            //Le prix final
            val prixFinal = prixBase + prixSucre + prixLait

            if (cafe >= cafeUtilise && sucre >= sucreUtilise && lait >= (laitUtilise + laitsupplementaire)) {
              cafe -= cafeUtilise
              sucre -= sucreUtilise
              lait -= (laitUtilise + laitsupplementaire)

              //Le paiement
              if (boisson == 1) {
                if (choixdusucre == 2 || choixdusucre == 3 || choixdusucre == 4) {
                  printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixSucre, prixFinal)
                } else if (choixdusucre == 0 || choixdusucre == 1) {
                  printf("Prix total: CHF %.2f\n", prixFinal)
                }
              } else if (boisson == 2 || boisson == 3) {
                if ((choixdusucre == 0 || choixdusucre == 1) && (dosedulait == 1 || dosedulait == 2 || dosedulait == 3)) {
                  printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixLait, prixFinal)
                } else if ((dosedulait == 0 || choixdulait == 2) && (choixdusucre == 2 || choixdusucre == 3 || choixdusucre == 4)) {
                  printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixSucre, prixFinal)
                } else if ((choixdusucre == 0 || choixdusucre == 1) && (dosedulait == 0 || choixdulait == 2)) {
                  printf("Prix total: CHF %.2f\n", prixFinal)
                } else {
                  printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixSucre, prixLait, prixFinal)
                }
              }

              println()
              println("Veuillez payer en utilisant Twint.")

              // Caractères pour le code de paiement
              val caracteres = ('A' to 'Z') ++ ('0' to '9')
              var codePaiement = ""
              for (_ <- 1 to 5) {
                codePaiement += caracteres(Random.nextInt(caracteres.length))
              }
              println("Votre code de paiement est: " + codePaiement)
              println("(En attente de paiement...)")

              // Attente  3 secondes pour le paiement
              Thread.sleep(3000)

              println()
              println("Paiement confirmé.")
              println("Préparation de votre boisson...")

              // Attente 5 secondes pour la préparation du boisson
              Thread.sleep(5000)


              if (boisson == 1) {
                println("Votre Expresso est prête! Bonne dégustation!")
              } else if (boisson == 2) {
                println("Votre Cappuccino est prête! Bonne dégustation!")
              } else if (boisson == 3) {
                println("Votre Latte est prête! Bonne dégustation!")
              }
              recommencer = 2

            } else {
              println()
              if (cafe < cafeUtilise && sucre >= sucreUtilise && lait >= (laitUtilise + laitsupplementaire)) { //si café en stock est moins que café à utilisé, et les deux suffisants.
                println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (cafe >= cafeUtilise && sucre < sucreUtilise && lait >= (laitUtilise + laitsupplementaire)) { //si sucre en stock est moins que sucre à utilisé, et les deux suffisants.
                println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (cafe >= cafeUtilise && sucre >= sucreUtilise && lait < (laitUtilise + laitsupplementaire)) { //si lait en stock est moins que lait à utilisé. et les deux suffisants.
                println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (cafe < cafeUtilise && sucre < sucreUtilise && lait >= (laitUtilise + laitsupplementaire)) { //si café et sucre en stock est moins que ceux qui sont à utilisé, et le lait suffisant.
                println("Erreur: Quantité de poudre de café et de sucre insuffisantes pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (cafe < cafeUtilise && sucre >= sucreUtilise && lait < (laitUtilise + laitsupplementaire)) {
                println("Erreur: Quantité de poudre de café et de lait insuffisantes pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (cafe >= cafeUtilise && sucre < sucreUtilise && lait < (laitUtilise + laitsupplementaire)) {
                println("Erreur: Quantité de sucre et de lait insuffisantes pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (cafe < cafeUtilise && sucre < sucreUtilise && lait < (laitUtilise + laitsupplementaire)) {
                println("Erreur: Quantité de poudre de café, de sucre et de lait insuffisantes pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              }
              recommencer = 1
            }
          } while (recommencer == 1) //Si stock insuffisant, on va retourner en mode clients, donc la séléction des boissons, si l'utilisateur choisit un autre boisson qui a le stock suffisant, alors ce boucle peut être fini, sinon ce serait un boucle infini.

        } else if (mode == 2) {
          // Mode Admin
          println()
          println("Mode Admin")
          println("Entrez le code PIN :")

          val pin = readLine("> ")

          if (pin == pinAdmin) {
            println("Accès autorisé.")
            println()
            println("Stocks actuels :")
            println("Poudre de café: " + cafe + "g")
            //println("Lait: " + lait + "L"), je vais mettre un printf pour avoir seulement 3 chiffres significative après la virgule.
            printf("Lait: %.2fL\n", lait)
            println("Sucre: " + sucre + "g")


            println()
            println("Réapprovisionnement des stocks...")
            println("Ajouter de la poudre de café (g) :")
            cafe += readLine("> ").toInt
            println("Ajouter du lait (L) :")
            lait += readLine("> ").toDouble
            println("Ajouter du sucre (g) :")
            sucre += readLine("> ").toInt

            println()
            println("Stocks actuels après réapprovisionnement:")
            println("Poudre de café: " + cafe + "g")
            //println("Lait:" + lait + "L")
            printf("Lait: %.2fL\n", lait)
            println("Sucre: " + sucre + "g")

            println("Niveaux de stock mis à jour.")
          } else {
            println("Code PIN incorrect.")
          }

        } else if (mode == 3) {
          println()
          println("Quitter le programme.")
          redemarrer = false // Met fin à la boucle si l'utilisateur veut quitter
        } else {
          println()
          println("Option invalide. Veuillez réessayer.")
        }
      }

      // Demander à l'utilisateur s'il souhaite recommencer
      if (redemarrer) {
        var reponse = 0
        do {
          println()
          println("Souhaitez-vous retour au menu principal pour recommencer?")
          println("1) Oui")
          println("2) Non (quitter)")
          reponse = readLine(">").toInt // si la réponse est '1', on va relancer en menu principal.
        } while (reponse != 1 && reponse != 2)
        redemarrer = reponse == 1
      }
    } while (redemarrer)

    println()
    println("Merci d'avoir utilisé Nospresso Café. À bientôt!")
  }
}
