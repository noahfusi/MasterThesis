import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var prog = 1
    var poudre = 50
    var sucre = 30
    var lait = 0.500.toDouble
    val code = 434343


    while (prog == 1) {
      var prixsucre = 0.toDouble
      var choixlait = 0
      var nbdose = 0
      var prix = 0.toDouble
      var prixboisson = 0.toDouble
      var boissonnom = ""
      println("\n        Nospresso Café        ")
      println("Veuillez selectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      var mode = readInt()

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Entrée invalide, veuillez saisir '1', '2', ou '3'")
        println("        Nospresso Café        ")
        println("Veuillez selectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        mode = readInt()
      }
      while (mode == 1) {
        val caractères =  "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        var twint = ""
        for (i <- 1 to 5) {
          val choixcaractere = (Math.random() * 36).toInt
          twint += caractères(choixcaractere)
        }
        println("Mode Client")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        var boisson = readInt()
        while (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Entrée invalide, veuillez saisir '1', '2', ou '3'")
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          boisson = readInt()
        }
        if (boisson == 1) {
          boissonnom = "Expresso"
          prixboisson = 2.00
          println("Souhaitez-vous ajouter du  sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucreclient = readInt()
          while (sucreclient != 1 && sucreclient != 2 && sucreclient != 3 && sucreclient != 4) {
            print("Entrée invalide, veuillez saisir '1', '2', '3' ou '4'")
            print("Souhaitez-vous ajouter du du sucre ?")
            print("1) Sans sucre")
            print("2) Peu (5g) - CHF 0.10")
            print("3) Moyen (10g) - CHF 0.20")
            print("4) Beaucoup (15g) - CHF 0.30")
            print("> ")
            sucreclient = readInt()
          }
          if (sucreclient == 1) {
            println("Boisson sélectionnée : Expresso")
            println("Niveau de Sucre : Sans sucre")

            if (poudre < 8) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else {
              prix = prixboisson + prixsucre
              println("Prix total : CHF " + prixboisson)
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 8
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
          else if (sucreclient == 2) {
            prixsucre = 0.10
            println("Boisson sélectionnée : Expresso")
            println("Niveau de Sucre : Peu (5g)")
            if (poudre < 8) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (sucre < 5) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")
            }
            else {
              prix = prixboisson + prixsucre
              printf("Prix total : CHF " + f"$prixboisson%.2f +" + " CHF " + f"$prixsucre%.2f" + " = " + f"$prix%.2f" + "CHF\n")
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 8
              sucre -= 5
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }

          }
          else if (sucreclient == 3) {
            prixsucre = 0.20
            println("Boisson sélectionnée : Expresso")
            println("Niveau de Sucre : Moyen (10g) ")
            if (poudre < 8) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (sucre < 10) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")
            }
            else {
              prix = prixboisson + prixsucre
              printf("Prix total : CHF " + f"$prixboisson%.2f" + "+" + " CHF " + f"$prixsucre%.2f" + " = " + f"$prix%.2f" + "CHF\n")
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 8
              sucre -= 10
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
          else if (sucreclient == 4) {
            prixsucre = 0.30
            println("Boisson sélectionnée : Expresso")
            println("Niveau de Sucre : Beaucoup (15g)")
            if (poudre < 8) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (sucre < 15) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")
            }
            else {
              prix = prixboisson + prixsucre
              printf("Prix total : CHF " + f"$prixboisson%.2f +" + " CHF " + f"$prixsucre%.2f" + " = " + f"$prix%.2f" + "CHF\n")
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 8
              sucre -= 15
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0


            }


          }
        }

        else if (boisson == 2) {
          boissonnom = "Cappuccino"
          prixboisson = 2.50
          println("Souhaitez-vous ajouter du  sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucreclient = readInt()
          while (sucreclient != 1 && sucreclient != 2 && sucreclient != 3 && sucreclient != 4) {
            print("Entrée invalide, veuillez saisir '1', '2', '3' ou '4'")
            print("Souhaitez-vous ajouter du du sucre ?")
            print("1) Sans sucre")
            print("2) Peu (5g) - CHF 0.10")
            print("3) Moyen (10g) - CHF 0.20")
            print("4) Beaucoup (15g) - CHF 0.30")
            print("> ")
            sucreclient = readInt()
          }
          println("Souhaitez-vous ajouter du  lait ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          choixlait = readInt()
          while (choixlait != 1 && choixlait != 2) {
            print("Entrée invalide, veuillez saisir '1'ou '2'")
            println("Souhaitez-vous ajouter du  lait ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            choixlait = readInt()
          }
          if (choixlait == 2) {
            nbdose = 0
          }

          if (choixlait == 1) {
            println("Combien de dose souhaitez vous ?")
            println("1) 1 dose")
            println("2) 2 doses")
            println("3) 3 doses")
            print("> ")
            nbdose = readInt()
          }
          while (nbdose != 1 && nbdose != 2 && nbdose != 3 && nbdose != 0) {
            println("Entrée invalide, veuillez saisir '1','2' ou '3'")
            println("Combien de doses souhaiter vous ?")
            println("1) 1 dose")
            println("2) 2 doses")
            println("3) 3 doses")
            print("> ")
            nbdose = readInt()
          }
          if (choixlait == 2) {
            nbdose = 0
          }


          var prixdose = nbdose * 0.05
          var besoinlait = 0.1 + 0.05 * nbdose


          if (sucreclient == 1) {
            println("Boisson sélectionnée : Cappuccino")
            println("Niveau de Sucre : Sans sucre")
            if (nbdose == 0) {
              println("Lait suppĺémentaire: Non")
            }
            else {
              println("Lait supplémentire : Oui, " + (nbdose) + " doses")
            }


            if (poudre < 6) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
            else {
              prix = prixboisson + prixsucre + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f")

              }
              else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixdose%.2f" + " = CHF " + f"$prix%.2f")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              lait -= besoinlait
              poudre -= 6
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
          if (sucreclient == 2) {
            println("Boisson sélectionnée : Cappuccino")
            println("Niveau de Sucre : Peu (5g) ")
            if (nbdose == 0) {
              println("Lait suppĺémentaire: Non")
            }
            else {
              println("Lait supplémentire : Oui, " + (nbdose) + " doses")
            }

            if (poudre < 6) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour pre ́parer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
            else if (sucre < 5) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")

            }

            else {
              prixsucre = 0.10
              prix = prixboisson + prixsucre + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f")

              }
              else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " +  CHF " + f"$prixdose%.2f" + " =  CHF " + f"$prix%.2f")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 6
              sucre -= 5
              lait -= besoinlait
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
          if (sucreclient == 3) {
            println("Boisson sélectionnée : Cappuccino")
            println("Niveau de Sucre : Moyen (10g) ")
            if (nbdose == 0) {
              println("Lait suppĺémentaire: Non")
            }
            else {
              println("Lait supplémentire : Oui, " + (nbdose) + " doses")
            }

            if (poudre < 6) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
            else if (sucre < 10) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")

            }

            else {
              prixsucre = 0.20
              prix = prixboisson + prixsucre + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f")

              }
              else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " +  CHF " + f"$prixdose%.2f" + " =  CHF " + f"$prix%.2f")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 6
              sucre -= 10
              lait -= besoinlait
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
          if (sucreclient == 4) {
            println("Boisson sélectionnée : Cappuccino")
            println("Niveau de Sucre : Beaucoup (15g) ")
            if (nbdose == 0) {
              println("Lait suppĺémentaire: Non")
            }
            else {
              println("Lait supplémentire : Oui, " + (nbdose) + " doses")
            }

            if (poudre < 6) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
            else if (sucre < 15) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une plus petite dose de sucre ou vérifier les stocks en mode Admin.")

            }

            else {
              prix = prixboisson + prixsucre + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f")

              }
              else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " +  CHF " + f"$prixdose%.2f" + " =  CHF " + f"$prix%.2f")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Pré́paration de votre boisson...")
              poudre -= 6
              sucre -= 15
              lait -= besoinlait
              prixsucre = 0.30
              Thread.sleep(3000)
              println("Votre " + boissonnom + " est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
        }
        else if (boisson == 3) {
          boissonnom = "Latte"
          println("Veuillez sélectionner la taille :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          var taille = readInt()
          while (taille != 1 && taille != 2 && taille != 3) {
            println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
            println("Veuillez sélectionner la taille :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            print("> ")
            taille = readInt()
          }
          var besoinpoudre = 0
          if (taille == 1) {
            besoinpoudre = 6
          } else if (taille == 2) {
            besoinpoudre = 8
          } else if (taille == 3) {
            besoinpoudre = 12
          }

          if (taille == 1) {
            prixboisson = 2.70
          } else if (taille == 2) {
            prixboisson = 3.20
          } else if (taille == 3) {
            prixboisson = 3.70
          }

          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucreclient = readInt()

          while (sucreclient != 1 && sucreclient != 2 && sucreclient != 3 && sucreclient != 4) {
            println("Entrée invalide, veuillez saisir '1', '2', '3' ou '4'")
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            print("> ")
            sucreclient = readInt()
          }

          if (sucreclient == 2) {
            prixsucre = 0.10
          } else if (sucreclient == 3) {
            prixsucre = 0.20
          } else if (sucreclient == 4) {
            prixsucre = 0.30
          }

          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          choixlait = readInt()

          while (choixlait != 1 && choixlait != 2) {
            println("Entrée invalide, veuillez saisir '1' ou '2'")
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            choixlait = readInt()
          }


          if (choixlait == 1) {
            println("Combien de doses souhaitez-vous ?")
            println("1) 1 dose")
            println("2) 2 doses")
            println("3) 3 doses")
            print("> ")
            nbdose = readInt()
            while (nbdose != 1 && nbdose != 2 && nbdose != 3) {
              println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
              println("Combien de doses souhaitez-vous ?")
              println("1) 1 dose")
              println("2) 2 doses")
              println("3) 3 doses")
              print("> ")
              nbdose = readInt()
            }
          } else {
            nbdose = 0
          }
          var besoinlait = 0.0
          if (taille == 1) {
            besoinlait = 0.12
          } else if (taille == 2) {
            besoinlait = 0.15
          } else if (taille == 3) {
            besoinlait = 0.2
          }
          besoinlait += nbdose * 0.05

          var prixdose = nbdose * 0.05


          var besoinsucre = 0
          if (sucreclient == 1) {
            besoinsucre = 0

          } else if (sucreclient == 2) {
            besoinsucre = 5

          } else if (sucreclient == 3) {
            besoinsucre = 10

          } else if (sucreclient == 4) {
            besoinsucre = 15
          }
          if (sucreclient == 1) {
            println("Boisson sélectionnée : Latte")
            println("Niveau de Sucre : Sans sucre")
            if (nbdose == 0) {
              println("Lait supplémentaire : Non")
            } else {
              println("Lait supplémentaire : Oui, " + nbdose + " dose(s)")
            }

            if (poudre < besoinpoudre) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            } else {
              printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixdose%.2f" + " = CHF " + f"$prix%.2f\n")
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Préparation de votre boisson...")
              poudre -= besoinpoudre
              lait -= besoinlait
              prix = prixboisson
              Thread.sleep(3000)
              println("Votre Latte est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          } else if (sucreclient == 2) {
            println("Boisson sélectionnée : Latte")
            println("Niveau de Sucre : Peu (5g)")
            if (nbdose == 0) {
              println("Lait supplémentaire : Non")
            } else {
              println("Lait supplémentaire : Oui, " + nbdose + " dose(s)")
            }

            if (poudre < besoinpoudre) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            } else if (sucre < 5) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir un niveau de sucre plus bas ou vérifier les stocks en mode Admin.")
            } else {
              prix = prixboisson + 0.10 + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f\n")
              } else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " + CHF " + f"$prixdose%.2f" + " = CHF " + f"$prix%.2f\n")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Préparation de votre boisson...")
              poudre -= besoinpoudre
              sucre -= 5
              lait -= besoinlait
              Thread.sleep(3000)
              println("Votre Latte est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          } else if (sucreclient == 3) {
            println("Boisson sélectionnée : Latte")
            println("Niveau de Sucre : Moyen (10g)")
            if (nbdose == 0) {
              println("Lait supplémentaire : Non")
            } else {
              println("Lait supplémentaire : Oui, " + nbdose + " dose(s)")
            }

            if (poudre < besoinpoudre) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            } else if (sucre < 10) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir un niveau de sucre plus bas ou vérifier les stocks en mode Admin.")
            } else {
              prix = prixboisson + 0.20 + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f\n")
              } else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " + CHF " + f"$prixdose%.2f" + " = CHF " + f"$prix%.2f\n")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Préparation de votre boisson...")
              poudre -= besoinpoudre
              sucre -= 10
              lait -= besoinlait
              Thread.sleep(3000)
              println("Votre Latte est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          } else if (sucreclient == 4) {
            println("Boisson sélectionnée : Latte")
            println("Niveau de Sucre : Beaucoup (15g)")
            if (nbdose == 0) {
              println("Lait supplémentaire : Non")
            } else {
              println("Lait supplémentaire : Oui, " + nbdose + " dose(s)")
            }

            if (poudre < besoinpoudre) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (besoinlait > lait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            } else if (sucre < 15) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir un niveau de sucre plus bas ou vérifier les stocks en mode Admin.")
            } else {
              prix = prixboisson + 0.30 + prixdose
              if (prixdose == 0) {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " = CHF " + f"$prix%.2f\n")
              } else {
                printf("Prix total : CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " + CHF " + f"$prixdose%.2f" + " = CHF " + f"$prix%.2f\n")
              }
              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + twint)
              println("En attente de paiement...\n")
              Thread.sleep(3000)
              println("Paiement confirmé.")
              println("Préparation de votre boisson...")
              poudre -= besoinpoudre
              sucre -= 15
              lait -= besoinlait
              Thread.sleep(3000)
              println("Votre Latte est prêt ! Bonne dégustation !\n\n\n\n\n")
              Thread.sleep(3000)
              mode = 0

            }
          }
        }
      }

      if (mode == 2) {
        println("Mode Admin")
        println("Entrez le code PIN : ******")
        print("> ")
        var mdpsaisie = readInt()

        while (mdpsaisie != code) {
          println("Accès refusé : Code PIN incorrect.")
          println("Veuillez réessayer.")
          print("> ")
          mdpsaisie = readInt()
        }

        println("Accès autorisé.")
        println("\nStocks :")
        println("Poudre de café : " + poudre + "g")
        printf("Lait : " + f"$lait%.3f" + "L")
        println("\nSucre : " + sucre + "g")

        println("\nRéapprovisionnement des stocks...")
        println("Ajout :\n")

        println("Ajout de Poudre de café : ")
        print("> ")
        var ajoutpoudre = readInt()
        while (ajoutpoudre < 0) {
          println("Veuillez entrer une valeur correcte.")
          print("> ")
          ajoutpoudre = readInt()
        }
        poudre += ajoutpoudre

        println("Lait : ")
        print("> ")
        var ajoutlait = readDouble()
        while (ajoutlait < 0) {
          println("Erreur : La quantité ne peut pas être négative.")
          println("Veuillez entrer une valeur correcte.")
          print("> ")
          ajoutlait = readDouble()
        }
        lait += ajoutlait

        println("Sucre : ")
        print("> ")
        var ajoutsucre = readInt()
        while (ajoutsucre < 0) {
          println("Erreur : La quantité ne peut pas être négative.")
          println("Veuillez entrer une valeur correcte.")
          print("> ")
          ajoutsucre = readInt()
        }
        sucre += ajoutsucre

        println("\nNiveaux de stock mis à jour.")
        println ("Addition :")
        println("Ajout Sucre : " + ajoutsucre )
        println("Ajout Lait : " + ajoutlait)
        println("Ajout Poudre : " + ajoutpoudre)
        println("Retour au menu principal...")
      }
      else if (mode == 3) {
        prog = 2

      }
    }
  }
}