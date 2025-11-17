import scala.io.StdIn.readLine                                                   //import scala.io.StdIn.readLine
object NospressoApp extends App {

  var laitstock = 0.5
  var cafestock = 50
  var sucrestock = 30


  while (true) {
      laitstock
      cafestock
      sucrestock
      val Pin = 434343
      var prix = 0.00


      // Choix mode


        println("       Nospresso café              ")
        println("Veuiller choisir votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        var choix = readLine("> ").toInt
        while (choix < 1 || choix > 3) choix = readLine("Veuillez selectionner 1,2 ou 3 : ").toInt



        // Mode Client

        if (choix == 1) {

          val expresso = 2.00
          val cappuccino = 2.50
          val latteP = 2.70
          val latteM = 3.20
          val latteG = 3.70
          var dose = 0

          // Sélection de boisson

          println("Veuillez selectionner votre boisson : ")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          var choixBoisson = readLine("> ").toInt
          while (choixBoisson < 1 || choixBoisson > 3) choixBoisson = readLine("Veuillez selectionner 1,2 ou 3 : ").toInt


          // Choix du sucre

          println("Souhaitez-vous ajouer du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          var sucre = readLine("> ").toInt
          while (sucre < 1 || sucre > 4) sucre = readLine("Veuillez selectionner 1, 2, 3 ou 4 : ").toInt
          if (sucre == 2) {
              sucrestock -= 5
              prix += 0.10
            }
          if (sucre == 3) {
              sucrestock -= 10
              prix += 0.20
          }
          if (sucre == 4) {
              sucrestock -= 15
              prix += 0.30
            }


          // Choix du lait

          if (choixBoisson == 2 || choixBoisson == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            var lait = readLine("> ").toInt
            while (lait < 1 || lait > 2) lait = readLine("Veuillez selectionner 1 ou 2 : ").toInt
            if (lait == 1) {
              println("1) 1 dose (50ml) - CHF 0.05")
              println("2) 2 doses (100ml) - CHF 0.10")
              println("3) 3 doses (150ml) - CHF 0.20")
              dose = readLine("> ").toInt
              while (dose < 1 || dose > 3) dose = readLine("Veuillez selectionner 1, 2, ou 3 : ").toInt
              if (dose == 1) {
                  laitstock -= 0.05
                  prix += 0.05
              }
              if (dose == 2) {
                  laitstock -= 0.1
                  prix += 0.10
              }
              if (dose == 3) {
                  laitstock -= 0.15
                  prix += 0.20
              }
            }
          }


          // Expresso

          if (choixBoisson == 1) {
            if (cafestock < 8) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              if (sucre == 2) {
                sucrestock += 5
              }
              if (sucre == 3) {
                sucrestock += 10
              }
              if (sucre == 4) {
                sucrestock += 15
              }
              while (true){}
            } else {
              prix += expresso
              cafestock -= 8
            }
          }


          // Cappuccino

          if (choixBoisson == 2) {
            if (cafestock < 6) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.")
              if (dose == 1) {
                laitstock += 0.05
              }
              if (dose == 2) {
                laitstock += 0.1
              }
              if (dose == 3) {
                laitstock += 0.150
              }
              if (sucre == 2) {
                sucrestock += 5
              }
              if (sucre == 3) {
                sucrestock += 10
              }
              if (sucre == 4) {
                sucrestock += 15
              }
              while (true){}
            } else if (laitstock < 0.100) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.")
              if (dose == 1) {
                laitstock += 0.05
              }
              if (dose == 2) {
                laitstock += 0.1
              }
              if (dose == 3) {
                laitstock += 0.150
              }
              if (sucre == 2) {
                sucrestock += 5
              }
              if (sucre == 3) {
                sucrestock += 10
              }
              if (sucre == 4) {
                sucrestock += 15
            }
              while (true){}
            } else {
              prix += cappuccino
              laitstock -= 0.1
              cafestock -= 6
            }
          }


          // Latte

          if (choixBoisson == 3) {
            println("Choissisez la taille de votre boisson : ")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            var taille = readLine("Choisir 1, 2 ou 3 : ").toInt
            while (taille < 1 || taille > 3) taille = readLine("Veuillez selectionner 1, 2, ou 3 : ").toInt
            if (taille == 1) {
              if (cafestock < 6) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                while (true){}
              } else if (laitstock < 0.120) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                while (true){}
              } else {
                cafestock -= 6
                laitstock -= 0.120
                prix += latteP
              }
            }
            if (taille == 2) {
              if (cafestock < 8) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                while (true){}
              } else if (laitstock < 0.150) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                while (true){}
              } else {
                cafestock -= 8
                laitstock -= 0.150
                prix += latteM
              }
            }
            if (taille == 3) {
              if (cafestock < 12) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                while (true){}
              } else if (laitstock < 0.200) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                if (sucre == 2) {
                  sucrestock += 5
                }
                if (sucre == 3) {
                  sucrestock += 10
                }
                if (sucre == 4) {
                  sucrestock += 15
                }
                if (dose == 1) {
                  laitstock += 0.05
                }
                if (dose == 2) {
                  laitstock += 0.1
                }
                if (dose == 3) {
                  laitstock += 0.150
                }
                while (true){}
              } else {
                cafestock -= 12
                laitstock -= 0.200
                prix += latteG
              }
            }
          }



          // Paiement

          printf("Le montant de votre commande est de : %.2f CHF \n", prix)
          println("Veuillez payer en utilisant Twint.")
          val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          var code = ""
          var seed = 7
          for (i <- 1 to 5) {
            seed = (seed * 37 + i * 19) % caracteres.length.toInt
            code += caracteres(seed)
          }
          println("Votre code de paiement est : " + code)
          println("(En attente de validation du paiement)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.")
          Thread.sleep(1500)
          println("Préparation de votre boisson...")
          println("[...]")
          if (choixBoisson == 1) {
            println("Votre Expresso est prêt ! Bonne dégustation !")
          }
          if (choixBoisson == 2) {
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          }
          if (choixBoisson == 3) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
          Thread.sleep(1500)
        }



        // Mode Admin

        if (choix == 2) {
          println("Mode Admin")
          var motdepasse = readLine("Entrez le code PIN : ").toInt
          while (motdepasse != Pin) {
            motdepasse = readLine("Mot de passe incorrect, veuillez réessayer : ").toInt
          }
          if (motdepasse == Pin) {
            println("Accès autorisé.")
            println("Stocks : ")
            println("Poudre de café : " + cafestock + "g")
            printf("Lait :           %.3fL \n", laitstock)
            println("Sucre :          " + sucrestock + "g")
            println("Réapprovisionnement des stocks...")
            println("Ajout : ")
            var ajoutcafe = readLine("Poudre de café : ").toInt
            var ajoutlait = readLine("Lait : ").toDouble
            var ajoutsucre = readLine("Sucre : ").toInt
            cafestock += ajoutcafe
            laitstock += ajoutlait
            sucrestock += ajoutsucre
          }
        }

        // Quitter

        if (choix == 3) {
          println("Merci d'avoir utilisé Nospresso. À bientôt !")
          System.exit(0)
        }


      }
  }

