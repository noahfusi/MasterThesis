    import scala.io.StdIn.readInt
    import scala.util.Random
    object Main {
      def main(args: Array[String]): Unit = {
        val prixesperso = 2.00
        val prixcappu = 2.50
        val prixlattepetit = 2.70
        val prixlattemoyen = 3.20
        val prixlattegrand = 3.70
        val prixsucrepeu = 0.10
        val prixsucremoyen = 0.20
        val prixsucrebeaucoup = 0.30
        val prixdoselait = 0.05
        val codepin = 246810 // code d'accès pour mode admin

        var totalelait = 500
        var totalesucre = 30
        var totalecaffe = 50


        while (true) {

          var total = 0.0

          println("Bienvenue au distributeur Nospresso Café")
          println("Les modes disponibles sont les suivant :")
          println("1 : Client")
          println("2 : Admin")
          println("3 : Quitter")

          var typemode = 0

          while (typemode < 1 || typemode > 3) {
            println("Veuillez choisir un mode (1, 2 ou 3) :")
            print("> ")
            typemode = readInt()

            if (typemode < 1 || typemode > 3) {
              println("Ce mode n'existe pas. Veuillez entrer un nombre entre 1 et 3.")
            }
          }

          if (typemode == 1) {

            var typecafe = 0
            var marchdispo = true

            while (typecafe < 1 || typecafe > 3) {
              println("Veuillez sélectionner votre café :")
              println("1) Expresso - CHF " + prixesperso)
              println("2) Cappuccino - CHF " + prixcappu)
              println("3) Latte - Petit CHF " + prixlattepetit + ", Moyen CHF " + prixlattemoyen + ", Grand CHF " + prixlattegrand)
              print("> ")
              typecafe = readInt()

              if (typecafe == 1) {
                if (totalecaffe > 8) {
                  total += prixesperso
                  totalecaffe -= 8
                } else {
                  println("Il n'y a plus assez de café. Veuillez contacter un Admin ou changer de boisson.")
                  marchdispo = false
                }
              } else if (typecafe == 2) {
                if (totalecaffe > 6 && totalelait > 100) {
                  total += prixcappu
                  totalecaffe -= 6
                  totalelait -= 100
                } else {
                  if(totalecaffe < 6) {
                    println("Il n'y a plus assez de café. Veuillez contacter un Admin ou changer de boisson.")
                    marchdispo = false
                  } else if (totalelait < 100) {
                    println("Il n'y a plus assez de lait. Veuillez contacter un Admin ou changer de boisson.")
                    marchdispo = false
                  }
                }
              } else if (typecafe == 3) {
                var taillelatte = 0
                while (taillelatte < 1 || taillelatte > 3) {
                  println("Veuillez sélectionner la taille du Latte :")
                  println("1) Petit")
                  println("2) Moyen")
                  println("3) Grand")
                  print("> ")
                  taillelatte = readInt()

                  if (taillelatte == 1) {
                    if (totalecaffe > 6 && totalelait > 120) {
                      total += prixlattepetit
                      totalecaffe -= 6
                      totalelait -= 120
                    } else {
                      if(totalecaffe < 6) {
                        println("Il n'y a plus assez de café. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      } else if (totalelait < 120) {
                        println("Il n'y a plus assez de lait. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      }
                    }

                  } else if (taillelatte == 2) {
                    if (totalecaffe > 8 && totalelait > 150) {
                      total += prixlattemoyen
                      totalecaffe -= 8
                      totalelait -= 150
                    } else {
                      if(totalecaffe < 8) {
                        println("Il n'y a plus assez de café. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      } else if (totalelait < 150) {
                        println("Il n'y a plus assez de lait. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      }
                    }
                  } else if (taillelatte == 3) {
                    if (totalecaffe > 12 && totalelait > 200) {
                      total += prixlattegrand
                      totalecaffe -= 12
                      totalelait -= 200
                    } else {
                      if(totalecaffe < 12) {
                        println("Il n'y a plus assez de café. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      } else if (totalelait < 200) {
                        println("Il n'y a plus assez de lait. Veuillez contacter un Admin ou changer de boisson.")
                        marchdispo = false
                      }
                    }
                  } else {
                    println("Choix incorrecte. Cette taille n'existe pas.")
                  }
                }
              } else {
                println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
              }
            }

            if (!marchdispo) {
              println("Retour au menu principal...")

            } else {
              var choixsucre = 0
              while (choixsucre < 1 || choixsucre > 4) {
                println("Souhaitez-vous ajouter du sucre ?")
                println("1) Sans sucre")
                println("2) Peu (5g) - CHF " + prixsucrepeu)
                println("3) Moyen (10g) - CHF " + prixsucremoyen)
                println("4) Beaucoup (15g) - CHF " + prixsucrebeaucoup)
                print("> ")
                choixsucre = readInt()

                if (choixsucre == 2) {
                  if (totalesucre > 5) {
                    total += prixsucrepeu
                    totalesucre -= 5
                  } else {
                    println("Il n'y a plus assez de sucre. Veuillez contacter un Admin ou changer de boisson.")
                    println("Retour au menu principal...")
                    marchdispo = false
                  }
                } else if (choixsucre == 3) {
                  if (totalesucre > 10) {
                    total += prixsucremoyen
                    totalesucre -= 10
                  } else {
                    println("Il n'y a plus assez de sucre. Veuillez contacter un Admin ou changer de boisson.")
                    println("Retour au menu principal...")
                    marchdispo = false
                  }
                }
                else if (choixsucre == 4) {
                  if (totalesucre > 15) {
                    total += prixsucrebeaucoup
                    totalesucre -= 15
                  } else {
                    println("Il n'y a plus assez de sucre. Veuillez contacter un Admin ou changer de boisson.")
                    println("Retour au menu principal...")
                    marchdispo = false
                  }
                } else if (choixsucre != 1) println("Choix incorrect.Veuillez entrer un chiffre entre 1 et 4.")
              }


              if (marchdispo && (typecafe == 2 || typecafe == 3)) {
                var ajoutelait = 0
                while (ajoutelait != 1 && ajoutelait != 2) {
                  println("Souhaitez-vous ajouter une dose de lait ? (jusqu'à 3 au maximum)")
                  println("1) Oui")
                  println("2) Non")
                  print("> ")
                  ajoutelait = readInt()

                  if (ajoutelait == 1) {
                    var dosedelait = 0
                    while (dosedelait < 1 || dosedelait > 3) {
                      println("Combien de dose de lait voulez-vous ajouter ? (1, 2 ou 3)")
                      print("> ")
                      dosedelait = readInt()

                      if (dosedelait >= 1 && dosedelait <= 3) {
                        if (totalelait > 50 * dosedelait) {
                          total += dosedelait * prixdoselait
                          totalelait -= 50 * dosedelait

                        } else {
                          println("Il n'y a plus assez de lait. Veuillez contacter un Admin ou changer de boisson.")
                          println("Retour au menu principal...")
                          marchdispo = false
                        }

                      } else {
                        println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
                      }
                    }
                  } else if (ajoutelait != 2) {
                    println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
                  }
                }
              }

              val alphanumcara = "AZERTYUIOPQSDFGHJKLMWXCVBNazertyuiopmlkjhgfdsqwxcvbn0123456789"
              val longeur = 5
              var code = ""
              var i = 0
              while (i < longeur) {
                val codealeatoir = Random.nextInt(alphanumcara.length)
                code += alphanumcara(codealeatoir)
                i += 1
              }
              if (!marchdispo) {
                println("Retour au menu principal...")

              } else {

                printf("Total à payer : CHF  %.2f \n", +total)
                println("Veuillez payer en utilisant Twint. ")
                println("Le code du paiement Twint est: " + code)
                println("En attente de validation du paiement...")
                Thread.sleep(2987)

                println("Merci ! Votre paiement a été accepté.")
                println("Préparation de votre boisson...")
                println("[...]")
                Thread.sleep(3000)
                if (typecafe == 1) {
                  println("Votre Expresso est prêt ! Bonne dégustation !")
                } else if (typecafe == 2) {
                  println("Votre Cappuccino est prêt ! Bonne dégustation !")
                } else {
                  println("Votre Latte est prêt ! Bonne dégustation !")
                }

              }
            }


            } else if (typemode == 2) {
                println("Mode administrateur sélectionné. Veuillez entrer le code d'accès :")
                print("> ")
                val codepinentree = readInt()

                if (codepinentree == codepin) {
                  println("Code correct. Accès aux informations de la marchandise.")
                  println("Le stock de café est de " + totalecaffe + " grammes.")
                  println("Le stock de lait est de " + totalelait + " millilitre.")
                  println("Le stock de sucre est de " + totalesucre + " grammes.")

                  var ajoutstock = 0

                  while (ajoutstock != 1 && ajoutstock != 2) {
                    println("Souhaitez-vous réapprovisionner de la marchandise ?")
                    println("1) Oui")
                    println("2) Non")
                    print("> ")
                    ajoutstock = readInt()

                    var ajoutcafe = 0
                    var ajoutsucre = 0
                    var ajoutlait = 0

                    if (ajoutstock == 1) {

                      while (ajoutcafe != 1 && ajoutcafe != 2) {
                        println("Souhaitez-vous réapprovisionner 50 grammes de café ?")
                        println("1) Oui")
                        println("2) Non")
                        print("> ")
                        ajoutcafe = readInt()

                        if (ajoutcafe == 1) {
                          totalecaffe += 50
                        } else if (ajoutcafe != 2) {
                          println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
                        }
                      }
                      while (ajoutsucre != 1 && ajoutsucre != 2) {
                        println("Souhaitez-vous réapprovisionner 30 grammes de sucre ?")
                        println("1) Oui")
                        println("2) Non")
                        print("> ")
                        ajoutsucre = readInt()

                        if (ajoutsucre == 1) {
                          totalesucre += 30
                        } else if (ajoutsucre != 2) {
                          println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
                        }
                      }

                      while (ajoutlait != 1 && ajoutlait != 2) {
                        println("Souhaitez-vous réapprovisionner 500 millilitres de lait ?")
                        println("1) Oui")
                        println("2) Non")
                        print("> ")
                        ajoutlait = readInt()

                        if (ajoutlait == 1) {
                          totalelait += 500
                        } else if (ajoutlait != 2) {
                          println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
                        }
                      }

                    } else if (ajoutstock != 2) {
                      println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
                    }
                  }

                  println("Le nouveau stock de café est de " + totalecaffe + " grammes.")
                  println("Le nouveau stock de lait est de " + totalelait + " millilitre.")
                  println("Le nouveau stock de sucre est de " + totalesucre + " grammes.")
                  println("Retour au menu principal...")
                  Thread.sleep(1500)

                } else {
                  println("Code incorrect. Accès refusé.")
                  Thread.sleep(1500)
                }
          }
        }
      }
    }