import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  //initialiser stocks
  var stockCafe = 50.0 // grammes
  var stockSucre = 30.0 // grammes
  var stockLait = 0.5 // litres

  def main(args: Array[String]): Unit = {
    // do while pour reaficher le  programme
    var continuer = true
    do {
      //affichage de l interface du programme
      println("     Nospresso Café     ")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      // 1ere structure de controle pour entrer une valeur demandee etre 1 et 3
      //  initialisee valide=false cad la boucle while s arrte lorsque valide prend true
      //initaliser choix1 a 0
      var choix1 = 0
      var valide = false
      while (!valide) {
        choix1 = readInt()
        if (choix1 >= 1 && choix1 <= 3) {
          valide = true
        } else {
          println("Veuillez entrer un nombre valide entre 1 et 3.") // Message d'erreur
        }
      }
      //choix 1 ==1 on est dans le 1 mode client
      if (choix1 == 1) {
        // le mode Client
        //afficahge de la deuxieme interface en mode client
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print(">")
        var choix2 = 0
        valide = false
        //initialsier les prix des 2 cafee prix fixe pour latee le prix change selon la taille
        var prix1 = 0.0
        var prix2 = 0.0
        //2eme structure de controle pour entrer une valeur demandee entre 1 et 3 pour le choix du cafee
        while (!valide) {
          choix2 = readInt()
          if (choix2 >= 1 && choix2 <= 3) {
            valide = true
          } else {
            println("Veuillez entrer un nombre valide entre 1 et 3.") // Message d'erreur
          }
        }
        //notre choix est fait mainteneant
        //si notre choix2 est 1 on  choisie l expresso
        if (choix2 == 1) {
          //initialiser les quantite et les prix de l expresso et mettre une var niveau de sucre utiliser aprer dans l affichage de niveau de sucre
          val quantitecafee1 = 8
          var quantitedesucre = 5
          //var quantitedesucretot=0
          var niveaudesucre = ""
          prix1 = 2.00
          var prixdosesuc=0.0
          var sucre = true
          var prixtot = prix1
          //interface pour le choix de niveau de sucre
          while(sucre) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println(">")
            var choix3 = 0
            valide = false
            //mettre une var prixtot qui prend la valeur de l expresso basic sans addition de sucre

            while (!valide) {
              choix3 = readInt()
              if (choix3 >= 1 && choix3 <= 4) {
                valide = true
                //le choix3 est fait maintenant on passe pour les choix possible niveau de sucre prend le message selon le choix fait,prixtot prend la valeur de prix1 de lexpresso + prix de sucre ajoutee et quantie de sucre prend la valeur de la dose de sucre ajoutter pour gerer les stock apres
                if (choix3 == 1) {
                  prixdosesuc = 0
                  niveaudesucre = "Sans sucre"
                  prixtot = prix1 + prixdosesuc
                  quantitedesucre = quantitedesucre * 0
                } else if (choix3 == 2) {
                  prixdosesuc = 0.10
                  niveaudesucre = "Peu (5g)"
                  prixtot = prix1 + prixdosesuc
                  quantitedesucre = quantitedesucre * 1
                } else if (choix3 == 3) {
                  prixdosesuc = 0.20
                  niveaudesucre = "Moyen (10g)"
                  prixtot = prix1 + prixdosesuc
                  quantitedesucre = quantitedesucre * 2
                } else {
                  prixdosesuc = 0.30
                  niveaudesucre = "Beaucoup(15g)"
                  prixtot = prix1 + prixdosesuc
                  quantitedesucre = quantitedesucre * 3
                }
              } else {
                println("Veuillez entrer un nombre valide entre 1 et 4.")
              }
            }
            sucre = false
          }
            println("Boisson Selectionnee : Expresso")
            println("Niveau de sucre:" + niveaudesucre)
            println(f"prix : CHF $prix1%.2f + CHF $prixdosesuc%.2f = CHF $prixtot%.2f")
            //verification des stocks
            if (stockCafe >= quantitecafee1 && stockSucre >= quantitedesucre) {
              //les stocks totaux change de valeur apres que le cafee est fait
              stockCafe -= quantitecafee1
              stockSucre -= quantitedesucre
              //afficher l interface suivante si c est possible de faire le cafee demandee
              println("stock suffisant")
              //afficher l interface de paiement de twint
              val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est :" + codeTwint)
              println("(En attente de validation du paiement...)")
              Thread.sleep(3000)//temps de validation de payement
              println("Merci ! Votre paiement a été accepté.")
              println("preparation de votre boisson...")
              Thread.sleep(5000)//temps de preparation de boisson cest pas deamdee mais pour que le distributeur soit reel)
              println("Votre Expresso est prêt ! Bonne dégustation !")

              //les autre posibilite si repture de stocks
            } else if (stockCafe < quantitecafee1 && stockSucre >= quantitedesucre) {
              println("stock  insuffisante")
              println("Erreur : Quantitee de poudre de cafee insuffisante pour preparer la boisson selectionnee.\nVeuillez choisir une autre boisson(Cappuccino ou bien latte petit) ou verifier les\nstocks en mode Admin")
            } else if (stockCafe >= quantitecafee1 && stockSucre < quantitedesucre) {
              println("stock  insuffisant")
              println("Erreur : Quantitee de Sucre insuffisante pour preparer la boisson selectionnee.\nVeuillez choisir un autre niveau de sucre inferieur ou sans sucre ou bien verifier\nstocks en mode Admin")
            } else if (stockCafe < quantitecafee1 && stockSucre < quantitedesucre) {
              println("stock insuffisant")
              println("Erreur : Quantitee de Sucre  et de poudre de cafee insuffisante pour preparer la boisson selectionnee.\nVeuillez choisir un autre niveau de sucre inferieur/sans sucre\n et un autre boisson(Cappuccino ou bien latte petit) ou bien verifier\nstocks en mode Admin")
            }
          }

        //capuccino
        else if (choix2 == 2) {
          val quantitecafee2 = 6
          val quantitedulait = 0.1
          var quantitelaittot = 0.0
          var quantitedesucre = 5
          var niveaudesucre = ""
          prix2 = 2.50
          var dose=0
          var prixdosesuc=0.0
          var dosesupp=""
          var prixdosetot=0.0
          var sucre = true
          var prixtot1 = prix2

          while (sucre) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println(">")

            var choix3 = 0
            valide = false

            while (!valide) {
              choix3 = readInt()
              if (choix3 >= 1 && choix3 <= 4) {
                valide = true
                if (choix3 == 1) {
                  prixdosesuc=0
                  niveaudesucre = "Sans sucre"
                  quantitedesucre = quantitedesucre * 0
                  prixtot1 = prix2
                } else if (choix3 == 2) {
                  prixdosesuc=0.10
                  niveaudesucre = "Peu (5g)"
                  quantitedesucre = quantitedesucre * 1
                  prixtot1 = prix2 + prixdosesuc
                } else if (choix3 == 3) {
                  prixdosesuc=0.20
                  niveaudesucre = "Moyen (10g)"
                  quantitedesucre = quantitedesucre * 2
                  prixtot1 = prix2 + prixdosesuc
                } else {
                  prixdosesuc=0.30
                  niveaudesucre = "Beaucoup(15g)"
                  quantitedesucre = quantitedesucre * 3
                  prixtot1 = prix2 + prixdosesuc
                }
              } else {
                println("Veuillez entrer un nombre valide entre 1 et 4.")
              }
            }
            sucre = false
          }
          // Ajout lait

          println("Souhaitez-vous ajouter du lait en supplément ?" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)")
          println("1) oui")
          println("2) non")
          var choix4 = 0
          var valide2 = false
          while (!valide2) {
            choix4 = readInt()
            if (choix4 >= 1 && choix4 <= 2) {
              valide2 = true
            } else {
              println("Veuillez entrer un nombre valide entre 1 et 2.")
            }
          }

          var lait = true
          var prixtot = prixtot1
          while (lait) {
            var valide = false
            if (choix4 == 1) {
              while (!valide) {
                println("Entrer le nombre de doses")
                dose = readInt()
                if (dose >= 1 && dose <= 3) {
                  valide = true
                  if (dose == 1) {
                    dosesupp="1"
                    prixdosetot = 0.05 * dose
                    prixtot = prixtot1 + prixdosetot
                    quantitelaittot = quantitedulait + 0.05 * dose

                  } else if (dose == 2) {
                    dosesupp="2"
                    prixdosetot = 0.05 * dose
                    prixtot = prixtot1 + prixdosetot
                    quantitelaittot = quantitedulait + 0.05 * dose

                  } else {
                    dosesupp="3"
                    prixdosetot = 0.05 * dose
                    prixtot = prixtot1 + prixdosetot
                    quantitelaittot = quantitedulait + 0.05 * dose
                  }
                } else {
                  println("Veuillez entrer un nombre valide entre 1 et 3.")
                }
              }
            } else {
              dosesupp="NON"
              prixtot = prixtot1
              quantitelaittot = quantitedulait
            }

            lait = false
          }
          if (stockCafe >= quantitecafee2 && stockSucre >= quantitedesucre && stockLait >= quantitelaittot) {
            stockCafe -= quantitecafee2
            stockSucre -= quantitedesucre
            stockLait -= quantitelaittot
            println("Boisson Selectionnee : cappucino")
            println("Personnalisation Sucre" + niveaudesucre)
            println("dose supplementaire" + dosesupp)
            println("stock suffisant")
            println(f"prix : CHF $prix2%.2f + CHF $prixdosesuc%.2f + CHF $prixdosetot%.2f = CHF $prixtot%.2f")

            val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est :" + codeTwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")
            println("preparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre Cappuccino est prêt ! Bonne dégustation !")


          } else if (stockCafe < quantitecafee2 && stockSucre >= quantitedesucre && stockLait >= quantitelaittot) {
            println("stock insuffisant")
            println("Erreur : Quantitee de poudre de cafee insuffisante pour preparer la boisson selectionnee.\n verifier les stocks en mode Admin")
          } else if (stockCafe >= quantitecafee2 && stockSucre < quantitedesucre && stockLait >= quantitelaittot) {
            println("stock insuffisant")
            println("Erreur : Quantitee de Sucre insuffisante pour preparer la boisson selectionnee.\nVeuillez choisir une autre niveau de sucre inferieur/sans sucre ou bien verifier lesstocks en mode Admin")
          } else if (stockCafe < quantitecafee2 && stockSucre < quantitedesucre && stockLait >= quantitelaittot) {

            println("stock insuffisant")
          } else if (stockCafe >= quantitecafee2 && stockSucre >= quantitedesucre && stockLait < quantitelaittot) {
            println("Erreur : Quantitee de Sucre insuffisante et de poudre de cafee insuffisante pour preparer la boisson selectionnee.\n verifier les stocks en mode Admin")
          } else if (stockCafe >= quantitecafee2 && stockSucre >= quantitedesucre && stockLait < quantitelaittot) {
            println("stock insuffisant")
            println("Erreur : Quantitee de lait insuffisante  pour preparer la boisson selectionnee.\n verifier les stocks en mode Admin")
          }





        //Latte
        //taille
      }else {
          var quantitecafee3 = 0
          var quantitedulait = 0.0
          var quantitelaittot = 0.0
          var quantitedesucre = 5
          var dose=0
          var niveaudesucre = ""
          var dosesupp=""
          var taillemessage = ""
          var taille = true
          var prixtaille = 0.0
          var prixdosesuc=0.0
          var prixdosetot=0.0


          while (taille) {
            println("1) Taille petite")
            println("2) Taille moyenne")
            println("3) Taille grande")

            var choix5 = 0
            valide = false

            while (!valide) {
              choix5 = readInt()
              if (choix5 >= 1 && choix5 <= 3) {
                valide = true
                if (choix5 == 1) {
                  taillemessage = "petit"
                  quantitecafee3 = 6
                  quantitedulait = 0.12
                  prixtaille = 2.70
                } else if (choix5 == 2) {
                  taillemessage = "moyen"
                  quantitecafee3 = 8
                  quantitedulait = 0.15
                  prixtaille = 3.20
                } else if (choix5 == 3) {
                  taillemessage = "grand"
                  quantitecafee3 = 12
                  quantitedulait = 0.2
                  prixtaille = 3.70
                }
              } else {
                println("Veuillez entrer un nombre valide entre 1 et 3.") // Message d'erreur pour le sucre
              }
            }
            taille = false
          }
          //ajout sucre
          var sucre = true
          var prixtot2 = prixtaille

          while (sucre) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println(">")

            var choix3 = 0
            valide = false

            while (!valide) {
              choix3 = readInt()
              if (choix3 >= 1 && choix3 <= 4) {
                valide = true
                if (choix3 == 1) {
                  prixdosesuc=0
                  niveaudesucre = "Sans sucre"
                  quantitedesucre = quantitedesucre * 0
                  prixtot2 = prixtaille
                } else if (choix3 == 2) {
                  prixdosesuc=0.10
                  niveaudesucre = "Peu (5g)"
                  quantitedesucre = quantitedesucre * 1
                  prixtot2 = prixtaille + 0.1
                } else if (choix3 == 3) {
                  prixdosesuc=0.20
                  niveaudesucre = "Moyen (10g)"
                  quantitedesucre = quantitedesucre * 2
                  prixtot2 = prixtaille + 0.2
                } else {
                  niveaudesucre = "Beaucoup(15g)"
                  prixdosesuc=0.30
                  quantitedesucre = quantitedesucre * 3
                  prixtot2 = prixtaille + 0.3
                }
              } else {
                println("Veuillez entrer un nombre valide entre 1 et 4.") // Message d'erreur pour le sucre
              }
            }
            sucre = false
          }
          // Ajout lait

          println("Souhaitez-vous ajouter du lait en supplément ?" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)")
          println("1) oui")
          println("2) non")
          var choix4 = 0
          var valide2 = false
          while (!valide2) {
            choix4 = readInt()
            if (choix4 >= 1 && choix4 <= 2) {
              valide2 = true
            } else {
              println("Veuillez entrer un nombre valide entre 1 et 2.")
            }
          }
          var lait = true
          var prixtot = prixtot2
          while (lait) {
            var valide = false
            if (choix4 == 1) {
              while (!valide) {
                println("Entrer le nombre de doses")
                dose = readInt()
                if (dose >= 1 && dose <= 3) {
                  valide = true
                  if (dose == 1) {
                    dosesupp="1"
                    prixdosetot = 0.05 * 1
                    prixtot = prixtot2 + prixdosetot
                    quantitelaittot = quantitedulait + 0.05 * 1

                  } else if (choix4 == 2) {
                    dosesupp="2"
                    prixdosetot = 0.05 * 2
                    prixtot = prixtot2 + prixdosetot
                    quantitelaittot = quantitedulait + 0.05 * 2

                  } else {
                    dosesupp="3"
                    prixdosetot = 0.05 * 3
                    prixtot = prixtot2 + 0.05 * 3
                    quantitelaittot = quantitedulait + 0.05 * 3
                  }
                } else {
                  println("Veuillez entrer un nombre valide entre 1 et 3.")
                }
              }
            } else {
              dosesupp="NON"
              prixdosetot = 0.00
              prixtot = prixtot2
              quantitelaittot = quantitedulait
            }

            lait = false
          }

          if (stockCafe >= quantitecafee3 && stockSucre >= quantitedesucre && stockLait >= quantitelaittot) {
            stockCafe -= quantitecafee3
            stockSucre -= quantitedesucre
            stockLait -= quantitelaittot

            println("Boisson Selectionnee : Latte:" + taillemessage)
            println("Personnalisation Sucre:" + niveaudesucre)
            println("dose supplementaire:" + dosesupp)
            println("stock suffisant")
            println(f"prix : CHF $prixtaille%.2f + CHF $prixdosesuc%.2f + CHF $prixdosetot%.2f = CHF $prixtot%.2f")

            val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est :" + codeTwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")
            println("preparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre Latte est prêt ! Bonne dégustation !")
          } else {
            if (stockCafe < quantitecafee3 && stockSucre >= quantitedesucre && stockLait >= quantitelaittot) {
              println("stock insuffisant")
              println("Erreur : Quantitee de poudre de cafee insuffisante pour preparer la boisson selectionnee.Veuillez choisir une taille plus petite ou essayer un autre boisson.\n verifier les stocks en mode Admin")
            } else if (stockCafe >= quantitecafee3 && stockSucre < quantitedesucre && stockLait >= quantitelaittot) {
              println("stock insuffisant")
              println("Erreur : Quantitee de Sucre insuffisante pour preparer la boisson selectionnee.\nVeuillez choisir une autre niveau de sucre inferieur/sans sucre ou bien verifier les stocks en mode Admin")
            } else if (stockCafe < quantitecafee3 && stockSucre < quantitedesucre && stockLait >= quantitelaittot) {
              println("stock insuffisant")
              println("Erreur : Quantitee de Sucre insuffisante et de poudre de cafee insuffisante pour preparer la boisson selectionnee.\n verifier les stocks en mode Admin")
            } else if (stockCafe >= quantitecafee3 && stockSucre >= quantitedesucre && stockLait < quantitelaittot) {
              println("stock insuffisant")
              println("Erreur : Quantitee de lait insuffisante  pour preparer la boisson selectionnee.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n ou verifier les stocks en mode Admin")
            }
          }
        }

        //choix admin
        //pour les stocks max on n a pas de limite dans l ennoncee mais j ai une structure de controle sur les stock ajoutee ils peuvent pas etre negatifs
      }else if (choix1 == 2) {
        var stockactuelcafe = 0.0
        var stockactuelsucre = 0.0
        var stockactuellait = 0.0
        println("Vous avez sélectionné le mode Admin.")
        val defaultcodepin = 434343
        println("Entrer le PIN code")
        var codePin = 0
        valide = false

        while (!valide) {
          codePin = readInt()
          if (codePin == defaultcodepin) {
            valide = true
            println("Le PIN code est valide")
            println("Stocks actuels :")
            println("Poudre de café :" + stockCafe + "g")
            stockactuelcafe = stockCafe
            println("Sucre :" + stockSucre + "g")
            stockactuelsucre = stockSucre
            println("Lait : " + stockLait + "L")
            stockactuellait = stockLait

            println("Reeapprovisionnement des stocks...\nAjout :")

            var cafeeajoutee = 0
            var valide1 = false
            while (!valide1) {
              println("Poudre de cafee: ")
              cafeeajoutee = readInt()
              if (cafeeajoutee >= 0) {
                valide1 = true
              } else {
                println("erreur") // on peut pas ajouter un stock negatif
              }
            }
            stockCafe = stockactuelcafe + cafeeajoutee

            var laitajoutee = 0.0
            var valide2 = false
            while (!valide2) {
              println("Lait :")
              laitajoutee = readDouble()
              if (laitajoutee >= 0 ) {
                valide2 = true
              } else {
                println("erreur") // on peut pas ajouter un stock negatif
              }
            }
            stockLait = stockactuellait + laitajoutee

            var sucreajoutee = 0
            var valide3 = false
            while (!valide3) {
              println("Sucre :")
              sucreajoutee = readInt()
              if (sucreajoutee >= 0 ) {
                valide3 = true
              } else {
                println("erreur") // on peut pas ajouter un stock negatif
              }
            }
            stockSucre = stockactuelsucre + sucreajoutee

            println("Niveaux de stock mise a jour.")
            println("Retour au menu principal...")
          } else {
            println("Le PIN code est invalide, veuillez réessayer.")
          }
        }
        //choix de quitter
      } else {
        println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
      }
    } while (continuer)
  }
}

