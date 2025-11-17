import io.StdIn._
import math._

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Machine sélectionnée (1-5) > " + machineId)
      val pin = readLine("Entrez le code PIN :\n> ").toInt
      if (machinePins(machineId).toInt == pin) {
        println("Accès accordé à la machine " + machineId+".")
        return true
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println("Code PIN incorrect. " + tentatives + " tentatives restantes")
        } else {
          println("Code PIN incorrect. 0 tentatives restantes")
          println(" ")
          println("Trop de tentatives échouées. Fin du programme.")
          return false
        }
      }
    }
    false
  }


  def choixmachine(machineId: Int, machine: Int): Unit = {
    var machine = 0
    println("Veuillez choisir votre machine (1-5)")
    var machineId = readLine("> ").toInt
    while(machineId < 1 || machineId > 5) {
      println("Veuillez saisir une valeure entre 1 et 5 Svp !")
      machineId = readLine("> ").toInt
    }
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId) + "\nVeuillez entrer un nouveau code PIN à 6 chiffres.")
    var newpin = readLine("> ")
    while (newpin.length != 6) {
      println("Veuillez entre un nouveau code PIN d'une longueur de 6 chiffres !!")
      newpin = readLine("> ")
    }
    machinePins(machineId) = newpin
    println("Le nouveau code PIN a été mis à jour avec succès !")
    println("Retour au menu principal...")
  }


  def serveClient(machineId: Int, coffeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var machine = 0
    var choixcafe = 0
    var affichage = 1
    var mode = 0

    var boisson = "café"
    var ajsucre = "avec"
    var ajlait = "combien de doses"

    var boissonprix = 0.00
    var prixfinal = 0.00
    var ajsucreprix = 0.00
    var ajlaitprix = 0.00
    var choixsucre = 0
    var choixlait = 0
    var quantitesucre = 0
    var doselait = 0
    var quantitelait = 0.00
    var client = 0

    while (client == 0) {
        choixcafe = readLine("\nMachine séléctionnée (1-5) > " + machineId + "\nVeuillez sélectionné votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n > ").toInt

        //Si client choisi esspresso
        if (choixcafe == 1) {
          var PCesspresso = 8
          boissonprix = 2.00
          prixfinal = boissonprix
          boisson = "Expresso"
          choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt

          //Si poudre de café (PC) en stock est >= à PC esspresso
          if (coffeStocks(machineId) >= PCesspresso) {
            if (choixsucre == 1) {
              quantitesucre = 0
              ajsucre = "Sans sucre"

            } else if (choixsucre == 2) {
              quantitesucre = 5
              ajsucreprix = 0.10
              ajsucre = "Peu (5g)"
              prixfinal += ajsucreprix

            } else if (choixsucre == 3) {
              quantitesucre = 10
              ajsucreprix = 0.20
              ajsucre = "Moyen (10g)"
              prixfinal += ajsucreprix

            } else if (choixsucre == 4) {
              quantitesucre = 15
              ajsucreprix = 0.30
              ajsucre = "Beaucoup (15g)"
              prixfinal += ajsucreprix
            }

            //Si pas assez de sucre mais assez de poudre à café
            if ((quantitesucre <= sugarStocks(machineId)) && (coffeStocks(machineId) >= PCesspresso)) {
              sugarStocks(machineId) -= quantitesucre
              coffeStocks(machineId) -= PCesspresso
              client = 1

            } else if (quantitesucre > sugarStocks(machineId)) {
              affichage = 0
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Retour au menu principal...")
              return false
            }

          } else if (coffeStocks(machineId) < PCesspresso) {
            affichage = 0
            println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.")
            println("Retour au menu principal...")
            return false
          }

        }

        //Si client choisi Capuccino
        else if (choixcafe == 2) {
          var PCcapuccino = 6
          var laitcapuccino = 100
          var boissonprix = 2.50
          prixfinal = boissonprix
          boisson = "Capuccino"
          choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
          choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt

          //Choix du lait à ajouter (même chose uniquement Cappucino et Latte)
          if (choixlait == 1) {
            doselait = readLine("Combien de dose ?\n > ").toInt

            if (doselait == 1) {
              quantitelait = 100 + 50
              ajlait = "1 dose de lait (50 ml)"
              ajlaitprix = 0.05 //CHF
              prixfinal += ajlaitprix

            } else if (doselait == 2) {
              quantitelait = 100 + 100
              ajlait = "2 dose de lait (100 ml)"
              ajlaitprix = 0.10 //CHF
              prixfinal += ajlaitprix

            } else if (doselait == 3) {
              quantitelait = 100 + 150
              ajlait = "3 dose de lait (150 ml)"
              ajlaitprix = 0.15 //CHF
              prixfinal += ajlaitprix

            } else {
              println("Erreur : La quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
              println("Retour au menu principal...")
              return false
            }

          } else {
            quantitelait = 100
            ajlait = "Non"
          }

          //Choix de sucre
          if (coffeStocks(machineId) >= PCcapuccino) {
            if (milkStocks(machineId) >= quantitelait) {
              if (choixsucre == 1) {
                quantitesucre = 0
                ajsucre = "Sans sucre"

              } else if (choixsucre == 2) {
                quantitesucre = 5
                ajsucreprix = 0.10
                ajsucre = "Peu (5g)"
                prixfinal += ajsucreprix

              } else if (choixsucre == 3) {
                quantitesucre = 10
                ajsucreprix = 0.20
                ajsucre = "Moyen (10g)"
                prixfinal += ajsucreprix

              } else if (choixsucre == 4) {
                quantitesucre = 15
                ajsucreprix = 0.30
                ajsucre = "Beaucoup (15g)"
                prixfinal += ajsucreprix
              }
              //Si quantités sucre lait et poudre à café sont OK
              if ((sugarStocks(machineId) >= quantitesucre) && (sugarStocks(machineId) >= quantitelait) && (coffeStocks(machineId) >= PCcapuccino)) {
                sugarStocks(machineId) -= quantitesucre
                coffeStocks(machineId) -= PCcapuccino
                milkStocks(machineId) -= quantitesucre
                client = 1
              } else {
                affichage = 0
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Retour au menu principal...")
                return false
              }
            } else {
              affichage = 0
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Retour au menu principal...")
              return false
            }
          } else {
            println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.")
            println("Retour au menu principal...")
            return false
          }

          //Si client choisi Latte
        } else if (choixcafe == 3) {
          var taille = readLine("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n > ").toInt
          choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
          choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt

          //Choix des différentes tailles pour le Latte (petit, moyen ou grand)
          //Si taille du Latte --> Petit
          if (taille == 1) {
            var PCpetitlatte = 6
            var laitpetitlatte = 120
            boisson = "Latte (Petit)"
            boissonprix = 2.70
            prixfinal = boissonprix

            //Choix du lait à ajouter (même chose uniquement pour Capuccino et Latte)
            if (choixlait == 1) {
              doselait = readLine("Combien de dose ?\n > ").toInt
              if (doselait == 1) {
                quantitelait = laitpetitlatte + 50
                ajlait = "1 dose de lait (50 ml)"
                ajlaitprix = 0.05
                prixfinal += ajlaitprix

              } else if (doselait == 2) {
                quantitelait = laitpetitlatte + 100
                ajlait = "2 dose de lait (100 ml)"
                ajlaitprix = 0.10
                prixfinal += ajlaitprix

              } else if (doselait == 3) {
                quantitelait = laitpetitlatte + 150
                ajlait = "3 dose de lait (150 ml)"
                ajlaitprix = 0.15
                prixfinal += ajlaitprix

              } else {
                println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                println("Retour au menu principal...")
                return false
              }

            } else {
              quantitelait = 100
              ajlait = "Non"
            }
            if (coffeStocks(machineId) >= PCpetitlatte) {
              if (milkStocks(machineId) >= quantitelait) {
                if (choixsucre == 1) {
                  quantitesucre = 0
                  ajsucre = "Sans sucre"

                } else if (choixsucre == 2) {
                  quantitesucre = 5
                  ajsucreprix = 0.10
                  ajsucre = "Peu (5g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 3) {
                  quantitesucre = 10
                  ajsucreprix = 0.20
                  ajsucre = "Moyen (10g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 4) {
                  quantitesucre = 15
                  ajsucreprix = 0.30
                  ajsucre = "Beaucoup (15g)"
                  prixfinal += ajsucreprix
                }
                if ((sugarStocks(machineId) >= quantitesucre) && (coffeStocks(machineId) >= PCpetitlatte) && (milkStocks(machineId) >= quantitelait)) {
                  sugarStocks(machineId) -= quantitesucre
                  coffeStocks(machineId) -= PCpetitlatte
                  milkStocks(machineId) -= quantitelait.toInt
                  client = 1
                } else {
                  affichage = 0
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Retour au menu principal...")
                  return false
                }

              } else {
                affichage = 0
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Retour au menu principal...")
                return false
              }

            } else {
              println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.")
              println("Retour au menu principal...")
              return false
            }

            //Si taille du Latte --> Moyen (pareil que petit mais avec variable et prix différent
          } else if (taille == 2) {
            var PCmoyenlatte = 8
            var laitmoyenlatte = 150
            boisson = "Latte (Moyen)"
            boissonprix = 3.20
            prixfinal = boissonprix
            if (choixlait == 1) {
              doselait = readLine("Combien de dose ?\n > ").toInt
              if (doselait == 1) {
                quantitelait = laitmoyenlatte + 50
                ajlait = "1 dose de lait (50 ml)"
                ajlaitprix = 0.05
                prixfinal += ajlaitprix

              } else if (doselait == 2) {
                quantitelait = laitmoyenlatte + 100
                ajlait = "2 dose de lait (100 ml)"
                ajlaitprix = 0.10
                prixfinal += ajlaitprix

              } else if (doselait == 3) {
                quantitelait = laitmoyenlatte + 150
                ajlait = "3 dose de lait (150 ml)"
                ajlaitprix = 0.15
                prixfinal += ajlaitprix

              } else {
                println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                println("Retour au menu principal...")
                return false
              }

            } else {
              quantitelait = 150
              ajlait = "Non"
            }
            if (coffeStocks(machineId) >= PCmoyenlatte) {
              if (milkStocks(machineId) >= quantitelait) {
                if (choixsucre == 1) {
                  quantitesucre = 0
                  ajsucre = "Sans sucre"

                } else if (choixsucre == 2) {
                  quantitesucre = 5
                  ajsucreprix = 0.10
                  ajsucre = "Peu (5g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 3) {
                  quantitesucre = 10
                  ajsucreprix = 0.20
                  ajsucre = "Moyen (10g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 4) {
                  quantitesucre = 15
                  ajsucreprix = 0.30
                  ajsucre = "Beaucoup (15g)"
                  prixfinal += ajsucreprix
                }

                if ((sugarStocks(machineId) >= quantitesucre) && (coffeStocks(machineId) >= PCmoyenlatte) && (milkStocks(machineId) >= quantitelait)) {
                  sugarStocks(machineId) -= quantitesucre
                  coffeStocks(machineId) -= PCmoyenlatte
                  milkStocks(machineId) -= quantitelait.toInt
                  client = 1
                } else {
                  affichage = 0
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Retour au menu principal...")
                  return false
                }

              } else {
                affichage = 0
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Retour au menu principal...")
                return false
              }
            } else {
              println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.")
              println("Retour au menu principal...")
              return false
            }

            //Si taille du Latte --> Grand (même chose encore une fois)
          } else if (taille == 3) {
            var PCgrandlatte = 12
            var laitgrandlatte = 200
            boisson = "Latte (Grand)"
            boissonprix = 3.70
            prixfinal = boissonprix
            if (choixlait == 1) {
              doselait = readLine("Combien de dose ?\n > ").toInt

              if (doselait == 1) {
                quantitelait = laitgrandlatte + 50
                ajlait = "1 dose de lait (50 ml)"
                ajlaitprix = 0.05
                prixfinal += ajlaitprix

              } else if (doselait == 2) {
                quantitelait = laitgrandlatte + 100
                ajlait = "2 dose de lait (100 ml)"
                ajlaitprix = 0.10
                prixfinal += ajlaitprix

              } else if (doselait == 3) {
                quantitelait = laitgrandlatte + 150
                ajlait = "3 dose de lait (150 ml)"
                ajlaitprix = 0.15
                prixfinal += ajlaitprix

              } else {
                println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                println("Retour au menu principal...")
                return false
              }
            } else {
              quantitelait = 100
              ajlait = "Non"
            }
            if (coffeStocks(machineId) >= PCgrandlatte) {
              if (milkStocks(machineId) >= quantitelait) {
                if (choixsucre == 1) {
                  quantitesucre = 0
                  ajsucre = "Sans sucre"
                } else if (choixsucre == 2) {
                  quantitesucre = 5
                  ajsucreprix = 0.10
                  ajsucre = "Peu (5g)"
                  prixfinal += ajsucreprix
                } else if (choixsucre == 3) {
                  quantitesucre = 10
                  ajsucreprix = 0.20
                  ajsucre = "Moyen (10g)"
                  prixfinal += ajsucreprix
                } else if (choixsucre == 4) {
                  quantitesucre = 15
                  ajsucreprix = 0.30
                  ajsucre = "Beaucoup (15g)"
                  prixfinal += ajsucreprix
                }
                if ((sugarStocks(machineId) >= quantitesucre) && (coffeStocks(machineId) >= PCgrandlatte) && (milkStocks(machineId) >= quantitelait)) {
                  sugarStocks(machineId) -= quantitesucre
                  coffeStocks(machineId) -= PCgrandlatte
                  milkStocks(machineId) -= quantitelait.toInt
                  client = 1
                } else {
                  affichage = 0
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Retour au menu principal...")
                  return false
                }
              } else {
                affichage = 0
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Retour au menu principal...")
                return false
              }
            } else {
              println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.")
              println("Retour au menu principal...")
              return false
            }
          }
        }
    }

      //Résumé de la commande pour chaque scénario de possible de commande
      //Si le choix du café est pour Capuccino ou Latte (tout sauf Esspresso)
      if (affichage == 1) {
        if (choixcafe != 1) {
          if ((choixlait == 1) && (choixsucre != 1)) {
            println("\nBoisson sélectionée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            println("Lait en supplément : " + ajlait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, ajlaitprix, prixfinal)
          } else if ((choixlait == 2) && (choixsucre != 1)) {
            println("\nBoisson sélectionée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            println("Lait en supplément : " + ajlait)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
          } else if ((choixlait == 1) && (choixsucre == 1)) {
            println("\nBoisson sélectionée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            println("Lait en supplément : " + ajlait)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajlaitprix, prixfinal)
          } else {
            println("\nBoisson sélectionée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            println("Lait en supplément : " + ajlait)
            printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
          }

          //Si choix du café est Esspresso
        } else if (choixcafe == 1) {
          if (choixsucre == 1) {
            println("\nBoisson sélectionnée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
          } else {
            println("\nBoisson sélectionnée : " + boisson)
            println("Niveau de sucre : " + ajsucre)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
          }
        }

        //Paiement de la boisson avec Twint et génération du code alphanumérique
        var alphanumérique = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        var codetwint = ""
        for (i <- 1 to 5) {
          var index = (math.random() * 62).toInt
          codetwint += alphanumérique(index)
        }
        //Affichage correcte paiement Twint
        println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint)
        println("(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.\n")
        println("Préparation de votre boisson...\n[...]\nVotre " + boisson + " est prêt ! Bonne dégustation !")
        client = 0
      }
      true
    }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var poudrecafe = coffeeStocks(machineId)
    var lait = milkStocks(machineId)
    var sucre = sugarStocks(machineId)

    println("Accès autorisé.")
    println(" ")
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + poudrecafe+"g")
    println("Sucre : " + sucre+"g")
    println("Lait : " + lait+"ml")
    println(" ")
    println("Entrez les quantités à ajouter : ")
    poudrecafe += readLine("Poudre de café > ").toInt
    sucre += readLine("Sucre > ").toInt
    lait += readLine("Lait > ").toInt
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(1000)
  }

  def main(args: Array[String]): Unit = {
    //Stocks initiaux
    var machine = 0
    val nbMachines = 5
    val machinePins = Array.fill(nbMachines + 1)("434343")
    var coffeStocks = Array.fill(nbMachines + 1)(50) //en grammes
    var sugarStocks = Array.fill(nbMachines + 1)(30) //en grammes
    var milkStocks = Array.fill(nbMachines+ 1)(500) //en millilitres

    //Affichage d'acceuil de la machine
    while(machine == 0) {
      println("\nNospresso café")
      var mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n >").toInt

      //Mode client pour commander le café
      if (mode == 1) {
       println("Veuillez choisir votre machine (1-5)")
        var machineId = readLine("> ").toInt
        while(machineId < 1 || machineId > 5) {
          println("Veuillez saisir une valeure entre 1 et 5 Svp !")
          machineId = readLine("> ").toInt
        }
        serveClient(machineId, coffeStocks, sugarStocks, milkStocks)

        //Mode Admin avec vérification code PIN
      } else if (mode == 2) {
        println("Veuillez choisir votre machine (1-5)")
        var machineId = readLine("> ").toInt
        while(machineId < 1 || machineId > 5) {
          println("Veuillez saisir une valeure entre 1 et 5 Svp !")
          machineId = readLine("> ").toInt
        }
        if(!validatePin(machineId, machinePins)){
          machine = 1
        } else {
          println("1) Modifier le PIN de la machine " + (machineId) + "\n2) Vérifier les stocks de la machine " + (machineId))
          var adminchoix = readLine("> ").toInt
          while(adminchoix != 1 && adminchoix != 2) {
            println("Veuillez entrer 1 ou 2 pour sélectionner votre choix.")
            adminchoix = readLine("> ").toInt
          }
          if(adminchoix == 1) {
            updatePin(machineId, machinePins)
          }
          if(adminchoix == 2) {
            restockMachine(machineId, coffeStocks, sugarStocks , milkStocks)
          }
        }

        //Mode Quitter
      } else if (mode == 3) {
        machine = 1
      }
      if((mode != 1) && (mode != 2) && (mode != 3)) {
        println("Erreur : Veuillez séléctionner parmi les 3 modes disponibles.")
        }

      }

    }
  }