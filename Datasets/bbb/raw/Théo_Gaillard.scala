import scala.io.StdIn._
import scala.util.Random

object Main {
  //def pour le code twint aléatoire
  def alphanumeric(length: Int): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to length).map(_ => caracteres(Random.nextInt(caracteres.length))).mkString
  }
  //def pour limiter nombre d'essais avec le code pin
  //j'ai aussi par choix remplacer le machineId de l'ex par choix machine car je préfère en corelation avec les autres noms de val
  def validatePin(choixmachine: Int, machinePins: Array[String]): Boolean = {
    var nbressais = 3
    while (nbressais > 0) {
      val essais = readLine("Entrez le code PIN :\n> ")
      if (essais == machinePins(choixmachine - 1)) {
        println("Code PIN correct.")
        return true
      } else {
        nbressais -= 1
        println("Code PIN incorrect. " + nbressais + " tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }
  //def pour les valeurs de restock
  def restockMachine(choixmachine: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Double]): Unit = {
    println("\nStocks") //on commence par afficher l'état actuel des stocks
    println("   Poudre de café: " + stockcafe(choixmachine - 1) + "g")
    println("   Lait          : " + stocklait(choixmachine - 1) + "l")
    println("   Sucre         : " + stocksucre(choixmachine - 1) + "g")
    println("\nRéapprovisionnement des stocks...")
    println("Ajout :")
    //On laisse la sélection des quantités
    var rajoutcafe = readLine("   Poudre de café: ").toInt
    var rajoutlait = readLine("   Lait          : ").toDouble
    var rajoutsucre = readLine("   Sucre         : ").toInt
    stockcafe(choixmachine - 1) += rajoutcafe
    stocklait(choixmachine - 1) += rajoutlait
    stocksucre(choixmachine - 1) += rajoutsucre
    //on remets les rajouts provisoires à 0
    rajoutcafe = 0
    rajoutlait = 0.000
    rajoutsucre = 0
    println("Niveaux de stock mis à jour\nRetour au menu principal")
  }
  //def pour changer le code pin
  def updatePin(choixmachine: Int, machinePins: Array[String]): Unit = {
    println("\nMise à jour du code PIN pour la Machine " + choixmachine)
    var nvcode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while (!(nvcode.length == 6)){
      nvcode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(choixmachine - 1) = nvcode
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }
  def serveClient(choixmachine: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Double]): Boolean = {
    var choixboisson = 0
    var choixtaille = 0
    var choixsucre = 0
    var choixlait = 0
    var quantitelait = 0
    //on remet la var du prix, comme ca lorsque le paiement se relance après une erreur le compte recommence à 0
    var prix = 0.00
    //la même avec le prix de chaque partie
    var prixboisson = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    //on reset aussi les doses provisoires
    var dosecafe = 0
    var doselait = 0.000
    var dosesucre = 0
    //pour chaque boucle on aura un code twint pour le paiement
    //on crée un code Twint aléatoire alphanumérique de 5 caractères
    val codetwint = alphanumeric(5)

    //on peut faire comme pour le choix de mode mais avec les boissons
    println("\nVeuillez sélectionn votre boisson :\n1) Expresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    choixboisson = readLine(">").toInt
    //comment avant on limite les choix à ce qui est possible
    while (!(choixboisson == 1) && !(choixboisson == 2) && !(choixboisson == 3)) {
      println("Commande pas reconnu, veuillez ressayer SVP")
      choixboisson = readLine(">").toInt
    }
    //pour le latte on permet le choix de taille
    if (choixboisson == 3) /*latte*/ {
      //on pose la question uniquement pour le 3 car c'est le seul avec plusieurs taille
      println("\nQuelle taille ?\n1) Petit\n2) Moyen\n3) Grand")
      choixtaille = readLine(">").toInt
      while (!(choixtaille == 1) && !(choixtaille == 2) && !(choixtaille == 3)) {
        println("Commande pas reconnu, veuillez ressayer SVP")
        choixtaille = readLine(">").toInt
      }
    }
    println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    choixsucre = readLine(">").toInt
    while (!(choixsucre == 1) && !(choixsucre == 2) && !(choixsucre == 3) && !(choixsucre == 4)) {
      println("Commande pas reconnu, veuillez ressayer SVP")
      choixsucre = readLine(">").toInt
    }
    //on permet aussi pour le lait, mais uniquement  pour le cappuccino et le latte
    if ((choixboisson == 2) || (choixboisson == 3)) {
      println("\nSouaitez-vous du lait en supplément ?\n1) Oui\n2) Non")
      choixlait = readLine(">").toInt
      //on limite sur deux choix
      while (!(choixlait == 1) && !(choixlait == 2)) {
        println("Choisissez entre 1 et 2")
        choixlait = readLine(">").toInt
      }
      //On continue que pour Oui
      if (choixlait == 1) {
        //on permet de mettre des doses de lait
        println("\nCombien de dose ?")
        quantitelait = readLine(">").toInt
        //on met des limites
        while (quantitelait < 0 || quantitelait > 3) {
          if (quantitelait < 0) {
            println("La quantité doit être positive")
            quantitelait = readLine(">").toInt
          } else if (quantitelait > 3) {
            println("Vous ne pouvez pas prendre plus de 3 doses")
            quantitelait = readLine(">").toInt
          }
        }
      } else {
        //on ne mets rien de plus
      }
    }
    //on affiche les choix finaux des boissons en premier et on compte la quantité de café et de lait
    if (choixboisson == 1) {
      println("\nBoisson sélectionnée : Expresso")
      prixboisson += 2.00
      dosecafe += 8
    } else if (choixboisson == 2) {
      println("\nBoisson sélectionnée : Cappucino")
      prixboisson += 2.50
      dosecafe += 6
      doselait += 0.100
    } else {
      //pour le latte la taille est aussi afficher donc on doit permettre cela
      if (choixtaille == 1) {
        println("\nBoisson sélectionnée : Latte (Petit)")
        prixboisson += 2.70
        dosecafe += 6
        doselait += 0.120
      } else if (choixtaille == 2) {
        println("\nBoisson sélectionnée : Latte (Moyen)")
        prixboisson += 3.20
        dosecafe += 8
        doselait += 0.150
      } else {
        println("\nBoisson sélectionnée : Latte (Grand)")
        prixboisson += 3.70
        dosecafe += 12
        doselait += 0.200
      }
    }
    //ensuite on affiche le niveau de sucre et cpmpte les doses aussi
    if (choixsucre == 1) {
      println("Niveau de sucre : Sans Sucre")
    } else if (choixsucre == 2) {
      println("Niveau de sucre : Peu (5g)")
      prixsucre = 0.10
      dosesucre = 5
    } else if (choixsucre == 3) {
      println("Niveau de sucre : Moyen (10g)")
      prixsucre = 0.20
      dosesucre = 10
    } else {
      println("Niveau de sucre : Beaucoup (15g)")
      prixsucre = 0.30
      dosesucre = 15
    }
    //Même choix avec le lait toujours que pour 2 et 3
    if ((choixboisson == 2) || (choixboisson == 3)) {
      if (choixlait == 1 && quantitelait > 0) {
        if (quantitelait == 1) {
          println("Lait supplémentaire: Oui (1 dose)")
          prixlait += 0.05
          doselait += 0.050
        } else if (quantitelait == 2) {
          println("Lait supplémentaire: Oui (2 dose)")
          prixlait += 0.10
          doselait += 0.100
        } else {
          println("Lait supplémentaire: Oui (3 dose)")
          prixlait += 0.15
          doselait += 0.150
        }
      } else {
        println("Lait supplémentaire: Non")
      }
    }
    //on affiche maintenant le prix et check les quantités
    if (choixboisson == 1 /*expresso*/ ) {
      //on doit d'abord check les quantités car si il n'y a pas assez cela ne propose pas le prix et quitte le programme
      if (stockcafe(choixmachine - 1) < dosecafe) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une autre boisson ou vérifier les \nstocks en mode Admin")
        return false
      }
      //on regarde maintenant pour si il y du sucre ou non. On modifie le prix et l'affichage
      //si il n'y a pas assez de sucre on arrête le programme
      if (stocksucre(choixmachine - 1) < dosesucre) {
        println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir moins de sucre.")
        false
      } else {
        prix += prixboisson + prixsucre //on additonne le prix total
        if (choixsucre == 1) { //on doit faire de manière différente si il y a ou non du sucre en plus
          printf("Prix total : CHF %.2f", prix)
        } else {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
        }
        //permet maintenant le paiment
        println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint + "\n(En attente de validation du paiement...)")
        //on fait attendre 3 secondes (3000 millisecondes)
        Thread.sleep(3000)
        //on check si le paiment est accepté
        println("\nPaiement confirmé.\nPréparation de votre boisson...")
        //on attends 5 secondes cette fois pour que le café se fasse
        Thread.sleep(5000)
        println("Votre Expresso est prêt ! Bonne déustation !")
        //on enlève que maintenant les quantités
        stockcafe(choixmachine - 1) -= dosecafe
        stocklait(choixmachine - 1) -= doselait
        stocksucre(choixmachine - 1) -= dosesucre
        true
      }
    } else if (choixboisson == 2 /*cappuccino*/ ) {
      if (stockcafe(choixmachine - 1) < dosecafe) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez vérifier les stocks en mode Admin")
        false
      } else {
        if (stocklait(choixmachine - 1) < doselait) { //ici on doit aussi vérifier pour le lait
          println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez essayer une autre boisson.")
          false
        } else {
          if (stocksucre(choixmachine - 1) < dosesucre) { //on refait avec le sucre
            println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir moins de sucre.")
            false
          } else {
            prix += prixboisson + prixsucre + prixlait //on additonne le prix total avec le lait en plus
            if (choixsucre == 1) { //on doit faire de manière différente si il y a ou non du sucre en plus
              if (choixlait == 1) { //cette fois-ci on doit faire le lait aussi, l'afficher ou non
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
              } else {
                printf("Prix total : CHF %.2f", prix)
              }
            } else {
              if (choixlait == 1) {
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
              } else {
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
              }
            }
            println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint + "\n(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("\nPaiement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre Cappuccino est prêt ! Bonne déustation !")
            stockcafe(choixmachine - 1) -= dosecafe
            stocklait(choixmachine - 1) -= doselait
            stocksucre(choixmachine - 1) -= dosesucre
            true
          }
        }
      }
    } else /*latte*/ {
      if (choixtaille == 1) {
        if (stockcafe(choixmachine - 1) < dosecafe) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez vérifier les stocks en mode Admin")
          false
        } else {
          if ((choixmachine - 1) < doselait) {
            println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une autre boisson.")
            false
          } else {
            if (stocksucre(choixmachine - 1) < dosesucre) {
              println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir moins de sucre.")
              false
            } else {
              prix += prixboisson + prixsucre + prixlait
              if (choixsucre == 1) {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                } else {
                  printf("Prix total : CHF %.2f", prix)
                }
              } else {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                } else {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                }
              }
            }
          }
        }
      } else if (choixtaille == 2) {
        if (stockcafe(choixmachine - 1) < dosecafe) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une taille plus petite ou essayer \nune autre boisson.")
          false
        } else {
          if (stocklait(choixmachine - 1) < doselait) {
            println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une taille plus petite ou essayer \nune autre boisson.")
            false
          } else {
            if (stocksucre(choixmachine - 1) < dosesucre) {
              println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir moins de sucre.")
              false
            } else {
              prix += prixboisson + prixsucre + prixlait
              if (choixsucre == 1) {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                } else {
                  printf("Prix total : CHF %.2f", prix)
                }
              } else {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                } else {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                }
              }
            }
          }
        }
      } else {
        if (stockcafe(choixmachine - 1) < dosecafe) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une taille plus petite ou essayer \nune autre boisson.")
          false
        } else {
          if (stocklait(choixmachine - 1) < doselait) {
            println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir une taille plus petite ou essayer \nune autre boisson.")
            false
          } else {
            if (stocksucre(choixmachine - 1) < dosesucre) {
              println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.\nVeuillez choisir moins de sucre.")
              false
            } else {
              prix += prixboisson + prixsucre + prixlait
              if (choixsucre == 1) {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                } else {
                  printf("Prix total : CHF %.2f", prix)
                }
              } else {
                if (choixlait == 1) {
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                } else {
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                }
              }
            }
          }
        }
      }
      println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint + "\n(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé.\nPréparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre Latte est prêt ! Bonne déustation !")
      stockcafe(choixmachine - 1) -= dosecafe
      stocklait(choixmachine - 1) -= doselait
      stocksucre(choixmachine - 1) -= dosesucre
      true
    }
  }
  def main(args: Array[String]): Unit = {
    //On définit des valeurs pour plus tard
    //on définit le total de machines
    val nbMachines = 5
    //var des stocks pour toute les machines
    val stockcafe = Array.fill(nbMachines)(50)
    val stocksucre = Array.fill(nbMachines)(30)
    val stocklait = Array.fill(nbMachines)(0.500)
    //on crée un val pour le code pin cette fois ci afin de permettre d'en avoir un différent pour chaque
    val machinePins = Array.fill(nbMachines)("434343")
    //var choix servent pour sélectionner dans les menus plus tard et quantité
    var choixmachine = 0
    var choixmodeadmin = 0
    var choixmode = 0
    //une crée une variable pour avoir une boucle sur le menu principal
    var lancement = true

    while (lancement) {
      println("\n        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      //la première étape est de permettre à l'utilisateur de choisir un mode
      choixmode = readLine(">").toInt
      //on doit refusser ce qui ne fais pas partie des trois choix
      while (!(choixmode == 1) && !(choixmode == 2) && !(choixmode == 3)) {
        println("Commande pas reconnu, veuillez ressayer SVP")
        choixmode = readLine(">").toInt
      }
      //on laisse le choix de la machine
      choixmachine = readLine("\nMachine sélectionnée (1-5) > ").toInt
      //on doit refusser ce qui ne fais pas partie des 5 machines
      while (choixmachine < 0 || choixmachine > 5) {
        println("Commande pas reconnu, veuillez ressayer SVP")
        choixmode = readLine(">").toInt
      }
      //Maintenant il faut lier la commande tapé aux trois options
      if (choixmode == 1 /*client*/ ) {
        serveClient(choixmachine, stockcafe, stocksucre, stocklait)
      } else if (choixmode == 2 /*admin*/ ) {
        //on demande ce que l'admin veut faire
        println("\nMode Admin\nQue voulez vous faire ?\n1) Réapprovisionner les ingrédients\n2) Changer le code Pin\n3) Quitter")
        choixmodeadmin = readLine(">").toInt
        //on doit refusser ce qui ne fais pas partie des trois choix
        while (!(choixmodeadmin == 1) && !(choixmodeadmin == 2) && !(choixmodeadmin == 3)) {
          println("Commande pas reconnu, veuillez ressayer SVP")
          choixmodeadmin = readLine(">").toInt
        }
        if (choixmodeadmin == 3){
          //on laisse quitter
        }else /*on permet le choix de machine*/ {
          //on utilise la def
          if (validatePin(choixmachine, machinePins)) {
            if (choixmodeadmin == 1 /*stocks*/ ) {
              println("Accès autorisé")
              //on utilise la def
              restockMachine(choixmachine, stockcafe, stocksucre, stocklait)
            } else if (choixmodeadmin == 2 /*code*/ ) {
              //on utilise la def
              updatePin(choixmachine, machinePins)
            }
          }
        }
      } else /*quitter*/ {
        lancement = false //on quitte la boucle pour eteindre
      }
    }
  }
 }
