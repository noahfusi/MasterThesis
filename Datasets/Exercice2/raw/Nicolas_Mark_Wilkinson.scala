import io.StdIn._
import scala.util.Random
object Main {
  //stock initial lors lancement du programme, ne le lit qu'une seule fois.
  val nbMachines = 5
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  val machinePins: Array[String] = Array("434343", "434343", "434343", "434343", "434343")
  var tentativePIN = 3
  var machineId = 0



  def menu_principal(): Unit = {
    var choix = 0
    print("\n=========Nospresso Café=========\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin\n3) Quitter\n> ")
    do {
      val entree_utilisateur = readLine()
      if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
        choix = entree_utilisateur.toInt
        if (choix != 1 && choix != 2 && choix != 3) {
          print("=========Nospresso Café=========\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin\n3) Quitter\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
        }
      }
      else {
        print("=========Nospresso Café=========\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin" + "\n3) Quitter\n")
        print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
      }
    } while (choix != 1 && choix != 2 && choix != 3)

    if (choix == 1) {
      //Start choix machine
      print("Veuillez sélectionner votre Machine (1-5):\n> ")
      do {
        val entree_utilisateur = readLine()
        if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
          machineId = entree_utilisateur.toInt
          if (machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5) {
            print("Veuillez sélectionner votre Machine (1-5):\n")
            print(">> Valeur non autorisée. Veuillez saisir un nombre (1 à 5).\n> ")
          }
        }
        else {
          print("Veuillez sélectionner votre Machine (1-5):\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1 à 5).\n> ")
        }
      } while (machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5)

      machineId -= 1

      serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      //end choix machine
    }

    if (choix == 2) menuAdmin()
    if (choix == 3) quitter()
  }


  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    //Start type de boisson
    var boisson = 0
    print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    do {
      val entree_utilisateur = readLine()
      if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
        boisson = entree_utilisateur.toInt
        if (boisson != 1 && boisson != 2 && boisson != 3) {
          print("Veuilleuz sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
        }
      }
      else {
        print("Veuilleuz sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")
        print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
      }
    } while (boisson != 1 && boisson != 2 && boisson != 3)
    //end type de boisson

    //start quelle taille, si latte
    if (boisson == 3){
      var taille = 0
      print("Veuillez choisir la taille du Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
      do {
        val entree_utilisateur = readLine()
        if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
          taille = entree_utilisateur.toInt
          if (taille != 1 && taille != 2 && taille != 3) {
            print("Veuillez choisir la taille du Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n")
            print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
          }
        }
        else {
          print("Veuillez choisir la taille du Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
        }
      } while (taille != 1 && taille != 2 && taille != 3)

      if (taille == 1){boisson = 3}; if(taille == 2){boisson = 4}; if (taille == 3){boisson = 5}
    }
    //end taille latte

    //start quantite sucre
    var sucre = 0
    print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
    do {
      val entree_utilisateur = readLine()
      if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
        sucre = entree_utilisateur.toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2, 3 ou 4).\n> ")
        }
      }
      else {
        print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n")
        print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2, 3 ou 4).\n> ")
      }
    } while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
    //end quantite sucre

    //start question lait supplé. y/n Seulement si boisson 2 ou 3
    var laityn = 0
    if (boisson >= 2) {
      print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuchino et Latte)\n1) Oui\n2) Non\n> ")
      do {
        val entree_utilisateur = readLine()
        if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
          laityn = entree_utilisateur.toInt
          if (laityn != 1 && laityn != 2) {
            print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuchino et Latte)\n1) Oui\n2) Non\n")
            print(">> Valeur non autorisée. Veuillez saisir un nombre (1 ou 2).\n> ")
          }
        }
        else {
          print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuchino et Latte)\n1) Oui\n2) Non\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1 ou 2).\n> ")
        }
      } while (laityn != 1 && laityn != 2)
    }
    //end question lait supplé.y/n

    //start cb supplement lait, si lait y
    var supplait = 0
    if (laityn == 1){
      print("Combien de dose ? \n> ")
      do {
        val entree_utilisateur = readLine()
        if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
          supplait = entree_utilisateur.toInt
          if (supplait != 1 && supplait != 2 && supplait != 3) {
            print("Combien de dose ? \n")
            print(">> Valeur non autorisée. Veuillez saisir un nombre entre 1 et 3\n> ")
          }
        }
        else {
          print("Combien de dose ? \n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre entre 1 et 3\n> ")
        }
      } while (supplait != 1 && supplait != 2 && supplait != 3)
    }
    //end cb supplement lait

    //start Vérif si assez stock
    val erreur_poudre = "\n>> Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n"
    val erreur_lait = "\n>> Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n"
    val erreur_sucre = "\n>> Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n"
    var stocksuffisant:Boolean = false
    val calcul_supplait = supplait * 50
    val qttesucre = (sucre - 1) * 5
    if (sucre > 1) {
      if (boisson == 1 && coffeeStocks(machineId) >= 8 && sugarStocks(machineId) >= qttesucre){
        stocksuffisant = true
        }
      else {
        if (boisson == 2 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
                stocksuffisant = true
              }
              else if(!stocksuffisant){
                if ((boisson == 1 || boisson == 4) && coffeeStocks(machineId) < 8)print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && coffeeStocks(machineId) < 6)print(erreur_poudre)
                if (boisson == 5 && coffeeStocks(machineId) < 12)print(erreur_poudre)
                if (boisson == 2 && milkStocks(machineId) < 100 + calcul_supplait)print(erreur_lait)
                if (boisson == 3 && milkStocks(machineId) < 120 + calcul_supplait)print(erreur_lait)
                if (boisson == 4 && milkStocks(machineId) < 150 + calcul_supplait)print(erreur_lait)
                if (boisson == 5 && milkStocks(machineId) < 200 + calcul_supplait)print(erreur_lait)
                if (sugarStocks(machineId) < qttesucre)print(erreur_sucre)
                Thread.sleep(2000)
                menu_principal()
              }
            }
          }
        }
      }
    }

    else if (sucre <= 1) { //si pas de sucre
      if (boisson == 1 && coffeeStocks(machineId) >= 8){
        stocksuffisant = true
      }
      else {
        if (boisson == 2 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100 + calcul_supplait) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120 + calcul_supplait) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150 + calcul_supplait) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200 + calcul_supplait) {
                stocksuffisant = true
              }
              else if(!stocksuffisant){
                if ((boisson == 1 || boisson == 4) && coffeeStocks(machineId) < 8)print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && coffeeStocks(machineId) < 6)print(erreur_poudre)
                if (boisson == 5 && coffeeStocks(machineId) < 12)print(erreur_poudre)
                if (boisson == 2 && milkStocks(machineId) < 100 + calcul_supplait)print(erreur_lait)
                if (boisson == 3 && milkStocks(machineId) < 120 + calcul_supplait)print(erreur_lait)
                if (boisson == 4 && milkStocks(machineId) < 150 + calcul_supplait)print(erreur_lait)
                if (boisson == 5 && milkStocks(machineId) < 200 + calcul_supplait)print(erreur_lait)
                Thread.sleep(2000)
                menu_principal()
              }
            }
          }
        }
      }
    }
    //end stock

    //start calcul prix
    var prix:Double = 0.0
    var prix_total = 0.0
    var calcul_sucre = 0.0
    calcul_sucre = (sucre - 1) * 0.10
    if (boisson == 1){prix = 2.00};else if(boisson ==2){prix = 2.50};else if(boisson==3){prix = 2.70};else if(boisson==4){prix = 3.20};else if(boisson==5){prix = 3.70}
    prix_total = (supplait * 0.05)+ prix + calcul_sucre
    //end calcul prix

    //start récap et affichage prix
    var boisson_selec = ""
    var sucre_selec = ""
    var supplait_selec = ""
    if (boisson==1){boisson_selec ="Expresso"};else if(boisson==2){boisson_selec ="Cappuccino"};else if(boisson==3){boisson_selec ="Latte (Petit)"};else if(boisson==4){boisson_selec ="Latte (Moyen)"};else if(boisson==5){boisson_selec ="Latte (Grand)"}
    if (sucre == 1){sucre_selec = "Sans Sucre"};else if(sucre==2){sucre_selec = "Peu (5g)"};else if(sucre==3){sucre_selec = "Moyen (10g)"};else if(sucre==4){sucre_selec = "Beaucoup (15g)"}
    print("\nBoisson sélectionner : "+ boisson_selec)
    print("\nNiveau de sucre : " + sucre_selec)

    if (boisson != 1){
      if(supplait==0){supplait_selec="Non"};else if(supplait==1){supplait_selec="1 * 50ml"};else if(supplait==2){supplait_selec="2 * 50ml"};else if(supplait==3){supplait_selec="3 * 50ml"}
      print("\nLait supplémentaire: "+supplait_selec)
    }
    if (supplait==0) {
      print("\nPrix total : CHF " +prix.toFloat+"0 +"+ " CHF "+calcul_sucre.toFloat +"0"+" = CHF " + prix_total.toFloat + "0\n")
    }
    else {
      print("\nPrix total : CHF " +prix.toFloat+"0 +"+ " CHF "+calcul_sucre.toFloat +"0 +"+" CHF "+ (supplait*0.05).toFloat + " = CHF " + prix_total.toFloat + "\n")
    }
    //end récap

    //start paiement
    var statu_paiement:Boolean = false
    val caractere = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    for (i <- 1 to 5) {
      val choixrandom = caractere(Random.nextInt(36))
      code += choixrandom
    }
    print("\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + code+"\n(En attente de validation du paiment...)\n")
    Thread.sleep(500);print(".");Thread.sleep(500);print(".");Thread.sleep(500);print(".")
    print("\nMerci ! Votre paiement a été accepté.")
    statu_paiement = true
    //end paiement

    //si paiement effectuer MàJ stock
    if (statu_paiement) {
      if (sucre > 1) {
        if (boisson == 1 && coffeeStocks(machineId) >= 8 && sugarStocks(machineId) >= qttesucre){
          coffeeStocks(machineId) -= 8
          sugarStocks(machineId) -= qttesucre
        }
        else {
          if (boisson == 2 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
            coffeeStocks(machineId) -= 6
            milkStocks(machineId) -= (100 + calcul_supplait)
            sugarStocks(machineId) -= qttesucre
          }
          else {
            if (boisson == 3 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
              coffeeStocks(machineId) -= 6
              milkStocks(machineId) -= (100 + calcul_supplait)
              sugarStocks(machineId) -= qttesucre
            }
            else {
              if (boisson == 4 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
                coffeeStocks(machineId) -= 8
                milkStocks(machineId) -= (150 + calcul_supplait)
                sugarStocks(machineId) -= qttesucre
              }
              else {
                if (boisson == 5 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200 + calcul_supplait && sugarStocks(machineId) >= qttesucre) {
                  coffeeStocks(machineId) -= 12
                  milkStocks(machineId) -= (200 + calcul_supplait)
                  sugarStocks(machineId) -= qttesucre
                }
                else if(!stocksuffisant){println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");Thread.sleep(2000); serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)}
              }
            }
          }
        }
      }
      else if (sucre <= 1) {
        if (boisson == 1 && coffeeStocks(machineId) >= 8){
          coffeeStocks(machineId) -= 8
        }
        else {
          if (boisson == 2 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100 + calcul_supplait) {
            coffeeStocks(machineId) -= 6
            milkStocks(machineId) -= (100 + calcul_supplait)
          }
          else {
            if (boisson == 3 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120 + calcul_supplait) {
              coffeeStocks(machineId) -= 6
              milkStocks(machineId) -= (100 + calcul_supplait)
            }
            else {
              if (boisson == 4 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150 + calcul_supplait) {
                coffeeStocks(machineId) -= 8
                milkStocks(machineId) -= (150 + calcul_supplait)
              }
              else {
                if (boisson == 5 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200 + calcul_supplait) {
                  coffeeStocks(machineId) -= 12
                  milkStocks(machineId) -= (200 + calcul_supplait)
                }
                else if(!stocksuffisant){println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");Thread.sleep(2000); serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)}
              }
            }
          }
        }
      }
    }
    //end stock

    //prépa boisson msg de fin
    println("\nPréparation de votre boisson...")
    Thread.sleep(500);print("[.");Thread.sleep(500);print(".");Thread.sleep(500);print(".]\n");Thread.sleep(1000)

    val phrase = "Votre "+boisson_selec+" est prêt ! Bonne dégustation !"
    for (i <- phrase){print(i);Thread.sleep(20)}

    Thread.sleep(1000)
    menu_principal()
    //fin prépa boisson
    true
  }



  def menuAdmin(): Unit = {
    var choixAdmin = 0
    print("\n===========Mode Admin===========\nQuelle opération souhaitez-vous faire ?\n1) Réapprovisionnement du stock\n2) Mettre à jour le code PIN\n3) Retour\n> ")
    do {
      val entree_utilisateur = readLine()
      if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
        choixAdmin = entree_utilisateur.toInt
        if (choixAdmin != 1 && choixAdmin != 2 && choixAdmin != 3) {
          print("===========Mode Admin===========\nQuelle opération souhaitez-vous faire ?\n1) Réapprovisionnement du stock\n2) Mettre à jour le code PIN\n3) Retour\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
        }
      }
      else {
        print("===========Mode Admin===========\nQuelle opération souhaitez-vous faire ?\n1) Réapprovisionnement du stock\n2) Mettre à jour le code PIN\n3) Retour\n")
        print(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).\n> ")
      }
    } while (choixAdmin != 1 && choixAdmin != 2 && choixAdmin != 3)

    if (choixAdmin == 3) menu_principal()

    else {
      print("Veuillez sélectionner votre Machine (1-5):\n> ")
      do {
        val entree_utilisateur = readLine()
        if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
          machineId = entree_utilisateur.toInt
          if (machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5) {
            print("Veuillez sélectionner votre Machine (1-5):\n")
            print(">> Valeur non autorisée. Veuillez saisir un nombre (1 à 5).\n> ")
          }
        }
        else {
          print("Veuillez sélectionner votre Machine (1-5):\n")
          print(">> Valeur non autorisée. Veuillez saisir un nombre (1 à 5).\n> ")
        }
      } while (machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5)

      machineId -= 1
      validatePin(machineId, choixAdmin, machinePins)
    }

  }




  def validatePin(machineId: Int, choixAdmin: Int, machinePins: Array[String]): Boolean = {
    print("===========Mode Admin===========\nEntrez le code PIN de la machine numéro "+(machineId + 1)+":\n> ")
    val correctPin = machinePins(machineId)
    while (tentativePIN > 0) {
      val tentative = readLine()
      if (tentative == correctPin) {
        println(">> Accès autorisé.")
        Thread.sleep(1000)
        if (choixAdmin == 1)restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        if (choixAdmin == 2)updatePin(machineId, machinePins)
        return true
      } else {
        tentativePIN -= 1
        if (tentativePIN == 0) {
          println(">> Trop de tentatives échouées. Fin du programme.")
          Thread.sleep(1000)
          quitter()
          return false
        } else {
          print(s">> Code PIN incorrect. $tentativePIN tentatives restantes.\n> ")
          Thread.sleep(1000)
        }
      }
    }
    menu_principal()
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId + 1) + ".")
    var newPin = 0.toString
    do {
      print("Entrez un nouveau code PIN à 6 chiffres: \n> ")
      newPin = readLine()
    } while (newPin.size != 6 || ! newPin.toIntOption.isDefined)
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(1000)
    menu_principal()
  }

  def quitter(): Unit = {
    val mot = "Au revoir... "
    for (i <- mot){print(i);Thread.sleep(100)}
    Thread.sleep(1000)
  }

  def restockMachine(machineId:Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var ajout_poudre_cafe = 0
    var ajout_sucre = 0
    var ajout_lait = 0.0
    var convertionlait = 0.0
    convertionlait = milkStocks(machineId).toFloat / 1000
    println("===========Mode Admin===========\nNiveaux de stock actuels:\n    Poudre de café: "+coffeeStocks(machineId)+"g\n    Lait          : "+f"$convertionlait%.2f"+"L\n    Sucre         : "+sugarStocks(machineId)+"g")
    print("Réapprovisionnement des stocks");Thread.sleep(500);print(".");Thread.sleep(500);print(".");Thread.sleep(500);print(".")
    print("\nAjout:\n    Pourdre de café: ")
    ajout_poudre_cafe = readInt()
    print("    Lait           : ")
    val convertion_inverse = readDouble()
    ajout_lait = convertion_inverse * 1000
    print("    Sucre          : ")
    ajout_sucre = readInt()
    coffeeStocks(machineId) += ajout_poudre_cafe
    sugarStocks(machineId) += ajout_sucre
    milkStocks(machineId) += ajout_lait.toInt
    print("Niveaux de stocks mis à jour.\nRetour au menu principal");Thread.sleep(500);print(".");Thread.sleep(500);print(".");Thread.sleep(500);print(".\n")
    menu_principal()
  }

  def main(args: Array[String]): Unit = {
    menu_principal()
  }

}
