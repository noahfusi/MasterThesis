import io.StdIn._
import scala.util.Random
object Main {

  def main(args: Array[String]): Unit = {
    menu_principal()
  }

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
    if (choix == 1) mode_client() //ceci est une définition pour simplifier le code en plusieurs grande partie...
    if (choix == 2) mode_adminPIN() //losrque qu'on appelle la définition le programme execute tout se qu'il y a dans la définition
    if (choix == 3) quitter() //Hors si la définition n'est pas appeller le programme ne passera jamais dedans
  }

  //stock initial lors lancement du programme, ne le lit qu'une seule fois.
  var stock_poudre_cafe = 50 //50
  var stock_sucre = 30 //30
  var stock_lait = 500.0 //500

  def mode_client(): Unit = {

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
      if (boisson == 1 && stock_poudre_cafe >= 8 && stock_sucre >= qttesucre){
        stocksuffisant = true
        }
      else {
        if (boisson == 2 && stock_poudre_cafe >= 6 && stock_lait >= 100 + calcul_supplait && stock_sucre >= qttesucre) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && stock_poudre_cafe >= 6 && stock_lait >= 120 + calcul_supplait && stock_sucre >= qttesucre) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && stock_poudre_cafe >= 8 && stock_lait >= 150 + calcul_supplait && stock_sucre >= qttesucre) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && stock_poudre_cafe >= 12 && stock_lait >= 200 + calcul_supplait && stock_sucre >= qttesucre) {
                stocksuffisant = true
              }
              else if(!stocksuffisant){
                if ((boisson == 1 || boisson == 4) && stock_poudre_cafe < 8)print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && stock_poudre_cafe < 6)print(erreur_poudre)
                if (boisson == 5 && stock_poudre_cafe < 12)print(erreur_poudre)
                if (boisson == 2 && stock_lait < 100 + calcul_supplait)print(erreur_lait)
                if (boisson == 3 && stock_lait < 120 + calcul_supplait)print(erreur_lait)
                if (boisson == 4 && stock_lait < 150 + calcul_supplait)print(erreur_lait)
                if (boisson == 5 && stock_lait < 200 + calcul_supplait)print(erreur_lait)
                if (stock_sucre < qttesucre)print(erreur_sucre)
                Thread.sleep(2000)
                menu_principal()
              }
            }
          }
        }
      }
    }

    else if (sucre <= 1) { //si pas de sucre
      if (boisson == 1 && stock_poudre_cafe >= 8){
        stocksuffisant = true
      }
      else {
        if (boisson == 2 && stock_poudre_cafe >= 6 && stock_lait >= 100 + calcul_supplait) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && stock_poudre_cafe >= 6 && stock_lait >= 120 + calcul_supplait) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && stock_poudre_cafe >= 8 && stock_lait >= 150 + calcul_supplait) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && stock_poudre_cafe >= 12 && stock_lait >= 200 + calcul_supplait) {
                stocksuffisant = true
              }
              else if(!stocksuffisant){
                if ((boisson == 1 || boisson == 4) && stock_poudre_cafe < 8)print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && stock_poudre_cafe < 6)print(erreur_poudre)
                if (boisson == 5 && stock_poudre_cafe < 12)print(erreur_poudre)
                if (boisson == 2 && stock_lait < 100 + calcul_supplait)print(erreur_lait)
                if (boisson == 3 && stock_lait < 120 + calcul_supplait)print(erreur_lait)
                if (boisson == 4 && stock_lait < 150 + calcul_supplait)print(erreur_lait)
                if (boisson == 5 && stock_lait < 200 + calcul_supplait)print(erreur_lait)
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
    Thread.sleep(1000);print(".");Thread.sleep(1000);print(".");Thread.sleep(1000);print(".")
    print("\nMerci ! Votre paiement a été accepté.")
    statu_paiement = true
    //end paiement

    //si paiement effectuer MàJ stock
    if (statu_paiement) {
      if (sucre > 1) {
        if (boisson == 1 && stock_poudre_cafe >= 8 && stock_sucre >= qttesucre){
          stock_poudre_cafe -= 8
          stock_sucre -= qttesucre
        }
        else {
          if (boisson == 2 && stock_poudre_cafe >= 6 && stock_lait >= 100 + calcul_supplait && stock_sucre >= qttesucre) {
            stock_poudre_cafe -= 6
            stock_lait -= (100 + calcul_supplait)
            stock_sucre -= qttesucre
          }
          else {
            if (boisson == 3 && stock_poudre_cafe >= 6 && stock_lait >= 120 + calcul_supplait && stock_sucre >= qttesucre) {
              stock_poudre_cafe -= 6
              stock_lait -= (100 + calcul_supplait)
              stock_sucre -= qttesucre
            }
            else {
              if (boisson == 4 && stock_poudre_cafe >= 8 && stock_lait >= 150 + calcul_supplait && stock_sucre >= qttesucre) {
                stock_poudre_cafe -= 8
                stock_lait -= (150 + calcul_supplait)
                stock_sucre -= qttesucre
              }
              else {
                if (boisson == 5 && stock_poudre_cafe >= 12 && stock_lait >= 200 + calcul_supplait && stock_sucre >= qttesucre) {
                  stock_poudre_cafe -= 12
                  stock_lait -= (200 + calcul_supplait)
                  stock_sucre -= qttesucre
                }
                else if(!stocksuffisant){println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");Thread.sleep(2000); mode_client()}
              }
            }
          }
        }
      }
      else if (sucre <= 1) {
        if (boisson == 1 && stock_poudre_cafe >= 8){
          stock_poudre_cafe -= 8
        }
        else {
          if (boisson == 2 && stock_poudre_cafe >= 6 && stock_lait >= 100 + calcul_supplait) {
            stock_poudre_cafe -= 6
            stock_lait -= (100 + calcul_supplait)
          }
          else {
            if (boisson == 3 && stock_poudre_cafe >= 6 && stock_lait >= 120 + calcul_supplait) {
              stock_poudre_cafe -= 6
              stock_lait -= (100 + calcul_supplait)
            }
            else {
              if (boisson == 4 && stock_poudre_cafe >= 8 && stock_lait >= 150 + calcul_supplait) {
                stock_poudre_cafe -= 8
                stock_lait -= (150 + calcul_supplait)
              }
              else {
                if (boisson == 5 && stock_poudre_cafe >= 12 && stock_lait >= 200 + calcul_supplait) {
                  stock_poudre_cafe -= 12
                  stock_lait -= (200 + calcul_supplait)
                }
                else if(!stocksuffisant){println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");Thread.sleep(2000); mode_client()}
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
  }

  def mode_adminPIN(): Unit = {
    val PIN = "434343"
    println("===========Mode Admin===========\nEntrez le code PIN :\n> ")
    val tentativePIN = readLine()
    if (tentativePIN.length == 6) {
      var boolPIN = false
      for (i <- 0 until 6) {
        if (tentativePIN(i) == PIN(i)) {
          boolPIN = true
        }
      }
      if (boolPIN) {
        println(">> Accès autorisé.")
        Thread.sleep(1000)
        mode_admin()
      }
      else {
        println(">> Accès refusé.")
        Thread.sleep(1000)
        menu_principal()
      }
    }
    else {
      println(">> Accès refusé.")
      Thread.sleep(1000)
      menu_principal()
    }
  }

  def quitter(): Unit = {
    val mot = "Au revoir... "
    for (i <- mot){print(i);Thread.sleep(100)}
    Thread.sleep(1000)
  }

  def mode_admin(): Unit = {
    var ajout_poudre_cafe = 0
    var ajout_sucre = 0
    var ajout_lait = 0.0
    var convertionlait = 0.0
    convertionlait = stock_lait / 1000
    println("===========Mode Admin===========\nStock:\n    Poudre de café: "+stock_poudre_cafe+"g\n    Lait          : "+convertionlait+"L\n    Sucre         : "+stock_sucre+"g")
    print("Réapprovisionnement des stocks");Thread.sleep(500);print(".");Thread.sleep(500);print(".");Thread.sleep(500);print(".")
    print("\nAjout:\n    Pourdre de café: ")
    ajout_poudre_cafe = readInt()
    print("    Lait           : ")
    val convertion_inverse = readDouble()
    ajout_lait = convertion_inverse * 1000
    print("    Sucre          : ")
    ajout_sucre = readInt()
    stock_poudre_cafe += ajout_poudre_cafe
    stock_sucre += ajout_sucre
    stock_lait += ajout_lait
    print("Niveaux de stocks mis à jour.\nRetour au menu principal");Thread.sleep(500);print(".");Thread.sleep(500);print(".");Thread.sleep(500);print(".\n")
    menu_principal()
  }
}
