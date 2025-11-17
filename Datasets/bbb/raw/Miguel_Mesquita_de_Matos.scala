import io.StdIn._
import scala.util.Random

object Main {


  def validatePin(machineId: Int, machinepin: Array[Int]): Boolean = {
    var tentatives = 0
    val tentativesMax = 3

    while (tentatives < tentativesMax) {
      val pin = readLine("Inscrivez votre code d'admin pour la machine " + (machineId + 1) + " : \n").toInt

      if (pin == machinepin(machineId)) {
        println("Code correct.")
        return true
      } else {
        tentatives += 1
        if (tentatives < tentativesMax) {
          println("Code incorrect. Il vous reste " + (tentativesMax - tentatives) + " tentative(s).")
        } else {
          println("Trop de tentatives, la machine est bloquée.")
          System.exit(0)
        }
      }
    }
    false
  }

  def updatePin(machineId: Int, machinepin: Array[Int]): Unit = {
    val nouveaucode = readLine("Écrivez le nouveau code pour la machine " + (machineId  + 1 ) + " il doit contenir 6 chiffres : ")

    if (nouveaucode.length == 6 ) {
      machinepin(machineId) = nouveaucode.toInt
      println("Le nouveau code pour la machine " + ( machineId + 1 ) + " est : " + machinepin(machineId))
    } else {
      println("Le code doit contenir exactement 6 chiffres. Retour au menu principal.")
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Machine " + ( machineId  + 1 ) + "  en mode Client")
    var prixFinal: Double = 0.0
    var prixBoisson: Double = 0.0
    var prixSucre: Double = 0.0
    var prixLaitSupplementaire: Double = 0.0
    var commandeReussie: Boolean = false

    println("Veuillez choisir votre boisson : \n1) Expresso (2.00 CHF) \n2) Cappuccino (2.50 CHF) \n3) Latte (Petit: 2.70 CHF, Moyen: 3.20 CHF, Grand: 3.70 CHF)")
    var choixBoisson = readInt()
    while (choixBoisson < 1 || choixBoisson > 3) {
      println(" vous devez choisir un nombre entre 1 et 3 ")
      choixBoisson = readInt()
    }
    var cafenecessaire = 0
    var laitnecessaire = 0
    var laitsupp = 0
    var sucrenecessaire = 0


    if (choixBoisson == 1) {
      prixBoisson = 2.00
      cafenecessaire = 8
      println(" boisson selectionner = expresso ")
    } else if (choixBoisson == 2) {
      prixBoisson = 2.50
      cafenecessaire = 6
      laitnecessaire = 100
      println(" boisson selectionner = capucinno  ")
    } else if (choixBoisson == 3) {
      println("Choisissez la taille :\n  1) Petit (2.70 CHF) \n  2) Moyen (3.20 CHF) \n  3) Grand (3.70 CHF)")
      var taille = readInt()
      while (taille < 1 || taille > 3) {
        println(" vous devez choisir un nombre entre 1 et 3 ")
        taille = readInt()
      }

      if (taille == 1) {
        prixBoisson = 2.70
        cafenecessaire = 6
        laitnecessaire = 120
        println(" boisson selectionner = petit Latte ")
        commandeReussie = true
      } else if (taille == 2) {
        prixBoisson = 3.20
        cafenecessaire = 8
        laitnecessaire = 150
        println(" boisson selectionner = moyen latte ")
        commandeReussie = true

      } else if (taille == 3) {
        prixBoisson = 3.70
        cafenecessaire = 12
        laitnecessaire = 200
        println(" boisson selectionner = grand Latte ")
        commandeReussie = true

      }
    }
    if (coffeeStocks(machineId) < cafenecessaire) {
      println("Erreur : Stock insuffisant de cafe  pour préparer la boisson. veuillez essayer une autre boisson ou vérifiez les stocks dans l'admin ")
      return false
    }




    println("Souhaitez-vous ajouter du sucre ?")
    println(" \n 1) Non \n  2) Peu (0.10 CHF) \n  3) Moyen (0.20 CHF) \n  4) Beaucoup (0.30 CHF)")
    var choixSucre = readInt()
    while (choixSucre < 1 || choixSucre > 4) {
      println(" vous devez choisir un nombre entre 1 et 4 ")
      choixSucre = readInt()
    }


    if (choixSucre == 1 ) {
      println(" sucre = pas de sucre ")
      commandeReussie = true
    }
    else if (choixSucre == 2) {
      sucrenecessaire = 5
      prixSucre = 0.10
      println(" sucre = peu de sucre ")
      commandeReussie = true
    } else if (choixSucre == 3) {
      sucrenecessaire = 10
      prixSucre = 0.20
      println(" sucre = moyen de sucre ")
      commandeReussie = true
    } else if (choixSucre == 4) {
      sucrenecessaire = 15
      prixSucre = 0.30
      println(" sucre = beaucoup de sucre ")
      commandeReussie = true
    }

    if (sugarStocks(machineId) < sucrenecessaire) {
      println("Erreur : Stock insuffisant de sucre.veuillez rajouter moins de sucre ou vérifiez les stocks dans l'admin")
      return false
    }


    if (choixBoisson == 2 || choixBoisson == 3) {

      if (milkStocks(machineId) <  laitnecessaire)  {
        println("Erreur : Stock insuffisant de lait. veuillez verifier les stocks dans l'admin ou choisir une autre boisson")
        return false
      }


      println("Souhaitez-vous ajouter du lait supplémentaire ? (0.05 CHF par dose, max 3 doses)")
      println("1) Non  2) Oui")
      var ajoutLait = readInt()
      while (ajoutLait < 1 || ajoutLait > 2) {
        println("Vous devez choisir un nombre entre 1 et 2")
        ajoutLait = readInt()
      }

      if (ajoutLait == 1) {
        println("Pas d'ajout de lait supplémentaire.")
        commandeReussie = true
      } else if (ajoutLait == 2) {
        println("Combien de doses souhaitez-vous ajouter ? (1 à 3)")
        var laitSupplementaire = readInt()
        while (laitSupplementaire < 1 || laitSupplementaire > 3) {
          println("Vous devez choisir un nombre entre 1 et 3")
          laitSupplementaire = readInt()
        }


        if (laitSupplementaire == 1) {
          laitsupp = 50
          prixLaitSupplementaire = 0.05
          println(" 1 dose de lait ajouté ")
          commandeReussie = true
        } else if (laitSupplementaire == 2) {
          laitsupp = 100
          prixLaitSupplementaire = 0.10
          println(" 2 doses de laits ajoutées")
          commandeReussie = true

        } else if (laitSupplementaire == 3) {
          laitsupp = 150
          prixLaitSupplementaire = 0.15
          println(" 3 doses de laits ajoutées ")
          commandeReussie = true
        }
      }

      if (milkStocks(machineId) < (laitsupp + laitnecessaire))  {
        println("Erreur : Stock insuffisant de lait. veuillez ajoutez moins de doses de laits ou vérifiez les stocks dans l'admin ")
        return false
      }
    }

    coffeeStocks(machineId) -= cafenecessaire
    milkStocks(machineId) -= (laitnecessaire + laitsupp)
    sugarStocks(machineId) -= sucrenecessaire



    prixFinal = prixBoisson + prixSucre + prixLaitSupplementaire
    if ( prixBoisson > 0 && prixSucre > 0 && prixLaitSupplementaire > 0 ) {
      printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson) + %.2f CHF (sucre) + %.2f CHF ( lait)\n  ", prixFinal, prixBoisson, prixSucre, prixLaitSupplementaire)
    }
    else if ( prixBoisson > 0 && prixSucre == 0 && prixLaitSupplementaire > 0 ) {
      printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  + %.2f CHF ( lait)\n  ", prixFinal, prixBoisson,  prixLaitSupplementaire)
    }
    else if ( prixBoisson > 0 && prixSucre > 0 && prixLaitSupplementaire == 0 ) {
      printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  + %.2f CHF (sucre)\n  ", prixFinal, prixBoisson,  prixSucre)
    }
    else if ( prixBoisson > 0 && prixSucre == 0 && prixLaitSupplementaire == 0 ) {
      printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  \n  ", prixFinal, prixBoisson)
    }





    println("\n veuillez procédez au paiement par twint ")



    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code_twint = ""

    for (i <- 1 to 5) {
      val index_aléatoire = Random.nextInt(chars.length)
      code_twint = code_twint + chars(index_aléatoire)
    }


    println("Votre code twint  est : " + code_twint)

    println("(en attente de la confirmation de votre paiement) \n  Merci ! votre paiement est confirmé ")

    println(" préparation de votre boisson...")
    Thread.sleep(3000)

    if (choixBoisson == 1) {
      println(" votre expresso est prêt")
    } else if (choixBoisson == 2) {
      println(" votre capucinno est prêt")
    } else if (choixBoisson == 3) {
      println(" votre Latte est prêt")
    }


    true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Réapprovisionnement de la machine " + ( machineId + 1 )  )

    println("Stock actuel de poudre de café : " + coffeeStocks(machineId)+" g")
    println("Combien de grammes de poudre de café souhaitez-vous ajouter ?")
    var ajoutCafe = readInt()
    while (ajoutCafe < 0) {
      println("Erreur : La quantité ajoutée doit être positive.")
      ajoutCafe = readInt()
    }
    coffeeStocks(machineId) += ajoutCafe
    println("Nouveau stock de poudre de café : " + coffeeStocks(machineId)+" g")

    println("Stock actuel de sucre : "+sugarStocks(machineId)+ "g")
    println("Combien de grammes de sucre souhaitez-vous ajouter ?")
    var ajoutSucre = readInt()
    while (ajoutSucre < 0) {
      println("Erreur : La quantité ajoutée doit être positive.")
      ajoutSucre = readInt()
    }
    sugarStocks(machineId) += ajoutSucre
    println("Nouveau stock de sucre : " + sugarStocks(machineId)+ "  g")

    println("Stock actuel de lait : " + milkStocks(machineId) +" ml")
    println("Combien de décilitres de lait souhaitez-vous ajouter ?")
    var ajoutLait = readInt()
    while (ajoutLait < 0) {
      println("Erreur : La quantité ajoutée doit être positive.")
      ajoutLait = readInt()
    }
    milkStocks(machineId) += ajoutLait
    println("Nouveau stock de lait : "+ milkStocks(machineId)+ "  ml")

    println("Les stocks de la machine  "+ ( machineId + 1 ) +" ont été mis à jour avec succès.")
  }
  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    var stock_poudredecafe: Array[Int] = Array.fill(nbMachines)(50) // en grammes
    var stock_sucre: Array[Int] = Array.fill(nbMachines)(30) // en grammes
    var stock_lait: Array[Int] = Array.fill(nbMachines)(500) // en mililitre
    val machinepin: Array[Int] = Array.fill(nbMachines)(434343) // Code PIN initial

    println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n > ")
    var mode = readInt()
    while (mode < 1 || mode > 3) {
      println(" vous devez choisir un mode entre 1 et 3 ")
      println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n >")
      mode = readInt()
    }

    while (mode != 3) {
      println("\n veuillez sélectionner une machine (1 à " + nbMachines    + ") \n > ")
      var machineId = readInt() - 1
      while (machineId < 0 || machineId >= nbMachines) {
        println(" vous devez choisir une machine valide entre 1 et " + nbMachines    )
        machineId = readInt() - 1
      }

      if (mode == 1) {
        if (serveClient(machineId, stock_poudredecafe, stock_sucre, stock_lait)) {
          println("Merci d'avoir utilisé la machine Nospresso. À bientôt !")
        }
      } else if (mode == 2) {
        if (validatePin(machineId, machinepin)) {
          println("Veuillez choisir votre choix :\n 1) Réapprovisionnement des stocks\n 2) Mettre à jour le code PIN\n> ")

          var choix = readInt()
          while (choix < 1 || choix > 2) {
            println("Vous devez faire votre choix entre 1 et 2.")
            println("Veuillez choisir votre choix :\n 1) Réapprovisionnement des stocks\n 2) Mettre à jour le code PIN\n> ")

            choix = readInt()
          }

          if (choix == 1) {

            restockMachine(machineId, stock_poudredecafe, stock_sucre, stock_lait)

          } else if (choix == 2) {

            updatePin(machineId, machinepin)

          }
        }
      }

      println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n > ")
      mode = readInt()
      while (mode > 3 || mode < 1) {
        println(" vous devez choisir un mode entre 1 et 3 ")
        println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n >")
        mode = readInt()
      }
    }
  }
}
