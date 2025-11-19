import scala.io.StdIn._
import scala.util.Random

object Main {

  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")

  // SI = stocks initiaux
  var SIpoudrecafe = Array.fill(5)(50) // grammes
  var SIsucre = Array.fill(5)(30) // grammes
  var SIlait = Array.fill(5)(500) // ml

  // Gestion des codes PIN
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var nbdetentative = 3
    var tentativerestantes = 0
    while (nbdetentative > 0) {
      println(" Entrez le code PIN: ******")
      println("<")
      var codePIN = readLine()
      if (codePIN == machinePins(machineId)) {
        println("Code Pin correct")
        return true
      }
      else {
        nbdetentative -= 1
        tentativerestantes =  nbdetentative
        if (tentativerestantes > 0) {
          println(" Code Pin incorrect, il vous reste " + tentativerestantes + " tentatives")
          println("<")
        }
      }
    }
    println("Trop de tentative échouées.")
    return false
  }

  // Méthode pour changer le code PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Veuillez entrer le nouveau code Pin à 6 chiffres")
    println("<")
    var nouveaucodePIN = readLine()
    while (nouveaucodePIN.length != 6 || !nouveaucodePIN.forall(_.isDigit)) {
      println("Saisie invalide : Veuillez sélectionner un autre mot de passe à 6 chiffres")
      println("<")
      nouveaucodePIN = readLine() }

      machinePins(machineId) = nouveaucodePIN
      println(" Le code PIN a été mis à jour")
  }

  // Méthode pour remplir les stocks
  def restockMachine(machineId: Int, SIpoudrecafe: Array[Int], SIsucre: Array[Int], SIlait: Array[Int]): Unit = {
    println("Affichage des stocks :")
    println("Poudre de café: " + SIpoudrecafe(machineId) + "g")
    println("Lait: " + (SIlait(machineId).toDouble / 1000) + "L")
    println("Sucre: " + SIsucre(machineId) + "g")
    println(" Réapprovisionnement des stocks...")
    println(" Ajout : ")

    println(" Poudre à café (en grammes):")
    var remplissagecafe = readInt()
    while (remplissagecafe < 0){
      println("Veuillez entrer une valeur positive")
      remplissagecafe = readInt()
    }
    SIpoudrecafe(machineId) += remplissagecafe

    println(" Lait (en litres):")
    var remplissagelait = (readDouble()*1000).toInt
    while (remplissagelait < 0) {
      println(" Veuillez entrer une valeur positive")
      remplissagelait = (readDouble()*1000).toInt
    }
    SIlait(machineId) += remplissagelait

    println(" Sucre (en grammes):")
    var remplissagesucre = readInt()
    while (remplissagesucre < 0){
      println("Veuillez entrer une valeur positive")
      remplissagesucre = readInt()
    }
    SIsucre(machineId) += remplissagesucre
    println("Niveau de stock mis à jour")
  }


  // Méthode pour servir le client
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var result = true
    // mode à selectionner
    var mode = 0.0
    // définir type de boisson
    var boisson = 0.0
    // quantité sucre
    var nbsucre = 0
    // prix sucre
    var prixsucre = 0.0
    // prix lait
    var prixdoselait = 0.0

    // prix boisson
    var prixtaillelatte = 0.0
    var prixexpresso = 2.00
    var prixcappuccino = 2.50
    var taillelatte = 0

    var poudrecafe = 0
    var quantitesucre = 0
    var lait = 0
    var doselait = 0
    var supplementlait = 0.0
    var quantitelait = 0
    var prixfinal = 0.0

    // Paiement Twint
    var code = ""
    val nbcaractere = 5
    val valeuralphanumeric = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


        doselait = 0
        quantitesucre = 0
        nbsucre = 0
        quantitelait = 0
        println("Vous avez sélectionner le mode client.")
        println(" Quelle boisson désirez-vous ?")
        println(" 1.1) Expresso - CHF 2.00")
        println(" 1.2) Cappuccino - CHF 2.50")
        println(" 1.3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand ")
        println(">")
        boisson = readDouble()
        while ((boisson != 1.1) && (boisson != 1.2) && (boisson != 1.3)) {
          println(" La valeur sélectionnée est incorrect, réssayer")
          println(">")
          boisson = readDouble()
        }
        // expresso
        if (boisson == 1.1) {
          poudrecafe = 8 // grammes
          quantitesucre = 0
          quantitelait = 0
          prixexpresso = 2.0 //CHF
          println("Quelle quantité de sucre desirez-vous?")
          println(" 1) un peu; 5g - 0.10CHF")
          println(" 2) moyen; 10g - 0.20CHF")
          println(" 3) beaucoup; 15g - 0.30CHF")
          println(" 4) pas de sucre ")
          println(">")
          quantitesucre = readInt()
          while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            quantitesucre = readInt()
          }
          if (quantitesucre == 1) {
            prixsucre = 0.10
            nbsucre = 5 // grammes
          }
          else if (quantitesucre == 2) {
            prixsucre = 0.20
            nbsucre = 10 // grammes
          }
          else if (quantitesucre == 3) {
            prixsucre = 0.30
            nbsucre = 15 // grammes
          }
          else if (quantitesucre == 4) {
            prixsucre = 0.0
          }
        }
        // cappuccino
        if (boisson == 1.2) {
          poudrecafe = 6 // grammes
          lait = 100// ml
          nbsucre = 0
          doselait = 0
          prixcappuccino = 2.50 // CHF

          println(" Quelle quantité desirez-vous?")
          println("1) un peu 5g- 0.10CHF")
          println("2) moyen 10g - 0.20CHF")
          println("3) beaucoup 15g - 0.30CHF")
          println("4) pas de sucre")
          println(">")
          quantitesucre = readInt()
          while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            quantitesucre = readInt()
          }
          if (quantitesucre == 1) {
            prixsucre = 0.10 // CHF
            nbsucre = 5 //grammes
          }
          else if (quantitesucre == 2) {
            prixsucre = 0.20 // CHF
            nbsucre = 10 // grammes
          }
          else if (quantitesucre == 3) {
            prixsucre = 0.30 // CHF
            nbsucre = 15 // grammes
          }
          else if (quantitesucre == 4) {
            prixsucre = 0.0 // CHF
          }
          println(" Voulez vous un supplément lait ?")
          println("1) oui")
          println("2) non")
          println(">")
          supplementlait = readInt()
          while ((supplementlait != 1) && (supplementlait != 2)) {
            println(" La valeur sélectionnée est incorrect, réssayer")
            println(">")
            supplementlait = readInt()
          }
          if (supplementlait == 1) {
            println(" Combien de dose? (maximum 3)")
            println("1)une dose, 50ml- 0.05CHF")
            println("2) deux dose 100ml- 0.10 CHF")
            println("3) trois doses 150ml- 0.15 CHF")
            println(">")
            doselait = readInt()
            while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              doselait = readInt()
            }
            if (doselait == 1) {
              doselait = 50
              prixdoselait = 0.05
              quantitelait = doselait + lait
            }
            else if (doselait == 2) {
              doselait = 50 * 2
              prixdoselait = 0.1
              quantitelait = doselait + lait
            }
            else if (doselait == 3) {
              doselait = 50 * 3
              prixdoselait = 0.15
              quantitelait = doselait + lait
            }
          } else {
            prixdoselait = 0.0
            quantitelait = lait
          }
        }
        // latte
        if (boisson == 1.3) {
          println("Quelle taille desirez-vous ?")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          println(">")
          taillelatte = readInt()

          while ((taillelatte != 1) && (taillelatte != 2) && (taillelatte != 3)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            taillelatte = readInt()
          }
          if (taillelatte == 1) {
            poudrecafe = 6 //g
            lait = 120 //ml
            prixtaillelatte = 2.70 // CHF

          } else if (taillelatte == 2) {
            poudrecafe = 8 // grammes
            lait = 150 // ml
            prixtaillelatte = 3.20 // CHF
          }
          else if (taillelatte == 3) {
            poudrecafe = 12 //grammes
            lait = 200 //ml
            prixtaillelatte = 3.70 // CHF
          }
          quantitesucre = 0
          doselait = 0

          println(" Quelle quantité de sucre desirez-vous? ")
          println("1) un peu 5g- 0.10CHF")
          println("2) moyen 10g - 0.20CHF")
          println("3) beaucoup 15g - 0.30CHF")
          println("4) pas de sucre")
          println(">")
          quantitesucre = readInt()
          while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            quantitesucre = readInt()
          }
          if (quantitesucre == 1) {
            prixsucre = 0.10
            nbsucre = 5
          }
          else if (quantitesucre == 2) {
            prixsucre = 0.20
            nbsucre = 10
          }
          else if (quantitesucre == 3) {
            prixsucre = 0.30
            nbsucre = 15
          }
          else if (quantitesucre == 4) {
            prixsucre = 0.0
          }
          println(" Voulez vous un supplément lait ?")
          println("1) oui ")
          println("2) non ")
          println(">")
          supplementlait = readDouble()
          while ((supplementlait != 1) && (supplementlait != 2)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            supplementlait = readDouble()
          }
          if (supplementlait == 1) {
            println(" Combien de dose? (maximum 3))")
            println("1) une dose (50ml)")
            println("2) deux dose (100ml)")
            println("3) trois doses (150ml)")
            println(">")
            doselait = readInt()
            while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              doselait = readInt()
            }
            if (doselait == 1) {
              doselait = 50 //ml
              prixdoselait = 0.05 // CHF
              quantitelait = doselait + lait
            }
            else if (doselait == 2) {
              doselait = 50 * 2 //ml
              prixdoselait = 0.1 //CHF
              quantitelait = doselait + lait
            }
            else if (doselait == 3) {
              doselait = 50 * 3 //ml
              prixdoselait = 0.15 // CHF
              quantitelait = doselait + lait
            }
          } else {
            quantitelait = lait
            prixdoselait = 0.0
          }
        }
        println()
        if (boisson == 1.1) println("Votre boisson: Expresso")
        if (boisson == 1.2) println("Votre boisson: Cappuccino")
        if (boisson == 1.3) println("Votre boisson: Latte")
        if (quantitesucre == 1) println("Niveau sucre: Un peu (5g)")
        if (quantitesucre == 2) println("Niveau sucre: Moyen (10g)")
        if (quantitesucre == 3) println("Niveau de sucre: Beaucoup (15g)")
        if (quantitesucre == 4) println("Niveau de sucre: Aucun")
        if ((boisson == 1.2) && (supplementlait == 1)) {
          println("Supplément lait: Oui")
        }
        if ((boisson == 1.2) && (supplementlait == 2)) {
          println("Supplément lait: Non")
        }
        if ((boisson == 1.3) && (supplementlait == 1)) {
          println("Supplément lait: Oui")
        }
        if ((boisson == 1.3) && (supplementlait == 2)) {
          println("Supplément lait: Non")
        }
        if (boisson == 1.1) {
          prixfinal = prixexpresso + prixsucre
          printf("Le prix de votre expresso est de %.2f + %.2f = %.2f CHF\n", prixexpresso, prixsucre, prixfinal)
        }
        if (boisson == 1.2) {
          prixfinal = prixcappuccino + prixsucre + prixdoselait
          printf("Le prix de votre cappuccino est de %.2f + %.2f + %.2f = %.2f CHF\n", prixcappuccino, prixsucre, prixdoselait, prixfinal)
        }
        if (boisson == 1.3) {
          prixfinal = prixtaillelatte + prixsucre + prixdoselait
          printf("Le prix de votre latte est de %.2f + %.2f  + %.2f = %.2f CHF\n", prixtaillelatte, prixsucre, prixdoselait, prixfinal)
        }

        if (((boisson == 1.1) && (coffeeStocks(machineId) >= poudrecafe) && (sugarStocks(machineId) >= nbsucre)) || ((boisson == 1.2) && (coffeeStocks(machineId) >= poudrecafe) && (sugarStocks(machineId) >= nbsucre) && (milkStocks(machineId) >= quantitelait)) || ((boisson == 1.3) && (coffeeStocks(machineId) >= poudrecafe) && (sugarStocks(machineId) >= nbsucre) && (milkStocks(machineId) >= quantitelait))) {
          println("Le stock est suffisant, veuillez payer en utilisant Twint")
          result = true

          // paiement twint
          code = ""
          for (i <- 1 to nbcaractere) {
            var caracterealeatoire = valeuralphanumeric(Random.nextInt(valeuralphanumeric.length))
            code += caracterealeatoire
          }
          if ((boisson == 1.1) || (boisson == 1.2) || (boisson == 1.3)) {
            println(" Votre code de paiement est :" + code)
            println("En attente de validation du paiement...")
            Thread.sleep(3000)
            println("Paiement accepté, merci")
            println("Préparation de votre boisson...")
            if (boisson == 1.1) {
              println("Votre expresso est prêt! Bonne dégustation !")
            }
            if (boisson == 1.2) {
              println("Votre capppuccino est prêt! Bonne dégustation !")
            }
            if (boisson == 1.3) {
              println("Votre latte est prêt! Bonne dégustation! ")
            }
            // déduction stocks
            coffeeStocks(machineId) -= poudrecafe
            sugarStocks(machineId) -= nbsucre
            milkStocks(machineId) -= quantitelait

          }
        }
        else {
          result = false
          if (poudrecafe > coffeeStocks(machineId)) {
            println()
            println("Erreur: Stock de café insuffisant")
            println("Veuillez choisir une autre boisson, une plus petite ou une autre machine")
          }
          if (nbsucre > sugarStocks(machineId)) {
            println()
            println("Erreur: Stock de sucre insuffisant")
            println("Veuillez choisir une autre boisson, moins de sucre ou une autre machine")
          }
          if (quantitelait > milkStocks(machineId)) {
            println()
            println("Erreur: Stock de lait insuffisant")
            println("Veuillez choisir une autre boisson, une plus petite, moins de lait ou une autre machine")
          }
        }

    return result
  }

  def main(args: Array[String]): Unit = {
    // mode à selectionner
    var machineId = 0
    var mode = 0
    var changerlecode = 0
    var approvisionnementstock = 0
    var verification = false
    while (mode != 3) {
      verification = false
      println()
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode:")
      println("1)Client")
      println("2)Admin")
      println("3)Quitter")
      println(">")
      mode = readInt()
      while ((mode != 1) && (mode != 2) && (mode != 3)) {
        println(" sélection incorrect, choissisez une autre valeur")
        println(">")
         mode = readInt()
      }
     if (mode == 1) {

         while (!verification) {
           println(" Vous avez choisi le mode client.")
           println("Choisissez une machine (de 1 à 5)")
           machineId = readInt() - 1

           while ((machineId != 0) && (machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4)) {
             println("La valeur sélectionnée est incorrect, réssayer")
             println(">")
             machineId = readInt() - 1
           }
           verification = serveClient(machineId, SIpoudrecafe, SIsucre, SIlait)
         }

     }

      // mode admin
      if (mode == 2) {
        println("Vous avez choisi le mode admin.")
        println("Choisissez une machine (de 1 à 5)")
        machineId = readInt() - 1
        while ((machineId != 0) && (machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4)) {
          println(" La valeur sélectionnée est incorrect, réssayer")
          println(">")
          machineId = readInt() - 1
        }
        // Gestion des codes PIN
        var reussite2 = validatePin(machineId, machinePins)

        if (reussite2) {
          println("Voulez-vous changez de code PIN ? 1)oui 2)non ")
          changerlecode = readInt()
          while ((changerlecode != 1) && (changerlecode != 2)) {
            println("Valeur incorrect, veuillez saisir une autre valeur ")
            changerlecode = readInt()
          }
          if (changerlecode == 1) {
            // Méthode pour changer le code PIN
            updatePin(machineId, machinePins)
          }

          println(" Voulez-vous réapprovisionner les stocks ? 1) oui 2) non")
          approvisionnementstock = readInt()
          while ((approvisionnementstock != 1) && (approvisionnementstock != 2)) {
            println("Valeur incorrect, veuillez saisir une autre valeur ")
            approvisionnementstock = readInt()
          }

          if (approvisionnementstock == 1) {
            // Méthode pour remplir les stocks
            restockMachine(machineId, SIpoudrecafe, SIsucre, SIlait)
          }
        } else mode = 3
      }

        if (mode == 3)  {
          println(" Fin du programme ")

        }

    }
  }
}









