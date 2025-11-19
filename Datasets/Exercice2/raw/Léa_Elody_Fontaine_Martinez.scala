import io.StdIn._
object Main {
  var quitter = false
  val nbMachines = 5
  val PIN_in = "434343"
  val coffeeStocks = Array(0, 50, 50, 50, 50, 50)
  val sugarStocks = Array(0, 30, 30, 30, 30, 30)
  val milkStock = Array(0, 500, 500, 500, 500, 500)
  val machinePins = Array.fill(nbMachines)(PIN_in)

  var assez_lait = false
  var assez_sucre = false

  //Conso
  val conso_cafe = Array (0, 8, 6, 6, 8, 12)
  val conso_lait = Array(0, 0, 100, 120, 150, 200)
  val conso_sucre = Array(0, 0, 5, 10, 15)
  val conso_lait_sup = Array(0, 50, 100, 150)

  // prix
  val prix_cafe = Array(0.00, 2.00, 2.50, 2.70, 3.20, 3.70)
  val prix_sucre = Array( 0.00, 0.10, 0.20, 0.30)
  val prix_lait_supp = Array( 0.00, 0.05, 0.10, 0.15)

  var choix_client_sucre = 0
  var choix_client_lait = 0
  var choix_client = 0

  def validatePin(machineId : Int): Boolean ={
    var tentative = 3
    while (tentative>0){
      println("Entrez le code PIN pour la machine " + machineId)
      val PIN = readLine(">")
      if (PIN == machinePins(machineId-1)) return true
      tentative -= 1
      println("Code PIN incorrect. "+ tentative + " tentative(s) restante(s)")
    }
    println("Trop de tentatives échouées. Fin du programme. ")
    quitter = true
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = readLine("Mise à jour du code PIN pour la machine " + machineId + ".\nEntrez un nouveau code PIN à 6 chiffres >")
    while (newPin.length != 6) {
      newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(machineId-1)= newPin
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks:Array[Int], milkStocks: Array[Int]): Unit = {
    println("Entrez les quantités à ajouter :")
    var ajout_cafe = readLine("Poudre de café > ").toInt
    var ajout_sucre = readLine("Sucre > ").toInt
    var ajout_lait = ((readLine("Lait > ").toDouble)*1000).toInt
    while (ajout_cafe <0 || ajout_lait<0 || ajout_sucre<0){
      println("Les quantités ajoutées doivent être positives.")
      ajout_cafe = readLine("Poudre de café > ").toInt
      ajout_sucre = readLine("Sucre > ").toInt
      ajout_lait = ((readLine("Lait > ").toDouble)*1000).toInt
    }
    coffeeStocks(machineId) += ajout_cafe
    sugarStocks(machineId) += ajout_sucre
    milkStocks(machineId) += ajout_lait
    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var recap_prix_cafe = 0.0
    var recap_prix_sucre = 0.0
    var recap_prix_lait = 0.0
    println("Machine " + machineId + " sélectionnée.")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00 ")
    println("2) Cappuccino - CHF 2.50 ")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    choix_client = readLine("> ").toInt

    while (!(choix_client == 1 || choix_client == 2 || choix_client == 3)) {
      print("Votre sélection n'est pas correcte, choisissez entre 1) Expresso - CHF 2.00  2) Cappuccino - CHF 2.50  3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
      choix_client = readLine("> ").toInt
    }

    var boisson_reussie = false
    var erreur = true
    var prix_total = 0.0
    var choix_latte_taille = 0
    if (choix_client == 1) {
      if (coffeeStocks(machineId) >= conso_cafe(choix_client)) {
        boisson_reussie = true
        assez_lait = true
        erreur = false
      }
      else {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
    }

    else if (choix_client == 2) {
      if ((coffeeStocks(machineId) >= conso_cafe(choix_client)) && (milkStocks(machineId) >= conso_lait(choix_client))) {
        boisson_reussie = true
        erreur = false
      }
      else if (coffeeStocks(machineId) < conso_cafe(choix_client)) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
      else if (milkStocks(machineId) < conso_lait(choix_client)) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
    }

    else if (choix_client == 3) {
      println("Quelle taille de latté souhaitez-vous ? ")
      println("1) Petit - CHF 2.70 ")
      println("2) Moyen - CHF 3.20 ")
      println("3) Grand - CHF 3.70 ")
      choix_latte_taille = readLine("> ").toInt
      //vérif des données valides
      while (!(choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1) Petit - CHF 2.70  2) Moyen - CHF 3.20  3) Grand - CHF 3.70")
        choix_latte_taille = readLine("> ").toInt
      }

      if (choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3) {
        if ((coffeeStocks(machineId) >= conso_cafe(choix_latte_taille + 2)) && (milkStocks(machineId) >= conso_lait(choix_latte_taille + 2))) {
          boisson_reussie = true
          erreur = false
        }

        else if ((coffeeStocks(machineId) < conso_cafe(choix_latte_taille + 2)) && (milkStocks(machineId) >= conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
          erreur = true
          return false
        }
        else if ((coffeeStocks(machineId) >= conso_cafe(choix_latte_taille + 2)) && (milkStocks(machineId) < conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
          erreur = true
          return false
        }
        else if ((coffeeStocks(machineId) < conso_cafe(choix_latte_taille + 2)) && (milkStocks(machineId) < conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          erreur = true
          return false
        }
      }
    }
    //SUCRE
    if (boisson_reussie) {
      println("Souhaitez-vous du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10 ")
      println("3) Moyen (10g) - CHF 0.20 ")
      println("4) Beaucoup (15g) - CHF 0.30")
      choix_client_sucre = readLine("> ").toInt

      while (!(choix_client_sucre == 1 || choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1)  Sans sucre  2) Peu (5g) - CHF 0.10  3) Moyen (10g) - CHF 0.20  4) Beaucoup (15g) - CHF 0.30 ")
        choix_client_sucre = readLine("> ").toInt
      }

      if (choix_client_sucre == 1) {
        assez_sucre = true
      }

      else if (choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4) {
        if (sugarStocks(machineId) < conso_sucre(choix_client_sucre)) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          assez_sucre = false
          erreur = true
          return false
        }
        else {
          assez_sucre = true
          erreur = false
        }
      }

      // LAIT SUPP
      if (boisson_reussie && assez_sucre) {
        if ((choix_client == 2) || (choix_client == 3)) {
          println("Souhaitez-vous ajouter du lait en supplément ? ")
          println("1) Oui ")
          println("2) Non ")
          choix_client_lait = readLine("> ").toInt


          while (!(choix_client_lait == 1 || choix_client_lait == 2)) {
            print("Votre sélection n'est pas correcte, choisissez entre 1) Oui  2) Non ")
            choix_client_lait = readLine("> ").toInt
          }
          //cbn doses
          if (choix_client_lait == 1) {
            println("Combien de dose(s) ? ")
            var dose_lait_choix = readLine("> ").toInt

            while (!(dose_lait_choix == 1 || dose_lait_choix == 2 || dose_lait_choix == 3)) {
              print("Votre sélection n'est pas correcte, choisissez entre 1, 2 ou 3 doses. ")
              dose_lait_choix = readLine("> ").toInt
            }

            if (dose_lait_choix == 1 || dose_lait_choix == 2 || dose_lait_choix == 3) {
              if (milkStocks(machineId) < conso_lait_sup(dose_lait_choix)) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                assez_lait = false
                erreur = true
                return false
              }
              else {
                milkStocks(machineId) -= conso_lait_sup(dose_lait_choix)
                prix_total += prix_lait_supp(dose_lait_choix)
                recap_prix_lait += prix_lait_supp(dose_lait_choix)
                assez_lait = true
                erreur = false
              }
            }
          }
          else {
            assez_lait = true
          }
        }
      }
    }

    if (boisson_reussie && assez_sucre && assez_lait) {
      erreur = false
      val cafe = Array("Expresso","Cappuccino")
      val latte = Array("Latté (Petit)", "Latté (Moyen)", "Latté (Grand)" )

      //expresso ou cappuccino
      if (choix_client == 1 || choix_client==2) {
        coffeeStocks(machineId) -= conso_cafe(choix_client)
        prix_total += prix_cafe(choix_client)
        recap_prix_cafe += prix_cafe(choix_client)
        println("Boisson sélectionnée : " + cafe(choix_client-1) )
      }
      //latte
      else {
        if (choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3) {
          coffeeStocks(machineId) -= conso_cafe(choix_latte_taille+2)
          milkStocks(machineId) -= conso_lait(choix_latte_taille+2)
          prix_total += prix_cafe(choix_latte_taille + 2)
          recap_prix_cafe += prix_cafe(choix_latte_taille + 2)
          println("Boisson sélectionnée : "+latte(choix_latte_taille-1))
        }
      }
      //sucre
      val sucre = Array("Peu", "Moyen", "Beaucoup")
      if (choix_client_sucre == 1) {
        println("Niveau de sucre : Sans sucre ")
      }
      else if (choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4) {
        sugarStocks(machineId) -= conso_sucre(choix_client_sucre)
        prix_total += prix_sucre(choix_client_sucre-1)
        recap_prix_sucre += prix_sucre(choix_client_sucre-1)
        println("Niveau de sucre : "+ sucre(choix_client_sucre-2))
      }

      //lait
      if (choix_client==2 || choix_client==3) {
        if (choix_client_lait==1){
          println("Lait supplémentaire: Oui ")
        }
        else {
          println("Lait supplémentaire: Non ")
        }
      }

      if ((recap_prix_sucre > 0) && (recap_prix_lait > 0)) {
        printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, recap_prix_lait, prix_total)
      }
      //sucre
      else if (recap_prix_sucre > 0) {
        printf("Prix total: CHF %.2f + CHF %.2f  = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, prix_total)
      }
      //lait
      else if (recap_prix_lait > 0) {
        printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_lait, prix_total)
      }
      // ni lait ni sucre
      else if ((recap_prix_sucre == 0) && (recap_prix_lait == 0)) {
        printf("Prix total: = CHF %.2f\n ",  prix_total)
      }

      //Paiement
      val alphanum = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      val codeLength = 5 // Longueur du code Twint
      val code_twint = Array.fill(codeLength) {
        alphanum((math.random() * alphanum.length).toInt)
      }.mkString

      println()
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + code_twint)
      println("En attente de paiement...")
      println()
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println()
      // Préparation de la boisson
      println("Préparation de votre boisson...")
      println("[...]")
      println("Votre Cappuccino est prêt ! Bonne dégustation !")
      println()
      return true
    }
    return false
  }


  def main(args: Array[String]): Unit = {
    while (!quitter) {
      //Menu d'accueil
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choix = readLine("> ").toInt

      while (!(choix == 1 || choix == 2 || choix == 3)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1) Client 2) Admin 3) Quitter ")
        choix = readLine("> ").toInt
      }
      if (choix==1) {
        var boissonServie = false
        do {
          println("Veuillez choisir une machine (1-5)")
          var machineId = readLine("> ").toInt

          if (machineId < 1 || machineId > nbMachines) {
            print("Votre sélection n'est pas correcte, choisissez une machine entre 1 et 5 ")
          }
          else {
            boissonServie = serveClient(machineId, coffeeStocks, sugarStocks, milkStock)
          }
        } while (!boissonServie)
      }

      if (choix==2){
        println("Veuillez sélectionner une machine")
        var machineId = readLine("> ").toInt
        while (!(machineId == 1 || machineId == 2 || machineId == 3 || machineId == 4 || machineId == 5)) {
          print("Votre sélection n'est pas correcte, choisissez une machine entre 1 et 5 ")
          machineId = readLine("> ").toInt
        }
        if (validatePin(machineId)) {
          println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN")
          var choix_admin = readLine(">").toInt
          while (!(choix_admin == 1 || choix_admin == 2)) {
            print("Votre sélection n'est pas correcte, choisissez entre 1 ou 2 ")
            choix_admin = readLine("> ").toInt
          }
          if (choix_admin == 1) {
            println("Stocks: ")
            println("    Poudre de cafe: " + coffeeStocks(machineId) + "g")
            printf("    Lait          :%.2fL", milkStock(machineId) / 1000.0)
            println()
            println("    Sucre         : " + sugarStocks(machineId) + "g")
            println()
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStock)
          }
          else {
            updatePin(machineId, machinePins)
          }
        }
      }

      if (choix == 3){
        println("Vous avez quitté le programme.")
        quitter = true
      }
    }
  }
}