object Main {
  val nbMachines = 5
  val stockCafe = Array(50, 50, 50, 50, 50)
  val stockSucre = Array(30, 30, 30, 30, 30)
  val stockLait = Array(500, 500, 500, 500, 500)
  var codesPIN = Array.fill(nbMachines)("434343")

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 0
    while (attempts < 3) {
      println("Entrez le code PIN :")
      val inputPin = readLine("> ")
      if (inputPin == machinePins(machineId)) {
        println(s"Accès accordé à la Machine ${machineId + 1}.")
        return true
      } else {
        attempts += 1
        println(s"Code PIN incorrect. ${(3 - attempts)} tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    sys.exit(1)
  }



  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${machineId + 1}")
    var newPin = ""
    do {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPin = readLine()
    } while (!newPin.matches("\\d{6}"))
    machinePins(machineId) = newPin
    println("Code PIN mis à jour avec succès.")
  }


  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], boisson: String): Boolean = {
    println(s"Préparation de $boisson pour la machine ${machineId + 1} en cours...")
    if (coffeeStocks(machineId) >= 8 && sugarStocks(machineId) >= 5 && milkStocks(machineId) >= 200) {
      coffeeStocks(machineId) -= 8
      sugarStocks(machineId) -= 5
      milkStocks(machineId) -= 200
      println(s"$boisson servi avec succès !")
      true
    } else {
      println(s"Stocks insuffisants pour préparer le $boisson.")
      false
    }
  }



  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var valid = false
    do {
      try {
        print("Poudre de café > ")
        val cafeAjoute = readLine().toInt

        print("Sucre > ")
        val sucreAjoute = readLine().toInt

        print("Lait > ")
        val laitAjouteLitres = readLine().replace(",", ".").toDouble
        val laitAjouteMillilitres = (laitAjouteLitres * 1000).toInt

        if (cafeAjoute >= 0 && sucreAjoute >= 0 && laitAjouteMillilitres >= 0) {
          coffeeStocks(machineId) += cafeAjoute
          sugarStocks(machineId) += sucreAjoute
          milkStocks(machineId) += laitAjouteMillilitres
          println("Les stocks ont été mis à jour avec succès.")
          valid = true
        } else {
          println("Veuillez entrer des quantités positives.")
        }
      } catch {
        case _: NumberFormatException => println("Entrée invalide. Veuillez entrer des nombres valides.")
      }
    } while (!valid)
  }



  def main(args: Array[String]): Unit = {


    def afficherStocks(machineId: Int): Unit = {
      println(f"- Poudre de café : ${stockCafe(machineId)}%.2f g")
      println(f"- Sucre : ${stockSucre(machineId)}%.2f g")
      println(f"- Lait : ${stockLait(machineId) / 1000.0}%.2f L")
    }


    def ajouterStock(machineId: Int): Unit = {
      println(s"\nEntrez les quantités à ajouter :")
      restockMachine(machineId, stockCafe, stockSucre, stockLait)
      afficherStocks(machineId)
      println("Retour au menu principal...")
      demarrerProgramme()
    }

    def verifierStock(machineId: Int, cafe: Double, lait: Double, sucre: Double): Boolean = {
      if (stockCafe(machineId) < cafe) {
        println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
        return false
      }
      if (stockLait(machineId) < lait / 1000.0) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        return false
      }
      if (stockSucre(machineId) < sucre) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        return false
      }
      true
    }

    def deduireStock(machineId: Int, cafe: Int, lait: Int, sucre: Int): Unit = {
      stockCafe(machineId) = stockCafe(machineId) - cafe
      stockSucre(machineId) = stockSucre(machineId) - sucre
      stockLait(machineId) = stockLait(machineId) - lait
      println("Stocks mis à jour après la transaction.")
      afficherStocks(machineId)
    }


    def selectionnerMachine(): Int = {
      var machineId = -1
      do {
        try {
          println(s"Machine sélectionnée (1-${nbMachines}) >")
          machineId = readLine("> ").toInt - 1
          if (machineId < 0 || machineId >= nbMachines) {
            println(s"Veuillez entrer un identifiant de machine valide (entre 1 et ${nbMachines}).")
          }
        } catch {
          case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
        }
      } while (machineId < 0 || machineId >= nbMachines)
      machineId
    }

    def demanderSucre(): (Double, Double) = {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")

      val choix = readLine().trim
      choix match {
        case "1" => (0.0, 0.0)
        case "2" => (5.0, 0.10)
        case "3" => (10.0, 0.20)
        case "4" => (15.0, 0.30)
        case _ =>
          println("Option invalide, aucun sucre ajouté.")
          (0.0, 0.0)
      }
    }


    def genererCodePaiement(): String = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      val random = new scala.util.Random
      var codePaiement = ""

      for (_ <- 1 to 5) {
        val indexAleatoire = random.nextInt(chars.length)
        codePaiement += chars(indexAleatoire)
      }

      codePaiement
    }

    def demarrerProgramme(): Unit = {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      var EntreeUtilisateur = 0
      do {
        try {
          EntreeUtilisateur = readLine("> ").toInt
          if (EntreeUtilisateur < 1 || EntreeUtilisateur > 3) {
            println("Veuillez entrer une valeur valide entre 1 et 3.")
          }
        } catch {
          case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre valide.")
        }
      } while (EntreeUtilisateur < 1 || EntreeUtilisateur > 3)

      if (EntreeUtilisateur == 2) {
        val machineId = selectionnerMachine()
        if (validatePin(machineId, codesPIN)) {
          println("Souhaitez-vous mettre à jour le PIN ?\n1) oui\n2) non")
          val pinChoix = readLine("> ").trim
          if (pinChoix == "1") {
            updatePin(machineId, codesPIN)
            println("Retour au menu principal...")
            demarrerProgramme()
            return
          }

          println("\nNiveaux de stocks actuels :")
          afficherStocks(machineId)

          println("\nSouhaitez-vous ajouter des stocks ?\n1) oui\n2) non")
          val stockChoix = readLine("> ").trim
          if (stockChoix == "1") {
            ajouterStock(machineId)
          }

          println("Retour au menu principal...")
          demarrerProgramme()
        } else {
          println("Retour au menu principal.")
          demarrerProgramme()
        }
      }


      if (EntreeUtilisateur == 1) {
        val machineId = selectionnerMachine()
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

        var PrixBase = 0.0
        var laitSupplementaire = 0.0
        var PrixLait = 0.0
        var sucreSupplementaire = 0.0
        var PrixSucre = 0.0

        val choixBoisson = readLine("> ")
        choixBoisson match {
          case "1" =>
            PrixBase = 2.00

          case "2" =>
            PrixBase = 2.50
            println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
            print("Nombre de doses > ")
            laitSupplementaire = readLine().toDouble * 50
            PrixLait = laitSupplementaire / 50 * 0.05

          case "3" =>
            println("Veuillez sélectionner la taille :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            val taille = readLine("> ")
            taille match {
              case "1" => PrixBase = 2.70
              case "2" => PrixBase = 3.20
              case "3" => PrixBase = 3.70
            }
            println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
            print("Nombre de doses > ")
            laitSupplementaire = readLine().toDouble * 50
            PrixLait = laitSupplementaire / 50 * 0.05

          case _ => println("Option invalide.")
        }

        val (sucreQuantite, sucrePrix) = demanderSucre()
        sucreSupplementaire = sucreQuantite
        PrixSucre = sucrePrix

        val PrixTotal = BigDecimal(PrixBase + PrixLait + PrixSucre).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
        println(f"Prix total : CHF $PrixBase%.2f + CHF $PrixLait%.2f + CHF $PrixSucre%.2f = CHF $PrixTotal%.2f")

        val codePaiement = genererCodePaiement()
        println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codePaiement \n(En attente de paiement...)")
        Thread.sleep(3000)
        println("\nPaiement confirmé..")
        println("Préparation de votre boisson...")

        println("Votre " + (choixBoisson match {
          case "1" => "Expresso"
          case "2" => "Cappuccino"
          case "3" => "Latte"
          case _ => "Boisson"
        }) + " est prêt ! Bonne dégustation !\n")

        deduireStock(machineId, 8, laitSupplementaire.toInt, sucreSupplementaire.toInt)
        demarrerProgramme()
      }
    }

    demarrerProgramme()
  }
}
//vfinale