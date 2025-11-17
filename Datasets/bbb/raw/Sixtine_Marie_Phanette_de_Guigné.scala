import scala.io.StdIn._
import scala.util.Random

object Main {

  val nbMachines = 5
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  val machinePins = Array.fill(nbMachines)("434343")

  def main(args: Array[String]): Unit = {

    val programme = true
    while (programme) {
      var machineId = -1

        println ("Nospresso Café")
        println ("Veuillez sélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println ("3) Quitter")
        println (">")

      val choix = readInt()

      if (choix == 1) { // Pour rentrer dans le Menu Client
        while (machineId < 0 || machineId >= nbMachines){
          println("\nSélectionnez une machine (1 - 5) > ")
          machineId = readInt() - 1
          if (machineId >= nbMachines || machineId < 0) {
            println("Machine non reconnu. Veuillez réessayer.")
          }
        }
        if (serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
        }
      }
      else if (choix == 2) {  // Pour rentrer dans le Mode Admin
        while (machineId < 0 || machineId >= nbMachines){
          println("\nSélectionnez une machine (1 - 5) > ")
          machineId = readInt() - 1
          if (machineId >= nbMachines || machineId < 0) {
            println("Machine non reconnu. Veuillez réessayer.")
          }
        }
        println("Entrée le code PIN :")
        println (">")
        if (validatePin(machineId, machinePins)) {
          println ("\n1) Réapprovisionnement des stocks")
          println ("2) Mise à jour du code PIN")

          val choixmodeAdmin = readInt()

          if (choixmodeAdmin == 1) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)

          } else if (choixmodeAdmin == 2) {
            updatePin(machineId, machinePins)
          }
        }
      }

      else if (choix == 3){ // Mode Quitter : Fin du programme
        return false
      }
      else {
        println("Choix non valide. Veuillez réessayer")
        Thread.sleep(1000)
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      val codePin = readLine()
      if (codePin == machinePins(machineId)) {
        println("Accès accordé à la Machine sélectionnée ")
        Thread.sleep(1000)
        return true
      }
      else {
        tentatives -= 1
        println(s"Code PIN incorrect. $tentatives tentatives restantes >.")
      }
    }
    println("\nTrop de tentatives essayées. Fin du programme.")
    println ("\nProcess finished with exit code 0")
    Thread.sleep(1000000000)
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine séléctionnée.")

    var MAJ = true
    while (MAJ) {

      println("Entrez un nouveau code PIN à 6 chiffres >")

      val nouveaucodePIN = readLine()

      if (nouveaucodePIN.forall(_.isDigit) && nouveaucodePIN.length == 6) {
        machinePins(machineId) = nouveaucodePIN
        println("Le code PIN a été mis à jour avec succès.")
        println ("Retour au menu principal ...")
        Thread.sleep(1000)
        MAJ = false
        return false
      }
      else {
        println("Nouveau code PIN refusé. Veuillez réessayer.")
        Thread.sleep(1000)
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Sélectionnez votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val choixBoisson = readChar()

    // Expresso
    if (choixBoisson == '1') {
      if (coffeeStocks(machineId) < 8) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
        coffeeStocks(machineId) -= 8
        Thread.sleep(1000)
        return false
      }
      else {
        val prixExpresso: Double = 2.0
        var prixsucre: Double = 0.0

        println("Souhaitez-vous ajouter du sucre ? ")
                println("1) Sans sucre \n" +
                  "2) Peu (5g) - CHF 0.10 \n" +
                  "3) Moyen (10g) - CHF 0.20 \n" +
                  "4) Beaucoup (15g) - CHF 0.30 \n > ")

                val choix_sucre: Int = readChar()
        if (choix_sucre == '1') {
          println("Boisson séléctionnée : Expresso")
          Thread.sleep(1000)
          println("Niveau de sucre : Sans sucre\n")
          Thread.sleep(1000)
        }
        else if (choix_sucre == '2' && sugarStocks(machineId) >= 5) {
          println("Boisson séléctionnée : Expresso ")
          Thread.sleep(1000)
          println("Niveau de sucre : Peu (5g)\n ")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 10) {
          println("Boisson séléctionnée : Expresso")
          Thread.sleep(1000)
          println("Niveau de sucre : Moyen (10g) \n")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && sugarStocks(machineId) >= 15) {
          println("Boisson séléctionnée : Expresso ")
          Thread.sleep(1000)
          println("Niveau de sucre : Beaucoup (15g)\n")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 15
          prixsucre = 0.30
        }
        else {
          println("Choix non reconnu ou Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée. \n" +
            "Veuillez réessayer ou veuillez choisir une quantité de sucre plus petite ou essayez une autre boisson")
          Thread.sleep(1000)
          println("Retour au menu principal.\n")
          Thread.sleep(1000)
          return false
        }
        val prixTotal = prixExpresso + prixsucre
        println(f"Prix total : $prixTotal%1.2f CHF")
        Thread.sleep(1000)

        //codetwint

        val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
                println("\nVeuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
                Thread.sleep(3000)
                println("\nMerci ! Votre paiement a été accepté.")
                println("Paiement confirmé.\n" +
                  "Préparation de votre boisson...\n" +
                  "(...)\n")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !\n")
                Thread.sleep(3000)

                coffeeStocks(machineId) -= 8
                return false
      }
    }
    //Cappuccino
    else if (choixBoisson == '2') {
      if (coffeeStocks(machineId) < 6) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
        Thread.sleep(1000)
        return false
      }
      else {
        val prixcappuccino: Double = 2.50
        var prixsucre: Double = 0.0
        var prixlait: Double = 0.0

        println("Souhaitez-vous ajouter du sucre ? ")
        println("1) Sans sucre \n" +
                  "2) Peu (5g) - CHF 0.10 \n" +
                  "3) Moyen (10g) - CHF 0.20 \n" +
                  "4) Beaucoup (15g) - CHF 0.30 \n > ")

        val choix_sucre: Int = readChar()

        if (choix_sucre == '1') {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Sans sucre")
          Thread.sleep(1000)
          prixsucre = 0.0
        }
        else if (choix_sucre == '2' && sugarStocks(machineId) >= 5) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 10) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && sugarStocks(machineId) >= 15) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 15
          prixsucre = 0.30
        }
        else {
          println("\nChoix non reconnu ou Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
          println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
          Thread.sleep(1000)
          return false

        }
        println("\nSouhaitez-vous ajouter du lait en supplément ? \n" +
                  "(Disponible uniquement pour Cappuccino et Latte")
                println("1) Oui")
                println("2) Non")

        val choix_lait: Int = readChar()

        if (choix_lait == '1') {
                println("Combien de dose ?\n")
                    println("1) 1 dose (50mL)\n" +
                      "2) 2 doses (100mL)\n" +
                      "3) 3 doses (150mL)\n " +
                      ">")

          val dose_lait: Int = readChar()

          if (dose_lait == '1' && milkStocks(machineId) >= 50) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 50
            prixlait = 0.05
          }
          else if (dose_lait == '2' && milkStocks(machineId) >= 100) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 100
            prixlait = 0.10
          }
          else if (dose_lait == '3' && milkStocks(machineId) >= 150) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 150
            prixlait = 0.15
          }
          else {
            println("\nChoix non reconnu ou Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
            Thread.sleep(1000)
            return false
          }
        }
        else if (choix_lait == '2') {
          println("\nLait en supplément : Non")
          Thread.sleep(1000)
          prixlait = 0.0
        }
        else {
          println("\nCommande non reconnu. Veuillez réessayer\n")
          Thread.sleep(1000)
          return false
        }
        val prixTotal = prixcappuccino + prixsucre + prixlait
        println(f"Prix total : $prixTotal%1.2f CHF")
        Thread.sleep(1000)

        //codetwint

        val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
                println("Veuillez payer en utilisant twint.")
                println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
                Thread.sleep(3000)
                println("\nMerci ! Votre paiement a été accepté.")
                println("Paiement confirmé.\n" +
                  "Préparation de votre boisson...\n" +
                  "(...)")
                Thread.sleep(3000)
                println("Votre Cappuccino est prêt ! Bonne dégustation !")
                Thread.sleep(3000)

                coffeeStocks(machineId) -= 6
                milkStocks(machineId) -= 100
                return false
      }
    }
    // Latte
    else if (choixBoisson == '3') {

      // Choix de taille latte

      println("Choisissez la taille de votre Latte")
              println("1) Petit - CHF 2.70 \n" +
                "2) Moyen - CHF 3.20 \n" +
                "3) Grand - CHF 3.70 ")

      val choixtaille: Int = readChar()
      var prixlatte: Double = 0.0
      var prixsucre: Double = 0.0
      var prixlait: Double = 0.0

      if (choixtaille == '1') {
        if (coffeeStocks(machineId) < 6) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
          if (milkStocks(machineId) <= 0){
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
            Thread.sleep(1000)
            return false
          }
        else {
          println("\nBoisson séléctionnée : Latte (Petit)")
          Thread.sleep(1000)
          prixlatte = 2.70
        }
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 120

        //choix sucre

        println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")

        val choix_sucre: Int = readChar()

        if (choix_sucre == '1') {
          println("\nNiveau de sucre : Sans sucre")
          Thread.sleep(1000)
          prixsucre = 0.0
        }
        else if (choix_sucre == '2' && sugarStocks(machineId) >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && sugarStocks(machineId) >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 15
          prixsucre = 0.30
        }
        else if (sugarStocks(machineId) <= 0){
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
          println("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
          Thread.sleep(1000)
          return false
        }
        else {
          println("Choix non reconnu. Veuillez réessayer")
          Thread.sleep(1000)
          return false
        }
        //choix dose lait

        println("Souhaitez-vous ajouter du lait en supplément ? \n" +
                    "Disponible uniquement pour Cappuccino et Latte ")
                  println("1) Oui")
                  println("2) Non")

        val choix_lait: Int = readChar()
        if (choix_lait == '1') {
          println("Combien de dose ? ")
          println("1) 1 dose (60mL) \n" +
                        "2) 2 doses (120mL) \n" +
                        "3) 3 doses (180mL) \n >")

          val dose_lait: Int = readChar()

          if (dose_lait == '1' && milkStocks(machineId) >= 60) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 60
            prixlait = 0.05
          }
          else if (dose_lait == '2' && milkStocks(machineId) >= 120) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 120
            prixlait = 0.10
          }
          else if (dose_lait == '3' && milkStocks(machineId) >= 180) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 180
            prixlait = 0.15
          }
          else {
            println("\nChoix non reconnu. Veuillez réessayer")
            Thread.sleep(1000)
            return false
          }

        }
        else if (choix_lait == '2') {
          println("\nLait en supplément : Non")
          Thread.sleep(1000)
          prixlait = 0.0
        }
        else {
          println("\nCommande non reconnu. Veuillez réessayer")
          Thread.sleep(1000)
          return false
        }
        val prixTotal: Double = prixlatte + prixsucre + prixlait
        println(f"Prix total : $prixTotal%1.2f CHF ")
        Thread.sleep(1000)

        //codetwint

        val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
        println("Veuillez payer en utilisant twint.")
        println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("\nMerci ! Votre paiement a été accepté.")
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson...\n" +
          "(...)")
        Thread.sleep(3000)
        println("Votre Latte est prêt ! Bonne dégustation !")
        Thread.sleep(3000)
        return false
      }
      else if (choixtaille == '2') {
        if (coffeeStocks(machineId) < 8) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        if (milkStocks(machineId) <= 0){
          println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        else {
          println("\nBoisson séléctionnée : Latte (Moyen)")
          Thread.sleep(1000)
          prixlatte = 3.20
        }
        coffeeStocks(machineId) -= 8
        milkStocks(machineId) -= 150

        //choix sucre

        println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")
        val choix_sucre: Int = readChar()

        if (choix_sucre == '1') {
          println("\nNiveau de sucre : Sans sucre")
          Thread.sleep(1000)
          prixsucre = 0.0
        }
        else if (choix_sucre == '2' && sugarStocks(machineId) >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 15
          prixsucre = 0.30
        }
        else if (sugarStocks(machineId) <= 0){
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
          println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petite ou essayer une autre boisson")
          Thread.sleep(1000)
          return false
        }
        else {
          println("Choix non reconnu. Veuillez réessayer")
          Thread.sleep(1000)
          return false
        }
        //choix dose lait

        println("Souhaitez-vous ajouter du lait en supplément ? \n" +
                    "Disponible uniquement pour Capuccino et Latte")
                  println("1) Oui")
                  println("2) Non")

        val choix_lait: Int = readChar()
        if (choix_lait == '1') {
          println("Combien de dose ? ")
          println("1) 1 dose (75mL) \n" +
                        "2) 2 doses (150mL) \n" +
                        "3) 3 doses (225mL) \n >")

          val dose_lait: Int = readChar()

          if (dose_lait == '1' && milkStocks(machineId) >= 75) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 75
            prixlait = 0.05
          }
          else if (dose_lait == '2' && milkStocks(machineId) >= 150) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 150
            prixlait = 0.10
          }
          else if (dose_lait == '3' && milkStocks(machineId) >= 225) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 225
            prixlait = 0.15
          }
          else {
            println("\nChoix non reconnu. Veuillez réessayer")
            Thread.sleep(1000)
            return false
          }
        }
        else if (choix_lait == '2') {
          println("\nLait en supplément : Non")
          Thread.sleep(1000)
          prixlait = 0.0
        }
        else {
          println("\nCommande non reconnu. Veuillez réessayer")
          Thread.sleep(1000)
          return false
        }
        val prixTotal: Double = prixlatte + prixsucre + prixlait
        println(f"Prix total : $prixTotal%1.2f CHF ")
        Thread.sleep(1000)

        //codetwint

        val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
        println("Veuillez payer en utilisant twint.")
        println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("\nMerci ! Votre paiement a été accepté.")
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson...\n" +
          "(...)")
        Thread.sleep(3000)
        println("Votre Latte est prêt ! Bonne dégustation !")
        Thread.sleep(3000)
        return false
      }
      else if (choixtaille == '3') {
        if (coffeeStocks(machineId) < 12) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        if (milkStocks(machineId) <= 0){
          println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        else {
          println("\nBoisson séléctionnée : Latte (Grand)")
          Thread.sleep(1000)
          prixlatte = 3.70
        }
        coffeeStocks(machineId) -= 12
        milkStocks(machineId) -= 200

        //choix sucre

        println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")
        val choix_sucre: Int = readChar()

        if (choix_sucre == '1') {
          println("\nNiveau de sucre : Sans sucre")
          Thread.sleep(1000)
          prixsucre = 0.0
        }
        else if (choix_sucre == '2' && sugarStocks(machineId) >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && sugarStocks(machineId) >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && sugarStocks(machineId) >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          sugarStocks(machineId) -= 15
          prixsucre = 0.30
        }
        else if (sugarStocks(machineId) <= 0){
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
          println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petite ou essayer une autre boisson")
          Thread.sleep(1000)
          return false
        }
        else {
          println("Choix non reconnu. Veuillez réessayer")
          Thread.sleep(1000)
          return false
        }
        //choix dose lait

        println("Souhaitez-vous ajouter du lait en supplément ?\n" +
                    "Disponible uniquement pour Cappuccino et Latte ")
                  println("1) Oui")
                  println("2) Non")

        val choix_lait: Int = readChar()
        if (choix_lait == '1') {
          println("Combien de dose ? ")
          println("1) 1 dose (100mL) \n" +
                        "2) 2 doses (200mL) \n" +
                        "3) 3 doses (300mL) \n >")

          val dose_lait: Int = readChar()

          if (dose_lait == '1' && milkStocks(machineId) >= 100) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 100
            prixlait = 0.05
          }
          else if (dose_lait == '2' && milkStocks(machineId) >= 200) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 200
            prixlait = 0.10
          }
          else if (dose_lait == '3' && milkStocks(machineId) >= 300) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            milkStocks(machineId) -= 300
            prixlait = 0.15
          }
          else {
            println("\nChoix non reconnu. Veuillez réessayer")
            Thread.sleep(1000)
            return false
          }
        }
        else if (choix_lait == '2') {
          println("\nLait en supplément : Non")
          Thread.sleep(3000)
          prixlait = 0.0
          return false
        }
        else {
          println("\nCommande non reconnu. Veuillez réessayer")
          Thread.sleep(3000)
          return false
        }
        val prixTotal: Double = prixlatte + prixsucre + prixlait
        println(f"Prix total : $prixTotal%1.2f CHF ")
        Thread.sleep(1000)

        //codetwint

        val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
        println("Veuillez payer en utilisant twint.")
        println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("\nMerci ! Votre paiement a été accepté.")
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson...\n" +
          "(...)")
        Thread.sleep(3000)
        println("Votre Latte est prêt ! Bonne dégustation !")
        Thread.sleep(3000)
        return false
      }
      else {
        println("\nChoix non reconnu. Veuillez réessayer")
        Thread.sleep(3000)
        return false
      }

    }
    false

  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    println ("Stocks initiaux: \n" +
      "1) Poudre de café : 50g \n" +
      "2) Sucre : 30g \n" +
      "3) Lait : 500 mL \n")
    Thread.sleep(1000)

    println ("Quantités actuelles dans les stocks : \n")
    println(s"Stock actuel de café : ${coffeeStocks(machineId)} g")
    println(s"Stock actuel de sucre : ${sugarStocks(machineId)} g")
    println(s"Stock actuel de lait : ${milkStocks(machineId)} mL")
    Thread.sleep(1000)

    println ("\nEntrez la quantité à ajouter : ")
    Thread.sleep(1000)

    println ("Poudre de café (grammes) >: ")
    val ajoutcafe: Int = readInt()
    coffeeStocks(machineId) += ajoutcafe

    println("Sucre (grammes) >")
    val ajoutsucre: Int = readInt()
    sugarStocks(machineId) += ajoutsucre

    println ("Lait (en millilitres) >")
    val ajoutlait: Int = readInt()
    milkStocks(machineId) += ajoutlait


    println ("Les stocks ont été mis à jour avec succès. ")
    Thread.sleep(1000)
    println ("Retour au menu principal...")
    Thread.sleep(1000)
    return false
  }

}
