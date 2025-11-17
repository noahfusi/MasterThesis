import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._
import scala.util.Random

case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int)

object Main {

  val machines: ArrayBuffer[Machine] = loadMachines("src/resources/machines.csv")

  def main(args: Array[String]): Unit = {
    println("   Bienvenue dans Nospresso Café   ")
    println("Chargement des machines depuis le fichier CSV...")
    machines.foreach(machine =>
      println(f"Machine ${machine.id}%d : PIN: ${machine.pincode}, Lait: ${machine.milk}mL, Sucre: ${machine.sugar}g, Café: ${machine.coffee}g")
    )

    var programme = true
    while (programme) {
      var machineId = -1

      println ("Nospresso Café")
      println ("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println ("3) Quitter")
      println (">")

      val choix = validateInput(readLine(), 1, 3)

      if (choix == 1) { // Pour rentrer dans le Menu Client
        val machineId = selectMachine()
        if (machineId >= 0)
          serveClient(machineId, machines(machineId))
      }
      else if (choix == 2) {  // Pour rentrer dans le Mode Admin
        val machineId = selectMachine()
        println("Entrée le code PIN :")
        println (">")
        if (validatePin(machineId)) {
          adminMode(machineId)
        }

      }

      else if (choix == 3){ // Mode Quitter : Fin du programme
        saveMachines("src/resources/machines.csv", machines)
        println("\nMerci d'avoir utilisé Nospresso Café. À bientôt !")
        programme = false
      }
      else {
        println("Choix non valide. Veuillez réessayer")
        Thread.sleep(1000)
      }
    }
  }

  def validateInput(input: String, min: Int, max: Int): Int = {
    var validation = false
    var nombre = 0

    while (!validation) {
      if (input.forall(_.isDigit)) {
        nombre = input.toInt
        if (nombre >= min && nombre <= max) validation = true
        else println(s"Entrez un nombre entre $min et $max.")
      } else println(s"Entrée invalide. Entrez un nombre entre $min et $max.")
      if (!validation) {
        print("> ")
        val retry = readLine()
        return validateInput(retry, min, max)
      }
    }
    nombre
  }

  def validatePin(machineId: Int): Boolean = {

    var tentatives = 3
    while (tentatives > 0) {
      val codePin = readLine()
      if (codePin == machines(machineId).pincode) {
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

  def selectMachine(): Int = {
    println("\nSélectionnez une machine :")
    machines.zipWithIndex.foreach { case (machine, idx) =>
      println(f"Machine ${idx + 1}%d : [Lait: ${machine.milk}%dmL, Sucre: ${machine.sugar}%dg, Café: ${machine.coffee}%dg]")
    }
    print("> ")
    val input = readLine()
    validateInput(input, 1, machines.size) - 1
  }

  def updatePin(machineId: Int): Unit = {
    var update = false

    while (!update) {
      print("\nEntrez un nouveau code PIN (6 chiffres) : ")
      val newPin = readLine()
      if (newPin.forall(_.isDigit) && newPin.length == 6) {
        machines(machineId).pincode = newPin
        println("Code PIN mis à été mise à jour avec succès.")
        update = true
      }
      else {
        println("Nouveau code PIN refusé. Veuillez réessayer.")
        Thread.sleep(1000)
      }
    }
  }

  def serveClient(machineId: Int, machine: Machine): Boolean = {
    val machine = machines(machineId)
    println("Sélectionnez votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val choixBoisson: Char = readChar()

    // Expresso
    if (choixBoisson == '1') {
      if (machine.coffee < 8) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
        machine.coffee -= 8
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
        else if (choix_sucre == '2' && machine.sugar >= 5) {
          println("Boisson séléctionnée : Expresso ")
          Thread.sleep(1000)
          println("Niveau de sucre : Peu (5g)\n ")
          Thread.sleep(1000)
          machine.sugar -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && machine.sugar >= 10) {
          println("Boisson séléctionnée : Expresso")
          Thread.sleep(1000)
          println("Niveau de sucre : Moyen (10g) \n")
          Thread.sleep(1000)
          machine.sugar -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && machine.sugar >= 15) {
          println("Boisson séléctionnée : Expresso ")
          Thread.sleep(1000)
          println("Niveau de sucre : Beaucoup (15g)\n")
          Thread.sleep(1000)
          machine.sugar -= 15
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

        machine.coffee -= 8
        return false
      }
    }
    //Cappuccino
    else if (choixBoisson == '2') {
      if (machine.coffee < 6) {
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
        else if (choix_sucre == '2' && machine.sugar >= 5) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          machine.sugar -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && machine.sugar >= 10) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          machine.sugar -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && machine.sugar >= 15) {
          println("Boisson séléctionnée : Cappuccino")
          Thread.sleep(1000)
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          machine.sugar -= 15
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

          if (dose_lait == '1' && machine.milk >= 50) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            machine.milk -= 50
            prixlait = 0.05
          }
          else if (dose_lait == '2' && machine.milk >= 100) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            machine.milk -= 100
            prixlait = 0.10
          }
          else if (dose_lait == '3' && machine.milk >= 150) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            machine.milk -= 150
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

        machine.coffee -= 6
        machine.milk -= 100
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
        if (machine.coffee < 6) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        if (machine.milk <= 0){
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
        machine.coffee -= 6
        machine.milk -= 120

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
        else if (choix_sucre == '2' && machine.sugar >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          machine.sugar -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && machine.sugar >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          machine.sugar -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && machine.sugar >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          machine.sugar -= 15
          prixsucre = 0.30
        }
        else if (machine.sugar <= 0){
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

          if (dose_lait == '1' && machine.milk >= 60) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            machine.milk -= 60
            prixlait = 0.05
          }
          else if (dose_lait == '2' && machine.milk >= 120) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            machine.milk -= 120
            prixlait = 0.10
          }
          else if (dose_lait == '3' && machine.milk >= 180) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            machine.milk -= 180
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
        if (machine.coffee < 8) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        if (machine.milk <= 0){
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
        machine.coffee -= 8
        machine.milk -= 150

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
        else if (choix_sucre == '2' && machine.sugar >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          machine.sugar -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && machine.sugar >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          machine.sugar -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '3' && machine.sugar >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          machine.sugar -= 15
          prixsucre = 0.30
        }
        else if (machine.sugar <= 0){
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

          if (dose_lait == '1' && machine.milk >= 75) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            machine.milk -= 75
            prixlait = 0.05
          }
          else if (dose_lait == '2' && machine.milk >= 150) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            machine.milk -= 150
            prixlait = 0.10
          }
          else if (dose_lait == '3' && machine.milk >= 225) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            machine.milk -= 225
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
        if (machine.coffee < 12) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          Thread.sleep(1000)
          return false
        }
        if (machine.milk <= 0){
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
        machine.coffee -= 12
        machine.milk -= 200

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
        else if (choix_sucre == '2' && machine.sugar >= 5) {
          println("\nNiveau de sucre : Peu (5g)")
          Thread.sleep(1000)
          machine.sugar -= 5
          prixsucre = 0.10
        }
        else if (choix_sucre == '3' && machine.sugar >= 10) {
          println("\nNiveau de sucre : Moyen (10g)")
          Thread.sleep(1000)
          machine.sugar -= 10
          prixsucre = 0.20
        }
        else if (choix_sucre == '4' && machine.sugar >= 15) {
          println("\nNiveau de sucre : Beaucoup (15g)")
          Thread.sleep(1000)
          machine.sugar -= 15
          prixsucre = 0.30
        }
        else if (machine.sugar <= 0){
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

          if (dose_lait == '1' && machine.milk >= 100) {
            println("\nLait en supplément : Oui (1 dose)")
            Thread.sleep(1000)
            machine.milk -= 100
            prixlait = 0.05
          }
          else if (dose_lait == '2' && machine.milk >= 200) {
            println("\nLait en supplément : Oui (2 doses)")
            Thread.sleep(1000)
            machine.milk -= 200
            prixlait = 0.10
          }
          else if (dose_lait == '3' && machine.milk >= 300) {
            println("\nLait en supplément : Oui (3 doses)")
            Thread.sleep(1000)
            machine.milk -= 300
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

  def adminMode(machineId: Int): Unit = {

    println("\nMode Admin :")
    println("1) Réapprovisionnement des stocks")
    println("2) Mise à jour du code PIN")
    println("3) Enlever des stocks")
    print("> ")


    val choix = validateInput(readLine(), 1, 3)

    if (choix == 1) {
      addIngredient(machineId)
    } else if (choix == 2) {
      updatePin(machineId)
    } else if (choix == 3){
      removeIngredient(machineId)
    }
  }

  def removeIngredient(machineId: Int): Unit = {
  val machine = machines(machineId)
    println(f"\nQuantités actuelles : Lait: ${machine.milk}%dmL, Sucre: ${machine.sugar}%dg, Café: ${machine.coffee}%dg")

    print("Quantité de lait à ajouter (mL) : ")
    val retirelait: Int = readInt()
    machine.milk -= retirelait

    print("Quantité de sucre à ajouter (g) : ")
    val retiresucre: Int = readInt()
    machine.sugar -= retiresucre

    print("Quantité de café à ajouter (g) : ")
    val retirecafe: Int = readInt()
    machine.coffee -= retirecafe

    println("\nStocks mis à jour avec succès.")
  }

  def addIngredient(machineId: Int): Unit = {
    val machine = machines(machineId)
    println(f"\nQuantités actuelles : Lait: ${machine.milk}%dmL, Sucre: ${machine.sugar}%dg, Café: ${machine.coffee}%dg")

    print("Quantité de lait à ajouter (mL) : ")
    val ajoutlait: Int = readInt()
    machine.milk += ajoutlait

    print("Quantité de sucre à ajouter (g) : ")
    val ajoutsucre: Int = readInt()
    machine.sugar += ajoutsucre

    print("Quantité de café à ajouter (g) : ")
    val ajoutcafe: Int = readInt()
    machine.coffee += ajoutcafe

    println("\nStocks mis à jour avec succès.")
  }

  def loadMachines(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines()
    lines.next()
    for ((line, index) <- lines.zipWithIndex) {
      val Array(pin, milk, sugar, coffee) = line.split(",")
      buffer += Machine(index + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
    }
    source.close()
    println(s"${buffer.size} machine(s) chargée(s) avec succès.")
    buffer
  }
  def saveMachines(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val writer = new java.io.PrintWriter(new java.io.File(filename))
    writer.println("PINCODE,MILK,SUGAR,COFFEE")
    machines.foreach { machine =>
      writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
    }
    writer.close()
    println("\nDonnées sauvegardées avec succès.")
  }

}