import io.StdIn._
import util.Random


object Main{

  // Déclaration des stocks pour 5 machines
  val nbMachines = 5
  var stocksCafe = Array.fill(nbMachines)(50)
  var stocksSucre = Array.fill(nbMachines)(30)
  var stocksLait = Array.fill(nbMachines)(0.5)
  val pins = Array.fill(nbMachines)("434343")

  val listeBoissons = Array("Expresso", "Cappuccino", "Latte")
  val prixExpresso = 2.00
  val prixCappuccino = 2.50
  val prixLatte = Array(2.70, 3.20, 3.70)

  val poudreCafeExpresso = 8
  val poudreCafeCappuccino = 6
  val poudreCafeLatte = Array(6, 8, 12)
  val laitCappuccino = 0.1
  val laitLatte = Array(0.12, 0.15, 0.2)

  val quantiteSucre = Array(0, 5, 10, 15)
  val prixSucre = Array(0.00, 0.10, 0.20, 0.30)
  val prixDoseLaitSupplement = 0.05


  def main(args: Array[String]): Unit = {
    var quitter = false

    while (!quitter) {
      println("\nVeuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val choixMode = readInt()

      if (choixMode == 1) {
        modeClient()
      } else if (choixMode == 2) {
        modeAdmin()
      } else if (choixMode == 3) {
        quitter = true
      } else {
      println("Erreur : veuillez entrer un nombre entre 1 et 3.")
      }
      }
  }

  def choisirMachine(): Int = {
    println("\nVeuillez choisir une machine (1 à 5) :")
    var choix = readInt()
    while (choix < 1 || choix > nbMachines) {
      println("Erreur : veuillez entrer un nombre entre 1 et 5.")
      choix = readInt()
    }
    choix - 1
  }

  def validatePin(machineId: Int, pins: Array[String]): Boolean = {
    var tentatives = 0
    val maxAttempts = 3
    while (tentatives < maxAttempts) {
      println("Entrez le code PIN :")
      val saisiePin = readLine()
      if (saisiePin == pins(machineId))return true
      tentatives += 1
      println(s"Code PIN incorrect. ${maxAttempts - tentatives} tentatives restantes.")
    }
    false
  }

  def updatePin(machineId: Int, pins: Array[String]): Unit = {
    println(f"Mise à jour du code PIN pour la machine ${machineId+1 }:")
    println("Entrez un nouveau code PIN à 6 chiffres >")
    var newPin = readLine()
    while (newPin.length != 6) {
      println("Erreur : Le code PIN doit contenir exactement 6 chiffres. Veuillez réessayer :")
      newPin = readLine()
    }
    pins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  def serveClient(machineId: Int, stocksCafe: Array[Int], stocksSucre: Array[Int], stocksLait: Array[Double]): Boolean = {

    println("\nVeuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")


    var choixBoisson = readInt()
    while (choixBoisson < 1 || choixBoisson > 3) {
      println("Erreur : veuillez entrer un nombre entre 1 et 3.")
      choixBoisson = readInt()
    }

    var poudreCafeRequise = 0
    var laitRequis = 0.0
    var prixBoisson = 0.0

    if (choixBoisson == 1) {
      poudreCafeRequise = poudreCafeExpresso
      laitRequis = 0.0
      prixBoisson = prixExpresso

    } else if (choixBoisson == 2) {
      poudreCafeRequise = poudreCafeCappuccino
      laitRequis = laitCappuccino
      prixBoisson = prixCappuccino

    } else {
      println("\nVeuillez choisir une taille pour le Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print("> ")

      var choixTailleLatte = readInt()
      while (choixTailleLatte < 1 || choixTailleLatte > 3) {
        println("Erreur : veuillez entrer un nombre entre 1 et 3.")
        choixTailleLatte = readInt()
      }

      poudreCafeRequise = poudreCafeLatte(choixTailleLatte - 1)
      laitRequis = laitLatte(choixTailleLatte - 1)
      prixBoisson = prixLatte(choixTailleLatte - 1)
    }

    // message d'erreur
    if (stocksCafe(machineId) < poudreCafeRequise) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    } else if (stocksLait(machineId) < laitRequis) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false

    }
    // Ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    var choixSucre = readInt()
    while (choixSucre < 1 || choixSucre > 4) {
      println("Erreur : veuillez entrer un nombre entre 1 et 4.")
      choixSucre = readInt()
    }

    val quantiteSucreChoisi = quantiteSucre(choixSucre - 1)
    val prixSucreChoisi = prixSucre(choixSucre - 1)

    if (stocksSucre(machineId) < quantiteSucreChoisi) {
      println("Erreur : pas assez de sucre en stock.")
      return false
    } else {

    // Supplément lait
      var laitSupplement = 0.0
      var prixLaitSupplement = 0.0
      if (choixBoisson == 2 || choixBoisson == 3) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print("> ")

        var choixLaitSupplementOption = readInt()
        while (choixLaitSupplementOption < 1 || choixLaitSupplementOption > 2) {
          println("Erreur : veuillez entrer un nombre entre 1 et 2.")
          choixLaitSupplementOption = readInt()
        }

        if (choixLaitSupplementOption == 1) {
          println("Combien de doses ?")
          print("> ")
          var choixNombreDoses = readInt()
          while (choixNombreDoses < 1 || choixNombreDoses > 3) {
            println("Erreur : veuillez entrer un nombre entre 1 et 3.")
            choixNombreDoses = readInt()
          }

          laitSupplement = choixNombreDoses * laitRequis // Calcul du lait supplémentaire
          prixLaitSupplement = choixNombreDoses * prixDoseLaitSupplement // Calcul du prix du supplément
        }
      }

      // Vérification des stocks de lait supplémentaires
      if (stocksLait(machineId) < laitRequis + laitSupplement) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        return false
      } else {
        val prixTotal = prixBoisson + prixSucreChoisi + prixLaitSupplement
        println(f"Prix total : CHF $prixTotal%.2f")
        println("Veuillez payer en utilisant Twint.")
        val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase()
        println(s"Votre code de paiement est : $codeTwint")
        println("(En attente de validation du paiement...)")

        println("Merci ! Votre paiement a été accepté.")
        println(s"Préparation de votre ${listeBoissons(choixBoisson - 1)}...")

        stocksCafe(machineId) -= poudreCafeRequise
        stocksLait(machineId) -= (laitRequis + laitSupplement)  // Déduire le lait de stock
        stocksSucre(machineId) -= quantiteSucreChoisi

        println(s"Votre ${listeBoissons(choixBoisson - 1)} est prêt ! Bonne dégustation !")
        true
      }
    }
  }

  def restockMachine (machineId: Int, stocksCafe: Array[Int], stocksSucre: Array[Int], stocksLait: Array[Double]): Unit = {

    println(s"\n Stocks actuels de la machine ${machineId + 1} :")
    println(s"- Café : ${stocksCafe(machineId)} g")
    println(s"- Lait : ${stocksLait(machineId)} L")
    println(s"- Sucre : ${stocksSucre(machineId)} g")

    println("\nRéapprovisionnement des stocks :")
    println("Entrez les quantités à ajouter :")

    println("Poudre de café (g) : ")
    val ajoutPoudreCafe = readInt()

    println("Lait (L) : ")
    val ajoutLait = readDouble()

    println("Sucre (g) : ")
    val ajoutSucre = readInt()


    stocksCafe(machineId) += ajoutPoudreCafe
    stocksLait(machineId) += ajoutLait
    stocksSucre(machineId) += ajoutSucre

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principale...")
  }

  def modeClient(): Unit = {
    val machineId = choisirMachine()
    serveClient(machineId, stocksCafe, stocksSucre, stocksLait)
  }

  def modeAdmin(): Unit = {
    val machineId = choisirMachine()

    if (validatePin(machineId, pins)) {
      var quitterAdmin = false
      while (!quitterAdmin) {

        println("1) Réapprovisionner les stocks")
        println("2) Modifier le code PIN")
        print("> ")

        val choix = readInt()

        if (choix == 1) {
          restockMachine(machineId, stocksCafe, stocksSucre, stocksLait)
          quitterAdmin = true
        } else if (choix == 2) {
          updatePin(machineId, pins)
          quitterAdmin = true
        } else {
          println("Erreur : option invalide. Veuillez entrer un nombre entre 1 et 3.")
        }
      }
    } else {
      println("Trop de tentatives échouées. Fin du programme.")
    }
  }
}


