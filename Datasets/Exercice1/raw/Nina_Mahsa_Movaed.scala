import io.StdIn.readLine
import util.Random

object MachineCafe {

  // Stocks initiaux

  var poudreCafe = 50
  var sucre = 30
  var lait = 500

  // Code PIN du mode admin

  val codePIN = "434343"

  def main(args: Array[String]): Unit = {
    println("Nospresso Café")
    menuPrincipal()
  }

  def menuPrincipal(): Unit = {
    var continuer = true
    while (continuer){
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = readLine("> ")

      if (choix == "1") {
        modeClient()
      } else if (choix == "2") {
        modeAdmin()
      } else if (choix == "3") {
        println("Au revoir !")
        continuer = false
      } else {
        println("Choix invalide. Veuillez réessayer")
      }
    }
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner votre boisson :")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    val choixBoisson = readLine("> ")

    var prixBase = 0.0
    var laitRequis = 0
    var cafeRequis = 0

    if (choixBoisson == "1") {
      cafeRequis = 8
      prixBase = 2.0
    } else if (choixBoisson == "2") {
      cafeRequis = 6
      laitRequis = 100
      prixBase = 2.5
    } else if (choixBoisson == "3") {
      println("Choisissez la taille de votre Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      val tailleLatte = readLine("> ")

      var choixInvalide = false

      if (tailleLatte == "1") {
        cafeRequis = 6
        laitRequis = 120
        prixBase = 2.7
      } else if (tailleLatte == "2") {
        cafeRequis = 8
        laitRequis = 150
        prixBase = 3.2
      } else if (tailleLatte == "3") {
        cafeRequis = 12
        laitRequis = 200
        prixBase = 3.7
      } else {
        println("Choix de taille invalide. Retour au menu principal.")
        choixInvalide = true
      }
      if (choixInvalide) {
        println("Veuillez réessayer.")
        // Actions de retour au menu principal
      } else {
        println("Préparation de votre boisson...")
      }
    }
    if (cafeRequis > poudreCafe || laitRequis > lait) {
      println("Quantité d'ingrédients insuffisante pour préparer la boisson. Veuillez choisir une autre option.")
    } else {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      val choixSucre = readLine("> ")

      var prixSucre = 0.0
      var sucreRequis = 0

      if (choixSucre == "2") {
        sucreRequis = 5
        prixSucre = 0.1
      } else if (choixSucre == "3") {
        sucreRequis = 10
        prixSucre = 0.2
      } else if (choixSucre == "4") {
        sucreRequis = 15
        prixSucre = 0.3
      }
      if (sucreRequis > sucre) {
        println("Quantité de sucre insuffisante pour préparer la boisson. Veuillez choisir une autre option.")
      } else {
        val prixTotal = prixBase + prixSucre
        println(s"Prix total : CHF " + prixTotal)

        println("Veuillez payer en utilisant Twint.")
        val codePaiement = Random.alphanumeric.take(5).mkString("")
        println(s"Votre code de paiement est : " + codePaiement)
        println("(En attente de paiement...)")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")

        println("Préparation de votre boisson...")
        Thread.sleep(5000)

        poudreCafe = poudreCafe - cafeRequis
        sucre = sucre - sucreRequis
        lait = lait - laitRequis
        println("Votre boisson est prête ! Bonne dégustation !")
      }
    }
  }

  def modeAdmin(): Unit = {
    println("Entrez le code PIN :")
    val pinEntree = readLine("> ")

    if (pinEntree == codePIN) {
      println("Accès autorisé.")
      println("Stocks actuels : ")
      println("Poudre de café : " + poudreCafe)
      println("Sucre : " + sucre)
      println("Lait : " + lait)

      println("Souhaitez-vous réapprovisionner ? (1 pour Oui, autre pour Non)")
      val reapprovisionnement = readLine("> ")

      if (reapprovisionnement == "1") {
        println("Quantité de poudre de café à ajouter :")
        poudreCafe = poudreCafe + readLine("> ").toInt

        println("Quantité de sucre à ajouter :")
        sucre = sucre + readLine("> ").toInt

        println("Quantité de lait à ajouter (en ml) :")
        lait = lait + readLine("> ").toInt

        println("Stocks mis à jour.")
      }
    } else {
      println("Code PIN incorrect. Retour au menu principal.")
    }
  }
}



