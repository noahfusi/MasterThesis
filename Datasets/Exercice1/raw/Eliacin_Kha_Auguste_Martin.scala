import scala.io.StdIn.readLine

object Nospresso {
  var poudreCafe: Int = 50
  var sucre: Int = 30
  var lait: Int = 500
  val codePin: String = "434343"

  def main(args: Array[String]): Unit = {
    var continuer: Boolean = true
    while (continuer) {
      var choix: String = ""
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      choix = readLine("> ")

      if (choix == "3") {
        continuer = false
      } else if (choix == "1") {
        var choixBoisson: String = ""
        var commandeComplete: Boolean = false

        while (!commandeComplete) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choixBoisson = readLine("> ")

          var tailleLatte: String = ""
          if (choixBoisson == "3") {
            println("Quelle taille souhaitez-vous ?")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            tailleLatte = readLine("> ")
          }

          if (choixBoisson == "1" || choixBoisson == "2" || (choixBoisson == "3" && tailleLatte != "")) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            var choixSucre: String = readLine("> ")

            var ajoutLait: Boolean = false
            if (choixBoisson == "2" || choixBoisson == "3") {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("1) Oui")
              println("2) Non")
              var choixLait: String = readLine("> ")
              ajoutLait = choixLait == "1"
            }

            println("Veuillez payer en utilisant Twint.")
            var codePaiement: String = (1 to 5).map(_ => {
              val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
              chars((Math.random() * chars.length).toInt)
            }).mkString
            Thread.sleep(3000)
            println("Votre code de paiement est : " + codePaiement)
            println("Merci ! Votre paiement a été accepté.")

            if (choixSucre == "2") sucre -= 5
            if (choixSucre == "4") sucre -= 15
            if (ajoutLait) lait -= 50
            if (choixSucre == "3") sucre -= 10

            if (choixBoisson == "3") {
              if (tailleLatte == "1") {
                poudreCafe -= 6
                lait -= 120
              } else if (tailleLatte == "2") {
                lait -= 150
                poudreCafe -= 8
              } else if (tailleLatte == "3") {
                poudreCafe -= 12
                lait -= 200
              }
            }

            if (choixBoisson == "1") poudreCafe -= 8
            if (choixBoisson == "2") {
              lait -= 100
              poudreCafe -= 6
            }

            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre boisson est prête ! Bonne dégustation !")
            commandeComplete = true
          } else {
            println("Choix invalide. Veuillez réessayer.")
          }
        }
      } else if (choix == "2") {
        println("Mode Admin")
        var pinEntree: String = ""
        println("Entrez le code PIN :")
        pinEntree = readLine("> ")

        if (pinEntree == codePin) {
          println("Accès autorisé.")
          println("Stocks actuels : Poudre de café: " + poudreCafe + " g, Sucre: " + sucre + " g, Lait: " + lait + " ml")
          println("Entrez les quantités à ajouter :")
          println("Poudre de café (g) :")
          var ajoutCafe: Int = readLine("> ").toInt
          println("Sucre (g) :")
          var ajoutSucre: Int = readLine("> ").toInt
          println("Lait (ml) :")
          var ajoutLait: Int = readLine("> ").toInt
          poudreCafe += ajoutCafe
          lait += ajoutLait
          sucre += ajoutSucre
          println("Stocks mis à jour.")
        } else {
          println("Code PIN incorrect.")
        }
      } else {
        println("Choix invalide. Veuillez réessayer.")
      }
    }
  }
}
