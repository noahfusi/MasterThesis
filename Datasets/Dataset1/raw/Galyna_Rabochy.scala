import scala.io.StdIn._
import scala.util.Random

object Nospresso {

  def main(args: Array[String]): Unit = {

    var poudredecafe: Int = 50
    var sucre: Int = 30
    var lait: Double = 0.5
    val codePin: String = "434343"

    var running = true

    while (running) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val modeChoice = readLine()
      if (modeChoice == null) {
        println("Non sélectionnée.")
        return
      } else if (modeChoice == "1") {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        val drinkChoice = readLine()

        var drink = ""
        var prixdebase = 0.0
        var cafequantite = 0
        var laitquantite = 0.0
        var niveaudesucre = 0
        var laitprix = 0.0
        var sucreprix = 0.0


        if (drinkChoice == null || drinkChoice.isBlank) {
          println("Non sélectionnée.")
          return
        } else if (drinkChoice == "1") {
          drink = "Expresso"
          prixdebase = 2.00
          cafequantite = 8
        } else if (drinkChoice == "2") {
          drink = "Cappuccino"
          prixdebase = 2.50
          cafequantite = 6
          laitquantite = 0.1
        } else if (drinkChoice == "3") {
          println("Choisissez une taille pour le Latte :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")


          val sizeChoice = readLine()

          if (sizeChoice == null || drinkChoice.isBlank) {
            println("Non sélectionnée.")
            return
          }else if (sizeChoice == "1") {
            drink = "Latte Petit"
            prixdebase = 2.70
            cafequantite = 6
            laitquantite = 0.12
          } else if (sizeChoice == "2") {
            drink = "Latte Moyen"
            prixdebase = 3.20
            cafequantite = 8
            laitquantite = 0.15
          } else if (sizeChoice == "3") {
            drink = "Latte Grand"
            prixdebase = 3.70
            cafequantite = 12
            laitquantite = 0.2
          } else {
            println("Option invalide pour le Latte.")
            return
          }
        }


        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        val sugarChoice = readLine()
        if (sugarChoice == null || drinkChoice.isBlank) {
          println("Non sélectionnée.")
          return
        }else if (sugarChoice == "1") {
          niveaudesucre = 0
          sucreprix = 0.00
        } else if (sugarChoice == "2") {
          niveaudesucre = 5
          sucreprix = 0.10
        } else if (sugarChoice == "3") {
          niveaudesucre = 10
          sucreprix = 0.20
        } else if (sugarChoice == "4") {
          niveaudesucre = 15
          sucreprix = 0.30
        } else {
          println("Option invalide pour le sucre.")
          return
        }


        if (drink == "Cappuccino" || drink.contains("Latte")) {
          println("Souhaitez-vous ajouter du lait supplémentaire ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
          print("> ")
          val addMilk = readLine()
          if (addMilk == null || drinkChoice.isBlank) {
            println("Non sélectionnée.")
            return
          }else if (addMilk == "1") {
            println("Combien de dose ?")
            print("> ")
            val doses = readInt()
            if (doses >= 0 && doses <= 3) {
              laitquantite += doses * 0.05
              laitprix = doses * 0.05
            } else {
              println("Quantité de doses invalide.")
            }
          } else if (addMilk != "2") {
            println("Option invalide pour le lait.")
            return
          }
        }
        poudredecafe -= cafequantite
        sucre -= niveaudesucre
        lait -= laitquantite

        if (poudredecafe < cafequantite) {
          println("Erreur : Quantité de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
        } else if (lait < laitquantite) {
          println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
        } else if (sucre < niveaudesucre) {
          println("Erreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
        } else {


          println("Boisson sélectionnée :" + drink+ " ")
          val Prixtotal = prixdebase + sucreprix + laitprix
          println("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f".format(prixdebase,sucreprix,laitprix,Prixtotal))
          println("Veuillez payer en utilisant Twint.")
          val paymentCode = Random.alphanumeric.take(5).mkString.toUpperCase()
          println("Votre code de paiement est :" + paymentCode + " ")
          println("(En attente de validation du paiement...)")
          Thread.sleep(5000)
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...\n [...]\n")
          Thread.sleep(5000)
          println("Votre " + drink + " est prêt ! Bonne dégustation !\n")

        }


      } else if (modeChoice == "2") {
        println("Mode Admin sélectionné.")
        print("Entrez le code PIN : ")
        val pin = readLine()
        if (pin == codePin) {
          println("Accès autorisé.")
          println("Stocks:\n Poudre de café ="+poudredecafe+" g\n Lait =" +lait+" L\n Sucre =" +sucre+" g")
          println("Souhaitez-vous réapprovisionner les stocks ?\n 1) Oui\n 2) Non")
          print("> ")
          val recharge = readLine()
          if (recharge == "1") {
            println("Entrez la quantité de café à ajouter (g) :")
            poudredecafe += readInt()
            println("Entrez la quantité de sucre à ajouter (g) :")
            sucre += readInt()
            println("Entrez la quantité de lait à ajouter (L) :")
            lait += readDouble()
            println("Ajout :\n Poudre de café =" +poudredecafe+ "\n Lait =" +lait+ "\n Sucre =" +sucre+ " ")
            println("Niveaux de stock mis à jour.")
            Thread.sleep(3000)
            println("Retour au menu principal...")

          }
        } else {
          println("Code PIN incorrect.")
        }

      } else if (modeChoice == "3") {
        println("Merci d'avoir utilisé Nospresso ! Au revoir.")
        running = false
      } else {
        println("Option invalide. Veuillez réessayer.")
      }
    }
  }
}
