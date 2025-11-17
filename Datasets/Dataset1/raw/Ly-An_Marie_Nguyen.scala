import io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Déclaration des Variables
    var mode = 0
    var boisson = 0
    var sucre = 0
    var lait = 0
    var latteTaille = 0
    var doseLait = 0

    var stockCafe = 50
    var stockSucre = 30
    var stockLait = 0.5

    var modePanne = 0

    do {
      // Réintialisation de certaines variable pour pas causer de problèmes
      boisson = 0
      sucre = 0
      lait = 0
      latteTaille = 0
      doseLait = 0

      if (modePanne == 0) {
        // ------------------- Sélection du mode -------------------
        println("       Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client\n2) Admin\n3) Quitter")
        mode = readLine(">").toInt
        while (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez entrer un mode valable")
          mode = readLine(">").toInt
        }
      }
      // ------------------- Mode Client -------------------
      if (mode == 1) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Espresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte")
        boisson = readLine(">").toInt
        while (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Veuillez entrer une boisson valable")
          boisson = readLine(">").toInt
        }
        // Séléction de la taille du Latte
        if (boisson == 3) {
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")

          latteTaille = readLine(">").toInt
          while (latteTaille != 1 && latteTaille != 2 && latteTaille != 3) {
            println("Veuillez entrer une taille valable")
            latteTaille = readLine(">").toInt
          }
        }

        // Sélection du sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        sucre = readLine(">").toInt
        while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez entrer une quantité de sucre valable")
          sucre = readLine(">").toInt
        }

        // Sélection du Lait
        if (boisson == 2 || boisson == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui\n2) Non")
          lait = readLine(">").toInt
          while (lait != 1 && lait != 2) {
            println("Veuillez entrer une valeur valable")
            lait = readLine(">").toInt
          }
          if (lait == 1) {
            println("Combien de dose ?")
            doseLait = readLine(">").toInt
            while (doseLait > 3) {
              println("Maximum 3 doses de lait")
              doseLait = readLine(">").toInt
            }
          }
        }


        // variable pour faciliter les prints (convertit les chiffres en leur nom en strings)
        // déclaration des prix
        // Manipulation des stocks des ingrédients
        var boissonName = ""
        var sucreName = ""
        var laitName = ""
        var tailleName = ""

        var boissonPrix = 0.0
        var sucrePrix = 0.0
        var laitSuppPrix = 0.0
        var total = 0.0

        // Tyype de boisson
        if (boisson == 1) {
          boissonName = "Expresso"
          boissonPrix = 2.00
          stockCafe = stockCafe - 8
        }
        else if (boisson == 2) {
          boissonName = "Cappuccino"
          boissonPrix = 2.50
          stockCafe = stockCafe - 6
          stockLait = stockLait - 0.1
        }
        else boissonName = "Latte"

        // Quantité de sucre
        if (sucre == 1) sucreName = "Sans sucre"
        else if (sucre == 2) {
          sucreName = "Peu (5g)"
          stockSucre = stockSucre - 5
          sucrePrix = 0.10
        }
        else if (sucre == 3) {
          sucreName = "Moyen (10g)"
          stockSucre = stockSucre - 10
          sucrePrix = 0.20
        }
        else {
          sucreName = "Beaucoup (15g)"
          stockSucre = stockSucre - 15
          sucrePrix = 0.30
        }


        // Quantité de lait
        if (lait == 2) laitName = "Non"
        else {
          laitName = "Oui (" + doseLait + ")"
          stockLait = stockLait - (doseLait * 0.05)
        }

        // Taille du Latte
        if (latteTaille == 1) {
          tailleName = "petit"
          boissonPrix = 2.70
          stockCafe = stockCafe - 6
          stockLait = stockLait - 0.12
        }
        else if (latteTaille == 2) {
          tailleName = "moyen"
          boissonPrix = 3.20
          stockCafe = stockCafe - 8
          stockLait = stockLait - 0.15
        }
        else if (latteTaille == 3) {
          tailleName = "grand"
          boissonPrix = 3.70
          stockCafe = stockCafe - 12
          stockLait = stockLait - 0.2
        }


        // Résumé de la commande
        print("Boisson sélectionnée : " + boissonName)
        if (boisson == 3)
          println(" (" + tailleName + ")")
        else println()
        println("Niveau de sucre : "+ sucreName)
        if (boisson == 2 || boisson == 3) {
          println("Lait en supplément : " + laitName)
        }

        // Vérification des stocks
        if (stockSucre >= 0 && stockCafe >= 0 && stockLait >= 0) {
          // Paiement
          if (lait == 1) {
            laitSuppPrix = 0.05 * doseLait
            total = boissonPrix + sucrePrix + laitSuppPrix
            printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, laitSuppPrix, total)
          }
          else {
            total = boissonPrix + sucrePrix
            printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, total)
          }

          println()
          println("Veuillez payer en utilisant Twint.")

          print("Votre code de paiement est : ")
          val codeChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          var index = 0
          for (i <- 0 to 4) {
            index = (math.random * 36).toInt
            print(codeChars(index))
          }
          println()
          println("(En attente de validation du paiement...)")
          // programme attend 3 secondes
          Thread.sleep(3000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre " + boissonName + " est prêt ! Bonne Dégustation !\n")
          modePanne = 0
        }
        else {
          if (stockSucre < 0) {
            println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            stockSucre = 0
          }
          else if (stockCafe < 0) {
            println("\nErreur : Quantité de poucre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            stockCafe = 0
          }
          else if (stockLait < 0 && (latteTaille == 2 || latteTaille == 3)) {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.\n")
          }
          else if (stockLait < 0) {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            stockLait = 0
          }
          modePanne = 1
        }
      }
      // ------------------- Mode Admin -------------------
      else if (mode == 2) {
        println("Mode Admin")
        val pin: String = readLine("Entrez le code PIN : ")
        if (pin == "434343") {
          println("Accès autorisé.\n")
          println("Stocks:")
          println("   Poudre de Café: " + stockCafe + "g")
          printf("   Lait          : %.1fL\n", stockLait)
          println("   Sucre         : " + stockSucre + "g")
          println()
          println("Réapprovisionnement des stocks...")
          println("Ajout:")
          stockCafe = stockCafe + readLine("   Poudre de Café: ").toInt
          stockLait = stockLait + readLine("   Lait          : ").toDouble
          stockSucre = stockSucre + readLine("   Sucre         : ").toInt
          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...\n")
        }
        else {
          println("Accès refusé. \n")
        }
      }
    } while (mode != 3)
  }
}