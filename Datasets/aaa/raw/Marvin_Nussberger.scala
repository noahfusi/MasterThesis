import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
object Nospresso {
  def main(args: Array[String]): Unit = {
    var stockcafe = 50
    var stocksucre = 30
    var stocklait = 500
    val code = "434343"
    var running = true
    while (running) {
      println("Nospresso Café")
      var mode = 0
      do {
        println("Veuillez sélectionner votre mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
      } while (mode != 1 && mode != 2 && mode != 3)
      if (mode == 1) {
        var boisson = 0
        do {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          boisson = readLine(">").toInt
        } while (boisson != 1 && boisson != 2 && boisson != 3)
        var cafededans = 0
        var laitdedans = 0
        var prix = 0.0
        var nomboisson = ""
        if (boisson == 1) {
          cafededans = 8
          prix = 2.0
          nomboisson = "Expresso"
        } else if (boisson == 2) {
          cafededans = 6
          laitdedans = 100
          prix = 2.5
          nomboisson = "Cappuccino"
        } else if (boisson == 3) {
          var tail = 0
          do {
            println("Choisissez la taille du Latte :")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            tail = readLine().toInt
          } while (tail != 1 && tail != 2 && tail != 3)
          if (tail == 1) {
            cafededans = 6
            laitdedans = 120
            prix = 2.7
            nomboisson = "Latte (Petit)"
          } else if (tail == 2) {
            cafededans = 8
            laitdedans = 150
            prix = 3.2
            nomboisson = "Latte (Moyen)"
          } else if (tail == 3) {
            cafededans = 12
            laitdedans = 200
            prix = 3.7
            nomboisson = "Latte (Grand)"
          }
        }
        var sucre = 0
        do {
          println("Souhatez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          sucre = readLine(">").toInt
        } while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
        var sucrededans = 0
        var prixS = 0.0
        var sucrelvl = "Sans sucre"
        if (sucre == 2) {
          sucrededans = 5
          prixS = 0.10
          sucrelvl = "Peu (5g)"
        } else if (sucre == 3) {
          sucrededans = 10
          prixS = 0.20
          sucrelvl = "Moyen (10g)"
        } else if (sucre == 4) {
          sucrededans = 15
          prixS = 0.30
          sucrelvl = "Beaucoup (15g)"
        }
        var laitsupp = 0
        var psupp = 0.0
        if (boisson == 2 || boisson == 3) {
          var choixSupplementLait = 0
          do {
            println("Souhaitez-vous ajouter du lait en suplément ? (disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            choixSupplementLait = readLine(">").toInt
          } while (choixSupplementLait != 1 && choixSupplementLait != 2)
          if (choixSupplementLait == 1) {
            var nbDosesLait = 0
            do {
              println("Combien de doses de lait supplémentaires ? (1 à 3x 50 ml)")
              nbDosesLait = readLine(">").toInt
            } while (nbDosesLait < 1 || nbDosesLait > 3)
            laitsupp = nbDosesLait * 50
            psupp = nbDosesLait * 0.05
          }
        }
        laitdedans += laitsupp
        if (stockcafe < cafededans) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        else if (stocklait < laitdedans) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        else if (stocksucre < sucrededans) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre option de sucre ou une autre boisson.")
        }
        else {
          def twint(): String = {
            val characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            var code = ""
            for (i <- 1 to 5) {
              val randomIndex = (math.random * characters.length).toInt
              code += characters(randomIndex)
            }
            code
          }
          val codeTwint = twint()
          val prixTotal = prix + prixS + psupp
          println(f"Veuillez payer en utilisant Twint. Prix total : CHF $prixTotal%.2f")
          println(s"Votre code de paiement est : $codeTwint")
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.")
          stockcafe -= cafededans
          stocklait -= laitdedans
          stocksucre -= sucrededans
          println("Votre boisson est en cours de préparation...")
          Thread.sleep(5000)
          println(s"Votre $nomboisson est prêt ! Bonne dégustation !")
        }
      } else if (mode == 2) {
        var codeentre = ""
        do {
          println("Entrez le code PIN : ")
          codeentre = readLine(">")
        } while (codeentre != code)
        println("Accès autorisé.")
        println(s"Stocks :")
        println(s"Poudre de café: $stockcafe g")
        println(s"Lait: ${stocklait / 1000.0} L")
        println(s"Sucre: $stocksucre g")
        var reapp = 0
        do {
          println("Réapprovisionnement ? 1) Oui, 2) Non")
          reapp = readLine().toInt
        } while (reapp != 1 && reapp != 2)
        if (reapp == 1) {
          println("Quantité de café à ajouter (g) : ")
          val caferea = readLine(">").toInt
          println("Quantité de sucre à ajouter (g) : ")
          val sucrerea = readLine(">").toInt
          println("Quantité de lait à ajouter (ml) : ")
          val laitrea = readLine(">").toInt
          stockcafe += caferea
          stocksucre += sucrerea
          stocklait += laitrea
          println(s"Niveaux de stock mis à jour :")
          println(s"Poudre de café: $stockcafe g")
          println(s"Lait: ${stocklait / 1000.0} L")
          println(s"Sucre: $stocksucre g")
          println("Retour au menu principal...")
        }
      } else if (mode == 3) {
        println("hasta la vista")
        running = false
      }
    }
  }
}