import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val code = 434343
    var mode = 0
    var prixboi: Double = 0.0
    var prixlait : Double = 0.0
    var prixsucre : Double = 0.0
    var taille = 0
    var sucre = 0
    var lait = 0
    var poudre = 50
    var sugar = 30
    var milk: Double = 0.500
    var qcafe = 0
    var qmilk: Double = 0.0
    var qsugar = 0
    while (mode != 3) {
      mode = 0
      prixboi = 0.0
      prixlait = 0.0
      prixsucre = 0.0
      sucre = 0
      lait = 0
      taille = 0
      println("    Nospresso Cafe")
      println("Veuillez sélectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      while (mode != 1 && mode != 2 && mode != 3) {
        mode = readLine(">").toInt
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez sélectionner 1, 2 ou 3")
        }
      }

      if (mode == 1) {
          println("Veuillez sélectionner votre boisson:")
          taille = 0
          lait = 0
          sucre = 0
          var boisson = 0
          var nom = ""
          var grandeur = ""
          var nivsucre = ""
          var laitier = ""
          println("1) Expresso - CHF 2.00")
          println("2) Capuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          while (boisson != 1 && boisson != 2 && boisson != 3) {
            boisson = readLine(">").toInt
            if (boisson != 1 && boisson != 2 && boisson != 3) {
              println("Veuillez sélectionner 1,2 ou 3")
            }
          }
          if (boisson == 1) {
            qcafe = 8
            prixboi = prixboi + 2.00
            nom = "Expresso"
          }
          if (boisson == 2) {
            qmilk = 0.100
            qcafe = 6
            prixboi = prixboi + 2.50
            nom = "Capuccino"
          }
          if (boisson == 3) {
            println("Vous avez choisi le Latte, choisissez une taille")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            while (taille != 1 && taille != 2 && taille != 3) {
              taille = readLine(">").toInt
              if (taille != 1 && taille != 2 && taille != 3) {
                println("Veuillez sélectionner 1,2 ou 3")
              }
            }
            if (taille == 1) {
              qmilk = 0.120
              qcafe = 6
              prixboi = prixboi + 2.70
              grandeur = "petit"
            }
            if (taille == 2) {
              qcafe = 8
              qmilk = 0.150
              prixboi = prixboi + 3.20
              grandeur = "moyen"
            }
            if (taille == 3) {
              qcafe = 12
              qmilk = 0.200
              prixboi = prixboi + 3.70
              grandeur = "grand"
            }
            nom = "Latte"
          }
          if (boisson == 1 || boisson == 2 || boisson == 3) {
            println("Souhaitez-vous ajouter du sucre?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
              sucre = readLine(">").toInt
              if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
                println("Veuillez sélectionner 1,2,3 ou 4")
              }
            }
            if (sucre == 1) {
              qsugar = 0
              prixsucre = prixsucre + 0.0
              nivsucre = "Sans"
            }
            if (sucre == 2) {
              qsugar = 5
              prixsucre = prixsucre + 0.10
              nivsucre = "Peu (5g)"
            }
            if (sucre == 3) {
              qsugar = 10
              prixsucre = prixsucre + 0.20
              nivsucre = "Moyen (10g)"
            }
            if (sucre == 4) {
              qsugar = 15
              prixsucre = prixsucre + 0.30
              nivsucre = "Beaucoup (15g)"
            }
          }
          if (boisson == 2 || boisson == 3) {
            println("Souhaitez-vous ajouter du lait en supplément?")
            println("1) oui")
            println("2) non")
            while (lait != 1 && lait != 2) {
              lait = readLine(">").toInt
              if (lait != 1 && lait != 2) {
                println("Veuillez sélectionner 1 ou 2")
              }
            }
            if (lait == 1) {
              println("Combien de dose?")
              laitier = "Oui"
              var dose = 0
              println("1) 50 ml - CHF 0.05")
              println("2) 100 ml - CHF 0.10")
              println("3) 150 ml - CHF 0.15")
              while (dose != 1 && dose != 2 && dose != 3) {
                dose = readLine(">").toInt
                if (dose != 1 && dose != 2 && dose != 3) {
                  println("Il y a uniquement 3 doses maximales")
                }
              }
              if (dose == 1) {
                qmilk = 0.050
                prixlait = prixlait + 0.05
              }
              if (dose == 2) {
                qmilk = 0.100
                prixlait = prixlait + 0.10
              }
              if (dose == 3) {
                qmilk = 0.150
                prixlait = prixlait + 0.15
              }
            }
            if (lait == 2) {
              laitier = "Non"
            }
          }
          println("Boisson séléctionnée : " + nom + " " + grandeur)
          println("Niveau de sucre : " + nivsucre)
          if (lait == 1 || lait == 2) {
            println("Lait supplémentaire : " + laitier)
          }
          Thread.sleep(2000)
          if (poudre < qcafe) {
            println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou remplir les stocks en mode Admin.")
            Thread.sleep(1500)
          }
          else if (milk < qmilk) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou remplir les stocks en mode Admin.")
            Thread.sleep(1500)
          }
          else if (sugar < qsugar) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou remplir les stocks en mode Admin.")
            Thread.sleep(1500)
          }
          else {
            poudre = poudre - qcafe
            milk = milk - qmilk
            sugar = sugar - qsugar
            printf("Prix total : CHF %.2f ", prixboi)
            printf("+ CHF %.2f ", prixsucre)
            printf("+ CHF %.2f ", prixlait)
            printf("= CHF %.2f ", prixlait + prixsucre + prixboi)
            Thread.sleep(2000)
            println("\nVeuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString)
            println("(En attente de de validation du paiement) ")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")
            Thread.sleep(1000)
            println("Préparation de votre boisson...")
            println("[...]")
            Thread.sleep(3000)
            println("Votre " + nom + " est prêt ! Bonne dégustation ! ")
            Thread.sleep(1500)
          }
        }
      }
      if (mode == 2) {
        println("Mode Admin")
        val codeclient = readLine("Entrez le code PIN : ").toInt
        if (code != codeclient){
          Thread.sleep(1500)
          println("Accès refusé.")
          Thread.sleep(1500)
        }
        else if (code == codeclient) {
          Thread.sleep(1000)
          println("Accès autorisé.")
          Thread.sleep(1000)
          println("")
          println("Stocks:")
          println("Poudre de café: " + poudre + "g")
          printf("Lait: %.2f", milk)
          print("L")
          println("\nSucre: " + sugar + "g")
          Thread.sleep(2000)
          println("Voulez-vous réapprovisionner les stocks?")
          println("1) Oui")
          println("2) Non")
          var stock = 0
          while (stock != 1 && stock != 2) {
            stock = readLine(">").toInt
            if (stock != 1 && stock != 2) {
              println("Veuillez sélectionner 1 ou 2")
            }
          }
          if (stock == 1) {
            println("Réapprovisionnement des stocks...")
            println("Ajout :")
            val ajoupoudre = readLine("Poudre de café: ").toInt
            poudre = poudre + ajoupoudre
            val ajoumilk = readLine("Lait:").toDouble
            milk = milk + ajoumilk
            val ajousugar = readLine("Sucre:").toInt
            sugar = sugar + ajousugar
            println("Niveau des stocks mis à jour")
            println("Retour au menu principal...")
            Thread.sleep(1500)
          }
          if (stock == 2){
            println("Retour au menu principal...")
            Thread.sleep(1500)
          }
        }
      }
    if (mode == 3) {
      println("Bonne journée, aurevoir!")
    }
  }
}