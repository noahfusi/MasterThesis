import scala.io.StdIn._
import scala.util.Random

object Main {
  var poudre = 50.0
  var sucre = 30.0
  var lait = 500.0

  def main(args: Array[String]): Unit = {
    while (true) {
      var choix = 0
      val boisson = Array("Expresso", "Cappuccino", "Latte (Petit)", "Latte (Moyen)", "Latte (Grand)")
      val tableausucre = Array("Sans sucre", "Peu (5g)", "Moyen (10g)", "Beacoup (15g)")
      val tableaulait = Array("Oui", "Non")
      val tableaupoudre = Array(8, 6, 6, 8, 12)
      val tableauqtesucre = Array(0, 5, 10, 15)
      val tableauqtelait = Array(0, 100, 120, 150, 200)

      val prixboisson = Array(2.00, 2.50, 2.70, 3.20, 3.70)
      val prixsucre = Array(0.00, 0.10, 0.20, 0.30)
      val prixlait = 0.05

      while (choix != 1 && choix != 2 && choix != 3) {
        println("Nospresso Cafe \nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter")
        choix = readInt()
      }
      if (choix == 1) {
        var choixboisson = 0
        while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3) {
          println("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choixboisson = readInt()
        }
        var choixtaille = 0
        if (choixboisson == 3) {
          while (choixtaille != 1 && choixtaille != 2 && choixtaille != 3) {
            println("\nVeuillez sélectionner la taille de votre latte: \n1) Petit \n2) Moyen \n3) Grand")
            choixtaille = readInt()
          }
        }
        var choixsucre = 0
        while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
          println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
          choixsucre = readInt()
        }
        var choixlait = 0
        var choixdose = 0
        if (choixboisson != 1) {
          while (choixlait != 1 && choixlait != 2) {
            println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte) \n1) Oui\n2) Non")
            choixlait = readInt()
          }
          if (choixlait == 1) {
            while (choixdose != 1 && choixdose != 2 && choixdose != 3) {
              println("\nCombien de dose ?")
              choixdose = readInt()
            }
          }
        }

        if (choixboisson == 3) {
          println("\nBoisson sélectionnée : " + boisson(choixboisson - 1 + choixtaille - 1))

        } else {
          println("\nBoisson sélectionnée : " + boisson(choixboisson - 1))

        }
        println("Niveau de sucre : " + tableausucre(choixsucre - 1))
        if (choixboisson != 1) {
          println("Lait supplémentaire: " + tableaulait(choixlait - 1))

        }
        var poudreqte = 0
        if (choixboisson == 3) {
          poudreqte = tableaupoudre(choixboisson - 1 + choixtaille - 1)

        } else {
          poudreqte = tableaupoudre(choixboisson - 1)

        }
        var laitqte = 0.0
        if (choixboisson == 2) {
          laitqte = tableauqtelait(choixboisson - 1)
        }
        if (choixboisson == 3) {
          laitqte = tableauqtelait(choixboisson - 1 + choixtaille - 1)
        }
        if (choixlait == 1) {
          laitqte += choixdose * 0.05
        }
        var sucreqte = tableauqtesucre(choixsucre - 1)
        if (poudreqte > poudre) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")

        }
        if (laitqte > lait) {
          println("Erreur : Quantité́ de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        if (sucreqte > sucre) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou vérifier les stocks en mode Admin.")
        }
        if (sucreqte <= sucre && laitqte <= lait && poudreqte <= poudre) {
          sucre -= sucreqte
          lait -= laitqte
          poudre -= poudreqte
          var prixtexte = ""
          var prix = 0.00
          if (choixboisson == 3) {
            prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1 + choixtaille - 1))
            prix = prixboisson(choixboisson - 1 + choixtaille - 1)
          } else {
            prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1))
            prix = prixboisson(choixboisson - 1)
          }

          if (choixsucre != 1) {
            prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, prixsucre(choixsucre - 1))
            prix += prixsucre(choixsucre - 1)

          }
          if (choixlait == 1) {
            prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, (choixdose * prixlait))
            prix += choixdose * prixlait

          }
          prixtexte += " = CHF " + "%.2f".formatLocal(java.util.Locale.US, prix)
          println("Prix total : " + prixtexte)
          println("\nVeuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString("").toUpperCase())
          println("(En attente de paiement...)\n")
          Thread.sleep(3000)
          println("Paiement confirmé.\nPréparation de votre boisson...")
          println("[...]")
          Thread.sleep(5000)
          println("Votre " + boisson(choixboisson - 1).split(" ")(0) + " est prêt ! Bonne dégustation !")
        }
      } else if (choix == 2) {
        print("Mode ADMIN\nEntrez le code PIN: ")
        var PIN = 434343
        var PINattempt = readInt()
        while (PIN != PINattempt) {
          print("Réessayer")
          var PINattempt = readInt()
        }

        println("Accès autorisé.\n\nStocks :")
        println("  Poudre de café : " + poudre + "g")
        println("  Lait           : " + lait / 1000.0 + "L")
        println("  Sucre          : " + sucre + "g")
        println("\nRéapprovisionnement des stocks...")

        println("Ajout :")
        print("  Poudre de café: ")
        val ajoutPoudre = readInt()
        print("  Lait          : ")
        val ajoutLait = readDouble()
        print("  Sucre         : ")
        val ajoutSucre = readInt()

        poudre += ajoutPoudre
        lait += ajoutLait * 1000
        sucre += ajoutSucre

        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal...")

      } else if (choix == 3) {
        return
      } else {
        print("Valeur non autorisée")
      }
      print("\n")
    }
  }
}