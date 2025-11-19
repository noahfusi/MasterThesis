import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Stocks initiaux
    var poudreCafe = 50 // en g
    var sucre = 30 // en g
    var lait = 0.5 // en litres

    var quitter = false
    var erreur = false
    while (quitter == false) { // boucle Menu principal

      // Menu principal
      println("         Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choixMode = readLine("> ").toInt
      while (!(choixMode == 1 || choixMode == 2 || choixMode == 3)) {
        choixMode = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
      }

      // Personnalisation de la commande
      var nomBoisson = "nom"
      var choixBoisson = 0
      var choixSucre = 0
      var choixLait = 0
      var nbDoseLait = 0

      // Prix
      val prixDoseSucre = 0.1 // CHF
      val prixDoseLait = 0.05 // CHF
      var prixBase = 0.0 // CHF
      var prixFinal = 0.0 // CHF

      // Mode Client
      if (choixMode == 1) {
        var modeClient = true
        while ((modeClient == true) || (erreur == true)) { // boucle Client

          // Sélection de la boisson
          println()
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choixBoisson = readLine("> ").toInt
          while (!(choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3)) {
            choixBoisson = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
          }

          // Expresso
          if (choixBoisson == 1) {
            // Ajout du sucre
            println()
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            choixSucre = readLine("> ").toInt
            while (!(choixSucre == 1 || choixSucre == 2 || choixSucre == 3 || choixSucre == 4)) {
              choixSucre = readLine("Veuillez choisir entre 1, 2, 3 et 4 > ").toInt
            }
            // Calculs + aperçu de la commande
            nomBoisson = "Expresso"
            prixBase = 2.0
            if (choixSucre == 1) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 8
                prixFinal = prixBase
                println()
                println("Boisson sélectionnée : Expresso")
                println("Niveau de sucre : Sans sucre")
                printf("Prix total : CHF %.2f.\n", prixFinal)
              }
            } else if (choixSucre == 2) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 5
                prixFinal = prixBase + prixDoseSucre
                println()
                println("Boisson sélectionnée : Expresso")
                println("Niveau de sucre : Peu (5g)")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixFinal)
              }
            } else if (choixSucre == 3) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 10
                prixFinal = prixBase + prixDoseSucre * 2
                println()
                println("Boisson sélectionnée : Expresso")
                println("Niveau de sucre : Moyen (10g)")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixFinal)
              }
            } else {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 15
                prixFinal = prixBase + prixDoseSucre * 3
                println()
                println("Boisson sélectionnée : Expresso")
                println("Niveau de sucre : Beaucoup (15g)")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixFinal)
              }
            }

            // Cappuccino
          } else if (choixBoisson == 2) {
            // Ajout du sucre
            println()
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            choixSucre = readLine("> ").toInt
            while (!(choixSucre == 1 || choixSucre == 2 || choixSucre == 3 || choixSucre == 4)) {
              choixSucre = readLine("Veuillez choisir entre 1, 2, 3 et 4 > ").toInt
            }
            // Ajout du lait
            println()
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            choixLait = readLine("> ").toInt
            while (!(choixLait == 1 || choixLait == 2)) {
              choixLait = readLine("Veuillez choisir entre 1 et 2 > ").toInt
            }
            if (choixLait == 1) {
              println("Combien de doses ? (Une dose contient 50 ml de lait. Vous ne pouvez pas ajouter plus de 3 doses.)")
              nbDoseLait = readLine("> ").toInt
              while (nbDoseLait > 3) {
                nbDoseLait = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
              }
            }
            // Calculs + aperçu de la commande
            nomBoisson = "Cappuccino"
            prixBase = 2.5
            if ((choixSucre == 1) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                lait -= 0.1
                prixFinal = prixBase
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f.\n", prixFinal)
              }
            } else if ((choixSucre == 2) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 5
                lait -= 0.1
                prixFinal = prixBase + prixDoseSucre
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixFinal)
              }
            } else if ((choixSucre == 3) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 10
                lait -= 0.1
                prixFinal = prixBase + prixDoseSucre * 2
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixFinal)
              }
            } else if ((choixSucre == 4) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 15
                lait -= 0.1
                prixFinal = prixBase + prixDoseSucre * 3
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixFinal)
              }
            } else if ((choixSucre == 1) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                lait -= (0.1 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseLait * nbDoseLait
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((choixSucre == 2) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 5
                lait -= (0.1 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre + prixDoseLait * nbDoseLait
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((choixSucre == 3) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 10
                lait -= (0.1 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 2 + prixDoseLait * nbDoseLait
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.1 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 15
                lait -= (0.1 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 3 + prixDoseLait * nbDoseLait
                println()
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixDoseLait * nbDoseLait, prixFinal)
              }
            }

            // Latte
          } else {
            // Taille du Latte
            println()
            println("Veuillez sélectionner la taille de votre Latte :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            var taille = readLine("> ").toInt
            while (!(taille == 1 || taille == 2 || taille == 3)) {
              taille = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
            }
            // Ajout du sucre
            println()
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            choixSucre = readLine("> ").toInt
            while (!(choixSucre == 1 || choixSucre == 2 || choixSucre == 3 || choixSucre == 4)) {
              choixSucre = readLine("Veuillez choisir entre 1, 2, 3 et 4 > ").toInt
            }
            // Ajout du lait
            println()
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            choixLait = readLine("> ").toInt
            while (!(choixLait == 1 || choixLait == 2)) {
              choixLait = readLine("Veuillez choisir entre 1 et 2 > ").toInt
            }
            if (choixLait == 1) {
              println("Combien de doses ? (Une dose contient 50 ml de lait. Vous ne pouvez pas ajouter plus de 3 doses.)")
              nbDoseLait = readLine("> ").toInt
              while (nbDoseLait > 3) {
                nbDoseLait = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
              }
            }
            nomBoisson = "Latte"
            // Prix de base
            if (taille == 1) {
              prixBase = 2.7
            } else if (taille == 2) {
              prixBase = 3.2
            } else {
              prixBase = 3.7
            }
            // Latte: petit
            if ((taille == 1) && (choixSucre == 1) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                lait -= 0.12
                prixFinal = prixBase // petit + sans sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f.\n", prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 2) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 5
                lait -= 0.12
                prixFinal = prixBase + prixDoseSucre // petit + peu de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 3) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 10
                lait -= 0.12
                prixFinal = prixBase + prixDoseSucre * 2 // petit + moyen sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 4) && (choixLait == 2)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 15
                lait -= 0.12
                prixFinal = prixBase + prixDoseSucre * 3 // petit + beaucoup de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 1) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                lait -= (0.12 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseLait * nbDoseLait // petit + sans sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 2) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 5
                lait -= (0.12 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre + prixDoseLait * nbDoseLait // petit + peu de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 3) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 10
                lait -= (0.12 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 2 + prixDoseLait * nbDoseLait // petit + moyen sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 1) && (choixSucre == 4) && (choixLait == 1)) {
              if (poudreCafe < 6) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (lait < 0.12 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else {
                erreur = false
                poudreCafe -= 6
                sucre -= 15
                lait -= (0.12 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 3 + prixDoseLait * nbDoseLait // petit + beaucoup de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Petit)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixDoseLait * nbDoseLait, prixFinal)
              }

              // Latte: moyen
            } else if ((taille == 2) && (choixSucre == 1) && (choixLait == 2)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                lait -= 0.15
                prixFinal = prixBase // moyen + sans sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f.\n", prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 2) && (choixLait == 2)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 5
                lait -= 0.15
                prixFinal = prixBase + prixDoseSucre // moyen + peu de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 3) && (choixLait == 2)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 10
                lait -= 0.15
                prixFinal = prixBase + prixDoseSucre * 2 // moyen + moyen sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 4) && (choixLait == 2)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 15
                lait -= 0.15
                prixFinal = prixBase + prixDoseSucre * 3 // moyen + beaucoup de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 1) && (choixLait == 1)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                lait -= (0.15 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseLait * nbDoseLait // moyen + sans sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 2) && (choixLait == 1)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 5
                lait -= (0.15 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre + prixDoseLait * nbDoseLait // moyen + peu de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 3) && (choixLait == 1)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 10
                lait -= (0.15 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 2 + prixDoseLait * nbDoseLait // moyen + moyen sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 2) && (choixSucre == 4) && (choixLait == 1)) {
              if (poudreCafe < 8) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.15 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 8
                sucre -= 15
                lait -= (0.15 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 3 + prixDoseLait * nbDoseLait // moyen + beaucoup de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Moyen)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixDoseLait * nbDoseLait, prixFinal)
              }

              // Latte: grand
            } else if ((taille == 3) && (choixSucre == 1) && (choixLait == 2)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                lait -= 0.2
                prixFinal = prixBase // grand + sans sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f.\n", prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 2) && (choixLait == 2)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 5
                lait -= 0.2
                prixFinal = prixBase + prixDoseSucre // grand + peu de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 3) && (choixLait == 2)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 10
                lait -= 0.2
                prixFinal = prixBase + prixDoseSucre * 2 // grand + moyen sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 4) && (choixLait == 2)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 15
                lait -= 0.2
                prixFinal = prixBase + prixDoseSucre * 3 // grand + beaucoup de sucre + sans lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Non")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 1) && (choixLait == 1)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                lait -= (0.2 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseLait * nbDoseLait // grand + sans sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 2) && (choixLait == 1)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 5) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 5
                lait -= (0.2 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre + prixDoseLait * nbDoseLait // grand + peu de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else if ((taille == 3) && (choixSucre == 3) && (choixLait == 1)) {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 10) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 10
                lait -= (0.2 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 2 + prixDoseLait * nbDoseLait // grand + moyen sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 2, prixDoseLait * nbDoseLait, prixFinal)
              }
            } else {
              if (poudreCafe < 12) { // Erreur poudre de café
                erreur = true
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (sucre < 15) { // Erreur sucre
                erreur = true
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else if (lait < 0.2 + 0.05 * nbDoseLait) { // Erreur lait
                erreur = true
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              } else {
                erreur = false
                poudreCafe -= 12
                sucre -= 15
                lait -= (0.2 + 0.05 * nbDoseLait)
                prixFinal = prixBase + prixDoseSucre * 3 + prixDoseLait * nbDoseLait // grand + beaucoup de sucre + lait sup.
                println()
                println("Boisson sélectionnée : Latte (Grand)")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire: Oui")
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f.\n", prixBase, prixDoseSucre * 3, prixDoseLait * nbDoseLait, prixFinal)
              }
            }
          }
          if (erreur == false) {
            // Interface de Paiement
            println()
            println("Veuillez payer en utilisant Twint.")
            var codeTwint = new String(Random.alphanumeric.take(5).toArray)
            println("Votre code de paiement est : " + codeTwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println()
            println("Paiement confirmé.")

            // Préparation de la Boisson
            println("Préparation de votre boisson...")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")
            modeClient = false
          }
        } // boucle Client

        // Mode Admin
      } else if (choixMode == 2) {
        var codePIN = readLine("Veuillez entrer le code PIN > ").toInt
        while (codePIN != 434343) {
          codePIN = readLine("Veuillez entrer le code PIN correct > ").toInt
        }
        println("Accès autorisé.")
        println()
        println("Stocks : ")
        println("  Poudre de café : " + poudreCafe + "g")
        println("  Sucre : " + sucre + "g")
        printf("  Lait : %.2fL \n", lait)
        println()
        println("Souhaitez-vous réapprovisionner les stocks ?")
        println("1) Oui")
        println("2) Non")
        var reapprovisionnement = readLine("> ").toInt
        while (!(reapprovisionnement == 1 || reapprovisionnement == 2)) {
          reapprovisionnement = readLine("Veuillez choisir entre 1 et 2 > ").toInt
        }
        if (reapprovisionnement == 1) {
          println()
          println("Réapprovisionnement des stocks...")
          println("Ajout :")
          var ajoutPoudre = readLine("  Poudre de café > ").toInt
          var ajoutSucre = readLine("  Sucre > ").toInt
          var ajoutLait = readLine("  Lait > ").toDouble
          poudreCafe += ajoutPoudre
          sucre += ajoutSucre
          lait += ajoutLait
          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
          // On pourrait indroduire des quantités max pour chaque ingrédient.
        }

        // Mode Quitter
      } else {
        quitter = true
      }
      println("---------------------------------")
    } // boucle Menu principal
  }
}