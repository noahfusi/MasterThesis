import scala.io.StdIn.readInt
import math._

object Main {
  def main(args: Array[String]): Unit = {

    var poudrecafeinitial = 50
    var sucreinitial = 30
    var laitinitial = 500
    var mode = 0

    do {
      println("Noespresso Café\n Veuillez sélectionner votre mode:\n 1) Client \n 2) Admin \n 3) Quitter \n >")
      mode = readInt()
    }
    while (mode != 1 && mode != 2 && mode != 3)

    if (mode == 1) {
      var choixboisson = 0
      do {
        println("Veuillez sélectionner votre boisson\n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n >")
        choixboisson = readInt()
      }
      while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3)

      var sucre = 0
      do {
        println("Souhaitez-vous ajouter du sucre ?\n 1) Sans sucre - CHF 0.00 \n 2) Peu (5g) - CHF 0.10\n 3) Moyen (10g) - CHF 0.20\n 4) Beaucoup (15g) - CHF 0.30\n > ")
        sucre = readInt()
      }
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
      var sucreutilise = 5 * (sucre - 1)
      var prixdusucre = (sucre - 1) * 0.10


      var dose = 0
      var prixdose = 0.00
      if (choixboisson == 2 || choixboisson == 3) {
        var lait = 0
        do {
          println("Souhaitez-vous ajouter du lait en supplément ?\n 1) Oui \n 2) Non\n >")
          lait = readInt()
        }
        while (lait != 1 && lait != 2)

        if (lait == 1) {
          do {
            println("Combien de dose ?\n 1) 1 dose (50mL) - CHF 0.05\n 2) 2 dose (100mL) - CHF 0.10\n 3) 3 dose (150mL) - CHF 0.15\n  >")
            dose = readInt()
          }
          while (dose != 1 && dose != 2 && dose != 3)
          prixdose = dose * 0.05
        }
      }


      var poudrecafe = 0
      var laitutilise = 0
      var prix = 0.00
      if (choixboisson == 1) {
        poudrecafe = 8
        prix = 2.00
      }
      if (choixboisson == 2) {
        poudrecafe = 6
        laitutilise = 100
        prix = 2.50
      }

      if (choixboisson == 3) {
        var taille = 0
        do {
          println("Veuillez choisir votre taille:\n 1) Petit - CHF 2.70\n 2) Moyen - CHF 3.20\n 3) Grand - CHF 3.70\n >")
          taille = readInt()
        }
        while (taille != 1 && taille != 2 && taille != 3)

        if (taille == 1) {
          poudrecafe = 6
          laitutilise = 120
          prix = 2.70

        } else if (taille == 2) {

          poudrecafe = 8
          laitutilise = 150
          prix = 3.20

        } else if (taille == 3) {
          poudrecafe = 12
          laitutilise = 200
          prix = 3.70
        }
        laitutilise = laitutilise + dose * 50
      }


      if (poudrecafeinitial >= poudrecafe && laitinitial >= laitutilise && sucreinitial >= sucreutilise) {
        val totalprix = prix + prixdusucre + prixdose


        println(f"Prix total: CHF $totalprix%.2f. Veuillez payer en utilisant Twint.")


        val lettre = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        var aleatoire = ""

        for (1 <- 1 to 5) {
          aleatoire = aleatoire + lettre((Math.random() * lettre.length).toInt)
        }
        println("Votre code de paiement est:JK9ZL \n(En attente de validation du paiement)")
        Thread.sleep(3000)


        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre boisson est prête ! Bonne dégustation !")


        poudrecafeinitial -= poudrecafe
        laitinitial -= laitutilise
        sucreinitial -= sucreutilise

      } else {
        println("Erreur: Quantité de lait/poudre à café/sucre insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson ou alor vérifier les stocks en mode Admin")
      }

    } else if (mode == 2) {
      println("Mode Admin\nEntrer le code PIN :")


      var codepin = readInt()
      if (codepin == 434343) {
        println("Accès autorisé.")

        println(s"Stocks:\n\nPoudre de Café : $poudrecafeinitial g\nLait:  $laitinitial g\nSucre: $sucreinitial g")

        println("Veuillez entrer la quantité de poudre de café (en g) à ajouter ?")
        var ajoutpoudredecafe = readInt()
        poudrecafeinitial = poudrecafeinitial + ajoutpoudredecafe


        println("\nVeuillez entrer la quantité de lait (en mL) à ajouter ?")
        var ajoutlait = readInt()
        laitinitial = laitinitial + ajoutlait

        println("Veuillez entrer la quantité de sucre (en g) à ajouter ?")
        var ajoutsucre = readInt()
        sucreinitial = sucreinitial + ajoutsucre

        println("Réapprovisionnement des stocks...")
        Thread.sleep(5000)
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")

      }


      if (codepin != 434343) {
        println("Accès non autorisé ")

    if (mode == 3) {
          println("quitter\nRetour au menu principal")

        }
      }
    }
  }
}








