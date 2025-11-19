import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    import io.StdIn._
    val continue = true
    var stock_poudre_de_cafe = 50.0
    var stock_sucre = 30.0
    var stock_lait = 0.5
    var prix_sucre = 0.0
    var prix_cafe = 0.0
    var prix_lait = 0.0
    val prix_un_sucre = 5
    val dose_de_lait = 0.05
    var consommation_cafe = 0.0
    var consommation_lait = 0.0
    var consommation_supplement_lait = 0.0
    var consommation_supplement_sucre = 0.0
    val caractères = "A1B2C3D4E5F6G7H8I9J1K2L3MN5OP7QR9S6T5U4V3W2X1YZ"
    if (continue == true) {
      while (continue == true) {
        println("Veuillez sélectionner le mode:\n1) Client\n2) Admin\n3) Quitter\n> ")
        var mode = readInt()
        while ((mode < 1) || (mode > 3)) {
          println("Le mode inscrit est incorrect\n")
          println("Veuillez sélectionner le mode:\n1) Client\n2) Admin\n3) Quitter\n > ")
          mode = readInt()
        }
        if (mode == 1) {
          var mode_1 = 1
          if (mode_1 < 2) {
            while (mode_1 < 2){
              println("Quel café voulez vous?:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
              var choix_de_cafe = readInt()
              while ((choix_de_cafe < 1) || (choix_de_cafe > 3)) {
                println("Le choix de café n'est pas correct\n")
                println("Quel café voulez vous?:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
                choix_de_cafe = readInt()
              }
              if (choix_de_cafe == 1) {
                prix_cafe = 2.0
                consommation_cafe = 8
              }
              if (choix_de_cafe == 2) {
                prix_cafe = 2.5
                consommation_cafe = 6
                consommation_lait = 0.1
              }
              var taille_latte = 0
              if (choix_de_cafe == 3) {
                println("Quel type de latte voulez vous?:\n1) CHF 2.70 (Petit) \n2)CHF 3.20 (Moyen)\n3)CHF 3.70 (Grand)\n>")
                taille_latte = readInt()
                while ((taille_latte < 1) || (taille_latte > 3)) {
                  println("Le choix de de latte n'est pas correct\n")
                  println("Quel type de latte voulez vous?:\n1) CHF 2.70 (Petit) \n2)CHF 3.20 (Moyen)\n3)CHF 3.70 (Grand)\n>")
                  taille_latte = readInt()
                }
                if (taille_latte == 1) {
                  prix_cafe = 2.7
                  consommation_cafe = 6
                  consommation_lait = 0.12
                }
                else if (taille_latte == 2) {
                  prix_cafe = 3.2
                  consommation_cafe = 8
                  consommation_lait = 0.15
                }
                else if (taille_latte == 3) {
                  prix_cafe = 3.7
                  consommation_cafe = 12
                  consommation_lait = 0.2
                }
              }
              var supplement_lait = 0
              if (choix_de_cafe == 2 || choix_de_cafe == 3) {
                println("Souhaitez-vous ajouter du lait en supplement ?\n1) Oui\n2) Non\n>")
                supplement_lait = readInt()
                while ((supplement_lait < 1) || (supplement_lait > 2)) {
                  println("Le choix de supplement de lait n'est pas correct\n")
                  println("Souhaitez-vous ajouter du lait en supplement ?\n1) Oui\n2) Non\n>")
                  supplement_lait = readInt()
                }

                if (supplement_lait == 1) {
                  println("Combien de doses voulez-vous?\n1) 1 doses\n2) 2 doses\n3) 3 doses\n >")
                  var nb_doses_de_lait = readInt()
                  if ((nb_doses_de_lait < 0) || (nb_doses_de_lait > 3)) {
                    while ((nb_doses_de_lait < 0) || (nb_doses_de_lait > 3)) {
                      println("Le choix de doses de lait n'est pas correct\n")
                      println("Combien de doses voulez-vous?\n1) 1 doses\n2) 2 doses\n3) 3 doses\n >")
                      nb_doses_de_lait = readInt()
                    }
                  }
                  else if (nb_doses_de_lait == 1) {
                    prix_lait = 0.05
                    consommation_supplement_lait = dose_de_lait * nb_doses_de_lait
                  }
                  else if (nb_doses_de_lait == 2) {
                    prix_lait = 0.1
                    consommation_supplement_lait = dose_de_lait * 2
                  }
                  else if (nb_doses_de_lait == 3) {
                    prix_lait = 0.15
                    consommation_supplement_lait = dose_de_lait * 1
                  }
                }
              }
              println("Souhaitez vous ajouter du sucre?: \n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
              var qte_sucre = readInt()
              while ((qte_sucre < 1) || (qte_sucre > 4)) {
                println("Le choix de sucre n'est pas correct\n")
                println("Souhaitez vous ajouter du sucre?: \n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
                qte_sucre = readInt()
              }
              if (qte_sucre == 2) {
                prix_sucre = 0.1
                consommation_supplement_sucre = prix_un_sucre * qte_sucre
              }
              else if (qte_sucre == 3) {
                prix_sucre = 0.2
                consommation_supplement_sucre = prix_un_sucre * qte_sucre
              }
              else if (qte_sucre == 4) {
                prix_sucre = 0.3
                consommation_supplement_sucre = prix_un_sucre * qte_sucre
              }
              val prix_final = prix_sucre + prix_lait + prix_cafe
              if (consommation_cafe > stock_poudre_de_cafe) {
                println("Erreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une autre boisson ou verifier les stocks en mode Admin.\n")
              }
              else if ((consommation_supplement_lait + consommation_lait) > stock_lait) {
                println("Erreur : Quantite de lait insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
              }
              else if (consommation_supplement_sucre > stock_sucre) {
                println("Erreur : Quantite de sucre insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
              }
              else {
                if (choix_de_cafe == 1){
                  println("Boisson séléctionnée : Expresso")
                }
                if (choix_de_cafe == 2){
                  println("Boisson séléctionnée : Cappucino")
                }
                if ((choix_de_cafe == 3) && (taille_latte == 1)){
                  println("Boisson séléctionnée : Latte (Petit) ")
                }
                if ((choix_de_cafe == 3) && (taille_latte == 2)){
                  println("Boisson séléctionnée : Latte (Moyen) ")
                }
                if ((choix_de_cafe == 3) && (taille_latte == 3)){
                  println("Boisson séléctionnée : Latte (Grand) ")
                }
                if (qte_sucre == 1){
                  println("Niveau de sucre : Sans sucre ")
                }
                if (qte_sucre == 2){
                  println("Niveau de sucre : Peu (5g) ")
                }
                if (qte_sucre == 3){
                  println("Niveau de sucre : Moyen (10g) ")
                }
                if (qte_sucre == 4){
                  println("Niveau de sucre : Beaucoup (15g) ")
                }
                if (supplement_lait == 2){
                  println("Lait en supplément: Non")
                }
                if (dose_de_lait == 1){
                  println("Lait en supplément: Peu (0.05L) ")
                }
                if (dose_de_lait == 2){
                  println("Lait en supplément: Moyen (0.1L) ")
                }
                if (dose_de_lait == 3){
                  println("Lait en supplément: Non: Beaucoup (0.15L) ")
                }
                if((prix_sucre > 0) && (prix_lait == 0)) {
                  println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_sucre + " = CHF " + prix_final+ "\n")
                }
                if((prix_sucre == 0) && (prix_lait > 0)) {
                  println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_lait + " = CHF " + prix_final + "\n")
                }
                if((prix_sucre > 0) && (prix_lait > 0)) {
                  println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_sucre +" + CHF " + prix_lait+ " = CHF " + prix_final + "\n")
                }
                if((prix_sucre == 0) && (prix_lait == 0)) {
                  println("Prix Total "+ "= CHF " + prix_final + "\n")
                }

                stock_sucre = stock_sucre - consommation_supplement_sucre
                stock_lait = stock_lait - consommation_lait - consommation_supplement_lait
                stock_poudre_de_cafe = stock_poudre_de_cafe - consommation_cafe
                println("Veuillez payer en utilisant Twint.")
                var twint = ""
                var i = 0
                while (i < 5) {
                  val nombre_et_caractères = Random.nextInt(caractères.length)
                  val carac = caractères.charAt(nombre_et_caractères)
                  twint = twint + carac
                  i = i + 1
                }
                mode_1 = mode_1 + 2
                println("Votre code de paiement: " + twint)
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("\nPaiement confirmé\n")
                println("Préparation de votre boisson...\n")
                Thread.sleep(5000)
                if (choix_de_cafe == 1) {
                  println("Votre Expresso est prêt ! Bonne dégustation !")
                }
                if (choix_de_cafe == 2){
                  println("Votre Cappucino est prêt ! Bonne dégustation !")
                }
                if (choix_de_cafe == 3){
                  println("Votre Latte est prêt ! Bonne dégustation !")
                }
              }
            }
          }
        }

        if (mode == 2) {
          println("Veuillez inscrire le code pin administrateur")
          var code_pin = readInt()
          if (code_pin != 434343) {
            while (code_pin != 434343) {
              print("Le code est incorrect.")
              println("Veuillez inscrire le code pin administrateur")
              code_pin = readInt()
            }
          }
          if (code_pin == 434343) {
            println ("Accès autorisé.")
            println("Stock disponible: \n")
            print("Stock de sucre: " + stock_sucre.toInt +"g" + "\nStock de poudre de café: " + stock_poudre_de_cafe.toInt + "g" + "\nStock de lait: " +
              stock_lait +"L" + "\n\n")
            println("Voulez-vous réapprovisionner les stocks?:\n1)Oui \n2)Non\n")
            var choix_du_reapprovisionnement = readInt()
            if (choix_du_reapprovisionnement < 1 || choix_du_reapprovisionnement > 2){
              while (choix_du_reapprovisionnement < 1 || choix_du_reapprovisionnement > 2) {
                println("Le choix est incorrect\n")
                println("Voulez-vous réapprovisionner les stocks?:\n1)Oui \n2)Non\n")
                choix_du_reapprovisionnement = readInt()
              }
            }
            if (choix_du_reapprovisionnement == 2) {
              println("Aurevoir.\n\n")
              Thread.sleep(2000)
            }
            else {
              println("Réapprovisionnement des stocks...")
              println("Ajout:\n")
              println("Poudre à café:")
              var reapprovisionnement_poudre_cafe = readInt()
              println("Lait:")
              var reapprovisionnement_lait = readInt()
              println("Sucre:")
              var reapprovisionnement_sucre = readInt()
              stock_sucre = stock_sucre + reapprovisionnement_sucre
              stock_lait = stock_lait + reapprovisionnement_lait
              stock_poudre_de_cafe = stock_poudre_de_cafe + reapprovisionnement_poudre_cafe
              println("Niveaux des stocks mis à jour.")
              println("Retour au menu principal...")
              Thread.sleep(2000)
            }

          }
        }
        if (mode == 3) {
          println("Aurevoir.\n\n")
          Thread.sleep(2000)
        }
      }
    }
  }
}

