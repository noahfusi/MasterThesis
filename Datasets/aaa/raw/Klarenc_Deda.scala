import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var q_cafe = 50
    var q_sucre = 30
    var q_lait = 0.5
    var quit = false
    while (!quit) {
      println("	Nospresso Café")
      println("Veuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var choix_mode = -1
      while ((choix_mode < 1) || (choix_mode > 3)) {
        choix_mode = scala.io.StdIn.readInt()
        if ((choix_mode < 1) || (choix_mode > 3)){
          println("Veuillez entrer un nombre entre 1 et 3.")
        }
      }

      if (choix_mode == 1) {
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        var choix_cafe = -1
        while ((choix_cafe < 1) || (choix_cafe > 3)) {
          choix_cafe = scala.io.StdIn.readInt()
          if ((choix_cafe < 1) || (choix_cafe > 3)){
            println("Veuillez entrer un nombre entre 1 et 3.")
          }
        }
        if (choix_cafe == 3) {
          println("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
          while ((choix_cafe < 4) || (choix_cafe > 6)) {
            choix_cafe = scala.io.StdIn.readInt() + 3
            if ((choix_cafe < 4) || (choix_cafe > 6)){
              println("Veuillez entrer un nombre entre 1 et 3.")
            }
          }
        }
        println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
        var choix_sucre = -1
        while ((choix_sucre < 1) || (choix_sucre > 4)) {
          choix_sucre = scala.io.StdIn.readInt()
          if ((choix_sucre < 1) || (choix_sucre > 4)){
            println("Veuillez entrer un nombre entre 1 et 4.")
          }
        }
        var choix_lait = -1
        if (choix_cafe != 1) {
          println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
          while ((choix_lait < 1) || (choix_lait > 2)) {
            choix_lait = scala.io.StdIn.readInt()
            if ((choix_lait < 1) || (choix_lait > 2)){
              println("Veuillez entrer un nombre entre 1 et 2.")
            }
          }
          if (choix_lait == 1) {
            println("Combien de dose ?")
            while ((choix_lait < 3) || (choix_lait > 5)) {
              choix_lait = scala.io.StdIn.readInt() + 2
              if ((choix_lait < 3) || (choix_lait > 5)){
                println("Veuillez entrer un nombre entre 1 et 3.")
              }
            }
          }
        }
        var boisson = ""
        var sucre = ""
        var lait = ""
        var prix_base = 0.0
        var prix_sucre = 0.0
        var prix_lait = 0.0

        var conso_cafe = 0
        var conso_sucre = 0
        var conso_lait = 0.0

        if (choix_cafe == 1) {
          boisson = "Expresso"
          prix_base = 2.0
          conso_cafe = 8
        }
        else if (choix_cafe == 2) {
          boisson = "Cappuccino"
          prix_base = 2.5
          conso_cafe = 6
          conso_lait = 0.1
        }
        else if (choix_cafe == 4) {
          boisson = "Latte (Petit)"
          prix_base = 2.7
          conso_cafe = 6
          conso_lait = 0.120
        }
        else if (choix_cafe == 5) {
          boisson = "Latte (Moyen)"
          prix_base = 3.2
          conso_cafe = 8
          conso_lait = 0.150
        }
        else if (choix_cafe == 6) {
          boisson = "Latte (Grand)"
          prix_base = 3.7
          conso_cafe = 12
          conso_lait = 0.2
        }

        if (choix_sucre == 1) {
          sucre = "Sans sucre"
        }
        else if (choix_sucre == 2) {
          sucre = "Peu (5g)"
          prix_sucre = 0.1
          conso_sucre = 5
        }
        else if (choix_sucre == 3) {
          sucre = "Moyen (10g)"
          prix_sucre = 0.2
          conso_sucre = 10
        }
        else if (choix_sucre == 4) {
          sucre = "Beaucoup (15g)"
          prix_sucre = 0.3
          conso_sucre = 15
        }

        if (choix_lait == -1) {
          lait = "Non"
        }
        else if (choix_lait == 2) {
          lait = "Non"
        }
        else if (choix_lait == 3) {
          lait = "Une dose"
          prix_lait = 0.05
          conso_lait += 0.05
        }
        else if (choix_lait == 4) {
          lait = "Deux doses"
          prix_lait = 0.10
          conso_lait += 0.1
        }
        else if (choix_lait == 5) {
          lait = "Trois doses"
          prix_lait = 0.15
          conso_lait += 0.15
        }

        val prix_total = prix_base + prix_sucre + prix_lait

        println("Boisson sélectionnée : " + boisson)
        println("Niveau de sucre : " + sucre)
        println("Lait supplémentaire : " + lait)

        if (conso_cafe > q_cafe) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        else if (conso_sucre > q_sucre) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez sélectionner une quantité plus faible de sucre ou vérifier les stocks en mode Admin.")
        }
        else if (conso_lait > q_lait) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        }
        else {
          print("Prix total : ")
          printf("CHF %.2f", prix_base)
          if (prix_sucre > 0)
            printf(" + CHF %.2f", prix_sucre)
          if (prix_lait > 0)
            printf(" + CHF %.2f", prix_lait)
          printf(" = CHF %.2f\n", prix_total)

          println("Veuillez payer en utilisant Twint.")
          println(s"Votre code de paiement est : " + Random.alphanumeric.take(5).mkString(""))
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("\nMerci ! Votre paiement a été accepté.")
          println("\nPréparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
          q_cafe -= conso_cafe
          q_sucre -= conso_sucre
          q_lait -= conso_lait
        }
      }

      else if (choix_mode == 2) {
        println("Mode Admin")
        print("Entrez le code PIN : ")
        var code = 0
        code = scala.io.StdIn.readInt()
        if (code == 434343) {
          println("\nAccès autorisé.\n")
          var admin_quit = false
          while (!admin_quit) {
            println("Stocks:")
            println("	Poudre à café : " + q_cafe + "g")
            println("	Sucre : " + q_sucre + "g")
            printf("    Lait : %.2f l\n", q_lait)
            println("Veuillez sélectionner une option :\n1) Ajouter 25g de poudre à café\n2) Ajouter 15g de sucre\n3) Ajouter 0.25l de lait\n4) Retourner à la sélection du mode")
            var choix_admin = -1
            while ((choix_admin < 1) || (choix_admin > 4)) {
              choix_admin = scala.io.StdIn.readInt()
              if ((choix_admin < 1) || (choix_admin > 4)){
                println("Veuillez entrer un nombre entre 1 et 4.")
              }
            }
            if (choix_admin == 1) {
              q_cafe += 25
            }
            else if (choix_admin == 2) {
              q_sucre += 15
            }
            else if (choix_admin == 3) {
              q_lait += 0.25
            }
            else if (choix_admin == 4) {
              admin_quit = true
            }
          }
        }
        else
          println("Code PIN incorrect.")
      }

      else if (choix_mode == 3) {
        quit = true
      }
    }
  }
}
