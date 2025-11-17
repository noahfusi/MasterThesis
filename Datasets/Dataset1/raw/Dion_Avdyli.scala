import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {

    // Nomination de toutes les  variables du programme
    var cafe = 50.0
    var sucreinitial = 30.0
    var laitinitial = 0.5
    val prixE = 2.0
    val prixC = 2.5
    val prixLP = 2.7
    val prixLM = 3.2
    val prixLG = 3.7
    var prixTotal = 0.0
    val prixsucre = 0.1
    val prixlait = 0.05

    // création de la répétition infinie du programme
    var boucle = true
    while (boucle == true){
      var menu = 0
      //construction du code aléatoire pour le paiement par code twint
      var codeTwint = ""
      for (_ <- 1 to 5) {
        val alphanumerique = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val caractere = (math.random() * 37).toInt
        codeTwint += alphanumerique(caractere)}

      // Interface de la machine
      println("\nNospresso Café")
      println("Bonjour, veuillez séletionner votre mode : ")
      //menu principal
      menu = readLine("\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      while ((menu != 1) && (menu != 2) && (menu != 3)) {
        println("Oups... Vous vous êtes trompés, veuillez sélectionner votre mode : ")
        menu = readLine("1) Client \n2) Admin \n3) Quitter \n> ").toInt}

      //menu client
      if (menu == 1) {
        var repet = false
        while (repet == false){
          println("Mode sélectionné : Client ")
          var choix = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) : \n> ").toInt
          while ((choix != 1) && (choix != 2) && (choix != 3)) {
            choix = readLine("Oups... Vous vous êtes trompés, veuillez taper : " + "\n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) : \n> ").toInt}

          // sélection = expresso
          if (choix == 1) {
            println("Boisson sélectionnée : Expresso ")
            //nombre de sucre dans le café
            var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toDouble
            val sucre = (nombredesucre - 1) * 5
            while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
              nombredesucre = readLine("\"Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

            if (nombredesucre == 1.0) println("Niveau de sucre : Sans sucre (0g) ")
            else if (nombredesucre == 2.0) println("Niveau de sucre : Peu de sucre (5g) ")
            else if (nombredesucre == 3.0) println("Niveau de sucre : Moyen (10g) ")
            else if (nombredesucre == 4.0) println("Niveau de sucre : Beaucoup de sucre (15g) ")
            //préparation du café
            if ((cafe >= 8) && (sucreinitial >= sucre)) {
              //println("Stock : Suffisant")
              boucle = true
              repet = true
              val prixsucrefinal = prixsucre * (nombredesucre - 1)
              prixTotal = prixE + prixsucrefinal
              if (nombredesucre == 1) { //prix sans le sucre
                printf("Prix total : CHF %.2f = CHF %.2f\n", prixE, prixTotal)
              } else { //prix avec le sucre
                printf("Prix total : CHF %.2f +  CHF %.2f = CHF %.2f\n", prixE, prixsucrefinal, prixTotal)}
              println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
              println("(En attente de paiement... )")
              Thread.sleep(3000)
              println("\nPaiement confirmé. ")
              println("Préparation de votre boisson...")
              Thread.sleep(5000)
              println("Votre expresso est prêt ! Bonne dégustation ! ")
              cafe = cafe - 8
              sucreinitial = sucreinitial - sucre}

            // cas de stocks insuffisants
            else if(cafe < 8) {
              println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
              boucle = false
              repet = false}
            else if (sucreinitial < sucre) {
              println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
              boucle = false
              repet = false}}

          if (choix == 2) {
            println("Boisson sélectionnée : Cappuccino")
            var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toDouble
            val sucre = (nombredesucre - 1) * 5
            while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
              nombredesucre = readLine("\"Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

            if (nombredesucre == 1.0) {
              println("Niveau de sucre : Sans sucre (0g) ")
            } else if (nombredesucre == 2.0) {
              println("Niveau de sucre : Peu de sucre (5g) ")
            } else if (nombredesucre == 3.0) {
              println("Niveau de sucre : Moyen (10g) ")
            } else if (nombredesucre == 4.0) {
              println("Niveau de sucre : Beaucoup de sucre (15g) ")}

            var nombredelait = 0.0
            var lait = 0.1 + 0.05 * nombredelait
            var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
            while ((laitsuppl != 1) && (laitsuppl != 2)) {
              laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}
            if (laitsuppl == 1) {
              nombredelait = readLine("Combien de doses ? \n> ").toDouble
              lait = 0.1 + 0.05 * nombredelait
              while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
                nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
              if (nombredelait == 1.0) {
                println("Lait en supplément : Oui (50ml) ")
              } else if (nombredelait == 2.0) {
                println("Lait en supplément : Oui (100ml) ")
              } else if (nombredelait == 3.0) {
                println("Lait en supplément : Oui (150ml) ")}

            } else if (laitsuppl == 2) {
              println("Lait supplémentaire : Non ")}

            if ((cafe > 6) && (sucreinitial >= sucre) && (laitinitial >= lait)) {
              //println("Stock : Suffisant")
              boucle = true
              repet = true
              val prixsucrefinal = prixsucre * (nombredesucre - 1)
              val prixlaitfinal = prixlait * nombredelait
              prixTotal = prixC + prixsucrefinal + prixlaitfinal
              if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
                printf("Prix total : CHF %.2f = CHF %.2f\n", prixC, prixTotal)
              } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixlaitfinal, prixTotal)
              } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixsucrefinal, prixTotal)
              } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixsucrefinal, prixlaitfinal, prixTotal)}

              println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
              println("(En attente de paiement... )")
              Thread.sleep(3000)
              println("\nPaiement confirmé. ")
              println("Préparation de votre boisson...")
              Thread.sleep(5000)
              println("Votre Cappuccino est prêt ! Bonne dégustation ! ")
              cafe = cafe - 6
              sucreinitial = sucreinitial - sucre
              laitinitial = laitinitial - lait}

            // cas de stocks insuffisants
            else if (cafe < 6) {
              println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
              boucle = false
              repet = false
            } else if (sucreinitial < sucre) {
              println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
              boucle = false
              repet = false
            } else if (laitinitial < lait) {
              println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
              boucle = false
              repet = false}}

          if (choix == 3) {
            var taille = readLine("\nVeuillez sélectionnez 1 pour Latte (Petit) - 2.7CHF, 2 pour Latte (Moyen), 3.2CHF, 3 pour Latte (Grand), 3.7 \n> ").toInt
            while ((taille != 1) && (taille != 2) && (taille != 3)) {
              taille = readLine("\nOups... vous vous êtes trompés, sélectionnez 1 pour Latte (Petit) - 2.7CHF, 2 pour Latte (Moyen), 3.2CHF, 3 pour Latte (Grand), 3.7 \n> ").toInt}

            if (taille == 1) {
              println("Boisson sélectionnée : Latte (Petit) ")
              var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toDouble
              val sucre = (nombredesucre - 1) * 5
              while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
                nombredesucre = readLine("Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

              if (nombredesucre == 1.0) {
                println("Niveau de sucre : Sans sucre (0g) ")
              } else if (nombredesucre == 2.0) {
                println("Niveau de sucre : Peu de sucre (5g) ")
              } else if (nombredesucre == 3.0) {
                println("Niveau de sucre : Moyen (10g) ")
              } else if (nombredesucre == 4.0) {
                println("Niveau de sucre : Beaucoup de sucre (15g) \"")}

              var nombredelait = 0.0
              var lait = 0.12 + 0.05 * nombredelait

              var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
              while ((laitsuppl != 1) && (laitsuppl != 2)) {
                laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

              if (laitsuppl == 1) {
                nombredelait = readLine("Combien de doses ? \n> ").toDouble
                lait = 0.12 + 0.05 * nombredelait
                while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
                  nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
                if (nombredelait == 1.0) {
                  println("Lait en supplément : Oui (50ml) ")
                } else if (nombredelait == 2.0) {
                  println("Lait en supplément : Oui (100ml) ")
                } else if (nombredelait == 3.0) {
                  println("Lait en supplément : Oui (150ml) ")}

              } else if (laitsuppl == 2) {
                println("Lait supplémentaire : Non ")}

              if ((cafe > 6) && (sucreinitial >= sucre) && (laitinitial >= lait)) {
                //println("Stock : Suffisant")
                boucle = true
                repet = true
                val prixsucrefinal = prixsucre * (nombredesucre - 1)
                val prixlaitfinal = prixlait * nombredelait
                prixTotal = prixLP + prixsucrefinal + prixlaitfinal
                if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
                  printf("Prix total : CHF %.2f = CHF %.2f\n", prixLP, prixTotal)
                } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixlaitfinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixsucrefinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixsucrefinal, prixlaitfinal, prixTotal)}
                println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
                println("(En attente de paiement... )")
                Thread.sleep(3000)
                println("\nPaiement confirmé. ")
                println("Préparation de votre boisson...")
                Thread.sleep(5000)
                println("Votre Latte est prêt ! Bonne dégustation ! ")
                cafe = cafe - 6
                sucreinitial = sucreinitial - sucre
                laitinitial = laitinitial - lait}

              // cas de stocks insuffisants
              else if (cafe < 6) {
                println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
                boucle = false
                repet = false
              } else if (sucreinitial < sucre) {
                println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
                boucle = false
                repet = false
              } else if (laitinitial < lait) {
                println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
                boucle = false
                repet = false}}

            if (taille == 2) {
              println("Boisson sélectionnée : Latte (Moyen) ")
              var nombredesucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toDouble
              val sucre = (nombredesucre - 1) * 5
              while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
                nombredesucre = readLine("\"Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

              if (nombredesucre == 1.0) {
                println("Niveau de sucre : Sans sucre (0g) ")
              } else if (nombredesucre == 2.0) {
                println("Niveau de sucre : Peu de sucre (5g) ")
              } else if (nombredesucre == 3.0) {
                println("Niveau de sucre : Moyen (10g) ")
              } else if (nombredesucre == 4.0) {
                println("Niveau de sucre : Beaucoup de sucre (15g) ")}
              var nombredelait = 0.0
              var lait = 0.15 + 0.05 * nombredelait
              var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
              while ((laitsuppl != 1) && (laitsuppl != 2)) {
                laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

              if (laitsuppl == 1) {
                nombredelait = readLine("Combien de doses ? \n> ").toDouble
                lait = 0.15 + 0.05 * nombredelait
                while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
                  nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
                if (nombredelait == 1.0) {
                  println("Lait en supplément : Oui (50ml) ")
                } else if (nombredelait == 2.0) {
                  println("Lait en supplément : Oui (100ml) ")
                } else if (nombredelait == 3.0) {
                  println("Lait en supplément : Oui (150ml) ")}

              } else if (laitsuppl == 2) {
                println("\nLait supplémentaire : Non ")}

              if ((cafe > 8) && (sucreinitial >= sucre) && (laitinitial >= lait)) {
                //println("Stock : Suffisant")
                boucle = true
                repet = true
                val prixsucrefinal = prixsucre * (nombredesucre - 1)
                val prixlaitfinal = prixlait * nombredelait
                prixTotal = prixLM + prixsucrefinal + prixlaitfinal
                if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
                  printf("Prix total : CHF %.2f = CHF %.2f\n", prixLM, prixTotal)
                } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixlaitfinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixsucrefinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixsucrefinal, prixlaitfinal, prixTotal)}
                println("\nVeuillet utilisant Twint. \nVotre code de paiement est : " + codeTwint)
                println("(En attente de paiement... )")
                Thread.sleep(3000)
                println("\nPaiement confirmé. ")
                println("Préparation de votre boisson...")
                Thread.sleep(5000)
                println("Votre Latte est prêt ! Bonne dégustation ! ")
                cafe = cafe - 8
                sucreinitial = sucreinitial - sucre
                laitinitial = laitinitial - lait}

              // cas de stocks insuffisants
              else if (cafe < 8) {
                println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
                boucle = false
                repet = false
              } else if (sucreinitial < sucre) {
                println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
                boucle = false
                repet = false
              } else if (laitinitial < lait) {
                println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
                boucle = false
                repet = false}}

            if (taille == 3) {
              println("Boisson sélectionnée : Latte (Grand) ")
              var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toDouble
              val sucre = (nombredesucre - 1) * 5
              while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
                nombredesucre = readLine("\nOups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

              if (nombredesucre == 1.0) {
                println("Niveau de sucre : Sans sucre (0g) ")
              } else if (nombredesucre == 2.0) {
                println("Niveau de sucre : Peu de sucre (5g) ")
              } else if (nombredesucre == 3.0) {
                println("Niveau de sucre : Moyen (10g) ")
              } else if (nombredesucre == 4.0) {
                println("Niveau de sucre : Beaucoup de sucre (15g) \n")}
              var nombredelait = 0.0
              var lait = 0.2 + 0.05 * nombredelait
              var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
              while ((laitsuppl != 1) && (laitsuppl != 2)) {
                laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

              if (laitsuppl == 1) {
                nombredelait = readLine("Combien de doses ? \n> ").toDouble
                lait = 0.2 + 0.05 * nombredelait
                while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
                  nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
                if (nombredelait == 1.0) {
                  println("Lait en supplément : Oui (50ml) ")
                } else if (nombredelait == 2.0) {
                  println("Lait en supplément : Oui (100ml) ")
                } else if (nombredelait == 3.0) {
                  println("Lait en supplément : Oui (150ml) ")}

              } else if (laitsuppl == 2) {
                println("Lait supplémentaire : Non ")}

              if ((cafe > 12) && (sucreinitial >= sucre) && (laitinitial >= lait)) {
                //println("Stock : Suffisant")
                boucle = true
                repet = true
                val prixsucrefinal = prixsucre * (nombredesucre - 1)
                val prixlaitfinal = prixlait * nombredelait
                prixTotal = prixLG + prixsucrefinal + prixlaitfinal
                if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
                  printf("Prix total : CHF %.2f = CHF %.2f\n", prixLG, prixTotal)
                } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixlaitfinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixsucrefinal, prixTotal)
                } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixsucrefinal, prixlaitfinal, prixTotal)}
                println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
                println("(En attente de paiement... )")
                Thread.sleep(3000)
                println("\nPaiement confirmé.")
                println("Préparation de votre boisson...")
                Thread.sleep(5000)
                println("Votre Latte est prêt ! Bonne dégustation ! ")
                cafe = cafe - 12
                sucreinitial = sucreinitial - sucre
                laitinitial = laitinitial - lait}

              // cas de stocks insuffisants
              else if (cafe < 12) {
                println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
                boucle = false
                repet = false
              } else if (sucreinitial < sucre) {
                println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
                boucle = false
                repet = false
              } else if (laitinitial < lait) {
                println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
                boucle = false
                repet = false}}}}}

      //mode admin
      if (menu == 2) {
        println("\nMode Admin : \nEntrez le code PIN : ****** : ")
        if (readInt() != 434343) {
          println("\nAccès refusé")
        } else {
          println("\nAccès autorisé. \nStocks : ")
          printf("   Poudre de café :  %.0f" + "g\n", cafe )
          printf("   Lait           :  %.1f" + "L\n", laitinitial )
          printf("   Sucre          :  %.0f" + "g\n", sucreinitial )

          //ajout de stock ou quitte le mode admin
          println("\nRéapprovisionnement des stocks... ")
          println("\nAjouts : ")
          print("   Poudre de café : ")
          cafe = cafe + readDouble()
          print("   Lait           : ")
          laitinitial = laitinitial + readDouble()
          print("   Sucre          : ")
          sucreinitial = sucreinitial + readDouble()
          println("Niveaux de stocks mis à jour. \nRetour au menu principal...")}}

      // quitter le programme => sortie de la boucle while, while = false
      if (menu == 3) {
        boucle = false
        println("Le programme a été quitté.")}}}}


