import io.StdIn.readLine
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {

    val StockI_PoudredeCafe: Double = 50.0
    val StockI_Sucre: Double = 30.0
    val StockI_Lait: Double = 0.500

    var Stock_PoudredeCafe: Double = 0.00
    var Stock_Sucre: Double = 0.00
    var Stock_Lait: Double = 0.00

    var deductionStock_cafe : Double = 0.0
    var deductionStock_lait : Double = 0.0
    var deductionStock_sucre : Double = 0.0
    var deductionStock_laitsupp : Double = 0.0

    var NouveauStock_PoudredeCafe : Double = 0.0
    var NouveauStock_Lait : Double = 0.0
    var NouveauStock_Sucre : Double = 0.0

    var prix_Expresso: Double = 2.00
    var prix_Cappuccino: Double = 2.50
    var prix_lait: Double = 0.00
    var prix_sucre: Double = 0.00
    var prix_Latte: Double = 0.00

    var Latte: Int = 0
    var lait: Int = 0
    var dose: Int = 0

    var boisson_selectionnee: String = "."
    var niveaudesucre: String = "."
    var laitsupp: String = "."

    val code_Pin = 434343
    val s = 17

    var mode: Int = 0
    var boisson : Int = 0
    var sucre : Int = 0


    Stock_PoudredeCafe = StockI_PoudredeCafe
    Stock_Lait = StockI_Lait
    Stock_Sucre = StockI_Sucre


    do {

      println("         Nospresso Café")
      println(" Veuilez sélectionner votre mode :")
      println(" 1) Client")
      println(" 2) Admin")
      println(" 3) Quitter")
      mode = readLine(" >").toInt
      println
      while ((mode < 1) || (mode > 3)) {
        if ((mode < 1) || (mode > 3)) {
          mode = readLine(" Veuillez entrer 1, 2 ou 3 pour sélectionner le mode que vous désirez").toInt
          println
        }
      }


      if (mode == 1) {

          do {
            println(" Veuillez sélectionner votre boissson :")
            println(" 1)Expresso - CHF 2.00")
            println(" 2)Cappuccino - CHF 2.50")
            println(" 3)Latte - CHF 2.70(Petit), CHF 3.20(Moyen), CHF 3.70(Grand)")
            boisson = readLine(" >").toInt
            println
            while ((boisson < 1) || (boisson > 3)) {
              if ((boisson < 1) || (boisson > 3)) {
                boisson = readLine(" Veuillez entrer un 1 pour choisir un Expresso, 2 pour un Cappuccino et 3 pour un Latte").toInt
                println
              }
            }


            if (boisson == 3) {
              Latte = readLine("Veuillez choisir la taille du Latte : entrer 1 pour le petit, " +
                "2 pour le moyen et 3 pour le grand").toInt
              while ((Latte < 1) || (Latte > 3)) {
                Latte = readLine("Veuillez choisir la taille du Latte : entrer 1 pour le petit," +
                  " 2 pour le moyen et 3 pour le grand ").toInt
              }
              if (Latte == 1) {
                prix_Latte = 2.70
              }
              if (Latte == 2) {
                prix_Latte = 3.20
              }
              if (Latte == 3) {
                prix_Latte = 3.70
              }
              println
            }


            println(" Souhaitez-vous ajouter du sucre ?")
            println(" 1) Sans sucre")
            println(" 2) Peu (5g) - CHF 0.10")
            println(" 3) Moyen (10g) - CHF 0.20")
            println(" 4) Beaucoup (15g) - CHF 0.30")
            sucre = readLine(" >").toInt
            while ((sucre < 1) || (sucre > 4)) {
              sucre = readLine(" Entrez : 1 pour une boisson sans sucre, 2 pour ajouter 5g, 3 pour ajouter 10g et 4 pour ajouter 15g").toInt
            }
            if (sucre == 1) {
              prix_sucre = 0.00
            }
            if (sucre == 2) {
              prix_sucre = 0.10
            }
            if (sucre == 3) {
              prix_sucre = 0.20
            }
            if (sucre == 4) {
              prix_sucre = 0.30
            }
            //prix_sucre = (sucre - 1) * 0.10
            println


            if ((boisson == 2) || (boisson == 3)) {

              println(" Souhaitez-vous ajouter du lait en supplément ?")
              println(" (Disponible uniquement pour Cappuccino et Latte)")
              println(" 1) Oui")
              println(" 2) Non")

              lait = readLine(" >").toInt
              while ((lait < 1) || (lait > 2)) {
                lait = readLine(" Veuillez entrer : 1 pour ajouter du lait supplémentaire et 2 pour ne pas le faire").toInt
              }
              println

              if (lait == 1) {
                println(" Combien de dose ? (entre 1 et 3)")
                dose = readLine(" >").toInt
                while ((dose < 1) || (dose > 3)) {
                  dose = readLine(" Veuillez entrer le nombre de doses entre 1 et 3 à rajouter").toInt
                }
                if (dose == 1) {
                  prix_lait = 0.05
                }
                if (dose == 2) {
                  prix_lait = 0.10
                }
                if ( dose == 3 ) {
                  prix_lait = 0.15
                }

                println
              }
              if (lait == 2) {
                dose = 0
                if (dose == 0) {
                  prix_lait = 0.00
                }
              }
            }


            if (boisson == 1) {
              boisson_selectionnee = "Expresso"
            }
            if (boisson == 2) {
              boisson_selectionnee = "Cappuccino"
            }
            if (boisson == 3) {
              if (Latte == 1) {
                boisson_selectionnee = "Latte (Petit)"
              }
              if (Latte == 2) {
                boisson_selectionnee = "Latte (Moyen)"
              }
              if (Latte == 3) {
                boisson_selectionnee = "Latte (Grand)"
              }
            }

            if (sucre == 1) {
              niveaudesucre = "Sans sucre"
            }
            if (sucre == 2) {
              niveaudesucre = "Peu (5g)"
            }
            if (sucre == 3) {
              niveaudesucre = "Moyen (10g)"
            }
            if (sucre == 4) {
              niveaudesucre = "Beaucoup (15g)"
            }

            if (lait == 1) {
              laitsupp = "Oui"
            }
            if (lait == 2) {
              laitsupp = "Non"
            }


            println(" Boisson sélectionnée : " + boisson_selectionnee)
            println(" Niveau de sucre : " + niveaudesucre)
            if ((boisson == 2) || (boisson == 3)) {
              println(" Lait supplémentaire : " + laitsupp)
            }


            if (boisson == 1) {
              deductionStock_cafe = 8
              deductionStock_lait = 0.000
              if ((Stock_PoudredeCafe - deductionStock_cafe) < 0) {
                println
                println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
                println
              }
            }

            if (boisson == 2) {
              deductionStock_cafe = 6
              deductionStock_lait = 0.100
              if ((Stock_PoudredeCafe - deductionStock_cafe) < 0) {
                println
                println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
                println
              }
              else if ((Stock_Lait - deductionStock_lait) < 0) {
                println
                println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println
              }
            }

            if (boisson == 3) {
              if (Latte == 1) {
                deductionStock_cafe = 6
                deductionStock_lait = 0.120
                if ((Stock_PoudredeCafe - deductionStock_cafe) < 0) {
                  println
                  println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
                  println
                }
                else if ((Stock_Lait - deductionStock_lait) < 0) {
                  println
                  println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println
                }

              }
              if (Latte == 2) {
                deductionStock_cafe = 8
                deductionStock_lait = 0.150
                if ((Stock_PoudredeCafe - deductionStock_cafe) < 0) {
                  println
                  println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
                  println
                }
                else if ((Stock_Lait - deductionStock_lait) < 0) {
                  println
                  println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println
                }
              }
              if (Latte == 3) {
                deductionStock_cafe = 12
                deductionStock_lait = 0.200
                if ((Stock_PoudredeCafe - deductionStock_cafe) < 0) {
                  println
                  println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
                  println
                }
                else if ((Stock_Lait - deductionStock_lait) < 0) {
                  println
                  println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println
                }
              }
            }


            if ((boisson == 1) || (boisson == 2) || (boisson == 3)) {
              if (niveaudesucre == "Sans sucre") {
                deductionStock_sucre = 0
              }
              if (niveaudesucre == "Peu (5g)") {
                deductionStock_sucre = 5
                if ((Stock_Sucre - deductionStock_sucre) < 0) {
                  println
                  println(" Erreur = Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson, une quantité plus faible de sucre ou alors vérifier les stocks en mode Admin.")
                  println
                }
              }
              if (niveaudesucre == "Moyen (10g)") {
                deductionStock_sucre = 10
                if ((Stock_Sucre - deductionStock_sucre) < 0) {
                  println
                  println(" Erreur = Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson, une quantité plus faible de sucre ou alors vérifier les stocks en mode Admin.")
                  println
                }
              }
              if (niveaudesucre == "Beaucoup (15g)") {
                deductionStock_sucre = 15
                if ((Stock_Sucre - deductionStock_sucre) < 0) {
                  println
                  println(" Erreur = Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println(" Veuillez choisir une autre boisson, une quantité plus faible de sucre ou alors vérifier les stocks en mode Admin.")
                  println
                }
              }
            }

            if (boisson == 1) {
              deductionStock_laitsupp = 0.000
            }
            if ((boisson == 2) || (boisson == 3)) {
              if (lait == 2) {
                deductionStock_laitsupp = 0.000
              }
              if (lait == 1) {
                if (dose == 1) {
                  deductionStock_laitsupp = 0.050
                  if ((Stock_Lait - deductionStock_laitsupp) < 0) {
                    println
                    println("Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    println
                  }
                }
                if (dose == 2) {
                  deductionStock_laitsupp = 0.100
                  if ((Stock_Lait - deductionStock_laitsupp) < 0) {
                    println
                    println("Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    println
                  }
                }
                if (dose == 3) {
                  deductionStock_laitsupp = 0.150
                  if ((Stock_Lait - deductionStock_laitsupp) < 0) {
                    println
                    println("Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    println
                  }
                }
              }
            }


            /*println(deductionStock_cafe)
            println(deductionStock_lait)
            println(deductionStock_laitsupp)
            println(deductionStock_sucre)*/

            Stock_PoudredeCafe = Stock_PoudredeCafe - deductionStock_cafe
            Stock_Lait = Stock_Lait - deductionStock_lait
            Stock_Lait = Stock_Lait - deductionStock_laitsupp
            Stock_Sucre = Stock_Sucre - deductionStock_sucre

            println



            /*println(Stock_PoudredeCafe)
            println(Stock_Lait)
            println(Stock_Sucre)*/

          } while ((Stock_PoudredeCafe < 0) || (Stock_Lait < 0) || (Stock_Sucre < 0))


          if ( (Stock_PoudredeCafe >= 0.0) && (Stock_Sucre  >= 0.0) && (Stock_Lait >= 0.000)) {

            if ( boisson == 1) {
              prix_Expresso = prix_Expresso + prix_sucre
              /*println (prix_Expresso)
              println (prix_sucre)*/
            }
            if ( boisson == 2) {
              prix_Cappuccino = prix_Cappuccino + prix_sucre + prix_lait
              /*println(prix_Cappuccino)
              println(prix_sucre)
              println(prix_lait)*/
            }
            if ( boisson == 3) {
              prix_Latte = prix_Latte + prix_sucre + prix_lait
              /*println(prix_Latte)
              println(prix_sucre)
              println(prix_lait)*/
            }


            if (boisson == 1) {
              if (sucre == 1) {
                printf(" Prix total : CHF 2.00 + CHF 0.00 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 2) {
                printf(" Prix total : CHF 2.00 + CHF 0.10 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 3) {
                printf(" Prix total : CHF 2.00 + CHF 0.20 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 4) {
                printf(" Prix total : CHF 2.00 + CHF 0.30 = CHF %.2f \n", prix_Expresso)
              }
            }
            if (boisson == 2) {
              if ((sucre == 1) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
            }
            if (boisson == 3) {
              if (Latte == 1) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
              if (Latte == 2) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }

              }
              if (Latte == 3) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
            }

            println

            if ( boisson == 1) {
              prix_Expresso = prix_Expresso - prix_sucre
              /*println (prix_Expresso)
              println (prix_sucre)*/
            }
            if ( boisson == 2) {
              prix_Cappuccino = prix_Cappuccino - prix_sucre
              prix_Cappuccino = prix_Cappuccino - prix_lait
              /*println(prix_Cappuccino)
              println(prix_sucre)
              println(prix_lait)*/
            }
            if ( boisson == 3) {
              prix_Latte = prix_Latte - prix_sucre
              prix_Latte = prix_Latte - prix_lait
              /*println(prix_Latte)
              println(prix_sucre)
              println(prix_lait)*/
            }


            val code_paiement = Random.alphanumeric.take(5).mkString
            println(" Veuillez payer en utilisant Twint.")
            println(" Votre code de paiement est : " + code_paiement)
            println(" (En attente de validation du paiement...)")
            Thread.sleep(3000) // 5 secondes 5000
            println
            println(" Merci! Votre paiement a été accepté.")
            println(" Préparation de votre boisson... ")
            println(" Votre " + boisson_selectionnee + " est prêt ! Bonne dégustation !")
            println
            println

          }

      }

          if (mode == 2) {
            println(" Mode Admin")
            var code = readLine(" Entrez le code Pin : ******").toInt

            while (code != code_Pin) {
              code = readLine(" Entrez le code Pin : ******").toInt
            }


            println(" Accès autorisé.")
            println(" Stocks : ")

            println("    Poudre de café : " + Stock_PoudredeCafe + "g")
            printf("    Lait : %.3fL \n", Stock_Lait)
            println("    Sucre : " + Stock_Sucre + "g")
            println
            println(" Réapprovisionnement des stocks...")
            println(" Ajout :")
            val Ajout_Stock_PoudredeCafe = readLine(" Quelle quantité de poudre de café comptez-vous ajouter aux stocks ?").toDouble
            val Ajout_Stock_Lait = readLine(" Quelle quantité de lait comptez-vous ajouter aux stocks ? ").toDouble
            val Ajout_Stock_Sucre = readLine(" Quelle quantité de sucre comptez-vous ajouter aux stocks ?").toDouble
            println("    Poudre de café : " + Ajout_Stock_PoudredeCafe)
            println("    Lait : " + Ajout_Stock_Lait)
            println("    Sucre : " + Ajout_Stock_Sucre)
            println(" Niveaux des stock mis à jour.")
            println(" Retour au menu principal...")
            println


            NouveauStock_PoudredeCafe = Stock_PoudredeCafe + Ajout_Stock_PoudredeCafe
            NouveauStock_Lait = Stock_Lait + Ajout_Stock_Lait
            NouveauStock_Sucre = Stock_Sucre + Ajout_Stock_Sucre

            Stock_PoudredeCafe = NouveauStock_PoudredeCafe
            Stock_Lait = NouveauStock_Lait
            Stock_Sucre = NouveauStock_Sucre

           /* println(Stock_PoudredeCafe)
            println(Stock_Lait)
            println(Stock_Sucre)*/

          }


          if (mode == 3) {
            println("Vous venez de quitter le menu principal.")
          }
    } while ((s != 6) && (mode != 3))

  }
}






