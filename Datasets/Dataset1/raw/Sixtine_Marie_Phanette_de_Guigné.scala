import scala.io.StdIn._
import scala.util.Random


object Main {

  def main(args: Array[String]): Unit = {
    //Stock café
    var stockcafe = 50
    val poudrecafe = 8
    val consocafecapu = 6
    val consocafelattepetit = 6
    val consocafelattemoyen = 8
    val consocafelattegrand = 12

    //Stock sucre
    var stocksucre = 30
    val poudresucrepeu = 5
    val poudresucremoyen = 10
    val poudresucrebeaucoup = 15

    //Stock lait
    var stocklait = 500

    //mode admin
    val seuilcafe = 49
    val seuillait = 499
    val seuilsucre = 29


    val programme = true
    while (programme) {

      println("  Nospresso Café ")
      val Client: Unit = println(" 1) Client ")
      val Admin: Unit = println(" 2) Admin ")
      val Quitter: Unit = println(" 3) Quitter")
      val autre: Unit = println(">")

      val choix: Int = readChar()

      //menu client
      if (choix == '1') {
        var menu_client = true
        while (menu_client) {
          println("Veuillez séléctionner votre boisson : ")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n >")

          val choixBoisson: Int = readChar()

          //Expresso

          if (choixBoisson == '1') {
            if (stockcafe < poudrecafe) {
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
              Thread.sleep(1000)
              menu_client = false
            }
            else {
              var prixExpresso: Double = 2.0
              var prixsucre: Double = 0.0

              var sucre = true
              while (sucre) {
                println("Souhaitez-vous ajouter du sucre ? ")
                println("1) Sans sucre \n" +
                  "2) Peu (5g) - CHF 0.10 \n" +
                  "3) Moyen (10g) - CHF 0.20 \n" +
                  "4) Beaucoup (15g) - CHF 0.30 \n > ")

                val choix_sucre: Int = readChar()

                if (choix_sucre == '1') {
                  println("Boisson séléctionnée : Expresso")
                  Thread.sleep(1000)
                  println("Niveau de sucre : Sans sucre\n")
                  Thread.sleep(1000)
                  sucre = false
                }

                else if (choix_sucre == '2' && stocksucre >= poudresucrepeu) {
                  println("Boisson séléctionnée : Expresso ")
                  Thread.sleep(1000)
                  println("Niveau de sucre : Peu (5g)\n ")
                  Thread.sleep(1000)
                  prixsucre = 0.10
                  stocksucre -= poudresucrepeu
                  sucre = false
                }

                else if (choix_sucre == '3' && stocksucre >= poudresucremoyen) {
                  println("Boisson séléctionnée : Expresso")
                  Thread.sleep(1000)
                  println("Niveau de sucre : Moyen (10g) \n")
                  Thread.sleep(1000)
                  prixsucre = 0.20
                  stocksucre -= poudresucremoyen
                  sucre = false
                }
                else if (choix_sucre == '4' && stocksucre >= poudresucrebeaucoup) {
                  println("Boisson séléctionnée : Expresso ")
                  Thread.sleep(1000)
                  println("Niveau de sucre : Beaucoup (15g)\n")
                  Thread.sleep(1000)
                  prixsucre = 0.30
                  stocksucre -= poudresucrebeaucoup
                  sucre = false

                }

                else {
                  println("Choix non reconnu ou erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée. \n" +
                    "Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
                  Thread.sleep(1000)
                  println("Retour au menu principal.\n")
                  Thread.sleep(1000)
                  sucre = false
                  menu_client = false

                }
              }
              if (menu_client){


              val prixTotal = prixExpresso + prixsucre
              println(f"Prix total : $prixTotal%1.2f CHF")
                Thread.sleep(1000)

              //codetwint
              var codetwint = true
              while (codetwint) {
                val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
                println("\nVeuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
                Thread.sleep(3000)
                println("\nMerci ! Votre paiement a été accepté.")
                codetwint = false
                println("Paiement confirmé.\n" +
                  "Préparation de votre boisson...\n" +
                  "(...)\n")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !\n")
                Thread.sleep(3000)

                stockcafe -= poudrecafe
                menu_client = false
              }
            }
          }
          }


          //Cappuccino
          else if (choixBoisson == '2') {
            if (stockcafe < consocafecapu) {
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
              Thread.sleep(1000)
              menu_client = false
            }
            else {
              var prixcappuccino: Double = 2.50
              var prixsucre: Double = 0.0
              var prixlait: Double = 0.0

              var sucre = true
              while (sucre) {
                println("Souhaitez-vous ajouter du sucre ? ")
                println("1) Sans sucre \n" +
                  "2) Peu (5g) - CHF 0.10 \n" +
                  "3) Moyen (10g) - CHF 0.20 \n" +
                  "4) Beaucoup (15g) - CHF 0.30 \n > ")

                val choix_sucre: Int = readChar()

                if (choix_sucre == '1') {
                  println("Boisson séléctionnée : Cappuccino")
                  Thread.sleep(1000)
                  println("\nNiveau de sucre : Sans sucre")
                  Thread.sleep(1000)
                  prixsucre = 0.0
                  sucre = false
                }
                else if (choix_sucre == '2' && stocksucre >= poudresucrepeu) {
                  println("Boisson séléctionnée : Cappuccino")
                  Thread.sleep(1000)
                  println("\nNiveau de sucre : Peu (5g)")
                  Thread.sleep(1000)
                  stocksucre -= poudresucrepeu
                  prixsucre = 0.10
                  sucre = false
                }
                else if (choix_sucre == '3' && stocksucre >= poudresucremoyen) {
                  println("Boisson séléctionnée : Cappuccino")
                  Thread.sleep(1000)
                  println("\nNiveau de sucre : Moyen (10g)")
                  Thread.sleep(1000)
                  stocksucre -= poudresucremoyen
                  prixsucre = 0.20
                  sucre = false
                }
                else if (choix_sucre == '4' && stocksucre >= poudresucrebeaucoup) {
                  println("Boisson séléctionnée : Cappuccino")
                  Thread.sleep(1000)
                  println("\nNiveau de sucre : Beaucoup (15g)")
                  Thread.sleep(1000)
                  stocksucre -= poudresucrebeaucoup
                  prixsucre = 0.30
                  sucre = false
                }
                else {
                  println("\nChoix non reconnu ou erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
                  println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
                  Thread.sleep(1000)
                  sucre = false
                  menu_client = false
                }
              }
              if (menu_client){

              var lait = true
              while (lait) {
                println("\nSouhaitez-vous ajouter du lait en supplément ? \n" +
                  "(Disponible uniquement pour Cappuccino et Latte")
                println("1) Oui")
                println("2) Non")

                val choix_lait: Int = readChar()

                if (choix_lait == '1') {
                  var doselait = true
                  while (doselait) {
                    println("Combien de dose ?\n")
                    println("1) 1 dose (50mL)\n" +
                      "2) 2 doses (100mL)\n" +
                      "3) 3 doses (150mL)\n " +
                      ">")

                    val dose_lait: Int = readChar()

                    if (dose_lait == '1' && stocklait >= 50) {
                      println("\nLait en supplément : Oui (1 dose)")
                      Thread.sleep(1000)
                      prixlait = 0.05
                      stocklait -= 50
                      doselait = false
                      lait = false
                    }
                    else if (dose_lait == '2' && stocklait >= 100) {
                      println("\nLait en supplément : Oui (2 doses)")
                      Thread.sleep(1000)
                      prixlait = 0.10
                      stocklait -= 100
                      doselait = false
                      lait = false
                    }
                    else if (dose_lait == '3' && stocklait >= 150) {
                      println("\nLait en supplément : Oui (3 doses)")
                      Thread.sleep(1000)
                      prixlait = 0.15
                      stocklait -= 150
                      doselait = false
                      lait = false
                    }
                    else {
                      println("\nErreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
                      println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                      Thread.sleep(1000)
                      doselait = false
                      lait = false
                      menu_client = false
                    }
                  }
                }
                else if (choix_lait == '2') {
                  println("\nLait en supplément : Non")
                  Thread.sleep(1000)
                  prixlait = 0.0
                  lait = false
                }

                else {
                  println("\nCommande non reconnu. Veuillez réessayer\n")
                  Thread.sleep(1000)
                }
              }
              }
              if (menu_client){

              val prixTotal = prixcappuccino + prixsucre + prixlait
              println(f"Prix total : $prixTotal%1.2f CHF")
                Thread.sleep(1000)

              //codetwint
              var codetwint = true
              while (codetwint) {
                val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
                println("Veuillez payer en utilisant twint.")
                println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
                Thread.sleep(3000)
                println("\nMerci ! Votre paiement a été accepté.")
                codetwint = false
                println("Paiement confirmé.\n" +
                  "Préparation de votre boisson...\n" +
                  "(...)")
                Thread.sleep(3000)
                println("Votre Cappuccino est prêt ! Bonne dégustation !")
                Thread.sleep(3000)

                stockcafe -= consocafecapu
                menu_client = false
              }
          }
          }
          }

          // Latte
          else if (choixBoisson == '3') {
            var taille = true
            var prixlatte: Double = 0.0
            var prixsucre: Double = 0.0
            var prixlait: Double = 0.0
            menu_client = false

            // Choix de taille latte
            while (taille) {
              println("Choisissez la taille de votre Latte")
              println("1) Petit - CHF 2.70 \n" +
                "2) Moyen - CHF 3.20 \n" +
                "3) Grand - CHF 3.70 ")

              val choixtaille: Int = readChar()

              if (choixtaille == '1') {
                if (stockcafe < consocafelattepetit) {
                  println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
                  Thread.sleep(1000)
                  menu_client = false
                }
                else {
                  println("\nBoisson séléctionnée : Latte (Petit)")
                  Thread.sleep(1000)
                  prixlatte = 2.70
                  taille = false
                }
                stockcafe -= consocafelattepetit

                //choix sucre
                var sucre = true
                while (sucre) {
                  println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")

                  val choix_sucre: Int = readChar()

                  if (choix_sucre == '1') {
                    println("\nNiveau de sucre : Sans sucre")
                    Thread.sleep(1000)
                    prixsucre = 0.0
                    sucre = false
                  }
                  else if (choix_sucre == '2' && stocksucre >= poudresucrepeu) {
                    println("\nNiveau de sucre : Peu (5g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrepeu
                    prixsucre = 0.10
                    sucre = false
                  }
                  else if (choix_sucre == '3' && stocksucre >= poudresucremoyen) {
                    println("\nNiveau de sucre : Moyen (10g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucremoyen
                    prixsucre = 0.20
                    sucre = false
                  }
                  else if (choix_sucre == '4' && stocksucre >= poudresucrebeaucoup) {
                    println("\nNiveau de sucre : Beaucoup (15g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrebeaucoup
                    prixsucre = 0.30
                    sucre = false
                  }

                  else {
                    println("\nChoix non reconnu ou erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
                    println("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
                    Thread.sleep(1000)
                    sucre = false
                    menu_client = true
                  }
                }

                //choix dose lait

                var lait = true
                while (lait) {
                  println("Souhaitez-vous ajouter du lait en supplément ? \n" +
                    "Disponible uniquement pour Cappuccino et Latte ")
                  println("1) Oui")
                  println("2) Non")

                  val choix_lait: Int = readChar()
                  if (choix_lait == '1') {
                    println("Combien de dose ? ")
                    var doselait = true
                    while (doselait) {
                      println("1) 1 dose (60mL) \n" +
                        "2) 2 doses (120mL) \n" +
                        "3) 3 doses (180mL) \n >")

                      val dose_lait: Int = readChar()

                      if (dose_lait == '1' && stocklait >= 60) {
                        println("\nLait en supplément : Oui (1 dose)")
                        Thread.sleep(1000)
                        prixlait = 0.05
                        stocklait -= 60
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '2' && stocklait >= 120) {
                        println("\nLait en supplément : Oui (2 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.10
                        stocklait -= 120
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '3' && stocklait >= 180) {
                        println("\nLait en supplément : Oui (3 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.15
                        stocklait -= 180
                        doselait = false
                        lait = false
                      }
                      else {
                        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
                        println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                        Thread.sleep(1000)
                        doselait = false
                      }
                    }
                  }
                  else if (choix_lait == '2') {
                    println("\nLait en supplément : Non")
                    Thread.sleep(1000)
                    prixlait = 0.0
                    lait = false
                  }
                  else {
                    println("\nCommande non reconnu. Veuillez réessayer")
                    Thread.sleep(1000)
                  }
                }


              }
              else if (choixtaille == '2') {
                if (stockcafe < consocafelattemoyen) {
                  println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
                  Thread.sleep(1000)
                  menu_client = false
                }
                else {
                  println("\nBoisson séléctionnée : Latte (Moyen)")
                  Thread.sleep(1000)
                  prixlatte = 3.20
                  taille = false
                }
                stockcafe -= consocafelattemoyen

                //choix sucre
                var sucre = true
                while (sucre) {
                  println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")
                  val choix_sucre: Int = readChar()

                  if (choix_sucre == '1') {
                    println("\nNiveau de sucre : Sans sucre")
                    Thread.sleep(1000)
                    prixsucre = 0.0
                    sucre = false
                  }
                  else if (choix_sucre == '2' && stocksucre >= poudresucrepeu) {
                    println("\nNiveau de sucre : Peu (5g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrepeu
                    prixsucre = 0.10
                    sucre = false
                  }
                  else if (choix_sucre == '3' && stocksucre >= poudresucremoyen) {
                    println("\nNiveau de sucre : Moyen (10g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucremoyen
                    prixsucre = 0.20
                    sucre = false
                  }
                  else if (choix_sucre == '4' && stocksucre >= poudresucrebeaucoup) {
                    println("\nNiveau de sucre : Beaucoup (15g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrebeaucoup
                    prixsucre = 0.30
                    sucre = false
                  }
                  else {
                    println("\nChoix non reconnu ou erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
                    println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
                    Thread.sleep(1000)
                    sucre = false
                    menu_client = false
                  }
                }
                //choix dose lait
                var lait = true
                while (lait) {
                  println("Souhaitez-vous ajouter du lait en supplément ? \n" +
                    "Disponible uniquement pour Capuccino et Latte")
                  println("1) Oui")
                  println("2) Non")

                  val choix_lait: Int = readChar()
                  if (choix_lait == '1') {
                    println("Combien de dose ? ")
                    var doselait = true
                    while (doselait) {
                      println("1) 1 dose (75mL) \n" +
                        "2) 2 doses (150mL) \n" +
                        "3) 3 doses (225mL) \n >")

                      val dose_lait: Int = readChar()

                      if (dose_lait == '1' && stocklait >= 75) {
                        println("\nLait en supplément : Oui (1 dose)")
                        Thread.sleep(1000)
                        prixlait = 0.05
                        stocklait -= 75
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '2' && stocklait >= 150) {
                        println("\nLait en supplément : Oui (2 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.10
                        stocklait -= 150
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '3' && stocklait >= 225) {
                        println("\nLait en supplément : Oui (3 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.15
                        stocklait -= 225
                        doselait = false
                        lait = false
                      }
                      else {
                        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
                        println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                        Thread.sleep(1000)
                        doselait = false
                      }
                    }
                  }

                  else if (choix_lait == '2') {
                    println("\nLait en supplément : Non")
                    Thread.sleep(1000)
                    prixlait = 0.0
                    lait = false
                  }
                  else {
                    println("\nCommande non reconnu. Veuillez réessayer")
                    Thread.sleep(1000)
                  }
                }

              }
              else if (choixtaille == '3') {

                if (stockcafe < consocafelattegrand) {
                  println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
                  Thread.sleep(1000)
                  menu_client = false

                }
                else {
                  println("\nBoisson séléctionnée : Latte (Grand)")
                  Thread.sleep(1000)
                  prixlatte = 3.70
                  taille = false
                }
                stockcafe -= consocafelattegrand
                //choix sucre
                var sucre = true
                while (sucre) {
                  println("Souhaitez-vous ajouter du sucre")
                  println("1) Sans sucre \n" +
                    "2) Peu (5g) - CHF 0.10 \n" +
                    "3) Moyen (10g) - CHF 0.20 \n" +
                    "4) Beaucoup (15g) - CHF 0.30\n >")
                  val choix_sucre: Int = readChar()

                  if (choix_sucre == '1') {
                    println("\nNiveau de sucre : Sans sucre")
                    Thread.sleep(1000)
                    prixsucre = 0.0
                    sucre = false
                  }
                  else if (choix_sucre == '2' && stocksucre >= poudresucrepeu) {
                    println("\nNiveau de sucre : Peu (5g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrepeu
                    prixsucre = 0.10
                    sucre = false
                  }
                  else if (choix_sucre == '3' && stocksucre >= poudresucremoyen) {
                    println("\nNiveau de sucre : Moyen (10g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucremoyen
                    prixsucre = 0.20
                    sucre = false
                  }
                  else if (choix_sucre == '4' && stocksucre >= poudresucrebeaucoup) {
                    println("\nNiveau de sucre : Beaucoup (15g)")
                    Thread.sleep(1000)
                    stocksucre -= poudresucrebeaucoup
                    prixsucre = 0.30
                    sucre = false
                  }
                  else {
                    println("\nChoix non reconnu ou erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
                    println ("Veuillez réessayer ou veuillez choisir une quantité de sucre plus petit ou essayer une autre boisson")
                    Thread.sleep(1000)
                    sucre = false
                    menu_client = false
                  }
                }
                //choix dose lait
                var lait = true
                while (lait) {
                  println("Souhaitez-vous ajouter du lait en supplément ?\n" +
                    "Disponible uniquement pour Cappuccino et Latte ")
                  println("1) Oui")
                  println("2) Non")

                  val choix_lait: Int = readChar()
                  if (choix_lait == '1') {
                    println("Combien de dose ? ")
                    var doselait = true
                    while (doselait) {
                      println("1) 1 dose (100mL) \n" +
                        "2) 2 doses (200mL) \n" +
                        "3) 3 doses (300mL) \n >")

                      val dose_lait: Int = readChar()

                      if (dose_lait == '1' && stocklait >= 100) {
                        println("\nLait en supplément : Oui (1 dose)")
                        Thread.sleep(1000)
                        prixlait = 0.05
                        stocklait -= 100
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '2' && stocklait >= 200) {
                        println("\nLait en supplément : Oui (2 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.10
                        stocklait -= 200
                        doselait = false
                        lait = false
                      }
                      else if (dose_lait == '3' && stocklait >= 300) {
                        println("\nLait en supplément : Oui (3 doses)")
                        Thread.sleep(1000)
                        prixlait = 0.15
                        stocklait -= 300
                        doselait = false
                        lait = false
                      }
                      else {
                        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
                        println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
                        doselait = false
                        Thread.sleep(1000)
                      }
                    }
                  }
                  else if (choix_lait == '2') {
                    println("\nLait en supplément : Non")
                    Thread.sleep(3000)
                    prixlait = 0.0
                    lait = false
                  }
                  else {
                    println("\nCommande non reconnu. Veuillez réessayer")
                    Thread.sleep(3000)
                  }
                }

              }
              else {
                println("\nChoix non reconnu. Veuillez réessayer")
                Thread.sleep(3000)
              }
            }
            //paiement
            val prixTotal: Double = prixlatte + prixsucre + prixlait
            println(f"Prix total : $prixTotal%1.2f CHF ")
            Thread.sleep(1000)

            //codetwint
            var codetwint = true
            while (codetwint) {
              val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
              println("Veuillez payer en utilisant twint.")
              println("Votre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
              Thread.sleep(3000)
              println("\nMerci ! Votre paiement a été accepté.")
              codetwint = false
              println("Paiement confirmé.\n" +
                "Préparation de votre boisson...\n" +
                "(...)")
              Thread.sleep(3000)
              println("Votre Latte est prêt ! Bonne dégustation !")
              Thread.sleep(3000)

              menu_client = false
            }

        }
      }
      }

      else if (choix == '2') {
        var mode_admin = true
        while (mode_admin) {
          println("Entrée le code PIN :")
          //mode admin
          val PIN = 434343
          val codePIN = readInt()
          mode_admin = false
          if (PIN == codePIN) {
            println("Accès autorisée \n")
            Thread.sleep(3000)


              println ("Stocks initiaux: \n" +
                "1) Poudre de café : 50g \n" +
                "2) Lait : 500 mL \n" +
                "3) Sucre : 30g \n")
              Thread.sleep(3000)

            println ("Quantités actuelles dans les stocks : \n" +
              s"1) Poudre de café : $stockcafe g")
            println (s"2) Lait : $stocklait mL")
            println (s"3) Sucre : $stocksucre g")
            Thread.sleep(3000)

              if (stockcafe <= seuilcafe || stocklait <= seuillait || stocksucre <= seuilsucre){
                println ("\nRéapprovisionnement des stocks...")
                println ("\nEntrez la quantité à ajouter pour chaque produit : ")
                Thread.sleep(3000)

                println ("Ajout de poudre de café (grammes) : ")
                val ajoutcafe: Int = readInt()
                  stockcafe += ajoutcafe

                println ("Ajout de lait (en millilitres)")
                val ajoutlait: Int = readInt()
                  stocklait += ajoutlait

                println("Ajout de sucre (grammes)")
                val ajoutsucre: Int = readInt()
                  stocksucre += ajoutsucre


                println ("Niveaux de stocks mis à jour. ")
                Thread.sleep(3000)
                println ("Retour au menu principal...")
                Thread.sleep(3000)

              }



          }
          else {
            println("Code PIN incorrect. Veuillez réessayer")
            Thread.sleep(3000)
            mode_admin = false
          }
        }
      }
      else if (choix == '3'){
        var mode_quitter = true
        while (mode_quitter){
          println ("Merci ! Au revoir ")
          Thread.sleep(3000)
          mode_quitter = false
        }

      }


    }
  }
}



