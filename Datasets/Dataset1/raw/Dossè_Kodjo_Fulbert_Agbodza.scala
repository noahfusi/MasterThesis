import io.StdIn._
import math._
import scala.reflect.ClassManifestFactory.Double
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var lancer = true
    var choix_mode = 0
    var code_pin = 0
    var choix_boisson = 0
    var choix_latte = 0
    var ajout_sucre = 0
    var ajout_lait = 0
    var cafe_initial = 50
    var sucre_initial = 30
    var lait_initial = 0.500
    val cafe_expresso = 8
    val cafe_cappuccino = 6
    val cafe_petit_latte = 6
    val cafe_moyen_latte = 8
    val cafe_grand_latte = 12
    var lait_cappuccino = 0.100
    var lait_petit_latte = 0.120
    var lait_moyen_latte = 0.150
    var lait_grand_latte = 0.200
    val sans_sucre = 0
    val sucre_peu = 5
    val sucre_moyen = 10
    val sucre_beaucoup = 15
    var zero_dose_lait = 0
    var une_dose_lait = 0.050
    var deux_dose_lait = 0.100
    var trois_dose_lait = 0.150
    val prix_expresso = 2.00
    val prix_cappuccino = 2.50
    val prix_petit_latte = 2.70
    val prix_moyen_latte = 3.20
    val prix_grand_latte = 3.70
    var prix_dose_lait = 0.05
    val prix_sans_sucre = 0.00
    val prix_sucre_peu = 0.10
    val prix_sucre_moyen = 0.20
    val prix_sucre_beaucoup = 0.30
    val lettreetnumero = ('A' to 'Z') ++ ('0' to '9')
    val random = new Random()
    val code_twint = (1 to 5).map(_ => lettreetnumero(random.nextInt(lettreetnumero.length))).mkString
    var nombre_dose_lait = 0
    var prix_nombre_dose_lait = 0






    while (lancer) {
      println("\n\n        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      choix_mode = readLine("> ").toInt
      while (choix_mode != 1 && choix_mode != 2 && choix_mode != 3) {
        println("Vous devez choisir entre 1, 2 et 3\n        Nospresso Café\nVeuillez s´electionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
        choix_mode = readLine("> ").toInt
      }
      if (choix_mode == 1) {

        var boucle_quantite_suffisant = true

        while (boucle_quantite_suffisant){

          println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choix_boisson = readLine("> ").toInt
          while (choix_boisson != 1 && choix_boisson != 2 && choix_boisson != 3) {
            println("Vous devez choisir entre 1, 2 et 3\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            choix_boisson = readLine("> ").toInt
          }
          if (choix_boisson == 1) {       // Expresso
            println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
            ajout_sucre = readLine("> ").toInt
            while (ajout_sucre != 1 &&  ajout_sucre != 2 && ajout_sucre != 3 && ajout_sucre != 4) {
              println("Vous devez choisir entre 1, 2, 3 et 4\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
              ajout_sucre = readLine("> ").toInt
            }
            if (ajout_sucre == 1) {                 // sans sucre
              println("Boisson sélectionnée : Expresso")
              println("Niveau de sucre : Sans sucre")
              if (cafe_initial >= cafe_expresso && sucre_initial >= sans_sucre) {
                cafe_initial -= cafe_expresso
                println(f"Prix total : CHF ${prix_expresso}%.2f + CHF  ${prix_sans_sucre}%.2f  = CHF ${prix_expresso+prix_sans_sucre}%.2f")
                println("\n\nVeuillez payer en utilisant Twint.")
                println(s"Votre code de paiement est : $code_twint")
                println("(En attente de paiement...)")
                Thread.sleep(3000)
                println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !")
                boucle_quantite_suffisant = false
              }
              else if (cafe_initial < cafe_expresso && sucre_initial >= sans_sucre) {
                println("\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
            }
            else if (ajout_sucre == 2) {                 // Peu de sucre
              println("Boisson sélectionnée : Expresso")
              println("Niveau de sucre : Peu (5g)")
              if (cafe_initial >= cafe_expresso && sucre_initial >= sucre_peu) {
                cafe_initial -= cafe_expresso
                sucre_initial -= sucre_peu
                println(f"Prix total : CHF ${prix_expresso}%.2f  + CHF  ${prix_sucre_peu}%.2f  = CHF ${prix_expresso+prix_sucre_peu}%.2f")
                println("\n\nVeuillez payer en utilisant Twint.")
                println(s"Votre code de paiement est : $code_twint")
                println("(En attente de paiement...)")
                Thread.sleep(3000)
                println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !")
                boucle_quantite_suffisant = false
              }
              else if (cafe_initial < cafe_expresso && sucre_initial >= sucre_peu) {
                println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else if (cafe_initial >= cafe_expresso && sucre_initial < sucre_peu) {
                println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else {
                println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes\npour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson et\nune autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
            }
            else if (ajout_sucre == 3) {                      //Sucre moyen
              println("Boisson sélectionnée : Expresso")
              println("Niveau de sucre : Moyen (10g)")
              if (cafe_initial >= cafe_expresso && sucre_initial >= sucre_moyen) {
                cafe_initial -= cafe_expresso
                sucre_initial -= sucre_moyen
                println(f"Prix total : CHF ${prix_expresso}%.2f  + CHF  ${prix_sucre_moyen}%.2f  = CHF ${prix_expresso+prix_sucre_moyen}%.2f")
                println("\n\nVeuillez payer en utilisant Twint.")
                println(s"Votre code de paiement est : $code_twint")
                println("(En attente de paiement...)")
                Thread.sleep(3000)
                println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !")
                boucle_quantite_suffisant = false
              }
              else if (cafe_initial < cafe_expresso && sucre_initial >= sucre_moyen) {
                println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else if (cafe_initial >= cafe_expresso && sucre_initial < sucre_moyen) {
                println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else {
                println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes\npour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson et\nune autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
            }
            else if (ajout_sucre == 4) {                     // Sucre beaucoup
              println("Boisson sélectionnée : Expresso")
              println("Niveau de sucre : Beaucoup (15g)")
              if (cafe_initial >= cafe_expresso && sucre_initial >= sucre_beaucoup) {
                cafe_initial -= cafe_expresso
                sucre_initial -= sucre_beaucoup
                println(f"Prix total : CHF ${prix_expresso}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f  = CHF ${prix_expresso+prix_sucre_beaucoup}%.2f")
                println("\n\nVeuillez payer en utilisant Twint.")
                println(s"Votre code de paiement est : $code_twint")
                println("(En attente de paiement...)")
                Thread.sleep(3000)
                println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                Thread.sleep(3000)
                println("Votre Expresso est prêt ! Bonne dégustation !")
                boucle_quantite_suffisant = false
              }
              else if (cafe_initial < cafe_expresso && sucre_initial >= sucre_beaucoup) {
                println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else if (cafe_initial >= cafe_expresso && sucre_initial < sucre_beaucoup) {
                println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
              else {
                println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes\npour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson et\nune autre quantité de sucre ou vérifier les\nstocks en mode Admin.")
                Thread.sleep(5000)
              }
            }
          }
          else if (choix_boisson == 2) {                            //Cappuccino
            println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
            ajout_sucre = readLine("> ").toInt
            while (ajout_sucre != 1 &&  ajout_sucre != 2 && ajout_sucre != 3 && ajout_sucre != 4) {
              println("Vous devez choisir entre 1, 2, 3 et 4\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
              ajout_sucre = readLine("> ").toInt
            }
            if (ajout_sucre == 1 ) {     //cappuccino sans sucre
              println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
              ajout_lait = readLine("> ").toInt
              while (ajout_lait != 1 && ajout_lait != 2) {
                println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
              }
              if (ajout_lait == 1) {
                println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                nombre_dose_lait = readLine("> ").toInt
                while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                  println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                }
                if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                  lait_cappuccino += une_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Une dose")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                  lait_cappuccino += deux_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Deux doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                  lait_cappuccino += trois_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Trois doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else {            // Sans lait en supplément
                nombre_dose_lait = 0
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Non")
                if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino) {
                  lait_initial -= lait_cappuccino
                  cafe_initial -= cafe_cappuccino
                  prix_dose_lait *= nombre_dose_lait
                  println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sans_sucre+prix_dose_lait}%.2f")
                  println("\n\nVeuillez payer en utilisant Twint.")
                  println(s"Votre code de paiement est : $code_twint")
                  println("(En attente de paiement...)")
                  Thread.sleep(3000)
                  println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                  Thread.sleep(3000)
                  println("Votre Cappuccino est prêt ! Bonne dégustation !")
                  boucle_quantite_suffisant = false
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino) {
                  println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino) {
                  println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                  Thread.sleep(5000)
                }
                else {
                  println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
              }
            }
            else if (ajout_sucre == 2) {     //cappuccino avec sucre Peu
              println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
              ajout_lait = readLine("> ").toInt
              while (ajout_lait != 1 && ajout_lait != 2) {
                println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
              }
              if (ajout_lait == 1) {
                println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                nombre_dose_lait = readLine("> ").toInt
                while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                  println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                }
                if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                  lait_cappuccino += une_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Une dose")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                  lait_cappuccino += deux_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Deux doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                  lait_cappuccino += trois_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Trois doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else {            // Sans lait en supplément
                nombre_dose_lait = 0
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Non")
                if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                  lait_initial -= lait_cappuccino
                  cafe_initial -= cafe_cappuccino
                  sucre_initial -= sucre_peu
                  prix_dose_lait *= nombre_dose_lait
                  println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_peu+prix_dose_lait}%.2f")
                  println("\n\nVeuillez payer en utilisant Twint.")
                  println(s"Votre code de paiement est : $code_twint")
                  println("(En attente de paiement...)")
                  Thread.sleep(3000)
                  println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                  Thread.sleep(3000)
                  println("Votre Cappuccino est prêt ! Bonne dégustation !")
                  boucle_quantite_suffisant = false
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_peu) {
                  println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_peu) {
                  println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                  println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_peu) {
                  println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_peu) {
                  println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                  println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else {
                  println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
              }
            }
            else if (ajout_sucre == 3) {     //cappuccino avec sucre Moyen
              println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
              ajout_lait = readLine("> ").toInt
              while (ajout_lait != 1 && ajout_lait != 2) {
                println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
              }
              if (ajout_lait == 1) {
                println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                nombre_dose_lait = readLine("> ").toInt
                while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                  println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                }
                if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                  lait_cappuccino += une_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Une dose")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                  lait_cappuccino += deux_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Deux doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                  lait_cappuccino += trois_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Trois doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else {                              // Sans lait en supplément
                nombre_dose_lait = 0
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Non")
                if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                  lait_initial -= lait_cappuccino
                  cafe_initial -= cafe_cappuccino
                  sucre_initial -= sucre_moyen
                  prix_dose_lait *= nombre_dose_lait
                  println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_moyen+prix_dose_lait}%.2f")
                  println("\n\nVeuillez payer en utilisant Twint.")
                  println(s"Votre code de paiement est : $code_twint")
                  println("(En attente de paiement...)")
                  Thread.sleep(3000)
                  println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                  Thread.sleep(3000)
                  println("Votre Cappuccino est prêt ! Bonne dégustation !")
                  boucle_quantite_suffisant = false
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_moyen) {
                  println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_moyen) {
                  println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                  println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_moyen) {
                  println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                  println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_moyen) {
                  println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else {
                  println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
              }
            }
            else if (ajout_sucre == 4) {     //cappuccino avec sucre beaucoup
              println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
              ajout_lait = readLine("> ").toInt
              while (ajout_lait != 1 && ajout_lait != 2) {
                println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
              }
              if (ajout_lait == 1) {
                println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                nombre_dose_lait = readLine("> ").toInt
                while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                  println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                }
                if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                  lait_cappuccino += une_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Une dose")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                  lait_cappuccino += deux_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Deux doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
                else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                  lait_cappuccino += trois_dose_lait
                  println("Boisson sélectionnée : Cappuccino")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Trois doses")
                  if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    cafe_initial -= cafe_cappuccino
                    lait_initial -= lait_cappuccino
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Cappuccino est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else {                              // Sans lait en supplément
                nombre_dose_lait = 0
                println("Boisson sélectionnée : Cappuccino")
                println("Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Non")
                if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                  lait_initial -= lait_cappuccino
                  cafe_initial -= cafe_cappuccino
                  sucre_initial -= sucre_beaucoup
                  prix_dose_lait *= nombre_dose_lait
                  println(f"Prix total : CHF ${prix_cappuccino}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_cappuccino+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                  println("\n\nVeuillez payer en utilisant Twint.")
                  println(s"Votre code de paiement est : $code_twint")
                  println("(En attente de paiement...)")
                  Thread.sleep(3000)
                  println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                  Thread.sleep(3000)
                  println("Votre Cappuccino est prêt ! Bonne dégustation !")
                  boucle_quantite_suffisant = false
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                  println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial >= sucre_beaucoup) {
                  println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                  println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial >= cafe_cappuccino && lait_initial < lait_cappuccino && sucre_initial < sucre_beaucoup) {
                  println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                  println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else if (cafe_initial < cafe_cappuccino && lait_initial >= lait_cappuccino && sucre_initial < sucre_beaucoup) {
                  println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
                else {
                  println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                  Thread.sleep(5000)
                }
              }
            }
          }
          else {                              // Latte
            println("Quel latte :\n1) Petit CHF 2.70 \n2) Moyen CHF 3.20\n3) Grand CHF 3.70")
            choix_latte = readLine("> ").toInt
            while (choix_latte != 1 && choix_latte != 2 && choix_latte != 3) {
              println("Vous devez choisir entre 1, 2 et 3\nQuel latte :\n1) Petit CHF 2.70 \n2) Moyen CHF 3.20\n3) Grand CHF 3.70")
              choix_latte = readLine("> ").toInt
            }
            if (choix_latte == 1) {               //Petit latte
              println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
              ajout_sucre = readLine("> ").toInt
              while (ajout_sucre != 1 &&  ajout_sucre != 2 && ajout_sucre != 3 && ajout_sucre != 4) {
                println("Vous devez choisir entre 1, 2, 3 et 4\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
                ajout_sucre = readLine("> ").toInt
              }
              if (ajout_sucre == 1 ) {     //  petit latte sans sucre
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_petit_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_petit_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_petit_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= lait_petit_latte && lait_initial < lait_petit_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Petit)")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte) {
                    lait_initial -= lait_petit_latte
                    cafe_initial -= cafe_petit_latte
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 2) {     //   petit latte avec sucre Peu
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_petit_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_petit_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_petit_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Petit)")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                    lait_initial -= lait_petit_latte
                    cafe_initial -= cafe_petit_latte
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 3) {     // petit latte avec sucre Moyen
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_petit_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_petit_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_petit_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Petit)")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                    lait_initial -= lait_petit_latte
                    cafe_initial -= cafe_petit_latte
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 4) {     // petit latte avec sucre beaucoup
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_petit_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_petit_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_petit_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Petit)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_petit_latte
                      lait_initial -= lait_petit_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Petit)")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                    lait_initial -= lait_petit_latte
                    cafe_initial -= cafe_petit_latte
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_petit_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_petit_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_petit_latte && lait_initial < lait_petit_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_petit_latte && lait_initial >= lait_petit_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
            }
            else if (choix_latte == 2) {                  // Moyen latte
              println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
              ajout_sucre = readLine("> ").toInt
              while (ajout_sucre != 1 &&  ajout_sucre != 2 && ajout_sucre != 3 && ajout_sucre != 4) {
                println("Vous devez choisir entre 1, 2, 3 et 4\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
                ajout_sucre = readLine("> ").toInt
              }
              if (ajout_sucre == 1 ) {     //  moyen latte sans sucre
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_moyen_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_moyen_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_moyen_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= lait_moyen_latte && lait_initial < lait_moyen_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Moyen)")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                    lait_initial -= lait_moyen_latte
                    cafe_initial -= cafe_moyen_latte
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 2) {     //   moyen latte avec sucre Peu
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_moyen_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_moyen_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_moyen_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Moyen)")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                    lait_initial -= lait_moyen_latte
                    cafe_initial -= cafe_moyen_latte
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 3) {     // moyen latte avec sucre Moyen
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_moyen_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_moyen_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_moyen_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Moyen)")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                    lait_initial -= lait_moyen_latte
                    cafe_initial -= cafe_moyen_latte
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 4) {     // moyen latte avec sucre beaucoup
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_moyen_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_moyen_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_moyen_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Moyen)")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                    lait_initial -= lait_moyen_latte
                    cafe_initial -= cafe_moyen_latte
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_moyen_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_moyen_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_moyen_latte && lait_initial < lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_moyen_latte && lait_initial >= lait_moyen_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
            }
            else {                                 // Grand latte
              println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
              ajout_sucre = readLine("> ").toInt
              while (ajout_sucre != 1 &&  ajout_sucre != 2 && ajout_sucre != 3 && ajout_sucre != 4) {
                println("Vous devez choisir entre 1, 2, 3 et 4\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
                ajout_sucre = readLine("> ").toInt
              }
              if (ajout_sucre == 1 ) {     //  grand latte sans sucre
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_grand_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_grand_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_grand_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Sans sucre")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Grand)")
                  println("Niveau de sucre : Sans sucre")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte) {
                    lait_initial -= lait_grand_latte
                    cafe_initial -= cafe_grand_latte
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sans_sucre}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sans_sucre+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 2) {     //   grand latte avec sucre Peu
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_grand_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_grand_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_grand_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Moyen)")
                    println("Niveau de sucre : Peu (5g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_peu
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_peu) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {            // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Grand)")
                  println("Niveau de sucre : Peu (5g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                    lait_initial -= lait_grand_latte
                    cafe_initial -= cafe_grand_latte
                    sucre_initial -= sucre_peu
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_peu}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_peu+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_peu) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson sans sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_peu) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 3) {     // grand latte avec sucre Moyen
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_grand_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_grand_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_grand_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Moyen (10g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_moyen
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_moyen) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Grand)")
                  println("Niveau de sucre : Moyen (10g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                    lait_initial -= lait_grand_latte
                    cafe_initial -= cafe_grand_latte
                    sucre_initial -= sucre_moyen
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_moyen}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_moyen) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec peu de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_moyen) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
              else if (ajout_sucre == 4) {     // grand latte avec sucre beaucoup
                println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                ajout_lait = readLine("> ").toInt
                while (ajout_lait != 1 && ajout_lait != 2) {
                  println("Vous devez choisir en 1 et 2\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
                  ajout_lait = readLine("> ").toInt
                }
                if (ajout_lait == 1) {
                  println("Combien de dose ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                  nombre_dose_lait = readLine("> ").toInt
                  while (nombre_dose_lait != 1 && nombre_dose_lait != 2 && nombre_dose_lait != 3) {
                    println("Le nombre de doses de lait supplémentaire est 3 au maximum\nCombien de dose ?\n1) Une dose (50ml) - CHF 0.05\n1) Deux doses (100ml) - CHF 0.10\n3) trois doses (150ml) - CHF 0.15")
                    nombre_dose_lait = readLine("> ").toInt
                  }
                  if (nombre_dose_lait == 1) {               // une dose supplémentaire de lait
                    lait_grand_latte += une_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Une dose")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_moyen_latte
                      lait_initial -= lait_moyen_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_moyen+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 2) {            // deux doses supplémentaires de lait
                    lait_grand_latte += deux_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Deux doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                  else if (nombre_dose_lait == 3) {            // trois doses supplémentaires de lait
                    lait_grand_latte += trois_dose_lait
                    println("Boisson sélectionnée : Latte (Grand)")
                    println("Niveau de sucre : Beaucoup (15g)")
                    println("Lait supplémentaire : Trois doses")
                    if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      cafe_initial -= cafe_grand_latte
                      lait_initial -= lait_grand_latte
                      sucre_initial -= sucre_beaucoup
                      prix_dose_lait *= nombre_dose_lait
                      println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                      println("\n\nVeuillez payer en utilisant Twint.")
                      println(s"Votre code de paiement est : $code_twint")
                      println("(En attente de paiement...)")
                      Thread.sleep(3000)
                      println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                      Thread.sleep(3000)
                      println("Votre Latte est prêt ! Bonne dégustation !")
                      boucle_quantite_suffisant = false
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                      println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                      println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                    else {
                      println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                      Thread.sleep(5000)
                    }
                  }
                }
                else {                              // Sans lait en supplément
                  nombre_dose_lait = 0
                  println("Boisson sélectionnée : Latte (Grand)")
                  println("Niveau de sucre : Beaucoup (15g)")
                  println("Lait supplémentaire : Non")
                  if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                    lait_initial -= lait_grand_latte
                    cafe_initial -= cafe_grand_latte
                    sucre_initial -= sucre_beaucoup
                    prix_dose_lait *= nombre_dose_lait
                    println(f"Prix total : CHF ${prix_grand_latte}%.2f  + CHF  ${prix_sucre_beaucoup}%.2f + CHF ${prix_dose_lait}%.2f = CHF ${prix_grand_latte+prix_sucre_beaucoup+prix_dose_lait}%.2f")
                    println("\n\nVeuillez payer en utilisant Twint.")
                    println(s"Votre code de paiement est : $code_twint")
                    println("(En attente de paiement...)")
                    Thread.sleep(3000)
                    println("\n\nPaiement confirmé.\nPréparation de votre boisson...")
                    Thread.sleep(3000)
                    println("Votre Latte est prêt ! Bonne dégustation !")
                    boucle_quantite_suffisant = false
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial >= sucre_beaucoup) {
                    println("\n\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez essayer une autre boisson.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial >= cafe_grand_latte && lait_initial < lait_grand_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une boisson avec moins de sucre ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de sucre insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else if (cafe_initial < cafe_grand_latte && lait_initial >= lait_grand_latte && sucre_initial < sucre_beaucoup) {
                    println("\n\nErreur : Quantité de poudre de café et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                  else {
                    println("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisantes pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    Thread.sleep(5000)
                  }
                }
              }
            }
          }
        }
      }
      else if (choix_mode == 2) {
        println("Mode Admin")
        code_pin = readLine("Entrez le code PIN : ").toInt
        while (code_pin != 434343){
          println("Code PIN incorrect")
          code_pin = readLine("Entrez le code PIN : ").toInt
        }
        println("Accès autorisé.\n\nStocks:\n")
        println("     Poudre de café: " + cafe_initial + "g")
        println("     Lait          : " + lait_initial + "L")
        println("     Sucre         : " + sucre_initial + "g")
        println("Réapprovisionnement des stocks...")
        Thread.sleep(3000)
        println("Ajout :")
        cafe_initial = readLine("     Poudre de café: ").toInt
        lait_initial = readLine("     Lait          : ").toDouble
        sucre_initial = readLine("     sucre         : ").toInt
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
        Thread.sleep(3000)
      }
      else {
        lancer = false
      }

    }
  }
}