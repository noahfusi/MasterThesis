import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var PoudreCafe = 50.0
    var Sucre = 30.0
    var Lait = 0.500
    var modeUser = 0
    var CafeBesoin = 0.00
    var SucreBesoin = 0.00
    var LaitBesoin = 0.00
    while (modeUser != 3) {
      println("     Nospresso Café")
      println("Veuillez séléctionner votre mode en tapant le chiffre correspondant")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      modeUser = readInt()
      while ((modeUser != 1) && (modeUser != 2) && (modeUser != 3)) {
        println("     Nospresso Café")
        println("Veuillez séléctionner votre mode en tapant le chiffre correspondant")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        println(">")
        modeUser = readInt()
      }
      var Prix = 0.00
      if (modeUser == 1) {
        println("Quelle boisson souhaitez-vous?")
        println("tapez le numéro correspondant à la boisson")
        println("1) Expresso - 2.00 CHF")
        println("2) Cappucino - 2.50 CHF")
        println("3) Latte - 2.70 CHF (petit), 3.20 CHF (moyen), 3.70 CHF (grand)")
        println(">")
        var typeBoisson = readInt()
        while ((typeBoisson != 1) && (typeBoisson != 2) && (typeBoisson != 3)) {
          println("Quelle boisson souhaitez-vous?")
          println("tapez le numéro correspondant à la boisson")
          println("1) Expresso - 2.00 CHF")
          println("2) Cappucino - 2.50 CHF")
          println("3) Latte - 2.70 CHF (petit), 3.20 CHF (moyen), 3.70 CHF (grand)")
          println(">")
          typeBoisson = readInt()
        }
        CafeBesoin = 0.00
        SucreBesoin = 0.00
        LaitBesoin = 0.00
        if (typeBoisson == 1) {
          CafeBesoin = CafeBesoin + 8
          Prix = Prix + 2.00
        }
        if (typeBoisson == 2) {
          CafeBesoin = CafeBesoin + 6
          LaitBesoin = LaitBesoin + 0.100
          Prix = Prix + 2.50
        }
        if (typeBoisson == 3) {
          println("Quelle taille voulez-vous?")
          println("tapez le numéro correspondant à la taille")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          println(">")
          var tailleLatte = readInt()
          while ((tailleLatte != 1) && (tailleLatte != 2) && (tailleLatte != 3)) {
            println("Quelle taille voulez-vous?")
            println("tapez le numéro correspondant à la taille")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            println(">")
            tailleLatte = readInt()
          }
          if (tailleLatte == 1) {
            CafeBesoin = CafeBesoin + 6
            LaitBesoin = LaitBesoin + 0.120
            Prix = Prix + 2.70
          }
          if (tailleLatte == 2) {
            CafeBesoin = CafeBesoin + 8
            LaitBesoin = LaitBesoin + 0.150
            Prix = Prix + 3.20
          }
          if (tailleLatte == 3) {
            CafeBesoin = CafeBesoin + 12
            LaitBesoin = LaitBesoin + 0.200
            Prix = Prix + 3.70
          }
        }
        println("Quelle dose de sucre voulez vous?")
        println("Tapez le numéro correspondant à la dose de sucre")
        println("0) pas de sucre")
        println("1) Peu de sucre (5g) - 0.10 CHF")
        println("2) Moyenne dose (10g) - 0.20 CHF ")
        println("3) Grande dose (15g) - 0.30 CHF")
        println(">")
        var doseSucre = readInt()
        while ((doseSucre != 0) && (doseSucre != 1) && (doseSucre != 2) && (doseSucre != 3)) {
          println("Quelle dose de sucre voulez vous?")
          println("Tapez le numéro correspondant à la dose de sucre")
          println("0) pas de sucre")
          println("1) Peu de sucre (5g) - 0.10 CHF")
          println("2) Moyenne dose (10g) - 0.20 CHF")
          println("3) Grande dose (15g) - 0.30 CHF")
          println(">")
          doseSucre = readInt()
        }
        if (doseSucre == 1) {
          SucreBesoin = SucreBesoin + 5
          Prix = Prix + 0.10
        }
        if (doseSucre == 2) {
          SucreBesoin = SucreBesoin + 10
          Prix = Prix + 0.20
        }
        if (doseSucre == 3) {
          SucreBesoin = SucreBesoin + 15
          Prix = Prix + 0.30
        }
        var doseLait = 0
        if ((typeBoisson == 2) || (typeBoisson == 3)) {
          println("Voulez vous du lait en supplément?")
          println("Tapez le numéro correspondant à la dose de lait en spplément")
          println("0) Pas de supplément lait")
          println("1) Une dose de lait en supplément - 0.05 CHF")
          println("2) Deux doses de lait en supplément - 0.10 CHF")
          println("3) Trois doses de lait en supplément - 0.15 CHF")
          doseLait = readInt()
          while ((doseLait != 0) && (doseLait != 1) && (doseLait != 2) && (doseLait != 3)) {
            println("Voulez vous du lait en supplément?")
            println("Tapez le numéro correspondant à la dose de lait en spplément")
            println("0) Pas de supplément lait")
            println("1) Une dose de lait en supplément - 0.05 CHF")
            println("2) Deux doses de lait en supplément - 0.10 CHF")
            println("3) Trois doses de lait en supplément - 0.15 CHF")
            doseLait = readInt()
          }
          if (doseLait == 1) {
            LaitBesoin = LaitBesoin + 0.050
            Prix = Prix + 0.05
          }
          if (doseLait == 2) {
            LaitBesoin = LaitBesoin + 0.100
            Prix = Prix + 0.10
          }
          if (doseLait == 3) {
            LaitBesoin = LaitBesoin + 0.150
            Prix = Prix + 0.15
          }
        }

        if (Sucre < SucreBesoin) {
          println(" Nous sommes désolé mais le stock de sucre est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
          Thread.sleep(5000)
        } else if (Lait < LaitBesoin) {
          println(" Nous sommes désolé mais le stock de lait est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
          Thread.sleep(5000)
        } else if (PoudreCafe < CafeBesoin) {
          println(" Nous sommes désolé mais le stock de café est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
          Thread.sleep(5000)
        } else {
          val CodePaiement = Random.alphanumeric.take(5).mkString
          Sucre = Sucre - SucreBesoin
          PoudreCafe = PoudreCafe - CafeBesoin
          Lait = Lait - LaitBesoin
          if ((typeBoisson == 1) && (doseSucre == 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Expresso")
            println("Sans sucre ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 1) && (doseSucre != 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Expresso")
            println("Avec sucre")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 2) && (doseSucre == 0) && (doseLait == 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Cappuccino")
            println("Sans sucre ")
            println("Sans Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 2) && (doseSucre != 0) && (doseLait != 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Cappuccino")
            println("Avec sucre ")
            println("Avec Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 2) && (doseSucre == 0) && (doseLait != 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Cappuccino")
            println("Sans sucre ")
            println("Avec Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 2) && (doseSucre != 0) && (doseLait == 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Cappuccino")
            println("Avec sucre ")
            println("Sans Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 3) && (doseSucre == 0) && (doseLait == 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Latte")
            println("Sans sucre ")
            println("Sans Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 3) && (doseSucre != 0) && (doseLait != 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Latte")
            println("Avec sucre ")
            println("Avec Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 3) && (doseSucre == 0) && (doseLait != 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Latte")
            println("Sans sucre ")
            println("Avec Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          } else if ((typeBoisson == 3) && (doseSucre != 0) && (doseLait == 0)) {
            println("récapitulation commande: ")
            println("Boisson:  Latte")
            println("Avec sucre ")
            println("Sans Lait ")
            println("")
            println("Veuillez payez par Twint.")
            printf("Le prix total est de  CHF %.2f\n", Prix)
            println("Votre code de paiement est : " + CodePaiement)
            println("(En attente de paiement...)")
            println("")
            Thread.sleep(3000)
          }
          if (typeBoisson == 1) {
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre Expresso est prêt ! Bonne dégustation !")
            println("")
            Thread.sleep(5000)
          } else if (typeBoisson == 2) {
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
            println("")
            Thread.sleep(5000)
          } else if (typeBoisson == 3) {
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre Latte est prêt ! Bonne dégustation !")
            println("")
            Thread.sleep(5000)
          }
        }
      }
      if (modeUser == 2) {
        println("Veuillez entrer le Code Pin pour accéder aux stocks")
        val CodePin = readInt
        if (CodePin == 434343) {
          println("Bienvenu dans le mode Admin")
          println("Voici les stocks : ")
          println("")
          println("Poudre de café : " + PoudreCafe + "g")
          println("Sucre : " + Sucre + "g")
          println("Lait : " + Lait + "L")
          println("")
          println("Combien de gramme de poudre de café souhaitez-vous remettre ?")
          PoudreCafe = PoudreCafe + readDouble()
          println("Combien de gramme de sucre souhaitez-vous remettre?")
          Sucre = Sucre + readDouble()
          println("Combien de litre de lait souhaitez-vous remettre?")
          Lait = Lait + readDouble()
          println("Les stocks sont mis à jours!")
          println("Voici les stocks actuels...:")
          println("")
          println("Poudre de café : " + PoudreCafe + "g")
          println("Sucre : " + Sucre + "g")
          println("Lait : " + Lait + "L")
          println("Retour au Menu principal...")
          println("")
        } else {
          println("Désolé, le code est faux")
          println("Retour au Menu principal...")
          println("")
        }
        Thread.sleep(5000)
      }
    }
  }
}

