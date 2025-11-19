import io.StdIn._
import scala.util.Random

  object Main {
    val PoudreCafe = Array(50, 50, 50, 50, 50)
    val Sucre = Array(30, 30, 30, 30, 30)
    val Lait = Array(500, 500, 500, 500, 500)
    var modeUser = 0
    var CafeBesoin = 0
    var SucreBesoin = 0
    var LaitBesoin = 0
    var Prix = 0.00
    var MachineId = 0
    val CodePin = Array("434343", "434343", "434343", "434343", "434343")
    var essaiCodePin = "0"
    var erreur = 0
    var reponse = 0
    var reponseBis = 0
    var NouveauCode = "0"
    var AjoutCafe = 0
    var AjoutSucre = 0
    var AjoutLait = 0

    def ValidatePin(MachineId: Int, CodePin: Array[String]): Boolean = {
      println("Veuillez entrer le Code PIN de la machine séléctionnée pour accéder aux stocks")
      essaiCodePin = readLine()
      if (essaiCodePin == CodePin(MachineId)) {
        return true
      } else {
        println(" Il reste 2 tentatives pour entrer le bon code")
        println(" Veuillez entrer à nouveau le code PIN de la machine")
        essaiCodePin = readLine()
        if (essaiCodePin == CodePin(MachineId)) {
          return true
        } else {
          println(" Il reste 1 tentatives pour entrer le bon code")
          println(" Veuillez entrer à nouveau le code PIN de la machine")
          essaiCodePin = readLine()
          if (essaiCodePin == CodePin(MachineId)) {
            return true
          } else {
            println(" Trop de tentatives echouées. Fin du programme")
            return false
          }
        }
      }
    }

    def restockMachine(MachineId: Int, PoudreCafe: Array[Int], Sucre: Array[Int], Lait: Array[Int] ): Unit = {
      println("Bienvenu dans le mode Admin")
      println("Voici les stocks de toutes les machines: ")
      println("")
      println("Poudre de café : " + PoudreCafe.mkString("g,") + "g")
      println("Sucre : " + Sucre.mkString("g,") + "g")
      println("Lait : " + Lait.mkString("mL,") + "mL")
      println("")
      println("Combien de gramme de poudre de café souhaitez-vous remettre dans la machine séléctionnée?")
      AjoutCafe = readInt()
      while (AjoutCafe < 0) {
        println("Vous devez mettre une valeur supérieur ou égale à 0")
        println("Combien de gramme de poudre de café souhaitez-vous remettre dans la machine séléctionnée?")
        AjoutCafe = readInt()
      }
      PoudreCafe(MachineId) = PoudreCafe(MachineId) + AjoutCafe
      println("Combien de gramme de sucre souhaitez-vous remettre?")
      AjoutSucre = readInt()
      while (AjoutSucre < 0){
        println("Vous devez mettre une valeur supérieur ou égale à 0")
        println("Combien de gramme de sucre souhaitez-vous remettre?")
        AjoutSucre = readInt()
      }
      Sucre(MachineId) = Sucre(MachineId) + AjoutSucre
      println("Combien de mL de lait souhaitez-vous remettre?")
      AjoutLait = readInt()
      while (AjoutLait < 0){
        println("Vous devez mettre une valeur supérieur ou égale à 0")
        println("Combien de mL de lait souhaitez-vous remettre?")
        AjoutLait = readInt()
      }
      Lait(MachineId) = Lait(MachineId) + AjoutLait
      println("Les stocks sont mis à jours!")
      println("Voici les stocks actuels...:")
      println("")
      println("Poudre de café : " + PoudreCafe.mkString("g,") + "g")
      println("Sucre : " + Sucre.mkString("g,") + "g")
      println("Lait : " + Lait.mkString("mL,") + "mL")
      println("Retour au Menu principal...")
      println("")
    }

    def updatePin(MachineId: Int, CodePin: Array[String]): Unit = {

      println("Quel code voulez-vous mettre? ")
      println("Il doit comporter 6 chiffres")
      NouveauCode = readLine()
      while (NouveauCode.length != 6){
        println("Quel code voulez-vous mettre? ")
        println("Il doit comporter 6 chiffres")
        NouveauCode = readLine()
      }
      CodePin(MachineId) = NouveauCode
    }

    def ServeCLient(MachineId: Int, PoudreCafe: Array[Int], Sucre: Array[Int], Lait: Array[Int]): Boolean = {
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
      CafeBesoin = 0
      SucreBesoin = 0
      LaitBesoin = 0
      Prix = 0.00
      if (typeBoisson == 1) {
        CafeBesoin = CafeBesoin + 8
        Prix = Prix + 2.00
      }
      if (typeBoisson == 2) {
        CafeBesoin = CafeBesoin + 6
        LaitBesoin = LaitBesoin + 100
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
          LaitBesoin = LaitBesoin + 120
          Prix = Prix + 2.70
        }
        if (tailleLatte == 2) {
          CafeBesoin = CafeBesoin + 8
          LaitBesoin = LaitBesoin + 150
          Prix = Prix + 3.20
        }
        if (tailleLatte == 3) {
          CafeBesoin = CafeBesoin + 12
          LaitBesoin = LaitBesoin + 200
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
          LaitBesoin = LaitBesoin + 50
          Prix = Prix + 0.05
        }
        if (doseLait == 2) {
          LaitBesoin = LaitBesoin + 100
          Prix = Prix + 0.10
        }
        if (doseLait == 3) {
          LaitBesoin = LaitBesoin + 150
          Prix = Prix + 0.15
        }
      }

      if (Sucre(MachineId) < SucreBesoin) {
        println(" Nous sommes désolé mais le stock de sucre est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre machine ou vérifier les stocks en mode admin")
        Thread.sleep(5000)
        return false
      } else if (Lait(MachineId) < LaitBesoin) {
        println(" Nous sommes désolé mais le stock de lait est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre machine ou vérifier les stocks en mode admin")
        Thread.sleep(5000)
        return false
      } else if (PoudreCafe(MachineId) < CafeBesoin) {
        println(" Nous sommes désolé mais le stock de café est insuffisant, nous ne pouvons préparer votre boisson, veuillez choisir une autre machine ou vérifier les stocks en mode admin")
        Thread.sleep(5000)
        return false
      } else {
        val CodePaiement = Random.alphanumeric.take(5).mkString
        Sucre(MachineId) = Sucre(MachineId) - SucreBesoin
        PoudreCafe(MachineId) = PoudreCafe(MachineId) - CafeBesoin
        Lait(MachineId) = Lait(MachineId) - LaitBesoin
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
        return true
      }
    }

    def main(args: Array[String]): Unit = {
      while ((modeUser != 3) && (erreur == 0)) {
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
        if ((modeUser == 1) || (modeUser == 2)) {
          println("Qu'elle machine utilisez-vous?")
          println("0) Machine 0")
          println("1) Machine 1")
          println("2) Machine 2")
          println("3) Machine 3")
          println("4) Machine 4")
          MachineId = readInt()
          while ((MachineId != 0) && (MachineId != 1) && (MachineId != 2) && (MachineId != 3) && (MachineId != 4)) {
            println("Qu'elle machine utilisez-vous?")
            println("0) Machine 0")
            println("1) Machine 1")
            println("2) Machine 2")
            println("3) Machine 3")
            println("4) Machine 4")
            MachineId = readInt()
          }
        }
        if (modeUser == 1) {
          ServeCLient(MachineId, PoudreCafe, Sucre, Lait)
        }
        if (modeUser == 2) {
          if (ValidatePin(MachineId, CodePin)) {
            println("Voulez-vous mettre à jour le code PIN ? ")
            println("1) OUI")
            println("2) NON")
            reponse = readInt()
            while ((reponse != 1) && (reponse != 2)) {
              println("Voulez-vous mettre à jour le code PIN ? ")
              println("1) OUI")
              println("2) NON")
              reponse = readInt()
            }
            if (reponse == 1) {
              updatePin(MachineId, CodePin)
            }
            println("Voulez-vous mettre à jour les stocks ? ")
            println("1) OUI")
            println("2) NON")
            reponseBis = readInt()
            while ((reponse != 1) && (reponse != 2)) {
              println("Voulez-vous mettre à jour les stocks ? ")
              println("1) OUI")
              println("2) NON")
              reponseBis = readInt()
            }
            if (reponseBis == 1){
              restockMachine(MachineId,PoudreCafe, Sucre, Lait)
              Thread.sleep(5000)
            } else {
              Thread.sleep(5000)
            }
          }else {
              erreur = erreur + 1
          }
        }
      }
    }
  }

