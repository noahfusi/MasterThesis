import io.StdIn._
import util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var SuiteProgramme = true
    val CodePin = Array("434343", "434343", "434343", "434343", "434343")
    val StockInitialCafe = Array(50.0, 50.0, 50.0, 50.0, 50.0)
    val StockInitialSucre = Array(30.0, 30.0, 30.0, 30.0, 30.0)
    val StockInitialLait = Array(0.5, 0.5, 0.5, 0.5, 0.5) // litre

    while (SuiteProgramme) {
      var BesoinCafe = 0
      var BesoinLait = 0.0
      var BesoinExtraLait = 0.0
      val QuantiteExtraLait = 0.05
      var NomBoisson = ""
      var PrixBoisson = 0.00
      var PrixExtraLait = 0.00
      val UneDose = 0.05
      val NombreMachine = 5

      // Choix du Mode
      def SelectionMode(): Int = {
        var Mode = 0
        do {
          println("Nospresso Café :")
          println("Veuillez sélectionner votre mode :")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter")
          Mode = readLine("> ").toInt
          if (Mode < 1 || Mode > 3) {
            println("Choix invalide, veuillez entrer une valeur autorisée (entre 1 et 3).")
          }
        } while (Mode < 1 || Mode > 3)
        Mode
      }
      //Séléction de la machine
      def SelectionMachine(): Int = {
        println("Entrez l'identifiant de la machine (1 à 5) :")
        val ChoixMachine = readLine("> ").toInt

        if (ChoixMachine >= 1 && ChoixMachine <= NombreMachine) {
          ChoixMachine - 1
        } else {
          println("Entrée non valide, veuillez taper une valeur autorisée (entre 1 et 5).")
          SelectionMachine()
        }
      }

      // 1.Mode Client
      def ServeClient(MachineId: Int, StockInitialCafe: Array[Double], StockInitialSucre: Array[Double], StockInitialLait: Array[Double]): Unit = {
        PrixBoisson = 0.00
        PrixExtraLait = 0.00
        var ChoixBoisson = ""
        do {
          println("Veuillez séléctionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          ChoixBoisson = readLine()

          if (ChoixBoisson == "1") {
            NomBoisson = "Expresso"
            PrixBoisson = 2.00
            BesoinCafe = 8
          } else if (ChoixBoisson == "2") {
            NomBoisson = "Cappuccino"
            PrixBoisson = 2.50
            BesoinCafe = 6
            BesoinLait = 0.10
          } else if (ChoixBoisson == "3") {
            var TailleLatte = ""
            do {
              println("Choisissez la taille du Latte :")
              println("1) Petit Latte - CHF 2.70")
              println("2) Moyen Latte - CHF 3.20")
              println("3) Grand Latte - CHF 3.70")
              print("> ")
              TailleLatte = readLine()
              if (TailleLatte == "1") {
                NomBoisson = "Latte (Petit)"
                PrixBoisson = 2.70
                BesoinCafe = 6
                BesoinLait = 0.12
              } else if (TailleLatte == "2") {
                NomBoisson = "Latte (Moyen)"
                PrixBoisson = 3.20
                BesoinCafe = 8
                BesoinLait = 0.15
              } else if (TailleLatte == "3") {
                NomBoisson = "Latte (Grand)"
                PrixBoisson = 3.70
                BesoinCafe = 12
                BesoinLait = 0.20
              } else {
                println("Taille invalide, veuillez réessayer.")
              }
            } while (TailleLatte != "1" && TailleLatte != "2" && TailleLatte != "3")
          } else {
            println("Choix invalide, veuillez réessayer.")
          }
        } while (ChoixBoisson != "1" && ChoixBoisson != "2" && ChoixBoisson != "3")

        var ChoixSucre = ""
        var PrixSucre = 0.0
        var BesoinSucre = 0.0
        var NomSucre = ""
        do {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          ChoixSucre = readLine()
          if (ChoixSucre == "1") {
            NomSucre = "Sans Sucre"
            PrixSucre = 0.00
            BesoinSucre = 0
          } else if (ChoixSucre == "2") {
            NomSucre = "Peu (5g)"
            PrixSucre = 0.10
            BesoinSucre = 5
          } else if (ChoixSucre == "3") {
            NomSucre = "Moyen (10g)"
            PrixSucre = 0.20
            BesoinSucre = 10
          } else if (ChoixSucre == "4") {
            NomSucre = "Beaucoup (15g)"
            PrixSucre = 0.30
            BesoinSucre = 15
          } else {
            println("Choix invalide, veuillez enter une valeur autorisée (entre 1 et 4).")
          }
        } while (ChoixSucre != "1" && ChoixSucre != "2" && ChoixSucre != "3" && ChoixSucre != "4")

        var LaitSupp = ""
        if (ChoixBoisson == "2" || ChoixBoisson == "3") {
          var ChoixDose = ""
          var NombreDoses = 0
          do {
            println("Souhaitez-vous ajouter du lait en supplément ? ")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            print("> ")
            ChoixDose = readLine()
            if (ChoixDose != "1" && ChoixDose != "2") {
              println("Choix invalide, veuillez enter une valeur autorisée (entre 1 et 2).")
            }
          } while (ChoixDose != "1" && ChoixDose != "2" )

          if (ChoixDose == "1") {
            var NombreDoses = ""
            do {
              println("Combien de doses de lait souhaitez-vous ajouter ? (1 à 3 doses max)")
              print("> ")
              NombreDoses = readLine()
              if (NombreDoses == "1" || NombreDoses == "2" || NombreDoses == "3") {
                BesoinExtraLait = QuantiteExtraLait * NombreDoses.toInt
                PrixExtraLait = UneDose * NombreDoses.toInt
                LaitSupp = " Vous avez séléctionné " + NombreDoses + " dose(s)"
              } else {
                println("Choix invalide, veuillez enter un nombre de doses valide (entre 1 et 3).")
              }
            } while (NombreDoses != "1" && NombreDoses != "2" && NombreDoses != "3")
          } else if (ChoixDose == "2") {
            LaitSupp = "Non"
            PrixExtraLait = 0.00
            NombreDoses = 0
            println("Pas de supplément de lait ajouté.")
          }
        }
        println("Boisson sélectionnée : " + NomBoisson)
        println("Niveau de sucre : " + NomSucre)
        if (ChoixBoisson == "2" || ChoixBoisson == "3") {
          println("Lait en supplément : " + LaitSupp)
        }
        if (StockInitialCafe(MachineId) < BesoinCafe) {
          println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou changer de machine.")
          val NewMachineId = SelectionMachine()
          ServeClient(NewMachineId, StockInitialCafe, StockInitialSucre, StockInitialLait)
        }
        if (StockInitialLait(MachineId) < (BesoinLait + BesoinExtraLait)) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite, essayer une autre boisson ou alors changer de machine.")
          val NewMachineId = SelectionMachine()
          ServeClient(NewMachineId, StockInitialCafe, StockInitialSucre, StockInitialLait)
        }
        if (StockInitialSucre(MachineId) < BesoinSucre) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez séléctionner moins de sucre ou changer de machine.")
          val NewMachineId = SelectionMachine()
          ServeClient(NewMachineId, StockInitialCafe, StockInitialSucre, StockInitialLait)
        } else {
          if (StockInitialCafe(MachineId) >= BesoinCafe && StockInitialLait(MachineId) >= (BesoinLait + BesoinExtraLait) && StockInitialSucre(MachineId) >= BesoinSucre) {
            StockInitialCafe(MachineId) -= BesoinCafe
            StockInitialLait(MachineId) -= (BesoinLait + BesoinExtraLait)
            StockInitialSucre(MachineId) -= BesoinSucre

            val PrixTotal = PrixBoisson + PrixSucre + PrixExtraLait
            printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", PrixBoisson, PrixSucre, PrixExtraLait, PrixTotal)
            println("Veuillez payer en utilisant Twint.")

            val Caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
            val LongueurCode = 5

            val PaymentCode = (1 to LongueurCode).map(_ => Caracteres(Random.nextInt(Caracteres.length))).mkString
            println("Veuillez effectuer votre paiement...")
            println("Votre code de paiement est :" + PaymentCode)
            println("En attente de validation du paiement...")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")
            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre" + NomBoisson + " est prêt! Bonne dégustation !")
          }
        }
      }
      def VerificationCode(MachineId: Int, CodePin: String): Boolean = {
        println("Mode Admin")
        var Tentative = 0
        val MaxTentative = 3
        var PinCorrect = false
        var PinAdmin = ""

        while (Tentative < MaxTentative && !PinCorrect) {
          println("Veuillez entrez le code PIN : ")
          print("> ")
          PinAdmin = readLine()

          if (PinAdmin == CodePin) {
            println("Accès autorisé.")
            PinCorrect = true
          } else {
            Tentative += 1
            println("Code PIN incorrect. Tentative " + Tentative + "/" + MaxTentative + ".")
          }
        }
        if (!PinCorrect) {
          println("Accès refusé. Trop de tentatives échouées.")
          SuiteProgramme = false
        }
        PinCorrect
      }
      // 2. Mode Admin
      def menuAdmin(MachineId: Int): String = {
        var ChoixAdmin = ""
        do {
          println("1. Mettre à jour le code PIN")
          println("2. Réapprovisionner les stocks")
          print("> ")
          ChoixAdmin = readLine()
          if (ChoixAdmin != "1" && ChoixAdmin != "2") {
            println("Choix invalide, veuillez enter une valeur autorisée (entre 1 et 2).")
          }
        } while (ChoixAdmin != "1" && ChoixAdmin != "2")
        ChoixAdmin
      }
      // Mise à jour du code Pin
      def NouveauCode(MachineId: Int): Unit = {
        var NouveauCode = ""
        var PinValid = false
        while (!PinValid) {
          println("Entrez un nouveau code PIN à 6 chiffres.")
          print("> ")
          NouveauCode = readLine()
          if (NouveauCode.length == 6) {
            PinValid = true
            println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal ...")
          } else {
            println("Code Pin invalide. Veuillez entrer un code PIN composé de 6 chiffres.")
          }
        }
        CodePin(MachineId) = NouveauCode
      }
      // Réapprovisionnement des stocks
      def RemplirMachine(MachineId: Int): Unit = {
        println("Niveau actuel du stock :\n" + "Poudre de café : " + StockInitialCafe(MachineId) + " g\n" + "Sucre : " + StockInitialSucre(MachineId) + " g\n" + "Lait : " + StockInitialLait(MachineId) + " L")
        println("Réapprovisionnement des stocks...")
        println("Entrez la quantité de café à ajouter (en grammes) :")
        print("> ")
        var CafeAjoute = readInt()
        StockInitialCafe(MachineId) += CafeAjoute

        println("Entrez la quantité de sucre à ajouter (en grammes) :")
        print("> ")
        var SucreAjoute = readInt()
        StockInitialSucre(MachineId) += SucreAjoute

        print("Entrez la quantité de lait à ajouter (en litres) :")
        print("> ")
        var LaitAjoute = readDouble()
        StockInitialLait(MachineId) += LaitAjoute

        if (CafeAjoute >= 0 && LaitAjoute >= 0 && SucreAjoute >= 0) {
          println("Réapprovisionnement des stocks ...")
          println("Ajout : \n" + "Poudre de café : " + CafeAjoute + " g\n" + "Sucre : " + SucreAjoute + " g\n" + "Lait : " + LaitAjoute + " L")
          println("Niveaux des stocks mis à jour.")
          println("Retour au menu principal...")
          Thread.sleep(2000)
        }
      }
      // 3. Quitter
      def Quitter(): Unit = {
        println("Vous avez choisi de quitter. A bientôt.")
        SuiteProgramme = false
      }
      ////////////////////////////////////////////////////////////////////////////////////
      val Mode = SelectionMode()
      // 1. Mode Client
      if (Mode == 1) {
        val MachineId = SelectionMachine()
        ServeClient(MachineId, StockInitialCafe, StockInitialSucre, StockInitialLait)
      }
      // 2. Mode Admin
      else if (Mode == 2) {
        val MachineId = SelectionMachine()
        if (VerificationCode(MachineId, CodePin(MachineId))) {
          val ChoixAdmin = menuAdmin(MachineId)

          // Mise à jour du code Pin
          if (ChoixAdmin == "1") {
            NouveauCode(MachineId)
          }
          // Réapprovisionnement des stocks
          else if (ChoixAdmin == "2") {
            RemplirMachine(MachineId)
          }
          else {
            println("Option séléctionée invalide.")
          }
        } else {
          println("Code pin erroné.")
        }
      }
      // 3. Quitter
      else if (Mode == 3) {
        Quitter()
        SuiteProgramme = false
      // 4. Entrée invalide
      } else {
        println("Mode séléctionné invalide. Veuillez entrer une valeur entre 1 et 3")
      }
    }
  }
}