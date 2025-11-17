import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var StockCafe = 50 //g
    var StockSucre = 30 //g
    var StockLait = 0.500 //L

    var NomBoisson = ""
    var NiveauSucre = ""
    var NiveauLait = ""

    //STOCK POUR DIFFERENT CAFé
    val CafeEspresso = 8 //g
    val CafeCappuccino = 6 //g
    val LaitCappuccino = 0.100 //L
    val CaffeLattePetit = 6
    val LaitLattePetit = 0.120
    val CaffeLatteMoyen = 8
    val LaitLatteMoyen = 0.150
    val CaffeLatteGrand = 12
    val LaitLatteGrand = 0.200

    //stock sucre
    val SucrePeu = 5
    val SucreMoyen = 10
    val SucreBeaucoup = 15

    //stock lait
    val LaitSup1 = 0.050
    val LaitSup2 = 0.100
    val LaitSup3 = 0.150

    //PRIX
    // Lait
    val PrixLait1 = 0.05
    val PrixLait2 = 0.10
    val PrixLait3 = 0.15

    //sucre
    val PrixSucrePeu = 0.10
    val PrixSucreMoyen = 0.20
    val PrixSucreBeaucoup = 0.30

    //type de café
    //Expresso
    val PrixEspresso = 2.00

    //Cappuccino
    val PrixCappuccino = 2.50

    //Latte
    val PrixLattePetit = 2.70
    val PrixLatteMoyen = 3.20
    val PrixLatteGrand = 3.70

    var PrixClient = 0.00
    var PrixCalcul = ""

    var continuerProgram = true

    while (continuerProgram) {

      println("Nospresso Café\nVeuillez s'electionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
      var mode = readInt()
      if (mode == 1) {
        var stockSuffisant = false

        if (!stockSuffisant) {
          println("Veuillez s´electionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
          var selectionboisson = readInt()

          if (selectionboisson == 1) {
            if (StockCafe >= CafeEspresso) {
              StockCafe -= CafeEspresso
              PrixClient += PrixEspresso
              NomBoisson = "Espresso"
              PrixCalcul += "CHF 2.00 "
              stockSuffisant = true
            }
          } else if (selectionboisson == 2) {
            if (StockCafe >= CafeCappuccino && StockLait >= LaitCappuccino) {
              StockCafe -= CafeCappuccino
              StockLait -= LaitCappuccino
              PrixClient += PrixCappuccino
              NomBoisson = "Cappuccino"
              PrixCalcul += "CHF 2.50 "
              stockSuffisant = true
            }
          }
          if (selectionboisson == 3) {
            println("Veuillez choisir la taille du latte :\n1) Petit - CHF 2.70\n2)Moyen - CHF 3.20\n3) Grand - CHF 3.70")
            var taillelatte = readInt()
            if (taillelatte == 1) {
              if (StockCafe >= CaffeLattePetit && StockLait >= LaitLattePetit) {
                StockCafe -= CaffeLattePetit
                StockLait -= LaitLattePetit
                PrixClient += PrixLattePetit
                NomBoisson = "Latte Petit"
                PrixCalcul += "CHF 2.70 "
                stockSuffisant = true
              }
            } else if (taillelatte == 2) {
              if (StockCafe >= CaffeLatteMoyen && StockLait >= LaitLatteMoyen) {
                StockCafe -= CaffeLatteMoyen
                StockLait -= LaitLatteMoyen
                PrixClient += PrixLatteMoyen
                NomBoisson = "Latte Moyen"
                PrixCalcul += "CHF 3.20 "
                stockSuffisant = true
              }
            } else if (taillelatte == 3) {
              if (StockCafe >= CaffeLatteGrand && StockLait >= LaitLatteGrand) {
                StockCafe -= CaffeLatteGrand
                StockLait -= LaitLatteGrand
                PrixClient += PrixLatteGrand
                NomBoisson = "Latte Grand"
                PrixCalcul += "CHF 3.70"
                stockSuffisant = true
              }
            }
          }
          println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          var Qsucre = readInt()
          if (Qsucre == 2) {
            if (StockSucre >= SucrePeu) {
              StockSucre -= SucrePeu
              PrixClient += PrixSucrePeu
              NiveauSucre = "Peu (5g)"
              PrixCalcul += " + CHF 0.10 "
              stockSuffisant = true
            }
          } else if (Qsucre == 3) {
            if (StockSucre >= SucreMoyen) {
              StockSucre -= SucreMoyen
              PrixClient += PrixSucreMoyen
              NiveauSucre = "Moyen (10g)"
              PrixCalcul += " + CHF 0.20 "
              stockSuffisant = true
            }
          } else if (Qsucre == 4) {
            if (StockSucre >= SucreBeaucoup) {
              StockSucre -= SucreBeaucoup
              PrixClient += PrixSucreBeaucoup
              NiveauSucre = "Beaucoup (15g)"
              PrixCalcul += " + CHF 0.30 "
              stockSuffisant = true
            }
          } else if (Qsucre == 1) {
            NiveauSucre = "Non"
          }

          println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>")
          var choixLait = readInt()

          if (choixLait == 1) {
            println("Combien de dose ?\n>")
            var QLait = readInt()
            while (QLait > 3) {
              if (QLait == 1 && StockLait >= LaitSup1) {
                StockLait -= LaitSup1
                PrixClient += PrixLait1
                NiveauLait = "Une dose (50ml)"
                PrixCalcul += " + CHF 0.05 "
                stockSuffisant = true
              } else if (QLait == 2 && StockLait >= LaitSup2) {
                StockLait -= LaitSup2
                PrixClient += PrixLait2
                NiveauLait = "Deux doses (100ml)"
                PrixCalcul += " + CHF 0.10 "
                stockSuffisant = true
              } else if (QLait == 3 && StockLait >= LaitSup3) {
                StockLait -= LaitSup3
                PrixClient += PrixLait3
                NiveauLait = "Trois doses(150ml)"
                PrixCalcul += " + CHF 0.15 "
                stockSuffisant = true
              } else if (QLait > 3) {
                println("Choisisez une quantité de lait plus bas (3 doses ou moin)\n>")
                var QLait = readInt()
              } else if (QLait == 2) {
                NiveauLait = "Non"
              }
            }
            //Scénario 1: commande de Boisson Réussie
            if (stockSuffisant == true) {
              println("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire: " + NiveauLait + "\n Prix total: " + PrixCalcul + " = ")
              printf("%.2f", PrixClient)
            } else if (stockSuffisant == false) {
              print("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire:" + NiveauLait + "\n Erreur : Stock insuffisante pour préparer le boisson sélectionnée. \nVeuillez choisir une tailleplus petite ou essayer une autre boisson.")
            }
          }


          println(" Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + Random.alphanumeric.take(5).mkString + "\n (En attente de validation du paiement...)")

          Thread.sleep(5000)
          println("Merci ! Votre paiement a été accepté. \n ")
          println("Préparation de votre boisson... \n[...]")
          println("votre " + NomBoisson + " est prêt ! Bonne dégustation !")
        }
      }
      else if (mode == 2) {
        println("Veillez entrer le code PIN (6 chiffres) : ")
        val motDePasse = readLine()
        if (motDePasse == "434343") {
          println("Accès autorisé.")
        }
        else if (motDePasse != 434343) {
          println("Le code que vous avez rentrée ne correspont PAS au code admin. Essayer de nouveau >")
        }
        println("Stocks: \n Poudre de café: " + StockCafe + "g \n Lait: " + StockLait + "L \n Sucre: " + StockSucre + "g")
        println("Réapprovisonnement des stocks...")
        println("Ajout : ")
        println(" Poudre de café: ")
        var AjoutCafe = readDouble()
        println("Lait: ")
        var AjoutLait = readDouble()
        println("Sucre: ")
        var AjoutSucre = readDouble()
        println("Ajout: \n  Poudre de café: " + AjoutCafe + "\n  Lait: " + AjoutLait + "\n  Sucre: " + AjoutSucre + "\n Niveaux de stock mis à jour")

      }
      else if (mode == 3) {
        continuerProgram = false
      }
    }
  }
}


