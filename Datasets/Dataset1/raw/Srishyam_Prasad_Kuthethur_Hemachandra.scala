import scala.io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    //Stockage
    var PoudreDeCafe = 50
    var Sucre = 30
    var Lait = 0.5

    //Erreur
    var ErreurPoudredeCafe=0
    var ErreurLait=0.0
    var ErreurSucre=0

    var selectionner=0
    while (selectionner != 3) {
      println("\nNospresso Cafe")
      println("Veuillez selectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      selectionner = readLine("> ").toInt

      //Stockage diminuer
      var PoudreDeCafeDiminue=PoudreDeCafe
      var SucreDiminue=Sucre
      var LaitDiminue=Lait

      if(selectionner==1){
        var select1= 0
        while(select1==0){
          println("\nVeuillez selectionner votre boisson:")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          var client = readLine("> ").toInt

          //Erreur de numero client:
          while (client != 1 && client != 2 && client != 3) {
            println("Erreur!! Numero entree n'est pas valide, \nVeuillez selectionner votre boisson:")
            client = readLine("> ").toInt
          }

          var prix = 0.0
          var Latte = 0
          //Gestion des stocks
          if (client == 1 && PoudreDeCafe >= 8) {
            PoudreDeCafe -= 8
            prix = 2.00
          }
          else if (client == 1 && PoudreDeCafe < 8) { //erreur
            ErreurPoudredeCafe = 1
            PoudreDeCafe=PoudreDeCafe
          }

          if (client == 2 && PoudreDeCafe >= 6 && Lait >= 0.1) {
            PoudreDeCafe -= 6
            Lait -= 0.1
            prix = 2.50
          }
          else if (client == 2) {        //erreur
            if (PoudreDeCafe < 6 ) {
              ErreurPoudredeCafe = 1
              PoudreDeCafe=PoudreDeCafe
            }
            if (Lait < 0.1) {
              ErreurLait = 1.1
              Lait=Lait
            }
          }

          else {
            if (client == 3) {
              println("Quel taille vous voulez votre Latte ?")
              println("1) Latte (Petit)")
              println("2) Latte (Moyen)")
              println("3) Latte (Grand)")
              Latte = readLine("> ").toInt

              //Erreur de Latte
              while (Latte != 1 && Latte != 2 && Latte != 3) {
                println("Erreur!! Numero entree n'est pas valide, \nQuel taille vous voulez votre Latte:")
                Latte = readLine("> ").toInt
              }

              if (Latte == 1 && PoudreDeCafe >= 6 && Lait >= 0.12) {
                PoudreDeCafe -= 6
                Lait -= 0.12
                prix = 2.70
              }
              else if (Latte == 1) { //erreur
                if (PoudreDeCafe < 6) {
                  ErreurPoudredeCafe = 1
                  PoudreDeCafe=PoudreDeCafe
                }
                else if (Lait < 0.12) {
                  ErreurLait = 1.1
                  Lait=Lait
                }
              }

              if (Latte == 2 && PoudreDeCafe >= 8 && Lait >= 0.15) {
                PoudreDeCafe -= 8
                Lait -= 0.15
                prix = 3.20
              }
              else if (Latte == 2) { //erreur
                if (PoudreDeCafe < 8) {
                  ErreurPoudredeCafe = 1
                  PoudreDeCafe=PoudreDeCafe
                }
                else if (Lait < 0.15) {
                  ErreurLait = 1.1
                  Lait=Lait
                }
              }

              if (Latte == 3 && PoudreDeCafe >= 12 && Lait >= 0.2) {
                PoudreDeCafe -= 12
                Lait -= 0.2
                prix = 3.70
              }
              else if (Latte == 3) { //erreur
                if (PoudreDeCafe < 12) {
                  ErreurPoudredeCafe = 1
                  PoudreDeCafe=PoudreDeCafe
                }
                else if (Lait < 0.2) {
                  ErreurLait = 1.1
                  Lait=Lait
                }
              }
            }
          }

          //sucre a ajouter
          var sucre2 = 0
          var prixsucre = 0.0
          if (client == 1 || client == 2 || client == 3) {
            println("\nSouhaitez-vous ajouter du sucre?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            sucre2 = readLine("> ").toInt
          }

          //Erreur de numero sucre:
          while (sucre2 != 1 && sucre2 != 2 && sucre2 != 3 && sucre2 != 4) {
            println("Erreur!! Numero entree n'est pas valide, \nSouhaitez-vous ajouter du sucre?")
            sucre2 = readLine("> ").toInt
          }


          if (sucre2 == 1) {
            Sucre = Sucre
            prixsucre = 0.0
          }
          if (sucre2 == 2 && Sucre >= 5) {
            Sucre -= 5
            prixsucre = 0.10
          }
          else if (sucre2 == 2 && Sucre < 5 ) { //erreur
            ErreurSucre = 1
            Sucre=Sucre
          }

          if (sucre2 == 3 && Sucre >= 10 ) {
            Sucre -= 10
            prixsucre = 0.20
          }
          else if (sucre2 == 3 && Sucre < 10) { //erreur
            ErreurSucre = 1
            Sucre=Sucre
          }

          if (sucre2 == 4 && Sucre >= 15) {
            Sucre -= 15
            prixsucre = 0.30
          }
          else if (sucre2 == 4 && Sucre < 15) { //erreur
            ErreurSucre = 1
            Sucre=Sucre
          }


          //lait supplementaire
          var lait = 0
          var prixlait = 0.05
          if (client == 2 || client == 3) {
            println("\nSouhaitez-vous ajouter du lait en supplement ?\n(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            lait = readLine("> ").toInt

            //Erreur de numero lait:
            while (lait != 1 && lait != 2) {
              println("Erreur!! Numero entree n'est pas valide, \nSouhaitez-vous ajouter du lait supplementaire?")
              lait = readLine("> ").toInt
            }

            if (lait == 1) {
              println("\nCombien de dose ? (0.05CHF par dose)")
              var dose = readLine("> ").toInt

              //Erreur de numero dose:
              while (dose != 1 && dose != 2 && dose != 3) {
                println("Erreur!! Numero entree n'est pas valide, \nCombien de dose?")
                dose = readLine("> ").toInt
              }

              if (dose == 1 && Lait >= 0.05) {
                Lait -= 0.05
                prixlait = prixlait * 1
              }
              else if (dose == 1 && Lait < 0.05 ) { //erreur
                ErreurLait = 1.1
                Lait=Lait
              }

              if (dose == 2 && Lait >= 0.1) {
                Lait -= (0.05 * 2)
                prixlait = prixlait * 2
              }
              else if (dose == 2 && Lait < 0.1) { //erreur
                ErreurLait = 1.1
                Lait=Lait
              }

              if (dose == 3 && Lait >= 0.15) {
                Lait -= (0.05 * 3)
                prixlait = prixlait * 3.0
              }
              else if (dose == 3 && Lait < 0.15) { //erreur
                ErreurLait = 1.1
                Lait=Lait
              }
            }
            else {
              prixlait = 0.0
            }
          }
          else if (client == 1) {
            prixlait = 0.0
          }


          //Commande de Boisson Reussie
          if (client == 1) {
            println("\nBoisson Selectionnee : Expresso")
          }
          else if (client == 2) {
            println("\nBoisson Selectionnee : Cappuccino")
          }
          else {
            if (client == 3 && Latte == 1) {
              println("\nBoisson Selectionnee : Latte (Petit)")
            }
            else if (client == 3 && Latte == 2) {
              println("\nBoisson Selectionnee : Latte (Moyen)")
            }
            else {
              println("\nBoisson Selectionnee : Latte (Grand)")
            }
          }

          if (sucre2 == 1) {
            println("Niveau de sucre : Sans sucre")
          }
          else if (sucre2 == 2) {
            println("Niveau de sucre : Peu (5g)")
          }
          else if (sucre2 == 3) {
            println("Niveau de sucre : Moyen (10g)")
          }
          else {
            println("Niveau de sucre : Beaucoup (15g)")
          }

          if (lait == 1) {
            println("Lait en supplement: OUI")
          }
          else {
            println("Lait en supplement: NON")
          }


          //Erreur
          if (ErreurPoudredeCafe==1) {
            println("\nErreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnee. \nVeuillez choisir une autre boisson ou verifier les stocks en mode Admin.")
          }
          if (ErreurLait == 1.1) {
            println("\nErreur : Quantite de lait insuffisante pour preparer la boisson selectionnee. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          if (ErreurSucre == 1) {
            println("\nErreur : Quantite de sucre insuffisante pour preparer la boisson selectionnee. \nVerifier les stocks en mode Admin.")
          }
          var PoudreDeCafeM= PoudreDeCafeDiminue-PoudreDeCafe
          var SucreM= SucreDiminue-Sucre
          var LaitM= LaitDiminue-Lait

          if (ErreurPoudredeCafe==1 || ErreurSucre==1 || ErreurLait==1.1) {
            PoudreDeCafe+=PoudreDeCafeM
            Sucre+= SucreM
            Lait+= LaitM

            ErreurPoudredeCafe=0
            ErreurSucre=0
            ErreurLait=0
          }


          else if (ErreurPoudredeCafe != 1 && ErreurLait != 1.1 && ErreurSucre != 1) {
            var prixTotal = (prix + prixsucre + prixlait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix, prixsucre, prixlait, prixTotal)

            //Paiement
            println("\nVeuillez payer en utilisant Twint.")

            def RandomTwintCode(): String = {
              Random.alphanumeric.take(5).mkString
            }
            var TwintCode = RandomTwintCode()
            println("Votre code de paiement est : " + TwintCode + " \n(En attente de validation du paiement...)")
            Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
            println("\nMerci ! Votre paiement a ete accepte.")


            //Preparation de la Boisson
            //apres l'affiche de paiement
            println("Preparation de votre boisson...")
            if (client == 1) {
              println("Votre Expresso est pret ! Bonne degustation !")
            }
            else if (client == 2) {
              println("Votre Cappuccino est pret ! Bonne degustation !")
            }
            else {
              println("Votre Latte est pret ! Bonne degustation !")
            }
            select1=1
          }
        }
      }



      val PIN= 434343
      if (selectionner == 2) {
        println("Mode Admin")
        var code= readLine("Entrez le code PIN : ").toInt
        while(code!=PIN){
          print("Le code PIN entree est faux. Veuillez reessayer le mode de passe: ")
          code=readInt()
        }
        if (code==PIN) {
          println("Acces autorise.")

          //Stockage apres l'utilisation
          println("\nStocks :")
          println("Poudre de Cafe : " + PoudreDeCafe + "g")
          printf("Lait : %.2f", Lait)
          print("L")
          println("\nSucre : " + Sucre + "g")


          println("\nReapprovisionnement des stocks... \nAjout :")
          var PoudreDeCafeAjout= readLine("Poudre de Cafe: ").toInt
          var LaitAjout = readLine("Lait: ").toDouble
          var SucreAjout = readLine("Sucre: ").toInt

          PoudreDeCafe= PoudreDeCafe+PoudreDeCafeAjout
          Lait= Lait+LaitAjout
          Sucre= Sucre+SucreAjout

          //erreur
          ErreurPoudredeCafe=0
          ErreurLait=0.0
          ErreurSucre=0

          println("Niveaux de stock mis a jour.")
          println("Retour au menu principal...")
        }
      }
    }


    if (selectionner==3){
      println("Vous avez quittez le programme")
    }
  }
}
