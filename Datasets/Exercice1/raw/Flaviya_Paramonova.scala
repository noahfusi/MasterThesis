import scala.util.Random
import scala.io.StdIn._
import math._

object Main {
  def main(args: Array[String]): Unit = {
    var prix = 0.0
    var prixBoisson = 0.0
    var prixSucre = 0.0
    var prixLaitSupplement= 0.0

    // on defini le stock
    var PoudreDeCafe = 50
    var QSucre = 30
    var QLait = 500.0


//on donne les noms
    var nomboisson = ""
    var NomSucre = ""
    var NomLait = ""

    // var provisoires qui servent à gerer le stock
    var PoudreDeCafeSpec = 0
    var QSucreSpec = 0
    var QLaitSpec = 0.0

    // var rajout, pour le mode admin
    var RajoutPoudreDeCafe = 0
    var RajoutLait = 0.0
    var RajoutSucre = 0

    // on cree la boucle qui fait qu'on retourne au menu principal apres chaque action
    var choixUtilisateur = 0
    do {
      // on choisi le mode
      do {
        choixUtilisateur = readLine("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n>").toInt
        if (!(choixUtilisateur == 1) && !(choixUtilisateur == 2) && !(choixUtilisateur == 3)) {
          println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
        }
      } while (!(choixUtilisateur == 1) && !(choixUtilisateur == 2) && !(choixUtilisateur == 3))

      // on fait le mode client
      if (choixUtilisateur == 1) {
        var boisson = 0
        //on choisi la boisson
        do {
          boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>").toInt
          if (!(boisson == 1) && !(boisson == 2) && !(boisson == 3)) {
            println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
          }
        } while (!(boisson == 1) && !(boisson == 2) && !(boisson == 3))

        if (boisson == 1) {
          nomboisson = "Expresso"
          prixBoisson += 2.00
          PoudreDeCafeSpec += 8
        }
        if (boisson == 2) {
          nomboisson = "Cappuccino"
          prixBoisson += 2.50
          PoudreDeCafeSpec += 6
          QLaitSpec += 100
        }

        // dans le cas où la personne a choisi un latte
        var TailleLatte = 0
        if (boisson == 3) {
          do {
            TailleLatte = readLine("Veuillez sélectionner la taille du latte:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
            if (!(TailleLatte == 1) && !(TailleLatte == 2) && !(TailleLatte == 3)) {
              println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
            }
          } while (!(TailleLatte == 1) && !(TailleLatte == 2) && !(TailleLatte == 3))
          if (TailleLatte == 1) {
            nomboisson = "Latte petit"
            prixBoisson += 2.70
            PoudreDeCafeSpec += 6
            QLaitSpec += 120

          }
          if (TailleLatte == 2) {
            nomboisson = "Latte moyen"
            prixBoisson += 3.20
            PoudreDeCafeSpec += 8
            QLaitSpec += 150
          }
          if (TailleLatte == 3) {
            nomboisson = "Latte grand"
            prixBoisson += 3.70
            PoudreDeCafeSpec += 12
            QLaitSpec += 200
          }
        }

        // on passe à la personnalisation de la commande
        // étape 1 sucre
        var sucre = 0
        if ((boisson == 1) || (boisson == 2) || (boisson == 3)) {
          do {
            sucre = readLine("Souhaitez-vous ajouter du sucre ? :\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
            if (!(sucre == 1) && !(sucre == 2) && !(sucre == 3) && !(sucre == 4)) {
              println("votre reponse n'est pas correcte , choisissez 1, 2, 3 ou 4")
            }
          } while (!(sucre == 1) && !(sucre == 2) && !(sucre == 3) && !(sucre == 4))
          if (sucre == 2) {
            prixSucre += 0.10
            QSucreSpec += 5
            NomSucre = "Peu (5g)"
          }
          if (sucre == 3) {
            prixSucre += 0.20
            QSucreSpec += 10
            NomSucre = "Moyen (10g)"
          }
          if (sucre == 4) {
            prixSucre += 0.30
            QSucreSpec += 15
            NomSucre = "Beaucoup (15g)"
          }
        }

        //etape 2.0 lait(oui ou non)
        var lait = 0
        var DoseLait = 0
        if ((boisson == 2) || (boisson == 3)) {
          //on cree la boucle qui fait qu'on retourne au menu initial apres chaque action
          do {
            lait = readLine("Souhaitez-vous ajouter du lait en supplément ? :\n1)oui\n2)non\n>").toInt

            if (!(lait == 1) && !(lait == 2)) {
              println("votre reponse n'est pas correcte , coisissez 1 ou 2")

              if(lait == 2){
                NomLait= "sans lait"
              }
            }

          }while (!(lait == 1) && !(lait == 2))
            //etape 2.1 lait(combien?)
            if (lait == 1) {
              do {
                DoseLait = readLine("combien de doses?\n>").toInt
                if (!(DoseLait == 1) && !(DoseLait == 2) && !(DoseLait == 3)) {
                  println("votre reponse n'est pas correcte , 3 est la dose maximale")
                }
              } while (!(DoseLait == 1) && !(DoseLait == 2) && !(DoseLait == 3))
            }
          if (lait == 1) {
            prixLaitSupplement += 0.05
            QLaitSpec += 50
            NomLait= "1 dose de lait"

          }
          if (lait == 2) {
            prixLaitSupplement += 0.10
            QLaitSpec += 100
            NomLait= "2 doses de lait"
          }
          if (lait == 3) {
            prixLaitSupplement += 0.15
            QLaitSpec += 150
            NomLait= "3 doses de lait"
          }
        }
        println("Boisson sélectionnée : "+nomboisson +" \n Niveau de sucre : "+ NomSucre +"\nLait en supplément:"+NomLait )
        // Vérification des stocks avant déduction
        if (PoudreDeCafe < PoudreDeCafeSpec) {
          println("Erreur : Quantité de poudre de café insuffisante.")
        } else if (QSucre < QSucreSpec) {
          println("Erreur : Quantité de sucre insuffisante.")
        } else if (QLait < QLaitSpec) {
          println("Erreur : Quantité de lait insuffisante.")
        } else {
          // deduction des ingredients du stock , apres la confirmation que le stock est suffisant!
          PoudreDeCafe -= PoudreDeCafeSpec
          QSucre -= QSucreSpec
          QLait -= QLaitSpec



          // on passe au paiement
          // On calcule le prix total à payer $$$
          prix = prixSucre + prixBoisson + prixLaitSupplement
          // find a function that keeps only les centimes dans les decimales
          // prix = function_name(prix)



          // on montre au client comment est cree le prix
          if (!(prixSucre == 0) && !(prixLaitSupplement == 0)) {
            println("Prix total: CHF " + prixBoisson + " + CHF" + prixSucre + " + CHF" + prixLaitSupplement + " = CHF " + prix)
          }
          else if (!(prixLaitSupplement == 0)&&(prixSucre == 0)) {
            println("Prix total: CHF " + prixBoisson + " + CHF " + prixLaitSupplement + " + CHF " + " = CHF " + prix)
          }
          else if (!(prixSucre == 0) && (prixLaitSupplement == 0) ) {
            println("Prix total: CHF " + prixBoisson + " + CHF " + prixSucre  + " = CHF" + prix)
          }
          else if ((prixSucre == 0) && (prixLaitSupplement == 0) ) {
            println("Prix total: CHF " + prixBoisson + "  = CHF " + prix)
          }



          // Création du code Twint
          println("Veuillez payer en utilisant Twint.\nVotre code de paiement est :")
          var chars = "ABCDEFGHIJKLMOPQRSTUVWXYZ1234567890"
          var code = ""
          for (i <- 1 to 5) {
            val index = (math.random() * chars.length).toInt
            code += chars(index)
          }
          println(code + "\n(En attente de paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté")
          println("Préparation de votre boisson...\n[...]\nVotre boisson est prête ! Bonne dégustation !")
        }
        // apres le paiement et la preparation de la boisson on remet les valeurs provisoires à 0
        PoudreDeCafeSpec = 0
        QSucreSpec = 0
        QLaitSpec = 0.0
        // de plus on remet les prix à 0
        prix = 0.0
        prixBoisson = 0.0
        prixSucre = 0.0
        prixLaitSupplement= 0.0

      } else if (choixUtilisateur == 2) {
        var CodeAdmin = 0
        do {
          CodeAdmin = readLine("Mode Admin\nEntrez le code PIN : ******\n>").toInt
          if (!(CodeAdmin == 434343)) println("Code incorrect")
        } while (!(CodeAdmin == 434343))

        //on redit au client ce qu'il a pris


        println("Stocks:\nPoudre de café :" + PoudreDeCafe + "g\nLait :" + QLait + "ml \nSucre :" + QSucre + "g")
        println("Réapprovisionnement des stocks...\n si vous ne voulez rien ajouter entrez la valeur 0!")
        RajoutPoudreDeCafe = readLine("Ajout :\n PoudreDeCafe >").toInt
        RajoutLait = readLine(" Lait :>").toDouble
        RajoutSucre = readLine(" Sucre :>").toInt

        // on rajoute les ingredients dans le stock
        PoudreDeCafe += RajoutPoudreDeCafe
        QSucre += RajoutSucre
        QLait += RajoutLait

        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      } else if (choixUtilisateur == 3) {
        println("Vous avez quitté le programme.\n :)")
      }
    } while (!(choixUtilisateur == 3))
  }
}
