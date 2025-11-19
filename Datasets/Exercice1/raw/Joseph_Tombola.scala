import scala.io.StdIn._
import math._
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val client = 1
    val admin = 2
    val quitter = 3
    var mode = 0
    var pin = 434343
    var poudrecaffe = 50.00
    var sucre = 30.00
    var lait = 0.50
    var prix = 0.00
    var demarage = true
    if (demarage == true) {
      do {
        println("Nospresso Cafe")
        println("Veuillez sélectionner votre mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
        if (mode != client && mode != admin && mode != quitter) {
          demarage = false

        }
      } while (demarage && (mode !=client && mode != admin && mode != quitter))
    }
    if (mode == admin){

      println("Entrez le code PIN : ")
      var pin = readLine(">").toInt
      if (pin != 434343){
      do {
        println("Accès non refusé")
      }while(pin == 434343)
      }
      println("Accès autorisé.")
    }
    if (mode == client) {
      var prix = 0.00
      var boissoncommande= ""
      var expresso = 1
      var cappuccino = 2
      var latte = 3
      var choixboisson = 0

     do {
       println("Veuillez sélectionner votre boisson:")
       println("1) Expresso - CHF 2.00")
       println("2) Cappuccino - CHF 2.50")
       println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
       choixboisson = readLine(">").toInt
     }while (choixboisson != expresso && choixboisson != cappuccino && choixboisson!= latte)

if ( choixboisson == expresso) {
  val consommationexpresso = 8.00


  if (poudrecaffe >= consommationexpresso) {
    poudrecaffe -= consommationexpresso
    prix += 2.00
     boissoncommande = "Expresso"
  }

  if (poudrecaffe < consommationexpresso) {
    do {
      println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionné.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }while (poudrecaffe> consommationexpresso)
  }
}

  if  ( choixboisson == cappuccino) {
    val consommationcapuccino = 6.00
    if (poudrecaffe >= consommationcapuccino && lait >= 0.10) {
      poudrecaffe -= consommationcapuccino
      lait -= 0.10
      prix += 2.50
       boissoncommande = "Capuccino"
    }


    if (poudrecaffe < consommationcapuccino || lait < 0.10) {
     do {
       println("Erreur: Quantité de poudre à café ou quantité de lait insuffisante")


       println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
     }while (poudrecaffe> consommationcapuccino)
    }
  }
      var choixlatte = 0
      var petitlatte = 1
      var moyenlatte = 2
      var grandlatte = 3


       if (choixboisson == latte) {
         boissoncommande = "Latte"
         do {
           println("Quel taille de Latte ?")
           println("1) Petit")
           println("2) Moyen")
           println("3) Grand")
           choixlatte = readLine(">").toInt
         } while (choixlatte != petitlatte && choixlatte != moyenlatte && choixlatte != grandlatte)
         if (choixlatte == petitlatte && poudrecaffe >= 6.00 && lait >= 0.12) {

           poudrecaffe -= 6.00

           lait -= 0.12

           prix += 2.70

           boissoncommande = "Petit Latte"


         }
         if ( choixlatte== moyenlatte && poudrecaffe>=8.00 && lait>= 0.15) {
           poudrecaffe -=8.00
           lait-= 0.15
           prix+=3.20
           boissoncommande= "Moyen Latte"
         }
       if (choixlatte== grandlatte && poudrecaffe >= 12.00 && lait >=0.20) {
         poudrecaffe-=12
         lait-=0.20
         prix+=3.70
         boissoncommande = "Latte Grand"
       }
         if (choixlatte == petitlatte && poudrecaffe < 6.00 && lait < 0.12 || choixlatte== moyenlatte && poudrecaffe<8.00 && lait< 0.15 ||choixlatte== grandlatte && poudrecaffe < 12.00 && lait <0.20 ){
          do {
           println("Erreur: Quantité de poudre à café ou quantité de lait insuffisante")


           println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }while (choixlatte == petitlatte && poudrecaffe >= 6.00 && lait >= 0.12 || choixlatte== moyenlatte && poudrecaffe>=8.00 && lait>= 0.15 ||choixlatte== grandlatte && poudrecaffe >= 12.00 && lait >=0.20 )

         }
       }
      var sanssucre = 1
        var peu = 2
        var moyen = 3
        var beaucoup = 4
        var choixsucre = 0
       do {
        println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        choixsucre = readLine(">").toInt
      }while ( choixsucre!= sanssucre && choixsucre != peu && choixsucre!= moyen && choixsucre != beaucoup)
var dosesucre = ""
      if(choixsucre== 2 && sucre>= 5.00) {
  sucre -= 5.00
  prix += 0.10
        dosesucre= "Peu(5g)"




  if (choixboisson == cappuccino || choixboisson == latte) {
        var oui = 1
        var non = 2
        var laitsup = 0
        do {
          println("Souhaitez-vous ajouter du lait supplémentaire ?")
          println("1) Oui")
          println("2) Non")
          laitsup = readLine(">").toInt
        } while (laitsup != oui && laitsup != non)

        if (laitsup == oui) {
          var dose = 0
          println("Combien de dose ?")
          dose = readLine(">").toInt
        }
      }
}
      println("Boisson commandé:" + boissoncommande)
      println("Niveau de sucre:" + dosesucre)


        println("Veuillez payer en utilisant Twint.")
        val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        var codepaiement = ""
        for (i <- 1 to 5) {
          val stringaleatoire = alphanumerique((math.random * alphanumerique.length).toInt).toString
          codepaiement.+=(stringaleatoire)
        }
          printf("Votre code de paiement est: %s\n", codepaiement)
println("En attente de validation du paiement...")
      Thread.sleep(5000)
      println("Merci ! Votre paiement a été accepté.")

      println("Péparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre "+ boissoncommande +  "est prêt ! Bonne dégustation !")



    }
  }
}
