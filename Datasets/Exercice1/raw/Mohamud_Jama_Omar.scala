import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var ajoutstocklait =0.0

    var PINdefaut = "434343"
    var PINsaisi = ""

    //stock consommation
    var consommepoudrecafe = 0
    var consommesucre = 0
    var consommelait = 0.0

    //Les stock
    var cafe = 50
    var lait = 0.5
    var sucre = 30

    //les ajout stock
    var ajoutCafe = 0
    var ajoutLait = 0.0
    var ajoutSucre = 0

    var typMode = 0
    var typeBoisson = 0
    var typeTaille = 0
    var typeLait = 0

    var extraSucre = 0
    var extraLait = 0.0
    var doseLait = 0

    var prixBase = 0.0

    //Personnalisation de la boisson
    println("Nospresso café")
    while (typMode != 1 && typMode != 2 && typMode != 3){
      println("Veuillez sélectionner votre mode :")
      println("1)Client")
      println("2)Admin")
      println("3)Quitter")
      typMode = readLine(">").toInt

      //Client Mode
      if(typMode == 1){
        while (typeBoisson != 1 && typeBoisson != 2 && typeBoisson != 3){
          println("Veuillez sélectionner votre boisson :")
          println("1)expresso")
          println("2)cappuccino")
          println("3)latte")
          typeBoisson = readLine(">").toInt

          //Expresso
          if(typeBoisson == 1){
            consommepoudrecafe = 8
            prixBase = 2.00
          }
          //Cappuccino
          else if(typeBoisson == 2){
            consommepoudrecafe = 6
            consommelait = 0.1
            prixBase = 2.50
          }
          //Latte
          else {
            while (typeTaille != 1 && typeTaille != 2 && typeTaille != 3) {
              println("Veuillez sélectionner votre taille de boisson :")
              println("1)petit")
              println("2)moyen")
              println("3)grand")
              typeTaille = readLine(">").toInt
              //petit
              if (typeTaille == 1) {
                consommepoudrecafe = 6
                consommelait = 0.12
                prixBase = 2.70
              }
              //Moyen
              else if(typeTaille == 2){
                consommepoudrecafe = 8
                consommelait = 0.15
                prixBase = 3.20
              }
              //Grand
              else{
                consommepoudrecafe = 12
                consommelait = 0.2
                prixBase = 3.70
              }
            }
          }

          //Supplement de sucre
          while (extraSucre != 1 && extraSucre != 2 && extraSucre != 3 &&  extraSucre != 4) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
            extraSucre = readLine(">").toInt

            if(extraSucre == 1){
              consommesucre = 0
            }
            else if(extraSucre == 2){
              consommesucre = 5
            }
            else if(extraSucre == 3){
              consommesucre = 10
            }
            else if(extraSucre == 4){
              consommesucre = 15
            }
          }

          //Supplement de lait
          while(typeLait != 1 && typeLait != 2) {
          println("Souhaitez-vous ajouter du lait en supplément ?")
            println("Quantité de poudre de café insuffisante pour\npréparer la boisson s´electionnée.")
            println("Veuillez choisir une taille plus petite ou essayer\nune autre boisson.")
            println("(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
            typeLait = readLine(">").toInt

            if(typeLait == 1){
              while(doseLait < 1 ||  doseLait  > 3){
                println("Combien de dose ?")
                doseLait = readLine(">").toInt
                extraLait = doseLait * 0.05
              }
            }
            else{
              doseLait = 0
            }
          }

          //Verifier le stock pour preparation le boisson
          if((consommepoudrecafe <= cafe) && ((consommelait + extraLait) <= lait) && (consommesucre <= sucre)) {
            //upgrade stock
            cafe = cafe - consommepoudrecafe
            sucre = sucre - consommesucre
            lait = lait - (consommelait + extraLait)


            //paiment
            println("Le prix total : CHF " + (prixBase + ((extraSucre - 1) * 0.10) + (doseLait * 0.05)))
            //twint
            println("Veuillez payer en utilisant Twint")
            println("code twint : " + Random.alphanumeric.take(5).mkString)
            Thread.sleep(3000)
            println("Merci ! Votre paiement a ´eté accepté.")

            //Preparation de la Boisson
            {
            println("Votre Cappuccino est pr^et ! Bonne d´egustation !")
            }

            typMode = 0
            typeTaille = 0
            typeLait = 0
            extraSucre = 0
          }
          else{
            println("Les stock n'est pas suffisante")
            typeBoisson = 0
            typeTaille = 0
            typeLait = 0
            extraSucre = 0
          }


        }









        println("Boisson type = " + typeBoisson)
        println("Taille type = " + typeTaille)
        println("Consommation cafe = " + consommepoudrecafe)
        println("Consommation lait = " + consommelait)
        println("Consommation sucre = " + consommesucre)
        println("Dose de lait = " + doseLait)



      }

      //Admin Mode
      else if(typMode == 2){
        println("Mode Admin")
        while(PINsaisi != PINdefaut){
          PINsaisi = readLine("Entrez le code PIN : ")

          if(PINsaisi == PINdefaut){
            println("Accès autorisé.")

            //ajouter des stock
            println("Stocks:")

            println("Poudre de café: " + cafe)
            println("Sucre         : " + sucre)
            println("Lait          : " + lait)

            println("Réapprovisionnement des stocks...")
            println("Combien de gram  cafe pour ajouter stock ? ")
            ajoutCafe = readLine(">").toInt
            cafe = cafe + ajoutCafe

            println("Combien de gram  sucre pour ajouter stock ? ")
            ajoutSucre = readLine(">").toInt
            sucre = sucre + ajoutSucre

            println("Combien de litre  lait pour ajouter stock ? ")
            ajoutLait = readLine(">").toDouble
            lait = lait + ajoutLait

            println("Ajout:")
            println("Poudre de café: " + ajoutCafe)
            println("Lait           : " + ajoutLait)
            println("Sucre          : " + ajoutSucre)

            println("Niveaux de stock mis `a jour.\nRetour au menu principal...")


          }
        }
        PINsaisi = ""
        typMode = 0
      }
      else{
        return
      }
    }
  }
}