import io.StdIn._
import math._
import scala.util.Random
import io.StdIn.readLine
import scala.reflect.internal.util.TriState.{False, True}

object Main {
  def main(args: Array[String]): Unit = {

    //prix boisson
    val expresso: Double = 2.00
    val Cappuccino: Double = 2.50
    val lattepetit: Double = 2.70
    val lattemoyen: Double = 3.20
    val lattegrand: Double = 3.70

    //variable different sucre prix
    var sanssucre: Double = 0.0
    var peusucre :Double = 0.10
    var moyensucre : Double = 0.20
    var beaucoupsucre:Double  = 0.30

    //variable grammage sucre

    var sisucrepeu: Double = 5.0
    var sisucremoyen: Double = 10.0
    var sisucrebeaucoup: Double = 15.0

    //stock initiaux : en gramme
    var sipoudredecafe: Double = 50.0
    var sisucre: Double = 30.0
    var silait: Double = 0.500

    //Consommation des ingrédients par type de boisson: en gramme
    var consoexpresso: Double = 8.0
    var consocappuccinocafe: Double = 6.0
    var consocappuccinolait: Double =0.100
    var consolattepetitcafe: Double = 6.0
    var consolattepetitlait: Double =0.100
    var consolattemoyencafe: Double = 8.0
    var consolattemoyenlait: Double =0.150
    var consolattegrandcafe: Double = 12.0
    var consolattegrandlait: Double =0.200
    var sucre = 0
    var encore = True

    //Pour avoir présentation à la fin correcte
    var boissonnom=""
    var sucrenom=""
    var laitnom= ""



    while (encore==True){
      println("\n\nNospresso Café")
      println()
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">")

      val choix= readInt()

      if (choix==1){
        println("Mode Client")
        println("\n\nVeuillez sélectionner votre boisson:")
        println("1) Expresso - " + expresso + " CHF")
        println("2) Cappuccino -" + Cappuccino + "CHF")
        println("3) Latte - " + " Latte petit " + lattepetit + " CHF " )
        println("4) Latte - " + " Latte Moyen " + lattemoyen + " CHF ")
        println("5) Latte -" + " Latte Grand " + lattegrand + " CHF ")
        print(">")

        //variable choix boisson
        var boisson=readInt()
        var prixtotal=0.0
        var prixboisson=0.0
        var prixsucre=0.0
        var laitconso=0.0
        var sucreconso=0.0
        var cafeconso=0.0


      if (boisson==1){
          boissonnom="Expresso"
          println("\n\nBoisson séléctionnée: " + boissonnom)
          prixboisson=expresso
          cafeconso+= consoexpresso

        }
      else if (boisson==2){
          boissonnom = "Cappuccino"
          println("Boisson séléctionné: " + boissonnom)
          prixboisson= Cappuccino
          cafeconso+= consocappuccinocafe
          laitconso+= consocappuccinolait

        }
      else if (boisson==3){
          boissonnom= "Latte petit"
          println("Boisson séléctionné: " + boissonnom)
          prixboisson= lattepetit
          cafeconso+= consolattepetitcafe
          laitconso+= consolattepetitlait

        }
      else if (boisson==4){
          boissonnom= "Latte moyen"
          println("Boisson séléctionné: " + boissonnom)
          prixboisson= lattemoyen
          cafeconso+= consolattemoyencafe
          laitconso+= consolattemoyenlait

        }
      else if (boisson==5){
          boissonnom= "Latte grand"
          println("Boisson séléctionné: " + boissonnom)
          prixboisson= lattegrand
          cafeconso+= consolattegrandcafe
          laitconso+= consolattegrandlait

        }
      else { while (boisson != 1 && boisson != 2 && boisson != 3 && boisson != 4 && boisson != 5) {
          println("Boisson n'existe pas, réessayer : " )
          boisson=readInt()

        }

        }

        //pour rajouter du sucre
      if (boisson>=1 && boisson<=5){
        println("\n\nSouhaitez-vous ajouter du sucre ?")
        println("1)Sans sucre")
        println("2)Peu (5g) -" + peusucre + "CHF")
        println("3)Moyen (10g) -" + moyensucre + "CHF")
        println("4)Beaucoup (15g)-" + beaucoupsucre + "CHF")
        print(">")
        var sucre = readInt()
        if (sucre==1){
          sucrenom= "Sans sucre"
          println("Niveau de sucre : " + sucrenom)
          prixsucre= sanssucre
        }else if (sucre==2){
          sucrenom= "Peusucre (5g)"
          println("Niveau de sucre : " + sucrenom)
         sucreconso+= sisucrepeu
          prixsucre= peusucre
        } else if (sucre==3){
          sucrenom = "Moyen sucre (10g)"
          println("Niveau de sucre : " + sucrenom)
          sucreconso+= sisucremoyen
          prixsucre = moyensucre


        }else {
          sucrenom= "Beaucoup sucre (15g)"
          println("Niveau de sucre : " + sucrenom)
          sucreconso+= sisucrebeaucoup
          prixsucre = beaucoupsucre

        }



      }
var choixlaitsupp=0
        var dose = 4
        //rajouter du lait ou non dans cappucino et latte
        if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5){
          println("\n\nSouhaitez-vous ajouter du lait en supplément ?")

          println("1) Oui")
          println("2) Non")
          print(">")
          choixlaitsupp =readInt()
          if (choixlaitsupp == 1) {
            laitnom= "Oui"
            println("Lait supplémentaire: " + laitnom)

            while(dose>3){
              println("Combien de dose ? (Maximum 3 doses)")
              dose=readInt()
              println("Le nombre de dose choisi est : " + dose )
              laitconso+=dose*0.05
            }
          }
          else {
            laitnom= "Non"
            println("\n\nLait supplémentaire : " + laitnom)
          }

      }

//Résumé de commande
println("\n\nBoisson Séléctionnée : " + boissonnom )
println("Niveau de sucre : " + sucrenom )

 if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5) {
        println ("Lait en supplément : " + laitnom)
}
 //faisons les stocks
 sipoudredecafe=sipoudredecafe-cafeconso
 silait=silait-laitconso
 sisucre= sisucre-sucreconso
  if (sisucre<0){
    println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
    println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
  }
  if (silait<0){
     println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
     println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
  if (sipoudredecafe<0){
     println("Quantité de cafe insuffisante pour préparer la boisson sélectionnée.")
     println("Veuillez choisir une autre boisson ou essayer une autre boisson.")
        }

        //Paiement
if (sisucre>0 && silait>0 && sipoudredecafe>0) {
        prixtotal=prixboisson + prixsucre
        //println("Prix Total :  CHF " + prixboisson+ " + CHF " + prixsucre + "= CHF " + prixtotal)
        printf("\n\nPrix Total :  CHF  %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixtotal)
        println("\nVeuillez payer en utilisant Twint.")
        val chars="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        var code = ""
        for (i<- 1 to 5){
    val randomChar= chars(Random.nextInt(chars.length))
    code += randomChar
  }

        println("Votre code de paiement est : " + code )
        println ("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("\n\nMerci ! Votre paiement a été accepté")

        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre "+ boissonnom + " est prêt ! Bonne dégustation !"   )
        //faisons les stocks
        Thread.sleep(5000)
        }
      }


      if (choix==2){

        val pincorrect = "434343"
        var pin = ""

        do {
          println("Entrez le code PIN:")
          pin=readLine()
          if (pin != pincorrect){
            println("Code PIN incorrect. Veuillez réessayer.")
          }
        }while (pin!=pincorrect)

          println("Accès autorisé.")
          println("\n\nStocks:")
          println("Poudre de café:" + sipoudredecafe +"g")
          println("Lait          :" + silait+"L" )
          println("Sucre         :" + sisucre+"g")

          println("\n\nRéapprovisionnement des stocks...")
          println("Ajout : ")


          println(f"Poudre de café: " )
          var acafe = readDouble
          println(f"Lait          : " )
          var alait= readDouble
          println(f"Sucre          : " )
          var asucre = readDouble
          println("Niveau de stock mis à jour.")
          sipoudredecafe+=acafe
          sisucre+=asucre
          silait+=alait
          println("Retour au menu principal...")


}
    if (choix==3){encore=False}

    }


 }

    }





