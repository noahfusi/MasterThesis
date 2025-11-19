import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var mode = 0

    var stockCafe = 50
    var stockLait = 0.5
    var stockSucre = 30

    val pinAdmin = "434343"

    var boissonPreparee = false
    var quitterProgramme = false

    do{

      do{
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
      }while(mode<1 || mode>3)

      if(mode == 1){
        do
        {
          var choixBoisson = 0
          var choixTailleLatte = 0
          var choixSucre = 0
          var choixSupplementLait = 0
          var nbDosesLait = 0

          do{
            println("")

            println("Veuillez sélectionner votre boisson :")
            println("1) Expresso - CHF 2.00")
            println("2) Cappuccino - CHF 2.50")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            choixBoisson = readLine(">").toInt
          }while(choixBoisson<1 || choixBoisson>3)

          if(choixBoisson == 3){

            do {
              println("Veuillez choisir la taille du latte :")
              println("1) Petit")
              println("2) Moyen")
              println("3) Grand")
              choixTailleLatte = readLine(">").toInt
            }while(choixTailleLatte<1 || choixTailleLatte>3)

          }

          do{
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            choixSucre = readLine(">").toInt
          }while(choixSucre<1 || choixSucre>4)


          if(choixBoisson == 2 || choixBoisson == 3){

            do{
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("1) Oui")
              println("2) Non")
              choixSupplementLait = readLine(">").toInt
            }while(choixSupplementLait<1 || choixSupplementLait>2)

            if(choixSupplementLait == 1){
              do{
                println("Combien de dose ?")
                nbDosesLait = readLine(">").toInt
              }while(nbDosesLait<1 || nbDosesLait>3)
            }

          }

          var prixBoisson = 0.0
          var prixSucre = 0.0
          var prixSupplementLait = 0.0
          var prixTotal = 0.0

          var qteCafeNecessaire = 0
          var qteSucreNecessaire = 0
          var qteLaitNecessaire = 0.0

          var nomBoisson = ""
          var tailleBoisson = ""

          if(choixBoisson == 1){
            nomBoisson = "Expresso"
            prixBoisson = 2
            qteCafeNecessaire = 8
          }else if (choixBoisson == 2){
            nomBoisson = "Cappuccino"
            prixBoisson = 2.5
            qteCafeNecessaire = 6
            qteLaitNecessaire = 0.1
          } else {

            nomBoisson = "Latte"

            if(choixTailleLatte == 1){
              prixBoisson = 2.7
              tailleBoisson = " (Petit)"
              qteCafeNecessaire = 6
              qteLaitNecessaire = 0.12
            }else if(choixTailleLatte == 2){
              prixBoisson = 3.2
              tailleBoisson = " (Moyen)"
              qteCafeNecessaire = 8
              qteLaitNecessaire = 0.15
            }else{
              prixBoisson = 3.7
              tailleBoisson = " (Grand)"
              qteCafeNecessaire = 12
              qteLaitNecessaire = 0.2
            }
          }

          var quantiteSucre = ""

          if(choixSucre == 1){
            quantiteSucre = "Sans sucre"
          } else if (choixSucre == 2){
            quantiteSucre = "Peu (5g)"
            prixSucre = 0.1
            qteSucreNecessaire+=5
          } else if (choixSucre == 3) {
            quantiteSucre = "Moyen (10g)"
            prixSucre = 0.2
            qteSucreNecessaire+=10
          } else {
            quantiteSucre = "Beaucoup (15g)"
            prixSucre = 0.3
            qteSucreNecessaire+=15
          }

          var avecSupplementLait = ""

          if(choixSupplementLait == 2){
            avecSupplementLait = "Non"
          } else if (choixSupplementLait == 1) {
            avecSupplementLait = nbDosesLait.toString+" dose(s)"
            prixSupplementLait =  nbDosesLait*0.05
            qteLaitNecessaire += nbDosesLait*0.05
          }


          //affichage boisson selectionnée et le niveau de sucre
          println("Boisson sélectionnée : " + nomBoisson + tailleBoisson)
          println("Niveau de sucre : " + quantiteSucre)

          //affichage supplément lait si la boisson n'est pas expresso
          if(choixBoisson != 1) {
            println("Lait supplémentaire : " + avecSupplementLait)
          }

          println("")

          if(qteCafeNecessaire > stockCafe){
            println("Erreur : Quantité de poudre de café insuffisante pour pràparer la boisson sàlectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }else if(qteSucreNecessaire > stockSucre){
            println("Erreur : Quantité de sucre insuffisante pour pràparer la boisson sàlectionnée.")
          }else if(qteLaitNecessaire > stockLait){
            println("Erreur : Quantité de lait insuffisante pour pràparer la boisson sàlectionnée.")

            //proposer au client de changer la taille du latte s'il a choisi grand ou moyen
            if(choixTailleLatte > 1)
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")

          }else{

            //réduire les stocks avec la quantité nécessaire
            stockCafe-=qteCafeNecessaire
            stockLait-=qteLaitNecessaire
            stockSucre-=qteSucreNecessaire

            prixTotal = prixBoisson+prixSucre+prixSupplementLait

            //affichage message prix total
            printf("Prix total : CHF %.2f",prixBoisson)

            //affiche le prix du sucre si ajouté
            if(prixSucre>0)
              printf(" + CHF %.2f",prixSucre)

            //affiche le prix du supplément lait si ajouté
            if(prixSupplementLait>0)
              printf(" + CHF %.2f",prixSupplementLait)

            //affiche le total si plusieurs prix ont été ajouté
            if(prixTotal!=prixBoisson)
              printf(" = CHF %.2f",prixTotal)

            print("\n")
            println("")

            //générer code Twint
            var codeTwint = ""
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

            for(i <- 1 to 5){
              codeTwint += chars(Random.nextInt(chars.length))
            }

            //affichage messages payment
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : "+codeTwint)
            println("(En attente de validation du paiement...)")
            println("")
            Thread.sleep(3000)
            println("Paiement confirmé.")

            //affichage messages préparation
            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

            boissonPreparee = true
          }
        }while(boissonPreparee == false)
      }

      if(mode == 2) {

        println("Mode Admin")

        var pinEntre = ""
        do{
          pinEntre = readLine("Entrez le code PIN : ")
        }while(pinEntre != pinAdmin)

        if(pinEntre == pinAdmin){
          println("Accès autorisé.")

          println("Stocks : ")
          println(" Poudre de café: : "+stockCafe+"g")
          println(" Lait : "+stockLait+"L")
          println(" Sucre : "+stockSucre+"g")

          println("Réapprovisionnement des stocks... ")
          println("Ajout : ")
          var qteReapCafe = readLine("Poudre de café : ").toInt
          var qteReapLait = readLine("Lait : ").toDouble
          var qteReapSucre = readLine("Sucre : ").toInt

          stockCafe+=qteReapCafe
          stockLait+=qteReapLait
          stockSucre+=qteReapSucre

          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
        }

      }

      if(mode == 3){
        quitterProgramme = true
      }

    }while(quitterProgramme == false)
  }
}