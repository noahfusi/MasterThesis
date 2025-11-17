import scala.io.StdIn.readLine
import scala.util.Random

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Main {
  def main(args: Array[String]): Unit = {
    /////////variables communes/////////
    var choix=""//      choix du menu
    var Ssucre=30//     stock (grammes)
    var Slait=0.5//     stock (Litres)
    var Spoudre=50//    stock (grammes)
    val PIN = "434343"//code PIN
    ////////////////////////////////////
    ////menu principal début
    do {
      println("\n         Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      do {
        choix = readLine(">")
      }
      while(choix < "1" || choix > "3")
      ////menu principal fin
      ////menu client début
      if (choix=="1") {
        while(true) {
          //////////////////////////////variables client///////////////////////////////
          var isEspresso = false //détremine si le supplément lait est dispo
          var isLatte = false //   corrige l'inscription sur le ticket pour les lattes
          var ad1 = 0.00 //        type de boisson (prix)
          var ad2 = 0.00 //        supplément sucre (prix)
          var ad3 = 0.00 //        supplément lait (prix)
          var com1 = "" //         type de boisson (nom)
          var com2 = "" //         supplément sucre (nom)
          var com3 = "" //         supplément lait (nom)
          var Csucre = 0 //        sucre nécessaire (grammes)
          var Cpoudre = 0 //       poudre nécessaire (grammes)
          var Clait = 0.0 //       lait nécessaire (Litres)
          /////////////////////////////////////////////////////////////////////////////
          //sélection type de boisson début
          println("\nVeuillez sélectionner votre boisson :\n1) Espresso   - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte      - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          do {
            choix = readLine(">")
          }
          while (choix < "1" || choix > "3")
          //sélection type de boisson fin
          //Latte début
          if (choix == "3") {
            isLatte = true
            println("\nVeuillez sélectionner la taille :")
            println("1) Petit\n2) Moyen\n3) Grand")
            do {
              choix = readLine(">")
            }
            while (choix < "1" || choix > "3")
            if (choix == "1") {
              ad1 = 2.70
              Clait = 0.120
              Cpoudre = 6
              com1 = "Latte (Petit)"
            }
            else if (choix == "2") com1 = {
              ad1 = 3.20
              Clait = 0.150
              Cpoudre = 8
              "Latte (Moyen)"
            }
            else if (choix == "3") com1 = {
              ad1 = 3.70
              Clait = 0.200
              Cpoudre = 12
              "Latte (Grand)"
            }
          }
          //Latte fin
          //esp+capp début
          else if (choix == "1") {
            ad1 = 2.00
            Cpoudre = 8
            com1 = "Espresso"
            isEspresso = true
          }
          else if (choix == "2") {
            ad1 = 2.50
            Cpoudre = 6
            Clait = 0.1
            com1 = "Cappuccino"
          }
          //esp+capp fin
          //supplément sucre début
          println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu      (5g)  - CHF 0.10\n3) Moyen    (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
          do {
            choix = readLine(">")
          }
          while (choix < "1" || choix > "4")
          Csucre = (choix.toInt - 1) * 5    ////ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
          ad2 = (choix.toInt - 1) * 0.10
          if (choix == "1") com2 = "Sans Sucre"
          else if (choix == "2") com2 = "Peu (5g)"
          else if (choix == "3") com2 = "Moyen (10g)"
          else com2 = "Beaucoup (15g)"
          //supplément sucre fin
          //supplément lait début
          if (!isEspresso) {
            println("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
            do {
              choix = readLine(">")
            }
            while (choix < "1" || choix > "2")
            if (choix == "1") {
              println("\nCombien de doses ? (0,05CHF par dose. 3 Doses maximum)")
              do {
                choix = readLine(">")
              }
              while (choix < "0" || choix > "3")
              ad3 = choix.toInt * 0.05 //ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
              Clait = Clait + (choix.toInt * 0.1)
              if (choix == "1") com3 = "1 Dose"
              else if (choix == "2") com3 = "2 Doses"
              else if (choix == "3") com3 = "3 Dose"
              else com3 = "Non"
            }
            else com3 = "Non"
          }
          //supplément lait fin
          //contrôle des stocks début
          //stock suffisant début
          if (Ssucre >= Csucre && Slait >= Clait && Spoudre >= Cpoudre) {
            //MàJ stocks début
            Ssucre = Ssucre - Csucre
            Slait = Slait - Clait
            Spoudre = Spoudre - Cpoudre
            //MàJ stocks fin3
            //addition début
            println("\nBoisson choisie   : " + com1 + "\nNiveau de sucre   : " + com2)
            if (!isEspresso) println("Lait en supplément: " + com3)
            print("Prix total: ")
            if (ad1 > 0) printf("CHF %.2f", ad1) //boisson
            if (ad2 > 0) printf(" + CHF %.2f", ad2) //supplément sucre
            if (ad3 > 0) printf(" + CHF %.2f", ad3) //supplément lait
            printf(" = CHF %.2f", ad1 + ad2 + ad3)
            //addition fin
            //paiement début
            if (isLatte) com1 = "Latte" //correction du nom
            val twint = Random.alphanumeric.take(5).mkString.toUpperCase //génère "code twint"
            println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + twint + "\n(En attente de paiement...)")
            Thread.sleep(3000) //délai obligatoire de 3000 milisecondes
            println("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + com1 + " est prêt ! Bonne dégustation !")
            //paiement fin
          }
          //stock suffisant fin
          //Erreurs stock début
          else {
            if (Slait < Clait)
              println("\nErreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou une autre boisson")
            if (Ssucre < Csucre)
              println("\nErreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            if (Spoudre < Cpoudre)
              println("\nErreur: Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
          //Erreur stock fin
          //contrôle des stocks fin
        }
      }
      ////menu client fin
      ////menu admin début
      else if (choix == "2") {
        ///////////////////////////////variables Admin///////////////////////////////
        var tpoudre = 0 //                                 poudre à ajouter (grammes)
        var tlait = 0.0 //                                 lait à ajouter (Litres)
        var tsucre = 0 //                                  sucre à ajouter (grammes)
        var CodeIn = "" //                                 code saisi
        ////////////////////////////////////////////////////////////////////////////
        //login début
        CodeIn = readLine("Mode Admin\nEntrez le code PIN:")
        if (CodeIn == PIN) {
          //login fin
          //remplissage début
          printf("Accès autorisé\n\nStock:\n    Poudre de café:%5d g\n    Lait          :%5.2f L\n    Sucre         :%5d g\nRéapprovisionnement des stocks...\nAjout :\n",Spoudre,Slait,Ssucre)
          tpoudre = readLine("    Poudre de café:").toInt
          tlait = readLine  ("    Lait          :").toDouble
          tsucre = readLine ("    Sucre         :").toInt
          Slait = Slait + tlait
          Ssucre = Ssucre + tsucre
          Spoudre = Spoudre + tpoudre
          println("Niveaux de stock mis à jour.\nRetour au menu principal...")
        }
        //remplissage fin
        else println("Accès refusé")
      }
      ////menu admin fin
    }while(choix !="3")//casse la boucle si "quitter" est choisi
  }
}