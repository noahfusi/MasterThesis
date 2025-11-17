import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val prixexpresso: Double = 2.00
    val prixcappuccino: Double = 2.50
    val prixlattePetit: Double = 2.70
    val prixlatteMoyen: Double = 3.20
    val prixlatteGrand: Double = 3.70

    val cafeexpresso: Int = 8
    val cafecappuccino: Int = 6
    val cafelattePetit: Int = 6
    val cafelatteMoyen: Int = 8
    val cafelatteGrand: Int = 12


    val laitcappuccino: Double = 0.1
    val laitlattePetit: Double = 0.12
    val laitlatteMoyen: Double = 0.15
    val laitlatteGrand: Double = 0.2

    var stockcafe = 50
    var stocksucre = 30
    var stocklait = 0.5

    var continuer = true
    var commandeenmodeclient = false

    while(continuer) {

      var mode = readLine("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt

      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println(mode)
        println("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readInt()
      }
      if (mode == 1) {
        do {
          var choixboisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ").toInt
          while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3)) {
            println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ")
            choixboisson = readInt()
          }

          var nomboisson = ""
          var prixboisson = 0.0
          var cafebesoin = 0
          var laitbesoin = 0.0

          if (choixboisson == 1) {
            nomboisson = "Expresso"
            prixboisson = prixexpresso
            cafebesoin = cafeexpresso

          } else if (choixboisson == 2) {
            nomboisson = "Cappuccino"
            prixboisson = prixcappuccino
            cafebesoin = cafecappuccino
            laitbesoin = laitcappuccino

          } else {
            var taillelate = readLine("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ").toInt
            while (!(taillelate == 1 || taillelate == 2 || taillelate == 3)) {
              print("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ")
              taillelate = readInt()
            }
            if (taillelate == 1) {
              nomboisson = "Latte(Petit)"
              prixboisson = prixlattePetit
              cafebesoin = cafelattePetit
              laitbesoin = laitlattePetit

            } else if (taillelate == 2) {
              nomboisson = "Latte(Moyen)"
              prixboisson = prixlatteMoyen
              cafebesoin = cafelatteMoyen
              laitbesoin = laitlatteMoyen

            } else {
              nomboisson = "Latte(Grand)"
              prixboisson = prixlatteGrand
              cafebesoin = cafelatteGrand
              laitbesoin = laitlatteGrand
            }
          }
          var choixsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4)) {
            println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
            choixsucre = readInt()
          }

          var prixsucre = 0.0
          var sucrebesoin = 0
          var nomsucre = ""

          if (choixsucre == 2) {
            prixsucre = 0.10
            sucrebesoin = 5
            nomsucre = "Peu (5g)"
          } else if (choixsucre == 3) {
            prixsucre = 0.20
            sucrebesoin = 10
            nomsucre = "Moyen (10g)"
          } else if (choixsucre == 4) {
            prixsucre = 0.30
            sucrebesoin = 15
            nomsucre = "Beaucoup (15g)"
          } else if (choixsucre ==1){
            nomsucre = "Sans sucre"
          }

          if (stockcafe < cafebesoin) {
            println("Boisson séléctionnée : " + nomboisson)
            println("Niveau de sucre: " + nomsucre)
            println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            commandeenmodeclient = true

          } else if (stocklait < laitbesoin) {
            println("Boisson séléctionnée : " + nomboisson)
            println("Niveau de sucre: " + nomsucre)
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            commandeenmodeclient = true
          } else {
            if (stocksucre < sucrebesoin) {
              println("Boisson séléctionée : " + nomboisson)
              println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              commandeenmodeclient = true
            } else {
              var laitsupplement = 0.0
              var prixlaitsupplement = 0.0
              var nomchoixlaitsupp = ""

              if (choixboisson == 2 || choixboisson == 3) {
                var choixlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
                while (!(choixlait == 1 || choixlait == 2)) {
                  println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
                  choixlait = readInt()
                }
                if (choixlait == 1) {
                  nomchoixlaitsupp = "Oui"
                  var choixdose = readLine("Combien de dose ?\n> ").toInt
                  while (!(choixdose == 1 || choixdose == 2 || choixdose == 3)) {
                    println("Combien de dose ?\n> ")
                    choixdose = readInt()
                  }
                  laitsupplement = choixdose * 0.05
                  prixlaitsupplement = choixdose * 0.05

                  if (choixdose ==1 && stocklait >= laitbesoin + 0.05){
                    choixdose = 1
                    laitsupplement = 0.05
                    prixlaitsupplement = 0.05
                  } else if (choixdose == 2 && stocklait >= laitbesoin + 0.10) {
                    choixdose = 2
                    laitsupplement = 0.10
                    prixlaitsupplement = 0.10
                  } else if (choixdose == 3 && stocklait >= laitbesoin + 0.15) {
                    choixdose = 3
                    laitsupplement = 0.15
                    prixlaitsupplement = 0.15
                  }

                } else if(choixlait==2){
                  nomchoixlaitsupp = "Non"
                }
              }

              if (stocklait < laitbesoin + laitsupplement) {
                println("Boisson séléctionnée : " + nomboisson)
                println("Niveau de sucre : " + nomsucre)
                println("Lait en supplément :" + nomchoixlaitsupp)
                println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
                commandeenmodeclient = true
              } else {
                if (!commandeenmodeclient){
                  println("Boisson séléctionnée : " + nomboisson)
                  println("Niveau de sucre : " + nomsucre)
                  if (choixboisson ==2 || choixboisson ==3) {
                    println("Lait en supplément : " + nomchoixlaitsupp )
                  }

                  val total = prixboisson + prixsucre + prixlaitsupplement
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n\n", prixboisson, prixsucre, prixlaitsupplement, total)
                  val chaine = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  var codepaiementtwint = ""
                  println("Veuillez payer en utilisant Twint.")
                  for (i <- 1 to 5) {
                    val caractere = (Math.random() * chaine.length).toInt
                    codepaiementtwint = codepaiementtwint + chaine(caractere)
                  }
                  println("Votre code de paiement est : " + codepaiementtwint + "\n(En attente de paiement...)")
                  Thread.sleep(3000)
                  println("\nPaiement confirmé.\nPréparation de votre boisson...")
                  println("Votre " + nomboisson + " est prêt ! Bonne dégustation !\n")

                  stockcafe = (stockcafe - cafebesoin)
                  stocklait = stocklait - (laitbesoin + laitsupplement)
                  stocksucre = stocksucre - sucrebesoin

                  //printf("Stocks restants :\nCafé : %d g\nLait : %.2f L\nSucre : %d g\n", stockcafe, stocklait, stocksucre)//
                }
              }
            }
          }
        } while (commandeenmodeclient)

      } else if (mode == 2) {

        val PINadmin = 434343
        println("Mode Admin")
        println("Entrez le code PIN : ")
        var codepin = readInt()

        var codepinjuste = false

        while(!codepinjuste) {
          if (codepin == PINadmin) {
            codepinjuste = true
            println("Accès autorisé. \n\nStocks :\n")
            printf("Poudre de café : %d g\n", stockcafe)
            printf("Lait : %.2f L\n", stocklait)
            printf("Sucre : %d g\n", stocksucre)
            println("Réaprovisionnement de stocks...")
            println("Ajout :\nPoudre de café :")
            stockcafe += readInt()
            println("Lait :")
            stocklait += readDouble()
            println("Sucre :")
            stocksucre += readInt()

            println("Niveaux de stock mis à jour. Retour au menu principal...\n")

          } else {
            codepin = readInt()
          }
        }
      } else {
        continuer = false
      }
    }
  }
}