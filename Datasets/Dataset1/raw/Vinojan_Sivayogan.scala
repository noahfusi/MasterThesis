import io.StdIn._
import util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //Décalration des variables
    var mode = 0
    var PIN = "434343"

    val prixExpresso = 2.00
    val prixCappuccino = 2.50
    val prixLatteS = 2.70
    val prixLatteM = 3.20
    val prixLatteL = 3.70
    val prixSucre = 0.10

    var stockCafe = 50
    var stockSucre = 30
    var stockLait = 500.00

    //Début du programme
    while (mode != 3){
      //Choix du mode
      println("\tNospresso Café" +
        "\nVeuillez sélectionner votre mode :" +
        "\n1) Client" +
        "\n2) Admin" +
        "\n3) Quitter")

      //Obliger l'utilisateur à choisir un mode
      do{
        mode = readLine(">").toInt
      } while ( mode != 1 && mode != 2 && mode != 3)

      //Mode Client
      if (mode == 1){
        var boisson = 0
        var nomBoisson = ""
        var tailleLatte = 0
        var nomTailleLatte = ""
        var nivSucre = 0
        var nomNivSucre = ""
        var nivLait = 0
        var nomNivLait = "Non"
        var cafeRequis = 0
        var sucreRequis = 0
        var laitRequis = 0
        var prix = 0.00

        //Choix de la boisson
        println("\nVeuillez sélectionner votre boisson :" +
          f"\n1) Expresso - CHF $prixExpresso%.2f" +
          f"\n2) Cappuccino - CHF $prixCappuccino%.2f" +
          f"\n3) Latte - CHF $prixLatteS%.2f (Petit), CHF $prixLatteM%.2f (Moyen), CHF $prixLatteL%.2f (Grand)")
        boisson = readLine(">").toInt

        if (boisson == 1) {
          nomBoisson = "Expresso"
          prix += prixExpresso
          cafeRequis = 8
        } else if (boisson == 2) {
          nomBoisson = "Cappuccino"
          prix += 2.50
          cafeRequis = 6
          laitRequis = 100
        } else if (boisson == 3) {
          println("\nVeuillez choisir la taille du Latte : " +
            "\n1) Petit" +
            "\n2) Moyen" +
            "\n3) Grand")
          tailleLatte = readLine(">").toInt
          nomBoisson = "Latte"
          if (tailleLatte == 1) {
            prix += 2.70
            cafeRequis = 6
            laitRequis = 120
            nomTailleLatte = "(Petit)"
          } else if (tailleLatte == 2) {
            prix += 3.20
            cafeRequis = 8
            laitRequis = 150
            nomTailleLatte = "(Moyen)"
          } else if (tailleLatte == 3) {
            prix += 3.70
            cafeRequis = 12
            laitRequis = 200
            nomTailleLatte = "(Grand)"
          }
        }

        //Choix taux de sucre
        println("\nSouhaitez-vous ajouter du sucre ?" +
          "\n1) Sans sucre" +
          f"\n2) Peu (5g) - CHF $prixSucre.2f" +
          f"\n3) Moyen (10g) - CHF ${prixSucre * 2}%.2f" +
          f"\n4) Beaucoup (15g) - CHF ${prixSucre * 3}%.2f")
        nivSucre = readLine(">").toInt
        if(nivSucre == 2){
          nomNivSucre = "Peu (5g)"
          sucreRequis = 5
          prix += prixSucre
        }else if(nivSucre == 3){
          nomNivSucre = "Moyen (10g)"
          sucreRequis = 10
          prix += prixSucre*2
        }else if(nivSucre == 4){
          nomNivSucre = "Beaucoup (15g)"
          sucreRequis = 15
          prix += prixSucre*3
        }else{
          nomNivSucre = "Sans sucre"
        }

        //Choix taux de lait pour Cappuccino et Latte
        if(boisson == 2 || boisson == 3) {
          println("\nSouhaitez-vous ajouter du lait en suppléement ?" +
            "\n(Disponible uniquement pour Cappuccino et Latte)" +
            "\n1) Oui" +
            "\n2) Non")

          //Choix nombre de dose
          if (readLine(">").toInt == 1) {
            nomNivLait = "Oui"
            println("\nCombien de dose ?")
            nivLait += readLine(">").toInt
            laitRequis += nivLait * 50
            prix += nivLait * 0.05
          }
        }

        //Vérification des stocks
        if (stockCafe >= cafeRequis && stockSucre >= sucreRequis && stockLait >= laitRequis){
          println(f"\nBoisson sélectionnée : $nomBoisson" +
            f"\nNiveau de sucre : $nomNivSucre" +
            f"\nLait supplémentaire: $nomNivLait" +
            f"Prix total : $prix%.2f")

          //Paiement
          val twint = Random.alphanumeric.take(5).mkString("")
          println("\nVeuillez payer en utilisant Twint." +
            f"\nVotre code de paiement est : $twint" +
            f"\n(En attente de validation du paiement...)")

          //Pause de 3 secondes
          Thread.sleep(3000)
          println("\nMerci ! Votre paiement a été accepté.")

          //Mise à jour des stocks
          stockSucre -= sucreRequis
          stockCafe -= cafeRequis
          stockLait -= laitRequis

          println("\nPréparation de votre boisson..." +
            "\n[...]")

          //Pause de 5 secondes
          Thread.sleep(5000)
          println(f"Votre $nomBoisson est prêt ! Bonne dégustation !")
        }else {
          if(stockCafe < cafeRequis){
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
          }else if(stockSucre < sucreRequis){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          }else if(stockLait < laitRequis){
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          }
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
      }else if (mode == 2) {
        //Mode Admin
        print("\nEntrez le code PIN : ")
        if(PIN.equals(readLine())){
          println("Accès autorisé.")

          //Affichage des stocks
          println("\nStocks:" +
            "\n\tPoudre de café\t:\t" + stockCafe + "g" +
            "\n\tLait\t\t\t:\t" + stockLait/1000 + "L" +
            "\n\tSucre\t\t\t:\t" + stockSucre + "g")

          //Réapprovisionnement
          println("\nRéapprovisionnement des stocks ...:" +
            "\nAjout :")
          println("\tPoudre de café\t:\t")
          stockCafe += readInt()
          println("\tLait\t\t\t:\t")
          stockLait += readFloat()*1000
          println("\tSucre\t\t\t:\t")
          stockSucre += readInt()

          //Retour au Menu
          println("Niveaux de stock mis à jour." +
            "\nRetour au menu principal...")
        }else{
          println("Mauvais PIN")
        }
      }
    }
  }
}