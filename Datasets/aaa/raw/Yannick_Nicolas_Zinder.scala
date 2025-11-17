import scala.io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {


    // Déclaration de variables pour condition
    var augmentationprixsucre: Double = 0 //variable augm = 0 car pas encore eu de variation
    var mode: Int = 0 // départ du code
    var poudrecafe: Int = 50 // selon consigne
    var stocksucre: Int = 30 // selon consigne
    var stocklait: Double = 0.5 // selon consigne

    //code numérique twint
    val coderandom = Random.nextInt(90000)

    // Stock initial
    val initpoudrecafe: Int = 50
    val initsucre: Int = 30
    val intitlait: Double = 0.5

    // Diminution d'ingrédients
    var dimcafe: Int = 0 // au départ à 0
    var dim_sucre: Int = 0
    var dimlait: Double = 0

    // Sélection du mode
    while (mode != 3)
    {
      mode = readLine("Veuillez sélectionner votre mode:\n1 si vous êtes un client, \n2 si vous êtes admin,  \n3 si vous voulez quitter. \n> ").toInt
      if (mode < 1 || mode > 3)
      {
        println("Vous n'avez pas sélectionné un mode valide.")
      }

      // Mode client
      else if (mode == 1)
      {
        // Nouvelle boucle pour revenir à la sélection des boissons
        var commandepossible: Boolean = false

        while (commandepossible == false)
        {
          // Réinitialisation des variables pour chaque nouvelle commande
          var cafe: Int = 0
          var sucre: Int = 0
          var lait: Int = 0
          var ouiounon: Int = 0
          var boisson: String = ""
          var niveausucre: String = ""
          var niveaulait: String = ""
          var prixtotal: String = ""
          var somme: Double = 0
          augmentationprixsucre = 0
          dimcafe = 0
          dim_sucre = 0
          dimlait = 0

          // Type de café
          while (cafe < 1 || cafe > 3)
          {
            cafe = readLine("Quelle boisson désirez-vous : \n" +
              "1) Expresso - CHF 2.00  " +
              "\n2) Cappuccino - CHF 2.50 " +
              "\n3) Latte  - CHF 2.70 (Petit), CHF 3.20 (moyen), CHF 3.70 (grand) \n > ").toInt

            if (cafe == 1)
            {
              boisson = "Expresso"
              somme = 2.00
              dimcafe = 8
            }
            else if (cafe == 2)
            {
              boisson = "Cappuccino"
              somme = 2.50
              dimcafe = 6
              dimlait = 0.1
            }
            else if (cafe == 3)
            {
              boisson = "Latte"
              var taillelatte: Int = 0
              while (taillelatte < 1 || taillelatte > 3)
              {
                taillelatte = readLine("Quelle taille de Latte désirez-vous ? " +
                  "\n1) Petit " +
                  "\n2) Moyen " +
                  "\n3) Grand " +
                  "\n > ").toInt

                if (taillelatte == 1)
                {
                  somme = 2.70
                  dimcafe = 6
                  dimlait = 0.12
                }
                else if (taillelatte == 2)
                {
                  somme = 3.20
                  dimcafe = 8
                  dimlait = 0.15
                }
                else if (taillelatte == 3)
                {
                  somme = 3.70
                  dimcafe = 12
                  dimlait = 0.2
                }
                else
                {
                  println("Veuillez sélectionner un nombre valide ")
                }
              }
            }
            else
            {
              println("Veuillez sélectionner un nombre valide")
            }
          }

          poudrecafe -= dimcafe
          stocklait -= dimlait
          prixtotal = "CHF " + somme

          // Sucre
          while (sucre < 1 || sucre > 4)
          {
            sucre = readLine("Voulez-vous du sucre dans votre café ?" +
              "\n1) Sans sucre" +
              "\n2) Peu (5g) - CHF 0.10" +
              "\n3) Moyen (10g) - CHF 0.20" +
              "\n4) Beaucoup (15g) - CHF 0.30" +
              "\n> ").toInt

            if (sucre == 1)
            {
              niveausucre = "Pas de sucre"
            }
            else if (sucre == 2)
            {
              augmentationprixsucre = 0.10
              somme += augmentationprixsucre
              niveausucre = "Peu (5g)"
              dim_sucre = 5
            }
            else if (sucre == 3)
            {
              augmentationprixsucre = 0.20
              somme += augmentationprixsucre
              niveausucre = "Moyen (10g)"
              dim_sucre = 10
            }
            else if (sucre == 4)
            {
              augmentationprixsucre = 0.30
              somme += augmentationprixsucre
              niveausucre = "Beaucoup (15g)"
              dim_sucre = 15
            }
            else
            {
              println("Veuillez sélectionner un nombre valide")
            }
          }

          stocksucre -= dim_sucre
          prixtotal += "+ CHF " + augmentationprixsucre + " = CHF " + somme

          // Lait, si applicable
          if (cafe == 2 || cafe == 3)
          {
            while (ouiounon < 1 || ouiounon > 2)
            {
              ouiounon = readLine("Souhaitez-vous ajouter du lait en supplément ?" +
                "\n1) Oui" +
                "\n2) Non" +
                "\n> ").toInt
              if (ouiounon == 1)
              {
                niveaulait = "Oui"
                var nbdose: Double = 0
                while (nbdose < 1 || nbdose > 3)
                {
                  nbdose = readLine("Combien de dose de lait désirez-vous ? ").toInt

                  if (nbdose < 1 || nbdose > 3)
                  {
                    println("Vous pouvez sélectionner maximum 3 doses.")
                  }

                }
                stocklait -= nbdose * 0.05

              }
              else
              {
                niveaulait = "Non"
              }

              if (ouiounon < 1 || ouiounon > 2)
              {
                println("Veuillez sélectionner un nombre valide")
              }
            }
          }

          // Affichage de la commande et vérification des stocks
          if (poudrecafe >= dimcafe && stocklait >= dimlait && stocksucre >= dim_sucre)
          {
            println("Boisson sélectionnée : " + boisson)
            println("Niveau de sucre : " + niveausucre)
            println("Lait supplémentaire : " + niveaulait)
            println("Prix total : " + prixtotal)
            println("\nVeuillez payer en utilisant Twint")
            println("Votre code de paiement est :" + coderandom)
            println("En attente de paiement...")
            Thread.sleep(2000)
            println(" ")

            println("Paiement confirmé. ")
            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre " + boisson + " est prêt ! Bonne dégustation ! ")
            println(" ")
            commandepossible = true // Les stocks sont suffisant, on peut faire la commande
          }
          else
          {
            if (cafe == 3)
            {
              if (poudrecafe < dimcafe) println("Erreur : Quantité de café insuffisante pour la boisson sélectionnée.")
              if (stocklait < dimlait) println("Erreur : Quantité de lait insuffisante pour la boisson sélectionnée.")
              if (stocksucre < dim_sucre) println("Erreur : Quantité de sucre insuffisante pour la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
            else
            {
              if (poudrecafe < dimcafe) println("Erreur : Quantité de café insuffisante pour la boisson sélectionnée.")
              if (stocklait < dimlait) println("Erreur : Quantité de lait insuffisante pour la boisson sélectionnée.")
              if (stocksucre < dim_sucre) println("Erreur : Quantité de sucre insuffisante pour la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          }
        }
      }

      // Mode Admin
      else if (mode == 2)
      {
        println("Mode Admin")
        val code: Int = 434343
        var entreecode: Int = 0
        while (entreecode != code)
        {
          println("Entrez votre code PIN : ")
          entreecode = readInt()
          if (entreecode != code)
          {println("Accès refusé.")}
        }
        println("Accès autorisé.\n")
        println("Stocks : ")
        println("Poudre de café: " + poudrecafe)
        printf("Lait : %.2f \n" , stocklait)
        println("Sucre: " + stocksucre)

        val reaprovisionnement = readLine("Souhaitez-vous réapprovisionner le stock ? (true/false)").toBoolean
        if (reaprovisionnement == true)
        {

          poudrecafe = readLine("Combien de grammes de poudre de café voulez vous rajouter ? ").toInt
          stocklait = readLine("Combien de litres de lait voulez vous rajouter ? ").toDouble
          stocksucre = readLine("Combien de grammes de sucre voulez vous rajouter ? ").toInt
          println("Réapprovisionnement des stocks...")
          println("Ajout : ")
          println("Poudre de café : " + poudrecafe)
          println("Lait : " + stocklait)
          println("Sucre : " + stocksucre)
          println ("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")

        }
      }
    }
    println("Merci d'avoir utilisé la machine à café")
  }
}


