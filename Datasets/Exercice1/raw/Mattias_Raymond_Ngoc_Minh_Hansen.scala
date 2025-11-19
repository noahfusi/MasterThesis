object Main {
  import scala.io.StdIn._
  import scala.util._
  def main(args: Array[String]): Unit = {
    //======================= INITIALISATION ========================//

    //AFFICHAGES
    var affichageInitial: String ="        Nospresso Café \nVeuillez Sélectionner votre mode: \n1) Client\n2) Admin\n3) Quitter"
    var affichageInput: String = "> " //AFFICHAGE LORSQUE L'ON ATTEND UNE REPONSE UTILISATEUR
    var affichageSelectBoisson: String ="Veuillez sélectionner votre boisson:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"
    var affichageTailleLatte: String ="Veuillez sélectionner la taille de votre Latte:\n1) Latte (Petit) - CHF 2.70\n2) Latte (Moyen) - CHF 3.20\n3) Latte (Grand) - CHF 3.70"
    var affichageSelectSucre: String ="Souhaitez-vous ajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30"
    var affichageSelectLait: String ="Souhaitez-vous ajouter du lait en supplément?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non"
    var affichageSelectDoses: String ="Combien de dose(s)?\n"
    var affichageErreur: String ="Erreur! Veuillez saisir une entrée valide!"
    var affichagePrixBoisson: String = "placeholder"
    var affichagePrixSuppSucre: String = "placeholder"
    var affichagePrixSuppLait: String = "placeholder"

    //VARIABLES
    var choixBoisson: Int = 0
    var choixTailleLatte: Int = 0
    var choixSucre: Int = 0
    var choixLait: Int = 0
    var dosesLait: Int = 0
    var prixBoissonFinale: Double = 0.0
    var codePaiementTwint: String ="XXXXX"

    val codePIN: Int = 434343
    var tentativeCodePIN: Int = 0

    //PRIX
    val pEspresso: Double = 2.00
    val pCappuccino: Double = 2.50
    val pLattePetit: Double = 2.70
    val pLatteMoyen: Double = 3.20
    val pLatteGrand: Double = 3.70
    val pDoseLait: Double = 0.05

    //QUANTITES COMMANDEES
    var qCommandePoudreCafe: Int = 0
    var qCommandeSucre: Int = 0
    var qCommandeLait: Int = 0

    //STOCKS INITIAUX
    var stockPoudreCafe: Int = 50   //Unité: g
    var stockSucre: Int = 30        //Unité: g
    var stockLait: Int = 500        //Unité: ml
    //"Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre"


    var mode: Int = 0 //la variable "mode" sera la variable qui définit quel mode de la machine tourne, ainsi que quand revenir à l'état initial
    //Mode Client : Permet à l’utilisateur-trice de commander une boisson
    //Mode Admin : Permet à l’administrateur-trice de réapprovisionner les stocks de la machine.
    //Quitter: Revient à l'affichage initial

    //======================= SELECTION DU MODE =======================//

    while (mode == 0){
      println(affichageInitial)
      print(affichageInput)
      mode = readInt()
      while (mode > 3 || mode < 1){
        println(affichageErreur)
        println(affichageInitial)
        print(affichageInput)
        mode = readInt()
      }

      while (mode < 3 || mode > 1){
        //======================= MODE CLIENT =======================//
        prixBoissonFinale = 0

        if (mode == 1){
          println("Option 1: Mode Client sélectionné")

          //======================= CHOIX DE LA BOISSON =======================//
          println(affichageSelectBoisson)
          print(affichageInput)
          choixBoisson = readInt()
          while (choixBoisson < 1 || choixBoisson > 3){
            println(affichageErreur)
            println(affichageSelectBoisson)
            print(affichageInput)

            choixBoisson = readInt()
          }

          if (choixBoisson==1){
            print("CHF +2.00 Option 1: Espresso sélectionnée")
            prixBoissonFinale += 2.0
            qCommandePoudreCafe += 8
            affichagePrixBoisson = "CHF 2.00"
          }
          else if (choixBoisson==2){
            print("CHF +2.50 Option 2: Cappuccino sélectionnée")
            prixBoissonFinale += 2.5
            qCommandePoudreCafe += 6
            qCommandeLait += 100
            affichagePrixBoisson = "CHF 2.50"
          }
          else {
            println(affichageTailleLatte)
            print(affichageInput)
            choixTailleLatte = readInt()
            while (choixTailleLatte < 1 || choixTailleLatte > 3){
              println(affichageErreur)
              println(affichageTailleLatte)
              print(affichageInput)
              choixTailleLatte = readInt()
            }

            if (choixTailleLatte == 1){
              println("CHF +2.70 Option 1: Latte (Petit) sélectionnée")
              prixBoissonFinale += 2.7
              qCommandePoudreCafe += 6
              qCommandeLait += 120
              affichagePrixBoisson = "CHF 2.70"
            }
            else if (choixTailleLatte == 2){
              println("CHF +3.20 Option 2: Latte (Moyen) sélectionnée")
              prixBoissonFinale += 3.2
              qCommandePoudreCafe += 8
              qCommandeLait += 150
              affichagePrixBoisson = "CHF 3.20"
            }
            else{
              println("CHF +3.70 Option 3: Latte (Grand) sélectionnée")
              prixBoissonFinale += 3.7
              qCommandePoudreCafe += 12
              qCommandeLait += 200
              affichagePrixBoisson = "CHF 3.70"
            }
          }
          //println("Prix actuel de votre boisson: " + prixBoissonFinale)

          //======================= AJOUT DU SUCRE =======================//
          println(affichageSelectSucre)
          print(affichageInput)
          choixSucre = readInt()
          while (choixSucre < 1 || choixSucre > 4){
            println(affichageErreur)
            println(affichageSelectSucre)
            print(affichageInput)

            choixSucre = readInt()
          }

          if (choixSucre == 1){
            println("CHF +0.00 Option 1: Sans Sucre sélectionnée")
            affichagePrixSuppSucre = ""
          }
          else if (choixSucre == 2){
            println("CHF +0.10 Option 2: Peu (5g) sélectionnée")
            prixBoissonFinale += 0.1
            qCommandeSucre += 5
            affichagePrixSuppSucre = " + CHF 0.10"
          }
          else if (choixSucre == 3){
            println("CHF +0.20 Option 3: Moyen (10g) sélectionnée")
            prixBoissonFinale += 0.2
            qCommandeSucre += 10
            affichagePrixSuppSucre = " + CHF 0.20"
          }
          else {
            println("CHF +0.30 Option 3: Beaucoup (15g) sélectionnée")
            prixBoissonFinale += 0.3
            qCommandeSucre += 15
            affichagePrixSuppSucre = " + CHF 0.30"
          }
          //println("Prix actuel de votre boisson: " + prixBoissonFinale)

          //======================= AJOUT DU LAIT =======================//
          if (choixBoisson != 1) {
            println(affichageSelectLait)
            print(affichageInput)
            choixLait = readInt()
            while (choixLait < 1 || choixLait > 2) {
              println(affichageErreur)
              println(affichageSelectLait)
              print(affichageInput)

              choixLait = readInt()
            }

            if (choixLait == 1){
              println("Combien de doses?")
              print(affichageInput)
              dosesLait = readInt()

              while (dosesLait < 1 || dosesLait > 3){
                println(affichageErreur)
                println(affichageSelectDoses)
                print(affichageInput)

                dosesLait = readInt()
              }
              println("CHF +" + (dosesLait*pDoseLait).toString.take(4) + ", " + dosesLait + " doses de lait commandées.")
              qCommandeLait += dosesLait*50
              prixBoissonFinale += dosesLait*pDoseLait
              affichagePrixSuppLait = " + CHF " + (((dosesLait*pDoseLait).toString) + "0").take(4)
              //println(affichagePrixSuppLait)
              //println("Prix actuel de votre boisson: " + prixBoissonFinale)
            }
          }
          else {
            affichagePrixSuppLait = ""
          }
          //========================== VERIFICATION STOCKS ========================//
          //println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
          //println("Stock nécessaire: \n" + qCommandePoudreCafe +"g de poudre de café \n" + qCommandeLait + "ml de lait \n" + qCommandeSucre + "g de sucre")

          if (qCommandeLait > stockLait || qCommandeSucre > stockSucre || qCommandePoudreCafe > stockPoudreCafe){
            println("Erreur de stock.")

            if (qCommandeLait > stockLait){
              println("Il n'y a plus assez de Lait en stock pour satisfaire cette commande!")
            }
            else if (qCommandeSucre > stockSucre) {
              println("Il n'y a plus assez de Sucre en stock pour satisfaire cette commande!")
            }
            else if (qCommandePoudreCafe > stockPoudreCafe){
              println("Il n'y a plus assez de Poudre de Café en stock pour satisfaire cette commande!")
            }
          }
          else {
            println("Prix total: " + affichagePrixBoisson + affichagePrixSuppSucre + affichagePrixSuppLait + " = CHF " + (prixBoissonFinale.toFloat.toString + "0").take(4))

            codePaiementTwint = Random.alphanumeric.take(5).mkString.toUpperCase()
            println("Veuillez payer en utilisant Twint.\nVotre code de paiement est: " + codePaiementTwint + "\n(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci! Votre paiement a été accepté.")
            stockPoudreCafe -= qCommandePoudreCafe
            stockLait -= qCommandeLait
            stockSucre -= qCommandeSucre
            println("Préparation de votre boisson...")
            Thread.sleep(1000)
            println("[...]")
            Thread.sleep(4000)
            println("Votre boisson est prête! Bonne dégustation!")
          }
          //println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
          mode = 3
        }


        //======================= MODE ADMIN =======================//
        else if (mode == 2){
          println("Option 2: Mode Admin sélectionné\nEntrez le code PIN: ")
          tentativeCodePIN = readInt()
          while (tentativeCodePIN != codePIN){
            println("Ce code PIN n'est pas valable\nEntrez le code PIN: ")
            tentativeCodePIN = readInt()
          }
          println("Accès Autorisé.")
          println("\n")
          println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
          print("Réapprovisionnement des stocks...\nAjout:\nPoudre de café: ")
          stockPoudreCafe += readInt()

          print("Lait: ")
          stockLait += readInt()

          print("Sucre: ")
          stockSucre += readInt()

          println("Niveau de stock mis à jour.")
          println("Retour au menu principal...")
          println(affichageInitial)
          print(affichageInput)

          mode = readInt()
        }
        else if (mode == 3){
          println("Bonne Journée!")
          qCommandeLait = 0
          qCommandePoudreCafe = 0
          qCommandeSucre = 0
          Thread.sleep(5000)
          println(affichageInitial)
          print(affichageInput)
          mode = readInt()
        }
        else {
          println(affichageErreur)
          println(affichageInitial)
          println(affichageInput)
          mode = readInt()
        }
      }
    }
  }
}