import scala.util.Random
import scala.io.StdIn._


object Main {
  def main(args: Array[String]): Unit = {


    //------variables principales------------------------------------------------------
    var mode = 0
    val client = 1
    val admin = 2
    val quitter = 3
    val nbMachines = 5
    var machineId = 0
    var BonPIN = false
    var choixadmin = 0
    val codealpha = Random.alphanumeric.take(5).mkString.toUpperCase()



    //------Variables d'affichage--------------------------------
    val pagedintro = "\n              Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> "
    val choisirmachine = "\nMachine sélectionnée (1-" + nbMachines + ") > "
    val pageselecafe = "\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> "
    val choixtaillelatte = "Veuillez choisir la taille du Latte \n 1) Petit \n 2) Moyen \n 3) Grand \n > "
    val choixsucre = "Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> "
    val choixlaitsupp = "Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> "
    val erreurselectioninvalide = "La séléction n'est pas valable.\nVeuillez entrer un choix valable: "


    //–-----tableaux des stocks et PIN----------–------------------------------------------------------------------------------------------------
    val stockscafe = Array.fill(nbMachines)(50)
    val stocksucre = Array.fill(nbMachines)(30)
    val stocklait = Array.fill(nbMachines)(500)
    val machinePins = Array.fill(nbMachines)("434343")


    //-------Fonctions-------------------------------------------------------------------
    def serveClient(machineId: Int, stockscafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Boolean = {
      //====================MODE CLIENT================================================================================================
      //----------Variables selection de café-------------------------------------------------------------------
      var prixcafe = 0.0
      var choixcafe = 0
      var cafesuffisant = true
      var qteexpresso = 8
      var qtecappuccino = 6
      var qtelaitcappuccino = 100
      var sucreutilise = 0
      var sucresuffisant = true
      var tailleLatte = 0
      var qtepetitlatte = 6
      var qtelaitpetitlatte = 120
      var qtemoyenlatte = 8
      var qtelaitmoyenlatte = 150
      var qtegrandlatte = 12
      var qtelaitgrandlatte = 200
      var laitsupp = 0
      var laitsuffisant = true
      var doseLaitSupp = 0
      var validationPIN = false //nous permet de valider le Pin

      //Les "if" permettent la gestion d'erreurs d'inputs
      do {
        println(pageselecafe)
        choixcafe = readInt()
        if (choixcafe != 1 && choixcafe != 2 && choixcafe != 3) {//Choix du type de café
          println(erreurselectioninvalide)
        }
      } while (choixcafe != 1 && choixcafe != 2 && choixcafe != 3)

      if (choixcafe == 3) {//choix de la taille du latte
        do {
          println(choixtaillelatte)
          tailleLatte = readInt()
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println(erreurselectioninvalide)
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
      }

      if (choixcafe == 1 || choixcafe == 2 || choixcafe == 3) {//choix de la quantité de sucre
        do {
          println(choixsucre)
          sucreutilise = readInt()
          if (sucreutilise != 1 && sucreutilise != 2 && sucreutilise != 3 && sucreutilise != 4)
          println(erreurselectioninvalide)
        } while (sucreutilise != 1 && sucreutilise != 2 && sucreutilise != 3 && sucreutilise != 4)
      }
      if (choixcafe == 2 || choixcafe == 3) {//choix si doses de lait supplémentaires
        do {
          println(choixlaitsupp)
          laitsupp = readInt()
          if (laitsupp != 1 && laitsupp != 2) {
            println(erreurselectioninvalide)
          }
        } while (laitsupp != 1 && laitsupp != 2)
      }

      if (laitsupp == 1) {//choix doses de lait supplémentaire
        println("Combien de doses? \n> ")
        doseLaitSupp = readInt()
        while (doseLaitSupp < 1 || doseLaitSupp > 3) {
          println(erreurselectioninvalide)
          doseLaitSupp = readInt()
        }
      }



      //--------Suffisance des stocks--------------------------------------------------------------------------------------------------------------

      if (choixcafe == 1) {//Pour expresso
        cafesuffisant = suffisanceStock(stockscafe(machineId), qteexpresso, "café")
        sucresuffisant = suffisanceStock(stocksucre(machineId), (sucreutilise - 1) * 5, "sucre")

        }else if (choixcafe == 2) {//Pour cappuccino
          cafesuffisant = suffisanceStock(stockscafe(machineId), qtecappuccino, "café")
          laitsuffisant = suffisanceStock(stocklait(machineId), qtelaitcappuccino + (doseLaitSupp * 50), "lait")
          sucresuffisant = suffisanceStock(stocksucre(machineId), (sucreutilise - 1) * 5, "sucre")

              }else if (choixcafe == 3) { //Pour les lattes
                    if (tailleLatte == 1) { //Petit latte
                      cafesuffisant = suffisanceStock(stockscafe(machineId), qtepetitlatte, "café")
                      laitsuffisant = suffisanceStock(stocklait(machineId), qtelaitpetitlatte + (doseLaitSupp * 50), "lait")
                      sucresuffisant = suffisanceStock(stocksucre(machineId), (sucreutilise - 1) * 5, "sucre")

                    }else if (tailleLatte == 2) { //Moyen latte
                            cafesuffisant = suffisanceStock(stockscafe(machineId), qtemoyenlatte, "café")
                            laitsuffisant = suffisanceStock(stocklait(machineId), qtelaitmoyenlatte + (doseLaitSupp * 50), "lait")
                            sucresuffisant = suffisanceStock(stocksucre(machineId), (sucreutilise - 1) * 5, "sucre")

                        }else if (tailleLatte == 3) { //Grand latte
                                cafesuffisant = suffisanceStock(stockscafe(machineId), qtegrandlatte, "café")
                                laitsuffisant = suffisanceStock(stocklait(machineId), qtelaitgrandlatte + (doseLaitSupp * 50), "lait")
                                sucresuffisant = suffisanceStock(stocksucre(machineId), (sucreutilise - 1) * 5, "sucre")}
              }

      //-------Calcul des prix-------------------------------------------------------------------------------------------------------------------
      if (cafesuffisant && laitsuffisant && sucresuffisant) {// si et seulement si les stocks sont suffisants, on peut passer au paiement

        if (choixcafe == 1) {//Prix espresso
          prixcafe = 2.00
        }else if (choixcafe == 2) {//Prix cappuccino
          prixcafe = 2.50
        }else if (tailleLatte == 1) {//Prix petit latte
          prixcafe = 2.70
        }else if (tailleLatte == 2) {//Prix moyen latte
          prixcafe = 3.20
        }else if (tailleLatte == 3) {//Prix grand latte
          prixcafe = 3.70
        }

        val prixlait = doseLaitSupp * 0.05
        val prixsucre = (sucreutilise - 1) * 0.10
        val Total = prixcafe + prixsucre + prixlait

        println("Prix total: " +  prixcafe + " CHF " + " + " + prixsucre + " CHF " + " + " + prixlait + " CHF = " + Total +" CHF.")
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + codealpha + ". \nEn attente...")
        Thread.sleep(2000)
        println("Paiement accepté. \nMerci pour votre achat! ")

        //-----Mis à jour des stocks-------------------------------------------------------------------------------------------------------------
        if (choixcafe == 1) {//déduction de 8 grammes de café pour un espresso
          stockscafe(machineId) -= qteexpresso
        }
        if (choixcafe == 2) {//déduction de 6 grammes de café pour un cappuccino et 100mL de lait
          stockscafe(machineId) -= qtecappuccino
          stocklait(machineId) -= qtelaitcappuccino
        }
        if (choixcafe == 3 && tailleLatte == 1) {//déduction des quantitées pour petit latte
          stockscafe(machineId) -= qtepetitlatte
          stocklait(machineId) -= qtelaitpetitlatte
        }
        if (choixcafe == 3 && tailleLatte == 2) {//déduction des quantitées pour moyen latte
          stockscafe(machineId) -= qtemoyenlatte
          stocklait(machineId) -= qtelaitmoyenlatte
        }
        if (choixcafe == 3 && tailleLatte == 3) {//déduction des quantitées pour grand latte
          stockscafe(machineId) -= qtegrandlatte
          stocklait(machineId) -= qtelaitgrandlatte
        }
        stocksucre(machineId) -= (sucreutilise - 1) * 5 //déduction des quantitées de sucre utiliser avec machineID puisque c'est une option disponible pour n'importe quel choix de café.
        stocklait(machineId) -= (doseLaitSupp * 50)//Même logique pour les doses de lait supplémentaire car, sachant que nous avons mis la contrainte que cette option n'est que pour choix café 2 et 3.

//------------------------------préparation de la boisson après validation du paiement---------------------------------------------------------------------------
        println("Paiement Accepté.\nPréparation de votre boisson...")
        Thread.sleep(3000)
        validationPIN = true //Si le paiement est effectué c'est qu'il y a assez de stocks
        if (choixcafe == 1) {
          println("Votre Expresso est prêt! Bonne dégustation!")
        }
        else if (choixcafe == 2) {
          println("Votre Cappuccino est prêt! Bonne dégustation!")
        }
        else if (choixcafe == 3) {
          println("Votre Latte est prêt! Bonne dégustation!")
        }
      }
    validationPIN
    }
    //====================MODE ADMIN================================================================================================
    def serveAdmin(machinePins: Array[String], stockscafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Unit = {
      do {
        println("1)Restock\n2)Changement du PIN\n> ")
        choixadmin = readInt()
      } while (choixadmin != 1 && choixadmin != 2)

      machineId = numeroMachine()
      if (!validatePin(machineId, machinePins)) {
        println("Nombre d'essais dépassées.\nProgramme terminé.")
        return
      }

      if (choixadmin == 1) {
        restockMachine(machineId, stockscafe, stocksucre, stocklait)
      } else if (choixadmin == 2) {
        updatePin(machineId, machinePins)
      }
    }
    //--------Choix de la machine que le client ou admin veut utiliser-----------------------------------------------------------

    def numeroMachine(): Int = {
      do {
        print(choisirmachine)
        machineId = readInt()
      } while (machineId < 1 || machineId > 5)
      machineId - 1
    }

    def suffisanceStock(qteingredients: Int, qteStock: Int, typedeStock: String): Boolean = {
      if (qteStock < qteingredients) {
        true
      } else {
        println("Erreur : Quantité insuffisante de " + typedeStock + ".\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        false
      }
    }


    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      val CodePIN = machinePins(machineId)
      var EssaisPIN = 3
      do {
        val EssaiPINutilisateur = readLine("Entrez le PIN: ")
        EssaisPIN -= 1
        if (CodePIN == EssaiPINutilisateur) {
          BonPIN = true
          println("Accès à la Machine " + machineId + ". ")
        } else if (EssaisPIN == 1) {
          println("PIN incorrect. " + EssaisPIN + " essai restant. ")
        } else {
          println("PIN incorrect. " + EssaisPIN + " essais restants. ")
        }
      } while (EssaisPIN > 0 && !BonPIN)

      BonPIN
    }



    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du PIN de la Machine " + (machineId + 1) + ". ")
      var NouveauPIN = 0.toString
      do {
        print("Entrez un nouveau PIN à 6 chiffres \n> ")
        NouveauPIN = readLine()
      } while (NouveauPIN.length != 6)

      machinePins(machineId) = NouveauPIN
      println("Le PIN est mis à jour.\nRetour au menu principal...")
    }

    def restockMachine(machineId: Int, stockscafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Unit = {

      println("Niveaux de stock actuels:\ncafé: " + stockscafe(machineId) + "\nLait : " + stocklait(machineId) + "\nSucre : " + stocksucre(machineId))

      println("Entrez les quantités à ajouter:\ncafé: ")
      stockscafe(machineId) += readInt()
      print("Lait: ")
      stocklait(machineId) += readInt()
      print("Sucre: ")
      stocksucre(machineId) += readInt()

      println("Les stocks sont à jour!\nRetour au menu principal...")
      Thread.sleep(2000)
    }
    //------------------------------------------------------------------------------------------------------------------------------------

    //Ce qui se passe lors de l'utilisation d'une machine-----------------------------------------------------------------------------------------------------------------------
//ca nous a permis une optimisation du code en gardant la structure initiale.
    do {//Accueil
      println(pagedintro)
      mode = readInt()

      if (mode != client && mode != admin && mode != quitter) {//gestion erreur choix mode
        println("Veuillez selectionner un mode valide")
      }
      if (mode == client) {
        machineId = numeroMachine()
        serveClient(machineId, stockscafe, stocksucre, stocklait) //si mode = 1, la fonction serveClient va s'executer.
      }
      if (mode == admin) {
        serveAdmin( machinePins, stockscafe, stocksucre, stocklait)//si mode = 2, la fonction serveAdmin va s'executer
      }
    } while (mode != quitter)//le programme tourne tant que mode n'est pas "quitter".

  }
}