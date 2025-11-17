import io.StdIn._
import math._

object Main {
  //déclaration des méthodes

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      print("Entrez le code PIN: ")
      val proposition = readInt().toString
      if (proposition == machinePins(machineId)) {
        println("Accès accordé à la Machine " + (machineId + 1))
        return true
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
        } else {
          println("Trop de tentatives échouées. Fin du programme.") //quittez le programme
          System.exit(0)
        }
      }
    }
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    print("Entrez un nouveau code PIN à 6 chiffres: ")
    val mdp = readInt().toString
    //mdp est égal à 6 chiffres
    if (mdp.toString.length == 6 && mdp.forall(_.isDigit)) {
      machinePins(machineId) = mdp.toString
      println("Le code PIN a été mis à jour.")
    } else {
      println("Le code PIN n’a pas été mis à jour.")
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    //prix
    //val prixespresso = 2.00
    //val prixcappucino = 2.50
    //val prixlattepetit = 2.70
    //val prixlattemoyen = 3.20
    //val prixlattegrand = 3.80
    //val prixpassucre = 0.00
    //val prixpeusucre = 0.10
    //val prixmoyensucre = 0.20
    //val prixbcpsucre = 0.30
    //val prixlait = 0.05
    //consommation des ingrédients //changer en int
    //val consommationespresso = 8
    //val consommationcappucino = 6
    //val consommationcappucinolait = 100
    //val consommationlattepetit = 6
    //val consommationlattepetitlait = 120
    //val consommationlattemoyen = 8
    //val consommationlattemoyenlait = 150
    //val consommationlattegrand = 12
    //val consommationlattegrandlait = 200
    //val consommationsucrepeu = 5
    //val consommationsucremoyen = 10
    //val consommationsucrebcp = 15

    //initialisation des variables
    var consommationcafe = 0
    var consommationlait = 0
    var consommationsucre = 0

    //calcul total
    var prixboisson = 0.00
    var boisson = " "
    var prixlait = 0.00

    //choix utlisateur entre Espresso, Cappucino, Latte
    println("Veuillez sélectionner votre boisson :")
    println("1) Espresso - CHF 2.00")
    println("2) Cappucino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var choixboisson = readInt()
    while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3)) {
      println("Votre sélection n'est pas correcte.")
      println("Choississez entre :")
      println("1) Espresso - CHF 2.00")
      println("2) Cappucino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      choixboisson = readInt()
    }

    //choix espresso 1 //ajouter déducation des stocks
    if (choixboisson == 1) {
      boisson = "Nespresso"
      prixboisson = 2.0
      consommationcafe = 8
      //choix cappucino 2
    } else if (choixboisson == 2) {
      boisson = "Cappucino"
      prixboisson = 2.50
      consommationcafe = 6
      consommationlait = 100
    } else {
      //choix latte 3
      println("Veuillez sélectionner la taille de votre latte :")
      println("1) Petit Latte - CHF 2.70")
      println("2) Moyen Latte - CHF 3.20")
      println("3) Grand Latte - CHF 3.70")
      var choixlatte = readInt()
      while (!(choixlatte == 1 || choixlatte == 2 || choixlatte == 3)) {
        println("Votre sélection n'est pas correcte.")
        println("Choississez entre :")
        println("1) Petit Latte - CHF 2.70")
        println("2) Moyen Latte - CHF 3.20")
        println("3) Grand Latte - CHF 3.70")
        choixlatte = readInt()
      }
      if (choixlatte == 1) {
        boisson = "Latte (Petit)"
        prixboisson = 2.70
        consommationcafe = 6
        consommationlait = 120
      } else if (choixlatte == 2) {
        boisson = "Latte (Moyen)"
        prixboisson = 3.20
        consommationcafe = 8
        consommationlait = 150
      } else {
        boisson = "Latte (Grand)"
        prixboisson = 3.70
        consommationcafe = 12
        consommationlait = 200
      }
    }
    //choix de sucre : Pas de sucre, Peu de sucre (5g), Moyen (10g), Beaucoup (15g)
    var prixsucre = 0.00
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    var choixsucre = readInt()
    var sucre = " "
    while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4)) {
      println("Votre sélection n'est pas correcte.")
      println("Choississez entre :")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      choixsucre = readInt()
    }
    if (choixsucre == 1) {
      sucre = "Sans sucre"
      prixsucre = 0.0
    } else if (choixsucre == 2) {
      sucre = "Peu (5g)"
      prixsucre = 0.10
      consommationsucre = 5
    } else if (choixsucre == 3) {
      sucre = "Moyen (10g)"
      prixsucre = 0.20
      consommationsucre = 10
    } else {
      sucre = "Beaucoup (15g)"
      prixsucre = 0.30
      consommationsucre = 15
    }

    //choix lait (cappucino ou latte seulement)
    //si cappucino ou latte (if else) : ajouter 50ml de lait (3 doses max/boisson)
    //Si oui (sinon l'instruction n'est pas lu)
    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("(Disponible uniquement pour Cappucino et Latte)")
    println("1) Oui")
    println("2) Non")
    var choixlait = readInt()
    var choixlaitsupplement = " "
    while (!(choixlait == 1 || choixlait == 2)) {
      println("Votre sélection n'est pas correcte.")
      println("Choississez entre :")
      println("1) Oui")
      println("2) Non")
      choixlait = readInt()
    }
    if (choixlait == 1) {
      choixlaitsupplement = "Oui"
      print("Combien de dose ?")
      var choixlaitdose = readInt()
      while (!(choixlaitdose >= 1) && (choixlaitdose <= 3)) {
        println("Votre sélection n'est pas correcte.")
        println("Vous avez le droit à 3 doses maximum par boisson")
        choixlaitdose = readInt()
      }
      milkStocks(machineId) -= choixlaitdose * 50
      prixlait = choixlaitdose * 0.5
    } else {
      choixlaitsupplement = "Non"
    }

    //gestion des stocks
    if ((coffeeStocks(machineId) < consommationcafe)) {
      println("Erreur : Quantité de café insuffisant pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou sélectionner une autre machine.")
      return false
    } else if (sugarStocks(machineId) < consommationsucre) {
      println("Erreur : Quantité de sucre insuffisant pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou sélectionner une autre machine.")
      return false
    } else if (milkStocks(machineId) < consommationlait) {
      println("Erreur : Quantité de lait insuffisant pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou sélectionner une autre machine.")
      return false
    } else {
      //déduction des stocks
      coffeeStocks(machineId) -= consommationcafe
      sugarStocks(machineId) -= consommationsucre
      milkStocks(machineId) -= consommationlait
      //Paiement :
      //afficher prix total
      val prixtotal = prixboisson + prixsucre + prixlait
      println("Boisson sélectionnée: " + boisson)
      println("Niveau de sucre: " + sucre)
      println("Lait en supplément: " + choixlaitsupplement) //(faire afficher : supplément lait OUI ou NON)
      printf("Prix total: CHF %.2f\n", prixtotal)
      //demander code TWINT -> code aléatoire (math.random) //code alphanumérique pas trouvé dans la library scala ??
      var twint = " "
      for (i <- 1 to 5) {
        twint += (math.random() * 10).toInt
      }
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est: " + twint)
      println("(En attente de validation du paiement...)")
      Thread.sleep(5000)
      println("Merci votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      println("[...]")
      println("Votre " + boisson + " est prêt ! Bonne dégustation !")
    }
    return true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Stocks : ")
    println("Poudre de café: " + coffeeStocks(machineId) + "g")
    println("Lait: " + milkStocks(machineId) + "mL") // /1000 + "L" comment faire passer du double ??
    println("Sucre: " + sugarStocks(machineId) + "g")
    println("Réaprovisionnement des stocks...")
    print("1) Poudre de café (en g) : ")
    var ajoutcafe = readInt()
    while (ajoutcafe < 0) {
      println("Votre saisie est invalide.")
      println("Veuillez réessayer.")
      ajoutcafe = readInt()
    }
    coffeeStocks(machineId) += ajoutcafe
    print("2) Lait (en L) : ")
    var ajoutlait = readDouble()
    while (ajoutlait < 0) {
      println("Votre saisie est invalide.")
      println("Veuillez réessayer.")
      ajoutlait = readInt()
    }
    milkStocks(machineId) += (ajoutlait * 1000).toInt
    print("3) Sucre (en g) : ")
    var ajoutsucre = readInt()
    while (ajoutsucre < 0) {
      println("Votre saisie est invalide.")
      println("Veuillez réessayer.")
      ajoutsucre = readInt()
    }
    sugarStocks(machineId) += ajoutsucre
    println("Niveaux de stock mis à jour.")
  }

  def main(args: Array[String]): Unit = {
    //déclaration variables: vérifier que tout est là
    //numero machine
    val nbMachines = 5
    val machinePins = Array.fill(nbMachines)("434343")
    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)

    //menu de sélection identifiant de machine
    //menu de sélection mode
    //boucle do while pour continuer le programme après les transactions
    var retourmenu = true
    do {
      println("         Nospresso Café          ")
      println("Veuillez sélectionner votre machine (1-5)")
      var machineID = readInt() - 1
      while (machineID < 0 || machineID >= nbMachines) {
        println("L'ID saisie n'existe pas.")
        println("Veuillez réessayez.")
        machineID = readInt() - 1
      }
      println("Machine sélectionnée (1-5): " + (machineID + 1))
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quittez")
      var choixmode = readInt()
      //(saut à la ligne entre 1) 2) 3))
      //(if client else if admin else quittez)
      //(gestion erreur : relancer jusqu'à entrée valide (boucle while))
      while (!(choixmode == 1 || choixmode == 2 || choixmode == 3)) {
        println("Votre sélection n'est pas correcte.")
        println("Choississez entre :")
        println("1) Client")
        println("2) Admin")
        println("3) Quittez")
        choixmode = readInt()
      }

      if (choixmode == 1) {
        //mode client: commander une boisson
        println("Mode Client")
        serveClient(machineID, coffeeStocks, sugarStocks, milkStocks)
      } else if (choixmode == 2) {
        //mode admin
        println("Mode Admin")
        if (validatePin(machineID, machinePins))
          //menu de sélection: changer code ou réapprovisionnement
          println("Que souhaitez-vous faire ? :")
        println("1) Changer le code PIN")
        println("2) Réapprovisionner les stocks")
        var choixmodeadmin = readInt()
        while (!(choixmodeadmin == 1 || choixmodeadmin == 2)) {
          println("Votre sélection n'est pas correcte.")
          println("Choississez entre :")
          println("1) Changer le code PIN")
          println("2) Réapprovisionner les stocks")
          choixmodeadmin = readInt()
        }
        if (choixmodeadmin == 1) {
          println("Mise à jour du code PIN pour la Machine " + (machineID + 1) + ".")
          updatePin(machineID, machinePins)
        } else {
          restockMachine(machineID, coffeeStocks, sugarStocks, milkStocks)
        }
      } else {
        retourmenu = false
      }
    } while (retourmenu)
  }
}