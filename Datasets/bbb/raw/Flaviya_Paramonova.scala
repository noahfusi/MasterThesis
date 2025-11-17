
import scala.io.StdIn._


object Main {

  //declaration de variables

  //compteur essais pin
  var compteurPin = 0

  // var rajout, pour le mode admin
  var RajoutPoudreDeCafe = 0
  var RajoutLait = 0
  var RajoutSucre = 0

  // on donne les noms
  var nomboisson = ""
  var NomSucre = ""
  var NomLait = ""

  // initialisation de tableaux
  val nbMachines = 5
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  var machinePins = Array.fill(nbMachines)("434343")

  // var provisoires qui servent à gerer le stock
  var PoudreDeCafeSpec = 0
  var QSucreSpec = 0
  var QLaitSpec = 0


  var machineId = 0

  def main(args: Array[String]): Unit = {
    var choixUtilisateur = 0
    // on cree la boucle qui fait qu'on retourne au menu principal apres chaque action
    do {


      // choix du mode
      do {
        choixUtilisateur = readLine("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n->").toInt
        if (!(choixUtilisateur == 1) && !(choixUtilisateur == 2) && !(choixUtilisateur == 3)) {
          println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
        }
      } while (!(choixUtilisateur == 1) && !(choixUtilisateur == 2) && !(choixUtilisateur == 3))





      //on est en mode client
      if (choixUtilisateur == 1) {
        // choix de la machine
        do {
          machineId = readLine("Quelle machine voulez vouz utiliser ?\n1\n2\n3\n4\n5\n>").toInt
          machineId -= 1
          if ((machineId > 4) || (machineId < 0)) println("Reponse invalide , choisissez une machine entre 1 et 5")
        } while (machineId > 4 || machineId < 0)


        val machineIdAffiche = machineId + 1
        println("Machine sé́lectionneé : Machine " + machineIdAffiche)
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      }


      //on passe en mode admin
      else if (choixUtilisateur == 2) {
        // choix de la machine
        do {
          machineId = readLine("Quelle machine voulez vouz utiliser ?\n1\n2\n3\n4\n5\n>").toInt
          machineId -= 1
          if ((machineId > 4) || (machineId < 0)) println("Reponse invalide , choisissez une machine entre 1 et 5")
        } while (machineId > 4 || machineId < 0)
        // methodes du mode admin
        val machineIdAffiche = machineId + 1
        println("Machine sé́lectionneé : Machine " + machineIdAffiche)
        val testValidatePin = validatePin(machineId, machinePins)
        if (testValidatePin == true) {
          println("Code PIN saisi : Correct \nAcceès accordé")
          val ChoixAdmin = readLine("\nPour acceder aux stocks entrez 1 \nPour changer de code entrez 2  \n->").toInt
          if (ChoixAdmin == 1) restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          if (ChoixAdmin == 2) updatePin(machineId, machinePins)
        }
      }


      else if (choixUtilisateur == 3) {
        println("Vous avez quitté le programme.\n :)")
      }


    } while (!(choixUtilisateur == 3) && (compteurPin < 3))
  }


  //methode 1
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var compteurPin = 3
    //les inputs
    //comparaison avec le code
    do {
      val CodeAdmin = readLine("Mode Admin\nEntrez le code PIN : ******\n->")
      if (CodeAdmin == machinePins(machineId)) {
        return true
      } else {
        compteurPin -= 1

        println("Code PIN incorrect. " + compteurPin + " tentatives restantes. ")
      }
      // retourne si meme ou pas
      //voir plus " return early "; un chemin d'execut mene a un return , dont tout ce qu;il y a apres ne sera pas considéré

    } while (compteurPin > 0) // ca marche car on compte 0,1,2
    println("Trop de tentatives  ́echoue ́es.\n Fin du programme.")
    false
  }


  //methode 2
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    while (true) {
      val machineIdAffiche = machineId + 1
      val NvCode = readLine("Mise à jour du code PIN pour la Machine " + machineIdAffiche + ".\n Entrez un nouveau code PIN à 6 chiffres ->")
      if ((NvCode.length == 6) && NvCode.forall(Character.isDigit)) {
        machinePins(machineId) = NvCode
        return
      }
      println("Le code PIN a  été́ mis à jour avec succès.\nRetour au menu principal...")
    }
  }


  //methode 3
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Stocks:\nPoudre de café :" + coffeeStocks(machineId) + "g\nLait :" + milkStocks(machineId) + "ml \nSucre :" + sugarStocks(machineId) + "g")

    println("Réapprovisionnement des stocks...\n si vous ne voulez rien ajouter entrez la valeur 0!")
    RajoutPoudreDeCafe = readLine("Ajout :\n PoudreDeCafe >").toInt
    RajoutLait = readLine(" Lait :>").toInt
    RajoutSucre = readLine(" Sucre :>").toInt

    // on rajoute les ingredients dans le stock
    coffeeStocks(machineId) += RajoutPoudreDeCafe
    sugarStocks(machineId) += RajoutSucre
    milkStocks(machineId) += RajoutLait

    println("Niveaux de stock mis à jour.\nRetour au menu principal...")
  }

  //methode 4
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var boisson = 0

    //variables pour calculer les prix
    var prix = 0.0
    var prixBoisson = 0.0
    var prixSucre = 0.0
    var prixLaitSupplement = 0.0

    //on choisi la boisson
    do {
      boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n->").toInt
      if (!(boisson == 1) && !(boisson == 2) && !(boisson == 3)) {
        println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
      }
    } while (!(boisson == 1) && !(boisson == 2) && !(boisson == 3))

    if (boisson == 1) {
      nomboisson = "Expresso"
      prixBoisson += 2.00
      PoudreDeCafeSpec += 8
    }
    if (boisson == 2) {
      nomboisson = "Cappuccino"
      prixBoisson += 2.50
      PoudreDeCafeSpec += 6
      QLaitSpec += 100
    }

    // dans le cas où la personne a choisi un latte
    var TailleLatte = 0
    if (boisson == 3) {
      do {
        TailleLatte = readLine("Veuillez sélectionner la taille du latte:\n1) Petit\n2) Moyen\n3) Grand\n->").toInt
        if (!(TailleLatte == 1) && !(TailleLatte == 2) && !(TailleLatte == 3)) {
          println("votre reponse n'est pas correcte , choisissez 1, 2 ou 3")
        }
      } while (!(TailleLatte == 1) && !(TailleLatte == 2) && !(TailleLatte == 3))
      if (TailleLatte == 1) {
        nomboisson = "Latte petit"
        prixBoisson += 2.70
        PoudreDeCafeSpec += 6
        QLaitSpec += 120

      }
      if (TailleLatte == 2) {
        nomboisson = "Latte moyen"
        prixBoisson += 3.20
        PoudreDeCafeSpec += 8
        QLaitSpec += 150
      }
      if (TailleLatte == 3) {
        nomboisson = "Latte grand"
        prixBoisson += 3.70
        PoudreDeCafeSpec += 12
        QLaitSpec += 200
      }
    }

    // on passe à la personnalisation de la commande
    // étape 1 sucre
    var sucre = 0
    if ((boisson == 1) || (boisson == 2) || (boisson == 3)) {
      do {
        sucre = readLine("Souhaitez-vous ajouter du sucre ? :\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n->").toInt
        if (!(sucre == 1) && !(sucre == 2) && !(sucre == 3) && !(sucre == 4)) {
          println("votre reponse n'est pas valide \n choisissez 1, 2, 3 ou 4")
        }
      } while (!(sucre == 1) && !(sucre == 2) && !(sucre == 3) && !(sucre == 4))
      if (sucre == 2) {
        prixSucre += 0.10
        QSucreSpec += 5
        NomSucre = "Peu (5g)"
      }
      if (sucre == 3) {
        prixSucre += 0.20
        QSucreSpec += 10
        NomSucre = "Moyen (10g)"
      }
      if (sucre == 4) {
        prixSucre += 0.30
        QSucreSpec += 15
        NomSucre = "Beaucoup (15g)"
      }
    }

    //etape 2.0 lait(oui ou non)
    var lait = 0
    var DoseLait = 0
    if ((boisson == 2) || (boisson == 3)) {
      //on cree la boucle qui fait qu'on retourne au menu initial apres chaque action
      do {
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? :\n1)oui\n2)non\n->").toInt

        if (!(lait == 1) && !(lait == 2)) {
          println("votre reponse n'est pas correcte , coisissez 1 ou 2")

          if (lait == 2) {
            NomLait = "sans lait"
          }
        }

      } while (!(lait == 1) && !(lait == 2))
      //etape 2.1 lait(combien?)
      if (lait == 1) {
        do {
          DoseLait = readLine("combien de doses?\n->").toInt
          if (!(DoseLait == 1) && !(DoseLait == 2) && !(DoseLait == 3)) {
            println("votre reponse n'est pas correcte , 3 est la dose maximale")
          }
        } while (!(DoseLait == 1) && !(DoseLait == 2) && !(DoseLait == 3))
      }
      if (DoseLait == 1) {
        prixLaitSupplement += 0.05
        QLaitSpec += 50
        NomLait = "1 dose de lait"

      }
      if (DoseLait == 2) {
        prixLaitSupplement += 0.10
        QLaitSpec += 100
        NomLait = "2 doses de lait"
      }
      if (DoseLait == 3) {
        prixLaitSupplement += 0.15
        QLaitSpec += 150
        NomLait = "3 doses de lait"
      }
    }

    // Vérification des stocks avant déduction
    if (coffeeStocks(machineId) < PoudreDeCafeSpec) {
      println("Erreur : Quantité de poudre de café insuffisante.")
      return false

    } else if (sugarStocks(machineId) < QSucreSpec) {
      println("Erreur : Quantité de sucre insuffisante.")
      return false

    } else if (milkStocks(machineId) < QLaitSpec) {
      println("Erreur : Quantité de lait insuffisante.")
      return false

    } else {
      // deduction des ingredients du stock , apres la confirmation que le stock est suffisant!
      coffeeStocks(machineId) -= PoudreDeCafeSpec
      sugarStocks(machineId) -= QSucreSpec
      milkStocks(machineId) -= QLaitSpec



      // on passe au paiement
      // On calcule le prix total à payer $$$
      prix = prixSucre + prixBoisson + prixLaitSupplement
      // find a function that keeps only les centimes dans les decimales
      // prix = function_name(prix)


      println("Boisson sélectionnée : " + nomboisson + " \n Niveau de sucre : " + NomSucre + "\nLait en supplément:" + NomLait)
      // on montre au client comment est cree le prix
      if (!(prixSucre == 0) && !(prixLaitSupplement == 0)) {
        println("Prix total: CHF " + prixBoisson + " + CHF" + prixSucre + " + CHF" + prixLaitSupplement + " = CHF " + prix)
      }
      else if (!(prixLaitSupplement == 0) && (prixSucre == 0)) {
        println("Prix total: CHF " + prixBoisson + " + CHF " + prixLaitSupplement + " + CHF " + " = CHF " + prix)
      }
      else if (!(prixSucre == 0) && (prixLaitSupplement == 0)) {
        println("Prix total: CHF " + prixBoisson + " + CHF " + prixSucre + " = CHF" + prix)
      }
      else if ((prixSucre == 0) && (prixLaitSupplement == 0)) {
        println("Prix total: CHF " + prixBoisson + "  = CHF " + prix)
      }



      // Création du code Twint
      println("Veuillez payer en utilisant Twint.\nVotre code de paiement est :")
      val chars = "ABCDEFGHIJKLMOPQRSTUVWXYZ1234567890"
      var code = ""
      for (i <- 1 to 5) {
        val index = (math.random() * chars.length).toInt
        code += chars(index)
      }
      println(code + "\n(En attente de paiement...)")
      Thread.sleep(3000)
      println("\nMerci ! Votre paiement a été accepté")
      println("Préparation de votre boisson...\n\n[...]\n\nVotre boisson est prête ! Bonne dégustation !\n")
    }
    //on enleve la remise à 0 car elles se renouvelent au debut de la methode

    return false
  }



  // utiliser les val tant qu'on a pas bsn de var


}
