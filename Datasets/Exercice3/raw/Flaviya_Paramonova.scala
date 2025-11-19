
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object Main {
  // on donne les noms des var
  var nomboisson = ""
  var NomSucre = ""
  var NomLait = ""

  // var provisoires qui servent à gerer le stock
  var PoudreDeCafeSpec = 0
  var QSucreSpec = 0
  var QLaitSpec = 0

  class Machine(idC: Int, pincodeC: String, milkC: Int, sugarC: Int, coffeeC: Int) {

    //declaration des attributs
    var id = idC
    var pincode = pincodeC
    var milk = milkC
    var sugar = sugarC
    var coffee = coffeeC


    // declaration des methodes

    //methode 1
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "coffee") coffee += amount
      if (ingredient == "sugar") sugar += amount
      if (ingredient == "milk") milk += amount
    }

    //methode 2
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "coffee") {
        if (coffee >= amount) {
          coffee -= amount
          return true
        } else {
          println("Erreur : Quantité de poudre de café insuffisante.")
          return false
        }
      }

      if (ingredient == "sugar") {
        if (sugar >= amount) {
          sugar -= amount
          return true
        } else {
          println("Erreur : Quantité de sucre insuffisante.")
          return false
        }
      }

      if (ingredient == "milk") {
        if (milk >= amount) {
          milk -= amount
          return true
        } else {
          println("Erreur : Quantité de lait insuffisante.")
          return false
        }
      }
      return false
    }
  }
// meth 3
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      println("Chargement des machines depuis " + filename)
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines
      var ligne = lignefr.next
      var i = 0
      while (!lignefr.isEmpty) {
        ligne = lignefr.next
        val UneMachine = ligne.split(",")
        machines += new Machine(i, UneMachine(0), UneMachine(1).toInt, UneMachine(2).toInt, UneMachine(3).toInt)
        i += 1
        println("\nMachine " + i + " chargée :")
        println("ID: " + i)
        println("Code Pin: " + UneMachine(0))
        println("Lait: " + UneMachine(1) + "mL")
        println("Sucre: " + UneMachine(2) + " g")
        println("Café: " + UneMachine(3) + "mL")
      }
      println("\n " + i + " machine(s) chargée(s) avec succès")
      fr.close()
    } catch {
      case _ => println("Fichier introuvable. Vérifiez le chemin d’accès et réessayez. ")
    }
    return machines
  }

//meth 4
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    //On vérifie les erreurs pouvant arriver lors de l'écriture
    try {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")

      for (machineObj <- machines) {
        fw.println(machineObj.pincode + "," + machineObj.milk + "," + machineObj.sugar + "," + machineObj.coffee)
      }
      var nbMachines = machines.length
      println("Sauvegarde de " + nbMachines + " machines dans " + filename)
      fw.close()
    }
    catch {
      case _ => println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println("---\nErreur : echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }

//meth 5
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val pinJuste = readLine("Enter PIN: ")
    if (machines(machineId).pincode == pinJuste) {
      return true
    } else {
      return false
    }
  }
//meth 6
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

  //meth 7
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
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
    if (machines(machineId).coffee < PoudreDeCafeSpec) {
      println("Erreur : Quantité de poudre de café insuffisante.")
      return false

    } else if ((machines(machineId).sugar < QSucreSpec)) {
      println("Erreur : Quantité de sucre insuffisante.")
      return false
    }
    else if ((machines(machineId).milk < QLaitSpec)) {
      println("Erreur : Quantité de lait insuffisante.")
      return false

    } else {
      // deduction des ingredients du stock , apres la confirmation que le stock est suffisant!
      machines(machineId).coffee -= PoudreDeCafeSpec
      machines(machineId).sugar -= QSucreSpec
      machines(machineId).milk -= QLaitSpec

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

  //methode 3
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Stocks:\nPoudre de café :" + machines(machineId).coffee + "g\nLait :" + machines(machineId).milk + "ml \nSucre :" + machines(machineId).sugar + "g")
    var RajoutPoudreDeCafe = 0
    var RajoutLait = 0
    var RajoutSucre = 0

    println("Réapprovisionnement des stocks...\n si vous ne voulez rien ajouter entrez la valeur 0!")
    RajoutPoudreDeCafe = readLine("Ajout :\n PoudreDeCafe >").toInt
    RajoutLait = readLine(" Lait :>").toInt
    RajoutSucre = readLine(" Sucre :>").toInt

    // on rajoute les ingredients dans le stock
    machines(machineId).coffee += RajoutPoudreDeCafe
    machines(machineId).sugar += RajoutSucre
    machines(machineId).milk += RajoutLait

    println("Niveaux de stock mis à jour.\nRetour au menu principal...")
  }

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

  def main(args: Array[String]): Unit = {


    //Variable qui contient le nom du fichier
    var filename = "machines.csv"

    //On initialise l'arrayBuffer avec les machines
    var machines = loadcsv(filename)

    //On met cette variable a true pour pouvoir entrer dans la boucle while une premiere fois
    // Sauf si le chargement du fichier n'a pas marché
    var просмотр = true
    if (machines.isEmpty) {
      просмотр = false
      println("---\nErreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }

    // pour boucler le reafichage
    while (просмотр) {
      просмотр = false
      // On affiche la page de sélection de mode
      var choixUtilisateur = 0
      while ((choixUtilisateur < 1) || (choixUtilisateur > 3)) {
        println("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
        choixUtilisateur = readLine("->").toInt
      }
      //mode client
      if (choixUtilisateur == 1) {
        var machineId = readLine("Machine sélectionnée (1-5) > ").toInt

        while ((machineId > 5) || (machineId < 1)) {
          machineId = readLine("Machine sélectionnée (1-5) > ").toInt
        }
        machineId -= 1

        val serve = serveClient(machineId, machines)

        choixUtilisateur = 0
        просмотр = true

      }
      //Mode admin

      else if (choixUtilisateur == 2) {
        // Sélection de la machine par l'admin
        val machineIdAffiche: Int = readLine("Machine sélectionnée (1-5) > ").toInt
        val machineId = machineIdAffiche - 1 // Index ajusté pour accéder à la machine dans ArrayBuffer

        // Gestion des essais pour la validation du PIN
        var compteurPin = 3
        var pinValide = false

        while (compteurPin > 0 && !pinValide) {
          val codeAdmin = readLine(s"Mode Admin\nEntrez le code PIN pour Machine $machineIdAffiche : ******\n-> ")
          if (machines(machineId).pincode == codeAdmin) {
            pinValide = true
            println("Code PIN saisi : Correct\nAccès accordé")
          } else {
            compteurPin -= 1
            println(s"Code PIN incorrect. Il vous reste $compteurPin tentative(s).")
          }
        }

        // Si le PIN est correct, accéder aux options admin
        if (pinValide) {
          val choixAdmin = readLine("\nPour accéder aux stocks, entrez 1 \nPour changer de code, entrez 2 \n->").toInt
          if (choixAdmin == 1) restockMachine(machineId, machines)
          if (choixAdmin == 2) {
            // Extraire les PINs dans un Array[String]
            val machinePins = machines.map(_.pincode).toArray
            updatePin(machineId, machinePins)

            // Mettre à jour le PIN dans `machines`
            machines(machineId).pincode = machinePins(machineId)
            savecsv(filename, machines)
          }
        } else {
          println("Trop de tentatives échouées. Accès refusé.")
        }
      }

      else if (choixUtilisateur == 3) {
        println("Vous avez quitté le programme.\n :)")
        savecsv(filename, machines)
      }
    }
  }
}