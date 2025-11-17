import scala.io.StdIn
import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.control.Breaks.break

object Main {

  //initialisation des caractères pour le code
  var codepaie = ""
  val caractere = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

  //initialisation des prix et quantités
  var prixcafe = 0.0
  var prixsucre = 0.0
  var prixlait = 0.0
  var taille = 0
  var doselait = 0
  var supplementlait = 0
  var supplementsucre = 0

  //variables de réapprovisionnement des stocks
  var ajoutcafe = 0
  var ajoutlait = 0
  var ajoutsucre = 0

  //quantités de café, sucre et lait nécessaire à la confection de la boisson
  var cafeNecessaire = 0
  var laitNecessaire = 0
  var sucreNecessaire = 0

  var boucle = true
  //
  var NbeTentative = 3
  var EssaiCode = ""
  var idDemande = 0
  var NewCode = ""

  case class Machine(var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "lait") {
        milk += amount
      }
      if (ingredient == "sucre") {
        sugar += amount
      }
      if (ingredient == "cafe") {
        coffee += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if ((ingredient == "cafe" && coffee >= amount) || (ingredient == "sucre" && sugar >= amount) || (ingredient == "lait" && milk >= amount)) {
        if (ingredient == "lait" && milk >= amount) {
          milk -= amount
        }
        if (ingredient == "sucre" && sugar >= amount) {
          sugar -= amount
        }
        if (ingredient == "cafe" && coffee >= amount) {
          coffee -= amount
        }
        true
      } else {
        false
      }
    }

  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("Chargement des machines depuis machines.csv...")
    val machines = ArrayBuffer[Machine]()

    try {
      val fr = Source.fromFile("machines.csv")
      val lignefr = fr.getLines
      val header = lignefr.next()
      var i = 1
      while (lignefr.hasNext) {

        var ligne = lignefr.next
        var colonne = ligne.split(',')


        val machine = new Machine(
          id = i,
          pincode = colonne(0),
          milk = colonne(1).toInt,
          sugar = colonne(2).toInt,
          coffee = colonne(3).toInt)
        println("Machine " + machine.id + " chargée :\nID: " + machine.id + "\nCode PIN: " + machine.pincode + "\nLait: " + machine.milk + "ml\nSucre: " + machine.sugar + "g\nCafé: " + machine.coffee + "g")
        i += 1
        machines += machine
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")
      fr.close()
       machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        break
         machines

    }


  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val fw = new PrintWriter(filename)
    try {
      //ecriture en-tête
      fw.println("pincode,milk,sugar,coffee")
      for (machine <- machines) {
        fw.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      println("Sauvegarde de " + machines.length + " machines dans machines.csv...\nFichier sauvegardé avec succès.")
    } catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      case ex: java.nio.file.AccessDeniedException => println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.\nFermeture du programme.")
        break
    }
    finally {
      fw.close() // Ferme le fichier pour éviter les fuites de ressources
      boucle = false
    }
  }

  def validatePin(idDemande: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var MachineId = machines(idDemande - 1).id
    while (NbeTentative > 0) {
      println("Entrez le code PIN :")
      EssaiCode = readLine("> ")
      if (EssaiCode == machines(idDemande - 1).pincode) {
        println("Accès accordé.")
        NbeTentative = 3
        return true
      } else {
        NbeTentative -= 1
        if (NbeTentative > 0) {
          println(s"Code PIN incorrect. $NbeTentative tentatives restantes.")
        } else {
          println("Trop de tentatives échouées. Fin du programme.")

        }
      }
    }
    NbeTentative = 3
    false
  }

  def updatePin(idDemande: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + idDemande)

    NewCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    if (NewCode.length == 6 && NewCode.forall(_.isDigit)) {
      machines(idDemande - 1).pincode = NewCode
      println("Code PIN enregistré avec succès.")
    } else {
      while (NewCode.length != 6 || !NewCode.forall(_.isDigit)) {
        NewCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        if (NewCode.length == 6 && NewCode.forall(_.isDigit)) {
          machines(idDemande - 1).pincode = NewCode
          println("Code PIN enregistré avec succès.")
        }
      }
    }
  }

  def serveClient(idDemande: Int, machines: ArrayBuffer[Machine]): Boolean = {
    //choix des boissons du client
    println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var boisson = readLine(">").toInt
    //test de la valeur de boisson
    while (boisson < 1 || boisson > 3) {
      println("cette boisson n'est pas valide")
      boisson = readLine(">").toInt
    }
    //choix de la taille du Latte
    if (boisson == 3) {
      println("1) Petit\n2) Moyen\n3) Grand")
      taille = readLine(">").toInt
      while (taille < 1 || taille > 3) {
        println("la taille n'est pas valide")
        taille = readLine(">").toInt
      }
    }
    // ajout sucre
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    supplementsucre = readLine(">").toInt
    while (supplementsucre < 1 || supplementsucre > 4) {
      println("cette quantité n'est pas valide")
      supplementsucre = readLine(">").toInt
    }

    //choix du supplément en lait, restraint au cappucino et latte
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
      supplementlait = readLine(">").toInt
      while (supplementlait < 0 || supplementlait > 3) {
        println("supplément de lait invalide")
        supplementlait = readLine(">").toInt
      }

      if (supplementlait == 1) {
        println("Combien de doses? - 1 à 3")
        doselait = readLine(">").toInt


        while (doselait < 1 || doselait > 3) {
          if (doselait < 1 || doselait > 3) {
            println("ce nombre de dose de lait n'est pas valide")
            doselait = readLine(">").toInt
          } else {
            println("combien de dose? - 1 à 3")
            doselait = readLine(">").toInt
          }
        }
      }
    }

    //afficher le choix de la boisson et déterminer les quantités nécessaire à sa confection
    if (boisson == 1) {
      println("Boisson sélectionnée : Expresso")
      cafeNecessaire = 8
      prixcafe += 2.0
    }
    if (boisson == 2) {
      println("Boisson sélectionnée : Cappuccino")
      cafeNecessaire = 6
      laitNecessaire = 100
      prixcafe += 2.5
    }

    if (boisson == 3) {
      if (taille == 1) {
        println("Boisson sélectionnée : Latte (Petit)")
        cafeNecessaire = 6
        laitNecessaire = 120
        prixcafe += 2.7
      }
      if (taille == 2) {
        println("Boisson sélectionnée : Latte (Moyen)")
        cafeNecessaire = 8
        laitNecessaire = 150
        prixcafe += 3.2
      }
      if (taille == 3) {
        println("Boisson sélectionnée : Latte (Grand)")
        cafeNecessaire = 12
        laitNecessaire = 200
        prixcafe += 3.7
      }
    }

    //afficher et déterminer la quantité de sucre
    if (supplementsucre == 1) {
      println("Niveau de sucre : Sans sucre")
    }
    if (supplementsucre == 2) {
      println("Niveau de sucre : Peu (5g)")
      sucreNecessaire = 5
      prixsucre += 0.1
    }
    if (supplementsucre == 3) {
      println("Niveau de sucre : Moyen (10g)")
      sucreNecessaire = 10
      prixsucre += 0.2
    }
    if (supplementsucre == 4) {
      println("Niveau de sucre : Beaucoup (15g)")
      sucreNecessaire = 15
      prixsucre += 0.3
    }

    //Afficher la dose de lait
    if (supplementlait == 2) {
      println("Lait en supplément: Non")
    }
    if (doselait == 1) {
      println("Lait en supplément: 1 dose")
      laitNecessaire += 50
      prixlait += 0.05
    }
    if (doselait == 2) {
      println("Lait en supplément: 2 dose")
      laitNecessaire += 50
      prixlait += 0.1
    }
    if (doselait == 3) {
      println("Lait en supplément: 3 dose")
      laitNecessaire += 150
      prixlait += 0.15
    }

    //indiquer si la quantité est insuffisante
    if (machines(idDemande - 1).coffee < cafeNecessaire || machines(idDemande - 1).sugar < sucreNecessaire || machines(idDemande - 1).milk < laitNecessaire) {
      if (machines(idDemande - 1).coffee < cafeNecessaire) {
        println("Erreur : Quantité de café insuffisante pour préparer\nla boisson sélectionnée.")
      }
      else if (machines(idDemande - 1).sugar < sucreNecessaire) {
        println("Erreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.")
      }
      else if (machines(idDemande - 1).milk < laitNecessaire) {
        println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.")
      }
      if (taille == 2 || taille == 3) {
        println("Veuillez choisir une taille plus petite ou essayer\nune autre boisson.")
      } else {
        println("Veuillez choisir une autre boisson ou v´erifier les\nstocks en mode Admin.")
      }
      return false
    }

    //affichage du prix total
    var prixtotal: Double = math.round((prixcafe + prixlait + prixsucre) * 100) / 100.0

    if (machines(idDemande - 1).coffee >= cafeNecessaire && machines(idDemande - 1).sugar >= sucreNecessaire && machines(idDemande - 1).milk >= laitNecessaire) {
      printf("Prix total : CHF %.2f", prixcafe)
      if (supplementsucre > 1 && supplementsucre < 5) {
        printf(" + CHF %.2f", prixsucre)
      }
      if (doselait > 0 && doselait < 4) {
        printf(" + CHF %.2f", prixlait)
      }
      printf(" = CHF %.2f", prixtotal)

      //génération du code de paiement
      for (_ <- 1 to 5) {
        val randomChar = caractere(Random.nextInt(caractere.length))
        codepaie += randomChar
      }
      //paiement
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codepaie + "\n(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé. \nen attente de votre boisson ...")
      Thread.sleep(5000)

      //message de fin
      if (boisson == 1) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
      }
      if (boisson == 2) {
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
      }
      if (boisson == 3) {
        println("Votre Latte est prêt ! Bonne dégustation !")
      }

      if (machines(idDemande - 1).coffee >= cafeNecessaire && machines(idDemande - 1).sugar >= sucreNecessaire && machines(idDemande - 1).milk >= laitNecessaire) {
        //réduction des stocks
        machines(idDemande - 1).removeIngredient("cafe": String, cafeNecessaire: Int)
        machines(idDemande - 1).removeIngredient("lait": String, laitNecessaire: Int)
        machines(idDemande - 1).removeIngredient("sucre": String, sucreNecessaire: Int)
        //réinitialisation des valeurs
        prixcafe = 0
        prixsucre = 0
        prixlait = 0
        prixtotal = 0.0
        cafeNecessaire = 0
        laitNecessaire = 0
        sucreNecessaire = 0
        supplementsucre = 0
        doselait = 0
        taille = 0
        supplementlait = 0
        codepaie = ""
      }
      true
    } else {
      false
    }
  }


  def main(args: Array[String]): Unit = {
    //initialisation de la machine
    var boucle = true
    while (boucle) {

      var machines = loadcsv("machines.csv")
      //choix de la machine
      idDemande = readLine("Machine sélectionnée (1-" + machines.length + ") >").toInt
      while (idDemande < 1 || idDemande > machines.length) {
        println("le la machine n'existe pas")
        idDemande = readLine("Machine sélectionnée (1-5) >").toInt
      }
      // affichage de la machine
      println("           Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var mode = readLine(">").toInt
      //assurer que le mode sélectionné soit valide
      while (mode < 1 || mode > 3) {
        println("le mode n'existe pas")
        mode = readLine(">").toInt
      }
      if (mode == 1) {
        serveClient(idDemande, machines)
        savecsv("machines.csv": String, machines: ArrayBuffer[Machine])
      }
      if (mode == 2) {
        var BoucleAdmin = true

        if (validatePin(idDemande, machines)) {

          while (BoucleAdmin) {
            println("que voulez vous faire? \n1) changer code pin\n2) changer les stocks\n3) Quitter")
            var choixaAdmin = readLine(">").toInt
            while (choixaAdmin < 1 || choixaAdmin > 3) {
              println("le choix n'existe pas")
              choixaAdmin = readLine(">").toInt
            }
            if (choixaAdmin == 1) {
              updatePin(idDemande: Int, machines: ArrayBuffer[Machine])
              savecsv("machines.csv", machines) // Sauvegarde les modifications immédiatement après le changement
              BoucleAdmin = false
            }
            if (choixaAdmin == 2) {
              println("Niveaux de stock actuels :\n  Poudre de café : " + machines(idDemande - 1).coffee + "g  \n Sucre :  " + machines(idDemande - 1).sugar + "g \n  Lait : " + machines(idDemande - 1).milk + "ml")

              println("Entrez les quantités à ajouter :")
              ajoutcafe = readLine("Poudre de café > ").toInt
              ajoutsucre = readLine("Sucre         > ").toInt
              ajoutlait = readLine("Lait          > ").toInt

              machines(idDemande - 1).addIngredient("cafe": String, ajoutcafe: Int)
              machines(idDemande - 1).addIngredient("sucre": String, ajoutsucre: Int)
              machines(idDemande - 1).addIngredient("lait": String, ajoutlait: Int)
              ajoutlait = 0
              ajoutcafe = 0
              ajoutsucre = 0
              savecsv("machines.csv", machines)
              BoucleAdmin = false
            }
            else {
              if (choixaAdmin == 3) {
                BoucleAdmin = false
              }
            }
          }

        } else {
          boucle = false
        }
      }
      if (mode == 3) {
        println("bonne journée!")
        savecsv("machines.csv": String, machines: ArrayBuffer[Machine])
        boucle = false
      }
    }
  }
}