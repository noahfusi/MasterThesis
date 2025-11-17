import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  //Classe Machine
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    //Ajouter une quantité spécifiée d'un ingrédient au stock de la machine
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println(s"Ingrédient $ingredient non reconnu.")
      }
    }

    //Retirer une quantité spécifiée d'un ingrédient du stock de la machine si le stock est suffisant
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient match {
        case "milk" =>
          if (milk >= amount) {
            milk -= amount
            true
          } else {
            false
          }
        case "sugar" =>
          if (sugar >= amount) {
            sugar -= amount
            true
          } else {
            false
          }
        case "coffee" =>
          if (coffee >= amount) {
            coffee -= amount
            true
          } else {
            false
          }
        case _ =>
          println(s"Ingrédient $ingredient non reconnu.")
          false
      }
    }
  }

  //Charger les machines depuis un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      //Afficher un message indiquant que le fichier est en cours de chargement
      println(s"Chargement des machines depuis $filename...\n")

      //Attente de 3 secondes pour simuler chargement du fichier
      Thread.sleep(1000)
      val source = io.Source.fromFile(filename)
      //Ignorer l'en-tête
      val lines = source.getLines().drop(1)
      //Parcourir chaque ligne et créer une machine
      for (line <- lines) {
        val Array(pincode, milk, sugar, coffee) = line.split(",").map(_.trim)
        val machine = new Machine(machines.size + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine

        //Afficher les détails après chargement
        println(s"Machine ${machines.size} chargée :\n  ID: ${machine.id}\n  Code PIN: ${machine.pincode}\n  Lait: ${machine.milk / 1000.0}L\n  Sucre: ${machine.sugar}g\n  Café: ${machine.coffee}g\n")
      }
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      //Terminer le programme en cas d'erreur
      case e: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        sys.exit(1)
      case e: IOException =>
        println("Erreur : lors de la lecture du fichier.")
        sys.exit(1)
      case e: Exception =>
        println("Erreur : Échec du chargement des machines.")
        sys.exit(1)
    }
    if (machines.isEmpty){
      println("Erreur : Aucun fichier chargé. Fermeture du programme.")
      sys.exit(1)
    }
    machines
  }

  //Sauvegarder les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      //En-tête CSV
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println(s"Sauvegarde de ${machines.size} machine(s) dans $filename...")
      Thread.sleep(1000)
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename. Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println("Erreur : Échec de la sauvegarde des machines.")
        sys.exit(1)
    }
  }

  //Valider le code PIN de la machine sélectionnée
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var tentatives = 3
    while (tentatives > 0 && machineId > 0 && machineId <= machines.length) {
      println(s"Entrez le code PIN de la machine $machineId :\n> ")
      val PIN = readLine()
      if (PIN == machines(machineId - 1).pincode) {
        println(s"Accès accordé à la machine $machineId.")
        return true
      } else {
        tentatives -= 1
        println(s"Code PIN incorrect. $tentatives tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  //Mettre à jour PIN de la machine sélectionnée
  def updatePIN(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var newPIN = ""
    println(s"Mise à jour du code PIN pour la machine $machineId.")
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :\n> ")
      newPIN = readLine()
    }
    while (newPIN.length != 6 || !newPIN.forall(_.isDigit))
    machines(machineId - 1).pincode = newPIN
    println(s"Le code PIN a été mis à jour avec succès pour la machine $machineId.\nRetour au menu principal...")
  }

  //Réapprovisionner les ingrédients pour la machine sélectionnée
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId - 1)
    println(s"Réapprovisionnement de la machine $machineId.\nEntrez les quantités à ajouter :")
    println("Poudre de café (en grammes):\n> ")
    val ajout_cafe = readLine().toInt
    println("Sucre (en grammes):\n> ")
    val ajout_sucre = readLine().toInt
    println("Lait (en litres):\n> ")
    val ajout_lait = (readDouble() * 1000).toInt

    //Validation des entrées
    if (ajout_cafe >= 0 && ajout_sucre >= 0 && ajout_lait >= 0) {
      machine.addIngredient("coffee", ajout_cafe)
      machine.addIngredient("sugar", ajout_sucre)
      machine.addIngredient("milk", ajout_lait)
      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
    }
    else {
      println("Quantités invalides. Les stocks n'ont pas été mis à jour.")
    }
  }

  //Traiter une transaction client pour la machine sélectionnée
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    //Réinitialisation des variables à chaque nouvelle commande
    var prix_boisson = 0.00
    var cafe_requis = 0
    var sucre_requis = 0
    var prix_sucre = 0.00
    var lait_requis = 0
    var prix_dose_lait = 0.00
    var lait_suppl = "Non"
    var nom_boisson = ""
    var nom_sucre = ""

    //Sélection de la boisson
    println(s"Machine $machineId sélectionnée.\nChoisissez votre boisson : \n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    val choix = readLine().toInt

    if (choix >= 1 && choix <= 3) {
      if (choix == 1) {
        nom_boisson = "Expresso"
        prix_boisson += 2.00
        cafe_requis += 8
      }
      else if (choix == 2) {
        nom_boisson = "Cappuccino"
        prix_boisson += 2.50
        cafe_requis += 6
        lait_requis += 100
      }
      else if (choix == 3) {
        println("Quelle taille désirez-vous ?\n1) Petit\n2) Moyen\n3) Grand\n> ")
        val Latte_taille = readLine().toInt
        if (Latte_taille == 1) {
          nom_boisson = "Latte petit"
          prix_boisson += 2.70
          cafe_requis += 6
          lait_requis += 120
        }
        else if (Latte_taille == 2) {
          nom_boisson = "Latte moyen"
          prix_boisson += 3.20
          cafe_requis += 8
          lait_requis += 150
        }
        else if (Latte_taille == 3) {
          nom_boisson = "Latte grand"
          prix_boisson += 3.70
          cafe_requis += 12
          lait_requis += 200
        }
        else {
          println("Choix invalide")
          return false
        }
      }
      else {
        println("Choix invalide")
        return false
      }
    }

    //Sélection du lait supplémentaire pour cappuccino et latte
    if (choix == 2 || choix == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
      val lait = readLine().toInt
      if (lait == 1) {
        lait_suppl = "Oui"
        println("Combien de dose (50ml)? (3 doses max par boisson)\n> ")
        val dose_lait = readLine().toInt

        if (dose_lait > 0 && dose_lait <= 3) {
          prix_dose_lait += dose_lait * 0.05
          lait_requis += dose_lait * 50
        }
        else if (dose_lait > 3) {
          println("Il n'y a que 3 doses maximales par boisson\\nChoix invalide, retour au menu principal")
          return false
        }
        else {
          println("Choix invalide")
          return false
        }
      }
      else if (lait == 2) {
      }
      else {
        println("Choix invalide")
        return false
      }
    }

    //Sélection du sucre
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
    val sucre = readLine().toInt
    if (sucre == 1) {
      nom_sucre = "Sans sucre"
      prix_sucre = 0.00
      sucre_requis += 0
    }
    else if (sucre == 2) {
      nom_sucre = "Peu (5g)"
      prix_sucre += 0.10
      sucre_requis += 5
    }
    else if (sucre == 3) {
      nom_sucre = "Moyen (10g)"
      prix_sucre += 0.20
      sucre_requis += 10
    }
    else if (sucre == 4) {
      nom_sucre = "Beaucoup (15g)"
      prix_sucre += 0.30
      sucre_requis += 15
    }
    else {
      println("Choix invalide, retour au menu principal")
      return false
    }

    //Vérifier les stocks
    val prix_final = prix_boisson + prix_sucre + prix_dose_lait
    val machine = machines(machineId - 1)
    if (machine.coffee >= cafe_requis && machine.sugar >= sucre_requis && machine.milk >= lait_requis) {
      println(s"Boisson sélectionnée : $nom_boisson\nNiveau de sucre : $nom_sucre\nLait supplémentaire : $lait_suppl\nPrix total : CHF $prix_final")

      //Code TWINT
      val code_Twint = Random.alphanumeric.take(5).mkString
      println("\nVeuillez payer en utilisant Twint\nVotre code de paiement est : " + code_Twint)
      println("(En attente de validation du paiement...)")

      //Attente de 3 secondes pour simuler validation paiement
      Thread.sleep(3000)

      //Mise à jour des stocks
      machine.coffee -= cafe_requis
      machine.sugar -= sucre_requis
      machine.milk -= lait_requis

      println("\nPaiement confirmé.\nPréparation de votre boisson...")

      //Attente de 5 secondes pour préparation boisson
      Thread.sleep(5000)
      println("Votre " + nom_boisson + " est prêt ! Bonne dégustation !")
      true
    }
    else if (machine.milk < lait_requis) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayez une autre boisson ou une autre machine.")
      false
    }
    else if (machine.coffee < cafe_requis) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou essayez une autre machine ou vérifiez les stocks en mode Admin")
      false
    }
    else if (machine.sugar < sucre_requis) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une quantité de sucre plus petite ou essayez sur une autre machine")
      false
    }
    else {
      false
    }
  }

  //Afficher le menu principal et gérer les choix
  def main(args: Array[String]): Unit = {
    //Charger les machines au démarrage depuis le fichier CSV
    val machines = loadcsv("machines.csv")
    if (machines.isEmpty){
      println("Erreur : Impossible de charger les machines. Fermeture du programme.")
      sys.exit(1)
    }
    var continuer = true



    while (continuer) {
      //Menu principal
      println("\n     Nospresso Café \nVeuillez sélectionner votre mode :\n1. Client\n2. Admin\n3. Quitter\n> ")
      val mode = readLine().toInt

      //Mode client
      if (mode == 1) {
        println(s"Sélectionnez une machine (1-${machines.length}):\n> ")
        val machineId = readLine().toInt
        if (machineId < 1 || machineId > machines.length) {
          println("Machine invalide. Essayez encore.")
        } else if (!serveClient(machineId, machines)) {
          println("Transaction échouée. Essayez encore.")
        }
        else {
          println(s"Machine $machineId sélectionnée pour la commande.")
        }
      }
      //Mode admin
      else if (mode == 2) {
        println(s"Sélectionnez une machine (1-${machines.length}):\n> ")
        val machineId = readLine().toInt
        if (machineId == -1) {
          continuer = false
          println("Fin du programme.")
        } else if (machineId < 1 || machineId > machines.length) {
          println("Machine invalide. Essayez encore.")
        }
        if (validatePin(machineId, machines)) {
          println("1. Réapprovisionner les ingrédients\n2. Mettre à jour le code PIN\n> ")
          val choix_admin = readLine().toInt
          if (choix_admin == 1) {
            println(s"Niveaux de stock actuels :\n   Poudre de café : ${machines(machineId - 1).coffee}g\n   Sucre : ${machines(machineId - 1).sugar}g\n   Lait : ${machines(machineId - 1).milk.toDouble / 1000})}L")
            restockMachine(machineId, machines)
          }
          else if (choix_admin == 2) {
            updatePIN(machineId, machines)
          }
          else {
            println("Choix invalide")
          }
        }
      }
      //Quitter
      else if (mode == 3) {
        //Sauvegarder les machines à la sortie
        savecsv("machines.csv", machines)
        println("Merci et à bientôt !")
        continuer = false
      }
      //Mode "erreur"
      else {
        println("Retour au menu principal")
      }
    }
  }
}