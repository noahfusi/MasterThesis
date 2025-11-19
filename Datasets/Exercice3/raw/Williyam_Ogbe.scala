import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient.toLowerCase == "coffee") {
      coffee = coffee + amount
    } else if (ingredient.toLowerCase == "sugar") {
      sugar = sugar + amount
    } else if (ingredient.toLowerCase == "milk") {
      milk = milk + amount
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient.toLowerCase == "coffee" && coffee >= amount) {
      coffee = coffee - amount
      true
    } else if (ingredient.toLowerCase == "sugar" && sugar >= amount) {
      sugar = sugar - amount
      true
    } else if (ingredient.toLowerCase == "milk" && milk >= amount) {
      milk = milk - amount
      true
    } else {
      false
    }
  }
}

object Nospresso2 {
  val fichier = "machines.csv"
  val machines = ArrayBuffer[Machine]()

  def main(args: Array[String]): Unit = {
    machines ++= loadcsv(fichier)

    var allumé = true
    while (allumé) {
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choix = readInt()
      if (choix == 1) {
        client()
      } else if (choix == 2) {
        admin()
      } else if (choix == 3) {
        savecsv(fichier, machines)
        Thread.sleep(2000)
        println("Fichier sauvegardé avec succès.")
        println("Au revoir !")
        allumé = false
      } else {
        println("Option invalide. Réessayez.")
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val Machines_existantes = ArrayBuffer[Machine]()
    println("Chargement des machines depuis machines.csv...")
    try {
      val file = Source.fromFile(filename)
      val lignes = file.getLines()
      val texte = lignes.drop(1)
      for (ligne_suivante <- texte) {
        val ingredients = ligne_suivante.split(",")
        if (ingredients.length == 4) {
          Machines_existantes+= new Machine(
            id = Machines_existantes.size + 1,
            pincode = ingredients(0),
            milk = ingredients(1).toInt,
            sugar = ingredients(2).toInt,
            coffee = ingredients(3).toInt
          )
          val nouvelle_Machine = Machines_existantes.last
          println("")
          println("Machine " + nouvelle_Machine.id + " chargée :")
          println("    ID: " + nouvelle_Machine.id)
          println("    Code PIN: " + nouvelle_Machine.pincode)
          println("    Lait: " + "%.3f".format(nouvelle_Machine.milk / 1000.0) + "L")
          println("    Sucre: " + nouvelle_Machine.sugar + "g")
          println("    Café: " + nouvelle_Machine.coffee + "g")
        }
      }
      file.close()
      println("")
      if (Machines_existantes.size == 1) {
        println(Machines_existantes.size + " machine chargée depuis machines.csv.")
      } else if (Machines_existantes.size > 1) {
        println(Machines_existantes.size + " machines chargées depuis machines.csv.")
      } else {
        println("Aucune machine n'a été chargée.")
      }
      println("")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Chargement des machines depuis machines.csv...")
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
      case ex: Exception =>
        println("Erreur technique : " + ex.getMessage)
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    Machines_existantes
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val nouveau_fichier = new PrintWriter("machines.csv")
      nouveau_fichier.println("PINCODE,MILK,SUGAR,COFFEE")
      for (lignes <- machines) {
        nouveau_fichier.println(lignes.pincode + "," + lignes.milk + "," + lignes.sugar + "," + lignes.coffee)
      }
      nouveau_fichier.close()
      if(machines.size == 1){
        println("Sauvegarde de " + machines.size + " machine dans machines.csv...")
      } else if (machines.size > 1){
        println("Sauvegarde de " + machines.size + " machines dans machines.csv...")
      } else {
        println("Aucune machine saubegardée dans machines.csv.")
      }
    } catch {
      case _: java.io.IOException =>
        println("Sauvegarde des machines dans machines.csv...")
        println("Erreur : Échec de l'écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
      case _: Exception =>
        println("Erreur : Impossible de sauvegarder les machines dans machines.csv.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
  }
  def selection_machine(): Option[Machine] = {
    println("Sélectionnez une machine :")
    for (i <- machines.indices) {
      println((i + 1) + ") Machine " + (i+1))
    }
    print("> ")
    var choix = readInt()
    if (choix > 0 && choix <= machines.size) {
      Some(machines(choix - 1))
    } else {
      None
    }
  }
  def client(): Unit = {
    val machine = selection_machine()
    if (machine.isDefined) {
      serveClient(machine.get)
    }
  }
  def admin(): Unit = {
    var selection = selection_machine()
    if (selection.isDefined) {
      val machine = selection.get
      var choix = 0
      while (choix !=1 && choix != 2){
        println("1) Réapprovisionner les stocks")
        println("2) Mettre à jour le code PIN")
        print("> ")
        choix = readInt()
        if (choix == 1) {
          restockMachine(machine)
        } else if (choix == 2) {
          updatePin(machine)
        } else {
          println("Option invalide.")
        }
      }
    }
  }
  def serveClient(machine: Machine): Unit = {
    println("Mode Client pour la Machine " + machine.id)
    println("")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val boisson = readInt()
    var type_boisson = ""
    var qte_cafe = 0
    var qte_lait = 0
    var prix_cafe = 0.0

    if (boisson == 1) {
      qte_cafe = 8
      prix_cafe = 2.0
      type_boisson = "Expresso"
    } else if (boisson == 2) {
      qte_cafe = 6
      qte_lait = 100
      prix_cafe = 2.5
      type_boisson = "Cappuccino"
    } else if (boisson == 3) {
      println("Choisissez la taille :")
      println("1) Petit - 120ml")
      println("2) Moyen - 150ml")
      println("3) Grand - 200ml")
      print("> ")

      val taille = readInt()
      if (taille == 1) {
        qte_cafe = 6
        qte_lait = 120
        prix_cafe = 2.7
      } else if (taille == 2) {
        qte_cafe = 8
        qte_lait = 150
        prix_cafe = 3.2
      } else if (taille == 3) {
        qte_cafe = 12
        qte_lait = 200
        prix_cafe = 3.7
      } else {
        println("Choix invalide, retour au menu principal.")
        return
      }
      type_boisson = "Latte"
    } else {
      println("Choix invalide, retour au menu principal.")
      return
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val taille_sucre = readInt()
    var qte_sucre = 0
    var cout_sucre = 0.0

    if (taille_sucre == 2) {
      qte_sucre = 5
      cout_sucre = 0.10
    } else if (taille_sucre == 3) {
      qte_sucre = 10
      cout_sucre = 0.20
    } else if (taille_sucre == 4) {
      qte_sucre = 15
      cout_sucre = 0.30
    }

    var dose = 0
    var cout_lait = 0.0
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? ")
      println("1) Oui")
      println("2) Non")
      print("> ")

      val choix_dose = readInt()
      if (choix_dose == 1) {
        print("Combien de dose > ")
        dose = readInt()
        cout_lait = dose * 0.05
        qte_lait += dose * 50
      }
    }

    val prix_total = prix_cafe + cout_sucre + cout_lait

    if (machine.coffee >= qte_cafe && machine.sugar >= qte_sucre && machine.milk >= qte_lait) {
      println(f"Prix total : CHF $prix_cafe%.2f + CHF ${(cout_sucre + cout_lait)}%.2f = CHF $prix_total%.2f")
      println("Veuillez payer en utilisant Twint.")
      val valeur = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var twint_code = ""
      var index = 0
      while (index < 5) {
        twint_code = twint_code + valeur(scala.util.Random.nextInt(valeur.length))
        index = index + 1
      }
      println("Votre code de paiement est : " + twint_code)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)

      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson ...")
      Thread.sleep(2000)
      println("[...]")
      println("Votre " + type_boisson + " est prêt ! Bonne dégustation !")

      machine.removeIngredient("coffee", qte_cafe)
      machine.removeIngredient("sugar", qte_sucre)
      machine.removeIngredient("milk", qte_lait)
    } else {
      if (machine.coffee < qte_cafe) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      }
      if (machine.sugar < qte_sucre) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      }
      if (machine.milk < qte_lait) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      }
      println("Veuillez choisir une autre boisson ou essayer une autre machine.")
    }
  }
  def restockMachine(machine: Machine): Unit = {
    println("Niveaux de stock actuels :")
    println("    Poudre de café : " + machine.coffee + "g")
    println("    Sucre : " + machine.sugar + "g")
    println("    Lait: " + "%.3f".format(machine.milk / 1000.0) + "L")
    println("")
    println("Que souhaitez-vous faire :")
    println("1) Ajouter stock")
    println("2) Enlever stock")
    print("> ")
    var choix = 0

    while (choix != 1 && choix != 2) {
      choix = readInt()
      if (choix == 1) {
        println("Entrez les quantités à ajouter :")
        print("Poudre de café > ")
        machine.addIngredient("coffee", readInt())
        print("Sucre > ")
        machine.addIngredient("sugar", readInt())
        print("Lait > ")
        val lait_conversion = (readDouble() * 1000).toInt
        machine.addIngredient("milk", lait_conversion)
        println("Les stocks ont été mis à jour avec succès.")
      } else if (choix == 2) {
        println("Entrez les quantités à enlever :")
        print("Poudre de café > ")
        var cafe_retiré = readInt()
        if (cafe_retiré > machine.coffee) {
          println("Impossible d'enlever " + cafe_retiré + "g de café. Stock insuffisant")
        } else {
          machine.removeIngredient("coffee", cafe_retiré)
        }
        print("Sucre > ")
        var sucre_retiré = readInt()
        if (sucre_retiré > machine.sugar) {
          println("Impossible d'enlever " + sucre_retiré + "g de sucre. Stock insuffisant")
        } else {
          machine.removeIngredient("sugar", sucre_retiré)
        }
        print("Lait > ")
        var lait_retiré = (readDouble() * 1000).toInt
        if (lait_retiré > machine.milk) {
          println("Impossible d'enlever " + lait_retiré/1000 + "L de lait. Stock insuffisant")
        } else {
          machine.removeIngredient("milk", lait_retiré)
        }
        println("Les stocks ont été mis à jour avec succès.")
      } else {
        println("Option invalide.")
      }
    }
    println("Niveaux de stocks mis à jour :")
    println("    Poudre de café : " + machine.coffee + "g")
    println("    Sucre : " + machine.sugar + "g")
    println("    Lait: " + "%.3f".format(machine.milk / 1000.0) + "L")
    println("")
    println("Retour au menu principal...")
  }
  def updatePin(machine: Machine): Unit = {
    println("Mise à jour du code PIN pour la Machine " + machine.id + ".")
    var nouveauPin = ""
    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machine.pincode = nouveauPin
    println("Le code PIN pour la machine " + machine.id + " a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
}