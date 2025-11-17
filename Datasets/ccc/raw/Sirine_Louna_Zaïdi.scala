import scala.io.StdIn._
import util.Random
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Double, var sugar: Double, var coffee: Double) {
  def addIngredient(ingredient: String, amount: Double): Unit = {
    val ingredientName = ingredient.trim.toLowerCase()
    if (ingredientName == "milk") {
      milk += amount
      println(s"Ajouté $amount de lait. Nouveau total : $milk")
    } else if (ingredientName == "sugar") {
      sugar += amount
      println(s"Ajouté $amount de sucre. Nouveau total : $sugar")
    } else if (ingredientName == "coffee") {
      coffee += amount
      println(s"Ajouté $amount de café. Nouveau total : $coffee")
    } else {
      println(s"Erreur : $ingredient n'est pas un ingrédient reconnu.")
    }
  }



  def removeIngredient(ingredient: String, amount: Double): Boolean = {
    val ingredientName = ingredient.trim.toLowerCase()
    if (ingredientName == "milk") {
      if (amount <= milk) {
        milk -= amount
        println(s"Retiré $amount de lait. Restant : $milk")
        true
      } else {
        println(s"Lait insuffisant. Vous avez $milk, mais il faut $amount.")
        false
      }
    } else if (ingredientName == "sugar") {
      if (amount <= sugar) {
        sugar -= amount
        println(s"Retiré $amount de sucre. Restant : $sugar")
        true
      } else {
        println(s"Sucre insuffisant. Vous avez $sugar, mais il faut $amount.")
        false
      }
    } else if (ingredientName == "coffee") {
      if (amount <= coffee) {
        coffee -= amount
        println(s"Retiré $amount de café. Restant : $coffee")
        true
      } else {
        println(s"Café insuffisant. Vous avez $coffee, mais il faut $amount.")
        false
      }
    } else {
      println(s"Erreur : $ingredient ne peut pas être retiré. Ce n'est pas un ingrédient valide.")
      false
    }
  }

}



def loadcsv(filename: String): ArrayBuffer[Machine] = {
  val machine = ArrayBuffer[Machine]()
  try {
    println("Chargement des machines depuis machines.csv...")
    val lines = Source.fromFile(filename).getLines()
    lines.next() // Skip header
    for ((line, index) <- lines.zipWithIndex) {
      val Array(pin, milk, sugar, coffee) = line.split(",")
      machine += new Machine(index + 1, pin, milk.toDouble, sugar.toDouble, coffee.toDouble)
      println(s"Machine ${index + 1} chargée :")
      println("ID: " + machine.last.id + ", Code PIN: " + machine.last.pincode + ", Lait: " + machine.last.milk + "L, Surcre:  " + machine.last.sugar + "g, Café: " + machine.last.coffee + "g")

    }
    println(s"${machine.size} machine(s) chargée(s) avec succès.")
  } catch {
    case _: Exception =>
      println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      System.exit(1)
  }
  machine
}



def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
  try {
    val file  = new PrintWriter(new File(filename))
    file .println("PINCODE,MILK,SUGAR,COFFEE")
    machines.foreach { machine =>
      file .println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
    }
    file .close()
    println(s"Sauvegarde de ${machines.size} machine(s) dans $filename réussie.")
  } catch {
    case _: Exception =>
      println("Erreur : Échec de l’écriture dans le fichier. Le fichier peut être verrouillé ou en lecture seule.\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      System.exit(1)
  }
}




object Principal {
  var nbMachines = -1

  // stock initial de toutes les machines
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    machines = loadcsv("machines.csv")
    nbMachines = machines.length
    menuPrincipal()
  }



  // Affichage du menu principal et les choix de l'utilisateur
  def menuPrincipal(): Unit = {
    var choix = -1
    while (choix != 3) {  // Boucle jusqu'à ce que l'utilisateur choisisse de quitter
      println("\n========= Nospresso Café =========")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      choix = readInt()

      if (choix == 1) {
        println("Sélectionnez le numéro de la machine (1 à 5) :")
        val idMachine = readInt() - 1
        if (idMachine >= 0 && idMachine < nbMachines) {
          serveClient(idMachine, machines)
        } else {
          println("Numéro de machine invalide.")
        }
      } else if (choix == 2) {
        println("Sélectionnez le numéro de la machine (1 à 5) :")
        val idMachine = readInt() - 1
        if (idMachine >= 0 && idMachine < nbMachines) {
          if (validatePin(idMachine, machines)) {
            modeAdmin(idMachine)
          }
        } else {
          println("Numéro de machine invalide.")
        }
      } else if (choix == 3) {
        quitter()
      } else {
        println(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).")
      }
    }
  }




  // Validation du code PIN pour l'accès admin
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {  // Boucle jusqu'à ce que le PIN soit correct ou que les trois tentatives soient épuisées
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val pin = readLine()
      if (pin == machines(machineId).pincode) {
        println(">> Accès autorisé.")
        return true
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println(s"Code PIN incorrect. $tentatives tentatives restantes.")
        } else {
          println("Code PIN incorrect. 0 tentatives restantes.")
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(0)
        }
      }
    }
    false
  }

  // mise à jour du code PIN pour les machines
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${machineId + 1}.")
    var nouveauPin = ""
    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {  // Boucle jusqu'à ce qu'un PIN valide soit entré
      println("Entrez un nouveau code PIN à 6 chiffres :")
      nouveauPin = readLine()
      if (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
        println("Le code PIN doit comporter exactement 6 chiffres.")
      }
    }
    machines(machineId).pincode = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }




  // servir le client et sélection des boissons
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var boisson = -1
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")


    while (boisson < 1 || boisson > 3) {  // Boucle jusqu'à ce qu'une boisson valide soit sélectionné

      boisson = readInt()
      if (boisson < 1 || boisson > 3) {
        println(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).")
      }

    }


    var taille = 0
    if (boisson == 3) {
      println("Veuillez choisir la taille du Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")

      while (taille < 1 || taille > 3) {  // Boucle jusqu'à ce qu'une taille valide soit sélectionné
        taille = readInt()
        if (taille < 1 || taille > 3) {
          println(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).")
        }

      }

    }

    var sucre = -1
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    while (sucre < 1 || sucre > 4) {  // Boucle jusqu'à ce qu'un niveau de sucre valide soit sélectionné
      sucre = readInt()
      if (sucre < 1 || sucre > 4) {
        println(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2, 3 ou 4).")
      }
    }

    var lait = 0
    if (boisson >= 2) {
      println("Souhaitez-vous ajouter du lait en supplément ? (1) Oui (2) Non")
      if (readInt() == 1) {
        println("Combien de doses ? (Max 3 doses)")
        lait = readInt()
        if (lait > 3) lait = 3
        lait *= 50
      }
    }


    // Vérification des stocks
    var stocksuffisant = true
    var messageErreur = ""
    if (boisson == 1) {
      if (machines(machineId).coffee < 8) {
        stocksuffisant = false
        messageErreur += "Erreur : Quantité de poudre de café insuffisante pour préparer l'Expresso.\n"
      }

    } else if (boisson == 2) {
      if (machines(machineId).coffee < 6 || machines(machineId).milk < 100 + lait) {
        stocksuffisant = false
        messageErreur += "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer le Cappuccino.\n"
      }

    } else if (boisson == 3) {
      var laitNecessaire = 0
      if (taille == 1) laitNecessaire = 120
      else if (taille == 2) laitNecessaire = 150
      else if (taille == 3) laitNecessaire = 200
      if (machines(machineId).coffee < 6 || machines(machineId).milk < laitNecessaire + lait) {
        stocksuffisant = false
        messageErreur += "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer le Latte.\n"
      }
    }

    var quantiteSucre = if (sucre > 1) (sucre - 1) * 5 else 0
    if (machines(machineId).sugar < quantiteSucre) {
      stocksuffisant = false
      messageErreur += "Erreur : Quantité de sucre insuffisante.\n"
    }

    if (!stocksuffisant) {
      println(messageErreur)
      return false
    }


    // Calcul du prix
    var prix: Double = 0.0
    var prixTotal: Double = 0.0
    var calculSucre: Double = 0.0
    calculSucre = (sucre - 1) * 0.10
    if (boisson == 1) prix = 2.00
    else if (boisson == 2) prix = 2.50
    else if (boisson == 3) {
      if (taille == 1) prix = 2.70
      else if (taille == 2) prix = 3.20
      else if (taille == 3) prix = 3.70
    }

    var prixLaitTotal: Double = 0.0
    if (lait > 0) prixLaitTotal = (lait / 50) * 0.05
    prixTotal = prix + calculSucre + prixLaitTotal


    // Affichage du recapitulatif de la commande du client
    println("\nRécapitulatif de votre commande:")
    println(s"Boisson sélectionnée : ${if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte"}")
    println(s"Niveau de sucre : ${if (sucre == 1) "Sans Sucre" else if (sucre == 2) "Peu (5g)" else if (sucre == 3) "Moyen (10g)" else "Beaucoup (15g)"}")
    if (boisson >= 2) {
      println(s"Lait supplémentaire : ${lait / 50} doses")
    }

    println(f"Prix total : CHF $prixTotal%.2f")
    val codeTwint = "AB12X"
    println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $codeTwint")
    println("En attente de validation du paiement...")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")


    // Mise à jour des stocks après la commande
    if (boisson == 1) {
      machines(machineId).removeIngredient("coffee", 8)
    } else if (boisson == 2) {
      machines(machineId).removeIngredient("coffee", 6)
      machines(machineId).removeIngredient("milk", (100 + lait))
    } else if (boisson == 3) {
      var laitNecessaire = 0
      if (taille == 1) laitNecessaire = 120
      else if (taille == 2) laitNecessaire = 150
      else if (taille == 3) laitNecessaire = 200
      machines(machineId).removeIngredient("coffee", 6)
      machines(machineId).removeIngredient("milk", (laitNecessaire + lait))
    }
    if (sucre > 1) {
      machines(machineId).removeIngredient("sugar",quantiteSucre)
    }

    println("Préparation de votre boisson... Votre boisson est prête ! Bonne dégustation !")
    true
  }


  // Réapprovisionne les stocks de la machine
  def restockMachine(machineId: Int, machines : ArrayBuffer[Machine]): Unit = {
    println(s"Réapprovisionnement des stocks pour la machine ${machineId + 1}.")
    println(s"Stocks actuels : Café: ${machines(machineId).coffee} g, Sucre: ${machines(machineId).sugar} g, Lait: ${machines(machineId).milk} ml")
    println("Entrez les quantités à ajouter (café, sucre, lait en millilitres) :")
    val ajoutCafe = readInt()
    val ajoutSucre = readInt()
    val ajoutLait = readInt()
    machines(machineId).addIngredient("coffee",ajoutCafe)
    machines(machineId).addIngredient("sugar",ajoutSucre)
    machines(machineId).addIngredient("milk",ajoutLait)
    println(s"Les stocks ont été mis à jour avec succès. Nouveaux stocks : Café: ${machines(machineId).coffee} g, Sucre: ${machines(machineId).sugar} g, Lait: ${machines(machineId).milk} ml")
  }


  // Mode administrateur pour gérer les options admin
  def modeAdmin(idMachine: Int): Unit = {
    println(s"Mode Administrateur pour la machine ${idMachine + 1}.")
    var adminChoice = -1
    while (adminChoice != 3) {  // Boucle jusqu'à ce que l'administrateur choisisse de retourner au menu principal
      println("1) Réapprovisionnement des stocks")
      println("2) Mise à jour du code PIN")
      println("3) Retour au menu principal")
      adminChoice = readInt()
      if (adminChoice == 1) {
        restockMachine(idMachine, machines)

      } else if (adminChoice == 2) {
        updatePin(idMachine, machines)
        return // Retour au menu principal après la mise à jour du code PIN

      } else if (adminChoice == 3) {
        println("Retour au menu principal.")

      } else {
        println(">> Valeur non autorisée. Veuillez saisir un nombre (1, 2 ou 3).")

      }
    }
  }

  // Quitte le programme
  def quitter(): Unit = {
    savecsv("machines.csv",machines)
    println("Au revoir...")

  }
}

