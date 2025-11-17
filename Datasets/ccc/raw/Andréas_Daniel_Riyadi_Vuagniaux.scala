import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.io.FileNotFoundException

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
	def addIngredient(ingredient: String, amount: Int): Unit = {
	  ingredient.toLowerCase match {
		case "milk"   => milk += amount
		case "sugar"  => sugar += amount
		case "coffee" => coffee += amount
	  }
	}
  
	def removeIngredient(ingredient: String, amount: Int): Boolean = {
	  ingredient.toLowerCase match {
		case "milk" if milk >= amount =>
		  milk -= amount
		  true
		case "sugar" if sugar >= amount =>
		  sugar -= amount
		  true
		case "coffee" if coffee >= amount =>
		  coffee -= amount
		  true
		case _ => false
	  }
	}
  }


object main {
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
  	println("Entrez le code PIN :")
  	val enteredPin = readLine().toInt
  	if (enteredPin.toString != machines(machineId).pincode) {
  	  false
  	} else {
  	  true
  	}
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
  	var validPin = false
  	while (!validPin) {
  	  println("Entrez le nouveau code PIN à 6 chiffres:")
  	  val newPin = readLine().toInt
  	  if (newPin.toString.length == 6) {
  	    validPin = true
  	    machines(machineId).pincode = newPin.toString
  	    println("Le code PIN a été mis à jour avec succès.")
  	  } else {
  	    println("Erreur : Le code PIN doit contenir exactement 6 chiffres.")
  	  }
  	}
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
		val machines = ArrayBuffer[Machine]()
		println("Chargement des machines depuis " + filename + "...")
		try {
		  val lines = Source.fromFile(filename).getLines().toArray
		  var index = 1
		  for (line <- lines.drop(1)) {
			val Array(pincode, milk, sugar, coffee) = line.split(",")
			val machine = new Machine(index, pincode, milk.toInt, sugar.toInt, coffee.toInt)
			machines += machine
			println("Machine " + machine.id + " chargée :")
			println("ID: " + machine.id)
			println("Code PIN: " + machine.pincode)
			println("Lait: " + "%.3f".format(machine.milk / 1000.0) + "L")
			println("Sucre: " + machine.sugar + "g")
			println("Café: " + machine.coffee + "g")
			index += 1
		  }
		  println(machines.length + " machine(s) chargée(s) avec succès.")
		  machines
		} catch {
			case _: FileNotFoundException if !Files.exists(Paths.get(filename)) =>
			  println("Erreur : Fichier " + filename + " introuvable. Vérifiez le chemin d’accès et réessayez.")
			  new ArrayBuffer[Machine]()
		  
			case _: java.io.IOException if Files.exists(Paths.get(filename)) && !Files.isReadable(Paths.get(filename)) =>
			  println("Erreur : Fichier " + filename + " ne peut pas être accédé en raison de problèmes de permissions.")
			  new ArrayBuffer[Machine]()
		  
			case e: Exception =>
			  println("Erreur : Impossible de lire " + filename + " : " + e.getMessage)
			  new ArrayBuffer[Machine]()
		}
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
		println("Sauvegarde de " + machines.length + " machine(s) dans " + filename + "...")
		try {
		  val writer = new PrintWriter(new File(filename))
		  writer.println("PINCODE,MILK,SUGAR,COFFEE")
		  for (machine <- machines) {
			writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
		  }
		  writer.close()
		  println("Fichier sauvegardé avec succès.")
		} catch {
		  case e: Exception =>
			println("Erreur : Échec de l’écriture dans " + filename + ". " + e.getMessage)
		}
	  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

  var boisson = ""
  var boissonPrix = 0.00
  var cafeUtiliser = 0
  var laitUtiliser = 0

  var sucreNivNom = ""
  var sucreUtiliser = 0
  var sucrePrix = 0.00

  var laitSupNom = ""
  var dosePrix = 0.00

  var erreur = false
  println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
  var choixBoisson = readLine(">").toInt
  while ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) {
    println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
    choixBoisson = readLine(">").toInt
  }
  if (choixBoisson == 1) {
    boisson = "Expresso"
    boissonPrix = 2.00
    cafeUtiliser = 8
    laitUtiliser = 0
  }

  else if (choixBoisson == 2) {
    boisson = "Cappuccino"
    boissonPrix = 2.50
    cafeUtiliser = 6
    laitUtiliser = 100
  }
  else {
    println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
    var taille = readLine(">").toInt
    while ((taille != 1) && (taille != 2) && (taille != 3)) {
      println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
      taille = readLine(">").toInt
    }
    boisson = "Latte (Petit)"
    boissonPrix = 2.70
    cafeUtiliser = 6
    laitUtiliser = 120
    if (taille == 2) {
      boisson = "Latte (Moyen)"
      boissonPrix = 3.20
      cafeUtiliser = 8
      laitUtiliser = 150
    }
    else if (taille == 3) {
      boisson = "Latte (Grand)"
      boissonPrix = 3.70
      cafeUtiliser = 12
      laitUtiliser = 200
    }
  }
  println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
  var sucreNiv = readLine(">").toInt
  while ((sucreNiv != 1) && (sucreNiv != 2) && (sucreNiv != 3) && (sucreNiv != 4)) {
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    sucreNiv = readLine(">").toInt
  }
  if (sucreNiv == 1) {
    sucreNivNom = "Sans Sucre"
    sucreUtiliser = 0
  }

  if (sucreNiv == 2) {
    sucreNivNom = "Peu (5g)"
    sucreUtiliser = 5
    sucrePrix = 0.10
  }
  else if (sucreNiv == 3) {
    sucreNivNom = "Moyen (10g)"
    sucreUtiliser = 10
    sucrePrix = 0.20
  }
  else if (sucreNiv == 4) {
    sucreNivNom = "Grand (15g)"
    sucreUtiliser = 15
    sucrePrix = 0.30
  }
  if ((choixBoisson == 2) || (choixBoisson == 3)) {
    println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
    var laitSup = readLine("").toInt
    while ((laitSup != 1) && (laitSup != 2)) {
      println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
      laitSup = readLine("").toInt
    }

    if (laitSup == 1) {
      laitSupNom = "Oui"
      println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
      var dose = readLine(">").toInt
      while ((dose != 0) && (dose != 1) && (dose != 2) && (dose != 3)) {
        println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
        dose = readLine(">").toInt
      }
      laitUtiliser += dose * 50
      dosePrix = dose * 0.05
    }
    else {
      laitSupNom = "Non"
      dosePrix = 0.00
    }
  }

  // Gestion de stock et d'erreur
  if (machines(machineId).coffee < cafeUtiliser) {
    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    erreur = true
  }
  if (machines(machineId).sugar < sucreUtiliser) {
    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    erreur = true
  }
  if (machines(machineId).milk < laitUtiliser) {
    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    erreur = true
  }

  if (!erreur) {
    println("Boisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucreNivNom + "\nLait en supplément : " + laitSupNom)
    val prixFinal = boissonPrix + sucrePrix + dosePrix
    println("Prix total : CHF " + prixFinal)

    // Génération du code de paiement Twint
    val Alphanumerique = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var codetwint = ""
    for (i <- 0 to 4) {
      codetwint = codetwint + Alphanumerique((math.random() * Alphanumerique.length).toInt)
    }
    println(" Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...\n[...]\nVotre " + boisson + " est prêt ! Bonne dégustation !")

    // Mise à jour des stocks
    machines(machineId).coffee -= cafeUtiliser
    machines(machineId).sugar -= sucreUtiliser
    machines(machineId).milk -= laitUtiliser
    true
  }
  else {
    println("Sélectionner une autre machine.")
    false
  }
}


  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
  val machine = machines(machineId)

  println("\tPoudre de café : ")
  var ajoutPoudre = -1
  while (ajoutPoudre < 0) {
    val input = readLine()
    ajoutPoudre = try {
      input.toInt
    } catch {
      case _: NumberFormatException => -1
    }

    if (ajoutPoudre < 0) {
      if (!machine.removeIngredient("coffee", -ajoutPoudre)) {
        ajoutPoudre = -1
      } else {
        ajoutPoudre = 0
      }
    } else {
      machine.addIngredient("coffee", ajoutPoudre)
    }
  }

  println("\tLait : ")
  var ajoutlait = -1.0
  while (ajoutlait < 0.0) {
    val input = readLine()
    ajoutlait = try {
      input.toDouble
    } catch {
      case _: NumberFormatException => -1.0
    }

    if (ajoutlait < 0.0) {
      if (!machine.removeIngredient("milk", (-ajoutlait * 1000).toInt)) {
        ajoutlait = -1.0
      } else {
        ajoutlait = 0.0
      }
    } else {
      machine.addIngredient("milk", (ajoutlait * 1000).toInt)
    }
  }

  println("\tSucre : ")
  var ajoutSucre = -1
  while (ajoutSucre < 0) {
    val input = readLine()
    ajoutSucre = try {
      input.toInt
    } catch {
      case _: NumberFormatException => -1
    }

    if (ajoutSucre < 0) {
      if (!machine.removeIngredient("sugar", -ajoutSucre)) {
        ajoutSucre = -1
      } else {
        ajoutSucre = 0
      }
    } else {
      machine.addIngredient("sugar", ajoutSucre)
    }
  }

  println("Réapprovisionnement terminé.")
}


  def main(args: Array[String]): Unit = {
  val filename = "machines.csv"
  val machines = loadcsv(filename)
  if (machines.length == 0){
	return
  }

  var quitter = 0
  var idMachine = 0
  var nbMachines = machines.length
  var choixMenu: Int = 0
  var choix1: Double = 0
  var choix2: Double = 0

  while (quitter == 0) {
    println("\nNospresso Cafe")
    println("Veuillez choisir votre mode :")
    println("1) Mode Client")
    println("2) Mode Admin")
    println("3) Quitter")
    do {
      choixMenu = readLine(">").toInt
    } while (!(choixMenu == 1 || choixMenu == 2 || choixMenu == 3))
    if (choixMenu == 3) {
      quitter = 1
    } else if (choixMenu == 2) {
      println("Mode Admin")
      do {
        choix1 = readLine("Machine sélectionnée (1-" + nbMachines + ") >").toDouble
      } while (choix1 < 1 || choix1 > nbMachines)
      idMachine = choix1.toInt - 1

      if (validatePin(idMachine, machines)) {
        println("Accès accordé à la Machine " + (idMachine + 1))
        do {
          println("1) Rapprovisionnement des ingrédients")
          println("2) Mise à jour du code PIN")
          choix2 = readLine(">").toInt
        } while (choix2 != 1 && choix2 != 2)
        if (choix2 == 1) {
          restockMachine(idMachine, machines)
        } else {
          updatePin(idMachine, machines)
        }
      } else {
        println("Code PIN incorrect. Accès refusé.")
      }
    } else if (choixMenu == 1) {
      do {
        choix1 = readLine("Machine sélectionnée (1-" + nbMachines + ") >").toDouble
      } while (choix1 < 1 || choix1 > nbMachines)
      idMachine = choix1.toInt - 1
      var checker = false
      do {
        checker = serveClient(idMachine, machines)
        if (!checker) {
          println("\nLa transaction n'a pas pu être effectuée en raison d'un stock insuffisant.")
          println("Veuillez sélectionner une autre machine.")
          do {
            choix1 = readLine("Sélectionnez une machine (1-" + nbMachines + ") > ").toInt
          } while (choix1 < 1 || choix1 > nbMachines)
          idMachine = choix1.toInt - 1
        }
      } while (!checker)
    }
  }

  savecsv(filename, machines)
}

}