import scala.io.{Source, StdIn}
import scala.util.{Random, Using}
import scala.collection.mutable.ArrayBuffer
import java.io.{BufferedWriter, FileWriter, IOException}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    } else {
      println("Invalid ingredient.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println(s"Not enough $ingredient.")
      false
    }
  }
}

object Main {
  val machines = ArrayBuffer[Machine]()

  def main(args: Array[String]): Unit = {
    machines ++= loadcsv("machines.csv")
    do {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choixMenu = StdIn.readInt() // choix de l'utilisateur du mode qu'il veut

      if (choixMenu == 1) { // mode client
        val machineId = selectMachine()
        serveClient(machineId)

      } else if (choixMenu == 2) { // mode admin
        val machineId = selectMachine()
        if (validatePin(machineId)) {
          ModeAdmin(machineId)
        } else {
          return
        }

      } else if (choixMenu == 3) { // Quitter
        println("Merci et à bientôt !")
        savecsv("machines.csv", machines)
        return

      } else {
        println("Option invalide. Veuillez sélectionner un choix valide.")
      }
    } while (true)
  }

  def selectMachine(): Int = {
    var machineId = -1
    while (machineId <= 0 || machineId > machines.length) {
      println(s"Veuillez sélectionner une machine 1 à ${machines.length}")
      machineId = StdIn.readInt()
      if (machineId <= 0 || machineId > machines.length) {
        println("Machine invalide. Veuillez réessayer.")
      }
    }
    machineId - 1
  }

  def validatePin(machineId: Int): Boolean = {
    println("Machine sélectionnée > " + (machineId + 1))
    println("Mode admin")

    val tentativesMax = 3 // Nombre maximum de tentatives autorisées
    var tentative = 0 // Compteur de tentatives
    var accessAutorise = false

    while (!accessAutorise && tentative < tentativesMax) {
      println(s"Veuillez entrer votre code PIN (tentative ${tentative + 1}/3)")
      val pinEntree = StdIn.readLine() // Lire l'entrée de l'utilisateur

      if (pinEntree == machines(machineId).pincode) {
        accessAutorise = true // Retourner immédiatement la validation réussie
        println("Accès autorisé. Bienvenue !")
        return true
      } else {
        tentative += 1
        if (tentative < tentativesMax) {
          println("Code PIN incorrect. Veuillez réessayer.")
        }
      }
    }

    if (!accessAutorise) {
      println("Nombre maximum de tentatives dépassé. Accès refusé.")
      return false // Retour immédiat de l'échec
    }
    accessAutorise
  }

  def updatePin(machineId: Int): Unit = {
    println("Machine sélectionnée > " + (machineId + 1))
    var newPin = ""
    do {
      print("Entrez un nouveau code PIN à 6 chiffres : ")
      newPin = StdIn.readLine()
      if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        println("Le code PIN doit comporter exactement 6 chiffres.")
      }
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))

    machines(machineId).pincode = newPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def restockMachine(machineId: Int): Unit = {
    println("Machine sélectionnée > " + (machineId + 1))
    println("Mode réapprovisionnement.\n")
    println("Niveaux de stock actuels:")
    val machine = machines(machineId)
    println(s"Poudre de café : ${machine.coffee}g")
    println(f"Lait : ${machine.milk / 1000.0}%.3fl")
    println(s"Sucre : ${machine.sugar}g")

    println("\nRéapprovisionnement des stocks...\nEntrez les quantités à ajouter : ")

    println("Poudre de café >")
    machine.addIngredient("coffee", StdIn.readLine().toInt)
    println("Lait >")
    machine.addIngredient("milk", StdIn.readLine().toInt)
    println("Sucre >")
    machine.addIngredient("sugar", StdIn.readLine().toInt)

    println("Niveaux de stocks mis à jour.\nRetour au menu principal...")
  }

  def serveClient(machineId: Int): Unit = {
    println("Machine sélectionnée > " + (machineId + 1))
    val codeTwint = Random.alphanumeric.take(5).mkString // génère aléatoirement un string avec 5 caractère

    var choixBoisson = 0
    do {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")

      choixBoisson = StdIn.readInt() // choix de l'utilisateur de la boisson qu'il veut

      if (choixBoisson == 1) { // choix de l'expresso
        Expresso(machineId, codeTwint)
      } else if (choixBoisson == 2) { // choisir la boisson cappuccino
        Cappuccino(machineId, codeTwint)
      } else if (choixBoisson == 3) { // choisir la boisson latte
        Latte(machineId, codeTwint)
      } else {
        println("Option invalide. Veuillez sélectionner un choix valide.")
      }
    } while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)
  }

  def ModeAdmin(machineId: Int): Unit = {
    println("Machine sélectionnée > " + (machineId + 1))
    println("1) Reapprovisionner")
    println("2) Mettre à jour le code PIN")
    print("> ")

    val choixDeAdmin = StdIn.readInt()

    if (choixDeAdmin == 1) {
      restockMachine(machineId)
    } else if (choixDeAdmin == 2) {
      updatePin(machineId)
    } else {
      println("Option invalide, retour au menu principal.")
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machinesBuffer = ArrayBuffer[Machine]()
    try {
      Using(Source.fromFile(filename)) { source =>
        val lines = source.getLines().drop(1) // Ignorer la ligne d'en-tête
        for ((line, index) <- lines.zipWithIndex) {
          val Array(pincode, milk, sugar, coffee) = line.split(",")
          machinesBuffer += new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt) // chargé les valeurs du fichier.csv
          println(s"Chargement des machines depuis $filename ...\n")
          Thread.sleep(1000)
          println(s"Machine ${index + 1} chargée ")
          println(s"Id : ${index + 1}")
          println(s"Poudre de café : ${coffee}g")
          println(f"Lait : ${milk.toInt / 1000.0}%.3fl")
          println(s"Sucre : ${sugar}g\n")
        }
      }.get

      println(s"${machinesBuffer.length} machine(s) chargée(s) avec succès.\n")
    } catch {
      case _: IOException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        System.exit(1)
      case e: Exception =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        System.exit(1)
    }
    machinesBuffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      Using(new BufferedWriter(new FileWriter(filename))) { writer =>
        writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
        for (machine <- machines) {
          writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n") //mnodifer le fichier.csv et sauvgarder
        }
      }.get
      println(s"Sauvegarde de ${machines.length} machines dans machines.csv...\nFichier sauvegardé avec succès.")
    } catch {
      case _: IOException =>
        println("Erreur : Échec de l’écriture dans machines.csv.\\n Le fichier peut être verrouillé ou en lecture seule.")
      case e: Exception =>
        println(s"Erreur : Échec du chargement ou de la sauvegarde des machines.\n Fermeture du programme. Details: ${e.getMessage}")
    }
  }

  def Expresso(machineId: Int, codeTwint: String): Unit = {  // choix de l'expresso
    val machine = machines(machineId)
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val choixDuSucre = StdIn.readInt() // choix du sucre ou non
    var quantiteSucre = 0
    var prixDuSucre = 0.0

    if (choixDuSucre == 1) { // pas de sucre
      quantiteSucre = 0
      prixDuSucre = 0.0
    } else if (choixDuSucre == 2) { // peu de sucre
      quantiteSucre = 5
      prixDuSucre = 0.10
    } else if (choixDuSucre == 3) { // moyen de sucre
      quantiteSucre = 10
      prixDuSucre = 0.20
    } else if (choixDuSucre == 4) { // beaucoup de sucre
      quantiteSucre = 15
      prixDuSucre = 0.30
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return
    } // Boucle jusqu'à ce que l'on choisisse une option qui est disponible

    if (machine.removeIngredient("sugar", quantiteSucre) && machine.removeIngredient("coffee", 8)) { //déduire du stock de café et sucre
      val prixTotal = 2.00 + prixDuSucre
      printf("prix : CHF 2.00 + CHF %.2f = CHF %.2f\n", prixDuSucre, prixTotal) // calculer et afficher le prix total de l'expresso
      println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codeTwint")
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé.\nPréparation de votre boisson...")
      println("Votre Expresso est prêt ! Bonne dégustation !")
    }
  }

  def Cappuccino(machineId: Int, codeTwint: String): Unit = { // choisir la boisson cappucinno
    val machine = machines(machineId)
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val choixDuSucre = StdIn.readInt()  // choisir si l'on veut du sucre ou non
    var quantiteSucre = 0
    var prixDuSucre = 0.0

    if (choixDuSucre == 1) { // pas de sucre
      quantiteSucre = 0
      prixDuSucre = 0.0
    } else if (choixDuSucre == 2) { // peu de sucre
      quantiteSucre = 5
      prixDuSucre = 0.10
    } else if (choixDuSucre == 3) { // moyen de sucre
      quantiteSucre = 10
      prixDuSucre = 0.20
    } else if (choixDuSucre == 4) { // beaucoup de sucre
      quantiteSucre = 15
      prixDuSucre = 0.30
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return
    }  // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide

    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) Oui")
    println("2) Non")
    print("> ")

    val choixDeLait = StdIn.readInt() // la valeur pour chosir si l'on veut du lait ou non
    var dosesDeLait = 0
    var prixDuLaitSuppl = 0.0

    if (choixDeLait == 1) {  // avec du lait
      do {
        println("Combien de dose (1 dose = 50ml, maximum 3 doses) ?")
        print("> ")
        dosesDeLait = StdIn.readInt() // déclare la variable à l'éxterieur de le boucle

        if (dosesDeLait > 0 && dosesDeLait <= 3) { // nombres de doses comprisent entre 1 et 3
          if (machine.milk >= dosesDeLait * 50) {
            machine.removeIngredient("milk", dosesDeLait * 50) // le stock du lait sera soustraire de 0.05L
            prixDuLaitSuppl = dosesDeLait * 0.05 // le prix du lait égale 0.05chf par doses
          } else {
            println("Stock insuffisant de lait.")
            return
          }
        } else {
          println("Option invalide. Veuillez sélectionner un choix valide.")
        }
      } while (dosesDeLait < 1 || dosesDeLait > 3)
    } else if (choixDeLait == 2) { // sans lait
      // No additional milk
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return
    }

    if (machine.removeIngredient("sugar", quantiteSucre) && machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)) { //déduire du stock du sucre,café et lait
      val prixTotal = 2.50 + prixDuSucre + prixDuLaitSuppl // calculer et afficher le prix total du cappuccino
      printf("prix : CHF 2.50 + CHF %.2f + CHF %.2f = CHF %.2f\n", prixDuSucre, prixDuLaitSuppl, prixTotal)
      println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codeTwint")
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé.\nPréparation de votre boisson...")
      println("Votre Cappuccino est prêt ! Bonne dégustation !")
    }
  }

  def Latte(machineId: Int, codeTwint: String): Unit = { // choisir la boisson latte

    val machine = machines(machineId)
    println("Sélectionnez la taille :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    print("> ")

    val tailleDeLaBoisson = StdIn.readInt()
    var prixTaille = 0.0
    var quantiteDeCafe = 0
    var quantiteDeLait = 0

    if (tailleDeLaBoisson == 1) { // prix et quantité nécessaire pour le petit latte
      prixTaille = 2.70
      quantiteDeCafe = 6
      quantiteDeLait = 120
    } else if (tailleDeLaBoisson == 2) { // prix et quantité nécessaire pour le moyen latte
      prixTaille = 3.20
      quantiteDeCafe = 8
      quantiteDeLait = 150
    } else if (tailleDeLaBoisson == 3) { // prix et quantité nécessaire pour le grand latte
      prixTaille = 3.70
      quantiteDeCafe = 12
      quantiteDeLait = 200
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val choixDuSucre = StdIn.readInt()   // choisir si l'on veut du sucre ou non
    var quantiteSucre = 0
    var prixDuSucre = 0.0

    if (choixDuSucre == 1) { // pas de sucre
      quantiteSucre = 0
      prixDuSucre = 0.0
    } else if (choixDuSucre == 2) {  // peu de sucre
      quantiteSucre = 5
      prixDuSucre = 0.10
    } else if (choixDuSucre == 3) { // moyen de sucre
      quantiteSucre = 10
      prixDuSucre = 0.20
    } else if (choixDuSucre == 4) { // beaucoup de sucre
      quantiteSucre = 15
      prixDuSucre = 0.30
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return
    } // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide

    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) Oui")
    println("2) Non")
    print("> ")

    val choixDeLait = StdIn.readInt() // la valeur pour chosir si l'on veut du lait ou non
    var dosesDeLait = 0
    var prixDuLaitSuppl = 0.0

    if (choixDeLait == 1) { // avec du lait
      do {
        println("Combien de dose (1 dose = 50ml, maximum 3 doses) ?")
        print("> ")
        dosesDeLait = StdIn.readInt() // déclare la variable à l'éxterieur de le boucle

        if (dosesDeLait > 0 && dosesDeLait <= 3) { // nombres de doses comprisent entre 1 et 3
          if (machine.milk >= dosesDeLait * 50) {
            machine.removeIngredient("milk", dosesDeLait * 50) // le stock du lait sera soustraire de 0.05L
            prixDuLaitSuppl = dosesDeLait * 0.05 // le prix du lait égale 0.05chf par doses
          } else {
            println("Stock insuffisant de lait.")
            return
          }
        } else {
          println("Option invalide. Veuillez sélectionner un choix valide.")
        }
      } while (dosesDeLait < 0 || dosesDeLait > 3)
    } else if (choixDeLait == 2) {  // sans lait
      // No additional milk
    } else {
      println("Option invalide. Veuillez sélectionner un choix valide.")
      return // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide
    }

    if (machine.removeIngredient("sugar", quantiteSucre) && machine.removeIngredient("coffee", quantiteDeCafe) && machine.removeIngredient("milk", quantiteDeLait)) { //déduire du stock du sucre,café et lait
      val prixTotal = prixTaille + prixDuSucre + prixDuLaitSuppl
      printf("prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixTaille, prixDuSucre, prixDuLaitSuppl, prixTotal)
      println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codeTwint")
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé.\nPréparation de votre boisson...")
      println("Votre Latte est prêt ! Bonne dégustation !")
    }
  }
}
