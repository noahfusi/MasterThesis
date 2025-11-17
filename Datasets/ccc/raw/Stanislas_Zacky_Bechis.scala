import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random
import java.io.{File, PrintWriter}
import scala.util.{Try, Using,Success, Failure}
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
case class Machine(
                    id: Int,
                    var pincode: String,
                    var milk: Int,  // Stock de lait en millilitres
                    var sugar: Int, // Stock de sucre en grammes
                    var coffee: Int // Stock de café en grammes
                  ) {
  def addIngredient(ingredient: String, amount: Int): Unit = ingredient.toLowerCase match {
    case "milk"   => milk += amount
    case "sugar"  => sugar += amount
    case "coffee" => coffee += amount
    case _         => println("Ingrédient invalide.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient.toLowerCase match {
    case "milk" if milk >= amount   => milk -= amount; true
    case "sugar" if sugar >= amount => sugar -= amount; true
    case "coffee" if coffee >= amount => coffee -= amount; true
    case _ => println("Stock insuffisant."); false
  }
}
object Main {
  var machineID = 0
  def loadCSV(fileName: String): Boolean = {
    println(s"Chargement des machines depuis $fileName...")

    // On utilise un Try pour capturer la réussite ou l'échec
    val tryResult: Try[Unit] = Using(Source.fromFile(fileName)) { source =>
      val lines = source.getLines().drop(1) // Ignorer l'en-tête
      machines.clear()
      lines.foreach { line =>
        val parts = line.split(",")
        if (parts.length == 4) {
          machines += Machine(
            id      = machines.length + 1,
            pincode = parts(0),
            milk    = parts(1).toInt,
            sugar   = parts(2).toInt,
            coffee  = parts(3).toInt
          )
        }
      }
    }

    tryResult match {
      case Success(_) =>
        println(s"Chargement de ${machines.length} machine avec succès")
        true

      case Failure(_: FileNotFoundException) =>
        // Fichier introuvable
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        false

      case Failure(_: IOException) =>
        println(s"Erreur : Échec de la lecture dans $fileName.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        false

      case Failure(e) =>
        println(s"Erreur inattendue : ${e.getMessage}")
        false
    }
  }


  def saveCSV(fileName: String): Unit = {
    Using(new PrintWriter(new File(fileName))) { writer =>

      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { m =>
        writer.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      println(s"sauvegarde de ${machines.length} machines dans $fileName")
    }.recover {
      case e: Exception => println(s"Erreur lors de la sauvegarde : ${e.getMessage}")
    }
  }
val machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {


  var choixb = 0
  var sugar = 0
  var lait = 0.0
  var laitselection = "Non"
  var selection = "Cappucino"
  var taille = 0
  var price = 0.0
  var dose = 0
  var x = 0


  def demanderMachineID(): Int = {
    var idValide = false
    var resultat = -1

    while (!idValide) {
      println(s"Quelle machine utilisez-vous (nombre de machine diponible : ${machines.length})")
      val input = readLine()


      try {
        val entree = input.toInt // Conversion de l'entrée en entier
        if (entree > 0 || entree <= machines.length) {
          resultat = entree -1
          idValide = true
          println(s"Vous utilisez la machine $entree")
        } else {
          println("Erreur : ID de machine invalide.")
        }
      } catch {
        case _: NumberFormatException =>
          println("Erreur : Veuillez entrer uniquement des chiffres entre 1 et 5.")
      }
    }

    resultat
  }
  def demanderChiffre(): Int = {
    var chiffreValide = false
    var resultat = 0

    while (!chiffreValide) {
      println("Veuillez entrer un chiffre entier positif :")
      val input = StdIn.readLine()

      if (input.matches("\\d+")) {
        resultat = input.toInt
        chiffreValide = true
      } else {
        println("Erreur : Veuillez entrer uniquement des chiffres !")
      }
    }
    resultat
  }
  def modeAdmin(m:Machine): Unit = {
    val machine = machines(machineID)
    var attempts = 3
    var pinOk = false

    while (attempts > 0 && !pinOk) {
      println(s"Entrez le code PIN de la machine ${machine.id} :")
      val inputPin = readLine()

      if (inputPin == machine.pincode) {
        pinOk = true
        println("PIN correct. Accès Admin autorisé.")
      } else {
        attempts -= 1
        if (attempts > 0)
          println(s"PIN incorrect. Il vous reste $attempts tentative(s).")
      }
    }

    if (!pinOk) {
      println("Trop de tentatives échouées. Fin du mode Admin.")
      return
    }
    var adminChoice = ""
    while (adminChoice != "1" && adminChoice != "2") {
      println(
        """Que voulez-vous faire ?
          |1) Changer le PIN
          |2) Réapprovisionner les stocks
          |""".stripMargin
      )
      adminChoice = readLine()
    }

    adminChoice match {
      case "1" =>
        println("Entrez un nouveau PIN (6 chiffres) :")
        val newPin = readLine()
        if (newPin.matches("\\d{6}")) {
          machine.pincode = newPin
          println("Le nouveau PIN a été enregistré avec succès.")
        } else {
          println("Format du PIN invalide : il faut exactement 6 chiffres.")
        }


      case "2" =>  gestiondestock(machines(machineID))
    }
    def gestiondestock (m: Machine): Unit = {
    println(s"Voici les stock de la machine ${machineID + 1}")
    println(s"Stock actuel de café : ${m.coffee}")
    println(s"Stock actuel de lait : ${m.milk}")
    println(s"Stock actuel de sucre : ${m.sugar}")
    println("de combien voulez vous réapprovisionner la machine en poudre de café (taper 0 si vous ne vouler par rajouter dans ce stock ")
    val ajoutCafe  = demanderChiffre()
    println("de combien voulez vous réapprovisionner la machine en Sucre (taper 0 si vous ne vouler par rajouter dans ce stock ")
    val ajoutSucre = demanderChiffre()
    println("de combien voulez vous réapprovisionner la machine en Lait (taper 0 si vous ne vouler par rajouter dans ce stock ")
    val ajoutLait  = demanderChiffre()
    m.addIngredient("coffee", ajoutCafe)
    m.addIngredient("sugar",  ajoutSucre)
    m.addIngredient("milk",   ajoutLait)
    println("Réapprovisionnement des stocks...")
    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")

  }}

  def generateRandomCode(length: Int): String = {
    val alphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    Random.alphanumeric.filter(alphanum.contains(_)).take(length).mkString
  }


  def serveClient(machine: Machine): Boolean = {
    while (choixb != 1 && choixb != 2 && choixb != 3) {
      println("\nSélectionnez votre Boisson :\n  1) Expresso - CHF 2.00\n  2) Cappuccino - CHF 2.50\n  3) Latte CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")
      choixb = readInt()
    }

    if (choixb == 3) {
      while (taille != 1 && taille != 2 && taille != 3) {
        println("Quelle taille ?\n 1) Petit (CHF 2.70)\n 2) Moyen (CHF 3.20)\n 3) Grand (CHF 3.70)")
        taille = readInt()
      }
    }

    while (sugar != 1 && sugar != 2 && sugar != 3 && sugar != 4) {
      println("\nSouhaitez-vous ajouter du sucre ?\n  1) Sans sucre\n  2) Peu (5g) - CHF 0.10\n  3) Moyen (10g) - CHF 0.20\n  4) Beaucoup (15g) - CHF 0.30\n")
      sugar = readInt()
    }

    var sugarLevel = "Sans sucre"
    var sugarPrice = 0.0
    var sugarAmount = 0

    if (sugar == 2) {
      sugarLevel = "Peu (5g)"
      sugarPrice = 0.10
      sugarAmount = 5
    } else if (sugar == 3) {
      sugarLevel = "Moyen (10g)"
      sugarPrice = 0.20
      sugarAmount = 10
    } else if (sugar == 4) {
      sugarLevel = "Beaucoup (15g)"
      sugarPrice = 0.30
      sugarAmount = 15
    }

    if (choixb == 2 || choixb == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n  1) Oui\n  2) Non\n")
      lait = readInt()

      if (lait == 1) {
        println("Combien de dose ? (1, 2 ou 3)")
        x = readInt()
        dose = x match {
          case 1 => 50
          case 2 => 100
          case 3 => 150
          case _ => 0
        }
        laitselection = "Oui"
      }
    }

    selection = choixb match {
      case 1 => "Expresso"
      case 2 => "Cappuccino"
      case 3 => "Latte"
      case _ => "Boisson"
    }

    price = choixb match {
      case 1 => 2.00
      case 2 => 2.50
      case 3 => taille match {
        case 1 => 2.70
        case 2 => 3.20
        case 3 => 3.70
        case _ => 0.0
      }
      case _ => 0.0
    }

    val laitPrice = x * 0.05
    val totalPrice = price + sugarPrice + laitPrice

    println(s"Boisson sélectionnée : $selection")
    if (selection == "Latte") println(s"Taille : ${taille match {
      case 1 => "Petit"
      case 2 => "Moyen"
      case 3 => "Grand"
    }}")
    println(s"Niveau de sucre : $sugarLevel")
    println(s"Lait supplémentaire : $laitselection")
    println(s"Prix total : CHF $totalPrice")

    if (!machine.removeIngredient("coffee", choixb match {
      case 1 => 8
      case 2 => 6
      case 3 => taille match {
        case 1 => 6
        case 2 => 8
        case 3 => 12
        case _ => 0
      }
      case _ => 0
    })) {
      println("Erreur : Quantité de café insuffisante. Veuillez réapprovisionner.")
      return false
    }

    if (!machine.removeIngredient("sugar", sugarAmount)) {
      println("Erreur : Quantité de sucre insuffisante. Veuillez réapprovisionner.")
      return false
    }

    if (!machine.removeIngredient("milk", dose + (choixb match {
      case 2 => 100
      case 3 => taille match {
        case 1 => 100
        case 2 => 150
        case 3 => 200
        case _ => 0
      }
      case _ => 0
    }))) {
      println("Erreur : Quantité de lait insuffisante. Veuillez réapprovisionner.")
      return false
    }
    val paymentCode = generateRandomCode(5)
    println ("Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + paymentCode)
    println( "En attente de validation du paiement...\n")
    Thread.sleep(4000)
    println("Merci ! Votre paiement a été accepté")
    println("Préparation de votre boisson...")
    Thread.sleep(3000)
    println(s"Votre $selection est prêt ! Bonne dégustation !")
    true
  }
    val csvFileName = "machines.csv"
    if (!loadCSV(csvFileName)) {
      println("Erreur : Impossible de charger les données des machines.")
      return
    }
    var exit = false
    while (!exit) {
      println("\nVeuillez sélectionner une option :")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")

      readLine() match {
        case "1" =>
          machineID = demanderMachineID()
          val machineselect = machines(machineID)
          serveClient (machineselect)
        case "2" =>
          machineID = demanderMachineID()
          val machineselect = machines(machineID)
          modeAdmin(machineselect)
        case "3" =>
          saveCSV(csvFileName)
          exit = true
        case _ => println("Option invalide. Veuillez réessayer.")
      }
    }












  }}






