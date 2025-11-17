import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}
import scala.io.StdIn._
import scala.util.Random

class Machine(val Id: Int, var CodePin: String, var Lait: Int, var Sucre: Int, var PoudreCafe: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient match{
      case "Lait" => Lait += amount
      case "Sucre" => Sucre += amount
      case "Café" => PoudreCafe += amount
      case _ => println(s"Ingrédient $ingredient non valide.")
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient match {
      case "Lait" if Lait >= amount => Lait -= amount; true
      case "Sucre" if Sucre >= amount => Sucre -= amount; true
      case "PoudreCafe" if PoudreCafe >= amount => PoudreCafe -= amount; true
      case _ => println(s"Stock insuffisant ou ingrédient non valide pour $ingredient."); false
    }
  }
  override def toString: String = s"Machine ID: $Id, PIN: $CodePin, Lait: $Lait mL, Sucre: $Sucre g, Café: $PoudreCafe g"
}
object Main {
  var modeUser = 0
  var CafeBesoin = 0
  var SucreBesoin = 0
  var LaitBesoin = 0
  var Prix = 0.00
  var essaiCodePin = "0"
  var erreur = 0
  var reponse = 0
  var reponseBis = 0
  var NouveauCode = "0"
  val Machines = ArrayBuffer[Machine]()


  def LoadCSV(NomFichier: String): Unit = {
    try {
      val source = Source.fromFile(NomFichier)
      val lines = source.getLines().drop(1)
      for ((line, index) <- lines.zipWithIndex) {
        val colonne = line.split(",")
        if (colonne.length == 4) {
          Machines += new Machine(index + 1, colonne(0), colonne(1).toInt, colonne(2).toInt, colonne(3).toInt)
        }
      }
      source.close()
      println(s"${Machines.size} machine(s) chargée(s) depuis $NomFichier.")
    } catch {
      case _: Exception =>
        println("Erreur : Fichier introuvable ou illisible. Création du fichier CSV.")
        try {
          val writer = new PrintWriter(new File(NomFichier))
          writer.println("CodePin,Lait,Sucre,PoudreCafe")
          // Ajoutez des machines par défaut
          val defaultMachines = Array(
            ("123456", 500, 200, 300),
            ("234567", 400, 150, 250),
            ("345678", 300, 100, 200),
            ("456789", 200, 50, 150),
            ("567890", 100, 20, 100)
          )

          for ((codePin, lait, sucre, cafe) <- defaultMachines) {
            writer.println(s"$codePin,$lait,$sucre,$cafe")
            Machines += new Machine(Machines.size + 1, codePin, lait, sucre, cafe)
          }
          writer.close()
          println("Fichier CSV créé avec succès et machines par défaut ajoutées.")
        } catch {
          case _: Exception => println("Erreur lors de la création du fichier CSV.")
        }

    }
  }

  def SaveCSV(NomFichier: String, Machines :ArrayBuffer[Machine] ): Unit = {
    try {
      val writer = new PrintWriter(new File(NomFichier))
      writer.println("CodePin,Lait,Sucre,PoudreCafe")
      for (machine <- Machines) {
        writer.println(s"${machine.CodePin},${machine.Lait},${machine.Sucre},${machine.PoudreCafe}")
      }
      writer.close()
      println("Données sauvegardées dans le fichier CSV.")
    } catch {
      case _: Exception => println("Erreur lors de la sauvegarde dans le fichier CSV.")
    }
  }
  def ValidatePin(MachineId: Int): Boolean = {
    val machine = Machines.find(_.Id == MachineId).orNull
    println("Veuillez entrer le Code PIN de la machine séléctionnée pour accéder aux stocks")
    essaiCodePin = readLine()
    if (essaiCodePin == machine.CodePin) {
      return true
    } else {
      println(" Il reste 2 tentatives pour entrer le bon code")
      println(" Veuillez entrer à nouveau le code PIN de la machine")
      essaiCodePin = readLine()
      if (essaiCodePin == machine.CodePin) {
        return true
      } else {
        println(" Il reste 1 tentatives pour entrer le bon code")
        println(" Veuillez entrer à nouveau le code PIN de la machine")
        essaiCodePin = readLine()
        if (essaiCodePin == machine.CodePin) {
          return true
        } else {
          println(" Trop de tentatives echouées. Fin du programme")
          return false
        }
      }
    }
  }

  def restockMachine(MachineId: Int): Unit = {
    val machine = Machines.find(_.Id == MachineId).orNull
    println("Bienvenu dans le mode Admin")
    println(s"Gestion des stocks pour la machine ${machine.Id} :")
    println(s"Stocks actuel : ${machine.Lait}mL de Lait, ${machine.Sucre}g de Sucre et ${machine.PoudreCafe}g de Café")
    println("Quel ingrédient souhaitez-vous recharger ? (Lait, Café, Sucre) (Veuillez faire attention à l'orthographier comme l'exemple)")
    val ingredient = readLine()
    println("Combien de quantité?")
    val quantite = readInt()
    machine.addIngredient(ingredient, quantite)
  }

  def updatePin(MachineId: Int): Unit = {
    val machine = Machines.find(_.Id == MachineId).orNull
    println("Quel code voulez-vous mettre? ")
    println("Il doit comporter 6 chiffres")
    NouveauCode = readLine()
    while (NouveauCode.length != 6) {
      println("Quel code voulez-vous mettre? ")
      println("Il doit comporter 6 chiffres")
      NouveauCode = readLine()
    }
    machine.CodePin = NouveauCode
  }


  def ServeCLient(MachineId: Int): Unit = {
    println("Quelle boisson souhaitez-vous?")
    println("tapez le numéro correspondant à la boisson")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappucino - 2.50 CHF")
    println("3) Latte - 2.70 CHF (petit), 3.20 CHF (moyen), 3.70 CHF (grand)")
    println(">")
    var typeBoisson = readInt()
    while ((typeBoisson != 1) && (typeBoisson != 2) && (typeBoisson != 3)) {
      println("Quelle boisson souhaitez-vous?")
      println("tapez le numéro correspondant à la boisson")
      println("1) Expresso - 2.00 CHF")
      println("2) Cappucino - 2.50 CHF")
      println("3) Latte - 2.70 CHF (petit), 3.20 CHF (moyen), 3.70 CHF (grand)")
      println(">")
      typeBoisson = readInt()
    }
    CafeBesoin = 0
    SucreBesoin = 0
    LaitBesoin = 0
    Prix = 0.00
    if (typeBoisson == 1) {
      CafeBesoin = CafeBesoin + 8
      Prix = Prix + 2.00
    }
    if (typeBoisson == 2) {
      CafeBesoin = CafeBesoin + 6
      LaitBesoin = LaitBesoin + 100
      Prix = Prix + 2.50
    }
    if (typeBoisson == 3) {
      println("Quelle taille voulez-vous?")
      println("tapez le numéro correspondant à la taille")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")
      println(">")
      var tailleLatte = readInt()
      while ((tailleLatte != 1) && (tailleLatte != 2) && (tailleLatte != 3)) {
        println("Quelle taille voulez-vous?")
        println("tapez le numéro correspondant à la taille")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        println(">")
        tailleLatte = readInt()
      }
      if (tailleLatte == 1) {
        CafeBesoin = CafeBesoin + 6
        LaitBesoin = LaitBesoin + 120
        Prix = Prix + 2.70
      }
      if (tailleLatte == 2) {
        CafeBesoin = CafeBesoin + 8
        LaitBesoin = LaitBesoin + 150
        Prix = Prix + 3.20
      }
      if (tailleLatte == 3) {
        CafeBesoin = CafeBesoin + 12
        LaitBesoin = LaitBesoin + 200
        Prix = Prix + 3.70
      }
    }
    println("Quelle dose de sucre voulez vous?")
    println("Tapez le numéro correspondant à la dose de sucre")
    println("0) pas de sucre")
    println("1) Peu de sucre (5g) - 0.10 CHF")
    println("2) Moyenne dose (10g) - 0.20 CHF ")
    println("3) Grande dose (15g) - 0.30 CHF")
    println(">")
    var doseSucre = readInt()
    while ((doseSucre != 0) && (doseSucre != 1) && (doseSucre != 2) && (doseSucre != 3)) {
      println("Quelle dose de sucre voulez vous?")
      println("Tapez le numéro correspondant à la dose de sucre")
      println("0) pas de sucre")
      println("1) Peu de sucre (5g) - 0.10 CHF")
      println("2) Moyenne dose (10g) - 0.20 CHF")
      println("3) Grande dose (15g) - 0.30 CHF")
      println(">")
      doseSucre = readInt()
    }
    if (doseSucre == 1) {
      SucreBesoin = SucreBesoin + 5
      Prix = Prix + 0.10
    }
    if (doseSucre == 2) {
      SucreBesoin = SucreBesoin + 10
      Prix = Prix + 0.20
    }
    if (doseSucre == 3) {
      SucreBesoin = SucreBesoin + 15
      Prix = Prix + 0.30
    }
    var doseLait = 0
    if ((typeBoisson == 2) || (typeBoisson == 3)) {
      println("Voulez vous du lait en supplément?")
      println("Tapez le numéro correspondant à la dose de lait en spplément")
      println("0) Pas de supplément lait")
      println("1) Une dose de lait en supplément - 0.05 CHF")
      println("2) Deux doses de lait en supplément - 0.10 CHF")
      println("3) Trois doses de lait en supplément - 0.15 CHF")
      doseLait = readInt()
      while ((doseLait != 0) && (doseLait != 1) && (doseLait != 2) && (doseLait != 3)) {
        println("Voulez vous du lait en supplément?")
        println("Tapez le numéro correspondant à la dose de lait en spplément")
        println("0) Pas de supplément lait")
        println("1) Une dose de lait en supplément - 0.05 CHF")
        println("2) Deux doses de lait en supplément - 0.10 CHF")
        println("3) Trois doses de lait en supplément - 0.15 CHF")
        doseLait = readInt()
      }
      if (doseLait == 1) {
        LaitBesoin = LaitBesoin + 50
        Prix = Prix + 0.05
      }
      if (doseLait == 2) {
        LaitBesoin = LaitBesoin + 100
        Prix = Prix + 0.10
      }
      if (doseLait == 3) {
        LaitBesoin = LaitBesoin + 150
        Prix = Prix + 0.15
      }
    }
    val Machine = Machines.find(_.Id == MachineId).orNull

    val EnleveCafe = Machine.removeIngredient("PoudreCafe", CafeBesoin)
    val EnleveSucre = Machine.removeIngredient("Sucre", SucreBesoin)
    val EnleveLait = Machine.removeIngredient("Lait", LaitBesoin)

    if ((EnleveCafe) && (EnleveSucre) && (EnleveLait)) {
      val CodePaiement = Random.alphanumeric.take(5).mkString
      if ((typeBoisson == 1) && (doseSucre == 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Expresso")
        println("Sans sucre ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 1) && (doseSucre != 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Expresso")
        println("Avec sucre")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 2) && (doseSucre == 0) && (doseLait == 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Cappuccino")
        println("Sans sucre ")
        println("Sans Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 2) && (doseSucre != 0) && (doseLait != 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Cappuccino")
        println("Avec sucre ")
        println("Avec Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 2) && (doseSucre == 0) && (doseLait != 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Cappuccino")
        println("Sans sucre ")
        println("Avec Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 2) && (doseSucre != 0) && (doseLait == 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Cappuccino")
        println("Avec sucre ")
        println("Sans Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 3) && (doseSucre == 0) && (doseLait == 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Latte")
        println("Sans sucre ")
        println("Sans Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 3) && (doseSucre != 0) && (doseLait != 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Latte")
        println("Avec sucre ")
        println("Avec Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 3) && (doseSucre == 0) && (doseLait != 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Latte")
        println("Sans sucre ")
        println("Avec Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      } else if ((typeBoisson == 3) && (doseSucre != 0) && (doseLait == 0)) {
        println("récapitulation commande: ")
        println("Boisson:  Latte")
        println("Avec sucre ")
        println("Sans Lait ")
        println("")
        println("Veuillez payez par Twint.")
        printf("Le prix total est de  CHF %.2f\n", Prix)
        println("Votre code de paiement est : " + CodePaiement)
        println("(En attente de paiement...)")
        println("")
        Thread.sleep(3000)
      }
      if (typeBoisson == 1) {
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        println("Votre Expresso est prêt ! Bonne dégustation !")
        println("")
        Thread.sleep(5000)
      } else if (typeBoisson == 2) {
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
        println("")
        Thread.sleep(5000)
      } else if (typeBoisson == 3) {
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        println("Votre Latte est prêt ! Bonne dégustation !")
        println("")
        Thread.sleep(5000)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    LoadCSV("machines.csv")
    println("Machines chargées avec succès")

    while ((modeUser != 3) && (erreur == 0)) {
      var MachineId = 0
      println("     Nospresso Café")
      println("Veuillez séléctionner votre mode en tapant le chiffre correspondant")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      modeUser = readInt()
      while ((modeUser != 1) && (modeUser != 2) && (modeUser != 3)) {
        println("     Nospresso Café")
        println("Veuillez séléctionner votre mode en tapant le chiffre correspondant")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        println(">")
        modeUser = readInt()
      }
      println("Liste des machines disponibles :")
      for (machine <- Machines) {
        println(machine)
      }
      if ((modeUser == 1) || (modeUser == 2)) {
        println("Qu'elle machine utilisez-vous?")
        MachineId = readInt()
      }
      val Machine = Machines.find(_.Id == MachineId).orNull
      if (Machine != null) {
        if (modeUser == 1) {
          ServeCLient(MachineId)
        }
        if (modeUser == 2) {
          if (ValidatePin(MachineId)) {
            println("Voulez-vous mettre à jour le code PIN ? ")
            println("1) OUI")
            println("2) NON")
            reponse = readInt()
            while ((reponse != 1) && (reponse != 2)) {
              println("Voulez-vous mettre à jour le code PIN ? ")
              println("1) OUI")
              println("2) NON")
              reponse = readInt()
            }
            if (reponse == 1) {
              updatePin(MachineId)
            }
            println("Voulez-vous mettre à jour les stocks ? ")
            println("1) OUI")
            println("2) NON")
            reponseBis = readInt()
            while ((reponse != 1) && (reponse != 2)) {
              println("Voulez-vous mettre à jour les stocks ? ")
              println("1) OUI")
              println("2) NON")
              reponseBis = readInt()
            }
            if (reponseBis == 1) {
              restockMachine(MachineId)
              Thread.sleep(5000)
            } else {
              Thread.sleep(5000)
            }
          } else {
            erreur = erreur + 1
          }
          println("Sauvegarde des machine...")
          SaveCSV("machines.csv", Machines)
          println("Données sauvegardées.")
        }
      }
    }
  }
}