import collection.mutable.ArrayBuffer
import scala.io.StdIn._
import scala.io.Source._
import java.io.{FileWriter, PrintWriter}
import scala.util.Random

object Main {
  class Machine(var id: Int, var pincode: String = "000000", var lait: Int = 0, var sucre: Int = 0, var cafe: Int = 0) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "lait") lait += amount
      else if (ingredient == "sucre") sucre += amount
      else if (ingredient == "cafe") cafe += amount
      else printf("Ingrédient inconnu : %s\n", ingredient)}

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "lait" && lait >= amount) { lait -= amount; true }
      else if (ingredient == "sucre" && sucre >= amount) { sucre -= amount; true }
      else if (ingredient == "cafe" && cafe >= amount) { cafe -= amount; true }
      else {
        printf("Stock insuffisant pour %s.\n", ingredient)
        false}}

    def afficher(): Unit = {
      printf("\nID: %d\nCode PIN: %s\nLait: %.2fL\nSucre: %dg\nCafé: %dg\n", id, pincode, lait / 1000.0, sucre, cafe)
      printf(" %d machine(s) chargée(s) avec succès!\n", machines.length)
    }
  }

  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = fromFile(filename)
      val machines = new ArrayBuffer[Machine]()
      var index = 0

      val lignes = fr.getLines().drop(1)

      for (ligne <- lignes) {
        val donnees = ligne.split(",")
        if (donnees.length == 4) {
          val pincode = donnees(0)
          val lait = donnees(1).toInt
          val sucre = donnees(2).toInt
          val cafe = donnees(3).toInt
          machines += new Machine(index + 1, pincode, lait, sucre, cafe)
          index += 1
        } else {
          println("Erreur : Ligne mal inscrite.")
        }
      }
      fr.close()
      machines
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable.Vérifiez le chemin d’accès et réessayez.")
        new ArrayBuffer[Machine]()
    }
  }

  def main(args: Array[String]): Unit = {
    val nomFichier = "machines.csv"
    val loadedMachines = loadcsv(nomFichier)
    if (loadedMachines.nonEmpty) {
      if (loadedMachines.isEmpty) {
        println("Aucune machine n'a été chargée. Ajout d'une machine par défaut.")
        machines += new Machine(1)
      } else {
        machines ++= loadedMachines}

      printf("Nombre de machines disponibles : %d\n", machines.length)
      for (machine <- machines) {
        machine.afficher()
      }
      var continuer = true
      while (continuer) {
        println("\nVeuillez choisir votre mode: \n1) Client \n2) Admin \n3) Quitter")
        print("> ")
        val choixmode = readLine()

        if (choixmode == "1") {
          printf("Veuillez sélectionner une machine (1-%d):\n", machines.length)
          print("> ")
          val machineId = readLine().toInt - 1

          if (machineId >= 0 && machineId < machines.length) {
            println("Sélectionnée: Machine " + (machineId + 1))
            serveClient(machineId)
          }
        } else if (choixmode == "2") {
          printf("Veuillez sélectionner une machine (1-%d):\n", machines.length)
          print("> ")

          val machineId = readLine().toInt - 1

          if (machineId >= 0 && machineId < machines.length) {
            println("Sélectionnée: Machine " + (machineId + 1))
            if (!validatePin(machineId)) {
              continuer = false
            } else {
              println("Accès Admin : Que souhaitez-vous faire ? \n1) Réapprovisionner \n2) Modifier le code PIN")
              print("> ")
              val choixadmin = readLine()
              if (choixadmin == "1") {
                restockMachine(machineId)
              } else if (choixadmin == "2") {
                updatePin(machineId)}
            }
          } else {
            println("Choix de machine invalide.")}

        } else if (choixmode == "3") {
          continuer = false
        } else {
          println("Choix invalide.")}
      }
      savecsv(nomFichier, machines)
      println("Fin du programme.")
    }

    def serveClient(machineId: Int): Boolean = {
      val machine = machines(machineId)
      println("Sélectionner votre boisson : \n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")

      val choixboisson = readLine()
      var cafe = 0
      var lait = 0
      var prixbase = 0.0

      if (choixboisson == "1") {
        cafe = 8
        prixbase = 2.00
      } else if (choixboisson == "2") {
        cafe = 6
        lait = 100
        prixbase = 2.50
      } else if (choixboisson == "3") {
        println("Choisissez la taille de votre Latte : \n1) Petit \n2) Moyen \n3) Grand")
        print("> ")
        val size = readLine()
        if (size == "1") {
          cafe = 6
          lait = 120
          prixbase = 2.70
        } else if (size == "2") {
          cafe = 8
          lait = 150
          prixbase = 3.20
        } else if (size == "3") {
          cafe = 12
          lait = 200
          prixbase = 3.70
        } else {
          println("Choix invalide.")
          return false}
      } else {
        println("Choix invalide.")
        return false
      }
      println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30")
      print("> ")

      val choixsucre = readLine()
      var sucrequantite = 0
      var prixsucre = 0.0
      if (choixsucre == "1") {
        sucrequantite = 0
      } else if (choixsucre == "2") {
        sucrequantite = 5
        prixsucre = 0.1
      } else if (choixsucre == "3") {
        sucrequantite = 10
        prixsucre = 0.2
      } else if (choixsucre == "4") {
        sucrequantite = 15
        prixsucre = 0.3
      } else {
        println("Choix de sucre invalide.")
        return false}

      println("Voulez-vous ajouter du lait supplémentaire ? (0.05 CHF par dose de 50ml) \n1) Oui \n2) Non")
      print("> ")

      val choixlait = readLine()
      var laitsupplementaire = 0
      var prixlaitsupplementaire = 0.0
      if (choixlait == "1") {
        println("Combien de doses supplémentaires (1-3 doses, une dose = 50ml) ?")
        print("> ")
        val nbDoses = readLine().toInt
        if (nbDoses >= 1 && nbDoses <= 3) {
          laitsupplementaire = nbDoses * 50
          prixlaitsupplementaire = nbDoses * 0.05
        } else {
          println("Nombre de doses invalides.")
          return false
        }}

      if (machine.lait >= (lait + laitsupplementaire) && machine.sucre >= sucrequantite && machine.cafe >= cafe) {
        machine.removeIngredient("cafe", cafe)
        machine.removeIngredient("sucre", sucrequantite)
        machine.removeIngredient("lait", lait + laitsupplementaire)

        val prixfinal = prixbase + prixsucre + prixlaitsupplementaire
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixbase, prixsucre, prixlaitsupplementaire, prixfinal)

        val alphanumeric = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var code = ""
        for (_ <- 1 to 5) {
          code = code + alphanumeric(Random.nextInt(alphanumeric.length))}

        printf("Code Twint : %s\n", code)
        println("(Attente de validation du paiement)")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")
        println("Préparation de votre boisson... \n[...]")
        Thread.sleep(3000)
        println("Votre boisson est prête ! Bonne dégustation !")
        Thread.sleep(1000)
        true
      } else {
        if (machine.cafe < cafe) println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        if (machine.sucre < sucrequantite) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        if (machine.lait < lait) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        false}
    }

    def validatePin(machineId: Int): Boolean = {
      var attempts = 3
      while (attempts > 0) {
        printf("Entrez le code PIN pour la Machine %d :\n", machineId + 1)
        print("> ")
        val pin = readLine()
        if (pin == machines(machineId).pincode) {
          println("Code PIN correct. Accès autorisé.")
          return true
        } else {
          attempts -= 1
          if (attempts > 0) {
            printf("Code PIN incorrect. Il vous reste %d tentative(s).\n", attempts)}}}

      println("Trop de tentatives échouées. Accès refusé.")
      false
    }

    def updatePin(machineId: Int): Unit = {
      val machine = machines(machineId)
      printf("Mise à jour du code PIN pour la Machine %d.\n", machineId + 1)
      var nouveaupinmachine = false
      while (!nouveaupinmachine) {
        printf("Entrez un nouveau code PIN à 6 chiffres :\n")
        print("> ")
        val nouveaupin = readLine()
        if (nouveaupin.length == 6) {
          var valide = true
          for (i <- 0 until 6) {
            if (nouveaupin(i) < '0' || nouveaupin(i) > '9') {
              valide = false}}
          if (valide) {
            machine.pincode = nouveaupin
            println("Le code PIN a été mis à jour avec succès. \nRetour au menu principal...")
            nouveaupinmachine = true
          } else {
            println("Le PIN doit avoir exactement 6 chiffres.")}}}
    }

    def restockMachine(machineId: Int): Unit = {
      val machine = machines(machineId)
      printf("Réapprovisionnement de la Machine %d.\n", machineId + 1)
      printf("Stock actuel de café en grammes: %dg\n", machine.cafe)
      printf("Stock actuel de sucre en grammes: %dg\n", machine.sucre)
      printf("Stock actuel de lait en litres: %.2fL\n", machine.lait / 1000.0)
      println("Combien de café ajouter ?")
      print("> ")
      val cafeajoute = readLine().toInt
      println("Combien de sucre ajouter ?")
      print("> ")
      val sucreajoute = readLine().toInt
      println("Combien de lait ajouter (en litres) ?")
      print("> ")
      val laitajoute = ((readLine().toDouble) * 1000).toInt
      machine.addIngredient("cafe", cafeajoute)
      machine.addIngredient("sucre", sucreajoute)
      machine.addIngredient("lait", laitajoute)

      printf("Stock de café : %d g \n Stock de sucre : %d g \n Stock de lait : %.2f L \n", machine.cafe, machine.sucre, machine.lait / 1000.0)
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        printf("Sauvegarde des machines dans %s...\n", filename)
        val writer = new PrintWriter(new FileWriter(filename))
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (i <- machines.indices) {
          val machine = machines(i)
          writer.printf("%s,%d,%d,%d\n", machine.pincode, machine.lait, machine.sucre, machine.cafe)}
        writer.close()
        println("Sauvegarde terminée avec succès.")
      } catch {
        case ex: Exception =>
          printf("Erreur : Échec de l'écriture dans %s (%s).\n", filename)
      }}}}
