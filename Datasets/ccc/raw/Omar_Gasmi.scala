import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._
//bon
object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "coffee") {
        if (coffee - amount >= 0) {
          coffee -= amount
          true
        } else {
          false
        }
      } else if (ingredient == "milk") {
        if (milk - amount >= 0) {
          milk -= amount
          true
        } else {
          false
        }
      } else if (ingredient == "sugar") {
        if (sugar - amount >= 0) {
          sugar -= amount
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "coffee") {
        coffee += amount
      } else if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      }
    }
  }
  var machines = new ArrayBuffer[Machine]()

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1)
      var id = 1
      for (line <- lines) {
        val cols = line.split(",")
        val pin = cols(0)
        val milk = cols(1).toInt
        val sugar = cols(2).toInt
        val coffee = cols(3).toInt
        machines += new Machine(id, pin, milk, sugar, coffee)
        id += 1
      }
      source.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        println("Fermeture du programme")
        System.exit(3)
      case _: IOException =>
        println("Erreur lors de la lecture du fichier")
        println("Fermeture du programme")
        System.exit(3)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      try {
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (m <- machines) {
          writer.println(m.pincode + "," + m.milk + "," + m.sugar + "," + m.coffee)
        }
        println("Fichier sauvegardé avec succès.")
      }
      writer.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Echec de l'écriture dans machines.csv.")
        println("Fermeture du programme")
        System.exit(3)
      case _: IOException =>
        println("Erreur : echec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme")
        System.exit(3)
    }
  }


  def main(args: Array[String]): Unit = {
    var running = true
    var filename = "machines.csv"
    println("Chargement des machines depuis machines.csv")
    machines = loadcsv(filename) // copier fichier csv

    for (m <- machines) {
      println("\nMachine " + m.id + " chargée :")
      println("    ID: " + m.id)
      println("    Code PIN: " + m.pincode)
      printf("    Lait : %.3f litres\n", m.milk / 1000.0)
      println("    Sucre: " + m.sugar + "g")
      println("    Café: " + m.coffee + "g")
    }
    println("\n" + machines.size + " machine(s) chargée(s) avec succès.")


    while (running) {
      println("\nNospresso Café !")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")
      var mode = readInt()

      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("Le mode sélectionné n'est pas valide, Veuillez resélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print(">")
        mode = readInt()
      }
      if (mode == 1) {
        var selectionmachine = true
        while (selectionmachine) {
          println("Veuillez choisir une machine (1-" + machines.size + ")")
          print("> ")
          var machineId = readInt()
          while ((machineId < 1 || machineId > machines.size)) {
            println("Veuillez choisir une machine (1-" + machines.size + ")")
            print("> ")
            machineId = readInt()
          }
          println("Accès accordé à la Machine " + machineId)
          if (serveClient(machineId - 1)) {
            selectionmachine = false
          } else {
            print("> ")
          }
        }
      }

      else if (mode == 2) {
        println("Veuillez choisir une machine (1-" + machines.size + ")")
        print("> ")
        var machineId = readInt()
        while ((machineId < 1 || machineId > machines.size)) {
          println("Veuillez choisir une machine (1-" + machines.size + ")")
          print("> ")
          machineId = readInt()
        }
        if ((machineId > 1 || machineId <= machines.size) && validatePin(machineId - 1)) {
          var retouraumenu = true
          while (retouraumenu) {
            println("Quelle option désirez-vous ? ")
            println("")
            println("1) Réapprovisionner")
            println("2) Mettre à jour le code PIN")
            print("> ")
            var adminChoice = readInt()
            while (!(adminChoice == 1 || adminChoice == 2)) {
              println("Veuillez selectionnez une option valide : ")
              println("")
              println("1) Réapprovisionner")
              println("2) Mettre à jour le code PIN")
              print("> ")
              adminChoice = readInt()
            }
            if (adminChoice == 1) {
              restockMachine(machineId - 1)
              retouraumenu = false
            } else if (adminChoice == 2) {
              updatePin(machineId - 1)
              retouraumenu = false
            }
          }
        } else {
          running = false // ici je dois stoppe boucle principale si trop tentatives dans validatepin, ok
        }

      } else if (mode == 3) {
        println("Merci de votre visite ! Au revoir ! ")
        println("Sauvegarde de la machine " + " dans machines.csv...")
        savecsv(filename, machines) // nom fichier
        running = false
      }
    }
  }


  def validatePin(machineId: Int): Boolean = {
    var nmbressaies = 3

    while (nmbressaies > 0) {
      println("Entrez le code PIN :")
      print("> ")
      val codePIN = readLine()

      if (codePIN == machines(machineId).pincode) {
        println("Accès accordé à la Machine " + (machineId + 1))
        return true
      } else {
        nmbressaies = nmbressaies - 1
        println("Code PIN incorrect " + nmbressaies + " tentatives restantes")
      }
    }
    while (nmbressaies == 0) {
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }
    false
  }


  def updatePin(machineId: Int): Unit = {

    var codePINvalid = false

    while (!codePINvalid) {
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
      println("Entrez un nouveau code PIN à 6 chiffres >")
      val nouveaucodePIN = readLine()

      if (nouveaucodePIN.length == 6 && nouveaucodePIN.forall(_.isDigit)) {
        machines(machineId).pincode = nouveaucodePIN
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal ...")
        codePINvalid = true
        // retour menu principale
      } else {
        println("")
      }
    }
  }


  def serveClient(machineId: Int): Boolean = {
    var prixtotal = 0.0
    var prixboisson = 0.0
    var prixsucre = 0.0
    var prixdoselait = 0.0
    val codetwint = scala.util.Random.alphanumeric.take(5).mkString

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print(">")
    var boisson = readInt()
    while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
      println("ERREUR : La boisson sélectionnée n'est pas valide, veuillez réessayer.")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print(">")
      boisson = readInt()
    }

    if (boisson == 3) {
      println("------------------------------------------------------------------")
      println("Note : la taille du Latte désirée par l'utilisateur sera demandée après.")
      println("-------------------------------------------------------------------")
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print(">")
    var sucreoption = readInt()
    while (!(sucreoption == 1 || sucreoption == 2 || sucreoption == 3 || sucreoption == 4)) {
      println("ERREUR : Option invalide, Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print(">")
      sucreoption = readInt()
    }

    if (sucreoption == 2 && machines(machineId).sugar >= 5) {
      prixsucre += 0.10
      prixtotal += 0.10
      machines(machineId).removeIngredient("sugar", 5)
    } else if (sucreoption == 3 && machines(machineId).sugar >= 10) {
      prixsucre += 0.20
      prixtotal += 0.20
      machines(machineId).removeIngredient("sugar", 10)
    } else if (sucreoption == 4 && machines(machineId).sugar >= 15) {
      prixsucre += 0.30
      prixtotal += 0.30
      machines(machineId).removeIngredient("sugar", 15)
    } else if (sucreoption != 1) {
      println("ERREUR : Quantité insuffisante de sucre. Veuillez choisir une autre option, machine ou vérifier les stocks en mode admin.")
      return false
    }

    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print(">")
      var laitsupplement = readInt()
      while (!(laitsupplement == 1 || laitsupplement == 2)) {
        println("ERREUR : Choix invalide, Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")
        laitsupplement = readInt()
      }

      if (laitsupplement == 1) {
        println("Combien de doses souhaitez-vous ? (3 max)")
        println("1) 1 dose -- CHF 0.05")
        println("2) 2 doses -- CHF 0.10")
        println("3) 3 doses -- CHF 0.15")
        print(">")
        var doselait = readInt()
        while (!(doselait == 1 || doselait == 2 || doselait == 3)) {
          println("ERREUR : Choix invalide, Combien de doses souhaitez-vous ?")
          println("1) 1 dose -- CHF 0.05")
          println("2) 2 doses -- CHF 0.10")
          println("3) 3 doses -- CHF 0.15")
          print(">")
          doselait = readInt()
        }

        val laitnecessaire = doselait * 50
        if (machines(machineId).milk >= laitnecessaire) {
          prixdoselait += doselait * 0.05
          prixtotal += doselait * 0.05
          machines(machineId).removeIngredient("milk", laitnecessaire)
        } else {
          println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre option, machine ou vérifier les stocks en mode admin.")
          return false
        }
      }
    }

    if (boisson == 1) {
      if (machines(machineId).coffee >= 8) {
        machines(machineId).removeIngredient("coffee", 8)
        prixboisson = 2.00
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else {
        println("ERREUR : Quantité insuffisante de café. Veuillez choisir une autre boisson, machine ou vérifier les stocks en mode admin.")
        return false
      }
    } else if (boisson == 2) {
      if (machines(machineId).coffee >= 6 && machines(machineId).milk >= 100) {
        machines(machineId).removeIngredient("coffee", 6)
        machines(machineId).removeIngredient("milk", 100)
        prixboisson = 2.50
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (machines(machineId).coffee < 6) {
        println("ERREUR : Quantité insuffisante de café. Veuillez choisir une autre boisson ou machine ")
        return false
      } else if (machines(machineId).milk < 100) {
        println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre boisson ou machine ")
        return false
      }
    } else if (boisson == 3) {
      println("Veuillez sélectionner la taille :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print(">")
      var taille = readInt()
      while (!(taille == 1 || taille == 2 || taille == 3)) {
        println("ERREUR : Taille invalide, veuillez réessayer.")
        print(">")
        taille = readInt()
      }

      if (taille == 1 && machines(machineId).coffee >= 6 && machines(machineId).milk >= 120) {
        machines(machineId).removeIngredient("coffee", 6)
        machines(machineId).removeIngredient("milk", 120)
        prixboisson = 2.70
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (taille == 2 && machines(machineId).coffee >= 8 && machines(machineId).milk >= 150) {
        machines(machineId).removeIngredient("coffee", 8)
        machines(machineId) removeIngredient("milk", 150)
        prixboisson = 3.20
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (taille == 3 && machines(machineId).coffee >= 12 && machines(machineId).milk >= 200) {
        machines(machineId).removeIngredient("coffee", 12)
        machines(machineId).removeIngredient("milk", 200)
        prixboisson = 3.70
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if ((taille == 1 && machines(machineId).coffee < 6) || (taille == 2 && machines(machineId).coffee < 8) || (taille == 3 && machines(machineId).coffee < 12)) {
        println("ERREUR : Quantité insuffisante de café . Veuillez choisir une autre option ou machine.")
        return false
      } else if ((taille == 1 && machines(machineId).milk < 120) || (taille == 2 && machines(machineId).milk < 150) || (taille == 3 && machines(machineId).milk < 200)) {
        println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre option ou machine.")
        return false
      }
    }
    true
  }

  // ok !
  def restockMachine(machineId: Int): Unit = {
    println("Niveaux de stock actuels pour la Machine " + machineId + 1 + " :")
    println("Poudre De Café : " + machines(machineId).coffee + "g")
    println("Sucre : " + machines(machineId).sugar + "g")
    printf("Lait : %.3f litres\n", machines(machineId).milk / 1000.0)
    println("Entrez les quantités à ajouter :")

    print("Poudre de café (gr) > ")
    var poudredecafeaajouter = readInt()
    while (!(poudredecafeaajouter >= 0)) {
      println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
      print("Poudre de café (gr) > ")
      poudredecafeaajouter = readInt()
    }
    if (poudredecafeaajouter >= 0) {
      machines(machineId).addIngredient("coffee", poudredecafeaajouter)

      print("Sucre (gr) > ")
      var sucreaajouter = readInt()
      while (!(sucreaajouter >= 0)) {
        println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
        print(" Sucre (g) > ")
        sucreaajouter = readInt()
      }
      if (sucreaajouter >= 0) {
        machines(machineId).addIngredient("sugar", sucreaajouter)

        print("Lait (A saisir en ml) > ")
        var laitaajouter = readInt()
        while (!(laitaajouter >= 0)) {
          println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
          print("Lait (a saisir en ml) > ")
          laitaajouter = readInt()
        }
        if (laitaajouter >= 0) {
          machines(machineId).addIngredient("milk", laitaajouter)

          println("Les stocks ont été mis à jour avec succès")

          println("Nouveaux niveaux de stock :")

          println("Café : " + machines(machineId).coffee + "g")
          println("Sucre : " + machines(machineId).sugar + "g")
          printf("Lait : %.3f litres\n", machines(machineId).milk / 1000.0)

          println("Retour au menu principal ...")
        }
      }
    }
  }
}