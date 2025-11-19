import io.StdIn._
import math._
import scala.collection.mutable.ArrayBuffer
import java.io.{BufferedWriter, FileNotFoundException, FileWriter, IOException, PrintWriter}
import scala.io.Source

object Main {
  class Machine (var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println("Choix d'igrédient invalide")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient match {
        case "milk" => if (milk < amount) {
          println("Quantité de café insuffisante")
          return false
        } else {
          milk -= amount
          return true
        }
        case "sugar" => if (sugar < amount) {
          println("Quantité de sucre insuffisante")
          return false
        } else {
          sugar -= amount
          return true
        }
        case "coffee" => if (coffee < amount) {
          println("Quantité de lait insuffisante")
          return false
        } else {
          coffee -= amount
          return true
        }
        case _ => println("Ingrédient invalide")
          return false
      }
    }
  }

  //Méthode de gestion du CSV
  def loadcsv(filename: String):  ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis machines.csv...")
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().toArray
      if (lines.isEmpty) throw new IOException("Le fichier CSV est plein !")


      var i = 0
      for (line <- lines.tail) {
        i+=1
        val parts = line.split(",")
        if (parts.length == 4) {
          machines += new Machine(i,parts(0), parts(1).toInt, parts(2).toInt, parts(3).toInt)
          println(s"Machine "+ i +" chargée:")
          println("   ID : "+i)
          println("   Code PIN : "+parts(0))
          println("   Milk :  "+ parts(1)+" ml")
          println("   Sugar : "+parts(2)+" g")
          println("   Coffee : "+parts(3)+" g")
        } else {
          throw new IOException(s"Format de csv invalide à la ligne: $line")
        }
      }
      println("Réussite du chargement "+i+" machine(s).")
      source.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur: Fichier introuvable. Veuillez vérifier le chemin du fichier et réessayer.")
        println("Erreur: impossible de charger ou sauvegarder les machines.\nFin du programme.")
        System.exit(1)
      case exception: IOException =>
        println(s"Erreur survenue lors de la lecture du fichier: ${exception.getMessage}")
        println("Erreur: impossible de charger ou sauvegarder les machines.\nFin du programme.")
        System.exit(1)
    }
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println("Sauvegarde "+ machines.length +" de machines à machines.csv...")
      val writer = new BufferedWriter(new FileWriter(filename))
      // Write the header
      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
      // Write machine data
      var i = 0
      for (machine <- machines) {
        i+=1
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      println("Sauvegarde du fichier complètée.")
      writer.close()
    } catch {
      case e =>
        println(s"Erreur: impossible d'écrire dans machines.csv. File may be read-only or locked.")
        System.exit(1)
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var tentatives = 3
    println("Vous avez choisi la machine "+ (machineId+1))
    while (tentatives > 0) {
      println(s"Veuillez entrer le code PIN (tentatives restantes: $tentatives)")
      var pin = readLine("> ")
      if (pin == machines(machineId).pincode) {
        println("Accès autorisé à la machine " + (machineId+1))
        return true
      } else {
        tentatives -= 1
        if(tentatives > 0){
          println("Code PIN incorrect. Veuillez réessayer.")
        }else {
          println("Accès refusé! Nombre maximal de tentatives atteint.\nFin de la cession\n")
          return false
        }
      }
    }
    return false
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId+1) + "\nVeuillez entrer un nouveau code a 6 chiffres")
    var nouveaupin = readLine("> ")
    while (nouveaupin.length != 6) {
      println("Veuillez entrer un code valide ! ")
      nouveaupin = readLine("> ")
    }
    machines(machineId).pincode = nouveaupin
    println("Nouveau code PIN mis à jour avec succès!\nRetour au menu principal...\n ")
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    var Code = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var TWINT = ""
    for (i <- 1 to 5) {
      var index = (random() * 36).toInt
      TWINT += Code(index)
    }

    //var StockCafe =
    machines(machineId).coffee
    //var StockSucre =
    machines(machineId).sugar
    //var StockLait =
    machines(machineId).milk
    var TailleLatte = 0
    var choix = 0
    var Cafe = 0
    var Lait = 0
    var Prix = 0.0
    var prixboisson = 0.0
    var prixsucre = 0.1
    var prixlait = 0.05
    var Dose = 0
    var SucrAffichage = ""
    var LaitAffichage = ""

    println("\nMachine "+(machineId+1)+"\nVous êtes dans le mode client. Veuillez choisir votre boisson\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Large)")
    var ChoixCafe = readLine("> ").toInt
    while ((ChoixCafe != 1) && (ChoixCafe != 2) && (ChoixCafe != 3)) {
      println("\nVeuillez choisir parmis les propositions ci-dessous.\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Large)")
      ChoixCafe = readLine("> ").toInt
    }
    var boisson = ""
    if (ChoixCafe == 1) {
      machines(machineId).coffee -= 8
      Prix = 2.0
      prixboisson = 2.0
      boisson = "Expresso"
    }
    if (ChoixCafe == 2) {
      machines(machineId).coffee -= 6
      machines(machineId).milk -= 100
      Prix = 2.5
      prixboisson = 2.5
      boisson = "Cappuccino"
    }
    if (ChoixCafe == 3) {
      while ((TailleLatte != 1) && (TailleLatte != 2) && (TailleLatte != 3)) {
        println("Veuillez sélectionner la taile de votre Latte:\n1) Petit (CHF 2.70) \n2) Moyen (CHF 3.20) \n3) Large (CHF 3.70) ")
        TailleLatte = readLine("> ").toInt
        if (TailleLatte == 1) {
          machines(machineId).coffee -= 6
          machines(machineId).milk -= 100
          Prix = 2.7
          prixboisson = 2.7
          boisson = "Latte petit"
        } else if (TailleLatte == 2) {
          machines(machineId).coffee -= 8
          machines(machineId).milk -= 120
          Prix = 3.2
          prixboisson = 3.2
          boisson = "Latte moyen"
        } else if (TailleLatte == 3) {
          machines(machineId).coffee -= 12
          machines(machineId).milk -= 150
          Prix = 3.7
          prixboisson = 3.7
          boisson = "Latte large"
        } else {
          println("Veuillez entrer une valeur valide! ")
          TailleLatte = readLine("> ").toInt
        }
      }
    }
    //SUCRE SUPLéMENTAIRE
    var SucreSupp = 0
    while ((SucreSupp != 1) && (SucreSupp != 2) && (SucreSupp != 3) && (SucreSupp != 4)) {
      println("Voulez vous ajouter du  sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      SucreSupp = readLine("> ").toInt
      if ((SucreSupp != 1) && (SucreSupp != 2) && (SucreSupp != 3) && (SucreSupp != 4)) {
        println("Choisissez un montant valide! ")
        SucreSupp = readLine("> ").toInt
      }
    }
    if (SucreSupp == 1) {
      SucrAffichage = "Sans sucre"
      prixsucre = 0.0
      machines(machineId).sugar -= 0
    } else if (SucreSupp == 2) {
      SucrAffichage = "Peu de sucre (5g)"
      Prix += 0.1
      prixsucre = 0.1
      machines(machineId).sugar -= 5
    } else if (SucreSupp == 3) {
      SucrAffichage = "Sucre moyen (10g)"
      Prix += 0.2
      prixsucre = 0.2
      machines(machineId).sugar -= 10
    } else if (SucreSupp == 4) {
      SucrAffichage = "Beaucoup de sucre (15g)"
      Prix += 0.3
      prixsucre = 0.3
      machines(machineId).sugar -= 15
    }
    //LAIT SUPPLéMENTAIRE
    Dose = 0
    var LaitSupp = 0
    if ((ChoixCafe == 2) || (ChoixCafe == 3)) {
      while ((LaitSupp != 1) && (LaitSupp != 2)) {
        println("Voulez vous du lait supplémentaire ?\n1) Oui\n2) Non")
        LaitSupp = readLine("> ").toInt
        if ((LaitSupp != 1) && (LaitSupp != 2)) {
          println("Veuillez entrer une valeur valide! ")
          LaitSupp = readLine("> ").toInt
        }
      }
      if (LaitSupp == 1) {
        while (Dose < 1 || Dose > 3) {
          println("Combien de doses souhaitez-vous ? (vous avez le droit à 3 doses max)")
          Dose = readLine("> ").toInt
          if (Dose < 1 || Dose > 3) {
            println("Veuillez entrer une valeur entre 1 et 3! ")
            Dose = readLine("> ").toInt
          }
        }
        if (Dose == 1) {
          Prix += 0.05
          prixlait = 0.05
          LaitAffichage = " 1 dose "
          machines(machineId).milk -= 50
        } else if (Dose == 2) {
          Prix += 0.1
          prixlait = 0.1
          LaitAffichage = " 2 doses "
          machines(machineId).milk -= 100
        } else {
          Prix += 0.15
          prixlait = 0.15
          LaitAffichage = " 3 doses "
          machines(machineId).milk -= 150
        }
      } else if(LaitSupp == 2){
        LaitAffichage = " 0 dose "
        prixlait = 0.0
      }
    } else if ((ChoixCafe == 1) || (LaitSupp == 2)) {
      LaitAffichage = " 0 dose "
      prixlait = 0.0
    }
    if ((machines(machineId).coffee < Cafe) || (machines(machineId).sugar < ((SucreSupp -1)* 5)) || (machines(machineId).milk < (Dose * 50))) {
      if (machines(machineId).coffee < Cafe) {
        println("Erreur: il n'y a pas assez de poudre à café pour votre boisson")
      } else if (machines(machineId).sugar < ((SucreSupp -1)* 5)) {
        println("Erreur: il n'y a pas assez de sucre pour votre boisson")
      } else if (machines(machineId).milk < (Dose * 50)) {
        println("Erreur: il n'y a pas assez de lait pour votre boisson")
      }
      println("Veuillez choisir une autre machine ou verifier les stocks dans admin\n")
      //le choix de la machine viendra après la demande du choix de mode
      return false
    }

    println("Votre commande: " + boisson + " - " + prixboisson + " CHF, avec " + SucrAffichage + " - " + prixsucre + " CHF et" + LaitAffichage + "de lait" + " - " + prixlait + " CHF =")
    printf("%.2f", Prix)
    println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + TWINT)
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.\n")
    //Apres paiement
    println("Préparation de votre boisson...\n[...]\nVotre " + boisson)

    machines(machineId).removeIngredient("coffee", Cafe)
    machines(machineId).removeIngredient("sugar", (SucreSupp-1 )* 5)
    machines(machineId).removeIngredient("milk",Dose*50)

    println(" est prêt ! Bonne dégustation !\n")
    return true
  }
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var StockCafe = machines(machineId).coffee
    var StockSucre = machines(machineId).sugar
    var StockLait = machines(machineId).milk
    if(StockCafe <= 0){
      println("0")
    }
    if(StockSucre <= 0){
      println("0")
    }
    if(StockLait <= 0){
      println("0")
    }

    var choixrestock = 0
    println("Voici les stocks: \n")
    println("Poudre à cafe: " + StockCafe +" grammes")
    println("Sucre: " + StockSucre +" grammes")
    println("Lait: " + StockLait +" millilitres")
    println("\nVoulez vous ajouter des ingrédients ?\n1) Oui\n2) Non ")
    choixrestock = readLine("> ").toInt
    if(choixrestock == 1) {
      var quantiteCafe = -1
      while(quantiteCafe < 0) {
        quantiteCafe = readLine("Ajout de poudre à café:\n> grammes ").toInt
        if(quantiteCafe < 0){
          println("Veuillez mettre des valeurs positives SVP.")
        }
      }
      machines(machineId).addIngredient("coffee", quantiteCafe)

      var quantiteSucre = -1
      while(quantiteSucre < 0) {
        quantiteSucre = readLine("Ajout de sucre:\n> grammes ").toInt
        if(quantiteSucre < 0){
          println("Veuillez mettre des valeurs positives SVP.")
        }
      }
      machines(machineId).addIngredient("sugar", quantiteSucre)

      var quantiteLait = -1
      while(quantiteLait < 0) {
        quantiteLait = readLine("Ajout de lait:\n> millilitres ").toInt
        if(quantiteLait < 0){
          println("Veuillez mettre des valeurs positives SVP.")
        }
      }
      machines(machineId).addIngredient("milk", quantiteLait)
      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
    }else{
      println(" ")
    }
  }
  def machine (nbMachines: Int): Int = {
    println("Veuillez choisir votre machine (1-"+nbMachines+").")
    var machineId = readLine("> ").toInt
    while ((machineId < 1) || (machineId > nbMachines)) {
      println("Veuillez choisir une machine entre 1 et 5 SVP.")
      machineId = readLine("> ").toInt
    }
    return machineId -1
  }


  def main(args: Array[String]): Unit = {
    val filename = "src/machines.csv"
    val machines = loadcsv(filename)
    val nbMachines = machines.size
    var quitter = false
    var Code = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var TWINT = ""
    for (i <- 1 to 5) {
      var index = (random() * 36).toInt
      TWINT += Code(index)
    }
    var choix = 0
    while (!quitter) {
      println("Nospresso Cafe\n" + "Sélectionnez votre mode:\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter")
      choix = readLine("> ").toInt
      while ((choix != 1) && (choix != 2) && (choix != 3)) {
        println("Veuillez entrer une valeur valide.\n")
        println("Nospresso Cafe\n" + "Sélectionnez votre mode:\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter")
        choix = readLine("> ").toInt
      }
      if (choix == 1) {
        var machineId = machine(nbMachines)
        serveClient(machineId, machines)
      }
      if (choix == 2) {
        var machineId = machine(nbMachines)
        if (!validatePin(machineId, machines)){
          quitter = true
        }else {
          println("1) Tapez 1 pour changer de PIN\n2) Tapez 2 pour vérifier les stocks")
          var choixadmin = readLine("> ").toInt
          while (choixadmin != 1 && choixadmin != 2) {
            println("1) Tapez 1 pour changer de PIN\n2) Tapez 2 pour modifier les stocks")
            choixadmin = readLine("> ").toInt
          }
          if (choixadmin == 1) {
            updatePin(machineId, machines)
          }
          if (choixadmin == 2) {
            restockMachine(machineId, machines)
          }
        }
      }
      if(choix == 3){
        println("Merci de votre visite et à bientôt.")
        quitter = true
        savecsv(filename,machines)
      }
    }

  }
}

