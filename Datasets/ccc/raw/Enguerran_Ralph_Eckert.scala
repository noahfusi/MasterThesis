import scala.io.StdIn.{readDouble, readInt, readLine}
import java.io.{PrintWriter,FileWriter}
import collection.mutable.ArrayBuffer
import scala.io.Source

class Machine (var id:Int, var pincode:String, var milk:Int, var sugar:Int, var coffee:Int){
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if(ingredient=="MILK"){
      milk += amount
    } else if (ingredient == "SUGAR") {
      sugar += amount
    } else if (ingredient == "COFFEE") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    var res = false
    if(ingredient=="MILK"){
      if(milk >= amount) {
        milk -= amount
        res = true
      }
    } else if (ingredient == "SUGAR") {
      if(sugar >= amount) {
        sugar -= amount
        res = true
      }
    } else if (ingredient == "COFFEE") {
      if(coffee >= amount) {
        coffee -= amount
        res = true
      }
    }
    return res
  }
}



object Main {

  var machines = ArrayBuffer[Machine]()
  var nbMachines = 0
  var tempInt = 0
  var tempDouble = 0.0
  var tempStr = ""
  var tempBool = false
  var tempPlural = ""
  var doExit = false
  var currentMachine = -1
  var newCode: Array[Char] = Array.fill(6)('-')
  var Boisson$ = ""
  var Sucre$ = ""
  var Lait$ = ""
  var PrixTotal$ = ""
  var cost = 0.0
  var lcost = 0.0
  var qteCafe = 0
  var qteSucre = 0
  var qteLait = 0
  var erreur = false
  var choix_initial = 0
  var choix_boisson = 0
  var choix = 0
  var choix_lait = 0
  val car_mdp = Array('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val mdp = Array.fill(5)('0')
  var ioError = true

  def main(args: Array[String]): Unit = {

    loadcsv("machines.csv")
    doExit = ioError

    while (!doExit) {
      choix_initial = inputChoice("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ", 3)

      if (choix_initial == 1) {
        tempBool = false
        while(!tempBool) {
          currentMachine = inputChoice("\nMachine sélectionnée (1-5) > ", 5)-1
          tempBool = serveClient(currentMachine)
        }

      } else if (choix_initial == 2) {
        currentMachine = inputChoice("Machine sélectionnée (1-5) > ", 5) - 1
        tempBool = validatePin(currentMachine)
        if (!tempBool) {
          println("\nTrop de tentatives échouées. Fin du programme.")
          doExit = true
        } else {
          choix = inputChoice("1) Gestion des stocks\n2) Modification du code PIN\n3) Retour\n> ", 3)
          if (choix == 1) {
            restockMachine(currentMachine)
          } else if (choix == 2) {
            updatePin(currentMachine)
          }

        }
      }  else if (choix_initial == 3) {
        doExit = true
        if(machines.nonEmpty) {
          savecsv("machines.csv", machines)
        }
      }
      println
    }
    if(ioError) {
      println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }

  def validatePin(machineId: Int): Boolean = {
    tempStr = ""
    tempInt = 0
    while((tempStr != machines(machineId).pincode) && (tempInt<3))
    {
      tempInt+=1
      print("Entrez le code PIN :\n> ")
      tempStr = readLine()
      if(tempStr==machines(machineId).pincode)
      {
        println("Accès accordé à la Machine " + (machineId+1) +"\n")
      } else {
        if(tempInt==2) {
          tempPlural=" tentative restante."
        } else {
          tempPlural=" tentatives restantes."
        }
        println("Code PIN incorrect. " + (3-tempInt) + tempPlural)
      }
    }

    return (tempStr==machines(machineId).pincode)
  }

  def updatePin(machineId: Int): Unit = {
    println("\nMise à jour du code PIN pour la Machine " + (machineId+1) + ".")
    tempBool = false
    while (!tempBool)
    {
      tempStr = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if(tempStr.length ==6)
      {
        tempBool = true
        newCode = tempStr.toCharArray
        for (i <- 0 to 5) {
          if((newCode(i)<'0') || (newCode(i)>'9')) {
            tempBool = false
          }
        }
        if (tempBool) {
          machines(machineId).pincode=tempStr
          println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
        }
      }
    }
  }

  def serveClient(machineId: Int): Boolean = {
    erreur = true
    choix_boisson = 0
    qteCafe = 0
    qteSucre = 0
    qteLait = 0
    cost = 0.0
    choix_boisson = inputChoice("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>", 3)
    if (choix_boisson == 1) {
      Boisson$ = "Expresso"
      PrixTotal$ = "CHF 2.00"
      cost = 2
      qteCafe = 8
      qteLait = 0
      Lait$ = "Non"
    } else if (choix_boisson == 2) {
      Boisson$ = "Capuccino"
      PrixTotal$ = "CHF 2.50"
      cost = 2.5
      qteCafe = 6
      qteLait = 100
    } else if (choix_boisson == 3) {
      choix = inputChoice("\nQuelle taille de Latte désirez-vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHR 3.70\n>", 3)
      if (choix == 1) {
        Boisson$ = "Latte (Petit)"
        PrixTotal$ = "CHF 2.70"
        cost = 2.7
        qteCafe = 6
        qteLait = 120
      } else if (choix == 2) {
        Boisson$ = "Latte (Moyen)"
        PrixTotal$ = "CHF 3.20"
        cost = 3.2
        qteCafe = 8
        qteLait = 150
      } else if (choix == 3) {
        Boisson$ = "Latte (Grand)"
        PrixTotal$ = "CHF 3.70"
        cost = 3.7
        qteCafe = 12
        qteLait = 200
      }
    }
    choix = inputChoice("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>", 4)
    if (choix == 1) {
      Sucre$ = "Sans sucre"
      qteSucre = 0
    } else if (choix == 2) {
      Sucre$ = "Peu (5g)"
      qteSucre = 5
      cost += 0.1
      PrixTotal$ = PrixTotal$ + " + CHF 0.10"
    } else if (choix == 3) {
      Sucre$ = "Moyen (10g)"
      qteSucre = 10
      cost += 0.2
      PrixTotal$ = PrixTotal$ + " + CHF 0.20"
    } else if (choix == 4) {
      Sucre$ = "Beaucoup (15 g)"
      qteSucre = 15
      cost += 0.3
      PrixTotal$ = PrixTotal$ + " + CHF 0.30"
    }
    if (choix_boisson != 1) {
      choix = inputChoice("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n>", 2)
      if (choix == 1) {
        choix_lait = inputChoice("\nCombien de doses (1-3) ?\n>", 3)
        if ((choix_lait > 0) && (choix_lait < 4)) {
          Lait$ = "Oui, " + choix_lait + " dose"
          if (choix_lait > 1) {
            Lait$ = Lait$ + "s"
          }
          qteLait += choix_lait * 50
          lcost = 0.05 * choix_lait
          PrixTotal$ = PrixTotal$ + " + CHF " + f"$lcost%1.2f"
          cost += lcost
        }
      } else if (choix == 2) {
        Lait$ = "Non"
      }
    }
    println("\nBoisson sélectionnée : " + Boisson$)
    println("Niveau de sucre : " + Sucre$)
    println("Lait supplémentaire : " + Lait$)
    if (qteCafe > machines(currentMachine).coffee) {
      println("\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez sélectionner une autre machine.")
    } else if (qteLait > machines(currentMachine).milk) {
      println("\nErreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez sélectionner une autre machine.")
    } else if (qteSucre > machines(currentMachine).sugar) {
      println("\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez sélectionner une autre machine.")
    } else {
      machines(currentMachine).removeIngredient("COFFEE", qteCafe)
      machines(currentMachine).removeIngredient("MILK", qteLait)
      machines(currentMachine).removeIngredient("SUGAR", qteSucre)
      if (PrixTotal$.length < 12) {
        println("Prix total : " + PrixTotal$)
      } else {
        println("Prix total : " + PrixTotal$ + " = CHF " + f"$cost%1.2f")
      }
      for (i <- 0 to 4) {
        mdp(i) = car_mdp((car_mdp.length * math.random()).toInt)
      }
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est " + mdp.mkString + "\n(En attente de validation de paiement...)")
      Thread.sleep(3000)
      println("\nMerci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
      Thread.sleep(5000)
      println("\nVotre " + Boisson$ + " est prêt ! Bonne dégustation !")
      erreur = false
    }

    return !erreur
  }

  def restockMachine(machineId: Int): Unit = {
    val tempD = (machines(machineId).milk.toDouble)/1000
    println("\nNiveaux de stock actuels :\n    Poudre de café: " + machines(machineId).coffee + "g\n    Sucre         : " + machines(machineId).sugar + "g\n    Lait          : " + f"$tempD%1.2f" + "L\n\nRéapprovisionnement des stocks...\nEntrez les quantités à ajouter :")
    tempInt = -1
    while(tempInt < 0) {
      print("Poudre de café > ")
      tempInt=readInt()
    }
    machines(machineId).addIngredient("COFFEE", tempInt)
    tempInt = -1
    while(tempInt < 0) {
      print("Sucre > ")
      tempInt=readInt()
    }
    machines(machineId).addIngredient("SUGAR", tempInt)
    tempDouble = -1.0
    while(tempDouble < 0) {
      print("Lait > ")
      tempDouble=readDouble()
    }
    machines(machineId).addIngredient("MILK", (tempDouble*1000).toInt)
    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }

  def inputChoice(whatMessage: String, maxValue: Int):Int = {
    tempInt = 0
    while(tempInt==0) {
      val b = readLine(whatMessage)
      if ((b.length == 1) && (b > "0") && (b < ('0' + maxValue).toString)) {
        tempInt = b.toInt
      }
    }
    return tempInt
  }

  def loadcsv(filename: String) : ArrayBuffer[Machine] = {
    println("Chargement des machines depuis " + filename + "...")
    machines.clear()
    ioError = true
    try {
      val fr = Source.fromFile(filename)
      //val fr = Source.fromFile("machines.csv")
      val lignefr = fr.reset.getLines
      var ligne = lignefr.next
      nbMachines = 0
      while(!lignefr.isEmpty){
        ligne = lignefr.next
        nbMachines += 1
        val v2= ligne.split(",")(0)
        val v3 = ligne.split(",")(1).toInt
        val v4= ligne.split(",")(2).toInt
        val v5 = ligne.split(",")(3).toInt
        val v6 = v3.toDouble/1000
        machines += new Machine(nbMachines, v2, v3, v4, v5)
        println("\nMachine " + nbMachines + " chargée :\nID : " + nbMachines + "\nCode PIN : " + v2 + "\nLait : " + f"$v6%1.3f" + "L\nSucre : " + v4 + "\nCafé : " + v5)
      }
      println("\n" + nbMachines + " machine(s) chargée(s) avec succès.\n")
      ioError = false
    }
    catch {
      case ex : java.io.FileNotFoundException =>  println("\nErreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.\n")
      case _=> println("\nErreur : Lecture du fichier impossible.\n")
    }
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println("\nSauvegarde de " + nbMachines + " machines dans " + filename + "...")
    ioError = true
    try {
      val fw = new PrintWriter(filename)
      val header = "PINCODE,MILK,SUGAR,COFFEE"
      fw.println (header)
      for (i<-1 to nbMachines) {
        val lineout = machines(i-1).pincode + "," + machines(i-1).milk + "," + machines(i-1).sugar + "," + machines(i-1).coffee
        fw.println(lineout)
      }
      fw.close
      println("Fichier sauvegardé avec succès.")
      ioError = false
    }
    catch {
      case ex : java.nio.file.AccessDeniedException =>  println("\nErreur : Echec de l'écriture dans " + filename + ".\nLe fichier peut être verrouillé ou en écriture seule.")
      case _=> println("\nErreur : Sauvegarde impossible.")
    }
  }

}
