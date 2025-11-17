import io.StdIn._
import scala.util.Random
import scala.io.Source
import java.io.{PrintWriter,FileWriter}
import scala.collection.mutable.ArrayBuffer



object Main {

  val nbMachines = 5
  val machines = ArrayBuffer[Machine]()
  var accesValide = false
  var prixBoisson = 0.0
  var prixSucre = 0.0
  var prixLait = 0.0
  var prixTotal = prixBoisson + prixSucre + prixLait
  var boucleModeAdmin = true


  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    try {
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines
      var skip = 0
      var id = 1
      for (ligne <- lignefr) {
        if (skip > 0) {
          val valeur = ligne.split(",")
          var pincode = valeur(0)
          var milk = valeur(1).toInt
          var sugar = valeur(2).toInt
          var coffee = valeur(3).toInt
          id += 1
          machines.append(new Machine(id, pincode, milk, sugar, coffee))
        }
        skip += 1


      }
      fr.close()

    }
    catch {
      case _: java.io.FileNotFoundException => println("Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        machines+=new Machine(0,"",0,0,-1)

    }
    machines

  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(new FileWriter(filename))
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      fw.close()
      println("Données sauvegardées dans le fichier.")
    }
    catch {
      case ex: Exception =>
        println("Erreur : Echec de l’écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")

    }
  }


  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {


    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "Lait") {
        milk += amount
      }
      else if (ingredient == "Sucre") {
        sugar += amount
      }
      else if (ingredient == "Cafe") {
        coffee += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "Lait" && amount <= milk) {
        milk -= amount
        true
      } else if (ingredient == "Sucre" && amount <= sugar) {
        sugar -= amount
        true
      } else if (ingredient == "Cafe" && amount <= coffee) {
        coffee -= amount
        true
      } else {
        false
      }
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var essai = 0
    val machine = machines(machineId)
    var SaisiePIN = readLine("Entrez le code PIN: ").toString
    while (essai != 3) {
      if (SaisiePIN == machine.pincode) {
        println("Accès accordé à la machine " + (machineId + 1))
        boucleModeAdmin = false
        return true
      }
      else {
        essai += 1
        if (essai == 3) {
          println("Code PIN incorrect.  " + (3 - essai) + " tentatives restantes. ")
          println("Trop de tentatives échouées. Fin du programme.")


        }
        else {
          println("Accès refusé à la machine " + (machineId + 1))
          println("Code PIN incorrect.  " + (3 - essai) + " tentatives restantes. ")
          SaisiePIN = readLine(">").toString
        }

      }
    }
    false
  }


  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId + 1))
    val machine = machines(machineId)
    var boucleNewPIN = true
    while (boucleNewPIN) {
      var newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if (newPin.length != 6) {
        println("Erreur : Le code PIN doit avoir exactement 6 chiffres.")
      }
      else {
        boucleNewPIN = false
        machine.pincode = newPin
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
      }
    }
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val machine = machines(machineId)
    var boisson = readLine("Veuillez sélectionner votre boisson : \n" + "1) Expresso - CHF 2.00\n" + "2) Cappuchino - CHF 2.50\n" + "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" + ">").toInt

    if (boisson == 1) {
      if (machine.coffee >= 8) {
        prixBoisson += 2.00
        machines(machineId).removeIngredient("Cafe", 8)

      }
      else {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false

      }
    }

    if (boisson == 2) {
      if (machine.coffee >= 6 && machine.milk >= 100) {
        prixBoisson += 2.50
        machines(machineId).removeIngredient("Cafe", 6)
        machines(machineId).removeIngredient("Lait", 100)
      }
      else {
        println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false

      }
    }

    if (boisson == 3) {
      var taille = readLine("Quelle taille désirez-vous ? :  \n" + "1) Petit \n" + "2) Moyen \n" + "3) Grand\n" + ">").toInt

      if (taille == 1) {
        if (machine.coffee >= 6 && machine.milk >= 120) {
          prixBoisson += 2.70
          machines(machineId).removeIngredient("Cafe", 6)
          machines(machineId).removeIngredient("Lait", 120)
        }

        else {
          println(
            "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false

        }
      }
      else if (taille == 2) {
        if (machine.coffee >= 8 && machine.milk >= 150) {
          prixBoisson += 3.20
          machines(machineId).removeIngredient("Cafe", 8)
          machines(machineId).removeIngredient("Lait", 200)
        }
        else {
          println("Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }
      else if (taille == 3) {
        if (machine.coffee >= 12 && machine.milk >= 200) {
          prixBoisson += 3.70
          machines(machineId).removeIngredient("Cafe", 12)
          machines(machineId).removeIngredient("Lait", 200)
        }
        else {
          println("Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }
    }
    if (boisson == 1 || boisson == 2 || boisson == 3) {
      val sucre = readLine("Souhaitez-vous ajouter du sucre ? \n" + "1) Sans sucre\n" + "2) Peu (5g) - CHF 0.10\n" + "3) Moyen (10g) - CHF 0.20\n" + "4) Beaucoup (15g) - CHF 0.30\n" + ">").toInt

      if (sucre == 2 && machine.sugar >= 5) {
        machines(machineId).removeIngredient("Sucre", 5)
        prixSucre += 0.10
      }
      else if (sucre == 3 && machine.sugar >= 10) {
        machines(machineId).removeIngredient("Sucre", 10)
        prixSucre += 0.20
      }
      else if (sucre == 4 && machine.sugar >= 15) {
        machines(machineId).removeIngredient("Sucre", 15)
        prixSucre += 0.30
      }

      else if (sucre == 2 || sucre == 3 || sucre == 4) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }

    }

    if (boisson == 2 || boisson == 3) {

      val laitSupplement = readLine("Souhaitez-vous ajouter du lait en supplément ?\n" + "(Disponible uniquement pour Cappuccino et Latte)\n" + "1) Oui\n" + "2) Non\n" + ">").toInt

      if (laitSupplement == 1) {
        var nbdose = readLine("Combien de doses ?\n>").toInt
        while (nbdose < 1 || nbdose > 3) {
          println("Erreur : Les doses doivent être comprises entre 1 et 3")
          nbdose = readLine("Combien de doses ?\n>").toInt
        }
        if (machine.milk >= nbdose * 50) {
          machine.milk -= nbdose * 50
          prixLait += nbdose * 50
        }

        else {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }

    }


    prixTotal = prixBoisson + prixSucre + prixLait
    printf("Prix total: %.2f CHF ", prixTotal)

    val characters = ('A' to 'Z') ++ ('0' to '9')
    val length = 5
    var randomString = ""

    for (_ <- 1 to length) {
      randomString += Random.shuffle(characters).head
    }
    println("Veuillez payer en utilisant Twint." + "\n" + "Votre code de paiement est :" + randomString)
    println("(En attente de validation du paiement...)")
    println("\n" + "Merci ! Votre paiement a été accepté.")

    println("Préparation de votre boisson...")
    println("[...]")
    Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)

    if (boisson == 1) {
      println("Votre expresso est prêt ! Bonne dégustation ! ")
    }

    if (boisson == 2) {
      println("Votre cappuchino est prêt ! Bonne dégustation ! ")
    }

    if (boisson == 3) {
      println("Votre Latte est prêt ! Bonne dégustation ! ")
    }
    true
  }


  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    println("\n" + "Stocks: " + "\n" + "    Poudre à café : " + machine.coffee + "g" + "\n" + "    Lait: " + (machine.milk.toDouble / 1000) + "L" + "\n" + "    Sucre : " + machine.sugar + "g")
    try {
      var SucreAjoute = readLine("Sucre : ").toInt
      var LaitAjoute = readLine("Lait : ").toInt
      var PoudreCafeAjoute = readLine("Poudre à café : ").toInt
      println("Réapprovisionnement des stocks... " + "\n" + "Ajout : ")
      println("Niveaux de stock actuels :")
      println("Poudre de café : " + machines(machineId).coffee + "g")
      println("Sucre : " + machines(machineId).sugar + " g")
      printf("Milk: " + (machines(machineId).milk.toDouble * 0.001) + "L")
      println("\nNiveaux de stocks mis à jour. ")
      machines(machineId).addIngredient("Sucre", SucreAjoute)
      machines(machineId).addIngredient("Lait", LaitAjoute)
      machines(machineId).addIngredient("Cafe", PoudreCafeAjoute)
    }
    catch {
      case ex: NumberFormatException => println("Erreur : Veuillez entrer un nombre valide.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv")
    loadcsv("machines.csv")

    if (machines(0).coffee != -1) {
      while (!accesValide) {
        try {
          val machineIdInput = readLine("Machine sélectionnée(1-" + machines.size + "): > ").toInt
          if (machineIdInput < 1 || machineIdInput > machines.size) {
            println("Erreur: Veuillez sélectionner une machine valable")
          }
          else {
            val machineId = machineIdInput - 1
            val choix = readLine("        Nospresso Café\n" + "Veuillez sélectionner votre mode :\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter\n" + ">").toInt
            if (choix == 1) {
              serveClient(machineId, machines)
            }


            else if (choix == 2) {


              if (validatePin(machineId, machines)) {
                val choixAdmin = readLine("Que souhaitez-vous faire? " + "\n" + "1. Modifier les stocks" + "\n" + "2. Changer le code PIN de la machine" + "\n" + ">").toInt

                if (choixAdmin == 1) {
                  restockMachine(machineId, machines)


                }

                else if (choixAdmin == 2) {
                  updatePin(machineId, machines)

                }
              }
            }
            else if (choix == 3) {
              println("Vous quittez le programme...")
              accesValide = true
            }
            else {
              println("Erreur : Option invalide")
            }
          }
        }
        catch {
          case _: NumberFormatException => println("Erreur veuillez entrer un nombre valide")
        }
      }
      savecsv("machines.csv", machines)
      println("Sauvegarde de " + machines.size + " machines dans machines.csv...")
      println("Fichier sauvegardé avec succès.")
    }
  else{
    println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
  }
  }
}

