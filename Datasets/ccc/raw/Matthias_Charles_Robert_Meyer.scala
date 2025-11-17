import io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object Main {
  class Machine(idc: Int, pincodec: String, milkc: Int, sugarc: Int, coffeec: Int) {
    var id = idc
    var pincode = pincodec
    var milk = milkc
    var sugar = sugarc
    var coffee = coffeec

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      } else if (ingredient == "coffee") {
        coffee += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "milk" && milk > amount) {
        milk -= amount
        return true
      } else if (ingredient == "sugar" && sugar > amount) {
        sugar -= amount
        return true
      } else if (ingredient == "coffee" && coffee > amount) {
        coffee -= amount
        return true
      } else {
        return false
      }

    }
  }
  def main(args: Array[String]): Unit = {
    var machines = ArrayBuffer[Machine]()
    machines = loadcsv("machines.csv")
    var mode = 0
    var machineId = 0
    do {
      mode = 0
      var test = false
      do {
        println("\n\n        Nospresso Café")
        println("Veuillez sélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print(">")
        mode = readInt()
        if (mode > 0 && mode < 4) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
      if (mode != 3) {
        var redo = true
        do {
          do {
            print("Machine sélectionnée (1-5) > ")
            machineId = readInt()
            machineId -= 1
          } while (machineId < 0 || machineId > 4)
          if (mode == 1) {
            if (serveClient(machines(machineId))) {
              redo = false
            }
          } else if (mode == 2) {
            redo = false
            if(validatePin(machines(machineId))) {
              var choix = 0
              do {
                println("Que voulez vous faire ?")
                println("1) Remplir les stocks")
                println("2) Changer le code PIN")
                print(">")
                choix = readInt()
              }while(choix<1||choix>2)
              if(choix == 1){
                restockMachine(machines(machineId))
              }else if(choix == 2){
                updatePin(machines(machineId))
              }

            }else{
              mode = 3
            }
          }
        }while(redo == true)
      }
    }while(mode!=3)
    savecsv("machines.csv", machines)
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val fr = Source.fromFile(filename)
    }
    catch {
      case _ => {
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(0)
      }
    }
    println("Chargement des machines depuis machines.csv...\n")
    val fr = Source.fromFile(filename)
    val lignefr = fr.reset.getLines
    var i = 0
    var ligne = lignefr.next
    while (!lignefr.isEmpty) {
      ligne = lignefr.next
      val uneMachine = ligne.split(",")
      machines += new Machine(i + 1, uneMachine(0), uneMachine(1).toInt, uneMachine(2).toInt, uneMachine(3).toInt)
      println("Machine "+machines(i).id+" chargée :")
      println("    ID: "+machines(i).id)
      println("    Code PIN: "+machines(i).pincode)
      println("    Lait: "+machines(i).milk.toFloat/1000+"L")
      println("    Sucre: "+machines(i).sugar+"g")
      println("    Café: "+machines(i).coffee+"g")
      i += 1
    }
    println(i+" machine(s) chargée(s) avec succès.")
    fr.close
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(filename)
    }
    catch {
      case _ => {
        println("Erreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(0)
      }
    }
    println("Sauvegarde de "+machines.size+" machines dans machines.csv...")
    val fw = new PrintWriter(filename)
    fw.println("PINCODE,MILK,SUGAR,COFFEE")
    for (i <- machines) {
      fw.println(i.pincode + "," + i.milk + "," + i.sugar + "," + i.coffee)
    }
    fw.close
    println("Fichier sauvegardé avec succès.")
  }


  def serveClient(machine: Machine): Boolean = {
    var boisson = 0
    var test = false
    do {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print(">")
      boisson = readInt()
      if (boisson > 0 && boisson < 4) {
        test = true
      } else {
        println("Choix Incorrect !")
      }
    } while (!test)
    var taille = 0
    if (boisson == 3) {
      test = false
      do {
        println("Quelle taille voulez-vous ?")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        print(">")
        taille = readInt()
        if (taille > 0 && taille < 4) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
    }
    var sucre = 0
    test = false
    do {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print(">")
      sucre = readInt()
      if (sucre > 0 && sucre < 5) {
        test = true
      } else {
        println("Choix Incorrect !")
      }
    } while (!test)
    var doses = 0
    if (boisson == 2 || boisson == 3) {
      var lait = 0
      test = false
      do {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")
        lait = readInt()
        if (lait == 1 || lait == 2) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
      if (lait == 1) {
        test = false
        do {
          println("Combien de dose ?")
          print(">")
          doses = readInt()
          if (doses > 0 && doses < 4) {
            test = true
          } else {
            println("Choix Incorrect")
          }
        } while (!test)
      }
    }
    // VERIFICATION DES STOCKS
    var cafe_use = 0
    var sucre_use = 0
    var lait_use = 0
    var prix = 0.0
    if (boisson == 1) {
      cafe_use = 8
      prix = 2
    } else if (boisson == 2) {
      cafe_use = 6
      lait_use = 100
      prix = 2.5
    } else if (taille == 1) {
      cafe_use = 6
      lait_use = 120
      prix = 2.7
    } else if (taille == 2) {
      cafe_use = 8
      lait_use = 150
      prix = 3.2
    } else {
      cafe_use = 12
      lait_use = 200
      prix = 3.7
    }
    if (sucre == 2) {
      sucre_use = 5
      prix = prix + 0.1
    } else if (sucre == 3) {
      sucre_use = 10
      prix = prix + 0.2
    } else if (sucre == 4) {
      sucre_use = 15
      prix = prix + 0.3
    }
    lait_use = lait_use + (50 * doses)
    var stock_ok = false
    if (cafe_use <= machine.coffee) {
      if (lait_use <= machine.milk) {
        if (sucre_use <= machine.sugar) {
          machine.removeIngredient("coffee", cafe_use)
          machine.removeIngredient("milk", lait_use)
          machine.removeIngredient("sugar", sucre_use)
          stock_ok = true
        } else {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
      } else {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      }
    } else {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stock_ok) {
      printf("Prix total : %.2f CHF\n", prix)
      println("Veuillez payer en utilisant TWINT.")
      var code_paiement = Random.alphanumeric.take(5).mkString
      println("Votre code de paiement est : " + code_paiement)
      println("(en attente de paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé !")
      println("Préparation de votre boisson ...")
      if (boisson == 1) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
        return (true)
      } else if (boisson == 2) {
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
        return (true)
      } else {
        println("Votre Latte est prêt ! Bonne dégustation !")
        return (true)
      }
    } else {
      return (false)
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var code = ""
    var fail = 0
    do {
      println("Entrez le code pin : ")
      code = readLine()
      if (code == machine.pincode) {
        println("Mise à jour du code PIN pour la Machine 5.")
        return (true)
      } else {
        fail += 1
        println("Code PIN incorrect. "+(3-fail)+" tentatives restantes.")
        if(fail==3){
          print("\nTrop de tentatives échouées. Fin du programme.")
          return (false)
        }
      }
    }while(code!=machine.pincode)
    return (false)
  }

  def restockMachine(machine: Machine): Unit = {
    println("\nNiveaux de stock actuels:")
    println("Poudre de café : " + machine.coffee + "g")
    println("Sucre : " + machine.sugar + "g")
    printf("Lait : %.2fL\n",(machine.milk).toFloat/1000)
    println("\nEntrez les quantités à ajouter :")
    var add_cafe = 0
    var add_lait = 0.0
    var add_sucre = 0
    do {
      print("Poudre de café > ")
      add_cafe = readInt()
    } while (add_cafe < 0)
    machine.addIngredient("coffee", add_cafe)
    do {
      print("Sucre > ")
      add_sucre = readInt()
    } while (add_sucre < 0)
    machine.addIngredient("sugar", add_sucre)
    do {
      print("Lait > ")
      add_lait = readDouble()
    } while (add_lait < 0)
    machine.addIngredient("milk", (add_lait*1000).toInt)
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def updatePin(machine: Machine): Unit = {
    println("Mise à jour du code PIN pour la Machine "+(machine.id)+".")
    var newPin = ""
    do{
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPin = readLine()
    }while(!newPin.forall(Character.isDigit)||newPin.size!=6)
    machine.pincode = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
}