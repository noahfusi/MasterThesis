import io.StdIn._
import scala.io._
import scala.util._
import java.io._
import scala.collection.mutable.ArrayBuffer

object Main {

  var error = 0
  val filename = "machines.csv"

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int,
                var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
        ingredient match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient match {
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

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      var tableauMachines: ArrayBuffer[Machine] = new ArrayBuffer[Machine]
      val file = Source.fromFile(filename)
      var lignes = file.getLines()
      var id: Int = 1
      for (ligne <- file.getLines().drop(1)) {
        var infos: Array[String] = ligne.split(",").map(_.trim)
        //println("Machine "+id+" chargée:")
        var pinCode: String = infos(0)
        var milk: Int = infos(1).toInt
        var sugar: Int = infos(2).toInt
        var coffee: Int = infos(3).toInt
        tableauMachines :+= new Machine(id, pinCode, milk.toInt, sugar.toInt, coffee.toInt)
        id += 1
      }
      file.close()
      Thread.sleep(1000)
      tableauMachines
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
      case ex: java.lang.NumberFormatException =>
        println("Erreur : Échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
      case _ =>
        println("Erreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(">> Sauvegarde des machines dans " + filename + "...\n")
    try {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE") // En-tête
      for (i <- machines) {
        fw.println(i.pincode + "," + i.milk + "," + i.sugar + "," + i.coffee)
        //println(machine.pincode + "," + machine.coffee + "," + machine.sugar + "," + machine.coffee) //ceci n'est que pour tester le savecsv
      }
      fw.close()
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
      case ex: java.lang.NumberFormatException =>
        println("Erreur : échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
      case _ =>
        println("Erreur : échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        new ArrayBuffer[Machine]
        Thread.sleep(500)
        sys.exit(1)
    }
  }

  var machines: ArrayBuffer[Machine] = loadcsv(filename)


  def InputManagement(prompt: String, diffOptions: Set[Int]): Int = { //cette fonction me permet à l'utilisateur d'input n'importe quel type d'entrée lors des questions sans crash le programme
    var choix = 0
    do {
      print(prompt)
      val entree_utilisateur = readLine()
      if (entree_utilisateur.nonEmpty && entree_utilisateur.forall(_.isDigit)) {
        choix = entree_utilisateur.toInt
        if (!diffOptions.contains(choix)) println(">> Valeur non autorisée. Veuillez réessayer.")
      } else {
        println(">> Entrée invalide. Veuillez saisir un nombre.")
      }
    } while (!diffOptions.contains(choix))
    return choix
  }


  def quitter(): Unit = {
    val mot = "Au revoir... "
    savecsv(filename, machines)
    for (i <- mot) {
      print(i); Thread.sleep(100)
    }
    Thread.sleep(1000)
    sys.exit(0)
  }

  def menu_principal(): Unit = {

    //start choix client/admin/quitter + plus choix des machines
    val choix = InputManagement("\n=========Nospresso Café=========\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin\n3) Quitter\n> ", Set(1, 2, 3))

    if (choix == 1) {
      val nbrmachine = machines.length
      val machineId = InputManagement("\nVeuillez sélectionner votre Machine (1-" + nbrmachine + "):\n> ", (1 to nbrmachine).toSet)
      val machineSelectionner = machines.find(_.id == machineId).get
      val coffeeStocks = machineSelectionner.coffee
      val sugarStocks = machineSelectionner.sugar
      val milkStocks = machineSelectionner.milk

      serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
    }
    if (choix == 2) menuAdmin()
    if (choix == 3) quitter()
    //end choix
  }



  def serveClient(machineId: Int, coffeeStocks: Int, sugarStocks: Int, milkStocks: Int): Boolean = {

    var boisson = InputManagement("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ", Set(1, 2, 3))

    //start taille, si latte
    if (boisson == 3) {
      val taille = InputManagement("\nVeuillez choisir la taille du Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ", Set(1, 2, 3))
      if (taille == 1) {
        boisson = 3
      };
      if (taille == 2) {
        boisson = 4
      };
      if (taille == 3) {
        boisson = 5
      }
    }
    //end taille latte

    //start quantite sucre
    val sucre = InputManagement("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ", Set(1, 2, 3, 4))
    //end quantite sucre

    //start question lait supplé. y/n Seulement si boisson 2 ou 3
    var laityn = 0
    if (boisson >= 2) {
      laityn = InputManagement("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuchino et Latte)\n1) Oui\n2) Non\n> ", Set(1, 2))
    }
    //end question lait supplé.y/n

    //start cb supplement lait, si lait y
    var supplait = 0
    if (laityn == 1) {
      supplait = InputManagement("Combien de dose ? \n> ", Set(1, 2, 3))
    }
    //end cb supplement lait


    //start Vérif si assez stock
    val erreur_poudre = "\n>> Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n"
    val erreur_lait = "\n>> Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n"
    val erreur_sucre = "\n>> Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n"
    var stocksuffisant: Boolean = false
    val calcul_supplait = supplait * 50
    val qttesucre = (sucre - 1) * 5
    if (sucre > 1) {
      if (boisson == 1 && coffeeStocks >= 8 && sugarStocks >= qttesucre) {
        stocksuffisant = true
      }
      else {
        if (boisson == 2 && coffeeStocks >= 6 && milkStocks >= (100 + calcul_supplait) && sugarStocks >= qttesucre) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && coffeeStocks >= 6 && milkStocks >= (120 + calcul_supplait) && sugarStocks >= qttesucre) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && coffeeStocks >= 8 && milkStocks >= (150 + calcul_supplait) && sugarStocks >= qttesucre) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && coffeeStocks >= 12 && milkStocks >= (200 + calcul_supplait) && sugarStocks >= qttesucre) {
                stocksuffisant = true
              }
              else if (!stocksuffisant) {
                if ((boisson == 1 || boisson == 4) && coffeeStocks < 8) print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && coffeeStocks < 6) print(erreur_poudre)
                if (boisson == 5 && coffeeStocks < 12) print(erreur_poudre)
                if (boisson == 2 && milkStocks < (100 + calcul_supplait)) print(erreur_lait)
                if (boisson == 3 && milkStocks < (120 + calcul_supplait)) print(erreur_lait)
                if (boisson == 4 && milkStocks < (150 + calcul_supplait)) print(erreur_lait)
                if (boisson == 5 && milkStocks < (200 + calcul_supplait)) print(erreur_lait)
                if (sugarStocks < qttesucre) print(erreur_sucre)
                Thread.sleep(2000)
                menu_principal()
              }
            }
          }
        }
      }
    }

    else if (sucre <= 1) { //si pas de sucre
      if (boisson == 1 && coffeeStocks >= 8) {
        stocksuffisant = true
      }
      else {
        if (boisson == 2 && coffeeStocks >= 6 && milkStocks >= (100 + calcul_supplait)) {
          stocksuffisant = true
        }
        else {
          if (boisson == 3 && coffeeStocks >= 6 && milkStocks >= (120 + calcul_supplait)) {
            stocksuffisant = true
          }
          else {
            if (boisson == 4 && coffeeStocks >= 8 && milkStocks >= (150 + calcul_supplait)) {
              stocksuffisant = true
            }
            else {
              if (boisson == 5 && coffeeStocks >= 12 && milkStocks >= (200 + calcul_supplait)) {
                stocksuffisant = true
              }
              else if (!stocksuffisant) {
                if ((boisson == 1 || boisson == 4) && coffeeStocks < 8) print(erreur_poudre)
                if ((boisson == 2 || boisson == 3) && coffeeStocks < 6) print(erreur_poudre)
                if (boisson == 5 && coffeeStocks < 12) print(erreur_poudre)
                if (boisson == 2 && milkStocks < (100 + calcul_supplait)) print(erreur_lait)
                if (boisson == 3 && milkStocks < (120 + calcul_supplait)) print(erreur_lait)
                if (boisson == 4 && milkStocks < (150 + calcul_supplait)) print(erreur_lait)
                if (boisson == 5 && milkStocks < (200 + calcul_supplait)) print(erreur_lait)
                Thread.sleep(2000)
                menu_principal()
              }
            }
          }
        }
      }
    }
    //end stock

    //start calcul prix
    var prix: Double = 0.0
    var prix_total = 0.0
    var calcul_sucre = 0.0
    calcul_sucre = (sucre - 1) * 0.10
    if (boisson == 1) {
      prix = 2.00
    }; else if (boisson == 2) {
      prix = 2.50
    }; else if (boisson == 3) {
      prix = 2.70
    }; else if (boisson == 4) {
      prix = 3.20
    }; else if (boisson == 5) {
      prix = 3.70
    }
    prix_total = (supplait * 0.05) + prix + calcul_sucre
    //end calcul prix

    //start récap et affichage prix
    var boisson_selec = ""
    var sucre_selec = ""
    var supplait_selec = ""
    if (boisson == 1) {
      boisson_selec = "Expresso"
    }; else if (boisson == 2) {
      boisson_selec = "Cappuccino"
    }; else if (boisson == 3) {
      boisson_selec = "Latte (Petit)"
    }; else if (boisson == 4) {
      boisson_selec = "Latte (Moyen)"
    }; else if (boisson == 5) {
      boisson_selec = "Latte (Grand)"
    }
    if (sucre == 1) {
      sucre_selec = "Sans Sucre"
    }; else if (sucre == 2) {
      sucre_selec = "Peu (5g)"
    }; else if (sucre == 3) {
      sucre_selec = "Moyen (10g)"
    }; else if (sucre == 4) {
      sucre_selec = "Beaucoup (15g)"
    }
    print("\nBoisson sélectionner : " + boisson_selec)
    print("\nNiveau de sucre : " + sucre_selec)

    if (boisson != 1) {
      if (supplait == 0) {
        supplait_selec = "Non"
      }; else if (supplait == 1) {
        supplait_selec = "1 * 50ml"
      }; else if (supplait == 2) {
        supplait_selec = "2 * 50ml"
      }; else if (supplait == 3) {
        supplait_selec = "3 * 50ml"
      }
      print("\nLait supplémentaire: " + supplait_selec)
    }
    if (supplait == 0) {
      print("\nPrix total : CHF " + prix.toFloat + "0 +" + " CHF " + calcul_sucre.toFloat + "0" + " = CHF " + prix_total.toFloat + "0\n")
    }
    else {
      print("\nPrix total : CHF " + prix.toFloat + "0 +" + " CHF " + calcul_sucre.toFloat + "0 +" + " CHF " + (supplait * 0.05).toFloat + " = CHF " + prix_total.toFloat + "\n")
    }
    //end récap

    //start paiement
    var statu_paiement: Boolean = false
    val caractere = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    for (i <- 1 to 5) {
      val choixrandom = caractere(Random.nextInt(36))
      code += choixrandom
    }
    print("\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + code + "\n(En attente de validation du paiment...)\n")
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".")
    print("\nMerci ! Votre paiement a été accepté.")
    statu_paiement = true
    //end paiement


    //si paiement effectuer MàJ stock  //Il vous faut rainbow brackets ici
    if (statu_paiement) {
      if (sucre > 1) {
        if (boisson == 1 && coffeeStocks >= 8 && sugarStocks >= qttesucre) {
          machines(machineId - 1).removeIngredient("coffee", 8)
          machines(machineId - 1).removeIngredient("sugar", qttesucre)
        }
        else {
          if (boisson == 2 && coffeeStocks >= 6 && milkStocks >= 100 + calcul_supplait && sugarStocks >= qttesucre) {
            machines(machineId - 1).removeIngredient("coffee", 8)
            machines(machineId - 1).removeIngredient("milk", (100 + calcul_supplait))
            machines(machineId - 1).removeIngredient("sugar", qttesucre)

          }
          else {
            if (boisson == 3 && coffeeStocks >= 6 && milkStocks >= 120 + calcul_supplait && sugarStocks >= qttesucre) {
              machines(machineId - 1).removeIngredient("coffee", 6)
              machines(machineId - 1).removeIngredient("milk", (120 + calcul_supplait))
              machines(machineId - 1).removeIngredient("sugar", qttesucre)

            }
            else {
              if (boisson == 4 && coffeeStocks >= 8 && milkStocks >= 150 + calcul_supplait && sugarStocks >= qttesucre) {
                machines(machineId - 1).removeIngredient("coffee", 6)
                machines(machineId - 1).removeIngredient("milk", (150 + calcul_supplait))
                machines(machineId - 1).removeIngredient("sugar", qttesucre)

              }
              else {
                if (boisson == 5 && coffeeStocks >= 12 && milkStocks >= 200 + calcul_supplait && sugarStocks >= qttesucre) {
                  machines(machineId - 1).removeIngredient("coffee", 12)
                  machines(machineId - 1).removeIngredient("milk", (150 + calcul_supplait))
                  machines(machineId - 1).removeIngredient("sugar", qttesucre)

                }
                else if (!stocksuffisant) {
                  println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");
                  Thread.sleep(2000);
                  serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
                }
              }
            }
          }
        }
      }
      else if (sucre <= 1) {
        if (boisson == 1 && coffeeStocks >= 8) {
          machines(machineId - 1).removeIngredient("coffee", 8)
        }
        else {
          if (boisson == 2 && coffeeStocks >= 6 && milkStocks >= 100 + calcul_supplait) {
            machines(machineId - 1).removeIngredient("coffee", 6)
            machines(machineId - 1).removeIngredient("milk", 100 + calcul_supplait)

          }
          else {
            if (boisson == 3 && coffeeStocks >= 6 && milkStocks >= 120 + calcul_supplait) {
              machines(machineId - 1).removeIngredient("coffee", 6)
              machines(machineId - 1).removeIngredient("milk", (120 + calcul_supplait))
            }
            else {
              if (boisson == 4 && coffeeStocks >= 8 && milkStocks >= 150 + calcul_supplait) {
                machines(machineId - 1).removeIngredient("coffee", 8)
                machines(machineId - 1).removeIngredient("milk", (150 + calcul_supplait))
              }
              else {
                if (boisson == 5 && coffeeStocks >= 12 && milkStocks >= 200 + calcul_supplait) {
                  machines(machineId).removeIngredient("coffee", 12)
                  machines(machineId).removeIngredient("milk", (200 + calcul_supplait))
                }
                else if (!stocksuffisant) {
                  println("\n>> Stock Insufisant. Veuillez sélectionner une autre boisson.\n");
                  Thread.sleep(2000);
                  serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
                }
              }
            }
          }
        }
      }
    }

    //end stock


    //prépa boisson msg de fin
    println("\nPréparation de votre boisson...")
    Thread.sleep(500);
    print("[.");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".]\n");
    Thread.sleep(1000)

    val phrase = "Votre " + boisson_selec + " est prêt ! Bonne dégustation !"
    for (i <- phrase) {
      print(i); Thread.sleep(20)
    }

    Thread.sleep(1000)
    menu_principal()
    //fin prépa boisson
    true
  }


  def menuAdmin(): Unit = {
    val choixAdmin = InputManagement("\n===========Mode Admin===========\nQuelle opération souhaitez-vous faire ?\n1) Réapprovisionnement du stock\n2) Mettre à jour le code PIN\n3) Retour\n> ", Set(1, 2, 3))

    if (choixAdmin == 3) menu_principal()

    else {
      val nbrmachine = machines.length
      val machineId = InputManagement("\nVeuillez sélectionner votre Machine (1-" + nbrmachine + "):\n> ", (1 to nbrmachine).toSet)
      val machineSelectionner = machines.find(_.id == machineId).get
      val machinePins = machineSelectionner.pincode
      validatePin(machineId, choixAdmin, machinePins.toInt)
    }
  }

  def validatePin(machineId: Int, choixAdmin: Int, machinePins: Int): Boolean = {
    var tentativePIN = 3
    val correctPin = machinePins
    while (tentativePIN > 0) {
      val tentative = InputManagement("Entrez le code PIN de la machine numéro " + (machineId) + ":\n> ", (0 to 999999).toSet) //malheureusement cela ralenti le processus
      if (tentative.toInt == machinePins) {
        println(">> Accès autorisé.")
        Thread.sleep(1000)
        if (choixAdmin == 1) restockMachine(machineId)
        else if (choixAdmin == 2) updatePin(machineId, machinePins)
        return true
      } else {
        tentativePIN -= 1
        if (tentativePIN == 0) {
          println(">> Trop de tentatives échouées. Fin du programme.")
          Thread.sleep(1000)
          quitter()
          return false
        } else {
          print(s">> Code PIN incorrect. "+tentativePIN+" tentatives restantes.\n> ")
          Thread.sleep(1000)
        }
      }
    }
    menu_principal()
    false
  }

  def updatePin(machineId: Int, machinePins: Int): Unit = {
    println("Mise à jour du code PIN pour la machine " + machineId + ".")
    var newPin = 0.toString
    do {
      print("Entrez un nouveau code PIN à 6 chiffres: \n> ")
      newPin = readLine()
    } while (newPin.size != 6 || !newPin.toIntOption.isDefined)
    machines(machineId - 1).pincode = newPin

    savecsv(filename, machines)

    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(1000)
    menu_principal()
  }

  def restockMachine(machineId: Int): Unit = {
    var ajout_poudre_cafe = 0
    var ajout_sucre = 0
    var ajout_lait = 0.0
    var convertionlait = 0.0

    val machineSelectionner = machines.find(_.id == machineId).get

    val coffeeStocks = machineSelectionner.coffee
    val sugarStocks = machineSelectionner.sugar
    val milkStocks = machineSelectionner.milk

    convertionlait = milkStocks.toFloat / 1000
    println("===========Mode Admin===========\nNiveaux de stock actuels:\n    Poudre de café: " + coffeeStocks + "g\n    Lait          : " + f"$convertionlait%.2f" + "L\n    Sucre         : " + sugarStocks + "g")
    print("Réapprovisionnement des stocks");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".")
    print("\nAjout:\n    Pourdre de café: ")
    ajout_poudre_cafe = readInt()
    print("    Lait           : ")
    val convertion_inverse = readDouble()
    ajout_lait = convertion_inverse * 1000
    print("    Sucre          : ")
    ajout_sucre = readInt()

    machines(machineId - 1).addIngredient("coffee", ajout_poudre_cafe)
    machines(machineId - 1).addIngredient("sugar", ajout_sucre)
    machines(machineId - 1).addIngredient("milk", ajout_lait.toInt)
    savecsv(filename, machines)
    print("Retour au menu principal");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".");
    Thread.sleep(500);
    print(".\n")
    menu_principal()
  }

  def main(args: Array[String]): Unit = {
    try {
      loadcsv(filename)
      val machineCount = Main.machines.length
      if (machineCount > 0) {
        println("Chargement des machines depuis "+filename+"...")
        println(">> " + machineCount + " machine(s) détectés")
        for (machine <- machines) {
          println("\nMachine " + machine.id + " chargée :")
          println(s"    ID: " + machine.id)
          println(f"    Code PIN: " + machine.pincode)
          println(f"    Lait: "+ machine.milk / 1000.0+"L")
          println(f"    Sucre: " + machine.sugar + "g")
          println(f"    Café: " + machine.coffee + "g")
        }
      }
      if (machineCount <= 0){println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFichier "+ filename +" vide, fermeture du programme."); sys.exit(1)}
    } catch {
      case _ => print("")
      case ex: java.io.IOException => error += 1; println(">>Erreur : Échec de l’écriture dans " + filename + ". Le fichier peut être verrouillé ou en lecture seule.")
      case _ => error += 1; println("Erreur : Échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")

    }
    if (error == 0) {
      println("\n>> Toute(s) les machine(s) ont été chargée(s) avec succès.")
      menu_principal()
    } else sys.exit(1)
  }

}
