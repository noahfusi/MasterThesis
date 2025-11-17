import io.StdIn._
import java.io.{FileWriter,PrintWriter}
import scala.io.Source._
import collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit ={

    if (ingredient == "lait") {
      milk += amount
    } else if (ingredient== "sucre") {
      sugar += amount
    } else if (ingredient== "café") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "lait") {
      if (milk >= amount) {
        milk -= amount
        true
      } else {
        false
      }
    } else if (ingredient== "sucre") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else {
        false
      }
    } else if (ingredient== "café") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else {
        false
      }
    }else {
      println("Ingrédient "+ingredient+ " pas pas trouvé")
      false
    }
  }
}

object Main {
  var quitter = false
  var assez_lait = false
  var assez_sucre = false

  //Conso
  val conso_cafe = Array(0, 8, 6, 6, 8, 12)
  val conso_lait = Array(0, 0, 100, 120, 150, 200)
  val conso_sucre = Array(0, 0, 5, 10, 15)
  val conso_lait_sup = Array(0, 50, 100, 150)

  // prix
  val prix_cafe = Array(0.00, 2.00, 2.50, 2.70, 3.20, 3.70)
  val prix_sucre = Array(0.00, 0.10, 0.20, 0.30)
  val prix_lait_supp = Array(0.00, 0.05, 0.10, 0.15)

  var choix_client_sucre = 0
  var choix_client_lait = 0
  var choix_client = 0
  val machines = ArrayBuffer[Machine]()
  val filename = "machines.csv"

  var erreurfichier = false
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machinesBuffer = ArrayBuffer[Machine]()
    val fichier = new java.io.File(filename)

    if (fichier.exists()) {
      println("Fichier " + filename + " trouvé. Chargement en cours...")

      try {
        val source = fromFile(filename)
        val lines = source.getLines().drop(1)

        var id = 1
        for (line <- lines) {
          val data = line.split(",")
          if (data.length == 4) {
            machinesBuffer += new Machine(id, data(0), data(1).toInt, data(2).toInt, data(3).toInt)
            println("Machine " + id + " chargée :")
            println("    ID: " + id)
            println("    Code PIN: " + data(0))
            printf("    Lait: %.3f L\n", data(1).toInt / 1000.0)
            println("    Sucre: " + data(2) + " g")
            println("    Café: " + data(3) + " g")
            id += 1
          }
        }
        source.close()
        println(machinesBuffer.length + " machine(s) chargée(s) avec succès.")
      } catch {
        case _: java.io.FileNotFoundException =>
          println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
          erreurfichier = true
        case _: Exception =>
          println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
          erreurfichier = true
      }
    } else {
      println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
      erreurfichier = true
    }
    machinesBuffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        val line = ( machine.pincode +","+ machine.milk+"," + machine.sugar+"," + machine.coffee)
        writer.println(line)
      }
      writer.close()
      println("Fichier " + filename + "sauvegardé avec succès. ")
    } catch {
      case _: Exception => println("Erreur : Impossible de sauvegarder dans le fichier " + filename)
        erreurfichier = true
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var tentative = 3
    while (tentative > 0) {
      println("Entrez le code PIN pour la machine " + machine.id)
      val PIN = readLine(">")
      if (PIN == machine.pincode) return true
      tentative -= 1
      println("Code PIN incorrect. " + tentative + " tentative(s) restante(s)")
    }
    println("Trop de tentatives échouées. Fin du programme.")
    quitter = true
    return false
  }

  def updatePin(machineId: Int): Unit = {
    var isValid = false
    var newPin =""
    println("Mise à jour du code PIN pour la machine " + machineId)
    while (!isValid) {
      newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if (newPin.length == 6) {
        isValid = true
        for (c <- newPin) {
          if (!"0123456789".contains(c)) {
            isValid = false
          }
        }
      }
      else {
        isValid=false
      }
    }
    machines(machineId - 1).pincode = newPin
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    //savecsv(filename, machines)
  }


  def serveClient(machineId: Int): Boolean = {
    var recap_prix_cafe = 0.0
    var recap_prix_sucre = 0.0
    var recap_prix_lait = 0.0
    var machine: Machine = null
    if (machineId > 0 && machineId <= machines.length) {
      machine = machines(machineId - 1)

    } else {
      println("Machine introuvable.")
    }
    println("Machine " + machineId + " sélectionnée.")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00 ")
    println("2) Cappuccino - CHF 2.50 ")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    choix_client = readLine("> ").toInt

    while (!(choix_client == 1 || choix_client == 2 || choix_client == 3)) {
      print("Votre sélection n'est pas correcte, choisissez entre 1) Expresso - CHF 2.00  2) Cappuccino - CHF 2.50  3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
      choix_client = readLine("> ").toInt
    }

    var boisson_reussie = false
    var erreur = true
    var prix_total = 0.0
    var choix_latte_taille = 0
    if (choix_client == 1) {
      if (machine.coffee >= conso_cafe(choix_client)) {
        boisson_reussie = true
        assez_lait = true
        erreur = false
      }
      else {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
    }

    else if (choix_client == 2) {
      if (machine.coffee >= conso_cafe(choix_client) && machine.milk >= conso_lait(choix_client)) {
        boisson_reussie = true
        erreur = false
      }
      else if (machine.coffee < conso_cafe(choix_client)) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
      else if (machine.milk < conso_lait(choix_client)) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        erreur = true
        return false
      }
    }

    else if (choix_client == 3) {
      println("Quelle taille de latté souhaitez-vous ? ")
      println("1) Petit - CHF 2.70 ")
      println("2) Moyen - CHF 3.20 ")
      println("3) Grand - CHF 3.70 ")
      choix_latte_taille = readLine("> ").toInt
      //vérif des données valides
      while (!(choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1) Petit - CHF 2.70  2) Moyen - CHF 3.20  3) Grand - CHF 3.70")
        choix_latte_taille = readLine("> ").toInt
      }

      if (choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3) {
        if ((machine.coffee >= conso_cafe(choix_latte_taille + 2)) && (machine.milk >= conso_lait(choix_latte_taille + 2))) {
          boisson_reussie = true
          erreur = false
        }

        else if ((machine.coffee < conso_cafe(choix_latte_taille + 2)) && (machine.milk >= conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
          erreur = true
          return false
        }
        else if ((machine.coffee >= conso_cafe(choix_latte_taille + 2)) && (machine.milk < conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
          erreur = true
          return false
        }
        else if ((machine.coffee < conso_cafe(choix_latte_taille + 2)) && (machine.milk < conso_lait(choix_latte_taille + 2))) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          erreur = true
          return false
        }
      }
    }
    //SUCRE
    if (boisson_reussie) {
      println("Souhaitez-vous du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10 ")
      println("3) Moyen (10g) - CHF 0.20 ")
      println("4) Beaucoup (15g) - CHF 0.30")
      choix_client_sucre = readLine("> ").toInt

      while (!(choix_client_sucre == 1 || choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1)  Sans sucre  2) Peu (5g) - CHF 0.10  3) Moyen (10g) - CHF 0.20  4) Beaucoup (15g) - CHF 0.30 ")
        choix_client_sucre = readLine("> ").toInt
      }

      if (choix_client_sucre == 1) {
        assez_sucre = true
      }

      else if (choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4) {
        if (machine.sugar < conso_sucre(choix_client_sucre)) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          assez_sucre = false
          erreur = true
          return false
        }
        else {
          assez_sucre = true
          erreur = false
        }
      }

      // LAIT SUPP
      if (boisson_reussie && assez_sucre) {
        if ((choix_client == 2) || (choix_client == 3)) {
          println("Souhaitez-vous ajouter du lait en supplément ? ")
          println("1) Oui ")
          println("2) Non ")
          choix_client_lait = readLine("> ").toInt


          while (!(choix_client_lait == 1 || choix_client_lait == 2)) {
            print("Votre sélection n'est pas correcte, choisissez entre 1) Oui  2) Non ")
            choix_client_lait = readLine("> ").toInt
          }
          //cbn doses
          if (choix_client_lait == 1) {
            println("Combien de dose(s) ? ")
            var dose_lait_choix = readLine("> ").toInt

            while (!(dose_lait_choix == 1 || dose_lait_choix == 2 || dose_lait_choix == 3)) {
              print("Votre sélection n'est pas correcte, choisissez entre 1, 2 ou 3 doses. ")
              dose_lait_choix = readLine("> ").toInt
            }

            if (dose_lait_choix == 1 || dose_lait_choix == 2 || dose_lait_choix == 3) {
              if (machine.milk < conso_lait_sup(dose_lait_choix)) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                assez_lait = false
                erreur = true
                return false
              }
              else {
                machine.milk -= conso_lait_sup(dose_lait_choix)
                prix_total += prix_lait_supp(dose_lait_choix)
                recap_prix_lait += prix_lait_supp(dose_lait_choix)
                assez_lait = true
                erreur = false
              }
            }
          }
          else {
            assez_lait = true
          }
        }
      }
    }

    if (boisson_reussie && assez_sucre && assez_lait) {
      erreur = false
      val cafe = Array("Expresso", "Cappuccino")
      val latte = Array("Latté (Petit)", "Latté (Moyen)", "Latté (Grand)")

      //expresso ou cappuccino
      if (choix_client == 1 || choix_client == 2) {
        machine.coffee -= conso_cafe(choix_client)
        prix_total += prix_cafe(choix_client)
        recap_prix_cafe += prix_cafe(choix_client)
        println("Boisson sélectionnée : " + cafe(choix_client - 1))
      }
      //latte
      else {
        if (choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3) {
          machine.coffee -= conso_cafe(choix_latte_taille + 2)
          machine.milk -= conso_lait(choix_latte_taille + 2)
          prix_total += prix_cafe(choix_latte_taille + 2)
          recap_prix_cafe += prix_cafe(choix_latte_taille + 2)
          println("Boisson sélectionnée : " + latte(choix_latte_taille - 1))
        }
      }
      //sucre
      val sucre = Array("Peu", "Moyen", "Beaucoup")
      if (choix_client_sucre == 1) {
        println("Niveau de sucre : Sans sucre ")
      }
      else if (choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4) {
        machine.sugar -= conso_sucre(choix_client_sucre)
        prix_total += prix_sucre(choix_client_sucre - 1)
        recap_prix_sucre += prix_sucre(choix_client_sucre - 1)
        println("Niveau de sucre : " + sucre(choix_client_sucre - 2))
      }

      //lait
      if (choix_client == 2 || choix_client == 3) {
        if (choix_client_lait == 1) {
          println("Lait supplémentaire: Oui ")
        }
        else {
          println("Lait supplémentaire: Non ")
        }
      }
      //savecsv(filename, machines)
      if ((recap_prix_sucre > 0) && (recap_prix_lait > 0)) {
        printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, recap_prix_lait, prix_total)
      }
      //sucre
      else if (recap_prix_sucre > 0) {
        printf("Prix total: CHF %.2f + CHF %.2f  = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, prix_total)
      }
      //lait
      else if (recap_prix_lait > 0) {
        printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_lait, prix_total)
      }
      // ni lait ni sucre
      else if ((recap_prix_sucre == 0) && (recap_prix_lait == 0)) {
        printf("Prix total: CHF %.2f\n ", prix_total)
      }

      //Paiement
      val alphanum = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      val codeLength = 5 // Longueur du code Twint
      val code_twint = Array.fill(codeLength) {
        alphanum((math.random() * alphanum.length).toInt)
      }.mkString

      println()
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + code_twint)
      println("En attente de paiement...")
      println()
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println()
      // Préparation de la boisson
      println("Préparation de votre boisson...")
      println("[...]")
      println("Votre Cappuccino est prêt ! Bonne dégustation !")
      println()
      return true
    }
    return false
  }

  def main(args: Array[String]): Unit = {

    val PIN_in = "434343"
    val machinePins = Array.fill(machines.length)(PIN_in)
    val filename = "machines.csv"

    println("Chargement des machines depuis machines.csv...")
    machines.clear()
    machines ++= loadcsv(filename)

    if (erreurfichier){
      println("Erreur : Echec du chargement ou de la sauvegarde des machines. \nFermeture du programme.")
      sys.exit(1)
    }

    while (!quitter) {
      //Menu d'accueil
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choix = readLine("> ").toInt

      while (!(choix == 1 || choix == 2 || choix == 3)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1) Client 2) Admin 3) Quitter ")
        choix = readLine("> ").toInt
      }
      if (choix == 1) {
        var boissonServie = false
        do {
          println("Veuillez choisir une machine ")
          var machineId = readLine("> ").toInt

          if (machineId < 1 || machineId > machines.length) {
            print("Votre sélection n'est pas correcte, choisissez une machine entre 1 et 5 ")
          }
          else {
            boissonServie = serveClient(machineId)
          }
        } while (!boissonServie)
      }

      if (choix == 2) {
        println("Veuillez sélectionner une machine")
        var machineId = readLine("> ").toInt
        while (machineId <0 || machineId > machines.length) {
          print("Votre sélection n'est pas correcte, choisissez une machine existante ")
          machineId = readLine("> ").toInt
        }
        val machine = machines(machineId - 1)
        if (validatePin(machine)) {
          println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN")
          var choix_admin = readLine(">").toInt
          while (!(choix_admin == 1 || choix_admin == 2)) {
            print("Votre sélection n'est pas correcte, choisissez entre 1 ou 2 ")
            choix_admin = readLine("> ").toInt
          }
          if (choix_admin == 1) {
            var machine: Machine = null
            if (machineId > 0 && machineId <= machines.length) {
              machine = machines(machineId - 1)

            } else {
              println("Machine introuvable.")
            }
            if (machine != null) {
              println("Stocks: ")
              println("    Poudre de cafe: " + machine.coffee + "g")
              printf("    Lait          :%.3fL", machine.milk / 1000.0)
              println()
              println("    Sucre         : " + machine.sugar + "g")
              println()

              println("1) Ajouter\n2) Retirer")
              var action = readLine("> ").toInt
              while (!(action == 1 || action == 2)) {
                print("Votre sélection n'est pas correcte, choisissez entre 1 ou 2 ")
                action = readLine("> ").toInt
              }

              if (action == 1) {
                for (ingredient <- List("café", "lait", "sucre")) {
                  if (ingredient == "lait"){
                    println("Quantité de " + ingredient + " à ajouter :")
                    var amount = ((readLine("> ").toDouble)*1000).toInt
                    while (amount<0){
                      println("Les quantités ajoutées doivent être positives.")
                      amount = ((readLine("> ").toDouble)*1000).toInt
                    }
                    machine.addIngredient(ingredient, amount)
                  }else{
                    println("Quantité de " + ingredient + " à ajouter :")
                    var amount = readLine("> ").toInt
                    while (amount<0){
                      println("Les quantités à retirer doivent être positives.")
                      amount = ((readLine("> ").toDouble)*1000).toInt
                    }
                    machine.addIngredient(ingredient, amount)
                  }
                }
              } else if (action == 2){
                for (ingredient <- List("café", "lait", "sucre")) {
                  if (ingredient == "lait") {
                    println("Quantité de " + ingredient + " à retirer :")
                    var amount = ((readLine("> ").toDouble) * 1000).toInt
                    machine.removeIngredient(ingredient, amount)
                  } else {
                    println("Quantité de " + ingredient + " à retirer :")
                    var amount = readLine("> ").toInt
                    machine.removeIngredient(ingredient, amount)
                  }
                }
              }
              // Sauvegarde après modification des stocks
              // savecsv(filename, machines)
            } else {
              println("Machine introuvable.")
            }
          }
          else {
            updatePin(machineId)
          }
        }
      }

      if (choix == 3) {
        println("Sauvegarde des machines dans machines.csv...")
        savecsv(filename, machines)
        println("Vous avez quitté le programme.")
        quitter = true
      }
    }
  }
}