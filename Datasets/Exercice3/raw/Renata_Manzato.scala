import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import java.io.FileNotFoundException
import scala.io.StdIn.{readDouble, readInt, readLine}
import java.nio.file.AccessDeniedException


object Main {
  def main(args: Array[String]): Unit = {
    var reiteration1 = true
    val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var codedepaiement = ""
    for (_ <- 1 to 5) {
        val randomChar = alphanumerique((math.random * alphanumerique.length).toInt)
        codedepaiement += randomChar
      }

    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

        override def toString: String = {
          f"Machine ${id + 1} chargée :\nID: ${id + 1}\nCode PIN: $pincode\nLait: ${milk}ml\nSucre: ${sugar}g\nCafé: ${coffee}g\n\n"
        }

        def addIngredient(ingredient: String, amount: Int): Unit = {
          if (ingredient == "COFFEE") {
            coffee += amount
          } else if (ingredient == "MILK") {
            milk += amount
          } else if (ingredient == "SUGAR") {
            sugar += amount
          } else {
            println("Frase da scegliere con bubu riguardo l'errore")
          }
        }

        def removeIngredient(ingredient: String, amount: Int): Boolean = {
          if (ingredient == "COFFEE" && coffee >= amount) {
            coffee -= amount
            true
          } else if (ingredient == "MILK" && milk >= amount) {
            milk -= amount
            true
          } else if (ingredient == "SUGAR" && sugar >= amount) {
            sugar -= amount
            true
          } else {
            println("Stock insuffisant")
            false
          }
        }

        def validatePin(): Boolean = {
          var tentatives = 3
          while (tentatives > 0) {
            print("Entrez le code PIN : \n> ")
            val PIN_saisi = readLine()
            if (PIN_saisi == pincode) {
              println("Accès accordé. \n")
              return true
            } else {
              tentatives -= 1
              if (tentatives > 0) {
                println("PIN incorrect, " + tentatives + " tentatives restantes.")
              } else {
                println("Trop de tentatives échouées. Fin du programme.")
                return false
              }
            }
          }
          false
        }
      }

    def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
        val machine = machines(machineId)
        var nouveauPIN = ""
        var estvalide = false

        while (!estvalide) {
          println("Entrez un nouveau code PIN à 6 chiffres >")
          nouveauPIN = readLine()

          if (nouveauPIN.length == 6) {
            estvalide = true
          }
        }
        machine.pincode = nouveauPIN
        println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
      }

    def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
        val machine = machines(machineId)
        println("Réapprovisionnement des stocks.")

        println("Niveaux de stock actuels : ")
        println(s"Poudre de café : ${machine.coffee}g")
        println(s"Sucre : ${machine.sugar}g")
        println(s"Lait : ${machine.milk} ml")

        print("Entrez la quantité de poudre de café à ajouter (g) : ")
        val ajoutCafe = readInt()
        machine.addIngredient("COFFEE", ajoutCafe)
        print("Entrez la quantité de sucre à ajouter (g) : ")
        val ajoutSucre = readInt()
        machine.addIngredient("SUGAR", ajoutSucre)
        print("Entrez la quantité de lait à ajouter (L) : ")
        val ajoutLait = readDouble()
        machine.addIngredient("MILK", (ajoutLait * 1000).toInt)


        println("Les stocks ont été mis à jour avec succès.")
        println(s"Poudre de café : ${machine.coffee}g")
        println(s"Sucre : ${machine.sugar}g")
        println(s"Lait : ${machine.milk} ml")
      }


    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val buffer = ArrayBuffer[Machine]()
      try {
        val source = Source.fromFile(filename)
        val lines = source.getLines.drop(1)
        var index = 0
        for (line <- lines) {
          val parts = line.split(",")
          if (parts.length == 4) {
            val machine = new Machine(index, parts(0), parts(1).toInt, parts(2).toInt, parts(3).toInt)
            buffer += machine
            index += 1
          }
        }
      }
      catch {
        case e: FileNotFoundException =>
          println("\n\nErreur : Fichier introuvable. Verifiez le chemin d'accès et réessayez.\n")
          println("Erreur : échec du chargement ou de la sauvegarde des machines.\n")
          println("Fermeture du programme.")
          System.exit(1)
        case e: Exception =>
          println("\n\n Erreur : échec du chargement ou de la sauvegarde des machines.\n")
          println("Fermeture du programme.")
          System.exit(1)
      }
      buffer
    }

    val machines: ArrayBuffer[Machine] = loadcsv("machines.csv")
    def printMachines(machines: ArrayBuffer[Machine]): Unit = {
        println(s"${machines.length} machine(s) chargée(s) avec succès.\n")
        machines.foreach(println)
      }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
        val pw = new PrintWriter(new File(filename))
        try {
          pw.println("PIN,MILK,SUGAR,COFFEE")
          machines.foreach { machine =>
            pw.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
          }
          println(s"Sauvegarde des ${machines.length} machines dans machines.csv...")
          Thread.sleep(2000)
          println("Fichier 'machines.csv' sauvegardé avec succès")
        } catch {
          case e: FileNotFoundException =>
            println(s"Sauvegarde des machines dans ${filename}...\n\n")
            Thread.sleep(2000)
            println(s"\n\nErreur : échec de l’écriture dans ${filename}.\nLe fichier peut être verrouillé ou en lecture seule\n")
            println("Erreur : échec du chargement ou de la sauvegarde des machines.\n")
            println("Fermeture du programme.")
            System.exit(1)
          case e: Exception =>
            println("\n\n Erreur : échec du chargement ou de la sauvegarde des machines.\n")
            println("Fermeture du programme.")
            System.exit(1)

          } finally {
          pw.close()
        }
      }

    def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

        val machine = machines(machineId)
        var prixtotal = 0.00

        var choixboisson = 0
        while (choixboisson < 1 || choixboisson > 3) {
          print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
          choixboisson = readInt()
          if (choixboisson < 1 || choixboisson > 3) {
            println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
          }
        }
        if (choixboisson == 1) {
          if (machine.removeIngredient("COFFEE", 8)) {
            prixtotal += 2.00
          }
          else return false

        }

        if (choixboisson == 2) {
          if (machine.removeIngredient("COFFEE", 6) && machine.removeIngredient("MILK", 100)) {
            prixtotal += 2.50
          }
          else return false
        }

        if (choixboisson == 3) {
          var choixtaille = 0
          while (choixtaille < 1 || choixtaille > 3) {
            print("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>")
            choixtaille = readInt()
            if (choixtaille < 1 || choixtaille > 3) {
              println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
            }
            if (choixtaille == 1) {
              if (machine.removeIngredient("COFFEE", 6) && machine.removeIngredient("MILK", 120)) {
                prixtotal += 2.70
              }else return false
            }
            if (choixtaille == 3) {
              if (machine.removeIngredient("COFFEE", 12) && machine.removeIngredient("MILK", 200)) {
                prixtotal += 3.70
              }else return false
            }
            if (choixtaille == 2) {
              if (machine.removeIngredient("COFFEE", 8) && machine.removeIngredient("MILK", 150)) {
                prixtotal += 3.20
              }else return false
            }
          }
        }

        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            print("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.\n")
          }
        }
        if (choixsucre == 2) {
          if (machine.removeIngredient("SUGAR", 5)) {
            prixtotal += 0.10
          }else return false
        }
        if (choixsucre == 3) {
          if (machine.removeIngredient("SUGAR", 10)) {
            prixtotal += 0.20
          }else return false
        }
        if (choixsucre == 4) {
          if (machine.removeIngredient("SUGAR", 15)) {
            prixtotal += 0.30
          }else return false
        }

        if (choixboisson == 2 || choixboisson == 3) {
          var choixlait = 0
          while (choixlait < 1 || choixlait > 2) {
            print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pout Cappucino et Latte)\n1) Oui\n2) Non\n>")
            choixlait = readInt()
            if (choixlait < 1 || choixlait > 2) {
              println("Choix invalide. Veuillez sélectionner 1 ou 2.")
            }
          }
          if (choixlait == 1) {
            var choixdose = 0
            while (choixdose < 1 || choixdose > 3) {
              print("Combien de doses ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) Trois doses (150ml) - CHF 0.15\n>")
              choixdose = readInt()
              if (choixdose < 1 || choixdose > 3) {
                println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
              }
              if (choixdose == 1) {
                if (machine.removeIngredient("MILK", 50)) {
                  prixtotal += 0.05
                }else return false
              }
              if (choixdose == 2) {
                if (machine.removeIngredient("MILK", 100)) {
                  prixtotal += 0.10
                }else return false
              }
              if (choixdose == 3) {
                if (machine.removeIngredient("MILK", 150)) {
                  prixtotal += 0.15
                }else return false
              }
            }
          }

        }

        println(f"Prix total : $prixtotal%.2f CHF")
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + codedepaiement + "\n(En attente de validation du paiement...)")
        Thread.sleep(2000)
        println("Merci, votre paiement a été accepté.\n")
        println("Préparation de votre boisson...\n[...]\n")
        Thread.sleep(3000)
        println("Votre boisson est prête ! Bonne dégustation !\n")

        true
      }


    println("\n\nChargement des machines depuis machines.csv...\n\n")
    Thread.sleep(2000)
    printMachines(machines)

    while(reiteration1) {
      var choixmode = 0
      while (choixmode < 1 || choixmode > 3) {
        print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
        choixmode = readInt()
        if (choixmode < 1 || choixmode > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
        }

        if (choixmode == 1) {
          var machineselectionne = false
          while (!machineselectionne) {
            var choixId = -1
            while (choixId < 1 || choixId > machines.length) {
              println(s"Veuillez sélectionner la machine (1-${machines.length}) >")
              choixId = readInt()
              if (choixId < 1 || choixId > machines.length) {
                println(s"Choix invalide. Veuillez sélectionner un numéro entre 1 et ${machines.length}")
              }
            }
            val selectedMachine = machines(choixId - 1)
            println(s"Vous avez sélectionné la machine ${choixId}.")
            if (selectedMachine.validatePin()) {
              val stocksuffisant = serveClient(choixId - 1, machines)
              if (stocksuffisant) {
                machineselectionne = true
              }
              else {
                machineselectionne = false
              }
            }
            else {
              println("PIN incorrect ou trop de tentatives.")
              savecsv("machines.csv", machines)
              reiteration1 = false
              machineselectionne = true
            }
          }
        }

        if (choixmode == 2) {
          var machineselectionne = false
          while (!machineselectionne) {
            var choixId = -1
            while (choixId < 1 || choixId > machines.length) {
              println(s"Veuillez sélectionner la machine (1-${machines.length}) >")
              choixId = readInt()
              if (choixId < 1 || choixId > machines.length) {
                println(s"Choix invalide. Veuillez sélectionner un numéro entre 1 et ${machines.length}")
              }
            }
            val selectedMachine = machines(choixId - 1)
            println(s"Vous avez sélectionné la machine ${choixId}.")
            if (selectedMachine.validatePin()) {
              var choixAdmin = 0
              while (choixAdmin < 1 || choixAdmin > 2) {
                println("1) Réapprovisionnement\n2) Mise à jour du code PIN")
                choixAdmin = readInt()
                if (choixmode < 1 || choixmode > 2) {
                  println("Choix invalide. Veuillez sélectionner 1 ou 2.")
                }
              }
              if (choixAdmin == 1) {
                restockMachine(choixId - 1, machines)
                machineselectionne = true
              }
              if (choixAdmin == 2) {
                updatePin(choixId - 1, machines)
                machineselectionne = true
              }
            }
            else {
              println("PIN incorrect ou trop de tentatives.")
              savecsv("machines.csv", machines)
              reiteration1 = false
              machineselectionne = true
            }
          }
        }

        if (choixmode == 3) {
          savecsv("machines.csv", machines)
          reiteration1 = false
          println("\t\tBye bye!")
        }
      }
    }

  }
}

