import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io._
import scala.util.{Random, Try}
//lors de l'ajout ou suppression d'ingredients en mode admin, il faut écrire en anglais.
object Main {
  def main(args: Array[String]): Unit = {

    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

      def addIngredient(ingredient: String, amount: Int): Unit = {
        if (ingredient == "milk"){milk += amount
        }else if (ingredient == "sugar"){sugar += amount
        }else if (ingredient == "coffee"){coffee += amount
        }else{println("Ingrédient inconnu.")}}

      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        if (ingredient == "milk") {
          if (milk >= amount) {
            milk -= amount
            true} else {false}
        } else if (ingredient == "sugar") {
          if (sugar >= amount) {
            sugar -= amount
            true} else {false}
        } else if (ingredient == "coffee") {
          if (coffee >= amount) {
            coffee -= amount
            true} else {false}
        } else {false}
      }

      //fonction d'affichage détaillé ajoutée pour la facilité d'usage
      def details(): Unit = {
        println("ID: " + id + "\nCode PIN: " + pincode + "\nLait: " + milk + "L \nSucre: " + sugar + "g \nCafé: " + coffee + "g")
      }
    }

    def serveClient(machine: Machine): Unit = {
      //====================MODE CLIENT================================================================================================
      var prixcafe = 0.0
      var choixcafe = 0
      var cafesuffisant = true
      var qteexpresso = 8
      var qtecappuccino = 6
      var qtelaitcappuccino = 100
      var sucreutilise = 0
      var sucresuffisant = true
      var tailleLatte = 0
      var qtepetitlatte = 6
      var qtelaitpetitlatte = 120
      var qtemoyenlatte = 8
      var qtelaitmoyenlatte = 150
      var qtegrandlatte = 12
      var qtelaitgrandlatte = 200
      var laitsupp = 0
      var laitsuffisant = true
      var doseLaitSupp = 0

      do {
        println("Veuillez sélectionner le type de café :\n1) Expresso\n2) Cappuccino\n3) Latte\n> ")
        choixcafe = readInt()
        if (choixcafe != 1 && choixcafe != 2 && choixcafe != 3) {
          println("Sélection invalide, veuillez choisir entre 1 et 3")
        }
      } while (choixcafe != 1 && choixcafe != 2 && choixcafe != 3)

      // Choix taille latte
      if (choixcafe == 3) {
        do {
          println("Veuillez choisir la taille de votre latte :\n1) Petit Latte\n2) Moyen Latte\n3) Grand Latte\n> ")
          tailleLatte = readInt()
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Sélection invalide. Veuillez choisir entre 1 et 3.")
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
      }

      // Choix sucre
      do {
        println("Combien de doses de sucre souhaitez-vous ?\n1)Sans Sucre\n2) Peu de sucre\n3) Sucre moyen\n4) Beaucoup de sucre\n> ")
        sucreutilise = readInt()
        if (sucreutilise < 1 || sucreutilise > 4) {
          println("Sélection invalide, veuillez choisir 1 à 4 portions.")
        }
      } while (sucreutilise < 1 || sucreutilise > 4)

      // Choix laitsupp
      if (choixcafe == 2 || choixcafe == 3) {
        do {
          println("Souhaitez-vous ajouter des doses supplémentaires de lait ? (1) Oui\n2) Non\n> ")
          laitsupp = readInt()
          if (laitsupp != 1 && laitsupp != 2) {
            println("Sélection invalide, veuillez choisir 1 ou 2.")
          }
        } while (laitsupp != 1 && laitsupp != 2)

        if (laitsupp == 1) {
          println("Combien de doses de lait supplémentaire ? (1 à 3 doses)")
          doseLaitSupp = readInt()
          while (doseLaitSupp < 1 || doseLaitSupp > 3) {
            println("Sélection invalide, veuillez choisir entre 1 et 3 doses.")
            doseLaitSupp = readInt()
          }
        }
      }

      // Suffisances des stocks
      cafesuffisant = machine.removeIngredient("coffee", qteexpresso * (if (choixcafe == 1) 1
                                                                        else if (choixcafe == 2) 1
                                                                        else qtepetitlatte))
      sucresuffisant = machine.removeIngredient("sugar", sucreutilise * 5)
      laitsuffisant = machine.removeIngredient("milk", (if (laitsupp == 1) 50 * doseLaitSupp else 0) +
                                                        (if (choixcafe == 2 || choixcafe == 3) 100 else 0))

      if (!cafesuffisant) {
        println("Stock de café insuffisant.")
      }
      if (!sucresuffisant) {
        println("Stock de sucre insuffisant.")
      }
      if (!laitsuffisant) {
        println("Stock de lait insuffisant.")
      }

      if (cafesuffisant && sucresuffisant && laitsuffisant) {
        if (choixcafe == 1) prixcafe = 2.00
        else if (choixcafe == 2) prixcafe = 2.50
        else if (tailleLatte == 1) prixcafe = 2.70
        else if (tailleLatte == 2) prixcafe = 3.20
        else if (tailleLatte == 3) prixcafe = 3.70

        val prixlait = doseLaitSupp * 0.05
        val prixsucre = (sucreutilise - 1) * 0.10
        val total = prixcafe + prixsucre + prixlait
        val codealpha = Random.alphanumeric.take(5).mkString.toUpperCase()

        println("Prix total: " + total + "\nVeuillez payer en utilisant Twint. Votre code de paiement est : " + codealpha + "\nPaiement accepté. Merci pour votre achat!")

        if (choixcafe == 1) machine.removeIngredient("coffee", qteexpresso)
        if (choixcafe == 2) machine.removeIngredient("coffee", qtecappuccino)
        if (choixcafe == 3) {
          if (tailleLatte == 1) machine.removeIngredient("coffee", qtepetitlatte)
          if (tailleLatte == 2) machine.removeIngredient("coffee", qtemoyenlatte)
          if (tailleLatte == 3) machine.removeIngredient("coffee", qtegrandlatte)
        }

        machine.removeIngredient("sugar", sucreutilise * 5)
        machine.removeIngredient("milk", if (laitsupp == 1) doseLaitSupp * 50 else 0)
        println("Votre boisson est prête, merci et bonne dégustation !")
      }
      else {
        println("Stock insuffisant, veuillez réessayer plus tard.")
      }
    }

    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()
      try {
        val lignes = Source.fromFile("machines.csv").getLines().drop(1)
        for (ligne <- lignes) {
          val colonne = ligne.split(",").map(_.trim)
          if (colonne.length == 4) {
              val machine = new Machine(
                machines.length + 1,
                colonne(0), // pincode
                colonne(1).toInt, // lait
                colonne(2).toInt, // sucre
                colonne(3).toInt // café
              )
              machines += machine

          } else {
            println("Format invalide dans la ligne : " + ligne + ". La ligne ne contient pas 4 colonnes.")
          }
        }
        println("Chargement des machines depuis " + filename + "...")
        println({machines.length} + " machine(s) chargée(s) avec succès.")
      } catch {
        case ex: FileNotFoundException => println("Erreur : Fichier " + filename + " introuvable.")
        case ex: Exception => println("Erreur lors de la lecture de " + filename + ".")
      }
      machines
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        val saver = new PrintWriter(new File("machines.csv"))
        saver.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          saver.println({machine.pincode} + ", " + {machine.milk} + ", " + {machine.sugar} + ", " + {machine.coffee})
        }
        saver.close()
        println("Sauvegarde des machines dans machines.csv...")
      } catch {
        case ex: Exception => println("Échec de l'écriture dans machines.csv.")
      }
    }

    def main(): Unit = {//Fonctionnement générale du code. On doit y faire appel à la fin pour executer le code!
      var machines = loadcsv("machines.csv")
      var execute = true
      var choixMachine: Option[Machine] = None

      while (execute) {

        println("       Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")

        val choixMode = readInt()

        if (choixMode == 1) {//mode client
            println("Mode Client sélectionné.\nVeuillez sélectionner la machine par ID:")
            machines.zipWithIndex.foreach//permet d'associer les données dans le fichier à leurs index.
            { case (machine, index) => println({index + 1} + " Machine ID: " + {machine.id})
            }
            var choixClientmachine = -1
            do {
              choixClientmachine = readInt()
              if (choixClientmachine < 1 || choixClientmachine > machines.length) {
                println("Sélection invalide. Veuillez essayer de nouveau.")
              }
            } while (choixClientmachine < 1 || choixClientmachine > machines.length)

            choixMachine = Some(machines(choixClientmachine - 1))

            println("Machine sélectionnée : ID " + {choixMachine.get.id})
            serveClient(choixMachine.get)

            }else if(choixMode == 2){//mode admin
            println("Mode Admin sélectionné.")
            println("Veuillez sélectionner une machine: ")
            var choixClientmachine = -1
            do {
              choixClientmachine = readInt()
              if (choixClientmachine < 1 || choixClientmachine > machines.length) {
                println("Sélection invalide. Veuillez essayer de nouveau.")
              }
            } while (choixClientmachine < 1 || choixClientmachine > machines.length)
                                                                                //Exactement même fonctionnement que mode client mais mots de passes en plus.
            choixMachine = Some(machines(choixClientmachine - 1))
            println("Machine sélectionnée : " + {choixMachine.get.id}+ " ID")

            println("Entrez le code PIN de la machine :")
            val pincode = readLine()
            val choixAdminmachine = machines.find(a => a.pincode == pincode)

            choixAdminmachine match {
              case Some(machine) =>
                println("Sélection de la machine réussie.\nQue voulez-vous faire ?\n1) Ajouter un ingrédient\n2) Retirer un ingrédient\n3) Afficher les détails de la machine\n> ")
                val choixAdmin = readInt()

                    if (choixAdmin == 1){//addition d'ingrédients
                    println("Ingrédient:")
                    val ingredient = readLine()
                    println("Quantité :")
                    val amount = readInt()
                    machine.addIngredient(ingredient, amount)
                    println(amount + " " + {ingredient} + " ajoutés à la machine.")

                      }
                    else if (choixAdmin == 2){//supression d'ingrédients
                    println("Ingrédient:")
                    val ingredient = readLine()
                    println("Quantité:")
                    val amount = readInt()
                    if (machine.removeIngredient(ingredient, amount)) {
                      println(amount + " " + {ingredient} + " retirés de la machine.")
                    } else {
                      println("Impossible de retirer " + amount + " " + {ingredient} + ". Stock insuffisant.")
                    }

                  }else if(choixAdmin == 3){machine.details()}
                  else{
                    println("Choix invalide.")}
                }//fin choix admin

           }else if (choixMode == 3){
            println("Quitter le programme.")
            savecsv("machines.csv", machines) // enregistre l'état actuel des stocks et pins dans le fichier avant de quitter le programme.
            execute = false

          }else {
            println("Choix invalide. Veuillez essayer de nouveau.")//cas dans lequel une entrée non valide à été fournie au programme.
        }
      }
      }
    main()//fonction qui active le code
  }
  }

